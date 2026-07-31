"""
macro_fundamentals.py
─────────────────────────────────────────────────────────────────────────────
Builds the monthly macro-fundamentals panel used in the thesis:

    data/processed/macro_fundamentals_forecasted_interpolated.csv

Forecast data only, following Burriel et al. (2024): gdp_growth, inflation
and current_account are composite 12-month-ahead fixed-horizon forecasts,
blending each WEO vintage's current- and next-calendar-year forecast with
weights k/12 and (12-k)/12 (k = months remaining to next December). debt_gdp
(vintages lack a consistent two-year-ahead forecast) instead takes a
Dovern-style rolling weighted average of the one-year-ahead April and October
vintage forecasts. policy_uncertainty / bank_nexus / bid_ask are actual
series carried over and forward-filled into the forecast horizon.

Columns: date, country, gdp_growth, inflation, debt_gdp, current_account,
         policy_uncertainty, bank_nexus, bid_ask

The script prints a clear warning and fills the corresponding column with
NaN if an input file is missing, so it still runs end-to-end.
─────────────────────────────────────────────────────────────────────────────
"""

import os
import warnings
import numpy as np
import pandas as pd

# Work from the repository root so all paths below are repo-relative
os.chdir(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..'))
warnings.filterwarnings("ignore")

OUT_DIR = 'data/processed'
os.makedirs(OUT_DIR, exist_ok=True)

COUNTRIES = ['AT', 'BE', 'FI', 'FR', 'DE', 'GR', 'IE', 'IT', 'NL', 'PT', 'ES']

# ISO3 -> ISO2, for forecast_vintage_panel.csv ('iso3' column)
ISO3_TO_ISO2 = {
    'AUT': 'AT', 'BEL': 'BE', 'FIN': 'FI', 'FRA': 'FR', 'DEU': 'DE',
    'GRC': 'GR', 'IRL': 'IE', 'ITA': 'IT', 'NLD': 'NL', 'PRT': 'PT', 'ESP': 'ES',
}

# Country full-name -> ISO2 (used for bank_sovereign_nexus_monthly.csv etc., kept from old script)
NAME_TO_CODE = {
    'Austria': 'AT', 'Belgium': 'BE', 'Finland': 'FI', 'France': 'FR',
    'Germany ("linked")': 'DE', 'Germany': 'DE', 'Greece': 'GR',
    'Ireland': 'IE', 'Italy': 'IT', 'Netherlands': 'NL',
    'Portugal': 'PT', 'Spain': 'ES',
}

MONTH_ABBR = {
    'jan': 1, 'feb': 2, 'mar': 3, 'apr': 4, 'may': 5, 'jun': 6,
    'jul': 7, 'aug': 8, 'sep': 9, 'oct': 10, 'nov': 11, 'dec': 12,
}


# ── Generic month-index <-> 'YYYY-MM' helpers ───────────────────────────────

def label_to_idx(label):
    """'YYYY-MM' -> absolute month index (year*12 + month0)."""
    y, m = str(label).split('-')
    return int(y) * 12 + (int(m) - 1)


def idx_to_label(idx):
    """absolute month index -> 'YYYY-MM'."""
    y, m = divmod(idx, 12)
    return f"{y}-{m + 1:02d}"


def vintage_to_idx(vintage):
    """'1990-Apr' / '1990-Oct' -> absolute month index of that vintage."""
    year, mon = str(vintage).strip().split('-')
    return int(year) * 12 + (MONTH_ABBR[mon.strip().lower()] - 1)


# ── Dovern / Banco de España eq.(1) rolling-weight interpolation ───────────

def dovern_interpolate(values_by_idx, period_length):
    """
    Generic rolling fixed-horizon weighted interpolation (Dovern et al. 2012 /
    Banco de Espana eq.1).

    values_by_idx : dict {month_idx (int): value (float)}
        Sparse observations, one per "period" (e.g. one per quarter, one per
        semi-annual vintage). Keys are the absolute month index of the FIRST
        month of each period.
    period_length : int
        Number of months in one period (3 = quarterly, 6 = semi-annual,
        12 = annual).

    Returns
    -------
    dict {month_idx: value} covering every month from the first period start
    through (last period start + period_length - 1). For each month m within
    period starting at s with k = period_length - (m - s) (k counts down from
    period_length to 1):
        if the NEXT period starts exactly `period_length` months later
        (i.e. consecutive/contiguous periods):
            value(m) = (k/L) * value(s) + (1 - k/L) * value(next_s)
        else (gap / last period):
            value(m) = value(s)   (carry forward)
    """
    L = period_length
    starts = sorted(k for k, v in values_by_idx.items() if pd.notna(v))
    out = {}
    for i, s in enumerate(starts):
        cur_val = values_by_idx[s]
        if i + 1 < len(starts) and starts[i + 1] - s == L:
            nxt_val = values_by_idx[starts[i + 1]]
            contiguous = True
        else:
            nxt_val = cur_val
            contiguous = False
        for k in range(L, 0, -1):
            m = s + (L - k)
            w = k / L
            out[m] = w * cur_val + (1 - w) * nxt_val if contiguous else cur_val
    return out


def interpolate_panel_column(df, country_col, idx_col, value_col, period_length, out_name=None):
    """
    Apply dovern_interpolate per country to a long dataframe with columns
    [country_col, idx_col, value_col]. Returns a long dataframe
    [country, month_idx, out_name].
    """
    out_name = out_name or value_col
    rows = []
    for country, g in df.groupby(country_col):
        values_by_idx = dict(zip(g[idx_col], g[value_col]))
        interp = dovern_interpolate(values_by_idx, period_length)
        for m, v in interp.items():
            rows.append({country_col: country, 'month_idx': m, out_name: v})
    return pd.DataFrame(rows)

# ── Burriel et al. (2024) eq.(1): fixed-horizon blend from the IMF WEO file ──
#
# The WEO historical Excel stores, per Spring (S) / Fall (F) vintage column,
# that vintage's forecasts for several target years. So a single vintage gives
# BOTH the current-calendar-year and next-calendar-year forecast — the two
# horizons Burriel's eq.(1) needs. We anchor at the vintage publication date
# (Spring=April, Fall=October): forecasts enter the panel when they come out.

WEO_SHEETS = {                       # output name -> Excel sheet / column suffix
    'gdp_growth':      'ngdp_rpch',
    'inflation':       'pcpi_pch',
    'current_account': 'bca_gdp_bp6',
}


def load_weo_two_horizon(path, iso3_to_iso2):
    """
    Read the IMF WEO historical Excel into a per-variable lookup
        lut[out_name] = {(country_iso2, vintage_idx, target_year): value}
    and the sorted list of all vintage month-indices. vintage_idx places Spring
    at April (month 3, 0-based) and Fall at October (month 9) of the vintage year.
    """
    lut = {}
    vintages = set()
    for out_name, sheet in WEO_SHEETS.items():
        df = pd.read_excel(path, sheet_name=sheet)
        df = df[df['ISOAlpha_3Code'].isin(iso3_to_iso2)].copy()
        df['country'] = df['ISOAlpha_3Code'].map(iso3_to_iso2)
        val_cols = [c for c in df.columns
                    if c[:1] in ('S', 'F') and c.endswith(sheet) and c[1:5].isdigit()]
        table = {}
        for c in val_cols:
            v_idx = int(c[1:5]) * 12 + (3 if c[0] == 'S' else 9)  # Apr / Oct
            vintages.add(v_idx)
            for country, ty, raw in df[['country', 'year', c]].itertuples(index=False):
                s = str(raw)
                if s in ('.', '', 'nan'):
                    continue
                try:
                    table[(country, v_idx, int(ty))] = float(raw)
                except ValueError:
                    pass
        lut[out_name] = table
    return lut, sorted(vintages)


def burriel_fixed_horizon(lut, var_name, out_name, start_idx, end_idx):
    """
    Monthly 12-month-ahead fixed-horizon forecast (Burriel et al. 2024, eq.1),
    anchored at vintage publication dates. For each month t the most recent
    vintage (<= t) supplies the current- and next-calendar-year forecasts, which
    are blended as
        y~_{t+12|t} = (k/12) * y^_currentyear + ((12-k)/12) * y^_nextyear
    with k = 12 - (t mod 12)  in {1,...,12}  (Jan -> 12, ..., Dec -> 1), matching
    Burriel's k-range. (For the alternative off-by-one convention use 11-(t%12).)
    Returns long df [country, month_idx, out_name].
    """
    table = lut[var_name]
    rows = []
    for country in sorted({c for (c, v, ty) in table}):
        cvint = sorted({v for (c, v, ty) in table if c == country})
        if not cvint:
            continue
        for t in range(max(start_idx, cvint[0]), end_idx + 1):
            v = None
            for vv in cvint:                 # most recent vintage <= t
                if vv <= t:
                    v = vv
                else:
                    break
            if v is None:
                continue
            cy = table.get((country, v, t // 12))        # current calendar year
            ny = table.get((country, v, t // 12 + 1))    # next calendar year
            if cy is None or ny is None:
                continue
            w = (12 - (t % 12)) / 12.0
            rows.append({'country': country, 'month_idx': t,
                         out_name: w * cy + (1 - w) * ny})
    return pd.DataFrame(rows, columns=['country', 'month_idx', out_name])

# ── Static / unchanged variable loaders (carried over from previous script) ─

def load_wide_monthly(path, date_col_candidates=('Date', 'date'), value_name='value',
                       country_cols=None):
    """Load a wide monthly CSV (date in rows, countries in columns) and melt
    to long [month_idx, country, value_name]."""
    if not os.path.exists(path):
        print(f"  [WARNING] {path} not found -> '{value_name}' will be all NaN.")
        return pd.DataFrame(columns=['country', 'month_idx', value_name])

    raw = pd.read_csv(path)
    raw.columns = raw.columns.str.strip()
    date_col = next((c for c in date_col_candidates if c in raw.columns), raw.columns[0])
    raw['month_idx'] = pd.to_datetime(raw[date_col], errors='coerce').dt.to_period('M') \
        .apply(lambda p: p.year * 12 + (p.month - 1))
    raw = raw.drop(columns=[date_col])

    if country_cols is None:
        country_cols = [c for c in raw.columns if c in NAME_TO_CODE.values()]

    long = (raw[['month_idx'] + country_cols]
            .melt(id_vars='month_idx', var_name='country', value_name=value_name)
            .dropna(subset=[value_name]))
    return long


# ── Configuration: input file paths ─────────────────────────────────────────

PATHS = {
    'forecasted_panel': 'data/variables/forecasted/forecast_vintage_panel.csv',
    'uncertainty':      'data/variables/policy_uncertainty_monthly.csv',
    'nexus':            'data/variables/bank_sovereign_nexus_monthly.csv',
    'bid_ask':          'data/variables/bid_ask_spreads_monthly.csv',
    # The WEO workbook is not shipped (10 MB; re-download from the IMF WEO
    # forecast archive); the script warns and leaves the three WEO-based
    # variables empty when it is absent.
    'weo_historical':   'data/variables/forecasted/weo_historical_forecasts.xlsx',
}


# ── 1. Static variables shared by all outputs ───────────────────────────────
print("Loading static variables (uncertainty, nexus, bid_ask) ...")

policy_unc = load_wide_monthly(PATHS['uncertainty'], value_name='policy_uncertainty')

if os.path.exists(PATHS['nexus']):
    nexus_raw = pd.read_csv(PATHS['nexus'])
    nexus_raw.columns = nexus_raw.columns.str.strip()
    nexus_raw['country'] = nexus_raw['country'].map(NAME_TO_CODE).fillna(nexus_raw['country'])
    nexus_raw = nexus_raw.dropna(subset=['country'])
    nexus_raw['month_idx'] = pd.to_datetime(nexus_raw['DATE']).dt.to_period('M') \
        .apply(lambda p: p.year * 12 + (p.month - 1))
    bank_nexus = nexus_raw[['country', 'month_idx', 'nexus']].rename(columns={'nexus': 'bank_nexus'})
else:
    print(f"  [WARNING] {PATHS['nexus']} not found -> 'bank_nexus' will be all NaN.")
    bank_nexus = pd.DataFrame(columns=['country', 'month_idx', 'bank_nexus'])

bid_ask = load_wide_monthly(PATHS['bid_ask'], value_name='bid_ask')


def merge_static(df):
    """Left-merge policy_uncertainty / bank_nexus / bid_ask onto [country, month_idx]."""
    df = df.merge(policy_unc, on=['country', 'month_idx'], how='left')
    df = df.merge(bank_nexus, on=['country', 'month_idx'], how='left')
    df = df.merge(bid_ask, on=['country', 'month_idx'], how='left')
    return df


FINAL_COLS = ['date', 'country', 'gdp_growth', 'inflation', 'debt_gdp', 'current_account',
              'policy_uncertainty', 'bank_nexus', 'bid_ask']


def finalize(df):
    df = df.copy()
    df['date'] = df['month_idx'].apply(idx_to_label)
    for c in FINAL_COLS:
        if c not in df.columns:
            df[c] = np.nan
    df = df[FINAL_COLS].sort_values(['date', 'country']).reset_index(drop=True)
    return df


# ── 2. FORECAST data — Burriel et al. (2024) fixed-horizon interpolation ──
print("\nLoading WEO historical forecasts ...")
if not os.path.exists(PATHS['weo_historical']):
    # Abort rather than overwrite the committed panel with a degraded one:
    # without the WEO workbook, gdp_growth/inflation/current_account would be
    # written out empty. Set ALLOW_DEGRADED=1 to force a partial run anyway.
    if os.environ.get('ALLOW_DEGRADED') != '1':
        raise SystemExit(
            f"ERROR: {PATHS['weo_historical']} not found (not committed for "
            f"size; see README 'Running the pipeline'). Aborting so the "
            f"committed macro_fundamentals_forecasted_interpolated.csv is "
            f"not overwritten with empty macro columns. "
            f"Set ALLOW_DEGRADED=1 to run anyway.")
    print(f"  [WARNING] {PATHS['weo_historical']} not found -> forecasted "
          f"gdp_growth/inflation/current_account will be empty.")
    fc_gdp = pd.DataFrame(columns=['country', 'month_idx', 'gdp_growth'])
    fc_cpi = pd.DataFrame(columns=['country', 'month_idx', 'inflation'])
    fc_ca  = pd.DataFrame(columns=['country', 'month_idx', 'current_account'])
else:
    weo_lut, weo_vint = load_weo_two_horizon(PATHS['weo_historical'], ISO3_TO_ISO2)
    weo_start, weo_end = min(weo_vint), max(weo_vint) + 12
    fc_gdp = burriel_fixed_horizon(weo_lut, 'gdp_growth',      'gdp_growth',      weo_start, weo_end)
    fc_cpi = burriel_fixed_horizon(weo_lut, 'inflation',       'inflation',       weo_start, weo_end)
    fc_ca  = burriel_fixed_horizon(weo_lut, 'current_account', 'current_account', weo_start, weo_end)
    print("  WEO two-horizon Burriel blend, vintage-date anchored "
          "(gdp_growth/inflation/current_account).")

# ── debt-to-GDP: no consistent two-year-ahead vintage forecast exists, so
# take the Dovern rolling weighted average of the one-year-ahead April and
# October vintage forecasts (period_length = 6) from forecast_vintage_panel.csv.
if os.path.exists(PATHS['forecasted_panel']):
    _fc = pd.read_csv(PATHS['forecasted_panel'])
    _fc.columns = _fc.columns.str.strip()
    _fc['country'] = _fc['iso3'].map(ISO3_TO_ISO2)
    _fc = _fc.dropna(subset=['country'])
    _fc['month_idx'] = _fc['vintage'].apply(vintage_to_idx)
    fc_debt = interpolate_panel_column(
        _fc[['country', 'month_idx', 'debt_gdp']].dropna(subset=['debt_gdp']),
        'country', 'month_idx', 'debt_gdp', 6, 'debt_gdp')
    print("  debt_gdp from forecast_vintage_panel.csv "
          "(next-year forecast, vintage-date anchored, period_length=6).")
else:
    print(f"  [WARNING] {PATHS['forecasted_panel']} not found -> debt_gdp empty.")
    fc_debt = pd.DataFrame(columns=['country', 'month_idx', 'debt_gdp'])

print("\nBuilding macro_fundamentals_forecasted_interpolated.csv ...")
df_fc = fc_gdp.merge(fc_cpi, on=['country', 'month_idx'], how='outer')
df_fc = df_fc.merge(fc_debt, on=['country', 'month_idx'], how='outer')
df_fc = df_fc.merge(fc_ca, on=['country', 'month_idx'], how='outer')
df_fc = merge_static(df_fc)

# policy_uncertainty / bank_nexus / bid_ask are ACTUAL series and don't extend
# into the forecast horizon -> forward-fill the last observed actual value
# per country.
df_fc = df_fc.sort_values(['country', 'month_idx'])
df_fc[['policy_uncertainty', 'bank_nexus', 'bid_ask']] = (
    df_fc.groupby('country')[['policy_uncertainty', 'bank_nexus', 'bid_ask']].ffill()
)

df_fc = finalize(df_fc)

out_path_fc = os.path.join(OUT_DIR, 'macro_fundamentals_forecasted_interpolated.csv')
df_fc.to_csv(out_path_fc, index=False)
print(f"  -> {out_path_fc}  shape={df_fc.shape}")
print(df_fc.isnull().sum().to_string())

print("\n✓ Done. macro_fundamentals_forecasted_interpolated.csv written to", OUT_DIR)