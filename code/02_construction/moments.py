"""
moments.py  —  Compute cross-sectional moments of sovereign spreads and macro fundamentals
──────────────────────────────────────────────────────────────────────────────────────────
Reads the panel produced by macro_fundamentals.py and computes cross-sectional
(across-country) standard deviation, skewness and kurtosis for every macro
variable, plus the same moments for sovereign spreads and the (static) VSTOXX
series.

Inputs:
    data/variables/sovereign_spreads.csv
    data/processed/macro_fundamentals_forecasted_interpolated.csv
    data/variables/vstoxx_monthly.csv   (static — not regenerated)

Output:
    data/processed/moments_forecasted_interpolated.csv

Countries: AT, BE, FI, FR, DE, GR, IE, IT, NL, PT, ES  (11 EA countries, no Luxembourg)
"""

import os
import numpy as np
import pandas as pd
from scipy import stats

# Work from the repository root so all paths below are repo-relative
os.chdir(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..'))

# ── Config ───────────────────────────────────────────────────────────────────
COUNTRIES = ['AT', 'BE', 'FI', 'FR', 'DE', 'GR', 'IE', 'IT', 'NL', 'PT', 'ES']

# Variables in the fundamentals panel (policy_uncertainty/bank_nexus/bid_ask
# are carried over unchanged from actual data)
MACRO_VARS_FORECAST = ['gdp_growth', 'inflation', 'debt_gdp', 'current_account',
                        'policy_uncertainty', 'bank_nexus', 'bid_ask']

# EXCLUDE_AT_BID_ASK: if True, Austria's bid_ask observations in the window
# [EXCLUDE_AT_BID_ASK_START, EXCLUDE_AT_BID_ASK_END] are set to NaN before
# computing cross-sectional moments, so bid_ask_stdev/skew/kurt are computed
# over the remaining 10 countries in those months. Rationale: the AT bid-ask
# series shows an isolated, box-shaped level jump around 2020 (no other
# country moves), consistent with a benchmark-bond roll or quoting-source
# change rather than genuine liquidity stress. Only bid_ask is affected;
# all other AT variables are untouched.
EXCLUDE_AT_BID_ASK       = False
EXCLUDE_AT_BID_ASK_START = '2020-01'
EXCLUDE_AT_BID_ASK_END   = '2021-06'

INPUT_DIR  = 'data/processed'
OUTPUT_DIR = 'data/processed'

FILES = {
    'forecasted_interpolated': (os.path.join(INPUT_DIR, 'macro_fundamentals_forecasted_interpolated.csv'), MACRO_VARS_FORECAST),
}


# ── Moment functions (cross-sectional, i.e. across countries at time t) ──────
def xstdev(row):
    v = row.dropna()
    return v.std(ddof=1) if len(v) >= 3 else np.nan

def xskew(row):
    v = row.dropna()
    return float(stats.skew(v, bias=False)) if len(v) >= 3 else np.nan

def xkurt(row):
    v = row.dropna()
    return float(stats.kurtosis(v, bias=False, fisher=False)) if len(v) >= 3 else np.nan

MOMENT_FUNCS = {'stdev': xstdev, 'skew': xskew, 'kurt': xkurt}


# ── 1. Load shared series: sovereign spreads + VSTOXX ────────────────────────
print("Loading shared series (spreads, vstoxx) ...")

spreads = pd.read_csv('data/variables/sovereign_spreads.csv').rename(columns={'Unnamed: 0': 'date'})
spreads['date'] = pd.to_datetime(spreads['date'], format='%Y-%m')
spreads = spreads.set_index('date')
# Restrict to EA11 — excludes Luxembourg explicitly
spreads = spreads[[c for c in COUNTRIES if c in spreads.columns]]
print(f"  Spreads:  {spreads.index[0].strftime('%Y-%m')} – {spreads.index[-1].strftime('%Y-%m')}"
      f"  ({spreads.shape[1]} countries)")

spread_mom = pd.DataFrame(index=spreads.index)
for mname, func in MOMENT_FUNCS.items():
    spread_mom[f'spread_{mname}'] = spreads.apply(func, axis=1)

vstoxx_raw = pd.read_csv('data/variables/vstoxx_monthly.csv')
vstoxx_raw.columns = vstoxx_raw.columns.str.strip().str.lower()
date_col_v = vstoxx_raw.columns[0]
val_col_v = next(
    c for c in vstoxx_raw.columns[1:]
    if pd.to_numeric(vstoxx_raw[c], errors='coerce').notna().any()
)
vstoxx_raw[date_col_v] = pd.to_datetime(vstoxx_raw[date_col_v], errors='coerce')
vstoxx_raw[val_col_v]  = pd.to_numeric(vstoxx_raw[val_col_v], errors='coerce')
vstoxx = (vstoxx_raw
          .dropna(subset=[date_col_v, val_col_v])
          .set_index(date_col_v)[val_col_v])
vstoxx.index = vstoxx.index.to_period('M').to_timestamp()
print(f"  VSTOXX:   {vstoxx.index[0].strftime('%Y-%m')} – {vstoxx.index[-1].strftime('%Y-%m')}"
      f"  non-null={vstoxx.notna().sum()}")


# ── 2. Per-file: load macro fundamentals, compute moments, merge, save ──────
os.makedirs(OUTPUT_DIR, exist_ok=True)

for label, (path, macro_vars) in FILES.items():
    print(f"\nProcessing {label} ...")
    if not os.path.exists(path):
        print(f"  [WARNING] {path} not found - skipping. "
              f"Run macro_fundamentals.py first.")
        continue

    mf_long = pd.read_csv(path)
    mf_long['date'] = pd.to_datetime(mf_long['date'], format='%Y-%m')

    # Safety filter: keep only EA11 countries (no Luxembourg, no stragglers)
    mf_long = mf_long[mf_long['country'].isin(COUNTRIES)]

    # Optional exclusion: AT bid_ask around the 2020 level-jump artifact
    if EXCLUDE_AT_BID_ASK and 'bid_ask' in mf_long.columns:
        excl_mask = (
            (mf_long['country'] == 'AT') &
            (mf_long['date'] >= pd.to_datetime(EXCLUDE_AT_BID_ASK_START, format='%Y-%m')) &
            (mf_long['date'] <= pd.to_datetime(EXCLUDE_AT_BID_ASK_END,   format='%Y-%m'))
        )
        n_excl = int(excl_mask.sum())
        mf_long.loc[excl_mask, 'bid_ask'] = np.nan
        print(f"  AT bid_ask exclusion active ({EXCLUDE_AT_BID_ASK_START} to "
              f"{EXCLUDE_AT_BID_ASK_END}): {n_excl} observation(s) set to NaN")

    # A column that exists but is entirely NaN (e.g. the WEO workbook was not
    # available when macro_fundamentals.py ran) is skipped like a missing one.
    available_vars = [v for v in macro_vars
                      if v in mf_long.columns and mf_long[v].notna().any()]
    missing_vars   = [v for v in macro_vars if v not in available_vars]
    if missing_vars:
        print(f"  [WARNING] columns missing or empty, skipped: {missing_vars}")

    macro_wide = mf_long.pivot_table(index='date', columns='country', values=available_vars)

    macro_mom = pd.DataFrame(index=macro_wide.index)
    for var in available_vars:
        var_data = macro_wide[var][[c for c in COUNTRIES if c in macro_wide[var].columns]]
        for mname, func in MOMENT_FUNCS.items():
            macro_mom[f'{var}_{mname}'] = var_data.apply(func, axis=1)

    df = spread_mom.join(macro_mom, how='outer').join(vstoxx.rename('vstoxx'))
    df = df.sort_index()

    print("  Null counts in analysis frame:")
    print(df.isnull().sum().to_string())

    df.index.name = 'date'
    out_path = os.path.join(OUTPUT_DIR, f'moments_{label}.csv')
    df.reset_index().to_csv(out_path, index=False)
    print(f"  -> ✓ {out_path}  ({len(df)} months x {len(df.columns)} columns)")

print("\n✓ All moments files written.")