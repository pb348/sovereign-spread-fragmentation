#!/usr/bin/env python3
"""
build_debt_gdp_manual.py
========================================================================
Assemble a patchwork debt-to-GDP forecast panel (OCTOBER vintages only)
for the Burriel-style fixed-horizon method, from heterogeneous IMF WEO
vintage files that live in the SAME folder as this script:

  * IMF WEO bulk release files (tab-delimited), October vintages only:
      - .xls  e.g.  weooct2020all.xls            (UTF-16 or Latin-1)
      - .tsv  e.g.  weo_2020_2.tsv   (_2 = Fall / October vintage)
    When the same October vintage exists as both .xls and .tsv, the .xls
    is preferred (your Excel vintages run from 2020 onward, the TSVs
    cover Oct-2010 .. 2019).
  * Optional hand-filled rows for years the bulk files do not cover
    (e.g. the Oct-2005 .. Oct-2009 vintages) and for any values you want
    to override, via MANUAL_SEED (same columns as the output).

Variable extracted: GGXWDG_NGDP  (general government gross debt, % of GDP).

Output: debt_gdp_oct_vintages.csv   columns:
    iso3, country, vintage, forecast_year, debt_gdp
one row per (country, October vintage, forecast-horizon year). The format
mirrors forecast_vintage_panel.csv so it plugs into the same machinery.

Notes
-----
* April vintages are skipped (October only). The April-2020 WEO omits
  gross debt anyway.
* Greece: the IMF suspended debt projections during the programme years,
  so some October vintages carry no Greek value -> left as NaN (a gap),
  by design. Fill by hand via MANUAL_SEED if/when you source them.
* 2005-2009: no bulk files -> add those October vintages by hand in
  MANUAL_SEED.
"""
import os
import re
import glob
import numpy as np
import pandas as pd

# ----------------------------- config -----------------------------
SCRIPT_DIR  = os.path.dirname(os.path.abspath(__file__))
_ROOT       = os.path.abspath(os.path.join(SCRIPT_DIR, '..', '..', '..'))
# Raw October-vintage WEO files are not shipped; re-download them into
# data/raw/weo_vintages/ (or set DEBT_SRC to point elsewhere).
SOURCE_DIR  = os.environ.get('DEBT_SRC') or os.path.join(_ROOT, 'data', 'raw', 'weo_vintages')
OUTPUT      = os.path.join(_ROOT, 'data', 'variables', 'forecasted', 'debt_gdp_oct_vintages.csv')
MANUAL_SEED = os.path.join(_ROOT, 'data', 'variables', 'forecasted',
                           'debt_gdp_manual_seed.csv')  # optional hand-filled rows
DEBT_CODES  = ['GGXWDG_NGDP', 'GGD_NGDP']         # new (>=Oct2010) then old (Oct2007-Apr2010)
HORIZON     = 2                                    # keep vintage_year, +1, +2 (Oct-only Burriel)

EMU_ISO3 = {
    'AUT': 'Austria', 'BEL': 'Belgium', 'FIN': 'Finland', 'FRA': 'France',
    'DEU': 'Germany', 'GRC': 'Greece',  'IRL': 'Ireland', 'ITA': 'Italy',
    'NLD': 'Netherlands', 'PRT': 'Portugal', 'ESP': 'Spain',
}


def read_weo_table(path):
    """Robustly read a tab-delimited WEO release (UTF-16 / Latin-1 / cp1252)."""
    for enc in ('utf-16', 'utf-16-le', 'cp1252', 'latin-1'):
        try:
            df = pd.read_csv(path, sep='\t', encoding=enc, low_memory=False)
        except Exception:
            continue
        if 'WEO Subject Code' in df.columns and df.shape[1] > 5:
            return df
    raise RuntimeError(f'Could not parse {path}')


def october_vintages(source_dir):
    """Discover ALL October vintage files (.xls and .tsv). Both formats are kept
    when present for the same year: some bulk exports are partial (missing some
    countries), so we read both and fill gaps from whichever has the value."""
    found = []   # (vintage_year, path, kind)
    for p in glob.glob(os.path.join(source_dir, '*')):
        name = os.path.basename(p).lower()
        m = re.match(r'weooct(\d{4})all\.xls$', name)
        if m:
            found.append((int(m.group(1)), p, 'xls'))
            continue
        m = re.match(r'weo_(\d{4})_2\.tsv$', name)   # _2 = Fall / October
        if m:
            found.append((int(m.group(1)), p, 'tsv'))
    return sorted(found)


def extract_debt(df, vintage_year):
    """Long rows for one October vintage: forecast years vintage_year .. +HORIZON."""
    code = next((c for c in DEBT_CODES
                 if not df[df['WEO Subject Code'] == c].empty), DEBT_CODES[0])
    d = df[df['WEO Subject Code'] == code]
    d = d[d['ISO'].isin(EMU_ISO3)]
    year_cols = {str(c).strip() for c in df.columns if str(c).strip().isdigit()}
    want = [y for y in range(vintage_year, vintage_year + HORIZON + 1) if str(y) in year_cols]
    rows = []
    for _, r in d.iterrows():
        iso3 = r['ISO']
        for y in want:
            raw = r[str(y)]
            val = pd.to_numeric(str(raw).replace(',', '').strip(), errors='coerce') \
                if pd.notna(raw) else np.nan
            rows.append({'iso3': iso3, 'country': EMU_ISO3[iso3],
                         'vintage': f'{vintage_year}-Oct', 'forecast_year': y,
                         'debt_gdp': val})
    return rows


def main():
    vints = october_vintages(SOURCE_DIR)
    print(f'Found {len(vints)} October vintage file(s) in {SOURCE_DIR!r}:')
    rows = []
    for vyear, path, kind in vints:
        df = read_weo_table(path)
        vr = extract_debt(df, vyear)
        ng = sum(pd.notna(r['debt_gdp']) for r in vr)
        print(f'  {vyear}-Oct  [{kind}]  {os.path.basename(path):24s} -> {ng:3d}/{len(vr)} values')
        rows.extend(vr)

    out = pd.DataFrame(rows, columns=['iso3', 'country', 'vintage', 'forecast_year', 'debt_gdp'])

    if os.path.exists(MANUAL_SEED):
        seed = pd.read_csv(MANUAL_SEED)
        out = pd.concat([out, seed], ignore_index=True)
        print(f'  + merged manual seed {MANUAL_SEED}: {len(seed)} rows')

    # de-duplicate on (country, vintage, horizon); keep a non-null value when available
    out['_nn'] = out['debt_gdp'].notna()
    out = (out.sort_values(['iso3', 'vintage', 'forecast_year', '_nn'])
              .drop_duplicates(['iso3', 'vintage', 'forecast_year'], keep='last')
              .drop(columns='_nn')
              .sort_values(['iso3', 'vintage', 'forecast_year'])
              .reset_index(drop=True))

    out.to_csv(OUTPUT, index=False)
    print(f"\nWrote {OUTPUT}: {len(out)} rows | "
          f"{out['debt_gdp'].notna().sum()} non-missing | "
          f"{out['vintage'].nunique()} vintages | {out['iso3'].nunique()} countries.")
    gaps = out[out['debt_gdp'].isna()]
    if not gaps.empty:
        print('Gaps (NaN) by country:')
        print(gaps.groupby('iso3')['forecast_year'].count().to_string())


if __name__ == '__main__':
    main()