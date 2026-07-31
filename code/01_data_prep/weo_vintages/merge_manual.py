"""
Merges manual.csv (OECD EO Maastricht debt) into weo_debt_gdp_vintages_long.csv.
Drops any existing rows whose vintage contains a year before 2010,
then adds clean OECD rows for 2005-Jun through 2010-Dec.

Usage: python merge_manual.py
"""

import pandas as pd
import re
from io import StringIO

from pathlib import Path

_FORECASTED = Path(__file__).resolve().parents[3] / 'data' / 'variables' / 'Forecasted'
MANUAL_CSV = _FORECASTED / 'manual.csv'
LONG_CSV   = _FORECASTED / 'weo_debt_gdp_vintages_long.csv'

VINTAGE_MAP = {
    2006: [('2005-Jun', 2005, 'Jun'), ('2005-Dec', 2005, 'Dec')],
    2007: [('2006-Jun', 2006, 'Jun'), ('2006-Dec', 2006, 'Dec')],
    2008: [('2007-Jun', 2007, 'Jun'), ('2007-Dec', 2007, 'Dec')],
    2009: [('2008-Jun', 2008, 'Jun'), ('2008-Dec', 2008, 'Dec')],
    2010: [('2009-Jun', 2009, 'Jun'), ('2009-Dec', 2009, 'Dec')],
    2011: [('2010-Jun', 2010, 'Jun'), ('2010-Dec', 2010, 'Dec')],
}

# ── Clean manual.csv ──────────────────────────────────────────────────────────
with open(MANUAL_CSV, 'r', encoding='utf-8') as f:
    lines = f.readlines()

clean = ['iso3,country,forecast_year,debt_gdp_forecast\n']
for line in lines[1:]:
    if (line.startswith('iso3') or 'Claude' in line
            or '\ue11d' in line or line.strip() == ''):
        continue
    clean.append(line)

manual = pd.read_csv(StringIO(''.join(clean)))
manual = manual[pd.to_numeric(manual['forecast_year'], errors='coerce').notna()].copy()
manual['forecast_year']     = manual['forecast_year'].astype(int)
manual['debt_gdp_forecast'] = pd.to_numeric(manual['debt_gdp_forecast'], errors='coerce')
manual['_rank'] = manual.groupby(['forecast_year', 'iso3']).cumcount()

# ── Build OECD rows ───────────────────────────────────────────────────────────
rows = []
for _, row in manual.iterrows():
    fy   = row['forecast_year']
    rank = row['_rank']
    v_label, v_year, v_month = VINTAGE_MAP[fy][rank]
    rows.append({
        'vintage':            v_label,
        'vintage_year':       v_year,
        'vintage_month':      v_month,
        'forecast_year':      fy,
        'iso3':               row['iso3'],
        'country':            row['country'],
        'subject_code':       'GGFLMQ',
        'subject_descriptor': 'Gross public debt, Maastricht criterion',
        'units':              'Percent of GDP',
        'debt_gdp_forecast':  row['debt_gdp_forecast'],
    })

oecd = pd.DataFrame(rows)
print(f"OECD rows built: {len(oecd)}")

# ── Load existing long CSV ────────────────────────────────────────────────────
weo = pd.read_csv(LONG_CSV)
print(f"Existing rows before cleaning: {len(weo)}")

# Drop any row whose vintage year (extracted from the vintage string) is < 2010
# This handles both integer and string vintage_year columns, and any format
def vintage_year_from_label(v):
    m = re.match(r'(\d{4})', str(v))
    return int(m.group(1)) if m else 9999

weo['_vy'] = weo['vintage'].apply(vintage_year_from_label)
weo_keep   = weo[weo['_vy'] >= 2010].drop(columns='_vy')
print(f"Dropped {len(weo) - len(weo_keep)} rows with vintage year < 2010")
print(f"Keeping {len(weo_keep)} rows")

# ── Combine and save ──────────────────────────────────────────────────────────
combined = pd.concat([oecd, weo_keep], ignore_index=True)
combined = combined.sort_values(
    ['forecast_year', 'vintage_year', 'vintage_month', 'iso3']
).reset_index(drop=True)

combined.to_csv(LONG_CSV, index=False)

print(f"\nSaved {LONG_CSV}: {len(combined)} rows total")
print(f"\nCoverage per vintage:")
print(combined.groupby('vintage')['debt_gdp_forecast']
      .apply(lambda x: f"{x.notna().sum()}/{len(x)}").to_string())