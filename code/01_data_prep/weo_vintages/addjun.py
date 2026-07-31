"""
Adds the missing 2005-Jun rows to weo_debt_gdp_vintages_long.csv.
These are the EO77 (Jun 2005) values for forecast year 2006.
"""

import pandas as pd

from pathlib import Path

LONG_CSV = (Path(__file__).resolve().parents[3]
            / 'data' / 'variables' / 'Forecasted' / 'weo_debt_gdp_vintages_long.csv')

# EO77 (Jun 2005) values for forecast year 2006 — from Annex Table 60
jun2005 = [
    ('AUT', 'Austria',      61.0),
    ('BEL', 'Belgium',      92.5),
    ('FIN', 'Finland',      48.4),
    ('FRA', 'France',       66.4),
    ('DEU', 'Germany',      68.7),
    ('GRC', 'Greece',      105.7),
    ('IRL', 'Ireland',      29.8),
    ('ITA', 'Italy',       109.1),
    ('NLD', 'Netherlands',  56.3),
    ('PRT', 'Portugal',     71.4),
    ('ESP', 'Spain',        43.5),
]

rows = [{
    'vintage':            '2005-Jun',
    'vintage_year':       2005,
    'vintage_month':      'Jun',
    'forecast_year':      2006,
    'iso3':               iso3,
    'country':            country,
    'subject_code':       'GGFLMQ',
    'subject_descriptor': 'Gross public debt, Maastricht criterion',
    'units':              'Percent of GDP',
    'debt_gdp_forecast':  val,
} for iso3, country, val in jun2005]

new_rows = pd.DataFrame(rows)

existing = pd.read_csv(LONG_CSV)

# Drop any existing 2005-Jun rows to avoid duplicates
existing = existing[existing['vintage'] != '2005-Jun']

combined = pd.concat([new_rows, existing], ignore_index=True)
combined.to_csv(LONG_CSV, index=False)

print(f"Added {len(new_rows)} 2005-Jun rows")
print(f"Total rows now: {len(combined)}")
print(f"\nFirst vintages:")
print(combined.groupby('vintage')['debt_gdp_forecast']
      .apply(lambda x: f"{x.notna().sum()}/{len(x)}").head(5).to_string())