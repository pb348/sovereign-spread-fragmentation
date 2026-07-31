"""
Extract 1-year-ahead forecasts from WEOhistorical-2.xlsx for 11 EMU countries.

Structure of the file:
  - Rows: (country, target_year) — each country has rows for years 1988-2031
  - Columns: S{YYYY}var and F{YYYY}var = Spring / Fall vintage of that year
  - 1-year-ahead forecast for target year T = column S{T-1} or F{T-1}

Output: long CSV matching weo_debt_gdp_vintages_long.csv format with columns:
  vintage, vintage_year, vintage_month, forecast_year,
  iso3, country, subject_code, subject_descriptor, units, value

Run: python extract_weo_historical.py
"""

import pandas as pd
import numpy as np

from pathlib import Path

_ROOT  = Path(__file__).resolve().parents[3]
# Workbook not shipped (10 MB); download WEOhistorical.xlsx from the IMF
# WEO forecast archive and place it at data/raw/WEOhistorical-2.xlsx.
INPUT  = _ROOT / 'data' / 'raw' / 'WEOhistorical-2.xlsx'
OUTPUT = _ROOT / 'data' / 'variables' / 'forecasted' / 'weo_historical_forecasts_long.csv'

EMU_ISO = {
    'AUT': 'Austria',     'BEL': 'Belgium',     'FIN': 'Finland',
    'FRA': 'France',      'DEU': 'Germany',      'GRC': 'Greece',
    'IRL': 'Ireland',     'ITA': 'Italy',        'LUX': 'Luxembourg',
    'NLD': 'Netherlands', 'PRT': 'Portugal',     'ESP': 'Spain',
}

SHEETS = {
    'ngdp_rpch':   ('NGDP_RPCH',   'Gross domestic product, constant prices',    'Annual percent change'),
    'pcpi_pch':    ('PCPI_PCH',    'Inflation, average consumer prices',         'Annual percent change'),
    'bca_gdp_bp6': ('BCA_GDP_BP6', 'Current account balance',                    'Percent of GDP'),
}

def extract_sheet(sheet_name, subject_code, subject_descriptor, units,
                  start_year=1990, end_year=2026):
    """
    For each (country, target_year), pull the 1-year-ahead Spring and Fall forecasts.
    Spring vintage year = target_year - 1, column = S{vintage_year}{sheet_name}
    Fall   vintage year = target_year - 1, column = F{vintage_year}{sheet_name}
    """
    df = pd.read_excel(INPUT, sheet_name=sheet_name)

    # Filter to EMU countries only
    df = df[df['ISOAlpha_3Code'].isin(EMU_ISO.keys())].copy()

    records = []
    for target_year in range(start_year, end_year + 1):
        vintage_year = target_year - 1
        s_col = f'S{vintage_year}{sheet_name}'
        f_col = f'F{vintage_year}{sheet_name}'

        year_rows = df[df['year'] == target_year]
        if year_rows.empty:
            continue

        for _, row in year_rows.iterrows():
            iso3    = row['ISOAlpha_3Code']
            country = EMU_ISO[iso3]

            for col, month, v_label in [
                (s_col, 'Apr', f'{vintage_year}-Apr'),
                (f_col, 'Oct', f'{vintage_year}-Oct'),
            ]:
                if col not in df.columns:
                    continue
                raw = row.get(col, np.nan)
                value = np.nan if str(raw) in ('.', '', 'nan') else float(raw)

                records.append({
                    'vintage':            v_label,
                    'vintage_year':       vintage_year,
                    'vintage_month':      month,
                    'forecast_year':      target_year,
                    'iso3':               iso3,
                    'country':            country,
                    'subject_code':       subject_code,
                    'subject_descriptor': subject_descriptor,
                    'units':              units,
                    'value':              value,
                })

    result = pd.DataFrame(records)
    print(f"  {sheet_name}: {len(result)} rows, "
          f"{result['value'].notna().sum()} non-missing")
    return result


if __name__ == '__main__':
    print(f"Reading {INPUT}...")
    frames = []

    for sheet_name, (code, desc, units) in SHEETS.items():
        print(f"\nExtracting {sheet_name}...")
        frame = extract_sheet(sheet_name, code, desc, units)
        frames.append(frame)

    panel = pd.concat(frames, ignore_index=True)
    panel = panel.sort_values(
        ['subject_code', 'forecast_year', 'vintage', 'iso3']
    ).reset_index(drop=True)

    panel.to_csv(OUTPUT, index=False)

    print(f"\nSaved {OUTPUT}: {len(panel)} rows")
    print(f"\nCoverage by variable and vintage (non-missing/total):")
    print(panel.groupby(['subject_code', 'vintage'])['value']
          .apply(lambda x: f"{x.notna().sum()}/{len(x)}")
          .head(24).to_string())

    print(f"\nSample (DEU GDP growth):")
    sample = panel[
        (panel['iso3'] == 'DEU') &
        (panel['subject_code'] == 'NGDP_RPCH') &
        (panel['forecast_year'].between(2005, 2010))
    ][['vintage','forecast_year','iso3','subject_code','value']]
    print(sample.to_string(index=False))