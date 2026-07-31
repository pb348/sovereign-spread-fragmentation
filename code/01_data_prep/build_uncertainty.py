"""
build_uncertainty.py — assemble the monthly Economic Policy Uncertainty panel.

Combines the country EPU indices used in the thesis into one wide monthly
panel, data/variables/uncertainty.csv (columns: Date, DE, ES, FR, GR, IE, IT,
BE, NL, PT — Austria and Finland have no published EPU index).

Sources (all from policyuncertainty.com and the papers cited therein; the
workbooks are committed in data/raw/):
    All_Country_Data.xlsx                    -> DE, ES, FR, GR, IE, IT
                                                (Baker, Bloom & Davis 2016)
    EPU_Belgium_data.xlsx                    -> BE  ("EPU Belgium",
                                                Algaba et al. 2020)
    Netherlands_Policy_Uncertainty_Data.xlsx -> NL  ("EBO-NL Index",
                                                Kroese, Kok & Parlevliet 2015)
    Portugal_epuptindex_data.xlsx            -> PT  ("epu_pt", Morão 2024)
"""

import pandas as pd
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
RAW = ROOT / 'data' / 'raw'
OUTPUT = ROOT / 'data' / 'variables' / 'uncertainty.csv'

# ── All-country file: Year/Month rows, one column per country ────────────────
ALL_COUNTRY_COLS = {
    'Germany': 'DE', 'Spain': 'ES', 'France': 'FR',
    'Greece': 'GR', 'Ireland': 'IE', 'Italy': 'IT',
}

ac = pd.read_excel(RAW / 'All_Country_Data.xlsx')
ac = ac.dropna(subset=['Year', 'Month'])          # trailing footnote rows
ac['Date'] = (ac['Year'].astype(int).astype(str) + '-'
              + ac['Month'].astype(int).astype(str).str.zfill(2))
panel = (ac.set_index('Date')[list(ALL_COUNTRY_COLS)]
           .rename(columns=ALL_COUNTRY_COLS)
           .apply(pd.to_numeric, errors='coerce'))


def monthly_series(path, date_col, value_col, iso2, date_format=None):
    """Read one country workbook -> Series indexed by 'YYYY-MM'."""
    df = pd.read_excel(path)
    dates = pd.to_datetime(df[date_col], format=date_format, errors='coerce')
    s = pd.Series(pd.to_numeric(df[value_col], errors='coerce').values,
                  index=dates.dt.strftime('%Y-%m'), name=iso2)
    return s[s.index.notna()].dropna()


# ── Country-specific files ───────────────────────────────────────────────────
be = monthly_series(RAW / 'EPU_Belgium_data.xlsx', 'date', 'EPU Belgium', 'BE')
nl = monthly_series(RAW / 'Netherlands_Policy_Uncertainty_Data.xlsx',
                    'Unnamed: 0', 'EBO-NL Index**', 'NL')
pt = monthly_series(RAW / 'Portugal_epuptindex_data.xlsx',
                    'Unnamed: 0', 'epu_pt', 'PT', date_format='%Ym%m')

panel = panel.join([be, nl, pt], how='outer').sort_index()
panel = panel.dropna(how='all')
panel.index.name = 'Date'

panel.to_csv(OUTPUT)
print(f"Saved {OUTPUT}: {len(panel)} months x {panel.shape[1]} countries")
print(f"Range: {panel.index.min()} - {panel.index.max()}")
print(panel.notna().sum().to_string())
