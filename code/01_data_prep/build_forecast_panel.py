"""
Combines weo_debt_gdp_vintages_long.csv and weo_historical_forecasts_long.csv
into a wide panel.

OECD debt vintages use Jun/Dec, IMF uses Apr/Oct.
We map Jun→Apr and Dec→Oct so all vintages align on the same semi-annual key.
Where both sources provide debt for the same (iso3, vintage, forecast_year),
we prefer the IMF value (it covers 2010 onwards with full country coverage).
"""

import pandas as pd
import numpy as np
from pathlib import Path

_FORECASTED = Path(__file__).resolve().parents[2] / 'data' / 'variables' / 'Forecasted'
DEBT_FILE = _FORECASTED / 'weo_debt_gdp_vintages_long.csv'
HIST_FILE = _FORECASTED / 'weo_historical_forecasts_long.csv'
OUTPUT    = _FORECASTED / 'forecasted_panel.csv'   # read by macro_fundamentals.py

# ── Load ──────────────────────────────────────────────────────────────────────
debt = pd.read_csv(DEBT_FILE)
hist = pd.read_csv(HIST_FILE)

# ── Normalise debt vintages: Jun→Apr, Dec→Oct ─────────────────────────────────
def normalise_vintage(v):
    return v.replace('-Jun', '-Apr').replace('-Dec', '-Oct')

debt['vintage'] = debt['vintage'].apply(normalise_vintage)

# Where duplicates arise (e.g. both 2010-Apr OECD and 2010-Apr IMF),
# keep the IMF value (GGXWDG_NGDP) over OECD (GGFLMQ)
# Sort so IMF rows come last, then drop_duplicates keeping last
debt = debt.sort_values('subject_code')  # GGFLMQ < GGXWDG_NGDP alphabetically
debt = debt.drop_duplicates(
    subset=['vintage', 'forecast_year', 'iso3'], keep='last'
)

debt_slim = debt[['vintage','forecast_year','iso3','debt_gdp_forecast']].copy()
debt_slim = debt_slim.rename(columns={'debt_gdp_forecast': 'debt_gdp'})

print(f"Debt (after normalise): {len(debt_slim)} rows, "
      f"{debt_slim['vintage'].nunique()} vintages")

# ── Pivot hist to wide ────────────────────────────────────────────────────────
hist_wide = hist.pivot_table(
    index=['vintage','forecast_year','iso3','country'],
    columns='subject_code',
    values='value',
    aggfunc='first'
).reset_index()
hist_wide.columns.name = None
hist_wide = hist_wide.rename(columns={
    'NGDP_RPCH':   'gdp_growth',
    'PCPI_PCH':    'cpi',
    'BCA_GDP_BP6': 'current_account',
})

print(f"Hist wide: {len(hist_wide)} rows, {hist_wide['vintage'].nunique()} vintages")

# ── Merge on vintage + forecast_year + iso3 ───────────────────────────────────
panel = pd.merge(
    hist_wide,
    debt_slim,
    on=['vintage','forecast_year','iso3'],
    how='outer'
)

# Fill country name from hist side where missing
country_map = hist[['iso3','country']].drop_duplicates().set_index('iso3')['country']
panel['country'] = panel['country'].fillna(panel['iso3'].map(country_map))

# ── Sort ──────────────────────────────────────────────────────────────────────
month_order = {'Jan':1,'Feb':2,'Mar':3,'Apr':4,'May':5,'Jun':6,
               'Jul':7,'Aug':8,'Sep':9,'Oct':10,'Nov':11,'Dec':12}
panel['_vy'] = panel['vintage'].str[:4].astype(int)
panel['_vm'] = panel['vintage'].str[5:].map(month_order).fillna(0).astype(int)
panel = (panel
         .sort_values(['iso3','_vy','_vm','forecast_year'])
         .drop(columns=['_vy','_vm'])
         .reset_index(drop=True))

panel = panel[['iso3','country','vintage','forecast_year',
               'gdp_growth','cpi','current_account','debt_gdp']]

panel.to_csv(OUTPUT, index=False)

print(f"\nSaved {OUTPUT}: {len(panel)} rows, {panel['vintage'].nunique()} vintages")
print(f"forecast_year: {int(panel['forecast_year'].min())} – {int(panel['forecast_year'].max())}")
print(f"\nNon-missing:")
for col in ['gdp_growth','cpi','current_account','debt_gdp']:
    n = panel[col].notna().sum()
    print(f"  {col}: {n}/{len(panel)}")

print(f"\nSample (ESP 2007-2009):")
print(panel[
    (panel['iso3']=='ESP') &
    (panel['forecast_year'].between(2008,2010))
].to_string(index=False))