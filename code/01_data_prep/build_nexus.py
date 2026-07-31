import pandas as pd
import os

countries = [
    "Austria", "Belgium", "Finland", "France", "Germany",
    "Greece", "Ireland", "Italy", "Netherlands", "Portugal", "Spain",
]

ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..'))
RAW  = os.path.join(ROOT, 'data', 'raw', 'sov_banknexus')

dfs = []
for country in countries:
    # Holdings
    h = pd.read_csv(os.path.join(RAW, f"{country.lower()}_gov_debt_holdings_raw.csv"))
    h = h.rename(columns={h.columns[2]: "holdings"})

    # Total assets
    a = pd.read_csv(os.path.join(RAW, f"{country.lower()}_mfi_total_assets_raw.csv"))
    a = a.rename(columns={a.columns[2]: "total_assets"})

    # Merge on date
    df = h.merge(a[["DATE", "total_assets"]], on="DATE", how="inner")
    df["nexus"] = df["holdings"] / df["total_assets"]
    df["country"] = country

    dfs.append(df)

nexus = pd.concat(dfs, ignore_index=True)
nexus = nexus[["country", "DATE", "TIME PERIOD", "holdings", "total_assets", "nexus"]]
nexus.to_csv(os.path.join(ROOT, 'data', 'variables', 'bank_sovereign_nexus_monthly.csv'), index=False)
print(f"Created bank_sovereign_nexus_monthly.csv with {len(nexus):,} rows")