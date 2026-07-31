"""
correlation_table.py — correlation matrix of the dispersion regressors.

Pearson correlations over the estimation sample (2005:09-2025:12) between the
cross-sectional standard deviations of the seven macro-fundamentals and the
VSTOXX level (the Stage 1 regressors).

Output: output/tables/correlation_matrix.csv
"""

from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parents[2]
START, END = "2005-09", "2025-12"

REGRESSORS = ["gdp_growth_stdev", "inflation_stdev", "debt_gdp_stdev",
              "current_account_stdev", "policy_uncertainty_stdev",
              "bank_nexus_stdev", "bid_ask_stdev", "vstoxx"]
LABELS = {"gdp_growth_stdev": "GDP growth", "inflation_stdev": "Inflation",
          "debt_gdp_stdev": "Debt/GDP", "current_account_stdev": "Current account",
          "policy_uncertainty_stdev": "Policy uncertainty",
          "bank_nexus_stdev": "Bank nexus", "bid_ask_stdev": "Bid-ask",
          "vstoxx": "VSTOXX"}

# moments_forecasted_interpolated.csv already includes the vstoxx level
df = pd.read_csv(ROOT / "data" / "processed" /
                 "moments_forecasted_interpolated.csv")
df["date"] = df["date"].str[:7]               # YYYY-MM-01 -> YYYY-MM
df = df[(df["date"] >= START) & (df["date"] <= END)]

corr = (df[REGRESSORS].corr(method="pearson")
        .round(3)
        .rename(index=LABELS, columns=LABELS))

out = ROOT / "output" / "tables" / "correlation_matrix.csv"
out.parent.mkdir(parents=True, exist_ok=True)
corr.to_csv(out)
print(f"Saved {out} ({len(df)} months, {len(REGRESSORS)} regressors)\n")
print(corr.to_string())
