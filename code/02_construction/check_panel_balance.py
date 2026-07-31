"""
check_panel_balance.py — panel-balance and missing-data check.

Confirms that the estimation dataset covers all 11 countries over the full
2005:09-2025:12 sample:
  1. macro_fundamentals_forecasted_interpolated.csv — one row per
     (month, country); reports missing country-months and per-variable NaNs.
  2. moments_forecasted_interpolated.csv — one row per month; reports NaNs in
     the dependent variable and the dispersion regressors.
  3. vstoxx_monthly.csv — full monthly coverage of the sample window.

Exits non-zero only on structural failures (missing rows/countries); expected
leading NaNs (e.g. bid-ask needs a 6-month estimation window) are reported
but do not fail the check.
"""

import sys
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parents[2]
START, END = "2005-09", "2025-12"
COUNTRIES = ["AT", "BE", "DE", "ES", "FI", "FR", "GR", "IE", "IT", "NL", "PT"]
MF_VARS = ["gdp_growth", "inflation", "debt_gdp", "current_account",
           "policy_uncertainty", "bank_nexus", "bid_ask"]

months = pd.period_range(START, END, freq="M").strftime("%Y-%m")
failures = []

print(f"Panel-balance check: {len(COUNTRIES)} countries x "
      f"{len(months)} months ({START}..{END})\n")

# ── 1. Country-level fundamentals panel ──────────────────────────────────────
mf = pd.read_csv(ROOT / "data" / "processed" /
                 "macro_fundamentals_forecasted_interpolated.csv")
mf_win = mf[mf["date"].isin(months)]

expected = {(m, c) for m in months for c in COUNTRIES}
present = set(zip(mf_win["date"], mf_win["country"]))
missing_rows = expected - present
print(f"macro_fundamentals: {len(mf_win)} rows in window "
      f"({len(expected)} expected)")
if missing_rows:
    failures.append(f"{len(missing_rows)} missing country-months")
    for m, c in sorted(missing_rows)[:10]:
        print(f"  MISSING ROW: {m} {c}")
else:
    print("  All country-months present.")

for var in MF_VARS:
    n_nan = mf_win[var].isna().sum()
    if n_nan:
        nan_rows = mf_win[mf_win[var].isna()]
        by_country = nan_rows.groupby("country")["date"].agg(["min", "max", "count"])
        rng = f"{nan_rows['date'].min()}..{nan_rows['date'].max()}"
        print(f"  {var}: {n_nan} NaN country-months ({rng}; "
              f"{len(by_country)} countries affected)")
    else:
        print(f"  {var}: complete")

# ── 2. Moments (regression dataset) ──────────────────────────────────────────
mo = pd.read_csv(ROOT / "data" / "processed" /
                 "moments_forecasted_interpolated.csv")
mo["date"] = mo["date"].str[:7]          # YYYY-MM-01 -> YYYY-MM
mo_win = mo[mo["date"].isin(months)]
print(f"\nmoments: {len(mo_win)} months in window ({len(months)} expected)")
missing_months = set(months) - set(mo_win["date"])
if missing_months:
    failures.append(f"{len(missing_months)} missing months in moments")
    print(f"  MISSING MONTHS: {sorted(missing_months)[:10]}")

for col in ["spread_stdev"] + [v + "_stdev" for v in MF_VARS]:
    n_nan = mo_win[col].isna().sum()
    if n_nan:
        nan_m = mo_win[mo_win[col].isna()]["date"]
        print(f"  {col}: {n_nan} NaN months ({nan_m.min()}..{nan_m.max()})")
    else:
        print(f"  {col}: complete")

# ── 3. VSTOXX ────────────────────────────────────────────────────────────────
vs = pd.read_csv(ROOT / "data" / "variables" / "vstoxx_monthly.csv")
vs_win = vs[vs["date"].isin(months)]
n_missing = len(months) - vs_win["vstoxx"].notna().sum()
print(f"\nvstoxx_monthly: {vs_win['vstoxx'].notna().sum()} of {len(months)} "
      f"months covered" + (f" ({n_missing} missing)" if n_missing else ""))
if n_missing:
    failures.append(f"{n_missing} missing VSTOXX months")

# ── Summary ──────────────────────────────────────────────────────────────────
if failures:
    print("\nFAILED:", "; ".join(failures))
    sys.exit(1)
print("\nOK: panel is balanced over the full sample window.")
