"""
DATA REQUIREMENT (not redistributed in this repo):
This script expects one daily 10Y benchmark-yield OHLC export per country,
named {CC}_10Y.csv (optionally a second file {CC}_10Y2.csv covering an
earlier/later date range), in data/raw/investing_com/. The originals were exported
from Investing.com, whose terms of use do not permit redistribution.
Expected columns (Investing.com export format):
    "Date","Price","Open","High","Low","Change %"   with Date = MM/DD/YYYY
("Price" is the daily close and is renamed to "Close" below.)
The {CC}_10Y.csv files currently in data/raw/investing_com/ are SYNTHETIC placeholders
(random walks) so the code runs end-to-end; replace them with real exports
to reproduce the thesis results. See the README's data-availability section.
"""
import pandas as pd
import numpy as np
from bidask import edge_rolling
import os

countries = ['AT', 'BE', 'DE', 'ES', 'FI', 'FR', 'GR', 'IE', 'IT', 'NL', 'PT']
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..'))
folder = os.path.join(ROOT, 'data', 'raw', 'investing_com') + os.sep

# ── Shift to handle negative yields ─────────────────────────────────────────
# The EDGE estimator (Ardia et al., 2024) uses log-price ratios internally
# (e.g. log(High/Low)). Log of a non-positive number is undefined, so EDGE
# returns NaN whenever any OHLC value is <= 0. During the low/negative rate
# environment of 2015-2022, core eurozone 10Y yields (AT, BE, DE, FI, FR)
# turned negative, causing systematic NaN output for those country-months.
# To resolve this we shift all OHLC values by a constant (SHIFT = 10 pp)
# before passing them to EDGE. Because the shift is applied uniformly across
# all countries and all time periods, it does not alter the relative spread
# estimates or introduce differential bias.
SHIFT = 10

# ── Data-quality cleaning ────────────────────────────────────────────────────
# investing.com export files contain occasional placeholder rows for days with
# no/insufficient trading: Close == 0.000 (often together with Open/High/Low
# that are *not* zero, and a "Change %" of 0.00% or -100.00%). A Close of
# exactly 0 is never a genuine 10Y sovereign yield (not even during the
# negative-rate period, where yields stayed within a few tenths of a percent
# of zero). If such a row falls on the last trading day of a month, it becomes
# that month's Close via resample(...).agg({'Close': 'last'}), corrupting the
# monthly OHLC bar and causing a spurious spike in every rolling EDGE window
# that contains that month (e.g. PT, Sep 2010 -> Close=0.000 -> EDGE jumps
# from ~0.003 to ~0.27 for Oct 2010-Feb 2011, reverting once Sep 2010 rolls
# out of the 6-month window).
#
# Fix: flag any daily row where Close == 0 (a value that cannot occur for a
# real yield quote) and treat that entire OHLC row as missing (NaN) rather
# than literal zero. Per the bidask FAQ, the EDGE estimator handles missing
# values natively and it is preferable to keep them as NaN on a regular time
# grid rather than drop/interpolate them.
def clean_ohlc(df, cols=('Open', 'High', 'Low', 'Close')):
    bad = df['Close'] == 0
    df = df.copy()
    df.loc[bad, list(cols)] = np.nan
    return df, int(bad.sum())


def monthly_ohlc(df, cols=('Open', 'High', 'Low', 'Close')):
    """Resample daily OHLC to monthly OHLC, skipping NaNs for first/last
    (the default pandas 'first'/'last' resample aggregations do NOT skip
    NaN, so a NaN'd-out last day would otherwise still poison the monthly
    Close)."""
    o, h, l, c = cols
    agg = pd.DataFrame({
        o: df[o].resample('ME').apply(lambda s: s.dropna().iloc[0] if s.dropna().size else np.nan),
        h: df[h].resample('ME').max(),
        l: df[l].resample('ME').min(),
        c: df[c].resample('ME').apply(lambda s: s.dropna().iloc[-1] if s.dropna().size else np.nan),
    })
    return agg


dfs = {}
quality_log = {}

for c in countries:
    dfs_country = []

    if os.path.exists(f"{folder}{c}_10Y.csv"):
        try:
            df1 = pd.read_csv(f"{folder}{c}_10Y.csv", encoding='utf-8')
        except UnicodeDecodeError:
            df1 = pd.read_csv(f"{folder}{c}_10Y.csv", encoding='latin1')
        dfs_country.append(df1)
        print(f"Loaded {c}_10Y.csv: {len(df1)} rows")

    if os.path.exists(f"{folder}{c}_10Y2.csv"):
        try:
            df2 = pd.read_csv(f"{folder}{c}_10Y2.csv", encoding='utf-8')
        except UnicodeDecodeError:
            df2 = pd.read_csv(f"{folder}{c}_10Y2.csv", encoding='latin1')
        dfs_country.append(df2)
        print(f"Loaded {c}_10Y2.csv: {len(df2)} rows")

    if not dfs_country:
        continue

    df = pd.concat(dfs_country, ignore_index=True)
    df = df.rename(columns={'Price': 'Close'})
    df['Date'] = pd.to_datetime(df['Date'], format='%m/%d/%Y')
    df = df.sort_values('Date').set_index('Date')
    df = df[~df.index.duplicated(keep='first')]

    cols = ['Open', 'High', 'Low', 'Close']

    # Count daily rows with any non-positive OHLC value (genuine negative
    # yields + placeholder zeros, before cleaning)
    neg_mask = (df[cols] <= 0).any(axis=1)
    n_nonpositive = int(neg_mask.sum())

    # Clean placeholder Close==0 rows -> NaN
    df, n_bad_close = clean_ohlc(df, cols)

    # Resample to monthly OHLC (NaN-aware first/last)
    monthly = monthly_ohlc(df, cols)

    dfs[c] = monthly
    quality_log[c] = {
        'daily_nonpositive_ohlc': n_nonpositive,
        'daily_close_eq_0_cleaned': n_bad_close,
        'months': len(monthly),
        'months_with_nan': int(monthly.isna().any(axis=1).sum()),
    }
    print(f"  {c}: {len(monthly)} months  "
          f"(daily rows <=0: {n_nonpositive}, Close==0 cleaned: {n_bad_close}, "
          f"months still containing NaN after cleaning: {quality_log[c]['months_with_nan']})\n")

# ── Log summary ──────────────────────────────────────────────────────────────
print("\n── Data quality summary ──")
for c, q in quality_log.items():
    print(f"  {c}: {q}")
print(f"\nFix for negative/near-zero yields: all OHLC values shifted by +{SHIFT} pp before EDGE estimation\n")

# ── Rolling 6-month EDGE estimates ──────────────────────────────────────────
# sign=True returns signed estimates (per the bidask FAQ, recommended over the
# default absolute-value estimates when the output will be averaged/used in a
# regression, since |.| introduces a small-sample upward bias). Negative
# estimates are then reset to zero (a negative "spread" has no economic
# meaning; keeping them would bias the series downward instead).
print("Computing EDGE rolling estimates (sign=True, negatives clipped to 0)...")
rolling = {}
for c in countries:
    if c not in dfs:
        continue
    est = edge_rolling(df=dfs[c] + SHIFT, window=6, sign=True)
    rolling[c] = est.clip(lower=0)

output = pd.DataFrame(rolling)
output = output[[c for c in ['AT', 'BE', 'FI', 'FR', 'DE', 'GR', 'IE', 'IT', 'NL', 'PT', 'ES'] if c in output.columns]]
output.index = output.index.strftime('%Y-%m')
output.to_csv(os.path.join(ROOT, 'data', 'variables', 'bid_ask_spreads_monthly.csv'))
print("Done. Saved to bid_ask_spreads_monthly.csv")