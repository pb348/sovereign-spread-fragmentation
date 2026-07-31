"""
Reconstructs ECB Securities Markets Programme (SMP) purchase volumes
(weekly and monthly) from the primary source: the commentary text of the
ECB's "Consolidated financial statement of the Eurosystem" weekly press
releases (WFS). This is the same source De Pooter, Martin & Pruitt (2015,
IFDP 1138) cite in footnote 10/12 for their Figures 2-3.

The ECB's commentary wording changed over the life of the programme:

  (1) EARLY PERIOD (May-Oct 2010): reports the WEEKLY FLOW directly, in one
      of a few phrasings:
        (a) "...due to settled purchases of EUR 16.3 billion under the
             Securities Markets Programme..."
        (b) "...settled purchases of EUR 796.5 million under the Securities
             Markets Programme..." (same as (a) but in millions)
        (c) "...increased by EUR 176 million to EUR 121.4 billion as a
             result of settled purchases under the Securities Markets
             Programme." (whole weekly change attributed to SMP, no
             separate EUR-X-under-SMP clause)
      -> use the reported figure directly as that week's net purchase.

  (2) LATER PERIOD (~Oct 2010 onward): reports the CUMULATIVE holdings, e.g.
      "...the value of accumulated purchases under the Securities Markets
      Programme amounted to EUR 207.6 billion..."
      -> a running-baseline reconciliation (rather than a simple diff())
      converts consecutive cumulative values into weekly net purchases,
      bridging any gap weeks (including the flow-era -> cumulative-era
      transition) without losing purchases into a NaN.

All are combined into a single weekly net-purchase series, then summed by
month to give monthly volumes comparable to Figure 2 of the paper. A
weekly bar chart styled after that same Figure 2 is also produced.

Every extracted "flow" value is sanity-capped at 30bn/week (no single week
of SMP purchases ever came close to that) -- a match that produces a
larger value is REJECTED and printed to the console rather than silently
corrupting the series, so a bad regex match fails loudly instead of
producing something like a -960bn outlier.

Coverage: SMP launch (10 May 2010) through official termination
(6 September 2012). After termination, the balance only shrinks via
redemptions, so net "purchases" are ~0/negative from Sept 2012 onward.

Usage:
    pip install requests pandas beautifulsoup4 matplotlib --break-system-packages
    python smp_scraper.py
Outputs:
    smp_weekly.csv        - one row per WFS release with kind/value/net weekly figure
    smp_monthly.csv       - monthly net purchase volumes (Figure-2-style)
    smp_weekly_plot.png   - weekly bar chart, styled after Figure 2 of the paper
"""

import html as html_module
import re
import time
from datetime import date, timedelta

import matplotlib.pyplot as plt
import pandas as pd
import requests
from bs4 import BeautifulSoup
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

HEADERS = {
    "User-Agent": "Mozilla/5.0 (academic research script; Uni Freiburg thesis; "
                  "contact: replace_with_your_email@uni-freiburg.de)"
}

START = date(2010, 5, 11)   # first Tuesday after SMP launch (10 May 2010)
END = date(2012, 9, 18)     # a couple weeks past official termination (6 Sep 2012)

MAX_PLAUSIBLE_WEEKLY_FLOW_BN = 30  # no single week ever purchased close to this much

# The ECB has used a couple of different URL schemes/paths over the years;
# try each in turn.
URL_TEMPLATES = [
    "https://www.ecb.europa.eu/press/annual-reports-financial-statements/wfs/{y}/html/fs{ymd}.en.html",
    "https://www.ecb.europa.eu/press/pr/wfs/{y}/html/fs{ymd}.en.html",
]

# --- Pattern (2): cumulative holdings ---------------------------------
#   "...accumulated purchases under the Securities Markets Programme
#    amounted to EUR 207.6 billion..."
#   "...accumulated purchases under the Securities Markets Programme and
#    that of the portfolio held under the covered bond purchase programme
#    totalled EUR 74.1 billion and EUR 60.8 billion respectively."
# [^.] (rather than DOTALL '.') stops the match at the first period so it
# can't cross into an unrelated sentence and grab the wrong number.
CUMULATIVE_PATTERN = re.compile(
    r"accumulated purchases under the Securities Markets Programme[^.]{0,160}?EUR\s*([\d]+(?:\.[\d]+)?)\s*billion",
    re.IGNORECASE,
)

# --- Pattern (1a): weekly flow, billions -------------------------------
#   "...due to settled purchases of EUR 16.3 billion under the Securities
#    Markets Programme..."
FLOW_PATTERN = re.compile(
    r"purchases of EUR\s*([\d]+(?:\.[\d]+)?)\s*billion under the Securities Markets Programme",
    re.IGNORECASE,
)

# --- Pattern (1b): weekly flow, millions -------------------------------
#   "...settled purchases of EUR 796.5 million under the Securities Markets
#    Programme..."
FLOW_PATTERN_MILLION = re.compile(
    r"purchases of EUR\s*([\d]+(?:\.[\d]+)?)\s*million under the Securities Markets Programme",
    re.IGNORECASE,
)

# --- Pattern (1c): whole weekly change attributed to SMP ---------------
#   "...increased by EUR 176 million to EUR 121.4 billion as a result of
#    settled purchases under the Securities Markets Programme."
#   "...increased by EUR 9 million to EUR 124.3 billion. This is due to
#    settled purchases under the Securities Markets Programme."
FLOW_PATTERN_WHOLE = re.compile(
    r"increased by EUR\s*([\d]+(?:\.[\d]+)?)\s*(million|billion) to EUR\s*[\d.,]+\s*billion[^.]*?"
    r"settled purchases under the Securities Markets Programme",
    re.IGNORECASE,
)
# Manually verified override -- takes precedence over anything the scraper
# would fetch/parse automatically for the same publication_date.
# Value is the week's NET SMP PURCHASE in EUR bn (a 'flow'-type figure).
# Manually verified overrides -- take precedence over anything the scraper
# would fetch/parse automatically for the same publication_date.
# Each value is the week's NET SMP PURCHASE in EUR bn (a 'flow'-type figure).
MANUAL_OVERRIDES = [
    {
        "publication_date": date(2010, 8, 10),
        "value": 0.009,  # EUR 9 million
        "note": "Manually verified: 'increased by EUR 9 million as a result of "
                "purchases settled under the Securities Markets Programme'.",
    },
    {
        "publication_date": date(2010, 8, 17),
        "value": 0.010,  # EUR 10 million
        "note": "Manually verified: 'increased by EUR 10 million as a result of "
                "purchases settled under the Securities Markets Programme'.",
    },
    {
        "publication_date": date(2010, 8, 24),
        "value": 0.338,  # EUR 338 million SMP purchases (NOT the net EUR 327m
                          # item 7.1 change -- that net figure already reflects
                          # EUR 10m of CBPP bonds maturing that same week, which
                          # is a separate portfolio and not an SMP purchase)
        "note": "Manually verified: 'settled purchases of nearly EUR 338 million "
                "under the Securities Markets Programme which more than offset "
                "maturing bonds of EUR 10 million... under the covered bond "
                "purchase programme'. Used the EUR 338m SMP-only figure, not "
                "the EUR 327m net item 7.1 change.",
    },
    {
        "publication_date": date(2010, 10, 6),  # published Wed (first WFS of Q4)
        "value": 1.384,
        "note": "Manually verified: week ending 1 Oct 2010, 'settled purchases of "
                "EUR 1,384 million under the Securities Markets Programme'. Cross-"
                "checked: cumulative total stated same week (EUR 63.3bn) matches "
                "the already-confirmed 8 Oct 2010 figure.",
    },
]


def fetch_smp_value(d: date):
    """Try to fetch the WFS release published on date d.

    Returns (kind, value, url) where kind is 'cumulative' or 'flow',
    or (None, None, None) if nothing was found. 'value' is always in
    billions of euro. 'flow' matches larger than
    MAX_PLAUSIBLE_WEEKLY_FLOW_BN are rejected (printed, not returned) as
    likely mismatches rather than real data.
    """
    ymd = d.strftime("%y%m%d")
    for tmpl in URL_TEMPLATES:
        url = tmpl.format(y=d.year, ymd=ymd)
        try:
            r = requests.get(url, headers=HEADERS, timeout=15)
        except requests.RequestException:
            continue
        if r.status_code != 200:
            continue

        # Extract plain text only (drops nav/menu junk, decodes &nbsp; etc.)
        soup = BeautifulSoup(r.text, "html.parser")
        text = html_module.unescape(soup.get_text(separator=" "))
        text = re.sub(r"\s+", " ", text)

        m = CUMULATIVE_PATTERN.search(text)
        if m:
            return "cumulative", float(m.group(1)), url

        m = FLOW_PATTERN.search(text)
        if m:
            value = float(m.group(1))
            if value <= MAX_PLAUSIBLE_WEEKLY_FLOW_BN:
                return "flow", value, url
            print(f"  REJECTED implausible flow match ({value}bn, FLOW_PATTERN) at {url}")

        m = FLOW_PATTERN_MILLION.search(text)
        if m:
            value = float(m.group(1)) / 1000
            if value <= MAX_PLAUSIBLE_WEEKLY_FLOW_BN:
                return "flow", value, url
            print(f"  REJECTED implausible flow match ({value}bn, FLOW_PATTERN_MILLION) at {url}")

        m = FLOW_PATTERN_WHOLE.search(text)
        if m:
            value = float(m.group(1))
            if m.group(2).lower() == "million":
                value = value / 1000
            if value <= MAX_PLAUSIBLE_WEEKLY_FLOW_BN:
                return "flow", value, url
            print(f"  REJECTED implausible flow match ({value}bn, FLOW_PATTERN_WHOLE) at {url}")

    return None, None, None


def plot_weekly(df: pd.DataFrame, out_path=ROOT / "output" / "figures" / "smp_weekly_plot.png"):
    """Weekly bar chart styled after Figure 2 of De Pooter, Martin & Pruitt (2015)."""
    plot_df = df.dropna(subset=["weekly_net_purchase_eur_bn"]).sort_values("reference_friday")

    fig, ax = plt.subplots(figsize=(11, 5))
    ax.bar(
        plot_df["reference_friday"],
        plot_df["weekly_net_purchase_eur_bn"],
        width=5,  # ~1 week wide bars
        color="#0000CD",
        edgecolor="none",
    )
    ax.axhline(0, color="black", linewidth=0.8)
    ax.set_ylabel("Billions of euro")
    ax.set_title("Securities Markets Programme Purchases: Weekly (reconstructed)")
    ax.text(
        0.01, 0.97, "Weekly", transform=ax.transAxes,
        va="top", ha="left", fontsize=10,
    )
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    fig.autofmt_xdate()
    fig.tight_layout()
    fig.savefig(out_path, dpi=150)
    plt.close(fig)


def main():
    records = []
    d = START
    while d <= END:
        found = False
        # WFS is normally published the Tuesday after the Friday reference date,
        # but shifts around quarter-ends and TARGET holidays -- probe a small
        # window of neighbouring days.
        for offset in [0, 1, -1, 2, -2, 3, 4, -3]:
            candidate = d + timedelta(days=offset)
            kind, value, url = fetch_smp_value(candidate)
            if value is not None:
                records.append(
                    {
                        "publication_date": candidate,
                        "kind": kind,
                        "value": value,
                        "source_url": url,
                    }
                )
                found = True
                break
        if not found:
            print(f"WARNING: no WFS release / no SMP mention found near {d}")
        d += timedelta(days=7)
        time.sleep(0.5)  # be polite to the ECB server
    
    for override in MANUAL_OVERRIDES:
        records = [r for r in records if r["publication_date"] != override["publication_date"]]
        records.append(
            {
                "publication_date": override["publication_date"],
                "kind": "flow",
                "value": override["value"],
                "source_url": f"MANUAL OVERRIDE: {override['note']}",
            }
        )
        print(f"Applied manual override for {override['publication_date']}: "
              f"{override['value']}bn ({override['note']})")

    df = pd.DataFrame(records)

    if df.empty:
        print("No data collected -- aborting.")
        return

    df = df.sort_values("publication_date").drop_duplicates(subset="publication_date")
    df = df.reset_index(drop=True)

    # Running-baseline reconciliation: bridges gaps (missing weeks, and the
    # flow-era -> cumulative-era transition) without losing any purchases.
    # Starts at 0 since SMP held nothing before 10 May 2010.
    running_baseline = 0.0
    weekly_net = []
    for _, row in df.iterrows():
        if row["kind"] == "flow":
            net = row["value"]
            running_baseline += net
        else:  # 'cumulative'
            net = row["value"] - running_baseline
            running_baseline = row["value"]  # resync to the reported total
        weekly_net.append(net)
    df["weekly_net_purchase_eur_bn"] = weekly_net

    suspicious = df[df["weekly_net_purchase_eur_bn"].abs() > 25]  # >25bn in one week is implausible except the Aug 2011 relaunch
    print("Suspicious weeks (check manually):")
    print(suspicious[["publication_date", "kind", "value", "weekly_net_purchase_eur_bn", "source_url"]].to_string(index=False))

    # WFS publication date is normally the Tuesday after the Friday
    # reference date it reports on.
    df["reference_friday"] = df["publication_date"] - pd.to_timedelta(4, unit="D")
    df["month"] = df["reference_friday"].values.astype("datetime64[M]")

    monthly = (
        df.groupby("month")["weekly_net_purchase_eur_bn"]
        .sum()
        .reset_index()
        .rename(columns={"weekly_net_purchase_eur_bn": "smp_net_purchases_eur_bn"})
    )

    df.to_csv(ROOT / "data" / "variables" / "smp_weekly.csv", index=False)
    monthly.to_csv(ROOT / "data" / "variables" / "smp_monthly.csv", index=False)
    plot_weekly(df)

    print()
    print(monthly.to_string(index=False))
    print(f"\nSaved smp_weekly.csv ({len(df)} rows), smp_monthly.csv ({len(monthly)} rows), "
          f"and smp_weekly_plot.png")


if __name__ == "__main__":
    main()