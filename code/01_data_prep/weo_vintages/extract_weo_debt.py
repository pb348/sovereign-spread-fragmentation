"""
Extract IMF WEO vintage debt/GDP forecasts for 11 EMU countries.

Run: pip install weo pandas openpyxl xlrd
     python extract_weo_debt.py
"""

import pandas as pd
import pathlib
import warnings
warnings.filterwarnings('ignore')

EMU_ISO = {
    'AUT': 'Austria', 'BEL': 'Belgium', 'FIN': 'Finland',
    'FRA': 'France',  'DEU': 'Germany', 'GRC': 'Greece',
    'IRL': 'Ireland', 'ITA': 'Italy',   'LUX': 'Luxembourg',
    'NLD': 'Netherlands', 'PRT': 'Portugal', 'ESP': 'Spain',
}

DEBT_CODE_OLD = 'GGD_NGDP'      # Oct 2007–Apr 2010
DEBT_CODE_NEW = 'GGXWDG_NGDP'   # Oct 2010–present

_ROOT = pathlib.Path(__file__).resolve().parents[3]
# Raw WEO vintage workbooks (~300 MB) are not shipped; the weo package
# downloads them here on first run (or fetch manually from the IMF site).
RAW_DIR = _ROOT / 'data' / 'raw' / 'weo_vintages'
RAW_DIR.mkdir(parents=True, exist_ok=True)


def find_file(year, release):
    mon = 'apr' if release == 1 else 'oct'
    candidates = [
        RAW_DIR / f'weo_{year}_{release}.tsv',
        RAW_DIR / f'weo_{year}_{release}.csv',
        RAW_DIR / f'weo_{year}_{release}.xls',
        RAW_DIR / f'weo_{year}_{release}.xlsx',
        RAW_DIR / f'weo{mon}{year}all.xls',
        RAW_DIR / f'weo{mon}{year}all.xlsx',
    ]
    for c in candidates:
        if c.exists() and c.stat().st_size > 10_000:
            return c
    return None


def collect_all():
    import weo
    releases = weo.all_releases()
    paths = {}
    print(f"Scanning {len(releases)} vintages...")

    for year, release in releases:
        label = f"{year}-{'Apr' if release == 1 else 'Oct'}"
        existing = find_file(year, release)

        if existing:
            print(f"  {label}: found {existing.name}")
            paths[(year, release)] = existing
        elif (year, release) <= (2020, 1):
            try:
                outpath = str(RAW_DIR / f'weo_{year}_{release}.tsv')
                path, url = weo.download(year, release, filename=outpath)
                p = pathlib.Path(path)
                if p.exists() and p.stat().st_size > 10_000:
                    print(f"  {label}: downloaded")
                    paths[(year, release)] = p
                else:
                    print(f"  {label}: download too small — skipping")
            except Exception as e:
                print(f"  {label}: download error — {e}")
        else:
            print(f"  {label}: NOT FOUND — skipping")

    return paths


def read_weo_file(filepath):
    """
    Try every reading strategy in order until one produces a df
    with the expected 'WEO Subject Code' column.
    """
    fp = pathlib.Path(filepath)

    strategies = [
        # 1. UTF-16-LE tab-delimited (newer manually downloaded .xls files)
        lambda: pd.read_csv(fp, sep='\t', encoding='utf-16-le',
                            low_memory=False, dtype=str),
        # 2. Latin-1 tab-delimited (weo package TSV files, pre-2020)
        lambda: pd.read_csv(fp, sep='\t', encoding='latin-1',
                            low_memory=False, dtype=str),
        # 3. UTF-8 tab-delimited
        lambda: pd.read_csv(fp, sep='\t', encoding='utf-8',
                            low_memory=False, dtype=str),
        # 4. Genuine Excel via openpyxl
        lambda: pd.read_excel(fp, dtype=str, engine='openpyxl'),
    ]

    for i, strategy in enumerate(strategies):
        try:
            df = strategy()
            df = df.dropna(axis=1, how='all')
            df.columns = [str(c).strip() for c in df.columns]
            if 'WEO Subject Code' in df.columns and 'ISO' in df.columns:
                return df
        except Exception:
            continue

    # Debug: print actual columns from TSV read
    try:
        df = pd.read_csv(fp, sep='\t', encoding='latin-1',
                         low_memory=False, dtype=str, nrows=2)
        df.columns = [str(c).strip() for c in df.columns]
        print(f"    DEBUG columns (TSV): {list(df.columns[:10])}")
    except Exception as e:
        print(f"    DEBUG read failed: {e}")

    return pd.DataFrame()


def parse_vintage(filepath, year, release):
    label         = f"{year}-{'Apr' if release == 1 else 'Oct'}"
    forecast_year = year + 1
    debt_code     = DEBT_CODE_OLD if (year, release) <= (2009, 2) \
                    else DEBT_CODE_NEW

    try:
        df = read_weo_file(filepath)

        if df.empty or 'WEO Subject Code' not in df.columns:
            print(f"  {label}: could not parse file — skipping")
            return pd.DataFrame()

        for col in ['ISO', 'WEO Subject Code', 'Country',
                    'Subject Descriptor', 'Units']:
            if col in df.columns:
                df[col] = df[col].astype(str).str.strip()

        mask = (df['WEO Subject Code'] == debt_code) & \
               (df['ISO'].isin(EMU_ISO.keys()))
        sub = df[mask].copy()

        if sub.empty:
            print(f"  {label}: no rows for {debt_code} — skipping")
            return pd.DataFrame()

        col = str(forecast_year)
        if col not in sub.columns:
            print(f"  {label}: column '{forecast_year}' not found")
            return pd.DataFrame()

        records = []
        for _, row in sub.iterrows():
            raw = str(row[col]).strip()
            try:
                value = float(raw.replace(',', '')) \
                        if raw not in ('n/a','--','','nan','None','NaN') \
                        else float('nan')
            except ValueError:
                value = float('nan')

            records.append({
                'vintage':            label,
                'vintage_year':       year,
                'vintage_month':      'Apr' if release == 1 else 'Oct',
                'forecast_year':      forecast_year,
                'iso3':               row['ISO'],
                'country':            row['Country'],
                'subject_code':       row['WEO Subject Code'],
                'subject_descriptor': row['Subject Descriptor'],
                'units':              row['Units'],
                'debt_gdp_forecast':  value,
            })

        result  = pd.DataFrame(records)
        n_valid = result['debt_gdp_forecast'].notna().sum()
        print(f"  {label}: {n_valid}/{len(result)} non-missing "
              f"(code={debt_code}, target={forecast_year})")
        return result

    except Exception as e:
        print(f"  {label}: ERROR — {e}")
        return pd.DataFrame()


if __name__ == '__main__':
    print("=" * 65)
    print("IMF WEO Vintage Debt/GDP Extractor")
    print("=" * 65)

    print("\n[1] Locating vintage files...")
    paths = collect_all()

    print(f"\n[2] Extracting forecasts ({len(paths)} vintages)...")
    frames = [parse_vintage(p, y, r) for (y, r), p in sorted(paths.items())]
    panel  = pd.concat([f for f in frames if not f.empty], ignore_index=True)

    panel.to_csv(_ROOT / 'data' / 'variables' / 'forecasted' / 'weo_debt_gdp_vintages_long.csv',
             index=False)

    print(f"\nSaved weo_debt_gdp_vintages_long.csv")
    print(f"  {len(panel)} rows | "
          f"{panel['vintage'].nunique()} vintages | "
          f"{panel['debt_gdp_forecast'].notna().sum()} non-missing")

    print("\nCoverage per vintage:")
    print(panel.groupby('vintage')['debt_gdp_forecast']
          .apply(lambda x: f"{x.notna().sum()}/{len(x)}").to_string())