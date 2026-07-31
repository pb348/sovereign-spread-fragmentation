# ECB Asset Purchases and Sovereign Bond Spread Fragmentation

Data pipeline and estimation code for a bachelor's thesis on the effect of ECB
asset purchase programmes (SMP, PSPP, PEPP) on the fragmentation of euro-area
sovereign bond spreads, following the moments-based approach of Kakes & Van den
End (2023). The pipeline builds monthly panels of macro fundamentals for 11
euro-area countries, computes cross-sectional moments of spreads and
fundamentals, estimates rolling and fixed-window regressions, and decomposes
spread dispersion into fundamental and non-fundamental components.

## Repository structure

```
code/
├── 01_data_prep/        # load & clean raw sources into analysis variables
│   ├── build_bid_ask_spreads.py    # EDGE bid-ask estimator on daily 10Y OHLC yields
│   ├── build_smp_purchases.py      # reconstructs SMP volumes from ECB weekly press releases
│   ├── build_nexus.py              # bank-sovereign nexus from ECB BSI statistics
│   ├── build_uncertainty.py        # EPU workbooks -> monthly policy-uncertainty panel
│   ├── build_forecast_panel.py     # combines WEO/OECD forecast vintages into a panel
│   └── weo_vintages/               # one-time IMF WEO vintage extraction utilities
├── 02_construction/     # construction of the estimation dataset
│   ├── macro_fundamentals.py       # Burriel et al. (2024) fixed-horizon forecast interpolation
│   └── moments.py                  # cross-sectional stdev/skew/kurtosis per variable
├── 03_regression/       # estimation
│   ├── model1_rolling.R            # 60-month rolling OLS (baseline)
│   ├── model1_fixed.R              # fixed-window counterpart
│   ├── model2_rolling.R            # + ECB purchase volumes, lag/PSPP variants
│   └── model2_fixed.R              # fixed-window Model 2 + decomposition
├── 04_regression_tests/ # tests on the regression
│   ├── annex_tests.R               # stability, endogeneity (2SLS), unit roots, cointegration, residual diagnostics
│   └── coefficient_diagnostics.R   # rolling-coefficient summary table + plot
└── 05_figures_tables/   # figures and descriptive/annex tables

data/
├── raw/                 # source data as downloaded (see Data availability below)
├── variables/           # cleaned analysis variables (written by 01, read by 02/03)
└── processed/           # constructed panels + moments (written by 02, read by 03/04/05)

output/
├── regression/          # regression results (model1_*/model2_*, announcements)
├── tests/               # annex test outputs (incl. Table A2 endogeneity tests)
├── figures/             # all generated figures
└── tables/              # descriptive and annex tables (csv/tex/docx)
```

## Running the pipeline

Python scripts anchor paths at the repository root automatically; R scripts use
the [`here`](https://here.r-lib.org/) package (the `.here` file marks the
root), so everything can be run from any working directory.

```bash
pip install -r requirements.txt

# 1. Data prep (only needed to rebuild data/variables from raw sources)
python code/01_data_prep/build_bid_ask_spreads.py
python code/01_data_prep/build_nexus.py
# ... remaining 01 scripts as needed

# 2. Construction
python code/02_construction/macro_fundamentals.py
python code/02_construction/moments.py

# 3. Regression
Rscript code/03_regression/model1_rolling.R
Rscript code/03_regression/model2_rolling.R
# fixed-window counterparts analogous

# 4. Tests on the regression
Rscript code/04_regression_tests/annex_tests.R
```

R package requirements: `here`, `tidyverse` (dplyr, tidyr, readr, ggplot2,
purrr, lubridate), `zoo`, `patchwork`, `scales`, `sandwich`,
`lmtest`, `tseries`, `urca`, `car`, `AER`, `fixest`, `cowplot`, `flextable`,
`officer`.

All intermediate and final CSVs are committed, so any stage can be run in
isolation without re-running upstream stages. Note that fully re-running
`macro_fundamentals.py` requires the IMF WEO historical-forecasts workbook
(~10 MB, not committed; see `PATHS['weo_historical']` in the script) — without
it the script warns and leaves the WEO-based columns empty, while the
committed panel keeps all downstream stages reproducible.

## Data availability and licensing

Most inputs in `data/raw/` come from freely available sources (IMF WEO vintages,
OECD/AMECO forecast archives, ECB Data Portal BSI statistics, the Economic
Policy Uncertainty project) and are included or can be re-downloaded from the
provider. Large re-downloadable archives (EBA Transparency Exercise, raw
IMF WEO vintage workbooks, OECD Economic Outlook PDFs) are not committed for
size reasons; the extracted long-format CSVs in `data/variables/` are.

Three market-data inputs were obtained under license and are **not redistributed**
in this repository. The files currently present under those names are **synthetic
placeholders** (random walks with the correct structure) so that the pipeline runs
end-to-end; results produced from them are meaningless. To reproduce the thesis
results, replace them with data from your own licensed access:

| Placeholder file(s) | Original source | Expected structure |
|---|---|---|
| `data/raw/investing_com/{CC}_10Y.csv` (11 countries; optionally `{CC}_10Y2.csv` for a second date range) | Investing.com daily 10Y benchmark government bond yield export | Columns `"Date","Price","Open","High","Low","Change %"`, one row per trading day, `Date` as `MM/DD/YYYY`; `Price` is the daily close |
| `data/raw/vstoxx50.txt` | STOXX — VSTOXX (V2TX) daily index values | Semicolon-separated `Date;Symbol;Indexvalue`, `Date` as `DD.MM.YYYY`, symbol `V2TX` |
| `data/raw/yields_ois_raw.csv` | Monthly averages of the 10Y yields above plus a euro-area OIS rate | First (unnamed) column `YYYY-MM`, then country columns `AT,BE,FI,FR,DE,GR,IE,IT,NL,ES,PT`, then `OIS`; values in percent |

All code that cleans, merges, and analyzes these series is our own work and is
included in full (e.g. `code/01_data_prep/build_bid_ask_spreads.py`, which
estimates bid–ask spreads from the daily OHLC data with the EDGE estimator).
Derived monthly series used downstream (`data/variables/sovereign_spreads.csv`,
`vstoxx_monthly.csv`, `bid_ask_spreads.csv`) are retained as substantially
transformed aggregates.
