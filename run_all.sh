#!/usr/bin/env bash
# run_all.sh — reproduce all results of the coding sample from committed data.
#
# Default run: construction checks -> regressions -> regression tests ->
# figures & tables. Every stage reads the committed intermediate CSVs, so the
# full analysis reproduces without any external data access.
#
# Stages that REBUILD data are opt-in, because parts of data/raw/ are
# synthetic licensing placeholders (see README) and two committed inputs
# contain values whose raw sources are not committed:
#   RUN_DATA_PREP=1    also run code/01_data_prep/ (overwrites
#                      data/variables/ — only sensible with real raw data)
#   RUN_CONSTRUCTION=1 also run macro_fundamentals.py + moments.py
#                      (macro_fundamentals aborts if the IMF WEO workbook
#                      is absent; see README "Running the pipeline")
set -euo pipefail
cd "$(dirname "$0")"

banner() { printf '\n==== %s ====\n' "$*"; }

if [[ "${RUN_DATA_PREP:-0}" == "1" ]]; then
  banner "1. DATA PREP (rebuilding data/variables from data/raw)"
  python3 code/01_data_prep/build_uncertainty.py
  python3 code/01_data_prep/build_nexus.py
  python3 code/01_data_prep/build_smp_purchases.py
  python3 code/01_data_prep/build_bid_ask_spreads.py
  python3 code/01_data_prep/build_forecast_panel.py
else
  banner "1. DATA PREP skipped (set RUN_DATA_PREP=1 to rebuild data/variables)"
fi

if [[ "${RUN_CONSTRUCTION:-0}" == "1" ]]; then
  banner "2. CONSTRUCTION (rebuilding data/processed)"
  python3 code/02_construction/macro_fundamentals.py
  python3 code/02_construction/moments.py
else
  banner "2. CONSTRUCTION skipped (set RUN_CONSTRUCTION=1 to rebuild data/processed)"
fi

banner "2b. PANEL-BALANCE CHECK"
python3 code/02_construction/check_panel_balance.py

banner "3. REGRESSIONS"
Rscript code/03_regression/model1_rolling.R
Rscript code/03_regression/model1_fixed.R
Rscript code/03_regression/model2_rolling.R
Rscript code/03_regression/model2_fixed.R

banner "4. REGRESSION TESTS"
Rscript code/04_regression_tests/annex_tests.R
Rscript code/04_regression_tests/coefficient_diagnostics.R

banner "5. FIGURES AND TABLES"
Rscript code/05_figures_tables/plot_spread_stdev.R
Rscript code/05_figures_tables/plot_macro_panels.R
Rscript code/05_figures_tables/plot_vstoxx.R
Rscript code/05_figures_tables/plot_stdev_gallery.R
Rscript code/05_figures_tables/plot_nonfund_component.R
Rscript code/05_figures_tables/plot_decomposition_model2.R
Rscript code/05_figures_tables/descriptive_stats_table.R
python3 code/05_figures_tables/correlation_table.py

banner "DONE — outputs in output/"
