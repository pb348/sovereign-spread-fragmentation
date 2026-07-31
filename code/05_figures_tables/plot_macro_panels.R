# plot_macro_panels.R
# Outputs: final_macro_stdev_forecasted.png
#          final_macro_fundamentals.png
#          final_macro_hf.png
# Range:   2005-09-01 to 2025-12-01
#
# CHANGELOG
# ---------
# [latest] Macro fundamentals timeseries (gdp_growth, inflation, debt_gdp,
#          current_account) now sourced from processed/macro_fundamentals_interpolated.csv
#          (monthly, YYYY-MM dates, country = ISO2) instead of
#          variables/forecasted/forecast_vintage_panel.csv (annual, h=1 WEO forecasts).
#          Removed iso3_to_iso2 lookup and FCAST_DIR path — no longer needed.
# [prev]   Split timeseries into two PNGs: macro fundamentals (2×2) and HF (1×3).
# [prev]   Replaced direct right-side labels with shared collected legend.
# [prev]   Restored original colour palette; full country names in legend.
# [prev]   Removed Luxembourg from ea_countries.
# [prev]   Hardcoded date parsing per file (no format guessing).

library(tidyverse)
library(lubridate)
library(patchwork)
library(cowplot)

# Paths anchor at the repository root, located via the {here} package
# (uses the `.here` marker file; run from any working directory).
ROOT <- here::here()

# ── Paths ─────────────────────────────────────────────────────────────────────
PROC_DIR  <- file.path(ROOT, "data", "processed")
VAR_DIR   <- file.path(ROOT, "data", "variables")
OUT_DIR   <- file.path(ROOT, "output", "figures")

DATE_START <- as.Date("2010-08-01")
DATE_END   <- as.Date("2022-10-01")

# ── Shared theme ──────────────────────────────────────────────────────────────
theme_panel <- function() {
  theme_bw(base_size = 9) +
    theme(
      plot.title        = element_text(hjust = 0.5, size = 9, face = "plain"),
      panel.grid.minor  = element_blank(),
      axis.text.x       = element_text(size = 7),
      axis.text.y       = element_text(size = 7),
      plot.margin       = margin(4, 6, 2, 4)
    )
}

# ── Country palette ───────────────────────────────────────────────────────────
ea_countries <- c("AT","BE","DE","ES","FI","FR","GR","IE","IT","NL","PT")

country_fullnames <- c(
  AT = "Austria",     BE = "Belgium",     DE = "Germany",
  ES = "Spain",       FI = "Finland",     FR = "France",
  GR = "Greece",      IE = "Ireland",     IT = "Italy",
  NL = "Netherlands", PT = "Portugal"
)

pal <- c(
  AT = "#e41a1c", BE = "#ff7f00", DE = "#4daf4a", ES = "#a65628",
  FI = "#f781bf", FR = "#999999", GR = "#984ea3", IE = "#00bcd4",
  IT = "#377eb8", NL = "#ff69b4", PT = "#e6ab02"
)

# ═══════════════════════════════════════════════════════════════════════════════
# 1.  STDEV PLOT — moments_forecasted_interpolated.csv
# ═══════════════════════════════════════════════════════════════════════════════
moments <- read_csv(
  file.path(PROC_DIR, "moments_forecasted_interpolated.csv"),
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= DATE_START, date <= DATE_END)

stdev_spec <- tribble(
  ~col,                       ~label,                           ~ymin, ~ymax, ~scale,
  "debt_gdp_stdev",           "Debt ratio (stdev)",              0,     0.6,   100,
  "gdp_growth_stdev",         "GDP growth (stdev)",              0,     2,     1,
  "inflation_stdev",          "Inflation (stdev)",               0,     1.2,   1,
  "current_account_stdev",    "Current account balance (stdev)", 0,     0.08,  100,
  "policy_uncertainty_stdev", "Policy uncertainty (stdev)",      0,     200,   1,
  "bank_nexus_stdev",         "Sovereign/bank nexus (stdev)",    0.01,  0.07,  1
  #  "bid_ask_stdev",            "bid_ask (stdev)"
)

make_stdev_panel <- function(col, label, ymin, ymax, scale) {
  df <- moments %>%
    transmute(date, value = .data[[col]] / scale) %>%
    drop_na()
  ggplot(df, aes(date, value)) +
    geom_line(colour = "#2166ac", linewidth = 0.45) +
    labs(title = label, x = NULL, y = NULL) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                 limits = c(DATE_START, DATE_END), expand = c(0.01, 0)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    theme_panel()
}
stdev_plots <- pmap(stdev_spec, make_stdev_panel)

stdev_grid <- wrap_plots(stdev_plots, ncol = 2) +
  plot_annotation(
    title = "Cross-Country Standard Deviation — by Variable",
    theme = theme(plot.title = element_text(face = "bold", size = 13,
                                            hjust = 0.5, margin = margin(b = 8)))
  )

ggsave(
  file.path(OUT_DIR, "final_macro_stdev_forecasted.png"),
  stdev_grid, width = 11, height = 12, dpi = 150
)
message("✓ saved final_macro_stdev_forecasted.png")


# ═══════════════════════════════════════════════════════════════════════════════
# 2.  TIME-SERIES PLOT — per-country, multi-source
# ═══════════════════════════════════════════════════════════════════════════════

# ── Name → ISO2 map (bank_sovereign_nexus_monthly.csv uses full country names) ──────────────────────
name_to_iso2 <- c(
  "Austria" = "AT", "Belgium" = "BE", "Finland" = "FI", "France" = "FR",
  "Germany" = "DE", "Greece"  = "GR", "Ireland" = "IE", "Italy"   = "IT",
  "Netherlands" = "NL", "Portugal" = "PT", "Spain"  = "ES"
)

# ── 2a. Macro fundamentals — macro_fundamentals_interpolated.csv ──────────────
# Monthly interpolated panel. date = "YYYY-MM", country = ISO2.
# Columns: date, country, gdp_growth, inflation, debt_gdp, current_account,
#          policy_uncertainty, bank_nexus, bid_ask  (latter three may be NA early on)
macro_fund <- read_csv(
  file.path(PROC_DIR, "macro_fundamentals_forecasted_interpolated.csv"),
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%   # "YYYY-MM" → "YYYY-MM-01"
  filter(date >= DATE_START, date <= DATE_END) %>%
  select(date, country, gdp_growth, inflation, debt_gdp, current_account) %>%
  pivot_longer(c(gdp_growth, inflation, debt_gdp, current_account),
               names_to = "variable", values_to = "value") %>%
  drop_na(value)

# ── 2b. High-frequency files ──────────────────────────────────────────────────

# nexus: long, DATE = "YYYY-MM-DD", country = full name, value = nexus
nexus <- read_csv(file.path(VAR_DIR, "bank_sovereign_nexus_monthly.csv"), show_col_types = FALSE) %>%
  select(country, date = DATE, value = nexus) %>%
  mutate(
    date     = as.Date(date),
    country  = name_to_iso2[country],
    variable = "bank_nexus"
  ) %>%
  drop_na(country)

# bid_ask: wide, Date = "YYYY-MM", cols = ISO2 codes
bid_ask <- read_csv(file.path(VAR_DIR, "bid_ask_spreads_monthly.csv"), show_col_types = FALSE) %>%
  pivot_longer(-Date, names_to = "country", values_to = "value") %>%
  mutate(
    date     = as.Date(paste0(Date, "-01")),
    variable = "bid_ask"
  ) %>%
  select(date, country, value, variable)

# uncertainty: wide, Date = "YYYY-MM", cols = ISO2 codes
uncertainty <- read_csv(file.path(VAR_DIR, "policy_uncertainty_monthly.csv"), show_col_types = FALSE) %>%
  pivot_longer(-Date, names_to = "country", values_to = "value") %>%
  mutate(
    date     = as.Date(paste0(Date, "-01")),
    variable = "policy_uncertainty"
  ) %>%
  select(date, country, value, variable)

hf_all <- bind_rows(nexus, bid_ask, uncertainty) %>%
  filter(date >= DATE_START, date <= DATE_END)

# ── 2c. Combine ───────────────────────────────────────────────────────────────
ts_all <- bind_rows(macro_fund, hf_all) %>%
  mutate(country = toupper(country)) %>%
  filter(country %in% ea_countries, !is.na(value))

# ── 2d. Panel factory ────────────────────────────────────────────────────────
make_ts_panel <- function(var, label = var) {
  df <- ts_all %>% filter(variable == var, !is.na(value))
  ggplot(df, aes(date, value, colour = country, group = country)) +
    geom_line(linewidth = 0.5) +
    scale_colour_manual(
      values = pal,
      labels = country_fullnames,
      name   = NULL,
      drop   = FALSE
    ) +
    scale_x_date(date_breaks = "4 years", date_labels = "%Y",
                 limits = c(DATE_START, DATE_END), expand = c(0.01, 0)) +
    labs(title = label, x = NULL, y = NULL) +
    guides(colour = guide_legend(
      keywidth  = unit(1.4, "cm"),
      keyheight = unit(0.42, "cm"),
      override.aes = list(linewidth = 1.4)
    )) +
    theme_panel()
}

# ── 2e. PNG 1 — Macro fundamentals (2×2 + legend on right) ──────────────────
macro_vars <- tribble(
  ~var,              ~label,
  "gdp_growth",      "GDP growth",
  "inflation",       "Inflation",
  "debt_gdp",        "Debt / GDP",
  "current_account", "Current account"
)

macro_plots <- map2(macro_vars$var, macro_vars$label, make_ts_panel)

macro_final <- wrap_plots(macro_plots, ncol = 2) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = NULL,
    theme = theme(plot.title = element_text(face = "bold", size = 13,
                                            hjust = 0.5, margin = margin(b = 8)))
  ) &
  theme(
    legend.position  = "right",
    legend.text      = element_text(size = 9),
    legend.key       = element_blank(),
    legend.spacing.y = unit(0.1, "cm")
  )

ggsave(
  file.path(OUT_DIR, "final_macro_fundamentals.png"),
  macro_final, width = 14, height = 9, dpi = 150
)
message("✓ saved final_macro_fundamentals.png")

# ── 2f. PNG 2 — HF variables (1×3 + legend below, 2 rows) ───────────────────
hf_vars <- tribble(
  ~var,                  ~label,
  "policy_uncertainty",  "Policy uncertainty",
  "bank_nexus",          "Bank–sovereign nexus",
  "bid_ask",             "Bid–ask spread"
)

hf_plots <- map2(hf_vars$var, hf_vars$label, make_ts_panel)

hf_final <- wrap_plots(hf_plots, ncol = 3) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Financial & Uncertainty Variables — Time Series by Country",
    theme = theme(plot.title = element_text(face = "bold", size = 13,
                                            hjust = 0.5, margin = margin(b = 8)))
  ) &
  theme(
    legend.position  = "bottom",
    legend.text      = element_text(size = 9),
    legend.key       = element_blank(),
    legend.direction = "horizontal"
  )

ggsave(
  file.path(OUT_DIR, "final_macro_hf.png"),
  hf_final, width = 16, height = 6.5, dpi = 150
)
message("✓ saved final_macro_hf.png")