# plot_stdev_gallery.R
# Combines plot_macro_panels.R (cross-country stdev panels) and plot-vstoxx.R
# into a single 2-column x 4-row gallery (8 panels), styled like the attached figure.
# Output: final_stdev_gallery.png
# Range:  2005-09-01 to 2025-12-01

library(tidyverse)
library(lubridate)
library(patchwork)

# Paths anchor at the repository root, located via the {here} package
# (uses the `.here` marker file; run from any working directory).
ROOT <- here::here()

# ── Paths ─────────────────────────────────────────────────────────────────────
PROC_DIR <- file.path(ROOT, "data", "processed")
VAR_DIR  <- file.path(ROOT, "data", "variables")
OUT_DIR  <- file.path(ROOT, "output", "figures")

DATE_START <- as.Date("2005-09-01")
DATE_END   <- as.Date("2025-12-01")

LINE_COL <- "#2166ac"

# ── Shared theme (from plot_macro_panels.R) ──────────────────────────────────
theme_panel <- function() {
  theme_bw(base_size = 9) +
    theme(
      plot.title       = element_text(hjust = 0.5, size = 9, face = "plain"),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(size = 7),
      axis.text.y      = element_text(size = 7),
      plot.margin      = margin(4, 6, 2, 4)
    )
}

x_scale <- scale_x_date(date_breaks = "4 years", date_labels = "%Y",
                        limits = c(DATE_START, DATE_END), expand = c(0.01, 0))

# ═══════════════════════════════════════════════════════════════════════════════
# 1.  Cross-country STDEV panels — moments_forecasted_interpolated.csv
# ═══════════════════════════════════════════════════════════════════════════════
moments <- read_csv(
  file.path(PROC_DIR, "moments_forecasted_interpolated.csv"),
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= DATE_START, date <= DATE_END)

# order = row-major fill at ncol = 2 (matches the attached layout, + 2 extra rows)
# divide  = rescale factor (debt ratio & current account are in pp -> /100 to show
#           as fractions, e.g. 25 -> 0.25, 3 -> 0.03)
# ybreaks = explicit y-axis breaks (waiver() = automatic)
stdev_spec <- tibble(
  col = c("debt_gdp_stdev", "gdp_growth_stdev", "inflation_stdev",
          "current_account_stdev", "policy_uncertainty_stdev",
          "bank_nexus_stdev", "bid_ask_stdev"),
  label = c("Debt ratio (stdev)", "GDP growth (stdev)", "Inflation (stdev)",
            "Current account balance (stdev)", "Policy uncertainty (stdev)",
            "Sovereign/bank nexus (stdev)", "Bid-ask spread (stdev)"),
  divide = c(100, 1, 1, 100, 1, 1, 1),
  ybreaks = list(seq(0.25, 0.50, 0.05), waiver(), waiver(),
                 seq(0.03, 0.07, 0.01), waiver(), waiver(), waiver())
)

make_stdev_panel <- function(col, label, divide = 1, ybreaks = waiver()) {
  df <- moments %>% transmute(date, value = .data[[col]] / divide) %>% drop_na()
  ggplot(df, aes(date, value)) +
    geom_line(colour = LINE_COL, linewidth = 0.45) +
    labs(title = label, x = NULL, y = NULL) +
    x_scale +
    scale_y_continuous(breaks = ybreaks) +
    theme_panel()
}

stdev_plots <- pmap(stdev_spec, make_stdev_panel)

# ═══════════════════════════════════════════════════════════════════════════════
# 2.  VSTOXX panel (level) — vstoxx_monthly.csv  (8th panel)
# ═══════════════════════════════════════════════════════════════════════════════
vstoxx <- read_csv(
  file.path(VAR_DIR, "vstoxx_monthly.csv"),
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  filter(date >= DATE_START, date <= DATE_END)

vstoxx_panel <- ggplot(vstoxx, aes(date, vstoxx)) +
  geom_line(colour = LINE_COL, linewidth = 0.45) +
  labs(title = "VSTOXX (level)", x = NULL, y = NULL) +
  x_scale +
  theme_panel()

# ═══════════════════════════════════════════════════════════════════════════════
# 3.  Assemble 2 x 4 gallery
# ═══════════════════════════════════════════════════════════════════════════════
all_plots <- c(stdev_plots, list(vstoxx_panel))

gallery <- wrap_plots(all_plots, ncol = 2) +
  plot_annotation(
    title = "Cross-Country Standard Deviation by Variable, and VSTOXX",
    theme = theme(plot.title = element_text(face = "bold", size = 13,
                                            hjust = 0.5, margin = margin(b = 8)))
  )

ggsave(
  file.path(OUT_DIR, "final_stdev_gallery.png"),
  gallery, width = 9.5, height = 12.5, dpi = 150
)
message("✓ saved final_stdev_gallery.png")