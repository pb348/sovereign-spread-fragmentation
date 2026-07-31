# Fig3_TableA1.R
# Fig 3  : Non-fundamental component of sovereign spread dispersion over time
# Table A1: Model 1 RMSE summary as a Word (.docx) table
#
# Reads from:  output/regression/model1_rolling/rolling_regression_results_forecasted.csv
#              output/regression/model1_rolling/rmse_summary.csv
# Outputs to:  output/figures/fig3_nonfundamental_component.png
#              output/tables/tableA1_rmse.docx

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(flextable)
library(officer)

# Paths anchor at the repository root, located via the {here} package
# (uses the `.here` marker file; run from any working directory).
ROOT <- here::here()
IN_DIR  <- file.path(ROOT, "output", "regression", "model1_rolling")
FIG_DIR <- file.path(ROOT, "output", "figures")
TBL_DIR <- file.path(ROOT, "output", "tables")

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 3 — Non-fundamental component
# ══════════════════════════════════════════════════════════════════════════════
roll <- read_csv(
  file.path(IN_DIR, "rolling_regression_results_forecasted.csv"),
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(non_fundamental_component))

# Separate positive and negative runs for two-tone shading
roll <- roll %>%
  mutate(
    nfc_pos = pmax(non_fundamental_component, 0),
    nfc_neg = pmin(non_fundamental_component, 0)
  )

p3 <- ggplot(roll, aes(x = date)) +
  # Zero reference
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  # Bars: positive (excess fragmentation) in salmon, negative in blue
  geom_col(aes(y = nfc_pos), fill = "#f4a582", width = 28) +
  geom_col(aes(y = nfc_neg), fill = "#92c5de", width = 28) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(
    x = NULL,
    y = "Non-fundamental component (spread_stdev)"
  ) +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey88"),
    axis.text          = element_text(size = 9),
    plot.margin        = margin(10, 16, 8, 10)
  )

ggsave(file.path(FIG_DIR, "fig3_nonfundamental_component.png"),
       p3, width = 9, height = 4.5, dpi = 150)
message("Saved fig3_nonfundamental_component.png")

# ══════════════════════════════════════════════════════════════════════════════
# TABLE A1 — RMSE summary (Model 1, rolling window OLS)
# ══════════════════════════════════════════════════════════════════════════════
rmse_raw <- read_csv(file.path(IN_DIR, "rmse_summary.csv"), show_col_types = FALSE)

rmse_tbl <- data.frame(
  Metric = c(
    "Stage 1 RMSE (fitted vs. actual)",
    "Stage 2 RMSE (fundamentals-implied vs. actual)",
    "Stage 2 RMSE (fitted vs. fundamentals-implied)",
    "Number of rolling windows",
    "Number of estimated parameters"
  ),
  Value = c(
    formatC(rmse_raw$rmse_fitted_vs_actual,   digits = 4, format = "f"),
    formatC(rmse_raw$rmse_tilde_vs_actual,    digits = 4, format = "f"),
    formatC(rmse_raw$rmse_tilde_vs_fitted,    digits = 4, format = "f"),
    as.character(rmse_raw$n_windows),
    as.character(rmse_raw$n_params)
  )
)

note_text <- paste0(
  "Note: Stage 1 is the full model (MS_fitted); Stage 2 is the prediction under ",
  "normal market conditions (S_t = S̅, i.e. sentiment at its full-sample mean). ",
  "Window length: 60 months. Sample: 2005-09 to 2025-12."
)

TEXT_WIDTH_IN <- 5.315
COL_METRIC <- 3.8
COL_VALUE  <- TEXT_WIDTH_IN - COL_METRIC

ft_a1 <- flextable(rmse_tbl) %>%
  set_header_labels(Metric = "Metric", Value = "Value") %>%
  bold(part = "header") %>%
  hline_top(part = "header", border = fp_border(width = 1.5)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 1.5)) %>%
  # thin rule before the count rows
  hline(i = 3, border = fp_border(width = 0.75, style = "dashed")) %>%
  align(j = "Metric", align = "left",  part = "all") %>%
  align(j = "Value",  align = "right", part = "all") %>%
  width(j = "Metric", width = COL_METRIC) %>%
  width(j = "Value",  width = COL_VALUE) %>%
  padding(padding.top = 1, padding.bottom = 1,
          padding.left = 3, padding.right = 3, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  set_caption(
    caption = "Table A1: Rolling Window OLS — Model Fit Statistics",
    style   = "Table Caption"
  ) %>%
  add_footer_lines(note_text) %>%
  italic(part = "footer") %>%
  font(fontname = "Times New Roman", part = "footer") %>%
  fontsize(size = 7, part = "footer")

page_props <- prop_section(
  page_size = page_size(width = 8.268, height = 11.693, orient = "portrait"),
  page_margins = page_mar(
    left = 2.362, right = 0.591, top = 0.787, bottom = 0.591,
    header = 0.394, footer = 0.394, gutter = 0
  )
)

doc_a1 <- read_docx() %>% body_add_flextable(ft_a1)
doc_a1 <- body_set_default_section(doc_a1, page_props)
print(doc_a1, target = file.path(TBL_DIR, "tableA1_rmse.docx"))
message("Saved tableA1_rmse.docx")