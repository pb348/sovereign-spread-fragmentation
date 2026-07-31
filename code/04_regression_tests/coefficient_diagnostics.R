# TableA2.R
# Table A2: Rolling window OLS — coefficient summary (mean, SD, min, max)
#           across all 182 rolling windows (Model 1).
#
# Reads:   output/regression/model1_rolling/coefficient_summary.csv
# Outputs: output/tables/tableA2_coefficients.docx
#          output/figures/coefficients_over_time.png

library(dplyr)
library(readr)
library(flextable)
library(officer)

# Paths anchor at the repository root, located via the {here} package
# (uses the `.here` marker file; run from any working directory).
ROOT <- here::here()
IN_DIR  <- file.path(ROOT, "output", "regression", "model1_rolling")  # model-1 results to summarize
OUT_DIR <- file.path(ROOT, "output", "tables")
FIG_DIR <- file.path(ROOT, "output", "figures")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ── Load and tidy ─────────────────────────────────────────────────────────────
coef_raw <- read_csv(file.path(IN_DIR, "coefficient_summary.csv"),
                     show_col_types = FALSE)

# Map internal names to display labels and group by coefficient type
coef_meta <- data.frame(
  coefficient = c(
    "alpha",
    "beta_gdp_growth_stdev",
    "beta_inflation_stdev",
    "beta_debt_gdp_stdev",
    "beta_current_account_stdev",
    "beta_policy_uncertainty_stdev",
    "beta_bank_nexus_stdev",
    "beta_bid_ask_stdev",
    "gamma",
    "lambda_gdp_growth_stdev",
    "lambda_inflation_stdev",
    "lambda_debt_gdp_stdev",
    "lambda_current_account_stdev",
    "lambda_policy_uncertainty_stdev",
    "lambda_bank_nexus_stdev",
    "lambda_bid_ask_stdev"
  ),
  group = c(
    "Intercept",
    rep("Fundamental (\\u03b2)", 7),
    "Sentiment (\\u03b3)",
    rep("Interaction (\\u03bb)", 7)
  ),
  label = c(
    "Intercept (α)",
    "GDP growth dispersion (β₁)",
    "Inflation dispersion (β₂)",
    "Debt/GDP dispersion (β₃)",
    "Current account dispersion (β₄)",
    "Policy uncertainty dispersion (β₅)",
    "Bank-sovereign nexus dispersion (β₆)",
    "Bid-ask spread dispersion (β₇)",
    "VSTOXX — market sentiment (γ)",
    "GDP growth dispersion (λ₁)",
    "Inflation dispersion (λ₂)",
    "Debt/GDP dispersion (λ₃)",
    "Current account dispersion (λ₄)",
    "Policy uncertainty dispersion (λ₅)",
    "Bank-sovereign nexus dispersion (λ₆)",
    "Bid-ask spread dispersion (λ₇)"
  ),
  stringsAsFactors = FALSE
)

fmt4 <- function(x) formatC(round(x, 4), format = "f", digits = 4)

tbl <- coef_meta %>%
  left_join(coef_raw, by = "coefficient") %>%
  mutate(
    Group = group,
    Variable = label,
    Mean  = fmt4(mean),
    SD    = fmt4(sd),
    Min   = fmt4(min),
    Max   = fmt4(max)
  ) %>%
  select(Group, Variable, Mean, SD, Min, Max)

# ── Flextable ─────────────────────────────────────────────────────────────────
TEXT_WIDTH_IN <- 5.315
COL_GROUP  <- 0.85
COL_VAR    <- 2.10
COL_NUM    <- (TEXT_WIDTH_IN - COL_GROUP - COL_VAR) / 4   # Mean SD Min Max

# Row indices where group changes (for thin separator lines)
group_change_rows <- which(tbl$Group != lag(tbl$Group, default = ""))
group_change_rows <- group_change_rows[group_change_rows > 1] - 1

note_text <- paste0(
  "Note: Each cell reports the statistic of the time-varying coefficient across all ",
  "182 rolling windows (window length = 60 months, sample 2005-09 to 2025-12). ",
  "β coefficients are the marginal effects of macro-fundamental dispersion on spread ",
  "dispersion at average market sentiment (S̅). ",
  "γ is the direct sentiment coefficient. ",
  "λ coefficients capture the interaction between fundamentals and sentiment deviations."
)

ft_a2 <- flextable(tbl) %>%
  set_header_labels(
    Group    = "Type",
    Variable = "Variable",
    Mean     = "Mean",
    SD       = "SD",
    Min      = "Min",
    Max      = "Max"
  ) %>%
  bold(part = "header") %>%
  hline_top(part = "header", border = fp_border(width = 1.5)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 1.5)) %>%
  # thin separator between groups
  hline(i = group_change_rows, border = fp_border(width = 0.5, style = "dashed")) %>%
  # merge group cells vertically
  merge_v(j = "Group") %>%
  valign(j = "Group", valign = "top", part = "body") %>%
  align(j = c("Group","Variable"), align = "left",  part = "all") %>%
  align(j = c("Mean","SD","Min","Max"), align = "right", part = "all") %>%
  width(j = "Group",                    width = COL_GROUP) %>%
  width(j = "Variable",                 width = COL_VAR) %>%
  width(j = c("Mean","SD","Min","Max"), width = COL_NUM) %>%
  padding(padding.top = 1, padding.bottom = 1,
          padding.left = 3, padding.right = 3, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 8,  part = "all") %>%
  set_caption(
    caption = "Table A2: Rolling Window OLS — Coefficient Summary Statistics",
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

doc_a2 <- read_docx() %>% body_add_flextable(ft_a2)
doc_a2 <- body_set_default_section(doc_a2, page_props)
print(doc_a2, target = file.path(OUT_DIR, "tableA2_coefficients.docx"))
message("Saved tableA2_coefficients.docx")

# ============================================================
# Coefficients over time (uses the full panel, not the summary
# stats above, since Mean/SD/Min/Max alone can't show a time path)
# ============================================================
library(ggplot2)
library(tidyr)

PANEL_CSV      <- file.path(IN_DIR, "rolling_regression_results_forecasted.csv")
OUTPUT_COEF_PLOT <- file.path(FIG_DIR, "coefficients_over_time.png")
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

panel_df <- read_csv(PANEL_CSV, show_col_types = FALSE)

coef_long <- panel_df %>%
  filter(!is.na(alpha)) %>%
  select(date, all_of(coef_meta$coefficient)) %>%
  pivot_longer(-date, names_to = "coefficient", values_to = "value") %>%
  left_join(coef_meta, by = "coefficient") %>%
  mutate(label = factor(label, levels = coef_meta$label))

p_coef <- ggplot(coef_long, aes(date, value)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_line(colour = "#0072B2", linewidth = 0.4) +
  facet_wrap(~ label, scales = "free_y", ncol = 4) +
  labs(title = "Rolling 60-Month Regression: Coefficients Over Time",
       x = NULL, y = NULL) +
  theme_classic(base_size = 9) +
  theme(plot.title = element_text(face = "bold", size = 12),
        strip.text = element_text(size = 7.5))

ggsave(OUTPUT_COEF_PLOT, plot = p_coef, width = 14, height = 9, dpi = 150)
message("Saved ", OUTPUT_COEF_PLOT)