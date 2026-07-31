# table_descriptive_stats.R
# Produces a compact descriptive statistics table for all regression variables.
# Outputs: output/tables/descriptive_stats.csv   (machine-readable)
#          output/tables/descriptive_stats.tex   (LaTeX booktabs table)
#          output/tables/descriptive_stats.docx  (Word table via flextable)

library(dplyr)
library(readr)
library(tidyr)
library(flextable)
library(officer)

# Paths anchor at the repository root, located via the {here} package
# (uses the `.here` marker file; run from any working directory).
ROOT <- here::here()

# ── Settings (must match regression script) ───────────────────────────────────
START_DATE <- as.Date("2005-09-01")
END_DATE   <- as.Date("2025-12-01")

INPUT_FILE <- file.path(ROOT, "data", "processed", "moments_forecasted_interpolated.csv")
OUTPUT_DIR <- file.path(ROOT, "output", "tables")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ── Variable order and display labels ─────────────────────────────────────────
VAR_META <- tribble(
  ~col,                         ~label,                             ~role,
  "spread_stdev",               "Sovereign spread dispersion",      "Dependent",
  "gdp_growth_stdev",           "GDP growth dispersion",            "Regressor",
  "inflation_stdev",            "Inflation dispersion",             "Regressor",
  "debt_gdp_stdev",             "Debt/GDP dispersion",              "Regressor",
  "current_account_stdev",      "Current account dispersion",       "Regressor",
  "policy_uncertainty_stdev",   "Policy uncertainty dispersion",    "Regressor",
  "bank_nexus_stdev",           "Bank-sovereign nexus dispersion",  "Regressor",
  "bid_ask_stdev",              "Bid-ask spread dispersion",        "Regressor",
  "vstoxx",                     "VSTOXX (market sentiment)",        "Regressor"
)

# ── Load data ─────────────────────────────────────────────────────────────────
# vstoxx is already joined into the moments file by moments.py — no separate load needed
df <- read_csv(INPUT_FILE, show_col_types = FALSE) %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  filter(date >= START_DATE, date <= END_DATE) %>%
  select(date, all_of(VAR_META$col)) %>%
  drop_na(all_of(VAR_META$col))

cat(sprintf("Sample: %s to %s  (%d months)\n",
            format(min(df$date), "%Y-%m"),
            format(max(df$date), "%Y-%m"),
            nrow(df)))

# ── Compute statistics ────────────────────────────────────────────────────────
stats_tbl <- VAR_META %>%
  rowwise() %>%
  mutate(
    N    = sum(!is.na(df[[col]])),
    Mean = mean(df[[col]], na.rm = TRUE),
    SD   = sd(df[[col]],   na.rm = TRUE),
    Min  = min(df[[col]],  na.rm = TRUE),
    Med  = median(df[[col]], na.rm = TRUE),
    Max  = max(df[[col]],  na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(role, label, N, Mean, SD, Min, Med, Max)

print(stats_tbl, n = 20)

# ── CSV output ────────────────────────────────────────────────────────────────
write_csv(stats_tbl, file.path(OUTPUT_DIR, "descriptive_stats.csv"))
message("Saved descriptive_stats.csv")

# ── LaTeX output (booktabs) ───────────────────────────────────────────────────
fmt <- function(x, digits = 3) formatC(round(x, digits), format = "f", digits = digits)

lines <- c(
  "\\begin{table}[htbp]",
  "  \\centering",
  "  \\caption{Descriptive Statistics of Regression Variables}",
  "  \\label{tab:descriptive_stats}",
  "  \\fontsize{8}{9.5}\\selectfont",
  "  \\begin{tabular}{llrrrrrr}",
  "    \\toprule",
  "    Role & Variable & $N$ & Mean & SD & Min & Median & Max \\\\",
  "    \\midrule"
)

prev_role <- ""
for (i in seq_len(nrow(stats_tbl))) {
  r <- stats_tbl[i, ]
  role_cell <- if (r$role != prev_role) r$role else ""
  prev_role <- r$role
  row <- sprintf("    %s & %s & %d & %s & %s & %s & %s & %s \\\\",
                 role_cell,
                 r$label,
                 r$N,
                 fmt(r$Mean), fmt(r$SD), fmt(r$Min), fmt(r$Med), fmt(r$Max))
  lines <- c(lines, row)
  # rule between Dependent and Regressors
  if (r$role == "Dependent") lines <- c(lines, "    \\midrule")
}

lines <- c(lines,
           "    \\bottomrule",
           "  \\end{tabular}",
           "  \\\\[4pt]",
           "  \\raggedright\\footnotesize",
           "  \\textit{Note:} All dispersion variables are cross-sectional standard deviations",
           "  across 11 euro area countries (AT, BE, DE, ES, FI, FR, GR, IE, IT, NL, PT),",
           sprintf("  computed monthly over %s–%s.",
                   format(START_DATE, "%Y-%m"), format(END_DATE, "%Y-%m")),
           "  VSTOXX is the euro area implied volatility index.",
           "\\end{table}"
)

tex_path <- file.path(OUTPUT_DIR, "descriptive_stats.tex")
writeLines(lines, tex_path)
message("Saved descriptive_stats.tex")

# ── Word output (flextable) ───────────────────────────────────────────────────
fmt3 <- function(x) formatC(round(x, 3), format = "f", digits = 3)

word_tbl <- stats_tbl %>%
  mutate(across(c(Mean, SD, Min, Med, Max), fmt3),
         N = as.character(N))

note_text <- paste0(
  "Note: All dispersion variables are cross-sectional standard deviations across ",
  "11 euro area countries (AT, BE, DE, ES, FI, FR, GR, IE, IT, NL, PT), ",
  "computed monthly over ",
  format(START_DATE, "%Y-%m"), "–", format(END_DATE, "%Y-%m"), ". ",
  "VSTOXX is the euro area implied volatility index."
)

# Page geometry:
#   A4 width = 21 cm; left = 6 cm, right = 1.5 cm  ->  text width = 13.5 cm = 5.315 in
#   Column widths below sum to 5.31 in to fill that text block exactly.
#   label column is kept tight (1.45 in) so N follows immediately after Variable.
TEXT_WIDTH_IN <- 5.315   # inches
COL_ROLE  <- 0.62        # "Dependent" / "Regressor"
COL_LABEL <- 1.45        # variable names (wraps at 8pt for longer strings)
COL_N     <- 0.32        # integer count
COL_NUM   <- (TEXT_WIDTH_IN - COL_ROLE - COL_LABEL - COL_N) / 5  # Mean SD Min Med Max equally

ft <- flextable(word_tbl) %>%
  set_header_labels(
    role  = "Role",
    label = "Variable",
    N     = "N",
    Mean  = "Mean",
    SD    = "SD",
    Min   = "Min",
    Med   = "Median",
    Max   = "Max"
  ) %>%
  # Bold header
  bold(part = "header") %>%
  # Horizontal rules: top, below header, above footer
  hline_top(part = "header", border = fp_border(width = 1.5)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 1.5)) %>%
  # Thin rule after the Dependent row
  hline(i = 1, border = fp_border(width = 0.75, style = "dashed")) %>%
  # Column alignment: left for Role/Variable, right for numbers
  align(j = c("role", "label"), align = "left",  part = "all") %>%
  align(j = c("N", "Mean", "SD", "Min", "Med", "Max"), align = "right", part = "all") %>%
  # Column widths (sum = TEXT_WIDTH_IN)
  width(j = "role",                                    width = COL_ROLE) %>%
  width(j = "label",                                   width = COL_LABEL) %>%
  width(j = "N",                                       width = COL_N) %>%
  width(j = c("Mean", "SD", "Min", "Med", "Max"),      width = COL_NUM) %>%
  # Tight cell padding to save vertical space
  padding(padding.top = 1, padding.bottom = 1, padding.left = 3, padding.right = 3, part = "all") %>%
  # Font: 8pt body
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  # Caption above table
  set_caption(
    caption = "Descriptive Statistics of Regression Variables",
    style   = "Table Caption"
  ) %>%
  # Footnote below table
  add_footer_lines(note_text) %>%
  italic(part = "footer") %>%
  font(fontname = "Times New Roman", part = "footer") %>%
  fontsize(size = 7, part = "footer")

# Page margins: left 6 cm, right/top/bottom 1.5 cm (all in inches)
page_props <- prop_section(
  page_size = page_size(width = 8.268, height = 11.693, orient = "portrait"),  # A4
  page_margins = page_mar(
    left   = 2.362,   # 6.0 cm
    right  = 0.591,   # 1.5 cm
    top    = 0.787,   # 2.0 cm  (to first text line)
    bottom = 0.591,   # 1.5 cm
    header = 0.394,
    footer = 0.394,
    gutter = 0
  )
)

doc <- read_docx() %>%
  body_add_flextable(ft)
doc <- body_set_default_section(doc, page_props)

docx_path <- file.path(OUTPUT_DIR, "descriptive_stats.docx")
print(doc, target = docx_path)
message("Saved descriptive_stats.docx")