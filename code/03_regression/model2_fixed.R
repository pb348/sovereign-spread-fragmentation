library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(purrr)
library(zoo)
library(ggplot2)

# Paths anchor at the repository root, located via the {here} package
# (uses the `.here` marker file; run from any working directory).
ROOT <- here::here()
FIG_DIR <- file.path(ROOT, "output", "figures")

# ============================================================
# Fixed-parameter Model 2: Kakes & Van den End (2024) + P_{t-1}
#
# Extends Fixed-Regression.R with one-month-lagged ECB sovereign
# bond purchases (SMP + PSPP + PEPP). Coefficients are estimated
# ONCE over FIXED_FIT_START/END and applied to every period.
#
# Stage 1 (fitted once):
#   MS_t = alpha + beta' MF_t + gamma S_t
#          + lambda' (MF_t - MF_bar)(S_t - S_bar)
#          + delta P_{t-1} + eps_t
#
# Stage 2 produces two predictions at S_t = S_bar:
#   MS_tilde         : alpha + beta' MF_t + delta P_{t-1}
#   MS_tilde_nopurch : alpha + beta' MF_t
#
# Lag choice: k = 1 month. See lag1_justification.md for full
# rationale (endogeneity correction, transmission lag, RMSE evidence).
# ============================================================

# ============================================================
# Settings  (mirrors Fixed-Regression.R)
# ============================================================
START_DATE   <- as.Date("2005-09-01")
END_DATE     <- as.Date("2025-12-01")

FIXED_FIT_START <- as.Date("2005-09-01")
FIXED_FIT_END   <- as.Date("2025-12-01")

NORMAL_REF_START <- as.Date("2005-09-01")
NORMAL_REF_END   <- as.Date("2022-12-01")

P_LAG <- 1   # one-month lag on purchases (see lag1_justification.md)

INPUT_FILE     <- file.path(ROOT, "data", "processed", "moments_forecasted_interpolated.csv")
PURCHASES_FILE <- file.path(ROOT, "data", "variables", "ecb_sovereign_purchases_monthly.csv")

OUTPUT_DIR  <- file.path(ROOT, "output", "regression", "model2_fixed")
dir.create(OUTPUT_DIR,          showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

OUTPUT_CSV   <- file.path(OUTPUT_DIR, "fixed_regression_model2_results.csv")
OUTPUT_PLOT  <- file.path(FIG_DIR, "predicted_vs_actual_fixed_model2.png")
OUTPUT_DECOMP_PLOT <- file.path(FIG_DIR, "decomposition_fixed_model2.png")

# ============================================================
# Variable selection  (mirrors Fixed-Regression.R)
# ============================================================
MF_VARS <- c(
  "gdp_growth_stdev",
  "inflation_stdev",
  "debt_gdp_stdev",
  "current_account_stdev",
  "policy_uncertainty_stdev",
  "bank_nexus_stdev",
  "bid_ask_stdev"
)

INTERACT_VARS <- MF_VARS

MS_VAR <- "spread_stdev"

DEMEAN_MF_IN_INTERACTION <- TRUE
stopifnot(all(INTERACT_VARS %in% MF_VARS))

# ============================================================
# Load data
# ============================================================
moments <- read_csv(INPUT_FILE, show_col_types = FALSE) %>%
  mutate(date = as.Date(paste0(date, "-01")))

vstoxx <- read_csv(file.path(ROOT, "data", "variables", "vstoxx_monthly.csv"), show_col_types = FALSE) %>%
  mutate(date = as.Date(paste0(date, "-01")))

# P_{t-1}: shift purchase dates forward by P_LAG months so that after
# left_join on date, column P_t_bn at row t holds purchases from t - P_LAG.
purchases_raw <- read_csv(PURCHASES_FILE, show_col_types = FALSE) %>%
  mutate(date = as.Date(date) %m+% months(P_LAG)) %>%
  transmute(date, P_t_bn = total_sovereign_mn / 1000)

df <- moments %>%
  select(date, all_of(MS_VAR), all_of(MF_VARS)) %>%
  left_join(vstoxx,        by = "date") %>%
  left_join(purchases_raw, by = "date") %>%
  arrange(date) %>%
  mutate(P_t_bn = replace_na(P_t_bn, 0)) %>%
  filter(date >= START_DATE, date <= END_DATE) %>%
  drop_na(all_of(c(MS_VAR, MF_VARS, "vstoxx")))

dates <- df$date
n_obs <- nrow(df)
n_mf  <- length(MF_VARS)

# ============================================================
# Reference point (S_bar, MF_bar)
# ============================================================
df_ref <- df %>% filter(date >= NORMAL_REF_START, date <= NORMAL_REF_END)
S_bar  <- mean(df_ref$vstoxx, na.rm = TRUE)
MF_bar <- colMeans(df_ref[, MF_VARS], na.rm = TRUE)

message(sprintf("S_bar (vstoxx, %s to %s) = %.3f", NORMAL_REF_START, NORMAL_REF_END, S_bar))
message(sprintf("P_{t-%d} descriptives (active months): mean = %.1f EUR bn, sd = %.1f",
                P_LAG,
                mean(df$P_t_bn[df$P_t_bn != 0]),
                sd(df$P_t_bn[df$P_t_bn != 0])))

# ============================================================
# Fixed Stage 1 fit  (estimated once over FIXED_FIT_START/END)
# ============================================================
fit_df <- df %>% filter(date >= FIXED_FIT_START, date <= FIXED_FIT_END)

if (DEMEAN_MF_IN_INTERACTION) {
  inter_fit <- sweep(as.matrix(fit_df[, INTERACT_VARS, drop = FALSE]), 2,
                     MF_bar[INTERACT_VARS], FUN = "-") *
    (fit_df$vstoxx - S_bar)
} else {
  inter_fit <- as.matrix(fit_df[, INTERACT_VARS, drop = FALSE]) *
    (fit_df$vstoxx - S_bar)
}
colnames(inter_fit) <- paste0("inter_", INTERACT_VARS)

reg_data <- fit_df %>%
  select(all_of(MS_VAR), all_of(MF_VARS), vstoxx, P_t_bn) %>%
  bind_cols(as.data.frame(inter_fit))

predictor_cols <- c(MF_VARS, "vstoxx", "P_t_bn", colnames(inter_fit))

form <- as.formula(paste0(
  MS_VAR, " ~ ",
  paste(MF_VARS, collapse = " + "),
  " + vstoxx + P_t_bn + ",
  paste(colnames(inter_fit), collapse = " + ")
))

fit <- lm(form, data = reg_data)
cf  <- coef(fit)

alpha_fixed  <- cf["(Intercept)"]
beta_fixed   <- cf[MF_VARS]
gamma_fixed  <- cf["vstoxx"]
delta_fixed  <- cf["P_t_bn"]

lambda_fixed <- setNames(rep(0, n_mf), MF_VARS)
lambda_fixed[INTERACT_VARS] <- cf[paste0("inter_", INTERACT_VARS)]

cat("\n=== Fixed Stage 1 coefficients (Model 2, P_LAG =", P_LAG, ") ===\n")
cat(sprintf("alpha = %.4f\n", alpha_fixed))
cat(sprintf("delta = %.6f  (EUR bn per unit spread_stdev)\n", delta_fixed))
print(round(beta_fixed,   4))
cat(sprintf("gamma = %.4f\n", gamma_fixed))
print(round(lambda_fixed, 4))
cat("\n")

# ============================================================
# Apply fixed coefficients to every period
# ============================================================
MS_actual        <- df[[MS_VAR]]
MS_fitted        <- rep(NA_real_, n_obs)
MS_tilde         <- rep(NA_real_, n_obs)   # S = S_bar, P at actual
MS_tilde_nopurch <- rep(NA_real_, n_obs)   # S = S_bar, P = 0
purchase_effect  <- rep(NA_real_, n_obs)

for (i in seq_len(n_obs)) {
  current <- df[i, ]
  MF_t <- as.numeric(current[, MF_VARS])
  S_t  <- current$vstoxx
  P_t  <- current$P_t_bn
  
  if (DEMEAN_MF_IN_INTERACTION) {
    inter_t <- (MF_t - MF_bar) * (S_t - S_bar)
  } else {
    inter_t <- MF_t * (S_t - S_bar)
  }
  names(inter_t) <- MF_VARS
  
  # Stage 1: full model
  MS_fitted[i] <- alpha_fixed +
    sum(beta_fixed * MF_t) +
    gamma_fixed * S_t +
    delta_fixed * P_t +
    sum(lambda_fixed[INTERACT_VARS] * inter_t[INTERACT_VARS])
  
  # Stage 2a: S = S_bar, P at actual
  MS_tilde[i]         <- alpha_fixed + sum(beta_fixed * MF_t) + delta_fixed * P_t
  
  # Stage 2b: S = S_bar, P = 0 (pure fundamentals)
  MS_tilde_nopurch[i] <- alpha_fixed + sum(beta_fixed * MF_t)
  
  purchase_effect[i]  <- delta_fixed * P_t
}

# ============================================================
# RMSE
# ============================================================
rmse <- function(a, b) sqrt(mean((a - b)^2, na.rm = TRUE))
cat("=== RMSE ===\n")
cat(sprintf("Stage 1 (fitted vs actual)          : %.4f\n", rmse(MS_actual, MS_fitted)))
cat(sprintf("Stage 2 with purchases vs actual    : %.4f\n", rmse(MS_actual, MS_tilde)))
cat(sprintf("Stage 2 no purchases vs actual      : %.4f\n", rmse(MS_actual, MS_tilde_nopurch)))
cat("\n")

# ============================================================
# Assemble output
# ============================================================
coef_row <- c(
  alpha = alpha_fixed,
  setNames(beta_fixed,   paste0("beta_", MF_VARS)),
  gamma = gamma_fixed,
  delta = delta_fixed,
  setNames(lambda_fixed, paste0("lambda_", MF_VARS))
)

coef_block <- as.data.frame(matrix(coef_row, nrow = n_obs, ncol = length(coef_row),
                                   byrow = TRUE))
colnames(coef_block) <- names(coef_row)

out <- data.frame(
  date               = dates,
  P_t_bn             = df$P_t_bn,
  MS_actual          = MS_actual,
  MS_fitted          = MS_fitted,
  MS_tilde           = MS_tilde,
  MS_tilde_nopurch   = MS_tilde_nopurch,
  purchase_effect    = purchase_effect,
  fundamental_component  = MS_tilde_nopurch,
  purchase_component     = purchase_effect,
  sentiment_component    = MS_fitted - MS_tilde,
  non_fundamental_component = MS_fitted - MS_tilde  # alias for compatibility
) %>% bind_cols(coef_block)

write_csv(out, OUTPUT_CSV)
message("Saved ", OUTPUT_CSV)

# Annual decomposition
decomp <- out %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    MS_actual             = mean(MS_actual,             na.rm = TRUE),
    MS_fitted             = mean(MS_fitted,             na.rm = TRUE),
    fundamental_component = mean(fundamental_component, na.rm = TRUE),
    purchase_component    = mean(purchase_component,    na.rm = TRUE),
    sentiment_component   = mean(sentiment_component,   na.rm = TRUE),
    P_t_bn_mean           = mean(P_t_bn,                na.rm = TRUE),
    .groups = "drop"
  )

write_csv(decomp, file.path(OUTPUT_DIR, "fixed_model2_decomposition.csv"))
cat("\n=== Annual decomposition ===\n")
print(as.data.frame(decomp))

# ============================================================
# Plot 1: Actual vs Stage 1 fitted vs Stage 2 series
# ============================================================
lbl_actual   <- "Actual"
lbl_fitted   <- sprintf("Fitted (fixed params, P_{t-%d})", P_LAG)
lbl_tilde    <- "Fundamentals + purchases (S = S̅)"
lbl_nopurch  <- "Pure fundamentals (S = S̅, P = 0)"

plot_df <- out %>%
  select(date, MS_actual, MS_fitted, MS_tilde, MS_tilde_nopurch) %>%
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  mutate(series = case_when(
    series == "MS_actual"        ~ lbl_actual,
    series == "MS_fitted"        ~ lbl_fitted,
    series == "MS_tilde"         ~ lbl_tilde,
    series == "MS_tilde_nopurch" ~ lbl_nopurch,
    TRUE ~ series
  ))

pal <- setNames(
  c("black", "#D55E00", "#009E73", "#0072B2"),
  c(lbl_actual, lbl_fitted, lbl_tilde, lbl_nopurch)
)
lty <- setNames(
  c("solid", "solid", "dashed", "dashed"),
  c(lbl_actual, lbl_fitted, lbl_tilde, lbl_nopurch)
)

p1 <- ggplot(plot_df, aes(date, value, colour = series, linetype = series)) +
  geom_hline(yintercept = c(0, 3, 6, 9), colour = "grey85", linewidth = 0.3) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  scale_colour_manual(values = pal) +
  scale_linetype_manual(values = lty) +
  labs(
    title    = sprintf("Sovereign Spread Dispersion: Fixed-Parameter Model 2 (P_{t-%d})", P_LAG),
    subtitle = sprintf("Coefficients estimated once over %s to %s",
                       FIXED_FIT_START, FIXED_FIT_END),
    x = NULL, y = "spread_stdev", colour = NULL, linetype = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 9, colour = "grey40"))

ggsave(OUTPUT_PLOT, plot = p1, width = 11, height = 5, dpi = 150)
message("Saved ", OUTPUT_PLOT)

# ============================================================
# Plot 2: Stacked area decomposition
# ============================================================
decomp_long <- out %>%
  select(date, fundamental_component, purchase_component, sentiment_component) %>%
  pivot_longer(-date, names_to = "component", values_to = "value") %>%
  mutate(component = case_when(
    component == "fundamental_component" ~ "Fundamental",
    component == "purchase_component"    ~ sprintf("ECB purchases (delta * P_{t-%d})", P_LAG),
    component == "sentiment_component"   ~ "Non-fundamental / sentiment",
    TRUE ~ component
  ),
  component = factor(component, levels = c(
    "Fundamental",
    sprintf("ECB purchases (delta * P_{t-%d})", P_LAG),
    "Non-fundamental / sentiment"
  )))

actual_line <- out %>% select(date, MS_actual)

pal_decomp <- setNames(
  c("#0072B2", "#009E73", "#D55E00"),
  c("Fundamental",
    sprintf("ECB purchases (delta * P_{t-%d})", P_LAG),
    "Non-fundamental / sentiment")
)

p2 <- ggplot(decomp_long, aes(date, value, fill = component)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_area(alpha = 0.7, position = "stack") +
  geom_line(data = actual_line, aes(date, MS_actual),
            inherit.aes = FALSE, colour = "black", linewidth = 0.8) +
  scale_fill_manual(values = pal_decomp) +
  labs(
    title    = "Decomposition of Sovereign Spread Dispersion (Fixed Model 2)",
    subtitle = "Black line = actual. Stacked areas = fixed-coefficient decomposition.",
    x = NULL, y = "spread_stdev", fill = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 9, colour = "grey40"))

ggsave(OUTPUT_DECOMP_PLOT, plot = p2, width = 11, height = 5, dpi = 150)
message("Saved ", OUTPUT_DECOMP_PLOT)
cat(sprintf("Fixed model R-squared: %.4f\n", summary(fit)$r.squared))
cat(sprintf("Adj. R-squared (Model 2): %.4f\n", summary(fit)$adj.r.squared))
cat(sprintf("delta t-stat: %.3f  p-value: %.4f\n",
            summary(fit)$coefficients["P_t_bn", "t value"],
            summary(fit)$coefficients["P_t_bn", "Pr(>|t|)"]))

message("\nAll Fixed Model 2 outputs written to '", OUTPUT_DIR, "/'.")
cat(sprintf("Fixed Stage 1 RMSE: %.4f   Stage 2 RMSE: %.4f\n",
            sqrt(mean(residuals(fit)^2)),
            sqrt(mean((out$MS_actual - out$fundamental_component)^2, na.rm=TRUE))))