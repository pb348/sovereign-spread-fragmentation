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
# ============================================================
# Settings
# ============================================================
WINDOW       <- 60
START_DATE   <- as.Date("2005-09-01")
END_DATE     <- as.Date("2025-12-01")

# NORMAL_REF_START / NORMAL_REF_END: the "normal conditions" reference
# period used to compute FULL_S_BAR / FULL_MF_BAR (when FIXED_S_BAR /
# FIXED_MF_BAR = TRUE). Per the paper's definition (p.12), S_bar is "the
# average market sentiment in 2005-2022" - adjust this window to test
# sensitivity to the choice of reference period.
NORMAL_REF_START <- as.Date("2005-09-01")
NORMAL_REF_END   <- as.Date("2025-12-01")

INPUT_FILE   <- file.path(ROOT, "data", "processed", "moments_forecasted_interpolated.csv")

# OUTPUT_DIR: all regression outputs (coefficients, RMSE, fitted series,
# coefficient stability diagnostics, plot) are written here.
OUTPUT_DIR   <- file.path(ROOT, "output", "regression", "model1_rolling")
FIG_DIR      <- file.path(ROOT, "output", "figures")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

OUTPUT_CSV   <- file.path(OUTPUT_DIR, "rolling_regression_results_forecasted.csv")
OUTPUT_COEF_SUMMARY <- file.path(OUTPUT_DIR, "coefficient_summary.csv")
OUTPUT_RMSE  <- file.path(OUTPUT_DIR, "rmse_summary.csv")
OUTPUT_PLOT  <- file.path(FIG_DIR, "predicted_vs_actual_spread_forecasted.png")

# ============================================================
# Variable selection
# ============================================================
# MF_VARS: macro-fundamental variables included as level regressors (beta_*).
# Comment/uncomment lines to add or remove variables from the model.
MF_VARS <- c(
  "gdp_growth_stdev",
  "inflation_stdev",
  "debt_gdp_stdev",
  "current_account_stdev",
  "policy_uncertainty_stdev",
  "bank_nexus_stdev",
  "bid_ask_stdev"
)

# INTERACT_VARS: subset of MF_VARS that also get a demeaned interaction term
# with vstoxx (lambda_*). This is the main lever for reducing the parameter
# count (each variable here adds +1 parameter: lambda_<var>).
#
# Full model (paper-style, 16 params on 60 obs):
#   INTERACT_VARS <- MF_VARS
#
# Reduced model (core De Grauwe & Ji fundamentals only, 12 params):
INTERACT_VARS <- c(
  "gdp_growth_stdev",
  "inflation_stdev",
  "debt_gdp_stdev",
  "current_account_stdev",
  "policy_uncertainty_stdev",
  "bank_nexus_stdev",
  "bid_ask_stdev"
)

MS_VAR  <- "spread_stdev"

# FIXED_S_BAR: if TRUE, S_bar (= vstoxx mean) used to build the demeaned
# interaction terms is computed ONCE over the full estimation sample
# (2005-2022 in the paper's case) and held fixed for every rolling window.
# This matches the paper's definition (p.12): "S_bar [is] the average
# market sentiment in 2005-2022, reflecting normal financial market
# circumstances" - i.e. a single fixed reference point for "normal
# conditions", not a window-specific one.
#
# If FALSE, S_bar is recomputed within each 60-month window (the original
# behaviour) - "normal" then drifts across windows.
FIXED_S_BAR <- TRUE

# FIXED_MF_BAR: if TRUE, MF_bar (= MF_VARS means) is also fixed at the
# full-sample average. NOTE: unlike S_bar, the paper does not actually
# require this - MF_bar's only role is as a centering constant for the
# interaction term (to reduce collinearity between beta_j and lambda_j
# regressors), and a WINDOW-SPECIFIC MF_bar centers more effectively for
# that window. Fixing MF_bar empirically made coefficient instability worse
# (RMSE(tilde) went from 1.47 -> 3.03). Default: FALSE.
FIXED_MF_BAR <- FALSE

# DEMEAN_MF_IN_INTERACTION: if TRUE (paper-style, recommended), interaction
# terms are (MF_t - MF_bar) * (S_t - S_bar). If FALSE, interaction terms are
# MF_t * (S_t - S_bar) (no demeaning of MF_t). For the "effect of MF at
# S=S_bar" interpretation these are equivalent (both give beta as the
# marginal effect at S=S_bar); the difference is purely collinearity:
# demeaning MF_t removes the part of the interaction term that is a
# constant multiple of (S_t - S_bar), which is otherwise collinear with
# vstoxx (the gamma regressor). Expect FALSE to make gamma/lambda collinearity
# WORSE, not better - included as a toggle to test empirically.
DEMEAN_MF_IN_INTERACTION <- TRUE

# Sanity check: INTERACT_VARS must be a subset of MF_VARS
stopifnot(all(INTERACT_VARS %in% MF_VARS))

# ============================================================
# Load data
# ============================================================
moments <- read_csv(INPUT_FILE, show_col_types = FALSE) %>%
  mutate(date = as.Date(paste0(date, "-01")))

vstoxx <- read_csv(file.path(ROOT, "data", "variables", "vstoxx_monthly.csv"), show_col_types = FALSE) %>%
  mutate(date = as.Date(paste0(date, "-01")))

df <- moments %>%
  select(date, all_of(MS_VAR), all_of(MF_VARS)) %>%
  left_join(vstoxx, by = "date") %>%
  arrange(date)

# Restrict to estimation sample
df <- df %>% filter(date >= START_DATE, date <= END_DATE)

# Drop rows with any missing values needed for the regression
df <- df %>% drop_na(all_of(c(MS_VAR, MF_VARS, "vstoxx")))

dates <- df$date
n_obs <- nrow(df)

if (n_obs < WINDOW) {
  stop("Not enough observations (", n_obs, ") for a ", WINDOW, "-month rolling window.")
}

# ============================================================
# Full-sample "normal conditions" reference point (MF_bar, S_bar)
# ============================================================
# Computed over [NORMAL_REF_START, NORMAL_REF_END] only, per the paper's
# explicit definition (p.12): "S_bar [is] the average market sentiment in
# 2005-2022". Used for every window's interaction terms when FIXED_S_BAR /
# FIXED_MF_BAR = TRUE. Note this may be narrower than the full estimation
# sample (which extends to END_DATE and may include a post-2022 regime with
# different vstoxx behaviour). Adjust NORMAL_REF_START/END above to test
# sensitivity to the choice of reference period.
df_normal_ref <- df %>% filter(date >= NORMAL_REF_START, date <= NORMAL_REF_END)

FULL_MF_BAR <- colMeans(df_normal_ref[, MF_VARS], na.rm = TRUE)
FULL_S_BAR  <- mean(df_normal_ref$vstoxx, na.rm = TRUE)

message(sprintf("Normal-conditions S_bar (vstoxx, %s to %s) = %.3f",
                NORMAL_REF_START, NORMAL_REF_END, FULL_S_BAR))
message("Normal-conditions MF_bar:")
print(round(FULL_MF_BAR, 4))

# ============================================================
# Data sanity check: *_stdev columns must be >= 0
# ============================================================
# All MF_VARS (and MS_VAR) are cross-sectional standard deviations across
# EMU countries and are mathematically non-negative. Negative values would
# indicate an interpolation/extrapolation artifact in INPUT_FILE and could
# be feeding bad data into the regression (e.g. causing "wrong sign"
# coefficients). Check the full loaded moments series (before drop_na/date
# filtering already applied above doesn't matter much - df already reflects
# the estimation sample).
stdev_check_vars <- unique(c(MF_VARS, MS_VAR))
stdev_check_vars <- stdev_check_vars[grepl("_stdev$", stdev_check_vars)]

cat("\n=== Data sanity check: negative values in *_stdev columns ===\n")
any_negative <- FALSE
for (v in stdev_check_vars) {
  neg_idx <- which(df[[v]] < 0)
  if (length(neg_idx) > 0) {
    any_negative <- TRUE
    cat(sprintf("  %-24s : %d negative value(s)\n", v, length(neg_idx)))
    bad <- data.frame(date = df$date[neg_idx], value = df[[v]][neg_idx])
    print(bad)
  }
}
if (!any_negative) {
  cat("  (none found - all *_stdev columns are >= 0 in the estimation sample)\n")
}
cat("\n")

# ============================================================
# Rolling regression
# ============================================================
n_mf <- length(MF_VARS)

# Storage for Stage 1 coefficients
# Note: lambda_* columns are only populated for variables in INTERACT_VARS;
# all other lambda_* columns are left as NA.
coef_names <- c("alpha", paste0("beta_", MF_VARS), "gamma",
                paste0("lambda_", MF_VARS))

results <- matrix(NA_real_, nrow = n_obs, ncol = length(coef_names))
colnames(results) <- coef_names

# Storage for fitted values
MS_actual   <- rep(NA_real_, n_obs)   # actual MS_t (end of window)
MS_fitted   <- rep(NA_real_, n_obs)   # fitted from full Stage 1 model
MS_tilde    <- rep(NA_real_, n_obs)   # Stage 2 prediction under S_t = S_bar
MS_tilde    <- rep(NA_real_, n_obs)   # Stage 2 prediction under S_t = S_bar
MS_tilde_se <- rep(NA_real_, n_obs)   # std error of the Stage 2 prediction
Rsq         <- rep(NA_real_, n_obs)   # in-sample R^2 of the Stage 1 fit, per window
AdjRsq      <- rep(NA_real_, n_obs)   # adjusted R^2 of the Stage 1 fit, per window
run_window_regression <- function(window_df) {
  
  # S_bar: fixed full-sample value (per paper's explicit definition) or
  # window-specific, controlled independently of MF_bar.
  if (FIXED_S_BAR) {
    S_bar <- FULL_S_BAR
  } else {
    S_bar <- mean(window_df$vstoxx, na.rm = TRUE)
  }
  
  # MF_bar: fixed full-sample value or window-specific. Default is
  # window-specific (FALSE) - MF_bar is only a centering constant for the
  # interaction term, not a quantity the paper requires to be fixed.
  if (FIXED_MF_BAR) {
    MF_bar <- FULL_MF_BAR
  } else {
    MF_bar <- colMeans(window_df[, MF_VARS], na.rm = TRUE)
  }
  
  # Build interaction terms for INTERACT_VARS, either demeaned (paper-style)
  # or not, per DEMEAN_MF_IN_INTERACTION:
  #   demeaned:     (MF_t - MF_bar) * (S_t - S_bar)
  #   non-demeaned:  MF_t           * (S_t - S_bar)
  if (DEMEAN_MF_IN_INTERACTION) {
    inter <- sweep(as.matrix(window_df[, INTERACT_VARS, drop = FALSE]), 2,
                   MF_bar[INTERACT_VARS], FUN = "-") *
      (window_df$vstoxx - S_bar)
  } else {
    inter <- as.matrix(window_df[, INTERACT_VARS, drop = FALSE]) *
      (window_df$vstoxx - S_bar)
  }
  colnames(inter) <- paste0("inter_", INTERACT_VARS)
  
  reg_data <- window_df %>%
    select(all_of(MS_VAR), all_of(MF_VARS), vstoxx) %>%
    bind_cols(as.data.frame(inter))
  
  predictor_cols <- c(MF_VARS, "vstoxx", colnames(inter))
  
  form <- as.formula(
    paste0(MS_VAR, " ~ ", paste(MF_VARS, collapse = " + "),
           " + vstoxx + ", paste(colnames(inter), collapse = " + "))
  )
  
  fit <- lm(form, data = reg_data)
  cf  <- coef(fit)

  # Coefficient covariance for the Stage 2 prediction SE
  V <- vcov(fit)

  # In-sample R^2 of this window's fit.
  X_orig      <- as.matrix(reg_data[, predictor_cols])
  y_orig      <- reg_data[[MS_VAR]]
  fitted_orig <- as.numeric(cf["(Intercept)"] + X_orig %*% cf[predictor_cols])
  resid_orig  <- y_orig - fitted_orig
  r_squared   <- 1 - sum(resid_orig^2) / sum((y_orig - mean(y_orig))^2)
  # Adjusted R^2: penalizes for the number of predictors p given window size n
  n_w <- nrow(reg_data)
  p_w <- length(predictor_cols)
  adj_r_squared <- 1 - (1 - r_squared) * (n_w - 1) / (n_w - p_w - 1)
  
  list(fit = fit, cf = cf, MF_bar = MF_bar, S_bar = S_bar, V = V,
       r_squared = r_squared, adj_r_squared = adj_r_squared)   # <- adj_r_squared added
  
}

for (i in WINDOW:n_obs) {
  
  window_idx <- (i - WINDOW + 1):i
  window_df  <- df[window_idx, ]
  
  reg <- run_window_regression(window_df)
  cf  <- reg$cf
  
  # --- Stage 1 coefficients ---
  alpha_t  <- cf["(Intercept)"]
  beta_t   <- cf[MF_VARS]
  gamma_t  <- cf["vstoxx"]
  
  # lambda_t: only INTERACT_VARS have estimated coefficients; the rest are
  # implicitly zero (no interaction term was included for them)
  lambda_t <- setNames(rep(0, n_mf), MF_VARS)
  lambda_t[INTERACT_VARS] <- cf[paste0("inter_", INTERACT_VARS)]
  
  results[i, "alpha"] <- alpha_t
  results[i, paste0("beta_", MF_VARS)]   <- beta_t
  results[i, "gamma"] <- gamma_t
  results[i, paste0("lambda_", MF_VARS)] <- lambda_t
  
  Rsq[i] <- reg$r_squared
  AdjRsq[i] <- reg$adj_r_squared
 
  # --- Current period (end of window) values for prediction ---
  current <- df[i, ]
  MF_t <- as.numeric(current[, MF_VARS])
  S_t  <- current$vstoxx
  
  if (DEMEAN_MF_IN_INTERACTION) {
    inter_t <- (MF_t - reg$MF_bar) * (S_t - reg$S_bar)
  } else {
    inter_t <- MF_t * (S_t - reg$S_bar)
  }
  names(inter_t) <- MF_VARS
  
  # Stage 1 fitted value (full model)
  MS_actual[i] <- current[[MS_VAR]]
  MS_fitted[i] <- alpha_t +
    sum(beta_t * MF_t) +
    gamma_t * S_t +
    sum(lambda_t[INTERACT_VARS] * inter_t[INTERACT_VARS])
  
  # --- Stage 2: prediction under normal conditions S_t = S_bar ---
  # gamma_t and lambda_t drop out since (S_bar - S_bar) = 0
  MS_tilde[i] <- alpha_t + sum(beta_t * MF_t)
  
  # SE of the Stage 2 prediction: c' V c, with c picking the intercept (1)
  # and the MF_t loadings; vstoxx & interaction terms are 0 at S_t = S_bar.
  if (!is.null(reg$V)) {
    cn   <- colnames(reg$V)
    cvec <- setNames(rep(0, length(cn)), cn)
    cvec["(Intercept)"] <- 1
    cvec[MF_VARS]       <- MF_t
    MS_tilde_se[i] <- sqrt(as.numeric(t(cvec) %*% reg$V %*% cvec))
  }
}

# ============================================================
# Assemble output
# ============================================================
out <- bind_cols(
  data.frame(date = dates),
  as.data.frame(results),
  data.frame(
    MS_actual = MS_actual,
    MS_fitted = MS_fitted,
    MS_tilde  = MS_tilde,
    MS_tilde_se  = MS_tilde_se,
    MS_tilde_lo  = MS_tilde - 2 * MS_tilde_se,
    MS_tilde_hi  = MS_tilde + 2 * MS_tilde_se,
    non_fundamental_component = MS_fitted - MS_tilde,
    Rsq = Rsq,
    AdjRsq = AdjRsq
  )
)

write_csv(out, OUTPUT_CSV)
message("Saved ", OUTPUT_CSV, " (", sum(!is.na(out$MS_fitted)),
        " rolling windows, ", n_obs, " total observations, ",
        "n_params = ", 2 + n_mf + length(INTERACT_VARS), ")")

# ============================================================
# Coefficient summary: mean, SD, min, max across all rolling windows
# ============================================================
coef_summary <- out %>%
  filter(!is.na(MS_fitted)) %>%
  select(all_of(coef_names)) %>%
  summarise(across(everything(),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd   = ~sd(.x, na.rm = TRUE),
                        min  = ~min(.x, na.rm = TRUE),
                        max  = ~max(.x, na.rm = TRUE)),
                   .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
  separate(stat, into = c("coefficient", "statistic"), sep = "__") %>%
  pivot_wider(names_from = statistic, values_from = value)

write_csv(coef_summary, OUTPUT_COEF_SUMMARY)
message("Saved ", OUTPUT_COEF_SUMMARY)

cat("\n=== Coefficient summary (across all rolling windows) ===\n")
print(coef_summary, n = 30)

# ============================================================
# RMSE summary
# ============================================================
rmse <- function(actual, fitted) sqrt(mean((actual - fitted)^2, na.rm = TRUE))

rmse_summary <- out %>%
  filter(!is.na(MS_fitted)) %>%
  summarise(
    rmse_fitted_vs_actual = rmse(MS_actual, MS_fitted),
    rmse_tilde_vs_actual  = rmse(MS_actual, MS_tilde),
    rmse_tilde_vs_fitted  = rmse(MS_fitted, MS_tilde),
    r_squared_mean        = mean(Rsq, na.rm = TRUE),
    adj_r_squared_mean    = mean(AdjRsq, na.rm = TRUE),
    n_windows             = sum(!is.na(MS_fitted)),
    n_params              = 2 + n_mf + length(INTERACT_VARS)
  )

write_csv(rmse_summary, OUTPUT_RMSE)
message("Saved ", OUTPUT_RMSE)

cat("\n=== RMSE summary ===\n")
print(rmse_summary)

cat("\n=== R^2 summary (across all rolling windows) ===\n")
cat(sprintf("  mean = %.3f, sd = %.3f, min = %.3f, max = %.3f\n",
            mean(out$Rsq, na.rm = TRUE), sd(out$Rsq, na.rm = TRUE),
            min(out$Rsq, na.rm = TRUE), max(out$Rsq, na.rm = TRUE)))

# ============================================================
# Plot: predicted vs actual spread dispersion
# ============================================================
plot_df <- out %>%
  filter(!is.na(MS_fitted)) %>%
  select(date, MS_actual, MS_fitted, MS_tilde) %>%
  pivot_longer(cols = c(MS_actual, MS_fitted, MS_tilde),
               names_to = "series", values_to = "value") %>%
  mutate(series = dplyr::recode(series,
                         MS_actual = "Actual",
                         MS_fitted = "Fitted (Stage 1)",
                         MS_tilde  = "Predicted under normal conditions (Stage 2)"))

# +/- 2 std error band around the Stage 2 prediction only (two dotted lines)
ci_long <- out %>%
  filter(!is.na(MS_tilde), !is.na(MS_tilde_se)) %>%
  transmute(date,
            lo = MS_tilde - 2 * MS_tilde_se,
            hi = MS_tilde + 2 * MS_tilde_se) %>%
  pivot_longer(c(lo, hi), names_to = "bound", values_to = "value") %>%
  mutate(series = "+/- 2 std error (Stage 2)")

series_levels <- c("Actual", "Fitted (Stage 1)",
                   "Predicted under normal conditions (Stage 2)",
                   "+/- 2 std error (Stage 2)")
plot_df <- plot_df %>% mutate(series = factor(series, levels = series_levels))
ci_long <- ci_long %>% mutate(series = factor(series, levels = series_levels))

pal_series <- c(
  "Actual"                                      = "black",
  "Fitted (Stage 1)"                            = "#D55E00",  # orange
  "Predicted under normal conditions (Stage 2)" = "#0072B2",  # blue
  "+/- 2 std error (Stage 2)"                   = "#56B4E9"   # light blue
)
lty_series <- c(
  "Actual"                                      = "solid",
  "Fitted (Stage 1)"                            = "solid",
  "Predicted under normal conditions (Stage 2)" = "dashed",
  "+/- 2 std error (Stage 2)"                   = "dotted"
)

p <- ggplot() +
  geom_hline(yintercept = c(0, 3, 6, 9), colour = "grey85", linewidth = 0.3) +
  geom_line(data = ci_long,
            aes(date, value, color = series, linetype = series, group = bound),
            linewidth = 0.5, na.rm = TRUE) +
  geom_line(data = plot_df,
            aes(date, value, color = series, linetype = series),
            linewidth = 0.7, na.rm = TRUE) +
  scale_color_manual(values = pal_series, drop = FALSE) +
  scale_linetype_manual(values = lty_series, drop = FALSE) +
  labs(title = "Sovereign Spread Dispersion: Actual vs Predicted",
       x = NULL, y = "spread_stdev", color = NULL, linetype = NULL) +
  theme_classic(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 13))

ggsave(OUTPUT_PLOT, plot = p, width = 11, height = 5, dpi = 150)
message("Saved ", OUTPUT_PLOT)

# ============================================================
# Plot: R^2 over time
# ============================================================
OUTPUT_R2_PLOT <- file.path(FIG_DIR, "rsquared_over_time.png")

p_r2 <- out %>%
  filter(!is.na(Rsq)) %>%
  ggplot(aes(date, Rsq)) +
  geom_line(colour = "#0072B2", linewidth = 0.6) +
  labs(title = "Rolling 60-Month Regression: In-Sample R^2",
       x = NULL, y = expression(R^2)) +
  theme_classic(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13))

ggsave(OUTPUT_R2_PLOT, plot = p_r2, width = 11, height = 4, dpi = 150)
message("Saved ", OUTPUT_R2_PLOT)

message("\nAll regression outputs written to '", OUTPUT_DIR, "/':")
message("  - ", basename(OUTPUT_CSV), " (full panel: dates, all coefficients, fitted/tilde/actual, Rsq)")
message("  - ", basename(OUTPUT_COEF_SUMMARY), " (mean/sd/min/max per coefficient across windows)")
message("  - ", basename(OUTPUT_RMSE), " (RMSE of fitted/tilde vs actual, n_windows, n_params)")
message("  - ", basename(OUTPUT_PLOT), " (actual vs fitted vs Stage 2 plot)")
message("  - ", basename(OUTPUT_R2_PLOT), " (in-sample R^2 per rolling window, over time)")

# ============================================================
# ADDITION: Fragmentation series + two-panel plot
# (append this after the existing OUTPUT_PLOT / ggsave() block,
#  and add library(patchwork) to the library() calls at the top)
# ============================================================
library(patchwork)

# ------------------------------------------------------------
# Fragmentation series: observed minus the *upper* 2-SE bound
# of the Stage 2 ("normal conditions") prediction, floored at 0.
# This mirrors the DNB paper's "potential fragmentation" bars:
# only the excess above what fundamentals can statistically
# explain counts as fragmentation - not the full gap to the
# point prediction, and never a negative value.
# ------------------------------------------------------------
frag_df <- out %>%
  filter(!is.na(MS_tilde), !is.na(MS_tilde_se), !is.na(MS_actual)) %>%
  transmute(
    date,
    MS_tilde_hi   = MS_tilde + 2 * MS_tilde_se,
    fragmentation = pmax(0, MS_actual - MS_tilde_hi)
  )

OUTPUT_FRAG_CSV  <- file.path(OUTPUT_DIR, "fragmentation_series.csv")
OUTPUT_FRAG_PLOT <- file.path(FIG_DIR, "fragmentation_two_panel.png")

write_csv(frag_df, OUTPUT_FRAG_CSV)
message("Saved ", OUTPUT_FRAG_CSV)

# ------------------------------------------------------------
# Top panel: same predicted/observed/CI plot as before, but
# with the x-axis stripped (shared axis comes from the bottom
# panel) so the two stack cleanly.
# ------------------------------------------------------------
p_top <- p +
  labs(x = NULL) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +   # date breaks
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top")

# ------------------------------------------------------------
# Bottom panel: fragmentation on its own scale, drawn as a
# filled black area (not stacked/dual-axis onto the top panel,
# since that scale relationship is what made Figure 3 misleading
# in the first place).
# ------------------------------------------------------------
p_frag <- ggplot(frag_df, aes(date, fragmentation)) +
  geom_area(fill = "black", colour = NA) +
  labs(x = NULL, y = "Non-Fundamental \n Fragmentation") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +   # <-- date breaks
  theme_classic(base_size = 11) +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))      # <-- change: theme(plot.title = element_blank()) only

# ------------------------------------------------------------
# Combine: top panel 3x the height of the bottom panel, shared
# x-axis range enforced automatically by patchwork since both
# plots use the same `date` column.
# ------------------------------------------------------------
p_combined <- p_top / p_frag + plot_layout(heights = c(3, 1))
library(car)

cat(sprintf("  R^2 min = %.3f, R^2 max = %.3f\n", min(out$Rsq, na.rm = TRUE), max(out$Rsq, na.rm = TRUE)))
cat("\n=== Adjusted R^2 summary (across all rolling windows) ===\n")
cat(sprintf("  mean = %.3f, sd = %.3f, min = %.3f, max = %.3f\n",
            mean(out$AdjRsq, na.rm = TRUE), sd(out$AdjRsq, na.rm = TRUE),
            min(out$AdjRsq, na.rm = TRUE), max(out$AdjRsq, na.rm = TRUE)))