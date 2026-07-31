library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(purrr)
library(zoo)
library(ggplot2)
library(patchwork)

# Paths anchor at the repository root, located via the {here} package
# (uses the `.here` marker file; run from any working directory).
ROOT <- here::here()
# ============================================================
# Settings
# ============================================================
# This script estimates Stage 1 (alpha, beta, gamma, lambda) ONCE over a
# fixed estimation window (FIXED_FIT_START/END) instead of re-fitting in
# every rolling window. The fixed coefficients are then applied to every
# period in [START_DATE, END_DATE] to produce MS_fitted / MS_tilde. This
# isolates how much of the rolling regression's time variation comes from
# time-varying COEFFICIENTS vs. time-varying REGRESSORS (MF_t, S_t) alone.
START_DATE   <- as.Date("2005-09-01")
END_DATE     <- as.Date("2025-12-01")

FIXED_FIT_START <- as.Date("2005-09-01")
FIXED_FIT_END   <- as.Date("2025-12-01")

NORMAL_REF_START <- as.Date("2005-09-01")
NORMAL_REF_END   <- as.Date("2022-12-01")

INPUT_FILE   <- file.path(ROOT, "data", "processed", "moments_forecasted_interpolated.csv")
OUT_DIR      <- file.path(ROOT, "output", "regression", "model1_fixed")
FIG_DIR      <- file.path(ROOT, "output", "figures")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
OUTPUT_CSV   <- file.path(OUT_DIR, "fixed_regression_results_forecasted.csv")
OUTPUT_PLOT  <- file.path(FIG_DIR, "predicted_vs_actual_spread_fixed.png")

# ============================================================
# Variable selection (mirrors rolling script)
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

INTERACT_VARS <- c(
  "gdp_growth_stdev",
  "inflation_stdev",
  "debt_gdp_stdev",
  "current_account_stdev",
  "policy_uncertainty_stdev",
  "bank_nexus_stdev",
  "bid_ask_stdev"
)

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

df <- moments %>%
  select(date, all_of(MS_VAR), all_of(MF_VARS)) %>%
  left_join(vstoxx, by = "date") %>%
  arrange(date)

df <- df %>% filter(date >= START_DATE, date <= END_DATE)
df <- df %>% drop_na(all_of(c(MS_VAR, MF_VARS, "vstoxx")))

dates <- df$date
n_obs <- nrow(df)
n_mf  <- length(MF_VARS)

# ============================================================
# "Normal conditions" reference point (MF_bar, S_bar)
# ============================================================
df_normal_ref <- df %>% filter(date >= NORMAL_REF_START, date <= NORMAL_REF_END)

S_bar  <- mean(df_normal_ref$vstoxx, na.rm = TRUE)
MF_bar <- colMeans(df_normal_ref[, MF_VARS], na.rm = TRUE)

message(sprintf("Normal-conditions S_bar (vstoxx, %s to %s) = %.3f",
                NORMAL_REF_START, NORMAL_REF_END, S_bar))
message("Normal-conditions MF_bar:")
print(round(MF_bar, 4))

# ============================================================
# Fixed Stage 1 fit: estimated once over FIXED_FIT_START/END
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
  select(all_of(MS_VAR), all_of(MF_VARS), vstoxx) %>%
  bind_cols(as.data.frame(inter_fit))

form <- as.formula(
  paste0(MS_VAR, " ~ ", paste(MF_VARS, collapse = " + "),
         " + vstoxx + ", paste(colnames(inter_fit), collapse = " + "))
)

fit <- lm(form, data = reg_data)
cf  <- coef(fit)
V   <- vcov(fit)

alpha_fixed  <- cf["(Intercept)"]
beta_fixed   <- cf[MF_VARS]
gamma_fixed  <- cf["vstoxx"]

lambda_fixed <- setNames(rep(0, n_mf), MF_VARS)
lambda_fixed[INTERACT_VARS] <- cf[paste0("inter_", INTERACT_VARS)]

cat("\n=== Fixed Stage 1 coefficients (estimated once, applied to all periods) ===\n")
cat(sprintf("alpha = %.4f\n", alpha_fixed))
print(round(beta_fixed, 4))
cat(sprintf("gamma = %.4f\n", gamma_fixed))
print(round(lambda_fixed, 4))
cat("\n")

# ============================================================
# Apply fixed coefficients to every period
# ============================================================
MS_actual   <- df[[MS_VAR]]
MS_fitted   <- rep(NA_real_, n_obs)
MS_tilde    <- rep(NA_real_, n_obs)
MS_tilde_se <- rep(NA_real_, n_obs)

for (i in seq_len(n_obs)) {
  current <- df[i, ]
  MF_t <- as.numeric(current[, MF_VARS])
  S_t  <- current$vstoxx
  
  if (DEMEAN_MF_IN_INTERACTION) {
    inter_t <- (MF_t - MF_bar) * (S_t - S_bar)
  } else {
    inter_t <- MF_t * (S_t - S_bar)
  }
  names(inter_t) <- MF_VARS
  
  MS_fitted[i] <- alpha_fixed +
    sum(beta_fixed * MF_t) +
    gamma_fixed * S_t +
    sum(lambda_fixed[INTERACT_VARS] * inter_t[INTERACT_VARS])
  
  # Stage 2: prediction under normal conditions S_t = S_bar
  # gamma/lambda drop out since (S_bar - S_bar) = 0
  MS_tilde[i] <- alpha_fixed + sum(beta_fixed * MF_t)
  
  cn   <- colnames(V)
  cvec <- setNames(rep(0, length(cn)), cn)
  cvec["(Intercept)"] <- 1
  cvec[MF_VARS]       <- MF_t
  MS_tilde_se[i] <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
}

# ============================================================
# Assemble output
# ============================================================
coef_row <- c(alpha = alpha_fixed,
              setNames(beta_fixed, paste0("beta_", MF_VARS)),
              gamma = gamma_fixed,
              setNames(lambda_fixed, paste0("lambda_", MF_VARS)))

out <- data.frame(
  date = dates,
  MS_actual   = MS_actual,
  MS_fitted   = MS_fitted,
  MS_tilde    = MS_tilde,
  MS_tilde_se = MS_tilde_se,
  MS_tilde_lo = MS_tilde - 2 * MS_tilde_se,
  MS_tilde_hi = MS_tilde + 2 * MS_tilde_se,
  non_fundamental_component = MS_fitted - MS_tilde
)

# Repeat the (single, fixed) coefficient vector on every row for easy
# comparison against the rolling-window output format.
coef_block <- as.data.frame(matrix(coef_row, nrow = n_obs, ncol = length(coef_row),
                                   byrow = TRUE))
colnames(coef_block) <- names(coef_row)

out <- bind_cols(out, coef_block)

write_csv(out, OUTPUT_CSV)
message("Saved ", OUTPUT_CSV, " (", n_obs, " observations, fixed fit over ",
        FIXED_FIT_START, " to ", FIXED_FIT_END, ", n_params = ",
        2 + n_mf + length(INTERACT_VARS), ")")

# ============================================================
# Plot: predicted vs actual spread dispersion, with +/- 2 SE band
# ============================================================
plot_df <- out %>%
  select(date, MS_actual, MS_fitted, MS_tilde) %>%
  pivot_longer(cols = c(MS_actual, MS_fitted, MS_tilde),
               names_to = "series", values_to = "value") %>%
  mutate(series = recode_values(series,
                                "MS_actual" ~ "Actual",
                                "MS_fitted" ~ "Fitted (fixed params)",
                                "MS_tilde"  ~ "Predicted under normal conditions (fixed params)"
  ))

# +/- 2 SE band around the Stage 2 (normal-conditions) prediction only,
# drawn as two dotted lines - same convention as the rolling script.
ci_long <- out %>%
  filter(!is.na(MS_tilde), !is.na(MS_tilde_se)) %>%
  transmute(date, lo = MS_tilde_lo, hi = MS_tilde_hi) %>%
  pivot_longer(c(lo, hi), names_to = "bound", values_to = "value") %>%
  mutate(series = "+/- 2 std error (fixed params)")

series_levels <- c("Actual", "Fitted (fixed params)",
                   "Predicted under normal conditions (fixed params)",
                   "+/- 2 std error (fixed params)")
plot_df <- plot_df %>% mutate(series = factor(series, levels = series_levels))
ci_long <- ci_long %>% mutate(series = factor(series, levels = series_levels))

pal_series <- c(
  "Actual" = "black",
  "Fitted (fixed params)" = "#D55E00",
  "Predicted under normal conditions (fixed params)" = "#0072B2",
  "+/- 2 std error (fixed params)" = "#56B4E9"
)
lty_series <- c(
  "Actual" = "solid",
  "Fitted (fixed params)" = "solid",
  "Predicted under normal conditions (fixed params)" = "dashed",
  "+/- 2 std error (fixed params)" = "dotted"
)

p <- ggplot() +
  geom_line(data = ci_long,
            aes(date, value, color = series, linetype = series, group = bound),
            linewidth = 0.5, na.rm = TRUE) +
  geom_line(data = plot_df,
            aes(date, value, color = series, linetype = series),
            linewidth = 0.7, na.rm = TRUE) +
  scale_color_manual(values = pal_series, drop = FALSE) +
  scale_linetype_manual(values = lty_series, drop = FALSE) +
  labs(
    title = "Sovereign Spread Dispersion: Actual vs Predicted (Fixed-Parameter Model)",
    subtitle = sprintf("Coefficients estimated once over %s to %s",
                       FIXED_FIT_START, FIXED_FIT_END),
    x = NULL, y = "spread_stdev", color = NULL, linetype = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 9)
  )

ggsave(OUTPUT_PLOT, plot = p, width = 11, height = 5, dpi = 150)
message("Saved ", OUTPUT_PLOT)
cat(sprintf("Fixed model R-squared: %.4f\n", summary(fit)$r.squared))
cat(sprintf("Fixed Stage 1 RMSE: %.4f   Stage 2 RMSE: %.4f\n",
            sqrt(mean(residuals(fit)^2)),
            sqrt(mean((out$MS_actual - out$MS_tilde)^2, na.rm = TRUE))))
cat(sprintf("Adj. R-squared (Model 1): %.4f\n", summary(fit)$adj.r.squared))
# ============================================================
# Fragmentation series: excess of MS_actual over the upper 2-SE bound
# of the Stage 2 ("normal conditions") prediction, floored at 0.
# ============================================================
frag_df <- out %>%
  filter(!is.na(MS_tilde), !is.na(MS_tilde_se), !is.na(MS_actual)) %>%
  transmute(
    date,
    MS_tilde_hi   = MS_tilde + 2 * MS_tilde_se,
    fragmentation = pmax(0, MS_actual - MS_tilde_hi)
  )

OUTPUT_FRAG_CSV           <- file.path(OUT_DIR, "fragmentation_series_fixed.csv")
OUTPUT_COMBINED_PLOT      <- file.path(FIG_DIR, "predicted_vs_actual_with_fragmentation_fixed.png")
OUTPUT_FRAG_ONLY_PLOT     <- file.path(FIG_DIR, "fragmentation_only_fixed.png")

write_csv(frag_df, OUTPUT_FRAG_CSV)
message("Saved ", OUTPUT_FRAG_CSV)

# Fragmentation panel, used both standalone and stacked below the main plot.
p_frag <- ggplot(frag_df, aes(date, fragmentation)) +
  geom_area(fill = "black", colour = NA) +
  labs(x = NULL, y = "Non-Fundamental\nFragmentation") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_classic(base_size = 11) +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# --- Output 1: main plot alone (already saved above as OUTPUT_PLOT) ---

# --- Output 2: main plot + fragmentation stacked underneath ---
p_top <- p +
  labs(x = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top")

p_combined <- p_top / p_frag + plot_layout(heights = c(3, 1))

ggsave(OUTPUT_COMBINED_PLOT, plot = p_combined, width = 11, height = 6.5, dpi = 150)
message("Saved ", OUTPUT_COMBINED_PLOT)

# --- Output 3: fragmentation alone ---
ggsave(OUTPUT_FRAG_ONLY_PLOT, plot = p_frag, width = 11, height = 3, dpi = 150)
message("Saved ", OUTPUT_FRAG_ONLY_PLOT)

cat("\n=== Fragmentation summary (fixed-parameter model) ===\n")
cat(sprintf("  Months with fragmentation > 0: %d / %d\n",
            sum(frag_df$fragmentation > 0), nrow(frag_df)))
cat(sprintf("  Max fragmentation: %.3f on %s\n",
            max(frag_df$fragmentation),
            frag_df$date[which.max(frag_df$fragmentation)]))