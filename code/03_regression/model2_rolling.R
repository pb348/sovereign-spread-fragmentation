library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(zoo)
library(ggplot2)
# Paths anchor at the repository root, located via the {here} package
# (uses the `.here` marker file; run from any working directory).
ROOT <- here::here()
FIG_DIR <- file.path(ROOT, "output", "figures")
# ============================================================
# TOGGLE
#   "PURCHASES"     — rolling ECB purchase-effect regression (Model 2)
#   "ANNOUNCEMENTS" — programme-announcement t-test analysis (Model 1)
# ============================================================
ANALYSIS_MODE <- "PURCHASES"   # <-- change here
# ============================================================
# Shared settings
# ============================================================
WINDOW     <- 60
START_DATE <- as.Date("2005-09-01")
END_DATE   <- as.Date("2025-12-01")
NORMAL_REF_START <- as.Date("2005-09-01")
NORMAL_REF_END   <- as.Date("2025-12-01")
INPUT_FILE         <- file.path(ROOT, "data", "processed", "moments_forecasted_interpolated.csv")
PURCHASES_FILE     <- file.path(ROOT, "data", "variables", "ecb_sovereign_purchases_monthly.csv")
ANNOUNCEMENTS_FILE <- file.path(ROOT, "data", "variables", "ecb_announcements.csv")
# ============================================================
# PURCHASES-mode settings  (ignored in ANNOUNCEMENTS mode)
# ============================================================
# P_LAG: months to lag ECB purchases (1 = baseline endogeneity correction)
P_LAG           <- 1
RUN_ALL_LAGS    <- FALSE
LAGS_TO_COMPARE <- c(0, 1, 3, 6, 12)

# ── PSPP TOGGLE ─────────────────────────────────────────────────────────────
# Set EXCLUDE_PSPP = TRUE to run with only SMP + PEPP purchases (drops PSPP).
# PSPP was a capital-key QE programme (deflation-fighting), not targeted at
# non-fundamental fragmentation — unlike SMP and PEPP which had explicit
# anti-fragmentation mandates.
#
# How the exclusion works:
#   If your CSV has per-programme columns, list them in PSPP_COLUMNS below
#   (e.g. c("pspp_mn") or c("pspp_sovereign_mn")).  The script will sum only
#   the remaining programme columns (SMP + PEPP) to build P_raw_bn.
#
#   If the CSV only has a single total column, the script falls back to
#   zeroing out the PSPP-only period (2015-03 to 2020-02).  Purchases from
#   2020-03 onward still include PSPP alongside PEPP, so this under-removes;
#   adding per-programme columns to your CSV is the cleaner solution.
EXCLUDE_PSPP  <- TRUE          # <-- set TRUE to drop PSPP
PSPP_COLUMNS  <- c("pspp_mn")   # <-- column name(s) for PSPP in your CSV
#     (only used when EXCLUDE_PSPP = TRUE
#      and the columns exist)
# ────────────────────────────────────────────────────────────────────────────

# ============================================================
# ANNOUNCEMENTS-mode settings  (ignored in PURCHASES mode)
# ============================================================
# Windows: [-1, +W] months around each first-announcement date
ANNOUNCEMENT_WINDOWS <- c(3, 6)
# Programmes to include (must match `programme` column in ANNOUNCEMENTS_FILE)
INCLUDE_PROGRAMMES   <- c("SMP", "OMT", "PSPP", "PEPP", "TPI")
# ============================================================
# Variable selection (shared)
# ============================================================
MF_VARS       <- c(
  "gdp_growth_stdev",
  "inflation_stdev",
  "debt_gdp_stdev",
  "current_account_stdev",
  "policy_uncertainty_stdev",
  "bank_nexus_stdev",
  "bid_ask_stdev"
)
INTERACT_VARS <- MF_VARS
MS_VAR        <- "spread_stdev"
FIXED_S_BAR              <- TRUE
FIXED_MF_BAR             <- FALSE
DEMEAN_MF_IN_INTERACTION <- TRUE
stopifnot(all(INTERACT_VARS %in% MF_VARS))
# ============================================================
# Load base data (always needed)
# ============================================================
moments <- read_csv(INPUT_FILE, show_col_types = FALSE) %>%
  mutate(date = as.Date(paste0(date, "-01")))
vstoxx <- read_csv(file.path(ROOT, "data", "variables", "vstoxx_monthly.csv"), show_col_types = FALSE) %>%
  mutate(date = as.Date(paste0(date, "-01")))

# ── PSPP TOGGLE: conditional purchases loading ───────────────────────────────
purchases_raw_all <- read_csv(PURCHASES_FILE, show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

if (EXCLUDE_PSPP) {
  found_pspp_cols <- intersect(PSPP_COLUMNS, names(purchases_raw_all))
  
  if (length(found_pspp_cols) > 0) {
    # Per-programme columns exist: sum all sovereign columns except PSPP ones
    all_mn_cols  <- grep("_mn$", names(purchases_raw_all), value = TRUE)
    keep_mn_cols <- setdiff(all_mn_cols, found_pspp_cols)
    if (length(keep_mn_cols) == 0) stop("No purchase columns remain after excluding PSPP.")
    purchases_raw <- purchases_raw_all %>%
      mutate(P_raw_bn = rowSums(across(all_of(keep_mn_cols)), na.rm = TRUE) / 1000) %>%
      select(date, P_raw_bn)
    message("EXCLUDE_PSPP = TRUE  |  keeping columns: ", paste(keep_mn_cols, collapse = ", "))
  } else {
    # Fallback: zero out the PSPP-only period (pre-PEPP months 2015-03 to 2020-02)
    # Note: PSPP purchases within the PEPP overlap period (2020-03 onward) are NOT
    # removed here. Add per-programme columns to your CSV for a clean exclusion.
    purchases_raw <- purchases_raw_all %>%
      transmute(date,
                P_raw_bn = ifelse(
                  date >= as.Date("2015-03-01") & date < as.Date("2020-03-01"),
                  0,
                  total_sovereign_mn / 1000
                ))
    warning("EXCLUDE_PSPP = TRUE but PSPP_COLUMNS not found in CSV. ",
            "Zeroing out PSPP-only period 2015-03 to 2020-02. ",
            "Add per-programme columns for a complete exclusion.")
  }
  PURCHASE_LABEL <- "SMP + PEPP"
} else {
  purchases_raw <- purchases_raw_all %>%
    transmute(date, P_raw_bn = total_sovereign_mn / 1000)
  PURCHASE_LABEL <- "SMP + PSPP + PEPP"
}
message("Purchase series: ", PURCHASE_LABEL)
# ────────────────────────────────────────────────────────────────────────────

# ============================================================
# Core regression function
#
#   include_P = TRUE  -> Model 2: includes P_{t-k} (PURCHASES mode)
#   include_P = FALSE -> Model 1: no purchase variable (ANNOUNCEMENTS mode)
#
# When include_P = FALSE, k is ignored.  The delta column in the
# output is set to NA throughout.
# ============================================================
run_model_for_lag <- function(k, include_P = TRUE) {
  
  # Build lagged purchases series (only used when include_P = TRUE)
  if (include_P) {
    if (k == 0) {
      purchases_lagged <- purchases_raw %>% rename(P_t_bn = P_raw_bn)
    } else {
      purchases_lagged <- purchases_raw %>%
        mutate(date = date %m+% months(k)) %>%
        rename(P_t_bn = P_raw_bn)
    }
  }
  
  # Assemble panel
  df_full <- moments %>%
    select(date, all_of(MS_VAR), all_of(MF_VARS)) %>%
    left_join(vstoxx, by = "date") %>%
    { if (include_P) left_join(., purchases_lagged, by = "date")
      else            mutate(., P_t_bn = 0)            } %>%
    arrange(date) %>%
    mutate(P_t_bn = if (include_P) replace_na(P_t_bn, 0) else 0) %>%
    filter(date >= START_DATE, date <= END_DATE) %>%
    drop_na(all_of(c(MS_VAR, MF_VARS, "vstoxx")))
  
  dates <- df_full$date
  n_obs <- nrow(df_full)
  n_mf  <- length(MF_VARS)
  
  if (n_obs < WINDOW) stop("Not enough observations for window.")
  
  df_ref     <- df_full %>% filter(date >= NORMAL_REF_START, date <= NORMAL_REF_END)
  FULL_S_BAR  <- mean(df_ref$vstoxx,  na.rm = TRUE)
  FULL_MF_BAR <- colMeans(df_ref[, MF_VARS], na.rm = TRUE)
  
  # Storage — always include delta column; stays NA when include_P = FALSE
  coef_names <- c("alpha", paste0("beta_", MF_VARS), "gamma",
                  paste0("lambda_", MF_VARS), "delta")
  results  <- matrix(NA_real_, nrow = n_obs, ncol = length(coef_names))
  colnames(results) <- coef_names
  
  MS_actual        <- rep(NA_real_, n_obs)
  MS_fitted        <- rep(NA_real_, n_obs)
  MS_tilde         <- rep(NA_real_, n_obs)
  MS_tilde_nopurch <- rep(NA_real_, n_obs)
  MS_tilde_se      <- rep(NA_real_, n_obs)
  purchase_effect  <- rep(NA_real_, n_obs)
  r_squared_vec    <- rep(NA_real_, n_obs)
  adj_r_squared_vec <- rep(NA_real_, n_obs)
  
  # Columns fed into lm()
  base_pred_cols <- c(MF_VARS, "vstoxx")
  if (include_P) base_pred_cols <- c(base_pred_cols, "P_t_bn")
  
  for (i in WINDOW:n_obs) {
    window_df <- df_full[(i - WINDOW + 1):i, ]
    
    S_bar  <- if (FIXED_S_BAR)  FULL_S_BAR  else mean(window_df$vstoxx, na.rm = TRUE)
    MF_bar <- if (FIXED_MF_BAR) FULL_MF_BAR else colMeans(window_df[, MF_VARS], na.rm = TRUE)
    
    inter <- if (DEMEAN_MF_IN_INTERACTION) {
      sweep(as.matrix(window_df[, INTERACT_VARS, drop = FALSE]), 2,
            MF_bar[INTERACT_VARS], FUN = "-") * (window_df$vstoxx - S_bar)
    } else {
      as.matrix(window_df[, INTERACT_VARS, drop = FALSE]) * (window_df$vstoxx - S_bar)
    }
    colnames(inter) <- paste0("inter_", INTERACT_VARS)
    
    reg_data <- window_df %>%
      select(all_of(c(MS_VAR, base_pred_cols))) %>%
      bind_cols(as.data.frame(inter))
    
    p_term <- if (include_P) " + P_t_bn" else ""
    form <- as.formula(paste0(
      MS_VAR, " ~ ",
      paste(MF_VARS, collapse = " + "),
      " + vstoxx",
      p_term, " + ",
      paste(colnames(inter), collapse = " + ")
    ))
    
    fit <- lm(form, data = reg_data)
    cf  <- coef(fit)
    V   <- vcov(fit)
    r_squared_vec[i] <- summary(fit)$r.squared
    sm  <- summary(fit)
    r_squared_vec[i]     <- sm$r.squared
    adj_r_squared_vec[i] <- sm$adj.r.squared
    
    alpha_t <- cf["(Intercept)"]
    beta_t  <- cf[MF_VARS]
    gamma_t <- cf["vstoxx"]
    delta_t <- if (include_P) cf["P_t_bn"] else NA_real_
    
    lambda_t <- setNames(rep(0, n_mf), MF_VARS)
    lambda_t[INTERACT_VARS] <- cf[paste0("inter_", INTERACT_VARS)]
    
    results[i, "alpha"] <- alpha_t
    results[i, paste0("beta_", MF_VARS)]   <- beta_t
    results[i, "gamma"] <- gamma_t
    results[i, paste0("lambda_", MF_VARS)] <- lambda_t
    results[i, "delta"] <- delta_t
    
    current <- df_full[i, ]
    MF_t <- as.numeric(current[, MF_VARS])
    S_t  <- current$vstoxx
    P_t  <- if (include_P) current$P_t_bn else 0
    
    inter_t <- if (DEMEAN_MF_IN_INTERACTION) {
      (MF_t - MF_bar) * (S_t - S_bar)
    } else {
      MF_t * (S_t - S_bar)
    }
    names(inter_t) <- MF_VARS
    
    MS_actual[i] <- current[[MS_VAR]]
    MS_fitted[i] <- alpha_t +
      sum(beta_t * MF_t) + gamma_t * S_t +
      (if (include_P) delta_t * P_t else 0) +
      sum(lambda_t[INTERACT_VARS] * inter_t[INTERACT_VARS])
    
    MS_tilde[i]         <- alpha_t + sum(beta_t * MF_t) +
      (if (include_P) delta_t * P_t else 0)
    MS_tilde_nopurch[i] <- alpha_t + sum(beta_t * MF_t)
    purchase_effect[i]  <- if (include_P) delta_t * P_t else 0
    
    cn   <- colnames(V)
    cvec <- setNames(rep(0, length(cn)), cn)
    cvec["(Intercept)"] <- 1
    cvec[MF_VARS]       <- MF_t
    MS_tilde_se[i] <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
  }
  
  out <- bind_cols(
    data.frame(date = dates),
    as.data.frame(results),
    data.frame(
      P_t_bn                    = df_full$P_t_bn,
      MS_actual                 = MS_actual,
      MS_fitted                 = MS_fitted,
      MS_tilde                  = MS_tilde,
      MS_tilde_nopurch          = MS_tilde_nopurch,
      MS_tilde_se               = MS_tilde_se,
      purchase_effect           = purchase_effect,
      r_squared                 = r_squared_vec,
      adj_r_squared             = adj_r_squared_vec,
      fundamental_component     = MS_tilde_nopurch,
      purchase_component        = purchase_effect,
      sentiment_component       = MS_fitted - MS_tilde,
      non_fundamental_component = MS_actual - MS_tilde_nopurch
    )
  )
  
  rmse_fn <- function(a, b) sqrt(mean((a - b)^2, na.rm = TRUE))
  active  <- out %>% filter(!is.na(MS_fitted))
  
  rsq_active <- active$r_squared[!is.na(active$r_squared)]
  
  list(
    out = out,
    rmse = data.frame(
      lag                          = if (include_P) k else NA_integer_,
      include_P                    = include_P,
      rmse_fitted_vs_actual        = rmse_fn(active$MS_actual, active$MS_fitted),
      rmse_tilde_vs_actual         = rmse_fn(active$MS_actual, active$MS_tilde),
      rmse_tilde_nopurch_vs_actual = rmse_fn(active$MS_actual, active$MS_tilde_nopurch),
      r_squared_mean     = mean(rsq_active),
      r_squared_median   = median(rsq_active),
      r_squared_min      = min(rsq_active),
      r_squared_max      = max(rsq_active),
      adj_r_squared_mean   = mean(active$adj_r_squared, na.rm = TRUE),
      adj_r_squared_median = median(active$adj_r_squared, na.rm = TRUE),
      adj_r_squared_min    = min(active$adj_r_squared, na.rm = TRUE),
      adj_r_squared_max    = max(active$adj_r_squared, na.rm = TRUE),
      delta_mean         = if (include_P) mean(active$delta, na.rm = TRUE) else NA_real_,
      delta_sd           = if (include_P) sd(active$delta,   na.rm = TRUE) else NA_real_,
      delta_pct_negative = if (include_P) mean(active$delta < 0, na.rm = TRUE) * 100 else NA_real_,
      n_windows          = sum(!is.na(active$MS_fitted)),
      n_params           = if (include_P) 17L else 16L
    )
  )
}
# ============================================================
# ══════════════════════════════════════════════════════════
#   PURCHASES MODE  (original Model 2 logic, unchanged)
# ══════════════════════════════════════════════════════════
# ============================================================
if (ANALYSIS_MODE == "PURCHASES") {
  
  # ── PSPP TOGGLE: output directory reflects which programmes are included ──
  pspp_suffix <- if (EXCLUDE_PSPP) "_no-pspp" else ""
  OUTPUT_DIR  <- file.path(ROOT, "output", "regression", "model2_variants",
                           paste0("lag", P_LAG, pspp_suffix))
  # ─────────────────────────────────────────────────────────────────────────
  dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
  dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
  
  message(sprintf("\n=== Running Model 2 with P_LAG = %d | Purchases: %s ===",
                  P_LAG, PURCHASE_LABEL))
  primary <- run_model_for_lag(P_LAG, include_P = TRUE)
  out     <- primary$out
  
  write_csv(out, file.path(OUTPUT_DIR, paste0("rolling_regression_results_lag", P_LAG, ".csv")))
  write_csv(primary$rmse, file.path(OUTPUT_DIR, paste0("rmse_lag", P_LAG, ".csv")))
  message("Primary lag RMSE:")
  print(primary$rmse)
  act <- out %>% filter(!is.na(MS_fitted))
  cat(sprintf("\n=== Model 2 (P_LAG = %d): fit across %d rolling windows ===\n",
              P_LAG, nrow(act)))
  cat(sprintf("  R^2      : mean = %.3f, median = %.3f, min = %.3f, max = %.3f\n",
              mean(act$r_squared, na.rm = TRUE), median(act$r_squared, na.rm = TRUE),
              min(act$r_squared, na.rm = TRUE), max(act$r_squared, na.rm = TRUE)))
  cat(sprintf("  adj. R^2 : mean = %.3f, median = %.3f, min = %.3f, max = %.3f\n",
              mean(act$adj_r_squared, na.rm = TRUE), median(act$adj_r_squared, na.rm = TRUE),
              min(act$adj_r_squared, na.rm = TRUE), max(act$adj_r_squared, na.rm = TRUE)))
  
  # Annual decomposition
  decomp <- out %>%
    filter(!is.na(MS_fitted)) %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    summarise(
      MS_actual             = mean(MS_actual,             na.rm = TRUE),
      MS_fitted             = mean(MS_fitted,             na.rm = TRUE),
      fundamental_component = mean(fundamental_component, na.rm = TRUE),
      purchase_component    = mean(purchase_component,    na.rm = TRUE),
      sentiment_component   = mean(sentiment_component,   na.rm = TRUE),
      P_t_bn_mean           = mean(P_t_bn,                na.rm = TRUE),
      delta_mean            = mean(delta,                 na.rm = TRUE),
      .groups = "drop"
    )
  write_csv(decomp, file.path(OUTPUT_DIR, paste0("decomposition_lag", P_LAG, ".csv")))
  cat("\n=== Annual decomposition (P_LAG =", P_LAG, ") ===\n")
  print(as.data.frame(decomp))
  
  # Plot: delta_t over time
  delta_df <- out %>% filter(!is.na(delta)) %>% select(date, delta)
  
  # ── PSPP TOGGLE: only show bands for programmes actually in the series ────
  programme_bands_all <- data.frame(
    xmin  = as.Date(c("2010-05-01", "2015-03-01", "2020-03-01")),
    xmax  = as.Date(c("2012-09-01", "2022-06-01", "2022-03-01")),
    label = c("SMP", "PSPP", "PEPP")
  )
  programme_bands <- if (EXCLUDE_PSPP) {
    programme_bands_all %>% filter(label != "PSPP")
  } else {
    programme_bands_all
  }
  programme_bands$ypos <- max(delta_df$delta, na.rm = TRUE) *
    c(0.9, if (!EXCLUDE_PSPP) 0.75, 0.6)[seq_len(nrow(programme_bands))]
  # ─────────────────────────────────────────────────────────────────────────
  
  p_delta <- ggplot(delta_df, aes(date, delta)) +
    geom_rect(data = programme_bands,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = label),
              inherit.aes = FALSE, alpha = 0.10) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.5, linetype = "dashed") +
    geom_line(colour = "#009E73", linewidth = 0.8) +
    scale_fill_manual(values = c(SMP = "#E69F00", PSPP = "#56B4E9", PEPP = "#CC79A7"),
                      name = "Programme") +
    labs(
      title    = sprintf("Rolling delta_t (purchase coefficient), P_LAG = %d | %s",
                         P_LAG, PURCHASE_LABEL),
      subtitle = "Negative values = purchases compress fragmentation (theory-consistent)",
      x = NULL, y = "delta_t (spread_stdev per EUR bn)"
    ) +
    theme_classic(base_size = 11) +
    theme(legend.position = "bottom",
          plot.title    = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(size = 9, colour = "grey40"))
  
  ggsave(file.path(FIG_DIR, sprintf("delta_over_time_lag%d%s.png", P_LAG, pspp_suffix)),
         plot = p_delta, width = 11, height = 4.5, dpi = 150)
  message(sprintf("Saved delta_over_time_lag%d%s.png", P_LAG, pspp_suffix))
  
  # Plot: Actual vs Stage 2 series
  lbl_actual  <- "Actual"
  lbl_fitted  <- sprintf("Fitted (Stage 1, P_{t-%d})", P_LAG)
  lbl_tilde   <- "Fundamentals + purchases (S = S̅)"
  lbl_nopurch <- "Pure fundamentals (S = S̅, P = 0)"
  
  plot_df <- out %>%
    filter(!is.na(MS_fitted)) %>%
    select(date, MS_actual, MS_fitted, MS_tilde, MS_tilde_nopurch) %>%
    pivot_longer(-date, names_to = "series", values_to = "value") %>%
    mutate(series = case_when(
      series == "MS_actual"        ~ lbl_actual,
      series == "MS_fitted"        ~ lbl_fitted,
      series == "MS_tilde"         ~ lbl_tilde,
      series == "MS_tilde_nopurch" ~ lbl_nopurch,
      TRUE ~ series
    ))
  
  pal <- setNames(c("black","#D55E00","#009E73","#0072B2"),
                  c(lbl_actual, lbl_fitted, lbl_tilde, lbl_nopurch))
  lty <- setNames(c("solid","solid","dashed","dashed"),
                  c(lbl_actual, lbl_fitted, lbl_tilde, lbl_nopurch))
  
  p_main <- ggplot(plot_df, aes(date, value, colour = series, linetype = series)) +
    geom_hline(yintercept = c(0,3,6,9), colour = "grey85", linewidth = 0.3) +
    geom_line(linewidth = 0.7, na.rm = TRUE) +
    scale_colour_manual(values = pal) +
    scale_linetype_manual(values = lty) +
    labs(title    = sprintf("Sovereign Spread Dispersion: Model 2 with P_{t-%d} | %s",
                            P_LAG, PURCHASE_LABEL),
         x = NULL, y = "spread_stdev", colour = NULL, linetype = NULL) +
    theme_classic(base_size = 11) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 13))
  
  ggsave(file.path(FIG_DIR, sprintf("predicted_vs_actual_lag%d%s.png", P_LAG, pspp_suffix)),
         plot = p_main, width = 11, height = 5, dpi = 150)
  message(sprintf("Saved predicted_vs_actual_lag%d%s.png", P_LAG, pspp_suffix))
  
  # Multi-lag comparison
  if (RUN_ALL_LAGS) {
    message("\n=== Running comparison across lags: ", paste(LAGS_TO_COMPARE, collapse=", "), " ===")
    rmse_all <- lapply(LAGS_TO_COMPARE, function(k) {
      message(sprintf("  lag = %d ...", k))
      tryCatch(run_model_for_lag(k, include_P = TRUE)$rmse,
               error = function(e) { message("  ERROR lag ", k, ": ", conditionMessage(e)); NULL })
    })
    rmse_comparison <- bind_rows(Filter(Negate(is.null), rmse_all))
    m1_path <- file.path(ROOT, "output", "regression", "model1_rolling", "rmse_summary.csv")
    if (file.exists(m1_path)) {
      m1 <- read_csv(m1_path, show_col_types = FALSE) %>%
        transmute(lag = NA_integer_, model = "Model 1 (no purchases)",
                  rmse_fitted_vs_actual, rmse_tilde_vs_actual,
                  rmse_tilde_nopurch_vs_actual = NA_real_,
                  delta_mean = NA_real_, delta_sd = NA_real_, delta_pct_negative = NA_real_,
                  n_windows, n_params)
      rmse_comparison <- rmse_comparison %>%
        mutate(model = paste0("Model 2 (lag = ", lag, ")")) %>%
        bind_rows(m1) %>% arrange(lag)
    } else {
      rmse_comparison <- rmse_comparison %>%
        mutate(model = paste0("Model 2 (lag = ", lag, ")")) %>% arrange(lag)
    }
    out_dir_lag <- file.path(ROOT, "output", "regression", "model2_variants",
                             paste0("lag_comparison", pspp_suffix))
    dir.create(out_dir_lag, showWarnings = FALSE, recursive = TRUE)
    write_csv(rmse_comparison, file.path(out_dir_lag, "rmse_lag_comparison.csv"))
    message("Saved rmse_lag_comparison.csv")
    print(as.data.frame(rmse_comparison %>%
                          select(model, lag, rmse_fitted_vs_actual, rmse_tilde_vs_actual,
                                 rmse_tilde_nopurch_vs_actual, delta_mean, delta_sd, delta_pct_negative)))
  }
  
  message("\nDone. Primary outputs in '", OUTPUT_DIR, "/'.")
}
# ============================================================
# ══════════════════════════════════════════════════════════
#   ANNOUNCEMENTS MODE — Model 1 + t-tests around announcement dates
# ══════════════════════════════════════════════════════════
#
# Replicates the approach of Kakes & Van den End (2024) Table D:
#   For each programme (SMP, OMT, PSPP, PEPP, TPI) and window
#   [-1, +W] months, test H0: mean(MS_actual - MS_tilde) = 0.
#   Rejection suggests the announcement shifted observed dispersion
#   away from the fundamentals-implied path.
#
# The t-test is one-sample: t.test(deviation, mu = 0).
# The full-sample result serves as a cross-check: if H0 is also
# rejected there it merely reflects systematic model mis-fit, not
# an announcement-specific effect.
# ============================================================
if (ANALYSIS_MODE == "ANNOUNCEMENTS") {
  
  OUTPUT_DIR_ANN <- file.path(ROOT, "output", "regression", "announcements")
  dir.create(OUTPUT_DIR_ANN, showWarnings = FALSE, recursive = TRUE)
  dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
  
  # Optional: fixed-parameter model results for side-by-side comparison
  # (produced by Fixed-Regression_Model2.R)
  FIXED_MODEL_FILE <- file.path(ROOT, "output", "regression", "model2_fixed",
                                "fixed_regression_model2_results.csv")
  
  # ── Run Model 1 (rolling, no purchase variable) ───────────────────────────
  message("\n=== ANNOUNCEMENTS mode: running Model 1 (rolling, no purchases) ===")
  res1 <- run_model_for_lag(k = 0, include_P = FALSE)
  out  <- res1$out
  write_csv(out, file.path(OUTPUT_DIR_ANN, "rolling_model1_results.csv"))
  message("Model 1 RMSE:"); print(res1$rmse)
  
  # Rolling deviation: MS_actual - MS_tilde_nopurch (= alpha + beta'*MF at S=S_bar)
  dev_roll <- out %>%
    filter(!is.na(MS_tilde_nopurch), !is.na(MS_actual)) %>%
    mutate(deviation = MS_actual - MS_tilde_nopurch) %>%
    select(date, deviation)
  
  # ── Load fixed-parameter results (if available) ───────────────────────────
  if (file.exists(FIXED_MODEL_FILE)) {
    dev_fixed <- read_csv(FIXED_MODEL_FILE, show_col_types = FALSE) %>%
      mutate(date = as.Date(date)) %>%
      filter(!is.na(MS_actual), !is.na(fundamental_component)) %>%
      # fundamental_component = alpha + beta'*MF for the fixed model (= MS_tilde_nopurch)
      mutate(deviation = MS_actual - fundamental_component) %>%
      select(date, deviation)
    message("Loaded fixed-parameter model results for comparison.")
  } else {
    dev_fixed <- NULL
    message("Fixed-model file not found at: ", FIXED_MODEL_FILE,
            "\nFixed parameters column will show 'n.a.'")
  }
  
  # ── Load ECB announcement dates ───────────────────────────────────────────
  FALLBACK_DATES <- data.frame(
    programme = c("SMP",  "OMT",  "PSPP", "PEPP", "TPI"),
    date      = as.Date(c("2010-05-01", "2012-09-01", "2015-01-01",
                          "2020-03-01", "2022-07-01"))
  )
  
  if (file.exists(ANNOUNCEMENTS_FILE)) {
    announcements_raw <- read_csv(ANNOUNCEMENTS_FILE, show_col_types = FALSE) %>%
      mutate(date = as.Date(paste0(date, "-01"))) %>%
      filter(programme %in% INCLUDE_PROGRAMMES)
    
    announcements <- bind_rows(lapply(INCLUDE_PROGRAMMES, function(p) {
      rows <- announcements_raw %>% filter(programme == p)
      if (nrow(rows) == 0) {
        message("  Programme '", p, "' not found; using fallback date.")
        FALLBACK_DATES %>% filter(programme == p)
      } else rows %>% slice_min(date, n = 1)
    }))
  } else {
    message("Announcements file not found; using hardcoded fallback dates.")
    announcements <- FALLBACK_DATES %>% filter(programme %in% INCLUDE_PROGRAMMES)
  }
  
  message("\nAnnouncement dates used:")
  print(as.data.frame(announcements %>% select(programme, date)))
  
  # ── Helpers ───────────────────────────────────────────────────────────────
  
  # Format p-value as "0.00 ***" or "0.35"
  fmt_pval <- function(deviations) {
    x <- deviations[!is.na(deviations)]
    if (length(x) < 3) return("n.a.")
    tt    <- t.test(x, mu = 0)
    stars <- dplyr::case_when(
      tt$p.value < 0.01 ~ "***",
      tt$p.value < 0.05 ~ "**",
      tt$p.value < 0.10 ~ "*",
      TRUE              ~ ""
    )
    trimws(sprintf("%.2f %s", tt$p.value, stars))
  }
  
  # Collect pooled window observations across all programmes (no double-counting)
  get_pool <- function(dev_df, window_months) {
    bind_rows(lapply(seq_len(nrow(announcements)), function(j) {
      ann <- announcements$date[j]
      dev_df %>% filter(date >= ann %m-% months(1),
                        date <= ann %m+% months(window_months))
    })) %>%
      distinct(date, .keep_all = TRUE) %>%
      pull(deviation)
  }
  
  # Collect pooled window observations for a programme subset (no double-counting)
  get_pool_subset <- function(dev_df, window_months, progs) {
    ann_sub <- announcements %>% filter(programme %in% progs)
    bind_rows(lapply(seq_len(nrow(ann_sub)), function(j) {
      ann <- ann_sub$date[j]
      dev_df %>% filter(date >= ann %m-% months(1),
                        date <= ann %m+% months(window_months))
    })) %>%
      distinct(date, .keep_all = TRUE) %>%
      pull(deviation)
  }
  
  # ── Build Table D (pooled across programmes) ──────────────────────────────
  table_d_rows <- list()
  for (w in ANNOUNCEMENT_WINDOWS) {
    
    # All programmes pooled
    table_d_rows[[length(table_d_rows) + 1]] <- data.frame(
      window           = sprintf("[-1, +%d]", w),
      rolling_window   = fmt_pval(get_pool(dev_roll,  w)),
      fixed_parameters = if (!is.null(dev_fixed))
        fmt_pval(get_pool(dev_fixed, w)) else "n.a.",
      stringsAsFactors = FALSE
    )
    
    # Subset pools (e.g. SMP + OMT + PEPP)
    for (sname in names(POOL_SUBSETS)) {
      table_d_rows[[length(table_d_rows) + 1]] <- data.frame(
        window           = sprintf("[-1, +%d] (%s)", w, sname),
        rolling_window   = fmt_pval(get_pool_subset(dev_roll,  w, POOL_SUBSETS[[sname]])),
        fixed_parameters = if (!is.null(dev_fixed))
          fmt_pval(get_pool_subset(dev_fixed, w, POOL_SUBSETS[[sname]])) else "n.a.",
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Full-sample cross-check row
  table_d_rows[[length(table_d_rows) + 1]] <- data.frame(
    window           = "Full sample",
    rolling_window   = fmt_pval(dev_roll$deviation),
    fixed_parameters = if (!is.null(dev_fixed))
      fmt_pval(dev_fixed$deviation) else "n.a.",
    stringsAsFactors  = FALSE
  )
  
  table_d <- bind_rows(table_d_rows)
  
  # ── Print Table D ─────────────────────────────────────────────────────────
  cat("\n")
  cat("Table D. T-test for mean difference between observed and model-implied\n")
  cat("spread dispersion in windows around programme announcements\n")
  cat("Programmes pooled: ", paste(INCLUDE_PROGRAMMES, collapse = ", "), "\n\n", sep = "")
  cat(sprintf("  %-16s  %-20s  %-20s\n", "", "Rolling window", "Fixed parameters"))
  cat(sprintf("  %-16s  %-20s  %-20s\n", "window", "stdev", "stdev"))
  cat("  ", strrep("-", 58), "\n", sep = "")
  for (i in seq_len(nrow(table_d))) {
    cat(sprintf("  %-16s  %-20s  %-20s\n",
                table_d$window[i],
                table_d$rolling_window[i],
                table_d$fixed_parameters[i]))
  }
  cat("\n  P-values shown. ***, **, * = rejection of H0 at 1%, 5%, 10%.\n")
  cat("  H0: mean(MS_actual - MS_tilde) = 0 in the window.\n")
  cat("  One-sample two-tailed t-test on pooled deviations.\n")
  cat("  Overlapping window months counted once.\n\n")
  
  write_csv(table_d, file.path(OUTPUT_DIR_ANN, "tableD_announcement_ttest.csv"))
  message("Saved tableD_announcement_ttest.csv to '", OUTPUT_DIR_ANN, "/'")
  
  # ── Per-programme breakdown (supplementary) ───────────────────────────────
  sig_stars <- function(p) {
    dplyr::case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
  }
  
  run_ttest_row <- function(x, window_label, prog_label) {
    x <- x[!is.na(x)]
    if (length(x) < 3)
      return(data.frame(window = window_label, programme = prog_label,
                        n = length(x), mean_dev = NA_real_,
                        t_stat = NA_real_, p_value = NA_real_, stars = "n.a.",
                        stringsAsFactors = FALSE))
    tt <- t.test(x, mu = 0)
    data.frame(window = window_label, programme = prog_label,
               n = length(x),
               mean_dev = round(as.numeric(tt$estimate), 4),
               t_stat   = round(as.numeric(tt$statistic), 3),
               p_value  = round(tt$p.value, 4),
               stars    = sig_stars(tt$p.value),
               stringsAsFactors = FALSE)
  }
  
  per_prog_rows <- list()
  for (w in ANNOUNCEMENT_WINDOWS) {
    wlbl <- sprintf("[−1, +%d]", w)
    for (prog in INCLUDE_PROGRAMMES) {
      ann_row <- announcements %>% filter(programme == prog)
      if (nrow(ann_row) == 0) next
      ann_date <- ann_row$date[1]
      wd <- dev_roll %>%
        filter(date >= ann_date %m-% months(1),
               date <= ann_date %m+% months(w)) %>%
        pull(deviation)
      per_prog_rows[[length(per_prog_rows) + 1]] <- run_ttest_row(wd, wlbl, prog)
    }
    # Pooled
    pool_devs <- get_pool(dev_roll, w)
    per_prog_rows[[length(per_prog_rows) + 1]] <-
      run_ttest_row(pool_devs, wlbl, "All (pooled)")
  }
  # Full sample
  per_prog_rows[[length(per_prog_rows) + 1]] <-
    run_ttest_row(dev_roll$deviation, "Full sample", "Cross-check")
  
  per_prog_df <- bind_rows(per_prog_rows)
  write_csv(per_prog_df, file.path(OUTPUT_DIR_ANN, "tableD_per_programme.csv"))
  
  cat("=== Per-programme breakdown (rolling window) ===\n")
  print(as.data.frame(per_prog_df))
  
  # ── Plot: deviation time series with announcement lines ───────────────────
  ann_colours <- c(SMP = "#E69F00", OMT = "#CC79A7", PSPP = "#56B4E9",
                   PEPP = "#009E73", TPI = "#D55E00")
  ann_colours_used <- ann_colours[names(ann_colours) %in% INCLUDE_PROGRAMMES]
  
  y_max   <- max(dev_roll$deviation, na.rm = TRUE)
  y_label <- y_max * 0.96
  
  p_ann <- ggplot(dev_roll, aes(date, deviation)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.45, linetype = "dashed") +
    geom_ribbon(aes(ymin = 0,                  ymax = pmax(deviation, 0)),
                fill = "#f4a582", alpha = 0.45) +
    geom_ribbon(aes(ymin = pmin(deviation, 0), ymax = 0),
                fill = "#92c5de", alpha = 0.45) +
    geom_line(colour = "black", linewidth = 0.7) +
    geom_vline(data = announcements,
               aes(xintercept = date, colour = programme),
               linewidth = 0.7) +
    geom_text(data = announcements,
              aes(x = date, y = y_label, label = programme, colour = programme),
              vjust = 1, hjust = -0.15, size = 3, fontface = "bold") +
    scale_colour_manual(values = ann_colours_used, guide = "none") +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                 limits = c(START_DATE, END_DATE), expand = c(0.01, 0)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
    labs(x = NULL,
         y = "Non-fundamental component  (MS_actual − MS_tilde)",
         title = "Non-fundamental spread dispersion and ECB programme announcements") +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey88"),
          plot.title         = element_text(face = "bold", size = 12),
          plot.margin        = margin(10, 16, 8, 10))
  
  ggsave(file.path(OUTPUT_DIR_ANN, "announcement_deviation_plot.png"),
         p_ann, width = 11, height = 5, dpi = 150)
  message("Saved announcement_deviation_plot.png")
  
  # ── Pre-post comparison ───────────────────────────────────────────────────
  models_list <- list(rolling = dev_roll)
  if (!is.null(dev_fixed)) models_list$fixed <- dev_fixed
  
  prepost_rows <- list()
  
  for (model_name in names(models_list)) {
    dev_df_pp <- models_list[[model_name]]
    
    for (w in ANNOUNCEMENT_WINDOWS) {
      win_label <- sprintf("[-1, +%d]", w)
      
      # Per-programme tests
      for (prog in INCLUDE_PROGRAMMES) {
        ann_row <- announcements %>% filter(programme == prog)
        if (nrow(ann_row) == 0) next
        ann_date <- ann_row$date[1]
        
        pre_devs <- dev_df_pp %>%
          filter(date >= ann_date %m-% months(1), date <= ann_date) %>%
          pull(deviation)
        
        post_devs <- dev_df_pp %>%
          filter(date >  ann_date, date <= ann_date %m+% months(w)) %>%
          pull(deviation)
        
        if (length(pre_devs) < 2 || length(post_devs) < 2) {
          prepost_rows[[length(prepost_rows) + 1]] <- data.frame(
            model = model_name, window = win_label, programme = prog,
            n_pre = length(pre_devs),   mean_pre  = NA_real_,
            n_post = length(post_devs), mean_post = NA_real_,
            diff = NA_real_, t_stat = NA_real_, p_value = NA_real_,
            stars = "n.a.", direction = NA_character_,
            stringsAsFactors = FALSE
          )
          next
        }
        
        tt   <- t.test(post_devs, pre_devs)
        diff <- mean(post_devs) - mean(pre_devs)
        stars <- dplyr::case_when(
          tt$p.value < 0.01 ~ "***",
          tt$p.value < 0.05 ~ "**",
          tt$p.value < 0.10 ~ "*",
          TRUE              ~ ""
        )
        
        prepost_rows[[length(prepost_rows) + 1]] <- data.frame(
          model     = model_name,
          window    = win_label,
          programme = prog,
          n_pre     = length(pre_devs),
          mean_pre  = round(mean(pre_devs),  4),
          n_post    = length(post_devs),
          mean_post = round(mean(post_devs), 4),
          diff      = round(diff, 4),
          t_stat    = round(as.numeric(tt$statistic), 3),
          p_value   = round(tt$p.value, 4),
          stars     = stars,
          direction = if (diff < 0) "compressed" else "elevated",
          stringsAsFactors = FALSE
        )
      }
      
      # Pooled across all programmes (unique dates — no double-counting)
      pre_pool <- bind_rows(lapply(seq_len(nrow(announcements)), function(j) {
        ann <- announcements$date[j]
        dev_df_pp %>% filter(date >= ann %m-% months(1), date <= ann)
      })) %>% distinct(date, .keep_all = TRUE) %>% pull(deviation)
      
      post_pool <- bind_rows(lapply(seq_len(nrow(announcements)), function(j) {
        ann <- announcements$date[j]
        dev_df_pp %>% filter(date > ann, date <= ann %m+% months(w))
      })) %>% distinct(date, .keep_all = TRUE) %>% pull(deviation)
      
      if (length(pre_pool) >= 2 && length(post_pool) >= 2) {
        tt_pool   <- t.test(post_pool, pre_pool)
        diff_pool <- mean(post_pool) - mean(pre_pool)
        stars_pool <- dplyr::case_when(
          tt_pool$p.value < 0.01 ~ "***",
          tt_pool$p.value < 0.05 ~ "**",
          tt_pool$p.value < 0.10 ~ "*",
          TRUE                   ~ ""
        )
        prepost_rows[[length(prepost_rows) + 1]] <- data.frame(
          model     = model_name,
          window    = win_label,
          programme = "All (pooled)",
          n_pre     = length(pre_pool),
          mean_pre  = round(mean(pre_pool),  4),
          n_post    = length(post_pool),
          mean_post = round(mean(post_pool), 4),
          diff      = round(diff_pool, 4),
          t_stat    = round(as.numeric(tt_pool$statistic), 3),
          p_value   = round(tt_pool$p.value, 4),
          stars     = stars_pool,
          direction = if (diff_pool < 0) "compressed" else "elevated",
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  prepost_df <- bind_rows(prepost_rows)
  write_csv(prepost_df, file.path(OUTPUT_DIR_ANN, "prepost_comparison.csv"))
  
  cat("\n=== Pre-Post Comparison: deviation before vs. after announcement ===\n")
  cat("pre  = [-1, 0]  (month before + announcement month)\n")
  cat("post = [+1, +W] (W months following announcement)\n")
  cat("diff = mean(post) - mean(pre)\n")
  cat("'compressed' = diff < 0 (spreads fell relative to fundamentals after announcement)\n")
  cat("'elevated'   = diff > 0 (spreads rose relative to fundamentals after announcement)\n")
  cat("Two-sample two-tailed t-test. *** p<0.01  ** p<0.05  * p<0.10\n\n")
  print(as.data.frame(prepost_df))
  message("Saved prepost_comparison.csv to '", OUTPUT_DIR_ANN, "/'")
  
  message("\nDone. All outputs in '", OUTPUT_DIR_ANN, "/'.")
}