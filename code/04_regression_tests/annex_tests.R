# annex_tests.R — statistical tests for thesis annex
# Annex 1: stability (regressor persistence, RMSE rolling vs fixed)
# Annex 2: endogeneity (2SLS, weak instruments, Hansen J, Wu-Hausman)
# Annex 3: unit roots (ADF) + cointegration (Engle-Granger, Johansen)
# Annex 4: residual diagnostics (BG, BP, JB, Newey-West SE comparison)
# Outputs -> output/tests/annex/

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(tseries)    # adf.test, jarque.bera.test
library(urca)       # ca.jo
library(AER)        # ivreg
library(lmtest)     # bgtest, bptest, coeftest
library(sandwich)   # NeweyWest
library(flextable)
library(officer)

# Paths anchor at the repository root, located via the {here} package
# (uses the `.here` marker file; run from any working directory).
ROOT <- here::here()

# ── Settings: mirror rolling_regression_reduced.R ────────────────────────────
WINDOW           <- 60
START_DATE       <- as.Date("2005-09-01")
END_DATE         <- as.Date("2025-12-01")
NORMAL_REF_START <- as.Date("2005-09-01")
NORMAL_REF_END <- as.Date("2022-12-01")
INPUT_FILE       <- file.path(ROOT, "data", "processed", "moments_forecasted_interpolated.csv")

MF_VARS <- c("gdp_growth_stdev", "inflation_stdev", "debt_gdp_stdev",
             "current_account_stdev", "policy_uncertainty_stdev",
             "bank_nexus_stdev", "bid_ask_stdev")
INTERACT_VARS <- MF_VARS
MS_VAR  <- "spread_stdev"

# Endogeneity test setup (mirrors K&vdE Annex 2: the four De Grauwe & Ji
# fundamentals are treated as potentially endogenous; instruments are their
# 3- and 6-month lags, i.e. one and two quarter lags on monthly data)
ENDOG_VARS <- c("gdp_growth_stdev", "inflation_stdev",
                "debt_gdp_stdev", "current_account_stdev")
EXOG_VARS  <- c("policy_uncertainty_stdev", "bank_nexus_stdev",
                "bid_ask_stdev", "vstoxx")

# Representative window end-date for the within-window persistence table
# (Table A1.2). Pick a window inside the unstable region; adjust as needed.
PERSISTENCE_DIAG_DATE <- as.Date("2017-06-01")

ANNEX_DIR <- file.path(ROOT, "output", "tests", "annex")
dir.create(ANNEX_DIR, recursive = TRUE, showWarnings = FALSE)

# ── Load data (same as main script) ──────────────────────────────────────────
moments <- read_csv(INPUT_FILE, show_col_types = FALSE) %>%
  mutate(date = as.Date(paste0(date, "-01")))
vstoxx <- read_csv(file.path(ROOT, "data", "variables", "vstoxx_monthly.csv"), show_col_types = FALSE) %>%
  mutate(date = as.Date(paste0(date, "-01")))

df <- moments %>%
  select(date, all_of(MS_VAR), all_of(MF_VARS)) %>%
  left_join(vstoxx, by = "date") %>%
  arrange(date) %>%
  filter(date >= START_DATE, date <= END_DATE) %>%
  drop_na(all_of(c(MS_VAR, MF_VARS, "vstoxx")))

n_obs <- nrow(df)

df_normal_ref <- df %>% filter(date >= NORMAL_REF_START, date <= NORMAL_REF_END)
FULL_MF_BAR <- colMeans(df_normal_ref[, MF_VARS], na.rm = TRUE)
FULL_S_BAR  <- mean(df_normal_ref$vstoxx, na.rm = TRUE)

# ── Helpers ──────────────────────────────────────────────────────────────────
star <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**",
                                                   ifelse(p < 0.10, "*", "")))

save_docx_table <- function(tbl, caption, file, note = NULL) {
  ft <- flextable(tbl) %>%
    bold(part = "header") %>%
    hline_top(part = "header", border = fp_border(width = 1.5)) %>%
    hline_bottom(part = "header", border = fp_border(width = 1)) %>%
    hline_bottom(part = "body", border = fp_border(width = 1.5)) %>%
    align(j = 1, align = "left", part = "all") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 8, part = "all") %>%
    padding(padding.top = 1, padding.bottom = 1, part = "all") %>%
    autofit() %>%
    set_caption(caption = caption, style = "Table Caption")
  if (!is.null(note)) {
    ft <- ft %>% add_footer_lines(note) %>% italic(part = "footer") %>%
      font(fontname = "Times New Roman", part = "footer") %>%
      fontsize(size = 7, part = "footer")
  }
  doc <- read_docx() %>% body_add_flextable(ft)
  print(doc, target = file.path(ANNEX_DIR, file))
  message("Saved ", file.path(ANNEX_DIR, file))
}

# ── Fixed-parameter model (full sample) — needed by Annexes 1, 3, 4 ─────────
inter_fx <- sweep(as.matrix(df[, INTERACT_VARS]), 2,
                  FULL_MF_BAR[INTERACT_VARS], FUN = "-") *
  (df$vstoxx - FULL_S_BAR)
colnames(inter_fx) <- paste0("inter_", INTERACT_VARS)

reg_fx <- df %>%
  select(all_of(MS_VAR), all_of(MF_VARS), vstoxx) %>%
  bind_cols(as.data.frame(inter_fx))

form_fx <- as.formula(paste0(
  MS_VAR, " ~ ", paste(MF_VARS, collapse = " + "),
  " + vstoxx + ", paste(colnames(inter_fx), collapse = " + ")))

fit_fixed <- lm(form_fx, data = reg_fx)

# ============================================================
# ANNEX 3: Unit root tests (ADF, 1 lag) — Table A3.1
# ============================================================
adf_vars <- c(MS_VAR, MF_VARS, "vstoxx")
adf_tbl <- bind_rows(lapply(adf_vars, function(v) {
  a <- suppressWarnings(adf.test(na.omit(df[[v]]), k = 1))
  data.frame(Variable = v,
             `ADF statistic` = sprintf("%.2f%s", a$statistic, star(a$p.value)),
             check.names = FALSE)
}))
save_docx_table(adf_tbl,
                "Table A3.1: Augmented Dickey-Fuller unit root tests (1 lag)",
                "tableA3_1_unit_roots.docx",
                note = "***, **, * denote rejection of the unit root hypothesis at the 1%, 5%, 10% level. Reported p-values from adf.test are interpolated and truncated at [0.01, 0.99].")

# ============================================================
# ANNEX 3: Cointegration — Table A3.2
# Engle-Granger: ADF on residuals of the static levels regression
# MS ~ MF + vstoxx (no interactions). Johansen trace test on the
# same variable system.
# ============================================================
form_static <- as.formula(paste0(MS_VAR, " ~ ",
                                 paste(MF_VARS, collapse = " + "), " + vstoxx"))
fit_static <- lm(form_static, data = df)
eg <- suppressWarnings(adf.test(residuals(fit_static), k = 1))

jo <- ca.jo(as.matrix(df[, c(MS_VAR, MF_VARS, "vstoxx")]),
            type = "trace", ecdet = "const", K = 2)
jo_stat <- jo@teststat[length(jo@teststat)]   # r = 0 row
jo_cv   <- jo@cval[nrow(jo@cval), ]           # r = 0 critical values (10/5/1%)
jo_sig  <- if (jo_stat > jo_cv["1pct"]) "***" else
  if (jo_stat > jo_cv["5pct"]) "**"  else
    if (jo_stat > jo_cv["10pct"]) "*"  else ""

coint_tbl <- data.frame(
  Test = c("Engle-Granger (ADF on residuals, 1 lag)",
           "Johansen trace test (H0: r = 0, K = 2)"),
  Statistic = c(sprintf("%.2f%s", eg$statistic, star(eg$p.value)),
                sprintf("%.2f%s", jo_stat, jo_sig)),
  check.names = FALSE)
save_docx_table(coint_tbl,
                "Table A3.2: Cointegration tests (H0: no cointegration)",
                "tableA3_2_cointegration.docx",
                note = "***, **, * denote rejection of H0 at 1%, 5%, 10%. Engle-Granger significance is indicative only: adf.test p-values use Dickey-Fuller rather than Engle-Granger critical values, which are stricter; the Johansen test is the primary evidence.")

# ============================================================
# ANNEX 2: Endogeneity — mirrors K&vdE Annex 2 / Table A structure
# ------------------------------------------------------------
# Tests, in their order:
#   (1) Underidentification: Anderson canonical-correlations LM
#       (= Kleibergen-Paap rk LM under i.i.d. errors; K&vdE report
#       the KP statistic from Stata ivreg2 with robust errors)
#   (2) Weak instruments: Cragg-Donald F (rule of thumb F > 10)
#   (3) Overidentification: Hansen J (heteroskedasticity-robust;
#       the robust counterpart of the Sargan statistic)
#   (4) Endogeneity of the suspect regressors: robust
#       Durbin-Wu-Hausman chi-square (augmented regression with
#       first-stage residuals, HC-robust Wald test)
# Instruments: one and two quarter lags (3 and 6 months) of the
# four De Grauwe & Ji fundamentals, as in K&vdE.
# ============================================================
df_iv <- df %>%
  mutate(across(all_of(ENDOG_VARS),
                list(l3 = ~lag(.x, 3), l6 = ~lag(.x, 6)),
                .names = "{.col}_{.fn}")) %>%
  drop_na()

inst_vars <- c(paste0(ENDOG_VARS, "_l3"), paste0(ENDOG_VARS, "_l6"))

n_iv <- nrow(df_iv)
W  <- cbind(`(Intercept)` = 1, as.matrix(df_iv[, EXOG_VARS]))  # included exogenous
Xe <- as.matrix(df_iv[, ENDOG_VARS])                            # suspect endogenous
Z  <- as.matrix(df_iv[, inst_vars])                             # excluded instruments
y  <- df_iv[[MS_VAR]]

K_end <- ncol(Xe)   # 4 endogenous regressors
L_exc <- ncol(Z)    # 8 excluded instruments
k_W   <- ncol(W)    # included exogenous incl. intercept

# Partial out the included exogenous variables (Frisch-Waugh)
resid_on_W <- function(A) A - W %*% solve(crossprod(W), crossprod(W, A))
Xt <- resid_on_W(Xe)
Zt <- resid_on_W(Z)

# Canonical correlations between (partialled) endogenous vars & instruments
cc      <- cancor(Xt, Zt)$cor
r2_min  <- min(cc)^2

# (1) Anderson LM (underidentification): H0 = smallest canonical
# correlation is zero, i.e. the instrument matrix is rank-deficient.
anderson_lm   <- n_iv * r2_min
anderson_df   <- L_exc - K_end + 1
anderson_p    <- pchisq(anderson_lm, df = anderson_df, lower.tail = FALSE)

# (2) Cragg-Donald Wald F (weak instruments): minimum-eigenvalue
# first-stage F. No p-value reported (K&vdE likewise rely on the
# F > 10 rule of thumb rather than Stock-Yogo critical values).
cd_f <- ((n_iv - k_W - L_exc) / L_exc) * (r2_min / (1 - r2_min))

# 2SLS estimation (needed for (3) and the coefficient comparison)
form_iv <- as.formula(paste0(
  MS_VAR, " ~ ", paste(c(ENDOG_VARS, EXOG_VARS), collapse = " + "),
  " | ", paste(c(EXOG_VARS, inst_vars), collapse = " + ")))
fit_iv <- ivreg(form_iv, data = df_iv)

# (3) Hansen J (overidentification, robust): J = u'Z (Z'diag(u^2)Z)^-1 Z'u
# with Z the full instrument matrix (included exogenous + excluded
# instruments) and u the 2SLS residuals. df = L - K.
u_iv   <- residuals(fit_iv)
Z_full <- cbind(W, Z)
S_hat  <- crossprod(Z_full * u_iv)          # Z' diag(u^2) Z  (HC0)
g_bar  <- crossprod(Z_full, u_iv)
hansen_j  <- as.numeric(t(g_bar) %*% solve(S_hat) %*% g_bar)
hansen_df <- L_exc - K_end
hansen_p  <- pchisq(hansen_j, df = hansen_df, lower.tail = FALSE)

# (4) Endogeneity test of the endogenous regressors: robust
# Durbin-Wu-Hausman. Augment the structural equation with the
# first-stage residuals V_hat; under H0 (regressors exogenous) their
# coefficients are jointly zero. HC1-robust Wald chi-square, df = 4.
V_hat <- Xe - cbind(W, Z) %*% solve(crossprod(cbind(W, Z)),
                                    crossprod(cbind(W, Z), Xe))
colnames(V_hat) <- paste0("v_", ENDOG_VARS)

aug_data <- data.frame(y = y, Xe, df_iv[, EXOG_VARS], V_hat,
                       check.names = FALSE)
form_aug <- as.formula(paste0(
  "y ~ ", paste(c(ENDOG_VARS, EXOG_VARS, colnames(V_hat)), collapse = " + ")))
fit_aug <- lm(form_aug, data = aug_data)

vc_aug   <- vcovHC(fit_aug, type = "HC1")
idx_v    <- colnames(V_hat)
b_v      <- coef(fit_aug)[idx_v]
dwh_chi2 <- as.numeric(t(b_v) %*% solve(vc_aug[idx_v, idx_v]) %*% b_v)
dwh_df   <- length(idx_v)
dwh_p    <- pchisq(dwh_chi2, df = dwh_df, lower.tail = FALSE)

# ── Table A2.1: mirrors K&vdE Table A (single column: stdev) ────────────────
iv_tbl <- data.frame(
  `Test statistics` = c(
    "Underidentification / weak instruments:",
    "Anderson canonical corr. LM statistic",
    "Cragg-Donald F statistic",
    "Overidentification / exogeneity of instruments:",
    "Hansen J statistic",
    "Endogeneity test of endogenous regressors:",
    "Chi-square test"),
  Stdev = c(
    "",
    sprintf("%.2f (%.4f)%s", anderson_lm, anderson_p, star(anderson_p)),
    sprintf("%.2f", cd_f),
    "",
    sprintf("%.2f (%.4f)%s", hansen_j, hansen_p, star(hansen_p)),
    "",
    sprintf("%.2f (%.4f)%s", dwh_chi2, dwh_p, star(dwh_p))),
  check.names = FALSE)

save_docx_table(iv_tbl,
  "Table A2.1: Tests of instrumental variables and endogeneity",
  "tableA2_1_endogeneity.docx",
  note = paste0(
    "2SLS with GDP growth, inflation, debt/GDP and current account dispersion ",
    "treated as endogenous; instruments are their one and two quarter lags ",
    "(3 and 6 months), following Kakes and van den End (2023, Annex 2). ",
    "p-values in parentheses; ***, **, * denote rejection of H0 at 1%, 5%, 10%. ",
    "Anderson LM: H0 = underidentification (rejection required); equals the ",
    "Kleibergen-Paap rk LM statistic under i.i.d. errors. Cragg-Donald F: ",
    "weak instruments, rule of thumb F > 10. Hansen J (robust counterpart of ",
    "Sargan): H0 = overidentifying restrictions valid (non-rejection desired). ",
    "Endogeneity chi-square (robust Durbin-Wu-Hausman): H0 = regressors ",
    "exogenous (non-rejection supports OLS). Specification in levels without ",
    "interaction terms."))

# ── Table A2.2: OLS vs 2SLS coefficient comparison ──────────────────────────
# Backs the K&vdE closing claim ("using 2SLS yields very similar results")
# with numbers — or shows it does not hold, in which case drop the claim.
form_ols_iv <- as.formula(paste0(
  MS_VAR, " ~ ", paste(c(ENDOG_VARS, EXOG_VARS), collapse = " + ")))
fit_ols_iv <- lm(form_ols_iv, data = df_iv)

common_coefs <- intersect(names(coef(fit_ols_iv)), names(coef(fit_iv)))
cmp_tbl <- data.frame(
  Coefficient = common_coefs,
  OLS   = sprintf("%.4f", coef(fit_ols_iv)[common_coefs]),
  `2SLS` = sprintf("%.4f", coef(fit_iv)[common_coefs]),
  check.names = FALSE)

save_docx_table(cmp_tbl,
  "Table A2.2: OLS vs 2SLS coefficient estimates (levels specification)",
  "tableA2_2_ols_vs_2sls.docx",
  note = "Same specification and estimation sample as Table A2.1. Reported to assess whether the choice between OLS and instrumental variables materially affects the estimates.")

message("Annex 2 (endogeneity) tables written.")
# ============================================================
# ANNEX 4: Residual diagnostics (fixed-parameter model) — Table A4.1
# ============================================================
bg <- bgtest(fit_fixed, order = 12)
bp <- bptest(fit_fixed)
jb <- jarque.bera.test(residuals(fit_fixed))

resid_tbl <- data.frame(
  Test = c("Breusch-Godfrey (serial correlation, 12 lags)",
           "Breusch-Pagan (heteroskedasticity)",
           "Jarque-Bera (normality)"),
  Statistic = sprintf("%.2f", c(bg$statistic, bp$statistic, jb$statistic)),
  `p-value` = sprintf("%.4f%s",
                      c(bg$p.value, bp$p.value, jb$p.value),
                      star(c(bg$p.value, bp$p.value, jb$p.value))),
  check.names = FALSE)
save_docx_table(resid_tbl,
                "Table A4.1: Residual diagnostics of the fixed-parameter model",
                "tableA4_1_residual_diagnostics.docx",
                note = "***, **, * denote rejection of H0 (no serial correlation / homoskedasticity / normality) at 1%, 5%, 10%.")

# OLS vs Newey-West standard errors — Table A4.2
ct_ols <- coeftest(fit_fixed)
ct_nw  <- coeftest(fit_fixed, vcov = NeweyWest(fit_fixed, lag = 12, prewhite = FALSE))

nw_tbl <- data.frame(
  Coefficient = rownames(ct_ols),
  Estimate = sprintf("%.4f", ct_ols[, "Estimate"]),
  `OLS SE` = sprintf("%.4f", ct_ols[, "Std. Error"]),
  `Newey-West SE` = sprintf("%.4f", ct_nw[, "Std. Error"]),
  Ratio = sprintf("%.2f", ct_nw[, "Std. Error"] / ct_ols[, "Std. Error"]),
  check.names = FALSE)
save_docx_table(nw_tbl,
                "Table A4.2: OLS vs Newey-West (HAC, 12 lags) standard errors, fixed-parameter model",
                "tableA4_2_newey_west.docx",
                note = "Ratio > 1 indicates OLS standard errors understate uncertainty in the presence of serial correlation / heteroskedasticity.")

# ============================================================
# ANNEX 1: Within-window regressor persistence — Table A1.2
# ============================================================
# First-order autocorrelation of each regressor within a representative
# 60-month window, plus an effective-sample-size approximation
# n_eff = n * (1 - rho) / (1 + rho). This is the direct evidence for the
# effective-observations argument: interpolated slow-moving regressors
# contain far fewer independent data points than the nominal window
# length, which is what weakly identifies individual coefficients.
pw_end <- which(df$date == PERSISTENCE_DIAG_DATE)
if (length(pw_end) == 0) stop("PERSISTENCE_DIAG_DATE not found in data.")
pw_df <- df[(pw_end - WINDOW + 1):pw_end, ]

persist_tbl <- bind_rows(lapply(c(MF_VARS, "vstoxx"), function(v) {
  x   <- pw_df[[v]]
  rho <- cor(x[-1], x[-length(x)], use = "complete.obs")
  n_eff <- WINDOW * (1 - rho) / (1 + rho)
  data.frame(Variable = v,
             `AR(1) autocorrelation` = sprintf("%.3f", rho),
             `Approx. effective n (of 60)` = sprintf("%.1f", n_eff),
             check.names = FALSE)
}))
save_docx_table(persist_tbl,
                sprintf("Table A1.2: Within-window persistence of regressors (window ending %s)",
                        format(PERSISTENCE_DIAG_DATE, "%Y-%m")),
                "tableA1_2_persistence.docx",
                note = "AR(1) autocorrelation computed within the 60-month window. Effective sample size approximated as n(1 - rho)/(1 + rho), the variance-equivalent number of independent observations for an AR(1) process. Values near 1 indicate the regressor behaves as a trend within the window, weakly identifying its individual coefficient.")

# ============================================================
# ANNEX 1: RMSE — rolling vs fixed parameters (mirrors K&vdE Table C)
# ============================================================
cf_fx <- coef(fit_fixed)
MS_tilde_fixed <- as.numeric(cf_fx["(Intercept)"] +
                               as.matrix(df[, MF_VARS]) %*% cf_fx[MF_VARS])
rmse <- function(a, f) sqrt(mean((a - f)^2, na.rm = TRUE))

roll_rmse <- read_csv(file.path(ROOT, "output", "regression", "model1_rolling", "rmse_summary.csv"),
                      show_col_types = FALSE)

rmse_tbl <- data.frame(
  Model = c("Rolling window (60 months)", "Fixed parameters"),
  `RMSE fitted vs actual` = sprintf("%.3f",
                                    c(roll_rmse$rmse_fitted_vs_actual, rmse(df[[MS_VAR]], fitted(fit_fixed)))),
  `RMSE Stage 2 vs actual` = sprintf("%.3f",
                                     c(roll_rmse$rmse_tilde_vs_actual, rmse(df[[MS_VAR]], MS_tilde_fixed))),
  check.names = FALSE)
save_docx_table(rmse_tbl,
                "Table A1.1: RMSE of rolling-window vs fixed-parameter model",
                "tableA1_1_rmse_comparison.docx",
                note = "Stage 2 = prediction under normal market conditions (S = S_bar). Rolling-window values are in-sample (each window includes the predicted month).")

message("\nAll annex outputs written to '", ANNEX_DIR, "/'.")