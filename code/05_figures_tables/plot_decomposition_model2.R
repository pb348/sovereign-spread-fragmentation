# Fig4_decomposition_ecb_style.R
# Stacked bar decomposition of sovereign spread dispersion — ECB chart style.
# Matches the visual format of ECB Economic Bulletin Chart B (HICPX decomposition):
#   stacked monthly bars + actual-value line overlay.
#
# Components (bars, stacked):
#   Fundamental          — dark navy blue   (#1F3A7D)
#   ECB purchases        — medium green     (#5CB85C)
#   Non-fundamental /
#     sentiment          — amber/yellow     (#FFBE00)
# Actual spread dispersion — black line
#
# Data: regression-results-fixed-model2/fixed_regression_model2_results.csv
#       (fixed-parameter Model 2, P_{t-1})
# Output: output/figures/fig4_decomposition_ecb_style.png

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)

# Paths anchor at the repository root, located via the {here} package
# (uses the `.here` marker file; run from any working directory).
ROOT <- here::here()

DATA_FILE  <- file.path(ROOT, "output", "regression", "model2_fixed",
                        "fixed_regression_model2_results.csv")
OUT_FILE   <- file.path(ROOT, "output", "figures", "fig4_decomposition_ecb_style.png")
dir.create(file.path(ROOT, "output", "figures"), showWarnings = FALSE, recursive = TRUE)

# ── Load monthly decomposition ────────────────────────────────────────────────
df <- read_csv(DATA_FILE, show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(MS_fitted)) %>%
  select(date,
         Fundamental          = fundamental_component,
         `ECB purchases`      = purchase_component,
         `Non-fundamental`    = sentiment_component,
         Actual               = MS_actual)

# ── Reshape for stacked bars ──────────────────────────────────────────────────
# ggplot2 geom_col with position = "stack" handles negative bars correctly:
# positive components stack upward, negative ones stack downward.
bars <- df %>%
  pivot_longer(c(Fundamental, `ECB purchases`, `Non-fundamental`),
               names_to = "component", values_to = "value") %>%
  mutate(component = factor(component,
                            levels = c("Fundamental",
                                       "ECB purchases",
                                       "Non-fundamental")))

# ── ECB-style colour palette ──────────────────────────────────────────────────
pal <- c(
  "Fundamental"       = "#1F3A7D",   # dark navy  (ECB demand-driven blue)
  "ECB purchases"     = "#5CB85C",   # medium green (ECB supply-driven green)
  "Non-fundamental"   = "#FFBE00"    # amber/yellow (ECB ambiguous yellow)
)

# ── Plot ──────────────────────────────────────────────────────────────────────
p4 <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  # Stacked bars (monthly)
  geom_col(data = bars,
           aes(x = date, y = value, fill = component),
           position = "stack",
           width     = 28,        # ~1 month in days
           colour    = NA) +
  # Actual spread dispersion — black line overlay
  geom_line(data = df,
            aes(x = date, y = Actual),
            colour    = "black",
            linewidth = 0.75,
            inherit.aes = FALSE) +
  # Colour scale
  scale_fill_manual(
    values = pal,
    labels = c("Fundamental", "ECB purchases (δ · P_{t-1})", "Non-fundamental / sentiment")
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand      = c(0.005, 0)
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 7),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  labs(
    x    = NULL,
    y    = NULL,
    fill = NULL
  ) +
  guides(fill = guide_legend(
    nrow          = 1,
    keywidth      = unit(0.5, "cm"),
    keyheight     = unit(0.35, "cm"),
    label.theme   = element_text(size = 8, family = "sans"),
    override.aes  = list(colour = NA)
  )) +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.35),
    axis.text         = element_text(size = 9),
    legend.position   = "bottom",
    legend.margin     = margin(t = 2),
    legend.spacing.x  = unit(0.4, "cm"),
    plot.margin       = margin(10, 16, 6, 10)
  )

ggsave(OUT_FILE, p4, width = 9, height = 5, dpi = 150)
message("Saved ", OUT_FILE)