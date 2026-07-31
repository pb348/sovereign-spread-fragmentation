# plot_spread_stdev.R
# Replicates Figure 2: Moments of sovereign spread distribution over time
# — stdev only, with ECB programme announcement vertical lines
# Output: final_spread_stdev.png

library(tidyverse)
library(lubridate)
library(ggplot2)

# Paths anchor at the repository root, located via the {here} package
# (uses the `.here` marker file; run from any working directory).
ROOT <- here::here()
# ── Paths ─────────────────────────────────────────────────────────────────────
PROC_DIR  <- file.path(ROOT, "data", "processed")
VAR_DIR   <- file.path(ROOT, "data", "variables")
OUT_DIR   <- file.path(ROOT, "output", "figures")

DATE_START <- as.Date("2005-09-01")
DATE_END   <- as.Date("2025-12-01")

# ── Load spread stdev ─────────────────────────────────────────────────────────
moments <- read_csv(
  file.path(PROC_DIR, "moments_forecasted_interpolated.csv"),
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= DATE_START, date <= DATE_END) %>%
  select(date, stdev = spread_stdev) %>%
  drop_na()

# ── Load ECB announcements ────────────────────────────────────────────────────
ecb <- read_csv(
  file.path(VAR_DIR, "ecb_announcements.csv"),
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  # one vertical line per programme (first announcement)
  group_by(programme) %>%
  slice_min(date, n = 1) %>%
  ungroup() %>%
  # keep only within plot window
  filter(date >= DATE_START, date <= DATE_END) %>%
  # fix label order for display
  mutate(programme = factor(programme, levels = c("SMP","OMT","PSPP","PEPP","TPI")))

# ── y range for label placement ───────────────────────────────────────────────
y_max   <- max(moments$stdev, na.rm = TRUE) * 1.05
y_label <- y_max * 0.97          # just below top of panel

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot() +
  # ECB vertical lines
  geom_vline(
    data = ecb,
    aes(xintercept = date),
    colour    = "#6baed6",
    linewidth = 0.6,
    linetype  = "solid"
  ) +
  # Programme labels at top
  geom_text(
    data = ecb,
    aes(x = date, y = y_label, label = programme),
    hjust    = 0.5,
    vjust    = 1,
    size     = 3.2,
    fontface = "bold",
    colour   = "black"
  ) +
  # Stdev line
  geom_line(
    data      = moments,
    aes(x = date, y = stdev),
    colour    = "black",
    linewidth = 0.7
  ) +
  # Axes
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    limits      = c(DATE_START, DATE_END),
    expand      = c(0.01, 0)
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 6),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title    = NULL,
    x        = NULL,
    y        = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title        = element_text(size = 11, hjust = 0.5,
                                     face = "italic", margin = margin(b = 10)),
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(colour = "grey85"),
    axis.text         = element_text(size = 9),
    legend.position   = "bottom",
    plot.margin       = margin(12, 16, 8, 10)
  )

ggsave(
  file.path(OUT_DIR, "final_spread_stdev.png"),
  p, width = 9, height = 5.5, dpi = 150
)
message("✓ saved final_spread_stdev.png")