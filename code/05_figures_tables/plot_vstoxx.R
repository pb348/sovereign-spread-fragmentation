# plot_vstoxx.R
# Output: final_vstoxx.png

library(tidyverse)
library(ggplot2)

# Paths anchor at the repository root, located via the {here} package
# (uses the `.here` marker file; run from any working directory).
ROOT <- here::here()

VAR_DIR  <- file.path(ROOT, "data", "variables")
OUT_DIR  <- file.path(ROOT, "output", "figures")

vstoxx <- read_csv(
  file.path(VAR_DIR, "vstoxx_monthly.csv"),
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(paste0(date, "-01")))   # "1999-01" → "1999-01-01"

p <- ggplot(vstoxx, aes(date, vstoxx)) +
  geom_line(colour = "#2166ac", linewidth = 0.5) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0.01, 0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(
    title = "VSTOXX — Monthly",
    x     = NULL,
    y     = "VSTOXX"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(size = 9)
  )

ggsave(file.path(OUT_DIR, "final_vstoxx.png"),
       p, width = 10, height = 4.5, dpi = 150)
message("✓ saved final_vstoxx.png")