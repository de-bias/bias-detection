# ---------- Setup ----------
# install.packages(c("tidyverse","scales","remotes"))  # if needed
# remotes::install_github("ricardo-bion/ggradar")      # ggradar

library(tidyverse)   # includes magrittr for %>%
library(scales)
library(ggradar)

# ---------- Inputs (replace with real data) ----------
set.seed(42)
vars       <- sprintf("Var_%02d", 1:20)
importance <- 1:20                       # 1 = most important
series     <- c("Twitter","Facebook","Mapp1","Mapp2")

df_raw <-
  expand_grid(variable = vars, series = series) %>%
  left_join(tibble(variable = vars, importance = importance), by = "variable") %>%
  mutate(
    value = round(runif(n(), 35, 95) + rnorm(n(), sd = 5), 1)  # demo values
  )

# ---------- Porter-style palette & formatting ----------
porter_cols <- c(
  Twitter  = "#D62728",  # red
  Facebook = "#FDBF00",  # yellow/gold
  Mapp1    = "#2CA02C",  # green
  Mapp2    = "#1F77B4"   # blue
)

porter_theme <- function(p) {
  p +
    labs(subtitle = "Axes scaled to [0,1]; variables ordered by importance") +
    theme(
      plot.title      = element_text(face = "bold", size = 12),
      plot.subtitle   = element_text(size = 9),
      legend.position = "bottom",
      legend.title    = element_blank()
    )
}

# ---------- Scale each variable to [0,1] across series ----------
scale01_by_var <- function(dat){
  dat %>%
    group_by(variable) %>%
    mutate(value01 = rescale(value, to = c(0,1))) %>%
    ungroup()
}

# ---------- Split into 4 groups of 5 by importance ----------
df <-
  df_raw %>%
  arrange(importance) %>%
  mutate(group_id = paste0("Group_", ceiling(importance/5))) %>%
  scale01_by_var()

# Wide format for ggradar (first column must be 'group')
make_wide_for_ggradar <- function(d) {
  d %>%
    select(series, variable, value01) %>%
    mutate(variable = factor(variable, levels = unique(variable))) %>%  # keep axis order
    pivot_wider(names_from = variable, values_from = value01) %>%
    rename(group = series)
}

# Porter-like ggradar wrapper (line-only)
radar_plot <- function(wide_df, title){
  ggradar(
    plot.data = wide_df,
    grid.min = 0, grid.mid = 0.5, grid.max = 1,
    values.radar = c("0", "0.5", "1"),
    group.line.width = 1.1,
    group.point.size = 2.0,
    axis.label.size = 3.0,
    background.circle.colour = "white",
    gridline.min.colour = "grey85",
    gridline.mid.colour = "grey90",
    gridline.max.colour = "grey85",
    axis.line.colour = "grey70",
    fill = FALSE
  ) %>%
    `+`(scale_colour_manual(values = porter_cols)) %>%
    `+`(scale_fill_manual(values = porter_cols)) %>%
    `+`(ggtitle(title)) %>%
    porter_theme()
}

# ---------- Output folder ----------
outdir <- "radar_out"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# ---------- (A) GLOBAL RADAR PLOTS: 4 plots, each with all four series ----------
groups <-
  df %>%
  distinct(group_id) %>%
  arrange(group_id) %>%
  pull()

global_files <- c()
for (g in groups) {
  d_g <-
    df %>%
    filter(group_id == g) %>%
    arrange(importance)
  
  wide <- make_wide_for_ggradar(d_g)
  
  p <-
    radar_plot(
      wide,
      title = paste0(
        "Global radial plot — ", g, " (vars: ",
        d_g %>% distinct(variable) %>% pull() %>% paste(collapse = ", "), ")"
      )
    )
  
  f <- file.path(outdir, paste0("radar_global_", g, ".png"))
  ggsave(f, p, width = 6, height = 6, dpi = 300)
  global_files <- c(global_files, f)
}

# ---------- (B) PER-SERIES RADAR PLOTS: for each series, 4 plots ----------
per_series_files <- c()
for (s in series) {
  for (g in groups) {
    d_sg <-
      df %>%
      filter(series == s, group_id == g) %>%
      arrange(importance)
    
    wide <-
      make_wide_for_ggradar(d_sg) %>%
      slice(1)  # single-series radar
    
    p <-
      ggradar(
        plot.data = wide,
        grid.min = 0, grid.mid = 0.5, grid.max = 1,
        values.radar = c("0", "0.5", "1"),
        group.line.width = 1.3,
        group.point.size = 2.4,
        axis.label.size = 3.0,
        background.circle.colour = "white",
        gridline.min.colour = "grey85",
        gridline.mid.colour = "grey90",
        gridline.max.colour = "grey85",
        axis.line.colour = "grey70",
        fill = FALSE
      ) %>%
      `+`(scale_colour_manual(values = porter_cols, guide = "none")) %>%
      `+`(scale_fill_manual(values = porter_cols, guide = "none")) %>%
      `+`(ggtitle(paste0(
        "Radial plot — ", s, " — ", g, " (vars: ",
        d_sg %>% distinct(variable) %>% pull() %>% paste(collapse = ", "), ")"
      ))) %>%
      porter_theme() +
      theme(legend.position = "none")
    
    f <- file.path(outdir, paste0("radar_", s, "_", g, ".png"))
    ggsave(f, p, width = 6, height = 6, dpi = 300)
    per_series_files <- c(per_series_files, f)
  }
}

message("Done. Files saved in: ", normalizePath(outdir))
cat("Global plots:\n", paste(global_files, collapse = "\n"), "\n\n")
cat("Per-series plots:\n", paste(per_series_files, collapse = "\n"))
