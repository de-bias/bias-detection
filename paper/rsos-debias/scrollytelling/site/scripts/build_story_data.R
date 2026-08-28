#!/usr/bin/env Rscript

# Build the small, publication-ready dataset used by the scrollytelling page.
#
# Inputs:
#   1. The released LAD-level data in outputs/manuscript-data.
#   2. ONS 2021 ultra-generalised LAD boundaries, downloaded from the
#      Open Geography Portal unless LAD_BOUNDARY_GEOJSON points to a local copy.
#
# Outputs:
#   data/meta-story.json            browser-ready data and SVG map paths
#   data/meta-areas.json            geometry-free data for the local explorer
#   media/meta-local-coverage.csv   accessible/downloadable source table

suppressPackageStartupMessages({
  library(jsonlite)
  library(sf)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) != 1L) {
  stop("Could not resolve this script's location.")
}

script_path <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
site_dir <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
repo_dir <- normalizePath(file.path(site_dir, "../../../.."), mustWork = TRUE)

counts_path <- file.path(
  repo_dir,
  "outputs/manuscript-data/lad-derived-population-counts.csv"
)
output_path <- file.path(site_dir, "data/meta-story.json")
explore_output_path <- file.path(site_dir, "data/meta-areas.json")
download_path <- file.path(site_dir, "media/meta-local-coverage.csv")

boundary_url <- paste0(
  "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
  "Local_Authority_Districts_December_2021_GB_BUC_2022/FeatureServer/0/query?",
  "where=1%3D1&outFields=LAD21CD%2CLAD21NM&returnGeometry=true&outSR=4326&f=geojson"
)

boundary_path <- Sys.getenv("LAD_BOUNDARY_GEOJSON", unset = "")
if (!nzchar(boundary_path)) {
  boundary_path <- tempfile(fileext = ".geojson")
  message("Downloading official ONS 2021 LAD boundaries...")
  download.file(boundary_url, boundary_path, mode = "wb", quiet = TRUE)
}

if (!file.exists(counts_path)) stop("Missing released counts: ", counts_path)
if (!file.exists(boundary_path)) stop("Missing LAD boundary file: ", boundary_path)

counts <- read.csv(counts_path, stringsAsFactors = FALSE, check.names = FALSE)
required_columns <- c("name", "population", "meta_population")
missing_columns <- setdiff(required_columns, names(counts))
if (length(missing_columns)) {
  stop("Released counts are missing: ", paste(missing_columns, collapse = ", "))
}
if (nrow(counts) != 331L) stop("Expected 331 LAD rows; found ", nrow(counts), ".")
if (anyDuplicated(counts$name)) stop("LAD names are not unique in the released data.")

boundaries <- st_read(boundary_path, quiet = TRUE, stringsAsFactors = FALSE)
if (!all(c("LAD21CD", "LAD21NM") %in% names(boundaries))) {
  stop("The boundary file does not contain LAD21CD and LAD21NM.")
}

# The released CSV uses the historical single-f spelling; the ONS 2021 layer
# uses the official double-f spelling. Keep the mapping explicit and auditable.
boundary_names <- counts$name
boundary_names[boundary_names == "Rhondda Cynon Taff"] <- "Rhondda Cynon Taf"
boundary_index <- match(boundary_names, boundaries$LAD21NM)
if (anyNA(boundary_index)) {
  stop(
    "Unmatched LAD boundaries: ",
    paste(counts$name[is.na(boundary_index)], collapse = ", ")
  )
}

lad <- boundaries[boundary_index, c("LAD21CD", "LAD21NM")]
lad$name <- counts$name
lad$population <- as.double(counts$population)
lad$meta_population <- as.double(counts$meta_population)
lad$rate <- 100 * lad$meta_population / lad$population

alpha <- sum(lad$population * lad$meta_population) / sum(lad$population^2)
alpha_per_100 <- 100 * alpha
lad$departure <- lad$rate - alpha_per_100

pearson <- cor(lad$population, lad$meta_population, method = "pearson")
rate_quantiles <- unname(quantile(lad$rate, c(0.05, 0.95), type = 7))
departure_limit <- max(abs(lad$departure))

pair_names <- c("Watford", "North East Derbyshire")
pair_index <- match(pair_names, lad$name)
if (anyNA(pair_index)) stop("The illustrative LAD pair is missing.")

# Prepare compact, browser-ready SVG paths. Using the ultra-generalised layer
# plus a modest one-kilometre simplification keeps the core page lightweight.
lad_bng <- st_transform(lad, 27700)
lad_bng <- st_make_valid(lad_bng)
lad_bng <- st_simplify(lad_bng, dTolerance = 1000, preserveTopology = TRUE)

canvas_width <- 900
canvas_height <- 680
map_margin_x <- 145
map_margin_y <- 38
map_bbox <- st_bbox(lad_bng)
scale_factor <- min(
  (canvas_width - 2 * map_margin_x) / (map_bbox[["xmax"]] - map_bbox[["xmin"]]),
  (canvas_height - 2 * map_margin_y) / (map_bbox[["ymax"]] - map_bbox[["ymin"]])
)
scaled_width <- (map_bbox[["xmax"]] - map_bbox[["xmin"]]) * scale_factor
scaled_height <- (map_bbox[["ymax"]] - map_bbox[["ymin"]]) * scale_factor
x_offset <- (canvas_width - scaled_width) / 2
y_offset <- (canvas_height - scaled_height) / 2

to_screen <- function(coords) {
  cbind(
    x_offset + (coords[, 1] - map_bbox[["xmin"]]) * scale_factor,
    y_offset + (map_bbox[["ymax"]] - coords[, 2]) * scale_factor
  )
}

geometry_rings <- function(geometry) {
  if (inherits(geometry, "POLYGON")) return(unclass(geometry))
  if (inherits(geometry, "MULTIPOLYGON")) {
    return(unlist(unclass(geometry), recursive = FALSE))
  }
  stop("Unsupported geometry type: ", class(geometry)[2])
}

geometry_to_path <- function(geometry) {
  rings <- geometry_rings(geometry)
  path_parts <- vapply(rings, function(ring) {
    points <- round(to_screen(ring), 1)
    paste0(
      "M", points[1, 1], ",", points[1, 2],
      paste0("L", points[-1, 1], ",", points[-1, 2], collapse = ""),
      "Z"
    )
  }, character(1))
  paste0(path_parts, collapse = "")
}

paths <- vapply(st_geometry(lad_bng), geometry_to_path, character(1))
label_points <- suppressWarnings(st_point_on_surface(lad_bng))
label_coordinates <- to_screen(st_coordinates(label_points)[, 1:2, drop = FALSE])

areas <- lapply(seq_len(nrow(lad)), function(i) {
  list(
    code = lad$LAD21CD[[i]],
    name = lad$name[[i]],
    population = lad$population[[i]],
    meta = lad$meta_population[[i]],
    rate = lad$rate[[i]],
    departure = lad$departure[[i]],
    map_x = label_coordinates[i, 1],
    map_y = label_coordinates[i, 2],
    path = paths[[i]]
  )
})

pair <- lapply(pair_index, function(i) {
  list(
    code = lad$LAD21CD[[i]],
    name = lad$name[[i]],
    population = lad$population[[i]],
    meta = lad$meta_population[[i]],
    rate = lad$rate[[i]],
    departure = lad$departure[[i]]
  )
})

story_data <- list(
  metadata = list(
    title = "Who is missing from the map?",
    analysis_area = "England and Wales",
    geography = "2021 local authority districts",
    area_count = nrow(lad),
    data_period = "March 2021",
    census = "2021 Census",
    meta_measure = "Average nighttime active-account estimate",
    pearson = pearson,
    alpha_per_100 = alpha_per_100,
    rate_q05 = rate_quantiles[[1]],
    rate_q95 = rate_quantiles[[2]],
    rate_min = min(lad$rate),
    rate_max = max(lad$rate),
    departure_limit = departure_limit,
    pair_rate_ratio = max(lad$rate[pair_index]) / min(lad$rate[pair_index]),
    pair_population_difference_pct =
      100 * abs(diff(lad$population[pair_index])) / mean(lad$population[pair_index]),
    public_measure = "Local Meta rate minus the fitted Meta rate",
    public_measure_note = paste(
      "Positive values mean more active-account estimates per 100 residents",
      "than the fitted proportional rate. This has the opposite sign to the",
      "paper's residual-bias convention."
    ),
    map_view_box = c(0, 0, canvas_width, canvas_height)
  ),
  pair = pair,
  areas = areas,
  sources = list(
    list(
      label = "Released LAD-level analysis data",
      url = paste0(
        "https://github.com/de-bias/bias-detection/blob/main/outputs/",
        "manuscript-data/lad-derived-population-counts.csv"
      )
    ),
    list(
      label = "ONS 2021 Local Authority District boundaries",
      url = paste0(
        "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
        "Local_Authority_Districts_December_2021_GB_BUC_2022/FeatureServer"
      )
    )
  )
)

writeLines(
  toJSON(story_data, auto_unbox = TRUE, digits = 7, pretty = FALSE),
  output_path,
  useBytes = TRUE
)

explore_data <- list(
  metadata = story_data$metadata[c("area_count", "data_period", "alpha_per_100", "departure_limit")],
  areas = lapply(areas, function(area) area[c("code", "name", "population", "meta", "rate", "departure")])
)
writeLines(
  toJSON(explore_data, auto_unbox = TRUE, digits = 7, pretty = FALSE),
  explore_output_path,
  useBytes = TRUE
)

download_table <- data.frame(
  lad_code = lad$LAD21CD,
  local_authority = lad$name,
  census_population_2021 = lad$population,
  meta_average_nighttime_active_account_estimate_march_2021 = lad$meta_population,
  meta_estimates_per_100_census_residents = round(lad$rate, 6),
  difference_from_fitted_meta_estimate_rate_per_100 = round(lad$departure, 6),
  stringsAsFactors = FALSE
)
write.csv(download_table, download_path, row.names = FALSE, na = "")

# Static, accessible fallbacks and downloadable media figures. These use the
# same calculations as the interactive page and require no browser runtime.
ink <- "#082b55"
muted <- "#767d86"
paper <- "#fbfaf6"
coral <- "#f2553d"
teal <- "#078481"

open_svg <- function(filename) {
  svg(
    file.path(site_dir, "media", filename),
    width = 12,
    height = 8,
    pointsize = 12,
    bg = paper,
    family = "sans"
  )
  par(fg = ink, col.axis = muted, col.lab = muted, col.main = ink)
}

add_figure_footer <- function(number) {
  mtext(
    paste0(number, "  ·  CABRERA & ROWE / DEBIAS  ·  ENGLAND AND WALES, 2021"),
    side = 1,
    outer = TRUE,
    line = -1.4,
    adj = 0.03,
    col = muted,
    cex = 0.66
  )
}

# 1. The revealing pair
open_svg("01-revealing-pair.svg")
par(mar = c(4, 3, 5, 3), oma = c(2, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 12), ylim = c(0, 8), xaxs = "i", yaxs = "i")
title(main = "Almost the same population. 6.7× different local rates.",
      sub = "Illustrative extreme from 331 local authorities in England and Wales",
      adj = 0, cex.main = 1.8, cex.sub = 0.92, col.sub = muted)

draw_record_grid <- function(x, y, value, colour) {
  cell <- 0.19
  gap <- 0.07
  for (index in 0:99) {
    row <- index %/% 10
    column <- index %% 10
    x_left <- x + column * (cell + gap)
    y_bottom <- y + (9 - row) * (cell + gap)
    rect(x_left, y_bottom, x_left + cell, y_bottom + cell,
         col = "#d8d8d5", border = NA)
    fill_width <- if (index < floor(value)) {
      cell
    } else if (index == floor(value)) {
      cell * (value - floor(value))
    } else {
      0
    }
    if (fill_width > 0) {
      rect(x_left, y_bottom, x_left + fill_width, y_bottom + cell,
           col = colour, border = NA)
    }
  }
}

rect(0.55, 0.7, 4.4, 6.65, border = teal, lwd = 1.5)
rect(7.6, 0.7, 11.45, 6.65, border = coral, lwd = 1.5)
abline_segments <- function(x1, y1, x2, y2) segments(x1, y1, x2, y2, col = "#cfd2d1", lwd = 1)
abline_segments(5.35, 5.35, 6.65, 5.35)
abline_segments(5.35, 2.15, 6.65, 2.15)
text(6, 4.6, "6.7×", family = "sans", font = 2, cex = 3.0, col = ink)
text(6, 3.72, "DIFFERENT\nLOCAL RATES", family = "sans", font = 2,
     cex = 0.9, col = ink)

for (i in seq_along(pair_index)) {
  x_pos <- c(2.475, 9.525)[i]
  idx <- pair_index[i]
  colour <- c(teal, coral)[i]
  text(x_pos, 6.05, toupper(lad$name[idx]), font = 2, cex = 1.05, col = colour)
  text(x_pos, 5.47,
       paste(format(lad$population[idx], big.mark = ",", scientific = FALSE), "residents"),
       cex = 0.83, col = ink)
  text(x_pos, 4.72, sprintf("%.2f", lad$rate[idx]), font = 2, cex = 1.55, col = colour)
  text(x_pos, 4.3, "estimates per 100 residents", cex = 0.72, col = colour)
  draw_record_grid(c(1.18, 8.23)[i], 1.2, lad$rate[idx], colour)
}
add_figure_footer("01")
dev.off()

# 2. The raw counts line up
open_svg("02-counts-line-up.svg")
par(mar = c(6, 7, 5, 3), oma = c(2, 0, 0, 0))
plot(
  lad$population,
  lad$meta_population,
  pch = 16,
  cex = 0.72,
  col = adjustcolor(ink, alpha.f = 0.55),
  xlab = "2021 Census resident population",
  ylab = "Meta average nighttime active-account estimate",
  main = "The counts line up",
  sub = "A strong population relationship is useful—but it does not make local coverage constant.",
  cex.main = 1.9,
  cex.sub = 0.92,
  bty = "l",
  las = 1
)
abline(a = 0, b = alpha, col = coral, lwd = 2.5, lty = 2)
points(lad$population[pair_index], lad$meta_population[pair_index], pch = 21,
       cex = 1.35, bg = c(teal, coral), col = ink, lwd = 1.5)
legend("topleft", legend = c("Pearson r = .91", "n = 331 local authorities"),
       bty = "n", text.col = ink, cex = 1.05)
add_figure_footer("02")
dev.off()

# 3. Divide the count by the census population
open_svg("03-local-rates.svg")
par(mar = c(6, 3, 5, 3), oma = c(2, 0, 0, 0))
set.seed(2021)
jitter_values <- runif(nrow(lad), -0.22, 0.22)
plot(
  lad$rate,
  jitter_values,
  xlim = c(0, 25),
  ylim = c(-0.6, 0.6),
  pch = 16,
  cex = 0.72,
  col = adjustcolor(ink, alpha.f = 0.55),
  xlab = "Meta active-user estimates per 100 census residents",
  ylab = "",
  yaxt = "n",
  main = "The local coverage rate varies",
  sub = "The shaded band contains the middle 90% of local authorities.",
  cex.main = 1.9,
  cex.sub = 0.92,
  bty = "n"
)
rect(rate_quantiles[1], -0.55, rate_quantiles[2], 0.55,
     col = adjustcolor(teal, alpha.f = 0.10), border = NA)
points(lad$rate, jitter_values, pch = 16, cex = 0.72,
       col = adjustcolor(ink, alpha.f = 0.55))
abline(v = alpha_per_100, col = ink, lwd = 2.5, lty = 2)
points(lad$rate[pair_index], jitter_values[pair_index], pch = 21, cex = 1.35,
       bg = c(teal, coral), col = ink, lwd = 1.5)
text(alpha_per_100, 0.5, sprintf("fitted rate %.2f", alpha_per_100),
     pos = 4, cex = 0.85, col = ink)
text(mean(rate_quantiles), -0.5,
     sprintf("middle 90%%: %.2f–%.2f", rate_quantiles[1], rate_quantiles[2]),
     cex = 0.85, col = muted)
add_figure_footer("03")
dev.off()

# 4. Map the intuitive departure measure: local rate minus fitted rate.
negative_palette <- colorRampPalette(c(teal, "#edece7"))(101)
positive_palette <- colorRampPalette(c("#edece7", coral))(101)
departure_colours <- vapply(lad$departure, function(value) {
  index <- 1 + round(min(1, abs(value) / departure_limit)^0.72 * 100)
  if (value < 0) negative_palette[102 - index] else positive_palette[index]
}, character(1))

open_svg("04-local-map.svg")
par(mar = c(3, 2, 5, 2), oma = c(2, 0, 0, 0))
plot(st_geometry(lad_bng), col = departure_colours, border = paper, lwd = 0.45,
     main = "The difference is local",
     sub = "Local Meta rate minus the fitted Meta rate, per 100 census residents")
legend(
  "bottom",
  legend = c("fewer than fitted", "near fitted rate", "more than fitted"),
  fill = c(teal, "#edece7", coral),
  border = NA,
  bty = "n",
  cex = 0.82,
  horiz = TRUE,
  inset = -0.02,
  xpd = TRUE
)
mtext("More estimated accounts do not mean better representation.", side = 3,
      line = -1.5, adj = 0.98, col = muted, cex = 0.78)
add_figure_footer("04")
dev.off()

# Portrait fallbacks keep labels readable on phones, reduced-motion layouts
# and narrow embeds. The surrounding page supplies the editorial heading.
open_mobile_svg <- function(filename, height = 9) {
  svg(
    file.path(site_dir, "media", filename),
    width = 7,
    height = height,
    pointsize = 16,
    bg = paper,
    family = "sans"
  )
  par(fg = ink, col.axis = "#5f6872", col.lab = ink, col.main = ink)
}

add_mobile_footer <- function(number) {
  mtext(
    paste0(number, " · CABRERA & ROWE / DEBIAS · ENGLAND AND WALES, 2021"),
    side = 1,
    outer = TRUE,
    line = -1,
    adj = 0.03,
    col = muted,
    cex = 0.58
  )
}

open_mobile_svg("02-counts-line-up-mobile.svg")
par(mar = c(6, 6, 3.5, 1.2), oma = c(1.5, 0, 0, 0))
plot(
  lad$population,
  lad$meta_population,
  xlim = c(0, 1200000),
  ylim = c(0, 100000),
  pch = 16,
  cex = 0.7,
  col = adjustcolor(ink, alpha.f = 0.55),
  xlab = "2021 Census resident population",
  ylab = "Meta active-user estimate",
  xaxt = "n",
  yaxt = "n",
  bty = "l",
  las = 1
)
axis(
  1,
  at = c(0, 250000, 500000, 750000, 1000000),
  labels = c("0", "250k", "500k", "750k", "1m"),
  col = muted,
  col.axis = muted
)
axis(
  2,
  at = c(0, 25000, 50000, 75000, 100000),
  labels = c("0", "25k", "50k", "75k", "100k"),
  las = 1,
  col = muted,
  col.axis = muted
)
abline(a = 0, b = alpha, col = "#9aa0a5", lwd = 2, lty = 2)
points(lad$population[pair_index], lad$meta_population[pair_index], pch = 21,
       cex = 1.35, bg = c(teal, coral), col = ink, lwd = 1.3)
mtext("Pearson r = .91 · n = 331", side = 3, line = 0.6, adj = 0.98,
      cex = 0.95, font = 2, col = ink)
add_mobile_footer("02")
dev.off()

open_mobile_svg("03-local-rates-mobile.svg")
par(mar = c(4, 6, 3.5, 1.2), oma = c(1.5, 0, 0, 0))
plot(
  jitter_values,
  lad$rate,
  xlim = c(-0.6, 0.6),
  ylim = c(0, 25),
  pch = 16,
  cex = 0.72,
  col = adjustcolor(ink, alpha.f = 0.55),
  xlab = "",
  ylab = "Meta estimates per 100 census residents",
  xaxt = "n",
  bty = "n",
  las = 1
)
rect(-0.55, rate_quantiles[1], 0.55, rate_quantiles[2],
     col = adjustcolor(teal, alpha.f = 0.10), border = NA)
points(jitter_values, lad$rate, pch = 16, cex = 0.72,
       col = adjustcolor(ink, alpha.f = 0.55))
abline(h = alpha_per_100, col = ink, lwd = 2.2, lty = 2)
points(jitter_values[pair_index], lad$rate[pair_index], pch = 21, cex = 1.4,
       bg = c(teal, coral), col = ink, lwd = 1.3)
mtext(sprintf("Fitted rate %.2f · middle 90%% %.2f–%.2f",
              alpha_per_100, rate_quantiles[1], rate_quantiles[2]),
      side = 3, line = 0.6, adj = 0.98, cex = 0.82, font = 2, col = ink)
add_mobile_footer("03")
dev.off()

open_mobile_svg("04-local-map-mobile.svg", height = 9.5)
par(mar = c(5.5, 1, 2.5, 1), oma = c(1.5, 0, 0, 0))
plot(st_geometry(lad_bng), col = departure_colours, border = paper, lwd = 0.45)
mtext("Local rate minus fitted Meta rate", side = 3, line = 0.5,
      adj = 0.5, cex = 0.92, font = 2, col = ink)
legend(
  "bottom",
  legend = c("fewer", "0 = fitted", "more"),
  fill = c(teal, "#edece7", coral),
  border = NA,
  bty = "n",
  cex = 0.78,
  horiz = TRUE,
  inset = -0.08,
  xpd = TRUE
)
add_mobile_footer("04")
dev.off()

message(
  sprintf(
    paste(
      "Built %s with %d LADs (Pearson r = %.6f; fitted rate = %.6f per 100;",
      "middle 90%% = %.6f-%.6f)."
    ),
    output_path,
    nrow(lad),
    pearson,
    alpha_per_100,
    rate_quantiles[[1]],
    rate_quantiles[[2]]
  )
)
