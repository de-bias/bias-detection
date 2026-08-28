#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(grid)
  library(jsonlite)
  library(ragg)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) != 1L) stop("Could not resolve this script's location.")
script_path <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
site_dir <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

story <- fromJSON(file.path(site_dir, "data/meta-story.json"), simplifyVector = FALSE)
pair <- story$pair
if (length(pair) != 2L || !identical(vapply(pair, `[[`, character(1), "name"), c("Watford", "North East Derbyshire"))) {
  stop("The reviewed illustrative pair is missing or out of order.")
}

paper <- "#fbfaf6"
navy <- "#082b55"
navy_soft <- "#27415f"
teal <- "#078481"
coral <- "#f2553d"
grey <- "#767d86"
grey_light <- "#d8d8d5"
line <- "#cfd2d1"
display <- "Arial Narrow"
body <- "Arial"

output_path <- file.path(site_dir, "assets/social-card.png")
agg_png(output_path, width = 1200, height = 630, units = "px", res = 72, background = paper)
pushViewport(viewport(xscale = c(0, 1200), yscale = c(0, 630)))

native <- function(value) unit(value, "native")
label <- function(text, x, y, size, colour = navy, family = body, face = "plain", just = c("left", "top"), lineheight = 0.9) {
  grid.text(
    text,
    x = native(x),
    y = native(630 - y),
    just = just,
    gp = gpar(col = colour, fontsize = size, fontfamily = family, fontface = face, lineheight = lineheight)
  )
}

grid.rect(gp = gpar(fill = paper, col = NA))
grid.lines(x = native(c(32, 1168)), y = native(630 - c(62, 62)), gp = gpar(col = line, lwd = 1))
label("DEBIAS", 34, 20, 26, family = display, face = "bold")
label("RESEARCH PROJECT", 148, 27, 10, family = display, face = "bold")

label("WHO IS MISSING\nFROM THE MAP?", 34, 101, 54, family = display, face = "bold", lineheight = 0.86)
label("THE DOTS LINE UP. THE LOCAL COVERAGE STILL VARIES.", 35, 250, 14, colour = teal, family = display, face = "bold")
label("A high correlation is a useful first check—\nnot proof of representativeness.", 35, 281, 18, colour = navy_soft, lineheight = 1.15)

draw_grid <- function(x, y, filled, colour) {
  cell <- 9
  gap <- 3.2
  for (index in 0:99) {
    row <- index %/% 10
    column <- index %% 10
    cell_x <- x + column * (cell + gap)
    cell_y <- y + row * (cell + gap)
    grid.rect(
      x = native(cell_x + cell / 2),
      y = native(630 - (cell_y + cell / 2)),
      width = native(cell),
      height = native(cell),
      gp = gpar(fill = grey_light, col = NA)
    )
    fill_width <- if (index < floor(filled)) {
      cell
    } else if (index == floor(filled)) {
      cell * (filled - floor(filled))
    } else {
      0
    }
    if (fill_width > 0) {
      grid.rect(
        x = native(cell_x + fill_width / 2),
        y = native(630 - (cell_y + cell / 2)),
        width = native(fill_width),
        height = native(cell),
        gp = gpar(fill = colour, col = NA)
      )
    }
  }
}

draw_card <- function(x, area, colour, title) {
  grid.rect(
    x = native(x + 113), y = native(630 - (105 + 195)), width = native(226), height = native(390),
    gp = gpar(fill = NA, col = colour, lwd = 1.25)
  )
  label(title, x + 113, 130, 23, colour = colour, family = display, face = "bold", just = c("centre", "top"), lineheight = 0.88)
  label(sprintf("%s residents", format(area$population, big.mark = ",", scientific = FALSE)), x + 113, 194, 12, just = c("centre", "top"))
  label(sprintf("%.2f", area$rate), x + 113, 235, 27, colour = colour, family = display, face = "bold", just = c("centre", "top"))
  label("estimates per 100", x + 113, 268, 11, colour = colour, just = c("centre", "top"))
  draw_grid(x + 53, 304, area$rate, colour)
}

label("META ACTIVE-USER EXAMPLE · ≈102,000 RESIDENTS EACH", 842, 76, 12, family = display, face = "bold", just = c("centre", "top"))
draw_card(626, pair[[1]], teal, "WATFORD")
draw_card(942, pair[[2]], coral, "NORTH EAST\nDERBYSHIRE")

grid.lines(x = native(c(871, 915)), y = native(630 - c(198, 198)), gp = gpar(col = line, lwd = 1))
label("6.7×", 893, 232, 46, family = display, face = "bold", just = c("centre", "top"))
label("DIFFERENT\nLOCAL RATES", 893, 287, 15, family = display, face = "bold", just = c("centre", "top"), lineheight = 0.9)
grid.lines(x = native(c(871, 915)), y = native(630 - c(353, 353)), gp = gpar(col = line, lwd = 1))

grid.rect(
  x = native(34 + 526 / 2), y = native(630 - (440 + 55 / 2)), width = native(526), height = native(55),
  gp = gpar(fill = "#eff7f5", col = teal, lwd = 1)
)
label("CORRELATION IS A FIRST DIAGNOSTIC, NOT A CERTIFICATE.", 52, 456, 18, family = display, face = "bold")

grid.lines(x = native(c(32, 1168)), y = native(630 - c(536, 536)), gp = gpar(col = line, lwd = 1))
label("Aggregate active-account estimates—not identified people", 34, 552, 13, colour = navy_soft)
label("CABRERA & ROWE · RSOS · DOI 10.1098/RSOS.251703", 1166, 554, 10, colour = grey, just = c("right", "top"))

dev.off()
message("Built ", output_path)
