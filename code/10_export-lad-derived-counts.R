#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sf)
})

args <- commandArgs(trailingOnly = TRUE)
data_root <- if (length(args) >= 1) args[[1]] else "/Volumes/DEBIAS"
repo_root <- if (length(args) >= 2) args[[2]] else getwd()

paths <- list(
  census = file.path(
    data_root,
    "data/inputs/census/census2021-ts/census2021-ts001/census2021-ts001-ltla.csv"
  ),
  twitter = file.path(
    data_root,
    "data/outputs/twitter/monthly/populations/active-population-bias/active_population_bias_lad.csv"
  ),
  meta = file.path(
    data_root,
    "data/outputs/fb/census-month/populations/tts/active-population-bias/active_population_bias_lad.csv"
  ),
  multiapp1 = file.path(
    data_root,
    "data/outputs/mapp1/census-month/populations/active-population-bias/active_population_bias_lad.csv"
  ),
  multiapp2 = file.path(
    data_root,
    "data/outputs/mapp2/census-month/populations/active-population-bias/active_population_bias_lad.csv"
  )
)

missing_paths <- names(paths)[!file.exists(unlist(paths))]
if (length(missing_paths) > 0) {
  stop(
    "Missing required input files for: ",
    paste(missing_paths, collapse = ", "),
    ".\nChecked under data root: ",
    data_root,
    call. = FALSE
  )
}

read_bias_counts <- function(path, platform_name) {
  read_csv(path, show_col_types = FALSE) %>%
    transmute(
      code = code,
      name = name,
      !!platform_name := pop_dfd
    )
}

pick_first <- function(df, candidates, label) {
  match <- candidates[candidates %in% names(df)][1]
  if (is.na(match)) {
    stop(
      "Could not detect ", label, " column in ", paths$census,
      ". Available columns: ", paste(names(df), collapse = ", "),
      call. = FALSE
    )
  }
  match
}

census_raw <- read_csv(paths$census, show_col_types = FALSE)

code_col <- pick_first(
  census_raw,
  c("geography.code", "geography code", "Geography code", "LAD21CD", "LAD22CD", "code"),
  "census code"
)
name_col <- pick_first(
  census_raw,
  c("geography", "Geography", "LAD21NM", "LAD22NM", "name"),
  "census name"
)
pop_col <- pick_first(
  census_raw,
  c(
    "Residence.type..Total..measures..Value",
    "Residence type: Total; measures: Value",
    "population",
    "Population",
    "pop"
  ),
  "census population"
)

census <- census_raw %>%
  transmute(
    code = .data[[code_col]],
    name = .data[[name_col]],
    population = .data[[pop_col]]
  ) %>%
  filter(!grepl("^[SN]", code))

derived_counts <- census %>%
  left_join(read_bias_counts(paths$twitter, "twitter_x_population"), by = c("code", "name")) %>%
  left_join(read_bias_counts(paths$meta, "meta_population"), by = c("code", "name")) %>%
  left_join(read_bias_counts(paths$multiapp1, "multiapp1_population"), by = c("code", "name")) %>%
  left_join(read_bias_counts(paths$multiapp2, "multiapp2_population"), by = c("code", "name")) %>%
  arrange(code)

output_path <- file.path(
  repo_root,
  "outputs/manuscript-data/lad-derived-population-counts.csv"
)

write_csv(derived_counts, output_path)
message("Wrote: ", output_path)
