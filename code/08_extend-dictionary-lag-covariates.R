#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

# Usage:
# Rscript code/08_extend-dictionary-lag-covariates.R \
#   /Volumes/DEBIAS/data/inputs/census/census2021-ts/dictionary.csv \
#   /Volumes/DEBIAS/data/inputs/census/census2021-ts/dictionary_with_lags.csv

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Provide input and output CSV paths.\n",
       "Example:\n",
       "Rscript code/08_extend-dictionary-lag-covariates.R <input.csv> <output.csv>")
}

in_path <- args[[1]]
out_path <- args[[2]]

dict <- readr::read_csv(in_path, show_col_types = FALSE)

# Try to detect key columns used across your notebooks.
col_var0 <- if ("var_label0" %in% names(dict)) "var_label0" else NA_character_
col_var2 <- if ("var_label2" %in% names(dict)) "var_label2" else NA_character_
col_var5 <- if ("var_label5" %in% names(dict)) "var_label5" else NA_character_
col_group <- if ("group" %in% names(dict)) "group" else NA_character_

if (is.na(col_var0) || is.na(col_var5)) {
  stop("Dictionary must contain at least var_label0 and var_label5 columns.")
}

# Exclude non-covariates commonly present in dictionary.
base_rows <- dict %>%
  filter(!.data[[col_var0]] %in% c("bias", "bias_w", "date", "geography", "geography.code", "code", "name"))

# Build lag rows by copying each covariate row and editing labels.
lag_rows <- base_rows %>%
  mutate(
    "{col_var0}" := paste0(.data[[col_var0]], "_lagq"),
    "{col_var5}" := paste0(.data[[col_var5]], "_lagq")
  )

if (!is.na(col_var2)) {
  lag_rows <- lag_rows %>%
    mutate("{col_var2}" := paste0("Spatial lag (queen): ", .data[[col_var2]]))
}

if (!is.na(col_group)) {
  lag_rows <- lag_rows %>%
    mutate("{col_group}" := if_else(is.na(.data[[col_group]]), "spatial_lag", paste0(.data[[col_group]], "_lag")))
}

# Keep only lag rows that do not already exist in dictionary.
lag_rows_new <- lag_rows %>%
  anti_join(dict %>% select(all_of(col_var0)), by = setNames(col_var0, col_var0))

dict_out <- bind_rows(dict, lag_rows_new)

readr::write_csv(dict_out, out_path)

message("Input rows: ", nrow(dict))
message("Lag rows added: ", nrow(lag_rows_new))
message("Output rows: ", nrow(dict_out))
message("Saved: ", out_path)
