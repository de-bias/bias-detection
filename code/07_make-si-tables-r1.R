#!/usr/bin/env Rscript

# Build supplementary tables for R1 revision from base-case outputs.
# Outputs:
# - CSV tables in outputs/preliminary/si-tables-r1/
# - Optional formatted Word document (if officer + flextable are installed)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

# -----------------------------
# Configuration
# -----------------------------
wd_outputs <- "/Volumes/rdm04/DEBIAS/data/outputs"
dict_path <- "/Volumes/rdm04/DEBIAS/data/inputs/census/census2021-ts/dictionary.csv"
repo_root <- "/Users/carmen/Documents/github/de-bias/bias-detection"
out_dir <- file.path(repo_root, "outputs/preliminary/si-tables-r1")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dataset_paths <- tibble::tribble(
  ~dataset_label, ~dataset_key, ~base_case_dir,
  "Twitter/X", "twitter", file.path(wd_outputs, "twitter/monthly/populations/active-population-bias/base-case"),
  "Meta", "meta", file.path(wd_outputs, "fb/census-month/populations/tts/active-population-bias/base-case"),
  "Multi-app1", "mapp1", file.path(wd_outputs, "mapp1/census-month/populations/active-population-bias/base-case"),
  "Multi-app2", "mapp2", file.path(wd_outputs, "mapp2/census-month/populations/active-population-bias/base-case")
)

# -----------------------------
# Helpers
# -----------------------------
pick_latest <- function(path_dir, pattern) {
  files <- list.files(path_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  files[which.max(file.info(files)$mtime)]
}

read_or_empty <- function(path_file) {
  if (is.na(path_file) || !file.exists(path_file)) return(tibble())
  readr::read_csv(path_file, show_col_types = FALSE)
}

round_numeric <- function(df, digits = 2) {
  df %>% mutate(across(where(is.numeric), ~ round(., digits)))
}

sig_stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

extract_first <- function(df, colname) {
  if (!is.data.frame(df) || nrow(df) == 0 || !(colname %in% names(df))) return(NA_real_)
  as.numeric(df[[colname]][1])
}

to_pretty_base <- function(df) {
  df %>%
    rename(
      `Data Source` = data_source,
      `Alpha Hat` = alpha_hat,
      `Predicted Bias` = predicted_bias,
      `R Squared` = r_squared,
      `RMSE` = rmse,
      `Moran's I (Residuals)` = moran_i_residuals,
      `P Value (Residuals)` = p_value_residuals
    )
}

to_pretty_corr <- function(df) {
  df %>%
    select(any_of(c(
      "variable_label", "n",
      "pearson_r", "pearson_p", "spearman_rho", "spearman_p",
      "distance_correlation",
      "lm_aic", "lm_bic", "gam_aic", "gam_bic"
    ))) %>%
    mutate(
      pearson_r = paste0(sprintf("%.2f", pearson_r), sig_stars(pearson_p)),
      spearman_rho = paste0(sprintf("%.2f", spearman_rho), sig_stars(spearman_p)),
      distance_correlation = sprintf("%.2f", distance_correlation)
    ) %>%
    rename(
      `Covariate` = variable_label,
      `N` = n,
      `Pearson r` = pearson_r,
      `Spearman rho` = spearman_rho,
      `Distance Correlation` = distance_correlation,
      `Linear AIC` = lm_aic,
      `Linear BIC` = lm_bic,
      `GAM AIC` = gam_aic,
      `GAM BIC` = gam_bic
    ) %>%
    select(-any_of(c("pearson_p", "spearman_p")))
}

# -----------------------------
# Build Table 1: baseline summary
# -----------------------------
summary_rows <- lapply(seq_len(nrow(dataset_paths)), function(i) {
  ds_label <- dataset_paths$dataset_label[i]
  ds_dir <- dataset_paths$base_case_dir[i]
  
  metrics_file <- pick_latest(ds_dir, "^base_case_metrics.*\\.csv$")
  moran_file <- pick_latest(ds_dir, "^base_case_residual_moran.*\\.csv$")
  
  metrics_df <- read_or_empty(metrics_file) %>% as.data.frame()
  moran_df <- read_or_empty(moran_file) %>% as.data.frame()
  
  data.frame(
    data_source = ds_label,
    alpha_hat = extract_first(metrics_df, "alpha_hat"),
    predicted_bias = extract_first(metrics_df, "predicted_bias"),
    r_squared = extract_first(metrics_df, "r_squared"),
    rmse = extract_first(metrics_df, "rmse"),
    moran_i_residuals = extract_first(moran_df, "moran_i_residuals"),
    p_value_residuals = extract_first(moran_df, "p_value_residuals")
  )
})

base_summary <- bind_rows(summary_rows) %>%
  round_numeric() %>%
  to_pretty_base()

readr::write_csv(base_summary, file.path(out_dir, "table_basecase_summary.csv"))

# -----------------------------
# Build Table 2+: per-dataset residual-covariate results
# -----------------------------
corr_tables <- list()
dict_df <- read_or_empty(dict_path) %>%
  as_tibble() %>%
  transmute(variable = var_label5, variable_label = var_label2) %>%
  distinct(variable, .keep_all = TRUE)

for (i in seq_len(nrow(dataset_paths))) {
  corr_file <- pick_latest(dataset_paths$base_case_dir[i], "^base_case_residual_pairwise_correlations.*\\.csv$")
  corr_df <- read_or_empty(corr_file)

  if (nrow(corr_df) == 0) {
    message("No pairwise correlation table found for: ", dataset_paths$dataset_label[i])
    next
  }

  corr_df <- corr_df %>%
    as_tibble() %>%
    left_join(dict_df, by = "variable") %>%
    mutate(variable_label = ifelse(is.na(variable_label) | variable_label == "", variable, variable_label)) %>%
    arrange(gam_aic) %>%
    round_numeric() %>%
    to_pretty_corr()

  out_csv <- file.path(out_dir, paste0("table_residual_covariates_", dataset_paths$dataset_key[i], ".csv"))
  readr::write_csv(corr_df, out_csv)
  corr_tables[[dataset_paths$dataset_label[i]]] <- corr_df
}

message("CSV tables saved in: ", out_dir)

# -----------------------------
# Optional: Build a formatted Word document
# -----------------------------
has_officer <- requireNamespace("officer", quietly = TRUE)
has_flextable <- requireNamespace("flextable", quietly = TRUE)

if (has_officer && has_flextable) {
  suppressPackageStartupMessages({
    library(officer)
    library(flextable)
  })

  make_ft <- function(df) {
    flextable(df) %>%
      theme_booktabs() %>%
      bold(part = "header") %>%
      fontsize(size = 10, part = "all") %>%
      autofit()
  }

  doc <- read_docx()
  doc <- doc %>%
    body_add_par("Supplementary Tables (R1 Revision)", style = "heading 1") %>%
    body_add_par("Table S1. Proportional baseline summary by data source.", style = "heading 2") %>%
    body_add_flextable(make_ft(base_summary))

  for (nm in names(corr_tables)) {
    doc <- doc %>%
      body_add_par(paste0("Table S: ", nm, " residual-covariate assessments (ordered by GAM AIC)."), style = "heading 2") %>%
      body_add_flextable(make_ft(corr_tables[[nm]]))
  }

  out_docx <- file.path(out_dir, "si_tables_r1.docx")
  print(doc, target = out_docx)
  message("Word document saved: ", out_docx)
} else {
  message("Skipping Word document export. Install packages `officer` and `flextable` to enable DOCX output.")
}
