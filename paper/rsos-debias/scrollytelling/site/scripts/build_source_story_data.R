#!/usr/bin/env Rscript

# Build the geometry-free evidence payload for the multi-source story act.
#
# The public ratio is source-specific:
#   (local source count / local Census population) / fitted source rate
# where the fitted rate is the slope from a through-origin regression of the
# source count on Census population across all 331 local authority districts.

required_packages <- c("digest", "jsonlite")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Install required R packages before building: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) != 1L) {
  stop("Run this builder with Rscript.", call. = FALSE)
}

script_path <- normalizePath(
  sub("^--file=", "", script_arg),
  winslash = "/",
  mustWork = TRUE
)
site_dir <- dirname(dirname(script_path))
repo_dir <- normalizePath(
  file.path(site_dir, "../../../.."),
  winslash = "/",
  mustWork = TRUE
)

counts_rel_path <- "outputs/manuscript-data/lad-derived-population-counts.csv"
lookup_rel_path <- "paper/rsos-debias/scrollytelling/site/data/meta-story.json"
counts_path <- file.path(repo_dir, counts_rel_path)
lookup_path <- file.path(repo_dir, lookup_rel_path)
output_path <- file.path(site_dir, "data/source-story.json")

expected_input_sha256 <-
  "e8e6d0bc4b9bb146f713c9ce6ab882316569a79d4664a9e7e7b0dcf29804e7a8"
input_sha256 <- digest::digest(
  file = counts_path,
  algo = "sha256",
  serialize = FALSE
)
if (!identical(input_sha256, expected_input_sha256)) {
  stop(
    "Released LAD input has changed. Expected SHA-256 ",
    expected_input_sha256,
    ", found ",
    input_sha256,
    ". Re-audit the public fixtures before rebuilding.",
    call. = FALSE
  )
}

counts <- read.csv(
  counts_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
required_columns <- c(
  "name",
  "population",
  "twitter_x_population",
  "meta_population",
  "multiapp1_population",
  "multiapp2_population"
)
missing_columns <- setdiff(required_columns, names(counts))
if (length(missing_columns) > 0L) {
  stop(
    "Released LAD input is missing columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}
if (nrow(counts) != 331L || anyDuplicated(counts$name)) {
  stop("Expected 331 uniquely named LAD rows.", call. = FALSE)
}

lookup_payload <- jsonlite::fromJSON(lookup_path, simplifyVector = TRUE)
lookup <- lookup_payload$areas[, c("code", "name", "population")]
if (nrow(lookup) != 331L || anyDuplicated(lookup$code) || anyDuplicated(lookup$name)) {
  stop("Expected 331 unique names and codes in meta-story.json.", call. = FALSE)
}
lookup_index <- match(counts$name, lookup$name)
if (anyNA(lookup_index) || !setequal(counts$name, lookup$name)) {
  stop("LAD names do not match the existing stable-code lookup.", call. = FALSE)
}

# Convert before multiplication: several input columns are read as integers,
# and integer products would overflow for the through-origin slope.
population <- as.double(counts$population)
lookup_population <- as.double(lookup$population[lookup_index])
if (
  any(!is.finite(population)) ||
    any(population <= 0) ||
    !isTRUE(all.equal(population, lookup_population, tolerance = 0))
) {
  stop("Census populations are invalid or disagree with meta-story.json.", call. = FALSE)
}

source_specs <- list(
  twitter_x = list(
    id = "twitter_x",
    label = "Twitter/X",
    source_type = "single-application",
    period = "March 2021",
    numerator_label = "Inferred monthly home locations of active accounts",
    count_field = "twitter_x_population",
    numerator_caveat = paste(
      "Unique accounts are used as a proxy for unique users;",
      "they are not verified unique residents."
    )
  ),
  meta = list(
    id = "meta",
    label = "Meta",
    source_type = "single-application",
    period = "March 2021",
    numerator_label = "Average nighttime active-account estimate",
    count_field = "meta_population",
    numerator_caveat = paste(
      "This is an aggregate estimate from accounts with location services",
      "and includes privacy transformations; it is not a count of verified",
      "unique residents."
    )
  ),
  multiapp1 = list(
    id = "multiapp1",
    label = "Multi-app1",
    source_type = "multi-application",
    period = "First week of April 2021",
    numerator_label = "Inferred home locations of qualifying observed devices",
    count_field = "multiapp1_population",
    numerator_caveat = paste(
      "Devices proxy users, and one individual may use multiple devices;",
      "the numerator is not identical to persons."
    )
  ),
  multiapp2 = list(
    id = "multiapp2",
    label = "Multi-app2",
    source_type = "multi-application",
    period = "November 2021",
    numerator_label = paste(
      "Inferred home locations from preprocessed multi-application GPS data"
    ),
    count_field = "multiapp2_population",
    numerator_caveat = paste(
      "This is an analysis-ready aggregate with source-specific home",
      "inference; the numerator is not identical to persons."
    )
  )
)
source_ids <- names(source_specs)

source_counts <- setNames(
  lapply(source_specs, function(spec) as.double(counts[[spec$count_field]])),
  source_ids
)
if (any(!vapply(source_counts, function(x) all(is.finite(x) & x >= 0), logical(1)))) {
  stop("Source counts must be finite and non-negative.", call. = FALSE)
}

fitted_denominator <- sum(population * population)
fitted_rates <- vapply(
  source_ids,
  function(id) sum(population * source_counts[[id]]) / fitted_denominator,
  numeric(1)
)
raw_count_pearson_r <- vapply(
  source_ids,
  function(id) stats::cor(population, source_counts[[id]]),
  numeric(1)
)

rates_per_100 <- setNames(
  lapply(source_ids, function(id) 100 * source_counts[[id]] / population),
  source_ids
)
ratio_to_fitted <- setNames(
  lapply(
    source_ids,
    function(id) (source_counts[[id]] / population) / fitted_rates[[id]]
  ),
  source_ids
)
ratio_matrix <- do.call(cbind, ratio_to_fitted)
colnames(ratio_matrix) <- source_ids
storage.mode(ratio_matrix) <- "double"

side_matrix <- ifelse(
  ratio_matrix < 1,
  "below",
  ifelse(ratio_matrix > 1, "above", "at_fitted")
)
colnames(side_matrix) <- source_ids
switches_sides <- apply(
  ratio_matrix,
  1L,
  function(x) min(x) < 1 && max(x) > 1
)
consistent_below <- apply(ratio_matrix, 1L, function(x) max(x) < 1)
consistent_above <- apply(ratio_matrix, 1L, function(x) min(x) > 1)
pattern_class <- ifelse(
  switches_sides,
  "switches_sides",
  ifelse(
    consistent_below,
    "consistent_below",
    ifelse(consistent_above, "consistent_above", "at_fitted_or_other")
  )
)

switch_count <- sum(switches_sides)
consistent_below_count <- sum(consistent_below)
consistent_above_count <- sum(consistent_above)
other_count <- sum(pattern_class == "at_fitted_or_other")
if (
  switch_count != 300L ||
    consistent_below_count != 22L ||
    consistent_above_count != 9L ||
    other_count != 0L
) {
  stop(
    "Area-pattern fixture failed: expected 300 switching, 22 consistently ",
    "below, 9 consistently above and 0 other.",
    call. = FALSE
  )
}

assert_close <- function(actual, expected, tolerance = 1e-9, label = "value") {
  if (
    length(actual) != length(expected) ||
      any(!is.finite(actual)) ||
      any(abs(actual - expected) > tolerance)
  ) {
    stop(
      label,
      " fixture failed. Expected ",
      paste(expected, collapse = ", "),
      "; found ",
      paste(actual, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
}

expected_fitted_rates <- c(
  twitter_x = 0.001918051487135464,
  meta = 0.08094803632650273,
  multiapp1 = 0.023770484224175808,
  multiapp2 = 0.010230419696305199
)
assert_close(
  fitted_rates[source_ids],
  expected_fitted_rates[source_ids],
  tolerance = 1e-14,
  label = "Through-origin fitted rates"
)

area_codes <- as.character(lookup$code[lookup_index])
area_names <- as.character(counts$name)

make_observations <- function(i) {
  setNames(
    lapply(
      source_ids,
      function(id) {
        list(
          count = as.double(source_counts[[id]][i]),
          rate_per_100_residents = as.double(rates_per_100[[id]][i]),
          ratio_to_fitted = as.double(ratio_to_fitted[[id]][i]),
          side_of_fitted = unname(side_matrix[i, id])
        )
      }
    ),
    source_ids
  )
}

compact_side_pattern <- function(i) {
  paste(
    ifelse(
      side_matrix[i, source_ids] == "above",
      "A",
      ifelse(side_matrix[i, source_ids] == "below", "B", "F")
    ),
    collapse = ""
  )
}

areas <- lapply(seq_len(nrow(counts)), function(i) {
  list(
    code = area_codes[[i]],
    name = area_names[[i]],
    switches_sides = isTRUE(switches_sides[[i]]),
    pattern_class = unname(pattern_class[[i]]),
    side_pattern = compact_side_pattern(i)
  )
})

pair_names <- c("Watford", "North East Derbyshire")
pair_indices <- match(pair_names, area_names)
if (anyNA(pair_indices)) {
  stop("The named comparison pair is missing.", call. = FALSE)
}
pair <- lapply(pair_indices, function(i) {
  list(
    code = area_codes[[i]],
    name = area_names[[i]],
    population = as.double(population[[i]]),
    switches_sides = isTRUE(switches_sides[[i]]),
    pattern_class = unname(pattern_class[[i]]),
    side_pattern = compact_side_pattern(i),
    observations = make_observations(i)
  )
})

expected_pair <- list(
  Watford = list(
    code = "E07000103",
    counts = c(twitter_x = 213, meta = 2419, multiapp1 = 3496, multiapp2 = 1325),
    ratios = c(
      twitter_x = 1.0861080076,
      meta = 0.2922693163,
      multiapp1 = 1.4384244894,
      multiapp2 = 1.2667067964
    ),
    sides = c(
      twitter_x = "above",
      meta = "below",
      multiapp1 = "above",
      multiapp2 = "above"
    )
  ),
  `North East Derbyshire` = list(
    code = "E07000038",
    counts = c(twitter_x = 95, meta = 16204, multiapp1 = 2004, multiapp2 = 1265),
    ratios = c(
      twitter_x = 0.4855779025,
      meta = 1.9625082318,
      multiapp1 = 0.8265236005,
      multiapp2 = 1.2122512630
    ),
    sides = c(
      twitter_x = "below",
      meta = "above",
      multiapp1 = "below",
      multiapp2 = "above"
    )
  )
)
for (pair_position in seq_along(pair_names)) {
  area_name <- pair_names[[pair_position]]
  fixture <- expected_pair[[area_name]]
  area <- pair[[pair_position]]
  if (!identical(area$code, fixture$code)) {
    stop(area_name, " stable-code fixture failed.", call. = FALSE)
  }
  actual_counts <- vapply(
    area$observations[source_ids],
    function(x) x$count,
    numeric(1)
  )
  actual_ratios <- vapply(
    area$observations[source_ids],
    function(x) x$ratio_to_fitted,
    numeric(1)
  )
  actual_sides <- vapply(
    area$observations[source_ids],
    function(x) x$side_of_fitted,
    character(1)
  )
  assert_close(
    actual_counts,
    as.double(fixture$counts[source_ids]),
    tolerance = 0,
    label = paste(area_name, "counts")
  )
  assert_close(
    actual_ratios,
    as.double(fixture$ratios[source_ids]),
    tolerance = 1e-9,
    label = paste(area_name, "ratios")
  )
  if (!identical(unname(actual_sides), unname(fixture$sides[source_ids]))) {
    stop(area_name, " side-of-fitted fixture failed.", call. = FALSE)
  }
}

sources <- lapply(source_ids, function(id) {
  spec <- source_specs[[id]]
  list(
    id = spec$id,
    label = spec$label,
    source_type = spec$source_type,
    period = spec$period,
    numerator_label = spec$numerator_label,
    denominator_label = "2021 Census resident population, all ages",
    count_field = spec$count_field,
    fitted_rate_per_resident = as.double(fitted_rates[[id]]),
    fitted_rate_per_100_residents = as.double(100 * fitted_rates[[id]]),
    raw_count_pearson_r = as.double(raw_count_pearson_r[[id]]),
    numerator_caveat = spec$numerator_caveat
  )
})

payload <- list(
  metadata = list(
    title = "The same place looks different through different data",
    analysis_area = "England and Wales",
    geography = "331 local authority districts using 2021 boundaries",
    area_count = as.integer(nrow(counts)),
    source_count = as.integer(length(source_ids)),
    switch_count = as.integer(switch_count),
    consistent_below_count = as.integer(consistent_below_count),
    consistent_above_count = as.integer(consistent_above_count),
    exact_switch_predicate = paste(
      "minimum ratio_to_fitted across the four sources < 1 AND",
      "maximum ratio_to_fitted across the four sources > 1"
    ),
    input_sha256 = input_sha256,
    source_order = source_ids,
    side_pattern_encoding = paste(
      "Four characters in source_order: A = above, B = below,",
      "F = exactly at the fitted rate"
    ),
    metric = list(
      public_name = "Relative local coverage index",
      field_name = "ratio_to_fitted",
      formula = paste(
        "(local source count / 2021 Census resident population) /",
        "fitted through-origin source rate"
      ),
      fitted_model = "source_count_i = alpha_source * census_population_i",
      baseline = 1.0,
      below_label = "Below that source's fitted proportional rate",
      above_label = "Above that source's fitted proportional rate",
      interpretation = paste(
        "A value of 1 lies on that source's fitted proportional line.",
        "Values below or above 1 indicate fewer or more observed identifiers",
        "per resident than that source's fitted rate."
      )
    ),
    predicates = list(
      below = "ratio_to_fitted < 1",
      above = "ratio_to_fitted > 1",
      switch = paste(
        "minimum ratio_to_fitted across sources < 1 AND",
        "maximum ratio_to_fitted across sources > 1"
      ),
      consistent_below = "maximum ratio_to_fitted across sources < 1",
      consistent_above = "minimum ratio_to_fitted across sources > 1"
    ),
    inputs = list(
      released_lad_counts = list(
        path = counts_rel_path,
        sha256 = input_sha256
      ),
      lad_code_lookup = list(
        path = lookup_rel_path,
        sha256 = digest::digest(
          file = lookup_path,
          algo = "sha256",
          serialize = FALSE
        )
      )
    ),
    caveats = c(
      paste(
        "Source numerators differ: active accounts, aggregate active-account",
        "estimates, observed devices and inferred home-location users are not",
        "interchangeable unique people."
      ),
      paste(
        "Source periods differ (March, early April and November 2021), so this",
        "is not a harmonised same-date comparison."
      ),
      paste(
        "The index normalises each source to its own fitted scale; it does not",
        "make the underlying source-generation mechanisms equivalent."
      ),
      paste(
        "A value above or below 1 is not a representativeness score, an",
        "individual inclusion probability or a judgement of data quality."
      ),
      paste(
        "Aggregate local-authority data cannot identify which individuals are",
        "absent, duplicated or differently observed."
      ),
      paste(
        "All calculations use the released LAD-level analytical table and",
        "should not be mixed with separately reported national source totals."
      ),
      paste(
        "Raw-count Pearson correlations are reported only to expose why high",
        "linear association does not rule out large local scale differences."
      )
    )
  ),
  sources = sources,
  pair = pair,
  areas = areas
)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  payload,
  path = output_path,
  auto_unbox = TRUE,
  digits = 12,
  pretty = FALSE,
  na = "null"
)

message(
  "Wrote ",
  output_path,
  " (", nrow(counts), " areas; ",
  switch_count, " switch sides, ",
  consistent_below_count, " consistently below, ",
  consistent_above_count, " consistently above)."
)
