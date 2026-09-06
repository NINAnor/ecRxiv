# ==============================================================================
# ecRxiv -> Miljødirektoratet Adminportal
# Forest + mountain ecological condition assessments
#
# PURPOSE
# -------
# This script:
#
#   1. Reads the forest and mountain indicator manifests.
#   2. Combines them into one indicator registry.
#   3. Uses the existing index_functions.R machinery to locate and read the
#      heterogeneous indicator result files.
#   4. Separates forest and mountain before calculating ecosystem indices.
#   5. Extracts OBSERVED indicator assessments for Adminportal.
#   6. Calculates ECT/property and total ecosystem assessments.
#   7. Joins Adminportal-specific reference IDs from lookup tables.
#   8. Performs QA checks.
#   9. Builds the Adminportal JSON structure.
#  10. Writes index.json and creates a ZIP file for each ecosystem.
#
#
# IMPORTANT
# ---------
# Adminportal reference IDs, protocol numbers and JSON property names are not
# yet known. The script creates template lookup CSVs if they do not exist.
#
# Until those have been filled in, the workflow can run through the ecological
# calculations and QA, but the final production export should be blocked.
#
#
# IMPORTANT DISTINCTION
# ---------------------
# INDICATOR assessments:
#
#   load_registry_draws()
#       -> observed indicator results only
#       -> indikatorVurderinger
#
# ECT and ecosystem assessments:
#
#   calculate_index()
#       -> may impute missing indicator reporting years for aggregation
#       -> ECT summaries -> egenskapsVurderinger
#       -> Index summaries -> totalVurdering
#
# Do NOT use calculate_index()$indicator_draws to populate
# indikatorVurderinger.
#
# ==============================================================================


# ==============================================================================
# 0. Packages
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tibble)
library(stringr)
library(jsonlite)
library(here)


# jsonvalidate is optional until the JSON schema is finalised.
# Install if required:
#
# install.packages("jsonvalidate")


# ==============================================================================
# 1. Load existing ecRxiv index functions
# ==============================================================================

source(
  here::here(
    "R",
    "NO_index_functions.R"
  )
)


# ==============================================================================
# 2. General settings
# ==============================================================================

# National assessment/reporting years.
#
# These are NOT the full temporal ranges of the underlying datasets.
# They are the years for which ecological-condition assessments are reported.

report_years <- c(
  2019L,
  2024L
)


# Monte Carlo draws used for ECT and index aggregation.

n_sim <- 1000L


# Set a seed so repeated runs of the aggregation are reproducible.

set.seed(123)


# Output directories.

dir.create(
  here::here("data"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here::here("export", "adminportal"),
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 3. Read forest and mountain indicator manifests
# ==============================================================================

indicator_manifest_forest <- readr::read_csv(
  paste0(
    "https://raw.githubusercontent.com/",
    "NINAnor/ecRxiv/refs/heads/no-index-001/",
    "data/indicators_2026.csv"
  ),
  show_col_types = FALSE
)


indicator_manifest_mountain <- readr::read_csv(
  paste0(
    "https://raw.githubusercontent.com/",
    "NINAnor/ecRxiv/refs/heads/no-index-001/",
    "data/indicators_mountain.csv"
  ),
  show_col_types = FALSE
)


# ==============================================================================
# 4. Check that both manifests have the same structure
# ==============================================================================

if (!setequal(
  names(indicator_manifest_forest),
  names(indicator_manifest_mountain)
)) {
  
  stop(
    "Forest and mountain indicator manifests do not have the same columns.",
    call. = FALSE
  )
}


message(
  "Forest indicators: ",
  nrow(indicator_manifest_forest)
)

message(
  "Mountain indicators: ",
  nrow(indicator_manifest_mountain)
)


# ==============================================================================
# 5. Combine manifests
# ==============================================================================

indicator_manifest <- dplyr::bind_rows(
  indicator_manifest_forest,
  indicator_manifest_mountain
) |>
  dplyr::mutate(
    
    # Ensure IDs are character.
    indicatorID =
      as.character(indicatorID),
    
    # This currently comes in as logical because it is all NA.
    # Force it to character so future values such as "2019;2024" work.
    reportingYears =
      as.character(reportingYears),
    
    # Extract A1, A2, B1, etc. from the full ECT label.
    ect =
      parse_ect(ECT)
  )


message(
  "Combined indicator manifest: ",
  nrow(indicator_manifest),
  " rows."
)


# ==============================================================================
# 6. Basic manifest QA
# ==============================================================================


# ------------------------------------------------------------------------------
# Missing indicator IDs
# ------------------------------------------------------------------------------

missing_indicator_ids <- indicator_manifest |>
  dplyr::filter(
    is.na(indicatorID) |
      indicatorID == ""
  )

if (nrow(missing_indicator_ids) > 0) {
  
  print(missing_indicator_ids)
  
  stop(
    "One or more indicators have no indicatorID.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# Duplicate IDs within ecosystem
# ------------------------------------------------------------------------------

duplicate_indicator_ids <- indicator_manifest |>
  dplyr::count(
    ecosystem,
    indicatorID,
    name = "n"
  ) |>
  dplyr::filter(n > 1)

if (nrow(duplicate_indicator_ids) > 0) {
  
  warning(
    "Duplicate indicator IDs found within an ecosystem.",
    call. = FALSE
  )
  
  print(duplicate_indicator_ids)
}


# ------------------------------------------------------------------------------
# Check ECT parsing
# ------------------------------------------------------------------------------

missing_ect <- indicator_manifest |>
  dplyr::filter(
    is.na(ect)
  )

if (nrow(missing_ect) > 0) {
  
  print(
    missing_ect |>
      dplyr::select(
        ecosystem,
        ECT,
        indicatorName,
        indicatorID
      )
  )
  
  stop(
    "One or more ECT codes could not be parsed.",
    call. = FALSE
  )
}


message(
  "ECT classes: ",
  paste(
    sort(unique(indicator_manifest$ect)),
    collapse = ", "
  )
)


# ==============================================================================
# 7. Save combined manifest
# ==============================================================================
#
# build_indicator_registry() currently expects a CSV path rather than an
# already-created tibble, so write the combined manifest and feed it into the
# existing function.

combined_manifest_path <- here::here(
  "data",
  "indicators_forest_mountain.csv"
)


readr::write_csv(
  indicator_manifest,
  combined_manifest_path
)


# ==============================================================================
# 8. Build the ecRxiv indicator registry
# ==============================================================================
#
# This existing function:
#
#   - adds the parsed ECT
#   - maps indicator IDs to published folders
#   - resolves actual result files
#   - detects CSV/parquet/RDS sources
#   - handles main/submission branches
#   - adds result URLs
#   - adds availability status
#
# Every row already contains ecosystem = forest/mountain.
#
# "forest" below is therefore only the fallback value required by the current
# function signature. Existing ecosystem values are preserved.

n_connections <- function() {
  nrow(showConnections(all = TRUE))
}

message("Connections before registry build: ", n_connections())
closeAllConnections()
indicator_registry <- build_indicator_registry(
  csv_path = combined_manifest_path,
  ecosystem = "forest",
  check_remote = TRUE
)

message("Connections after registry build: ", n_connections())
closeAllConnections()
# ==============================================================================
# 9. Inspect registry
# ==============================================================================

message("\nRegistry columns:")

print(
  names(indicator_registry)
)


message("\nIndicator result sources:")

indicator_registry |>
  dplyr::select(
    ecosystem,
    ect,
    indicatorName,
    indicatorID,
    folder,
    has_results,
    match_status,
    results_ref,
    results_source,
    results_path
  ) |>
  print(n = Inf)


# ==============================================================================
# 10. Check indicators without usable results
# ==============================================================================

missing_results <- indicator_registry |>
  dplyr::filter(
    !has_results
  ) |>
  dplyr::select(
    ecosystem,
    ect,
    indicatorName,
    indicatorID,
    folder,
    match_status
  )


if (nrow(missing_results) > 0) {
  
  message(
    "\nIndicators currently without resolved results:"
  )
  
  print(missing_results)
}


message("\nResult availability by ecosystem:")

indicator_registry |>
  dplyr::count(
    ecosystem,
    has_results
  ) |>
  print()


# ==============================================================================
# 11. Check ID/folder differences
# ==============================================================================
#
# These are expected in some cases, e.g. Brown bear and combined
# butterfly/bumblebee indicator folders.

message("\nIndicator IDs where folder != ID:")

indicator_registry |>
  dplyr::filter(
    indicatorID != folder
  ) |>
  dplyr::select(
    ecosystem,
    indicatorID,
    folder,
    indicatorName
  ) |>
  print(n = Inf)

# read in lookup

#https://raw.githubusercontent.com/NINAnor/ecRxiv/refs/heads/no-index-001/data/region_lookup.csv

# ==============================================================================
# 12. Split registry by ecosystem BEFORE index calculation
# ==============================================================================
#
# Combining the manifests is useful for managing the registry.
#
# But forest and mountain MUST be calculated independently.

registry_forest <- indicator_registry |>
  dplyr::filter(
    ecosystem == "forest"
  )


registry_mountain <- indicator_registry |>
  dplyr::filter(
    ecosystem == "mountain"
  )


message(
  "\nForest registry rows: ",
  nrow(registry_forest)
)

message(
  "Mountain registry rows: ",
  nrow(registry_mountain)
)


# ==============================================================================
# 13. Load OBSERVED indicator draws
# ==============================================================================
#
# These are the values that should ultimately become indikatorVurderinger.
#
# load_registry_draws() harmonises the heterogeneous indicator result files but
# does NOT perform the reporting-year imputation used inside calculate_index().

forest_draws_observed <- load_registry_draws(
  registry = registry_forest,
  n_years = length(report_years),
  report_years = report_years
)


mountain_draws_observed <- load_registry_draws(
  registry = registry_mountain,
  n_years = length(report_years),
  report_years = report_years
)


# ==============================================================================
# 14. Inspect observed draw structure
# ==============================================================================

message("\nObserved forest draw columns:")

print(
  names(forest_draws_observed)
)


message("\nObserved mountain draw columns:")

print(
  names(mountain_draws_observed)
)


# Canonical parts should normally be:
#
# C, E, S, W, N, Norway

message("\nForest geographic parts:")

print(
  sort(
    unique(
      forest_draws_observed$part
    )
  )
)


message("\nMountain geographic parts:")

print(
  sort(
    unique(
      mountain_draws_observed$part
    )
  )
)


# ==============================================================================
# 15. Helper: summarise observed indicator distributions
# ==============================================================================
#
# Uses summarise_draw_stats() from index_functions.R:
#
#   median
#   2.5% quantile
#   97.5% quantile

summarise_indicator_estimates <- function(draws) {
  
  draws |>
    dplyr::group_by(
      ecosystem,
      indicator_id,
      indicator_name,
      ect,
      year,
      part
    ) |>
    dplyr::group_modify(
      ~ summarise_draw_stats(.x)
    ) |>
    dplyr::ungroup()
}


# ==============================================================================
# 16. Summarise observed indicator estimates
# ==============================================================================

indicator_estimates_forest <-
  summarise_indicator_estimates(
    forest_draws_observed
  )


indicator_estimates_mountain <-
  summarise_indicator_estimates(
    mountain_draws_observed
  )


# Combined object useful for QA only.

indicator_estimates <- dplyr::bind_rows(
  indicator_estimates_forest,
  indicator_estimates_mountain
)


# ==============================================================================
# 17. Check genuinely available reporting years
# ==============================================================================
#
# This is OBSERVED coverage.
#
# If an indicator only has 2024 data, it should appear only in 2024 here.

observed_years <- indicator_estimates |>
  dplyr::distinct(
    ecosystem,
    indicator_id,
    indicator_name,
    year
  ) |>
  dplyr::arrange(
    ecosystem,
    indicator_id,
    year
  )


message("\nObserved indicator reporting years:")

print(
  observed_years,
  n = Inf
)


message("\nNumber of observed indicators by ecosystem/year:")

observed_years |>
  dplyr::count(
    ecosystem,
    year,
    name = "n_indicators"
  ) |>
  print()


# ==============================================================================
# 18. Indicator × geography × year coverage
# ==============================================================================

observed_coverage <- indicator_estimates |>
  dplyr::distinct(
    ecosystem,
    indicator_id,
    indicator_name,
    ect,
    year,
    part
  ) |>
  dplyr::arrange(
    ecosystem,
    indicator_id,
    year,
    part
  )


qa_indicator_coverage <- observed_coverage |>
  dplyr::count(
    ecosystem,
    indicator_id,
    indicator_name,
    year,
    name = "n_areas"
  )


print(
  qa_indicator_coverage,
  n = Inf
)


# ==============================================================================
# 19. Validate observed indicator intervals
# ==============================================================================

bad_indicator_intervals <- indicator_estimates |>
  dplyr::filter(
    is.na(median) |
      is.na(q025) |
      is.na(q975) |
      q025 > median |
      median > q975
  )


if (nrow(bad_indicator_intervals) > 0) {
  
  print(bad_indicator_intervals)
  
  stop(
    "One or more observed indicator estimates have invalid intervals.",
    call. = FALSE
  )
}


# ==============================================================================
# 20. Calculate forest and mountain ECT/index distributions
# ==============================================================================
#
# calculate_index():
#
#   indicator draws
#       ->
#   ECT distributions
#       ->
#   overall ecosystem index
#
# Missing report-year indicator values can be imputed here for index
# aggregation. This is why these indicator draws must NOT be used directly for
# indikatorVurderinger.

set.seed(123)

index_forest <- calculate_index(
  registry = registry_forest,
  n_sim = n_sim,
  n_years = length(report_years),
  report_years = report_years,
  include_national = TRUE,
  include_direct_index = FALSE
)


set.seed(123)

index_mountain <- calculate_index(
  registry = registry_mountain,
  n_sim = n_sim,
  n_years = length(report_years),
  report_years = report_years,
  include_national = TRUE,
  include_direct_index = FALSE
)


# ==============================================================================
# 21. Extract ECT and total ecosystem estimates
# ==============================================================================

ect_estimates_forest <- index_forest$summaries |>
  dplyr::filter(
    level == "ECT"
  )


ect_estimates_mountain <- index_mountain$summaries |>
  dplyr::filter(
    level == "ECT"
  )


total_estimates_forest <- index_forest$summaries |>
  dplyr::filter(
    level == "Index"
  )


total_estimates_mountain <- index_mountain$summaries |>
  dplyr::filter(
    level == "Index"
  )


# ==============================================================================
# 22. Save an audit of imputed indicator years used in index calculation
# ==============================================================================
#
# These values are legitimate inputs to the composite-index methodology,
# but they are NOT observed indicator assessments for Adminportal.

extract_imputation_log <- function(index_result) {
  
  draws <- index_result$indicator_draws
  
  if (!"imputed" %in% names(draws)) {
    
    return(
      tibble::tibble()
    )
  }
  
  draws |>
    dplyr::filter(
      dplyr::coalesce(
        imputed,
        FALSE
      )
    ) |>
    dplyr::distinct(
      ecosystem,
      indicator_id,
      indicator_name,
      part,
      year,
      source_year
    ) |>
    dplyr::arrange(
      ecosystem,
      indicator_id,
      part,
      year
    )
}


imputation_log_forest <-
  extract_imputation_log(
    index_forest
  )


imputation_log_mountain <-
  extract_imputation_log(
    index_mountain
  )


imputation_log <- dplyr::bind_rows(
  imputation_log_forest,
  imputation_log_mountain
)


readr::write_csv(
  imputation_log,
  here::here(
    "export",
    "adminportal",
    "index_imputation_log.csv"
  )
)


# ==============================================================================
# 23. Create/read Adminportal indicator lookup
# ==============================================================================
#
# This file stores portal-specific information that is NOT part of the
# ecological result calculation.
#
# portal_created:
#   TRUE after the indicator has been manually created in Adminportal.
#
# protocol_number:
#   Fill with the Adminportal protocol number once known.
#
# description_public:
#   Short public-friendly description requested by Miljødirektoratet.
#
# protocol_owner:
#   Use named owner where appropriate, otherwise NINA.
#
# NOTE:
# The exact JSON location/name for protocol number still needs to be confirmed
# against the final Adminportal requirements.

portal_indicator_path <- here::here(
  "data",
  "adminportal_indicators.csv"
)


if (!file.exists(portal_indicator_path)) {
  
  portal_indicators <- indicator_registry |>
    dplyr::distinct(
      ecosystem,
      indicatorID,
      indicatorName,
      verbatimeName,
      ect
    ) |>
    dplyr::mutate(
      portal_created = FALSE,
      protocol_number = NA_character_,
      description_public = NA_character_,
      protocol_owner = "NINA",
      pressures = NA_character_,
      factsheet_url = NA_character_
    )
  
  readr::write_csv(
    portal_indicators,
    portal_indicator_path
  )
  
  message(
    "\nCreated Adminportal indicator template:\n",
    portal_indicator_path
  )
  
} else {
  
  portal_indicators <- readr::read_csv(
    portal_indicator_path,
    show_col_types = FALSE
  )
}


# ==============================================================================
# 24. Create/read Adminportal geographic-area lookup
# ==============================================================================
#
# The existing ecRxiv functions already convert all source region aliases to
# these six canonical parts.
#
# Fill the second column once the official Adminportal UIDs are known.

portal_geography_path <- here::here(
  "data",
  "adminportal_geography.csv"
)


if (!file.exists(portal_geography_path)) {
  
  portal_geography <- tibble::tribble(
    ~part,     ~geografiskOmradeReferanseUid,
    "Norway",  NA_character_,
    "C",       NA_character_,
    "E",       NA_character_,
    "S",       NA_character_,
    "W",       NA_character_,
    "N",       NA_character_
  )
  
  readr::write_csv(
    portal_geography,
    portal_geography_path
  )
  
  message(
    "\nCreated Adminportal geography template:\n",
    portal_geography_path
  )
  
} else {
  
  portal_geography <- readr::read_csv(
    portal_geography_path,
    show_col_types = FALSE
  )
}


# ==============================================================================
# 25. Create/read Adminportal ECT/property lookup
# ==============================================================================
#
# IMPORTANT:
#
# There appear to be TWO different portal concepts we need:
#
# property_reference_uid
#     used where the schema requires egenskapReferanseUid
#
# json_property_name
#     weighting-object field name, e.g. potentially "biomasse",
#     "abiotiske-forhold", etc.
#
# Do not assume these are the same thing.
#
# Fill both columns from the official Adminportal information.

portal_property_path <- here::here(
  "data",
  "adminportal_properties.csv"
)


if (!file.exists(portal_property_path)) {
  
  portal_properties <- tibble::tribble(
    ~ect, ~property_reference_uid, ~json_property_name,
    "A1", NA_character_, NA_character_,
    "A2", NA_character_, NA_character_,
    "B1", NA_character_, NA_character_,
    "B2", NA_character_, NA_character_,
    "B3", NA_character_, NA_character_,
    "C1", NA_character_, NA_character_
  )
  
  readr::write_csv(
    portal_properties,
    portal_property_path
  )
  
  message(
    "\nCreated Adminportal property template:\n",
    portal_property_path
  )
  
} else {
  
  portal_properties <- readr::read_csv(
    portal_property_path,
    show_col_types = FALSE
  )
}


# ==============================================================================
# 26. Ecosystem reference lookup
# ==============================================================================
#
# Fill the reference UIDs with the official Adminportal ecosystem IDs.
#
# Forest and mountain apparently already exist in the portal, so these should
# not require creating new ecosystems.

ecosystem_lookup_path <- here::here(
  "data",
  "adminportal_ecosystems.csv"
)


if (!file.exists(ecosystem_lookup_path)) {
  
  ecosystem_lookup <- tibble::tribble(
    ~ecosystem, ~ecosystem_reference_uid,
    "forest",   NA_character_,
    "mountain", NA_character_
  )
  
  readr::write_csv(
    ecosystem_lookup,
    ecosystem_lookup_path
  )
  
  message(
    "\nCreated Adminportal ecosystem template:\n",
    ecosystem_lookup_path
  )
  
} else {
  
  ecosystem_lookup <- readr::read_csv(
    ecosystem_lookup_path,
    show_col_types = FALSE
  )
}


# ==============================================================================
# 27. Dataset metadata
# ==============================================================================
#
# This is deliberately separate from the indicator result files.
#
# Miljødirektoratet has clarified that dataset period should be the FULL
# temporal range of the source dataset, even if only part of that range was
# used in the assessment.
#
# Example:
#
# snow source dataset:
#     1959 -> 2024
#
# indicator assessment:
#     2019
#     2024
#
# These are different things.

portal_dataset_path <- here::here(
  "data",
  "adminportal_datasets.csv"
)


if (!file.exists(portal_dataset_path)) {
  
  portal_datasets <- indicator_registry |>
    dplyr::distinct(
      ecosystem,
      indicatorID,
      indicatorName
    ) |>
    dplyr::transmute(
      ecosystem,
      indicatorID,
      navn = indicatorName,
      link = NA_character_,
      kilde = NA_character_,
      periodeStart = NA_character_,
      periodeSlutt = NA_character_,
      type = NA_character_
    )
  
  readr::write_csv(
    portal_datasets,
    portal_dataset_path
  )
  
  message(
    "\nCreated Adminportal dataset template:\n",
    portal_dataset_path
  )
  
} else {
  
  portal_datasets <- readr::read_csv(
    portal_dataset_path,
    show_col_types = FALSE
  )
}


# ==============================================================================
# 28. Portal date helper
# ==============================================================================
#
# TEMPORARY ASSUMPTION:
#
# The available documentation appears to require both periodeStart and
# periodeSlutt even where the assessment represents one reporting year.
#
# We are currently encoding a reporting year as 1 January of that year in both
# fields.
#
# VERIFY THIS WITH MILJØDIREKTORATET BEFORE FINAL UPLOAD.

make_portal_date <- function(year) {
  
  sprintf(
    "%d-01-01 00:00:00",
    as.integer(year)
  )
}


# ==============================================================================
# 29. Build Adminportal indicator assessments
# ==============================================================================

build_portal_indicator_assessments <- function(
    estimates,
    portal_indicators,
    portal_geography) {
  
  estimates |>
    
    dplyr::left_join(
      portal_indicators |>
        dplyr::select(
          indicatorID,
          protocol_number,
          description_public
        ),
      by = c(
        "indicator_id" = "indicatorID"
      )
    ) |>
    
    dplyr::left_join(
      portal_geography,
      by = "part"
    ) |>
    
    dplyr::transmute(
      
      indikatorBeskrivelse =
        dplyr::coalesce(
          description_public,
          indicator_name
        ),
      
      indikatorReferanseUid =
        indicator_id,
      
      # IMPORTANT:
      # Confirm the exact JSON field name/location required by Adminportal.
      protokollnummer =
        protocol_number,
      
      geografiskOmradeReferanseUid,
      
      nedreKonfidensIntervalGrense =
        q025,
      
      tilstandsverdi =
        median,
      
      ovreKonfidensIntervalGrense =
        q975,
      
      periodeStart =
        make_portal_date(year),
      
      periodeSlutt =
        make_portal_date(year)
    )
}


indikator_vurderinger_forest <-
  build_portal_indicator_assessments(
    indicator_estimates_forest,
    portal_indicators,
    portal_geography
  )


indikator_vurderinger_mountain <-
  build_portal_indicator_assessments(
    indicator_estimates_mountain,
    portal_indicators,
    portal_geography
  )


# ==============================================================================
# 30. Build Adminportal ECT/property assessments
# ==============================================================================

build_portal_ect_assessments <- function(
    index_result,
    portal_properties,
    portal_geography) {
  
  index_result$summaries |>
    
    dplyr::filter(
      level == "ECT"
    ) |>
    
    dplyr::left_join(
      portal_properties,
      by = c(
        "id" = "ect"
      )
    ) |>
    
    dplyr::left_join(
      portal_geography,
      by = "part"
    ) |>
    
    dplyr::transmute(
      
      egenskapReferanseUid =
        property_reference_uid,
      
      geografiskOmradeReferanseUid,
      
      nedreKonfidensIntervalGrense =
        q025,
      
      aggregertTilstandsverdi =
        median,
      
      ovreKonfidensIntervalGrense =
        q975,
      
      periodeStart =
        make_portal_date(year),
      
      periodeSlutt =
        make_portal_date(year)
    )
}


egenskaps_vurderinger_forest <-
  build_portal_ect_assessments(
    index_forest,
    portal_properties,
    portal_geography
  )


egenskaps_vurderinger_mountain <-
  build_portal_ect_assessments(
    index_mountain,
    portal_properties,
    portal_geography
  )


# ==============================================================================
# 31. Build overall ecosystem assessments
# ==============================================================================

build_portal_total_assessments <- function(
    index_result,
    portal_geography,
    ecosystem_reference_uid,
    assessment_name,
    description = NA_character_,
    link = NA_character_) {
  
  index_result$summaries |>
    
    dplyr::filter(
      level == "Index"
    ) |>
    
    dplyr::left_join(
      portal_geography,
      by = "part"
    ) |>
    
    dplyr::transmute(
      
      okosystemReferanseUid =
        ecosystem_reference_uid,
      
      geografiskOmradeReferanseUid,
      
      nedreKonfidensIntervalGrense =
        q025,
      
      tilstandsverdi =
        median,
      
      ovreKonfidensIntervalGrense =
        q975,
      
      dato =
        make_portal_date(year),
      
      link =
        link,
      
      navn =
        assessment_name,
      
      beskrivelse =
        description
    )
}


forest_ecosystem_uid <- ecosystem_lookup |>
  dplyr::filter(
    ecosystem == "forest"
  ) |>
  dplyr::pull(
    ecosystem_reference_uid
  )


mountain_ecosystem_uid <- ecosystem_lookup |>
  dplyr::filter(
    ecosystem == "mountain"
  ) |>
  dplyr::pull(
    ecosystem_reference_uid
  )


total_vurdering_forest <-
  build_portal_total_assessments(
    index_forest,
    portal_geography,
    ecosystem_reference_uid =
      forest_ecosystem_uid,
    assessment_name =
      "Økologisk tilstand – skog"
  )


total_vurdering_mountain <-
  build_portal_total_assessments(
    index_mountain,
    portal_geography,
    ecosystem_reference_uid =
      mountain_ecosystem_uid,
    assessment_name =
      "Økologisk tilstand – fjell"
  )


# ==============================================================================
# 32. Build sparse indicator -> property weighting
# ==============================================================================
#
# Miljødirektoratet has clarified that we do NOT need to list every ecological
# property with zeros.
#
# Only the relevant property needs to be present, e.g.
#
# {
#   "indikatorReferanseUid": "...",
#   "biomasse": 1
# }

build_portal_weights <- function(
    registry,
    portal_properties) {
  
  weight_data <- registry |>
    
    dplyr::filter(
      match_status == "matched"
    ) |>
    
    dplyr::left_join(
      portal_properties,
      by = "ect"
    )
  
  
  purrr::pmap(
    weight_data |>
      dplyr::select(
        indicatorID,
        json_property_name,
        weight
      ),
    
    function(
    indicatorID,
    json_property_name,
    weight) {
      
      out <- list(
        indikatorReferanseUid =
          indicatorID
      )
      
      if (
        !is.na(json_property_name) &&
        nzchar(json_property_name)
      ) {
        
        out[[json_property_name]] <-
          weight
      }
      
      out
    }
  )
}


indikator_vekting_forest <-
  build_portal_weights(
    registry_forest,
    portal_properties
  )


indikator_vekting_mountain <-
  build_portal_weights(
    registry_mountain,
    portal_properties
  )


# ==============================================================================
# 33. Build kode table
# ==============================================================================
#
# Exact semantics of `kode$link` should be checked against the final Adminportal
# documentation.
#
# For now use the ecRxiv indicator pages/factsheets where available.

build_kode <- function(
    ecosystem,
    portal_indicators) {
  
  portal_indicators |>
    dplyr::filter(
      .data$ecosystem == .env$ecosystem
    ) |>
    dplyr::transmute(
      navn =
        indicatorName,
      link =
        factsheet_url
    )
}


kode_forest <- build_kode(
  "forest",
  portal_indicators
)


kode_mountain <- build_kode(
  "mountain",
  portal_indicators
)


# ==============================================================================
# 34. Build dataset table for each ecosystem
# ==============================================================================

datasett_forest <- portal_datasets |>
  dplyr::filter(
    ecosystem == "forest"
  ) |>
  dplyr::select(
    -ecosystem,
    -indicatorID
  )


datasett_mountain <- portal_datasets |>
  dplyr::filter(
    ecosystem == "mountain"
  ) |>
  dplyr::select(
    -ecosystem,
    -indicatorID
  )


# ==============================================================================
# 35. Generic semantic QA
# ==============================================================================

validate_assessment_values <- function(
    data,
    estimate_column) {
  
  required <- c(
    "nedreKonfidensIntervalGrense",
    estimate_column,
    "ovreKonfidensIntervalGrense"
  )
  
  missing <- setdiff(
    required,
    names(data)
  )
  
  
  if (length(missing) > 0) {
    
    stop(
      "Missing assessment columns: ",
      paste(
        missing,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  
  lower <-
    data[["nedreKonfidensIntervalGrense"]]
  
  estimate <-
    data[[estimate_column]]
  
  upper <-
    data[["ovreKonfidensIntervalGrense"]]
  
  
  invalid <-
    is.na(lower) |
    is.na(estimate) |
    is.na(upper) |
    lower > estimate |
    estimate > upper
  
  
  if (any(invalid)) {
    
    print(
      data[
        invalid,
        ,
        drop = FALSE
      ]
    )
    
    stop(
      sum(invalid),
      " assessment row(s) have invalid values.",
      call. = FALSE
    )
  }
  
  
  invisible(TRUE)
}


# Indicator values.

validate_assessment_values(
  indikator_vurderinger_forest,
  "tilstandsverdi"
)

validate_assessment_values(
  indikator_vurderinger_mountain,
  "tilstandsverdi"
)


# ECT values.

validate_assessment_values(
  egenskaps_vurderinger_forest,
  "aggregertTilstandsverdi"
)

validate_assessment_values(
  egenskaps_vurderinger_mountain,
  "aggregertTilstandsverdi"
)


# Overall values.

validate_assessment_values(
  total_vurdering_forest,
  "tilstandsverdi"
)

validate_assessment_values(
  total_vurdering_mountain,
  "tilstandsverdi"
)


# ==============================================================================
# 36. Check duplicate indicator assessments
# ==============================================================================

check_indicator_duplicates <- function(x) {
  
  duplicates <- x |>
    
    dplyr::count(
      indikatorReferanseUid,
      geografiskOmradeReferanseUid,
      periodeStart,
      periodeSlutt,
      name = "n"
    ) |>
    
    dplyr::filter(
      n > 1
    )
  
  
  if (nrow(duplicates) > 0) {
    
    print(duplicates)
    
    stop(
      "Duplicate indicator-region-period records found.",
      call. = FALSE
    )
  }
  
  
  invisible(TRUE)
}


# Run this fully once the geography UIDs have been filled in.
#
# Until then multiple NA geography values may cause misleading duplicate
# detection.

if (
  !any(
    is.na(
      portal_geography$
      geografiskOmradeReferanseUid
    )
  )
) {
  
  check_indicator_duplicates(
    indikator_vurderinger_forest
  )
  
  check_indicator_duplicates(
    indikator_vurderinger_mountain
  )
}


# ==============================================================================
# 37. QA output tables
# ==============================================================================

qa_indicator_summary <- indicator_estimates |>
  
  dplyr::group_by(
    ecosystem,
    indicator_id,
    indicator_name,
    year
  ) |>
  
  dplyr::summarise(
    
    n_areas =
      dplyr::n_distinct(part),
    
    min_estimate =
      min(
        median,
        na.rm = TRUE
      ),
    
    max_estimate =
      max(
        median,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


readr::write_csv(
  qa_indicator_summary,
  here::here(
    "export",
    "adminportal",
    "qa_indicator_summary.csv"
  )
)


qa_ect_summary <- dplyr::bind_rows(
  
  index_forest$summaries |>
    dplyr::mutate(
      ecosystem = "forest"
    ),
  
  index_mountain$summaries |>
    dplyr::mutate(
      ecosystem = "mountain"
    )
  
) |>
  dplyr::filter(
    level == "ECT"
  )


readr::write_csv(
  qa_ect_summary,
  here::here(
    "export",
    "adminportal",
    "qa_ect_summary.csv"
  )
)


qa_total_summary <- dplyr::bind_rows(
  
  index_forest$summaries |>
    dplyr::mutate(
      ecosystem = "forest"
    ),
  
  index_mountain$summaries |>
    dplyr::mutate(
      ecosystem = "mountain"
    )
  
) |>
  dplyr::filter(
    level == "Index"
  )


readr::write_csv(
  qa_total_summary,
  here::here(
    "export",
    "adminportal",
    "qa_total_summary.csv"
  )
)


# ==============================================================================
# 38. Adminportal readiness check
# ==============================================================================
#
# This is expected to FAIL until you have received/entered:
#
#   - protocol numbers
#   - geographic reference UIDs
#   - property reference UIDs
#   - property JSON field names
#   - ecosystem reference UIDs
#
# portal_created should also be TRUE once each indicator has been created
# manually in Adminportal.

check_adminportal_ready <- function(
    portal_indicators,
    portal_properties,
    portal_geography,
    ecosystem_lookup) {
  
  
  problems <- character()
  
  
  # ---------------------------------------------------------------------------
  # Indicators not yet manually created
  # ---------------------------------------------------------------------------
  
  not_created <- portal_indicators |>
    dplyr::filter(
      !portal_created
    )
  
  
  if (nrow(not_created) > 0) {
    
    problems <- c(
      problems,
      paste0(
        nrow(not_created),
        " indicators have not been marked as created in Adminportal"
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Protocol numbers
  # ---------------------------------------------------------------------------
  
  missing_protocol <- portal_indicators |>
    dplyr::filter(
      is.na(protocol_number) |
        protocol_number == ""
    )
  
  
  if (nrow(missing_protocol) > 0) {
    
    problems <- c(
      problems,
      paste0(
        nrow(missing_protocol),
        " indicators are missing protocol numbers"
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Geography
  # ---------------------------------------------------------------------------
  
  missing_geography <- portal_geography |>
    dplyr::filter(
      is.na(
        geografiskOmradeReferanseUid
      ) |
        geografiskOmradeReferanseUid == ""
    )
  
  
  if (nrow(missing_geography) > 0) {
    
    problems <- c(
      problems,
      paste0(
        nrow(missing_geography),
        " geographic areas are missing reference UIDs"
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Properties
  # ---------------------------------------------------------------------------
  
  missing_properties <- portal_properties |>
    dplyr::filter(
      is.na(property_reference_uid) |
        property_reference_uid == "" |
        is.na(json_property_name) |
        json_property_name == ""
    )
  
  
  if (nrow(missing_properties) > 0) {
    
    problems <- c(
      problems,
      paste0(
        nrow(missing_properties),
        " ECT/property mappings are incomplete"
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Ecosystems
  # ---------------------------------------------------------------------------
  
  missing_ecosystem <- ecosystem_lookup |>
    dplyr::filter(
      is.na(ecosystem_reference_uid) |
        ecosystem_reference_uid == ""
    )
  
  
  if (nrow(missing_ecosystem) > 0) {
    
    problems <- c(
      problems,
      paste0(
        nrow(missing_ecosystem),
        " ecosystem reference UIDs are missing"
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Report
  # ---------------------------------------------------------------------------
  
  if (length(problems) > 0) {
    
    message(
      "\nAdminportal export is NOT ready:"
    )
    
    message(
      paste0(
        "  - ",
        problems,
        collapse = "\n"
      )
    )
    
    return(
      invisible(FALSE)
    )
  }
  
  
  message(
    "\nAdminportal metadata check passed."
  )
  
  
  invisible(TRUE)
}


adminportal_ready <- check_adminportal_ready(
  portal_indicators,
  portal_properties,
  portal_geography,
  ecosystem_lookup
)


# ==============================================================================
# 39. Convert data frames to JSON records
# ==============================================================================

df_to_records <- function(x) {
  
  if (!is.data.frame(x)) {
    
    stop(
      "Input must be a data frame.",
      call. = FALSE
    )
  }
  
  
  if (nrow(x) == 0L) {
    
    return(
      list()
    )
  }
  
  
  lapply(
    seq_len(nrow(x)),
    function(i) {
      
      row <- as.list(
        x[
          i,
          ,
          drop = FALSE
        ]
      )
      
      
      row <- lapply(
        row,
        function(value) {
          
          if (is.factor(value)) {
            
            value <-
              as.character(value)
          }
          
          unname(value)
        }
      )
      
      
      row
    }
  )
}


# ==============================================================================
# 40. Build complete index.json object
# ==============================================================================

build_index_object <- function(
    kode,
    datasett,
    indikator_vurderinger,
    rapport_navn,
    total_vurdering,
    egenskaps_vurderinger,
    indikator_vekting) {
  
  
  list(
    
    input = list(
      
      kode =
        df_to_records(
          kode
        ),
      
      datasett =
        df_to_records(
          datasett
        )
    ),
    
    
    output = list(
      
      indikatorVurderinger =
        df_to_records(
          indikator_vurderinger
        ),
      
      
      rapportData = list(
        
        navn =
          rapport_navn,
        
        totalVurdering =
          df_to_records(
            total_vurdering
          ),
        
        egenskapsVurderinger =
          df_to_records(
            egenskaps_vurderinger
          ),
        
        indikatorVurderinger_egenskapVurderinger_vekting =
          indikator_vekting
      )
    )
  )
}


# ==============================================================================
# 41. Build forest and mountain JSON objects
# ==============================================================================

forest_index_object <- build_index_object(
  
  kode =
    kode_forest,
  
  datasett =
    datasett_forest,
  
  indikator_vurderinger =
    indikator_vurderinger_forest,
  
  rapport_navn =
    "Økologisk tilstand – skog",
  
  total_vurdering =
    total_vurdering_forest,
  
  egenskaps_vurderinger =
    egenskaps_vurderinger_forest,
  
  indikator_vekting =
    indikator_vekting_forest
)


mountain_index_object <- build_index_object(
  
  kode =
    kode_mountain,
  
  datasett =
    datasett_mountain,
  
  indikator_vurderinger =
    indikator_vurderinger_mountain,
  
  rapport_navn =
    "Økologisk tilstand – fjell",
  
  total_vurdering =
    total_vurdering_mountain,
  
  egenskaps_vurderinger =
    egenskaps_vurderinger_mountain,
  
  indikator_vekting =
    indikator_vekting_mountain
)


# ==============================================================================
# 42. Write JSON helper
# ==============================================================================

write_adminportal_json <- function(
    index_object,
    output_dir) {
  
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  path <- file.path(
    output_dir,
    "index.json"
  )
  
  
  jsonlite::write_json(
    
    index_object,
    
    path = path,
    
    pretty = TRUE,
    
    auto_unbox = TRUE,
    
    digits = NA,
    
    na = "null",
    
    null = "null"
  )
  
  
  # Syntax test:
  # If this fails, the JSON itself is malformed.
  
  jsonlite::read_json(
    path,
    simplifyVector = FALSE
  )
  
  
  message(
    "JSON syntax OK: ",
    path
  )
  
  
  invisible(path)
}


# ==============================================================================
# 43. ONLY write production JSON once portal metadata are complete
# ==============================================================================
#
# During development you may comment out this condition if you deliberately
# want to inspect a JSON containing null placeholder IDs.
#
# For an actual upload, leave the guard in place.

if (isTRUE(adminportal_ready)) {
  
  
  forest_json_path <-
    write_adminportal_json(
      forest_index_object,
      here::here(
        "export",
        "adminportal",
        "forest"
      )
    )
  
  
  mountain_json_path <-
    write_adminportal_json(
      mountain_index_object,
      here::here(
        "export",
        "adminportal",
        "mountain"
      )
    )
  
}


# ==============================================================================
# 44. OPTIONAL: write development JSON with missing portal IDs
# ==============================================================================
#
# This is useful NOW while waiting for Adminportal references.
#
# These files are for inspection only and should NOT be uploaded.

write_adminportal_json(
  forest_index_object,
  here::here(
    "export",
    "adminportal",
    "development_forest"
  )
)


write_adminportal_json(
  mountain_index_object,
  here::here(
    "export",
    "adminportal",
    "development_mountain"
  )
)


# ==============================================================================
# 45. Optional JSON Schema validation
# ==============================================================================
#
# Use this only if:
#
#   config/index-schema.json
#
# exists and represents the current index-method schema.
#
# The Adminportal team has warned that schema validation does not necessarily
# reproduce all behaviour of the actual importer, so schema validation should
# be considered LOCAL QA, not proof that the upload will succeed.

schema_path <- here::here(
  "config",
  "index-schema.json"
)


validate_adminportal_schema <- function(
    json_path,
    schema_path) {
  
  
  if (!file.exists(schema_path)) {
    
    warning(
      "JSON schema not found: ",
      schema_path,
      call. = FALSE
    )
    
    return(
      invisible(NA)
    )
  }
  
  
  if (!requireNamespace(
    "jsonvalidate",
    quietly = TRUE
  )) {
    
    warning(
      "Install jsonvalidate to run schema validation.",
      call. = FALSE
    )
    
    return(
      invisible(NA)
    )
  }
  
  
  result <- jsonvalidate::json_validate(
    
    json =
      json_path,
    
    schema =
      schema_path,
    
    engine =
      "ajv",
    
    verbose =
      TRUE
  )
  
  
  if (isTRUE(result)) {
    
    message(
      "Schema validation passed: ",
      json_path
    )
    
  } else {
    
    message(
      "Schema validation FAILED: ",
      json_path
    )
    
    print(
      attr(
        result,
        "errors"
      )
    )
  }
  
  
  invisible(result)
}


# Example once production JSON exists:
#
# validate_adminportal_schema(
#   forest_json_path,
#   schema_path
# )
#
# validate_adminportal_schema(
#   mountain_json_path,
#   schema_path
# )


# ==============================================================================
# 46. ZIP helper
# ==============================================================================
#
# Adminportal requires the file inside the ZIP to be named exactly:
#
#     index.json
#
# and it should be at the root of the ZIP.

zip_adminportal_json <- function(
    json_dir,
    zip_path) {
  
  
  json_file <- file.path(
    json_dir,
    "index.json"
  )
  
  
  if (!file.exists(json_file)) {
    
    stop(
      "index.json not found: ",
      json_file,
      call. = FALSE
    )
  }
  
  
  if (file.exists(zip_path)) {
    
    file.remove(
      zip_path
    )
  }
  
  
  old_wd <- getwd()
  
  on.exit(
    setwd(old_wd),
    add = TRUE
  )
  
  
  setwd(
    json_dir
  )
  
  
  utils::zip(
    zipfile =
      normalizePath(
        zip_path,
        mustWork = FALSE
      ),
    files =
      "index.json"
  )
  
  
  message(
    "ZIP written: ",
    zip_path
  )
  
  
  invisible(zip_path)
}


# ==============================================================================
# 47. Create production ZIPs once ready
# ==============================================================================

if (isTRUE(adminportal_ready)) {
  
  
  forest_zip <- zip_adminportal_json(
    
    json_dir =
      here::here(
        "export",
        "adminportal",
        "forest"
      ),
    
    zip_path =
      here::here(
        "export",
        "adminportal",
        "forest_2024.zip"
      )
  )
  
  
  mountain_zip <- zip_adminportal_json(
    
    json_dir =
      here::here(
        "export",
        "adminportal",
        "mountain"
      ),
    
    zip_path =
      here::here(
        "export",
        "adminportal",
        "mountain_2024.zip"
      )
  )
  
  
  # Confirm ZIP contents.
  
  message(
    "\nForest ZIP contents:"
  )
  
  print(
    utils::unzip(
      forest_zip,
      list = TRUE
    )
  )
  
  
  message(
    "\nMountain ZIP contents:"
  )
  
  print(
    utils::unzip(
      mountain_zip,
      list = TRUE
    )
  )
}


# ==============================================================================
# 48. Final console summary
# ==============================================================================

message(
  "\n============================================================"
)

message(
  "Adminportal workflow complete."
)

message(
  "============================================================"
)


message(
  "\nObserved forest indicator assessments: ",
  nrow(indicator_estimates_forest)
)

message(
  "Observed mountain indicator assessments: ",
  nrow(indicator_estimates_mountain)
)


message(
  "\nForest ECT assessments: ",
  nrow(ect_estimates_forest)
)

message(
  "Mountain ECT assessments: ",
  nrow(ect_estimates_mountain)
)


message(
  "\nForest total assessments: ",
  nrow(total_estimates_forest)
)

message(
  "Mountain total assessments: ",
  nrow(total_estimates_mountain)
)


message(
  "\nImputed indicator-region-years used in index calculation: ",
  nrow(imputation_log)
)


if (!isTRUE(adminportal_ready)) {
  
  message(
    paste0(
      "\nThe ecological calculations are complete, but production upload ",
      "is blocked until the Adminportal lookup CSVs have been completed."
    )
  )
  
} else {
  
  message(
    "\nProduction JSON and ZIP files are ready for Adminportal testing."
  )
}

