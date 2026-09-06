# ============================================================
# ecRxiv -> Adminportal JSON export workflow
# ============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tibble)
library(stringr)
library(jsonlite)
library(jsonvalidate)
library(arrow)
source("R/NO_index_functions.R")
# ------------------------------------------------------------
# 1. Helpers
# ------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}


df_to_records <- function(x) {
  
  if (!is.data.frame(x)) {
    stop("Input must be a data frame.", call. = FALSE)
  }
  
  if (nrow(x) == 0L) {
    return(list())
  }
  
  lapply(seq_len(nrow(x)), function(i) {
    
    row <- as.list(x[i, , drop = FALSE])
    
    row <- lapply(row, function(value) {
      
      if (is.factor(value)) {
        value <- as.character(value)
      }
      
      # turn length-1 vectors into scalars
      unname(value)
    })
    
    row
  })
}


format_upload_datetime <- function(year) {
  
  year <- as.integer(year)
  
  sprintf(
    "%d-01-01 00:00:00",
    year
  )
}


# ------------------------------------------------------------
# 2. Read indicator manifest
# ------------------------------------------------------------


indicator_manifest_forest <- readr::read_csv(
  "https://raw.githubusercontent.com/NINAnor/ecRxiv/refs/heads/no-index-001/data/indicators_2026.csv",
  show_col_types = FALSE
)

indicator_manifest_mountains <- readr::read_csv(
  "https://raw.githubusercontent.com/NINAnor/ecRxiv/refs/heads/no-index-001/data/indicators_mountain.csv",
  show_col_types = FALSE
)


indicator_manifest <- dplyr::bind_rows(
  indicator_manifest_forest,
  indicator_manifest_mountains
)

indicator_manifest <- indicator_manifest |>
  dplyr::mutate(
    ect = parse_ect(ECT)
  )


indicator_registry <- build_indicator_registry(
  registry_in = indicator_manifest,
  ecosystem = "forest",  # fallback only; existing ecosystem column wins
  check_remote = TRUE
)

indicator_registry

# ------------------------------------------------------------
# 3. Add Adminportal fields
# ------------------------------------------------------------
#
# These can initially be NA.
#
# property_reference_uid:
#   actual Adminportal reference ID for the ECT/property
#
# protocol_number:
#   Adminportal protocol number for each indicator
#
# portal_created:
#   TRUE once you have manually created the indicator
#
# description_public:
#   short tooltip/public description
#
# protocol_owner:
#    "NINA"
#
# pressures:
#   impact factors
#
# factsheet_url:
#   factsheet link

required_extra_columns <- list(
  
  property_reference_uid = NA_character_,
  
  protocol_number = NA_character_,
  
  portal_created = FALSE,
  
  description_public = NA_character_,
  
  protocol_owner = "NINA",
  
  pressures = NA_character_,
  
  factsheet_url = NA_character_
)


for (nm in names(required_extra_columns)) {
  
  if (!nm %in% names(indicator_manifest)) {
    
    indicator_manifest[[nm]] <-
      required_extra_columns[[nm]]
  }
}


# ------------------------------------------------------------
# 4. Check the manifest itself
# ------------------------------------------------------------

required_manifest_columns <- c(
  "ecosystem",
  "indicatorName",
  "indicatorID",
  "ECT"#,
  #"results_path",
  #"weight",
  #"has_results"
)

missing_columns <- setdiff(
  required_manifest_columns,
  names(indicator_manifest)
)

if (length(missing_columns) > 0) {
  
  stop(
    "Manifest is missing columns: ",
    paste(missing_columns, collapse = ", ")
  )
}


# Check duplicate indicator IDs

duplicate_ids <- indicator_manifest |>
  count(indicatorID, name = "n") |>
  filter(n > 1)

if (nrow(duplicate_ids) > 0) {
  
  message(
    "Note: some indicator IDs occur more than once:"
  )
  
  print(duplicate_ids)
}


# ------------------------------------------------------------
# 5. Inspect readiness
# ------------------------------------------------------------

indicator_status <- indicator_manifest |>
  mutate(
    
    # results_ready =
    #   has_results &
    #   !is.na(results_path),
    
    adminportal_ready =
      portal_created &
      !is.na(protocol_number) &
      !is.na(property_reference_uid),
    
    json_ready =
#results_ready &
      adminportal_ready
    
  ) |>
  select(
    ecosystem,
    ECT,
    indicatorID,
    indicatorName,
    #has_results,
    portal_created,
    property_reference_uid,
    protocol_number,
    #results_ready,
    adminportal_ready,
    json_ready
  )

print(indicator_status)


# ------------------------------------------------------------
# 6. Read result files
# ------------------------------------------------------------

read_indicator_results <- function(path) {
  
  if (is.na(path) || path == "") {
    return(NULL)
  }
  
  if (!file.exists(path)) {
    
    stop(
      "Results file not found: ",
      path,
      call. = FALSE
    )
  }
  
  ext <- tolower(
    tools::file_ext(path)
  )
  
  switch(
    
    ext,
    
    csv = readr::read_csv(
      path,
      show_col_types = FALSE
    ),
    
    parquet = arrow::read_parquet(
      path
    ),
    
    rds = readRDS(
      path
    ),
    
    stop(
      "Unsupported result type: ",
      ext,
      "\nFile: ",
      path
    )
  )
}


# Test that all referenced files can be read

results_manifest <- indicator_manifest |>
  filter(has_results) |>
  mutate(
    raw_results =
      map(results_path, read_indicator_results)
  )


# ------------------------------------------------------------
# 7. Inspect result structures
# ------------------------------------------------------------
#
# Very useful at this stage because formats differ.

result_structure <- results_manifest |>
  transmute(
    indicatorID,
    indicatorName,
    n_rows =
      map_int(
        raw_results,
        ~ if (is.data.frame(.x)) nrow(.x) else NA_integer_
      ),
    columns =
      map_chr(
        raw_results,
        ~ paste(names(.x), collapse = ", ")
      )
  )

print(result_structure)


# Optionally save it

write_csv(
  result_structure,
  "export/result_file_structure.csv"
)


# ============================================================
# 8. Region lookup
# ============================================================
#
# IMPORTANT:
# replace these UIDs with the actual official
# geographical area reference UIDs.
#
# Do not rely on these example values unless confirmed.

region_lookup <- tribble(
  
  ~region_name,   ~geografiskOmradeReferanseUid,
  
  "Norway",       NA_character_,
  "Norge",        NA_character_,
  "Hele Norge",   NA_character_,
  "Nord-Norge",   NA_character_,
  "Midt-Norge",   NA_character_,
  "Vestlandet",   NA_character_,
  "Sørlandet",    NA_character_,
  "Østlandet",    NA_character_
)


# Save a template that can be edited manually

write_csv(
  region_lookup,
  "config/geographic_area_lookup.csv"
)