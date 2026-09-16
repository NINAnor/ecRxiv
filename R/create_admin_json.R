# ================================================================
# Export NO_IDEX_001 results for Miljødirektoratet Adminportal
#
# Uses the published NO_IDEX_001 index objects from the current
# ecRxiv repository and prepares the components required for
# Adminportal index.json files.
#
# IMPORTANT
# - Forest and mountain are exported separately.
# - Only observed (non-imputed) indicator values are reported as
#   individual indicator assessments.
# - Imputed values remain part of the official composite index
#   calculations contained in the RDS objects.
# ================================================================


# 1. Packages -----------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(jsonlite)
library(here)


# 2. Paths --------------------------------------------------------

index_dir <- here("indicators", "NO_IDEX_001")

forest_rds <- file.path(
  index_dir,
  "data",
  "forest_index_170826.rds"
)

mountain_rds <- file.path(
  index_dir,
  "data",
  "mountain_index_170826.rds"
)

output_dir <- here("export", "adminportal")

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# 3. Load Adminportal helper functions ----------------------------

source(
  here(
    "R",
    "admin_helpers.R"
  )
)


# 4. Provenance ---------------------------------------------------

git_commit <- system(
  "git rev-parse HEAD",
  intern = TRUE
)

git_branch <- system(
  "git branch --show-current",
  intern = TRUE
)

message("Branch: ", git_branch)
message("Commit: ", git_commit)


# 5. Load official index objects ---------------------------------

forest_index <- readRDS(forest_rds)
mountain_index <- readRDS(mountain_rds)


# 6. Basic object QA ----------------------------------------------

required_objects <- c(
  "indicator_draws",
  "distributions",
  "coverage",
  "summaries"
)

stopifnot(
  all(required_objects %in% names(forest_index)),
  all(required_objects %in% names(mountain_index))
)


# 7. Summarise individual indicators -----------------------------

summarise_indicators <- function(x) {
  
  x |>
    group_by(
      indicator_id,
      indicator_name,
      year,
      part
    ) |>
    summarise(
      estimate = median(value, na.rm = TRUE),
      lower = quantile(
        value,
        0.025,
        na.rm = TRUE
      ),
      upper = quantile(
        value,
        0.975,
        na.rm = TRUE
      ),
      n_draws = n(),
      n_unique = n_distinct(value),
      .groups = "drop"
    ) |>
    
    # A single deterministic value does not constitute
    # an uncertainty interval.
    mutate(
      lower = if_else(
        n_draws == 1L,
        NA_real_,
        lower
      ),
      upper = if_else(
        n_draws == 1L,
        NA_real_,
        upper
      )
    )
}


# Only non-imputed values become individual assessments.

forest_indicators <- forest_index$indicator_draws |>
  filter(!imputed) |>
  summarise_indicators() |>
  add_adminportal_geography() |>
  add_adminportal_period()

mountain_indicators <- mountain_index$indicator_draws |>
  filter(!imputed) |>
  summarise_indicators() |>
  add_adminportal_geography() |>
  add_adminportal_period()


# 8. Extract ECT assessments -------------------------------------

forest_ect <- forest_index$summaries |>
  filter(level == "ECT") |>
  transmute(
    year,
    part,
    ect = id,
    estimate = median,
    lower = q025,
    upper = q975,
    n
  ) |>
  add_adminportal_geography() |>
  add_adminportal_property() |>
  add_adminportal_period()

mountain_ect <- mountain_index$summaries |>
  filter(level == "ECT") |>
  transmute(
    year,
    part,
    ect = id,
    estimate = median,
    lower = q025,
    upper = q975,
    n
  ) |>
  add_adminportal_geography() |>
  add_adminportal_property() |>
  add_adminportal_period()


# 9. Extract ecosystem index assessments -------------------------

# Use the hierarchical index, NOT "Index (direct)".

forest_total <- forest_index$summaries |>
  filter(level == "Index") |>
  transmute(
    year,
    part,
    estimate = median,
    lower = q025,
    upper = q975,
    n
  ) |>
  add_adminportal_geography() |>
  add_adminportal_period()

mountain_total <- mountain_index$summaries |>
  filter(level == "Index") |>
  transmute(
    year,
    part,
    estimate = median,
    lower = q025,
    upper = q975,
    n
  ) |>
  add_adminportal_geography() |>
  add_adminportal_period()


# 10. QA: expected dimensions ------------------------------------

stopifnot(
  n_distinct(forest_indicators$indicator_id) == 21L,
  n_distinct(mountain_indicators$indicator_id) == 11L,
  
  nrow(forest_indicators) == 220L,
  nrow(mountain_indicators) == 106L,
  
  nrow(forest_ect) == 72L,
  nrow(mountain_ect) == 46L,
  
  nrow(forest_total) == 12L,
  nrow(mountain_total) == 12L
)

message(
  "Forest indicators: ",
  n_distinct(forest_indicators$indicator_id)
)

message(
  "Mountain indicators: ",
  n_distinct(mountain_indicators$indicator_id)
)


# 11. QA: reporting years -----------------------------------------

forest_indicators |>
  count(year) |>
  print()

mountain_indicators |>
  count(year) |>
  print()


# 12. QA: deterministic indicators -------------------------------

forest_indicators |>
  filter(n_draws == 1L) |>
  print(n = Inf)

mountain_indicators |>
  filter(n_draws == 1L) |>
  print(n = Inf)


# 13. QA: imputed values ------------------------------------------
#
# These may contribute to the composite index but must NOT occur
# as individual indicator assessments.

forest_index$indicator_draws |>
  filter(imputed) |>
  distinct(
    indicator_id,
    year,
    source_year
  ) |>
  arrange(indicator_id, year) |>
  print(n = Inf)

mountain_index$indicator_draws |>
  filter(imputed) |>
  distinct(
    indicator_id,
    year,
    source_year
  ) |>
  arrange(indicator_id, year) |>
  print(n = Inf)


# Specifically check NO_CONN_002:
# only genuine 2024 assessments should remain.

conn_check <- forest_indicators |>
  filter(indicator_id == "NO_CONN_002")

stopifnot(
  nrow(conn_check) == 6L,
  all(conn_check$year == 2024),
  all(is.na(conn_check$lower)),
  all(is.na(conn_check$upper))
)


# 14. QA: expected structural absence in mountain B2 --------------

missing_mountain_ect <- tidyr::expand_grid(
  year = c(2019, 2024),
  part = c(
    "C",
    "E",
    "N",
    "Norway",
    "S",
    "W"
  ),
  ect = c(
    "A1",
    "B1",
    "B2",
    "B3"
  )
) |>
  anti_join(
    mountain_ect,
    by = c(
      "year",
      "part",
      "ect"
    )
  )

stopifnot(
  nrow(missing_mountain_ect) == 2L,
  all(missing_mountain_ect$part == "N"),
  all(missing_mountain_ect$ect == "B2"),
  setequal(
    missing_mountain_ect$year,
    c(2019, 2024)
  )
)

message(
  "Expected mountain ECT gaps confirmed: N × B2 in 2019 and 2024."
)


# 15. QA: Adminportal property mapping ----------------------------

forest_ect |>
  distinct(
    ect,
    property_name,
    property_uid
  ) |>
  arrange(ect) |>
  print(n = Inf)

mountain_ect |>
  distinct(
    ect,
    property_name,
    property_uid
  ) |>
  arrange(ect) |>
  print(n = Inf)


# ================================================================
# Build Adminportal components
# ================================================================


# 16. Indicator assessments --------------------------------------

forest_indicator_json <- make_indicator_assessments(
  forest_indicators
)

mountain_indicator_json <- make_indicator_assessments(
  mountain_indicators
)


# QA indicator IDs

forest_indicator_ids <- forest_indicator_json |>
  map_chr("indikatorReferanseUid") |>
  unique() |>
  sort()

mountain_indicator_ids <- mountain_indicator_json |>
  map_chr("indikatorReferanseUid") |>
  unique() |>
  sort()

stopifnot(
  length(forest_indicator_ids) == 21L,
  length(mountain_indicator_ids) == 11L
)

print(forest_indicator_ids)
print(mountain_indicator_ids)


# 17. Build indicator -> ECT weighting ----------------------------
#
# Derive assignments directly from the official index objects.
# This ensures that the Adminportal weighting reflects the ECT
# assignments actually used to calculate the published index.

forest_weight_registry <- forest_index$indicator_draws |>
  distinct(
    indicator_id,
    ect
  )

mountain_weight_registry <- mountain_index$indicator_draws |>
  distinct(
    indicator_id,
    ect
  )


# Each indicator should occur in exactly one ECT.


forest_weight_counts <- forest_weight_registry |>
  count(indicator_id)

mountain_weight_counts <- mountain_weight_registry |>
  count(indicator_id)

stopifnot(
  all(forest_weight_counts$n == 1L),
  all(mountain_weight_counts$n == 1L)
)


forest_weighting <- pmap(
  forest_weight_registry,
  function(indicator_id, ect) {
    make_weighting(
      indicator_id = indicator_id,
      ect = ect
    )
  }
)

mountain_weighting <- pmap(
  mountain_weight_registry,
  function(indicator_id, ect) {
    make_weighting(
      indicator_id = indicator_id,
      ect = ect
    )
  }
)


# 18. QA weighting ------------------------------------------------

check_weighting <- function(x) {
  
  walk(
    x,
    function(z) {
      
      weights <- unlist(
        z[
          setdiff(
            names(z),
            "indikatorReferanseUid"
          )
        ],
        use.names = FALSE
      )
      
      stopifnot(
        all(weights %in% c(0L, 1L)),
        sum(weights) == 1L
      )
    }
  )
  
  invisible(TRUE)
}


stopifnot(
  length(forest_weighting) == 21L,
  length(mountain_weighting) == 11L
)

check_weighting(forest_weighting)
check_weighting(mountain_weighting)

message("Forest weighting QA passed.")
message("Mountain weighting QA passed.")


# Inspect examples

forest_weighting[[1]]
mountain_weighting[[1]]


# 19. Build ECT/property assessments ------------------------------

forest_ect_json <- make_property_assessments(
  forest_ect
)

mountain_ect_json <- make_property_assessments(
  mountain_ect
)


stopifnot(
  length(forest_ect_json) == 72L,
  length(mountain_ect_json) == 46L
)


# Inspect examples

forest_ect_json[[1]]
mountain_ect_json[[1]]


# 20. Build total ecosystem assessments --------------------------

forest_total_json <- make_total_assessments(
  forest_total,
  ecosystem_uid = "skog"
)

mountain_total_json <- make_total_assessments(
  mountain_total,
  ecosystem_uid = "fjell"
)


stopifnot(
  length(forest_total_json) == 12L,
  length(mountain_total_json) == 12L
)


# Inspect examples

forest_total_json[[1]]
mountain_total_json[[1]]

# 21. Assemble report data ----------------------------------------

forest_report_data <- list(
  navn = "Økologisk tilstand - skog",
  totalVurdering = forest_total_json,
  egenskapsVurderinger = forest_ect_json,
  indikatorVurderinger_egenskapVurderinger_vekting =
    forest_weighting
)

mountain_report_data <- list(
  navn = "Økologisk tilstand - fjell",
  totalVurdering = mountain_total_json,
  egenskapsVurderinger = mountain_ect_json,
  indikatorVurderinger_egenskapVurderinger_vekting =
    mountain_weighting
)

mountain_report_data$
  indikatorVurderinger_egenskapVurderinger_vekting <-
  unname(
    mountain_report_data$
      indikatorVurderinger_egenskapVurderinger_vekting
  )

forest_report_data$
  indikatorVurderinger_egenskapVurderinger_vekting <-
  unname(
    forest_report_data$
      indikatorVurderinger_egenskapVurderinger_vekting)

mountain_report_data$beskrivelse <-
  paste(
    "Økologisk tilstand for fjell i Norge.",
    "Vurderingen er basert på indikatorer for økologisk tilstand",
    "som er aggregert til økologiske egenskaper og en samlet indeks."
  )
forest_report_data$beskrivelse <-
  paste(
    "Økologisk tilstand for skog i Norge.",
    "Vurderingen er basert på indikatorer for økologisk tilstand",
    "som er aggregert til økologiske egenskaper og en samlet indeks."
  )

# 22. QA report components ----------------------------------------

stopifnot(
  length(forest_indicator_json) == 220L,
  length(forest_report_data$totalVurdering) == 12L,
  length(forest_report_data$egenskapsVurderinger) == 72L,
  length(
    forest_report_data$
      indikatorVurderinger_egenskapVurderinger_vekting
  ) == 21L,
  
  length(mountain_indicator_json) == 106L,
  length(mountain_report_data$totalVurdering) == 12L,
  length(mountain_report_data$egenskapsVurderinger) == 46L,
  length(
    mountain_report_data$
      indikatorVurderinger_egenskapVurderinger_vekting
  ) == 11L
)

message("All report component QA checks passed.")
forest_ect_json[[1]]
mountain_ect_json[[1]]
forest_indicator_json[[1]]

# QA: NO_CONN_002 JSON --------------------------------------------

conn_json <- forest_indicator_json |>
  keep(
    ~ .x$indikatorReferanseUid == "no-conn-002"
  )

stopifnot(
  length(conn_json) == 6L,
  
  all(
    map_chr(conn_json, "periodeStart") ==
      "2024-01-01 00:00:00"
  ),
  
  all(
    map_chr(conn_json, "periodeSlutt") ==
      "2024-12-31 00:00:00"
  ),
  
  all(
    map_lgl(
      conn_json,
      ~ !"nedreKonfidensIntervalGrense" %in% names(.x)
    )
  ),
  
  all(
    map_lgl(
      conn_json,
      ~ !"ovreKonfidensIntervalGrense" %in% names(.x)
    )
  )
)

message("NO_CONN_002 JSON QA passed.")

# ================================================================
# 23. Input metadata
# ================================================================


# 23.1 Read indicator metadata ------------------------------------

forest_metadata <- readr::read_csv(
  file.path(
    index_dir,
    "data",
    "indicators_2026.csv"
  ),
  show_col_types = FALSE
)

mountain_metadata <- readr::read_csv(
  file.path(
    index_dir,
    "data",
    "indicators_mountain.csv"
  ),
  show_col_types = FALSE
)


stopifnot(
  nrow(forest_metadata) == 21L,
  nrow(mountain_metadata) == 11L
)


# Important final check after the FUHR fix.

#stopifnot(
#  mountain_metadata$ECT[
#    mountain_metadata$indicatorID == "NO_FUHR_002"
#  ] == "A1"
#)


# ================================================================
# 24. Build input metadata
# ================================================================
#
# Adminportal requires:
#
# input
# ├── kode
# └── datasett
#
# For this export:
#
# kode
#   links to the published ecRxiv indicator documentation.
#
# datasett
#   links to the data/ directory in the ecRxiv repository used
#   for each indicator.
#
# We deliberately do NOT attempt to describe the underlying raw
# monitoring datasets here. Some are not publicly available and
# they are not the files being supplied to the index calculation.
# ================================================================


# 24.1 Clean text --------------------------------------------------

clean_adminportal_text <- function(x) {
  
  x |>
    stringr::str_replace_all("\u00a0", " ") |>
    stringr::str_squish()
}


# ================================================================
# 24.2 Resolve ecRxiv folders
# ================================================================
#
# Usually the folder name is the same as the indicator ID.
#
# There are exceptions, for example:
#
# NO_BFLY_002 -> NO_BFLY_001_002
# NO_BUMB_002 -> NO_BUMB_001_002
# NO_BAER_001 -> NO_BEAR_001
#
# The index functions already contain the authoritative catalogue,
# so use that rather than trying to reconstruct the folder names.
# ================================================================


source(
  file.path(
    index_dir,
    "R",
    "index_functions.R"
  )
)


folder_lookup <- indicator_catalog() |>
  dplyr::select(
    indicator_id,
    folder
  ) |>
  dplyr::distinct() |>
  dplyr::group_by(indicator_id) |>
  dplyr::slice(1L) |>
  dplyr::ungroup()


# Add folder to forest metadata.

forest_metadata_export <- forest_metadata |>
  dplyr::left_join(
    folder_lookup,
    by = c(
      "indicatorID" = "indicator_id"
    )
  ) |>
  dplyr::mutate(
    folder = dplyr::coalesce(
      folder,
      indicatorID
    )
  )


# Add folder to mountain metadata.

mountain_metadata_export <- mountain_metadata |>
  dplyr::left_join(
    folder_lookup,
    by = c(
      "indicatorID" = "indicator_id"
    )
  ) |>
  dplyr::mutate(
    folder = dplyr::coalesce(
      folder,
      indicatorID
    )
  )


# Inspect the mapping carefully.

forest_metadata_export |>
  dplyr::select(
    indicatorID,
    verbatimeName,
    folder
  ) |>
  print(
    n = Inf,
    width = Inf
  )


mountain_metadata_export |>
  dplyr::select(
    indicatorID,
    verbatimeName,
    folder
  ) |>
  print(
    n = Inf,
    width = Inf
  )


# Every indicator must have a folder.

stopifnot(
  !any(is.na(forest_metadata_export$folder)),
  !any(is.na(mountain_metadata_export$folder))
)


# ================================================================
# 24.3 Code/method links
# ================================================================
#
# Link each indicator to its published ecRxiv documentation.
# ================================================================


code_base_url <-
  "https://ninanor.github.io/ecRxiv/indicators/"


make_code_json <- function(metadata) {
  
  metadata |>
    dplyr::transmute(
      
      navn = clean_adminportal_text(
        .data$verbatimeName
      ),
      
      link = paste0(
        code_base_url,
        .data$folder,
        "/"
      )
      
    ) |>
    dplyr::distinct() |>
    purrr::pmap(
      function(
    navn,
    link
      ) {
        
        list(
          navn = navn,
          link = link
        )
      }
    )
}


forest_code_json <- make_code_json(
  forest_metadata_export
)

mountain_code_json <- make_code_json(
  mountain_metadata_export
)


stopifnot(
  length(forest_code_json) == 21L,
  length(mountain_code_json) == 11L
)


message(
  "Code metadata created: ",
  length(forest_code_json),
  " forest; ",
  length(mountain_code_json),
  " mountain."
)


# Inspect examples.

forest_code_json[[1]]

mountain_code_json[[1]]


# ================================================================
# 24.4 Dataset links
# ================================================================
#
# Here "datasett" means the indicator data supplied through ecRxiv.
#
# Each link therefore points to:
#
# github.com/NINAnor/ecRxiv/tree/main/indicators/<folder>/data
#
# We only provide:
#
# navn
# link
#
# The Adminportal schema permits the additional fields kilde,
# periodeStart, periodeSlutt and type, but does not require them.
#
# This avoids inventing metadata for underlying datasets that may
# not be public or may combine multiple data sources.
# ================================================================


data_base_url <-
  "https://github.com/NINAnor/ecRxiv/tree/main/indicators/"


make_dataset_json <- function(metadata) {
  
  metadata |>
    dplyr::transmute(
      
      navn = paste0(
        clean_adminportal_text(.data$verbatimeName),
        " – data"
      ),
      
      link = paste0(
        data_base_url,
        .data$folder,
        "/data"
      ),
      
      # Required by the Adminportal validator
      kilde = "NINA / ecRxiv",
      
      # Period represented by this ecological condition assessment
      periodeStart = "2019-01-01 00:00:00",
      periodeSlutt = "2024-12-31 00:00:00",
      
      # The schema accepts a string here
      type = "Indikatordata"
      
    ) |>
    dplyr::distinct() |>
    purrr::pmap(
      function(
    navn,
    link,
    kilde,
    periodeStart,
    periodeSlutt,
    type
      ) {
        
        list(
          navn = navn,
          link = link,
          kilde = kilde,
          periodeStart = periodeStart,
          periodeSlutt = periodeSlutt,
          type = type
        )
      }
    )
}

forest_dataset_json <- make_dataset_json(
  forest_metadata_export
)

mountain_dataset_json <- make_dataset_json(
  mountain_metadata_export
)


stopifnot(
  length(forest_dataset_json) == 21L,
  length(mountain_dataset_json) == 11L
)



message(
  "Dataset links created: ",
  length(forest_dataset_json),
  " forest; ",
  length(mountain_dataset_json),
  " mountain."
)


# Inspect ALL dataset links before export.

purrr::walk(
  forest_dataset_json,
  print
)

purrr::walk(
  mountain_dataset_json,
  print
)

mountain_dataset_json[[1]]
# ================================================================
# 25. Final FUHR check
# ================================================================
#
# This is worth retaining because it catches the exact problem
# we encountered earlier.
# ================================================================


# fuhr_rds_check <- mountain_index$indicator_draws |>
#   dplyr::filter(
#     indicator_id == "NO_FUHR_002"
#   ) |>
#   dplyr::distinct(
#     indicator_id,
#     ect,
#     year,
#     imputed,
#     source_year
#   ) |>
#   dplyr::arrange(year)
# 
# 
# print(
#   fuhr_rds_check,
#   n = Inf
# )
# 
# 
# stopifnot(
#   nrow(fuhr_rds_check) > 0L,
#   all(fuhr_rds_check$ect == "A1")
# )
# 
# 
# Also check the weighting object generated earlier.

# fuhr_weight_check <- mountain_weight_registry |>
#   dplyr::filter(
#     indicator_id == "NO_FUHR_002"
#   )
# 
# 
# print(fuhr_weight_check)
# 
# 
# stopifnot(
#   nrow(fuhr_weight_check) == 1L,
#   fuhr_weight_check$ect == "A1"
# )
# 
# 
# message("NO_FUHR_002 final QA passed.")
# 

# ================================================================
# 26. Assemble complete Adminportal objects
# ================================================================


forest_index_json <- list(
  
  input = list(
    
    kode = forest_code_json,
    
    datasett = forest_dataset_json
    
  ),
  
  output = list(
    
    indikatorVurderinger =
      forest_indicator_json,
    
    rapportData =
      forest_report_data
    
  )
)


mountain_index_json <- list(
  
  input = list(
    
    kode = mountain_code_json,
    
    datasett = mountain_dataset_json
    
  ),
  
  output = list(
    
    indikatorVurderinger =
      mountain_indicator_json,
    
    rapportData =
      mountain_report_data
    
  )
)



# ================================================================
# 27. Final structural QA
# ================================================================


stopifnot(
  
  identical(
    names(forest_index_json),
    c(
      "input",
      "output"
    )
  ),
  
  identical(
    names(mountain_index_json),
    c(
      "input",
      "output"
    )
  ),
  
  identical(
    names(forest_index_json$input),
    c(
      "kode",
      "datasett"
    )
  ),
  
  identical(
    names(mountain_index_json$input),
    c(
      "kode",
      "datasett"
    )
  ),
  
  all(
    c(
      "indikatorVurderinger",
      "rapportData"
    ) %in%
      names(
        forest_index_json$output
      )
  ),
  
  all(
    c(
      "indikatorVurderinger",
      "rapportData"
    ) %in%
      names(
        mountain_index_json$output
      )
  ),
  
  length(
    forest_index_json$input$kode
  ) == 21L,
  
  length(
    mountain_index_json$input$kode
  ) == 11L,
  
  length(
    forest_index_json$input$datasett
  ) == 21L,
  
  length(
    mountain_index_json$input$datasett
  ) == 11L,
  
  length(
    forest_index_json$output$
      indikatorVurderinger
  ) == 220L,
  
  length(
    mountain_index_json$output$
      indikatorVurderinger
  ) == 106L,
  
  length(
    forest_index_json$output$
      rapportData$
      totalVurdering
  ) == 12L,
  
  length(
    mountain_index_json$output$
      rapportData$
      totalVurdering
  ) == 12L,
  
  length(
    forest_index_json$output$
      rapportData$
      egenskapsVurderinger
  ) == 72L,
  
  length(
    mountain_index_json$output$
      rapportData$
      egenskapsVurderinger
  ) == 46L
  
)


message(
  "Final Adminportal object structure QA passed."
)


# ================================================================
# 28. Write JSON files
# ================================================================


forest_export_dir <- file.path(
  output_dir,
  "skog"
)

mountain_export_dir <- file.path(
  output_dir,
  "fjell"
)


dir.create(
  forest_export_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  mountain_export_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


forest_json_file <- file.path(
  forest_export_dir,
  "index.json"
)

mountain_json_file <- file.path(
  mountain_export_dir,
  "index.json"
)


jsonlite::write_json(
  forest_index_json,
  path = forest_json_file,
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null",
  digits = NA
)


jsonlite::write_json(
  mountain_index_json,
  path = mountain_json_file,
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null",
  digits = NA
)


stopifnot(
  file.exists(forest_json_file),
  file.exists(mountain_json_file)
)


message(
  "JSON files written."
)


# ================================================================
# 29. Read JSON back in
# ================================================================
#
# This catches malformed JSON before we create the ZIPs.
# ================================================================


forest_test <- jsonlite::fromJSON(
  forest_json_file,
  simplifyVector = FALSE
)

mountain_test <- jsonlite::fromJSON(
  mountain_json_file,
  simplifyVector = FALSE
)


stopifnot(
  all(
    c(
      "input",
      "output"
    ) %in%
      names(forest_test)
  ),
  
  all(
    c(
      "input",
      "output"
    ) %in%
      names(mountain_test)
  )
)


message(
  "JSON read-back QA passed."
)


# ================================================================
# 30. Check that data links look correct
# ================================================================


forest_links <- purrr::map_chr(
  forest_dataset_json,
  "link"
)

mountain_links <- purrr::map_chr(
  mountain_dataset_json,
  "link"
)


stopifnot(
  
  all(
    stringr::str_starts(
      forest_links,
      "https://github.com/NINAnor/ecRxiv/tree/main/indicators/"
    )
  ),
  
  all(
    stringr::str_starts(
      mountain_links,
      "https://github.com/NINAnor/ecRxiv/tree/main/indicators/"
    )
  ),
  
  all(
    stringr::str_ends(
      forest_links,
      "/data"
    )
  ),
  
  all(
    stringr::str_ends(
      mountain_links,
      "/data"
    )
  )
  
)


message(
  "Dataset URL QA passed."
)


# ================================================================
# 31. Create Adminportal ZIP files
# ================================================================
#
# Each ZIP must contain index.json at the ROOT of the archive.
# ================================================================


forest_zip <- file.path(
  output_dir,
  "NO_IDEX_001_skog.zip"
)

mountain_zip <- file.path(
  output_dir,
  "NO_IDEX_001_fjell.zip"
)


# Remove old versions first.

if (file.exists(forest_zip)) {
  file.remove(forest_zip)
}

if (file.exists(mountain_zip)) {
  file.remove(mountain_zip)
}


# Use working directory so that index.json is placed at
# the root of the ZIP rather than inside skog/ or fjell/.

old_wd <- getwd()


setwd(forest_export_dir)

utils::zip(
  zipfile = normalizePath(
    forest_zip,
    winslash = "/",
    mustWork = FALSE
  ),
  files = "index.json"
)


setwd(mountain_export_dir)

utils::zip(
  zipfile = normalizePath(
    mountain_zip,
    winslash = "/",
    mustWork = FALSE
  ),
  files = "index.json"
)


setwd(old_wd)


stopifnot(
  file.exists(forest_zip),
  file.exists(mountain_zip)
)


# ================================================================
# 32. Inspect ZIP contents
# ================================================================


forest_zip_contents <- unzip(
  forest_zip,
  list = TRUE
)

mountain_zip_contents <- unzip(
  mountain_zip,
  list = TRUE
)


print(forest_zip_contents)

print(mountain_zip_contents)


stopifnot(
  
  nrow(forest_zip_contents) == 1L,
  
  nrow(mountain_zip_contents) == 1L,
  
  forest_zip_contents$Name == "index.json",
  
  mountain_zip_contents$Name == "index.json"
  
)


message(
  "ZIP structure QA passed."
)


# ================================================================
# 33. Final summary
# ================================================================


cat(
  "\n",
  "========================================\n",
  "ADMINPORTAL EXPORT COMPLETE\n",
  "========================================\n",
  "\n",
  "Git branch: ", git_branch, "\n",
  "Git commit: ", git_commit, "\n",
  "\n",
  "FOREST\n",
  "  indicators: ", length(forest_indicator_json), "\n",
  "  ECT assessments: ", length(forest_ect_json), "\n",
  "  total assessments: ", length(forest_total_json), "\n",
  "  code links: ", length(forest_code_json), "\n",
  "  data links: ", length(forest_dataset_json), "\n",
  "  ZIP: ", forest_zip, "\n",
  "\n",
  "MOUNTAIN\n",
  "  indicators: ", length(mountain_indicator_json), "\n",
  "  ECT assessments: ", length(mountain_ect_json), "\n",
  "  total assessments: ", length(mountain_total_json), "\n",
  "  code links: ", length(mountain_code_json), "\n",
  "  data links: ", length(mountain_dataset_json), "\n",
  "  ZIP: ", mountain_zip, "\n",
  "\n",
  "========================================\n"
)

