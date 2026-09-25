# ================================================================
# Export NO_IDEX_001 results for Miljødirektoratet Adminportal
#
# Produces:
#   export/adminportal/NO_IDEX_001_skog.zip
#   export/adminportal/NO_IDEX_001_fjell.zip
#
# Each ZIP contains one root-level file:
#   index.json
#
# IMPORTANT
# - Forest and mountain are exported separately.
# - Only observed (non-imputed) indicator values are reported as
#   individual indicator assessments.
# - Imputed values remain part of the official composite index
#   calculations contained in the RDS objects.
# - Dataset metadata fields required by the Adminportal validator
#   are supplied explicitly.
# - Weighting is forced to an unnamed list so jsonlite writes [...]
#   rather than {...}.
# - Report navn/link/beskrivelse are supplied within each
#   totalVurdering record, as expected by Adminportal.
# ================================================================


# ================================================================
# 1. Packages
# ================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(jsonlite)
library(here)


# ================================================================
# 2. Paths
# ================================================================

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


# ================================================================
# 3. Load Adminportal helper functions
# ================================================================

source(
  here(
    "R",
    "admin_helpers.R"
  )
)


# ================================================================
# 4. Provenance
# ================================================================

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


# ================================================================
# 5. Load official index objects
# ================================================================

forest_index <- readRDS(forest_rds)
mountain_index <- readRDS(mountain_rds)

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


# ================================================================
# 6. Summarise individual indicators
# ================================================================

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
    mutate(
      
      # A deterministic single value does not have
      # an uncertainty interval.
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


# Only observed values become individual assessments.

# Only non-imputed values become individual assessments.

forest_indicators <- forest_index$indicator_draws |>
  filter(!imputed) |>
  summarise_indicators() |>
  
  # Adminportal cannot display NULL confidence limits.
  # For deterministic indicators, use the estimate as both limits.
  mutate(
    lower = if_else(is.na(lower), estimate, lower),
    upper = if_else(is.na(upper), estimate, upper)
  ) |>
  
  add_adminportal_geography() |>
  add_adminportal_period()


mountain_indicators <- mountain_index$indicator_draws |>
  filter(!imputed) |>
  summarise_indicators() |>
  add_adminportal_geography() |>
  add_adminportal_period()


# ================================================================
# 7. Extract ECT assessments
# ================================================================

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


# ================================================================
# 8. Extract total ecosystem assessments
# ================================================================

# Use hierarchical Index, NOT "Index (direct)".

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


# ================================================================
# 9. Core QA
# ================================================================

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

message("Core dimensions QA passed.")


# ================================================================
# 10. QA deterministic NO_CONN_002
# ================================================================
# 
# conn_json <- forest_indicator_json |>
#   keep(
#     ~ .x$indikatorReferanseUid == "no-conn-002"
#   )
# 
# stopifnot(
#   length(conn_json) == 6L,
#   
#   all(
#     map_chr(conn_json, "periodeStart") ==
#       "2024-01-01 00:00:00"
#   ),
#   
#   all(
#     map_chr(conn_json, "periodeSlutt") ==
#       "2024-12-31 00:00:00"
#   ),
#   
#   # Adminportal needs numeric confidence limits
#   all(
#     map_lgl(
#       conn_json,
#       ~ !is.null(.x$nedreKonfidensIntervalGrense)
#     )
#   ),
#   
#   all(
#     map_lgl(
#       conn_json,
#       ~ !is.null(.x$ovreKonfidensIntervalGrense)
#     )
#   )
# )
# 
# message("NO_CONN_002 JSON QA passed.")
# ================================================================
# 11. QA expected mountain B2 structural absence
# ================================================================

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


# ================================================================
# 12. Build individual indicator assessments
# ================================================================
forest_indicators <- forest_indicators |>
  mutate(
    indicator_id = recode(
      indicator_id,
      "NO_AATS_002" = "NO_AATS_001",
      "NO_BAER_001" = "NO_BEAR_001"
    )
  )
forest_indicator_json <- make_indicator_assessments(
  forest_indicators
)

mountain_indicator_json <- make_indicator_assessments(
  mountain_indicators
)

stopifnot(
  length(forest_indicator_json) == 220L,
  length(mountain_indicator_json) == 106L
)


# ================================================================
# 13. Build indicator -> ECT weighting
# ================================================================

forest_weight_registry <- forest_index$indicator_draws |>
  distinct(
    indicator_id,
    ect
  )
forest_weight_registry <- forest_index$indicator_draws |>
  distinct(
    indicator_id,
    ect
  ) |>
  mutate(
    indicator_id = recode(
      indicator_id,
      "NO_AATS_002" = "NO_AATS_001",
      "NO_BAER_001" = "NO_BEAR_001"
    )
  )
mountain_weight_registry <- mountain_index$indicator_draws |>
  distinct(
    indicator_id,
    ect
  )


# Every indicator must belong to exactly one ECT.

stopifnot(
  all(
    forest_weight_registry |>
      count(indicator_id) |>
      pull(n) == 1L
  ),
  
  all(
    mountain_weight_registry |>
      count(indicator_id) |>
      pull(n) == 1L
  )
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


# CRITICAL:
# pmap can leave names on the outer list.
# Adminportal expects a JSON array [...], not an object {...}.

forest_weighting <- unname(
  forest_weighting
)

mountain_weighting <- unname(
  mountain_weighting
)


# ================================================================
# 14. QA weighting
# ================================================================

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
  length(mountain_weighting) == 11L,
  is.null(names(forest_weighting)),
  is.null(names(mountain_weighting))
)

check_weighting(forest_weighting)
check_weighting(mountain_weighting)

message("Weighting QA passed.")


# ================================================================
# 15. FUHR QA
# ================================================================
# 
# fuhr_rds_check <- mountain_index$indicator_draws |>
#   filter(
#     indicator_id == "NO_FUHR_002"
#   ) |>
#   distinct(
#     indicator_id,
#     ect
#   )
# 
# stopifnot(
#   nrow(fuhr_rds_check) == 1L,
#   fuhr_rds_check$ect == "A1"
# )
# 
# fuhr_weight_check <- mountain_weight_registry |>
#   filter(
#     indicator_id == "NO_FUHR_002"
#   )
# 
# stopifnot(
#   nrow(fuhr_weight_check) == 1L,
#   fuhr_weight_check$ect == "A1"
# )
# 
# message("NO_FUHR_002 A1 QA passed.")


# ================================================================
# 16. Build ECT/property assessments
# ================================================================

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


# ================================================================
# 17. Build total ecosystem assessments
# ================================================================

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


# ================================================================
# 18. Add required report metadata to EACH totalVurdering
# ================================================================
#
# IMPORTANT:
# beskrivelse does NOT belong directly under rapportData.
#
# Adminportal expects link, navn and beskrivelse within each
# totalVurdering object.
# ================================================================

report_link <-
  "https://ninanor.github.io/ecRxiv/indicators/NO_IDEX_001/"


forest_total_json <- purrr::map(
  forest_total_json,
  function(x) {
    
    x$link <- report_link
    
    x$navn <-
      "Økologisk tilstand - skog"
    
    x$beskrivelse <- paste(
      "Økologisk tilstand for skog i Norge.",
      "Vurderingen er basert på indikatorer for økologisk tilstand",
      "som er aggregert til økologiske egenskaper og en samlet indeks."
    )
    
    x
  }
)


mountain_total_json <- purrr::map(
  mountain_total_json,
  function(x) {
    
    x$link <- report_link
    
    x$navn <-
      "Økologisk tilstand - fjell"
    
    x$beskrivelse <- paste(
      "Økologisk tilstand for fjell i Norge.",
      "Vurderingen er basert på indikatorer for økologisk tilstand",
      "som er aggregert til økologiske egenskaper og en samlet indeks."
    )
    
    x
  }
)


# QA the exact fields Adminportal complained about.

stopifnot(
  all(
    map_lgl(
      forest_total_json,
      ~ all(
        c(
          "link",
          "navn",
          "beskrivelse"
        ) %in% names(.x)
      )
    )
  ),
  
  all(
    map_lgl(
      mountain_total_json,
      ~ all(
        c(
          "link",
          "navn",
          "beskrivelse"
        ) %in% names(.x)
      )
    )
  ),
  
  all(
    map_lgl(
      forest_total_json,
      ~ nzchar(.x$beskrivelse)
    )
  ),
  
  all(
    map_lgl(
      mountain_total_json,
      ~ nzchar(.x$beskrivelse)
    )
  )
)

message("Total assessment report metadata QA passed.")


# ================================================================
# 19. Assemble report data
# ================================================================

forest_report_data <- list(
  
  navn = "Økologisk tilstand - skog",
  
  totalVurdering =
    forest_total_json,
  
  egenskapsVurderinger =
    forest_ect_json,
  
  indikatorVurderinger_egenskapVurderinger_vekting =
    forest_weighting
)


mountain_report_data <- list(
  
  navn = "Økologisk tilstand - fjell",
  
  totalVurdering =
    mountain_total_json,
  
  egenskapsVurderinger =
    mountain_ect_json,
  
  indikatorVurderinger_egenskapVurderinger_vekting =
    mountain_weighting
)


# Do NOT add:
#
# mountain_report_data$beskrivelse
# forest_report_data$beskrivelse
#
# That is not where the supplied Adminportal schema places it.


# Force the weighting arrays again at their final location.

forest_report_data$
  indikatorVurderinger_egenskapVurderinger_vekting <-
  unname(
    forest_report_data$
      indikatorVurderinger_egenskapVurderinger_vekting
  )

mountain_report_data$
  indikatorVurderinger_egenskapVurderinger_vekting <-
  unname(
    mountain_report_data$
      indikatorVurderinger_egenskapVurderinger_vekting
  )


# ================================================================
# 20. Report QA
# ================================================================

stopifnot(
  length(forest_report_data$totalVurdering) == 12L,
  length(forest_report_data$egenskapsVurderinger) == 72L,
  length(
    forest_report_data$
      indikatorVurderinger_egenskapVurderinger_vekting
  ) == 21L,
  
  length(mountain_report_data$totalVurdering) == 12L,
  length(mountain_report_data$egenskapsVurderinger) == 46L,
  length(
    mountain_report_data$
      indikatorVurderinger_egenskapVurderinger_vekting
  ) == 11L,
  
  is.null(
    names(
      forest_report_data$
        indikatorVurderinger_egenskapVurderinger_vekting
    )
  ),
  
  is.null(
    names(
      mountain_report_data$
        indikatorVurderinger_egenskapVurderinger_vekting
    )
  )
)

message("Report component QA passed.")


# ================================================================
# 21. Read indicator metadata
# ================================================================

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


# ================================================================
# 22. Resolve indicator folders
# ================================================================

source(
  file.path(
    index_dir,
    "R",
    "index_functions.R"
  )
)


folder_lookup <- indicator_catalog() |>
  select(
    indicator_id,
    folder
  ) |>
  distinct() |>
  group_by(indicator_id) |>
  slice(1L) |>
  ungroup()


forest_metadata_export <- forest_metadata |>
  left_join(
    folder_lookup,
    by = c(
      "indicatorID" = "indicator_id"
    )
  ) |>
  mutate(
    folder = coalesce(
      folder,
      indicatorID
    )
  )


mountain_metadata_export <- mountain_metadata |>
  left_join(
    folder_lookup,
    by = c(
      "indicatorID" = "indicator_id"
    )
  ) |>
  mutate(
    folder = coalesce(
      folder,
      indicatorID
    )
  )


stopifnot(
  !any(is.na(forest_metadata_export$folder)),
  !any(is.na(mountain_metadata_export$folder))
)


# ================================================================
# 23. Clean metadata text
# ================================================================

clean_adminportal_text <- function(x) {
  
  x |>
    str_replace_all("\u00a0", " ") |>
    str_squish()
}


# ================================================================
# 24. Code/method links
# ================================================================

code_base_url <-
  "https://github.com/NINAnor/ecRxiv/tree/main/indicators/"


make_code_json <- function(metadata) {
  
  metadata |>
    transmute(
      
      navn = clean_adminportal_text(
        .data$verbatimeName
      ),
      
      link = paste0(
        code_base_url,
        .data$folder,
        "/"
      )
    ) |>
    distinct() |>
    pmap(
      function(
    navn,
    link
      ) {
        
        list(
          navn = navn,
          link = link
        )
      }
    ) |>
    unname()
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


# ================================================================
# 25. Dataset links
# ================================================================
#
# Adminportal's actual validator requires all six fields below.
#
# These entries describe the indicator data supplied through
# ecRxiv for the current index assessment, rather than attempting
# to describe every underlying raw monitoring dataset.
# ================================================================

data_base_url <-
  "https://github.com/NINAnor/ecRxiv/tree/main/indicators/"


make_dataset_json <- function(metadata) {
  
  metadata |>
    transmute(
      
      navn = paste0(
        clean_adminportal_text(
          .data$verbatimeName
        ),
        " – data"
      ),
      
      link = paste0(
        data_base_url,
        .data$folder,
        "/data"
      ),
      
      kilde =
        "NINA / ecRxiv",
      
      periodeStart =
        "2019-01-01 00:00:00",
      
      periodeSlutt =
        "2024-12-31 00:00:00",
      
      type =
        "Indikatordata"
      
    ) |>
    distinct() |>
    pmap(
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
    ) |>
    unname()
}


forest_dataset_json <- make_dataset_json(
  forest_metadata_export
)

mountain_dataset_json <- make_dataset_json(
  mountain_metadata_export
)


# ================================================================
# 26. Dataset QA
# ================================================================

check_datasets <- function(x) {
  
  required <- c(
    "navn",
    "link",
    "kilde",
    "periodeStart",
    "periodeSlutt",
    "type"
  )
  
  stopifnot(
    length(x) > 0L,
    
    all(
      map_lgl(
        x,
        ~ all(
          required %in% names(.x)
        )
      )
    ),
    
    all(
      map_lgl(
        x,
        function(z) {
          
          values <- unlist(
            z[required],
            use.names = FALSE
          )
          
          all(
            !is.na(values) &
              nzchar(values)
          )
        }
      )
    )
  )
  
  invisible(TRUE)
}


check_datasets(forest_dataset_json)
check_datasets(mountain_dataset_json)

stopifnot(
  length(forest_dataset_json) == 21L,
  length(mountain_dataset_json) == 11L
)

message("Dataset metadata QA passed.")


# ================================================================
# 27. Assemble complete Adminportal objects
# ================================================================

forest_index_json <- list(
  
  input = list(
    
    kode =
      forest_code_json,
    
    datasett =
      forest_dataset_json
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
    
    kode =
      mountain_code_json,
    
    datasett =
      mountain_dataset_json
  ),
  
  output = list(
    
    indikatorVurderinger =
      mountain_indicator_json,
    
    rapportData =
      mountain_report_data
  )
)


# ================================================================
# 28. FINAL Adminportal QA
# ================================================================

stopifnot(
  
  # Top level
  identical(
    names(forest_index_json),
    c("input", "output")
  ),
  
  identical(
    names(mountain_index_json),
    c("input", "output")
  ),
  
  # Input
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
  
  # Indicators
  length(
    forest_index_json$output$
      indikatorVurderinger
  ) == 220L,
  
  length(
    mountain_index_json$output$
      indikatorVurderinger
  ) == 106L,
  
  # Total
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
  
  # ECT
  length(
    forest_index_json$output$
      rapportData$
      egenskapsVurderinger
  ) == 72L,
  
  length(
    mountain_index_json$output$
      rapportData$
      egenskapsVurderinger
  ) == 46L,
  
  # Weighting
  length(
    forest_index_json$output$
      rapportData$
      indikatorVurderinger_egenskapVurderinger_vekting
  ) == 21L,
  
  length(
    mountain_index_json$output$
      rapportData$
      indikatorVurderinger_egenskapVurderinger_vekting
  ) == 11L,
  
  # CRITICAL: weighting must become JSON arrays
  is.null(
    names(
      forest_index_json$output$
        rapportData$
        indikatorVurderinger_egenskapVurderinger_vekting
    )
  ),
  
  is.null(
    names(
      mountain_index_json$output$
        rapportData$
        indikatorVurderinger_egenskapVurderinger_vekting
    )
  ),
  
  # CRITICAL: descriptions belong in totalVurdering
  all(
    map_lgl(
      forest_index_json$output$
        rapportData$
        totalVurdering,
      ~ !is.null(.x$beskrivelse) &&
        nzchar(.x$beskrivelse)
    )
  ),
  
  all(
    map_lgl(
      mountain_index_json$output$
        rapportData$
        totalVurdering,
      ~ !is.null(.x$beskrivelse) &&
        nzchar(.x$beskrivelse)
    )
  )
)

message("FINAL Adminportal object QA passed.")


# ================================================================
# 29. Write JSON files
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

message("JSON files written.")


# ================================================================
# 30. READ THE ACTUAL JSON BACK
# ================================================================
#
# This is important: QA the actual file that will go into the ZIP,
# not merely the R object from which it was produced.
# ================================================================

forest_test <- jsonlite::fromJSON(
  forest_json_file,
  simplifyVector = FALSE
)

mountain_test <- jsonlite::fromJSON(
  mountain_json_file,
  simplifyVector = FALSE
)


# ================================================================
# 31. Critical read-back QA
# ================================================================

stopifnot(
  
  # Report names
  mountain_test$output$
    rapportData$
    navn ==
    "Økologisk tilstand - fjell",
  
  forest_test$output$
    rapportData$
    navn ==
    "Økologisk tilstand - skog",
  
  # Description exists in every totalVurdering
  all(
    map_lgl(
      mountain_test$output$
        rapportData$
        totalVurdering,
      ~ !is.null(.x$beskrivelse) &&
        nzchar(.x$beskrivelse)
    )
  ),
  
  all(
    map_lgl(
      forest_test$output$
        rapportData$
        totalVurdering,
      ~ !is.null(.x$beskrivelse) &&
        nzchar(.x$beskrivelse)
    )
  ),
  
  # Weighting survived as arrays
  is.null(
    names(
      mountain_test$output$
        rapportData$
        indikatorVurderinger_egenskapVurderinger_vekting
    )
  ),
  
  is.null(
    names(
      forest_test$output$
        rapportData$
        indikatorVurderinger_egenskapVurderinger_vekting
    )
  ),
  
  length(
    mountain_test$output$
      rapportData$
      indikatorVurderinger_egenskapVurderinger_vekting
  ) == 11L,
  
  length(
    forest_test$output$
      rapportData$
      indikatorVurderinger_egenskapVurderinger_vekting
  ) == 21L
)


message("✓ Actual JSON read-back QA passed.")
message("✓ Report descriptions present.")
message("✓ Weighting objects are JSON arrays.")


# ================================================================
# 32. Show exactly what Adminportal will receive
# ================================================================

cat(
  "\nMOUNTAIN REPORT:\n"
)

print(
  mountain_test$output$
    rapportData$
    navn
)

cat(
  "\nFIRST MOUNTAIN TOTAL ASSESSMENT:\n"
)

print(
  mountain_test$output$
    rapportData$
    totalVurdering[[1]]
)

cat(
  "\nFIRST MOUNTAIN DATASET:\n"
)

print(
  mountain_test$
    input$
    datasett[[1]]
)

cat(
  "\nMOUNTAIN WEIGHTING COUNT:\n"
)

print(
  length(
    mountain_test$output$
      rapportData$
      indikatorVurderinger_egenskapVurderinger_vekting
  )
)


# ================================================================
# 33. Check dataset URLs
# ================================================================

forest_links <- map_chr(
  forest_dataset_json,
  "link"
)

mountain_links <- map_chr(
  mountain_dataset_json,
  "link"
)

stopifnot(
  
  all(
    str_starts(
      forest_links,
      "https://github.com/NINAnor/ecRxiv/tree/main/indicators/"
    )
  ),
  
  all(
    str_starts(
      mountain_links,
      "https://github.com/NINAnor/ecRxiv/tree/main/indicators/"
    )
  ),
  
  all(
    str_ends(
      forest_links,
      "/data"
    )
  ),
  
  all(
    str_ends(
      mountain_links,
      "/data"
    )
  )
)

message("Dataset URL QA passed.")


# ================================================================
# 34. Create ZIP files
# ================================================================
#
# index.json must be at the ROOT of each ZIP.
# ================================================================

forest_zip <- file.path(
  output_dir,
  "NO_IDEX_001_skog.zip"
)

mountain_zip <- file.path(
  output_dir,
  "NO_IDEX_001_fjell.zip"
)


# Delete old ZIPs so there is no possibility of uploading a
# previous version.

if (file.exists(forest_zip)) {
  file.remove(forest_zip)
}

if (file.exists(mountain_zip)) {
  file.remove(mountain_zip)
}


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
# 35. Inspect ZIP contents
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

message("ZIP structure QA passed.")


# ================================================================
# 36. FINAL SUMMARY
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
  "  weighting records: ", length(forest_weighting), "\n",
  "  code links: ", length(forest_code_json), "\n",
  "  data links: ", length(forest_dataset_json), "\n",
  "  ZIP: ", forest_zip, "\n",
  "\n",
  "MOUNTAIN\n",
  "  indicators: ", length(mountain_indicator_json), "\n",
  "  ECT assessments: ", length(mountain_ect_json), "\n",
  "  total assessments: ", length(mountain_total_json), "\n",
  "  weighting records: ", length(mountain_weighting), "\n",
  "  code links: ", length(mountain_code_json), "\n",
  "  data links: ", length(mountain_dataset_json), "\n",
  "  ZIP: ", mountain_zip, "\n",
  "\n",
  "========================================\n"
)
