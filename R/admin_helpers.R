# ================================================================
# Adminportal helper functions for NO_IDEX_001
# ================================================================


# Geography -------------------------------------------------------

adminportal_geography <- tibble::tribble(
  ~part,    ~geography_uid,
  "Norway", "hele-norge-2020",
  "C",      "midt-norge-2020",
  "E",      "ostlandet-2020",
  "S",      "sorlandet-2020",
  "W",      "vestlandet-2020",
  "N",      "nord-norge-2020"
)


add_adminportal_geography <- function(x) {
  
  out <- x |>
    dplyr::left_join(
      adminportal_geography,
      by = "part"
    )
  
  if (any(is.na(out$geography_uid))) {
    
    missing_parts <- out |>
      dplyr::filter(is.na(.data$geography_uid)) |>
      dplyr::distinct(.data$part) |>
      dplyr::pull(.data$part)
    
    stop(
      "Missing Adminportal geography UID for: ",
      paste(missing_parts, collapse = ", ")
    )
  }
  
  out
}


# ECT properties --------------------------------------------------

adminportal_properties <- tibble::tribble(
  ~ect, ~property_name,                  ~property_uid,
  "A1", "Fysiske egenskaper (A1)",       "fysisk-a1",
  "A2", "Kjemiske egenskaper (A2)",      "kjemisk-a2",
  "B1", "Biologisk mangfold (B1)",       "biomangfold-b1",
  "B2", "Strukturelle egenskaper (B2)",  "strukturell-b2",
  "B3", "Funksjonelle egenskaper (B3)",  "funksjonell-b3",
  "C1", "Landskapsegenskaper (C1)",      "landskap-c1"
)


add_adminportal_property <- function(x) {
  
  out <- x |>
    dplyr::left_join(
      adminportal_properties,
      by = "ect"
    )
  
  if (any(is.na(out$property_uid))) {
    
    missing_ect <- out |>
      dplyr::filter(is.na(.data$property_uid)) |>
      dplyr::distinct(.data$ect) |>
      dplyr::pull(.data$ect)
    
    stop(
      "Missing Adminportal property UID for ECT: ",
      paste(missing_ect, collapse = ", ")
    )
  }
  
  out
}


# Reporting periods -----------------------------------------------

add_adminportal_period <- function(x) {
  
  x |>
    dplyr::mutate(
      periodeStart = paste0(
        .data$year,
        "-01-01 00:00:00"
      ),
      periodeSlutt = paste0(
        .data$year,
        "-12-31 00:00:00"
      )
    )
}


# Indicator Reference IDs -----------------------------------------

add_indicator_uid <- function(x) {
  
  x |>
    dplyr::mutate(
      indicator_uid = .data$indicator_id |>
        stringr::str_to_lower() |>
        stringr::str_replace_all("_", "-")
    )
}


# Individual indicator assessment --------------------------------

make_indicator_assessment <- function(x) {
  
  out <- list(
    indikatorBeskrivelse = x$indicator_name,
    indikatorReferanseUid = x$indicator_uid,
    geografiskOmradeReferanseUid = x$geography_uid,
    tilstandsverdi = unname(x$estimate),
    periodeStart = x$periodeStart,
    periodeSlutt = x$periodeSlutt
  )
  
  if (!is.na(x$lower)) {
    out$nedreKonfidensIntervalGrense <- unname(x$lower)
  }
  
  if (!is.na(x$upper)) {
    out$ovreKonfidensIntervalGrense <- unname(x$upper)
  }
  
  out
}

make_indicator_assessments <- function(x) {
  
  x <- add_indicator_uid(x)
  
  purrr::map(
    seq_len(nrow(x)),
    function(i) {
      make_indicator_assessment(
        x[i, , drop = FALSE]
      )
    }
  )
}


# Indicator -> ECT weighting --------------------------------------
#
# NOTE:
# The supplied Adminportal schema still enumerates the previous
# property IDs. These fields use the current ECT Reference IDs
# registered in Adminportal.

make_weighting <- function(indicator_id, ect) {
  
  indicator_uid <- indicator_id |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("_", "-")
  
  out <- list(
    indikatorReferanseUid = indicator_uid,
    `fysisk-a1` = 0L,
    `kjemisk-a2` = 0L,
    `biomangfold-b1` = 0L,
    `strukturell-b2` = 0L,
    `funksjonell-b3` = 0L,
    `landskap-c1` = 0L
  )
  
  property_uid <- adminportal_properties |>
    dplyr::filter(.data$ect == !!ect) |>
    dplyr::pull(.data$property_uid)
  
  if (length(property_uid) != 1L) {
    stop(
      "Could not identify property UID for ECT ",
      ect
    )
  }
  
  out[[property_uid]] <- 1L
  
  out
}

# ECT/property assessments ----------------------------------------

make_property_assessment <- function(x) {
  
  list(
    egenskapReferanseUid = x$property_uid,
    geografiskOmradeReferanseUid = x$geography_uid,
    nedreKonfidensIntervalGrense = unname(x$lower),
    aggregertTilstandsverdi = unname(x$estimate),
    ovreKonfidensIntervalGrense = unname(x$upper),
    periodeStart = x$periodeStart,
    periodeSlutt = x$periodeSlutt,
    vekting = 1L
  )
}


make_property_assessments <- function(x) {
  
  purrr::map(
    seq_len(nrow(x)),
    function(i) {
      make_property_assessment(
        x[i, , drop = FALSE]
      )
    }
  )
}


# Total ecosystem assessments -------------------------------------

make_total_assessment <- function(x, ecosystem_uid) {
  
  list(
    okosystemReferanseUid = ecosystem_uid,
    geografiskOmradeReferanseUid = x$geography_uid,
    nedreKonfidensIntervalGrense = unname(x$lower),
    tilstandsverdi = unname(x$estimate),
    ovreKonfidensIntervalGrense = unname(x$upper),
    dato = paste0(
      x$year,
      "-12-31 00:00:00"
    )
  )
}


make_total_assessments <- function(x, ecosystem_uid) {
  
  purrr::map(
    seq_len(nrow(x)),
    function(i) {
      make_total_assessment(
        x[i, , drop = FALSE],
        ecosystem_uid = ecosystem_uid
      )
    }
  )
}