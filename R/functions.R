# Functions for indicator calculation

# Helper to merge split county names (e.g., Telemark SØ/NV -> Telemark)
# Uses hard-coded mappings for reliability
normalize_county_parts <- function(x) {
  # Hard-coded mapping for split counties that need to be merged
  county_mapping <- c(
    "Aust-Agder" = "Agder",
    "Vest-Agder" = "Agder",
    "Sør-Trøndelag" = "Trøndelag",
    "Nord-Trøndelag" = "Trøndelag"
  )
  
  # Clean up input
  x <- stringr::str_squish(x)
  
  # Apply mapping (vectorized)
  x[x %in% names(county_mapping)] <- county_mapping[x[x %in% names(county_mapping)]]
  
  # For non-matched entries, remove trailing directional tokens (SØ, NV, etc.)
  not_matched <- !x %in% names(county_mapping)
  if (any(not_matched)) {
    x[not_matched] <- stringr::str_replace_all(
      x[not_matched], 
      "(?i)[ -](SØ|SO|S\\u00D8|S\\u00F8|NV|NE|SV|SE|N|S|Ø|O|V)$", 
      ""
    ) |>
      stringr::str_squish()
  }
  
  x
}

# Map cleaned county names to region codes per specification
# Returns: "1"=Øst, "2"=Sør, "3"=Vest, "4"=Midt, "5"=Nord
assign_region_code <- function(fylke_name_clean) {
  dplyr::case_when(
    # Øst: Østfold, Oslo/Akershus, Innlandet (Hedmark + Oppland), Buskerud, Vestfold
    fylke_name_clean %in% c(
      "ostfold", "oslo og akershus",
      "hedmark", "oppland",
      "buskerud", "vestfold"
    ) ~ "1",

    # Sør: Telemark + Agder
    fylke_name_clean %in% c("telemark", "agder") ~ "2",

    # Vest: Rogaland, Vestland (incl. historic Hordaland/Sogn og Fjordane names if present)
    fylke_name_clean %in% c("rogaland", "hordaland", "sogn og fjordane") ~ "3",

    # Midt: Møre og Romsdal + Trøndelag
    # Note: Sør-Trøndelag and Nord-Trøndelag are normalized to Trøndelag before this function
    fylke_name_clean %in% c("more og romsdal", "trondelag") ~ "4",

    # Nord: Nordland, Troms, Finnmark (incl. combined Troms og Finnmark)
    fylke_name_clean %in% c(
      "nordland", "troms", "finnmark"
    ) ~ "5",

    TRUE ~ NA_character_
  )
}

# Map region codes to display names
# Inputs: code as character ("1".."5") or numeric 1..5
assign_region_name <- function(region_code) {
  code_chr <- as.character(region_code)
  dplyr::case_when(
    code_chr == "1" ~ "Øst",
    code_chr == "2" ~ "Sør",
    code_chr == "3" ~ "Vest",
    code_chr == "4" ~ "Midt",
    code_chr == "5" ~ "Nord",
    TRUE ~ NA_character_
  )
}

# Map NFI county names to BBCA county-part reference codes
assign_bbca_county_part <- function(fylke_name) {
  name <- stringi::stri_trans_general(
    stringr::str_squish(tolower(fylke_name)),
    "Latin-ASCII"
  )

  south <- stringr::str_detect(
    name,
    "[ -](so|sor|sud|south)(?![a-z])|[ -]s$"
  )
  north <- stringr::str_detect(
    name,
    "[ -](nv|no|nord|north)(?![a-z])|[ -]n$"
  )
  west <- stringr::str_detect(
    name,
    "[ -](v|vest|west)(?![a-z])|[ -]v$"
  )

  base <- dplyr::if_else(
    south | north | west,
    stringr::str_replace(
      name,
      "(?i)[ -](so|sor|sud|south|s|nv|no|nord|north|n|v|vest|west)$",
      ""
    ) |>
      stringr::str_squish(),
    name
  )

  dplyr::case_when(
    base == "ostfold" ~ "Øs",
    base == "oslo og akershus" ~ "OA",
    base == "hedmark" & south ~ "HeS",
    base == "hedmark" & north ~ "HeN",
    base == "oppland" & south ~ "OpS",
    base == "oppland" & north ~ "OpN",
    base == "buskerud" & south ~ "BuS",
    base == "buskerud" & west ~ "BuV",
    base == "vestfold" ~ "Ve",
    base == "telemark" & south ~ "TeS",
    base == "telemark" & north ~ "TeN",
    name %in% c("aust-agder", "aust agder") ~ "AA",
    name %in% c("vest-agder", "vest agder") ~ "VA",
    name %in% c("sor-trondelag", "sortrondelag") ~ "ST",
    name %in% c("nord-trondelag", "nordtrondelag") ~ "NT",
    base == "rogaland" ~ "Ro",
    base == "hordaland" ~ "Ho",
    base == "sogn og fjordane" ~ "SF",
    base == "more og romsdal" ~ "MR",
    base == "nordland" & south ~ "NoS",
    base == "nordland" & north ~ "NoN",
    base == "troms" ~ "Tr",
    base == "finnmark" ~ "Fi",
    TRUE ~ NA_character_
  )
}

# ---- AIVS (NO_AIVS_001): ditching indicator ----

# Scale area-weighted ditched share to 0-1 using unit-specific X0 (ditchable_share)
# and X100 = 0% ditched. Returns NA when ditchable_share is missing or zero.
scale_aivs_value <- function(ditched_share, ditchable_share) {
  if (!is.finite(ditchable_share) || ditchable_share <= 0) {
    return(NA_real_)
  }
  ecTools::ec_normalise(ditched_share, x0 = ditchable_share, x100 = 0, fun = "linear")
}

# Area-weighted point estimates for one geographic unit: ditched_share and
# ditchable_share (both as proportions of total assessed area), scaled indicator
# value, total area, and plot count.
summarise_aivs <- function(data) {
  total_area <- sum(data$au_areal, na.rm = TRUE)
  ditched_share <- if (total_area > 0) {
    sum(data$ditched_weighted, na.rm = TRUE) / total_area
  } else {
    NA_real_
  }
  ditchable_share <- if (total_area > 0) {
    sum(data$ditchable_area_weighted, na.rm = TRUE) / total_area
  } else {
    NA_real_
  }
  dplyr::tibble(
    total_area = total_area,
    ditched_share = ditched_share,
    ditchable_share = ditchable_share,
    scaled_value = scale_aivs_value(ditched_share, ditchable_share),
    n_plots = nrow(data)
  )
}

# Bootstrap NFI plots with replacement; each replicate returns ditched_share,
# ditchable_share, and the scaled value from that same resample (for ec_upscale).
bootstrap_aivs <- function(data, n_bootstrap = 1000) {
  na_summary <- list(
    draws = numeric(0),
    scaled_draws = numeric(0),
    ditchable_share_draws = numeric(0)
  )

  n <- nrow(data)
  if (n == 0L) {
    return(na_summary)
  }

  non_scaled_draws <- numeric(n_bootstrap)
  scaled_draws <- numeric(n_bootstrap)
  ditchable_share_draws <- numeric(n_bootstrap)

  for (i in seq_len(n_bootstrap)) {
    boot_sample <- data |>
      dplyr::slice_sample(n = n, replace = TRUE)
    total_area <- sum(boot_sample$au_areal, na.rm = TRUE)
    ditched_share <- if (total_area > 0) {
      sum(boot_sample$ditched_weighted, na.rm = TRUE) / total_area
    } else {
      NA_real_
    }
    ditchable_share <- if (total_area > 0) {
      sum(boot_sample$ditchable_area_weighted, na.rm = TRUE) / total_area
    } else {
      NA_real_
    }
    non_scaled_draws[i] <- ditched_share
    ditchable_share_draws[i] <- ditchable_share
    scaled_draws[i] <- scale_aivs_value(ditched_share, ditchable_share)
  }

  list(
    draws = non_scaled_draws,
    scaled_draws = scaled_draws,
    ditchable_share_draws = ditchable_share_draws
  )
}

# Filter to a reporting period and aggregate county, region, and national results
# via summarise_aivs(); also returns the filtered plot-level data for bootstrapping.
calculate_aivs_period <- function(data, period_name, years) {
  period_data <- data |>
    dplyr::filter(sesong %in% years)

  if (nrow(period_data) == 0) {
    return(NULL)
  }

  county_to_region <- period_data |>
    dplyr::distinct(dplyr::across(dplyr::any_of(
      c("fylke_name", "county_part", "region_code", "fylke_name_clean")
    )))

  county_results <- period_data |>
    dplyr::group_by(fylke_name) |>
    dplyr::group_modify(~ summarise_aivs(.x)) |>
    dplyr::ungroup() |>
    dplyr::left_join(county_to_region, by = "fylke_name") |>
    dplyr::mutate(period = period_name)

  region_results <- period_data |>
    dplyr::group_by(region_code) |>
    dplyr::group_modify(~ summarise_aivs(.x)) |>
    dplyr::ungroup() |>
    dplyr::mutate(period = period_name)

  national_results <- summarise_aivs(period_data) |>
    dplyr::mutate(
      region = "National",
      period = period_name
    )

  list(
    county = county_results,
    region = region_results,
    national = national_results,
    data = period_data
  )
}
