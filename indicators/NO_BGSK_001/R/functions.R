# Shared functions for NFI indicator calculation

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

# Function to calculate area-weighted indicator values
calculate_indicator <- function(data, group_var) {
  
  # Area-weighted calculation
  result <- data %>%
    group_by({{ group_var }}) %>%
    summarise(
      # Total area represented
      total_area = sum(au_areal, na.rm = TRUE),
      
      # Area-weighted indicator value
      indicator_value = sum(indicator_continuous_weighted, na.rm = TRUE) / total_area,
      
      # Number of plots
      n_plots = n(),
      
      .groups = "drop"
    )
  
  return(result)
}

# Bootstrap uncertainty; `draws` is used by ecTools::ec_upscale() workflows.
bootstrap_indicator <- function(data, n_bootstrap = 1000) {
  na_summary <- list(
    draws = numeric(0),
    mean = NA_real_,
    se = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    q1 = NA_real_,
    median = NA_real_,
    q3 = NA_real_
  )

  n <- nrow(data)
  if (n == 0L) {
    return(na_summary)
  }

  bootstrap_results <- numeric(n_bootstrap)

  for (i in seq_len(n_bootstrap)) {
    boot_sample <- data %>%
      dplyr::slice_sample(n = n, replace = TRUE)

    bootstrap_results[i] <- sum(boot_sample$indicator_continuous_weighted, na.rm = TRUE) /
      sum(boot_sample$au_areal, na.rm = TRUE)
  }

  bootstrap_results <- bootstrap_results[is.finite(bootstrap_results)]

  if (length(bootstrap_results) < 1L) {
    return(na_summary)
  }

  list(
    draws = bootstrap_results,
    mean = mean(bootstrap_results),
    se = stats::sd(bootstrap_results),
    ci_lower = as.numeric(stats::quantile(bootstrap_results, 0.025)),
    ci_upper = as.numeric(stats::quantile(bootstrap_results, 0.975)),
    q1 = as.numeric(stats::quantile(bootstrap_results, 0.25)),
    median = as.numeric(stats::quantile(bootstrap_results, 0.50)),
    q3 = as.numeric(stats::quantile(bootstrap_results, 0.75))
  )
}

# Function to calculate indicators for a specific period
calculate_period_indicators <- function(data, period_name, years) {
  period_data <- data %>% filter(sesong %in% years)
  
  if (nrow(period_data) == 0) {
    return(NULL)
  }
  
  # Calculate county indicators (attach region_code and cleaned name)
  county_to_region <- period_data %>%
    dplyr::distinct(dplyr::across(dplyr::any_of(
      c("fylke_name", "county_part", "region_code", "fylke_name_clean")
    )))

  county_results <- calculate_indicator(period_data, fylke_name) %>%
    dplyr::left_join(county_to_region, by = "fylke_name") %>%
    mutate(period = period_name)

  # Calculate region indicators (group by region_code)
  region_results <- calculate_indicator(period_data, region_code) %>%
    mutate(period = period_name)
  
  # Calculate national indicator
  national_results <- period_data %>%
    summarise(
      region = "National",
      indicator_value = sum(indicator_continuous_weighted, na.rm = TRUE) / 
                       sum(au_areal, na.rm = TRUE),
      total_area = sum(au_areal, na.rm = TRUE),
      n_plots = n(),
      period = period_name
    )
  
  list(
    county = county_results,
    region = region_results,
    national = national_results,
    data = period_data
  )
}
