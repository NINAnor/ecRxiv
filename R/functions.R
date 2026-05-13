# Functions for indicator calculation
# Absence of alien coniferous tree species

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

  base <- dplyr::if_else(
    south | north,
    stringr::str_replace(
      name,
      "(?i)[ -](so|sor|sud|south|s|nv|no|nord|north|n)$",
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
    base == "buskerud" & north ~ "BuN",
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

# Function to calculate bootstrap uncertainty
bootstrap_indicator <- function(data, n_bootstrap = 1000) {
  
  bootstrap_results <- numeric(n_bootstrap)
  
  for (i in 1:n_bootstrap) {
    # Resample with replacement
    boot_sample <- data %>%
      slice_sample(n = nrow(data), replace = TRUE)
    
    # Calculate indicator
    bootstrap_results[i] <- sum(boot_sample$indicator_continuous_weighted, na.rm = TRUE) / 
                            sum(boot_sample$au_areal, na.rm = TRUE)
  }
  
  return(list(
    mean = mean(bootstrap_results),
    se = sd(bootstrap_results),
    ci_lower = as.numeric(quantile(bootstrap_results, 0.025)),
    ci_upper = as.numeric(quantile(bootstrap_results, 0.975)),
    q1 = as.numeric(quantile(bootstrap_results, 0.25)),      # First quartile (25th percentile)
    median = as.numeric(quantile(bootstrap_results, 0.50)),  # Median (50th percentile)
    q3 = as.numeric(quantile(bootstrap_results, 0.75))       # Third quartile (75th percentile)
  ))
}

# Function to scale indicator values
scale_indicator <- function(value, x0, x100) {
  scaled <- (value - x0) / (x100 - x0)
  scaled <- pmax(0, pmin(1, scaled))  # Truncate to [0, 1]
  return(scaled)
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
  
  return(list(
    county = county_results,
    region = region_results,
    national = national_results,
    data = period_data
  ))
}

# MLFA: share of multi-layered productive forest within plots where layer class is known
# or coded as NA (NA = single-layer forest in NFI; denominator = ensiktet + tosjiktet + NA + flersjiktet).
# Expects columns mlfa_num_weighted and mlfa_den_weighted on data.
calculate_mlfa_indicator <- function(data, group_var) {
  data %>%
    dplyr::group_by({{ group_var }}) %>%
    dplyr::summarise(
      total_area = sum(.data$au_areal, na.rm = TRUE),
      indicator_value = sum(.data$mlfa_num_weighted, na.rm = TRUE) /
        sum(.data$mlfa_den_weighted, na.rm = TRUE),
      n_plots = dplyr::n(),
      .groups = "drop"
    )
}

bootstrap_mlfa_indicator <- function(data, n_bootstrap = 1000) {
  na_summary <- list(
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
    boot_sample <- data %>% dplyr::slice_sample(n = n, replace = TRUE)
    den <- sum(boot_sample$mlfa_den_weighted, na.rm = TRUE)
    bootstrap_results[i] <- if (den > 0) {
      sum(boot_sample$mlfa_num_weighted, na.rm = TRUE) / den
    } else {
      NA_real_
    }
  }

  bootstrap_results <- bootstrap_results[is.finite(bootstrap_results)]

  if (length(bootstrap_results) < 2L) {
    return(na_summary)
  }

  list(
    mean = mean(bootstrap_results),
    se = stats::sd(bootstrap_results),
    ci_lower = as.numeric(stats::quantile(bootstrap_results, 0.025)),
    ci_upper = as.numeric(stats::quantile(bootstrap_results, 0.975)),
    q1 = as.numeric(stats::quantile(bootstrap_results, 0.25)),
    median = as.numeric(stats::quantile(bootstrap_results, 0.50)),
    q3 = as.numeric(stats::quantile(bootstrap_results, 0.75))
  )
}

calculate_mlfa_period_indicators <- function(data, period_name, years) {
  period_data <- data %>% dplyr::filter(.data$sesong %in% years)

  if (nrow(period_data) == 0) {
    return(NULL)
  }

  county_to_region <- period_data %>%
    dplyr::distinct(dplyr::across(dplyr::any_of(
      c("fylke_name", "county_part", "region_code", "fylke_name_clean")
    )))

  county_results <- calculate_mlfa_indicator(period_data, fylke_name) %>%
    dplyr::left_join(county_to_region, by = "fylke_name") %>%
    dplyr::mutate(period = period_name)

  region_results <- calculate_mlfa_indicator(period_data, region_code) %>%
    dplyr::mutate(period = period_name)

  national_results <- period_data %>%
    dplyr::summarise(
      region = "National",
      indicator_value = sum(.data$mlfa_num_weighted, na.rm = TRUE) /
        sum(.data$mlfa_den_weighted, na.rm = TRUE),
      total_area = sum(.data$au_areal, na.rm = TRUE),
      n_plots = dplyr::n(),
      period = period_name,
      .groups = "drop"
    )

  list(
    county = county_results,
    region = region_results,
    national = national_results,
    data = period_data
  )
}

# Bootstrap national uncertainty for scaled-then-aggregated workflow
# Resample plots, compute county non-scaled, scale counties, aggregate scaled to national
bootstrap_national_scaled_agg <- function(
  period_data,
  x0 = 0,
  x100 = 0.33,
  county_reference = NULL,
  n_bootstrap = 1000
) {
  bootstrap_results <- numeric(n_bootstrap)

  for (i in 1:n_bootstrap) {
    boot_sample <- period_data %>%
      dplyr::slice_sample(n = nrow(period_data), replace = TRUE)

    # county non-scaled from bootstrapped plots (by cleaned county name)
    boot_county <- calculate_indicator(boot_sample, fylke_name)

    county_meta <- boot_sample %>%
      dplyr::distinct(dplyr::across(dplyr::any_of(c("fylke_name", "county_part"))))

    boot_county <- boot_county %>%
      dplyr::left_join(county_meta, by = "fylke_name")

    if (!is.null(county_reference)) {
      boot_county <- boot_county %>%
        dplyr::left_join(county_reference, by = "county_part")
    } else {
      boot_county$x100 <- x100
    }

    # scale counties
    boot_county_scaled <- boot_county %>%
      dplyr::mutate(
        scaled_value = scale_indicator(indicator_value, x0, x100)
      )

    # aggregate scaled to national (area-weighted by total_area)
    boot_national_scaled <- boot_county_scaled %>%
      dplyr::summarise(
        scaled_value = sum(scaled_value * total_area, na.rm = TRUE) /
                       sum(total_area, na.rm = TRUE)
      ) %>%
      dplyr::pull(scaled_value)

    bootstrap_results[i] <- boot_national_scaled
  }

  return(list(
    mean = mean(bootstrap_results),
    se = stats::sd(bootstrap_results),
    ci_lower = as.numeric(stats::quantile(bootstrap_results, 0.025)),
    ci_upper = as.numeric(stats::quantile(bootstrap_results, 0.975)),
    q1 = as.numeric(stats::quantile(bootstrap_results, 0.25)),      # First quartile (25th percentile)
    median = as.numeric(stats::quantile(bootstrap_results, 0.50)),  # Median (50th percentile)
    q3 = as.numeric(stats::quantile(bootstrap_results, 0.75))       # Third quartile (75th percentile)
  ))
}

# Bootstrap region uncertainty for scaled-then-aggregated workflow
# For each bootstrap: compute county non-scaled by fylke_name, scale counties,
# aggregate scaled to each region_code using total_area
bootstrap_region_scaled_agg <- function(
  period_data,
  x0 = 0,
  x100 = 0.33,
  county_reference = NULL,
  n_bootstrap = 1000
) {
  region_codes <- unique(period_data$region_code)
  # storage: matrix n_bootstrap x n_regions
  res_mat <- matrix(NA_real_, nrow = n_bootstrap, ncol = length(region_codes))
  colnames(res_mat) <- region_codes

  for (i in 1:n_bootstrap) {
    boot_sample <- period_data %>% dplyr::slice_sample(n = nrow(period_data), replace = TRUE)

    # county non-scaled by cleaned county name
    boot_county <- calculate_indicator(boot_sample, fylke_name)

    # bring region_code and total_area per county by joining a distinct mapping
    county_to_region <- boot_sample %>%
      dplyr::distinct(dplyr::across(dplyr::any_of(c("fylke_name", "county_part", "region_code"))))

    boot_county <- boot_county %>% dplyr::left_join(county_to_region, by = "fylke_name")

    if (!is.null(county_reference)) {
      boot_county <- boot_county %>%
        dplyr::left_join(county_reference, by = "county_part")
    } else {
      boot_county$x100 <- x100
    }

    # scale county values
    boot_county_scaled <- boot_county %>%
      dplyr::mutate(scaled_value = scale_indicator(indicator_value, x0, x100))

    # aggregate to region_code
    boot_region_scaled <- boot_county_scaled %>%
      dplyr::group_by(region_code) %>%
      dplyr::summarise(
        scaled_value = sum(scaled_value * total_area, na.rm = TRUE) / sum(total_area, na.rm = TRUE),
        .groups = "drop"
      )

    # record
    res_mat[i, match(boot_region_scaled$region_code, region_codes)] <- boot_region_scaled$scaled_value
  }

  # summarize per region_code
  out <- tibble::tibble(
    region_code = region_codes,
    mean = apply(res_mat, 2, mean, na.rm = TRUE),
    se = apply(res_mat, 2, stats::sd, na.rm = TRUE),
    ci_lower = as.numeric(apply(res_mat, 2, stats::quantile, probs = 0.025, na.rm = TRUE)),
    ci_upper = as.numeric(apply(res_mat, 2, stats::quantile, probs = 0.975, na.rm = TRUE)),
    q1 = as.numeric(apply(res_mat, 2, stats::quantile, probs = 0.25, na.rm = TRUE)),      # First quartile (25th percentile)
    median = as.numeric(apply(res_mat, 2, stats::quantile, probs = 0.50, na.rm = TRUE)),  # Median (50th percentile)
    q3 = as.numeric(apply(res_mat, 2, stats::quantile, probs = 0.75, na.rm = TRUE))       # Third quartile (75th percentile)
  )

  return(out)
}

# MLFA: same scaled-then-aggregated bootstrap as bootstrap_national_scaled_agg,
# but county non-scaled values use the MLFA ratio (mlfa_num / mlfa_den).
bootstrap_national_scaled_agg_mlfa <- function(
  period_data,
  x0 = 0,
  x100 = 0.30,
  county_reference = NULL,
  n_bootstrap = 1000
) {
  bootstrap_results <- numeric(n_bootstrap)

  for (i in seq_len(n_bootstrap)) {
    boot_sample <- period_data %>%
      dplyr::slice_sample(n = nrow(period_data), replace = TRUE)

    boot_county <- calculate_mlfa_indicator(boot_sample, fylke_name)

    county_meta <- boot_sample %>%
      dplyr::distinct(dplyr::across(dplyr::any_of(c("fylke_name", "county_part"))))

    boot_county <- boot_county %>%
      dplyr::left_join(county_meta, by = "fylke_name")

    if (!is.null(county_reference)) {
      boot_county <- boot_county %>%
        dplyr::left_join(county_reference, by = "county_part")
    } else {
      boot_county$x100 <- x100
    }

    boot_county_scaled <- boot_county %>%
      dplyr::mutate(
        scaled_value = scale_indicator(indicator_value, x0, x100)
      )

    boot_national_scaled <- boot_county_scaled %>%
      dplyr::summarise(
        scaled_value = sum(scaled_value * total_area, na.rm = TRUE) /
          sum(total_area, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::pull(scaled_value)

    bootstrap_results[i] <- boot_national_scaled
  }

  list(
    mean = mean(bootstrap_results, na.rm = TRUE),
    se = stats::sd(bootstrap_results, na.rm = TRUE),
    ci_lower = as.numeric(stats::quantile(bootstrap_results, 0.025, na.rm = TRUE)),
    ci_upper = as.numeric(stats::quantile(bootstrap_results, 0.975, na.rm = TRUE)),
    q1 = as.numeric(stats::quantile(bootstrap_results, 0.25, na.rm = TRUE)),
    median = as.numeric(stats::quantile(bootstrap_results, 0.50, na.rm = TRUE)),
    q3 = as.numeric(stats::quantile(bootstrap_results, 0.75, na.rm = TRUE))
  )
}

# MLFA: regional scaled-then-aggregated bootstrap (county ratio → scale → area-weight to region).
bootstrap_region_scaled_agg_mlfa <- function(
  period_data,
  x0 = 0,
  x100 = 0.30,
  county_reference = NULL,
  n_bootstrap = 1000
) {
  region_codes <- stats::na.omit(unique(period_data$region_code))
  res_mat <- matrix(NA_real_, nrow = n_bootstrap, ncol = length(region_codes))
  colnames(res_mat) <- region_codes

  for (i in seq_len(n_bootstrap)) {
    boot_sample <- period_data %>%
      dplyr::slice_sample(n = nrow(period_data), replace = TRUE)

    boot_county <- calculate_mlfa_indicator(boot_sample, fylke_name)

    county_to_region <- boot_sample %>%
      dplyr::distinct(dplyr::across(dplyr::any_of(c("fylke_name", "county_part", "region_code"))))

    boot_county <- boot_county %>%
      dplyr::left_join(county_to_region, by = "fylke_name")

    if (!is.null(county_reference)) {
      boot_county <- boot_county %>%
        dplyr::left_join(county_reference, by = "county_part")
    } else {
      boot_county$x100 <- x100
    }

    boot_county_scaled <- boot_county %>%
      dplyr::mutate(scaled_value = scale_indicator(indicator_value, x0, x100))

    boot_region_scaled <- boot_county_scaled %>%
      dplyr::group_by(region_code) %>%
      dplyr::summarise(
        scaled_value = sum(scaled_value * total_area, na.rm = TRUE) /
          sum(total_area, na.rm = TRUE),
        .groups = "drop"
      )

    res_mat[i, match(boot_region_scaled$region_code, region_codes)] <-
      boot_region_scaled$scaled_value
  }

  tibble::tibble(
    region_code = region_codes,
    mean = apply(res_mat, 2, mean, na.rm = TRUE),
    se = apply(res_mat, 2, stats::sd, na.rm = TRUE),
    ci_lower = as.numeric(apply(res_mat, 2, stats::quantile, probs = 0.025, na.rm = TRUE)),
    ci_upper = as.numeric(apply(res_mat, 2, stats::quantile, probs = 0.975, na.rm = TRUE)),
    q1 = as.numeric(apply(res_mat, 2, stats::quantile, probs = 0.25, na.rm = TRUE)),
    median = as.numeric(apply(res_mat, 2, stats::quantile, probs = 0.50, na.rm = TRUE)),
    q3 = as.numeric(apply(res_mat, 2, stats::quantile, probs = 0.75, na.rm = TRUE))
  )
}
