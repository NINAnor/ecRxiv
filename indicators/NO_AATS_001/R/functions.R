# Functions for NO_AATS_001 indicator calculation
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
      "ostfold", "oslo", "akershus", "oslo akershus", "oslo og akershus",
      "innlandet", "hedmark", "oppland",
      "buskerud", "vestfold"
    ) ~ "1",

    # Sør: Telemark + Agder
    fylke_name_clean %in% c("telemark", "agder") ~ "2",

    # Vest: Rogaland, Vestland (incl. historic Hordaland/Sogn og Fjordane names if present)
    fylke_name_clean %in% c("rogaland", "vestland", "hordaland", "sogn og fjordane") ~ "3",

    # Midt: Møre og Romsdal + Trøndelag
    # Note: Sør-Trøndelag and Nord-Trøndelag are normalized to Trøndelag before this function
    fylke_name_clean %in% c("more og romsdal", "trondelag") ~ "4",

    # Nord: Nordland, Troms, Finnmark (incl. combined Troms og Finnmark)
    fylke_name_clean %in% c(
      "nordland", "troms", "finnmark", "troms og finnmark"
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
    dplyr::distinct(fylke_name, region_code, fylke_name_clean)

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

# Bootstrap national uncertainty for scaled-then-aggregated workflow
# Resample plots, compute county non-scaled, scale counties, aggregate scaled to national
bootstrap_national_scaled_agg <- function(period_data, x0, x100, n_bootstrap = 1000) {
  bootstrap_results <- numeric(n_bootstrap)

  for (i in 1:n_bootstrap) {
    boot_sample <- period_data %>%
      dplyr::slice_sample(n = nrow(period_data), replace = TRUE)

    # county non-scaled from bootstrapped plots (by cleaned county name)
    boot_county <- calculate_indicator(boot_sample, fylke_name)

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
bootstrap_region_scaled_agg <- function(period_data, x0, x100, n_bootstrap = 1000) {
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
      dplyr::distinct(fylke_name, region_code)

    boot_county <- boot_county %>% dplyr::left_join(county_to_region, by = "fylke_name")

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
    se = apply(res_mat, 2, stats::sd, na.rm = TRUE),
    ci_lower = as.numeric(apply(res_mat, 2, stats::quantile, probs = 0.025, na.rm = TRUE)),
    ci_upper = as.numeric(apply(res_mat, 2, stats::quantile, probs = 0.975, na.rm = TRUE)),
    q1 = as.numeric(apply(res_mat, 2, stats::quantile, probs = 0.25, na.rm = TRUE)),      # First quartile (25th percentile)
    median = as.numeric(apply(res_mat, 2, stats::quantile, probs = 0.50, na.rm = TRUE)),  # Median (50th percentile)
    q3 = as.numeric(apply(res_mat, 2, stats::quantile, probs = 0.75, na.rm = TRUE))       # Third quartile (75th percentile)
  )

  return(out)
}
