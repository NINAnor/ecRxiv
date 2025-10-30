# Functions for NO_AATS_001 indicator calculation
# Absence of alien coniferous tree species

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
    ci_lower = quantile(bootstrap_results, 0.025),
    ci_upper = quantile(bootstrap_results, 0.975)
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
    dplyr::distinct(region_fylke_name, region_code, region_fylke_name_clean)

  county_results <- calculate_indicator(period_data, region_fylke_name) %>%
    dplyr::left_join(county_to_region, by = "region_fylke_name") %>%
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
    boot_county <- calculate_indicator(boot_sample, region_fylke_name)

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
    ci_lower = stats::quantile(bootstrap_results, 0.025),
    ci_upper = stats::quantile(bootstrap_results, 0.975)
  ))
}

# Bootstrap region uncertainty for scaled-then-aggregated workflow
# For each bootstrap: compute county non-scaled by region_fylke_name, scale counties,
# aggregate scaled to each region_code using total_area
bootstrap_region_scaled_agg <- function(period_data, x0, x100, n_bootstrap = 1000) {
  region_codes <- unique(period_data$region_code)
  # storage: matrix n_bootstrap x n_regions
  res_mat <- matrix(NA_real_, nrow = n_bootstrap, ncol = length(region_codes))
  colnames(res_mat) <- region_codes

  for (i in 1:n_bootstrap) {
    boot_sample <- period_data %>% dplyr::slice_sample(n = nrow(period_data), replace = TRUE)

    # county non-scaled by cleaned county name
    boot_county <- calculate_indicator(boot_sample, region_fylke_name)

    # bring region_code and total_area per county by joining a distinct mapping
    county_to_region <- boot_sample %>%
      dplyr::distinct(region_fylke_name, region_code)

    boot_county <- boot_county %>% dplyr::left_join(county_to_region, by = "region_fylke_name")

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
    ci_lower = apply(res_mat, 2, stats::quantile, probs = 0.025, na.rm = TRUE),
    ci_upper = apply(res_mat, 2, stats::quantile, probs = 0.975, na.rm = TRUE)
  )

  return(out)
}
