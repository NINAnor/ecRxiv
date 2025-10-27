# Functions for NO_AATS_001 indicator calculation
# Absence of alien coniferous tree species

# Function to calculate area-weighted indicator values
calculate_indicator <- function(data, group_var) {
  
  # Area-weighted calculation
  result <- data %>%
    group_by({{ group_var }}) %>%
    summarise(
      # Area-weighted indicator value
      indicator_value = sum(indicator_continuous_weighted, na.rm = TRUE) / 
                       sum(au_areal, na.rm = TRUE),
      
      # Total area represented
      total_area = sum(au_areal, na.rm = TRUE),
      
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
