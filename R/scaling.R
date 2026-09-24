# every NiN-type is represented by one 'generalisert artsliste'
# some NiN-types are represented by two such species lists
# in some cases two NiN-types are represented by the same species list


one_sided_indicators <- c("Nitrogen", "Light", "Phosphorus", "Soil_disturbance", "Soil_reaction_pH")
two_sided_indicators <- c("Moisture", "Soil_disturbance", "Grazing_mowing")

build_ref_for_indicator <- function(ref_cov, ind_name, n_levels, indEll_n = 2) {
  # Choose quantiles based on indicator type
  if (ind_name %in% one_sided_indicators) {
    # one-sided: 0.05 / 0.95 around median
    myQuantiles <- c(0.05, 0.5, 0.95)
  } else {
    # two-sided: 0.025 / 0.975 around median
    myQuantiles <- c(0.025, 0.5, 0.975)
  }
  
  # Matrix 
  ind_mat   <- ref_cov[[ind_name]]
  all_names <- colnames(ind_mat)
  
  # Strip a/b/c/d suffixes from column names to get base NiN IDs
  base_names <- all_names %>%
    str_remove("[abcd]$")
  
  nin_map <- tibble(
    col_index = seq_along(all_names),
    col_name  = all_names,
    base_nin  = base_names
  )
  
  nin_counts <- nin_map %>%
    count(base_nin, name = "n_cols")
  
  nin_single <- nin_counts %>%
    filter(n_cols == 1) %>%
    pull(base_nin)
  
  nin_multi <- nin_counts %>%
    filter(n_cols > 1) %>%
    pull(base_nin)
  
  # Helper to compute quantiles for one base NiN
  calc_q_for_nin <- function(base_id) {
    idx  <- nin_map %>% filter(base_nin == base_id) %>% pull(col_index)
    vals <- as.matrix(ind_mat[, idx])
    q    <- quantile(vals, probs = myQuantiles, na.rm = TRUE)
    
    tibble(
      NiN      = base_id,
      Q_low    = q[1],
      Q_med    = q[2],
      Q_high   = q[3],
      Q_low_dup  = q[1],
      Q_med_dup  = q[2],
      Q_high_dup = q[3]
    )
  }
  
  tab_single <- map_dfr(nin_single, calc_q_for_nin)
  tab_multi  <- map_dfr(nin_multi,  calc_q_for_nin)
  
  tab <- bind_rows(tab_single, tab_multi)
  
  # Take first NiN type where multiple are listed
  tab <- tab %>%
    mutate(NiN = str_replace(NiN, "(.*?C[^C]*?)C.*", "\\1")) %>%
    mutate(NiN = if_else(
      !NiN %in% c("T2-C-7", "T2-C-8"),
      gsub("C", "C-", NiN),
      NiN
    ))
  
  # Restructuring lower and higher limits
  y_low <- numeric(length = nrow(tab) * 2)
  y_low[((1:nrow(tab)) * 2) - 1] <- tab$Q_low
  y_low[((1:nrow(tab)) * 2)     ] <- tab$Q_high
  
  y_ref <- numeric(length = nrow(tab) * 2)
  y_ref[((1:nrow(tab)) * 2) - 1] <- tab$Q_low_dup
  y_ref[((1:nrow(tab)) * 2)     ] <- tab$Q_high_dup
  
  # Build indicator names
  ind_labels <- c(paste0(ind_name, "1"), paste0(ind_name, "2"))
  
  ind_ref <- data.frame(
    grunn  = rep(rep(tab$NiN, each = 2), indEll_n),
    county = rep("all", nrow(tab) * 2 * indEll_n),
    region = rep("all", nrow(tab) * 2 * indEll_n),
    Ind    = rep(ind_labels, nrow(tab) * indEll_n),
    Rv     = c(rep(tab$Q_med,     each = 2),
               rep(tab$Q_med_dup, each = 2)),
    Gv     = c(y_low, y_ref),
    maxmin = rep(c(1, n_levels), nrow(tab) * indEll_n)
  )
  
  ind_ref %>%
    as_tibble() %>%
    mutate(
      grunn = as.factor(grunn),
      Ind   = as.factor(Ind)
    )
}

# loop over all indicators
indicators <- c("Moisture", "Soil_reaction_pH", "Light", "Nitrogen",
                "Phosphorus", "Grazing_mowing", "Soil_disturbance")  

levels_per_indicator <- c(
  Moisture          = 12,
  Soil_reaction_pH  = 8,
  Light             = 7,
  Nitrogen          = 9,
  Phosphorus        = 5,
  Grazing_mowing    = 8,
  Soil_disturbance  = 9
)

lowland_ref_cov_val_list <- map(indicators, function(ind) {
  build_ref_for_indicator(
    ref_cov  = lowland_ref_cov,
    ind_name = ind,
    n_levels = levels_per_indicator[[ind]],
    indEll_n = 2
  )
})

# Combine all indicators into one table
lowland_ref_cov_val <- bind_rows(lowland_ref_cov_val_list)

lowland_ref_cov_val <- lowland_ref_cov_val |> 
  distinct() |> 
  filter(!is.na(Rv)) |> 
  mutate(grunn = str_replace_all(grunn, "--", "-"))

summary(lowland_ref_cov_val)

#saveRDS(lowland_ref_cov_val, paste0(here::here(),"/data/cache/lowland_ref_cov_val.RDS"))

