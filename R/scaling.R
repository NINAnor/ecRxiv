# every NiN-type is represented by one 'generalisert artsliste'
# some NiN-types are represented by two such species lists
# in some cases two NiN-types are represented by the same species list


one_sided_indicators <- c("Nitrogen", "Light", "Phosphorus", "Soil_disturbance_1", "Soil_reaction_pH")
two_sided_indicators <- c("Moisture", "Soil_disturbance_2", "Grazing_mowing")


# loop over all indicators
indicators <- c("Moisture", "Soil_reaction_pH", "Light", "Nitrogen",
                "Phosphorus", "Grazing_mowing", "Soil_disturbance_1", "Soil_disturbance_2")  

levels_per_indicator <- c(
  Moisture          = 12,
  Soil_reaction_pH  = 8,
  Light             = 7,
  Nitrogen          = 9,
  Phosphorus        = 5,
  Grazing_mowing    = 8,
  Soil_disturbance_1  = 9,
  Soil_disturbance_2  = 9
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

#write_rds(lowland_ref_cov_val, paste0(here::here(),"/data/cache/lowland_ref_cov_val.RDS"))

