# create variable for distinguishing T2-C7/C8 from other T2s
combined_lowlands$hovedtype_rute_new <- as.character(
  combined_lowlands$hovedtype_rute
)

combined_lowlands <- combined_lowlands %>%
  mutate(
    hovedtype_rute_new = if_else(
      kartleggingsenhet %in% c("T2-C-7", "T2-C-8"),
      "GRUK",
      as.character(hovedtype_rute)
    )
  )

## Fit ordered beta regression model with a random effects for dataset and the spatial clustering, for the two different sides of the indicator we only do the ranef for spatial clustering (lacking enough data)
# Norway
Moisture_no <- glmmTMB(
  Moisture ~ hovedtype_rute_new + (1|dataset/nest_1),
  data = combined_lowlands,
  family = ordbeta
)

Moisture1_no <- glmmTMB(
  Moisture1 ~ hovedtype_rute_new + (1|nest_1),
  data = combined_lowlands |> filter(!is.na(Moisture1)),
  family = ordbeta
)

Moisture2_no <- glmmTMB(
  Moisture2 ~ hovedtype_rute_new + (1|nest_1),
  data = combined_lowlands |> filter(!is.na(Moisture2)),
  family = ordbeta
)

summary(Moisture_no)
summary(Moisture1_no)
summary(Moisture2_no)



# regions, no ranef for dataset due to lack of data for statistic estimation
Moisture_re <- glmmTMB(
  Moisture ~ hovedtype_rute_new * region + (1 | nest_1),
  data = combined_lowlands,
  family = ordbeta
)

Moisture1_re <- glmmTMB(
  Moisture1 ~ hovedtype_rute_new * region + (1 | nest_1),
  data = combined_lowlands[!is.na(combined_lowlands$Moisture1),],
  family = ordbeta
)

Moisture2_re <- glmmTMB(
  Moisture2 ~ hovedtype_rute_new * region + (1 | nest_1),
  data = combined_lowlands[!is.na(combined_lowlands$Moisture2),],
  family = ordbeta
)

summary(Moisture_re)
summary(Moisture1_re)
summary(Moisture2_re)


## then we calculate the global mean for Norway and the regions accounting for the share of T2-C7/C8 in ANO

# Norway
# 1. Get the proportion of each nature type based ONLY on the representative ANO data
ano_proportions <- combined_lowlands %>%
  filter(dataset == "ANO") %>%
  count(hovedtype_rute_new) %>%
  mutate(share = n / sum(n)) %>%
  pull(share) #, name = hovedtype_rute_new
# 2. Get the estimated marginal means for each nature type
grid_means <- emmeans(Moisture_no, ~ hovedtype_rute_new, type = "link")
# 3. Collapse all levels into ONE single global mean using the ANO weights
global_mean_estimate <- emmeans(grid_means, ~ 1, weights = ano_proportions)
# 4. View the final result
summary(global_mean_estimate)

# 1. Get the proportion of each nature type based ONLY on the representative ANO data
ano_proportions1 <- combined_lowlands %>%
  filter(dataset == "ANO") %>%
  filter(!is.na(Moisture1) | hovedtype_rute_new=="GRUK") %>%
  count(hovedtype_rute_new) %>%
  mutate(share = n / sum(n)) %>%
  pull(share) #, name = hovedtype_rute_new
# 2. Get the estimated marginal means for each nature type
grid_means1 <- emmeans(Moisture1_no, ~ hovedtype_rute_new, type = "link")
# 3. Collapse all levels into ONE single global mean using the ANO weights
global_mean_estimate1 <- emmeans(grid_means1, ~ 1, weights = ano_proportions1)
# 4. View the final result
summary(global_mean_estimate1)

# 1. Get the proportion of each nature type based ONLY on the representative ANO data
ano_proportions2 <- combined_lowlands %>%
  filter(dataset == "ANO") %>%
  filter(!is.na(Moisture2) | hovedtype_rute_new=="GRUK") %>%
  count(hovedtype_rute_new) %>%
  mutate(share = n / sum(n)) %>%
  pull(share) #, name = hovedtype_rute_new
# 2. Get the estimated marginal means for each nature type
grid_means2 <- emmeans(Moisture2_no, ~ hovedtype_rute_new, type = "link")
# 3. Collapse all levels into ONE single global mean using the ANO weights
global_mean_estimate2 <- emmeans(grid_means2, ~ 1, weights = ano_proportions2)
# 4. View the final result
summary(global_mean_estimate2)


### regions
## moisture model
# 1. Calculate the regional weights from the ANO data
# This creates a data frame showing the proportion of each nature type within each region
regional_weights_df <- combined_lowlands %>%
  filter(dataset == "ANO") %>%
  count(region, hovedtype_rute_new) %>%
  group_by(region) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

# there is no ANO records of T2-C7/C8 in Eastern and Southern Norway, but that is where GRUK is. So, we need to add 1 dummy observation each.
regional_weights_df <- rbind(regional_weights_df,
                             regional_weights_df |> filter(hovedtype_rute_new =="GRUK") |> mutate(region = "Eastern_Norway"),
                             regional_weights_df |> filter(hovedtype_rute_new =="GRUK") |> mutate(region = "Southern_Norway"))

regional_weights_df <- regional_weights_df %>%
  group_by(region) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

# 2. Get the grid of predictions on the link scale for EVERY combination
grid_regional <- emmeans(Moisture_re, ~ hovedtype_rute_new | region, type = "link")

# 3. Convert the emmeans object into a regular data frame
grid_df <- as.data.frame(grid_regional)

# 4. Merge with your regional ANO weights and drop non-existing (NA) combinations
NO_FUMO_001_table <- grid_df %>%
  # Filter out cells where the model couldn't calculate an estimate (non-existing combinations)
  filter(!is.na(emmean)) %>%
  # Join your previously calculated regional weights data frame
  left_join(regional_weights_df, by = c("region", "hovedtype_rute_new")) %>%
  # Re-normalize weights within each region so they sum up to 1 for the remaining existing cells
  group_by(region) %>%
  mutate(share = share / sum(share, na.rm = TRUE)) %>%
  filter(!is.na(share)) %>%
  # Calculate the weighted mean and pool the SEs on the logit scale
  summarise(
    # Weighted mean of logits
    mu_logit = sum(emmean * share),
    # Pooled standard error (square root of the sum of weighted variances)
    se_logit = sqrt(sum((SE * share)^2)),
    .groups = "drop"
  ) %>%
  # 4. Calculate 95% CIs on the logit scale and back-transform with plogis()
  mutate(
    lower_logit = mu_logit - (1.96 * se_logit),
    upper_logit = mu_logit + (1.96 * se_logit),
    global_mean = plogis(mu_logit),
    lower_CI    = plogis(lower_logit),
    upper_CI    = plogis(upper_logit)
  ) %>%
  # Keep only the final clean output
  select(region, global_mean, lower_CI, upper_CI)


## moisture1
# 1. Calculate the regional weights from the ANO data
# This creates a data frame showing the proportion of each nature type within each region
regional_weights_df <- combined_lowlands %>%
  filter(dataset == "ANO") %>%
  filter(!is.na(Moisture1) | hovedtype_rute_new=="GRUK") %>%
  count(region, hovedtype_rute_new) %>%
  group_by(region) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

# there is no ANO records of T2-C7/C8 in Eastern and Southern Norway, but that is where GRUK is. So, we need to add 1 dummy observation each.
regional_weights_df <- rbind(regional_weights_df,
                             regional_weights_df |> filter(hovedtype_rute_new =="GRUK") |> mutate(region = "Eastern_Norway"),
                             regional_weights_df |> filter(hovedtype_rute_new =="GRUK") |> mutate(region = "Southern_Norway"))

regional_weights_df <- regional_weights_df %>%
  group_by(region) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

# 2. Get the grid of predictions on the link scale for EVERY combination
grid_regional <- emmeans(Moisture1_re, ~ hovedtype_rute_new | region, type = "link")

# 3. Convert the emmeans object into a regular data frame
grid_df <- as.data.frame(grid_regional)

# 4. Merge with your regional ANO weights and drop non-existing (NA) combinations
moisture1_table <- grid_df %>%
  # Filter out cells where the model couldn't calculate an estimate (non-existing combinations)
  filter(!is.na(emmean)) %>%
  # Join your previously calculated regional weights data frame
  left_join(regional_weights_df, by = c("region", "hovedtype_rute_new")) %>%
  # Re-normalize weights within each region so they sum up to 1 for the remaining existing cells
  group_by(region) %>%
  mutate(share = share / sum(share, na.rm = TRUE)) %>%
  filter(!is.na(share)) %>%
  # Calculate the weighted mean and pool the SEs on the logit scale
  summarise(
    # Weighted mean of logits
    mu_logit = sum(emmean * share),
    # Pooled standard error (square root of the sum of weighted variances)
    se_logit = sqrt(sum((SE * share)^2)),
    .groups = "drop"
  ) %>%
  # 4. Calculate 95% CIs on the logit scale and back-transform with plogis()
  mutate(
    lower_logit = mu_logit - (1.96 * se_logit),
    upper_logit = mu_logit + (1.96 * se_logit),
    global_mean = plogis(mu_logit),
    lower_CI    = plogis(lower_logit),
    upper_CI    = plogis(upper_logit)
  ) %>%
  # Keep only the final clean output
  select(region, global_mean, lower_CI, upper_CI)

## moisture2
# 1. Calculate the regional weights from the ANO data
# This creates a data frame showing the proportion of each nature type within each region
regional_weights_df <- combined_lowlands %>%
  filter(dataset == "ANO") %>%
  filter(!is.na(Moisture2) | hovedtype_rute_new=="GRUK") %>%
  count(region, hovedtype_rute_new) %>%
  group_by(region) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

# there is no ANO records of T2-C7/C8 in Eastern and Southern Norway, but that is where GRUK is. So, we need to add 1 dummy observation each.
regional_weights_df <- rbind(regional_weights_df,
                             regional_weights_df |> filter(hovedtype_rute_new =="GRUK") |> mutate(region = "Eastern_Norway"),
                             regional_weights_df |> filter(hovedtype_rute_new =="GRUK") |> mutate(region = "Southern_Norway"))

regional_weights_df <- regional_weights_df %>%
  group_by(region) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

# 2. Get the grid of predictions on the link scale for EVERY combination
grid_regional <- emmeans(Moisture2_re, ~ hovedtype_rute_new | region, type = "link")

# 3. Convert the emmeans object into a regular data frame
grid_df <- as.data.frame(grid_regional)

# 4. Merge with your regional ANO weights and drop non-existing (NA) combinations
moisture2_table <- grid_df %>%
  # Filter out cells where the model couldn't calculate an estimate (non-existing combinations)
  filter(!is.na(emmean)) %>%
  # Join your previously calculated regional weights data frame
  left_join(regional_weights_df, by = c("region", "hovedtype_rute_new")) %>%
  # Re-normalize weights within each region so they sum up to 1 for the remaining existing cells
  group_by(region) %>%
  mutate(share = share / sum(share, na.rm = TRUE)) %>%
  filter(!is.na(share)) %>%
  # Calculate the weighted mean and pool the SEs on the logit scale
  summarise(
    # Weighted mean of logits
    mu_logit = sum(emmean * share),
    # Pooled standard error (square root of the sum of weighted variances)
    se_logit = sqrt(sum((SE * share)^2)),
    .groups = "drop"
  ) %>%
  # 4. Calculate 95% CIs on the logit scale and back-transform with plogis()
  mutate(
    lower_logit = mu_logit - (1.96 * se_logit),
    upper_logit = mu_logit + (1.96 * se_logit),
    global_mean = plogis(mu_logit),
    lower_CI    = plogis(lower_logit),
    upper_CI    = plogis(upper_logit)
  ) %>%
  # Keep only the final clean output
  select(region, global_mean, lower_CI, upper_CI)


NO_FUMO_001_table <- rbind(NO_FUMO_001_table,
                           c("Norway",
                             plogis(summary(global_mean_estimate)$emmean),
                             plogis(summary(global_mean_estimate)$emmean + 1.96 * summary(global_mean_estimate)$SE),
                             plogis(summary(global_mean_estimate)$emmean - 1.96 * summary(global_mean_estimate)$SE)
                           )
)


NO_FUMO_001_table$upper_indicator_value = c(
  plogis(moisture2_table$global_mean),
  plogis(summary(global_mean_estimate2)$emmean)
)
NO_FUMO_001_table$n_upper_indicator_value = c(
  as.vector(table(Moisture2_re$frame$region)),
  summary(Moisture2_no)$nobs
)
NO_FUMO_001_table$lower_indicator_value = c(
  plogis(moisture1_table$global_mean),
  plogis(summary(global_mean_estimate1)$emmean)
)
NO_FUMO_001_table$n_lower_indicator_value = c(
  as.vector(table(Moisture1_re$frame$region)),
  summary(Moisture1_no)$nobs
)