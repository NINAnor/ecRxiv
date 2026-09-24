# create cover scale data frame
coverscale <- data.frame(
  orig = 0:6,
  cov  = c(0, 1/32, 1/8, 3/8, 0.6, 4/5, 1)
)


# prepare for bootstrapping
abun_wide <- nin_sp_ind |>
  filter(cover != 0) |> 
  mutate(cover = coverscale$cov[match(cover, coverscale$orig)]) |> 
  select(
    sp = species,
    nin_code,
    cover
  ) |> 
  distinct() |> 
  pivot_wider(
    names_from  = nin_code,
    values_from = cover,
    values_fill = 0
  )

# Create wide indicator matrix
ind_wide <- nin_sp_ind  |> 
  select(
    sp = species,
    Moisture:Light
  )  |> 
  group_by(sp)  |> 
  summarise(
    across(
      .cols = everything(), 
      .fns  = ~ first(.x),
      .names = "{.col}"
    ),
    .groups = "drop"
  )


# Extract sp, abun, ind for indBoot.freq()
sp    <- abun_wide$sp
abun  <- abun_wide[, -1]  # drop 'sp' column; remaining columns are NiN types
ind   <- ind_wide[, -1]   # drop 'sp'; remaining columns are indicators


lowland_ref_cov <- indBoot.freq(
  sp      = sp,
  abun    = abun,
  ind     = ind,
  iter    = 1000,
  obl     = 0.8,
  rat     = 1/3,
  var.abun = FALSE
)



# fixing NaNs
for (i in 1:length(lowland_ref_cov) ) {
  for (j in 1:ncol(lowland_ref_cov[[i]]) ) {
    v <- lowland_ref_cov[[i]][,j]
    v[is.nan(v)] <- NA
    lowland_ref_cov[[i]][,j] <- v
  }
}



#saveRDS(lowland_ref_cov, paste0(here::here(),"/data/cache/lowland_ref_cov.RDS"))
