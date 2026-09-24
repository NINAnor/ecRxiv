# ANO
ano_lowlands <- ano_all |>
  select(globalid, ano_flate_id, ano_punkt_id, ssb_id, aar, hovedoekosystem_250m2, hovedoekosystem_rute, hovedtype_rute, hovedtype_250m2, hovedtype_1m2, kartleggingsenhet_1m2, kartleggingsenhet_250m2, shape) |> 
  st_as_sf(sf_column_name = "shape", crs = 25833)  |> 
  mutate(
    X = st_coordinates(shape)[, 1],
    Y = st_coordinates(shape)[, 2],
    kartleggingsenhet_1m2 = word(kartleggingsenhet_1m2, 1, 1)
  ) |>
  st_drop_geometry() |> 
  mutate(globalid_chr = as.character(globalid)) %>%
  semi_join(
    ANO_species_ind %>% mutate(parent_chr = as.character(parentglobalid)),
    by = c("globalid_chr" = "parent_chr")
  ) |> 
  rename(kartleggingsenhet = kartleggingsenhet_1m2,
         nest_1 = ano_flate_id) |> 
  mutate(scale = "1m2")


# GRUK
gruk_lowlands <- GRUK_all |>
  rename(globalid = global_id) |> 
  mutate(globalid_chr = as.character(globalid)) |> 
  select(-c(rute_id, precision, UTM33_E_ne, UTM33_N_ne, UTM33_E_sw, UTM33_N_sw)) |> 
  distinct() |> 
  rename(
    X = UTM33_E,
    Y = UTM33_N, 
    aar = year) |> 
  st_drop_geometry() |> 
  semi_join(
    GRUK_species_ind  |>  mutate(parent_chr = as.character(parent_global_id)) |>  st_drop_geometry(),
    by = c("globalid_chr" = "parent_chr")
  ) |> 
  mutate(hovedtype_rute = str_sub(kartleggingsenhet_1_5000, 1, 2)) |> 
  select(globalid, globalid_chr, polygon_id, hovedtype_rute, aar, X, Y, kartleggingsenhet_1_5000, nin_kartleggingsenheter, hovedokosystem) |> 
  rename(kartleggingsenhet = kartleggingsenhet_1_5000,
         nest_1 = polygon_id) |> 
  mutate(scale = "1_5000")

# ASO

ASO_lowlands <- ASO_all |>
  rename(globalid = parent_event_id) |> 
  mutate(globalid_chr = as.character(globalid)) |> 
  distinct() |> 
  st_transform(crs = 25833) |> 
  mutate(
    X = st_coordinates(st_sfc(geometry))[, 1],
    Y = st_coordinates(st_sfc(geometry))[, 2]) |> 
  st_drop_geometry() |> 
  semi_join(
    ASO_species_ind  |>  mutate(parent_chr = as.character(parent_event_id)) |>  st_drop_geometry(),
    by = c("globalid_chr" = "parent_chr")
  ) |> 
  mutate(hovedtype_rute = str_sub(nin_grunntype, 1, 3)) |> 
  select(globalid, globalid_chr, eng_id, omradenummer_flatenummer, aso_id, hovedtype_rute, X, Y, nin_grunntype) |> 
  rename(kartleggingsenhet = nin_grunntype,
         nest_1 = omradenummer_flatenummer,
         nest_2 = eng_id)


combined_lowlands <- bind_rows(ano_lowlands, gruk_lowlands, ASO_lowlands, .id = "dataset") |> 
  mutate(dataset = case_match(
    dataset, 
    "1" ~ "ANO",
    "2" ~ "GRUK",
    "3" ~ "ASO"
  )) |> 
  # harmonise projections
  st_as_sf(coords = c("X", "Y"), remove = FALSE, crs = 25833) |> 
  # join to Norway regions
  st_join(
    regnor,
    left = TRUE,
    join = st_nearest_feature
  ) |> 
  st_drop_geometry() |> 
  filter(hovedtype_rute %in% c("T31", "T32", "T33", "T34", "V9" , "V10",
                               "T2" , "T8" , "T11", "T12", "T13", "T15", 
                               "T16", "T18", "T21", "T24", "T29", "T41", "T45"))



# combine species data

GRUK_species_ind <- GRUK_species_ind |> 
  filter(!is.na(species), !is.na(art_dekning)) |> 
  rename(nest_1 = polygon_id)

ASO_species_ind <- ASO_species_ind |> 
  filter(!is.na(species), !is.na(art_dekning)) |> 
  rename(nest_1 = omradenummer_flatenummer#,
         #nest_2 = eng_id
  )

ANO_species_ind <- ANO_species_ind |> 
  rename(nest_1 = ano_flate_id)

combined_species <-  bind_rows(ANO_species_ind |> select(-c(kartleggingsenhet_1m2)), 
                               GRUK_species_ind |> rename(parentglobalid = parent_global_id),
                               ASO_species_ind |> rename(parentglobalid = ParentGlobalID),
                               .id = "dataset") |> 
  mutate(dataset = case_match(
    dataset, 
    "1" ~ "ANO",
    "2" ~ "GRUK",
    "3" ~ "ASO"
  )) |> 
  filter(hovedtype_rute %in% c("T31", "T32", "T33", "T34", "V9" , "V10",
                               "T2" , "T8" , "T11", "T12", "T13", "T15", 
                               "T16", "T18", "T21", "T24", "T29", "T41", "T45"))
