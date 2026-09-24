#load data

#ind_tyler <- readRDS("P:/41201785_okologisk_tilstand_2022_2023/data/functional plant indicators/ind.Tyler.RDS")
load(paste0(here(), "/data/NiN/Eco_State.RData"))
# str(Eco_State)

# for GRUK
natopen_NiN_ref <- read_rds(paste0(here(), "/data/NiN/natopen_NiN_ref.RDS"))
natopen_NiN_species <- read_rds(paste0(here(), "/data/NiN/natopen_NiN_ref_spInfo.RDS"))



# 2.3.1 data handling NiN
head(natopen_NiN_ref)
head(natopen_NiN_species)

colnames(natopen_NiN_ref)[1] <- "species"
head(natopen_NiN_ref)

natopen_NiN_ref <- merge(natopen_NiN_ref, natopen_NiN_species[,c(1,4)], by.x="species", by.y="ScientificName", all.x=T)
unique(natopen_NiN_ref[is.na(natopen_NiN_ref$Phylum),'species']) # Pucinella does not exist in ind.dat, so we don't care
# we're only interested in vascular plants and ferns, which we have indicators on
unique(natopen_NiN_ref$Phylum)
natopen_NiN_ref <- natopen_NiN_ref %>%
  filter(Phylum %in% c("Magnoliophyta","Pteridophyta"))
unique(natopen_NiN_ref$Phylum)



# only genus and species name
natopen_NiN_ref$sp.orig <- natopen_NiN_ref$species
natopen_NiN_ref$species <- word(natopen_NiN_ref$species, 1,2)
natopen_NiN_ref <- natopen_NiN_ref[!is.na(natopen_NiN_ref$species),]

natopen_NiN_ref <- natopen_NiN_ref |> 
  mutate(species = str_to_sentence(species)
         # if needed later: filter out groups, e.g. trees
         # |> filter(spgr != "a1a")
  ) |>
  select(-sp.orig) |> 
  pivot_longer(
    cols = -c(species, Phylum),     # columns to pivot
    names_to   = "nin_id",                                   # new column for former column names
    values_to  = "cover"                                     # new column for values
  ) |> 
  filter(cover != 0)                                         # filter out 0 cover 


natopen_NiN_ref <- natopen_NiN_ref %>%
  mutate(species = clean_species_with_patterns(species, species_dict_pattern)) |> 
  group_by(species, nin_id) |> 
  summarise(cover = max(cover),           # summing cover of species which have ended up appearing several times per nature type
            .groups = "drop"
  )

natop_nin_prepared_wfo <- natopen_NiN_ref |> distinct(species)

# prepare dataset for WFO matching
natop_nin_prepared_wfo <- WFO.prepare(natop_nin_prepared_wfo$species)


# reconnect subspecies with corresponding species from authorship column
natop_nin_prepared <- natop_nin_prepared_wfo |>
  rename(clean_string = spec.name) |> 
  tibble() |> 
  mutate(
    clean_string = clean_string |>
      str_replace_all("\\bsect\\b\\.?", " ") |>
      str_squish()
  ) |> 
  # unique values in spec.full and clean_string only
  distinct(spec.full, clean_string)



# standardise names to the WFO backbone
natop_nin_sp_matched <- WFO.match(spec.data = natop_nin_prepared$clean_string,
                                  WFO.data = wfo_backbone,
                                  Fuzzy = 0.15,
                                  Fuzzy.max = 50,
                                  Fuzzy.one = FALSE)




# finalise accepted name column according to latest taxonomical nomenclature
natop_nin_sp_clean <- natop_nin_sp_matched |>
  # make copy of the original species string
  rename(clean_string = spec.name.ORIG) |>
  group_by(clean_string) |>
  summarise(
    # 1. Flag multiple scientificName suggestions
    flag_multiple_suggestions = n_distinct(scientificName) > 1,
    
    # 2. Candidate accepted_name from Old.name when possible
    accepted_name = case_when(
      any(New.accepted == TRUE & Old.name != "") ~ 
        # take one Old.name where New.accepted == TRUE and Old.name non-empty
        Old.name[New.accepted == TRUE & Old.name != ""][1],
      TRUE ~ 
        # otherwise fall back to (one) scientificName
        scientificName[1]
    ),
    
    # 3. Where did accepted_name come from?
    accepted_from = case_when(
      any(New.accepted == TRUE & Old.name != "") ~ "Old.name",
      TRUE ~ "scientificName"
    ),
    .groups = "drop"
  )


# check the species that change name where many options were available
natop_nin_sp_clean |> filter(clean_string != accepted_name) #|> view()
natop_nin_sp_clean |> filter(flag_multiple_suggestions == TRUE, clean_string != accepted_name) #|> view()


# bind new species names onto indicator dataset
natop_nin_species_clean <- left_join(natop_nin_prepared, natop_nin_sp_clean, by = "clean_string") |> 
  full_join(natopen_NiN_ref, by = join_by(spec.full == species)) |> 
  # filter out sect. species and subspecies
  #filter(!grepl("subsp.", scientific_name_original)) |> 
  distinct()

natop_nin_species_clean |> filter(is.na(accepted_name))


# Merge with indicator values
natop_nin_sp_ind <- natop_nin_species_clean |>
  select(nin_id, species = accepted_name, cover) |> 
  left_join(tyler_indicators,
            by = join_by(species)) |> 
  tibble() |>
  filter(grepl("T2-C-7", nin_id) | grepl("T2-C-8", nin_id) | grepl("T8", nin_id) | grepl("T11", nin_id) | grepl("T12", nin_id) | grepl("T13", nin_id) | grepl("T15", nin_id) | grepl("T16", nin_id) | grepl("T18", nin_id) | grepl("T21", nin_id) | grepl("T24", nin_id) | grepl("T29", nin_id)) |> 
  mutate(nin_id = sub("_.*", "", nin_id),
         nin_id = str_remove(nin_id, "-Bratli21")) |> 
  group_by(species, nin_id, Grazing_mowing) |> 
  summarise(cover = max(cover),           # summing cover of species which have ended up appearing several times per nature type
            .groups = "drop"
  )



# double check species matching
natop_nin_sp_ind |> filter(is.na(species))  
natop_nin_sp_ind |> filter(is.na(Grazing_mowing)) |>  distinct(species)




# 2.3.2 reference data - data handling

### Inspect Eco_State structure 

str(Eco_State)

# species list, env data, abundance data (same as your checks)
Eco_State$Concept_Data$Species$Species_List$species
t(Eco_State$Concept_Data$Env$Env_Data)
t(Eco_State$Concept_Data$Species$Species_Data)

# Transpose & prepare species abundance data

NiN_sp <- Eco_State$Concept_Data$Species$Species_Data |>
  t() |> 
  as_tibble()

NiN_sp <- NiN_sp |>
  mutate(
    species_original = as_factor(as.vector(Eco_State$Concept_Data$Species$Species_List$species)),
    species_group = as_factor(as.vector(Eco_State$Concept_Data$Species$Species_List$art.code)),
    # only genus + species
    species = word(species_original, 1, 2),
    species = str_to_sentence(species)
    # if needed later: filter out groups, e.g. trees
    # |> filter(spgr != "a1a")
  ) |>
  select(-species_original) |> 
  group_by(species, species_group) |> 
  summarise(
    across(
      where(is.numeric),  # all numeric columns (e.g. abundances)
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |> 
  pivot_longer(
    cols = -c(species, species_group),     # columns to pivot
    names_to   = "nin_id",                                   # new column for former column names
    values_to  = "cover"                                     # new column for values
  ) |> 
  filter(!species_group %in% c("a2m", "a2lb", "a2l")) |>     # filter out moss and lichen species 
  filter(cover != 0)                                         # filter out 0 cover 



# 2.3.3 Environment data 

NiN_env <- Eco_State$Concept_Data$Env$Env_Data

NiN_sp <- NiN_sp |> 
  left_join(NiN_env, 
            by = join_by(nin_id == ID)) |> 
  filter(Nature_Type %in% c("Semi_Natural ", "Coastal_Heath")) |>   # filter for the desired nature type
  mutate(nin_code = case_match(
    nin_id,
    "CH01" ~ "T34-C1",
    "CH02" ~ "T34-C2a",
    "CH03" ~ "T34-C2b",
    "CH04" ~ "T34-C2c",
    "CH06" ~ "T34-C3",
    "CH07" ~ "T34-C4a",
    "CH08" ~ "T34-C4b",
    "CH09" ~ "T34-C4c",
    #"CH10" ~ "T34-C4d",
    #"CH11" ~ "T34-C5a",
    "CH12" ~ "T34-C5b",
    #"CH13" ~ "T34-C5c",
    #"CH14" ~ "T34-C6a",
    "CH15" ~ "T34-C6b",
    #"CH16" ~ "T34-C6c",
    "SN08" ~ "T32-C1C2",
    "SN09" ~ "T32-C3C4",
    "SN10" ~ "T32-C5C20a",
    "SN11" ~ "T32-C7C8",
    "SN12" ~ "T32-C5C20b",
    "SN13" ~ "T32-C9a",
    "SN14" ~ "T32-C9b",
    "SN15" ~ "T32-C15",
    "SN16" ~ "T32-C21C6a",
    "SN17" ~ "T32-C21C6b",
    "SN18" ~ "T32-C10a",
    "SN19" ~ "T32-C10b",
    "SN20" ~ "T32-C16",
    "SN21" ~ "T41a",
    "SN22" ~ "T41b",
    "SN23" ~ "T45-C1C2",
    "SN24" ~ "T45-C3",
    "SN25" ~ "V10-C1C2",
    "SN26" ~ "V10-C3",
    .default = NA_character_
  )
  ) |>
  filter(!is.na(nin_code))


NiN_sp <- NiN_sp |> 
  mutate(scientific_name = clean_species_with_patterns(species, species_dict_pattern),
         species = str_replace(species, "Hierochlo.? hirta", "Hierochloë hirta"),
         species = str_replace_all(species, "spp.", "sp.")
         # and similar for other bad names
  ) |> 
  group_by(species, species_group, nin_id, Nature_Type, Sub_Type, nin_code) |> 
  summarise(cover = max(cover),           # summing cover of species which have ended up appearing several times per nature type
            .groups = "drop"
  )


nin_prepared_wfo <- NiN_sp |> 
  distinct(species)

# prepare dataset for WFO matching
nin_prepared_wfo <- WFO.prepare(nin_prepared_wfo$species)


# reconnect subspecies with corresponding species from authorship column
nin_prepared <- nin_prepared_wfo |>
  rename(clean_string = spec.name) |> 
  tibble() |> 
  mutate(
    clean_string = clean_string |>
      str_replace_all("\\bsect\\b\\.?", " ") |>
      str_squish()
  ) |> 
  # unique values in spec.full and clean_string only
  distinct(spec.full, clean_string)



# standardise names to the WFO backbone
nin_sp_matched <- WFO.match(spec.data = nin_prepared$clean_string,
                            WFO.data = wfo_backbone,
                            Fuzzy = 0.15,
                            Fuzzy.max = 50,
                            Fuzzy.one = FALSE)




# finalise accepted name column according to latest taxonomical nomenclature
nin_sp_clean <- nin_sp_matched |>
  # make copy of the original species string
  rename(clean_string = spec.name.ORIG) |>
  group_by(clean_string) |>
  summarise(
    # 1. Flag multiple scientificName suggestions
    flag_multiple_suggestions = n_distinct(scientificName) > 1,
    
    # 2. Candidate accepted_name from Old.name when possible
    accepted_name = case_when(
      any(New.accepted == TRUE & Old.name != "") ~ 
        # take one Old.name where New.accepted == TRUE and Old.name non-empty
        Old.name[New.accepted == TRUE & Old.name != ""][1],
      TRUE ~ 
        # otherwise fall back to (one) scientificName
        scientificName[1]
    ),
    
    # 3. Where did accepted_name come from?
    accepted_from = case_when(
      any(New.accepted == TRUE & Old.name != "") ~ "Old.name",
      TRUE ~ "scientificName"
    ),
    .groups = "drop"
  )


# check the species that change name where many options were available
nin_sp_clean |> filter(clean_string != accepted_name) #|> view()
nin_sp_clean |> filter(flag_multiple_suggestions == TRUE, clean_string != accepted_name) #|> view()



# correct incorrect corrections. haha
nin_sp_clean <- nin_sp_clean |> 
  mutate(
    flag_species_revert =
      case_when(
        grepl("Hieracium", clean_string) ~ "edited",
        TRUE ~ ""
        
      ),
    accepted_name = case_when(
      grepl("Hieracium", clean_string) ~ clean_string,
      TRUE ~ accepted_name
    ))

# check name changes
nin_sp_clean |> filter(flag_multiple_suggestions == TRUE, clean_string != accepted_name) #|> view()



# bind new species names onto indicator dataset
nin_species_clean <- left_join(nin_prepared, nin_sp_clean, by = "clean_string") |> 
  full_join(NiN_sp, by = join_by(spec.full == species)) |> 
  # filter out sect. species and subspecies
  #filter(!grepl("subsp.", scientific_name_original)) |> 
  distinct()

nin_species_clean |> filter(is.na(accepted_name))




# 2.3.4  Merge with indicator values
nin_sp_ind <- nin_species_clean |>
  select(Nature_Type, Sub_Type, nin_id, nin_code, species = accepted_name, species_group, cover) |> 
  left_join(tyler_indicators,
            by = join_by(species)) |> 
  tibble()


# double check species matching
nin_sp_ind |> filter(is.na(species))  
nin_sp_ind |> filter(is.na(Grazing_mowing)) |>  distinct(species)



# merge nin datasets
nin_sp_ind <- nin_sp_ind |> 
  full_join(natop_nin_sp_ind |> rename(nin_code = nin_id))


#write_rds(nin_sp_ind, paste0(here::here(), "/data/cache/nin_sp_ind.RDS"))
