# 2.2.1 ASO points data

ASO_points <- st_as_sf(x = ASO_points, 
                       coords = c("x", "y"),
                       crs = "+proj=longlat +datum=WGS84 +ellps=WGS84")

ASO_points <- ASO_points |> 
  janitor::clean_names()

# Rename using base R
nms <- names(ASO_points)

nms[nms == "dominerende_kartleggingsenhet_1_5000_t32"] <- "nin_grunntype"
nms[nms == "aktuell_bruksintensitet_7jb_ba"]           <- "bruksintensitet"
nms[nms == "beitetrykk_7jb_bt"]                        <- "beitetrykk"
nms[nms == "slatteintensitet_7jb_si"]                  <- "slatteintensitet"
nms[nms == "spor_etter_ferdsel_med_tunge_kjoretoy_m_dir_prtk"] <-
  "tungekjoretoy"
nms[nms == "spor_etter_slitasje_og_slitasjebetinget_erosjon_m_dir_prse"] <-
  "slitasje"

names(ASO_points) <- nms

## fix NiN-variables
# remove variable code in the data
ASO_points <- ASO_points |>
  mutate(
    bruksintensitet = bruksintensitet |>
      str_remove("^7JB-BA_") |>
      na_if("X") |>
      as.numeric(),
    
    beitetrykk = beitetrykk |>
      str_remove("^7JB-BT_") |>
      na_if("X") |>
      as.numeric(),
    
    slatteintensitet = slatteintensitet |>  # 4 NAs
      str_remove("^7JB-SI_") |>
      na_if("X") |>
      as.numeric(),
    
    tungekjoretoy = tungekjoretoy |>
      str_remove("^MDirPRTK_") |>
      na_if("X") |>
      as.numeric(),
    
    slitasje = slitasje |>
      str_remove("^MDirPRSE_") |>
      na_if("X") |>
      as.numeric()
  ) |> 
  mutate(
    nin_grunntype = case_when(is.na(nin_grunntype) ~ annen_dominerende_kartleggingsenhet,
                              TRUE ~ nin_grunntype)
  )



# 2.2.2 species data

## fixing variable names and issues in ASO.sp
head(as.data.frame(ASO_species))

ASO_species <- rename(ASO_species, art_dekning = Dekning)

# fix species names
ASO_species <- ASO_species |>
  mutate(scientific_name_original = karplantenavn) |> 
  separate(
    col  = Navn,
    into = c("norsk_navn", "scientific_name"),
    sep  = "_",
    extra = "merge",   # keep any additional _ in the "after" part
    fill  = "right"    # if no _, "after" becomes NA
  ) |> 
  mutate(
    scientific_name = scientific_name  |> 
      str_replace_all("_", " ")  |> 
      str_to_sentence(),
    
    scientific_name = if_else(
      is.na(word(scientific_name, 2)),        # only one word (no species epithet)
      word(scientific_name, 1),              # just genus
      word(scientific_name, 1, 2)            # genus + species
    ) %>%
      str_replace_all("-", " ")  |> 
      str_squish()
  )

ASO_species <- ASO_species  |>  
  mutate(scientific_name = clean_species_with_patterns(scientific_name, species_dict_pattern),
         scientific_name = str_replace(
           scientific_name,
           "^Hiero\\S*",
           "Hierochloe"))      # fixing Hierochloe naming issue


# filter out NAs for WFO cleaning
ASO_species_prepared <- filter(ASO_species, !is.na(scientific_name))


ASO_prepared_wfo <- WFO.prepare(ASO_species_prepared$scientific_name)

ASO_prepared <- ASO_prepared_wfo |>
  mutate(
    spec.name = case_when(
      !is.na(Authorship) & Authorship != "" ~ paste0(spec.name, "-", Authorship),
      TRUE ~ spec.name) |>
      str_squish() |> 
      str_to_sentence()
  ) |>
  rename(clean_string = spec.name) |>
  distinct(spec.full, clean_string)



# standardise names to the WFO backbone
ASO_sp_matched <- WFO.match(spec.data = ASO_prepared$clean_string,
                            WFO.data = wfo_backbone,
                            Fuzzy = 0.15,
                            Fuzzy.max = 50,
                            Fuzzy.one = FALSE)


# create accepted name column according to latest taxonomical nomenclature
ASO_sp_clean <- ASO_sp_matched |>
  # First, store the original string clearly
  rename(clean_string = spec.name.ORIG) |>
  group_by(clean_string) |>
  summarise(
    # 1) Flag multiple scientificName suggestions
    flag_multiple_suggestions = n_distinct(scientificName) > 1,
    
    # 2) Candidate accepted_name from Old.name when possible
    accepted_name = case_when(
      any(New.accepted == TRUE & Old.name != "") ~ 
        # take one Old.name where New.accepted == TRUE and Old.name non-empty
        Old.name[New.accepted == TRUE & Old.name != ""][1],
      TRUE ~ 
        # otherwise fall back to (one) scientificName
        scientificName[1]
    ),
    
    # 3) Where did accepted_name come from?
    accepted_from = case_when(
      any(New.accepted == TRUE & Old.name != "") ~ "Old.name",
      TRUE ~ "scientificName"
    ),
    .groups = "drop"
  )


# check the species that change name where many options were available
ASO_sp_clean |> filter(clean_string != accepted_name)
ASO_sp_clean |> filter(flag_multiple_suggestions == TRUE, clean_string != accepted_name)


# correct incorrect corrections. haha
ASO_sp_clean <- ASO_sp_clean |> 
  mutate(
    flag_species_revert =
      case_when(
        clean_string == "Hieracium vulgata" ~ "added",
        clean_string == "Taraxacum crocea" ~ "added",
        clean_string == "Taraxacum hamata" ~ "added",
        TRUE ~ ""
        
      ),
    accepted_name = case_when(
      clean_string == "Hieracium vulgata" ~ "Hieracium Vulgata",
      clean_string == "Taraxacum crocea" ~ clean_string,
      clean_string == "Taraxacum hamata" ~ clean_string,
      TRUE ~ accepted_name
    )) |> 
  # add "sp." back onto genus-level identifications
  mutate(accepted_name = case_when(
    is.na(word(accepted_name, 2)) ~ paste(accepted_name, "sp."),
    TRUE ~ accepted_name
  ))



# bind new species names onto original dataset
ASO_species_clean <- left_join(ASO_prepared, ASO_sp_clean, by = "clean_string") |> 
  full_join(ASO_species, by = join_by(spec.full == scientific_name)) |> 
  distinct() |> 
  # retrieving Hieracium group info lost in Norwegian name
  mutate(
    accepted_name = case_when(
      norsk_navn == "skogsvevegruppa" ~ paste("Hieracium", str_sub(karplantenavn, 19, 27))  |> str_squish(),
      norsk_navn == "beitesvevegruppa" ~ paste("Hieracium", str_sub(karplantenavn, 20, 26))  |> str_squish(),
      norsk_navn == "skjermsvevegruppa" ~ paste("Hieracium", str_sub(karplantenavn, 21, 32))  |> str_squish(),
      TRUE ~ accepted_name
    )
  )


# check for original species with no matched accepted name.
ASO_species_clean |> 
  filter(is.na(accepted_name)) |> 
  tibble()



# 2.2.3 merge with indicator data

## merge species data with indicators
ASO_species_ind <- ASO_species_clean |>
  select(species = accepted_name, art_dekning, ParentGlobalID) |> 
  filter(!is.na(art_dekning)) |> 
  left_join(tyler_indicators) |>
  left_join(ASO_points |> select(global_id, omradenummer_flatenummer, nin_grunntype), by = join_by(ParentGlobalID == global_id)) |> 
  mutate(hovedtype_rute = str_sub(nin_grunntype, 1, 3)) |> 
  filter(!is.na(hovedtype_rute))


# checking which species didn't find a match
unique(ASO_species_ind[is.na(ASO_species_ind$Grazing_mowing),'species'])



## adding information on ecosystem and condition variables to species data
ASO_all <- ASO_species_ind |> 
  left_join(ASO_points |>  
              select(global_id, eng_id, aso_id, annen_dominerende_kartleggingsenhet), 
            by = join_by(ParentGlobalID == global_id)) |> 
  select(-c(species, art_dekning, Moisture:Light)) |> 
  distinct()



# fixing variable types
ASO_all <- ASO_all |> 
  mutate(across(
    c(nin_grunntype, omradenummer_flatenummer, eng_id, aso_id),
    as.factor
  )) #|> 
# trimming away the points without information on NiN, species or cover  
# filter(!is.na(species), !is.na(art_dekning), !is.na(nin_grunntype))

summary(ASO_all)

#rm(ASO_species, ASO_prepared, ASO_sp_matched, ASO_points, ASO_sp_clean, ASO_species_clean, ASO_species_prepared, ASO_prepared_wfo)

#saveRDS(ASO_all, paste0(here::here(),"/data/cache/ASO_all.RDS"))
#saveRDS(ASO_species_ind, paste0(here::here(),"/data/cache/ASO_species_ind.RDS"))


