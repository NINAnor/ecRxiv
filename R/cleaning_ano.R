# download 
#url <- "https://nedlasting.miljodirektoratet.no/naturovervaking/naturovervaking_eksport.gdb.zip"
#download(url, dest="P:/41201785_okologisk_tilstand_2022_2023/data/ANO/naturovervaking_eksport.gdb.zip", mode="wb") 
#unzip("P:/41201785_okologisk_tilstand_2022_2023/data/ANO/naturovervaking_eksport.gdb.zip", exdir = "P:/41201785_okologisk_tilstand_2022_2023/data/ANO/naturovervaking_eksport.gdb")

ano_species <- st_read("P:/41201785_okologisk_tilstand_2022_2023/data/ANO/naturovervaking_eksport.gdb", layer="ANO_Art", quiet = T)
ano_geo <- st_read("P:/41201785_okologisk_tilstand_2022_2023/data/ANO/naturovervaking_eksport.gdb", layer="ANO_SurveyPoint", quiet = T)

#write_rds(ano_species, file = paste0(here(), "/data/cache/ano_species.RDS"))
#write_rds(ano_geo, file = paste0(here(), "/data/cache/ano_geo.RDS"))

# 2.4.1 extract species data
ano_species <- ano_species |> 
  mutate(scientific_name = str_replace_all(art_navn, "_", " "),
         scientific_name_original = scientific_name,
         scientific_name = str_replace_all(scientific_name, "ssp.", "subsp."),         # correct subspecies labelling
         scientific_name = str_replace_all(scientific_name, "\u00EB", "e"),
         scientific_name = str_remove_all(scientific_name, "agg."),                    # remove aggregates
         scientific_name = str_replace_all(scientific_name, " x ", " \u00D7 "),     # correct hybrids labelling
         scientific_name = str_to_sentence(scientific_name),
         scientific_name = if_else(
           is.na(word(scientific_name, 2)),        # only one word (no species epithet)
           word(scientific_name, 1),              # just genus
           word(scientific_name, 1, 2)            # genus + species
         ) |> 
           #str_replace_all("-", " ")  |> 
           str_squish()
  ) |> 
  filter(!is.na(scientific_name), !scientific_name == "") |> 
  tibble()


# recode species names for WFO matching
ano_species <- ano_species |> 
  mutate(scientific_name = clean_species_from_dictionary(scientific_name, species_dict_pattern), # 4% of scientific_name changed
         scientific_name = str_replace(
           scientific_name,
           "^Hiero\\S*",
           "Hierochloe"))

ano_prepared_wfo <- ano_species |> 
  distinct(scientific_name, scientific_name_original)

# prepare dataset for WFO matching
ano_prepared_wfo <- WFO.prepare(ano_prepared_wfo$scientific_name)

# reconnect subspecies with corresponding species from authorship column
ano_prepared <- ano_prepared_wfo |>
  tibble() |> 
  mutate(
    spec.name = spec.name |>
      str_replace_all("\\bsect\\b\\.?", " ") |>
      str_squish()
  ) |>
  rename(clean_string = spec.name) |> 
  # unique values in spec.full and clean_string only
  distinct(spec.full, clean_string)



# standardise names to the WFO backbone (slow)
ano_sp_matched <- WFO.match(spec.data = ano_prepared$clean_string,
                            WFO.data = wfo_backbone,
                            Fuzzy = 0.15,
                            Fuzzy.max = 50,
                            Fuzzy.one = FALSE)


# finalise accepted name column according to latest taxonomical nomenclature
ano_sp_clean <- ano_sp_matched |>
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
ano_sp_clean |> filter(clean_string != accepted_name) #|> view()
ano_sp_clean |> filter(flag_multiple_suggestions == TRUE, clean_string != accepted_name) #|> view()


# correct incorrect corrections
ano_sp_clean <- ano_sp_clean |> 
  mutate(
    flag_species_revert =
      case_when(
        grepl("Hieracium", clean_string) ~ "edited",
        clean_string == "Poa jemtlandica" ~ "edited",
        TRUE ~ ""
        
      ),
    accepted_name = case_when(
      grepl("Hieracium", clean_string) ~ clean_string,
      accepted_name == "Poa jemtlandica" ~ "Poa alpina",
      TRUE ~ accepted_name
    ))

# check name changes
ano_sp_clean |> filter(flag_multiple_suggestions == TRUE, clean_string != accepted_name) #|> view()


ano_species_clean <- left_join(ano_prepared, ano_sp_clean, by = "clean_string") |> 
  full_join(ano_species, by = join_by(spec.full == scientific_name)) |> 
  # filter out sect. species and subspecies
  #filter(!grepl("subsp.", scientific_name_original)) |> 
  distinct() |> 
  filter(!is.na(accepted_name))

ano_species_clean |> filter(is.na(accepted_name))


## merge species data with indicators
ANO_species_ind <- ano_species_clean |>                                 # fix species match here
  select(species = accepted_name, art_dekning, parentglobalid) |> 
  full_join(tyler_indicators) |> 
  tibble()



# checking which species didn't find a match
unique(ANO_species_ind[is.na(ANO_species_ind$Grazing_mowing),'species']) # |> view()



# fixing variable types
ANO_species_ind <- ANO_species_ind |> 
  mutate(across(
    c(species),
    as.factor
  )) |> 
  # trimming away the points without information on NiN, species or cover  
  filter(!is.na(species), !is.na(art_dekning))

summary(ANO_species_ind)



## fix NiN information
ano_geo <- ano_geo |>
  mutate(
    # 3 first characters of kartleggingsenhet_1m2, no hyphen
    hovedtype_rute = substr(kartleggingsenhet_1m2, 1, 3),
    hovedtype_rute = gsub("-", "", hovedtype_rute),
    
    # recode to hovedøkosystem
    hovedoekosystem_rute = recode(
      hovedtype_rute,
      "T31" = "Seminat", "T32" = "Seminat", "T33" = "Seminat", "T34" = "Seminat", "V9"  = "Seminat", "V10" = "Seminat",
      "T2"  = "Natopen", "T8"  = "Natopen", "T11" = "Natopen", "T12" = "Natopen", "T13" = "Natopen", "T15" = "Natopen", 
      "T16" = "Natopen", "T18" = "Natopen", "T21" = "Natopen", "T24" = "Natopen", "T29" = "Natopen",
      "T41" = "Seminat", 
      "T45" = "Seminat"
    )
  )

# 2. Fix NiN variable names
ano_geo <- ano_geo |> 
  rename(
    groeftingsintensitet = bv_7jb_ba,
    bruksintensitet      = bv_7jb_bt,
    beitetrykk           = bv_7jb_si,
    slatteintensitet     = bv_7tk,
    tungekjoretoy        = bv_7se,
    slitasje             = forekomst_ntyp
  )

## 3. Clean NiN codes (remove prefixes, X -> NA, to numeric)

ano_geo <- ano_geo |>
  mutate(
    groeftingsintensitet = groeftingsintensitet |>
      str_remove("^7GR-GI_") |>
      na_if("X") |>
      as.numeric(),
    
    bruksintensitet = bruksintensitet |>
      str_remove("^7JB-BA_") |>
      na_if("X") |>
      as.numeric(),
    
    beitetrykk = beitetrykk |>
      str_remove("^7JB-BT_") |>
      na_if("X") |>
      as.numeric(),
    
    slatteintensitet = slatteintensitet |>
      str_remove("^7JB-SI_") |>
      na_if("X") |>
      as.numeric(),
    
    tungekjoretoy = tungekjoretoy |>
      str_remove("^7TK_") |>
      na_if("X") |>
      as.numeric(),
    
    slitasje = slitasje |>
      str_remove("^7SE_") |>
      na_if("X") |>
      as.numeric()
  )

## 4. Filter lowland plots points
ano_all <- ano_geo |>
  tibble() |> 
  filter(hovedoekosystem_rute %in% c("Natopen", "Seminat")) |> 
  semi_join(ANO_species_ind, by = join_by(globalid == parentglobalid))



ANO_species_ind <- ANO_species_ind |> 
  left_join(ano_all |> select("globalid","ano_flate_id","ano_punkt_id","ssb_id","aar", "hovedtype_rute","kartleggingsenhet_1m2"), 
            by = join_by(parentglobalid == globalid)) |> 
  filter(!is.na(hovedtype_rute))


#write_rds(ano_all, paste0(here::here(),"/data/cache/ano_all.RDS"))
#write_rds(ANO_species_ind, paste0(here::here(),"/data/cache/ANO_species_ind.RDS"))



