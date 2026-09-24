#Tyler indicators cleaning

#ind_tyler <- readRDS("P:/41201785_okologisk_tilstand_2022_2023/data/functional plant indicators/ind.Tyler.RDS")
tyler_indicators <- readRDS(paste0(here(), "/data/indicators/ind.Tyler.RDS"))


ind_tyler <- ind_tyler |>
  rename(scientific_name = Scientific_name) |> 
  mutate(scientific_name_original = scientific_name,
         scientific_name = str_replace_all(scientific_name, "ssp.", "subsp."),         # correct subspecies labelling
         scientific_name = str_replace_all(scientific_name, "\u00EB", "e"),
         scientific_name = str_remove_all(scientific_name, "agg."),                    # remove aggregates
         scientific_name = str_replace_all(scientific_name, " x ", " \u00D7 ")) |>     # correct hybrids labelling
  filter(!is.na(scientific_name), !scientific_name == "") |> 
  distinct(scientific_name_original, scientific_name, Moisture, Nitrogen, Soil_disturbance, Grazing_mowing, Phosphorus, Soil_reaction_pH, Light) |>           # select indicator here
  tibble()

ind_tyler <- ind_tyler |> 
  mutate(scientific_name = clean_species_with_patterns(scientific_name, species_dict_pattern))

# remove certain species
ind_tyler <- ind_tyler |>  
  filter( !(scientific_name_original %in% list("Ammophila arenaria x Calamagrostis epigejos",
                                               "Anemone nemorosa x ranunculoides",
                                               "Armeria maritima ssp. elongata",
                                               "Asplenium trichomanes ssp. quadrivalens",
                                               "Calystegia sepium ssp. spectabilis",
                                               "Campanula glomerata 'Superba'",
                                               "Dactylorhiza maculata ssp. fuchsii",
                                               "Erigeron acris ssp. droebachensis",
                                               "Erigeron acris ssp. politus",
                                               "Erysimum cheiranthoides L. ssp. alatum",
                                               "Euphrasia nemorosa x stricta var. brevipila",
                                               "Galium mollugo x verum",
                                               "Geum rivale x urbanum",
                                               "Hylotelephium telephium (ssp. maximum)",
                                               "Juncus alpinoarticulatus ssp. rariflorus",
                                               "Lamiastrum galeobdolon ssp. argentatum",
                                               "Lathyrus latifolius ssp. heterophyllus",
                                               "Medicago sativa ssp. falcata",
                                               "Medicago sativa ssp. x varia",
                                               "Monotropa hypopitys ssp. hypophegea",
                                               "Ononis spinosa ssp. hircina",
                                               "Ononis spinosa ssp. procurrens",
                                               "Pilosella aurantiaca ssp. decolorans",
                                               "Pilosella aurantiaca ssp. dimorpha",
                                               "Pilosella cymosa ssp. gotlandica",
                                               "Pilosella cymosa ssp. praealta",
                                               "Pilosella officinarum ssp. peleteranum",
                                               "Poa x jemtlandica (Almq.) K. Richt.",
                                               "Poa x herjedalica Harry Sm.",
                                               "Ranunculus peltatus ssp. baudotii",
                                               "Sagittaria natans x sagittifolia",
                                               "Salix repens ssp. rosmarinifolia",
                                               "Stellaria nemorum L. ssp. montana",
                                               "Trichophorum cespitosum ssp. germanicum")
  ))

# prepare dataset for WFO matching
tyler_prepared_wfo <- WFO.prepare(ind_tyler$scientific_name)


# reconnect subspecies with corresponding species from authorship column
tyler_prepared <- tyler_prepared_wfo |>
  tibble() |> 
  mutate(
    # first word of Authorship
    subspecies = if_else(!is.na(Authorship), word(Authorship, 1, 1), ""),
    clean_string = word(spec.name, 1, 2),
    clean_string = if_else(
      str_detect(spec.name, " x$") |                                # ending with " x" OR
        str_detect(spec.name, "\u00D7$") |                            # ending with "×" OR
        str_detect(spec.name, "^\\s*[x\u00D7]\\b"),                   # starting with x/× hybrid marker
      paste(spec.name, subspecies),
      clean_string
    ),
    clean_string = if_else(                                         # remove 'sect.'
      str_detect(spec.name, "sect."),
      paste(word(clean_string, 1, 1), word(spec.full, 3, 3)),
      clean_string
    )
  ) |>
  # unique values in spec.full and clean_string only
  distinct(spec.full, clean_string)



# standardise names to the WFO backbone (slow)
tyler_sp_matched <- WFO.match(spec.data = tyler_prepared$clean_string,
                              WFO.data = wfo_backbone,
                              Fuzzy = 0.15,
                              Fuzzy.max = 50,
                              Fuzzy.one = FALSE)



# finalise accepted name column according to latest taxonomical nomenclature
tyler_sp_clean <- tyler_sp_matched |>
  # make copy of the original species string
  rename(clean_string = spec.name.ORIG) |>
  group_by(clean_string) |>
  summarise(
    # Flag multiple scientificName suggestions
    flag_multiple_suggestions = n_distinct(scientificName) > 1,
    
    # Candidate accepted_name from Old.name when possible
    accepted_name = case_when(
      any(New.accepted == TRUE & Old.name != "") ~ 
        # take one Old.name where New.accepted == TRUE and Old.name non-empty
        Old.name[New.accepted == TRUE & Old.name != ""][1],
      TRUE ~ 
        # otherwise fall back to (one) scientificName
        scientificName[1]
    ),
    
    # Where did accepted_name come from?
    accepted_from = case_when(
      any(New.accepted == TRUE & Old.name != "") ~ "Old.name",
      TRUE ~ "scientificName"
    ),
    .groups = "drop"
  )


# check the species that change name where many options were available
tyler_sp_clean |> filter(clean_string != accepted_name) #|> view()
tyler_sp_clean |> filter(flag_multiple_suggestions == TRUE, clean_string != accepted_name) #|> view()


# bind new species names onto indicator dataset
tyler_species_clean <- left_join(tyler_prepared, tyler_sp_clean, by = "clean_string") |> 
  full_join(ind_tyler, by = join_by(spec.full == scientific_name)) |> 
  # filter out sect. species and subspecies
  #filter(!grepl("subsp.", scientific_name_original)) |> 
  distinct()

# correct incorrect corrections. haha
tyler_species_clean <- tyler_species_clean |> 
  mutate(
    flag_species_revert =
      case_when(
        accepted_name == "Rosa vinodora" ~ "reverted",
        #accepted_name == "Salix mollissima" ~ "reverted",
        accepted_name == "Rosa canina subsp. glauca" ~ "edited",
        clean_string == "Heracleum 'kungsholm'" ~ "reverted",
        clean_string == "Iris germanica" ~ "edited",
        clean_string == "Hedlundia atrata" ~ "reverted",
        clean_string == "Hedlundia faohraei" ~ "reverted",
        grepl("Hieracium", clean_string) & grepl("sect.", spec.full) ~ "edited",
        is.na(accepted_name) ~ "added",                                                          # fill missing accepted_name with clean_string
        TRUE ~ ""
        
      ),
    accepted_name = case_when(
      accepted_name == "Clinopodium acinos" ~ "Acinos arvensis",
      accepted_name == "Rosa vinodora" ~ "Rosa inodora",
      clean_string == "Heracleum 'kungsholm'" ~ clean_string,
      #accepted_name == "Salix mollissima" ~ clean_string,
      clean_string == "Iris germanica" ~ clean_string,
      clean_string == "Hedlundia atrata" ~ clean_string,
      clean_string == "Hedlundia faohraei" ~ clean_string,
      grepl("Hieracium", clean_string) & grepl("sect.", spec.full) ~ paste(word(spec.full, 1, 1), word(spec.full, 3, 3)),
      is.na(accepted_name) ~ clean_string,
      TRUE ~ accepted_name
    ))

# check name changes
tyler_species_clean |> filter(clean_string != accepted_name)


tyler_species_clean |> filter(is.na(accepted_name))


tyler_indicators <- tyler_species_clean |> 
  select(species = accepted_name,
         Moisture:Light)                          # select all indicators

# remove unused dataframes
#rm(ind_tyler, tyler_prepared, tyler_prepared_wfo, tyler_sp_clean, tyler_sp_matched, tyler_species_clean)


# cache the indicator data here
#saveRDS(tyler_indicators, paste0(here::here(),"/data/cache/tyler_indicators.RDS"))
