## 2.1.1 GRUK species data handling
GRUK_species <- GRUK_species |> 
  janitor::clean_names() |> 
  rename(scientific_name = latinsk_navn,
         art_dekning = dekning_percent)


# fix species names
GRUK_species <- GRUK_species |>
  mutate(scientific_name_original = scientific_name,
         scientific_name = str_replace_all(scientific_name, "ssp.", "subsp."),         # correct subspecies labelling
         scientific_name = str_replace_all(scientific_name, "\u00EB", "e"),
         scientific_name = str_remove_all(scientific_name, "agg."),                    # remove aggregates
         scientific_name = str_replace_all(scientific_name, " x ", " \u00D7 ")) |>     # correct hybrids labelling
  filter(!is.na(scientific_name), !scientific_name == "") |> 
  tibble()

# update species names
GRUK_species <- GRUK_species |> 
  mutate(scientific_name = clean_species_with_patterns(scientific_name, species_dict_pattern),
         scientific_name = str_replace(
           scientific_name,
           "^Hiero\\S*",
           "Hierochloe"))      # fixing Hierochloe naming issue

GRUK_prepared_wfo <- WFO.prepare(GRUK_species$scientific_name)

GRUK_prepared <- GRUK_prepared_wfo |>
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



# standardise names to the WFO backbone
GRUK_sp_matched <- WFO.match(spec.data = GRUK_prepared$clean_string,
                             WFO.data = wfo_backbone,
                             Fuzzy = 0.15,
                             Fuzzy.max = 50,
                             Fuzzy.one = FALSE)


# create accepted name column according to latest taxonomical nomenclature
GRUK_sp_clean <- GRUK_sp_matched |>
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
GRUK_sp_clean |> filter(clean_string != accepted_name)
GRUK_sp_clean |> filter(flag_multiple_suggestions == TRUE, clean_string != accepted_name)


# correct incorrect corrections
GRUK_sp_clean <- GRUK_sp_clean |> 
  mutate(
    flag_species_revert =
      case_when(
        clean_string == "Hieracium vulgata" ~ "added",
        clean_string == "Hieracium murorum" ~ "edited",
        clean_string == "Potentilla anserina" ~ "added",
        TRUE ~ ""
        
      ),
    accepted_name = case_when(
      clean_string == "Hieracium vulgata" ~ "Hieracium Vulgata",
      clean_string == "Hieracium murorum" ~ "Hieracium Vulgata",
      clean_string == "Potentilla anserina" ~ "Argentina anserina",
      TRUE ~ accepted_name
    ))



# bind new species names onto original dataset
GRUK_species_clean <- full_join(GRUK_prepared, GRUK_sp_clean, by = "clean_string") |> 
  full_join(GRUK_species, by = join_by(spec.full == scientific_name)) |> 
  mutate(accepted_name = case_when(
    is.na(accepted_name)& !grepl("Hieracium", spec.full) ~ str_to_sentence(spec.full),
    is.na(accepted_name)& grepl("Hieracium", spec.full) ~ spec.full,
    TRUE ~ accepted_name
  )) |> 
  distinct() |> 
  filter(!is.na(accepted_name))


# check for original species with no matched accepted name.
GRUK_species_clean |> 
  filter(is.na(accepted_name)) |> 
  tibble()
# none.


#GRUK_species_clean |> group_by(accepted_name, global_id) |> mutate(n = n_distinct(art_dekning)) |>  filter(n > 1)

# merge species data with indicators

GRUK_species_ind <- GRUK_species_clean |>
  select(parent_global_id, polygon_id, rute_id, creation_date, species = accepted_name, art_dekning) |> 
  left_join(tyler_indicators,
            by = join_by("species")) |> 
  tibble()


summary(GRUK_species_ind)

# checking which species didn't find a match
unique(GRUK_species_ind[is.na(GRUK_species_ind$Grazing_mowing),'species'])# |> view()




#########################################################
## 2.1.2 GRUK ruter data handling
names(GRUK_ruter)

GRUK_ruter <- GRUK_ruter |> 
  janitor::clean_names() |>
  select(
    global_id,
    polygon_id,
    rute_id,
    rute_id_loknr,
    dekning_karplanter_feltsjikt = dekning_percent_av_karplanter_i_feltsjikt,
    dekning_moser =  dekning_percent_av_moser,
    dekning_lav = dekning_percent_av_lav,
    dekning_stro = dekning_percent_av_stro,
    dekning_bar_jord_grus_stein_berg = dekning_percent_av_bar_jord_grus_stein_berg,
    precision,
    UTM33_E_ne = utm33_e_ne, 
    UTM33_N_ne = utm33_n_ne,
    UTM33_E_sw = utm33_e_sw,
    UTM33_N_sw = utm33_n_sw,
    areal_m2,
    registeringsdato
  ) |> 
  mutate(year = year(registeringsdato)) |> 
  select(-registeringsdato)


# make coordinates numeric
GRUK_ruter <- GRUK_ruter |> 
  mutate(UTM33_E_ne = as.numeric(UTM33_E_ne),
         UTM33_N_ne = as.numeric(UTM33_N_ne),
         UTM33_E_sw = as.numeric(UTM33_E_sw),
         UTM33_N_sw = as.numeric(UTM33_N_sw) )

# calculate central coordinates for each plot
GRUK_ruter <- GRUK_ruter |> 
  mutate(UTM33_N = (UTM33_N_ne + UTM33_N_sw)/2,
         UTM33_E = (UTM33_E_ne + UTM33_E_sw)/2)

# some of the calculations throw NA's because there's only one set of coordinates, coalesce that set into the calculation column 
GRUK_ruter <- GRUK_ruter |> 
  mutate (UTM33_N = coalesce(UTM33_N,UTM33_N_ne),
          UTM33_E = coalesce(UTM33_E,UTM33_E_ne),
          UTM33_N = coalesce(UTM33_N,UTM33_N_sw),
          UTM33_E = coalesce(UTM33_E,UTM33_E_sw) 
  )


## 2.1.3 GRUK sirkler data handling
## merge information on mapping units and condition variables from GRUK.sirkler into GRUK.ruter
names(GRUK_ruter)
names(GRUK_sirkler)

GRUK_sirkler <- GRUK_sirkler |> 
  janitor::clean_names() |> 
  select(
    global_id,
    kartleggingsenhet_1_5000,
    spor_etter_slitasje_og_slitasjebetinget_erosjon = spor_etter_slitasje_og_slitasjebetinget_erosjon_percent,
    dekning_nakent_berg= dekning_percent_av_nakent_berg,
    total_dekning_vedplanter_i_feltsjikt = total_dekning_percent_av_vedplanter_i_feltsjikt,
    dekning_busker_busksjikt = dekning_percent_av_busker_i_busksjikt,
    dekning_tresjikt = dekning_percent_av_tresjikt,
    dekning_problemarter = dekning_percent_av_problemarter,
    total_dekning_fremmede_arter = total_dekning_percent_av_fremmede_arter,
    registeringsdato
  ) |> 
  mutate(year = year(registeringsdato)) |> 
  select(-registeringsdato)


GRUK_variables <- GRUK_ruter |> 
  full_join(GRUK_sirkler, by = c("global_id", "year")) |>           # join plots with circles
  filter(!is.na(UTM33_N))                                           # filter away plots with no coordinates

summary(GRUK_variables)



## merge information on condition and quality from GRUK.polygoner into GRUK.variables
# transform GRUK.variables into spatial object
GRUK_variables <- st_as_sf(GRUK_variables, coords = c("UTM33_E","UTM33_N"), remove = FALSE, crs = 25833)


## 2.1.4 GRUK polygoner data handling
# transform GRUK.polygoner into spatial object
GRUK_polygoner <- st_as_sf(GRUK_polygoner, wkt = "WKT" ,remove=F, crs = 25833) |> 
  janitor::clean_names() |> 
  rename(
    nin_id = ni_nid,
    year = ar,
    nin_kartleggingsenheter = ni_n_kartleggingsenheter,
    nin_beskrivelsesvariabler = ni_n_beskrivelsesvariabler
  )





tm_shape(GRUK_polygoner) +
  tm_graticules() +
  tm_polygons("polygon_id") +
  tm_shape(GRUK_variables) +
  tm_dots("rute_id")

# run a spatial join to get columns from GRUK.polygoner into GRUK.variables
#GRUK.variables <- st_join(GRUK.variables,GRUK.polygoner[,c(3:4,9,15,18,20,22,24,60)])
#names(GRUK.variables)[1:33]<-c("GlobalID","PolygonID.x","RuteID","RuteID_loknr",
#                               "Dekning_karplanter_feltsjikt","Dekning_moser","Dekning_lav","Dekning_strø",
#                               "Dekning_bar_substrat","Precision","UTM33_E_ne","UTM33_N_ne",
#                               "UTM33_E_sw","UTM33_N_sw","areal(m2)","UTM33_N","UTM33_E","Kartleggingsenhet_1til5000",
#                               "erosjon_prosent","Dekning_nakentberg",
#                               "Totaldekning_vedplanter_feltsjikt","Dekning_busker_busksjikt","Dekning_tresjikt",
#                               "Dekning_problemarter","Totaldekning_fremmedearter","LokalitetID","PolygonID.y",
#                               "Kartleggingsdato","Lokalitetskvalitet","Kommune","Tilstand","Naturmangfold","NiNKartleggingsenheter")

# check how good the spatial join worked
#cbind(GRUK.variables$PolygonID.x,GRUK.variables$PolygonID.y)
#GRUK.variables[7,]
#GRUK.polygoner[GRUK.polygoner$PolygonID=="46-2",]
# some points could not be matched to polygons -> merge by PolygonID instead, drop geometry of GRUK.variables first
GRUK_variables <- GRUK_variables |> 
  st_drop_geometry()

names(GRUK_variables)
names(GRUK_polygoner)


GRUK_variables <- GRUK_variables |> 
  left_join(GRUK_polygoner, by = c("year", "polygon_id")) # there are 94 rows only found in GRUK_polygoner


summary(GRUK_variables) 
summary(as.factor(GRUK_variables$tilstand)) # 5 plots with NAs


# edit the column names
GRUK_variables <- GRUK_variables |> 
  rename(erosion_percent = spor_etter_slitasje_og_slitasjebetinget_erosjon)



## adding information on ecosystem and condition variables to species+indicator data
names(GRUK_species_ind)
names(GRUK_variables)

GRUK_species_ind <- GRUK_species_ind |> 
  left_join(GRUK_variables |>  select(global_id, kartleggingsenhet_1_5000), by = join_by(parent_global_id == global_id)) |> 
  mutate(hovedtype_rute = str_sub(kartleggingsenhet_1_5000, 1, 2)) |> 
  filter(!is.na(hovedtype_rute))



# fixing variable types
GRUK_all <- GRUK_variables |> 
  mutate(across(
    c(kartleggingsenhet_1_5000, lokalitetskvalitet, kommune, tilstand, naturmangfold, nin_kartleggingsenheter),
    as.factor
  ),
  across(
    c(areal_m2, dekning_nakent_berg, dekning_problemarter),
    as.numeric
  )) |> 
  distinct()

summary(GRUK_all)


#rm(GRUK_polygoner, GRUK_prepared, GRUK_prepared_wfo, GRUK_ruter, GRUK_sirkler, GRUK_sp_clean, GRUK_sp_matched, GRUK_species, GRUK_species_clean, GRUK_variables)

#saveRDS(GRUK_all, paste0(here::here(),"/data/cache/GRUK_all.RDS"))
#saveRDS(GRUK_species_ind, paste0(here::here(),"/data/cache/GRUK_species_ind.RDS"))


