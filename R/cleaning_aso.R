# ASO dataset

# 1. Load processing environments
library(readr)
library(dplyr)

# 2. Extract UUID via the NIBIO dataset DOI
aso_ds  <- dataset_search(doi = "10.15468/gq6wa5")
aso_key <- aso_ds$data$datasetKey

# 3. Request Darwin Core Archive Download 
# (Requires your GBIF username/password loaded in your .Renviron file)
download_request <- occ_download(
  pred("datasetKey", aso_key),
  format = "DWCA" 
)

# 4. Stand by for GBIF compilation to complete
occ_download_wait(download_request)

# 5. Bring the zip archive into your local workspace folder
downloaded_zip_path <- occ_download_get(download_request, path = ".")

# 6. Extract the zip file's contents into a local temporary folder
temp_extraction_dir <- file.path(tempdir(), "gbif_aso_raw")
unzip(zipfile = downloaded_zip_path, exdir = temp_extraction_dir)

# 7. Read the untouched source matrix file ("verbatim.txt") directly into R
# This keeps the character formatting intact before any GBIF database interpretations
aso_verbatim <- read_tsv(
  file = file.path(temp_extraction_dir, "verbatim.txt"),
  guess_max = 30000,
  show_col_types = FALSE  # Silences the data frame schema text window you saw
)



ASO_species <- read_delim("C:/Users/francesca.jaroszynsk/OneDrive - NINA/nina_projects/ANO/lowlands/occurrence.txt")
ASO_points <- read_delim("C:/Users/francesca.jaroszynsk/OneDrive - NINA/nina_projects/ANO/lowlands/event.txt")


# 2.2.1 ASO points data
ASO_points <- ASO_points |> 
  filter(!is.na(decimalLatitude)) |> 
  janitor::clean_names()

ASO_points <- st_as_sf(x = ASO_points, 
                       coords = c("decimal_latitude", "decimal_longitude"),
                       crs = "+proj=longlat +datum=WGS84 +ellps=WGS84") |> 
  st_transform(crs = 25833) |> 
  mutate(
    X = st_coordinates(st_sfc(geometry))[, 1],
    Y = st_coordinates(st_sfc(geometry))[, 2])


## fix NiN-variables
ASO_points <- ASO_points |> 
  mutate(habitat = str_remove_all(habitat, "NA_")) |> 
  filter(!is.na(habitat)) |> 
  select(id, event_id, parent_event_id, event_date, nin_grunntype = habitat, location_id, geometry, X, Y)



# 2.2.2 species data

## fixing variable names and issues in ASO.sp
head(as.data.frame(ASO_species))

# fix species names
ASO_species <- ASO_species |>
  janitor::clean_names() |> 
  mutate(
    scientific_name = if_else(
      is.na(word(scientific_name, 2)),        # only one word (no species epithet)
      word(scientific_name, 1),              # just genus
      word(scientific_name, 1, 2)            # genus + species
    )  |> 
      str_replace_all("-", " ")  |> 
      str_squish() |> 
      str_to_sentence()
  )

ASO_species <- ASO_species  |>  
  filter(!is.na(scientific_name), !is.na(organism_quantity)) |>                                    # remove empty species rows
  mutate(scientific_name = clean_species_from_dictionary(scientific_name, species_dict_pattern),
         scientific_name = str_replace(                                 # fixing Hierochloe naming issue
           scientific_name,
           "^Hiero\\S*",
           "Hierochloe"), 
  scientific_name = case_when(                                          # retrieving Hieracium group info lost in Norwegian name
    vernacular_name == "bm: skogsvevegruppa" ~ "Hieracium Hieracium",
    vernacular_name == "bm: beitesvevegruppa" ~ "Hieracium Vulgata",
    vernacular_name == "bm: skjermsvevegruppa" ~ "Hieracium Hieracioides",
    TRUE ~ scientific_name
  )
)


ASO_prepared_wfo <- WFO.prepare(ASO_species$scientific_name)

ASO_prepared <- ASO_prepared_wfo |>
  mutate(
    spec.name = case_when(
      !is.na(Authorship) & Authorship != "" ~ paste0(spec.name, "-", Authorship),
      TRUE ~ spec.name) |>
      str_squish() |> 
      str_to_sentence(),
    spec.name = if_else(grepl(" na$", spec.name), paste0(word(spec.name, 1, 1), " sp."), spec.name)
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


# correct incorrect corrections
ASO_sp_clean <- ASO_sp_clean |> 
  mutate(
    flag_species_revert =
      case_when(
        grepl("Hieracium", clean_string) ~ "edited",
        TRUE ~ ""
        
      ),
    accepted_name = case_when(
      grepl("Hieracium", clean_string) ~ clean_string,
      TRUE ~ accepted_name
    ),
    accepted_name = case_when(
      accepted_name == "Hieracium vulgata" ~ "Hieracium Vulgata",
      accepted_name == "Hieracium hieracium" ~ "Hieracium Hieracium",
      accepted_name == "Hieracium alpina" ~ "Hieracium Alpina",
      accepted_name == "Hieracium hieracioides" ~ "Hieracium Hieracioides",
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
  distinct()


# check for original species with no matched accepted name.
ASO_species_clean |> 
  filter(is.na(accepted_name)) |> 
  tibble()

ASO_species_clean <- ASO_species_clean |> 
  tibble() |> 
  mutate(organism_quantity = str_remove_all(organism_quantity, " % dekning"),
         organism_quantity = case_when(
           organism_quantity == "< 1" ~ "A7_0",
           organism_quantity == "1 - 6,25" ~ "A7_1",
           organism_quantity == "6,25 - 12,5" ~ "A7_2",
           organism_quantity == "12,5 - 25" ~ "A7_3",
           organism_quantity == "25-50" ~ "A7_4",
           organism_quantity == "50 - 75" ~ "A7_5",
           organism_quantity == "75 - 90" ~ "A7_6",
           organism_quantity == "> 90" ~ "A7_7",
           TRUE ~ organism_quantity
         ),
         organism_quantity = case_when(
           organism_quantity == "A7_0" ~ 0.1,
           organism_quantity == "A7_1" ~ 3.625,
           organism_quantity == "A7_2" ~ 9.375,
           organism_quantity == "A7_3" ~ 18.75, 
           organism_quantity == "A7_4" ~ 37.5,
           organism_quantity == "A7_5" ~ 62.5,
           organism_quantity == "A7_6" ~ 82.5,
           organism_quantity == "A7_7" ~ 95,
           TRUE ~ 0
         ),
         organism_quantity = as.numeric(organism_quantity))

# 2.2.3 merge with indicator data

## merge species data with indicators
ASO_species_ind <- ASO_species_clean |>
  tibble() |> 
  select(id, occurrence_id, event_id, species = accepted_name, art_dekning = organism_quantity) |> 
  filter(!is.na(art_dekning)) |> 
  left_join(tyler_indicators) |>
  left_join(ASO_points) |> 
  mutate(hovedtype_rute = str_sub(nin_grunntype, 1, 3)) |>
  filter(!is.na(hovedtype_rute), !hovedtype_rute =="T4-")


# checking which species didn't find a match
unique(ASO_species_ind[is.na(ASO_species_ind$Grazing_mowing),'species'])



## adding information on ecosystem and condition variables to species data
ASO_all <- ASO_points |> 
  semi_join(ASO_species_ind) |> 
  distinct() |> 
  mutate(across(
    c(nin_grunntype, parent_event_id, event_id, id),
    as.factor
  ))


summary(ASO_all)
summary(ASO_species_ind)

#rm(ASO_species, ASO_prepared, ASO_sp_matched, ASO_points, ASO_sp_clean, ASO_species_clean, ASO_species_prepared, ASO_prepared_wfo)

#saveRDS(ASO_all, paste0(here::here(),"/data/cache/ASO_all.RDS"))
#saveRDS(ASO_species_ind, paste0(here::here(),"/data/cache/ASO_species_ind.RDS"))


