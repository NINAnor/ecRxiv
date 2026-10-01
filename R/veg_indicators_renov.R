# load packages
library(tidyverse)
library(pwr)
library(lme4)
library(simr)
library(readxl)
library(formattable)
library(sf)
library(tmap)
library(WorldFlora)
library(zen4R)
library(tidylog)
library(rgbif)
library(here)


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# download WFO plant list from zenodo
download_zenodo("https://doi.org/10.5281/zenodo.20782718", files = "_DwC_backbone_R.zip", path = "/Users/francesca.jaroszynsk/OneDrive - NINA/nina_projects/ANO/lowlands/ecRxiv_veg_indicators/data")

wfo_backbone <- read_delim("C:/Users/francesca.jaroszynsk/OneDrive - NINA/nina_projects/ANO/lowlands/ecRxiv_veg_indicators/data/_DwC_backbone_R/classification.csv", delim = "\t")



## 7. Import and prepare datasets

<!--# All the data import and preparation should be done in this chapter, before the analysis chapter. As a minimum, add code for importing data, and describe or show the data structure. You may also explain certain variables, what they represent, and their distributions -->
  
  ### 7.1 Dataset A
  Import ANO testing data:
  First, download from  Miljødirektoratet (downloaded spring 2026)...
```{r, eval = F, echo = F}
# download 
url <- "https://nedlasting.miljodirektoratet.no/naturovervaking/naturovervaking_eksport.gdb.zip"
download(url, dest="P:/41201785_okologisk_tilstand_2022_2023/data/ANO/naturovervaking_eksport.gdb.zip", mode="wb") 
unzip("P:/41201785_okologisk_tilstand_2022_2023/data/ANO/naturovervaking_eksport.gdb.zip", exdir = "P:/41201785_okologisk_tilstand_2022_2023/data/ANO/naturovervaking_eksport.gdb")
```

...then upload from local storage
The geodatabase contains several layers; 'ANO_SurveyPoint' contains site and point data including alien plant species cover.
```{r loadData}
#Sys.setlocale("LC_ALL", "no_NB.utf8") #works with æøå or use "Norwegian"
ANO_sp <- st_read("P:/41201785_okologisk_tilstand_2022_2023/data/ANO/naturovervaking_eksport.gdb",
                 layer="ANO_Art", quiet = T)
ANO_geo <- st_read("P:/41201785_okologisk_tilstand_2022_2023/data/ANO/naturovervaking_eksport.gdb",
                   layer="ANO_SurveyPoint", quiet = T)
#head(ANO.sp)
#head(ANO.geo)

```

### 7.2. Dataset B
Import Reference data from NiN:
  ```{r}
load("P:/41201785_okologisk_tilstand_2022_2023/data/functional plant indicators/reference from NiN/Eco_State.RData")
# str(Eco_State)
```

The generalized species lists underlying the ecosystem categorization in NiN (Halvorsen et al. 2020) represent expert-compiled species lists based on empirical evidence from the literature and expert knowledge of the systems and their species. In these lists, every species is assigned an abundance value on a 6-step scale, with each step representing a range for the ‘expected combination of frequency and cover’ of occurrence

1 = < 1/32

2 = 1/32 - 1/8

3 = 1/8 - 3/8

4 = 3/8 - 4/5

5 = 3/8 - 4/5 + dominance

6 = > 4/5

For the purpose of this project, these steps are simplified to maximum expected combination of frequency and cover, whereby steps 4 & 5 are assigned 0.6 and 0.8, respectively, in order to distinguish between them.



### 7.3. Dataset C
Import plant indicator values from Tyler et al. (2021):
  ```{r}
ind_tyler <- readRDS("P:/41201785_okologisk_tilstand_2022_2023/data/functional plant indicators/ind.Tyler.RDS")
```

The Swedish plant indicator values dataset published by Tyler et al. (2021) contains a large collection of plant indicators based on the Swedish flora, which is well representative of the Norwegian flora as well. From this set, we use indicator data for moisture and Moisture as these are thought to be subject to potential change due to ongoing pressures in the respective ecosystems (see details above under 3.4 'Impact factors').

### 7.4 Dataset D



## 8. Spatial units
The spatial units for functional plant community indicators are governed by the ANO data, which consist of 1000 randomly chosen sites in Norway. Each site is a 500 x 500 m grid cell with 18 monitoring points of 250sqm and a central 1 x 1 m vegetation plot. The basic unit for which observational community data exist, and for which every functional plant community indicator is computed, is this central 1 x 1 m vegetation plot. Aggregation of these basic spatial units to higher ones - like sites, municipalities, counties, regions, or the national level - should be done with appropriate consideration of the spatial structure of the data, i.e. imbalances and clustering (highly unequal number of points of an ecosystem type between sites).

## 9. Analyses

<!--# 
  
  Use this header for documenting the analyses. Put code in separate code chunks, and annotate the code in between using normal text (i.e. between the chunks, and try to avoid too many hashed out comments inside the code chunks). Add subheaders as needed. 

Code folding is activated, meaning the code will be hidden by default in the html (one can click to expand it).

Caching is also activated (from the top YAML), meaning that rendering to html will be quicker the second time you do it. This will create a folder inside you project folder (called INDICATORID_cache). Sometimes caching created problems because some operations are not rerun when they should be rerun. Try deleting the cash folder and try again.

-->
  
  #### Data handling
  - Checking for errors
- Checking species nomenclature in the different species lists to make species and indicator data possible to merge
- Merging indicator data with monitoring data and indicator data with reference data
(not shown here, but documented in the code)

```{r dataHandling, results='hide', warning=F, message=F}

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####

########## STEP ONE: clean indicator species

#### Plant indicator data
ind_tyler <- ind_tyler |>
  rename(scientific_name = Scientific_name) |> 
  mutate(scientific_name_original = scientific_name,
         scientific_name = str_replace_all(scientific_name, "ssp.", "subsp."),         # correct subspecies labelling
         scientific_name = str_replace_all(scientific_name, "\u00EB", "e"),
         scientific_name = str_remove_all(scientific_name, "agg."),                    # remove aggregates
         scientific_name = str_replace_all(scientific_name, " x ", " \u00D7 ")) |>     # correct hybrids labelling
  filter(!is.na(scientific_name), !scientific_name == "") |> 
  distinct(scientific_name_original, scientific_name, Moisture, Nitrogen) |>           # select indicator here
  tibble()

ind_tyler <- ind_tyler |> 
  mutate(
    scientific_name = case_when(
      grepl("Aconitum lycoctonum", scientific_name) ~ "Aconitum septentrionale",
      grepl("Carex simpliciuscula", scientific_name) ~ "Kobresia simpliciuscula",
      grepl("Carex myosuroides", scientific_name) ~ "Kobresia myosuroides",
      grepl("Clinopodium acinos", scientific_name) ~ "Acinos arvensis",
      grepl("Artemisia rupestris", scientific_name) ~ "Artemisia norvegica",
      grepl("Cherleria biflora", scientific_name) ~ "Minuartia biflora",
      grepl("Rosa vosagica", scientific_name) ~ "Rosa vosagiaca",
      TRUE ~ scientific_name
    )
  )

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



### WHAT TO DO WITH SUBSPECIES???
# reconnect subspecies with corresponding species from authorship column
tyler_prepared <- tyler_prepared_wfo |>
  tibble() |> 
  mutate(
    # first word of Authorship
    first_author = if_else(!is.na(Authorship), word(Authorship, 1, 1), ""),
    
    # 1) If first_author looks like a missed subspecies epithet (lowercase),
    #    AND the name is NOT a hybrid, append "subsp. <first_author>"
    spec.name = if_else(
      first_author != "" &
        str_detect(first_author, "^[a-z]") &               # starts lowercase
        !str_detect(spec.name, " x$") &                    # not ending with " x"
        !str_detect(spec.name, "\u00D7$") &                # not ending with "×"
        !str_detect(spec.name, "^\\s*[x\u00D7]\\b"),       # not starting with x/× hybrid marker
      paste(spec.name, "subsp.", first_author),
      spec.name
    ),
    
    # 2) Hybrids: append first_author after trailing " x" or "×"
    spec.name = if_else(
      str_detect(spec.name, " x$"),
      paste0(spec.name, " ", first_author),
      spec.name
    ),
    spec.name = if_else(
      str_detect(spec.name, "\u00D7$"),
      paste0(spec.name, " ", first_author),
      spec.name
    ),
    spec.name = if_else(
      str_detect(Authorship, "^\u00D7"),
      paste0(spec.name, Authorship),
      spec.name
    ),
    
    # 3) Clean "sect." and whitespace
    spec.name = spec.name |>
      str_replace_all("\\bsect\\b\\.?", " ") |>
      str_squish() |> 
      str_to_sentence()
  ) |>
  rename(clean_string = spec.name) |> 
  # unique values in spec.full and clean_string only
  distinct(spec.full, clean_string) |> 
  # create species names without subspecies
  mutate(clean_string_no_sub = str_remove(pattern = "subsp\\..*$", string = clean_string))



# standardise names to the WFO backbone (slow)
tyler_sp_matched <- WFO.match(spec.data = tyler_prepared$clean_string_no_sub,
                             WFO.data = wfo_backbone,
                             Fuzzy = 0.15,
                             Fuzzy.max = 50,
                             Fuzzy.one = FALSE)



# finalise accepted name column according to latest taxonomical nomenclature
tyler_sp_clean <- tyler_sp_matched |>
  # make copy of the original species string
  rename(clean_string_no_sub = spec.name.ORIG) |>
  group_by(clean_string_no_sub) |>
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
tyler_sp_clean |> filter(clean_string_no_sub != accepted_name) #|> view()
tyler_sp_clean |> filter(flag_multiple_suggestions == TRUE, clean_string_no_sub != accepted_name) #|> view()


# bind new species names onto indicator dataset
tyler_species_clean <- left_join(tyler_prepared, tyler_sp_clean, by = "clean_string_no_sub") |> 
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
      clean_string_no_sub == "Heracleum 'kungsholm'" ~ "reverted",
      clean_string_no_sub == "Iris germanica" ~ "edited",
      clean_string_no_sub == "Hedlundia atrata" ~ "reverted",
      clean_string_no_sub == "Hedlundia faohraei" ~ "reverted",
      grepl("Hieracium", clean_string_no_sub) & grepl("sect.", spec.full) ~ "edited",
      is.na(accepted_name) ~ "added",                                                          # fill missing accepted_name with clean_string_no_sub
      TRUE ~ ""
      
    ),
  accepted_name = case_when(
    accepted_name == "Rosa vinodora" ~ "Rosa inodora",
    clean_string_no_sub == "Heracleum 'kungsholm'" ~ clean_string_no_sub,
    #accepted_name == "Salix mollissima" ~ clean_string_no_sub,
    clean_string_no_sub == "Iris germanica" ~ clean_string_no_sub,
    clean_string_no_sub == "Hedlundia atrata" ~ clean_string_no_sub,
    clean_string_no_sub == "Hedlundia faohraei" ~ clean_string_no_sub,
      grepl("Hieracium", clean_string_no_sub) & grepl("sect.", spec.full) ~ paste(word(spec.full, 1, 1), word(spec.full, 3, 3)),
    is.na(accepted_name) ~ clean_string_no_sub,
    TRUE ~ accepted_name
  ))

# check name changes
tyler_species_clean |> filter(clean_string_no_sub != accepted_name) #|> view()


tyler_species_clean |> filter(is.na(accepted_name))


# remove unused dataframes
rm(ind_tyler, tyler_prepared, tyler_prepared_wfo, tyler_sp_clean, tyler_sp_matched)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

###### STEP 2: clean GRUK, ANO, ASO and NiN species
# 2.1 GRUK

# read in data
GRUK_species <- read_excel("P:/41201785_okologisk_tilstand_2022_2023/data/GRUK/GRUK_alle_artsdata_2020-24.xlsx", sheet="Arter i ruter")
GRUK_ruter <- read_excel("P:/41201785_okologisk_tilstand_2022_2023/data/GRUK/GRUK_alle_artsdata_2020-24.xlsx", sheet="Ruter")
GRUK_sirkler <- read_excel("P:/41201785_okologisk_tilstand_2022_2023/data/GRUK/GRUK_alle_artsdata_2020-24.xlsx", sheet="Sirkler")
GRUK_polygoner <- read_excel("P:/41201785_okologisk_tilstand_2022_2023/data/GRUK/GRUK_alle_artsdata_2020-24.xlsx", sheet="Polygoner")


## 2.1.1 GRUK species data handling
GRUK_species <- GRUK_species |> 
  janitor::clean_names() |> 
  rename(scientific_name = latinsk_navn,
         art_dekning = dekning_percent)


# fix species names
GRUK_species <- GRUK_species |>
  mutate(
    scientific_name_original = scientific_name,
    scientific_name = str_replace_all(scientific_name, "ssp\\.", "subsp."),
    scientific_name = str_replace_all(scientific_name, " x ", " \u00D7 ") |> 
      str_squish()
  ) |>
  filter(!is.na(scientific_name), scientific_name != "") |>
  tibble()

# update species names
GRUK_species <- GRUK_species |> 
  mutate(scientific_name = recode(
    scientific_name,
    "Arabis wahlenbergii"       = "Arabis hirsuta",
    "Carex paupercula"          = "Carex magellanica",
    "Carex viridula"            = "Carex flava",
    "Cotoneaster scandinavicus" = "Cotoneaster integerrimus",
    "Cotoneaster symondsii"     = "Cotoneaster simonsii",
    "Cyanus montanus"           = "Centaurea montana",
    "Erysimum virgatum"         = "Erysimum strictum",
    "Festuca trachyphylla"      = "Festuca brevipila",
    "Galium album"              = "Galium mollugo",
    "Helictotrichon pratense"   = "Avenula pratensis",
    "Helictotrichon pubescens"  = "Avenula pubescens",
    "Hieracium murorum"         = "Hieracium Hieracium",
    "Hieracium vulgatum"        = "Hieracium Vulgata",
    "Hylotelephium maximum"     = "Sedum telephium",
    "Lepidotheca suaveolens"    = "Matricaria discoidea",
    "Malus ×domestica"          = "Malus domestica",
    "Pilosella peletariana"     = "Pilosella officinarum",
    "Poa angustifolia"          = "Poa pratensis",
    "Poa humilis"               = "Poa pratensis",
    "Rosa dumalis"              = "Rosa vosagiaca",
    "Sorbus hybrida"            = "Hedlundia hybrida",
    "Spergularia salina"        = "Spergularia marina",
    "Trifolium pallidum"        = "Trifolium pratense"
  )
  )


GRUK_prepared_wfo <- WFO.prepare(GRUK_species$scientific_name)

GRUK_prepared <- GRUK_prepared_wfo |>
  mutate(
    subspecies_epithet = if_else(!is.na(Authorship), word(Authorship, 1, 1), ""),
    
    # Attach subspecies epithet only if:
    #   - we have an epithet
    #   - AND Authorship does not contain "agg."
    spec.name = if_else(
      subspecies_epithet != "" & !str_detect(Authorship, "\\bagg\\.?"),
      paste(spec.name, "subsp.", subspecies_epithet),
      spec.name
    ),
   # # attach agg. info
   # spec.name = if_else(
   #   str_detect(Authorship, "\\bagg\\.?") & !str_detect(spec.name, "\\bagg\\.?"),
   #   paste(spec.name, subspecies_epithet),  # just add "agg." as a trailing word
   #   spec.name
   # ),
    
    # Handle hybrids / ×
    spec.name = if_else(grepl(" x$", spec.name), paste(spec.name, subspecies_epithet), spec.name),
    spec.name = if_else(grepl("\u00D7$", spec.name), paste(spec.name, subspecies_epithet), spec.name),
    spec.name = if_else(grepl("^\u00D7", Authorship), paste(spec.name, subspecies_epithet), spec.name),
    
    # Clean out 'sect.'
    spec.name = spec.name |>
      str_replace_all("\\bsect\\b\\.?", " ") |>
      str_squish() |> 
     str_to_sentence()
  ) |>
  rename(clean_string = spec.name) |>
  # create species names without subspecies
  mutate(clean_string_no_sub = str_remove(pattern = "subsp\\..*$", string = clean_string)) |> 
  distinct(spec.full, clean_string_no_sub) 



# standardise names to the WFO backbone
GRUK_sp_matched <- WFO.match(spec.data = GRUK_prepared$clean_string_no_sub,
                             WFO.data = wfo_backbone,
                             Fuzzy = 0.15,
                             Fuzzy.max = 50,
                             Fuzzy.one = FALSE)


# create accepted name column according to latest taxonomical nomenclature
GRUK_sp_clean <- GRUK_sp_matched |>
  # First, store the original string clearly
  rename(clean_string_no_sub = spec.name.ORIG) |>
  group_by(clean_string_no_sub) |>
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
GRUK_sp_clean |> filter(clean_string_no_sub != accepted_name)
GRUK_sp_clean |> filter(flag_multiple_suggestions == TRUE, clean_string_no_sub != accepted_name)


# correct incorrect corrections. haha
GRUK_sp_clean <- GRUK_sp_clean |> 
  mutate(
    flag_species_revert =
      case_when(
        clean_string_no_sub == "Hieracium vulgata" ~ "added",
        clean_string_no_sub == "Potentilla anserina" ~ "added",
        TRUE ~ ""
        
      ),
    accepted_name = case_when(
        clean_string_no_sub == "Hieracium vulgata" ~ "Hieracium Vulgata",
        clean_string_no_sub == "Potentilla anserina" ~ "Argentina anserina",
      TRUE ~ accepted_name
    ))



# bind new species names onto original dataset
GRUK_species_clean <- left_join(GRUK_prepared, GRUK_sp_clean, by = "clean_string_no_sub") |> 
  full_join(GRUK_species, by = join_by(spec.full == scientific_name)) |> 
  # add "sp." back onto genus-level identifications
  mutate(accepted_name = case_when(
    is.na(word(accepted_name, 2)) ~ paste(accepted_name, "sp."),
    TRUE ~ accepted_name
    )) |> 
  # filter out sect. species and subspecies
  #filter(!grepl("subsp.", scientific_name_original)) |> 
  distinct()


# check for original species with no matched accepted name.
GRUK_species_clean |> 
  filter(is.na(accepted_name)) |> 
  tibble()
# none.




# merge species data with indicators

GRUK_species_ind <- GRUK_species_clean |>
  select(accepted_name, art_dekning, ParentGlobalID, PolygonID, RuteID) |> 
  left_join(tyler_species_clean |> 
              select(accepted_name, Moisture, Nitrogen)) |> 
  tibble()


summary(GRUK_species_ind)

# checking which species didn't find a match
unique(GRUK_species_ind[is.na(GRUK_species_ind$Moisture & 
                                is.na(GRUK_species_ind$Nitrogen)),'accepted_name'])




#########################################################
## GRUK ruter data handling
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


## GRUK sirkler data handling


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


#remove 
## merge information on condition and quality from GRUK.polygoner into GRUK.variables
# transform GRUK.variables into spatial object
GRUK_variables <- st_as_sf(GRUK_variables, coords = c("UTM33_E","UTM33_N"), remove = FALSE, crs = 25833)


## GRUK polygoner data handling
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
GRUK_variables <- st_drop_geometry(GRUK_variables)
names(GRUK_variables)
names(GRUK_polygoner)


GRUK_variables <- GRUK_variables |> 
  left_join(GRUK_polygoner, by = c("year", "polygon_id")) # there are 94 rows only found in GRUK_polygoner


summary(GRUK_variables) 
summary(as.factor(GRUK_variables$tilstand)) # no unexpected NA's


# edit the column names
GRUK_variables <- GRUK_variables |> 
  rename(erosion_percent = spor_etter_slitasje_og_slitasjebetinget_erosjon)



## adding information on ecosystem and condition variables to species+indicator data
names(GRUK_species_ind)
GRUK_species_ind <- GRUK_species_ind |> 
  rename(polygon_id  = PolygonID,
         rute_id = RuteID)


names(GRUK_variables)

GRUK_all <- GRUK_species_ind |> 
  left_join(GRUK_variables |>  select(-polygon_id, -rute_id), by = join_by(ParentGlobalID == global_id))



# fixing variable types
GRUK_all <- GRUK_all |> 
  mutate(across(
    c(accepted_name, kartleggingsenhet_1_5000, lokalitetskvalitet, kommune, tilstand, naturmangfold, nin_kartleggingsenheter),
    as.factor
  ),
  across(
    c(areal_m2, dekning_nakent_berg, dekning_problemarter),
    as.numeric
  )) |> 
  rename(species = accepted_name) |> 
# trimming away the points without information on NiN, species or cover  
    filter(!is.na(species), !is.na(art_dekning))

summary(GRUK_all)


#rm(GRUK_polygoner, GRUK_prepared, GRUK_prepared_wfo, GRUK_ruter, GRUK_sirkler, GRUK_sp_clean, GRUK_sp_matched, GRUK_species, GRUK_species_clean, GRUK_species_ind, GRUK_variables)


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 


# ASO dataset


# GBIF query
#aso_ds <- dataset_search(doi = "10.15468/gq6wa5")
#
#aso_ds$data |>
#  select(title, datasetKey, publishingOrganizationTitle)
#
## extract dataset key
#aso_key <- aso_ds$data$datasetKey[1]
#
## Request GBIF download
#occ <- occ_search(datasetKey = aso_key, limit = 20000)
#
#ASO_species <- occ$data


# no abundance data. Falling back onto the available abundance data from 2022 only

ASO_species <- read_excel("P:/41201785_okologisk_tilstand_2022_2023/data/ASO/Semi-naturlig_eng_S123_2022.xlsx", sheet = "transektregistreringer_4")

ASO_points <- read_excel("P:/41201785_okologisk_tilstand_2022_2023/data/ASO/Semi-naturlig_eng_S123_2022.xlsx", sheet = "surveyPoint_0")


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
  )



## fixing variable names and issues in ASO.sp
head(as.data.frame(ASO_species))

ASO_species <- rename(ASO_species, art_dekning = Dekning)

# fix species names
ASO_species <- ASO_species |>
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
    
    scientific_name_no_sub = if_else(
      is.na(word(scientific_name, 2)),        # only one word (no species epithet)
      word(scientific_name, 1),              # just genus
      word(scientific_name, 1, 2)            # genus + species
    ) %>%
      str_replace_all("-", " ")  |> 
      str_squish()
  )

ASO_species <- ASO_species  |>  
  mutate(scientific_name_no_sub = recode(
    scientific_name_no_sub,
    "Cardamine dentata"           = "Cardamine pratensis",
    "Chamaepericlymenum suecicum" = "Cornus suecica",
    "Chamerion angustifolium"     = "Chamaenerion angustifolium",
    "Cicerbita alpina"            = "Lactuca alpina",
    "Galium album"                = "Galium mollugo",
    "Helictotrichon pratense"     = "Avenula pratensis",
    "Helictotrichon pubescens"    = "Avenula pubescens",
    "Hieracium murorum"           = "Hieracium Vulgata",
    "Hieracium vulgatum"          = "Hieracium umbellatum",
    "Hylotelephium maximum"       = "Hylotelephium telephium",
    "Listera cordata"             = "Neottia cordata",
    "Listera ovata"               = "Neottia ovata",
    "Omalotheca norvegica"        = "Gnaphalium norvegicum",
    "Omalotheca sylvatica"        = "Gnaphalium sylvaticum",
    "Oreopteris limbosperma"      = "Thelypteris limbosperma",
    "Potentilla anserina"         = "Argentina anserina",
    "Rosa dumalis"                = "Rosa vosagiaca",
    "Rubus fruticosus"            = "Rubus plicatus",
    "Rumex alpestris"             = "Rumex acetosa"
  ))


# filter out NAs for WFO cleaning
ASO_species_prepared <- filter(ASO_species, !is.na(scientific_name_no_sub))


ASO_prepared_wfo <- WFO.prepare(ASO_species_prepared$scientific_name_no_sub)

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
  full_join(ASO_species, by = join_by(spec.full == scientific_name_no_sub)) |> 
  distinct()


# check for original species with no matched accepted name.
ASO_species_clean |> 
  filter(is.na(accepted_name)) |> 
  tibble()

# there are some norwegian names with no scientific name associated.


## merge species data with indicators
ASO_species_ind <- ASO_species_clean |>
  select(accepted_name, art_dekning, ParentGlobalID) |> 
  left_join(tyler_species_clean |> 
              select(accepted_name, Moisture, Nitrogen)) |> 
  tibble()



# checking which species didn't find a match
unique(ASO_species_ind[is.na(ASO_species_ind$Moisture & 
                                is.na(ASO_species_ind$Nitrogen)),'accepted_name'])



## adding information on ecosystem and condition variables to species data
ASO_all <- ASO_species_ind |> 
  full_join(ASO_points |>  
              select(global_id, omradenummer_flatenummer, eng_id, aso_id, nin_grunntype), 
            by = join_by(ParentGlobalID == global_id))



# fixing variable types
ASO_all <- ASO_all |> 
  mutate(across(
    c(accepted_name, nin_grunntype, omradenummer_flatenummer, eng_id, aso_id),
    as.factor
  )) |> 
  rename(species = accepted_name) |> 
  # trimming away the points without information on NiN, species or cover  
  filter(!is.na(species), !is.na(art_dekning), !is.na(nin_grunntype))

summary(ASO_all)




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 


#### ANO monitoring data


# load in ano data
ano_species <- st_read("P:/41201785_okologisk_tilstand_2022_2023/data/ANO/naturovervaking_eksport.gdb",
                  layer="ANO_Art", quiet = T)
ano_geo <- st_read("P:/41201785_okologisk_tilstand_2022_2023/data/ANO/naturovervaking_eksport.gdb",
                   layer="ANO_SurveyPoint", quiet = T)

# extract species data
ano_species <- ano_species |> 
  mutate(scientific_name = str_replace_all(art_navn, "_", " "),
         scientific_name_original = scientific_name,
         scientific_name = str_replace_all(scientific_name, "ssp.", "subsp."),         # correct subspecies labelling
         scientific_name = str_replace_all(scientific_name, "\u00EB", "e"),
         scientific_name = str_remove_all(scientific_name, "agg."),                    # remove aggregates
         scientific_name = str_replace_all(scientific_name, " x ", " \u00D7 "),     # correct hybrids labelling
         scientific_name = str_to_sentence(scientific_name),
         scientific_name_no_sub = if_else(
           is.na(word(scientific_name, 2)),        # only one word (no species epithet)
           word(scientific_name, 1),              # just genus
           word(scientific_name, 1, 2)            # genus + species
           ) |> 
           #str_replace_all("-", " ")  |> 
           str_squish()
         ) |> 
  filter(!is.na(scientific_name), !scientific_name == "") |> 
  tibble()

ano_prepared_wfo <- ano_species |> 
  distinct(scientific_name_no_sub, scientific_name_original)


# recode species names for WFO matching
ano_prepared_wfo <- ano_prepared_wfo |> 
  mutate(spec.name = recode(
    spec.name,
    "Agrostis hyemalis"              = "Agrostis scabra",
    "Antennaria lapponica"           = "Antennaria alpina",
    "Antennaria porsildii"           = "Antennaria alpina",
    "Arctous alpinus"                = "Arctous alpina",
    "Betula tortuosa"                = "Betula pubescens",
    "Blysmopsis rufa"                = "Blysmus rufus",
    "Cardamine nymanii"              = "Cardamine pratensis",
    "Carex adelostoma"               = "Carex buxbaumii",
    "Carex concolor"                 = "Carex aquatilis",
    "Carex leersii"                  = "Carex echinata",
    "Carex myosuroides"              = "Kobresia myosuroides",
    "Carex paupercula"               = "Carex magellanica",
    "Carex simpliciuscula"           = "Kobresia simpliciuscula",
    "Carex viridula"                 = "Carex flava",
    "Chamaepericlymenum suecicum"    = "Cornus suecia",
    "Cicerbita alpina"               = "Lactuca alpina",
    "Cornus suecia"                  = "Cornus suecica",
    "Cotoneaster scandinavicus"      = "Cotoneaster integerrimus",
    "Dactylorhiza viridis"           = "Coeloglossum viride",
    "Diphasiastrum alpinum"          = "Lycopodium alpinum",
    "Diphasiastrum complanatum"      = "Lycopodium complanatum",
    "Dryopteris affinis"             = "Dryopteris filix-mas",
    "Empetrum hermaphroditum"        = "Empetrum nigrum",
    "Elymus alaskanus"               = "Elymus kronokensis",
    "Festuca prolifera"              = "Festuca rubra",
    "Galium album"                   = "Galium mollugo",
    "Galium elongatum"               = "Galium palustre",
    "Helictotrichon pratense"        = "Avenula pratensis",
    "Helictotrichon pubescens"       = "Avenula pubescens",
    "Hieracium alpina"               = "Hieracium Alpina",
    "Hieracium alpinum"              = "Hieracium Alpina",
    "Hieracium hieracium"            = "Hieracium Hieracium",
    "Hieracium hieracioides"         = "Hieracium umbellatum",
    "Hieracium murorum"              = "Hieracium Vulgata",
    "Hieracium oreadea"              = "Hieracium Oreadea",
    "Hieracium prenanthoidea"        = "Hieracium Prenanthoidea",
    "Hieracium vulgata"              = "Hieracium Vulgata",
    "Hieracium pilosella"            = "Pilosella officinarum",
    "Hieracium vulgatum"             = "Hieracium umbellatum",
    "Hierochloã« alpina"             = "Hierochloë alpina",
    "Hierochloã« hirta"              = "Hierochloë hirta",
    "Hierochloã« odorata"            = "Hierochloë odorata",
    "Huperzia appressa"              = "Huperzia selago",
    "Huperzia arctica"               = "Huperzia selago",
    "Hylotelephium maximum"          = "Sedum telephium",
    "Listera cordata"                = "Neottia cordata",
    "Leontodon autumnalis"           = "Scorzoneroides autumnalis",
    "Loiseleuria procumbens"         = "Kalmia procumbens",
    "Minuartia rubella"              = "Sabulina rubella",
    "Minuartia stricta"              = "Sabulina stricta",
    "Mycelis muralis"                = "Lactuca muralis",
    "Omalotheca supina"              = "Gnaphalium supinum",
    "Omalotheca norvegica"           = "Gnaphalium norvegicum",
    "Omalotheca sylvatica"           = "Gnaphalium sylvaticum",
    "Oreopteris limbosperma"         = "Thelypteris limbosperma",
    "Oxycoccus microcarpus"          = "Vaccinium microcarpum",
    "Oxycoccus palustris"            = "Vaccinium oxycoccos",
    "Phalaris minor"                 = "Phalaris arundinacea",
    "Pinus unicinata"                = "Pinus mugo",
    "Poa alpigena"                   = "Poa pratensis",
    "Poa angustifolia"               = "Poa pratensis",
    "Poa ×jemtlandica"               = "Poa alpina",
    "Potentilla anserina"            = "Argentina anserina",
    "Potentilla arenosa"             = "Potentilla nivea",
    "Pyrola grandiflora"             = "Pyrola rotundifolia",
    "Rubus fruticosus"               = "Rubus plicatus",
    "Rumex alpestris"               = "Rumex acetosa",
    "Stellaria uliginosa"            = "Stellaria alsine",
    "Syringa emodi"                  = "Syringa vulgaris",
    "Taraxacum crocea"               = "Taraxacum officinale",
    "Taraxacum croceum"              = "Taraxacum officinale",
    "Trientalis europaea"           = "Lysimachia europaea",
    "Trifolium pallidum"            = "Trifolium pratense",
    "Veratrum lobelianum"           = "Veratrum album"
  )
)

# prepare dataset for WFO matching
ano_prepared_wfo <- WFO.prepare(ano_prepared_wfo$scientific_name_no_sub)

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



# correct incorrect corrections. haha
ano_sp_clean <- ano_sp_clean |> 
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
ano_sp_clean |> filter(flag_multiple_suggestions == TRUE, clean_string != accepted_name) #|> view()



# bind new species names onto indicator dataset
ano_species_clean <- left_join(ano_prepared, ano_sp_clean, by = "clean_string") |> 
  full_join(ano_species, by = join_by(spec.full == scientific_name_no_sub)) |> 
  # filter out sect. species and subspecies
  #filter(!grepl("subsp.", scientific_name_original)) |> 
  distinct()

ano_species_clean |> filter(is.na(accepted_name))




## fix NiN information
ano_geo <- ano_geo |>
  mutate(
    # 3 first characters of kartleggingsenhet_1m2, no hyphen
    hovedtype_rute = substr(kartleggingsenhet_1m2, 1, 3),
    hovedtype_rute = gsub("-", "", hovedtype_rute),
    
    # recode to hovedøkosystem
    hovedoekosystem_rute = recode(
      hovedtype_rute,
      "T4"  = "Forest", "T30" = "Forest",
      "T3"  = "Mountain", "T7"  = "Mountain", "T14" = "Mountain", "T22" = "Mountain",
      "V1"  = "Wetland", "V2"  = "Wetland", "V3"  = "Wetland", "V4"  = "Wetland", "V5"  = "Wetland", "V6"  = "Wetland", 
      "V7"  = "Wetland", "V8"  = "Wetland",
      "T31" = "Seminat", "T32" = "Seminat", "T33" = "Seminat", "T34" = "Seminat", "V9"  = "Seminat", "V10" = "Seminat",
      "T2"  = "Natopen", "T8"  = "Natopen", "T11" = "Natopen", "T12" = "Natopen", "T13" = "Natopen", "T15" = "Natopen", 
      "T16" = "Natopen", "T18" = "Natopen", "T21" = "Natopen", "T24" = "Natopen", "T29" = "Natopen"
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
ano_lowlands <- ano_geo |>
  filter(hovedoekosystem_rute %in% c("Natopen", "Seminat"))

## 5. Read Norway / regions, join to open lowlands points

nor <- st_read("P:/41201785_okologisk_tilstand_2022_2023/gjengroing/R_project/DATA/outlineOfNorway_EPSG25833.shp", quiet = TRUE) |>
  st_as_sf() |>
  st_transform(crs = st_crs(ano_lowlands))

reg <- st_read("P:/41201785_okologisk_tilstand_2022_2023/gjengroing/R_project/DATA/regions.shp", quiet = TRUE) |>
  st_as_sf() |>
  st_transform(crs = st_crs(ano_lowlands))

# update region names
reg$region <- c(
  "Northern_Norway",
  "Central_Norway",
  "Eastern_Norway",
  "Western_Norway",
  "Southern_Norway"
)

regnor <- st_intersection(reg, nor)

ano_lowlands <- st_join(
  ano_lowlands,
  regnor,
  left = TRUE,
  join = st_nearest_feature
)




## merge species data with indicators
ANO_species_ind <- ano_species_clean |>
  select(accepted_name, art_dekning, parentglobalid) |> 
  left_join(tyler_species_clean |> 
              select(accepted_name, Moisture, Nitrogen)) |> 
  tibble()



# checking which species didn't find a match
unique(ANO_species_ind[is.na(ANO_species_ind$Moisture & 
                               is.na(ANO_species_ind$Nitrogen)),'accepted_name'])



# fixing variable types
ANO_species_ind <- ANO_species_ind |> 
  mutate(across(
    c(accepted_name),
    as.factor
  )) |> 
  rename(species = accepted_name)# |> 
  # trimming away the points without information on NiN, species or cover  
  #filter(!is.na(species), !is.na(art_dekning))

summary(ANO_species_ind)




#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

#### reference data - data handling

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
    species_original   = as_factor(as.vector(Eco_State$Concept_Data$Species$Species_List$species)),
    species_group = as_factor(as.vector(Eco_State$Concept_Data$Species$Species_List$art.code)),
    # only genus + species
    species = word(species_original, 1, 2),
    species = str_to_sentence(species)
    # if needed later: filter out groups, e.g. trees
    # |> filter(spgr != "a1a")
  ) |>
  pivot_longer(
    cols = -c(species, species_group, species_original),     # columns to pivot
    names_to   = "nin_id",                                   # new column for former column names
    values_to  = "cover"                                     # new column for values
  ) |> 
  filter(!species_group %in% c("a2m", "a2lb", "a2l"))                             # filter out moss species



### Environment data 

NiN_env <- Eco_State$Concept_Data$Env$Env_Data

NiN_sp <- NiN_sp |> 
  left_join(NiN_env, 
                    by = join_by(nin_id == ID)) |> 
  filter(Nature_Type %in% c("Semi_Natural ", "Coastal_Heath"))  # filter for the desired nature type


NiN_sp <- NiN_sp |> 
  mutate(species = recode(
    species,
    "Aconitum lycoctonum"           = "Aconitum septentrionale",
    "Anagallis arvensis"           = "Lysimachia arvensis",
    "Anagallis minima"             = "Lysimachia minima",
    "Arctous alpinus"              = "Arctous alpina",
    "Betula tortuosa"              = "Betula pubescens",
    "Blysmopsis rufa"              = "Blysmus rufus",
    "Chamerion angustifolium"      = "Chamaenerion angustifolium",
    "Cardamine nymanii"            = "Cardamine pratensis",
    "Carex adelostoma"             = "Carex buxbaumii",
    "Carex leersii"                = "Carex echinata",
    "Carex paupercula"             = "Carex magellanica",
    "Carex simpliciuscula"         = "Kobresia simpliciuscula",
    "Carex _vacillans"             = "Carex vacillans",
    "Carex viridula"               = "Carex flava",
    "Chamaepericlymenum suecicum"  = "Cornus suecia",
    "Cornus suecia"                = "Cornus suecica",
    "Cicerbita alpina"             = "Lactuca alpina",
    "Dactylorhiza fuchsii"         = "Dactylorhiza maculata",
    "Dactylorhiza sphagnicola"     = "Dactylorhiza majalis",
    "Diphasiastrum alpinum"        = "Lycopodium alpinum",
    "Diphasiastrum complanatum"    = "Lycopodium complanatum",
    "Elymus alaskanus"             = "Elymus kronokensis",
    "Empetrum hermaphroditum"      = "Empetrum nigrum",
    "Erigeron acer"                = "Erigeron acris",
    "Erigeron eriocephalus"        = "Erigeron uniflorus",
    "Festuca altissima"            = "Drymochloa sylvatica",
    "Festuca prolifera"            = "Festuca rubra",
    "Galium album"                 = "Galium mollugo",
    "Galium elongatum"             = "Galium palustre",
    "Glaux maritima"               = "Lysimachia maritima",
    "Helictotrichon pratense"      = "Avenula pratensis",
    "Helictotrichon pubescens"     = "Avenula pubescens",
    "Hieracium alpina"             = "Hieracium Alpina",
    "Hieracium alpinum"            = "Hieracium Alpina",
    "Hieracium aurantiacum"        = "Pilosella aurantiaca",
    "Hieracium dovrense"           = "Hieracium Alpestria",
    "Hieracium hieracium"          = "Hieracium Hieracium",
    "Hieracium hieracioides"       = "Hieracium umbellatum",
    "Hieracium lactucella"         = "Pilosella lactucella",
    "Hieracium murorum"            = "Hieracium Vulgata",
    "Hieracium oreadea"            = "Hieracium Oreadea",
    "Hieracium prenanthoidea"      = "Hieracium Prenanthoidea",
    "Hieracium vulgata"            = "Hieracium Vulgata",
    "Hieracium pilosella"          = "Pilosella officinarum",
    "Hieracium vulgatum"           = "Hieracium umbellatum",
    "Hierochloã« alpina"           = "Hierochloë alpina",
    "Hierochloã« hirta"            = "Hierochloë hirta",
    "Hierochlo\x95 hirta"          = "Hierochloë hirta",
    "Hierochloã« odorata"          = "Hierochloë odorata",
    "Huperzia appressa"            = "Huperzia selago",
    "Hylotelephium maximum"        = "Hylotelephium telephium",
    "Lappula myosotis"             = "Lappula squarrosa",
    "Lepidotheca suaveolens"       = "Matricaria discoidea",
    "Listera cordata"              = "Neottia cordata",
    "Listera ovata"                = "Neottia ovata",
    "Leontodon autumnalis"         = "Scorzoneroides autumnalis",
    "Loiseleuria procumbens"       = "Kalmia procumbens",
    "Logfia arvensis"              = "Filago arvensis",
    "Mentha _verticillata"         = "Mentha verticillata",
    "Minuartia rubella"            = "Sabulina rubella",
    "Minuartia stricta"            = "Sabulina stricta",
    "Mycelis muralis"              = "Lactuca muralis",
    "Omalotheca supina"            = "Gnaphalium supinum",
    "Omalotheca norvegica"         = "Gnaphalium norvegicum",
    "Omalotheca sylvatica"         = "Gnaphalium sylvaticum",
    "Ononis arvensis"              = "Ononis spinosa",
    "Oreopteris limbosperma"       = "Thelypteris limbosperma",
    "Oxycoccus microcarpus"        = "Vaccinium microcarpum",
    "Oxycoccus palustris"          = "Vaccinium oxycoccos",
    "Phalaris minor"               = "Phalaris arundinacea",
    "Phalaroides arundinacea"      = "Phalaris arundinacea",
    "Pinus unicinata"              = "Pinus mugo",
    "Platanthera montana"          = "Platanthera chlorantha",
    "Poa alpigena"                 = "Poa pratensis",
    "Poa angustifolia"             = "Poa pratensis",
    "Poa laxa"                     = "Poa flexuosa",
    "Poa _herjedalica"             = "Poa herjedalica",
    "Poa _jemtlandica"             = "Poa alpina",
    "Poa lindebergii"              = "Poa arctica",
    "Potentilla anserina"          = "Argentina anserina",
    "Pyrola grandiflora"           = "Pyrola rotundifolia",
    "Rhamnus catharticus"          = "Rhamnus cathartica",
    "Rumex alpestris"              = "Rumex acetosa",
    "Salix _fragilis"              = "Salix fragilis",
    "Saxifraga _opdalensis"        = "Saxifraga opdalensis",
    "Sorbus hybrida"               = "Hedlundia hybrida",
    "Spergularia salina"           = "Spergularia marina",
    "Syringa emodi"                = "Syringa vulgaris",
    "Taraxacum crocea"             = "Taraxacum officinale",
    "Taraxacum croceum"            = "Taraxacum officinale",
    "Taraxacum erythrospermum"     = "Taraxacum officinale",
    "Taraxacum hamatum"            = "Taraxacum officinale",
    "Trientalis europaea"         = "Lysimachia europaea",
    "Trifolium pallidum"          = "Trifolium pratense",
    "Vicia orobus"                = "Vicia cassubica"
  ),
  species = str_replace(species, "Hierochlo.? hirta", "Hierochloë hirta")
    # and similar for other bad names
  )

nin_prepared_wfo <- NiN_sp |> 
  distinct(species, species_original)

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




# Merge with indicator values
nin_sp_ind <- nin_species_clean |>
  left_join(tyler_species_clean |> 
              select(accepted_name, Moisture, Nitrogen),
            by = join_by(accepted_name)) |> 
  tibble()


# double check species matching
nin_sp_ind |> filter(is.na(accepted_name))  
nin_sp_ind |> filter(is.na(Moisture)) |>  distinct(accepted_name)  # some species to sort here.


coverscale <- tibble(
  orig = 0:6,
  cov  = c(0, 1/32, 1/8, 3/8, 0.6, 4/5, 1)
)


### Translate abundance classes to %-cover
nin_sp_ind <- nin_sp_ind |>
  mutate(
    cover = case_match(
      cover,           
      0 ~ 0,
      1 ~ 1/32,
      2 ~ 1/8,
      3 ~ 3/8,
      4 ~ 0.6,
      5 ~ 4/5,
      6 ~ 1,
      .default = NA_real_   
    )
  )
```

This leaves us with the monitoring data including plant indicators (ano_lowlands) and the reference data including plant indicators (nin_sp_ind):
  
  <!-- Print head of data set with horizontal scrolling -->
  ```{r ano-sp-tab, echo=F}
head(ANO.sp.ind) |>
  kable("html",caption = "ANO species occurrence data set with attached plant trait data.") |> 
  kable_styling("striped") |> scroll_box(width = "100%")
```

```{r wetland_mountain_forest_seminatural-ref-data-tab, echo=FALSE}
head(NiN.forest.cov) |> 
  kable("html", caption = "Reference data set for semi-natural ecosystems") |> 
  kable_styling("striped") |> scroll_box(width = "100%")
```

<br />
  
  For each ecosystem type with a NiN species list, we can calculate a community weighted mean (CWM) for the relevant functional plant indicators.
In order to get distributions of these metrics rather than one single value (for comparison with the empirical testing data) the NiN lists can be bootstrapped.

<br />
  
  ##### Bootstrap functions for frequency abundance and cumulative density
  - function to calculate community weighted means (indBoot.freq() ) or cumulative density (indBoot.HeatOverhang() ) of selected indicator values (ind)
- for species lists (sp) with given abundances in percent (or on a scale from 0 to 1) in one or more 'sites' (abun)
- with a given number of iterations (iter),
- with species given a certain minimum abundance occurring in all bootstraps (obl), and
- with a given re-sampling ratio of the original species list (rat)
- in every bootstrap iteration the abundance of the sampled species can be randomly changed by a limited amount if wished by introducing a re-sampling of abundance values from adjacent abundance steps with a certain probability (var.abun)

```{r bootstrapping}



indBoot_freq <- function(df_long, indicators, iter, obl, rat = 2/3, var.abun = FALSE) {
  # df_long   : tibble/data.frame with columns:
  #   accepted_name : species
  #   nin_id        : ecosystem / NiN type
  #   cover         : abundance (cover proportion 0–1)
  #   ...           : other columns, including indicators
  # indicators: character vector of column names to use as indicators
  # iter      : number of bootstrap iterations
  # obl       : abundance threshold for "obligate" species (on cover)
  # rat       : proportion of non-obligate species to sample
  # var.abun  : whether to vary cover via coverscale (optional)
  
  df_long <- as_tibble(df_long)
  
  # basic checks for required columns
  stopifnot(all(c("accepted_name", "nin_id", "cover") %in% colnames(df_long)))
  stopifnot(all(indicators %in% colnames(df_long)))
  
  indicator_cols <- indicators
  n_indicators   <- length(indicator_cols)
  nin_ids        <- df_long$nin_id %>% unique() %>% sort()
  
  # optional abundance (cover) variation setup
  if (var.abun) {
    prob_list <- list(
      c(0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0),
      c(0.2, 0.3, 0.5, 0.0, 0.0, 0.0, 0.0),
      c(0.0, 0.2, 0.3, 0.5, 0.0, 0.0, 0.0),
      c(0.0, 0.0, 0.2, 0.3, 0.5, 0.0, 0.0),
      c(0.0, 0.0, 0.0, 0.2, 0.3, 0.5, 0.0),
      c(0.0, 0.0, 0.0, 0.0, 0.2, 0.3, 0.5)
    )
  }
  
  vary_cover <- function(cover_vec) {
    dat_b <- tibble(cover = cover_vec)
    for (m in seq_len(nrow(coverscale) - 1)) {
      target_cov <- coverscale$cov[m + 1]
      idx_class  <- dat_b$cover == target_cov
      if (any(idx_class)) {
        n_class <- sum(idx_class)
        dat_b$cover[idx_class] <- sample(
          c(0.01, coverscale$cov[2:7]),
          size    = n_class,
          replace = TRUE,
          prob    = prob_list[[m]]
        )
      }
    }
    dat_b <- dat_b %>%
      mutate(
        cover = case_when(
          !is.na(cover) & cover <= 0 ~ 0.01,
          !is.na(cover) & cover >  1 ~ 1,
          TRUE                       ~ cover
        )
      )
    dat_b$cover
  }
  
  boot_one_nin <- function(nin_df) {
    # nin_df: rows for one NiN type, columns: accepted_name, nin_id, cover + indicators
    
    # only species present & with indicator values
    nin_df <- nin_df %>%
      filter(
        cover > 0,
        !if_any(all_of(indicator_cols), is.na)
      )
    
    if (nrow(nin_df) == 0) {
      return(
        map(
          seq_len(n_indicators),
          ~ matrix(NA_real_, nrow = iter, ncol = 1) %>% as.data.frame()
        )
      )
    }
    
    obligate_idx   <- nin_df$cover >= obl
    non_obligate   <- nin_df$accepted_name[!obligate_idx]
    n_obligate     <- sum(obligate_idx)
    n_non_obligate <- length(non_obligate)
    
    n_sample <- round((nrow(nin_df) - n_obligate) * rat, 0)
    
    iter_results <- map(seq_len(iter), function(i) {
      speciesSample <- if (n_sample > 0) {
        sample(non_obligate, size = n_sample, replace = FALSE)
      } else {
        character(0)
      }
      
      nin_b <- bind_rows(
        nin_df %>% filter(obligate_idx),
        nin_df %>% filter(accepted_name %in% speciesSample)
      )
      
      if (var.abun) {
        nin_b <- nin_b %>%
          mutate(cover = vary_cover(cover))
      }
      
      map(indicator_cols, function(ind_name) {
        ind_col <- nin_b[[ind_name]]
        valid   <- !is.na(nin_b$cover) & !is.na(ind_col)
        
        if (nrow(nin_b) > 2 && any(valid)) {
          num <- sum(nin_b$cover[valid] * ind_col[valid], na.rm = TRUE)
          den <- sum(nin_b$cover[valid], na.rm = TRUE)
          if (den > 0) num / den else NA_real_
        } else {
          NA_real_
        }
      })
    })
    
    per_indicator <- map(seq_len(n_indicators), function(k) {
      vals <- map_dbl(iter_results, ~ .x[[k]])
      matrix(vals, nrow = iter, ncol = 1) %>% as.data.frame()
    })
    
    per_indicator
  }
  
  # Group by NiN ID and run bootstrap for each NiN type
  nin_results <- df_long %>%
    group_by(nin_id) %>%
    group_map(~ boot_one_nin(.x))
  
  indicator_results <- map(seq_len(n_indicators), function(k) {
    per_nin <- map(nin_results, ~ .x[[k]])
    df <- per_nin %>%
      bind_cols()
    colnames(df) <- nin_ids
    df
  })
  
  names(indicator_results) <- indicator_cols
  
  indicator_results
}





indBoot.freq <- function(sp, abun, ind, iter, obl, rat = 2/3,var.abun = F) {
  
  ind.b <- matrix(nrow=iter,ncol=length(colnames(abun)))
  colnames(ind.b) <- colnames(abun)
  ind.b <- as.data.frame(ind.b)  
  
  ind <- as.data.frame(ind)
  ind.list <- as.list(1:length(colnames(ind)))
  names(ind.list) <- colnames(ind)
  
  for (k in 1:length(colnames(ind)) ) {
    ind.list[[k]] <- ind.b }
  
  for (j in 1:length(colnames(abun)) ) {
    
    dat <- cbind(sp,abun[,j],ind)
    dat <- dat[dat[,2]>0,]            # only species that are present in the ecosystem
    dat <- dat[!is.na(dat[,3]),]      # only species that have indicator values
    
    for (i in 1:iter) {
      
      speciesSample <- sample(dat$sp[dat[,2] < obl], size=round( (length(dat$sp)-length(dat$sp[dat[,2]>=obl])) *rat,0), replace=F)  
      dat.b <- rbind(dat[dat[,2] >= obl,],
                     dat[match(speciesSample,dat$sp),]
      )
      
      if (var.abun==T) {
        for (m in 1:nrow(coverscale[-1,]) ) {
          xxx <- dat.b[dat.b[,2]==coverscale[-1,][m,2],2]
          if ( m==1 ) { dat.b[dat.b[,2]==coverscale[-1,][m,2],2] <- sample( c(0.01,coverscale[2:7,2]), prob = c(0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0) ,size=length(xxx),replace=T) }
          if ( m==2 ) { dat.b[dat.b[,2]==coverscale[-1,][m,2],2] <- sample( c(0.01,coverscale[2:7,2]), prob = c(0.2, 0.3, 0.5, 0.0, 0.0, 0.0, 0.0) ,size=length(xxx),replace=T) }
          if ( m==3 ) { dat.b[dat.b[,2]==coverscale[-1,][m,2],2] <- sample( c(0.01,coverscale[2:7,2]), prob = c(0.0, 0.2, 0.3, 0.5, 0.0, 0.0, 0.0) ,size=length(xxx),replace=T) }
          if ( m==4 ) { dat.b[dat.b[,2]==coverscale[-1,][m,2],2] <- sample( c(0.01,coverscale[2:7,2]), prob = c(0.0, 0.0, 0.2, 0.3, 0.5, 0.0, 0.0) ,size=length(xxx),replace=T) }
          if ( m==5 ) { dat.b[dat.b[,2]==coverscale[-1,][m,2],2] <- sample( c(0.01,coverscale[2:7,2]), prob = c(0.0, 0.0, 0.0, 0.2, 0.3, 0.5, 0.0) ,size=length(xxx),replace=T) }
          if ( m==6 ) { dat.b[dat.b[,2]==coverscale[-1,][m,2],2] <- sample( c(0.01,coverscale[2:7,2]), prob = c(0.0, 0.0, 0.0, 0.0, 0.2, 0.3, 0.5) ,size=length(xxx),replace=T) }
        }
        dat.b[!is.na(dat.b[,2]) & dat.b[,2]<=(0),2] <- 0.01
        dat.b[!is.na(dat.b[,2]) & dat.b[,2]>1,2] <- 1
      }
      
      for (k in 1:length(colnames(ind))) {
        
        if ( nrow(dat.b)>2 ) {
          
          ind.b <- sum(dat.b[!is.na(dat.b[,2+k]),2] * dat.b[!is.na(dat.b[,2+k]),2+k] , na.rm=T) / sum(dat.b[!is.na(dat.b[,2+k]),2],na.rm=T)
          ind.list[[k]][i,j] <- ind.b
          
        } else {ind.list[[k]][i,j] <- NA}
        
      }
      
      #      print(paste(i,"",j)) 
    }
    
  }
  return(ind.list)
}
```

```{r}
#colnames(NiN.forest.cov)
# 1st column is the species
# 2nd-27th column is the abundances of sp in different ecosystem types
# 28th-29th column is the indicator values of the respective species
# we choose 1000 iterations
# species with abundance 0.8 or more (dominant species) must be included in each sample
# each sample re-samples 1/3 of the number of species
# the abundance of the re-sampled species may vary (see bootstrap function for details)
```

Running the bootstraps:
  ```{r boot2, eval = F}

lowland_ref_cov <- indBoot_freq(
  df_long    = nin_sp_ind,
  indicators = "Moisture",
  iter       = 1000,
  obl        = 0.8,
  rat        = 1/2,
  var.abun   = TRUE
)


#forest.ref.cov <- indBoot.freq(sp=NiN.forest.cov[,1],abun=NiN.forest.cov[,2:27],ind=NiN.forest.cov[,28:29],
                               iter=1000,obl=0.8,rat=1/2,var.abun=T)

# fixing NaNs
for (i in 1:length(lowland_ref_cov) ) {
  for (j in 1:ncol(lowland_ref_cov[[i]]) ) {
    v <- lowland_ref_cov[[i]][,j]
    v[is.nan(v)] <- NA
    lowland_ref_cov[[i]][,j] <- v
  }
}

#saveRDS(forest.ref.cov, paste0(here::here(),"/data/cache/forest.ref.cov.RDS"))

```

```{r, include = F}
# Data from cache
forest.ref.cov<-readRDS(paste0(here::here(), "/data/cache/forest.ref.cov.RDS"))
```

```{r}
head(forest.ref.cov[[1]]) |>
  kable("html", caption = "Table showing the first 6 rows of the bootstrapped data set.") |> kable_styling("striped") |> scroll_box(width = "100%")
```

This results in an R-list for forest ecosystem, with a data frame for every selected plant indicator and each data frame with as many columns as there are NiN species lists and as many rows as there were iterations in the bootstrap.

Next, we need to derive scaling values from these bootstrap-lists (the columns) for every mapping unit in NiN. Here, we define things in the following way:
  
  - Median = reference values
- 0.025 and 0.975 quantiles = lower and upper limit values
- min and max of the respective indicators scale = min/max values



```{r scalingValues, attr.output='style="max-height: 300px;"', results='hide'}

# every NiN-type is represented by one 'generalisert artsliste'
# some NiN-types are represented by two such species lists
# in some cases two NiN-types are represented by the same species list
#head(lowland_ref_cov[[1]])
lowland_ref_cov[[1]][0,]

# NiN-types where each type is represented by one species list (including when one species list represents two NiN-types), i.e. excluding types with a- and b-suffix
names(lowland_ref_cov[["Moisture"]])
x <- c(1:17,22:26)

# checking the actual NiN-types in the forest lists
forest.NiNtypes <- colnames(lowland_ref_cov[["Moisture"]])
forest.NiNtypes[-x] <- substr(forest.NiNtypes[-x], 1, nchar(forest.NiNtypes[-x])-1)
forest.NiNtypes

# 2 indicator-value indicators: Moisture, "Moisture"
indEll.n=2
# creating a table to hold:
# for every indicator: the 0.5 quantile (median), 0.025 quantile and  0.975 quantile for each NiN-type (3 metrics -> 3 columns per indicator), for Heat_requirement in alpine/arctic systems we do 0.05 quantile and  0.95 quantile and ignore the former as it is a one-sided indicator
# for every nature type (nrows)
tab <- matrix(ncol=3*indEll.n, nrow=24 ) # 24 basic ecosystem types
# coercing the values into the table

myQuantiles <- c(0.025, 0.5, 0.975)

for (i in 1:length(x) ) {
  tab[i,1:3] <- quantile(as.matrix(lowland_ref_cov[["Moisture"]][,x[i]]),probs=myQuantiles,na.rm=T)
  tab[i,4:6] <- quantile(as.matrix(lowland_ref_cov[["Moisture"]][,x[i]]),probs=myQuantiles,na.rm=T)
}

tab <- as.data.frame(tab)
tab$NiN <- NA
tab$NiN[1:length(x)] <- names(lowland_ref_cov[[1]])[x]
tab


# NiN-types represented by several species lists
forest.NiNtypes2 <- forest.NiNtypes[-x]
unique(forest.NiNtypes2)
grep(pattern=unique(forest.NiNtypes2)[1], x=forest.NiNtypes) # finds columns in e.g. colnames(forest.ref.cov[["Light"]]) that match the first NiN-type


for (i in 1:length(unique(forest.NiNtypes2)) ) {
  
  tab[length(x)+i,1:3] <- quantile(as.matrix(forest.ref.cov[["Moisture"]][,grep(pattern=unique(forest.NiNtypes2)[i], x=forest.NiNtypes)]),probs=myQuantiles,na.rm=T)
  tab[length(x)+i,4:6] <- quantile(as.matrix(forest.ref.cov[["Moisture"]][,grep(pattern=unique(forest.NiNtypes2)[i], x=forest.NiNtypes)]),probs=myQuantiles,na.rm=T)

  tab$NiN[length(x)+i] <- unique(forest.NiNtypes2)[i]
  
}

tab

# when species lists represent several NiN-types
# does not apply here
#tab$NiN
#tab <- rbind(tab,tab[c(27:30,42,44,47,55:57,60,62,71,83,85),])
#tab$NiN[c(27:30,42,44,47,55:57,60,62,71,83,85,88:102)] <- c("T3-C3","T3-C9","T3-C5","T3-C8",
#                                                            "T7-C13","T7-C7","T22-C2","T32-C1",
#                                                            "T32-C3","T32-C7","T45-C1","V10-C1","T22-C1",
#                                                            "T32-C5","T32-C21",
#                                                            
#                                                            "T3-C6","T3-C12","T3-C5","T3-C11",
#                                                            "T7-C14","T7-C9","T22-C4","T32-C2",
#                                                            "T32-C4","T32-C8","T45-C2","V10-C2","T22-C3",
#                                                            "T32-C20","T32-C6")
#tab

# making it a proper data frame
dim(tab)
round(tab[,1:6],digits=2)

colnames(tab) <- c("Moist_q2.5","Moist_q50","Moist_q97.5",
                   "Moisture_q2.5","Moisture_q50","Moisture_q97.5",

                   "NiN")
summary(tab)
tab$NiN <- gsub("C", "C-", tab$NiN) # add extra hyphen after C for NiN-types
tab


# restructuring into separate indicators for lower (q2.5) and higher (q97.5) than reference value (=median, q50)
y.Moist <- numeric(length=nrow(tab)*2)
y.Moist[((1:dim(tab)[1])*2)-1] <- tab$Moist_q2.5 
y.Moist[((1:dim(tab)[1])*2)] <- tab$Moist_q97.5 

y.Moisture <- numeric(length=nrow(tab)*2)
y.Moisture[((1:dim(tab)[1])*2)-1] <- tab$Moisture_q2.5 
y.Moisture[((1:dim(tab)[1])*2)] <- tab$Moisture_q97.5 

# creating final objects holding the reference and limit values for all indicators

# ref object for indicators
forest.ref.cov.val <- data.frame( grunn=c(rep(rep(tab$NiN,each=2),indEll.n)),
                                                    county=rep('all',(nrow(tab)*2*indEll.n)),
                                                    region=rep('all',(nrow(tab)*2*indEll.n)),
                                                    Ind=c(rep(c('Moist1','Moist2'),nrow(tab)),
                                                          rep(c('Moisture1','Moisture2'),nrow(tab))
                                                          
                                                    ),
                                                    Rv=c(rep(tab$Moist_q50,each=2),
                                                         rep(tab$Moisture_q50,each=2)
                                                    ),
                                                    Gv=c(y.Moist,y.Moisture),
                                                    maxmin=c(rep(c(1,12),nrow(tab)), # 12 levels of moisture
                                                             rep(c(1,9),nrow(tab))  # 9 levels of Moisture
                                                    )
)

forest.ref.cov.val
forest.ref.cov.val$grunn <- as.factor(forest.ref.cov.val$grunn)
forest.ref.cov.val$Ind <- as.factor(forest.ref.cov.val$Ind)
summary(forest.ref.cov.val)


```

```{r}
forest.ref.cov.val |>
  kable("html", caption = "Reference values (Rv and threshold values (Gv) for each indicator and nature type combination") |> kable_styling("striped") |> scroll_box(width = "100%", height = "300px")
```

Once test data and the scaling values from the reference data are in place, we can calculate CWMs of the selected indicator for the ANO community data and scale them against the scaling values from the reference distribution. Note that we scale each ANO plots CWM against either the lower threshold value and the min value OR the upper threshold value and the max value based on whether the CWM is smaller or higher than the reference value. Since the scaled values for both sides range between 0 and 1, we generate separate lower and upper indicators for each plant functional indicator type. An ANO plot can only have a scaled value in either the lower or the upper indicator (the other one will be 'NA'), except for the unlikely event that the CWM exactly matches the reference value, in which case both lower and upper indicator will receive a scaled indicator value of 1.

For scaling ANO-data against the reference we use the normalisation function from package ecTools.

```{r}
#| warning: false

#### calculating scaled and non-truncated values for the indicators based on the dataset ####
# drop geometry
ANO.forest <- ANO.forest |>
  mutate(
    X = st_coordinates(.)[, 1],
    Y = st_coordinates(.)[, 2]
  ) |>
  st_drop_geometry()

ANO.forest <- ANO.forest |>
  mutate(
    kartleggingsenhet_1m2 = stringr::str_remove(kartleggingsenhet_1m2, "\\s.*$")
  )

for (i in 1:nrow(ANO.forest) ) {  
  tryCatch({
    #print(i)
    #print(paste(ANO.forest$ano_flate_id[i]))
    #print(paste(ANO.forest$ano_punkt_id[i]))
    #    ANO.forest$Hovedoekosystem_sirkel[i]
    #    ANO.forest$Hovedoekosystem_rute[i]
    
    
    
    # if the ANO.hovedtype exists in the forest reference
    if ( ANO.forest$hovedtype_rute[i] %in% unique(sub("\\-.*", "", forest.ref.cov.val$grunn)) ) {
      
      # if there is any species present in current ANO point  
      if ( length(ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.forest$globalid[i]),'Species']) > 0 ) {
        
        
        # Moisture
        dat <- ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.forest$globalid[i]),c('art_dekning','Moisture')]
        ANO.forest[i,'richness'] <- nrow(dat)
        dat <- dat[!is.na(dat$Moisture),]
        
        if ( nrow(dat)>0 ) {
          
          val <- sum(dat[,'art_dekning'] * dat[,'Moisture'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
          # lower part of distribution
          ref <- forest.ref.cov.val[forest.ref.cov.val$Ind=='Moisture1' & forest.ref.cov.val$grunn==as.character(ANO.forest[i,"kartleggingsenhet_1m2"]),'Rv']
          lim <- forest.ref.cov.val[forest.ref.cov.val$Ind=='Moisture1' & forest.ref.cov.val$grunn==as.character(ANO.forest[i,"kartleggingsenhet_1m2"]),'Gv']
          maxmin <- forest.ref.cov.val[forest.ref.cov.val$Ind=='Moisture1' & forest.ref.cov.val$grunn==as.character(ANO.forest[i,"kartleggingsenhet_1m2"]),'maxmin']
          # normalisation
          ANO.forest[i,'Moisture1'] <- ec_normalise(variable = val, x0 = maxmin, x60 = lim, x100 = ref, fun = "linear")
          
          # upper part of distribution
          ref <- forest.ref.cov.val[forest.ref.cov.val$Ind=='Moisture2' & forest.ref.cov.val$grunn==as.character(ANO.forest[i,"kartleggingsenhet_1m2"]),'Rv']
          lim <- forest.ref.cov.val[forest.ref.cov.val$Ind=='Moisture2' & forest.ref.cov.val$grunn==as.character(ANO.forest[i,"kartleggingsenhet_1m2"]),'Gv']
          maxmin <- forest.ref.cov.val[forest.ref.cov.val$Ind=='Moisture2' & forest.ref.cov.val$grunn==as.character(ANO.forest[i,"kartleggingsenhet_1m2"]),'maxmin']
          # normalisation
          ANO.forest[i,'Moisture2'] <- ec_normalise(variable = val, x0 = maxmin, x60 = lim, x100 = ref, fun = "linear")
          
        }
        
        
      }
      
    }
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# generate overall Moisture indicator
ANO.forest$Moisture <- ANO.forest$Moisture1*ANO.forest$Moisture2

ANO.forest$Moisture1[ANO.forest$Moisture1==1] <- NA
ANO.forest$Moisture2[ANO.forest$Moisture2==1] <- NA

# add geometry again
ANO.forest <- ANO.forest |>
  st_as_sf(coords = c("X", "Y"), crs = st_crs(ANO.geo))

```

```{r}
head(ANO.forest) |>
  kable("html", caption = "ANO plots for ecosystems with scaled indicator values in the far-right columns.") |> 
  kable_styling("striped") |> scroll_box(width = "100%")
```


We can visualise the normalisation with a generic figure (@fig-norm).

```{r fig-norm}
#| fig-cap: 'A figure illustrating the normalisation process.The placement of the x60 values are arbitrary in this figure and does not give a representative picture of the relationship between the variable and indicator values.'

var <- seq(0, 100, by = 0.5)
var2 <- seq(101, 200, by = 0.5)

ind <- ec_normalise(var, x0 = 0, x100 = 100, x60 = 50)
ind2 <- ec_normalise(var2, x100 = 101, x0 = 200, x60 = 150)

ec_norm_plot(
  variable = c(var, var2),
  indicator = c(ind, ind2)
) +
  scale_x_continuous(
    breaks = c(0, 50, 100, 150, 200),
    labels = c("lowest possible\nvalue", "2.5%", "50%", "97.5%", "highest possible\nvalue"),
    expand = expansion(add = 30)
  ) +
  labs(x = "Variable values as percentiles of \nbootstrapped reference values")
```

```{r}
#| eval: false
#| include: false

ec_norm_plot(
  variable = c(var, var2),
  indicator = c(ind, ind2)
) +
  scale_x_continuous(
    breaks = c(0, 50, 100, 150, 200),
    labels = c("lowest possible\nvalue", "2.5%", "50%", "97.5%", "highest possible\nvalue"),
    expand = expansion(add = 30)
  ) +
  labs(x = "Variabelverdier som persentiler ift. \nbootstrappede referanse verdier",
       y = "Indikatorverdi")

ggsave(here::here("img/norm_plot.png"))
```

#### Calculating national and regional indices

We can calculate national and region-wise indicator values using a mixed-effects, zero-one inflated beta-regression analysis. Since the indicator values are on a scale from 0-1 the data per definition dont follow a Gaussian distribution, hence the zero-one inflated beta-regression. Since the underlying monitoring data are spatially aggregated (several points per site), not all points are independent, hence the mixed-effects model.

The uncertainty around the national and region indicator values is derived from the same statistical model.

The national and regional estimates are computed separately.

```{r tmbs}
#| warning: false
## Fit ordered beta regression null-model with a random effect
# Norway

Moisture.no <- glmmTMB(
  Moisture ~ 1 + (1|ano_flate_id),
  data = ANO.forest,
  family = ordbeta
)

Moisture1.no <- glmmTMB(
  Moisture1 ~ 1 + (1|ano_flate_id),
  data = ANO.forest[!is.na(ANO.forest$Moisture1),],
  family = ordbeta
)

Moisture2.no <- glmmTMB(
  Moisture2 ~ 1 + (1|ano_flate_id),
  data = ANO.forest[!is.na(ANO.forest$Moisture2),],
  family = ordbeta
)

summary(Moisture.no)
summary(Moisture1.no)
summary(Moisture2.no)

# regions
Moisture.re <- glmmTMB(
  Moisture ~ 0 + region + (1|ano_flate_id),
  data = ANO.forest,
  family = ordbeta
)

Moisture1.re <- glmmTMB(
  Moisture1 ~ 0 + region + (1|ano_flate_id),
  data = ANO.forest[!is.na(ANO.forest$Moisture1),],
  family = ordbeta
)

Moisture2.re <- glmmTMB(
  Moisture2 ~ 0 + region + (1|ano_flate_id),
  data = ANO.forest[!is.na(ANO.forest$Moisture2),],
  family = ordbeta
)

summary(Moisture.re)
summary(Moisture1.re)
summary(Moisture2.re)
```


## 10. Results

The model estimates and standard errors are on a logit scale, but we can calculate indicator values and confidence intervals (CI) by transforming back from logit to values between 0-1
```{r}

NO_FUMO_004_table <- tibble(
  Region = c(
    "Norway",
    "Central Norway",
    "Eastern Norway",
    "Northern Norway",
    "Southern Norway",
    "Western Norway"
  ),
  indicator_value = c(
    plogis(summary(Moisture.no)$coef$cond[, 1]),
    plogis(summary(Moisture.re)$coef$cond[, 1])
  ),
  CI_upper = c(
    plogis(
      summary(Moisture.no)$coef$cond[, 1] +
        1.96 * summary(Moisture.no)$coef$cond[, 2]
    ),
    plogis(
      summary(Moisture.re)$coef$cond[, 1] +
        1.96 * summary(Moisture.re)$coef$cond[, 2]
    )
  ),
  CI_lower = as.numeric(
    plogis(
      summary(Moisture.no)$coef$cond[, 1] -
        1.96 * summary(Moisture.no)$coef$cond[, 2]
    ),
    plogis(
      summary(Moisture.re)$coef$cond[, 1] -
        1.96 * summary(Moisture.re)$coef$cond[, 2]
    )
  ),
  upper_indicator_value = c(
    plogis(summary(Moisture2.no)$coef$cond[, 1]),
    plogis(summary(Moisture2.re)$coef$cond[, 1])
  ),
  n_upper_indicator_value = c(
    summary(Moisture2.no)$nobs,
    as.vector(table(Moisture2.re$frame$region))
  ),
  lower_indicator_value = c(
    plogis(summary(Moisture1.no)$coef$cond[, 1]),
    plogis(summary(Moisture1.re)$coef$cond[, 1])
  ),
  n_lower_indicator_value = c(
    summary(Moisture1.no)$nobs,
    as.vector(table(Moisture1.re$frame$region))
  )
)
NO_FUMO_004_table
```

To express the results as a distribution, we need to simulate mean estimates based on the models intercept and associated standard error.
```{r}

## Norway
# Fixed-effect intercept and SE
b0 <- fixef(Moisture.no)$cond["(Intercept)"]
se_b0 <- sqrt(vcov(Moisture.no)$cond["(Intercept)", "(Intercept)"])
# Dispersion parameter (For glmmTMB this is usually on the response/positive scale via sigma() )
phi <- sigma(Moisture.no)


# draw estimates and backtransform from logit-scale
eta_draws <- rnorm(1000, mean = b0, sd = se_b0)
mu_draws  <- plogis(eta_draws)


## regions
b_re <- fixef(Moisture.re)$cond
vc_re <- vcov(Moisture.re)$cond
phi <- sigma(Moisture.re)

eta_draws_reg <- MASS::mvrnorm(
  n = 1000,
  mu = b_re,
  Sigma = vc_re
)

mu_draws_reg <- plogis(eta_draws_reg)


# combining Moisture_no_sim and Moisture_re_sim
mu <- mu_draws_reg |>
  as_tibble() |>
  add_column(Norway = mu_draws) |>
  pivot_longer(everything(), names_to = "part") |>
  mutate(part = case_when(
    part == "regionCentral.Norway" ~ "C",
    part == "regionEastern.Norway" ~ "E",
    part == "regionNorthern.Norway" ~ "N",
    part == "regionWestern.Norway" ~ "W",
    part == "regionSouthern.Norway" ~ "S",
    .default = part
  )) |>
  add_column(year = "2024")
```


```{r tbl-results}
#| tbl-cap: 'Indicator values and 95% confidence intervals fro NO_FUMO_004, calculated for Norwegian alpine ecosystems.'
mu |>
  group_by(part) |>
  summarise(
    indicator = mean(value),
    low = round(quantile(value, probs = 0.025), 2),
    high = round(quantile(value, probs = .975), 2)
  ) |>
  unite(col = "95% CI", c(low, high), sep = " - ") |>
  gt::gt()
```

## 11. Export file

<!--# 
  
  Optional: Display the code (dont execute it) or the workflow for exporting the indicator values to file. Ideally the indicator values are exported as a georeferenced shape or raster file with indicators values, reference values and errors. You can also chose to export the raw (un-normalised or unscaled variable) as a separate product. You should not save large sptaial output data on GitHub. You can use eval=FALSE to avoid code from being executed (example below - delete if not relevant) 

-->

```{r export}
#| eval: false
arrow::write_parquet(mu,"data/results_NO_FUMO_004.parquet")
```

Demonstrating how to read back the data:

```{r readingBack}
arrow::read_parquet(here::here("data/results_NO_FUMO_004.parquet"))
```

## 12. Author contributions



**CRediT statement**

* Conceptualization: [JT]
* Methodology: [JT]
* Data curation: [JT, ALK]
* Formal analysis: [JT]
* Investigation: [JT]




**MeRIT attribution**

This part supplements the CRediT statement with more granularity for method section.

**Study design**
The study was designed by [JT].

**Data processing**
Raw data were curated and cleaned by [JT and AK].

**Statistical analysis**
All analyses were conducted by [JT] , with verification and minor edits by [AK].

**Visualization**
Figures were produced by [AK].

**Reproducibility**
All code was reviewed and validated by [AK].



::: {.callout-tip collapse="true"}
## Session Info
<!-- You can leave this last part as it is, unless you have a good reason to alter it. -->

This workflow uses renv. Run `renv::restore()` to get started.

:::