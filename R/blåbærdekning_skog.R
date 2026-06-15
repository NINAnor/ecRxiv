library(tidyverse)
library(kableExtra)
library(here)
library(anybadger)
library(yaml)
library(tibble)
library(conflicted)
library(sf)
library(ecTools)
library(glmmTMB)
library(MASS)



ANO.sp<- st_read("P:/41201785_okologisk_tilstand_2022_2023/data/ANO/naturovervaking_eksport.gdb",
                 layer="ANO_Art", quiet = T)
ANO.geo <- st_read("P:/41201785_okologisk_tilstand_2022_2023/data/ANO/naturovervaking_eksport.gdb",
                   layer="ANO_SurveyPoint", quiet = T)

reg <- st_read("P:/41201785_okologisk_tilstand_2022_2023/data/regioner/regNorway_wgs84 - MERGED.shp",
               quiet = T) %>%
  st_as_sf() %>%
  st_transform(crs = st_crs(ANO.geo))

ANO.geo = st_join(ANO.geo, reg, left = TRUE, join = st_nearest_feature)


library(dplyr)

result <- ANO.sp %>%
  left_join(
    ANO.geo,
    by = c("parentglobalid" = "globalid")
  ) %>%
  dplyr::filter(
    art_navn == "vaccinium myrtillus",
    hovedtype_1m2 == "Skogsmark"
  ) %>%
  group_by(region) %>%
  summarise(
    mean_art_dekning = mean(art_dekning, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )





result <- ANO.geo %>%
  dplyr::filter(hovedtype_1m2 == "Skogsmark") %>%
  left_join(
    ANO.sp %>%
      dplyr::filter(art_navn == "vaccinium myrtillus") %>%
      dplyr::select(parentglobalid, art_dekning),
    by = c("globalid" = "parentglobalid")
  ) %>%
  mutate(
    art_dekning = if_else(is.na(art_dekning), 0, art_dekning)
  ) %>%
  group_by(region) %>%
  summarise(
    mean_art_dekning = mean(art_dekning, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
