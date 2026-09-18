#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| include: false
#| cache: false
library(knitr)
library(tidyverse)
library(kableExtra)
library(here)
library(anybadger)
library(yaml)
library(tibble)
library(conflicted)
# weird workarounds for cache issues
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::select)
pull<-dplyr::pull

knitr::opts_chunk$set(echo = TRUE)
#
#
#
#
#
#
#| echo: false
#| cache: false
source(here::here("_common.R"))
#
#
#
#| echo: false
#| results: asis

if (!is.null(yaml_data$codeReviewers)) {
  reviewers <- purrr::map_chr(yaml_data$codeReviewers, "name")
  
  htmltools::HTML(paste0(
    '<div id="title-block-header" class="quarto-title-block default">
       <div class="quarto-title-meta">
         <div>
           <div class="quarto-title-meta-heading">Reviewed by</div>
           <div class="quarto-title-meta-contents">',
    paste(reviewers, collapse = "; "),
    '</div>
         </div>
       </div>
     </div>',
    sep = ""
  ))
}
#
#
#
#
#
#| echo: false
status_badge(as.character(st))
#
#
#
#| echo: false
version_badge(my_version_number = as.character(version[[1]]))
#
#
#
#| echo: false
data_badge(as.character(badges[[1]]))
#
#
#
#| echo: false
code_badge(as.character(badges[[2]]))
#
#
#
#| echo: false
open_science_badge(as.character(badges[[3]]))
#
#
#
#| echo: false
# The following inserts a badge specifying the GPL GNU version 3 license. ecRxiv recommends using this license, but you can decide to use another. Contact ecRxiv for help in insert a badge for a different license
license_badge()
#
#
#
#
#
#
#
#
#
#
#
#
#
#| tbl-cap: 'Indicator metadata'
#| echo: false
#| warning: false
meta |>
  dplyr::filter(Variable %in% c(
    "Indicator ID",
    "Indicator Name",
    "verbatimName",
    "Continent",
    "Country",
    "Ecosystem Condition Typology Class", 
    "Realm",
    "Biome",
    "Ecosystem",
    "verbatimEcosystem",
    "Year added", 
    "Last update",
    "Version",
    "Version comment",
    "Normalised",
    "Spatial aggregation pathway",
    "Precision")) |>   
  kable()

```
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| fig-cap: 'Interactions between pollinator genus and their interracting plant species using data from both the ASO and GRUK datasets.'
#| out-width: 100%
#| echo: false
#| warning: false
knitr::include_graphics("../img/interaction_plots_genus.png")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| eval: false

# Masterscript to run all the analysis.
source("01_codeForIndicator/00_masterScript.R")
#
#
#
#
#
#
#| eval: false

# This script is used to create the open lowland boundary used in this analysis

source("01_codeForIndicator/00E_get_openlowland_boundary.R")
#
#
#
#
#
#| fig-cap: 'The open-lowland ecosystem area in Norway.'
#| out-width: 100%
#| echo: false
#| warning: false
knitr::include_graphics("../img/openlowland_mask.png")
#
#
#
#
#
#
#
#
#| eval: false

source("01_codeForIndicator/00G_get_nature_regions.R")
#
#
#
#
#| fig-cap: 'The classification of regions in Norway used in this study. The classification were done by putting together counties as done in the NaturIndeks.'
#| out-width: 100%
#| echo: false
#| warning: false
knitr::include_graphics("../img/nature_regions.png")
#
#
#
#
#
#
#
#
#
#| eval: false

# resultFolder should be where the output from the Hotspot project is stored. 
#This folder should contain the raster files for pollinator and plant occurrence probabilities.

resultFolder <- "path/to/hotspot/output"
# ==============================================================================
# LOAD SPECIES DISTRIBUTION RASTERS
# ==============================================================================

## ------------------------------------------------------------------
## Insect occurrence probabilities
## ------------------------------------------------------------------

insects <- terra::rast(
  file.path(
    resultFolder,
    "insectProbabilities.tiff"
  )
)


## ------------------------------------------------------------------
## Plant occurrence probabilities
## ------------------------------------------------------------------

plants <- terra::rast(
  file.path(
    resultFolder,
    "plantProbabilities.tiff"
  )
)

## ========================================================
## Expected plant species richness
## ========================================================

plant_expected_richness <- terra::app(
  plants,
  fun = sum,
  na.rm = TRUE,
  cores = 1,
  filename = file.path(
    resultFolder,
    "plant_expected_richness.tif"
  ),
  overwrite = TRUE
)

names(plant_expected_richness) <-
  "plant_expected_richness"

#
#
#
#
#
#
#
#
#
#| eval: false

source("01_codeForIndicator/webPlotsForModels.R")
#
#
#
#
#
#
#
#
#| eval: false

source("01_codeForIndicator/00A_get_CLC_raster.R")
#
#
#
#
#
#| eval: false
source("01_codeForIndicator/00C_get_elevation_raster.R")
#
#
#
#
#
#
#| eval: false

source("01_codeForIndicator/00D_get_calibration_raster.R")
#
#
#
#
#
#| eval: false

source("01_codeForIndicator/04_process_national_insect_monitoring_data.R")
#
#
#
#
#
#
#| fig-cap: "Spatial distribution of insect richness at national monitoring sites."
#| echo: false
#| warning: false
#| message: false


library(sf)
library(dplyr)
library(leaflet)
library(viridis)
library(htmltools)
library(ggplot2)


# -------------------------------------------------------------------------------
# Prepare data
# -------------------------------------------------------------------------------

  national_monitoring_insect_richness <- sf::st_read( "../data/ninaInsect_monitoring_with_richness_for_all.shp",
    quiet = TRUE
  )

ggplot(national_monitoring_insect_richness) +
  geom_sf(
    aes(
      colour = insct_r,
      size = insct_r
    ),
    alpha = 0.75
  ) +
  scale_colour_viridis_c(
    name = "Insect richness",
    option = "viridis"
  ) +
  scale_size_continuous(
    name = "Insect richness",
    range = c(2, 8)
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_line(linewidth = 0.2),
    legend.position = "right"
  ) +
  labs(
    x = NULL,
    y = NULL
  )
#
#
#
#
#
#
#
#
#| eval: false

source("01_codeForIndicator/00F_get_reference_locations.R")
#
#
#
#
#| fig-cap: 'The reference polygons of semi-natural grasslands in good condition from the Natur i Norge (NiN) database. Left: NiN polygons in good conditions. Right: Centroid of the polygons to clearly show the different NiN classes in the reference polygons.'
#| out-width: 100%
#| echo: false
#| warning: false

knitr::include_graphics("../img/good_seminatural_grassland_nin.png")
#
#
#
#
#
#
#
#
#
#
#| eval: false

## -----------------------
## ASO and ANO data locations
## -----------------------

source("01_codeForIndicator/04_process_ASO_data.R")
source("01_codeForIndicator/04_process_ANO_data.R")
#
#
#
#| fig-cap: 'The ASO and ANO sampling locations used to evaluate the effect of plant-host availability on pollinator indicator values. The ASO dataset represents semi-natural meadows, while the ANO dataset represents agricultural landscapes that are within the study area. The background map shows the observed/detected plant-host richness out the 50 plant species under consideration.'
#| echo: false
#| warning: false

  aso_plant_richness <-  sf::st_read(
    "../data/aso_location_with_richness.shp",
    quiet = TRUE
  )

ano_plant_richness <- sf::st_read(
  "../data/ano_location_with_richness.shp",
  quiet = TRUE
)

evaluation_locs <- bind_rows(aso_plant_richness,
                             ano_plant_richness)

dataset_colours <- c(
  "ASO" = "#0072B2",
  "ANO" = "#D55E00"
)

# ==============================================================================
# Interactive map of plant richness evaluation locations
# ==============================================================================

library(sf)
library(dplyr)
library(leaflet)
library(viridis)
library(htmltools)
library(ggplot2)

ggplot(evaluation_locs) +
  geom_sf(
    aes(size = plnt_rc,
    colour = plnt_rc),
    alpha = 0.8
  ) +
  facet_wrap(~ dataset) +
  scale_size_continuous(
    name = "Plant richness"
  ) +
  scale_colour_viridis_c(
    name = "Plant richness",
    option = "viridis"
  ) +
  theme_bw() +
  labs(
    x = NULL,
    y = NULL
  )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| eval: false

source("01_codeForIndicator/01_richnessEstimation.R")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| eval: false

source("01_codeForIndicator/02_getCaliberationModel.R")
#
#
#
#
#
#
#
#
#| fig-cap: 'Normalisation function (piecewise linear)'

temp <- tibble(
  variable = 0:100
)

temp <- temp %>% mutate(indicator = ecTools::ec_normalise(variable = variable,
                          x0 = 0,
                          #x60 = 5,
                          x100 = 100,
                          fun = "linear")
)

temp |>
  ggplot() +
  geom_line(aes(x = variable,
                 y = indicator))
#
#
#
#
#
#| eval: false

source("01_codeForIndicator/02_scaledIndicatorFunction.R")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| eval: false

source("01_codeForIndicator/05_insect_evaluation.R")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| eval: false

source("01_codeForIndicator/05_plant_evaluation.R")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| fig-cap: 'Scatter plot showinng the relationship between the observed and predicted pollinator richness from the three candidate models fitted. The blue line shows the fitted regression line, while the dashed black line shows the 1:1 relationship.'
#| out-width: 100%
#| echo: false
#| warning: false
knitr::include_graphics("../img/validation_plots.png")
#
#
#
#
#
#
#
#
#| echo: false
#| label: tbl-insect-indicator-reference-levels
#| warning: false
#| message: false

tbl_refs <- readRDS(
  "../data/insect_indicator_reference_values.rds"
)

tbl_refs$min_cal_richness <- 0

names(tbl_refs) <- c(
  "Region",
  "Mean richness (uncalibrated)",
  "$X_0$ (uncalibrated)",
  "$X_{100}$ (uncalibrated)",
  "Mean richness (no interaction)",
  "$X_0$ (no interaction)",
  "$X_{100}$ (no interaction)",
  "Mean richness",
  "$X_0$",
  "$X_{100}$",
  "Reference locations"
)

knitr::kable(
  tbl_refs %>% dplyr::select("Region", "$X_0$", "$X_{100}$", "Reference locations"),
  format = "markdown",
  digits = 0,
    caption = "Regional reference levels for the pollinator potential indicator. For each region, the table reports the lower ($X_0$) and upper ($X_{100}$) reference levels and the number of reference locations used in their estimation. Reference locations were derived from aemi-natural NIN polygons in good condition. All the reference levels have been rounded to the nearest whole number."
)
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| warning: false
#| message: false

tbl_refs <- readRDS(
  "../data/region_results.rds"
) %>%
  
  filter(
    indicator == "Calibrated regional indicator"
  ) %>%
  
  select(
    nature_region,
    bootstrap_mean,
    bootstrap_sd,
    lower_quantile,
    upper_quantile,
    percentage_ge_0.6
  ) %>%
  
  mutate(
    nature_region = if_else(
      nature_region == "national",
      "National",
      nature_region
    ),
    
    # Set the desired order explicitly, with National last.
    nature_region = factor(
      nature_region,
      levels = c(
        "Central",
        "East",
        "North",
        "South",
        "West",
        "National"
      )
    )
  ) %>%
  
  arrange(
    nature_region
  )


names(tbl_refs) <- c(
  "Region",
  "Indicator value",
  "Standard deviation",
  "Lower quantile",
  "Upper quantile",
  "Percetage above threshold"
)

knitr::kable(
  tbl_refs,
  format = "markdown",
  digits = 4,
    caption = "Regional mean, standard deviation, lower and upper quantiles of the scaled pollinator potential indicator across open lowland areas. Values are shown for each region and for the national level. Higher indicator values represent greater estimated pollinator potential relative to the reference conditions used for calibration. Standard deviation indicates the bootstrap variation around the indicator value. The lower and upper quantiles refer to the 2.5% and 97.5% quantiles from the boostrap. The percentage above threshold represents the percentage of the 50m x 50 m grid cells within the region that have scaled indicator values greater or equal to 0.6. "
)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| fig-cap: 'Relationships between observed insect richness and measures of pollinator potential. Scatter plots show the relationship between observed insect richness at evaluation locations and (A) unscaled pollinator potential, (B) the scaled pollinator potential indicator, and (C) expected richness from the uncalibrated model. Points represent individual evaluation locations; lines show the fitted linear relationships with 95% confidence intervals. Pearson correlation coefficients are shown in each panel ($r = 0.78$, $0.35$, and $0.21$, respectively).'
#| out-width: 100%
#| echo: false
#| warning: false
knitr::include_graphics("../img/richness_calibration_relationships.png")
#
#
#
#
#
#
#| fig-cap: 'Differences between uncalibrated and calibrated pollinator potential indicators across regions. Points show the estimated mean difference between the uncalibrated and calibrated indicators, calculated as uncalibrated minus calibrated values, with error bars representing 95% confidence intervals. The dashed horizontal line indicates no difference between the two approaches. Regional and national (scaled the minimum and maximum reference values from the entire study region) indicators are shown separately.'
#| out-width: 100%
#| echo: false
#| warning: false
knitr::include_graphics("../img/insect_calibration_difference_effect.png")
#
#
#
#
#
#
#
#
#
#| fig-cap: 'Relationships between plant richness and measures of pollinator and insect richness. Relationships between observed plant richness and (A) pollinator potential, (B) the scaled pollinator potential indicator, and (C) expected insect richness from the uncalibrated model, shown separately for the ANO and ASO datasets. Points represent individual sampling locations, with fitted linear relationships shown for each dataset. Pearson correlation coefficients are reported for ANO and ASO in each panel.'
#| out-width: 100%
#| echo: false
#| warning: false
knitr::include_graphics("../img/plant_indicator_evaluation.png")
#
#
#
#
#
#
#
#
#
#| fig-cap: 'Effect of calibration on the pollinator potential indicator at ANO and ASO survey locations. Points show the mean difference between the uncalibrated and calibrated scaled indicator, calculated as uncalibrated minus calibrated values, across the five regions. Error bars represent 95% confidence intervals, and the dashed line indicates no difference between uncalibrated and calibrated values. Results are shown separately for the regional and national calibration approaches. Negative values indicate that calibration increased the indicator value relative to the uncalibrated value, whereas positive values indicate a decrease following calibration.'
#| out-width: 100%
#| echo: false
#| warning: false
knitr::include_graphics("../img/plant_calibration_difference_effect.png")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| eval: false
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
sessionInfo()
#
#
#
#
#
#
