library(tidyverse)
# list.files("indicators/NO_SNOW_001/R")
# inspect <- quarto::quarto_inspect("indicators/NO_SNOW_001/R/NO_SNOW_001.qmd")
# metadata <- inspect$fileInformation$`indicators/NO_SNOW_001/R/NO_SNOW_001.qmd`
# 
# metadata$metadata$indicatorName
# indicator_metadata <- tibble::tibble(
#   indicator_id = metadata$metadata$ID,
#   title = metadata$metadata$title,
#   ecosystem = metadata$metadata$Ecosystem,
#   version = metadata$metadata$Version,
#   status = metadata$metadata$status
# )
# 
# indicator_metadata


`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

metadata_chr <- function(x, collapse = "; ") {
  x <- x %||% NA_character_
  
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_character_)
  }
  
  paste(as.character(x), collapse = collapse)
}

read_indicator_metadata <- function(path) {
  inspect <- quarto::quarto_inspect(path)
  
  file_info <- inspect$fileInformation[[path]]
  meta <- file_info$metadata
  
  tibble::tibble(
    file = as.character(path),
    indicator_id = metadata_chr(meta$indicatorID),
    indicator_name = metadata_chr(meta$indicatorName),
    title = metadata_chr(meta$title),
    ecosystem = metadata_chr(meta$Ecosystem),
    version = metadata_chr(meta$Version),
    status = metadata_chr(meta$status)
  )
}
indicator_metadata <- read_indicator_metadata(
  "indicators/NO_SNOW_001/R/NO_SNOW_001.qmd"
)

indicator_metadata

qmd_files <- list.files(
  "indicators",
  pattern = "\\.qmd$",
  recursive = TRUE,
  full.names = TRUE
)

all_indicator_metadata <- purrr::map_dfr(
  qmd_files,
  read_indicator_metadata
)
all_indicator_metadata


all_indicator_metadata <- all_indicator_metadata |>
  dplyr::mutate(
    indicator_link = paste0(
      "https://ninanor.github.io/ecRxiv/indicators/",
      indicator_id,
      "/R/",
      indicator_id,
      ".html"
    )
  )

kode <- all_indicator_metadata |>
  dplyr::transmute(
    navn = indicator_name,
    link = indicator_link
  )

kode

snow_data<-arrow::read_parquet(here::here("indicators/NO_SNOW_001/data/snow_indicator_fjell_by_region_winter_year_normalised.parquet"))
snow_data

snow_summary <- snow_data |>
  mutate(
    year = as.integer(year)
  ) |>
  group_by(part, year) |>
  summarise(
    lower = quantile(sampled_mean, 0.025, na.rm = TRUE),
    estimate = median(sampled_mean, na.rm = TRUE),
    upper = quantile(sampled_mean, 0.975, na.rm = TRUE),
    n_draws = sum(!is.na(sampled_mean)),
    .groups = "drop"
  )


snow_summary |>
  arrange(year, part)

region_lookup <- c(
  "Norway" = "hele-norge-2020",
  "Hele Norge" = "hele-norge-2020",
  "Norge" = "hele-norge-2020",
  "Nord-Norge" = "nord-norge-2020",
  "Midt-Norge" = "midt-norge-2020",
  "Vestlandet" = "vestlandet-2020",
  "Sørlandet" = "sorlandet-2020",
  "Østlandet" = "ostlandet-2020"
)

snow_summary <- snow_summary |>
  mutate(
    geografiskOmradeReferanseUid = unname(region_lookup[part])
  )

snow_summary |>
  filter(is.na(geografiskOmradeReferanseUid))

snow_summary

snow_indicator_vurderinger <- snow_summary |>
  dplyr::filter(!is.na(geografiskOmradeReferanseUid)) |>
  dplyr::transmute(
    indikatorBeskrivelse =
      indicator_metadata$indicator_name[[1]],
    
    indikatorReferanseUid =
      indicator_metadata$indicator_id[[1]],
    
    geografiskOmradeReferanseUid,
    
    nedreKonfidensIntervalGrense = lower,
    tilstandsverdi = estimate,
    ovreKonfidensIntervalGrense = upper,
    
    periodeStart = sprintf(
      "%d-01-01 00:00:00",
      year
    ),
    
    periodeSlutt = sprintf(
      "%d-12-31 00:00:00",
      year
    )
  )

indikator_vurderinger <- indicator_results |>
  dplyr::left_join(
    indicator_metadata |>
      dplyr::select(
        indicator_id,
        indicator_name
      ),
    by = "indicator_id"
  ) |>
  dplyr::transmute(
    indikatorBeskrivelse = indicator_name,
    indikatorReferanseUid = indicator_id,
    geografiskOmradeReferanseUid = region_uid,
    nedreKonfidensIntervalGrense = lower,
    tilstandsverdi = estimate,
    ovreKonfidensIntervalGrense = upper,
    periodeStart = period_start,
    periodeSlutt = period_end
  )


#kode
#datasett
#indikator_vurderinger
#total_vurdering
#egenskaps_vurderinger
#indikator_vekting

names(kode)
# "navn" "link"

names(datasett)
# "navn" "link" "kilde" "periodeStart" "periodeSlutt" "type"

names(indikator_vurderinger)
# "indikatorBeskrivelse"
# "indikatorReferanseUid"
# "geografiskOmradeReferanseUid"
# "nedreKonfidensIntervalGrense"
# "tilstandsverdi"
# "ovreKonfidensIntervalGrense"
# "periodeStart"
# "periodeSlutt"

names(total_vurdering)
# "okosystemReferanseUid"
# "geografiskOmradeReferanseUid"
# "nedreKonfidensIntervalGrense"
# "tilstandsverdi"
# "ovreKonfidensIntervalGrense"
# "dato"
# "link"
# "navn"
# "beskrivelse"

names(egenskaps_vurderinger)
# "egenskapReferanseUid"
# "geografiskOmradeReferanseUid"
# "nedreKonfidensIntervalGrense"
# "aggregertTilstandsverdi"
# "ovreKonfidensIntervalGrense"
# "periodeStart"
# "periodeSlutt"
# "vekting"

names(indikator_vekting)
# "indikatorReferanseUid"
# "primaerproduksjon"
# "arter-og-biofysiske-strukturer"
# "biomasse"
# "funksjonell-sammensetning"
# "biologisk-mangfold"
# "landskapsokologi"
# "abiotiske-forhold"



df_to_records <- function(x) {
  if (!is.data.frame(x)) {
    stop("Input must be a data frame.", call. = FALSE)
  }
  
  if (nrow(x) == 0L) {
    return(list())
  }
  
  lapply(seq_len(nrow(x)), function(i) {
    row <- as.list(x[i, , drop = FALSE])
    
    # Remove data-frame attributes from individual values
    row <- lapply(row, function(value) {
      if (is.factor(value)) {
        as.character(value)
      } else {
        unname(value)
      }
    })
    
    row
  })
}

df_to_records <- function(x) {
  if (!is.data.frame(x)) {
    stop("Input must be a data frame.", call. = FALSE)
  }

  if (nrow(x) == 0L) {
    return(list())
  }

  lapply(seq_len(nrow(x)), function(i) {
    row <- as.list(x[i, , drop = FALSE])

    # Remove data-frame attributes from individual values
    row <- lapply(row, function(value) {
      if (is.factor(value)) {
        as.character(value)
      } else {
        unname(value)
      }
    })

    row
  })
}

datasett <- datasett |>
  dplyr::mutate(
    periodeStart = format_upload_datetime(periodeStart),
    periodeSlutt = format_upload_datetime(periodeSlutt)
  )

indikator_vurderinger <- indikator_vurderinger |>
  dplyr::mutate(
    periodeStart = format_upload_datetime(periodeStart),
    periodeSlutt = format_upload_datetime(periodeSlutt)
  )

total_vurdering <- total_vurdering |>
  dplyr::mutate(
    dato = format_upload_datetime(dato)
  )

egenskaps_vurderinger <- egenskaps_vurderinger |>
  dplyr::mutate(
    periodeStart = format_upload_datetime(periodeStart),
    periodeSlutt = format_upload_datetime(periodeSlutt)
  )

build_index_object <- function(
    kode,
    datasett,
    indikator_vurderinger,
    rapport_navn,
    total_vurdering,
    egenskaps_vurderinger,
    indikator_vekting) {
  
  list(
    input = list(
      kode = df_to_records(kode),
      datasett = df_to_records(datasett)
    ),
    
    output = list(
      indikatorVurderinger = df_to_records(
        indikator_vurderinger
      ),
      
      rapportData = list(
        navn = rapport_navn,
        
        totalVurdering = df_to_records(
          total_vurdering
        ),
        
        egenskapsVurderinger = df_to_records(
          egenskaps_vurderinger
        ),
        
        indikatorVurderinger_egenskapVurderinger_vekting =
          df_to_records(indikator_vekting)
      )
    )
  )
}