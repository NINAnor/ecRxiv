list.files("indicators/NO_SNOW_001/R")
inspect <- quarto::quarto_inspect("indicators/NO_SNOW_001/R/NO_SNOW_001.qmd")
metadata <- inspect$fileInformation$`indicators/NO_SNOW_001/R/NO_SNOW_001.qmd`

metadata$metadata$indicatorName
indicator_metadata <- tibble::tibble(
  indicator_id = metadata$metadata$ID,
  title = metadata$metadata$title,
  ecosystem = metadata$metadata$Ecosystem,
  version = metadata$metadata$Version,
  status = metadata$metadata$status
)

indicator_metadata


read_indicator_metadata <- function(path) {
  inspect <- quarto::quarto_inspect(path)
  
  file_info <- inspect$fileInformation[[path]]
  meta <- file_info$metadata
  
  tibble::tibble(
    file = path,
    indicator_id = meta$indicatorID %||% NA_character_,
    indicator_name = meta$indicatorName %||% NA_character_,
    title = meta$title %||% NA_character_,
    ecosystem = meta$Ecosystem %||% NA_character_,
    version = meta$Version %||% NA_character_,
    status = meta$status %||% NA_character_
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
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
