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
