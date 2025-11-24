library(yaml)
library(tidyverse)
library(here)

#' Recursively flatten YAML metadata
#' Converts nested lists or vectors into single strings
flatten_meta <- function(x, parent_key = "") {
  out <- list()
  if (is.list(x)) {
    for (n in names(x)) {
      key <- if (parent_key != "") paste(parent_key, n, sep = ".") else n
      out <- c(out, flatten_meta(x[[n]], key))
    }
  } else {
    val <- if (length(x) > 1) paste(x, collapse = "; ") else x
    out[[parent_key]] <- val
  }
  out
}

#' Create metadata table from Quarto YAML headers
#'
#' @param path Path to the indicators folder
#' @return A list of one-row tibbles with flattened metadata
#' @export
create_metadata_table <- function(path) {
  
  qmd_files <- list.files(
    here::here(path),
    pattern = "\\.qmd$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  qmd_files <- qmd_files[
    !grepl("template", qmd_files, ignore.case = TRUE) &
      !grepl("sandbox", qmd_files, ignore.case = TRUE) &
      !grepl("version", qmd_files, ignore.case = TRUE)
  ]
  
  metadata_list <- map(qmd_files, function(file) {
    lines <- readLines(file, warn = FALSE)
    yaml_start <- which(trimws(lines) == "---")[1]
    yaml_end <- which(trimws(lines) == "---")[2]
    if (is.na(yaml_start) || is.na(yaml_end)) return(NULL)
    
    yaml_text <- paste(lines[(yaml_start + 1):(yaml_end - 1)], collapse = "\n")
    meta <- tryCatch(yaml::yaml.load(yaml_text), error = function(e) NULL)
    if (is.null(meta)) return(NULL)
    
    # Recursively flatten
    meta_flat <- flatten_meta(meta)
    
    df <- tibble::tibble(
      file = file,
      !!!set_names(meta_flat),
      .name_repair = "unique"
    )
    
    # Add fallback indicator ID if missing
    if (!"indicatorID" %in% names(df)) {
      df$indicatorID <- basename(dirname(file))
    }
    
    df
  })
  
  compact(metadata_list)
}

#' Combine all metadata into one dataframe
process_and_bind_dfs <- function(df_list) {
  bind_rows(df_list) |>
    janitor::clean_names() |>
    distinct(file, .keep_all = TRUE)
}

#' Create and return App data
create_data <- function(combined_metadata) {
  App_data <- combined_metadata
  assign("App_data", App_data, envir = .GlobalEnv)
  App_data
}

# ---- Run ----
combined_metadata <- create_metadata_table("indicators")
combined_metadata <- process_and_bind_dfs(combined_metadata)
App_data <- create_data(combined_metadata)

