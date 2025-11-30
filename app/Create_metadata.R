library(yaml)
library(tidyverse)
library(here)

#' Recursively flatten YAML metadata
#' Converts nested lists or vectors into single strings

flatten_meta <- function(x, parent_key = NULL) {
  out <- list()
  
  if (is.list(x)) {
    nms <- names(x)
    
    # Unnamed list items → numeric names
    if (is.null(nms)) {
      nms <- seq_along(x)
    }
    
    for (i in seq_along(x)) {
      key <- if (!is.null(parent_key)) {
        paste0(parent_key, ".", nms[i])
      } else {
        as.character(nms[i])
      }
      out <- c(out, flatten_meta(x[[i]], key))
    }
    
  } else {
    # Safe character conversion (for logical, numeric, date, etc)
    safe_val <- tryCatch(
      as.character(x),
      error = function(e) NA_character_
    )
    out[[parent_key]] <- paste(safe_val, collapse = "; ")
  }
  
  out
}



#' Create metadata table from Quarto YAML headers
#'
#' @param path Path to the indicators folder
#' @return A list of one-row tibbles with flattened metadata
#' @export
# create_metadata_table <- function(path) {
#   
#   qmd_files <- list.files(
#     here::here(path),
#     pattern = "\\.qmd$",
#     recursive = TRUE,
#     full.names = TRUE
#   )
#   
#   qmd_files <- qmd_files[
#     !grepl("template", qmd_files, ignore.case = TRUE) &
#       !grepl("sandbox", qmd_files, ignore.case = TRUE) &
#       !grepl("version", qmd_files, ignore.case = TRUE)
#   ]
#   
#   html_file <- sub("\\.qmd$", ".html", qmd_files)
#   
#   html_file_relative <- file.path("indicators", basename(dirname(qmd_files)), basename(html_file))
#  
#    metadata_list <- map(qmd_files, function(file) {
#     lines <- readLines(file, warn = FALSE)
#     yaml_start <- which(trimws(lines) == "---")[1]
#     yaml_end <- which(trimws(lines) == "---")[2]
#     if (is.na(yaml_start) || is.na(yaml_end)) return(NULL)
#     
#     yaml_text <- paste(lines[(yaml_start + 1):(yaml_end - 1)], collapse = "\n")
#     meta <- tryCatch(yaml::yaml.load(yaml_text), error = function(e) NULL)
#     if (is.null(meta)) return(NULL)
#     
#     # Recursively flatten
#     meta_flat <- flatten_meta(meta)
#     
#     df <- tibble::tibble(
#       file = file,
#       html_file_abs = html_file,
#       html_file_rel = html_file_relative,
#       !!!set_names(meta_flat),
#       .name_repair = "unique"
#     )
#     
#     # Add fallback indicator ID if missing
#     if (!"indicatorID" %in% names(df)) {
#       df$indicatorID <- basename(dirname(file))
#     }
#     
#     df
#   })
#   
#   compact(metadata_list)
# }

create_metadata_table <- function(path) {
  
  qmd_files <- list.files(
    here::here(path),
    pattern = "\\.qmd$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  qmd_files <- qmd_files[
    !grepl("template|sandbox|version", qmd_files, ignore.case = TRUE)
  ]
  
  metadata_list <- map(qmd_files, function(qmd_path) {
    
    meta <- tryCatch(
      rmarkdown::yaml_front_matter(qmd_path),
      error = function(e) NULL
    )
    if (is.null(meta)) return(NULL)
    
    meta_flat <- flatten_meta(meta)
    
    # --- Extract indicator IDs safely ---
    id_fields <- grep("^indicatorID", names(meta_flat), value = TRUE)
    
    if (length(id_fields) == 0) {
      # fallback to folder name
      indicator_ids <- basename(dirname(qmd_path))
    } else {
      indicator_ids <- unlist(meta_flat[id_fields])
    }
    
    # Remove YAML indicatorID fields to avoid duplication
    meta_flat[id_fields] <- NULL
    
    # Paths
    html_file_abs <- sub("\\.qmd$", ".html", qmd_path)
    
    indicator_folder <- basename(dirname(dirname(qmd_path)))
    
    html_file_rel <- file.path(
      "indicators",
      indicator_folder,
      "R",
      paste0(indicator_folder, ".html")
    )
    
    # --- Create one row per indicatorID ---
    rows <- map_df(indicator_ids, function(id) {
      tibble(
        indicatorID = id,
        file = qmd_path,
        html_file_abs = html_file_abs,
        html_file_rel = html_file_rel,
        !!!meta_flat
      )
    })
  })
  
  metadata_list |> purrr::compact() |> bind_rows()
}


#' Create and return App data
create_data <- function(combined_metadata) {
  App_data <- combined_metadata
  assign("App_data", App_data, envir = .GlobalEnv)
  App_data
}

# ---- Run ----
combined_metadata <- create_metadata_table("indicators")
#combined_metadata <- process_and_bind_dfs(combined_metadata)
combined_metadata<-combined_metadata |>
  janitor::clean_names() |>
  distinct()
App_data <- create_data(combined_metadata)

