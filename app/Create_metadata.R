library(yaml)
library(tidyverse)
#library(here)
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

create_metadata_table <- function(path) {
  
  qmd_files <- list.files(
    file.path(app_dir, path),
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
#print(App_data)
App_data<-App_data |> 
  filter(hide=="FALSE") 
App_data <- App_data %>%
  transmute(
    indicator_id,
    title,
    indicator_name,
    ECT = str_to_upper(ect),                 # Capitalise to ECT
    continent,
    country,
    realm,
    biome,
    ecosystem,                               # Assuming this is the correct ecosystem column
    authors = author_list,                   # Renamed
    year_added,
    year_last_update,
    status,
    version,
    version_comment,
    normalised,
    data_availability,
    code_reproducibility,
    open_science_badge,
    html_file_rel, url,html_file_abs, file
  ) |> 
  filter(!indicator_id=="R")



