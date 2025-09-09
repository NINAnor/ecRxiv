library(yaml)
library(purrr)
library(dplyr)

# Folder containing qmd files
qmd_folder <- "indicators/"  # adjust to your folder

# List all qmd files
qmd_files <- list.files(qmd_folder, pattern = "\\.qmd$", full.names = TRUE, recursive = TRUE)

# Function to extract YAML header from a single qmd file
extract_yaml <- function(file) {
  lines <- readLines(file, warn = FALSE)
  
  # Find lines with '---'
  dash_lines <- which(trimws(lines) == "---")
  
  if (length(dash_lines) >= 2) {
    yaml_block <- lines[(dash_lines[1] + 1):(dash_lines[2] - 1)]
    yaml_list <- yaml::yaml.load(paste(yaml_block, collapse = "\n"))
    
    # Convert lists to semicolon-separated strings for flat table
    yaml_list <- lapply(yaml_list, function(x) {
      if (is.list(x)) paste(x, collapse = ";") else x
    })
    
    yaml_list[["file"]] <- basename(file)
    return(as.data.frame(yaml_list, stringsAsFactors = FALSE))
  } else {
    return(data.frame(file = basename(file), error = "No YAML found"))
  }
}

# Apply to all files and combine into one data frame
yaml_data <- map_df(qmd_files, extract_yaml)

# View the result
print(yaml_data)

yaml_data<-yaml_data |> 
  select("indicatorID","indicatorName","country",
         "continent","ECT","Realm","Biome","Ecosystem", 
         "yearAdded", "yearLastUpdate", "status", "Version") |> 
  drop_na()



check_vocab <- function(file) {
  data <- yaml::read_yaml(file)
  
  errors <- list()
  for (param in names(vocab)) {
    if (!is.null(data[[param]])) {
      vals <- if (is.list(data[[param]])) data[[param]] else list(data[[param]])
      bad <- setdiff(vals, vocab[[param]])
      if (length(bad) > 0) {
        errors[[param]] <- bad
      }
    }
  }
  
  if (length(errors) > 0) {
    return(list(file = basename(file), errors = errors))
  } else {
    return(NULL)
  }
}

# Check all files
problems <- map(yaml_files, check_vocab)
problems <- compact(problems)  # remove NULLs

if (length(problems) == 0) {
  message("All files passed controlled vocabulary check!")
} else {
  print(problems)
}
