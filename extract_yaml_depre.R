# code to extract from yaml
this_file <- get_file_info()

# Read the YAML front matter
lines <- readLines(this_file$path, warn = FALSE)
dash_lines <- which(trimws(lines) == "---")
yaml_block <- lines[(dash_lines[1] + 1):(dash_lines[2] - 1)]
yaml_data <- yaml::yaml.load(paste(yaml_block, collapse = "\n"))

# Robust flatten function
flatten_to_string <- function(x) {
  if (is.list(x)) {
    # recursively flatten any nested lists
    x <- unlist(x, recursive = TRUE)
  }
  # convert to single string
  paste(x, collapse = "; ")
}

# Apply to all YAML entries
yaml_data_flat <- lapply(yaml_data, flatten_to_string)

# Create tibble
meta <- tibble::enframe(yaml_data_flat, name = "Variable", value = "Value")

# Check structure
#str(meta)

st <- meta |>
  dplyr::filter(Variable == "status") |>
  pull(Value)
version <- meta |>
  dplyr::filter(Variable == "Version") |>
  pull(Value)
auth <- meta |>
  dplyr::filter(Variable == "AuthorList") |>
  pull(Value)
year <- meta |>
  dplyr::filter(Variable == "yearAdded") |>
  pull(Value)
id <- meta |>
  dplyr::filter(Variable == "indicatorID") |>
  pull(Value)
name <- meta |>
  dplyr::filter(Variable == "indicatorName") |>
  pull(Value)
url <- meta |>
  dplyr::filter(Variable == "url") |>
  pull(Value)
badges<-meta |> 
  dplyr::filter(Variable %in% c("data_availability",
         "code_reproducibility", "open_science_badge")) |> 
  pull(Value)
folder_name <-meta |> 
  dplyr::filter(Variable == "folderName") |> 
  pull(Value)

meta <- meta |>
  dplyr::mutate(Variable = dplyr::case_match(Variable,
    "indicatorID" ~ "Indicator ID" ,
    "indicatorName" ~ "Indicator Name",
    "continent" ~ "Continent",
    "country" ~ "Country",
    "ECT" ~ "Ecosystem Condition Typology Class",
    "yearAdded" ~ "Year added",
    "yearLastUpdate" ~ "Last update",
    "VersionComment" ~ "Version comment",
    "SpatialAggregationPathway" ~ "Spatial aggregation pathway",
    .default = Variable
  )) |>
  dplyr::filter(Variable != "authors")