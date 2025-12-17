# Function to add url to images embedded using knitr
image_link <- function(image,url,...){
  htmltools::a(
    href=url,
    htmltools::img(src=image, height = "20px", ...)
    )
}

# print status badge
status_badge <- function(type) {
  image_path <- switch(type,
                   complete = "https://raw.githubusercontent.com/NINAnor/ecRxiv/main/docs/badge_status_operational.svg",
                   incomplete = "https://raw.githubusercontent.com/NINAnor/ecRxiv/main/docs/badge_status_under_development.svg",
                   deprecated = "https://raw.githubusercontent.com/NINAnor/ecRxiv/main/docs/badge_status_deprecated.svg",
                   stop("Invalid `type`", call. = FALSE)
  )
  
  image_link(image_path, "https://github.com/NINAnor/ecRxiv/wiki#status-badge")

}


# Create and print version badge
version_badge <- function(my_version_number, folder_name){

  version_badge_name <- paste0("badge_version_", my_version_number, ".svg")

  version_badge_path <- here::here("indicators", folder_name, "img", version_badge_name)

  dir_to_create <- dirname(version_badge_path)
  if (!dir.exists(dir_to_create)) {
    dir.create(dir_to_create, recursive = TRUE)
  }
  anybadger::create_badge(
    version_badge_path, 
    label = "Version", 
    value = as.character(my_version_number), 
    color = "#add8e6")
  
  image_link(
    version_badge_path, 
    "https://github.com/NINAnor/ecRxiv/wiki#naming-convention")
}

# print open science badges
data_badge <- function(dataAvailability = none) {
  image_path <- switch(dataAvailability,
                   gold = "https://raw.githubusercontent.com/NINAnor/ecRxiv/main/docs/badge_data_gold.svg",
                   silver = "https://raw.githubusercontent.com/NINAnor/ecRxiv/main/docs/badge_data_silver.svg",
                   bronze = "https://raw.githubusercontent.com/NINAnor/ecRxiv/main/docs/badge_data_bronze.svg",
                   none = "https://raw.githubusercontent.com/NINAnor/ecRxiv/main/docs/badge_data_none.svg",
                   stop("Invalid `type`", call. = FALSE)
  )
  
  image_link(image_path, "https://github.com/NINAnor/ecRxiv/wiki#data-availability")

}

code_badge <- function(codeReproducibility = none) {
  image_path <- switch(codeReproducibility,
                   gold = "https://raw.githubusercontent.com/NINAnor/ecRxiv/main/docs/badge_code_gold.svg",
                   silver = "https://raw.githubusercontent.com/NINAnor/ecRxiv/main/docs/badge_code_silver.svg",
                   bronze = "https://raw.githubusercontent.com/NINAnor/ecRxiv/main/docs/badge_code_bronze.svg",
                   none = "https://raw.githubusercontent.com/NINAnor/ecRxiv/main/docs/badge_code_none.svg",
                   stop("Invalid `type`", call. = FALSE)
  )
  
  image_link(image_path, "https://github.com/NINAnor/ecRxiv/wiki#code-reproducibility")

}

open_science_badge <- function(openScienceBadge = none) {
  image_path <- switch(openScienceBadge,
                   gold = "https://raw.githubusercontent.com/NINAnor/ecRxiv/main/docs/badge_overall_gold.svg",
                   silver = "https://raw.githubusercontent.com/NINAnor/ecRxiv/main/docs/badge_overall_silver.svg",
                   bronze = "https://raw.githubusercontent.com/NINAnor/ecRxiv/main/docs/badge_overall_bronze.svg",
                   none = "https://raw.githubusercontent.com/NINAnor/ecRxiv/main/docs/badge_overall_none.svg",
                   stop("Invalid `type`", call. = FALSE)
  )
  
  image_link(image_path, "https://github.com/NINAnor/ecRxiv/wiki#open-science-badges")

}


# function to get the current file path
# irrespective of rendering or being in interactive mode

get_file_info <- function() {
  # During rendering
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr_file <- knitr::current_input()
    if (!is.null(knitr_file) && knitr_file != "") {
      return(list(
        path = knitr_file,
        name = basename(knitr_file),
        dir = dirname(knitr_file),
        context = "rendering"
      ))
    }
  }
  
  # Interactive RStudio session
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudio_file <- rstudioapi::getSourceEditorContext()$path
    if (rstudio_file != "") {
      return(list(
        path = rstudio_file,
        name = basename(rstudio_file),
        dir = dirname(rstudio_file),
        context = "interactive"
      ))
    }
  }
  
  return(NULL)
}

# This is a legacy function that is not used in the current version of the qmd template
# use results: "asis" when setting a status for a chapter
status <- function(type) {
  status <- switch(type,
                   complete = "complete, and indicator values can be calculated as described below.",
                   incomplete = "incomplete and needs further development before indicator values can be calculated.",
                   deprecated = "describing an indicator that is deprecated.",
                   stop("Invalid `type`", call. = FALSE)
  )
  
  class <- switch(type,
                  complete = "note",
                  incomplete = "warning",
                  deprecated = "important"
  )
  
  color <- switch(type,
                  complete = "lightgreen",
                  incomplete = "orange",
                  deprecated = "salmon"
  )
  
  cat(paste0(
    "\n",
    ':::  {.callout-', class, ' style="background: ', color,  ';"}', " \n",
    "## Status",  " \n",
    "This indicator documentation is ",
    status,
    "\n",
    ":::\n"
  ))
}

get_root_NINA <- function(server = "P") {
  server <- toupper(server)
  if (!server %in% c("P", "R")) {
    stop("server must be 'P' or 'R'")
  }
  if (.Platform$OS.type == "windows") {
    base <- switch(server,
                   P = "P:/",
                   R = "R:/")
  } else {
    base <- switch(server,
                   P = "/data/P-Prosjekter2/",
                   R = "/data/R/")
  } 
  return(base)
}


### ----------------------------- ####
## Code to extract top yaml
### ----------------------------- ####



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