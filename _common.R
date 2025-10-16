# Function to add url to images embedded using knitr
image_link <- function(image,url,...){
  htmltools::a(
    href=url,
    htmltools::img(src=image,...)
    )
}

# print status badge
status_badge <- function(type) {
  image_path <- switch(type,
                   complete = "../../../badge_status_operational.svg",
                   incomplete = "../../../badge_status_under_development.svg",
                   deprecated = "../../../badge_status_deprecated.svg",
                   stop("Invalid `type`", call. = FALSE)
  )
  
  image_link(image_path, "https://github.com/NINAnor/ecRxiv/wiki#status-badge")

}


# Create and print version badge
version_badge <- function(version_number){
  version_badge_path <- paste0("badge_version_", version_number, ".svg")
  anybadger::create_badge(version_badge_path, label = "Version", value = version_number, color = "#add8e6")
  image_link(version_badge_path, "https://github.com/NINAnor/ecRxiv/wiki#naming-convention")
}

# print open science badges
data_badge <- function(dataAvailability = none) {
  image_path <- switch(dataAvailability,
                   gold = "../../../badge_data_gold.svg",
                   silver = "../../../badge_data_silver.svg",
                   bronze = "../../../badge_data_bronze.svg",
                   none = "../../../badge_data_none.svg",
                   stop("Invalid `type`", call. = FALSE)
  )
  
  image_link(image_path, "https://github.com/NINAnor/ecRxiv/wiki#data-availability")

}

code_badge <- function(codeReproducibility = none) {
  image_path <- switch(codeReproducibility,
                   gold = "../../../badge_code_gold.svg",
                   silver = "../../../badge_code_silver.svg",
                   bronze = "../../../badge_code_bronze.svg",
                   none = "../../../badge_code_none.svg",
                   stop("Invalid `type`", call. = FALSE)
  )
  
  image_link(image_path, "https://github.com/NINAnor/ecRxiv/wiki#code-reproducibility")

}

open_science_badge <- function(openScienceBadge = none) {
  image_path <- switch(openScienceBadge,
                   gold = "../../../badge_overall_gold.svg",
                   silver = "../../../badge_overall_silver.svg",
                   bronze = "../../../badge_overall_bronze.svg",
                   none = "../../../badge_overall_none.svg",
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

