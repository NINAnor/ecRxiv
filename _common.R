# use results: "asis" when setting a status for a chapter
status <- function(type) {
  image_path <- switch(type,
                   complete = "../../../badge_status_operational.svg",
                   incomplete = "../../../badge_status_under_development.svg",
                   deprecated = "../../../badge_status_deprecated.svg",
                   stop("Invalid `type`", call. = FALSE)
  )
  
  image_link(image_path, "https://github.com/NINAnor/ecRxiv/wiki#status-badge")

}
library(anybadger)
version_badge <- function(type){
  anybadger::create_badge("version_badge.svg", label = "Version", value = type, color = "fuchsia")
  knitr::include_graphics(tmp)

}

# Function to add url to images embedded using knitr

image_link <- function(image,url,...){
  htmltools::a(
    href=url,
    htmltools::img(src=image,...)
    )
}