# use results: "asis" when setting a status for a chapter
status <- function(type) {
  status <- switch(type,
                   complete = "complete, and indicator values can be calculated as described below.",
                   incomplete = "incomplete and needs further developement before indicator values can be calculated.",
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
