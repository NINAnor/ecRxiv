# Global
# Determine the path of this file (global.R) reliably
get_app_dir <- function() {
  # Case 1: Running normally, global.R is sourced and ofile is set
  ofile <- sys.frame(1)$ofile
  if (!is.null(ofile)) {
    return(dirname(normalizePath(ofile)))
  }
  
  # Case 2: Running via runApp(), fallback to app.R
  # app.R becomes sys.frame(2)$ofile
  for (i in 1:5) {
    f <- sys.frame(i)$ofile
    if (!is.null(f)) {
      return(dirname(normalizePath(f)))
    }
  }
  
  # Case 3: Last fallback: assume working directory *has been locked* by Shiny (after app loads)
  return(normalizePath("."))
}

app_dir <- get_app_dir()
# test for correct directory can be commented out when working
print(paste("App directory detected:", app_dir))

source("Create_metadata.R")
