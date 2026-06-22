# Global

# Route R errors and warnings to stderr so they appear in Docker logs
options(
  shiny.error = function() {
    msg <- paste0(
      "[R ERROR] ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), " - ",
      geterrmessage()
    )
    cat(msg, "\n", file = stderr())
  },
  shiny.sanitize.errors = FALSE,
  warn = 1  # print warnings immediately rather than collecting them
)

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

  # Case 3: Docker container specific - check if we're in /srv/shiny-server
  if (dir.exists("/srv/shiny-server") && file.exists("/srv/shiny-server/app.R")) {
    return("/srv/shiny-server")
  }

  # Case 4: Last fallback: assume working directory *has been locked* by Shiny (after app loads)
  return(normalizePath("."))
}

app_dir <- get_app_dir()
# test for correct directory can be commented out when working
print(paste("App directory detected:", app_dir))

message("app_dir = ", app_dir)
message("Looking for indicators in: ", file.path(app_dir, "indicators"))

source("Create_metadata.R")
