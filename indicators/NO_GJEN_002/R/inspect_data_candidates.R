#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || args[[1]] != "--candidates") {
  stop("Usage: Rscript R/inspect_data_candidates.R --candidates data/datalag_kandidater.csv",
       call. = FALSE)
}
requireNamespace("sf", quietly = TRUE) || stop("Package 'sf' is required.", call. = FALSE)

input <- args[[2]]
candidates <- utils::read.csv(input, sep = ";", stringsAsFactors = FALSE,
                              fileEncoding = "UTF-8-BOM")
if (!nrow(candidates)) stop("Candidate list is empty: ", input, call. = FALSE)

inspect <- function(path) {
  extension <- tolower(tools::file_ext(path))
  tryCatch({
    if (extension %in% c("shp", "gpkg", "geojson")) {
      x <- sf::st_read(path, quiet = TRUE)
      data.frame(
        FullSti = path, Type = "spatial", Rows = nrow(x),
        CRS = sf::st_crs(x)$input %||% NA_character_,
        Fields = paste(setdiff(names(x), attr(x, "sf_column")), collapse = "|"),
        Status = "readable", Message = "", stringsAsFactors = FALSE)
    } else {
      first <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
      semicolons <- lengths(regmatches(first, gregexpr(";", first, fixed=TRUE)))
      commas <- lengths(regmatches(first, gregexpr(",", first, fixed=TRUE)))
      separator <- if (semicolons > commas) ";" else ","
      x <- utils::read.table(path, header=TRUE, sep=separator, nrows=5L,
                             stringsAsFactors=FALSE, check.names=FALSE,
                             quote='"', comment.char="")
      data.frame(
        FullSti = path, Type = "table", Rows = NA_integer_, CRS = NA_character_,
        Fields = paste(names(x), collapse = "|"), Status = "readable", Message = "",
        stringsAsFactors = FALSE)
    }
  }, error = function(e) data.frame(
    FullSti = path, Type = extension, Rows = NA_integer_, CRS = NA_character_,
    Fields = "", Status = "unreadable", Message = conditionMessage(e),
    stringsAsFactors = FALSE))
}

`%||%` <- function(x, y) if (is.null(x) || !length(x) || is.na(x)) y else x
result <- do.call(rbind, lapply(candidates$FullSti, inspect))
output <- file.path(dirname(input), "datalag_kandidatkontroll.csv")
utils::write.csv(result, output, row.names = FALSE, na = "")
message("Candidate inspection written to ", output)
