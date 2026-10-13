#!/usr/bin/env Rscript

# Merge the three national NiN polygon exports and standardize them for the
# production pipeline. The source files are EPSG:25832; production is 25833.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("Usage: Rscript R/prepare_aoi_layers.R <grunnkart-folder> <output.gpkg>", call. = FALSE)
}

requireNamespace("sf", quietly = TRUE) || stop("Package 'sf' is required.", call. = FALSE)
requireNamespace("dplyr", quietly = TRUE) || stop("Package 'dplyr' is required.", call. = FALSE)

source_dir <- normalizePath(args[[1]], mustWork = TRUE)
output <- normalizePath(args[[2]], mustWork = FALSE)
sources <- list(
  "Naturlig aapne" = c("NaturligAapne.shp"),
  "Semi-naturlig" = c("SemiNaturlig.shp", "Semi-naturlig.shp"),
  "Vaatmark" = c("Vaatmark.shp")
)

read_one <- function(ecosystem, filenames) {
  candidates <- file.path(source_dir, filenames)
  path <- candidates[file.exists(candidates)][1]
  if (is.na(path)) stop("Missing AOI source; tried: ", paste(candidates, collapse = ", "), call. = FALSE)
  x <- sf::st_read(path, quiet = TRUE, stringsAsFactors = FALSE)
  required <- c("id", "hvdksys", "tilstnd")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop(filename, " lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  if (is.na(sf::st_crs(x))) stop(basename(path), " has no CRS.", call. = FALSE)
  if (!sf::st_crs(x)$epsg %in% c(4326, 25832)) {
    stop(basename(path), " must be EPSG:4326 or EPSG:25832, found ", sf::st_crs(x)$input, call. = FALSE)
  }
  if (any(x$hvdksys != ecosystem, na.rm = TRUE)) {
    stop(basename(path), " contains an unexpected hvdksys value.", call. = FALSE)
  }
  x <- sf::st_make_valid(x)
  x <- sf::st_transform(x, 25833)
  x$ecosystem <- ecosystem
  dplyr::select(x, id, ecosystem, tilstnd, dplyr::any_of(c("Pred_media", "PredAj_med", "chm_median")))
}

layers <- Map(read_one, names(sources), unname(sources))
aoi <- do.call(rbind, layers)
if (nrow(aoi) != 58871L) {
  stop("Expected 58,871 AOIs, found ", nrow(aoi), call. = FALSE)
}
if (anyDuplicated(aoi$id)) stop("AOI identifiers are not unique.", call. = FALSE)
if (any(sf::st_is_empty(aoi))) stop("AOI layer contains empty geometries.", call. = FALSE)

csv_path <- file.path(source_dir, "nin_chm_zonal.csv")
if (file.exists(csv_path)) {
  legacy <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  if (anyDuplicated(legacy$id)) stop("nin_chm_zonal.csv has duplicate IDs.", call. = FALSE)
  missing_csv <- setdiff(aoi$id, legacy$id)
  missing_aoi <- setdiff(legacy$id, aoi$id)
  if (length(missing_csv) || length(missing_aoi)) {
    stop("AOI and nin_chm_zonal.csv IDs do not match exactly.", call. = FALSE)
  }
}

dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
sf::st_write(aoi, output, layer = "aoi", delete_dsn = TRUE, quiet = TRUE)
message("Wrote ", nrow(aoi), " AOIs in EPSG:25833 to ", output)
