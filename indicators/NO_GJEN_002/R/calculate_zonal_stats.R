calculate_zonal_statistics <- function(cfg) {
  requireNamespace("terra", quietly = TRUE) || stop("Package 'terra' is required.", call. = FALSE)
  requireNamespace("exactextractr", quietly = TRUE) || stop("Package 'exactextractr' is required.", call. = FALSE)
  prediction_dir <- resolve_path(cfg, cfg$paths$predictions, must_exist = TRUE)
  inference_manifest <- utils::read.csv(
    resolve_path(cfg, cfg$paths$inference_manifest, must_exist = TRUE),
    stringsAsFactors = FALSE)
  require_columns(inference_manifest, c("prediction", "status"), "Inference manifest")
  files <- unique(inference_manifest$prediction[
    inference_manifest$status %in% c("completed", "existing") &
      file.exists(inference_manifest$prediction)])
  if (!length(files)) stop("No prediction rasters found in ", prediction_dir, call. = FALSE)

  aoi <- read_spatial_input(resolve_path(cfg, cfg$paths$aoi, TRUE),
                            cfg$project$target_crs, "AOI layer")
  id_field <- cfg$fields$aoi_id
  require_columns(aoi, c(id_field, cfg$fields$ecosystem), "AOI layer")

  vrt_path <- file.path(dirname(resolve_path(cfg, cfg$paths$zonal_statistics)), "predictions.vrt")
  terra::vrt(files, filename = vrt_path, overwrite = TRUE)
  chm <- terra::rast(vrt_path)
  aoi$canopy_median_m <- exactextractr::exact_extract(chm, aoi, "median", progress = FALSE)
  aoi$raster_coverage <- exactextractr::exact_extract(
    chm, aoi,
    function(values, coverage_fraction) {
      denominator <- sum(coverage_fraction)
      if (!is.finite(denominator) || denominator <= 0) return(0)
      sum(coverage_fraction[is.finite(values)]) / denominator
    }, progress = FALSE
  )
  aoi$area_m2 <- as.numeric(sf::st_area(aoi))

  manifest_path <- resolve_path(cfg, cfg$paths$tile_manifest, must_exist = TRUE)
  mapping_path <- resolve_path(cfg, cfg$paths$tile_aoi_map, must_exist = TRUE)
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
  mapping <- utils::read.csv(mapping_path, stringsAsFactors = FALSE)
  dates <- merge(mapping, manifest[c("tile_id", "image_date", "status")], by = "tile_id", all.x = TRUE)
  dates$image_date <- as.Date(dates$image_date)
  summarize_dates <- function(values) {
    values <- values[!is.na(values)]
    if (!length(values)) return(c(earliest = NA_character_, latest = NA_character_))
    c(earliest = as.character(min(values)), latest = as.character(max(values)))
  }
  date_rows <- lapply(split(dates$image_date, dates$aoi_id), summarize_dates)
  date_summary <- data.frame(
    aoi_id = names(date_rows),
    image_date_earliest = as.Date(vapply(date_rows, `[[`, character(1), "earliest")),
    image_date_latest = as.Date(vapply(date_rows, `[[`, character(1), "latest"))
  )
  aoi <- merge(aoi, date_summary, by.x = id_field, by.y = "aoi_id", all.x = TRUE, sort = FALSE)

  output <- resolve_path(cfg, cfg$paths$zonal_statistics)
  ensure_directory(dirname(output))
  sf::st_write(aoi, output, layer = "polygon_statistics", delete_dsn = TRUE, quiet = TRUE)
  aoi
}
