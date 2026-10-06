x100_v0_4_calibration <- function(cfg) {
  required <- c("method", "source_metric", "qa_min_m", "qa_max_m",
                "minimum_retained_fraction", "knot_m", "lower_slope", "upper_slope")
  calibration <- cfg$calibration
  if (is.null(calibration)) stop("Candidate config lacks calibration section.", call. = FALSE)
  missing <- setdiff(required, names(calibration))
  if (length(missing)) stop("Candidate calibration lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!identical(calibration$method, "piecewise_knot_1m")) {
    stop("calibration.method must be piecewise_knot_1m.", call. = FALSE)
  }
  if (!identical(calibration$source_metric, "qa_le10_mean_m")) {
    stop("calibration.source_metric must be qa_le10_mean_m.", call. = FALSE)
  }
  numeric_fields <- setdiff(required, c("method", "source_metric"))
  for (field in numeric_fields) {
    calibration[[field]] <- as.numeric(calibration[[field]])
    if (!is.finite(calibration[[field]])) stop("Non-finite calibration field: ", field, call. = FALSE)
  }
  if (calibration$qa_min_m >= calibration$qa_max_m) stop("Invalid calibration QA range.", call. = FALSE)
  if (calibration$minimum_retained_fraction < 0 || calibration$minimum_retained_fraction > 1) {
    stop("minimum_retained_fraction must be in [0,1].", call. = FALSE)
  }
  calibration
}

calibrate_x100_v0_4 <- function(values, cfg) {
  p <- x100_v0_4_calibration(cfg)
  x <- pmax(as.numeric(values), 0)
  ifelse(x <= p$knot_m,
         p$lower_slope * x,
         p$lower_slope * p$knot_m + p$upper_slope * (x - p$knot_m))
}

calculate_zonal_statistics_x100_v0_4 <- function(cfg) {
  requireNamespace("terra", quietly = TRUE) || stop("Package 'terra' is required.", call. = FALSE)
  requireNamespace("exactextractr", quietly = TRUE) || stop("Package 'exactextractr' is required.", call. = FALSE)
  p <- x100_v0_4_calibration(cfg)
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

  vrt_path <- file.path(dirname(resolve_path(cfg, cfg$paths$zonal_statistics)), "predictions_x100_v0_4.vrt")
  terra::vrt(files, filename = vrt_path, overwrite = TRUE)
  chm <- terra::rast(vrt_path)
  metrics <- exactextractr::exact_extract(
    chm, aoi,
    function(values, coverage_fraction) {
      covered <- is.finite(coverage_fraction) & coverage_fraction > 0
      denominator <- sum(coverage_fraction[covered])
      if (!is.finite(denominator) || denominator <= 0) {
        return(data.frame(canopy_median_raw_m = NA_real_, canopy_mean_qa_raw_m = NA_real_,
                          raster_coverage = 0, qa_le10_retained_fraction = 0))
      }
      finite <- covered & is.finite(values)
      qa <- finite & values >= p$qa_min_m & values <= p$qa_max_m
      finite_weight <- sum(coverage_fraction[finite])
      qa_weight <- sum(coverage_fraction[qa])
      qa_mean <- if (qa_weight > 0) {
        sum(values[qa] * coverage_fraction[qa]) / qa_weight
      } else NA_real_
      data.frame(
        canopy_median_raw_m = if (any(finite)) stats::median(values[finite]) else NA_real_,
        canopy_mean_qa_raw_m = qa_mean,
        raster_coverage = finite_weight / denominator,
        qa_le10_retained_fraction = qa_weight / denominator
      )
    }, progress = FALSE
  )
  for (field in names(metrics)) aoi[[field]] <- metrics[[field]]
  aoi$canopy_calibrated_m <- calibrate_x100_v0_4(aoi$canopy_mean_qa_raw_m, cfg)
  aoi$canopy_median_m <- aoi$canopy_median_raw_m
  aoi$qa_le10_eligible <- is.finite(aoi$canopy_mean_qa_raw_m) &
    aoi$qa_le10_retained_fraction >= p$minimum_retained_fraction
  aoi$area_m2 <- as.numeric(sf::st_area(aoi))

  manifest <- utils::read.csv(resolve_path(cfg, cfg$paths$tile_manifest, TRUE), stringsAsFactors = FALSE)
  mapping <- utils::read.csv(resolve_path(cfg, cfg$paths$tile_aoi_map, TRUE), stringsAsFactors = FALSE)
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
