validate_inputs <- function(cfg, require_model = FALSE) {
  aoi <- read_spatial_input(resolve_path(cfg, cfg$paths$aoi, TRUE),
                            cfg$project$target_crs, "AOI layer")
  require_columns(aoi, c(cfg$fields$aoi_id, cfg$fields$ecosystem), "AOI layer")
  if (anyDuplicated(aoi[[cfg$fields$aoi_id]])) stop("AOI identifiers are not unique.", call. = FALSE)
  if (any(sf::st_is_empty(aoi))) stop("AOI layer contains empty geometries.", call. = FALSE)

  regions <- read_spatial_input(resolve_path(cfg, cfg$paths$regions, TRUE),
                                cfg$project$target_crs, "Region layer")
  require_columns(regions, c(cfg$fields$region_id, cfg$fields$region_name), "Region layer")
  if (nrow(regions) != 5L) stop("Region layer must contain exactly five reporting regions.", call. = FALSE)

  zones <- read_spatial_input(resolve_path(cfg, cfg$paths$bioclimatic_zones, TRUE),
                              cfg$project$target_crs, "Bioclimatic-zone layer")
  require_columns(zones, cfg$fields$bioclimatic_zone, "Bioclimatic-zone layer")

  keys <- c("ecosystem", "region_id", "bioclimatic_zone")
  good <- utils::read.csv(resolve_path(cfg, cfg$paths$good_reference, TRUE), stringsAsFactors = FALSE)
  forest <- utils::read.csv(resolve_path(cfg, cfg$paths$forest_reference, TRUE), stringsAsFactors = FALSE)
  require_columns(good, c(keys, "reference_height_m"), "Good-reference table")
  require_columns(forest, c(keys, "forest_height_m"), "Forest-reference table")
  if (anyDuplicated(good[keys])) stop("Good-reference keys are not unique.", call. = FALSE)
  if (anyDuplicated(forest[keys])) stop("Forest-reference keys are not unique.", call. = FALSE)

  archive <- resolve_path(cfg, cfg$paths$ortho_archive, TRUE)
  if (!dir.exists(archive)) stop("Orthophoto archive is not a directory: ", archive, call. = FALSE)
  if (!identical(cfg$imagery$source, "existing_nib_archive")) {
    stop("imagery.source must be 'existing_nib_archive' in this revision.", call. = FALSE)
  }
  expected_width <- cfg$imagery$resolution_m * cfg$imagery$source_tile_pixels
  if (!isTRUE(all.equal(as.numeric(expected_width),
                        as.numeric(cfg$imagery$source_tile_width_m)))) {
    stop("resolution_m * source_tile_pixels must equal source_tile_width_m.", call. = FALSE)
  }
  chip <- as.integer(cfg$imagery$model_chip_pixels)
  overlap <- as.integer(cfg$imagery$chip_overlap_pixels)
  if (chip != 256L) stop("Meta inference requires model_chip_pixels: 256.", call. = FALSE)
  if (overlap < 0L || overlap >= chip) stop("chip_overlap_pixels must be in [0, 255].", call. = FALSE)
  if (!cfg$imagery$resolution_m %in% c(0.5, 1.0)) {
    if (!isTRUE(cfg$imagery$allow_untested_resolution)) {
      stop("The Meta workflow has only been evaluated at 0.5 and 1 m. Set " ,
           "allow_untested_resolution: true only for an explicitly experimental run.", call. = FALSE)
    }
    warning("The ", cfg$imagery$resolution_m,
            " m imagery is outside the evaluated 0.5/1 m range; outputs are experimental.")
  }
  low <- as.numeric(cfg$model$input_low_quantile)
  high <- as.numeric(cfg$model$input_high_quantile)
  if (!is.finite(low) || !is.finite(high) || low < 0 || high > 1 || low >= high) {
    stop("model input quantiles must satisfy 0 <= low < high <= 1.", call. = FALSE)
  }
  if (!is.finite(as.numeric(cfg$model$output_scale)) ||
      !is.finite(as.numeric(cfg$model$output_offset_m))) {
    stop("model output scale and offset must be finite.", call. = FALSE)
  }
  if (require_model) {
    model_repository <- resolve_path(cfg, cfg$model$model_repository, TRUE)
    resolve_path(cfg, cfg$model$checkpoint, TRUE)
    resolve_path(cfg, cfg$model$normalization_checkpoint, TRUE)
    expected_revision <- cfg$model$repository_revision %||% ""
    if (!nzchar(expected_revision) || expected_revision == "REQUIRED_PINNED_COMMIT") {
      stop("Set model.repository_revision to the reviewed Meta model Git commit.", call. = FALSE)
    }
    actual_revision <- tryCatch(
      system2("git", c("-C", model_repository, "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
      error = function(e) character()
    )
    if (!length(actual_revision) || !startsWith(actual_revision[[1]], expected_revision)) {
      stop("Meta model checkout does not match model.repository_revision.", call. = FALSE)
    }
  }
  message("Input validation passed for ", nrow(aoi), " AOIs and five regions.")
  invisible(TRUE)
}
