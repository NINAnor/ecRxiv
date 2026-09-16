scale_encroachment <- function(population_height, reference_height, forest_height) {
  denominator <- forest_height - reference_height
  invalid <- !is.finite(population_height) | !is.finite(reference_height) |
    !is.finite(forest_height) | denominator <= 0
  relative <- (population_height - reference_height) / denominator
  relative <- pmin(pmax(relative, 0), 1)
  deterioration <- 100.68 * (1 - exp(-5 * relative^2.5)) / 100
  condition <- 1 - deterioration
  condition[invalid] <- NA_real_
  round(condition, 4)
}

weighted_result <- function(values, weights) {
  keep <- is.finite(values) & is.finite(weights) & weights > 0
  if (!any(keep)) return(c(value = NA_real_, area_m2 = 0, n = 0))
  c(value = stats::weighted.mean(values[keep], weights[keep]),
    area_m2 = sum(weights[keep]), n = sum(keep))
}

assign_largest_overlap <- function(x, zones, zone_fields) {
  sf::st_join(x, zones[zone_fields], left = TRUE, largest = TRUE)
}

calculate_indicator <- function(cfg) {
  requireNamespace("dplyr", quietly = TRUE) || stop("Package 'dplyr' is required.", call. = FALSE)
  stats_path <- resolve_path(cfg, cfg$paths$zonal_statistics, must_exist = TRUE)
  x <- sf::st_read(stats_path, layer = "polygon_statistics", quiet = TRUE)
  id_field <- cfg$fields$aoi_id
  ecosystem_field <- cfg$fields$ecosystem
  require_columns(x, c(id_field, ecosystem_field, "canopy_median_m", "raster_coverage", "area_m2"),
                  "Polygon statistics")

  regions <- read_spatial_input(resolve_path(cfg, cfg$paths$regions, TRUE),
                                cfg$project$target_crs, "Region layer")
  zones <- read_spatial_input(resolve_path(cfg, cfg$paths$bioclimatic_zones, TRUE),
                              cfg$project$target_crs, "Bioclimatic-zone layer")
  require_columns(regions, c(cfg$fields$region_id, cfg$fields$region_name), "Region layer")
  require_columns(zones, cfg$fields$bioclimatic_zone, "Bioclimatic-zone layer")
  x <- assign_largest_overlap(x, regions, c(cfg$fields$region_id, cfg$fields$region_name))
  x <- assign_largest_overlap(x, zones, cfg$fields$bioclimatic_zone)

  good <- utils::read.csv(resolve_path(cfg, cfg$paths$good_reference, TRUE), stringsAsFactors = FALSE)
  forest <- utils::read.csv(resolve_path(cfg, cfg$paths$forest_reference, TRUE), stringsAsFactors = FALSE)
  keys <- c("ecosystem", "region_id", "bioclimatic_zone")
  require_columns(good, c(keys, "reference_height_m"), "Good-reference table")
  require_columns(forest, c(keys, "forest_height_m"), "Forest-reference table")

  names(x)[names(x) == ecosystem_field] <- "ecosystem"
  names(x)[names(x) == cfg$fields$region_id] <- "region_id"
  names(x)[names(x) == cfg$fields$region_name] <- "region_name"
  names(x)[names(x) == cfg$fields$bioclimatic_zone] <- "bioclimatic_zone"

  # CSV type inference can represent numeric-looking region or zone identifiers
  # as integers even when spatial inputs store them as character strings.
  # Join identifiers are categorical, so normalize all keys before joining.
  for (key in keys) {
    x[[key]] <- as.character(x[[key]])
    good[[key]] <- as.character(good[[key]])
    forest[[key]] <- as.character(forest[[key]])
  }

  x <- dplyr::left_join(x, good, by = keys)
  x <- dplyr::left_join(x, forest, by = keys)
  x$eligible <- x$raster_coverage >= cfg$indicator$minimum_raster_coverage &
    is.finite(x$canopy_median_m) & is.finite(x$reference_height_m) & is.finite(x$forest_height_m)
  x$eligible[is.na(x$eligible)] <- FALSE
  x$indicator_value <- scale_encroachment(x$canopy_median_m,
                                          x$reference_height_m,
                                          x$forest_height_m)
  x$indicator_value[!x$eligible] <- NA_real_

  # Split polygon area at region borders for correct regional weights while
  # retaining the polygon-level indicator value.
  regional_pieces <- suppressWarnings(sf::st_intersection(
    x[c(id_field, "indicator_value", "eligible")],
    regions[c(cfg$fields$region_id, cfg$fields$region_name)]
  ))
  regional_pieces$weight_m2 <- as.numeric(sf::st_area(regional_pieces))
  regional_observed <- regional_pieces |>
    sf::st_drop_geometry() |>
    dplyr::group_by(.data[[cfg$fields$region_id]], .data[[cfg$fields$region_name]]) |>
    dplyr::summarise(
      regional_indicator_value = weighted_result(indicator_value, weight_m2)[["value"]],
      included_area_m2 = weighted_result(indicator_value, weight_m2)[["area_m2"]],
      total_area_m2 = sum(weight_m2, na.rm = TRUE),
      n_polygons_included = dplyr::n_distinct(.data[[id_field]][is.finite(indicator_value)]),
      n_polygons_total = dplyr::n_distinct(.data[[id_field]]),
      .groups = "drop"
    )
  names(regional_observed)[1:3] <- c("region_id", "region_name", "indicator_value")
  region_lookup <- unique(sf::st_drop_geometry(regions)[c(cfg$fields$region_id, cfg$fields$region_name)])
  names(region_lookup) <- c("region_id", "region_name")
  regional <- dplyr::left_join(region_lookup, regional_observed,
                               by = c("region_id", "region_name"))
  regional$included_area_m2[is.na(regional$included_area_m2)] <- 0
  regional$total_area_m2[is.na(regional$total_area_m2)] <- 0
  regional$n_polygons_included[is.na(regional$n_polygons_included)] <- 0
  regional$n_polygons_total[is.na(regional$n_polygons_total)] <- 0
  national_value <- weighted_result(x$indicator_value, x$area_m2)
  national <- data.frame(
    region_id = "NO", region_name = "Norge",
    indicator_value = national_value[["value"]],
    included_area_m2 = national_value[["area_m2"]],
    total_area_m2 = sum(x$area_m2, na.rm = TRUE),
    n_polygons_included = national_value[["n"]],
    n_polygons_total = nrow(x)
  )
  summary <- rbind(national, as.data.frame(regional))
  summary$coverage_fraction <- ifelse(summary$total_area_m2 > 0,
                                      summary$included_area_m2 / summary$total_area_m2,
                                      NA_real_)
  summary$reporting_year <- cfg$project$reporting_year
  summary$version <- cfg$project$version
  summary$resolution_status <- cfg$indicator$resolution_status
  summary$calibration_status <- cfg$indicator$calibration_status

  out_dir <- resolve_path(cfg, cfg$paths$outputs)
  ensure_directory(out_dir)
  atomic_write_csv(sf::st_drop_geometry(x), file.path(out_dir, "NO_GJEN_002_polygon_values.csv"))
  atomic_write_csv(summary, file.path(out_dir, "NO_GJEN_002_national_regions.csv"))
  coverage <- sf::st_drop_geometry(x) |>
    dplyr::mutate(image_year = as.integer(format(as.Date(image_date_latest), "%Y"))) |>
    dplyr::group_by(region_name, image_year) |>
    dplyr::summarise(n_polygons = dplyr::n(), area_m2 = sum(area_m2, na.rm = TRUE),
                     eligible_area_m2 = sum(area_m2[eligible], na.rm = TRUE), .groups = "drop")
  atomic_write_csv(coverage, file.path(out_dir, "NO_GJEN_002_coverage_by_image_year.csv"))
  sf::st_write(x, file.path(out_dir, "NO_GJEN_002_results.gpkg"),
               layer = "polygon_values", delete_dsn = TRUE, quiet = TRUE)
  list(polygons = x, summary = summary, coverage = coverage)
}
