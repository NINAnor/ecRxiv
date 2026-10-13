#!/usr/bin/env Rscript

args_all <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args_all, value = TRUE)
script <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else "tests/run_x100_v0_4_candidate_contract.R"
root <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
Sys.setenv(NO_GJEN_002_ROOT = root)
for (package in c("sf", "terra", "exactextractr", "dplyr", "yaml")) {
  requireNamespace(package, quietly = TRUE) || stop("Missing R package: ", package, call. = FALSE)
}
source(file.path(root, "R", "pipeline_common.R"))
source(file.path(root, "R", "calculate_zonal_stats_x100_v0_4.R"))
source(file.path(root, "R", "calculate_indicator_x100_v0_4.R"))

calibration <- list(
  method = "piecewise_knot_1m", source_metric = "qa_le10_mean_m",
  qa_min_m = 0, qa_max_m = 10, minimum_retained_fraction = 0.80,
  knot_m = 1.0, lower_slope = 3.752292459275057,
  upper_slope = 0.7369730201769807)
cfg_cal <- list(calibration = calibration)
expected <- c(0, 3.752292459275057,
              3.752292459275057 + 0.7369730201769807)
stopifnot(max(abs(calibrate_x100_v0_4(c(0, 1, 2), cfg_cal) - expected)) < 1e-12)

work <- file.path(tempdir(), paste0("NO_GJEN_002_x100_v0_4_", Sys.getpid()))
dir.create(work, recursive = TRUE)
on.exit(unlink(work, recursive = TRUE), add = TRUE)
dir.create(file.path(work, "predictions"))
dir.create(file.path(work, "outputs"))
rectangle <- function(xmin, ymin, xmax, ymax) {
  sf::st_polygon(list(matrix(c(xmin,ymin, xmin,ymax, xmax,ymax, xmax,ymin, xmin,ymin),
                             ncol = 2, byrow = TRUE)))
}
regions <- sf::st_sf(
  region_id = as.character(1:5), region_name = paste0("Region", 1:5),
  geometry = sf::st_sfc(lapply(0:4, function(i) rectangle(i*200, 0, (i+1)*200, 100)), crs = 25833))
aoi <- sf::st_sf(
  id = paste0("aoi", 1:5),
  ecosystem = c("Vaatmark", "Semi-naturlig", "Naturlig aapne", "Vaatmark", "Naturlig aapne"),
  geometry = sf::st_sfc(lapply(0:4, function(i) rectangle(i*200+20, 20, i*200+180, 80)), crs = 25833))
zones <- sf::st_sf(KLASSE = "2", geometry = sf::st_sfc(rectangle(0,0,1000,100), crs=25833))
sf::st_write(aoi, file.path(work, "aoi.gpkg"), quiet=TRUE)
sf::st_write(regions, file.path(work, "regions.gpkg"), quiet=TRUE)
sf::st_write(zones, file.path(work, "zones.gpkg"), quiet=TRUE)
keys <- data.frame(ecosystem=aoi$ecosystem, region_id=as.character(1:5),
                   bioclimatic_zone="2", stringsAsFactors=FALSE)
utils::write.csv(transform(keys, reference_height_m=1), file.path(work,"good.csv"), row.names=FALSE)
utils::write.csv(transform(keys, forest_height_m=10), file.path(work,"forest.csv"), row.names=FALSE)
r <- terra::rast(ncols=100, nrows=10, xmin=0, xmax=1000, ymin=0, ymax=100, crs="EPSG:25833")
terra::values(r) <- 0.5
prediction <- file.path(work,"predictions","x0_y0_pred.tif")
terra::writeRaster(r, prediction, overwrite=TRUE, datatype="FLT4S", NAflag=-9999)
utils::write.csv(data.frame(tile_id="x0_y0", image_date="2024-07-15", status="indexed"),
                 file.path(work,"tile_manifest.csv"), row.names=FALSE)
utils::write.csv(data.frame(tile_id=rep("x0_y0",5), aoi_id=aoi$id),
                 file.path(work,"tile_aoi_map.csv"), row.names=FALSE)
utils::write.csv(data.frame(tile_id="x0_y0", tile="synthetic", prediction=prediction,
                            status="completed", message=""),
                 file.path(work,"inference_manifest.csv"), row.names=FALSE)
cfg <- list(
  root=work, project=list(indicator_id="NO_GJEN_002",version="x100-test",reporting_year=2024,target_crs=25833),
  paths=list(aoi="aoi.gpkg",regions="regions.gpkg",bioclimatic_zones="zones.gpkg",
             good_reference="good.csv",forest_reference="forest.csv",predictions="predictions",
             inference_manifest="inference_manifest.csv",zonal_statistics="polygon_statistics.gpkg",
             tile_manifest="tile_manifest.csv",tile_aoi_map="tile_aoi_map.csv",outputs="outputs"),
  fields=list(aoi_id="id",ecosystem="ecosystem",region_id="region_id",
              region_name="region_name",bioclimatic_zone="KLASSE"),
  indicator=list(minimum_raster_coverage=0.80,aggregation="area_weighted",
                 resolution_status="candidate_validated_1m",calibration_status="x100_1m_v0_4_candidate"),
  calibration=calibration)
stats <- calculate_zonal_statistics_x100_v0_4(cfg)
stopifnot(nrow(stats)==5L, all(abs(stats$canopy_mean_qa_raw_m-0.5)<1e-10),
          all(abs(stats$canopy_calibrated_m-1.8761462296375285)<1e-10),
          all(stats$qa_le10_retained_fraction>0.99))
result <- calculate_indicator_x100_v0_4(cfg)
stopifnot(nrow(result$summary)==6L, all(is.finite(result$polygons$indicator_value)),
          file.exists(file.path(work,"outputs","NO_GJEN_002_results_x100_v0_4.gpkg")))
message("X100 V0.4 SYNTHETIC PIPELINE CONTRACT: PASS")

pilot_path <- "C:/NO_GJEN_002_RUNTIME/x100_combined_pilot85_v0_4/x100_combined_pilot85_polygon_statistics.csv"
good_path <- file.path(root,"data","x100_reference_v0_4","reference_good_1m_v0_4.csv")
forest_path <- file.path(root,"data","x100_reference_v0_4","reference_forest_1m_v0_4.csv")
pilot <- utils::read.csv(pilot_path, sep=";", stringsAsFactors=FALSE, check.names=FALSE)
good <- utils::read.csv(good_path, stringsAsFactors=FALSE)
forest <- utils::read.csv(forest_path, stringsAsFactors=FALSE)
truth <- function(x) tolower(trimws(as.character(x))) %in% c("true","1","yes","ja")
analysis_ok <- tolower(trimws(pilot$analysis_status)) %in% c("completed","existing")
zonal_ok <- tolower(trimws(pilot$zonal_status)) %in% c("completed","existing")
keep <- pilot$ecosystem %in% c("Naturlig aapne","Semi-naturlig","Vaatmark") &
  !grepl("alpin sone", pilot$vegClimZoneLab, ignore.case=TRUE) &
  (analysis_ok | zonal_ok) & truth(pilot$qa_le10_eligible) &
  is.finite(pilot$qa_le10_mean_m) & is.finite(pilot$qa_le10_retained_fraction) &
  pilot$qa_le10_retained_fraction >= calibration$minimum_retained_fraction &
  is.finite(pilot$area_m2) & pilot$area_m2 > 0
pilot <- pilot[keep,]
stopifnot(nrow(pilot)==591L)
pilot$region_id <- as.character(pilot$region_code)
pilot$bioclimatic_zone <- as.character(pilot$zone_code)
for (key in c("ecosystem","region_id","bioclimatic_zone")) {
  good[[key]] <- as.character(good[[key]]); forest[[key]] <- as.character(forest[[key]])
}
pilot <- merge(pilot, good, by=c("ecosystem","region_id","bioclimatic_zone"), all.x=TRUE, sort=FALSE)
pilot <- merge(pilot, forest, by=c("ecosystem","region_id","bioclimatic_zone"), all.x=TRUE, sort=FALSE)
stopifnot(all(is.finite(pilot$reference_height_m)), all(is.finite(pilot$forest_height_m)))
pilot$canopy_calibrated_m <- calibrate_x100_v0_4(pilot$qa_le10_mean_m, cfg_cal)
pilot$indicator_value <- scale_encroachment_x100_v0_4(
  pilot$canopy_calibrated_m,pilot$reference_height_m,pilot$forest_height_m)
pilot_value <- stats::weighted.mean(pilot$indicator_value,pilot$area_m2)
stopifnot(abs(pilot_value-0.96271791)<5e-8)
message(sprintf("PILOT85 NUMERICAL CONTRACT: PASS (%.8f; n=%d)",pilot_value,nrow(pilot)))
