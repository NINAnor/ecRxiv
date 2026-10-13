#!/usr/bin/env Rscript

args_all <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args_all, value = TRUE)
script <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else "tests/run_synthetic_end_to_end.R"
root <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
Sys.setenv(NO_GJEN_002_ROOT = root)

for (package in c("sf", "terra", "exactextractr", "dplyr", "yaml")) {
  requireNamespace(package, quietly = TRUE) || stop("Missing R package: ", package, call. = FALSE)
}
source(file.path(root, "R", "pipeline_common.R"))
source(file.path(root, "R", "calculate_zonal_stats.R"))
source(file.path(root, "R", "calculate_indicator.R"))

work <- file.path(tempdir(), paste0("NO_GJEN_002_synthetic_", Sys.getpid()))
dir.create(work, recursive = TRUE)
on.exit(unlink(work, recursive = TRUE), add = TRUE)
dir.create(file.path(work, "predictions"))
dir.create(file.path(work, "outputs"))

rectangle <- function(xmin, ymin, xmax, ymax) {
  sf::st_polygon(list(matrix(c(xmin,ymin, xmin,ymax, xmax,ymax, xmax,ymin, xmin,ymin),
                             ncol = 2, byrow = TRUE)))
}
region_names <- c("Nord-Norge", "Midt-Norge", "Ostlandet", "Vestlandet", "Sorlandet")
regions <- sf::st_sf(
  region_id = as.character(seq_len(5)), region_name = region_names,
  geometry = sf::st_sfc(lapply(0:4, function(i) rectangle(i*200, 0, (i+1)*200, 100)),
                        crs = 25833))
aoi <- sf::st_sf(
  id = paste0("aoi", 1:5),
  ecosystem = c("wetland", "semi-natural", "open", "wetland", "open"),
  geometry = sf::st_sfc(lapply(0:4, function(i) rectangle(i*200+20, 20, i*200+180, 80)),
                        crs = 25833))
zones <- sf::st_sf(vegClimZone = "BN", geometry = sf::st_sfc(rectangle(0,0,1000,100), crs=25833))
sf::st_write(aoi, file.path(work, "aoi.gpkg"), quiet=TRUE)
sf::st_write(regions, file.path(work, "regions.gpkg"), quiet=TRUE)
sf::st_write(zones, file.path(work, "zones.gpkg"), quiet=TRUE)

keys <- data.frame(
  ecosystem = aoi$ecosystem, region_id = as.character(1:5), bioclimatic_zone = "BN",
  stringsAsFactors = FALSE)
good <- transform(keys, reference_height_m = 1)
forest <- transform(keys, forest_height_m = 10)
utils::write.csv(good, file.path(work,"good.csv"), row.names=FALSE)
utils::write.csv(forest, file.path(work,"forest.csv"), row.names=FALSE)

r <- terra::rast(ncols=100, nrows=10, xmin=0, xmax=1000, ymin=0, ymax=100,
                 crs="EPSG:25833")
terra::values(r) <- 3
prediction <- file.path(work,"predictions","x0_y0_pred.tif")
terra::writeRaster(r, prediction, overwrite=TRUE, datatype="FLT4S", NAflag=-9999)

utils::write.csv(data.frame(
  tile_id="x0_y0", image_date="2024-07-15", status="indexed"),
  file.path(work,"tile_manifest.csv"), row.names=FALSE)
utils::write.csv(data.frame(
  tile_id=rep("x0_y0",5), aoi_id=aoi$id),
  file.path(work,"tile_aoi_map.csv"), row.names=FALSE)
utils::write.csv(data.frame(
  tile_id="x0_y0", tile="synthetic", prediction=prediction,
  status="completed", message=""),
  file.path(work,"inference_manifest.csv"), row.names=FALSE)

cfg <- list(
  root=work,
  project=list(indicator_id="NO_GJEN_002",version="synthetic",reporting_year=2024,target_crs=25833),
  paths=list(
    aoi="aoi.gpkg", regions="regions.gpkg", bioclimatic_zones="zones.gpkg",
    good_reference="good.csv", forest_reference="forest.csv",
    predictions="predictions", inference_manifest="inference_manifest.csv",
    zonal_statistics="polygon_statistics.gpkg", tile_manifest="tile_manifest.csv",
    tile_aoi_map="tile_aoi_map.csv", outputs="outputs"),
  fields=list(aoi_id="id",ecosystem="ecosystem",region_id="region_id",
              region_name="region_name",bioclimatic_zone="vegClimZone"),
  indicator=list(minimum_raster_coverage=0.80,aggregation="area_weighted"))

stats <- calculate_zonal_statistics(cfg)
stopifnot(nrow(stats)==5L, all(abs(stats$canopy_median_m-3)<1e-6),
          all(stats$raster_coverage>0.99))
result <- calculate_indicator(cfg)
stopifnot(nrow(result$summary)==6L, result$summary$region_id[[1]]=="NO",
          all(is.finite(result$polygons$indicator_value)),
          file.exists(file.path(work,"outputs","NO_GJEN_002_results.gpkg")))
message("SYNTHETIC END-TO-END TEST PASSED: 5 AOIs, 5 regions, zonal statistics and indicator outputs")
