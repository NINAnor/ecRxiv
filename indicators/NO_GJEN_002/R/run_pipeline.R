#!/usr/bin/env Rscript

script_args <- commandArgs(trailingOnly = FALSE)
script_flag <- grep("^--file=", script_args, value = TRUE)
script_path <- if (length(script_flag)) sub("^--file=", "", script_flag[[1]]) else "R/run_pipeline.R"
root_guess <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
Sys.setenv(NO_GJEN_002_ROOT = root_guess)

source(file.path(root_guess, "R", "pipeline_common.R"))
source(file.path(root_guess, "R", "validate_inputs.R"))
source(file.path(root_guess, "R", "index_ortho_archive.R"))
source(file.path(root_guess, "R", "calculate_zonal_stats.R"))
source(file.path(root_guess, "R", "calculate_indicator.R"))

options <- parse_cli()
cfg <- read_pipeline_config(options$config)
valid_stages <- c("validate", "preflight", "manifest", "inference", "zonal", "indicator", "all")
if (!options$stage %in% valid_stages) stop("--stage must be one of: ", paste(valid_stages, collapse = ", "))

if (options$stage %in% c("validate", "preflight", "all")) {
  validate_inputs(cfg, require_model = options$stage %in% c("preflight", "all"))
}
if (options$stage %in% c("manifest", "all")) index_ortho_archive(cfg, options$overwrite)
if (options$stage %in% c("inference", "all")) {
  args <- c(
    resolve_path(cfg, cfg$model$script, TRUE),
    "--source-manifest", resolve_path(cfg, cfg$paths$tile_manifest, TRUE),
    "--aoi-map", resolve_path(cfg, cfg$paths$tile_aoi_map, TRUE),
    "--output", resolve_path(cfg, cfg$paths$predictions),
    "--model-repo", resolve_path(cfg, cfg$model$model_repository, TRUE),
    "--checkpoint", resolve_path(cfg, cfg$model$checkpoint, TRUE),
    "--normalization-checkpoint", resolve_path(cfg, cfg$model$normalization_checkpoint, TRUE),
    "--batch-size", cfg$model$batch_size,
    "--workers", cfg$model$workers,
    "--source-tile-size", cfg$imagery$source_tile_pixels,
    "--chip-size", cfg$imagery$model_chip_pixels,
    "--chip-overlap", cfg$imagery$chip_overlap_pixels,
    "--resolution", cfg$imagery$resolution_m,
    "--target-crs", cfg$project$target_crs,
    "--device", cfg$model$device,
    "--input-low-quantile", cfg$model$input_low_quantile,
    "--input-high-quantile", cfg$model$input_high_quantile,
    "--output-scale", cfg$model$output_scale,
    "--output-offset", cfg$model$output_offset_m,
    "--model-revision", cfg$model$repository_revision,
    "--manifest", resolve_path(cfg, cfg$paths$inference_manifest)
  )
  if (options$overwrite) args <- c(args, "--overwrite")
  max_tiles <- suppressWarnings(as.integer(Sys.getenv("NO_GJEN_MAX_TILES", unset = "0")))
  if (is.finite(max_tiles) && max_tiles > 0L) args <- c(args, "--max-tiles", max_tiles)
  status <- system2(cfg$model$python, args)
  if (!identical(status, 0L)) stop("Python inference failed with status ", status, call. = FALSE)
}
if (options$stage %in% c("zonal", "all")) calculate_zonal_statistics(cfg)
if (options$stage %in% c("indicator", "all")) calculate_indicator(cfg)
