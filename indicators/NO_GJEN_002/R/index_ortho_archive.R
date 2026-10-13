parse_tile_name <- function(path) {
  name <- basename(path)
  match <- regexec("^nib_([0-9]{4})_x(-?[0-9]+)_y(-?[0-9]+)\\.jpg$",
                   name, ignore.case = TRUE)
  parts <- regmatches(name, match)[[1]]
  if (!length(parts)) return(NULL)
  data.frame(
    tile_id = sprintf("x%s_y%s", parts[[3]], parts[[4]]),
    image_year = as.integer(parts[[2]]),
    xmin = as.numeric(parts[[3]]),
    ymin = as.numeric(parts[[4]]),
    file = normalizePath(path, winslash = "/", mustWork = TRUE),
    stringsAsFactors = FALSE
  )
}

read_block_metadata <- function(archive) {
  registers <- list.files(archive, pattern = "^flisregister\\.csv$",
                          recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  rows <- lapply(registers, function(path) {
    x <- tryCatch(utils::read.csv(path, sep = ";", stringsAsFactors = FALSE,
                                  fileEncoding = "UTF-8-BOM"),
                  error = function(e) NULL)
    if (is.null(x) || !nrow(x) || !all(c("XMin", "YMin") %in% names(x))) return(NULL)
    data.frame(
      tile_id = sprintf("x%s_y%s", x$XMin, x$YMin),
      image_date = if ("Fotodato" %in% names(x)) as.character(x$Fotodato) else NA_character_,
      project = if ("Prosjekt" %in% names(x)) as.character(x$Prosjekt) else NA_character_,
      project_id = if ("ProsjektID" %in% names(x)) as.character(x$ProsjektID) else NA_character_,
      source_resolution = if ("Kildeopplosning" %in% names(x)) as.character(x$Kildeopplosning) else NA_character_,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  result <- do.call(rbind, rows)
  result[!duplicated(result$tile_id), , drop = FALSE]
}

index_ortho_archive <- function(cfg, overwrite = FALSE) {
  manifest_path <- resolve_path(cfg, cfg$paths$tile_manifest)
  map_path <- resolve_path(cfg, cfg$paths$tile_aoi_map)
  if (file.exists(manifest_path) && file.exists(map_path) && !overwrite) {
    return(utils::read.csv(manifest_path, stringsAsFactors = FALSE))
  }

  archive <- resolve_path(cfg, cfg$paths$ortho_archive, TRUE)
  files <- list.files(archive, pattern = "^nib_[0-9]{4}_x-?[0-9]+_y-?[0-9]+\\.jpg$",
                      recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  parsed <- Filter(Negate(is.null), lapply(files, parse_tile_name))
  if (!length(parsed)) stop("No completed nib_YEAR_xX_yY.jpg tiles found in ", archive, call. = FALSE)
  manifest <- do.call(rbind, parsed)
  if (anyDuplicated(manifest$tile_id)) stop("Duplicate tile coordinates found in orthophoto archive.", call. = FALSE)
  width <- as.numeric(cfg$imagery$source_tile_width_m)
  manifest$xmax <- manifest$xmin + width
  manifest$ymax <- manifest$ymin + width
  manifest$status <- "indexed"

  metadata <- read_block_metadata(archive)
  if (!is.null(metadata)) manifest <- merge(manifest, metadata, by = "tile_id", all.x = TRUE, sort = FALSE)
  if (!"image_date" %in% names(manifest)) manifest$image_date <- NA_character_
  missing_date <- is.na(manifest$image_date) | !nzchar(manifest$image_date)
  manifest$image_date[missing_date] <- sprintf("%04d-01-01", manifest$image_year[missing_date])
  manifest <- manifest[order(manifest$ymin, manifest$xmin), , drop = FALSE]

  aoi <- read_spatial_input(resolve_path(cfg, cfg$paths$aoi, TRUE),
                            cfg$project$target_crs, "AOI layer")
  id_field <- cfg$fields$aoi_id
  require_columns(aoi, id_field, "AOI layer")
  box_geometry <- sf::st_sfc(lapply(seq_len(nrow(manifest)), function(i) {
    sf::st_polygon(list(matrix(c(
      manifest$xmin[[i]], manifest$ymin[[i]], manifest$xmin[[i]], manifest$ymax[[i]],
      manifest$xmax[[i]], manifest$ymax[[i]], manifest$xmax[[i]], manifest$ymin[[i]],
      manifest$xmin[[i]], manifest$ymin[[i]]
    ), ncol = 2, byrow = TRUE)))
  }), crs = cfg$project$target_crs)
  boxes <- sf::st_sf(manifest, geometry = box_geometry)
  hits <- sf::st_intersects(boxes, aoi)
  keep <- lengths(hits) > 0L
  mapping <- do.call(rbind, lapply(which(keep), function(i) {
    data.frame(tile_id = manifest$tile_id[[i]],
               aoi_id = as.character(aoi[[id_field]][hits[[i]]]),
               stringsAsFactors = FALSE)
  }))
  if (is.null(mapping) || !nrow(mapping)) stop("No orthophoto tiles intersect the AOI layer.", call. = FALSE)
  manifest$selected_for_inference <- keep
  atomic_write_csv(mapping, map_path)
  atomic_write_csv(manifest, manifest_path)
  message("Indexed ", nrow(manifest), " archive tiles; ", sum(keep), " intersect one or more AOIs.")
  manifest
}
