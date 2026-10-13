`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || identical(x, "")) y else x

indicator_root <- function() {
  env_root <- Sys.getenv("NO_GJEN_002_ROOT", unset = "")
  if (nzchar(env_root)) return(normalizePath(env_root, mustWork = TRUE))

  candidates <- c(getwd(), file.path(getwd(), "indicators", "NO_GJEN_002"))
  hit <- candidates[file.exists(file.path(candidates, "config.yml"))]
  if (!length(hit)) {
    stop("Cannot locate NO_GJEN_002/config.yml. Set NO_GJEN_002_ROOT.", call. = FALSE)
  }
  normalizePath(hit[[1]], mustWork = TRUE)
}

read_pipeline_config <- function(path = NULL) {
  requireNamespace("yaml", quietly = TRUE) ||
    stop("Package 'yaml' is required.", call. = FALSE)
  root <- indicator_root()
  path <- path %||% file.path(root, "config.yml")
  cfg <- yaml::read_yaml(path)
  cfg$root <- root
  cfg$config_path <- normalizePath(path, mustWork = TRUE)
  cfg
}

resolve_path <- function(cfg, path, must_exist = FALSE) {
  if (is.null(path) || !nzchar(path)) return(NA_character_)
  value <- if (grepl("^([A-Za-z]:|/)", path)) path else file.path(cfg$root, path)
  if (must_exist && !file.exists(value)) {
    stop("Required path does not exist: ", value, call. = FALSE)
  }
  value
}

ensure_directory <- function(path) {
  if (!dir.exists(path) && !dir.create(path, recursive = TRUE)) {
    stop("Could not create directory: ", path, call. = FALSE)
  }
  invisible(path)
}

require_columns <- function(x, columns, label) {
  missing <- setdiff(columns, names(x))
  if (length(missing)) {
    stop(label, " is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(x)
}

read_spatial_input <- function(path, target_crs, label) {
  requireNamespace("sf", quietly = TRUE) || stop("Package 'sf' is required.", call. = FALSE)
  x <- sf::st_read(path, quiet = TRUE)
  if (is.na(sf::st_crs(x))) stop(label, " has no coordinate reference system.", call. = FALSE)
  x <- sf::st_make_valid(x)
  if (sf::st_crs(x)$epsg != target_crs) x <- sf::st_transform(x, target_crs)
  if (!nrow(x)) stop(label, " contains no features.", call. = FALSE)
  x
}

atomic_write_csv <- function(x, path) {
  ensure_directory(dirname(path))
  temporary <- paste0(path, ".tmp")
  utils::write.csv(x, temporary, row.names = FALSE, na = "")
  if (file.exists(path) && !unlink(path)) stop("Could not replace ", path, call. = FALSE)
  if (!file.rename(temporary, path)) stop("Could not write ", path, call. = FALSE)
  invisible(path)
}

parse_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  out <- list(config = NULL, stage = "all", overwrite = FALSE)
  i <- 1L
  while (i <= length(args)) {
    if (args[[i]] == "--config") {
      i <- i + 1L; out$config <- args[[i]]
    } else if (args[[i]] == "--stage") {
      i <- i + 1L; out$stage <- args[[i]]
    } else if (args[[i]] == "--overwrite") {
      out$overwrite <- TRUE
    } else {
      stop("Unknown argument: ", args[[i]], call. = FALSE)
    }
    i <- i + 1L
  }
  out
}
