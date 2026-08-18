# Functions for NO_IDEX_001 composite index calculation
# Tidyverse orchestration + ecTools::ec_upscale for MC aggregation
#
# Sections follow the analysis pipeline:
#   helpers → catalog → read/harmonise → source resolution →
#   registry → load draws → aggregate → summarise/plot/export

# =============================================================================
# 1. Constants and small helpers
# =============================================================================

# NI region codes used throughout (plus Norway).
CANONICAL_PARTS <- c("C", "E", "S", "W", "N", "Norway")

# Extract ECT class code (e.g. A1) from a label.
parse_ect <- function(x) {
  stringr::str_extract(stringr::str_squish(x), "^[ABC][0-9]")
}

# Load alias → part lookup table.
load_region_lookup <- function(path = here::here("data/region_lookup.csv")) {
  lookup <- readr::read_csv(path, show_col_types = FALSE)
  stats::setNames(lookup$part, lookup$alias)
}

# Map region labels to canonical NI parts.
standardize_part <- function(x, lookup = load_region_lookup()) {
  x_clean <- stringr::str_squish(as.character(x))
  out <- lookup[x_clean]
  unknown <- is.na(out) & !is.na(x_clean) & x_clean != ""
  if (any(unknown)) {
    warning(
      "Unknown region labels: ",
      paste(unique(x_clean[unknown]), collapse = ", "),
      call. = FALSE
    )
  }
  out
}

# Map a reporting period to its end year.
period_to_year <- function(period) {
  p <- as.character(period)
  out <- suppressWarnings(as.integer(p))
  needs_parse <- is.na(out) | nchar(p) > 4L
  if (any(needs_parse, na.rm = TRUE)) {
    end_year <- stringr::str_match(p[needs_parse], "-([0-9]{4})$")[, 2]
    out[needs_parse] <- as.integer(end_year)
  }
  out
}

# TRUE if path is an http(s) URL.
is_results_url <- function(path) {
  grepl("^https?://", path)
}

# =============================================================================
# 2. Indicator catalog and ID mapping
# =============================================================================

# Known ecRxiv folders, IDs, and ecosystems.
indicator_catalog <- function() {
  tibble::tribble(
    ~folder, ~title, ~indicator_id, ~ecosystem_hint,
    "NO_AATS_001", "Absence of alien coniferous tree species", "NO_AATS_001", "forest",
    "NO_AATS_002", "Alien conifers", "NO_AATS_002", "forest",
    "NO_AFOX_001", "Arctic fox", "NO_AFOX_001", "mountain",
    "NO_ALIE_001", "Alien species", "NO_ALIE_001", "mountain",
    "NO_ALIE_002", "Alien plant species", "NO_ALIE_002", "forest,mountain",
    "NO_BFLY_001_002", "Butterflies", "NO_BFLY_002", "forest",
    "NO_BFLY_001_002", "Butterflies", "NO_BFLY_001", "grassland",
    "NO_BUMB_001_002", "Bumblebees", "NO_BUMB_002", "forest",
    "NO_BUMB_001_002", "Bumblebees", "NO_BUMB_001", "grassland",
    "NO_BBCA_001", "Bilberry cover by area", "NO_BBCA_001", "forest",
    "NO_CONN_001", "Connectivity", "NO_CONN_001", "forest",
    "NO_DEER_001", "Red deer", "NO_DEER_001", "forest",
    "NO_DTVS_001", "Dead trees by volume as share of volume of all trees in productive forests", "NO_DTVS_001", "forest",
    "NO_FUMO_004", "Moisture impact on vegetation", "NO_FUMO_004", "forest",
    "NO_FUNI_004", "Nitrogen impact on vegetation", "NO_FUNI_004", "forest",
    "NO_FUNI_002", "Nitrogen impact on vegetation", "NO_FUNI_002", "grassland",
    "NO_FUHR_002", "Warming impact on vegetation", "NO_FUHR_002", "mountain",
    "NO_GLAC_001", "Glaciers", "NO_GLAC_001", "mountain",
    "NO_GOLD_001", "Golden eagle", "NO_GOLD_001", "mountain",
    "NO_JERV_001", "Wolverine", "NO_JERV_001", "mountain",
    "NO_LAVH_001", "Lichen biomass", "NO_LAVH_001", "mountain",
    "NO_LTFA_001", "Large trees share of forest area", "NO_LTFA_001", "forest",
    "NO_MLFA_001", "Multi-layered forest as share of productive forest area", "NO_MLFA_001", "forest",
    "NO_MOOS_001", "Moose", "NO_MOOS_001", "forest",
    "NO_PTAR_001", "Ptarmigan", "NO_PTAR_001", "mountain",
    "NO_RODE_001", "Rodents", "NO_RODE_001", "mountain",
    "NO_ROED_001", "Roe deer", "NO_ROED_001", "forest",
    "NO_SNOW_001", "Snow depth", "NO_SNOW_001", "mountain",
    "NO_TDTA_001", "Temperate deciduous trees by share of forest area in the nemoral and boreonemoral zones", "NO_TDTA_001", "forest",
    # Published mountain indicator lives in NO_SLIT_001 (file may be named *_002_*).
    "NO_SLIT_001", "Erosive disturbance", "NO_SLIT_001", "mountain",
    "NO_SLIT_001-004", "Erosive disturbance", "NO_SLIT_001", "mountain",
    "NO_SLIT_002", "Erosive disturbance", "NO_SLIT_002", "forest",
    "NO_BEAR_001", "Brown bear", "NO_BAER_001", "forest",
    "NO_LYNX_001", "Lynx", "NO_LYNX_001", "forest",
    "NO_WOLF_001", "Wolf", "NO_WOLF_001", "forest",
    "NO_AIFH_001", "Clear-cutting", "NO_AIFH_001", "forest",
    "NO_AIVS_001", "Ditching", "NO_AIVS_001", "forest",
    "NO_BLAA_001", "Bilberry cover", "NO_BLAA_001", "forest",
    "NO_BGSK_001", "Old forest", "NO_BGSK_001", "forest",
    "NO_ROSE_001", "Rowan, aspen, and goat willow", "NO_ROSE_001", "forest",
    "NO_DLTA_001", "Large trees", "NO_DLTA_001", "forest",
    "NO_DWTO_001", "Dead wood share", "NO_DWTO_001", "forest",
    "NO_ALIE_004", "Alien plant species", "NO_ALIE_004", "forest"
  )
}

# Map registry display names to indicator IDs.
name_to_id_overrides <- function() {
  c(
    "Ditching" = "NO_AIVS_001",
    "Clear-cutting" = "NO_AIFH_001",
    "Moisture impacts on vegetation" = "NO_FUMO_004",
    "Nitrogen impacts on vegetation" = "NO_FUNI_004",
    "Butterflies" = "NO_BFLY_002",
    "Bumblebees" = "NO_BUMB_002",
    "Dead wood volume" = "NO_DTVS_001",
    "Dead wood share" = "NO_DWTO_001",
    "Trampling disturbance" = "NO_SLIT_001",
    "Erosive disturbance" = "NO_SLIT_001",
    "Warming impact on vegetation" = "NO_FUHR_002",
    "Vegetation functional indicators" = "NO_FUHR_002",
    "Alien plantspecies" = "NO_ALIE_004",
    "Alien conifers" = "NO_AATS_002",
    "Rowan, aspen and goat willow" = "NO_ROSE_001",
    "Multi-layered forest" = "NO_MLFA_001",
    "Large trees" = "NO_DLTA_001",
    "Old forest" = "NO_BGSK_001",
    "Moose" = "NO_MOOS_001",
    "Red deer" = "NO_DEER_001",
    "Roe deer" = "NO_ROED_001",
    "Bilberry cover" = "NO_BLAA_001",
    "Connected low-infrastructure forest habitat" = "NO_CONN_001",
    "Glaciers" = "NO_GLAC_001",
    "Glacier area" = "NO_GLAC_001",
    "Snow depth" = "NO_SNOW_001",
    "Snow cover" = "NO_SNOW_001",
    "Alien plant species" = "NO_ALIE_002",
    "Lichen biomass" = "NO_LAVH_001",
    "Arctic fox" = "NO_AFOX_001",
    "Ptarmigan" = "NO_PTAR_001",
    "Small rodents" = "NO_RODE_001",
    "Wolverine" = "NO_JERV_001",
    "Golden eagle" = "NO_GOLD_001",
    "Connectivity" = "NO_CONN_001",
    "Brown bear" = "NO_BAER_001",
    "Lynx" = "NO_LYNX_001",
    "Wolf" = "NO_WOLF_001"
  )
}

# =============================================================================
# 3. Reading and harmonising results
# =============================================================================

# Read CSV, RDS, or parquet results from path/URL.
read_indicator_file <- function(path, n_max = Inf) {
  ext <- tolower(tools::file_ext(path))

  if (ext == "csv") {
    return(readr::read_csv(path, n_max = n_max, show_col_types = FALSE))
  }

  if (ext == "rds") {
    dat <- if (is_results_url(path)) {
      con <- url(path, open = "rb")
      on.exit(close(con), add = TRUE)
      readRDS(con)
    } else {
      readRDS(path)
    }
    if (n_max < Inf && is.data.frame(dat)) {
      dat <- utils::head(dat, n_max)
    }
    return(dat)
  }

  if (ext %in% c("parquet", "pq")) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Install the arrow package to read parquet indicator results.", call. = FALSE)
    }
    dat <- arrow::read_parquet(path)
    if (n_max < Inf) {
      dat <- utils::head(dat, n_max)
    }
    return(dat)
  }

  stop("Unsupported results file type: ", ext, " (", path, ")", call. = FALSE)
}

# End-year fallback for single-period exports that omit year/period columns.
indicator_default_year <- function(indicator_id) {
  defaults <- c(
    # ANO cycle 2020-2024; published parquet has region + sim only.
    "NO_SLIT_001" = 2024L,
    "NO_SLIT_002" = 2024L
  )
  if (is.na(indicator_id) || !indicator_id %in% names(defaults)) {
    return(NA_integer_)
  }
  unname(defaults[[indicator_id]])
}

# Standardise columns to year, part, value.
harmonise_indicator_draws <- function(
    dat,
    indicator_id = NA_character_,
    lookup = load_region_lookup(),
    default_year = NULL) {
  if (!is.data.frame(dat)) {
    stop("Indicator results must be a data frame.", call. = FALSE)
  }

  value_col <- intersect(
    names(dat),
    c("sampled_value", "sampled_mean", "indicator", "value", "sim")
  )[1]
  if (is.na(value_col)) {
    value_col <- intersect(
      tolower(names(dat)),
      c("sampled_value", "sampled_mean", "indicator", "value", "sim")
    )[1]
    if (!is.na(value_col)) {
      value_col <- names(dat)[match(value_col, tolower(names(dat)))]
    }
  }
  if (is.na(value_col)) {
    stop(
      "Results must contain sampled_value, sampled_mean, indicator, value, or sim column.",
      call. = FALSE
    )
  }

  name_map <- stats::setNames(names(dat), tolower(names(dat)))
  part_key <- intersect(
    c("part", "reg", "region", "region_code", "nation"),
    names(name_map)
  )[1]
  if (is.na(part_key)) {
    stop(
      "Results must contain part, reg, region, region_code, or nation column.",
      call. = FALSE
    )
  }
  part_col <- name_map[[part_key]]

  time_col <- if ("year" %in% names(dat)) {
    "year"
  } else if ("period" %in% names(dat)) {
    "period"
  } else {
    NA_character_
  }

  fallback_year <- if (!is.null(default_year) && !is.na(default_year)) {
    as.integer(default_year)
  } else {
    indicator_default_year(indicator_id)
  }

  if (is.na(time_col) && is.na(fallback_year)) {
    stop("Results must contain year or period column.", call. = FALSE)
  }

  dat |>
    dplyr::mutate(
      indicator_id = indicator_id,
      period = if (!is.na(time_col) && time_col == "period") {
        as.character(.data[[time_col]])
      } else {
        NA_character_
      },
      year = if (!is.na(time_col) && time_col == "year") {
        as.integer(.data[[time_col]])
      } else if (!is.na(time_col) && time_col == "period") {
        period_to_year(.data[[time_col]])
      } else {
        fallback_year
      },
      part_raw = as.character(.data[[part_col]]),
      part = standardize_part(.data[[part_col]], lookup = lookup),
      value = as.numeric(.data[[value_col]])
    ) |>
    dplyr::select(
      "indicator_id",
      "year",
      "period",
      "part_raw",
      "part",
      "value"
    ) |>
    dplyr::filter(!is.na(.data$part), !is.na(.data$value), !is.na(.data$year))
}

# Read and harmonise results (incl. companion national RDS).
read_indicator_results <- function(
    path,
    indicator_id = NA_character_,
    lookup = load_region_lookup(),
    n_max = Inf,
    default_year = NULL) {
  dat <- read_indicator_file(path, n_max = n_max)
  out <- harmonise_indicator_draws(
    dat,
    indicator_id = indicator_id,
    lookup = lookup,
    default_year = default_year
  )

  if (grepl("_mc_regional\\.rds$", path, ignore.case = TRUE)) {
    nat_path <- sub("_mc_regional\\.rds$", "_mc_national.rds", path, ignore.case = TRUE)
    nat_ok <- if (nat_path != path) {
      if (is_results_url(nat_path)) {
        probe_results_source(nat_path, indicator_id = indicator_id)
      } else {
        file.exists(nat_path)
      }
    } else {
      FALSE
    }
    if (nat_ok) {
      nat_dat <- read_indicator_file(nat_path, n_max = n_max)
      out <- dplyr::bind_rows(
        out,
        harmonise_indicator_draws(nat_dat, indicator_id = indicator_id, lookup = lookup)
      )
    }
  }

  out
}

# Resolve source then load harmonised draws.
fetch_indicator_results <- function(
    indicator_id,
    folder = indicator_id,
    version = "1.0.0",
    repo = "NINAnor/ecRxiv",
    ref = "main",
    lookup = load_region_lookup(),
    include_submission = TRUE,
    local_dir = here::here("data")) {
  src <- resolve_indicator_source(
    indicator_id,
    folder,
    version,
    repo,
    include_submission = include_submission,
    local_dir = local_dir
  )
  if (is.null(src)) {
    stop("No results found for ", indicator_id, call. = FALSE)
  }
  read_indicator_results(src$results_url, indicator_id = indicator_id, lookup = lookup)
}

# Pull indicator ID from a file path or branch name.
extract_indicator_id_from_path <- function(path, ref_name = NA_character_) {
  id <- stringr::str_match(path, "results_(NO_[A-Z0-9]+_[0-9]{3})")[, 2]
  if (!is.na(id)) {
    return(id)
  }

  id <- stringr::str_match(path, "(?:^|/)(NO_[A-Z0-9]+_[0-9]{3})/")[, 2]
  if (!is.na(id)) {
    return(id)
  }

  if (!is.na(ref_name) && ref_name != "") {
    id <- stringr::str_match(ref_name, "submission[-_](NO_[A-Z0-9]+_[0-9]{3})")[, 2]
    if (!is.na(id)) {
      return(id)
    }
  }

  NA_character_
}

# =============================================================================
# 4. Finding and resolving result sources
# =============================================================================

# Build default raw GitHub URL for a results CSV.
fetch_indicator_results_url <- function(
    indicator_id,
    folder = indicator_id,
    version = "1.0.0",
    repo = "NINAnor/ecRxiv",
    ref = "main") {
  file <- glue::glue("indicators/{folder}/data/results_{indicator_id}_v{version}.csv")
  glue::glue("https://raw.githubusercontent.com/{repo}/{ref}/{file}")
}

# Build raw.githubusercontent.com URL for repo/ref/path.
build_raw_url <- function(repo, ref, results_path) {
  glue::glue("https://raw.githubusercontent.com/{repo}/{ref}/{results_path}")
}

# Load scanned (+ optional extra) source manifest.
load_indicator_sources_manifest <- function(
    path = here::here("data/indicator_sources.csv"),
    extra_path = here::here("data/indicator_sources_extra.csv")) {
  empty <- tibble::tibble(
    indicator_id = character(),
    ref = character(),
    results_path = character(),
    priority = integer(),
    repo = character()
  )

  parts <- list()
  if (file.exists(path)) {
    parts <- c(parts, list(readr::read_csv(path, show_col_types = FALSE)))
  }
  if (file.exists(extra_path)) {
    extra <- readr::read_csv(extra_path, show_col_types = FALSE)
    if (nrow(extra) > 0) {
      if ("priority" %in% names(extra)) {
        extra$priority <- as.integer(extra$priority)
      }
      parts <- c(parts, list(extra))
    }
  }

  if (length(parts) == 0) {
    return(empty)
  }

  dplyr::bind_rows(parts) |>
    dplyr::distinct(.data$indicator_id, .data$ref, .data$results_path, .keep_all = TRUE) |>
    dplyr::arrange(.data$indicator_id, .data$priority, .data$ref)
}

# Likely submission-* branch names for an indicator.
submission_branch_candidates <- function(indicator_id, folder = indicator_id) {
  unique(c(
    glue::glue("submission-{indicator_id}"),
    glue::glue("submission-{folder}"),
    glue::glue("submission_{indicator_id}"),
    glue::glue("submission_{folder}")
  ))
}

# Short slug for MC RDS filenames (e.g. dtvs).
indicator_mc_slug <- function(indicator_id) {
  core <- sub("^NO_", "", indicator_id)
  core <- sub("_0[0-9]{2}.*$", "", core)
  tolower(core)
}

# Prefer slug-matching MC RDS when several exist.
filter_indicator_source_candidates <- function(candidates, indicator_id) {
  if (nrow(candidates) == 0) {
    return(candidates)
  }

  preferred_rds <- glue::glue("{indicator_mc_slug(indicator_id)}_mc_regional.rds")
  candidates <- candidates |>
    dplyr::mutate(
      is_mc_rds = grepl("_mc_regional\\.rds$", .data$results_path, ignore.case = TRUE),
      slug_match = basename(.data$results_path) == preferred_rds
    )

  if (any(candidates$is_mc_rds & candidates$slug_match, na.rm = TRUE)) {
    candidates <- candidates |>
      dplyr::filter(!.data$is_mc_rds | .data$slug_match)
  }

  candidates |>
    dplyr::arrange(.data$priority, .data$ref, .data$results_path) |>
    dplyr::select(-"is_mc_rds", -"slug_match")
}

# Possible results file paths inside a repo.
results_path_candidates <- function(
    indicator_id,
    folder = indicator_id,
    version = "1.0.0") {
  slug <- indicator_mc_slug(indicator_id)
  fname_v <- glue::glue("results_{indicator_id}_v{version}.csv")
  fname <- glue::glue("results_{indicator_id}.csv")
  fname_parquet <- glue::glue("results_{indicator_id}.parquet")
  fname_parquet_sim <- glue::glue("results_{indicator_id}_sim.parquet")
  fname_bare_sim <- glue::glue("{indicator_id}_sim.parquet")
  mc_regional <- glue::glue("{slug}_mc_regional.rds")

  paths <- c(
    glue::glue("indicators/{folder}/data/{fname_v}"),
    glue::glue("indicators/{folder}/data/{fname}"),
    glue::glue("indicators/{indicator_id}/data/{fname_v}"),
    glue::glue("indicators/{indicator_id}/data/{fname}"),
    glue::glue("indicators/{folder}/data/{fname_parquet}"),
    glue::glue("indicators/{folder}/data/{fname_parquet_sim}"),
    glue::glue("indicators/{folder}/data/{fname_bare_sim}"),
    glue::glue("indicators/{indicator_id}/data/{fname_bare_sim}"),
    glue::glue("indicators/{folder}/data/{mc_regional}"),
    glue::glue("indicators/{indicator_id}/data/{mc_regional}"),
    glue::glue("{folder}/data/{fname_v}"),
    glue::glue("R/{indicator_id}/data/{fname_v}"),
    glue::glue("R/{folder}/data/{fname_v}"),
    glue::glue("data/{fname_v}"),
    glue::glue("data/{fname}"),
    glue::glue("data/{mc_regional}"),
    glue::glue("{indicator_id}/data/{fname_v}")
  )

  # Upstream NO_SLIT_001 currently exports under a *_002_* filename.
  if (identical(indicator_id, "NO_SLIT_001")) {
    paths <- c(
      paths,
      glue::glue("indicators/{folder}/data/NO_SLIT_002_sim.parquet"),
      "indicators/NO_SLIT_001/data/NO_SLIT_002_sim.parquet"
    )
  }

  unique(paths)
}

# Default main/submission candidate table for probing.
default_source_candidates <- function(
    indicator_id,
    folder = indicator_id,
    version = "1.0.0",
    repo = "NINAnor/ecRxiv",
    include_submission = TRUE) {
  paths <- results_path_candidates(indicator_id, folder, version)
  rows <- tibble::tibble(
    indicator_id = indicator_id,
    ref = "main",
    results_path = paths,
    priority = dplyr::if_else(
      grepl("_mc_regional\\.rds$", paths) | grepl("\\.parquet$", paths),
      2L,
      1L
    ),
    repo = repo
  )

  if (include_submission) {
    sub_rows <- tidyr::crossing(
      ref = submission_branch_candidates(indicator_id, folder),
      results_path = paths
    ) |>
      dplyr::mutate(
        indicator_id = indicator_id,
        priority = dplyr::if_else(
          grepl("_mc_regional\\.rds$", .data$results_path) |
            grepl("\\.parquet$", .data$results_path),
          4L,
          3L
        ),
        repo = repo
      )
    rows <- dplyr::bind_rows(rows, sub_rows)
  }

  rows
}

# Check whether a results path/URL can be read.
probe_results_source <- function(path_or_url, indicator_id = NA_character_) {
  # Candidate URLs often 404; readr/arrow emit warnings rather than (only) errors.
  suppressWarnings(
    tryCatch({
      read_indicator_results(
        path_or_url,
        indicator_id = indicator_id,
        n_max = 1
      )
      TRUE
    }, error = function(e) {
      FALSE
    })
  )
}

# Search local data/ for a results file.
find_local_results_path <- function(
    indicator_id,
    local_dir = here::here("data")) {
  patterns <- c(
    glue::glue("results_{indicator_id}_v*.csv"),
    glue::glue("results_{indicator_id}.csv"),
    glue::glue("results_{indicator_id}*.parquet"),
    glue::glue("*_{indicator_mc_slug(indicator_id)}_mc_regional.rds")
  )
  hits <- unlist(lapply(patterns, function(pat) {
    list.files(local_dir, pattern = glob2rx(pat), full.names = TRUE, recursive = TRUE)
  }))
  hits <- unique(hits[file.exists(hits)])
  if (length(hits) == 0) {
    return(NA_character_)
  }
  hits[[1]]
}

# Resolve the best available results source for an ID.
resolve_indicator_source <- function(
    indicator_id,
    folder = indicator_id,
    version = "1.0.0",
    repo = "NINAnor/ecRxiv",
    manifest = load_indicator_sources_manifest(),
    include_submission = TRUE,
    local_dir = here::here("data")) {
  if (is.na(indicator_id) || indicator_id == "") {
    return(NULL)
  }

  manifest_candidates <- manifest |>
    dplyr::filter(.data$indicator_id == !!indicator_id) |>
    dplyr::arrange(.data$priority, .data$ref)

  default_candidates <- default_source_candidates(
    indicator_id,
    folder,
    version,
    repo,
    include_submission = include_submission
  )

  # Prefer manifest hits, but always fall back to default path guesses if needed.
  candidates <- dplyr::bind_rows(manifest_candidates, default_candidates) |>
    dplyr::distinct(.data$repo, .data$ref, .data$results_path, .keep_all = TRUE)

  candidates <- filter_indicator_source_candidates(candidates, indicator_id)

  for (i in seq_len(nrow(candidates))) {
    url <- build_raw_url(
      candidates$repo[i],
      candidates$ref[i],
      candidates$results_path[i]
    )
    if (probe_results_source(url, indicator_id = indicator_id)) {
      return(list(
        indicator_id = indicator_id,
        repo = candidates$repo[i],
        ref = candidates$ref[i],
        results_path = candidates$results_path[i],
        results_url = url,
        source = if (candidates$ref[i] == "main") "main" else "branch"
      ))
    }
  }

  local_path <- find_local_results_path(indicator_id, local_dir = local_dir)
  if (!is.na(local_path) && probe_results_source(local_path, indicator_id = indicator_id)) {
    return(list(
      indicator_id = indicator_id,
      repo = NA_character_,
      ref = "local",
      results_path = local_path,
      results_url = local_path,
      source = "local"
    ))
  }

  NULL
}

# Scan upstream git refs and write the source manifest.
scan_git_indicator_sources <- function(
    git_remote = "upstream",
    repo = "NINAnor/ecRxiv",
    write_path = here::here("data/indicator_sources.csv")) {
  # Always run git from the project root (Quarto often knits with cwd = R/).
  git_root <- tryCatch(here::here(), error = function(e) getwd())

  git_run <- function(args, stdout = TRUE) {
    suppressWarnings(system2(
      "git",
      c("-C", git_root, args),
      stdout = stdout,
      stderr = if (isFALSE(stdout)) FALSE else FALSE
    ))
  }

  branches <- tryCatch(
    git_run(c("branch", "-r", "--list", paste0(git_remote, "/*"))),
    error = function(e) character()
  )
  branches <- trimws(branches)
  branches <- branches[grepl(paste0("^", git_remote, "/"), branches)]

  submission_refs <- sub(
    paste0("^", git_remote, "/"),
    "",
    branches[grepl("submission", branches, ignore.case = TRUE)]
  )
  submission_refs <- submission_refs[!submission_refs %in% c("submission", "")]

  refs <- unique(c("main", submission_refs))

  git_branch_exists <- function(branch) {
    status <- git_run(c("rev-parse", "--verify", branch), stdout = FALSE)
    identical(status, 0L)
  }

  git_ls_files <- function(branch) {
    if (!git_branch_exists(branch)) {
      return(character())
    }
    out <- git_run(c("ls-tree", "-r", "--name-only", branch))
    status <- attr(out, "status")
    if (!is.null(status) && status != 0) {
      return(character())
    }
    out
  }

  rows <- lapply(refs, function(ref_name) {
    branch <- if (ref_name == "main") {
      paste0(git_remote, "/main")
    } else {
      paste0(git_remote, "/", ref_name)
    }
    files <- git_ls_files(branch)

    csv_files <- files[
      grepl("results_NO_", files, fixed = TRUE) & grepl("\\.csv$", files)
    ]
    pq_files <- files[
      grepl("results_NO_.*\\.parquet$", files) |
        grepl("(^|/)NO_[A-Z0-9]+_[0-9]{3}_sim\\.parquet$", files)
    ]
    rds_files <- files[grepl("_mc_regional\\.rds$", files)]

    matched <- c(csv_files, pq_files, rds_files)
    if (length(matched) == 0) {
      return(NULL)
    }

    tibble::tibble(
      indicator_id = vapply(
        matched,
        extract_indicator_id_from_path,
        character(1),
        ref_name = ref_name
      ),
      ref = ref_name,
      results_path = matched,
      priority = dplyr::case_when(
        ref_name == "main" & grepl("\\.csv$", matched) ~ 1L,
        ref_name == "main" ~ 2L,
        grepl("\\.csv$", matched) ~ 3L,
        TRUE ~ 4L
      ),
      repo = repo
    ) |>
      dplyr::filter(!is.na(.data$indicator_id))
  })

  out <- dplyr::bind_rows(rows) |>
    dplyr::distinct()

  if (nrow(out) > 0 && "indicator_id" %in% names(out)) {
    out <- dplyr::arrange(out, .data$indicator_id, .data$priority, .data$ref)
  }

  # Avoid clobbering a good manifest with a failed/partial scan.
  if (!is.null(write_path) && file.exists(write_path) && nrow(out) > 0) {
    existing_n <- tryCatch(
      nrow(readr::read_csv(write_path, show_col_types = FALSE)),
      error = function(e) 0L
    )
    if (existing_n > nrow(out) * 2L) {
      warning(
        "scan_git_indicator_sources() found only ", nrow(out),
        " rows but existing manifest has ", existing_n,
        "; keeping the existing file.",
        call. = FALSE
      )
      out <- readr::read_csv(write_path, show_col_types = FALSE)
    } else {
      readr::write_csv(out, write_path)
    }
  } else if (!is.null(write_path) && nrow(out) > 0) {
    readr::write_csv(out, write_path)
  }

  extra_path <- here::here("data/indicator_sources_extra.csv")
  if (file.exists(extra_path)) {
    extra <- readr::read_csv(extra_path, show_col_types = FALSE)
    if (nrow(extra) > 0) {
      if ("priority" %in% names(extra)) {
        extra$priority <- as.integer(extra$priority)
      }
      out <- dplyr::bind_rows(out, extra) |>
        dplyr::distinct(.data$indicator_id, .data$ref, .data$results_path, .keep_all = TRUE) |>
        dplyr::arrange(.data$indicator_id, .data$priority, .data$ref)
    }
  }

  out
}

# TRUE if any results source resolves for an ID.
results_available_remote <- function(
    indicator_id,
    folder = indicator_id,
    version = "1.0.0",
    repo = "NINAnor/ecRxiv",
    ref = "main",
    include_submission = TRUE,
    local_dir = here::here("data")) {
  !is.null(resolve_indicator_source(
    indicator_id,
    folder,
    version,
    repo,
    include_submission = include_submission,
    local_dir = local_dir
  ))
}

# =============================================================================
# 5. Building the indicator registry
# =============================================================================

# Build forest/mountain registry and match status.
build_indicator_registry <- function(
    csv_path,
    ecosystem = c("forest", "mountain"),
    write_path = NULL,
    check_remote = TRUE) {
  ecosystem <- rlang::arg_match(ecosystem)
  registry_in <- readr::read_csv(csv_path, show_col_types = FALSE)
  overrides <- name_to_id_overrides()
  catalog <- indicator_catalog()

  registry <- registry_in |>
    dplyr::mutate(
      ecosystem = dplyr::coalesce(.data$ecosystem, ecosystem),
      ect = parse_ect(.data$ECT),
      indicatorID = dplyr::if_else(
        !is.na(.data$indicatorID) & .data$indicatorID != "",
        .data$indicatorID,
        overrides[.data$indicatorName]
      )
    )

  registry <- registry |>
    dplyr::left_join(
      catalog |>
        dplyr::select(indicator_id, folder) |>
        # Prefer the concrete published folder when several aliases exist.
        dplyr::arrange(.data$indicator_id, .data$folder) |>
        dplyr::distinct(.data$indicator_id, .keep_all = TRUE),
      by = c("indicatorID" = "indicator_id")
    ) |>
    dplyr::mutate(
      folder = dplyr::coalesce(.data$folder, .data$indicatorID),
      results_path = glue::glue("indicators/{folder}/data/results_{indicatorID}_v1.0.0.csv"),
      weight = 1,
      national_weight_scheme = "equal",
      match_status = dplyr::if_else(
        is.na(.data$indicatorID) | .data$indicatorID == "",
        "missing",
        "matched"
      )
    )

  if (check_remote) {
    registry <- registry |>
      dplyr::rowwise() |>
      dplyr::mutate(
        source_info = list(
          if (is.na(.data$indicatorID) || .data$indicatorID == "") {
            NULL
          } else {
            suppressWarnings(
              resolve_indicator_source(.data$indicatorID, .data$folder)
            )
          }
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        has_results = !vapply(.data$source_info, is.null, logical(1)),
        results_ref = vapply(.data$source_info, function(x) {
          if (is.null(x)) NA_character_ else x$ref
        }, character(1)),
        results_path = vapply(.data$source_info, function(x) {
          if (is.null(x)) NA_character_ else x$results_path
        }, character(1)),
        results_url = vapply(.data$source_info, function(x) {
          if (is.null(x)) NA_character_ else x$results_url
        }, character(1)),
        results_source = vapply(.data$source_info, function(x) {
          if (is.null(x)) NA_character_ else x$source
        }, character(1)),
        match_status = dplyr::case_when(
          .data$match_status == "missing" ~ "missing",
          .data$has_results ~ "matched",
          TRUE ~ "no_results"
        )
      ) |>
      dplyr::select(-source_info)
  }

  if (!is.null(write_path)) {
    readr::write_csv(registry, write_path)
  }

  registry
}

# =============================================================================
# 6. Loading draws for the index
# =============================================================================

# Load MC draws for all matched registry indicators.
#
# Years are filtered to a fixed national reporting schedule (`report_years`,
# e.g. Norway's 5-year assessment cycle, defined once in the qmd) rather
# than each indicator's own last N years, so every retained year draws from
# the same set of report years across indicators. Indicators that publish
# extra years outside the retained schedule (e.g. older cycles) simply have
# those rows dropped here; gaps at a retained report year are handled
# afterwards by `impute_missing_indicator_years()` in `calculate_index()`.
load_registry_draws <- function(
    registry,
    n_years = 2,
    report_years,
    repo = "NINAnor/ecRxiv",
    ref = "main",
    local_root = NULL,
    local_dir = here::here("data"),
    include_submission = TRUE) {
  use <- registry |>
    dplyr::filter(.data$match_status == "matched")

  if (nrow(use) == 0) {
    stop("No matched indicators with results in registry.", call. = FALSE)
  }

  target_years <- utils::tail(sort(report_years), n_years)

  purrr::map_dfr(seq_len(nrow(use)), function(i) {
    row <- use[i, ]
    path <- if (!is.null(local_root)) {
      file.path(
        local_root,
        row$folder,
        "data",
        glue::glue("results_{row$indicatorID}_v1.0.0.csv")
      )
    } else if ("results_url" %in% names(row) && !is.na(row$results_url) && row$results_url != "") {
      row$results_url
    } else {
      src <- resolve_indicator_source(
        row$indicatorID,
        row$folder,
        repo = repo,
        include_submission = include_submission,
        local_dir = local_dir
      )
      if (is.null(src)) {
        stop("No results for ", row$indicatorID, call. = FALSE)
      }
      src$results_url
    }

    read_indicator_results(path, indicator_id = row$indicatorID) |>
      dplyr::mutate(
        ect = row$ect,
        ecosystem = row$ecosystem,
        indicator_name = row$indicatorName,
        results_ref = if ("results_ref" %in% names(row)) row$results_ref else NA_character_,
        results_source = if ("results_source" %in% names(row)) row$results_source else NA_character_
      )
  }) |>
    dplyr::filter(.data$year %in% target_years)
}

# Ensure every matched indicator/part has a value at every retained report
# year.
#
# `load_registry_draws()` filters to the fixed national report years
# (`target_years`), but some indicators only publish one of them (e.g. a
# first submission with only 2024 data, nothing for 2019). Rather than
# silently dropping such indicators from the years they don't cover, we
# carry the nearest other retained year's draws forward/backward, so the
# same indicator set contributes to every reported year. Bounded by
# `max_gap`: if no other retained year is within `max_gap` of a missing one,
# the indicator/part stays excluded for that year rather than being
# force-filled with a stale value.
impute_missing_indicator_years <- function(draws, target_years, max_gap = Inf) {
  draws <- draws |>
    dplyr::mutate(imputed = FALSE, source_year = .data$year)

  have <- draws |>
    dplyr::distinct(.data$indicator_id, .data$part, .data$year)

  gaps <- tidyr::expand_grid(
    have |> dplyr::distinct(.data$indicator_id, .data$part),
    year = target_years
  ) |>
    dplyr::anti_join(have, by = c("indicator_id", "part", "year"))

  if (nrow(gaps) == 0) {
    return(draws)
  }

  imputed <- purrr::map_dfr(seq_len(nrow(gaps)), function(i) {
    ind <- gaps$indicator_id[i]
    p <- gaps$part[i]
    missing_year <- gaps$year[i]

    available_years <- have |>
      dplyr::filter(.data$indicator_id == ind, .data$part == p) |>
      dplyr::pull(.data$year) |>
      intersect(target_years)

    if (length(available_years) == 0) {
      return(NULL)
    }

    gap <- abs(available_years - missing_year)
    if (min(gap) > max_gap) {
      return(NULL)
    }
    source_year <- available_years[which.min(gap)]

    draws |>
      dplyr::filter(
        .data$indicator_id == ind,
        .data$part == p,
        .data$year == source_year
      ) |>
      dplyr::mutate(
        year = missing_year,
        imputed = TRUE,
        source_year = source_year
      )
  })

  dplyr::bind_rows(draws, imputed)
}

# =============================================================================
# 7. Aggregation with ecTools::ec_upscale
# =============================================================================

# Unnest list-column sampled_value from ec_upscale output.
# Older ecTools versions named this column sampled_mean; rename that if present.
unnest_upscale <- function(x) {
  if ("sampled_mean" %in% names(x) && !"sampled_value" %in% names(x)) {
    x <- dplyr::rename(x, sampled_value = "sampled_mean")
  }
  if (vctrs::vec_size(x$sampled_value[[1]]) == 1L) {
    return(x)
  }
  tidyr::unnest_longer(x, sampled_value)
}

# Resample indicators to ECT classes (equal weights).
aggregate_to_ect <- function(draws, n = 1000) {
  parts <- unique(draws$part)
  purrr::map_dfr(parts, function(p) {
    df <- draws |>
      dplyr::filter(.data$part == p) |>
      dplyr::mutate(wgt = 1)

    out <- ecTools::ec_upscale(
      data = df,
      variable = value,
      weight = wgt,
      start_units = indicator_id,
      end_units = ect,
      year = year,
      end_units_name = "ect",
      n = n
    )

    unnest_upscale(out) |>
      dplyr::mutate(part = p)
  })
}

# Resample ECT classes to overall index (equal weights).
aggregate_to_index <- function(ect_draws, n = 1000) {
  parts <- unique(ect_draws$part)
  purrr::map_dfr(parts, function(p) {
    df <- ect_draws |>
      dplyr::filter(.data$part == p) |>
      dplyr::mutate(wgt = 1, index_level = "Index")

    out <- ecTools::ec_upscale(
      data = df,
      variable = sampled_value,
      weight = wgt,
      start_units = ect,
      end_units = index_level,
      year = year,
      end_units_name = "id",
      n = n
    )

    unnest_upscale(out) |>
      dplyr::mutate(part = p)
  })
}

# Resample indicators directly to the overall index, skipping the ECT step.
#
# This is a separate, alternative aggregation: every matched indicator gets
# equal weight directly in the national/regional index, rather than first
# being equal-weighted within its ECT class and then having ECT classes
# equal-weighted into the index (`aggregate_to_ect()` + `aggregate_to_index()`
# above). An ECT class with many indicators therefore has more influence
# here than it does in the two-step workflow. Kept separate (own `level`,
# `"Index (direct)"`) so it never mixes into the `"ECT"`/`"Index"` levels.
aggregate_indicators_to_index_direct <- function(draws, n = 1000) {
  parts <- unique(draws$part)
  purrr::map_dfr(parts, function(p) {
    df <- draws |>
      dplyr::filter(.data$part == p) |>
      dplyr::mutate(wgt = 1, index_level = "Index (direct)")

    out <- ecTools::ec_upscale(
      data = df,
      variable = value,
      weight = wgt,
      start_units = indicator_id,
      end_units = index_level,
      year = year,
      end_units_name = "id",
      n = n
    )

    unnest_upscale(out) |>
      dplyr::mutate(part = p, level = "Index (direct)")
  })
}

# Ensure every indicator/year has a national ("Norway") value.
#
# Most ecRxiv indicators already publish their own national draws alongside
# the five NI regions. Those published values may embed weighting that an
# equal-weight regional average would not reproduce - e.g. NO_GLAC_001's own
# national value down-weights southern Norway relative to the other regions
# (see ECA_2026 #76: "For NO_GLAC blir sor-Norge veid ned, slik det er gjort
# i den nasjonale verdien for denne indikatoren"). We therefore always prefer
# an indicator's own published Norway draws. Only indicators that do not
# publish a national value get an equal-weight fallback computed here from
# their regional draws.
fill_missing_national_draws <- function(draws, n = 1000) {
  needs_national <- draws |>
    dplyr::group_by(.data$indicator_id, .data$year) |>
    dplyr::summarise(has_norway = "Norway" %in% .data$part, .groups = "drop") |>
    dplyr::filter(!.data$has_norway)

  if (nrow(needs_national) == 0) {
    return(draws)
  }

  synthesized <- purrr::map_dfr(seq_len(nrow(needs_national)), function(i) {
    ind <- needs_national$indicator_id[i]
    yr <- needs_national$year[i]
    df <- draws |>
      dplyr::filter(
        .data$indicator_id == ind,
        .data$year == yr,
        .data$part %in% setdiff(CANONICAL_PARTS, "Norway")
      ) |>
      dplyr::mutate(wgt = 1, national = "Norway")

    if (nrow(df) == 0) {
      return(NULL)
    }

    out <- ecTools::ec_upscale(
      data = df,
      variable = value,
      weight = wgt,
      start_units = part,
      end_units = national,
      year = year,
      end_units_name = "part",
      n = n
    )

    unnest_upscale(out) |>
      dplyr::transmute(
        indicator_id = ind,
        year = .data$year,
        period = df$period[1],
        part_raw = "Norway (equal-weight fallback)",
        part = "Norway",
        value = .data$sampled_value,
        ect = df$ect[1],
        ecosystem = df$ecosystem[1],
        indicator_name = df$indicator_name[1],
        results_ref = df$results_ref[1],
        results_source = df$results_source[1]
      )
  })

  dplyr::bind_rows(draws, synthesized)
}

# =============================================================================
# 8. Index calculation, summaries, plots, and export
# =============================================================================

# Count contributing indicators by year and region, incl. imputed values.
indicator_coverage <- function(draws, registry) {
  imputed_flag <- if ("imputed" %in% names(draws)) {
    dplyr::coalesce(draws$imputed, FALSE)
  } else {
    FALSE
  }
  draws |>
    dplyr::mutate(.imputed = imputed_flag) |>
    dplyr::group_by(.data$year, .data$part) |>
    dplyr::summarise(
      n_indicators = dplyr::n_distinct(.data$indicator_id),
      indicators = paste(sort(unique(.data$indicator_name)), collapse = "; "),
      n_registry_matched = sum(registry$match_status == "matched", na.rm = TRUE),
      n_indicators_imputed = dplyr::n_distinct(.data$indicator_id[.data$.imputed]),
      n_indicators_observed = dplyr::n_distinct(.data$indicator_id[!.data$.imputed]),
      .groups = "drop"
    )
}

# Full index pipeline: load, aggregate, summarise.
calculate_index <- function(
    registry,
    n_sim = 1000,
    n_years = 2,
    report_years,
    max_gap = Inf,
    include_national = TRUE,
    include_direct_index = TRUE,
    ...) {
  target_years <- utils::tail(sort(report_years), n_years)

  draws <- load_registry_draws(
    registry,
    n_years = n_years,
    report_years = report_years,
    ...
  )

  # Fixed report years mean an indicator may still be missing one of the
  # retained years (e.g. a first submission with only 2024 data); carry the
  # nearest other retained year's draws forward/backward so the same
  # indicator set contributes to every reported year (see
  # impute_missing_indicator_years()).
  draws <- impute_missing_indicator_years(draws, target_years, max_gap = max_gap)

  # Each indicator's own published Norway draws are used as-is (so any
  # indicator-specific weighting, e.g. NO_GLAC_001's down-weighted southern
  # Norway, is preserved). Indicators without a national value get an
  # equal-weight regional fallback. "Norway" is then just another part fed
  # through the same indicator -> ECT -> Index resampling as the regions.
  draws <- if (include_national) {
    fill_missing_national_draws(draws, n = n_sim)
  } else {
    draws |> dplyr::filter(.data$part != "Norway")
  }

  ect_draws <- aggregate_to_ect(draws, n = n_sim) |>
    dplyr::mutate(level = "ECT")

  index_draws <- aggregate_to_index(ect_draws, n = n_sim) |>
    dplyr::mutate(level = "Index")

  # Alternative, separate aggregation: indicators straight to the index,
  # skipping the ECT step (see aggregate_indicators_to_index_direct()).
  # Added as its own `level` ("Index (direct)") so it flows through the
  # same summaries/export as ECT/Index without altering either.
  direct_draws <- if (include_direct_index) {
    aggregate_indicators_to_index_direct(draws, n = n_sim)
  } else {
    NULL
  }

  direct_draws <- if (!is.null(direct_draws)) {
    direct_draws |> dplyr::select(dplyr::any_of(c("year", "part", "id", "level", "sampled_value")))
  } else {
    NULL
  }

  results <- dplyr::bind_rows(
    ect_draws |> dplyr::rename(id = ect),
    index_draws |> dplyr::select(dplyr::any_of(c("year", "part", "id", "level", "sampled_value"))),
    direct_draws
  )

  list(
    indicator_draws = draws,
    distributions = results,
    coverage = indicator_coverage(draws, registry),
    summaries = summarise_index(results)
  )
}

# Median and 95% interval of MC distributions.
summarise_index <- function(distributions) {
  distributions |>
    dplyr::group_by(.data$year, .data$part, .data$level, .data$id) |>
    dplyr::summarise(
      median = stats::median(.data$sampled_value, na.rm = TRUE),
      q025 = stats::quantile(.data$sampled_value, 0.025, na.rm = TRUE),
      q975 = stats::quantile(.data$sampled_value, 0.975, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )
}

# Forest-style plot of ECT and index summaries.
# Uses the same MetBrewer "Archambault" region colours as forest indicator
# time series, with Norway shown as filled circles and regions as triangles.
plot_index_forest <- function(summaries, title = "NO_IDEX_001") {
  part_labels <- c(
    Norway = "Norway",
    C = "Central",
    E = "East",
    N = "North",
    S = "South",
    W = "West"
  )
  region_levels <- c("Norway", "Central", "East", "North", "South", "West")
  region_cols <- if (requireNamespace("MetBrewer", quietly = TRUE)) {
    stats::setNames(
      MetBrewer::met.brewer("Archambault", n = length(region_levels)),
      region_levels
    )
  } else {
    # Fallback Archambault-like colours if MetBrewer is unavailable
    stats::setNames(
      c("#88A0DC", "#381A61", "#7C4B73", "#ED968C", "#AB3329", "#E78429"),
      region_levels
    )
  }

  ect_ids <- summaries |>
    dplyr::filter(.data$level == "ECT") |>
    dplyr::pull(.data$id) |>
    unique() |>
    sort()
  id_levels <- c(ect_ids, "Index")

  # One legend: Norway = filled circle; regions = triangles in their colours.
  region_shapes <- c(
    Norway = 19,
    Central = 17,
    East = 17,
    North = 17,
    South = 17,
    West = 17
  )

  plot_dat <- summaries |>
    dplyr::mutate(
      id = factor(.data$id, levels = id_levels),
      region = dplyr::recode(as.character(.data$part), !!!part_labels),
      region = factor(.data$region, levels = region_levels)
    ) |>
    dplyr::filter(!is.na(.data$region))

  ggplot2::ggplot(
    plot_dat,
    ggplot2::aes(
      x = median,
      y = id,
      xmin = q025,
      xmax = q975,
      colour = region,
      shape = region
    )
  ) +
    ggplot2::geom_vline(xintercept = 0.6, linetype = "dashed", colour = "grey70") +
    ggplot2::geom_pointrange(
      position = ggplot2::position_dodge(width = 0.55),
      size = 0.7,
      linewidth = 0.45
    ) +
    ggplot2::facet_wrap(~year, nrow = 1) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::scale_colour_manual(values = region_cols, name = NULL) +
    ggplot2::scale_shape_manual(values = region_shapes, name = NULL) +
    ggplot2::labs(
      title = title,
      x = "Index value",
      y = NULL
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing.x = ggplot2::unit(0.8, "lines"),
      plot.margin = ggplot2::margin(5, 5, 5, 5)
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        nrow = 1,
        override.aes = list(
          shape = unname(region_shapes),
          size = 0.8,
          linewidth = 0.45
        )
      ),
      shape = "none"
    )
}

# Detailed dot-and-whisker plot for one year: every matched indicator
# individually, then ECT classes, then the overall totals (the ECT-based
# "Index" and the alternative "Index (direct)", see aggregate_indicators_to_
# index_direct()). Horizontal lines separate the three groups; grey dashed
# vertical lines mark 0, 0.6 (assessment "good condition" reference) and 1
# (reference condition). One figure per year (not facetted) - call once per
# report year.
#
# `national_only = TRUE` drops the five regions and shows a single national
# ("Norway") value per row instead, with no region colour/shape legend.
plot_index_detailed <- function(index_result, year, title = "NO_IDEX_001", national_only = FALSE) {
  part_labels <- c(
    Norway = "Norway",
    C = "Central",
    E = "East",
    N = "North",
    S = "South",
    W = "West"
  )
  region_levels <- c("Norway", "Central", "East", "North", "South", "West")
  region_cols <- if (requireNamespace("MetBrewer", quietly = TRUE)) {
    stats::setNames(
      MetBrewer::met.brewer("Archambault", n = length(region_levels)),
      region_levels
    )
  } else {
    stats::setNames(
      c("#88A0DC", "#381A61", "#7C4B73", "#ED968C", "#AB3329", "#E78429"),
      region_levels
    )
  }
  region_shapes <- c(
    Norway = 19, Central = 17, East = 17, North = 17, South = 17, West = 17
  )

  ect_order <- c("A1", "A2", "B1", "B2", "B3", "C1")
  ect_shapes <- c(
      A1 = 15,
      A2 = 16,
      B1 = 17,
      B2 = 18,
      B3 = 3,
      C1 = 4
    )
  
  indicator_summary <- index_result$indicator_draws |>
    dplyr::filter(.data$year == .env$year) |>
    dplyr::group_by(
      .data$part, 
      .data$indicator_id, 
      .data$indicator_name,
      .data$ect) |>
    dplyr::summarise(
      median = stats::median(.data$value, na.rm = TRUE),
      q025 = stats::quantile(.data$value, 0.025, na.rm = TRUE),
      q975 = stats::quantile(.data$value, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::transmute(
      part = .data$part,
      level = "Indicator",
      indicator_id = as.character(.data$indicator_id),
      label = .data$indicator_name,
      ect = as.character(.data$ect),
      row_key = paste0("Indicator::", .data$indicator_id),
      median = .data$median,
      q025 = .data$q025,
      q975 = .data$q975
    )

  agg_summary <- index_result$summaries |>
    dplyr::filter(.data$year == .env$year) |>
    dplyr::select("part", "level", "id", "median", "q025", "q975")
  
  ect_summary <- agg_summary |>
    dplyr::filter(.data$level == "ECT") |>
    dplyr::transmute(
      part = .data$part,
      level = "ECT",
      indicator_id = NA_character_,
      label = as.character(.data$id),
      ect = as.character(.data$id),
      row_key = paste0("ECT::", .data$id),
      median = .data$median,
      q025 = .data$q025,
      q975 = .data$q975
    )

  total_summary <- agg_summary |>
    dplyr::filter(
      .data$id %in% c("Index", "Index (direct)")
    ) |>
    dplyr::transmute(
      part = .data$part,
      level = "Total",
      indicator_id = NA_character_,
      label = as.character(.data$id),
      ect = NA_character_,
      row_key = paste0("Total::", .data$id),
      median = .data$median,
      q025 = .data$q025,
      q975 = .data$q975
    )

  indicator_rows <- indicator_summary |>
  dplyr::distinct(
    .data$row_key,
    .data$indicator_id,
    .data$label,
    .data$ect
  ) |>
  dplyr::mutate(
    ect = factor(.data$ect, levels = ect_order)
  ) |>
  dplyr::arrange(
    .data$ect,
    .data$indicator_id
  )
  indicator_keys <- indicator_rows$row_key

  ect_keys <- paste0(
    "ECT::",
    ect_order[ect_order %in% unique(ect_summary$ect)]
  )

  total_order <- c("Index", "Index (direct)")

  total_keys <- paste0(
    "Total::",
    total_order[
      total_order %in% unique(total_summary$label)
    ])
  top_to_bottom <- c(
    total_keys,
    ect_keys,
    indicator_keys
  )
  # ggplot's discrete y axis runs factor levels BOTTOM -> TOP,
  # hence rev().
  y_levels <- rev(top_to_bottom)

  # Labels shown on the y axis
  label_lookup <- c(
    stats::setNames(total_summary$label, total_summary$row_key),
    stats::setNames(ect_summary$label, ect_summary$row_key),
    stats::setNames(indicator_rows$label, indicator_rows$row_key)
  )

  label_lookup <- label_lookup[!duplicated(names(label_lookup))]

  plot_dat <- dplyr::bind_rows(
    indicator_summary,
    ect_summary,
    total_summary
  ) |>
    dplyr::mutate(
      row_key = factor(
        .data$row_key,
        levels = y_levels
      ),

      region = dplyr::recode(
        as.character(.data$part),
        !!!part_labels
      ),

      region = factor(
        .data$region,
        levels = region_levels
      ),

      # Shape grouping:
      # indicators + ECT summaries get their ECT-class shape;
      # totals receive their own shape.
      shape_group = dplyr::if_else(
        .data$level == "Total",
        "Total",
        .data$ect
      )
    ) |>
    dplyr::filter(!is.na(.data$row_key), !is.na(.data$region))

  if (national_only) {
    plot_dat <- plot_dat |> dplyr::filter(.data$region == "Norway")
  }

  # Boundaries between indicators/ECT and ECT/totals blocks.
  n_ind <- length(indicator_keys)
  n_ect <- length(ect_keys)

  group_breaks <- c(
    n_ind + 0.5,
    n_ind + n_ect + 0.5
  )

  p <- ggplot2::ggplot(
    plot_dat,
    if (national_only) {
      ggplot2::aes(x = median, y = row_key, xmin = q025, xmax = q975, shape = shape_group)
    } else {
      ggplot2::aes(
        x = median,
        y = row_key,
        xmin = q025,
        xmax = q975,
        colour = region,
        shape = shape_group
      )
    }
  ) +
    #ggplot2::geom_vline(
    #  xintercept = c(0, 0.6, 1), linetype = "dashed", colour = "grey70"
    #) +
    ggplot2::geom_hline(
      yintercept = group_breaks, colour = "grey40", linewidth = 0.4
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::scale_y_discrete(
      labels = label_lookup) +
    ggplot2::labs(
      title = paste0(title, " \u2014 ", year, if (national_only) " (Norway)" else ""),
      x = "Index value",
      y = NULL
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(5, 5, 5, 5)
    )

  # Add one extra shape for the total index rows.
  shape_values <- c(
    ect_shapes,
    Total = 19
  )

  if (national_only) {
    p +
      ggplot2::geom_pointrange(
        colour = unname(region_cols["Norway"]),
        size = 0.45,
        linewidth = 0.35
      ) +
      ggplot2::scale_shape_manual(values = shape_values, name = "ECT") +
      ggplot2::theme(legend.position = "bottom")
  } else {
    p +
      ggplot2::geom_pointrange(
        position = ggplot2::position_dodge(width = 0.55),
        size = 0.5,
        linewidth = 0.35
      ) +
      ggplot2::scale_colour_manual(values = region_cols, name = NULL) +
      ggplot2::scale_shape_manual(values = shape_values, name = "ECT") +
      ggplot2::theme(legend.position = "bottom") 
      
  }
}

# Write MC distributions to CSV.
export_index_results <- function(distributions, path) {
  out <- distributions |>
    dplyr::transmute(
      year = .data$year,
      part = .data$part,
      level = .data$level,
      id = .data$id,
      sampled_value = .data$sampled_value
    )
  readr::write_csv(out, path)
}

