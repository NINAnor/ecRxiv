# Functions for NO_INDEX_001 composite index calculation
# Tidyverse orchestration + ecTools::ec_upscale for MC aggregation

CANONICAL_PARTS <- c("C", "E", "S", "W", "N", "Norway")

parse_ect <- function(x) {
  stringr::str_extract(stringr::str_squish(x), "^[ABC][0-9]")
}

load_region_lookup <- function(path = here::here("data/region_lookup.csv")) {
  lookup <- readr::read_csv(path, show_col_types = FALSE)
  stats::setNames(lookup$part, lookup$alias)
}

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

is_results_url <- function(path) {
  grepl("^https?://", path)
}

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

harmonise_indicator_draws <- function(
    dat,
    indicator_id = NA_character_,
    lookup = load_region_lookup()) {
  if (!is.data.frame(dat)) {
    stop("Indicator results must be a data frame.", call. = FALSE)
  }

  value_col <- intersect(
    names(dat),
    c("sampled_mean", "indicator", "value", "sim")
  )[1]
  if (is.na(value_col)) {
    value_col <- intersect(tolower(names(dat)), c("sampled_mean", "indicator", "value", "sim"))[1]
    if (!is.na(value_col)) {
      value_col <- names(dat)[match(value_col, tolower(names(dat)))]
    }
  }
  if (is.na(value_col)) {
    stop("Results must contain sampled_mean, indicator, value, or sim column.", call. = FALSE)
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
  if (is.na(time_col)) {
    stop("Results must contain year or period column.", call. = FALSE)
  }

  dat |>
    dplyr::mutate(
      indicator_id = indicator_id,
      period = if (time_col == "period") as.character(.data[[time_col]]) else NA_character_,
      year = if (time_col == "year") {
        as.integer(.data[[time_col]])
      } else {
        period_to_year(.data[[time_col]])
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

indicator_catalog <- function() {
  tibble::tribble(
    ~folder, ~title, ~indicator_id, ~ecosystem_hint,
    "NO_AATS_001", "Absence of alien coniferous tree species", "NO_AATS_001", "forest",
    "NO_AATS_002", "Alien conifers", "NO_AATS_002", "forest",
    "NO_AFOX_001", "Arctic fox", "NO_AFOX_001", "mountain",
    "NO_ALIE_001", "Alien species", "NO_ALIE_001", "mountain",
    "NO_ALIE_002", "Alien plants", "NO_ALIE_002", "forest,mountain",
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
    "NO_DIST_001", "Disturbance index", "NO_DIST_001", "mountain",
    "NO_FUNC_001-004", "Vegetation functional indicators", "NO_FUNC_xxx", "mountain",
    "NO_GLAC_001", "Glacier area", "NO_GLAC_001", "mountain",
    "NO_GOLD_001", "Golden eagle", "NO_GOLD_001", "mountain",
    "NO_JERV_001", "Wolverine", "NO_JERV_001", "mountain",
    "NO_LAVH_001", "Lichen biomass", "NO_LAVH_001", "mountain",
    "NO_LTFA_001", "Large trees share of forest area", "NO_LTFA_001", "forest",
    "NO_MLFA_001", "Multi-layered forest as share of productive forest area", "NO_MLFA_001", "forest",
    "NO_MOOS_001", "Moose", "NO_MOOS_001", "forest",
    "NO_PTAR_001", "Ptarmigan", "NO_PTAR_001", "mountain",
    "NO_RODE_001", "Rodents", "NO_RODE_001", "mountain",
    "NO_ROED_001", "Roe deer", "NO_ROED_001", "forest",
    "NO_SNOW_001", "NO_SNOW_001", "NO_SNOW_001", "mountain",
    "NO_TDTA_001", "Temperate deciduous trees by share of forest area in the nemoral and boreonemoral zones", "NO_TDTA_001", "forest",
    "NO_SLIT_001-004", "Trampling disturbance", "NO_SLIT_001", "mountain",
    "NO_SLIT_002", "Erosive disturbance", "NO_SLIT_002", "forest",
    "NO_BAER_001", "Brown bear", "NO_BAER_001", "forest",
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
    "Disturbance index" = "NO_DIST_001",
    "Trampling disturbance" = "NO_SLIT_001",
    "Vegetation functional indicators" = "NO_FUNC_xxx",
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
    "Glacier area" = "NO_GLAC_001",
    "Snow cover" = "NO_SNOW_001",
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

fetch_indicator_results_url <- function(
    indicator_id,
    folder = indicator_id,
    version = "1.0.0",
    repo = "NINAnor/ecRxiv",
    ref = "main") {
  file <- glue::glue("indicators/{folder}/data/results_{indicator_id}_v{version}.csv")
  glue::glue("https://raw.githubusercontent.com/{repo}/{ref}/{file}")
}

build_raw_url <- function(repo, ref, results_path) {
  glue::glue("https://raw.githubusercontent.com/{repo}/{ref}/{results_path}")
}

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
    parts <- c(parts, list(readr::read_csv(extra_path, show_col_types = FALSE)))
  }

  if (length(parts) == 0) {
    return(empty)
  }

  dplyr::bind_rows(parts) |>
    dplyr::distinct(.data$indicator_id, .data$ref, .data$results_path, .keep_all = TRUE) |>
    dplyr::arrange(.data$indicator_id, .data$priority, .data$ref)
}

submission_branch_candidates <- function(indicator_id, folder = indicator_id) {
  unique(c(
    glue::glue("submission-{indicator_id}"),
    glue::glue("submission-{folder}"),
    glue::glue("submission_{indicator_id}"),
    glue::glue("submission_{folder}")
  ))
}

indicator_mc_slug <- function(indicator_id) {
  core <- sub("^NO_", "", indicator_id)
  core <- sub("_0[0-9]{2}.*$", "", core)
  tolower(core)
}

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

results_path_candidates <- function(
    indicator_id,
    folder = indicator_id,
    version = "1.0.0") {
  slug <- indicator_mc_slug(indicator_id)
  fname_v <- glue::glue("results_{indicator_id}_v{version}.csv")
  fname <- glue::glue("results_{indicator_id}.csv")
  fname_parquet <- glue::glue("results_{indicator_id}.parquet")
  fname_parquet_sim <- glue::glue("results_{indicator_id}_sim.parquet")
  mc_regional <- glue::glue("{slug}_mc_regional.rds")

  unique(c(
    glue::glue("indicators/{folder}/data/{fname_v}"),
    glue::glue("indicators/{folder}/data/{fname}"),
    glue::glue("indicators/{indicator_id}/data/{fname_v}"),
    glue::glue("indicators/{indicator_id}/data/{fname}"),
    glue::glue("indicators/{folder}/data/{fname_parquet}"),
    glue::glue("indicators/{folder}/data/{fname_parquet_sim}"),
    glue::glue("indicators/{folder}/data/{mc_regional}"),
    glue::glue("indicators/{indicator_id}/data/{mc_regional}"),
    glue::glue("{folder}/data/{fname_v}"),
    glue::glue("R/{indicator_id}/data/{fname_v}"),
    glue::glue("R/{folder}/data/{fname_v}"),
    glue::glue("data/{fname_v}"),
    glue::glue("data/{fname}"),
    glue::glue("data/{mc_regional}"),
    glue::glue("{indicator_id}/data/{fname_v}")
  ))
}

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

probe_results_source <- function(path_or_url) {
  # Candidate URLs often 404; readr/arrow emit warnings rather than (only) errors.
  suppressWarnings(
    tryCatch({
      read_indicator_results(path_or_url, n_max = 1)
      TRUE
    }, error = function(e) {
      FALSE
    })
  )
}

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

  candidates <- manifest |>
    dplyr::filter(.data$indicator_id == !!indicator_id) |>
    dplyr::arrange(.data$priority, .data$ref)

  if (nrow(candidates) == 0) {
    candidates <- default_source_candidates(
      indicator_id,
      folder,
      version,
      repo,
      include_submission = include_submission
    )
  }

  candidates <- filter_indicator_source_candidates(candidates, indicator_id)

  for (i in seq_len(nrow(candidates))) {
    url <- build_raw_url(
      candidates$repo[i],
      candidates$ref[i],
      candidates$results_path[i]
    )
    if (probe_results_source(url)) {
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
  if (!is.na(local_path) && probe_results_source(local_path)) {
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

scan_git_indicator_sources <- function(
    git_remote = "upstream",
    repo = "NINAnor/ecRxiv",
    write_path = here::here("data/indicator_sources.csv")) {
  branches <- tryCatch(
    suppressWarnings(system2(
      "git",
      c("branch", "-r", "--list", paste0(git_remote, "/*")),
      stdout = TRUE,
      stderr = FALSE
    )),
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
    status <- suppressWarnings(system2(
      "git",
      c("rev-parse", "--verify", branch),
      stdout = FALSE,
      stderr = FALSE
    ))
    identical(status, 0L)
  }

  git_ls_files <- function(branch) {
    if (!git_branch_exists(branch)) {
      return(character())
    }
    out <- suppressWarnings(system2(
      "git",
      c("ls-tree", "-r", "--name-only", branch),
      stdout = TRUE,
      stderr = FALSE
    ))
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
    pq_files <- files[grepl("results_NO_.*\\.parquet$", files)]
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
    dplyr::distinct() |>
    dplyr::arrange(.data$indicator_id, .data$priority, .data$ref)

  if (!is.null(write_path) && nrow(out) > 0) {
    readr::write_csv(out, write_path)
  }

  extra_path <- here::here("data/indicator_sources_extra.csv")
  if (file.exists(extra_path)) {
    out <- dplyr::bind_rows(out, readr::read_csv(extra_path, show_col_types = FALSE)) |>
      dplyr::distinct(.data$indicator_id, .data$ref, .data$results_path, .keep_all = TRUE) |>
      dplyr::arrange(.data$indicator_id, .data$priority, .data$ref)
  }

  out
}

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
        dplyr::distinct(),
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

read_indicator_results <- function(
    path,
    indicator_id = NA_character_,
    lookup = load_region_lookup(),
    n_max = Inf) {
  dat <- read_indicator_file(path, n_max = n_max)
  out <- harmonise_indicator_draws(dat, indicator_id = indicator_id, lookup = lookup)

  if (grepl("_mc_regional\\.rds$", path, ignore.case = TRUE)) {
    nat_path <- sub("_mc_regional\\.rds$", "_mc_national.rds", path, ignore.case = TRUE)
    nat_ok <- if (nat_path != path) {
      if (is_results_url(nat_path)) {
        probe_results_source(nat_path)
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

select_reporting_years <- function(data, n = 2) {
  years_keep <- data |>
    dplyr::distinct(.data$indicator_id, .data$year) |>
    dplyr::group_by(.data$indicator_id) |>
    dplyr::arrange(dplyr::desc(.data$year), .by_group = TRUE) |>
    dplyr::slice_head(n = n) |>
    dplyr::ungroup()

  data |>
    dplyr::semi_join(years_keep, by = c("indicator_id", "year"))
}

load_registry_draws <- function(
    registry,
    n_years = 2,
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
    select_reporting_years(n = n_years)
}

unnest_upscale <- function(x) {
  if (vctrs::vec_size(x$sampled_mean[[1]]) == 1L) {
    return(x)
  }
  tidyr::unnest_longer(x, sampled_mean)
}

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

aggregate_to_index <- function(ect_draws, n = 1000) {
  parts <- unique(ect_draws$part)
  purrr::map_dfr(parts, function(p) {
    df <- ect_draws |>
      dplyr::filter(.data$part == p) |>
      dplyr::mutate(wgt = 1, index_level = "Index")

    out <- ecTools::ec_upscale(
      data = df,
      variable = sampled_mean,
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

aggregate_to_national <- function(
    draws,
    value_col = "sampled_mean",
    id_col = "ect",
    n = 1000) {
  regional <- draws |>
    dplyr::filter(.data$part %in% setdiff(CANONICAL_PARTS, "Norway"))

  if (nrow(regional) == 0) {
    return(tibble::tibble())
  }

  id_sym <- rlang::sym(id_col)

  regional |>
    dplyr::mutate(wgt = 1, national = "Norway") |>
    dplyr::group_by(!!id_sym, .data$year) |>
    dplyr::group_split(.keep = TRUE) |>
    purrr::map_dfr(function(df) {
      id_val <- df[[id_col]][1]
      out <- ecTools::ec_upscale(
        data = df,
        variable = !!rlang::sym(value_col),
        weight = wgt,
        start_units = part,
        end_units = national,
        year = year,
        end_units_name = "part",
        n = n
      )
      unnest_upscale(out) |>
        dplyr::mutate(!!id_col := id_val, part = "Norway")
    })
}

indicator_coverage <- function(draws, registry) {
  draws |>
    dplyr::group_by(.data$year, .data$part) |>
    dplyr::summarise(
      n_indicators = dplyr::n_distinct(.data$indicator_id),
      indicators = paste(sort(unique(.data$indicator_name)), collapse = "; "),
      n_registry_matched = sum(registry$match_status == "matched", na.rm = TRUE),
      .groups = "drop"
    )
}

calculate_index <- function(
    registry,
    n_sim = 1000,
    n_years = 2,
    include_national = TRUE,
    ...) {
  draws <- load_registry_draws(registry, n_years = n_years, ...)

  ect_draws <- aggregate_to_ect(draws, n = n_sim) |>
    dplyr::mutate(level = "ECT")

  index_draws <- aggregate_to_index(ect_draws, n = n_sim) |>
    dplyr::mutate(level = "Index")

  results <- dplyr::bind_rows(
    ect_draws |> dplyr::rename(id = ect),
    index_draws |> dplyr::select(dplyr::any_of(c("year", "part", "id", "level", "sampled_mean")))
  )

  if (include_national) {
    nat_ect <- aggregate_to_national(ect_draws, id_col = "ect", n = n_sim) |>
      dplyr::mutate(level = "ECT") |>
      dplyr::rename(id = ect)
    nat_index <- aggregate_to_national(
      index_draws |> dplyr::mutate(id = "Index"),
      id_col = "id",
      n = n_sim
    ) |>
      dplyr::mutate(level = "Index")
    results <- dplyr::bind_rows(results, nat_ect, nat_index)
  }

  list(
    indicator_draws = draws,
    distributions = results,
    coverage = indicator_coverage(draws, registry),
    summaries = summarise_index(results)
  )
}

summarise_index <- function(distributions) {
  distributions |>
    dplyr::group_by(.data$year, .data$part, .data$level, .data$id) |>
    dplyr::summarise(
      median = stats::median(.data$sampled_mean, na.rm = TRUE),
      q025 = stats::quantile(.data$sampled_mean, 0.025, na.rm = TRUE),
      q975 = stats::quantile(.data$sampled_mean, 0.975, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )
}

plot_index_forest <- function(summaries, title = "NO_INDEX_001") {
  ect_ids <- summaries |>
    dplyr::filter(.data$level == "ECT") |>
    dplyr::pull(.data$id) |>
    unique() |>
    sort()
  id_levels <- c(ect_ids, "Index")

  plot_dat <- summaries |>
    dplyr::mutate(
      id = factor(.data$id, levels = id_levels),
      part = factor(.data$part, levels = CANONICAL_PARTS)
    )

  ggplot2::ggplot(
    plot_dat,
    ggplot2::aes(
      x = median,
      y = id,
      xmin = q025,
      xmax = q975,
      colour = part
    )
  ) +
    ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey70") +
    ggplot2::geom_pointrange(
      position = ggplot2::position_dodge(width = 0.5),
      size = 0.35
    ) +
    ggplot2::facet_wrap(~year, nrow = 1) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      title = title,
      x = "Index value",
      y = NULL,
      colour = "Region"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      panel.spacing.x = ggplot2::unit(0.8, "lines"),
      plot.margin = ggplot2::margin(5, 5, 5, 5)
    )
}

export_index_results <- function(distributions, path) {
  out <- distributions |>
    dplyr::transmute(
      year = .data$year,
      part = .data$part,
      level = .data$level,
      id = .data$id,
      sampled_mean = .data$sampled_mean
    )
  readr::write_csv(out, path)
}
