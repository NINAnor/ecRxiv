# ================================================================
# ONE-OFF REBUILD OF MOUNTAIN NO_IDEX_001
#
# Purpose:
# Recalculate the mountain index with NO_FUHR_002 assigned to A1
# using its existing local parquet results.
#
# This does NOT modify index_functions.R.
# ================================================================


# 1. Packages -----------------------------------------------------

library(dplyr)
library(readr)
library(here)


# 2. Paths --------------------------------------------------------

index_dir <- here::here(
  "indicators",
  "NO_IDEX_001"
)

mountain_indicator_file <- file.path(
  index_dir,
  "data",
  "indicators_mountain.csv"
)

mountain_rds <- file.path(
  index_dir,
  "data",
  "mountain_index_170826.rds"
)

mountain_results_csv <- file.path(
  index_dir,
  "data",
  "results_NO_IDEX_001_mountain_v1.0.0.csv"
)

fuhr_path <- here::here(
  "indicators",
  "NO_FUHR_002",
  "data",
  "results_NO_FUHR_002.parquet"
)


# 3. Source index functions ---------------------------------------

source(
  file.path(
    index_dir,
    "R",
    "index_functions.R"
  )
)


# 4. Check FUHR source file ---------------------------------------

stopifnot(
  file.exists(fuhr_path)
)

message(
  "FUHR results found: ",
  fuhr_path
)


# 5. Read current mountain indicator list -------------------------

mountain_metadata <- readr::read_csv(
  mountain_indicator_file,
  show_col_types = FALSE
)

stopifnot(
  nrow(mountain_metadata) == 11L
)


# 6. Confirm current FUHR assignment ------------------------------

fuhr_metadata <- mountain_metadata |>
  filter(
    indicatorID == "NO_FUHR_002"
  )

print(fuhr_metadata)

stopifnot(
  nrow(fuhr_metadata) == 1L,
  fuhr_metadata$ECT == "A1"
)

message(
  "Current mountain metadata correctly assigns NO_FUHR_002 to A1."
)


# 7. Build normal mountain registry -------------------------------
closeAllConnections()
registry_mountain <- build_indicator_registry(
  mountain_indicator_file,
  ecosystem = "mountain",
  write_path = NULL
)


# 8. ONE-OFF FUHR FIX ---------------------------------------------
#
# The resolver currently does not find the local FUHR parquet.
# We therefore provide its known local path directly.
#
# We also explicitly enforce ECT = A1.

registry_mountain <- registry_mountain |>
  mutate(
    ECT = if_else(
      indicatorID == "NO_FUHR_002",
      "A1",
      ECT
    ),
    results_path = if_else(
      indicatorID == "NO_FUHR_002",
      fuhr_path,
      results_path
    ),
    results_source = if_else(
      indicatorID == "NO_FUHR_002",
      "local",
      results_source
    ),
    match_status = if_else(
      indicatorID == "NO_FUHR_002",
      "matched",
      match_status
    )
  )


# 9. Check FUHR registry row --------------------------------------

fuhr_registry <- registry_mountain |>
  filter(
    indicatorID == "NO_FUHR_002"
  )

fuhr_registry |>
  select(
    indicatorID,
    indicatorName,
    ECT,
    match_status,
    any_of(
      c(
        "results_path",
        "results_source"
      )
    )
  ) |>
  print(width = Inf)

stopifnot(
  nrow(fuhr_registry) == 1L,
  fuhr_registry$ECT == "A1",
  fuhr_registry$match_status == "matched"
)


# 10. Check all mountain indicators -------------------------------

registry_mountain |>
  select(
    indicatorID,
    ECT,
    match_status
  ) |>
  arrange(
    ECT,
    indicatorID
  ) |>
  print(
    n = Inf
  )

registry_mountain |>
  count(
    match_status
  ) |>
  print()


# IMPORTANT:
# We expect all 11 mountain indicators to be matched.

stopifnot(
  nrow(registry_mountain) == 11L,
  sum(registry_mountain$match_status == "matched") == 11L
)

message(
  "All 11 mountain indicators matched."
)


# 11. Recalculate mountain index ----------------------------------
#
# Same settings as NO_IDEX_001.qmd.

set.seed(123)

mountain_index <- calculate_index(
  registry_mountain |>
    filter(
      match_status == "matched"
    ),
  n_sim = 1000,
  n_years = 2,
  report_years = c(
    2014,
    2019,
    2024
  ),
  max_gap = 5,
  include_direct_index = TRUE
)


# 12. Critical QA: FUHR must now be A1 ----------------------------

fuhr_check <- mountain_index$indicator_draws |>
  filter(
    indicator_id == "NO_FUHR_002"
  ) |>
  distinct(
    indicator_id,
    ect,
    year,
    imputed,
    source_year
  ) |>
  arrange(
    year
  )

print(fuhr_check)

stopifnot(
  nrow(fuhr_check) == 2L,
  all(fuhr_check$ect == "A1"),
  setequal(
    fuhr_check$year,
    c(2019, 2024)
  )
)


# 2024 must be the genuine FUHR result.

stopifnot(
  fuhr_check |>
    filter(year == 2024) |>
    pull(imputed) ==
    FALSE
)


# 2019 should be carried backwards from 2024.

stopifnot(
  fuhr_check |>
    filter(year == 2019) |>
    pull(imputed) ==
    TRUE
)

stopifnot(
  fuhr_check |>
    filter(year == 2019) |>
    pull(source_year) ==
    2024
)

message(
  "FUHR QA passed: A1, observed 2024, imputed 2019."
)


# 13. Check all 11 indicators survived ----------------------------

used_indicators <- mountain_index$indicator_draws |>
  distinct(
    indicator_id
  ) |>
  arrange(
    indicator_id
  )

print(used_indicators, n = Inf)

stopifnot(
  nrow(used_indicators) == 11L
)

message(
  "All 11 mountain indicators are present in recalculated index."
)


# 14. Check ECT assignments ---------------------------------------

ect_check <- mountain_index$indicator_draws |>
  distinct(
    indicator_id,
    ect
  ) |>
  arrange(
    ect,
    indicator_id
  )

print(
  ect_check,
  n = Inf
)

# FUHR should occur ONLY in A1.

stopifnot(
  ect_check |>
    filter(
      indicator_id == "NO_FUHR_002"
    ) |>
    pull(ect) ==
    "A1"
)


# 15. Inspect recalculated mountain summaries ---------------------

mountain_index$summaries |>
  filter(
    level %in% c(
      "ECT",
      "Index"
    )
  ) |>
  arrange(
    year,
    part,
    level,
    id
  ) |>
  print(
    n = Inf
  )


# 16. Back up old RDS ---------------------------------------------
#
# Do this before overwriting it.

backup_rds <- file.path(
  index_dir,
  "data",
  "mountain_index_170826_BEFORE_FUHR_A1_FIX.rds"
)

if (
  file.exists(mountain_rds) &&
  !file.exists(backup_rds)
) {
  file.copy(
    mountain_rds,
    backup_rds
  )
  
  message(
    "Old mountain RDS backed up to: ",
    backup_rds
  )
}


# 17. Save corrected RDS ------------------------------------------

saveRDS(
  mountain_index,
  mountain_rds
)

message(
  "Corrected mountain index saved to: ",
  mountain_rds
)


# 18. Regenerate exported mountain results CSV --------------------

export_index_results(
  mountain_index$distributions,
  mountain_results_csv
)

message(
  "Corrected mountain results CSV saved to: ",
  mountain_results_csv
)


# 19. Final reload check ------------------------------------------

mountain_index_test <- readRDS(
  mountain_rds
)

final_fuhr_check <- mountain_index_test$indicator_draws |>
  filter(
    indicator_id == "NO_FUHR_002"
  ) |>
  distinct(
    indicator_id,
    ect,
    year,
    imputed,
    source_year
  ) |>
  arrange(
    year
  )

print(final_fuhr_check)

stopifnot(
  all(final_fuhr_check$ect == "A1")
)

message(
  "SUCCESS: saved mountain index now contains NO_FUHR_002 in A1."
)