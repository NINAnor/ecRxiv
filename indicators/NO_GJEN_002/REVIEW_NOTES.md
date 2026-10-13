# NO_GJEN_002 v0.2.2 — internal review notes

Prepared for internal NINA code review in September 2026.

## Scope of this revision

- Audited inference against Meta's published `inference.py`; restored the
  documented input p5/p95 normalization and `10 * raw prediction` height scale.
- Made normalization quantiles and output scale/offset explicit in config and
  provenance. The inherited 20/80 and `9.51*x+0.4` values had no documented
  calibration source and are not used in v0.2.2.
- Added checkpoint SHA-256, model revision and inference settings to every run.
- Existing predictions are now checked for dimensions, CRS, resolution and
  bounds before being reused.

- Added a fully isolated synthetic end-to-end test covering five AOIs, five
  regions, zonal extraction, reference joins, aggregation and all main outputs.
- Added a bounded NINA file-candidate search and schema inspection report; no
  candidate is automatically promoted to an authoritative production input.

- Replaced the internal WMS downloader with an indexer for the independently
  harvested nationwide NIB archive (2 m, 1000 px, 2 km tiles, EPSG:25833).
- Limited inference to archive tiles intersecting one or more AOIs.
- Added overlapping 256 px inference windows with feather blending and one
  georeferenced 1000 px CHM output per source tile.
- Added resumable inference and imported photo/project metadata from each block.
- Marked all 2 m model outputs experimental pending resolution-specific
  validation.

- Added Vegar Bakkestuen as an author.
- Updated metadata to semantic version `0.1.0` and retained status `incomplete`.
- Replaced workstation-specific repository paths with repository-local paths.
- Added environment variables for external NINA data roots:
  `NINA_P_PROJECTS_ROOT` and `NINA_R_DATA_ROOT`.
- Corrected aerial-image tensor dimensions and channel order in the Python
  inference example.
- Corrected command-line Boolean handling and CPU/GPU selection.
- Changed region and vegetation-zone assignment to use the largest spatial
  overlap instead of the count of intersecting features.
- Added input checks to the indicator scaling function.
- Aligned the ECT label in the text with metadata (`B3`).
- Added a modular, resumable production pipeline for tile acquisition, model
  inference, zonal statistics, reference joins, aggregation and export.
- Added an explicit input contract and configuration file.
- Added area-weighted outputs for Norway and all five reporting regions.
- Added acquisition-date and raster-coverage reporting so reporting year 2024
  is not confused with nationwide imagery acquired in 2024.
- Added smoke tests for scaling and aggregation helpers.

## Items for the reviewer

1. Confirm that `B3 - Functional State Characteristics` is the intended ECT
   classification.
2. The unexplained 20th/80th input percentiles were removed in v0.2.2. Meta's
   published 5th/95th percentiles are now the versioned default.
3. Verify the inference example against the exact pinned revision and model
   checkpoints from Meta's High Resolution Canopy Height repository.
4. Verify that `vegHeights_skog_climZoneRegion.csv` is the authoritative forest
   reference table and document its provenance/version.
5. Confirm the intended treatment of polygons spanning region or vegetation-zone
   boundaries. This revision uses the class with the largest overlapping area.
6. Recalibrate aerial-image reference levels before changing status from
   `incomplete` or releasing version `1.0.0`.
7. Confirm that the target AOI layer is a census of the reporting population.
   If it is a probability sample, replace area-only weights with documented
   design weights before reporting national or regional estimates.
8. Confirm that "2024" is the reporting year using the latest imagery available
   up to 2024, and approve the minimum acceptable acquisition year.
9. Test WMS layer selection and Meta inference on NINA infrastructure, then run
   the pipeline twice to confirm that completed tiles resume without changes.
10. Review the six-row national and regional table together with the coverage
    report before publishing indicator values.
11. Decide whether overlapping inference windows and edge blending are required
    for production. The current manifest uses one aligned, non-overlapping tile
    grid and reports this known model limitation.

## Validation status

- Quarto fence structure and Python syntax have been checked.
- Metadata values and workbook rendering have been checked.
- Full Quarto execution was not performed in the review workspace because R,
  Quarto, the model checkpoints and NINA's external forest-reference table were
  unavailable.
- The production scripts have been statically checked and the Python entrypoint
  parses. End-to-end execution requires the authoritative national inputs,
  GDAL-enabled R packages and Meta model environment.
