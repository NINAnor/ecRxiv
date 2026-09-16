# NO_GJEN_002 input contract

The production pipeline does not store national imagery, predictions or result
rasters in Git. Paths are configured in `config.yml` and may point to mounted
NINA storage.

## AOI layer

The AOI layer must be polygonal and contain unique `id` values plus an
`ecosystem` class. It is transformed to EPSG:25833. Change field mappings in
`config.yml` when the authoritative data uses different names.

## Region layer

The region layer must contain the five reporting regions with `region_id` and
`region_name`. Polygon pieces are used as regional area weights, so polygons
crossing a boundary contribute to both regions in proportion to area.

## Bioclimatic zones

The zone layer must contain `vegClimZone`. Each AOI receives the class with the
largest overlap for reference-value lookup.

## Reference tables

Both CSV files use the keys `ecosystem`, `region_id` and `bioclimatic_zone`.
The good-condition table adds `reference_height_m`; the forest table adds
`forest_height_m`. Values must be calibrated from the aerial-image model before
an operational release. LiDAR-derived values may only be used when explicitly
labelled as provisional.

## Orthophoto archive and reporting year

`paths.ortho_archive` points to the output root from `host_norge_master.ps1`.
The indexer expects recursive block folders containing files named
`nib_AAR_xX_yY.jpg` and, where available, `flisregister.csv`. Coordinates are
interpreted as the lower-left corner of a 2 × 2 km tile in EPSG:25833. JPEGs
must be 1000 × 1000 pixels at 2 m. Actual centre-project dates from the block
registers are retained; the year in the filename is used as fallback. Because
the downloaded mosaic may use older ordinary orthophoto to fill NoData at a
project edge, this date is source metadata, not a per-pixel acquisition date.

The reporting year is 2024 in the supplied configuration. The archive contains
the newest ordinary orthophoto available when it was harvested, with skew
photography, satellite imagery and IR excluded by the downloader.

The Meta workflow has been evaluated at 0.5 and 1 m. Set
`allow_untested_resolution: true` only for an explicitly experimental 2 m run,
and validate the derived heights before indicator reporting.

## Model version

Set `model.repository_revision` in `config.yml` to the full reviewed Git commit
of Meta High Resolution Canopy Height. Input validation stops when the revision
is left as a placeholder or the local checkout does not match it.
