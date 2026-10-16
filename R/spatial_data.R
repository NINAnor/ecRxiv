## Read Norway / regions, join to open lowlands points

nor <- st_read(paste0(here(), "/data/geo/outlineOfNorway_EPSG25833.shp"), quiet = TRUE) |>
  st_as_sf() |>
  st_transform(crs = st_crs(ano_geo))  # originally ano_lowlands

reg <- st_read(paste0(here(), "/data/geo/regions.shp"), quiet = TRUE) |>
  st_as_sf() |>
  st_transform(crs = st_crs(ano_geo))

# update region names
reg$region <- c(
  "Northern_Norway",
  "Central_Norway",
  "Eastern_Norway",
  "Western_Norway",
  "Southern_Norway"
)

regnor <- st_intersection(reg, nor)


write_rds(regnor, paste0(here::here(), "/data/cache/regnor.RDS"))
