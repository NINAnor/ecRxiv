# ============================================================
# Snow depth indicator for fjell regions using seNorge data
# Winter year: Dec(previous year) + Jan-May(current year)
# ============================================================

library(sf)
library(terra)
library(ncdf4)
library(lubridate)
library(abind)
library(dplyr)

# -------------------------------
# 1. LOAD DATA
# -------------------------------

fjell <- st_read(here::here(
  "data/Fjell_norge_publisering.gdb"),
  layer = "Modellert_fjell_50"
)

reg_helperfile <- readRDS(here::here(
  "C:/Users/matthew.grainger/Downloads/reg_helperfile.rds"
))

reg_helperfile <- st_transform(reg_helperfile, st_crs(fjell))

bbox <- st_bbox(fjell)

# -------------------------------
# 2. DEFINE YEARS AND EMPTY OUTPUT
# -------------------------------

# Starts at 1959 because winter 1958 would need Dec 1957
years <- 1959:2026

results <- data.frame(
  year = integer(),
  id = integer(),
  region = character(),
  arealFjell = numeric(),
  mean_snow = numeric()
)

# -------------------------------
# 3. LOOP THROUGH WINTER YEARS
# -------------------------------

for (yr in years) {
  
  cat("Processing winter year:", yr, "\n")
  
  tryCatch({
    
    url_prev <- paste0(
      "https://thredds.met.no/thredds/dodsC/senorge/seNorge_snow/sd/sd_",
      yr - 1,
      ".nc"
    )
    
    url_curr <- paste0(
      "https://thredds.met.no/thredds/dodsC/senorge/seNorge_snow/sd/sd_",
      yr,
      ".nc"
    )
    
    # -------------------------------
    # 3a. OPEN CURRENT YEAR: JAN-MAY
    # -------------------------------
    
    nc_curr <- nc_open(url_curr)
    
    time_curr <- ncvar_get(nc_curr, "time")
    
    dates_curr <- as.POSIXct(
      time_curr,
      origin = "1900-01-01",
      tz = "UTC"
    )
    
    idx_jan_may <- which(month(dates_curr) %in% 1:5)
    
    x_vals <- nc_curr$dim$x$vals
    y_vals <- nc_curr$dim$y$vals
    
    x_idx <- which(
      x_vals >= bbox["xmin"] &
        x_vals <= bbox["xmax"]
    )
    
    y_idx <- which(
      y_vals >= bbox["ymin"] &
        y_vals <= bbox["ymax"]
    )
    
    sd_jan_may <- ncvar_get(
      nc_curr,
      "snow_depth",
      start = c(min(x_idx), min(y_idx), min(idx_jan_may)),
      count = c(length(x_idx), length(y_idx), length(idx_jan_may))
    )
    
    nc_close(nc_curr)
    
    # -------------------------------
    # 3b. OPEN PREVIOUS YEAR: DECEMBER
    # -------------------------------
    
    nc_prev <- nc_open(url_prev)
    
    time_prev <- ncvar_get(nc_prev, "time")
    
    dates_prev <- as.POSIXct(
      time_prev,
      origin = "1900-01-01",
      tz = "UTC"
    )
    
    idx_dec <- which(month(dates_prev) == 12)
    
    sd_dec <- ncvar_get(
      nc_prev,
      "snow_depth",
      start = c(min(x_idx), min(y_idx), min(idx_dec)),
      count = c(length(x_idx), length(y_idx), length(idx_dec))
    )
    
    nc_close(nc_prev)
    
    # -------------------------------
    # 3c. COMBINE DEC + JAN-MAY
    # -------------------------------
    
    sd <- abind(sd_dec, sd_jan_may, along = 3)
    
    # -------------------------------
    # 3d. CONVERT TO RASTER STACK
    # -------------------------------
    
    r <- rast(aperm(sd, c(2, 1, 3)))
    
    ext(r) <- c(
      min(x_vals[x_idx]), max(x_vals[x_idx]),
      min(y_vals[y_idx]), max(y_vals[y_idx])
    )
    
    crs(r) <- "EPSG:25833"
    
     
    r <- flip(r, direction = "vertical")
    
    r_mean <- mean(r, na.rm = TRUE)
   # test raster 
   # plot(r_mean)
    # plot(st_geometry(reg_helperfile), add = TRUE, border = "red", lwd = 2)
    
    
    # -------------------------------
    # 3e. MASK TO FJELL
    # -------------------------------
    
    fjell_rast <- rasterize(
      vect(fjell),
      r_mean,
      field = 1
    )
    
    r_masked <- mask(r_mean, fjell_rast)
    
    region_vals <- terra::extract(
      r_masked,
      vect(reg_helperfile),
      fun = mean,
      na.rm = TRUE
    )
    
    # -------------------------------
    # 3f. CALCULATE MEAN SNOW BY REGION
    # -------------------------------
    
    region_vals <- terra::extract(
      r_masked,
      vect(reg_helperfile),
      fun = mean,
      na.rm = TRUE
    )
    
    region_results <- cbind(
      st_drop_geometry(reg_helperfile)[, c("id", "region", "arealFjell")],
      mean_snow = region_vals[, 2]
    )
    
    region_results$year <- yr
    
    results <- bind_rows(
      results,
      region_results[, c("year", "id", "region", "arealFjell", "mean_snow")]
    )
    
    rm(
      sd_dec, sd_jan_may, sd,
      r, r_mean, r_masked, fjell_rast,
      region_vals, region_results
    )
    
    gc()
    
  }, error = function(e) {
    message("Failed for winter year ", yr, ": ", e$message)
  })
}

# -------------------------------
# 4. OUTPUT
# -------------------------------

print(results)

write.csv(
  results,
  "snow_indicator_fjell_by_region_winter_year.csv",
  row.names = FALSE
)

library(tidyverse)
results<-read.csv("snow_indicator_fjell_by_region_winter_year.csv")
results |> 
  ggplot(aes(year, mean_snow))+
  geom_bar(stat="identity")+
  facet_wrap(~region)

# workout baseline 1961-1990

baselines<-results |> 
  group_by(region) |> 
  filter(between(year, 1961,1990)) |> 
  summarise(baseline_mean=mean(mean_snow),
            baseline_sd=sd(mean_snow))

baselines  


