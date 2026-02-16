# Settlements per LAUs ---------------------------------------------------------
# Create function to do the spatial intersection
# includes registration of grunnkart and its cleaning
do_intersection <- function(db_connexion, fylke, path_list, fylke_index_path_list, bd_path){
  
  if(fylke %in% c(3, 11, 15, 40, 42, 46, 31, 33, 32, 39, 50)){
    
    # Register Grunnkart for the specific fylke
    idx <- which(fylke_index_path_list == fylke)
    ar_path <- normalizePath(path_list[idx])
    ar_layer <- "arealregnskap"
    dbExecute(db_connexion, glue("
  CREATE VIEW grunnkart AS 
  SELECT *,
  FROM ST_Read('{ar_path}', layer='{ar_layer}')"))
    
    # Register LAU for specific Fylke
    lau_path <- bd_path
    dbExecute(db_connexion, glue("
  CREATE VIEW lau_fylke AS 
  SELECT *,
  FROM ST_Read('{bd_path}')"))
    
    # Spatial intersect to join the variables of both datasets before intersection
    dbExecute(db_connexion, query_valid)
    dbExecute(db_connexion, query_intersect)
    dbExecute(db_connexion, query_drop)
    
    # Perform spatial intersection
    soaa_lau <- ddbs_intersection(db_connexion, x = "grunnkart_intersection", y = "lau_fylke", crs = "EPSG:3035")
    
    # Drop the grunnkart register tables
    dbExecute(db_connexion, "DROP VIEW grunnkart")
    dbExecute(db_connexion, "DROP VIEW lau_fylke")
    dbExecute(db_connexion, "DROP TABLE grunnkart_valid")
    dbExecute(db_connexion, "DROP TABLE grunnkart_intersection")
    
    # Return the spatial intersection
    return(soaa_lau)
    
  } else if(fylke %in% c(18, 55)){
    
    # Register grunnkart for the specific fylke
    idx <- which(fylke_index_path_list == fylke)
    ar_path <- path_list[idx]
    ar_layer <- "arealregnskap"
    dbExecute(db_connexion, glue("
  CREATE VIEW grunnkart AS 
  SELECT *,
  FROM ST_Read('{ar_path}', layer='{ar_layer}')"))
    
    # Register LAU for specific Fylke
    lau_path <- bd_path
    dbExecute(db_connexion, glue("
  CREATE VIEW lau_fylke AS 
  SELECT *,
  FROM ST_Read('{bd_path}')"))
    
    # Spatial intersect to join the variables of both datasets before intersection
    dbExecute(db_connexion, query_valid_18_55)
    dbExecute(db_connexion, query_intersect)
    dbExecute(db_connexion, query_drop)
    
    # Perform spatial intersection
    soaa_lau <- ddbs_intersection(db_connexion, x = "grunnkart_intersection", y = "lau_fylke", crs = "EPSG:3035")
    
    # Drop the grunnkart register tables
    dbExecute(db_connexion, "DROP VIEW grunnkart")
    dbExecute(db_connexion, "DROP VIEW lau_fylke")
    dbExecute(db_connexion, "DROP TABLE grunnkart_valid")
    dbExecute(db_connexion, "DROP TABLE grunnkart_intersection")
    
    # Return the spatial intersection
    return(soaa_lau)
    
  } else if(fylke == 56){
    
    # Register Grunnkart for the specific fylke
    idx <- which(fylke_index_path_list == fylke)
    ar_path <- path_list[idx]
    ar_layer <- "arealregnskap"
    dbExecute(db_connexion, glue("
  CREATE VIEW grunnkart AS 
  SELECT *,
  FROM ST_Read('{ar_path}', layer='{ar_layer}')"))
    
    # Register LAU for specific Fylke
    lau_path <- bd_path
    dbExecute(db_connexion, glue("
  CREATE VIEW lau_fylke AS 
  SELECT *,
  FROM ST_Read('{bd_path}')"))
    
    # Spatial intersect to join the variables of both datasets before intersection
    dbExecute(db_connexion, query_valid_56)
    dbExecute(db_connexion, query_intersect)
    dbExecute(db_connexion, query_drop)
    
    # Perform spatial intersection
    soaa_lau <- ddbs_intersection(db_connexion, x = "grunnkart_intersection", y = "lau_fylke", crs = "EPSG:3035")
    
    # Drop the grunnkart register tables
    dbExecute(db_connexion, "DROP VIEW grunnkart")
    dbExecute(db_connexion, "DROP VIEW lau_fylke")
    dbExecute(db_connexion, "DROP TABLE grunnkart_valid")
    dbExecute(db_connexion, "DROP TABLE grunnkart_intersection")
    
    return(soaa_lau)
    
  } else if(fylke == 34){
    
    # Register LAU for specific Fylke
    lau_path <- bd_path
    dbExecute(db_connexion, glue("
  CREATE VIEW lau_fylke AS 
  SELECT *,
  FROM ST_Read('{bd_path}')"))
    
    # Spatial intersect to join the variables of both datasets before intersection
    dbExecute(db_connexion, query_valid_34)
    dbExecute(db_connexion, query_intersect)
    dbExecute(db_connexion, query_drop)
    
    # Perform spatial intersection
    soaa_lau <- ddbs_intersection(db_connexion, x = "grunnkart_intersection", y = "lau_fylke", crs = "EPSG:3035")
    
    # Drop the grunnkart register tables
    dbExecute(db_connexion, "DROP VIEW lau_fylke")
    dbExecute(db_connexion, "DROP TABLE grunnkart_valid")
    dbExecute(db_connexion, "DROP TABLE grunnkart_intersection")
    
    return(soaa_lau)
    
  } else (retrun("an error has occured"))
  
}



# Load PM2.5 concentration maps and store them on disk --------------------------------
# Write API query function: retrieve the data and write them on the disk
api_fetch_data <- function(year_vec, municipality_vec, year, municipality, out_path, url_list){
  
  idx_year <- which(year_vec == year)
  idx_municip <- which(municipality_vec == municipality)
  
  # Create a request
  req <- request(url_list[[idx_year]][idx_municip])
  
  # Perform the request
  fetch_data <- req %>% 
    req_perform()
  
  # Write data in GIS folder       
  resp_body_raw(fetch_data) %>% 
    writeBin(., paste0(out_path, year, "/PM25_", year, "_", municipality, ".tif"))
}



# Compile PM2.5 concentration in urban areas ---------------------------------------
# Create function to read the raster and extract the raster cells corresponding to SOOA
extract_rast <- function(path_rast_folder, soaa_shp, municipality, year){
  
  # Load packages so they are available in environment fo the worker for parallel computing
  library(dplyr)
  library(sf)
  library(terra)
  library(exactextractr)
  
  # Read in data
  municip_rast <- rast(paste0(path_rast_folder, year, "/PM25_", year, "_", municipality, ".tif"))
  soaa_bd <- soaa_shp %>%
    st_transform(., crs(municip_rast)) %>%
    mutate(soaa_area_m2 = unclass(st_area(.)))
  
  # Extract raster cells
  soaa_lau_pm25 <- soaa_bd %>%
    exact_extract(municip_rast,
                  .,
                  fun = c("mean"),
                  append_cols = TRUE) %>%
    filter(!is.na(mean) == TRUE) %>%  # Cleaning step
    mutate(., year = year)
  return(soaa_lau_pm25)
}




# Mean PM2.5 concentration in urban areas -------------------------------------
# Create an averaging function
read_and_compile_pm25 <- function(path_rast_folder, year, municipality){
  
  # Load libraries for workers
  library(terra)
  library(dplyr)
  
  # Read in data
  municip_rast <- rast(paste0(path_rast_folder, year, "/PM25_", year, "_", municipality, ".tif"))
  
  # Average
  av_pm25_municip <- values(municip_rast) %>% 
    .[!is.na(.)] %>%
    mean()
  sd_pm25_municip <- values(municip_rast) %>% 
    .[!is.na(.)] %>%
    sd()
  
  # Return a vector of results
  res <- data.frame(mean_pm25_municip_ug_m3 = av_pm25_municip, 
                    sd_pm25_municip_ug_m3 = sd_pm25_municip,
                    year = year, 
                    kommune_nummer = as.numeric(municipality))
  
  return(res)
  
}
