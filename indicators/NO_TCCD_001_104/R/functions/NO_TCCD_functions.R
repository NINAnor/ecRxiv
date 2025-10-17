# Load and save Copernicus Tree Cover Density data------------------------------
# Create a function to create a list of file names to read
files_list_fn <- function(path){
  files_names <- list.files(path, pattern = "\\.zip$", 
                            full.names = FALSE) %>%
    map_vec(., ~gsub('.zip','',.))
  
  tcd_files <- list.files(path, pattern = "\\.zip$", 
                          full.names = TRUE)
  
  tcd_files_to_read <- map_vec(seq_along(tcd_files), function(i){paste0("/vsizip/{", tcd_files[i], "}/", files_names[i], ".tif")})
  
  return(list(tcd_files_to_read, files_names))
}

# Create a function to read the rasters, mask Norway and export the result
process_raster_fn <- function(path_read, norway_mask, path_write, files_names){
  
  # Load libraries for parallel computing
  library(terra)
  library(sf)
  
  # Read
  tcd_rast <- rast(path_read)
  
  # Re-project norway boundary
  norway_mask_pj <- st_transform(norway_mask, crs = crs(tcd_rast))
  
  # Test overlap, crop and export 
  ext_tcd_rast <- ext(tcd_rast)
  ext_no <- ext(norway_mask_pj)
  test_intersect <- intersect(ext_tcd_rast, ext_no)
  if(is.null(test_intersect) == TRUE){
    return("Not in Norway")
  } else{
    # Mask
    tcd_no_mask <- crop(tcd_rast, norway_mask_pj, mask = TRUE)
    
    # Clean values 0 and 255
    rclfy_mx <- matrix(c(0, 255,
                         NaN, NaN), 
                       nrow = 2, 
                       ncol = 2)
    tcd_reclfy <- classify(tcd_no_mask, rclfy_mx)
    levels(tcd_reclfy) <- levels(tcd_rast)[[1]]
    
    # Export
    writeRaster(tcd_reclfy, paste0(path_write, files_names, "_NOcln.tif"))
    
    return("Successfully written")
  }
  
}

# Load and prepare the forest area data -----------------------------------------

# Create function to do the spatial intersection
# includes registration of grunnkart and its cleaning
do_intersection_forest <- function(db_connexion, fylke, path_list, fylke_index_path_list, bd_path){
  
  if(fylke %in% c(3, 11, 15, 40, 42, 46, 31, 32, 39)){
    
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
    forest_lau <- ddbs_intersection(db_connexion, x = "grunnkart_intersection", y = "lau_fylke", crs = "EPSG:3035")
    
    # Drop the grunnkart register tables
    dbExecute(db_connexion, "DROP VIEW grunnkart")
    dbExecute(db_connexion, "DROP VIEW lau_fylke")
    dbExecute(db_connexion, "DROP TABLE grunnkart_valid")
    dbExecute(db_connexion, "DROP TABLE grunnkart_intersection")
    
    # Return the spatial intersection
    return(forest_lau)
    
  } else if(fylke %in% c(18)){
    
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
    forest_lau <- ddbs_intersection(db_connexion, x = "grunnkart_intersection", y = "lau_fylke", crs = "EPSG:3035")
    
    # Drop the grunnkart register tables
    dbExecute(db_connexion, "DROP VIEW grunnkart")
    dbExecute(db_connexion, "DROP VIEW lau_fylke")
    dbExecute(db_connexion, "DROP TABLE grunnkart_valid")
    dbExecute(db_connexion, "DROP TABLE grunnkart_intersection")
    
    # Return the spatial intersection
    return(forest_lau)
    
  } else if(fylke == 56){
    
    #   # Register Grunnkart for the specific fylke
    #   idx <- which(fylke_index_path_list == fylke)
    #   ar_path <- path_list[idx]
    #   ar_layer <- "arealregnskap"
    #   dbExecute(db_connexion, glue("
    # CREATE VIEW grunnkart AS 
    # SELECT *,
    # FROM ST_Read('{ar_path}', layer='{ar_layer}')"))
    
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
    forest_lau <- ddbs_intersection(db_connexion, x = "grunnkart_intersection", y = "lau_fylke", crs = "EPSG:3035")
    
    # Drop the grunnkart register tables
    dbExecute(db_connexion, "DROP VIEW lau_fylke")
    dbExecute(db_connexion, "DROP TABLE grunnkart_valid")
    dbExecute(db_connexion, "DROP TABLE grunnkart_intersection")
    
    return(forest_lau)
    
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
    forest_lau <- ddbs_intersection(db_connexion, x = "grunnkart_intersection", y = "lau_fylke", crs = "EPSG:3035")
    
    # Drop the grunnkart register tables
    dbExecute(db_connexion, "DROP VIEW lau_fylke")
    dbExecute(db_connexion, "DROP TABLE grunnkart_valid")
    dbExecute(db_connexion, "DROP TABLE grunnkart_intersection")
    
    return(forest_lau)
    
  } else if(fylke == 33){
    
    # Register LAU for specific Fylke
    lau_path <- bd_path
    dbExecute(db_connexion, glue("
  CREATE VIEW lau_fylke AS
  SELECT *,
  FROM ST_Read('{bd_path}')"))
    
    # Spatial intersect to join the variables of both datasets before intersection
    dbExecute(db_connexion, query_valid_33)
    dbExecute(db_connexion, query_intersect)
    dbExecute(db_connexion, query_drop)
    
    # Perform spatial intersection
    forest_lau <- ddbs_intersection(db_connexion, x = "grunnkart_intersection", y = "lau_fylke", crs = "EPSG:3035")
    
    # Drop the grunnkart register tables
    dbExecute(db_connexion, "DROP VIEW lau_fylke")
    dbExecute(db_connexion, "DROP TABLE grunnkart_valid")
    dbExecute(db_connexion, "DROP TABLE grunnkart_intersection")
    
    return(forest_lau)
    
  } else if(fylke == 55){
    
    # Register LAU for specific Fylke
    lau_path <- bd_path
    dbExecute(db_connexion, glue("
  CREATE VIEW lau_fylke AS
  SELECT *,
  FROM ST_Read('{bd_path}')"))
    
    # Spatial intersect to join the variables of both datasets before intersection
    dbExecute(db_connexion, query_valid_55)
    dbExecute(db_connexion, query_intersect)
    dbExecute(db_connexion, query_drop)
    
    # Perform spatial intersection
    forest_lau <- ddbs_intersection(db_connexion, x = "grunnkart_intersection", y = "lau_fylke", crs = "EPSG:3035")
    
    # Drop the grunnkart register tables
    dbExecute(db_connexion, "DROP VIEW lau_fylke")
    dbExecute(db_connexion, "DROP TABLE grunnkart_valid")
    dbExecute(db_connexion, "DROP TABLE grunnkart_intersection")
    
    return(forest_lau)
    
  } else if(fylke == 50){
    
    # Register LAU for specific Fylke
    lau_path <- bd_path
    dbExecute(db_connexion, glue("
  CREATE VIEW lau_fylke AS 
  SELECT *,
  FROM ST_Read('{bd_path}')"))
    
    # Spatial intersect to join the variables of both datasets before intersection
    dbExecute(db_connexion, query_valid_50)
    dbExecute(db_connexion, query_intersect)
    dbExecute(db_connexion, query_drop)
    
    # Perform spatial intersection
    forest_lau <- ddbs_intersection(db_connexion, x = "grunnkart_intersection", y = "lau_fylke", crs = "EPSG:3035")
    
    # Drop the grunnkart register tables
    dbExecute(db_connexion, "DROP VIEW lau_fylke")
    dbExecute(db_connexion, "DROP TABLE grunnkart_valid")
    dbExecute(db_connexion, "DROP TABLE grunnkart_intersection")
    
    return(forest_lau)
    
  } else (return("an error has occured"))
  
}

# Load and prepare the urban area data --------------------------------------------

# Create function to do the spatial intersection
# includes registration of grunnkart and its cleaning
do_intersection_urban <- function(db_connexion, fylke, path_list, fylke_index_path_list, bd_path){
  
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

# Extract Tree Cover Density percenatges in forest and urban areas----------------
## Create function to extract tcd raster cells corresponding to polygons
extract_rast <- function(bd_shp, year){
  
  # Load packages so they are available in environment fo the worker for parallel computing
  library(dplyr)
  library(sf)
  library(terra)
  library(exactextractr)
  
  # Create the Tree Cover Density virtual raster
  path_write <- paste0("/data/P-Prosjekter2/412413_2023_no_egd/git_data/TCD/",
                       as.character(year),"/")
  
  filename_vrt <- list.files(path = path_write, pattern = '\\.tif$', 
                             all.files = TRUE, full.names = TRUE)
  
  filename_vrt_cln <- normalizePath(filename_vrt)
  
  tcd_vrt <- vrt(filename_vrt_cln)
  
  
  # Read in boundary data (forest or urban)
  crs_tcd <- rast(filename_vrt_cln[1]) %>%
    crs()
  boundaries <- bd_shp %>%
    st_transform(., crs = crs_tcd) %>%
    mutate(area_m2 = unclass(st_area(.)))
  
  # Extract raster cells
  tcd_cells_ex <- boundaries %>%
    exact_extract(tcd_vrt,
                  .,
                  fun = c("mean", "stdev"), # kept only for lau_tcd
                  append_cols = TRUE) %>%
    filter(!is.na(mean) == TRUE) %>%  # Cleaning step
    mutate(., year = year)
  return(tcd_cells_ex)
}