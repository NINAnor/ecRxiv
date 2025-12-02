# Global
source("Create_metadata.R")
App_data<-combined_metadata
App_data<-App_data |> 
  filter(hide=="FALSE") 
App_data <- App_data %>%
  transmute(
    indicator_id,
    title,
    indicator_name,
    ECT = str_to_upper(ect),                 # Capitalise to ECT
    continent,
    country,
    realm,
    biome,
    ecosystem,                               # Assuming this is the correct ecosystem column
    authors = author_list,                   # Renamed
    year_added,
    year_last_update,
    status,
    version,
    version_comment,
    normalised,
    data_availability,
    code_reproducibility,
    open_science_badge,
    html_file_rel, url,html_file_abs, file
  ) |> 
  filter(!indicator_id=="R")

  
