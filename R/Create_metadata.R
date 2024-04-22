# find the metadata files & use them to populate the table

library(readxl)
library(tidyverse)

create_metadata_table <- function(path) {
  # Get list of all Excel files
  excel_files <- list.files(path, pattern = "\\.xlsx$", recursive = TRUE, full.names = TRUE)
  # Get list of all HTML files
  html_files <- list.files(path, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
  
  # Read each Excel file and assign corresponding HTML file
  metadata <- map2(excel_files, html_files, ~{
    df <- read_excel(.x)
    df$HTML_File <- .y
    df
  })
  
  # Define and call the function to process and bind dataframes
  process_and_bind_dfs <- function(df_list) {
    # Transform each dataframe
    df_wide_list <- map(df_list, ~{
      # Extract the ID column name (assumes the name of the second column as the ID)
      id_name <- names(.x)[2]
      # Extract the URL from any row (assuming it's the same across all rows of a single dataframe)
      url <- unique(.x$HTML_File)
      
      # Pivot the dataframe to wide format, using names from 'indicatorID'
      wide_df <- pivot_wider(.x, names_from = indicatorID, values_from = !!sym(id_name), values_fill = NA)
      
      # Add the URL and ID as separate columns
      wide_df %>%
        mutate(URL = url, ID = id_name)
    })
    
    # Combine all transformed dataframes into one
    bind_rows(df_wide_list)
  }
  
  # Return the combined data
  process_and_bind_dfs(metadata)
}

create_data<-function(){
  data<-create_metadata_table("indicators")
  
  data<-data |> 
    dplyr::mutate(HTML_File=paste0(here::here(HTML_File)))
App_data<-write_rds(data, here::here("data/App_data.RDS"))

}

App_data<-create_data()


# # Example usage
# combined_metadata <- create_metadata_table("indicators")
# print(combined_metadata)
# 
# data<-create_metadata_table(path="indicators")
#  data

