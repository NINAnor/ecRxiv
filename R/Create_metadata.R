# find the metadata files & use them to populate the table

#' create the metadata table
#'
#' @param path path to the indicators folder
#'
#' @return creates a metadata dataframe
#' @export

create_metadata_table <- function(path) {
   # Get list of all Excel files
  excel_files <- list.files(here::here(path), pattern = "\\.xlsx$", recursive = TRUE, full.names = TRUE)
  # Get list of all HTML files
  html_files <- list.files(here::here(path), pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
  # Read each Excel file and assign corresponding HTML file
  metadata <- map2(excel_files, html_files, ~{
    df <- read_excel(.x)
    df$HTML_File <- .y
    df
  })
}
  
#' Function to process and bind dataframes
#'
#' @param df_list list of dataframes 
#'
#' @return combined metadata dataframe
#' @export

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

#' create the app data
#'
#' @return App data
#' @export
#'
create_data<-function(combined_metadata){
    App_data<-combined_metadata |> 
    dplyr::mutate(HTML_File=combined_metadata$HTML_File)
    App_data<-write_rds(App_data, here::here("data/App_data.RDS"))

}

library(readxl)
library(tidyverse)
combined_metadata <- create_metadata_table("indicators")
combined_metadata<-process_and_bind_dfs(combined_metadata)
create_data(combined_metadata = combined_metadata)
