# find the metadata files & use them to populate the table

#' create the metadata table
#'
#' @param path path to the indicators folder
#'
#' @return creates a metadata dataframe
#' @export

create_metadata_table <- function(path) {
   # Get list of all Excel files
  excel_files <- list.files(here::here(path), pattern = "metadata.xlsx$", recursive = TRUE, full.names = TRUE)
  excel_files <- excel_files[!grepl("template", excel_files)]
  
  html_files <- list.files(here::here(path), pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
  
  html_files <- html_files[
    !grepl("Sandbox", html_files)&
      grepl("/R/", html_files) & 
      #!grepl("/data/", html_files)&
      !grepl("/template/R/", html_files) &
      !grepl("version", html_files, ignore.case = TRUE)
        ]
  
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
  # Process each dataframe in the list
  df_wide_list <- map(df_list, ~{
    # Pivot the dataframe so that Variable values become column names
    wide_df <- .x |> 
      # Filter only relevant variables
      filter(Variable %in% c("indicatorID", "indicatorName", "country", "continent", "ECT", "yearAdded", "yearLastUpdate")) |> 
      select(Variable, Value, HTML_File) |> 
      pivot_wider(names_from = Variable, values_from = Value, values_fn = list(Value = ~ first(na.omit(.)))) |> 
      # Group by HTML_File and fill missing values within each group
      group_by(HTML_File) %>%
      fill(everything(), .direction = "downup")  |> 
      ungroup() |> 
      # Select only the columns you care about
      select(HTML_File, indicatorName, country, continent, ECT, yearAdded, yearLastUpdate, indicatorID)  |> 
      # Remove duplicates if any
      distinct() |> 
      rename(ID=indicatorID)
    
    wide_df
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
    App_data<-assign("App_data", App_data)

}

library(readxl)
library(tidyverse)
combined_metadata <- create_metadata_table("indicators")
combined_metadata<-process_and_bind_dfs(combined_metadata)
create_data(combined_metadata = combined_metadata)
