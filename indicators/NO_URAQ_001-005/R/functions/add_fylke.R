#' Add fylke
#'
#' This function adds the fylke number and name based on the municipalities 
#' numbers or the fylke numbers.

#' @param data the dataframe to which the fylke number and / or name should 
#' be adted.
#'
#' @param kommune_nummer is the name of the column containing the municipality numbers. 
#' Should be a character. Default is NULL.
#' 
#'  @param fylke_nummer is the name of the column containing the fylke number. 
#'  Should be a character. Default is NULL.
#' 
#' @return a vector with the fylke number and the fylke name
#' 
#' @export
#'
#' @examples

add_fylke <- function(data, kommune_nummer = NULL, fylke_nummer = NULL){
  
  
  # Create a fylke name and number dataset
  fylke_nb <- c("03", "11", "15", "18", "31", "32", "33", "34", "39", "40", "42", "46", "50", "55", "56")
  fylke_nm <- c("Oslo", "Rogaland", "Møre og Romsdal", "Nordland - Nordlánnda", "Østfold", "Akershus",
                "Buskerud", "Innlandet", "Vestfold", "Telemark", "Agder", "Vestland", "Trøndelag - Trööndelage",
                "Troms – Romsa – Tromssa", "Finnmark – Finnmárku – Finmarkku")
  
  fylke_df <- data.frame(fylke_number = fylke_nb,
                         fylke_name = fylke_nm)
  
  
  if(is_null(kommune_nummer) == TRUE){
    
    fylke_num <- data[[fylke_nummer]] 
    
    # Join
    fylke_name <- left_join(as.data.frame(fylke_num), fylke_df, join_by(fylke_num == "fylke_number")) %>%
                    select(!fylke_num)
    
    # Return the column with the name
    return(fylke_name[["fylke_name"]])
    
  }else{
    
    kom_num <- data[[kommune_nummer]]
    
    # Fylke number
    fylke_nb <- substr(as.character(kom_num), 1, 2)
    
    # Join with the fylke name and number
    fylke_name <- left_join(as.data.frame(fylke_nb), fylke_df, join_by(fylke_nb == "fylke_number")) %>%
                    select(!fylke_nb)
    
    return(list(fylke_nb, fylke_name[["fylke_name"]]))
  }
  

}
