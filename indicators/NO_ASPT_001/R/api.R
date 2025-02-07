library(httr)
library(jsonlite)

# Lenke: https://vannmiljowebapi.miljodirektoratet.no/swagger/ui/index#/Public

# Define the base URL for the API
  base_url <- "https://vannmiljowebapi.miljodirektoratet.no/api/Public"

# Example endpoint (change this to the specific endpoint you want to call by browsing 
# the link https://vannmiljowebapi.miljodirektoratet.no/swagger/ui/index#/Public)
endpoint <- "/GetParameterList" # liste over vannforskriftsparameterne
endpoint <- "/GetActivityList"  # liste over overvåkingsaktiviteter
endpoint <- "/GetUnitList"      # liste over måleenheter
# endpoint <- "/GetParameterInfo"
# endpoint <- "/GetParameters"
endpoint <- "/GetQualityTypeElementInfo"

# endpoint <- "/GetRegistrations"
# endpoint <- "/GetWaterLocations"

# Full URL
url <- paste0(base_url, endpoint)

# Make the GET request
response <- GET(url)

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the JSON content
  content_data <- content(response, "text")
  json_data <- fromJSON(content_data, flatten = TRUE)
  df <- as.data.frame(json_data)
  #
  # Display data
  # print(json_data)
} else {
  cat("Failed to retrieve data. Status code:", status_code(response), "\n")
}

str(df)
head(json_data)

##################################################


headers = c("Content-Type" = "application/json; charset=UTF-8",
            "vannmiljoWebAPIKey" = "4!_55ddgfde905+_!24!;vv")
data = '{"ParameterIDFilter":["ASPT"]}' #, "MaxReturnCount":12}'
#data = '{"WaterLocationIDFilter":[301], "MaxReturnCount":12}'
response <- POST(url, add_headers(.headers=headers), body = data)
# 

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the JSON content
  content_data <- content(response, "text")
  json_data <- fromJSON(content_data, flatten = TRUE)
  df <- as.data.frame(json_data)
  #
  # Display data
  # print(json_data)
} else {
  cat("Failed to retrieve data. Status code:", status_code(response), "\n")
}

str(df)
head(json_data)


readWFD <- function(parameter, years) {
  base_url <- "https://vannmiljowebapi.miljodirektoratet.no/api/Public"
  endpoint <- "/GetRegistrations"
  url <- paste0(base_url, endpoint)
  headers = c("Content-Type" = "application/json; charset=UTF-8",
              "vannmiljoWebAPIKey" = "4!_55ddgfde905+_!24!;vv")
  body <- paste0('{"ParameterIDFilter":["', parameter, '"]}')
  response <- POST(url, add_headers(.headers=headers), body = body)
  if (status_code(response) != 200) stop("Failed to retrieve data.")
  content_data <- content(response, "text")
  json_data <- fromJSON(content_data, flatten = TRUE)
  dataset <- as.data.frame(json_data)
  subset <- 
    as.Date(dataset$Result.SamplingTime) >= as.Date(paste0(min(years), "-01-01")) &
    as.Date(dataset$Result.SamplingTime) <= as.Date(paste0(max(years), "-12-31"))
  return(dataset[subset, ])
}

aspt.data <- readWFD("ASPT", 2021:2023)

# må lese inn 14832 datapunkt, for så å forkaste 10601 av dem

