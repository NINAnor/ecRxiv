### lesMaalinger
# Funksjoner til WFD2ECA
# ved Hanno Sandvik
# februar 2025
# se https://github.com/NINAnor/NI_vannf #¤?
###



lesMaalinger <- function(parameter,
                         filsti = "../data",
                         kolonnenavn = "navnVM.csv") {
  
  # Funksjonen leser inn målinger av en oppgitt vannforskriftsparameter
  # fra vannmiljø-databasen
  
  # Kolonner som datarammen V trenger for å fungere:
  nyeKolonner <- c(
    "lokid",
    "aktid",
    "oppdrg",
    "oppdrt",
    "parid",
    "medium",
    "navnid",
    "vitnavn",
    "provmet",
    "analmet",
    "tidpkt",
    "odyp",
    "ndyp",
    "filt",
    "operator",
    "verdi",
    "enhet",
    "provnr",
    "detgr",
    "kvantgr",
    "antverdi"
  )
  
  OK <- TRUE
  ut <- NULL
  baseURL  <- "https://vannmiljowebapi.miljodirektoratet.no/api/Public"
  ENDpoint <- "/GetRegistrations"
  APIkey   <- "4!_55ddgfde905+_!24!;vv"

  # Innlesing av "tolkningstabellen": 
  # Hvilke kolonner i vannmiljø-tabellen svarer til hvilke kolonner i DATA
  if (nchar(filsti)) {
    if (substr(filsti, nchar(filsti), nchar(filsti)) %!=% "/" &
        substr(filsti, nchar(filsti), nchar(filsti)) %!=% "\\") {
      filsti <- filsti %+% "/"
    }
  }
  navnVM <- try(read.table(filsti %+% kolonnenavn, 
                           header = TRUE, sep = ";", quote = "", 
                           na.strings = "", strip.white = TRUE, comment.char = "", 
                           stringsAsFactors = FALSE, fileEncoding="latin1"))
  if (inherits(navnVM, "try-error")) {
    OK <- FALSE
    skriv("Dette skjedde en feil under innlesing av fila \"", filsti %+%
          kolonnenavn, "\". Sjekk om fila fins, at det er oppgitt korrekt ",
          "navn på den, og at den er formatert som semikolondelt tabell.",
          pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
  }
  
  # Innlesing av vannmiljø-registreringer: 
  if (OK) {
    URL <- baseURL %+% ENDpoint
    headers = c("Content-Type" = "application/json; charset=UTF-8",
                "vannmiljoWebAPIKey" = APIkey)
    body <- '{"ParameterIDFilter":["' %+% parameter %+% '"]}'
    respons <- POST(URL, add_headers(.headers = headers), body = body)
    if (status_code(respons) != 200) {
      OK <- FALSE
      skriv("Det lyktes ikke å hente data fra vannmiljø-databasen. ",
            "Statuskoden var ", status_code(respons), ".",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    } else {
      JSONdata <- fromJSON(content(respons, "text"), flatten = TRUE)
      DATA   <- as.data.frame(JSONdata)
      DATA[] <- lapply(DATA, as.character)
    }
  }
  
  # Så "oversettes" kolonnenavnene
  if (OK) {
    if (colnames(DATA) %=% navnVM$vm) {
      colnames(DATA) <- navnVM$nytt
      if (all(nyeKolonner %in% navnVM$nytt)) {
        DATA <- DATA[, nyeKolonner]
        DATA$verdi <- as.numeric(erstatt(DATA$verdi, ",", "."))
        DATA$antverdi <- as.numeric(DATA$antverdi)
        DATA$antverdi[which(  is.na(DATA$antverdi))] <- 1
        DATA$antverdi[which(DATA$antverdi < 1)] <- 1
      } else {
        OK <- FALSE
        skriv("Kolonnenavnene i \"", kolonnenavn, "\" er ikke som forventa!",
              pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
      }
    } else {
      OK <- FALSE
      skriv("Kolonnenavnene i den innleste datafila fra \"vannmiljø\" er ikke " %+%
              "som forventa!", pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    }
  }
  if (OK) {
    skriv("Innlesing av ", nrow(DATA), " vannmålinger var vellykka.", 
          linjer.over = 1)
    ut <- DATA
  }
  return(ut)
}

