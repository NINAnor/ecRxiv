### lesMaalinger
# Funksjoner til WFD2ECA
# ved Hanno Sandvik
# oktober 2025
# se https://github.com/NINAnor/WFD2ECA
###



lesMaalinger <- function(parameter,
                         filsti = "../data",
                         kolonnenavn = "navnVM.csv",
                         medium = "VF",
                         ...) {
  
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
  kontroll <- list(...)
  baseURL  <- kontroll$baseURL
  ENDpoint <- kontroll$ENDpoint
  ENDunits <- kontroll$ENDunits
  APIkey   <- kontroll$APIkey
  if (is.null(ENDpoint)) ENDpoint <- "/GetRegistrations"
  if (is.null(ENDunits)) ENDunits <- "/GetUnitList"
  if (is.null(APIkey))   APIkey   <- "4!_55ddgfde905+_!24!;vv"
  if (is.null(baseURL))  baseURL  <- 
    "https://vannmiljoapi.miljodirektoratet.no/api/Public"
  
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
  
  # Innlesing av vannmiljø-registreringer
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
  
  # Kontroll av medium
  if (OK) {
    if (!is.null(medium) && !is.na(medium) && !(substr(medium, 1, 3) %=% "all")) {
      w <- which(DATA$medium %in% medium)
      if (length(w) < nrow(DATA)) {
        skriv((nrow(DATA) - length(w)), " målinger har blitt droppa fordi de ",
              "ble foretatt i feil medium (i ", 
              paste(sort(unique(DATA$medium[-w])), collapse = ", "), 
              " istedenfor i ", paste(sort(unique(medium)), collapse = ", "), 
              ").", pre = "OBS: ", linjer.over = 1)
      }
      DATA <- DATA[w, ]
    }
  }
  
  # Innlesing av måleenheter
  if (OK) {
    respons <- GET(baseURL %+% ENDunits)
    if (status_code(respons) != 200) {
      OK <- FALSE
      skriv("Det lyktes ikke å hente data om måleenheter fra vannmiljø-databasen. ",
            "Statuskoden var ", status_code(respons), ".",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    } else {
      JSONdata <- fromJSON(content(respons, "text"), flatten = TRUE)
      enh   <- as.data.frame(JSONdata)
      enh[] <- lapply(enh, as.character)
      #if (exists("Enheter")) skriv("Variabelen \"Enheter\" eksisterte fra før ",
      #    "og har blitt erstatta!", pre = "OBS: ", linjer.over = 1)
      Enheter <<- enh
    }
  }
  
  # Vannmiljø-registreringene sjekkes for potensielle fallgruver
  OBS <- FALSE
  if (OK) {
    if (length(unique(DATA$medium)) > 1) {
      OBS <- TRUE
      skriv("Målingene er tatt i flere ulike medier:", 
            pre = "OBS: ", linjer.over = 1)
      print(table(DATA$medium, useNA = "ifany"))
    }
    if (length(unique(DATA$enhet)) > 1) {
      OBS <- TRUE
      enh <- Enheter
      rownames(enh) <- "u" %+% Enheter$UnitID
      skriv("Målingene har flere ulike enheter:", 
            pre = "OBS: ", linjer.over = 1)
      print(table(enh["u" %+% DATA$enhet, "Name"], useNA = "ifany"))
    }
  }
  
  # Utmating
  if (OK) {
    skriv("Innlesing av ", nrow(DATA), " vannmålinger var vellykka.", ifelse(OBS,
          " (Men legg merke til beskjedene over!)", ""), linjer.over = 1)
    ut <- DATA
  }
  return(ut)
}

