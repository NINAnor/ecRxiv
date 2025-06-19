### lesVannlokaliteter
# Funksjoner til WFD2ECA
# ved Hanno Sandvik
# februar 2025
# se https://github.com/NINAnor/NI_vannf #¤
###



lesVannlokaliteter <- function(vannkategori = c("L", "R", "C"),
                               filsti = "../data",
                               kolonnenavn = "navnVL.csv") {
  
  # Funksjonen leser inn vannlokaliteter fra vannmiljø-databasen
  
  # Kolonner som datarammen VL trenger for å fungere:
  nyeKolonner <- c(
    "lokid",
    "lokkod",
    "loknam",
    #"sjønr", # mangler i nye eksporter
    "id",
    "kat",
    "X",
    "Y"
  )
  
  OK <- TRUE
  VL <- list()
  baseURL  <- "https://vannmiljowebapi.miljodirektoratet.no/api/Public"
  ENDpoint <- "/GetWaterLocations"
  APIkey   <- "4!_55ddgfde905+_!24!;vv"
  vannkategori <- toupper(vannkategori) %A% c("L", "R", "C")
  if (length(vannkategori) %=% 0) {
    OK <- FALSE
    skriv("Parameteren \"vannkategori\" må være minst én av bokstavene \"L\", ",
          "\"R\" og \"C\"!", pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
  }
  
  # Innlesing av "tolkningstabellen": Hvilke kolonner i vannlokalitetstabellen
  # svarer til hvilke kolonner i VL
  if (OK) {
    if (nchar(filsti)) {
      if (substr(filsti, nchar(filsti), nchar(filsti)) %!=% "/" &
          substr(filsti, nchar(filsti), nchar(filsti)) %!=% "\\") {
        filsti <- filsti %+% "/"
      }
    }
    navnVL <- try(read.table(filsti %+% kolonnenavn, 
                             header = TRUE, sep = ";", quote = "", 
                             na.strings = "", strip.white = TRUE, comment.char = "", 
                             stringsAsFactors = FALSE, fileEncoding="latin1"))
    if (inherits(navnVL, "try-error")) {
      OK <- FALSE
      skriv("Dette skjedde en feil under innlesing av fila \"", filsti %+%
              kolonnenavn, ". Sjekk om fila fins, at det er oppgitt korrekt ",
            "navn på den, og at den er formatert som semikolondelt tabell.",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    }
  }
  
  # Innlesing av vannlokalitetene fra vannmiljø
  if (OK) {
    URL <- baseURL %+% ENDpoint
    headers = c("Content-Type" = "application/json; charset=UTF-8",
                "vannmiljoWebAPIKey" = APIkey)
    body <- 
      '{"BoundingBox":{"xmin":-120000,"ymin":6000000,"xmax":1200000,"ymax":8000000}}'
    respons <- POST(URL, add_headers(.headers = headers), body = body)
    if (status_code(respons) != 200) {
      OK <- FALSE
      skriv("Det lyktes ikke å hente data fra vannmiljø-databasen. ",
            "Statuskoden var ", status_code(respons), ".",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    } else {
      JSONdata <- fromJSON(content(respons, "text"), flatten = TRUE)
      VL   <- as.data.frame(JSONdata)
      VL[] <- lapply(VL, as.character)
      VL   <- VL[VL$Result.WaterCategory %in% vannkategori, ]
      if (nrow(VL) < 1) {
        OK <- FALSE
        skriv("Datasettet var tomt, uvisst av hvilken grunn.",
              pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
      }
    }
  }
  
  # Så "oversettes" kolonnenavnene
  if (OK) {
    if (colnames(VL) %=% navnVL$VL) {
      colnames(VL) <- navnVL$nytt
      if (all(nyeKolonner %in% navnVL$nytt)) {
        VL <- VL[, nyeKolonner]
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
    VL$lokid <- as.numeric(VL$lokid)
    #VL$sjønr <- as.numeric(VL$sjønr)
    VL$X     <- as.numeric(VL$X)
    VL$Y     <- as.numeric(VL$Y)
  }
  if (OK) {
    skriv("Innlesing av ", nrow(VL), " vannlokaliteter var vellykka.", 
          linjer.over = 1)
  }
  return(VL)
}
