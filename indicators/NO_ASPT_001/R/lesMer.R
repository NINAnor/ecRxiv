### lesMer
# Funksjoner til WFD2ECA
# ved Hanno Sandvik
# februar 2025
# se https://github.com/NINAnor/NI_vannf #¤?
###



lesMer <- function() {
  
  # Funksjonen leser inn lister over:
  # (1) vannforskriftsparametere (fra en excel-fil)
  # (2) overvåkingsaktiviteter (fra en excel-fil)
  # (3) måleenheter for vannforskriftsparametere (direkte fra vannmiljø-databasen)
  # og gjør disse tilgjengelig som tabeller (data-rammer) som heter hhv.
  # "Parametere", "Aktiviteter" og "Enheter"
  
  OK <-TRUE
  baseURL  <- "https://vannmiljowebapi.miljodirektoratet.no/api/Public"
  ENDpoint <- "/GetUnitList"

  # Parametere
  par <- as.data.frame(read_xlsx("../data/VM-param.xlsx", na = "NA",
                                 col_types = c("text", "text", 
                                               "numeric", "numeric")))
  if (inherits(par, "try-error")) {
    OK <- FALSE
    skriv("Dette skjedde en feil under innlesing av fila \"VM-param.xlsx\".",
          "Sjekk om fila fins, og at den er formatert som excel-regneark.",
          pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
  } else {
    if (exists("Parametere")) {
      skriv("Variabelen \"Parametere\" eksisterte fra før og har blitt erstatta!", 
            pre = "OBS: ", linjer.over = 1)
    }
    Parametere <<- par
  }

  # Aktiviteter
  akt <- as.data.frame(read_xlsx("../data/VM-aktiv.xlsx", na = "NA",
                                 col_types = c("text", "text", "numeric")))
  if (inherits(akt, "try-error")) {
    OK <- FALSE
    skriv("Dette skjedde en feil under innlesing av fila \"VM-aktiv.xlsx\".",
          "Sjekk om fila fins, og at den er formatert som excel-regneark.",
          pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
  } else {
    if (exists("Aktiviteter")) {
      skriv("Variabelen \"Aktiviteter\" eksisterte fra før og har blitt erstatta!", 
            pre = "OBS: ", linjer.over = 1)
    }
    Aktiviteter <<- akt
  }
  
  # Enheter
  respons <- GET(baseURL %+% ENDpoint)
  if (status_code(respons) != 200) {
    OK <- FALSE
    skriv("Det lyktes ikke å hente data om måleenheter fra vannmiljø-databasen. ",
          "Statuskoden var ", status_code(respons), ".",
          pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
  } else {
    JSONdata <- fromJSON(content(respons, "text"), flatten = TRUE)
    enh <- as.data.frame(JSONdata)
    enh <- lapply(enh, as.character)
    if (exists("Enheter")) {
      skriv("Variabelen \"Enheter\" eksisterte fra før og har blitt erstatta!", 
            pre = "OBS: ", linjer.over = 1)
    }
    Enheter <<- enh
  }
  
  if (OK) {
    skriv("Innlesing av parametere, aktiviteter og enheter var vellykka.", 
          linjer.over = 1)
  }
  invisible(OK)
}

