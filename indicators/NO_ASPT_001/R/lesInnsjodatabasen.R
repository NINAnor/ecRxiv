### lesInnsjodatabasen
# Funksjoner til WFD2ECA
# ved Hanno Sandvik
# januar 2025
###



lesInnsjodatabasen <- function(filnavn = "Innsjo_Innsjo.dbf",
                               filsti = "../data",
                               kolonnenavn = "navnNVEl.csv",
                               CACHE = NULL) {
  
  # Kolonner som datarammen "nve" trenger for å fungere:
  nyeKolonner <- c( 
    "lnr",
    "nam",
    "reg",
    "mnr",
    "hoh",
    "areal",
    "arealn",
    "tilsig",
    "vassdrag",
    "område",
    "lat",
    "lon"
  )
  
  OK  <- TRUE
  OBS <- FALSE
  nve <- nveL <- NULL
  
  if (nchar(filsti)) {
    if (substr(filsti, nchar(filsti), nchar(filsti)) %!=% "/" &
        substr(filsti, nchar(filsti), nchar(filsti)) %!=% "\\") {
      filsti <- filsti %+% "/"
    }
  }
  
  # Innlesing av "tolkningstabellen": Hvilke kolonner i NVEs tabell
  # svarer til hvilke kolonner i "nve"
  navnNVE <- try(read.table(filsti %+% kolonnenavn, 
                            header = TRUE, sep = ";", quote = "", 
                            na.strings = "", strip.white = TRUE, comment.char = "", 
                            stringsAsFactors = FALSE, fileEncoding="latin1"))
  if (inherits(navnNVE, "try-error")) {
    OK <- FALSE
    skriv("Dette skjedde en feil under innlesing av fila \"", filsti %+%
          kolonnenavn, ". Sjekk om fila fins, at det er oppgitt korrekt ",
          "navn på den, og at den er formatert som semikolondelt tabell.",
          pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
  }
  if (!is.null(CACHE)) {
    if (file.exists(filsti %+% CACHE)) {
      load(filsti %+% CACHE)
      if (!exists("nveL")) {
        OK <- FALSE
        skriv("Filen \"", filsti, CACHE, "\" inneholdt ikke variabelen \"nveL\".",
              pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
      }
    } else {
      OK <- FALSE
      skriv("Filen \"", filsti, CACHE, "\" ble ikke funnet.",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    }
  }
  if (OK & is.null(CACHE)) {
    # Innlesing av dbf-delen av NVEs datasett
    nve <- try(read.dbf(filsti %+% filnavn, T))
    if (inherits(nve, "try-error")) {
      OK <- FALSE
      skriv("Dette skjedde en feil under innlesing av fila \"", filsti %+%
            filnavn, ". Sjekk om fila fins, at det er oppgitt korrekt ",
            "navn på den, og at den er formatert som semikolondelt tabell.",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    }
  }
  if (OK & is.null(CACHE)) {
    for (i in 1:ncol(nve)) {
      if (is.character(nve[, i])) {
        Encoding(nve[, i]) <- "latin1"
      }
    }
    
    # "Oversettelse" av kolonnenavn
    w <- which(is.na(navnNVE$NVE))
    if (colnames(nve) %=% navnNVE$NVE[-w]) {
      colnames(nve) <- navnNVE$nytt[-w]
    } else {
      OK <- FALSE
      skriv("Kolonnenavnene i den innleste fila \"", filsti %+% filnavn,
            "\" er ikke som forventa!", 
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    }
  }
  if (OK & is.null(CACHE)) {
    if (all(nyeKolonner %in% navnNVE$nytt)) {
      nveL <- data.frame(lnr = nve$lnr)
      for (j in 2:length(nyeKolonner)) {
        if (nyeKolonner[j] %in% colnames(nve)) {
          nveL[, nyeKolonner[j]] <- nve[, nyeKolonner[j]]
        } else {
          nveL[, nyeKolonner[j]] <- NA
        }
      }
    } else {
      OK <- FALSE
      skriv("Kolonnenavnene i \"", filsti, kolonnenavn, "\" er ikke som forventa!",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    }
  }
  if (OK) {
    # Kolonnen for sterkt modifiserte vannforekomster gjøres om til logisk variabel
    nveL$reg <- (nveL$reg %inneholder% "ulert")
    if (!any(nveL$reg)) {
      skriv("Det var ikke mulig å identifisere regulerte innsjøer i NVEs ",
            "innsjødatabase, fordi betegnelsen ser ut til å være endra. ",
            "Variabelen er satt til <NA>.", 
            pre = "OBS: ", linjer.over = 1)
      OBS <- TRUE
      nveL$reg <- NA
    }
    
    # Sjekk av kolonner med tallverdier
    for (i in 5:8) {
      w <- which(nveL[, i] < 0)
      if (length(w)) {
        skriv("For " %+% length(w) %+% " innsjøer var " %+%
                c("", "", "", "", "høyden over havet", "arealet", 
                  "det norske arealet", "tilsigsfeltet")[i] %+%
                " angitt å være negativ. Disse ble satt til <NA>.",
              pre = "OBS: ", linjer.over = 1)
        OBS <- TRUE
        nveL[w, i] <- NA
      }
    }
    w <- which(is.na(nveL$areal) & !is.na(nveL$arealn))
    if (length(w)) {
      skriv("For " %+% length(w) %+% " innsjøer var det norske arealet angitt, ",
            "men ikke det totale. For disse ble totalarealet satt til det ",
            "norske arealet.", pre = "OBS: ", linjer.over = 1)
      OBS <- TRUE
      nveL$areal[w] <- nveL$arealn[w]
    }
    w <- which(nveL$areal < nveL$arealn)
    if (length(w)) {
      skriv("For " %+% length(w) %+% " innsjøer var det totale arealet angitt å ",
            "være mindre enn det norske. For disse ble totalarealet satt til det ",
            "norske arealet.", pre = "OBS: ", linjer.over = 1)
      OBS <- TRUE
      nveL$areal[w] <- nveL$arealn[w]
    }
    w <- which(nveL$tilsig < nveL$arealn)
    if (length(w)) {
      skriv("For " %+% length(w) %+% " innsjøer var deres tilsigsfelt angitt å ",
            "være mindre enn deres areal. For disse ble tilsigsfeltet satt til ",
            "arealet.", pre = "OBS: ", linjer.over = 1)
      OBS <- TRUE
      nveL$tilsig[w] <- nveL$areal[w]
    }
  }
  if (OK) {
    skriv("Innlesing av innsjødatabasen var vellykka.", ifelse(OBS, 
          " (Men legg merke til beskjedene over!)", ""), linjer.over = 1)
  }
  return(nveL)
}




