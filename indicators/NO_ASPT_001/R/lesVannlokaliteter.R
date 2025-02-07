### lesVannlokaliteter
# Funksjoner til NI_vannf
# ved Hanno Sandvik
# juni 2024
# se https://github.com/NINAnor/NI_vannf
###



lesVannlokaliteter <- function(vannkategori = c("L", "R", "C"),
                               filsti = "data",
                               kolonnenavn = "navnVL.csv") {
  
  # Kolonner som datarammen VL trenger for å fungere:
  nyeKolonner <- c(
    "lokid",
    "lokkod",
    "loknam",
    "sjønr",
    "id",
    "kat",
    "X",
    "Y"
  )
  
  OK <- TRUE
  VL <- list()
  vannkategori <- toupper(vannkategori) %A% c("L", "R", "C")
  if (length(vannkategori) %=% 0) {
    OK <- FALSE
    skriv("Parameteren \"vannkategori\" må være minst én av bokstavene \"L\", ",
          "\"R\" og \"C\"!", pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
  }
  if (OK) {
    # Innlesing av "tolkningstabellen": Hvilke kolonner i vannlokalitetstabellen
    # svarer til hvilke kolonner i VL
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
  if (OK) {
    # Innlesing av vannlokalitetsfilene som har blitt lasta ned fra vannmiljø
    VL <- list()
    for (i in vannkategori) {
      VL[[i]] <- try(as.data.frame(read_xlsx(filsti %+% "VL-" %+% i %+% ".xlsx", 
                                             col_types = "text")))
      if (inherits(VL[[i]], "try-error")) {
        OK <- FALSE
        skriv("Dette skjedde en feil under innlesing av fila \"", filsti,
              "VL-", i, ".xlsx", ". Sjekk om fila fins, og at det er oppgitt ",
              "korrekt navn på den.",
              pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
      }
      if (OK) {
        if (colnames(VL[[i]]) %=% navnVL$VL) {
          colnames(VL[[i]]) <- navnVL$nytt
          if (all(nyeKolonner %in% navnVL$nytt)) {
            VL[[i]] <- VL[[i]][, nyeKolonner]
          } else {
            OK <- FALSE
            skriv("Kolonnenavnene i \"", kolonnenavn, "\" er ikke som forventa!",
                  pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
          }
        } else {
          OK <- FALSE
          skriv("Kolonnenavnene i den innleste fila \"VL-" %+% i %+%
                  ".csv\" er ikke som forventa!",
                pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
        }
      }
    }
    if (OK) {
      VL. <- VL[[1]]
      if (length(vannkategori) > 1) {
        for (k in 2:length(vannkategori)) {
          VL. <- rbind(VL., VL[[k]])
        }
      }
      VL <- VL.
    }
  }
  if (OK) {
    VL$lokid <- as.numeric(VL$lokid)
    VL$sjønr <- as.numeric(VL$sjønr)
    VL$X     <- as.numeric(VL$X)
    VL$Y     <- as.numeric(VL$Y)
  }
  if (OK) {
    skriv("Innlesing av ", nrow(VL), " vannlokaliteter var vellykka.", 
          linjer.over = 1)
  }
  return(VL)
}
