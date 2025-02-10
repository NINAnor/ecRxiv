### lesVannforekomster
# Funksjoner til WFD2ECA
# ved Hanno Sandvik
# februar 2025
# se https://github.com/NINAnor/NI_vannf #¤
###



lesVannforekomster <- function(vannkategori = c("L", "R", "C"),
                               filsti = "../data",
                               kolonnenavn = "navnVN.csv",
                               turbid = TRUE,
                               NVEnavn = c("SHAPE",
                                           "identifikasjon_lokalId",
                                           "arealKvadratkilometer",
                                           "lengdeKilometer"),
                               slingringsmonn = 0.02,
                               CACHE = NULL) {
  
  # Kolonner som datarammen V trenger for å fungere:
  nyeKolonner <- c( 
    "id",
    "nam",
    "typ",
    "nas",
    "int",
    "kat",
    "reg",
    "son",
    "stø",
    "alk",
    "hum",
    "tur",
    "dyp",
    "kys",
    "sal",
    "tid",
    "eks",
    "mix",
    "opp",
    "str",
    "smvf",
    "øtil",
    "øpot",
    "ktil",
    "ømål",
    "pmål",
    "kmål",
    "vassdrag",
    "område",
    "region",
    "knr",
    "kommune",
    "fylke",
    "hoh",
    "areal",
    "artot",
    "lengd",
    "dybde",
    "tilsig",
    "utmx",
    "utmy",
    "lat",
    "long"
  )
  
  koord <- function(x) {
    x <- unlist(x)
    l <- mean(x[x < 48])
    b <- mean(x[x > 48])
    return(round(c(l, b), 4))
  }
  
  OK <- TRUE
  OBS <- FALSE
  V <- V. <- list()
  
  vannkategori <- toupper(vannkategori) %A% c("L", "R", "C")
  if (length(vannkategori) %=% 0) {
    OK <- FALSE
    skriv("Parameteren \"vannkategori\" må være minst én av bokstavene \"L\", ",
          "\"R\" og \"C\"!", pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
  }
  if (OK) {
    # Innlesing av "tolkningstabellen": 
    # Hvilke kolonner i vann-nett-tabellen svarer til hvilke kolonner i V
    if (nchar(filsti)) {
      if (substr(filsti, nchar(filsti), nchar(filsti)) %!=% "/" &
          substr(filsti, nchar(filsti), nchar(filsti)) %!=% "\\") {
        filsti <- filsti %+% "/"
      }
    }
    navnV <- try(read.table(filsti %+% kolonnenavn, 
                            header = TRUE, sep = ";", quote = "", 
                            na.strings = "", strip.white = TRUE, comment.char = "", 
                            stringsAsFactors = FALSE, fileEncoding="latin1"))
    if (inherits(navnV, "try-error")) {
      OK <- FALSE
      skriv("Dette skjedde en feil under innlesing av fila \"", filsti %+%
              kolonnenavn, ". Sjekk om fila fins, at det er oppgitt korrekt ",
            "navn på den, og at den er formatert som semikolondelt tabell.",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    }
  }
  if (OK) {
    # Innlesing av filene som har blitt lasta ned fra vann-nett
    for (i in vannkategori) {
      if (OK) {
        # leser inn separate tabeller for innsjø- (L), elve- (R) og
        # kystvannforekomster (C)
        test <- try(readLines(filsti %+% "V-" %+% i %+% ".csv", 1,
                              encoding = "latin1"))
        if (inherits(test, "try-error")) {
          OK <- FALSE
          skriv("Dette skjedde en feil under innlesing av fila \"", filsti,
                "V-", i, ".csv", ". Sjekk om fila fins, og at det er oppgitt ",
                "korrekt navn på den.",
                pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
        }
      }
      if (OK) {
        # først testes det om tabellformatet er som forventa, 
        # nemlig som tabulatordelt tabell
        if (length(strsplit(test, "\t")[[1]]) < 20) {
          OK <- FALSE
          skriv("Datafila fra vann-nett forventes å være en tabulator-delt " %+%
                  "tabell, men hadde et annet format.",
                pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
        }
      }
      if (OK) {
        V[[i]] <- try(read.table(filsti %+% "V-" %+% i %+% ".csv",
                                 header = TRUE, sep = "\t", quote = "\"", 
                                 na.strings = "Not applicable", 
                                 strip.white = TRUE, comment.char = "", 
                                 stringsAsFactors = FALSE, fileEncoding="latin1"))
        if (inherits(V[[i]], "try-error")) {
          OK <- FALSE
          skriv("Dette skjedde en feil under innlesing av fila \"", filsti,
                "V-", i, ".csv", ". Sjekk om fila fins, og at det er oppgitt ",
                "korrekt navn på den.",
                pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
        }
      }
      if (OK) {
        # Tabellene fra vann-nett kan inneholde flere rader per vannforekomst
        # (nemlig en per påvirkning per vannforekomst). Her fjernes alle bortsett
        # fra den første.
        fjern <- numeric(0)
        for (j in unique(V[[i]][, 1])) {
          fjern <- c(fjern, which(V[[i]][, 1] == j)[-1]) 
        }
        V[[i]] <- V[[i]][-fjern, ]
        V[[i]] <- V[[i]][order(V[[i]][, 1]), ]
        w <- which(is.na(navnV[, i]))
        if (colnames(V[[i]]) %=% navnV[-w, i]) {
          colnames(V[[i]]) <- navnV$nytt[-w]
        } else {
          OK <- FALSE
          skriv("Kolonnenavnene i den innleste fila \"V-" %+% i %+%
                  ".csv\" er ikke som forventa!", 
                pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
        }
      }
      if (OK) {
        # Så "oversettes" kolonnenavnene
        if (all(nyeKolonner %in% navnV$nytt)) {
          V.[[i]] <- data.frame(id = V[[i]]$id)
          for (j in 2:length(nyeKolonner)) {
            if (nyeKolonner[j] %in% colnames(V[[i]])) {
              V.[[i]][, nyeKolonner[j]] <- V[[i]][, nyeKolonner[j]]
            } else {
              V.[[i]][, nyeKolonner[j]] <- NA
            }
          }
        } else {
          OK <- FALSE
          skriv("Kolonnenavnene i \"navnVN.csv\" er ikke som forventa!",
                pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
        }
      }
    }
    if (OK) {
      # Til slutt kombineres dataene fra de tre vannkategoriene i én felles dataramme
      V <- V.[[1]]
      if (length(vannkategori) > 1) {
        for (k in 2:length(vannkategori)) {
          V <- rbind(V, V.[[k]])
        }
      }
    }
  }
  
  # Innlesing av formfila som har blitt lasta ned fra NVE
  if (!is.null(CACHE)) {
    #if (file.exists(filsti %+% CACHE)) { #¤ må tilpasses til lenke (prøv try)!
      load(file(filsti %+% CACHE))
      if (!exists("vf")) {
        OK <- FALSE
        skriv("Filen \"", filsti, CACHE, "\" inneholdt ikke variabelen \"vf\".",
              pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
      }
    #} else {
    #  OK <- FALSE
    #  skriv("Filen \"", filsti, CACHE, "\" ble ikke funnet.",
    #        pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    #}
  }
  if (OK & is.null(CACHE)) {
    vf <- NULL
    formfil <- filsti %+% "VF.gdb"
    lag <- try(st_layers(formfil)$name)
    if (inherits(lag, "try-error")) {
      OK <- FALSE
      skriv("Dette skjedde en feil under innlesing av filpakka \"", formfil,
            "\". Sjekk om filpakka fins, og at det er oppgitt korrekt navn på den.",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    }
  }
  if (OK & is.null(CACHE)) {
    for (i in tolower(lag)) {
      if (("L" %in% vannkategori & i %inneholder% "innsj") |
          ("R" %in% vannkategori & i %inneholder% "elv")   |
          ("C" %in% vannkategori & i %inneholder% "kyst")) {
        #skriv("Nå bearbeides \"", i, "\".")
        lesinn <- suppressWarnings(try(st_read(formfil, i, quiet = TRUE)))
        if (inherits(lesinn, "try-error")) {
          OK <- FALSE
          skriv("Dette skjedde dessverre en uventa feil under innlesing av ",
                "filpakka \"", formfil, "\".",
                pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
        } else {
          if (sum(NVEnavn %in% names(lesinn)) != 3) {
            OK <- FALSE
            skriv("Kolonnenavna i ", formfil, " var ikke i tråd med ",
                  "forventninga (dvs. med argumentet \"NVEnavn\").",
                  pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
          }
        }
        if (OK) {
          form <- lesinn[[NVEnavn[1]]]
          lesinn <- as.data.frame(lesinn)[, colnames(lesinn) %A% NVEnavn[-1]]
          colnames(lesinn) <- c("id", "str")
          lesinn$lon <- lesinn$lat <- 0
          for (j in 1:nrow(lesinn)) {
            lesinn[j, 4:3] <- koord(form[[j]])
          }
          rm(form)
          vf <- rbind(vf, lesinn)
        }
      }
    }
    if (any(duplicated(vf$id))) {
      vf <- vf[-which(duplicated(vf$id)), ]
    }
    vf$str <- round(as.numeric(vf$str), 2)
    vf$str[vf$str == 0] <- 0.01
  }
  
  # Koding og overfladisk kontroll av vanntypologien
  if (OK) {
    V$kat <- substr(V$typ, 1, 1)
    if ("L" %in% vannkategori) {
      # Estimert dybde likestilles med kjent dybde
      na <- which(substr(V$typ, 8, 8) %in% c("4", "5", "6"))
      if (length(na)) {
        skriv("Noen vannforekomsters dybde ble justert:", 
              pre = "OBS: ", linjer.over = 1)
        OBS <- TRUE
        for (d in 4:6) {
          w <- which(V$kat == "L" & substr(V$typ, 8, 8) == d)
          if (length(w)) {
            V$typ[w] <- substr(V$typ[w], 1, 7) %+% (d - 3)
            skriv(length(w), ifelse(length(w) > 1, " ganger fra ", " gang fra "),
                  d, " til ", (d - 3), pre = "* ")
          }
        }
      }
    }
    if (turbid) {
      txt <- character(0)
      w <- which(V$kat != "C" & 
                 substr(V$typ, 6, 6) == "0" & substr(V$typ, 7, 7) == "2")
      if (length(w)) {
        V$typ[w] <- substr(V$typ[w], 1, 5) %+% 1 %+% 
                    substr(V$typ[w], 7, nchar(V$typ[w]))
        txt <- c(txt, length(w) %+% 
                 " turbide brepåvirka vannforekomster ble satt til \"klar\"")
      }
      w <- which(V$kat != "C" & 
                   substr(V$typ, 6, 6) == "0" & substr(V$typ, 7, 7) == "3")
      if (length(w)) {
        V$typ[w] <- substr(V$typ[w], 1, 5) %+% 2 %+% 
                    substr(V$typ[w], 7, nchar(V$typ[w]))
        txt <- c(txt, length(w) %+% 
                 " turbide leirpåvirka vannforekomster ble satt til \"humøs\"")
      }
      if (length(txt)) {
        skriv("Noen vannforekomsters humøsitet ble justert:", pre = "OBS: ",
              linjer.over = 1)
        OBS <- TRUE
        for (i in txt) {
          skriv(i, pre = "* ")
        }
      }
    }
    for (vkat in vannkategori) {
      w <- which(V$kat == vkat)
      hvilke <- get("Typologi" %+% vkat)
      Vanntyper$reg <- Vanntyper[["reg" %+% vkat]]
      for (i in 1:length(hvilke)) {
        verdi <- substr(V$typ[w], i + 1, i + 1)
        na <- which(!(verdi %in% Vanntyper[[hvilke[i]]]))
        if (length(na)) {
          skriv("Noen vannforekomster har ukjente verdier for " %+%
                  tolower(Typologi[hvilke[i]]) %+% ":",
                pre = "OBS: ", linjer.over = 1)
          OBS <- TRUE
          for (j in 1:length(unique(verdi[na]))) {
            ukjent <- sort(unique(verdi[na]))[j]
            skriv(length(which(verdi[na] == ukjent)), " med \"",
                  ukjent %+% "\" = \"", 
                  V[w[which(verdi == ukjent)], hvilke[i]][1],
                  "\"", pre = "* ")
          }
          skriv("Disse blir satt til <NA>!")
          verdi[na] <- NA
        }
        for (j in unique(verdi)) {
          if (length(unique(V[w[which(verdi == j)], hvilke[i]])) > 1) {
            skriv("Verdien ", j, " av ", tolower(Typologi[hvilke[i]]),
                  " har ulike beskrivelser:", pre = "OBS: ", linjer.over = 1)
            OBS <- TRUE
            for (k in unique(V[w[which(verdi == j)], hvilke[i]])) {
              skriv(k, pre = "* ")
            }
            skriv("Dette blir ignorert!")
          }
        }
        V[w, hvilke[i]] <- verdi
      }
    }
    
    # Kolonnen for sterkt modifiserte vannforekomster gjøres om til logisk variabel
    V$smvf <- (V$smvf == "Sterkt modifisert")
    
    # Sjekk av kolonnene for økologisk og kjemisk tilstand, potensial og mål
    for (i in 1:2) {
      kolonne <- c("øtil", "ømål")[i]
      klasser <- c("Svært god", "God", "Moderat", "Dårlig", "Svært dårlig")
      V[which(V[, kolonne] == "Svært godt"), kolonne] <- "Svært god"
      V[which(V[, kolonne] ==       "Godt"), kolonne] <-       "God"
      na <- which(!(V[, kolonne] %in% klasser))
      if (length(na)) {
        ukjent <- sort(unique(V[na, kolonne]))
        skriv("Noen vannforekomster har ukjente verdier for økologisk " %+% 
                c("tilstand", "miljømål")[i] %+% ":", 
              pre = "OBS: ", linjer.over = 1)
        OBS <- TRUE
        for (j in ukjent) {
          skriv(length(which(V[na, kolonne] == j)) %+%
                  " med \"" %+% j %+% "\"", pre = "* ")
        }
        skriv("Disse blir satt til <NA>!")
      }
      V[na, kolonne] <- NA
    }
    for (i in 1:2) {
      kolonne <- c("øpot", "pmål")[i]
      klasser <- c("Svært godt", "Godt", "Moderat", "Dårlig", "Svært dårlig")
      V[which(V[, kolonne] == "Svært god"), kolonne] <- "Svært godt"
      V[which(V[, kolonne] ==       "God"), kolonne] <-       "Godt"
      na <- which(!(V[, kolonne] %in% klasser))
      if (length(na)) {
        ukjent <- sort(unique(V[na, kolonne]))
        skriv("Noen vannforekomster har ukjente verdier for økologisk " %+% 
                c("potensial", "potensial miljømål")[i] %+% ":",
              pre = "OBS: ", linjer.over = 1)
        OBS <- TRUE
        for (j in ukjent) {
          skriv(length(which(V[na, kolonne] == j)) %+% 
                  " med \"" %+% j %+% "\"", pre = "* ")
        }
        skriv("Disse blir satt til <NA>!")
      }
      V[na, kolonne] <- NA
    }
    for (i in 1:2) {
      kolonne <- c("ktil", "kmål")[i]
      klasser <- c("God", "Dårlig")
      V[which(V[, kolonne] ==       "Godt"), kolonne] <-       "God"
      na <- which(!(V[, kolonne] %in% klasser))
      if (length(na)) {
        ukjent <- sort(unique(V[na, kolonne]))
        skriv("Noen vannforekomster har ukjente verdier for kjemisk " %+% 
                c("tilstand", "miljømål")[i] %+% ":", 
              pre = "OBS: ", linjer.over = 1)
        OBS <- TRUE
        for (j in ukjent) {
          skriv(length(which(V[na, kolonne] == j)) %+%
                  " med \"" %+% j %+% "\"", pre = "* ")
        }
        skriv("Disse blir satt til <NA>!")
      }
      V[na, kolonne] <- NA
    }
  }
  
  # Kobling av vann-nett-data og NVE-data
  if (OK) {
    V  <-     V[order( V$id), ]
    vf <-    vf[order(vf$id), ]
    w <-    V$id[which(V$kat == "L"  &  V$id %in% vf$id)]
    V$areal[V$id %in% w]     <- vf$str[vf$id %in% w]
    w <-    V$id[which(V$kat == "R"  &  V$id %in% vf$id)]
    V$lengd[V$id %in% w]     <- vf$str[vf$id %in% w]
    w <-    V$id[which(V$kat == "C"  &  V$id %in% vf$id)]
    V$areal[V$id %in% w]     <- vf$str[vf$id %in% w]
    V$lat  [V$id %in% vf$id] <- vf$lat[vf$id %in% V$id]
    V$long [V$id %in% vf$id] <- vf$lon[vf$id %in% V$id]
    V$artot <- V$areal
  }
  
  # Sjekk av størrelsesklasser mot faktiske arealer
  forLiten <- forStor <- 0
  w <- which(V$kat == "L" & is.na(V$areal))
  if (length(w)) V$areal[w] <- 16 * 10^(as.numeric(V$stø[w]) - 3)
  vorher <- hinterher <- character(0)
  for (i in 1:3) {
    w <- which(V$kat == "L" & V$stø == i &
               V$areal > (5 * 10^(i - 2) * (1 + slingringsmonn)))
    forLiten <- forLiten + length(w)
    if (length(w)) {
      vorher    <- c(vorher,       V$stø[w])
      V$stø[w]  <- sapply(floor(lg(V$areal[w] * 200)), max, 1)
      hinterher <- c(hinterher,    V$stø[w])
    }
  }
  if (length(vorher)) {
    skriv("Noen vannforekomsters størrelsesklasse ble justert opp:",
          pre = "OBS: ", linjer.over = 1)
    OBS <- TRUE
    endra <- vorher %+% hinterher
    for (i in sort(unique(endra))) {
      skriv(length(which(endra == i)), 
            ifelse(length(which(endra == i)) > 1, " ganger fra ", " gang fra "), 
            substr(i, 1, 1), " til ", substr(i, 2, 2), pre = "* ")
    }
  }
  vorher <- hinterher <- character(0)
  for (i in 4:2) {
    w <- which(V$kat == "L" & V$stø == i &
               V$areal < (5 * 10^(i - 3) * (1 - slingringsmonn)))
    forStor <- forStor + length(w)
    if (length(w)) {
      vorher    <- c(vorher,       V$stø[w])
      V$stø[w]  <- sapply(floor(lg(V$areal[w] * 200)), max, 1)
      hinterher <- c(hinterher,    V$stø[w])
    }
  }
  if (length(vorher)) {
    skriv("Noen vannforekomsters størrelsesklasse ble justert ned:",
          pre = "OBS: ", linjer.over = 1)
    OBS <- TRUE
    endra <- vorher %+% hinterher
    for (i in sort(unique(endra))) {
      skriv(length(which(endra == i)), 
            ifelse(length(which(endra == i)) > 1, " ganger fra ", " gang fra "), 
            substr(i, 1, 1), " til ", substr(i, 2, 2), pre = "* ")
    }
  }
  if (!all(V$kat %in% c("L", "R", "C"))) {
    skriv("Det ble funnet ukjente vannkategorier (\"",
          paste(sort(unique(V$kat %-% c("L", "R", "C"))), collapse = "\", \""),
          "\")!", pre = "OBS: ", linjer.over = 1)
    OBS <- TRUE
  }
  
  if (OK) {
    skriv("Innlesing av ", nrow(V), " vannforekomster var vellykka.", ifelse(OBS, 
          " (Men legg merke til beskjedene over!)", ""), linjer.over = 1)
  }
  return(V)
}
