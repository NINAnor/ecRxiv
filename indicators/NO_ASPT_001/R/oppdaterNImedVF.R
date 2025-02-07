### oppdaterNImedVF
# Funksjoner til NI_vannf
# ved Hanno Sandvik
# januar 2024
# se https://github.com/NINAnor/NI_vannf
###



oppdaterNImedVF <- function(indikatorID, nyeData, avrunding = 6) {
  
  # Første trinn er å laste ned de nåværende indikatorverdiene for de ønska årene:
  NIdata <- NIcalc::getIndicatorValues(indikatorID,
                                       years = c("Referanseverdi", 
                                                 dimnames(nyeData[[1]]$aar)))
  
  # Måleenheten for alle data som flyter fra vannforskriften, er nEQR:
  NIdata[[1]]$unitOfMeasurement <- "nEQR"
  
  nyeData <- round(nyeData$kommune, avrunding)
  feil <- c()
  sistnavn <- ""
  taVare <- FALSE
  
  for (i in 1:nrow(NIdata[[1]])) {
    aar <- NIdata[[1]]$yearName[i]
    if (sistnavn == "") {
      sistnavn <- NIdata[[1]]$areaName[i]
    }
    if (NIdata[[1]]$areaName[i] %in% dimnames(nyeData)$kommune) { # kommune kjent?
      if (NIdata[[1]]$yearId[i]) { # er det en årsverdi (ikke referanseverdi)?
        if (as.numeric(aar) > 1900) { # er det et meningsfullt årstall?
          if (any(is.na(nyeData[NIdata[[1]]$areaName[i], aar, -1]))) {
            feil <- c(feil, "Det manglet verdier for " %+% 
                            NIdata[[1]]$areaName[i] %+%
                            " i " %+% aar %+% "!\n")
          } else { # kun kjente dataverdier
            w <- which(nyeData[NIdata[[1]]$areaName[i], aar, ] < 0)
            if (length(w)) {
              # negative verdier settes til null!
              nyeData[NIdata[[1]]$areaName[i], aar, w] <- 0
            }
            # er den spesifikke verdien basert på målinger eller modellering?
            # typ <- if (alle.maalt[NIdata[[1]]$areaName[i], aar]) 2 else 3
            typ <- 3 # foreløpig må det bli slik. Må endres!!! (¤)
            # oppdatering med de simulerte dataene:
            NIdata <- NIcalc::setIndicatorValues(
              NIdata,
              areaId = NIdata[[1]]$areaId[i],
              years = as.numeric(aar),
              distribution = NIcalc::makeDistribution(
                nyeData[NIdata[[1]]$areaName[i], aar, -1]
              ),
              datatype = typ
            )
          }
        } else { # feil årstall
          NIdata <- NIcalc::setIndicatorValues(NIdata,
                                          areaId = NIdata[[1]]$areaId[i],
                                          years = as.numeric(aar),
                                          est = -1, lower = 0, upper = 0,
                                          datatype = 1)
        }
      } else { # referanseverdi
        NIdata <- NIcalc::setIndicatorValues(NIdata,
                                        areaId = NIdata[[1]]$areaId[i],
                                        years = "Referanseverdi",
                                        est = 1, lower = 1, upper = 1,
                                        datatype = 1)
      }
    } else { # ukjent kommune!
      feil <- c(feil, "Arealnavnet \"" %+% NIdata[[1]]$areaName[i] %+% 
                  "\" ble ikke funnet i datafila!\n")
    }
    if (i == nrow(NIdata[[1]]) || NIdata[[1]]$areaName[i + 1] != sistnavn) {
      cat(floor(i * 100 / nrow(NIdata[[1]])) %+% 
          " % er unnagjort (sist: " %+% sistnavn %+% ").\r")
      sistnavn <- NIdata[[1]]$areaName[i+1]
    }
  } # i
  cat("\n")
  if (length(feil)) {
    skriv("Følgende feilmelding", ifelse(length(feil) > 1, "er", ""), 
          "ble samla opp underveis:", linjer.over = 1)
    for (i in 1:length(feil)) {
      skriv(feil[i], pre = "* ")
    }
    cat("\n")
  }
  if (!length(feil)) {
    skriv("Det hele skjedde uten feilmeldinger.", linjer.under = 1)
    # i så fall er resultatet klart til å skrives til NI-databasen:
    svar <- try(askYesNo("Er du sikker på at du vil overskrive databasen?", TRUE, 
                         c("ja ", " nei ", " kanskje en annen gang")),
                silent = TRUE)
    cat("\n")
    if (svar %=% TRUE) {
      NIcalc::writeIndicatorValues(NIdata)
      skriv("Med mindre \"NIcalc\" har generert en feilmelding rett over denne ",
            "setninga, skal dataimporten ha fungert etter planen.", 
            linjer.under = 1)
    } else {
      taVare <- TRUE
      skriv("OBS: Dataene ble ikke skrevet over.")
    }
  }
  if (taVare) {
    invisible(NIdata)
  }
}



