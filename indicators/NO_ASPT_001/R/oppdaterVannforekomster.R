### oppdaterVannforekomster
# Funksjoner til NI_vannf
# ved Hanno Sandvik
# juni 2024
# se https://github.com/NINAnor/NI_vannf
###



oppdaterVannforekomster <- function(V, nve,
                                    slingringsmonn = 0.1) {
  
  OBS <- FALSE
  
  # Fyll på V med data fra nve:
  Atot <- Anor <- numeric(nrow(V))
  for (i in which(V$kat == "L")) {
    ID <- as.numeric(unlist(strsplit(V$id[i], "-"))[2])
    if (is.na(ID)) cat(i, V$id[i], "\n")
    w <- which(nve$lnr == ID)
    if (length(w)) {
      w <- w[1]
      V$hoh[i]    <- nve$hoh[w]
      V$tilsig[i] <- nve$tilsig[w]
      #V$areal[i] <- nve$areal[w]
      #V$artot[i] <- nve$areal[w]
      #V$lat[i]   <- nve$lat[w]
      #V$long[i]  <- nve$lon[w]
      Atot[i]     <- nve$areal[w]
      Anor[i]     <- nve$arealn[w]
    }
  }
  
  # Sjekk totalarealet for innsjøer ved grensen
  w <- which(Atot > Anor & Atot > V$areal * (1 + slingringsmonn))
  if (length(w)) {
    V$artot[w] <- Atot[w]
    nyttAreal  <- sapply(floor(lg(Atot[w] * 200)), max, 1)
    endra      <- length(which(nyttAreal > V$stø[w]))
    V$stø[w]   <- nyttAreal
    skriv("Totalarealet har blitt tilføyd for ", length(w), " innsjøer som har ",
          "en arealandel utenfor Norge. For ", endra,  " av disse medførte det ",
          "en oppjustering av størrelsesklassen.", pre = "OBS: ", linjer.over = 1)
    OBS <- TRUE
  }
  
  # Sjekk av høydesone mot faktiske høyder over havet
  forLav <- forHoy <- 0
  w <- which(V$kat == "L" & is.na(V$hoh) & V$son == "L")
  if (length(w)) V$hoh[w] <- 50
  w <- which(V$kat == "L" & is.na(V$hoh) & V$son == "M" & 
               V$reg %in% c("E","S","W","M"))
  if (length(w)) V$hoh[w] <- 400
  w <- which(V$kat == "L" & is.na(V$hoh) & V$son == "M" & 
               V$reg %in% c("N", "F"))
  if (length(w)) V$hoh[w] <- 50
  w <- which(V$kat == "L" & is.na(V$hoh) & V$son == "H" & 
               V$reg %in% c("E","S","W"))
  if (length(w)) V$hoh[w] <- 800
  w <- which(V$kat == "L" & is.na(V$hoh) & V$son == "H" & 
               V$reg == "M")
  if (length(w)) V$hoh[w] <- 600
  w <- which(V$kat == "L" & is.na(V$hoh) & V$son == "H" & 
               V$reg %in% c("N", "F"))
  if (length(w)) V$hoh[w] <- 200
  w <- which(V$kat == "L" & V$son == "L" & V$hoh > 800 * (1 + slingringsmonn))
  forLav <- forLav + length(w)
  if (length(w)) V$son[w] <- "H"
  w <- which(V$kat == "L" & V$son == "L" & V$hoh > 200 * (1 + slingringsmonn))
  forLav <- forLav + length(w)
  if (length(w)) V$son[w] <- "M"
  w <- which(V$kat == "L" & V$son == "M" & V$hoh > 500 * (1 + slingringsmonn) &
               V$reg %in% c("N", "F"))
  forLav <- forLav + length(w)
  if (length(w)) V$son[w] <- "H"
  w <- which(V$kat == "L" & V$son == "M" & V$hoh < 200 * (1 - slingringsmonn) &
               V$reg %in% c("E", "S", "W", "M"))
  forHoy <- forHoy <- length(w)
  if (length(w)) V$son[w] <- "L"
  w <- which(V$kat == "L" & V$son == "H" & V$hoh < 200 * (1 - slingringsmonn) &
               V$reg %in% c("E", "S", "W", "M"))
  forHoy <- forHoy <- length(w)
  if (length(w)) V$son[w] <- "L"
  w <- which(V$kat == "L" & V$son == "H" & V$hoh < 200 * (1 - slingringsmonn) &
               V$reg %in% c("N", "F"))
  forHoy <- forHoy <- length(w)
  if (length(w)) V$son[w] <- "M"
  w <- which(V$kat == "L" & V$son == "H" & V$hoh < 400 * (1 - slingringsmonn) &
               V$reg %in% c("E", "S", "W", "M"))
  forHoy <- forHoy <- length(w)
  if (length(w)) V$son[w] <- "M"
  if (forLav) {
    skriv("For " %+% forLav %+% " innsjøer ble høydesonen " %+%
            "justert opp basert på deres faktiske høyde over havet.",
          pre = "OBS: ", linjer.over = 1)
    OBS <- TRUE
  }
  if (forHoy) {
    skriv("For " %+% forHoy %+% " innsjøer ble høydesonen " %+%
            "justert ned basert på deres faktiske høyde over havet.",
          pre = "OBS: ", linjer.over = 1)
    OBS <- TRUE
  }
  skriv("Oppdatering av vannforekomster var vellykka.", ifelse(OBS, 
        " (Men legg merke til beskjedene over!)", ""), linjer.over = 1)
  return(V)
}
