### sjekkPAR
# Funksjoner til NI_vannf
# ved Hanno Sandvik
# februar 2024
# se https://github.com/NINAnor/NI_vannf
###



sjekkAIP <- 
sjekkPIT <- 
  function(maaling, slingring, fraMaaned, tilMaaned) {
  # Funksjonen fjerner målinger som ikke er tatt mellom juni og oktober
  if (missing(fraMaaned)) fraMaaned <-  6  # juni måned
  if (missing(tilMaaned)) tilMaaned <- 10  # oktober måned
  if (missing(slingring)) slingring <- 14  # dager
  DAG <- somDag(maaling$dag, maaling$mnd, maaling$aar)
  fjern <- c(which(DAG < somDag(1, fraMaaned,     maaling$aar) - slingring),
             which(DAG > somDag(1, tilMaaned + 1, maaling$aar) + slingring - 1))
  return(fjern)
}



sjekkASPT <- 
  function(maaling) {
  # Funksjonen fjerner målinger som er tatt i brepåvirka vannforekomster
  fjern <- which(maaling$tur == 2)
  attr(fjern, "ikkeInkluder") <- list(typ = "tur", vrd = 2)
  return(fjern)
}



sjekkHBI2 <- 
  function(maaling) {
  # Funksjonen fjerner data fra vannforekomster der det ikke er foretatt 
  # målinger på både våren (januar til april) og høsten (oktober til desember)
  fjern <- c()
  ID <- maaling$aar %+% maaling$id
  for (i in unique(ID)) {
    w <- which(ID == i)
    m <- maaling[w, ]
    y <- m$aar[1]
    ganger <- 0
    DAG <- somDag(m$dag, m$mnd, y)
    if (any(DAG >= somDag(1, 1, y) & DAG <= somDag(30, 4, y))) {
      ganger <- ganger + 1
    }
    if (any(DAG >= somDag(1, 10, y) & DAG <= somDag(31, 12, y))) {
      ganger <- ganger + 1
    }
    if (ganger < 2) {
      fjern <- c(fjern, w)
    }
  }
  return(fjern)
}
  
  
  
sjekkPPBIOMTOVO <- 
sjekkPPTI <- 
  function(maaling, slingring, fraMaaned, tilMaaned, antallSyd, antallNor) {
  # Funksjonen fjerner data fra vannforekomster der det ikke er foretatt
  # målinger (omtrent) månedlig gjennom hele vekstsesongen
  if (missing(fraMaaned)) fraMaaned <-  5  # mai måned
  if (missing(tilMaaned)) tilMaaned <- 10  # oktober måned
  if (missing(slingring)) slingring <- 14  # dager
  if (missing(antallSyd)) antallSyd <-  4  # målinger
  if (missing(antallNor)) antallNor <-  3  # målinger
  DAG <- somDag(maaling$dag, maaling$mnd, maaling$aar)
  fjern <- c(which(DAG < somDag(1, fraMaaned,     maaling$aar) - slingring),
             which(DAG > somDag(1, tilMaaned + 1, maaling$aar) + slingring - 1))
  ID <- maaling$aar %+% maaling$vfo
  for (i in unique(ID)) {
    w <- which(ID == i)
    m <- maaling[w, ]
    y <- m$aar[1]
    DAG <- somDag(m$dag, m$mnd, y)
    ganger <- 0
    if (length(w) >= antallSyd | 
       (length(w) >= antallNor & (m$reg[1] %in% c("N", "F") | 
                                  m$son[1] %in% c("M", "H")))) {
      for (j in fraMaaned:tilMaaned) {
        if (any(DAG >= somDag(1, j,     y) - slingring     &
                DAG <= somDag(1, j + 1, y) + slingring - 1)) {
          ganger <- ganger + 1
        }
      }
    }
    if (!(ganger >= antallSyd |
         (ganger >= antallNor & (m$reg[1] %in% c("N", "F") | 
                                 m$son[1] %in% c("M", "H"))))) {
      fjern <- c(fjern, w)
    }
  }
  return(unique(fjern))
}



sjekkRADDUM1 <- 
  function(maaling) {
  # Funksjonen fjerner målinger som er tatt i humøse vannforekomster
  fjern <- which(maaling$hum %in% 2:3)
  attr(fjern, "ikkeInkluder") <- list(typ = rep("hum", 2), vrd = 2:3)
  return(fjern)
}



sjekkTIANTL <- 
  function(maaling, slingring, fraMaaned, tilMaaned) {
  # Funksjonen fjerner målinger som ikke er tatt mellom juli og september
  if (missing(fraMaaned)) fraMaaned <-  7  # juni måned
  if (missing(tilMaaned)) tilMaaned <-  9  # september måned
  if (missing(slingring)) slingring <- 14  # dager
  DAG <- somDag(maaling$dag, maaling$mnd, maaling$aar)
  fjern <- c(which(DAG < somDag(1, fraMaaned,     maaling$aar) - slingring),
             which(DAG > somDag(1, tilMaaned + 1, maaling$aar) + slingring - 1))
  return(fjern)
}



# Variabel som holder oversikt over de mulige argumentene av "sjekk"-funksjonene
sjekkArgumenter <- 
  c("slingring", "fraMaaned", "tilMaaned", "antallSyd", "antallNor")

