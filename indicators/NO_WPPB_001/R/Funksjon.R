### Hjelpefunksjoner
# Hjelpefunksjoner til NI_vannf
# ved Hanno Sandvik
# juni 2024
# se https://github.com/NINAnor/NI_vannf
###



# Tester om argumentene er like - ikke følsom for avrundingsfeil!
"%=%" <- function(arg1, arg2) { 
  attributes(arg1) <- NULL
  attributes(arg2) <- NULL
  return(identical(all.equal(arg1, arg2), TRUE))
}



# Tester om argumentene er ulike - ikke følsom for avrundingsfeil!
"%!=%" <- function(arg1, arg2) !(arg1 %=% arg2)



# Tester om elementene i argumentene er like - ikke følsom for avrundingsfeil 
"%==%" <- function(arg1, arg2) {
  if(is.numeric(arg1) & is.numeric(arg2)) {
    (abs(arg1 - arg2) < 1e-12)
  } else {
    (arg1 == arg2)
  }
}



# Limer sammen tekstvariabler
"%+%" <- function(string1, string2) paste0(string1, string2)



# Fjerner et element fra en vektor
"%-%" <- function(arg1, arg2) arg1[which(!(arg1 %in% na.omit(arg2)))]



# Beregner snittmengden av to vektorer
"%A%" <- function(set1, set2)
  if (is.null(set1)) logical(0) else as.vector(na.omit(set1[set1 %in% set2]))



# Sjekker om en variabel inneholder en angitt søketekst
"%inneholder%" <- function(vector, search) grepl(search, vector, fixed = TRUE)



# Sjekker om verdier ligger innenfor et intervall
"%mellom%" <- function(a, b) (a > b[1]         | a %==% b[1]) &
  (a < b[length(b)] | a %==% b[length(b)])



# Sjekker om verdier ligger utenfor et intervall
"%utafor%" <- function(a, b) !(a > b[1]         | a %==% b[1]) |
  !(a < b[length(b)] | a %==% b[length(b)])



# Sjekker om begynnelsen av ordet er lik
"%begynner%" <- function(a, b) substr(a, 1, nchar(b)) %=% b



# Kombinerer ulike forklaringsvariabler til én tekststreng
"%pluss%" <- function(x, y)
  paste(sort(c(unlist(strsplit(x, "[+]")), unlist(strsplit(y, "[+]")))), 
        collapse = "+")



# Naturlig logaritme skal forkortes som "ln"!
ln <- function(x) log(x)



# Dekadisk logaritme skal forkortes som "lg"!
lg <- function(x) log10(x)



# Pen utmating av tekst
skriv <- function(..., pre = "", linjer.over = 0, linjer.under = 0,
                  Bredde, ut = FALSE, skjerm = TRUE) {
  if (missing(Bredde)) {
    if (exists("breddeUtmating")) {
      Bredde <- breddeUtmating
    } else {
      Bredde <- NULL
    }
  }
  txt <- paste(c(...), collapse = "")
  if (skjerm) {
    cat(rep("\n", linjer.over),
        paste(strwrap(txt,
                      width = if (is.null(Bredde))
                        0.84 * getOption("width") else Bredde,
                      initial = pre,
                      exdent = nchar(pre)),
              collapse = "\n"), 
        rep("\n", linjer.under + 1), 
        sep="")
  }
  if (ut) return(txt)
}



# Trunkering av tall til intervallet [0; 1]
trunker <- function(x) {
  if (is.list(x) & length(x) > 0) {
    for (i in 1:length(x)) {
      x[[i]] <- trunker(x[[i]])
    }
  } else {
    x[] <- ifelse(x[] > 1, 1, x[])
    x[] <- ifelse(x[] < 0, 0, x[])
  }
  return(x)
}



# Erstatte tegn i en tekststreng
erstatt <- function(i, hva, med) gsub(hva, med, i, fixed = TRUE)



# Erstatte desimalpunkt med desimalkomma
komma <- function(x) erstatt(x, ".", ",")



# Sjekker om og sørger for at systemets tegnsett er norsk
UTF8 <- function() {
  ctype <<- Sys.getlocale("LC_CTYPE")
  if (!(tolower(ctype) %inneholder% "utf8" | tolower(ctype) %inneholder% "utf-8")) {
    LC_CTYPE.backup <<- ctype
    Sys.setlocale("LC_CTYPE", "nb_NO.utf8")
    skriv("Det ble oppdaga at systeminnstillingene ikke er forenlige med norske ",
          "tegnsett. Dette har nå blitt gjort om på. Den opprinnelige ",
          "innstillinga har blitt tatt vare på som \"LC_TYPE.backup\" ",
          "[og kan gjenskapes slik: Sys.setlocale(\"LC_CTYPE\", LC_TYPE.backup)].",
          pre = "OBS: ")
  }
  invisible()
}
UTF8()



# Oversette vannmiljøs parameterID til parameternavn
parameterNavn <- function(id) {
  rownames(Parametere) <- Parametere$id
  navn <- Parametere[toupper(id), "navn"]
  navn[which(is.na(navn))] <- "?"
  return(navn)
}



# Sjekker intervallet for tillatte verdier for vannmiljø-parametere
tillatteVerdier <- function(id) {
  rownames(Parametere) <- Parametere$id
  as.numeric(unlist(Parametere[toupper(id), 3:4]))
}



# Oppsummerer en vektor 
oppsummer <- function(x) {
  x <- as.vector(summary(x))
  N <- nchar(trunc(abs(x))) + (x < 0)
  L <- max(floor(log10(max(abs(x)))), 0)
  x <- ("        ") %+% ifelse(x < 0, "-", "") %+% trunc(abs(x)) %+% "," %+%
    substr(round(abs(x), 5 - L) - trunc(abs(x)), 3, 9) %+% "000000"
  x <- substr(x, 7 + N - L, 14 + N - L)
  n <- c(" minimum", "ned. kv.", "  median", "gj.snitt", "øvr. kv.", "maksimum")
  return(c(paste(n, collapse = "  "), paste(x, collapse = "  ")))
}



# Modifisert logit-transformasjon (fra mEQR)
skaler <- function(x, l, h, m = 0.001) {
  x <- (x - l) / (h - l) * (1 - 2 * m) + m
  return(log(x / (1 - x)))
}



# Invers modifisert logit-transformasjon (til mEQR)
reskaler <- function(x, l, h, m = 0.001) {
  x <- exp(x) / (1 + exp(x))
  x <- (x - m) * (h - l) / (1 - 2 * m) + l
  x <- pmin(pmax(x, -0.2), 1.2)
  return(round(x, 9))
}



# Regner om en dato til dagen i året
somDag <- function(d, m, y) as.POSIXlt(y %+% "-" %+% m %+% "-" %+% d)$yday



# Hjelpefunksjon til funksjonen "mEQR"
iNv <- function(mx) optimise(function(i) (i / atan(i) - mx)^2, c(0, 1000))$minimum



# Tabulerer koeffisienter fra et modellsammendrag (x <- summary(lm(...))
tabulerKoeffisienter <- function (x, digits = 3L, dig.tst = 2L, 
                                  cs.ind = 1:2, tst.ind = 3L) {
  # basert på funksjonen "printCoefmat" skrevet av Martin Maechler
  d <- dim(x)
  nc <- d[2L]
  xm <- data.matrix(x)
  k <- nc - 2
  Cf <- array("", dim = d, dimnames = dimnames(xm))
  ok <- !(ina <- is.na(xm))
  acs <- abs(coef.se <- xm[, cs.ind, drop = FALSE])
  if (any(ia <- is.finite(acs))) {
    digmin <- 1 + if (length(acs <- acs[ia & acs != 0])) 
      floor(log10(range(acs[acs != 0], finite = TRUE)))
    else 0
    Cf[, cs.ind] <- format(round(coef.se, max(1L, digits - digmin)), 
                           digits = digits)
  }
  Cf[, tst.ind] <- format(round(xm[, tst.ind], digits = dig.tst), digits = digits)
  if (any(r.ind <- !((1L:nc) %in% c(cs.ind, tst.ind, nc)))) 
    for (i in which(r.ind)) Cf[, i] <- format(xm[, i], digits = digits)
  ok[, tst.ind] <- FALSE
  okP <- ok[, -nc]
  x1 <- Cf[okP]
  x0 <- (xm[okP] == 0) != (as.numeric(x1) == 0)
  if (length(not.both.0 <- which(x0 & !is.na(x0)))) {
    Cf[okP][not.both.0] <- format(xm[okP][not.both.0], 
                                  digits = max(1L, digits - 1L))
  }
  if (any(ina)) 
    Cf[ina] <- "NA"
  if (any(inan <- is.nan(xm))) 
    Cf[inan] <- "NaN"
  if (any(okP <- ok[, nc])) {
    pv <- as.vector(xm[, nc])
    Cf[okP, nc] <- format.pval(pv[okP], digits = dig.tst, eps = 1e-12, nsmall = 5L)
    signif.stars <- any(pv[okP] < 0.1)
    Signif <- symnum(pv, corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                     symbols = c("***", "**", "*", ".", " "))
    Cf <- cbind(Cf, format(Signif))
  }
  Cf[!is.na(Cf)] <- chartr("e", "E", Cf[!is.na(Cf)])
  Cf[!is.na(Cf)] <- chartr(".", ",", Cf[!is.na(Cf)])
  Cf[, ncol(Cf)] <- chartr(",", ".", Cf[, ncol(Cf)])
  print.default(Cf, quote = FALSE, right = TRUE, na.print = "NA")
  if (signif.stars) {
    cat("---\nSignifikansnivåer:  0 *** 0,001 ** 0,01 * 0,05 . 0,1\n")
  }
  invisible(x)
}



# Omregning av Raddum-II- til Raddum-I-verdier
Raddum2_1 <- Raddum1_2 <- function(DATA) {
  w1 <- which(DATA$parid == "RADDUM2")
  w2 <- which(DATA$parid == "RADDUM2" & DATA$verdi > 0.5)
  if (length(w2)) {
    DATA$verdi[w2] <- 1
  }
  if (length(w1)) {
    DATA$parid[w1] <- "RADDUM1"
    skriv(length(w1), " Raddum-II-målinger har blitt regna om til Raddum I.",
          linjer.under = 1)
  }
  return(DATA)
}



# Kombinerer to utmatinger av "fraVFtilNI"
# (Må kun brukes for ulike vannkategorier innenfor samme parameter!)
kombiner <- function(ut1, ut2) {
  ok <- TRUE
  UT <- ut1
  if (names(ut1) %=% names(ut2) &
      !is.null(attr(ut1, "parameter")) &
      attr(ut1, "parameter") %=% attr(ut2, "parameter") &
      !is.null(attr(ut1, "vannkategori")) &
      !is.null(attr(ut2, "vannkategori"))) {
    for (i in names(ut1)) {
      if (dimnames(ut1[[i]])[1:2] %=% dimnames(ut2[[i]])[1:2]) {
        ny <- array(0, dim = dim(ut1[[i]]) + c(0, 0, dim(ut2[[i]])[3] - 1))
        dimnames(ny) <- list(dimnames(ut1[[i]])[[1]],
                             dimnames(ut1[[i]])[[2]],
                             c("pred", 2:(dim(ny)[3]) - 1))
        ny[, , 2:dim(ut1[[i]])[3]] <- ut1[[i]][, , 2:dim(ut1[[i]])[3]]
        ny[, , dim(ut1[[i]])[3] - 1 +
             2:dim(ut2[[i]])[3]] <- ut2[[i]][, , 2:dim(ut2[[i]])[3]]
        for (j in 1:dim(ny)[1]) {
          for (k in 1:dim(ny)[2]) {
            ny[j, k, 1] <- median(ny[j, k, -1])
          }
        }
        UT[[i]] <- ny
      } else {
        ok <- FALSE
      }
    }
    if (ok) {
      attr(UT, "parameter")     <- attr(ut1, "parameter")
      attr(UT, "vannkategori")  <- attr(ut1, "vannkategori") %+% "," %+%
        attr(ut2, "vannkategori")
      attr(UT, "tidspunkt")     <- Sys.time()
      attr(UT, "innstillinger") <- NULL
      attr(UT, "beskjeder")     <- NULL
    }
  } else {
    ok <- FALSE
  }
  if (ok) {
    return(UT)
  } else {
    skriv("De to utmatingene var ikke kompatible og kunne ikke kombineres!",
          pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    return(NULL)
  }
}



# Definere en fargepalett tilpassa vannforskriften
farge <- function(eqr, na.farge=0.84) {
  r <- ifelse(is.na(eqr), na.farge,
              ifelse(eqr < 0.1, eqr * 5 + 0.5,
                     ifelse(eqr < 0.5, 1,
                            ifelse(eqr < 0.7, sqrt(3.5 - eqr * 5),
                                   ifelse(eqr < 0.9, 0, eqr * 5 - 4.5)))))
  g <- ifelse(is.na(eqr), na.farge,
              ifelse(eqr < 0.1, 0,
                     ifelse(eqr < 0.5, eqr * 2.5 - 0.25,
                            ifelse(eqr < 0.8, 1,
                                   ifelse(eqr < 0.9, 9 - eqr * 10, 0)))))
  b <- ifelse(is.na(eqr), na.farge,
              ifelse(eqr < 0.7, 0,
                     ifelse(eqr < 0.8, eqr * 10 - 7, 1)))
  rgb(r,g,b)
}
