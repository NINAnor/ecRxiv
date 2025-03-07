### WFD2ECA
# Dataflyt fra vannforskriften til økologisk tilstandsregnskap
# ved Hanno Sandvik
# januar 2025
# se https://github.com/NINAnor/NI_vannf
###



WFD2ECA <- function(
  DATA,
  vannforekomster,
  vannlokaliteter,
  parameter,
  vannkategori,
  filKlasser = NULL,
  rapportaar = c(2012, 2015, 2018, 2021, 2024), #¤
  rapportenhet = c("norge", "landsdel"),
  adminAar = 2010,
  kommHist = "../data/knr.xlsx",
  fylkHist = "../data/fnr.xlsx",
  #paramFil = "data/VM-param.xlsx",
  #aktivFil = "data/VM-aktiv.xlsx",
  rapportperiode = 3,
  vedMaalefeil = "dato",
  maksSkjevhet = 3,
  bareInkluder = NULL,
  ikkeInkluder = NULL,
  maalingPer = 100, #¤
  maalingTot = 1000, #¤
  maalingTyp = 50, #¤
  maalingInt = 50, #¤
  EQR = "asymptotisk",
  ignorerVariabel = NULL,
  fastVariabel = NULL,
  aktVekting = TRUE,
  aktivitetsvekt = 3, #¤
  antallvekt = 0.5,
  tidsvekt = 1,
  arealvekt = 2,
  logit = TRUE,
  DeltaAIC = 2,
  interaksjon = TRUE,
  ekstrapolering = "kjente",
  iterasjoner = 10000,
  SEED = NULL,
  bredde = NULL,
  vis = TRUE,
  tell = TRUE,
  ...
) {
  
  ### WFD2ECA [Water Framework Directive to Ecological Condition Account
  # = fra vannforskrift til økologisk tilstandsregnskap]
  # ved Hanno Sandvik
  # januar 2025
  # se https://github.com/NINAnor/NI_vannf #¤
  ###
  
  
  ### Oversikt over argumenter
  # DATA: navn på R-datarammen med målinger fra vannmiljø
  # vannforekomster: navnet på R-datarammen med vannforekomster
  # vannlokaliteter: navnet på R-datarammen med vannlokaliteter
  # parameter: vannmiljøs forkortelse på vannforskriftsparameteren
  # vannkategori: "L", "R" eller "C" for innsjø, elv eller kyst
  # filKlasser: filnavn på excel-regnearket med parameterens klassegrenser
  # rapportaar: rapporteringsår for tilstandsregnskapet 
  # rapportenhet: romlig enhet til skal skaleres opp til. Kan være én eller flere
  #   av "kommune", "fylke", "landsdel" og "Norge"
  # adminAar: årstallet for kommune- og fylkesinndelinga som skal legges til grunn 
  # kommHist: filnavn på excel-regneark med Norges kommunehistorikk
  # fylkHist: filnavn på excel-regneark med Norges  fylkeshistorikk
  # paramFil: filnavn på excel-regneark med vannforskriftsparametere
  # aktivFil: filnavn på excel-regneark med overvåkingsaktiviteter
  # rapportperiode: hvor mange år før rapporteringstidspunkt skal målinger inkluderes
  # vedMaalefeil: hva skal ekskluderes når det avdekkes feilrapportere verdier
  #   - "måling" (kun feilmålingen ekskluderes)
  #   - "dato" (alle målinger utført av samme oppdragstager på samme dato ekskluderes)
  #   - "oppdragstager" (aller målinger utført av samme oppdragstager ekskluderes)
  # maksSkjevhet: aktiviteter med høyere absolutt skjevhetsskår blir ekskludert
  # bareInkluder: typologifaktorer (hvis oppgitt, blir bare disse hensyntatt)
  # ikkeInkluder: typologifaktorer som skal utelates fra modelleringa
  #   [f.eks. vil list(typ="tur", vrd=2) ekskludere brepåvirka elver]
  # maalingPer: minste antall vannforekomster med målinger per rapporteringsperiode
  # maalingTot: minste antall vannforekomster med målinger totalt
  # maalingTyp: minste antall målinger for å inkludere en typologifaktor i modellen
  # maalingInt: minste antall målinger per kombinasjon av parametere ved interaksjon
  # EQR: måte å beregne mEQR-verdier på
  #   - "asymptotisk" (asymptotisk begrensning til intervallet mellom -0,2 og +1,2)
  #   - "forlengelse" (lineær forlengelse av tilstøtende tilstandsklasser)
  #   - "knekk" (lineær begrensning til intervallet mellom -0,2 og +1,2)
  #   - "nEQR" (trunkert ved 0 og 1)
  # ignorerVariabel: typologifaktorer som ikke skal  inngå i modelltilpasninga
  # fastVariabel: typologifaktorer som ikke skal droppes fra modelltilpasninga
  # aktVekting: måten vekting av overvåkingsaktiviteter blir implementert på
  # aktivitetsvekt: tallverdi som blir brukt for vekting av overvåkingsaktiviteter
  # antallvekt: tallverdi som blir brukt for vekting av antall prøver per maaleverdi
  # tidsvekt: faktor for nedvekting av eldre målinger
  # arealvekt: tallverdi for vekting av innsjøstørrelse
  #   - 0 (lik vekt for alle innsjøvannforekomster)
  #   - 1 (vekting med innsjøvannforekomstenes idealiserte diameter)
  #   - 2 (vekting med innsjøvannforekomstenes areal)
  #   - 3 (vekting med innsjøvannforekomstenes idealiserte volum)
  # logit: skal mEQR-verdier logit-transformeres for modelltilpasninga
  # DeltaAIC: hvor mye lavere AIC skal en mer kompleks modell ha for å bli valgt
  # interaksjon: skal modellseleksjonen teste interaksjoner med rapporteringsperiode
  # ekstrapolering: skal det ekstrapoleres til "alle" eller til "kjente" vanntyper
  # iterasjoner: antall iterasjoner som skal brukes i simuleringa
  # SEED: frø for slumptallgeneratoren
  # bredde: bredden til beskjeder i antall tegn
  # vis: skal beskjeder om modelltilpasninga vises 
  # tell: skal prosenttellingen av fremgangen vises
  # ...: eventuelle argumenter for "sjekkXXX"-funksjonene
  ###

  
  if (vis) {
    cat("\n\n")
    cat("*** Fra vannforskrift til økologisk tilstandsregnskap ***\n")
    cat("*******************   versjon  0.15   *******************\n")
  }
  
  OK <- TRUE
  u  <- c()
  breddeUtmating <<- bredde
  
  #########################################################################
  skriv("Innledende tester", pre = "   ", linjer.over  = 1)
  skriv("=================", pre = "   ", linjer.under = 1)

  # Sjekke om nødvendige funksjonsargumenter er oppgitt
  if (missing(DATA)) {
    OK <- FALSE
    skriv("Argumentet \"DATA\" må være oppgitt og inneholde målinger ",
          "som er eksportert fra vannmiljø.",
          pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
  } else {
    if (!is.data.frame(DATA) || 
        !(all(c("lokid", "parid", "verdi", "tidpkt", "aktid") %in% names(DATA)))) {
      OK <- FALSE
      skriv("Datafila har et uventa format!",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    }
  }
  if (missing(vannforekomster)) {
    OK <- FALSE
    skriv("Argumentet \"vannforekomster\" må være oppgitt og inneholde ",
          "informasjon fra vann-nett om vannforekomstene.",
          pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
  } else {
    if (!is.data.frame(vannforekomster) || 
        !(all(c("id", "typ", "kat", "knr") %in% names(vannforekomster)))) {
      OK <- FALSE
      skriv("Datarammen med vannforekomster har et uventa format!",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    }
  }
  if (missing(vannlokaliteter)) {
    OK <- FALSE
    skriv("Argumentet \"vannlokaliteter\" må være oppgitt og inneholde ",
          "informasjon fra vannmiljø om vannlokalitetene.",
          pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
  } else {
    if (!is.data.frame(vannlokaliteter) || 
        !(all(c("lokid", "lokkod", "id") %in% names(vannlokaliteter)))) {
      OK <- FALSE
      skriv("Datarammen med vannlokaliteter har et uventa format!",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    }
  }
  if (missing(parameter) ||
    !(length(parameter) %=% 1 && is.character(parameter))) {
      OK <- FALSE
    skriv("Argumentet \"parameter\" må være oppgitt og bestå av ",
          "en parameter-id fra vannmiljø.",
          pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
  } else {
    parameter <- toupper(parameter)
  }
  if (missing(vannkategori) ||
      !(length(vannkategori) %=% 1 && 
        toupper(vannkategori) %in% c("L", "R", "C"))) {
    OK <- FALSE
    skriv("Argumentet \"vannkategori\" må være nøyaktig én av bokstavene \"L\", ",
          "\"R\" eller \"C\"!", pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
  } else {
    vannkategori <- toupper(vannkategori)
  }
  if (!(vedMaalefeil %in% c("måling", "dato", "oppdragstager") &
        length(vedMaalefeil) %=% 1)) {
    vedMaalefeil <- tolower(vedMaalefeil[1])
    if (vedMaalefeil %begynner% "m") vedMaalefeil <- "måling"
    if (vedMaalefeil %begynner% "d") vedMaalefeil <- "dato"
    if (vedMaalefeil %begynner% "o") vedMaalefeil <- "oppdragstager"
    if (!(vedMaalefeil %in% c("måling", "dato", "oppdragstager"))) {
      vedMaalefeil <- "dato"
    }
    skriv("Variabelen \"vedMaalefeil\" ble satt til \"", vedMaalefeil, ".",
          pre = "OBS: ", linjer.under = 1)
  }
  if (vannkategori != "L" & !(arealvekt %in% c(0, 2))) {
    skriv("Verdien ", arealvekt, " til variabelen \"arealvekt\" gir ikke mening ",
          "for ", ifelse(vannkategori == "R", "elve", "kyst"), "vannforekomster.",
          " Den ble derfor satt til 2.", pre = "OBS: ", linjer.under = 1)
    arealvekt <- 2
  }
  if (!(arealvekt %in% 0:3)) {
    u <- c(u, skriv("Å sette \"arealvekt = ", arealvekt, "\" er et overraskende ",
                    "valg. Jeg håper at du vet hva du gjør!", 
                    pre = "OBS: ", linjer.under = 1, ut = TRUE))
  }

  # Sjekke om nødvendig informasjon er tilgjengelig
  if (OK) {
    if (vannkategori == "C") {
      if (exists("TypologiC")) {
        Variabler <- c("akt", "smvf", TypologiC)
      } else {
        OK <- FALSE
        skriv("Konstanten \"TypologiC\" fins ikke!", pre = "FEIL: ",
              linjer.over = 1, linjer.under = 1)
      }
      if (exists("ord2numC")) {
        ord2num <- ord2numC
      } else {
        OK <- FALSE
        skriv("Konstanten \"ord2numC\" fins ikke!", pre = "FEIL: ",
              linjer.over = 1, linjer.under = 1)
      }
    }
    if (vannkategori == "L") {
      if (exists("TypologiL")) {
        Variabler <- c("akt", "smvf", TypologiL)
      } else {
        OK <- FALSE
        skriv("Konstanten \"TypologiL\" fins ikke!", pre = "FEIL: ",
              linjer.over = 1, linjer.under = 1)
      }
      if (exists("ord2numL")) {
        ord2num <- ord2numL
      } else {
        OK <- FALSE
        skriv("Konstanten \"ord2numL\" fins ikke!", pre = "FEIL: ",
              linjer.over = 1, linjer.under = 1)
      }
    }
    if (vannkategori == "R") {
      if (exists("TypologiR")) {
        Variabler <- c("akt", "smvf", TypologiR)
      } else {
        OK <- FALSE
        skriv("Konstanten \"TypologiR\" fins ikke!", pre = "FEIL: ",
              linjer.over = 1, linjer.under = 1)
      }
      if (exists("ord2numR")) {
        ord2num <- ord2numR
      } else {
        OK <- FALSE
        skriv("Konstanten \"ord2numR\" fins ikke!", pre = "FEIL: ",
              linjer.over = 1, linjer.under = 1)
      }
    }
    for (v in c("Vanntyper.nominal", "Vanntyper.ordinal", "Tallverdier",
                "Typologi.nominal", "Typologi.ordinal", "Typologi.numerisk")) {
      if (!exists(v)) {
        OK <- FALSE
        skriv("Konstanten \"", v, "\" fins ikke!", pre = "FEIL: ",
              linjer.over = 1, linjer.under = 1)
      }
    }
    for (v in c("%=%", "%!=%", "%==%", "%+%", "%-%", "%A%",
                "%inneholder%", "%mellom%", "%utafor%", "%begynner%", "%pluss%",
                "ln", "lg", "skriv", "erstatt", "komma", "oppsummer", "somDag",
                "parameterNavn", "tillatteVerdier", "skaler", "reskaler", "iNv",
                "tabulerKoeffisienter")) {
      if (!exists(v)) {
        OK <- FALSE
        skriv("Funksjonen \"", v, "\" fins ikke!", pre = "FEIL: ",
              linjer.over = 1, linjer.under = 1)
      }
    }
    
    if (exists("Parametere")) {
#    if (file.exists(paramFil)) {
#      Parametere <- try(as.data.frame(read_xlsx(paramFil, na = "NA", col_types =
#                                        c("text", "text", "numeric", "numeric"))))
#      if (inherits(Parametere, "try-error")) {
#        OK <- FALSE
#        skriv("Dette skjedde en feil under innlesing av fila \"", paramFil,
#              ". Sjekk om den har korrekt format (xlsx-regneark).",
#              pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
#      } else {
        if (is.data.frame(Parametere) && 
            names(Parametere) %=% c("id", "navn", "min", "max")) {
          Parametere <<- Parametere
          w <- which(Parametere$id == parameter)
          if (length(w) %=% 1) {
            if (any(is.na(Parametere[w, ]))) {
              OK <- FALSE
              skriv("Tillatte verdier er ikke angitt for \"", parameter,
                    "\" i regnearket \"", paramFil, "!",
                    pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
            }
          } else {
            OK <- FALSE
            skriv("Parameter-id-en \"", parameter, "\" fins ikke i ",
                  "regnearket \"", paramFil, "!",
                  pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
          }
        } else {
          OK <- FALSE
          skriv("Kolonnenavnene i regnearket \"", paramFil, "\" er ikke som ",
                "forventa!", pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
        }
#      }
    } else {
      OK <- FALSE
      skriv("Fila \"Parametere\" ble ikke funnet.",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
#      skriv("Dette skjedde en feil under innlesing av fila \"", paramFil,
#            ". Sjekk om fila fins, og at det er oppgitt korrekt navn på den.",
#            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    }

    if (exists("Aktiviteter")) {
#    if (file.exists(aktivFil)) {
#      Aktiviteter <- try(as.data.frame(read_xlsx(aktivFil, na = "NA", col_types =
#                                                   c("text", "text", "numeric"))))
#      if (inherits(Aktiviteter, "try-error")) {
#        OK <- FALSE
#        skriv("Dette skjedde en feil under innlesing av fila \"", aktivFil,
#              ". Sjekk om den har korrekt format (xlsx-regneark).",
#              pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
#      } else {
        if (!is.data.frame(Aktiviteter) || 
            names(Aktiviteter) %!=% c("id", "navn", "skaar")) {
          OK <- FALSE
          skriv("Kolonnenavnene i regnearket \"", aktivFil, "\" er ikke som ",
                "forventa!", pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
        }
#      }
    } else {
      OK <- FALSE
      skriv("Fila \"Aktiviteter\" ble ikke funnet.",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
#      skriv("Dette skjedde en feil under innlesing av fila \"", aktivFil,
#            ". Sjekk om fila fins, og at det er oppgitt korrekt navn på den.",
#            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    }
  }

  if (OK) {
    # Lese inn klassegrenser for den angitte parameteren
    if (is.null(filKlasser)) {
      filKlasser <- "../data/klassegrenser_" %+% parameter %+% ".xlsx" #¤
    }
    KlasseGrenser <- hentKlassegrenser(filKlasser)
    if (is.null(KlasseGrenser)) {
      OK <- FALSE
    }
  }
  
  if (OK) {
    if ("neqr" %begynner% tolower(EQR)) {
      if (exists("nEQR") && is.function(nEQR)) {
        mEQR <-  nEQR
         EQR <- "nEQR"
      } else {
        OK <- FALSE
        skriv("Funksjonen \"nEQR\" ble ikke funnet.", pre = "FEIL: ",
              linjer.over = 1, linjer.under = 1)
      }
    } else {
      if ("forlengelse" %begynner% tolower(EQR) |
          "forlenga"    %begynner% tolower(EQR)) {
        if (exists("mEQR.forlenga") && is.function(mEQR.forlenga)) {
          mEQR <- mEQR.forlenga
           EQR <- "forlengelse"
        } else {
          OK <- FALSE
          skriv("Funksjonen \"mEQR.forlenga\" ble ikke funnet.", pre = "FEIL: ",
                linjer.over = 1, linjer.under = 1)
        }
      } else {
        if ("knekk" %begynner% tolower(EQR)) {
          if (exists("mEQR.knekk") && is.function(mEQR.knekk)) {
            mEQR <- mEQR.knekk
             EQR <- "knekk"
          } else {
            OK <- FALSE
            skriv("Funksjonen \"mEQR.knekk\" ble ikke funnet.", pre = "FEIL: ",
                  linjer.over = 1, linjer.under = 1)
          }
        } else {
          if (!("asymptotisk" %begynner% tolower(EQR))) {
            u <- c(u, skriv("Argumentet \"mEQR\" har en ugyldig verdi. ",
                            "Det brukes mEQR-verdier med asymptotisk begrensning.", 
                            pre = "OBS: ", linjer.under = 1, ut = TRUE))
          }
          EQR <- "asymptotisk"
          if (!exists("mEQR") || !is.function(mEQR)) {
            OK <- FALSE
            skriv("Funksjonen \"mEQR\" ble ikke funnet.", pre = "FEIL: ",
                  linjer.over = 1, linjer.under = 1)
          }
        }
      }
    }
    if (!( "alle"  %begynner% tolower(ekstrapolering) | 
          "kjente" %begynner% tolower(ekstrapolering))) {
      u <- c(u, skriv("Argumentet \"ekstrapolering\" har en ugyldig verdi og ",
                      "settes derfor til \"kjente\".", pre = "OBS: ",
                      linjer.under = 1, ut = TRUE))
      ekstrapolering <- "kjente"
    }
    ekstrapol <- "alle" %begynner% tolower(ekstrapolering)
  }
  
  if (OK) {
    skriv("De nødvendige datafilene ble funnet. Da setter vi i gang.",
          linjer.under = 1)
    Vf <- vannforekomster
    VL <- vannlokaliteter
    
    #########################################################################
    skriv("Lasting av administrative enheter", pre = "   ", linjer.over  = 1)
    skriv("=================================", pre = "   ", linjer.under = 1)

    kommunehistorikk <- try(as.data.frame(read_xlsx(kommHist, col_types = "text")))
    if (inherits(kommunehistorikk, "try-error")) {
      OK <- FALSE
      skriv("Dette skjedde en feil under innlesing av fila \"", kommHist,
            ". Sjekk om fila fins, og at det er oppgitt korrekt navn på den.",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    }
    fylkeshistorikk <- try(as.data.frame(read_xlsx(fylkHist, col_types = "text")))
    if (inherits(fylkeshistorikk, "try-error")) {
      OK <- FALSE
      skriv("Dette skjedde en feil under innlesing av fila \"", fylkHist,
            ". Sjekk om fila fins, og at det er oppgitt korrekt navn på den.",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    }
  }
  
  if (OK) {
    kommunehistorikk$Nummer[which(nchar(kommunehistorikk$Nummer) == 3)] <-
      "0" %+% kommunehistorikk$Nummer[which(nchar(kommunehistorikk$Nummer) == 3)]
    relevanteRader   <- 4:(ncol(kommunehistorikk) - 1)
    rownames(kommunehistorikk) <- kommunehistorikk$Nummer
    rownames(fylkeshistorikk)  <-  fylkeshistorikk$nr
    fylke <- function(i) fylkeshistorikk[as.character(i), "navn"]
    
    rapportenhet <- tolower(unique(rapportenhet))
    if (length(rapportenhet)) {
      for (i in 1:length(rapportenhet)) {
        if ("kommuner"   %begynner% rapportenhet[i]) rapportenhet[i] <- "kommune"
        if ("fylker"     %begynner% rapportenhet[i]) rapportenhet[i] <- "fylke"
        if ("landsdeler" %begynner% rapportenhet[i]) rapportenhet[i] <- "landsdel"
        if ("norge"      %begynner% rapportenhet[i]) rapportenhet[i] <- "norge"
      }
    }
    if (!length(rapportenhet %A% c("kommune", "fylke", "landsdel", "norge"))) {
      rapportenhet            <- c("kommune", "fylke", "landsdel", "norge")
    }
    if (!is.numeric(adminAar) || length(adminAar) != 1) {
      adminAar <- 9000
    }
    
    kaar <- as.numeric(colnames(kommunehistorikk)[relevanteRader])
    if (adminAar < 1977) {
      adminAar <- 1980
      skriv("Kommunegrenser fra før 1977 er dessverre ikke tilgjengelig. Det ",
            "administrative året ble satt til 1980",
            pre="OBS: ", linjer.under = 1)
    }
    KOM <- sort(unique(kommunehistorikk[, 
                as.character(kaar[max(which(kaar <= adminAar))])])) %-% "."
    
    FYL <- fylkeshistorikk$navn[which(fylkeshistorikk$fra <= adminAar &
                                      fylkeshistorikk$til >= adminAar)]
    FNR <- fylkeshistorikk$nr  [which(fylkeshistorikk$fra <= adminAar &
                                      fylkeshistorikk$til >= adminAar)]
    WF <- list(Ø = c("0" %+% 1:7, 30:34)      %+% "00",
               S = c("08", "09", "10", "40"	) %+% "00",
               V = c(11:14, 46)               %+% "00",
               M = c(15:17, 50)               %+% "00",
               N = c(18:20, 54:56)            %+% "00")
    Vk <- Vf$knr
    Vn <- Vf$kommune
    Fn <- ""
    for (k in 1:nrow(kommunehistorikk)) {
      knr <- kommunehistorikk$Nummer[k]
      w <- unique(unlist(kommunehistorikk[k, relevanteRader])) %-% "."
      for (kk in w) {
        knr <- c(knr, kommunehistorikk$Nummer[which(kommunehistorikk == kk, 
                                                    arr.ind = TRUE)[, 1]])
      }
      Vk <- erstatt(Vk,         knr[1], paste(sort(unique(knr)), collapse = ","))
      Vk <- erstatt(Vk, "0" %+% knr[1], paste(sort(unique(knr)), collapse = ","))
    }
    for (k in 1:nrow(Vf)) {
      knr <- sort(unique(unlist(strsplit(Vk[k], ","))))
      Vk[k] <- paste(knr, collapse = ",")
      knv <- unique(unlist(kommunehistorikk[knr, 4:ncol(kommunehistorikk)]))
      Vn[k] <- paste(((knv %-% ".") %-% "") %+% ",", collapse = "")
      fnr <- sort(unique(substr(knr, 1, 2))) %+% "00"
      Fn[k] <- paste(fnr, collapse = ",")
    }
    Vf$knr     <- Vk
    Vf$kommune <- Vn
    Fy <- Fn
    for (f in 1:nrow(fylkeshistorikk)) {
      if (adminAar %mellom% fylkeshistorikk[f, c("fra", "til")]) {
        Fy <- erstatt(Fy, fylkeshistorikk$nr[f], fylkeshistorikk$navn[f])
      } else {
        Fy <- erstatt(Fy, fylkeshistorikk$nr[f] %+% ",", "")
        Fy <- erstatt(Fy, fylkeshistorikk$nr[f],         "")
        Fn <- erstatt(Fn, fylkeshistorikk$nr[f] %+% ",", "")
        Fn <- erstatt(Fn, fylkeshistorikk$nr[f],         "")
      }
    }
    Vf$fylke <- Fn
    skriv("De administrative enhetene er på plass. Per ", adminAar,
          ifelse(adminAar < as.numeric(format(Sys.Date(), "%Y")),
                 " fantes", " fins"),
          " det ", length(unique(FYL)), " fylker",
          ifelse("kommune" %in% rapportenhet,
            " og " %+% length(unique(KOM)) %+% " kommuner.",
            "."),
          linjer.under = 1)
  }
  
  if (OK) {

    ######################################################################
    skriv("Undersøkelse av innmatingsdata", pre = "   ", linjer.over  = 1)
    skriv("==============================", pre = "   ", linjer.under = 1)

    # Sjekk av år mot rapporteringsperioden
    aar <- rappAar <- as.numeric(substr(DATA$tidpkt, 1, 4))
    rapportaar <- sort(rapportaar)
    startaar <- min(rapportaar) - rapportperiode + 1
    sluttaar <- max(rapportaar)
    er.med <- which(DATA$parid == parameter & !is.na(DATA$verdi) &
                    aar >= startaar & aar <= sluttaar)
    for (i in rev(rapportaar)) {
      rappAar[which(aar <= i)] <- i 
    } # frem til v. 1.1 sto det "<":
      # rappAar[which(aar < i)] <- i
    relaar <- aar - rappAar
    u <- c(u, skriv("Det foreligger ", length(which(DATA$parid == parameter)), 
                    " målinger av parameteren ", parameter, " [", 
                    parameterNavn(parameter), "].", linjer.under = 1, ut = TRUE))
    feil <- FALSE
    ikke.med <- which(DATA$parid == parameter & aar < startaar)
    if (length(ikke.med)) {
      u <- c(u, skriv(length(ikke.med), ifelse(length(ikke.med) == 1,
                                          " måling ble ekskludert fordi den",
                                          " målinger ble ekskludert fordi de"),
                      " ble tatt før " %+% startaar %+% ".", 
                      pre = "OBS: ", linjer.under = 1, ut = TRUE))
      feil <- TRUE
    }
    ikke.med <- which(DATA$parid == parameter & aar > sluttaar)
    if (length(ikke.med)) {
      u <- c(u, skriv(length(ikke.med), ifelse(length(ikke.med) == 1,
                                          " måling ble ekskludert fordi den",
                                          " målinger ble ekskludert fordi de"),
                      " ble tatt etter " %+% sluttaar %+% ".", 
                      pre = "OBS: ", linjer.under = 1, ut = TRUE))
      feil <- TRUE
    }
    ikke.med <- which(DATA$parid == parameter & is.na(aar))
    if (length(ikke.med)) {
      u <- c(u, skriv(length(ikke.med), ifelse(length(ikke.med) == 1,
                                          " måling ble ekskludert fordi den",
                                          " målinger ble ekskludert fordi de"),
                      " ble foretatt på et ukjent tidspunkt.", 
                      pre = "OBS: ", linjer.under = 1, ut = TRUE))
      feil <- TRUE
    }
    if (!feil) {
      u <- c(u, skriv("Alle målinger ble tatt mellom ", min(aar[er.med]), " og ",
                      max(aar[er.med]), ".", linjer.under = 1, ut = TRUE))
    }

    # Parameterens måleenhet
    enhet <- NULL
    if (unique(DATA$enhet[er.med]) %!=% "3" & exists("Enheter")) {
        enhet <- Enheter[which(Enheter[, 1] == unique(DATA$enhet[er.med])), 2]
    }

    # Sjekk av måleverdiene mot parameterens definisjonsområde
    feil1 <- er.med %A% which(DATA$verdi %utafor% tillatteVerdier(parameter))
    if (length(feil1)) {
      tillt <- tillatteVerdier(parameter)
      feil2 <- range(DATA$verdi[feil1])
      feil3 <- feil1
      tekst <- " ligger utafor parameterens definisjonsområde! "
      tekst <- length(feil1) %+% 
               ifelse(length(feil1) == 1, 
                      " måling"   %+% tekst %+% "Dens verdi er ",
                      " målinger" %+% tekst %+% "Deres verdier er ")
      txt2 <- c()
      if (feil2[1] < tillt[1]) {
        txt2 <- c(txt2, "mindre enn " %+% komma(tillt[1]) %+% 
                  " (ned til " %+% komma(feil2[1]) %+% ")")
        if (!is.null(enhet)) {
          txt2 <- c(txt2, " " %+% enhet)
        }
      }
      if (feil2[2] > tillt[2]) {
        txt2 <- c(txt2, "større enn " %+% komma(tillt[2]) %+% 
                    " (opp til " %+% komma(feil2[2]) %+% ")")
        if (!is.null(enhet)) {
          txt2 <- c(txt2, " " %+% enhet)
        }
      }
      tekst <- tekst %+% paste(txt2, collapse = " eller ") %+% "."
      if (vedMaalefeil %=% "måling") {
        feil3 <- unique(feil3)
        u <- c(u, skriv(tekst, ifelse(length(feil1) == 1, " Denne", " Disse"), 
                        " ble ekskludert.", 
                        pre = "OBS: ", linjer.under = 1, ut = TRUE))
      }
      if (vedMaalefeil %=% "dato") {
        for (i in (unique(DATA$oppdrt[feil1]) %-% "")) {
          for (j in unique(as.Date(DATA$tidpkt[feil1 %A% 
                                               which(DATA$oppdrt == i)]))) {
            feil3 <- c(feil3, er.med %A% which(DATA$oppdrt == i & 
                                               as.Date(DATA$tidpkt) == j))
          }
        }
        feil3 <- unique(feil3)
        u <- c(u, skriv(tekst, " I tillegg til d", 
                        ifelse(length(feil1) == 1, "enne", 
                               "isse " %+% length(feil1)), 
                        " ble ytterligere ", (length(feil3) - length(feil1)),
                        ifelse((length(feil3) - length(feil1)) == 1,
                               " måling", " målinger"), 
                        " ekskludert, fordi de hadde samme oppdragstaker (",
                        paste(unique(DATA$oppdrt[feil1]), collapse=", "), 
                        ") og prøvetakingsdato (", 
                        paste(unique(format(as.Date(DATA$tidpkt[feil1]), 
                                            "%d.%m.%Y")), 
                              collapse=", "), ").", 
                        pre = "OBS: ", linjer.under = 1, ut = TRUE))
      }
      if (vedMaalefeil %=% "oppdragstager") {
        for (i  in (unique(DATA$oppdrt[feil1]) %-% "")) {
          feil3 <- c(feil3, er.med %A% which(DATA$oppdrt == i))
        }
        feil3 <- unique(feil3)
        u <- c(u, skriv(tekst, " I tillegg til d", 
                        ifelse(length(feil1) == 1, "enne", 
                               "isse " %+% length(feil1)), 
                        " ble ytterligere ", (length(feil3) - length(feil1)),
                        ifelse((length(feil3) - length(feil1)) == 1,
                               " måling", " målinger"), 
                        " ekskludert, fordi de hadde samme oppdragstaker (",
                        paste(unique(DATA$oppdrt[feil1]), collapse=", "), ").", 
                        pre = "OBS: ", linjer.under = 1, ut = TRUE))
      }
      if (length(feil1) > length(er.med) / 10) {
        u <- c(u, skriv("Hele ", round(100 * length(feil1) / length(er.med)),
                        " % av målingene er rapportert med verdier som ligger ",
                        "utafor parameterens definisjonsområde! Det kan tyde på ",
                        "at parameterens definisjon er dårlig forstått blant ",
                        "oppdragstagerne. Det bør vurderes om de resterende ",
                        "målingene er til å stole på!", pre = "OBS: ",
                        linjer.under = 1, ut = TRUE))
      }
      er.med <- er.med %-% feil3
    } else {
      u <- c(u,skriv("Alle målinger ligger innafor parameterens definisjonsområde.", 
                     linjer.under = 1, ut = TRUE))
    }
    
    # Sjekk av overvåkingsaktivitetene bak målingene
    rownames(Aktiviteter) <- Aktiviteter$id
    skjev <- er.med %A% which(abs(Aktiviteter[DATA$aktid, "skaar"]) > maksSkjevhet)
    if (length(skjev)) {
      er.med <- er.med %-% skjev
      u <- c(u, skriv(length(skjev), ifelse(length(skjev) == 1, 
                                            " måling", " målinger"),
                      " bla samla inn i rammen av aktiviteter som er for lite ",
                      "representative. Disse ble ekskludert.", 
                      pre = "OBS: ", linjer.under = 1, ut = TRUE))
    } else {
      if (maksSkjevhet < 3) {
        u <- c(u, skriv("Ingen målinger ble ekskludert pga. aktivitetens skjevhet.",
                        linjer.under = 1, ut = TRUE))
      }
    }
    
    # Kobling av målinger mot vannforekomster
    maaling <- data.frame(vfo ="", inn = 0, lok = 0,
                          areal=0, A_tot=0, lengd=0,
                          tilsf=0, høyde=0, gbred=0, gleng=0,
                          aar = 0, mnd = 0, dag = 0, per = 0, rar = 0, 
                          akt ="", typ ="", kat ="", reg ="", son ="", 
                          stø ="", alk ="", hum ="", tur ="", dyp ="", kys ="", 
                          sal ="", tid ="", eks ="", mix ="", opp ="", str ="",
                          kom ="", fyl ="", ant = 1, vkt = 1, vrd = 0,
                          smvf="", CaCO3=0, P_tot=0, dybde=0, 
                          kystt=0, saltk=0, ekspo=0, miksg=0, oppht=0, vknop=0,
                          stringsAsFactors = FALSE)
    sam <- 17:32 # dette er typologifaktorene! forsiktig med å endre! (¤)
    uten.kode <- uten.id <- numeric()
    skriv("Vennligst vent mens målingene kobles mot vannforekomster!")
    for (i in er.med) {
      lok <- c(which(VL$lokkod == DATA$lokid[i]), which(VL$lokid == DATA$lokid[i]))
      if (length(lok) %=% 1) {
        vfk <- VL$id[lok]
        # fjernes fordi innsjønumre ikke lenger er inkludert i vannlokasjonsdataene
        # (men effekten av dette er uvisst!):
        #if (substr(vfk, nchar(vfk), nchar(vfk)) %=% "L") {
        #  vfk <- which(Vf$id == (substr(vfk, 1, 4) %+% VL$sjønr[lok] %+% "-L"))
        #  if (!length(vfk)) {
        #    vfk <- which(Vf$id == VL$id[lok])
        #  }
        #} else {
          vfk <- which(Vf$id == vfk)
        #}
        if (length(vfk) %=% 1) {
          maaling <- rbind(maaling, maaling[1, ])
          L <- nrow(maaling)
          maaling$vfo[L]   <- Vf$id  [vfk]  # vannforekomst-id
#          maaling$inn[L]   <- VL$sjøn[lok]  # innsjønummer
          maaling$lok[L]   <- VL$loki[lok]  # vannlokalitets-id
          maaling$aar[L]   <- aar    [i]    # år, måned og dag
          maaling$mnd[L]   <- as.numeric(     substr(DATA$tidpkt[i], 6,  7))
          maaling$dag[L]   <- as.numeric(     substr(DATA$tidpkt[i], 9, 10))
          maaling$per[L]   <- rappAar[i]    # rapporteringsår
          maaling$rar[L]   <- relaar [i]    # relativt år (i rapporteringsperioden)
          maaling$akt[L]   <- DATA$ak[i]    # overvåkingsaktivitets-id
          maaling$kom[L]   <- Vf$komm[vfk]  # kommunenummer
          maaling$fyl[L]   <- Vf$fylk[vfk]  # fylkesnummer
          maaling$vrd[L]   <- DATA$verd[i]  # maleverdi
          maaling$ant[L]   <- DATA$antv[i]  # antall verdier
          for (j in colnames(maaling)[sam]) # typologifaktorene ("kat"-"str")
              maaling[L,j] <- Vf     [vfk,    j]
          maaling$areal[L] <- Vf$area[vfk]  # areal i Norge i km^2
          maaling$A_tot[L] <- Vf$arto[vfk]  # totalt areal i km^2
          maaling$tilsf[L] <- Vf$tils[vfk]  # tilsigsfelt i km^2
          maaling$lengd[L] <- Vf$leng[vfk]  # lengde i km
          maaling$høyde[L] <- Vf$hoh [vfk]  # høyde over havet i m
          maaling$gbred[L] <- Vf$lat [vfk]  # geografisk bredde i grader nord
          maaling$gleng[L] <- Vf$long[vfk]  # geografisk lengde i grader øst
          maaling$smvf [L] <- if            # sterk modifisert vannforekomst?
                             (Vf$smvf[vfk] %=% TRUE) "ja" else "nei"
        } else {
          uten.id <- c(uten.id, i)
        }
      } else {
        uten.kode <- c(uten.kode, i)
      }
      if (tell) {
        cat("Ferdig med " %+% floor(100*(which(er.med == i) / length(er.med))) %+%
              " % av målingene.\r")
      }
    }
    if (tell) {
      cat("\n\n")
    } else {
      cat("Ferdig med 100 % av målingene.\n\n")
    }
    maaling <- maaling[-1,]
    
    # Oppsummering av koblinga mellom målinger og vannforekomster
    if (length(uten.kode)) {
      u <- c(u, skriv(length(uten.kode), ifelse(length(uten.kode) == 1,
                                           " måling ble ekskludert fordi den",
                                           " målinger ble ekskludert fordi de"),
                      " ikke kunne knyttes til noen kjent vannlokalitet.", 
                      pre = "OBS: ", linjer.under = 1, ut = TRUE))
    } else {
      u <- c(u, skriv("Alle målinger kunne knyttes til en vannlokalitet.", 
                      linjer.under = 1, ut = TRUE))
    }
    if (length(uten.id)) {
      u <- c(u, skriv(length(uten.id),
                      ifelse(length(uten.id) == 1,
                        " måling ble ekskludert fordi dens vannlokalitet",
                        " målinger ble ekskludert fordi deres vannlokaliteter"),
                      " ikke kunne knyttes til noen typifisert vannforekomst.",
                      pre = "OBS: ", linjer.under = 1, ut = TRUE))
    } else {
      u <- c(u, skriv("Alle vannlokaliteter kunne knyttes til en typifisert ",
                      "vannforekomst.", linjer.under = 1, ut = TRUE))
    }

    # Sjekk av målingenes vannkategorier og vanntyper    
    if (nrow(maaling)) {
      feil <- which(!(maaling$kat %in% vannkategori))
      if (length(feil)) {
        txt1 <- length(feil) %+% ifelse(length(feil) %=% 1, " måling", " målinger")
        txt2 <- " foretatt i en " %+% 
          ifelse(vannkategori %=% "L",
                 "innsjø",
                 ifelse(vannkategori %=% "R",
                        "elve", 
                        ifelse(vannkategori %=% "C", 
                               "kyst", 
                               "passende ")
                 )
          ) %+% "vannforekomst"
        maaling <- maaling[-feil,]
        u <- c(u, skriv(txt1, " ble ekskludert fordi de", 
                        ifelse(length(feil) %=% 1,"n",""), " ikke ble", txt2, ".", 
                        pre = "OBS: ", linjer.under = 1, ut = TRUE))
        rm(txt1, txt2, feil)
      } else {
        u <- c(u, skriv("Alle målinger ble foretatt i den riktige vannkategorien.", 
                        linjer.under = 1, ut = TRUE))
      }
      rader <- nrow(maaling)
      if (!is.null(bareInkluder)) {
        ta.med <- numeric(0)
        if (is.list(bareInkluder)) {
          for (i in 1:length(bareInkluder$typ)) {
            ok <- which(maaling[, bareInkluder$typ[i]] == bareInkluder$vrd[i])
            if (length(ok)) {
              ta.med <- c(ta.med, ok)
            }
          }
        } else {
          ta.med <- ok <- which(maaling$typ %in% bareInkluder)
        }
        ta.med <- unique(ta.med)
        feil <- nrow(maaling) - length(ta.med)
        if (feil %!=% 0) {
          maaling <- maaling[ta.med, ]
          u <- c(u, skriv(feil, ifelse(feil %=% 1, 
                                       " måling ble ekskludert fordi den",
                                       " målinger ble ekskludert fordi de"),
                          " ble foretatt i en vanntype som parameteren ikke kan ",
                          "brukes i.", pre = "OBS: ", linjer.under = 1, ut = TRUE))
        }
        rm(ta.med, ok, feil)
      }
      if (!is.null(ikkeInkluder)) {
        fjern <- c()
        for (i in 1:length(ikkeInkluder$typ)) {
          fjern <- c(fjern, 
                     which(maaling[, ikkeInkluder$typ[i]] == ikkeInkluder$vrd[i]))
        }
        fjern <- unique(fjern)
        if (length(fjern)) {
          u <- c(u, skriv(length(fjern), ifelse(length(fjern) %=% 1,
                                           " måling ble ekskludert fordi den",
                                           " målinger ble ekskludert fordi de"),
                          " ble foretatt i en vanntype som parameteren ikke kan ",
                          "brukes i.", pre = "OBS: ", linjer.under = 1, ut = TRUE))
          maaling <- maaling[-fjern, ]
        }
        rm(fjern)
      }
      ikkeInkl <- ikkeInkluder
      if (nrow(KlasseGrenser) > pi) {
        okTyper<-rownames(KlasseGrenser)[which(!apply(is.na(KlasseGrenser),1,any))]
        feil <- which(!(maaling$typ %in% okTyper))
        if (length(feil)) {
          maaling <- maaling[-feil, ]
          u <- c(u, skriv(length(feil), ifelse(length(feil) %=% 1,
                                          " måling ble ekskludert fordi den",
                                          " målinger ble ekskludert fordi de"),
                          " ble foretatt i en vanntype som parameteren ikke har ",
                          "definerte referanseverdier og klassegrenser i.",
                          pre = "OBS: ", linjer.under = 1, ut = TRUE))
        }
      }
      if (nrow(maaling) %=% rader) {
        u <- c(u, skriv("Alle målinger ble foretatt i de riktige vanntypene.", 
                        linjer.under = 1, ut = TRUE))
      }
    }
    
    # Sjekk om målingene oppfyller parameterspesifikke krav
    if (nrow(maaling)) {
      sjekkPar <- "sjekk" %+% parameter
      if (exists(sjekkPar) && is.function(get(sjekkPar))) {
        sjekk <- get(sjekkPar)
        slett <- sjekk(maaling)
        if (length(slett)) {
          maaling <- maaling[-slett, ]
          u <- c(u, skriv(length(slett), " datapunkt måtte fjernes fra datasettet",
                          " fordi de ikke oppfyller de spesifikke kravene ",
                          "som stilles til målinger av ", parameter, ".", 
                          pre = "OBS: ", linjer.under = 1, ut = TRUE))
        } else {
          u <- c(u , skriv("Alle målinger ser ut til å oppfylle de spesifikke ",
                           "parameterkravene til ", parameter, ".",
                           linjer.under = 1, ut = TRUE))
        }
        iI <- attr(slett, "ikkeInkluder")
        if (!is.null(iI)) {
          if (is.null(ikkeInkl)) {
            ikkeInkl <- iI
          } else {
            ikkeInkl$typ <- c(ikkeInkl$typ, iI$typ)
            ikkeInkl$vrd <- c(ikkeInkl$vrd, iI$vrd)
          }
        }
      } else {
        u <- c(u, skriv("Det foreligger ingen funksjon som kan sjekke eventuelle ",
                        "parameterspesifikke krav for ", parameter, ".",
                        linjer.under = 1, ut = TRUE))
      }
    }
    
    # Sjekk av antall målinger per rapporteringsperiode
    if (nrow(maaling)) {
      fjernAar <- c()
      for (i in rapportaar) {
        w <- length(unique(maaling$vfo[which(maaling$per == i)]))
        if (w < maalingPer) {
          if (w > 0) {
            maaling <- maaling[-which(maaling$per == i), ]
          }
          fjernAar <- c(fjernAar, i)
          u <- c(u, skriv("For rapportåret ",i," foreligger bare målinger fra ",w, 
                          " vannforekomster. Det er dessverre for få, og denne",
                          " rapportperioden må derfor utgå.", 
                          pre = "OBS: ", linjer.under = 1, ut = TRUE))
        }
      }
      rappAar <- rappAar %-% fjernAar
      rapportaar  <- rapportaar  %-% fjernAar
    }
    maaling$per <- as.factor(maaling$per)
    
    # Oppsummering av innlesinga
    if (length(unique(maaling$vfo)) < maalingTot) {
      OK <- FALSE
      skriv("De foreliggende målingene fra ", length(unique(maaling$vfo)),
            " vannforekomster er dessverre for få til å tilpasse noen modell!",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    } else {
      u <- c(
        u,
        skriv("Dataene som inngår i modelltilpasninga, inneholder dermed",ut=TRUE),
        skriv(nrow(maaling), " målinger fra", pre = "- ", ut = TRUE),
        skriv(length(unique(maaling$lok)), " vannlokaliteter i", pre = "- ", ut=T),
        skriv(length(unique(maaling$vfo)), " vannforekomster i", pre = "- ", ut=T),
        skriv(length(unique(fylke(unlist(strsplit(maaling$fyl, ","))) %A% FYL)), 
                     " fylker", pre = "- ", ut = TRUE),
        skriv("mellom ", min(maaling$aar), " og ", max(maaling$aar), ".", 
              pre = "- ", linjer.under = 1, ut = TRUE)
      )
      
      # Mer informasjon?
      # - landsdelsvis fordeling (for Norge og landsdel) [per år?]
      # - antall fylker per landsdeler (hvis ikke bare nasjonalt!)
      # - minste antall vannforekomster med målinger i fylket per landsdel
      # 
      #if (rapportenhet %inneholder% "landsdel" |
      #    rapportenhet %inneholder% "norge") {
      #  
      #  Mfyl <- rep(0, max(FNR))
      #  for (i in 1:max(FNR)) {
      #    fyn <- ifelse(i < 10, "0", "") %+% i %+% "00"
      #    Mfyl[i]<-length(unique(maaling$vfo[which(maaling$fyl %inneholder% fyn)]))
      #  }
      #  # må tilpasses:
      #  Mdel[1] <- sum(Mfyl[ 1:7 ])
      #  Mdel[2] <- sum(Mfyl[ 8:10])
      #  Mdel[3] <- sum(Mfyl[11:14])
      #  Mdel[4] <- sum(Mfyl[15:17])
      #  Mdel[5] <- sum(Mfyl[18:20])
      #  
      #  skriv("Målinger per landsdel:")
      #  names(Mdel) <- c("Øst", "Sør", "Vest", "Midt", "Nord")
      #  print(Mdel)
        
    }
  }
  
  if (OK) {
    E <- "mEQR-"
    if ("nEQR" %begynner% EQR) E <- "nEQR-"
    
    ##################################################################
    skriv("Skalering til ",E,"verdier", pre = "   ", linjer.over  = 1)
    skriv("==========================", pre = "   ", linjer.under = 1)

    if (is.null(enhet)) {
      mEnhet <- ":"
    } else {
      mEnhet <- " (målt i " %+% enhet %+% "):"
    }
    u <- c(u, skriv("Oppsummering av variabelverdier før skalering", mEnhet,
                    ut = TRUE))
    o <- oppsummer(maaling$vrd)
    cat(o[1], "\n")
    cat(o[2], "\n")
    u <- c(u, o)
    
    # Spesialbehandling for Raddum I! (del 1 av 2)
    if (parameter == "RADDUM1") {
      maaling$vrd <- ifelse(maaling$ant == 1,
                     ifelse(maaling$vrd > 0.501, 1,
                     ifelse(maaling$vrd > 0.251, 0.5,
                     ifelse(maaling$vrd > 0.001, 0.25, 0))), maaling$vrd)
      maaling$vrd[which(maaling$vrd > 1)] <- 1
      for (i in 1:nrow(maaling)) {
        if (maaling$ant[i] %=% 1) {
          maaling$ant[i] <- 1 + (length(which(maaling$lok==maaling$lok[i] &
                                                maaling$aar==maaling$aar[i])) > 1)
        }
        maaling$vrd[i] <- maaling$vrd[i] + (maaling$ant[i] > 1) / 10000
      }
    }
    
    # Skalering
    minV <- Inf
    maxV <- -Inf
    if (dim(KlasseGrenser) %=% c(1, 8)) {
      # parametere som har de samme terskelverdiene for alle vanntyper:
      K <- as.vector(unlist(KlasseGrenser[1, ]))
      maaling$vrd <- mEQR(maaling$vrd, K)
      minV <- mEQR(K[1], K)
      maxV <- mEQR(K[8], K)
    } else {
      if (nrow(KlasseGrenser) < pi) {
        # parametere som har de samme terskelverdiene 
        # for alle vanntyper innafor vannkategorien:
        VT <- substr(maaling$typ, 1, 1)
        for (v in unique(VT)) {
          K <- as.vector(unlist(KlasseGrenser[toupper(substr(v, 1, 1)), ]))
          maaling$vrd[which(VT == v)] <- mEQR(maaling$vrd[which(VT == v)], K)
          minV <- min(minV, mEQR(K[1], K))
          maxV <- max(maxV, mEQR(K[8], K))
        }
      } else {
        # resten, dvs. parametere der terskelverdiene varierer mellom vanntyper:
        for (v in unique(maaling$typ)) {
          K <- as.vector(unlist(KlasseGrenser[v, ]))
          maaling$vrd[which(maaling$typ == v)] <- 
            mEQR(maaling$vrd[which(maaling$typ == v)], K)
          minV <- min(minV, mEQR(K[1], K))
          maxV <- max(maxV, mEQR(K[8], K))
        }
      }
    }
    
    # Spesialbehandling for Raddum I! (del 2 av 2)
    if (parameter == "RADDUM1") {
      maaling$vrd[which(maaling$vrd >= 0.8)] <- 0.9
    }
    
    u <- c(u, skriv("Oppsummering av variabelverdier etter skalering:", 
                    linjer.over = 1, ut = TRUE))
    o <- oppsummer(maaling$vrd)
    cat(o[1], "\n")
    cat(o[2], "\n")
    u <- c(u, o)
    if (logit) {
      maaling$vrd <- skaler(maaling$vrd, minV, maxV)
    }
  }

  # dett var dett
  
  if (OK) {
    
    ######################################################################
    skriv("Modelltilpasning til målingene", pre = "   ", linjer.over  = 2)
    skriv("==============================", pre = "   ", linjer.under = 1)

    w <- which(!(toupper(maaling$akt) %in% toupper(Aktiviteter$id)))
    if (aktivitetsvekt > 1 & length(w) > 0) {
      w <- toupper(unique(maaling$akt[w]))
      for (i in 1:length(w)) {
        Aktiviteter <- rbind(Aktiviteter, list(w[i], "?", 0))
        rownames(Aktiviteter)[nrow(Aktiviteter)] <- w[i]
      }
      u <- c(u, skriv("Noen målinger er foretatt i sammenheng med overvåkings",
                      "aktiviteter som ikke har fått tildelt noen aktivitetsvekt ",
                      "ennå. Dette gjelder ",
                      paste("\"" %+% w %+% "\"", collapse=", "), 
                      ". De respektive skårene har blitt satt til 0. ",
                      "Hvis det blir feil, må dette rettes opp!",
                      pre = "OBS: ", linjer.under = 1, ut = TRUE))
    }
    
    # Holde styr på variablene og deres mulige verdier
    Variabler <- Variabler %-% tolower(ignorerVariabel)
    RF1 <- append(list(akt = sort(unique(maaling$akt)), smvf = c("ja", "nei")),
                  Vanntyper.nominal)
    RF1 <- RF1.sik <- RF1a <- RF1[-which(!(names(RF1) %in% Variabler))]
    RF2 <- append(list(reg = Vanntyper.ordinal[["reg" %+% vannkategori]]), 
                  Vanntyper.ordinal)
    RF2 <- RF2.sik <- RF2a <- RF2[-which(!(names(RF2) %in% Variabler))]
    VN1 <- c(akt = "Aktivitet", smvf = "SMVF", Typologi.nominal)
    VN2 <- Typologi.ordinal
    VN3 <- Typologi.numerisk
    TV2 <- Tallverdier
    for (i in 1:length(RF1a)) RF1a[[i]] <- character(0)
    for (i in 1:length(RF2a)) RF2a[[i]] <- character(0)

    # Ta sikkerhetskopi (lurt å ha ved feilsøking ...)
    maaling.sik <- maaling
    
    # Rydd opp i de ulike størrelsesmålene
    if (vannkategori %=% "L") {
      maaling$lengd <- maaling$areal
      maaling$areal <- maaling$A_tot
      Areal <- Vf$areal
    }
    if (vannkategori %=% "R") {
      Areal <- Vf$lengd
    }
    if (vannkategori %=% "C") {
      maaling$lengd <- maaling$areal
      Areal <- Vf$areal
    } # Heretter er størrelsesmålet (areal eller lengde) lagra som "lengd"!
    
    # Forbered numeriske variabler
    numVar <- names(ord2num) %A% Variabler
    if (length(which( is.na(maaling$gbred))) < nrow(maaling) / 10) {
      Vf$gbred <- Vf$lat
      numVar <- numVar %-% "reg"
    }
    if (length(which( is.na(maaling$høyde))) < nrow(maaling) / 10) {
      maaling$høyde <- sqrt(maaling$høyde) # kvadratrota av høyde over havet
      Vf$høyde <- sqrt(Vf$hoh)
      numVar <- numVar %-% "son"
    }
    if (length(which( is.na(maaling$areal))) < nrow(maaling) / 10 &
        vannkategori %=% "L") {
      maaling$areal <-   lg(maaling$areal) # dekadisk logaritme av innsjøareal
      Vf$areal <- lg(Vf$artot)
      numVar <- numVar %-% "stø"
    }
    for (typ in numVar) {
      for (i in 1:length(RF2[[typ]])) {
        maaling[which(maaling[, typ] == RF2[[typ]][i]), ord2num[typ]] <-
            TV2[[typ]][i]
        Vf     [which(     Vf[, typ] == RF2[[typ]][i]), ord2num[typ]] <-
            TV2[[typ]][i]
      }
    }

    # Tilpass SMVF i "Vf"
    Vf$smvf <- ifelse(Vf$smvf, "ja", "nei")
    Vf$smvf[    is.na(Vf$smvf)] <- "nei"

    # Fjern typologifaktorer som ikke er oppgitt
    husk <- list()
    for (typ in Variabler %-% "akt") {
      w <- which(is.na(maaling[, typ]))
      if (length(w)) {
        if (length(w) > nrow(maaling) * 0.1) {
          Variabler <- Variabler %-% typ
          u <- c(u, skriv("Typologifaktoren \"", tolower(Typologi[typ]), 
                          "\" mangla såpass ofte (", length(w), 
                          " ganger) at den ignoreres.", 
                          pre = "OBS: ", linjer.under = 1, ut = TRUE))
        } else {
          if (length(unique(maaling[, typ])) > 2) {
            husk[[typ]] <- maaling[+w, ]
            maaling     <- maaling[-w, ]
            u <- c(u, skriv(length(w), " målinger ble ekskludert fordi ",
                            "typologifaktoren \"", tolower(Typologi[typ]), 
                            "\" ikke var kjent for dem.", 
                            pre = "OBS: ", linjer.under = 1, ut = TRUE))
          }
        }
      }
    }

    # Sett opp modellformelen
    formel. <- character(0)
    if (length(unique(maaling$per)) > 1) formel. <- c(formel., "per")
    if (length(unique(maaling$rar)) > 2) formel. <- c(formel., "rar")
    if (length(formel.) < 1) {
      OK <- FALSE
      skriv("Det er totalt sett for få år med data til å tilpasse en modell!",
            pre = "FEIL:", linjer.over = 1, linjer.under = 1)
    }
    formel. <- paste(c(formel., Variabler), collapse = " + ")
    formel. <- erstatt(formel., "per + rar", "per * rar")
    
    # Beregn vekting for målingene
    maaling$vkt <- maaling$ant^antallvekt * tidsvekt^maaling$rar *
      as.vector(aktivitetsvekt^(-abs(Aktiviteter[maaling$akt, "skaar"])))
  }
  
  if (OK) {
    f <- function(x) as.formula("vrd ~ " %+% x)
    if (any(is.na(maaling$akt))) {
      maaling$akt[which(is.na(maaling$akt))] <- "NA!!"
    }
    runde <- 0
    RF12 <- list(NULL, NULL)
    endring <- TRUE

    while (endring) { ####################### Modelltilpasning
      runde <- runde + 1
      endring <- FALSE
      RF12 <- list(RF1, RF2)
      if (vis) {
        u <- c(u, skriv("Modelltilpasning, runde ", runde, ":", 
                        linjer.over = 1, linjer.under = 1, ut = TRUE))
      }
      
      # tomme variabler ----------------------------------------------
      REKKEF <- RF1
      VNAVN  <- VN1
      formel <- formel. <- formel.. <- erstatt(formel., ".", "")
      vliste <- names(VN1) %A% unlist(strsplit(formel., " "))
      for (vrb in vliste) {
        endra <- FALSE
        vrb. <- vrbSIK <- maaling[ , vrb]
        formel.. <- erstatt(formel., vrb, vrb %+% "..")
        formel.  <- erstatt(formel., vrb, vrb %+% ".")
        vrber <- rekke <- REKKEF[[vrb]]
        for (i in length(vrber):1) {
          if (!(vrber[i] %in% vrb.)) {
            vrber <- rekke <- vrber[-i]
          }
        }
        vnavn <- VNAVN[[vrb]]
        if (length(vrber) < 2) {
          formel. <- erstatt(formel., " + " %+% vrb %+% ".", "")
          formel. <- erstatt(formel., vrb %+% "." %+% " + ", "")
          vrber <- character()
          endra <- TRUE
          if (vis) {
            u <- c(u, skriv(vnavn, " har blitt droppa fordi alle data var fra én ",
                            "klasse.", pre = "* ", ut = TRUE))
          }
        } else {
          if (length(which(vrb. %in% vrber)) < 2 * maalingTyp) {
            formel. <- erstatt(formel., " + " %+% vrb %+% ".", "")
            formel. <- erstatt(formel., vrb %+% "." %+% " + ", "")
            vrber <- character()
            endra <- TRUE
            if (vis) {
              u <- c(u, skriv(vnavn, "\ har blitt droppa pga. for lite data.", 
                              pre = "* ", ut = TRUE))
            }
          } else {
            if (!all(vrb. %in% vrber)) {
              natall <- length(which(!(vrb. %in% vrber)))
              if (natall > max(table(vrb.))) {
                formel. <- erstatt(formel., " + " %+% vrb %+% ".", "")
                formel. <- erstatt(formel., vrb %+% "." %+% " + ", "")
                vrber <- character()
                endra <- TRUE
                if (vis) {
                  u <- c(u, skriv(vnavn, " har blitt droppa fordi variabelen ",
                                  "hadde for mange manglende verdier.", 
                                  pre = "* ", ut = TRUE))
                }
              } else {
                vrb.[which(!(vrb. %in% vrber))] <- RF1a[[vrb]] <-
                  names(table(vrb.))[which(table(vrb.) == max(table(vrb.)))]
              }
            }
          }
        }
        endring <- endring | endra
        if (endra) {
          assign(vrb %+% ".", vrb.)
          maaling[,vrb] <- vrb.
          REKKEF[[vrb]] <- vrber
          RF1 <- REKKEF
        }
      }
      REKKEF <- RF2
      VNAVN  <- VN2
      vliste <- names(VN2) %A% unlist(strsplit(formel., " "))
      for (vrb in vliste) {
        endra <- FALSE
        vrb. <- vrbSIK <- maaling[ , vrb]
        formel.. <- erstatt(formel., vrb, vrb %+% "..")
        formel.  <- erstatt(formel., vrb, vrb %+% ".")
        vrber <- rekke <- REKKEF[[vrb]]
        for (i in length(vrber):1) {
          if (!(vrber[i] %in% vrb.)) {
            vrber <- rekke <- vrber[-i]
          }
        }
        vnavn <- VNAVN[[vrb]]
        if (length(vrber) < 2) {
          formel. <- erstatt(formel., " + " %+% vrb %+% ".", "")
          formel. <- erstatt(formel., vrb %+% "." %+% " + ", "")
          vrber <- character()
          endra <- TRUE
          if (vis) {
            u <- c(u, skriv(vnavn, " har blitt droppa fordi alle data var fra én ",
                            "klasse.", pre = "* ", ut = TRUE))
          }
        } else {
          if (length(which(vrb. %in% vrber)) < 2 * maalingTyp) {
            formel. <- erstatt(formel., " + " %+% vrb %+% ".", "")
            formel. <- erstatt(formel., vrb %+% "." %+% " + ", "")
            vrber <- character()
            endra <- TRUE
            if (vis) {
              u <- c(u, skriv(vnavn, " har blitt droppa pga. for lite data.", 
                              pre = "* ", ut = TRUE))
            }
          } else {
            if (!all(vrb. %in% vrber)) {
              natall <- length(which(!(vrb. %in% vrber)))
              if (natall > max(table(vrb.))) {
                formel. <- erstatt(formel., " + " %+% vrb %+% ".", "")
                formel. <- erstatt(formel., vrb %+% "." %+% " + ", "")
                vrber <- character()
                endra <- TRUE
                if (vis) {
                  u <- c(u, skriv(vnavn, " har blitt droppa fordi variabelen ",
                                  "hadde for mange manglende verdier.", 
                                  pre = "* ", ut = TRUE))
                }
              } else {
                vrb.[which(!(vrb. %in% vrber))] <- RF2a[[vrb]] <-
                  names(table(vrb.))[which(table(vrb.) == max(table(vrb.)))]
              }
            }
          }
        }
        endring <- endring | endra
        if (endra) {
          assign(vrb %+% ".", vrb.)
          maaling[,vrb] <- vrb.
          REKKEF[[vrb]] <- vrber
          RF2 <- REKKEF
        }
      }
      
      # nominale variabler ------------------------------------------
      REKKEF <- RF1
      VNAVN  <- VN1
      formel <- formel. <- formel.. <- erstatt(formel., ".", "")
      vliste <- names(VN1) %A% unlist(strsplit(formel., " "))
      if (!is.null(fastVariabel)) {
        for (i in tolower(fastVariabel)) {
          if (i %in% vliste) {
            vliste <- vliste[-which(vliste == i)]
          }
        }
      }
      for (vrb in vliste) {
        endra <- FALSE
        vrb. <- vrbSIK <- maaling[ , vrb]
        formel.. <- erstatt(formel., vrb, vrb %+% "..")
        formel.  <- erstatt(formel., vrb, vrb %+% ".")
        vrber <- rekke <- REKKEF[[vrb]]
        for (i in length(vrber):1) {
          if (!(vrber[i] %in% vrb.)) {
            vrber <- rekke <- vrber[-i]
          }
        }
        vnavn <- VNAVN[[vrb]]
        lengde <- length(vrber)
        if (lengde > 1 & sum(table(vrb.)) >= 2 * maalingTyp) {
          while (sort(table(vrb.))[[1]] < maalingTyp) { # hvis det er få datapunkt,
            pm1 <- names(sort(table(vrb.)))[1]          # må det slås sammen
            aic <- Inf
            VLI <- list(vrb.)
            tekst <- ""
            for (pm2 in names(sort(table(vrb.))[-1])) {
              vrb.. <- vrb.
              vrb..[which(vrb.. == pm1 | vrb.. == pm2)] <- pm1 %pluss% pm2
              VLI[[length(VLI) + 1]] <- vrb..
              assign(vrb %+% "..", vrb..)
              if (length(vrber) %=% 2) {
                aic <- c(aic, -Inf)
              } else {
                aic <- c(aic, AIC(lm(f(formel..), data=maaling, weights=vkt)))
              }
              tekst <- c(tekst, pm1 %+% " og " %+% pm2)
            }
            lav <- order(aic)[1]
            if (aic[lav] < aic[1] + DeltaAIC) {
              vrb. <- VLI[[lav]]
              pm12 <- sort(unlist(strsplit(tekst[lav], " og ")))
              vrber <- vrber %-% pm12
              vrber <- sort(unique(c(vrber, pm12[1] %pluss% pm12[2])))
              endra <- TRUE
              if (vis && nchar(tekst[lav])) {
                u <- c(u, skriv(vnavn, ": ", tekst[lav],
                          " har blitt slått sammen pga. for lite data.", 
                          pre = "* ", ut = TRUE))
              }
            }
          }
        }
        if (lengde > 1 & length(vrber) < 2) {
          formel. <- erstatt(formel., " + " %+% vrb %+% ".", "")
          formel. <- erstatt(formel., vrb %+% "." %+% " + ", "")
          vrber <- character()
          endra <- TRUE
          if (vis) {
            u <- c(u, skriv(vnavn," har blitt droppa fordi nesten alle data var ",
                            "fra én klasse.", pre = "* ", ut = TRUE))
          }
        }
        lengde <- length(unique(vrb.))
        if (lengde > 1 & sum(table(vrb.)) >= 2 * maalingTyp) {
          if (interaksjon) {
            if (formel. %inneholder% (vrb %+% ". * per")) {
              formel... <- erstatt(formel., vrb %+% ". * per", vrb %+% ".")
              if (AIC(lm(f(formel...), data=maaling, weights=vkt)) <
                  AIC(lm(f(formel.),   data=maaling, weights=vkt)) + DeltaAIC) {
                formel. <- formel...
                endra <- TRUE
                if (vis) {
                  u <- c(u, skriv(vnavn, ": interaksjonen med rapporteringsperiode",
                                  "har blitt droppa igjen.", pre = "* ", ut = TRUE))
                }
              }
            } else {
              assign(vrb %+% ".", vrb.)
              m <- lm(f(formel.), data=maaling, weights=vkt)
              if (m$df / m$rank                               >= maalingInt * 4 &
                  all(table(maaling[, "per"], maaling[, vrb]) >= maalingInt)) {
                formel... <- erstatt(formel., vrb %+% ".", vrb %+% ". * per")
                if (AIC(lm(f(formel...), data=maaling, weights=vkt)) <
                    AIC(lm(f(formel.),   data=maaling, weights=vkt)) - DeltaAIC) {
                  formel. <- formel...
                  endra <- TRUE
                  if (vis) {
                    u <- c(u, skriv(vnavn, " har en interaksjon med rapporterings",
                                    "periode.", pre = "* ", ut = TRUE))
                  }
                }
              }
            }
          }
          L <- c(length(unique(vrb.)), Inf)
          while (L[1] > 1 & L[2] > L[1]) {
            VLI <- list(vrb.)
            assign(vrb %+% ".", vrb.)
            aic <- AIC(lm(f(formel.), data=maaling, weights=vkt))
            tekst <- ""
            for (i in 2:length(vrber)) {
              for (j in 1:(i-1)) {
                pm1 <- vrber[i]
                pm2 <- vrber[j]
                vrb.. <- vrb.
                vrb..[which(vrb.. == pm1 | vrb.. == pm2)] <- pm1 %pluss% pm2
                VLI[[length(VLI) + 1]] <- vrb..
                formel... <- formel..
                if (length(unique(vrb..)) == 1) {
                  formel... <- erstatt(formel..., " + " %+% vrb %+% ".. * per", "")
                  formel... <- erstatt(formel..., vrb %+% ".. * per" %+% " + ", "")
                  formel... <- erstatt(formel..., " + " %+% vrb %+% "..", "")
                  formel... <- erstatt(formel..., vrb %+% ".." %+% " + ", "")
                }
                assign(vrb %+% "..", vrb..)
                aic <- c(aic, AIC(lm(f(formel...), data=maaling, weights=vkt)))
                tekst <- c(tekst, pm2 %+% " og " %+% pm1)
              }
            }
            lav <- order(aic)[1]
            if (lav %=% 1) {
              lav <- order(aic)[2]
            }
            if (aic[lav] < aic[1] + DeltaAIC) {
              vrb. <- VLI[[lav]]
              if (nchar(tekst[lav])) {
                pm12 <- sort(unlist(strsplit(tekst[lav], " og ")))
                vrber <- vrber %-% pm12
                vrber <- sort(unique(c(vrber, pm12[1] %pluss% pm12[2])))
                endra <- TRUE
                if (vis) {
                  u <- c(u, skriv(vnavn, ": ", tekst[lav], " har blitt slått ",
                                  "sammen.", pre = "* ", ut = TRUE))
                }
              }
            }
            L <- c(length(unique(vrb.)), L)
          }
        }
        if (length(unique(vrb.)) %=% 1 & lengde > 1) {
          formel. <- erstatt(formel., " + " %+% vrb %+% ". * per", "")
          formel. <- erstatt(formel., vrb %+% ". * per" %+% " + ", "")
          formel. <- erstatt(formel., " + " %+% vrb %+% ".", "")
          formel. <- erstatt(formel., vrb %+% "." %+% " + ", "")
          vrber <- character()
          endra <- TRUE
          if (vrb %in% names(husk)) {
            maaling     <- rbind(maaling, husk[[vrb]])
          }
          if (vis) {
            u <- c(u, skriv(vnavn, " har blitt droppa fordi det ikke var ",
                            "forskjell mellom klassene.", pre = "* ", ut = TRUE))
          }
        }
        endring <- endring | endra
        if (!endra & vis) {
          u <- c(u, skriv(vnavn, " har blitt beholdt uendra (med ", 
                          length(unique(vrb.)), " ulike verdier).", 
                          pre = "* ", ut = TRUE))
        }
        assign(vrb %+% ".", vrb.)
        maaling[,vrb] <- vrb.
        REKKEF[[vrb]] <- vrber
        RF1 <- REKKEF
      }
      
      # ordinale variabler ---------------------------------
      REKKEF <- RF2
      VNAVN  <- VN2
      vliste <- names(VN2) %A% unlist(strsplit(formel., " "))
      if (!is.null(fastVariabel)) {
        for (i in tolower(fastVariabel)) {
          if (i %in% vliste) {
            vliste <- vliste[-which(vliste == i)]
          }
        }
      }
      for (vrb in vliste) {
        endra <- FALSE
        vrb. <- vrbSIK <- maaling[ , vrb]
        formel.. <- erstatt(formel., vrb, vrb %+% "..")
        formel.  <- erstatt(formel., vrb, vrb %+% ".")
        vrber <- rekke <- REKKEF[[vrb]]
        blittNumerisk <- FALSE
        for (i in length(vrber):1) {
          if (!(vrber[i] %in% vrb.)) {
            vrber <- rekke <- vrber[-i]
          }
        }
        vnavn <- VNAVN[[vrb]]
        lengde <- length(vrber)
        if (lengde > 1 & length(which(vrb. %in% vrber)) >= 2 * maalingTyp) {
          while (length(which(vrb. == vrber[1])) < maalingTyp) { # her slås klasser 
                                         # som har få lite data, sammen fra venstre
            vrb.[which(vrb. %in% vrber[1:2])] <- paste(vrber[1:2], collapse="+")
            endra <- TRUE
            if (vis) {
              u <- c(u, skriv(vnavn, ": ", paste(vrber[1:2], collapse=" og "),
                              " har blitt slått sammen pga. for lite data.", 
                              pre = "* ", ut = TRUE))
            }
            vrber[2] <- paste(vrber[1:2], collapse="+")
            vrber <- vrber[-1]
          }
          while (length(which(vrb. == vrber[length(vrber)])) < maalingTyp) { # det 
                                                                 # samme fra høyre
            vrb.[which(vrb. %in% vrber[length(vrber) - 1:0])] <-
              paste(vrber[length(vrber) - 1:0], collapse="+")
            endra <- TRUE
            if (vis) {
              u <- c(u, skriv(vnavn, ": ", 
                              paste(vrber[length(vrber) - 1:0], collapse=" og "),
                              " har blitt slått sammen pga. for lite data.", 
                              pre = "* ", ut = TRUE))
            }
            vrber[length(vrber) - 1] <- paste(vrber[length(vrber) - 1:0], 
                                              collapse = "+")
            vrber <- vrber[-length(vrber)]
          }
        }
        if (length(which(vrb. %in% vrber)) >= 2 * maalingTyp) {
          while (length(vrber) > 1 & any(table(vrb.) < maalingTyp)) { # det samme 
            w <- names(which(table(vrb.) < maalingTyp)[1])            # i midten
            vrb.. <- vrb.
            hvilk1 <- which(vrber == w)
            en <- 1
            while (length(which(vrb.. == w)) < maalingTyp) {
              vrb..[which(vrb.. == vrber[which(vrber == w) - en])] <- w
              hvilk1 <- c(hvilk1[1] - en, hvilk1)
              en <- en + 1
            }
            assign(vrb %+% "..", vrb..)
            aic1 <- AIC(lm(f(formel..), data=maaling, weights=vkt))
            vrb.. <- vrb.
            hvilk2 <- which(vrber == w)
            en <- 1
            while (length(which(vrb.. == w)) < maalingTyp) {
              vrb..[which(vrb.. == vrber[which(vrber == w) + en])] <- w
              hvilk2 <- c(hvilk2,  hvilk2[length(hvilk2)] + en)
              en <- en + 1
            }
            assign(vrb %+% "..", vrb..)
            aic2 <- AIC(lm(f(formel..), data=maaling, weights=vkt))
            if (aic1 < aic2) {
              vrb.[which(vrb. %in% vrber[hvilk1])] <- paste(vrber[hvilk1], 
                                                            collapse = "+")
              endra <- TRUE
              if (vis) {
                u <- c(u, skriv(vnavn, ": ", paste(vrber[hvilk1], collapse=" og "),
                                " har blitt slått sammen pga. for lite data.", 
                                pre = "* ", ut = TRUE))
              }
              vrber[max(hvilk1)] <- paste(vrber[hvilk1], collapse="+")
              vrber <- vrber[-hvilk1[1:(length(hvilk1) - 1)]]
            } else {
              vrb.[which(vrb. %in% vrber[hvilk2])] <- paste(vrber[hvilk2], 
                                                            collapse = "+")
              endra <- TRUE
              if (vis) {
                u <- c(u, skriv(vnavn, ": ", paste(vrber[hvilk2], collapse=" og "),
                                " har blitt slått sammen pga. for lite data.", 
                                pre = "* ", ut = TRUE))
              }
              vrber[hvilk2[1]] <- paste(vrber[hvilk2], collapse="+")
              vrber <- vrber[-hvilk2[2:length(hvilk2)]]
            }
          }
        }
        if (lengde > 1 & length(vrber) < 2) {
          formel. <- erstatt(formel., " + " %+% vrb %+% ".", "")
          formel. <- erstatt(formel., vrb %+% "." %+% " + ", "")
          vrber <- character()
          endra <- TRUE
          if (vis) {
            u <- c(u, skriv(vnavn," har blitt droppa fordi nesten alle data var ",
                            "fra én klasse.", pre = "* ", ut = TRUE))
          }
        }
        lengde <- length(unique(vrb.))
        if (length(vrber) > 1 & length(which(vrb. %in% vrber)) >= 2 * maalingTyp) {
          if (interaksjon) {
            if (formel. %inneholder% (vrb %+% ". * per")) {
              formel... <- erstatt(formel., vrb %+% ". * per", vrb %+% ".")
              if (AIC(lm(f(formel...), data=maaling, weights=vkt)) <
                  AIC(lm(f(formel.),   data=maaling, weights=vkt)) + DeltaAIC) {
                formel. <- formel...
                endra <- TRUE
                if (vis) {
                  u <- c(u, skriv(vnavn, ": interaksjonen med rapporteringsperiode",
                                  "har blitt droppa igjen.", pre = "* ", ut = TRUE))
                }
              }
            } else {
              assign(vrb %+% ".", vrb.)
              m <- lm(f(formel.), data=maaling, weights=vkt)
              if (m$df / m$rank                               >= maalingInt * 4 &
                  all(table(maaling[, "per"], maaling[, vrb]) >= maalingInt)) {
                formel... <- erstatt(formel., vrb %+% ".", vrb %+% ". * per")
                if (AIC(lm(f(formel...), data=maaling, weights=vkt)) <
                    AIC(lm(f(formel.),   data=maaling, weights=vkt)) - DeltaAIC) {
                  formel. <- formel...
                  endra <- TRUE
                  if (vis) {
                    u <- c(u, skriv(vnavn, " har en interaksjon med rapporterings",
                                    "periode.", pre = "* ", ut = TRUE))
                  }
                }
              }
            }
          }
          L <- c(length(unique(vrb.)), Inf)
          while (L[1] > 1 & L[2] > L[1]) {
            VLI <- list(vrb.)
            assign(vrb %+% ".", vrb.)
            aic <- AIC(lm(f(formel.), data=maaling, weights=vkt))
            tekst <- ""
            for (j in 2:length(vrber)) {
              i <- vrber[j-1]
              j <- vrber[j]
              vrb.. <- vrb.
              vrb..[which(vrb.. == i | vrb.. == j)] <- i %+% "+" %+% j
              VLI[[length(VLI) + 1]] <- vrb..
              formel... <- formel..
              if (length(unique(vrb..)) == 1) {
                formel... <- erstatt(formel..., " + " %+% vrb %+% ".. * per", "")
                formel... <- erstatt(formel..., vrb %+% ".. * per" %+% " + ", "")
                formel... <- erstatt(formel..., " + " %+% vrb %+% "..", "")
                formel... <- erstatt(formel..., vrb %+% ".." %+% " + ", "")
              }
              assign(vrb %+% "..", vrb..)
              aic <- c(aic, AIC(lm(f(formel...), data=maaling, weights=vkt)))
              tekst <- c(tekst, i %+% " og " %+% j)
            }
            lav <- order(aic)[1]
            if (lav %=% 1) {
              lav <- order(aic)[2]
            }
            if (aic[lav] < aic[1] + DeltaAIC) {
              vrb. <- VLI[[lav]]
              if (nchar(tekst[lav])) {
                vrber[lav-1] <- vrber[lav-1] %+% "+" %+% vrber[lav]
                vrber <- vrber[-lav]
                endra <- TRUE
                if (vis && nchar(tekst[lav])) {
                  u <- c(u, skriv(vnavn, ": ", tekst[lav], 
                                  " har blitt slått sammen.", 
                                  pre = "* ", ut = TRUE))
                }
              }
            }
            L <- c(length(unique(vrb.)), L)
          }
          formel.. <- erstatt(formel., vrb %+% ".", ord2num[vrb])
          dA <- DeltaAIC * sign(length(unique(vrb.)) - 2)
          if (AIC(lm(f(formel..), data = maaling, weights = vkt)) < 
              AIC(lm(f(formel.),  data = maaling, weights = vkt)) + dA) {
            formel. <- formel..
            vrber <- numeric()
            endra <- TRUE
            blittNumerisk <- TRUE
            if (vrb %in% names(husk)) {
              maaling     <- rbind(maaling, husk[[vrb]])
            }
            if (vis) {
              if (vrb %in% numVar) {
                u <- c(u, skriv(vnavn, " har blitt omgjort til en numerisk ",
                                "variabel.", pre = "* ", ut = TRUE))
              } else {
                u <- c(u, skriv(vnavn, " har blitt erstatta med faktisk ",
                                tolower(VN3[ord2num[vrb]]), ".", 
                                pre = "* ", ut = TRUE))
              }
            }
          }
        }
        if (length(unique(vrb.)) %=% 1 & lengde > 1 & !blittNumerisk) {
          formel. <- erstatt(formel., " + " %+% vrb %+% ". * per", "")
          formel. <- erstatt(formel., vrb %+% ". * per" %+% " + ", "")
          formel. <- erstatt(formel., " + " %+% vrb %+% ".", "")
          formel. <- erstatt(formel., vrb %+% "." %+% " + ", "")
          vrber <- character()
          endra <- TRUE
          if (vrb %in% names(husk)) {
            maaling     <- rbind(maaling, husk[[vrb]])
          }
          if (vis) {
            u <- c(u, skriv(vnavn, " har blitt droppa fordi det ikke var ",
                            "forskjell mellom klassene.",
                            pre = "* ", ut = TRUE))
          }
        }
        endring <- endring | endra
        if (!endra & vis) {
          u <- c(u, skriv(vnavn, " har blitt beholdt uendra (med ", 
                          length(unique(vrb.)), " ulike verdier).", 
                          pre = "* ", ut = TRUE))
        }
        assign(vrb %+% ".", vrb.)
        maaling[,vrb] <- vrb.
        REKKEF[[vrb]] <- vrber
        RF2 <- REKKEF
      }
      
      # numeriske variabler ------------------------------------
      if (runde > 1) {
        VNAVN  <- VN3
        vliste <- names(VN3) %A% unlist(strsplit(formel., " "))
        if (!is.null(fastVariabel)) {
          for (i in tolower(fastVariabel)) {
            if (ord2num[i] %in% vliste) {
              vliste <- vliste[-which(vliste == ord2num[i])]
            }
          }
        }
        for (vrb in vliste) {
          endra <- FALSE
          vnavn <- VNAVN[[vrb]]
          if (interaksjon) {
            if (formel. %inneholder% (vrb %+% " * per")) {
              formel... <- erstatt(formel., vrb %+% " * per", vrb)
              if (AIC(lm(f(formel...), data=maaling, weights=vkt)) <
                  AIC(lm(f(formel.),   data=maaling, weights=vkt)) + DeltaAIC) {
                formel. <- formel...
                endra <- TRUE
                if (vis) {
                  u <- c(u, skriv(vnavn, ": interaksjonen med rapporteringsperiode",
                                  "har blitt droppa igjen.", pre = "* ", ut = TRUE))
                }
              }
            } else {
              formel... <- erstatt(formel., vrb, vrb %+% " * per")
              if (AIC(lm(f(formel...), data=maaling, weights=vkt)) <
                  AIC(lm(f(formel.),   data=maaling, weights=vkt)) - DeltaAIC) {
                formel. <- formel...
                endra <- TRUE
                if (vis) {
                  u <- c(u, skriv(vnavn, " har en interaksjon med rapporterings",
                                  "periode.", pre = "* ", ut = TRUE))
                }
              }
            }
          }
          formel.. <- erstatt(formel.,  " + " %+% vrb %+% " * per", "")
          formel.. <- erstatt(formel.., vrb %+% " * per" %+% " + ", "")
          formel.. <- erstatt(formel.., " + " %+%  vrb , "")
          formel.. <- erstatt(formel..,  vrb %+% " + ", "")
          if (AIC(lm(f(formel..), data=maaling, weights=vkt)) < 
              AIC(lm(f(formel.),  data=maaling, weights=vkt)) - DeltaAIC) {
            formel. <- formel..
            vrber <- character()
            endra <- TRUE
            if (vis) {
              u <- c(u, skriv(vnavn, " har blitt droppa fordi variabelen ikke ",
                              "bidro nok til modellen.", pre = "* ", ut = TRUE))
            }
          }
          endring <- endring | endra
          if (!endra & vis) {
            u <- c(u, skriv(vnavn, " har blitt beholdt uendra (som numerisk ", 
                            "variabel).", pre = "* ", ut = TRUE))
          }
        }
      }
    }
    
    # oppsummer modellen ------------------------------------------
    formel <- erstatt(formel., ".", "")
    explv <- erstatt(formel, " * ", " ")
    explv <- erstatt(explv,  " + ", " ")
    explv <- unique(unlist(strsplit(explv, " ")))
    modell <- lm(f(formel), data=maaling, weights=vkt)
    sdrag <- summary(modell)
    u <- c(u, skriv("Oppsummering av den tilpassa modellen ...", 
                    linjer.over = 1, linjer.under = 1,  ut = TRUE))
    u <- c(u, skriv("Modelltype: ", "lineær regresjon", ut = TRUE))
    skriv(formel, pre = "Modellstruktur: vrd ~ ")
    u <- c(u,           "Modellstruktur: vrd ~ " %+% formel)
    u <- c(u, skriv("Residualer:", linjer.over = 1,     ut = TRUE))
    o <- oppsummer(sdrag$residuals)
    cat(o[1], "\n")
    cat(o[2], "\n")
    u <- c(u, o)
    u <- c(u, skriv("standardfeil: ", komma(round(sdrag$sigma, 3)), " med ",
                    sdrag$df[2], " frihetsgrader", ut = TRUE))
    if (vis) {
      skriv("Koeffisienter:", linjer.over = 1)
      koeff <- sdrag$coefficients
      rownames(koeff)[1] <- "(konstantledd)"
      colnames(koeff) <- c("estimat", "standardfeil", "t-verdi", "Pr(>|t|)")
      tabulerKoeffisienter(koeff)
    }
    u <- c(u, skriv("AIC = ", komma(round(AIC(modell), 2)), 
                    linjer.over = 1, ut = TRUE))
    u <- c(u, skriv("R² = ", komma(round(sdrag$r.squared, 4)), ut = TRUE))
    Fst <- sdrag$fstatistic
    sifre <- ifelse(Fst[1] > 1000, 0, ifelse(Fst[1] > 100, 1, 2))
    u <- c(u, skriv("F(", Fst[2], ", ", Fst[3], ") = ", 
                    komma(round(Fst[1], sifre)), ut = TRUE))
    pverdi <- pf(Fst[1], Fst[2], Fst[3], lower.tail = FALSE)
    pverdi <- format.pval(pverdi, 2, 1e-12, scientific = 2, decimal.mark = ",",
                          nsmall = ceiling(abs(lg(pverdi + 1e-15))) + 1)
    if (substr(pverdi, 1, 1) %=% "<") {
      if (substr(pverdi, 2, 2) %!=% " ") pverdi <- erstatt(pverdi, "<", "< ")
      pverdi <- "p "   %+% pverdi
    } else {
      pverdi <- "p = " %+% pverdi
    }
    pverdi <- erstatt(pverdi, "e", "E")
    u <- c(u, skriv(pverdi, linjer.under = 1, ut = TRUE))
  }
  
  if (OK) {
    
    #################################################################################
    skriv("Ekstrapolering til ikke-målte vannforekomster",pre = "   ",linjer.over =1)
    skriv("=============================================",pre = "   ",linjer.under=1)

    utvalg <- ta.med <- which(Vf$kat == vannkategori)
    txt <- c(ifelse(vannkategori %inneholder% "L", "innsjø", NA),
             ifelse(vannkategori %inneholder% "R", "elve",   NA),
             ifelse(vannkategori %inneholder% "C", "kyst",   NA))
    txt <- paste(na.omit(txt), collapse="- og ")
    u <- c(u, skriv("Det fins ", length(utvalg), " typifiserte ", txt, 
                    "vannforekomster.", ut = TRUE))
    if (!is.null(bareInkluder)) {
      ta.med <- numeric(0)
      if (is.list(bareInkluder)) {
        for (i in 1:length(bareInkluder$typ)) {
          ok <- which(Vf[, bareInkluder$typ[i]] == bareInkluder$vrd[i])
          if (length(ok)) {
            ta.med <- c(ta.med, ok)
          }
        }
      } else {
        ta.med <- ok <- which(Vf$typ %in% bareInkluder)
      }
    }
    forskj <- length(utvalg) - length(utvalg %A% ta.med)
    utvalg <- utvalg %A% ta.med
    if (!is.null(ikkeInkl)) {
      fjern <- numeric(0)
      for (i in 1:length(ikkeInkl$typ)) {
        fjern <- c(fjern, which(Vf[, ikkeInkl$typ[i]] == ikkeInkl$vrd[i]))
      }
      fjern <- unique(fjern)
      forskj <- forskj + length(which(fjern %in% utvalg))
      utvalg <- utvalg %-% fjern
    }
    ta.med <- utvalg
    fjern <- numeric(0)
    if (nrow(KlasseGrenser) > pi) {
      ta.med <- which(Vf$typ %in% 
                      rownames(KlasseGrenser)[which(!is.na(KlasseGrenser[, 7]))])
      forskj <- forskj + (length(utvalg) - length(utvalg %A% ta.med))
      utvalg <- utvalg %A% ta.med
    }
    if (forskj > 0) {
      u <- c(u, skriv("Av disse har ", length(utvalg), 
                      " vannforekomster en vanntype som parameteren ",
                      parameter, " er definert for.", ut = TRUE))
    }
    nydata <- matrix("", length(utvalg), length(explv), TRUE, 
                     list(Vf$id[utvalg], explv))
    nydata <- as.data.frame(nydata, stringsAsFactors = FALSE)
    hvilke <- 1:length(explv) %-% which(explv %in% c("per", "rar", "akt"))
    nydata[, explv[hvilke]] <- Vf[utvalg, explv[hvilke]]
    fjern <- numeric(0)
    beskjed <- NB <- c()
    for (i in names(RF1.sik) %-% "akt") { # nominale variabler
      if (i %in% names(RF1) && length(RF1[[i]])) {
        nydata[which(is.na(nydata[, i])), i] <- "<NA>"
        andre <- unique(nydata[, i]) %-% unique(maaling[, i])
        if   (length(andre)) {
          for (j in (andre)) {
            w <- which(RF1[[i]] %inneholder% j)
            if (length(w)) {
              if (i %in% fastVariabel) {
                fjern <- c(fjern, which(Vf[utvalg, i] == j))
                beskjed <- c(beskjed, length(which(Vf[utvalg, i] == j))    %+%
                  " vannforekomster er av typen \"" %+%  tolower(VN2[[i]]) %+% 
                  " = " %+%j%+% "\", som det ikke foreligger målinger av " %+%
                  parameter %+% " fra")
              } else {
                nydata[which(nydata[, i] == j), i] <- RF1[[i]][w]
              }
            } else {
              if (length(RF1a[[i]]) && any(RF1 %inneholder% RF1a[[i]])) {
                if (ekstrapol) {
                  nydata[which(nydata[, i] == j), i] <- RF1a[[i]]
                  NB <- c(NB, length(which(nydata[, i] == j)) %+% 
                            " vannforekomster som er av typen \"" %+% 
                            tolower(VN1[[i]]) %+% " = " %+% j %+% 
                            "\", antas å ligne på \"" %+% tolower(VN1[[i]]) %+%
                            " = " %+% RF1a[[i]][1] %+% "\"")
                } else {
                  fjern <- c(fjern, which(nydata[, i] == j))
                  if (j %in% RF1.sik[[i]]) {
                    beskjed[length(beskjed)+1] <- length(which(nydata[,i]==j)) %+%
                      " vannforekomster er av typen \""  %+% tolower(VN1[[i]]) %+% 
                      " = "%+% j%+% "\", som det ikke foreligger målinger av " %+%
                      parameter %+% " fra"
                  } else {
                    beskjed[length(beskjed)+1] <- length(which(nydata[,i]==j)) %+%
                      " vannforekomst"                                         %+% 
                      ifelse(length(which(nydata[, i] == j)) == 1, "", "er")   %+%
                      " har den ukjente vanntypen \"" %+% tolower(VN1[[i]])    %+% 
                      " = " %+% j %+% "\""
                  }
                }
              } else {
                fjern <- c(fjern, which(nydata[, i] == j))
                if (j %in% RF1.sik[[i]]) {
                  beskjed[length(beskjed) + 1] <- length(which(nydata[,i]==j)) %+%
                    " vannforekomster er av typen \""   %+%  tolower(VN1[[i]]) %+% 
                    " = " %+% j %+% "\", som det ikke foreligger målinger av " %+%
                    parameter %+% " fra"
                } else {
                  beskjed[length(beskjed) + 1] <- length(which(nydata[,i]==j)) %+%
                    " vannforekomst"                                           %+% 
                    ifelse(length(which(nydata[, i] == j)) == 1, "", "er")     %+%
                    " har den ukjente vanntypen \"" %+% tolower(VN1[[i]])      %+% 
                    " = " %+% j %+% "\""
                }
              }
            }
          }
        }
      } else {
        Vf[which(is.na(Vf[, i])), i] <- "<NA>"
        andre <- unique(Vf[utvalg, i]) %-% 
                 unlist(strsplit(unique(maaling[, i]), "[+]"))
        if   (length(andre)) {
          for (j in (andre)) {
            if (ekstrapol) {
              NB <- c(NB, length(which(maaling[, i] == j)) %+% 
                        " vannforekomster som er av typen \"" %+% 
                        tolower(VN1[[i]]) %+% " = " %+% j %+% 
                        "\", antas å ligne på \"" %+% tolower(VN1[[i]]) %+%
                        " = " %+% paste(sort(unique(maaling[, i])), 
                                        collapse = "+") %+% "\"")
            } else {
              fjern <- c(fjern, which(Vf[utvalg, i] == j))
              if (j %in% RF1.sik[[i]]) {
                beskjed[length(beskjed) + 1] <- length(which(Vf[utvalg,i]==j)) %+%
                  " vannforekomster er av typen \""   %+%    tolower(VN1[[i]]) %+% 
                  " = " %+%  j  %+% "\", som det ikke foreligger målinger av " %+%
                  parameter %+% " fra"
              } else {
                beskjed[length(beskjed) + 1] <- length(which(Vf[utvalg,i]==j)) %+%
                  " vannforekomst"                                             %+% 
                  ifelse(length(which(Vf[utvalg, i] == j)) == 1, "", "er")     %+%
                  " har den ukjente vanntypen \"" %+% tolower(VN1[[i]])        %+%
                  " = " %+% j %+% "\""
              }
            }
          }
        }
      }
    }
    ordVar <- numVar <- character(0)
    for (i in names(RF2.sik)) {
      if (is.numeric(RF2[[i]])) {
        numVar <- c(numVar, ord2num[[i]])
      } else {
        ordVar <- c(ordVar, i)
      }
    }
    for (i in ordVar) { # ordinale variabler
      if (i %in% names(RF2) && !length(RF2[[i]])) {
        RF2[[i]] <- paste(sort(unique(maaling[, i])), collapse = "+")
      }
      Vf[which( is.na(Vf[, i])), i] <- "<NA>"
      andre <- unique(Vf[utvalg, i]) %-% unique(maaling[, i])
      if   (length(andre)) {
        for (j in (andre)) {
          if (is.numeric(maaling[, i]) & i %in% names(RF2)) {
            in.mente <- rep(0, nrow(nydata))
            for (n in 1:length(TV2[[i]])) {
              in.mente[which(nydata[, i] == RF2.sik[[i]][n])] <- TV2[[i]][n]
            }
            nydata[, i] <- in.mente
          } else {
            w <- which(RF2[[i]] %inneholder% j)
            if (length(w)) {
              if (length(RF2[[i]]) > 1) {
                if (i %in% fastVariabel) {
                  fjern <- c(fjern, which(Vf[utvalg, i] == j))
                  beskjed <- c(beskjed, length(which(Vf[utvalg, i] == j))    %+%
                    " vannforekomster er av typen \"" %+%  tolower(VN2[[i]]) %+% 
                    " = " %+%j%+% "\", som det ikke foreligger målinger av " %+%
                    parameter %+% " fra")
                } else {
                  nydata[which(nydata[, i] == j), i] <- RF2[[i]][w]
                }
              }
            } else {
              if (j %in% RF2.sik[[i]]) {
                w <- which(RF2.sik[[i]] == j)
                if (w < min(which(RF2.sik[[i]] %in% 
                                  unlist(strsplit(RF2[[i]][1], "[+]"))))) {
                  if (ekstrapol) {
                    if (length(RF2[[i]]) > 1) {
                      nydata[which(nydata[, i] == j), i] <- RF2[[i]][1]
                    }
                    NB <- c(NB, length(which(Vf[utvalg, i] == j)) %+% 
                            " vannforekomster som er av typen \"" %+% 
                            tolower(VN2[[i]]) %+% " = " %+% j %+% 
                            "\", antas å ligne på \"" %+% tolower(VN2[[i]]) %+%
                            " = " %+% RF2[[i]][1] %+% "\"")
                  } else {
                    fjern <- c(fjern, which(Vf[utvalg, i] == j))
                    beskjed <- c(beskjed, length(which(Vf[utvalg, i] == j))    %+%
                      " vannforekomster er av typen \"" %+%  tolower(VN2[[i]]) %+% 
                      " = " %+%j%+% "\", som det ikke foreligger målinger av " %+%
                      parameter %+% " fra")
                  }
                }
                for (n in 1:length(RF2[[i]])) {
                  k <- min(which(RF2.sik[[i]] %in% 
                                 unlist(strsplit(RF2[[i]][n], "[+]"))))
                  l <- max(which(RF2.sik[[i]] %in%
                                 unlist(strsplit(RF2[[i]][n], "[+]"))))
                  m <- min(which(RF2.sik[[i]] %in%
                                 unlist(strsplit(RF2[[i]][min(n + 1, 
                                          length(RF2[[i]]))], "[+]"))))
                  if (w >= k & w <= l) {
                    if (length(RF2[[i]]) > 1) {
                      nydata[which(nydata[, i] == j), i] <- RF2[[i]][n]
                    }
                    NB <- c(NB, length(which(Vf[utvalg, i] == j)) %+% 
                            " vannforekomster som er av typen \"" %+% 
                            tolower(VN2[[i]]) %+% " = " %+% j %+% 
                            "\", antas å ligne på \"" %+% tolower(VN2[[i]]) %+%
                            " = " %+% RF2[[i]][n] %+% "\"")
                  }
                  if (w > l & w < m) {
                    if (ekstrapol) {
                      if (length(which(maaling[, i] == RF2[[i]][n])) >
                          length(which(maaling[, i] == RF2[[i]][n + 1]))) {
                        if (length(RF2[[i]]) > 1) {
                          nydata[which(nydata[, i] == j), i] <- RF2[[i]][n]
                        }
                        NB <- c(NB, length(which(Vf[utvalg, i] == j)) %+% 
                                " vannforekomster som er av typen \"" %+% 
                                tolower(VN2[[i]]) %+% " = " %+% j %+% 
                                "\", antas å ligne på \"" %+%tolower(VN2[[i]])%+%
                                " = " %+% RF2[[i]][n] %+% "\"")
                      } else {
                        if (length(RF2[[i]]) > 1) {
                          nydata[which(nydata[, i] == j), i] <- RF2[[i]][n + 1]
                        }
                        NB <- c(NB, length(which(Vf[utvalg, i] == j)) %+% 
                                " vannforekomster som er av typen \"" %+% 
                                tolower(VN2[[i]]) %+% " = " %+% j %+% 
                                "\", antas å ligne på \"" %+%tolower(VN2[[i]])%+%
                                " = " %+% RF2[[i]][n + 1] %+% "\"")
                      }
                    } else {
                      fjern <- c(fjern, which(Vf[utvalg, i] == j))
                      beskjed <- c(beskjed, length(which(Vf[utvalg, i] == j)) %+%
                        " vannforekomster er av typen \""%+%tolower(VN2[[i]]) %+% 
                        " = "%+%j%+%"\", som det ikke foreligger målinger av "%+%
                        parameter %+% " fra")
                    }
                  }
                }
                if (w > l) {
                  if (length(RF2[[i]]) > 1) {
                    nydata[which(nydata[,i] == j),i] <- RF2[[i]][length(RF2[[i]])]
                  }
                  if (ekstrapol) {
                    NB <- c(NB, length(which(Vf[utvalg, i] == j)) %+% 
                            " vannforekomster som er av typen \"" %+% 
                            tolower(VN2[[i]]) %+% " = " %+% j %+% 
                            "\", antas å ligne på \"" %+% tolower(VN2[[i]]) %+%
                            " = " %+% RF2[[i]][length(RF2[[i]])] %+% "\"")
                  } else {
                    fjern <- c(fjern, which(Vf[utvalg, i] == j))
                    beskjed <- c(beskjed, length(which(Vf[utvalg, i] == j))   %+%
                      " vannforekomster er av typen \"" %+% tolower(VN2[[i]]) %+% 
                      " = " %+%j%+% "\", som det ikke foreligger målinger av "%+%
                      parameter %+% " fra")
                  }
                }
              } else {
                if (length(RF2a[[i]]) && any(RF2 %inneholder% RF2a[[i]])) {
                  if (ekstrapol) {
                    nydata[which(nydata[, i] == j), i] <- RF2a[[i]]
                    NB <- c(NB, length(which(nydata[, i] == j)) %+% 
                            " vannforekomster som er av typen \"" %+% 
                            tolower(VN2[[i]]) %+% " = " %+% j %+% 
                            "\", antas å ligne på \"" %+% tolower(VN2[[i]]) %+%
                            " = " %+% RF2a[[i]][1] %+% "\"")
                  } else {
                    fjern <- c(fjern, which(Vf[utvalg, i] == j))
                    beskjed <- c(beskjed, length(which(Vf[utvalg, i] == j))    %+%
                      " vannforekomst"                                         %+%
                      ifelse(length(which(Vf[utvalg, i] == j)) == 1, "", "er") %+%
                      " har den ukjente vanntypen \"" %+% tolower(VN2[[i]])    %+% 
                      " = " %+% j %+% "\"")
                  }
                } else {
                  fjern <- c(fjern, which(Vf[utvalg, i] == j))
                  beskjed[length(beskjed) + 1] <- 
                    length(which(Vf[utvalg, i] == j)) %+% " vannforekomst"   %+% 
                    ifelse(length(which(Vf[utvalg, i] == j)) == 1, "", "er") %+%
                    " har den ukjente vanntypen \"" %+% tolower(VN2[[i]])    %+% 
                    " = " %+% j %+% "\""
                }
              }
            }
          }
        }
      }
    }
    for (i in numVar) { # numeriske variabler
      forStor <- forLita <- numeric()
      if (i %=% "areal") {
        # En størrelsesforskjell på 20 % anses som (ev. for) stor
        forStor <- which(Vf[utvalg, i] > max(maaling[, i], na.rm = TRUE) + 0.176)
        forLita <- which(Vf[utvalg, i] < min(maaling[, i], na.rm = TRUE) - 0.176)
        txtStor <- "større"
        txtLita <- "mindre"
      }
      if (i %=% "høyde") {
        # En høydeforskjell på 100 m til 200 m anses som (ev. for) stor
        forStor <- which(Vf[utvalg, i] > max(maaling[, i], na.rm = TRUE) + 3)
        forLita <- which(Vf[utvalg, i] < min(maaling[, i], na.rm = TRUE) - 3)
        txtStor <- "høyere"
        txtLita <- "lavere"
      }
      if (i %=% "gbred") {
        # En forskjell i geografisk bredde på én grad anses som (ev. for) stor
        forStor <- which(Vf[utvalg, i] > max(maaling[, i], na.rm = TRUE) + 1)
        forLita <- which(Vf[utvalg, i] < min(maaling[, i], na.rm = TRUE) - 1)
        txtStor <- "lenger nord"
        txtLita <- "lenger sør"
      }
      if (!(i %in% c("areal", "høyde", "gbred"))) {
        # Et avvik på 20 % anses som (ev. for) stor
        vb <- (max(maaling[,i], na.rm=TRUE) - min(maaling[,i], na.rm=TRUE)) / 5
        forStor <- which(Vf[utvalg, i] > max(maaling[, i], na.rm = TRUE) + vb)
        forLita <- which(Vf[utvalg, i] < min(maaling[, i], na.rm = TRUE) - vb)
        forklaring <- switch(i,
                             CaCO3 = "alkalisk",
                             P_tot = "humusrik",
                             dybde = "dyp",
                             kystt = "lukka",
                             saltk = "saltholdig",
                             ekspo = "eksponert",
                             miksg = "blanda",
                             oppht = "statisk",
                             vknop = "strømpåvirka")
        txtStor <- "mer "    %+% forklaring
        txtLita <- "mindre " %+% forklaring
      }
      if (ekstrapol) {
        if (length(forStor)) {
          NB <- c(NB, length(forStor) %+% " vannforekomster er en god del " %+% 
                      txtStor %+% " enn de det foreligger målinger av " %+%
                      parameter %+% " fra")
        }
        if (length(forLita)) {
          NB <- c(NB, length(forLita) %+% " vannforekomster er en god del " %+% 
                      txtLita %+% " enn de det foreligger målinger av " %+%
                      parameter %+% " fra")
        }
      } else {
        if (length(forStor)) {
          fjern <- c(fjern, forStor)
          beskjed <- c(beskjed, length(forStor) %+% 
                                " vannforekomster er en god del " %+% txtStor %+%
                                " enn de det foreligger målinger av " %+% 
                                parameter %+% " fra")
        }
        if (length(forLita)) {
          fjern <- c(fjern, forLita)
          beskjed <- c(beskjed, length(forLita) %+%
                                " vannforekomster er en god del " %+% txtLita %+% 
                                " enn de det foreligger målinger av " %+%
                                parameter %+% " fra")
        }
      }
    }
    
    if (length(fjern)) {
      utvalg <- utvalg[-fjern]
      nydata <- nydata[-fjern, ]
      sluttbeskjed <- ifelse(length(fjern) > 1, "Diss", "Denn") %+%
        "e blir ekskludert fra ekstrapoleringa, slik at " %+% length(utvalg) %+%
        " vannforekomster er igjen."
      if (length(beskjed) > 1) {
        for (i in 1:length(beskjed)) {
          u <- c(u, skriv(beskjed[i], ifelse(i == length(beskjed), ".", ";"), 
                          pre = "- ", ut = TRUE))
        }
        u <- c(u, skriv(sluttbeskjed, ut = TRUE))
      } else {
        u <- c(u, skriv(beskjed[1], ". ", sluttbeskjed, ut = TRUE))
      }
    }
    if (vis & length(NB)) {
      sluttbeskjed <- "Disse blir tatt med i ekstrapoleringa."
      if (length(NB) > 1) {
        for (i in 1:length(NB)) {
          u <- c(u, skriv(NB[i], ifelse(i == length(NB), ".", ";"), 
                          pre = "- ", ut = TRUE))
        }
        u <- c(u, skriv(sluttbeskjed, ut = TRUE))
      } else {
        u <- c(u, skriv(NB[1], ". ", sluttbeskjed, ut = TRUE))
      }
    }
    
    felles <- length(unique(maaling$vfo %A% rownames(nydata)))
    if (felles < length(unique(maaling$vfo))) {
      u <- c(u, skriv("Det forelå målinger for ", 
                      length(unique(maaling$vfo)) - felles,
                      " av vannforekomstene som ble fjerna. Disse inngår da ",
                      "heller ikke i ekstrapoleringa.", 
                      pre = "OBS: ", linjer.under = 1, ut = TRUE))
    }
    
    andel <- felles / length(utvalg)
    tekst <- " % av de relevante vannforekomstene (" %+% felles %+% " av " %+%
      length(utvalg) %+% ")."
    if (andel < 0.01) {
      u <- c(u, skriv("Det foreligger målinger for under 1 ", tekst, 
                      linjer.under = 1, ut = TRUE))
    } else {
      u <- c(u, skriv("Det foreligger altså målinger for ", round(andel * 100), 
                      tekst, linjer.under = 1, ut = TRUE))
    }
  }
  
  if (OK) {

    set.seed(SEED, "Mersenne-Twister", "Inversion", "Rejection")
    
    if (any(names(nydata) == "akt")) {
      alist <- sort(unique(maaling$akt))
      akter <- unlist(strsplit(alist, "[+]"))
      if (!all(akter %in% Aktiviteter$id)) {
        ukjenteAktiviteter <- akter %-% Aktiviteter$id
        for (j in ukjenteAktiviteter) {
          Aktiviteter <- rbind(Aktiviteter, 
                               list(j, "ukjent aktivitet", 0))
          rownames(Aktiviteter)[nrow(Aktiviteter)] <- j
        }
        u <- c(u, skriv("Noen overvåkingsaktiviteter er ukjent! Disse ble nå ",
                        "tildelt en skår på 0. (Dette gjelder: ", 
                        paste(ukjenteAktiviteter, collapse=", "), ".)",
                        pre = "OBS OBS! ", linjer.under = 1, ut = TRUE))
      }
      if (aktVekting) {
        akter <- strsplit(alist, "[+]")
        alist <- rep.int(alist, lengths(akter))
        akter <- unlist(akter)
        avekt <- aktivitetsvekt^(-abs(Aktiviteter[akter, "skaar"]))
      } else {
        avekt <- aktivitetsvekt^(-abs(Aktiviteter[akter, "skaar"]))
        bvekt <- rep(0, length(alist))
        for (i in 1:length(akter)) {
          w <- which(alist %inneholder% akter[i])
          bvekt[w] <- bvekt[w] + avekt[i]
        }
        avekt <- bvekt / sum(avekt)
        rm(bvekt)
      }
      nydata$akt <- sample(alist, nrow(nydata), TRUE, avekt)
    }
    if ("per" %in% names(nydata)) {
      nydata$per <- factor(rapportaar[1], levels=levels(maaling$per))
    }
    if ("rar" %in% names(nydata)) {
      nydata$rar <- 0
    }
    for (i in explv[hvilke]) {
      if (is.character(maaling[,i]) & !is.character(nydata[,i])) {
        nydata[,i] <- as.character(nydata[,i])
      }
      if (is.numeric(maaling[,i]) & !is.numeric(nydata[,i])) {
        nydata[,i] <- as.numeric(nydata[,i])
      }
    }
    konfident <- which(rownames(nydata) %in% maaling$vfo)
    #BLAA
    #  konfident <- matrix(FALSE, nrow(nydata), length(rapportaar))
    #  maalt.gs <- maalt.sd <- matrix(NA, nrow(nydata), length(rapportaar))
    #  colnames(konfident) <- colnames(maalt.gs) <- colnames(maalt.sd) <- rapportaar
    #  rownames(konfident) <- rownames(maalt.gs) <- rownames(maalt.sd) <- 
    #                         rownames(nydata)
    #  for (i in rownames(nydata)) {
    #    for (j in as.character(rapportaar)) {
    #      hvilke <- which(maaling$vfo == i & as.character(maaling$per) == j)
    #      if (length(hvilke)) {
    #        konfident[i,j] <- TRUE
    #        maalt.gs[i,j] <- weighted.mean(maaling$vrd[hvilke],maaling$ant[hvilke])
    #        maalt.sd[i,j] <- -1
    #        if (length(hvilke) > 2) {
    #          maalt.sd[i,j] <- sd(maaling$vrd[hvilke])
    #          if (length(hvilke) < 50) {
    #            maalt.sd[i,j] <- sd(maaling$vrd[hvilke]) / 
    #              c(NA, 0.80, 0.89, 0.92, 0.94, 0.95, 0.96, 0.97, 0.97, 0.97, 
    #                rep(0.98, 7), rep(0.99, 48))[length(hvilke)]
    #          }
    #        }
    #      }
    #    }
    #  }
    #  if (!any(maalt.sd >= 0)) {
    #    cat("HVA NÅ???\n")
    #  } #¤ ?!
    #  if (any(maalt.sd < 0)) {
    #    cv <- as.vector(maalt.sd / (maalt.gs + 1e-6))
    #    md <- lm(cv ~ as.vector(maalt.gs), 
    #             subset = which(as.vector(maalt.sd) >= 0))
    #    yaa <- md$coeff[1]
    #    stg <- md$coeff[2]
    #    hvilke <- (maalt.sd < 0 & !is.na(maalt.sd))
    #    maalt.sd[hvilke] <- (yaa + stg * maalt.gs[hvilke]) * maalt.gs[hvilke]
    #    rm(cv, md, yaa, stg, hvilke)
    #  }
    df <- modell$df
  }

  if (OK) {
    
    ##################################################
    skriv("Simulering", pre = "   ", linjer.over  = 1)
    skriv("==========", pre = "   ", linjer.under = 1)

    nsim <- iterasjoner
    UT <- list()
    for (e in tolower(rapportenhet)) {
      UT[e] <- NA
      if ("fylker" %begynner% e) {
        names(UT)[length(UT)] <- "fylke"
        UT$fylke <- array(0, c(length(FYL), length(rapportaar), nsim + 1),
                          list(fylke=FNR, aar=rapportaar, 
                               simuleringer=c("pred", 1:nsim)))
      }
      if ("kommuner" %begynner% e) {
        names(UT)[length(UT)] <- "kommune"
        UT$kommune <- array(0, c(length(KOM), length(rapportaar), nsim + 1),
                            list(kommune=KOM, aar=rapportaar, 
                                 simuleringer=c("pred", 1:nsim)))
      }
      if ("landsdeler" %begynner% e) {
        names(UT)[length(UT)] <- "landsdel"
        UT$landsdel <- array(0, c(5, length(rapportaar), nsim + 1),
                             list(landsdel=c("Østlandet", "Sørlandet", "Vestlandet", 
                                             "Midt-Norge", "Nord-Norge"),
                                  aar=rapportaar, simuleringer=c("pred", 1:nsim)))
      }
      if ("norge" %begynner% e) {
        names(UT)[length(UT)] <- "Norge"
        UT$Norge <- array(0, c(1, length(rapportaar), nsim + 1),
                          list(rike="Norge", aar=rapportaar, 
                               simuleringer=c("pred", 1:nsim)))
      }
    }

    { # Simulering -----------------------------------------------
    
      Areal <- Areal[utvalg]
      if (length(which(is.na(Areal))) > length(utvalg) / 10) {
        Areal <- rep(1, nrow(nydata))
      }
      #Areal[which(Areal < 0.5)] <- 0.5 # Dette var foreslått i NINA-rapport 1723,
      # men det er strengt tatt ikke forenlig med naturindeksens arealvekting
      w <- which(is.na(Areal))
      if (length(w)) {
        if (vannkategori %=% "L") {
          Areal[w] <- c(0.5, 1.58, 15.8, 158)[Vf$stø[utvalg[w]]]
          # dette er de geometriske middelverdiene for størrelsesklassene
        } else {
          Areal[w] <- mean(Areal[-w])
        }
      }
      Areal <- Areal^(arealvekt / 2)
      
      # BLAA
      # alle.maalt <- matrix(TRUE, length(KOM), length(rapportaar), 
      #                      dimnames=list(KOM, rapportaar))
      if (nsim < 1) {
        simdata   <- array(0, c(nrow(nydata), length(rapportaar), 1),
                           list(vannforekomst=rownames(nydata), aar=rapportaar, 
                                simuleringer="pred"))
        for (j in 1:length(rapportaar)) {
          if ("per" %in% names(nydata)) {
            nydata$per <- factor(rapportaar[i], levels=levels(maaling$per))
          }
          pred <- predict(modell, nydata, TRUE, interval = "pred", level = 0.5,
                          weights = 1)
          simdata[,i,1] <- pred$fit[,1]
        }
      } else {
        skriv("Nå begynner simuleringa. Det er valgt ", 
              format(nsim, scientific=FALSE, big.mark=ifelse(nsim < 1e4, ""," ")), 
              " iterasjoner.", ifelse(nsim > 1000, " Dette vil ta sin tid.", ""))
        s <- 0
        while (s < nsim) {
          SIM <- min(1000, nsim - s)
          simdata   <- array(0, c(nrow(nydata), length(rapportaar), SIM),
                             list(vannforekomst = rownames(nydata), aar = rapportaar, 
                                  simuleringer = 1:SIM))
          for (i in (1:SIM + s)) {
            slumptall <- rt(nrow(nydata), df)
            names(slumptall) <- rownames(nydata)
            if (any(names(nydata) == "akt")) {
              nydata$akt <- sample(alist, nrow(nydata), TRUE, avekt)
              # BLAA
              # for (j in which(apply(konfident, 1, any))) {
              #   nydata$akt[j] <- sample(unique(maaling$akt[which(maaling$vfo ==
              #     rownames(nydata)[j])]), 1)
              # }
            }
            for (j in 1:length(rapportaar)) {
              if ("per" %in% names(nydata)) {
                nydata$per <- factor(rapportaar[j], levels=levels(maaling$per))
              }
              pred <- predict(modell, nydata, TRUE, 
                              interval = "pred", level = 0.5, weights = 1)
              conf <- predict(modell, nydata[konfident, ], TRUE, 
                              interval = "conf", level = 0.5, weights = 1) # !BLAA
              pred$fit[,2] <- 0.5 * (pred$fit[,3] - pred$fit[,2]) / qt(0.75, df)
              pred <- pred$fit[,1:2]
              # BLAA
              # pred[konfident[,j],1] <- maalt.gs[konfident[,j],j]
              # pred[konfident[,j],2] <- maalt.sd[konfident[,j],j]
              colnames(pred)[2] <- "SD"
              simdata[, j, i - s] <- pred[,1] + pred[,2] * slumptall
            }
            if (tell) {
              cat("Ferdig med " %+% floor(100*i/nsim) %+% " % av simuleringene.\r")
            }
          }
          if (tell) {
            cat("\n")
          } else {
            cat("Ferdig med 100% av simuleringene.\n")
          }
          rm(slumptall)
          gc(verbose = FALSE)

          # Oppskalering til rapporteringsenheter
          
          for (e in rapportenhet) {
            if ("fylker" %begynner% e) {
              for (j in as.character(rapportaar)) {
                for (f in FNR) {
                  w <- which(Vf$fylke %inneholder% f)
                  w <- which(rownames(nydata) %in% Vf$id[w])
                  if (length(w)) {
                    if (length(w) > 1) {
                      UT$fylke[f, j, 1:SIM + s + 1] <- apply(simdata[w, j, ], 2, 
                                                             weighted.mean,
                                                             Areal[w], na.rm = TRUE)
                    } else {
                      UT$fylke[f, j, 1:SIM + s + 1] <- simdata[w, j, ]
                    }
                  } else {
                    UT$fylke[f, j, 1:SIM + s + 1] <- NA
                  }
                }
              }
            }
            if ("kommuner" %begynner% e) {
              if (adminAar > 1976) {
                for (k in KOM) {
                  w <- which(Vf$kommune %inneholder% (k %+% ","))
                  w <- which(rownames(nydata) %in% Vf$id[w])
                  if (length(w)) {
                    if (length(w) > 1) {
                      for (j in as.character(rapportaar)) {
                        UT$kommune[k, j, 1:SIM + s + 1] <-
                          apply(simdata[w, j, ], 2, weighted.mean, 
                                Areal[w], na.rm = TRUE)
                        # BLAA
                        # alle.maalt[k,j] <- alle.maalt[k,j] & all(konfident[w,j])
                      }
                    } else {
                      UT$kommune[k, , 1:SIM + s + 1] <- simdata[w,,]
                      # BLAA
                      # alle.maalt[k,] <- alle.maalt[k,] & konfident[w,]
                    }
                  } else {
                    UT$kommune[k, , 1:SIM + s + 1] <- NA
                  }
                  if (tell) {
                    cat("Ferdig med " %+% which(KOM==k) %+% " av " %+% 
                        length(KOM) %+% " kommuner.\r")
                  }
                }
                if (tell) {
                  cat("\n")
                } else {
                  cat("Ferdig med " %+% length(KOM) %+% " av " %+% 
                        length(KOM) %+% " kommuner.\n")
                }
              }
            }
            if ("landsdeler" %begynner% e) {
              for (j in as.character(rapportaar)) {
                for (f in 1:5) {
                  w <- c()
                  for (k in 1:length(WF[[f]])) {
                    w <- c(w, which(Vf$fylke %inneholder% WF[[f]][k]))
                  }
                  w <- which(rownames(nydata) %in% Vf$id[w])
                  if (length(w)) {
                    if (length(w) > 1) {
                      UT$landsdel[f, j, 1:SIM + s + 1] <- 
                        apply(simdata[w, j, ], 2, weighted.mean,
                              Areal[w], na.rm=TRUE)
                    } else {
                      UT$landsdel[f, j, 1:SIM + s + 1] <- simdata[w, j, ]
                    }
                  } else {
                    UT$landsdel[f, j, 1:SIM + s + 1] <- NA
                  }
                }
              }
            }
            if ("norge" %begynner% e) {
              for (j in as.character(rapportaar)) {
                UT$Norge[1, j, 1:SIM + s + 1] <- 
                  apply(simdata[, j, ], 2, weighted.mean, Areal, na.rm = TRUE)
              }
            }
          }
          rm(simdata)
          gc(verbose = FALSE)
          s <- i
        }
      }
      for (i in 1:length(UT)) {
        if (logit) {
          UT[[i]] <- reskaler(UT[[i]], minV, maxV)
        }
        for (j in 1:length(rapportaar)) {
          for (k in 1:dim(UT[[i]])[1]) {
            UT[[i]][k, j, 1] <- median(UT[[i]][k, j, -1], na.rm = TRUE)
          }
        }
      }
    }
  }
  
  # utmating ---------------------------------------------
  if (OK) {
    attr(UT, "parameter")     <- parameter
    attr(UT, "vannkategori")  <- vannkategori
    attr(UT, "tidspunkt")     <- Sys.time()
    attr(UT, "versjon")       <- "WFD2ECA v. 0.15"
    innstillinger <- list(
      adminAar        =        adminAar,
      rapportperiode  =  rapportperiode,
      vedMaalefeil    =    vedMaalefeil,
      maksSkjevhet    =    maksSkjevhet,
      bareInkluder    =    bareInkluder,
      ikkeInkluder    =    ikkeInkluder,
      maalingPer      =      maalingPer,
      maalingTot      =      maalingTot,
      maalingTyp      =      maalingTyp,
      maalingInt      =      maalingInt,
      EQR             =             EQR,
      ignorerVariabel = ignorerVariabel,
      fastVariabel    =    fastVariabel,
      aktVekting      =      aktVekting,
      aktivitetsvekt  =  aktivitetsvekt,
      antallvekt      =      antallvekt,
      tidsvekt        =        tidsvekt,
      arealvekt       =       arealvekt,
      logit           =           logit,
      DeltaAIC        =        DeltaAIC,
      interaksjon     =     interaksjon,
      ekstrapolering  =  ekstrapolering,
      iterasjoner     =     iterasjoner,
      SEED            =            SEED
    )
    if (exists("sjekkArgumenter")) {
      for (i in sjekkArgumenter  ) {
        args <- as.list(match.call())
        if (i %in% names(args)) {
          innstillinger[i] <- args[i]
        }
      }
    }
    attr(UT, "innstillinger") <- innstillinger
    attr(UT, "beskjeder") <- u
    sisteTekst <- parameter %+% "s " %+% E %+% "verdier har medianen " %+% 
      komma(format(round(median(unlist(UT), na.rm=TRUE), 3), nsm=3, sci=FALSE)) %+% 
      " og strekker seg fra " %+%
      komma(format(round(   min(unlist(UT), na.rm=TRUE), 3), nsm=3, sci=FALSE)) %+%
      " til " %+%
      komma(format(round(   max(unlist(UT), na.rm=TRUE), 3), nsm=3, sci=FALSE)) %+%
      "."
    skriv("Sånn. Da har vi omsider kommet i mål.", linjer.over = 1)
    skriv(sisteTekst, linjer.under = 1)
  } else {
    UT <- NULL
  }
  return(UT)
}
