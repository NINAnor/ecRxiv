### Vanntyper
# Hjelpefil til NI_vannf
# ved Hanno Sandvik
# mai 2024
# se https://github.com/NINAnor/NI_vannf
###



# Navn på nominale typologifaktorer
Typologi.nominal <- c(
  tur = "Turbiditet", 
  tid = "Tidevann"
)



# Navn på ordinale typologifaktorer
Typologi.ordinal <- c(
  reg = "Region",
  son = "Sone",
  stø = "Størrelse", 
  alk = "Alkalitet", 
  hum = "Humøsitet", 
  dyp = "Dybde",
  kys = "Kysttype",
  sal = "Salinitet",
  eks = "Eksponering",
  mix = "Miksing",
  opp = "Oppholdstid",
  str = "Strøm"
)



# Navn på numeriske typologifaktorer
Typologi.numerisk <- c(
  gbred = "Geografisk bredde",
  høyde = "Høyde over havet",
  areal = "Innsjøareal", 
  tilsf = "Tilsigsfelt",
  CaCO3 = "Kalkinnhold", 
  P_tot = "Totalfosfor", 
  dybde = "Innsjødybde",
  kystt = "Kysttype",
  saltk = "Saltkonsentrasjon",
  ekspo = "Eksponering",
  miksg = "Miksing",
  oppht = "Oppholdstid",
  vknop = "Strømhastighet"
)



# Navn på alle typologfaktorer
Typologi <- c(Typologi.ordinal, Typologi.nominal)



# Samsvar mellom ordinale og numeriske typologifaktorer
ord2numC <- c(reg = "gbred", kys = "kystt", sal = "saltk", eks = "ekspo", 
              mix = "miksg", opp = "oppht", str = "vknop")
ord2numL <- c(reg = "gbred", son = "høyde", stø = "areal", 
              alk = "CaCO3", hum = "P_tot", dyp = "dybde")
ord2numR <- c(reg = "gbred", son = "høyde", stø = "tilsf", 
              alk = "CaCO3", hum = "P_tot")



# Forkortelser for typologifaktorer som inngår for de ulike vannkategoriene
# Rekkefølgen tilsvarer vanntypekoden ifølge vann-nett
TypologiC <- c("reg", "kys", "sal", "tid", "eks", "mix", "opp", "str")
TypologiL <- c("reg", "son", "stø", "alk", "hum", "tur", "dyp")
TypologiR <- c("reg", "son", "stø", "alk", "hum", "tur")



# Mulige verdier for nominale typologifaktorer
Vanntyper.nominal <- list(
  tur = c("1", "2", "3"),
  tid = c("1", "2")
)



# Mulige verdier for ordinale typologifaktorer og deres rekkefølge
Vanntyper.ordinal <- list(
  regC= c("S", "N", "M", "H", "G", "B"),
  regL= c("S", "W", "E", "M", "N", "F"),
  regR= c("S", "W", "E", "M", "N", "F"),
  son = c("L", "M", "H"),
  stø = c("1", "2", "3", "4", "5"),
  alk = c("5", "6", "7", "1", "8", "2", "3", "4"),
  hum = c("4", "1", "2", "3"),
  dyp = c("1", "2", "3"),
  kys = c("1", "7", "2", "3", "4", "5", "6", "8"),
  sal = c("1", "2", "3", "6", "4", "7", "5"),
  eks = c("1", "2", "3"),
  mix = c("1", "2", "3"),
  opp = c("1", "2", "3"),
  str = c("1", "2", "3")
)



# Mulige verdier for alle typologifaktorer
Vanntyper <- append(Vanntyper.ordinal, Vanntyper.nominal)[c(1:7,15,8:10,16,11:14)]



# Tallverdier for ordinale typologifaktorer
Tallverdier <- list(
  reg = 1:6,
  son = 1:3,
  stø = 1:5,
  alk = c(-0.8, -0.4, -0.2, -0.1, 0, 0.3, 0.9, 1.5),
  hum = c(0.75, 1.25, 1.75, 2.25),
  dyp = 1:3,
  kys = 1:8,
  sal = c(-0.6, 0.2, 1, 1.1, 1.4, 1.5, 1.6),
  eks = 1:3,
  mix = 1:3,
  opp = 1:3,
  str = 1:3
)



# Funksjonen kombinerer typologifaktorer til en vektor med vanntyper
"%&%" <- function(a,b) rep(a, each=length(b)) %+% rep(b, length(a))



# Norske vanntyper
LL <- "L" %&% c("E","F","M","N","S","W") %&% "L" %&% 1:4
LM <- "L" %&% c("E","F","M","N","S","W") %&% "M" %&% 1:4
LH <- "L" %&% c("E","F","M","N","S","W") %&% "H" %&% 1:4

RL <- "R" %&% c("E","F","M","N","S","W") %&% "L" %&% 1:5
RM <- "R" %&% c("E","F","M","N","S","W") %&% "M" %&% 1:5
RH <- "R" %&% c("E","F","M","N","S","W") %&% "H" %&% 1:5

gamleTyper <- list(
  L101a = LL %&% "541" %&% c(2,3,5,6),
  L101b = LL %&% "641" %&% c(2,3,5,6),
  L101c = LL %&% "741" %&% c(2,3,5,6),
  L101d = LL %&% "841" %&% c(2,3,5,6),
  L101  = LL %&% c(1,5:8) %&% "41" %&% c(2,3,5,6),
  L102a = LL %&% "511" %&% c(2,3,5,6),
  L102b = LL %&% "611" %&% c(2,3,5,6),
  L102c = LL %&% "711" %&% c(2,3,5,6),
  L102d = LL %&% "811" %&% c(2,3,5,6),
  L102  = LL %&% c(1,5:8) %&% "11" %&% c(2,3,5,6),
  L103a = LL %&% "521" %&% c(2,3,5,6),
  L103b = LL %&% "621" %&% c(2,3,5,6),
  L103c = LL %&% "721" %&% c(2,3,5,6),
  L103d = LL %&% "821" %&% c(2,3,5,6),
  L103  = LL %&% c(1,5:8) %&% "21" %&% c(2,3,5,6),
  L104  = LL %&% "241" %&% c(2,3,5,6),
  L105a = LL %&% "2112",
  L105b = LL %&% "2113",
  L105  = LL %&% "211" %&% c(2,3),
  L106  = LL %&% "221" %&% c(2,3,5,6),
  L107  = LL %&% "3" %&% c(1,4) %&% "1" %&% c(2,3,5,6),
  L108  = LL %&% "321" %&% c(2,3,5,6),
  L109  = LL %&% "4" %&% c(1,4) %&% "1" %&% 1:6,
  L110  = LL %&% "421" %&% 1:6,
  L111  = LL %&% 3:4 %&% 0:4 %&% "3" %&% c(1,2,4,5),
  L201a = LM %&% "541" %&% c(2,3,5,6),
  L201b = LM %&% "641" %&% c(2,3,5,6),
  L201c = LM %&% "741" %&% c(2,3,5,6),
  L201d = LM %&% "841" %&% c(2,3,5,6),
  L201  = LM %&% c(1,5:8) %&% "41" %&% c(2,3,5,6),
  L202a = LM %&% "511" %&% c(2,3,5,6),
  L202b = LM %&% "611" %&% c(2,3,5,6),
  L202c = LM %&% "711" %&% c(2,3,5,6),
  L202d = LM %&% "811" %&% c(2,3,5,6),
  L202  = LM %&% c(1,5:8) %&% "11" %&% c(2,3,5,6),
  L203a = LM %&% "521" %&% c(2,3,5,6),
  L203b = LM %&% "621" %&% c(2,3,5,6),
  L203c = LM %&% "721" %&% c(2,3,5,6),
  L203d = LM %&% "821" %&% c(2,3,5,6),
  L203  = LM %&% c(1,5:8) %&% "21" %&% c(2,3,5,6),
  L204  = LM %&% "241" %&% c(2,3,5,6),
  L205  = LM %&% "211" %&% c(2,3,5,6),
  L206  = LM %&% "221" %&% c(2,3,5,6),
  L207  = LM %&% "3" %&% c(1,4) %&% "1" %&% c(2,3,5,6),
  L208  = LM %&% "321" %&% c(2,3,5,6),
  L211  = LM %&% 1:8 %&% c(1,4) %&% "2" %&% 1:6,
  L301a = LH %&% "541" %&% c(2,3,5,6),
  L301b = LH %&% "641" %&% c(2,3,5,6),
  L301c = LH %&% "741" %&% c(2,3,5,6),
  L301d = LH %&% "841" %&% c(2,3,5,6),
  L301  = LH %&% c(1,5:8) %&% "41" %&% c(2,3,5,6),
  L302a = LH %&% "511" %&% c(2,3,5,6),
  L302b = LH %&% "611" %&% c(2,3,5,6),
  L302c = LH %&% "711" %&% c(2,3,5,6),
  L302d = LH %&% "811" %&% c(2,3,5,6),
  L302  = LH %&% c(1,5:8) %&% "11" %&% c(2,3,5,6),
  L303a = LH %&% "521" %&% c(2,3,5,6),
  L303b = LH %&% "621" %&% c(2,3,5,6),
  L303c = LH %&% "721" %&% c(2,3,5,6),
  L303d = LH %&% "821" %&% c(2,3,5,6),
  L303  = LH %&% c(1,5:8) %&% "21" %&% c(2,3,5,6),
  L304  = LH %&% "241" %&% c(2,3,5,6),
  L305  = LH %&% "211" %&% c(2,3,5,6),
  L306  = LH %&% "221" %&% c(2,3,5,6),
  L307  = LH %&% "3" %&% c(1,4) %&% "1" %&% c(2,3,5,6),
  L308  = LH %&% "321" %&% c(2,3,5,6),
  L311  = LH %&% 1:8 %&% c(1,4) %&% "2" %&% 1:6,
  LN1 = c(LL %&% "3" %&% c(1,4) %&% "1" %&% c(2,3,5,6),
          LL %&% "4" %&% c(1,4) %&% "1" %&% 1:6),
  LN2a= c(LL %&% "241" %&% c(2,3,5,6),
          LL %&% "2112",
          LM %&% "3" %&% c(1,4) %&% "1" %&% c(2,3,5,6)),
  LN2b  = LL %&% "2113",
  LN3 = c(LL %&% "221" %&% c(2,3,5,6),
          LM %&% "321" %&% c(2,3,5,6)),
  LN5 = c(LL %&% c(1,5:8) %&% "41" %&% c(2,3,5,6),
          LL %&% c(1,5:8) %&% "11" %&% c(2,3,5,6),
          LM %&% c(1,5:8) %&% "41" %&% c(2,3,5,6),
          LM %&% c(1,5:8) %&% "11" %&% c(2,3,5,6),
          LM %&% "241" %&% c(2,3,5,6),
          LM %&% "211" %&% c(2,3,5,6)),
  LN6 = c(LL %&% c(1,5:8) %&% "21" %&% c(2,3,5,6),
          LM %&% c(1,5:8) %&% "21" %&% c(2,3,5,6),
          LM %&% "221" %&% c(2,3,5,6)),
  LN7 = c(LH %&% c(1,5:8) %&% "41" %&% c(2,3,5,6),
          LH %&% c(1,5:8) %&% "21" %&% c(2,3,5,6),
          LH %&% "241" %&% c(2,3,5,6),
          LH %&% "211" %&% c(2,3,5,6)),
  LN8 = c(LL %&% "221" %&% c(2,3,5,6),
          LL %&% "421" %&% 1:6),
  R101a = RL %&% "541",
  R101b = RL %&% "641",
  R101c = RL %&% "741",
  R101d = RL %&% "841",
  R101  = RL %&% c(1,5:8) %&% "41",
  R102a = RL %&% "511",
  R102b = RL %&% "611",
  R102c = RL %&% "711",
  R102d = RL %&% "811",
  R102  = RL %&% c(1,5:8) %&% "11",
  R103a = RL %&% "521",
  R103b = RL %&% "621",
  R103c = RL %&% "721",
  R103d = RL %&% "821",
  R103  = RL %&% c(1,5:8) %&% "21",
  R104  = RL %&% "241",
  R105  = RL %&% "211",
  R106  = RL %&% "221",
  R107  = RL %&% "3" %&% c(1,4) %&% "1",
  R108  = RL %&% "321",
  R109  = RL %&% "4" %&% c(1,4) %&% "1",
  R110  = RL %&% "421",
  R111  = RL %&% 3:4 %&% 0:4 %&% "3",
  R201a = RM %&% "541",
  R201b = RM %&% "641",
  R201c = RM %&% "741",
  R201d = RM %&% "841",
  R201  = RM %&% c(1,5:8) %&% "41",
  R202a = RM %&% "511",
  R202b = RM %&% "611",
  R202c = RM %&% "711",
  R202d = RM %&% "811",
  R202  = RM %&% c(1,5:8) %&% "11",
  R203a = RM %&% "521",
  R203b = RM %&% "621",
  R203c = RM %&% "721",
  R203d = RM %&% "821",
  R203  = RM %&% c(1,5:8) %&% "21",
  R204  = RM %&% "241",
  R205  = RM %&% "211",
  R206  = RM %&% "221",
  R207  = RM %&% "3" %&% c(1,4) %&% "1",
  R208  = RM %&% "321",
  R211  = RM %&% 1:8 %&% c(1,4) %&% "2",
  R301a = RH %&% "541",
  R301b = RH %&% "641",
  R301c = RH %&% "741",
  R301d = RH %&% "841",
  R301  = RH %&% c(1,5:8) %&% "41",
  R302a = RH %&% "511",
  R302b = RH %&% "611",
  R302c = RH %&% "711",
  R302d = RH %&% "811",
  R302  = RH %&% c(1,5:8) %&% "11",
  R303a = RH %&% "521",
  R303b = RH %&% "621",
  R303c = RH %&% "721",
  R303d = RH %&% "821",
  R303  = RH %&% c(1,5:8) %&% "21",
  R304  = RH %&% "241",
  R305  = RH %&% "211",
  R306  = RH %&% "221",
  R307  = RH %&% "3" %&% c(1,4) %&% "1",
  R308  = RH %&% "321",
  R311  = RH %&% 1:8 %&% c(1,4) %&% "2"
)
