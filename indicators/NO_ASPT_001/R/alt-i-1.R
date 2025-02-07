

# Forberedelser
library(foreign)
library(sf)
library(readxl)
library(httr)
library(jsonlite)


# ... Laste inn funksjoner:
source("Funksjon.R")
source("lesMaalinger.R")
source("lesMer.R")

#for (filnavn in list.files("R", full.names = TRUE)) {
#  source(filnavn)
#}


# --- Vannforekomster
V <- lesVannforekomster(c("L", "R", "C"), CACHE = "vf.rdata")

# --- NVEs innsjødatabase
nve <- lesInnsjodatabasen("../ymse/Innsjo_Innsjo.dbf")

# --- Vannlokaliteter
VL <- lesVannlokaliteter(c("L", "R", "C"))

# --- Kobling av informasjon
V <- oppdaterVannforekomster(V, nve)

# ... Ytterligere datafiler
lesMer()
# fylker + kommuner?

# + Målinger fra vannmiljø-databasen
DATA <- lesMaalinger("ASPT")

# --- Analysen
utmating <- fraVFtilNI(
  DATA, 
  vannforekomster = V,
  vannlokaliteter = VL,
  parameter = "TIANTL",
  vannkategori = "L",
  NI.aar = c(1990, 2000, 2010, 2014, 2019, 2024),
  rapportenhet = c("kommune"), # "fylke", "landsdel", "norge"),
  adminAar = 2010,
  interaksjon = F,
  iterasjoner = 1000
)
print(summary(utmating$fylke))


# --- Visualisering
hist(utmating$fylke["1900", "2019", ], 
     breaks = 36, 
     main = "ASPT i Troms i 2019", 
     xlab = "nEQR-verdi", 
     ylab = "Trolighet", 
     cex.lab = 1.2, cex.main = 1.8)

library(raster)
load("norge.map")
source("adminenh.R")
plot(Norge.fylker, asp = 2.1)
text(6, 70, "ASPT", cex = 2.4, font = 1.6)
text(6, 69, "fylkesvis", cex = 0.96)
for (i in dimnames(utmating$fylke)$fylke) {
  plot(Norge.fylker[which(Norge.fylker@data$NAME_1 == fylke(i)), ],
       col=farge(min(1, utmating$fylke[i, "2019", 1])), add = T)
}
for (i in seq(0, 0.999, 0.001)) {
  rect(24, 59+i*8, 26, 59+(i+0.001)*8, col = farge(i), border = farge(i))
}
for (i in 1:5) {
  rect(24, 59+(i-1)*1.6, 26, 59+i*1.6, col = NA, border = T, lwd = 2.4)
}
text(rep(24, 6), 59+0:5*1.6, c("0,0", "0,2", "0,4", "0,6", "0,8", "1,0"), 
     pos = 2, cex = 0.96)
text(rep(26, 5), 59.8+0:4*1.6, c("SD", "D", "M", "G", "SG"), pos = 4, cex = 1.2)

plot(Norge.kontur, asp = 2.1, col = grey(0.84))
text(6, 70, "ASPT", cex = 2.4, font = 1.6)
text(6, 69, "kommunevis", cex = 0.96)
for (i in dimnames(utmating$kommune)$kommune) {
  for (kmn in kommunehistorikk[which(kommunehistorikk[, "2008"] == i), "1992"]) {
    plot(Norge.kommuner[which(Norge.kommuner@data$NAME_2 == kmn),], 
         col=farge(min(1, utmating$kommune[i, "2019", 1])), border = NA, add = T)
  }
}
plot(Norge.fylker, add = T)
for (i in seq(0, 0.999, 0.001)) {
  rect(24, 59+i*8, 26, 59+(i+0.001)*8, col = farge(i), border = farge(i))
}
for (i in 1:5) {
  rect(24, 59+(i-1)*1.6, 26, 59+i*1.6, col = NA, border = T, lwd = 2.4)
}
text(rep(24, 6), 59+0:5*1.6, c("0,0", "0,2", "0,4", "0,6", "0,8", "1,0"), 
     pos = 2, cex = 0.96)
text(rep(26, 5), 59.8+0:4*1.6, c("SD", "D", "M", "G", "SG"), pos = 4, cex = 1.2)


