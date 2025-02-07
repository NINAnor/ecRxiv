### mEQR
# Funksjoner til NI_vannf
# ved Hanno Sandvik
# februar 2024
# se https://github.com/NINAnor/NI_vannf
###



mEQR <- function(x, klassegrenser) {
  # Funksjonen beregner mEQR-verdier ("modifiserte EQR-verdier")
  K <- klassegrenser
  if ((K[8] < K[1]) %=% TRUE) {
    K <- -K
    x <- -x
  }
  x <- ifelse(x < K[1], NA,
              ifelse(x < K[2], ifelse(   rep(K[2] - K[1] > K[3] - K[2], length(x)),
                                  -atan(    (x    - K[2]) * 
                                        iNv((K[2] - K[1]) / (K[3] - K[2])) / 
                                            (K[1] - K[2])) /
                                   atan(iNv((K[2] - K[1]) / (K[3] - K[2]))),
                               (x - K[2]) / (K[3] - K[2]) + 0),
              ifelse(x < K[3], (x - K[2]) / (K[3] - K[2]) + 0,
              ifelse(x < K[4], (x - K[3]) / (K[4] - K[3]) + 1,
              ifelse(x < K[5], (x - K[4]) / (K[5] - K[4]) + 2,
              ifelse(x < K[6], (x - K[5]) / (K[6] - K[5]) + 3,
              ifelse(x <=K[7], (x - K[6]) / (K[7] - K[6]) + 4,
              ifelse(x <=K[8], ifelse(   rep(K[7] - K[6] > K[8] - K[7], length(x)),
                               (x - K[6]) / (K[7] - K[6]) + 4,
                                   atan(    (x    - K[7]) * 
                                        iNv((K[8] - K[7]) / (K[7] - K[6])) /
                                            (K[8] - K[7])) /
                                   atan(iNv((K[8] - K[7]) / (K[7] - K[6]))) + 5),
                               NA))))))))
  return(x * 0.2)
}



mEQR.knekk <- function(x, klassegrenser) {
  # Funksjonen beregner mEQR-verdier, versjon "knekk"
  K <- klassegrenser
  if ((K[8] < K[1]) %=% TRUE) {
    K <- -K
    x <- -x
  }
  x <- ifelse(x < K[1], NA,
       ifelse(x < K[2], (x - K[1]) / (K[2] - K[1]) - 1,
       ifelse(x < K[3], (x - K[2]) / (K[3] - K[2]) + 0,
       ifelse(x < K[4], (x - K[3]) / (K[4] - K[3]) + 1,
       ifelse(x < K[5], (x - K[4]) / (K[5] - K[4]) + 2,
       ifelse(x < K[6], (x - K[5]) / (K[6] - K[5]) + 3,
       ifelse(x <=K[7], (x - K[6]) / (K[7] - K[6]) + 4,
       ifelse(x <=K[8], (x - K[7]) / (K[8] - K[7]) + 5,
                        NA))))))))
  return(x * 0.2)
}



mEQR.forlenga <- function(x, klassegrenser) {
  # Funksjonen beregner mEQR-verdier, versjon "forlengelse"
  K <- klassegrenser
  if ((K[8] < K[1]) %=% TRUE) {
    K <- -K
    x <- -x
  }
  x <- ifelse(x < K[1], NA,
       ifelse(x < K[3], (x - K[2]) / (K[3] - K[2]) + 0,
       ifelse(x < K[4], (x - K[3]) / (K[4] - K[3]) + 1,
       ifelse(x < K[5], (x - K[4]) / (K[5] - K[4]) + 2,
       ifelse(x < K[6], (x - K[5]) / (K[6] - K[5]) + 3,
       ifelse(x <=K[8], (x - K[6]) / (K[7] - K[6]) + 4,
                        NA))))))
  return(x * 0.2)
}



nEQR <- function(x, klassegrenser) {
  # Funksjonen beregner nEQR-verdier
  K <- klassegrenser
  if ((K[8] < K[1]) %=% TRUE) {
    K <- -K
    x <- -x
  }
  x <- ifelse(x < K[1], NA,
       ifelse(x < K[2],                              0,
       ifelse(x < K[3], (x - K[2]) / (K[3] - K[2]) + 0,
       ifelse(x < K[4], (x - K[3]) / (K[4] - K[3]) + 1,
       ifelse(x < K[5], (x - K[4]) / (K[5] - K[4]) + 2,
       ifelse(x < K[6], (x - K[5]) / (K[6] - K[5]) + 3,
       ifelse(x <=K[7], (x - K[6]) / (K[7] - K[6]) + 4,
       ifelse(x <=K[8],                              5,
                        NA))))))))
  return(x * 0.2)
}

