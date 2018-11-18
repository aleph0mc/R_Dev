# http://www.convertalot.com/earthquake_power__calculator.html
richter <- function(joules) {
  e0 <- 10 ^ 4.8 # joules
  2 /3 * log10(joules / e0)
}

energy <- function(richter) {
  10 ^ (1.5 * richter + 4.8) # joules
}

energyTNT <- function(richter) {
  oneTon <- 4.184 * 10^9 #joules
  energy(richter) / oneTon
}

TNT2Joules <- function(TNT) {
  oneTon <- 4.184 * 10^9 #joules
  TNT * oneTon
}

energy(8)
energyTNT(8)
