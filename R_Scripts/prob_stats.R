# Prob & Stats

normaldistr <- function(x, s, m) {
  1/(s*sqrt(2*pi)) * exp(-.5 * ((x - m)/s)^2)
}


# value of PDF (probability density functoin) at x = 3
# with mean = 2 and standard deviatoin = 5
dnorm(x = 3, mean = 2, sd = 5)
normaldistr(x = 3, m = 2, s = 5)
# simple linear regression
str(cars)
dim(cars)
nrow(cars)
ncol(cars)