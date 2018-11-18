#functions
add2 <- function(x,y) {
  x + y
}

above <- function(x, n = 10) {
  use <- x > n
  x[use]
}

columnmean <- function(frameOrMatrix, removeNA = T) {
  nc <- ncol(frameOrMatrix)
  means <- numeric(nc) #initialize a 0 vector with length the number of columns
  for(i in 1:nc) {
    means[i] <- mean(frameOrMatrix[,i], na.rm = removeNA)
  }
  means
}

#elipsys for multiple parameters
myplot <- function(x, y, type = 'l', ...) {
  plot(x, y, type = type, ...)
}


#Maximizing a Normal Likelihood
#constructor function
make.NegLogLik <- function(data, fixed=c(F,F)) {
  params <- fixed
  function(p) {
    params[!fixed] <- p
    mu <- params[1]
    sigma <- params[2]
    a <- -.5*length(data)*log(2*pi*sigma^2)
    b <- -.5*sum(data-mu)^2 / (sigma^2)
    -(a + b)
  }
}

#compute avg by rows (dim = 1) or columns (dim = 2, default) in a matrix skipping na vlues
matrixMean <- function(mat, dim = 2){
  apply(mat, dim, function(comp) { mean(comp[complete.cases(comp)]) })
}

#apply a function on a matrix component (rows, columns), default
applyOnMatrix <- function(mat, dim = 2, func = mean) {
  apply(mat, dim, function(comp) { func(comp[complete.cases(comp)]) })
}

#apply a function on a matrix component (rows, columns), default
applyOnMatrix2 <- function(mat, func = mean, dim = 2) {
  apply(mat, dim, function(comp) { func(comp, na.rm = TRUE) })
}

#print message
printmessage <- function(x) {
  if (is.na(x))
    print("x is a missing value")
  else if (x > 0)
    print("x is greater than 0")
  else
    print("x is less than 0 or equal to")
  
  invisible(x) #hide the output of a possible computation, in this context is useless.
}

#tapply(mtcars$mpg, mtcars$am, mean)
