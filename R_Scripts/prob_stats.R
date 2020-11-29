# Prob & Stats

normaldistr <- function(x, s, m) {
  1/(s*sqrt(2*pi)) * exp(-.5 * ((x - m)/s)^2)
}


# value of PDF (probability density function) at x = 3
# with mean = 2 and standard deviation = 5
dnorm(x = 3, mean = 2, sd = 5)
normaldistr(x = 3, m = 2, s = 5)
# simple linear regression
str(cars)
dim(cars)
nrow(cars)
ncol(cars)
x = cars$speed
y = cars$dist
Sxy = sum((x - mean(x)) * (y - mean(y)))
Sxx = sum((x - mean(x))^2)
Syy = sum((y - mean(y))^2)
beta_1_hat = Sxy / Sxx
beta_0_hat = mean(y) - beta_1_hat * mean(x)
c(beta_0_hat, beta_1_hat)
unique(cars$speed)
beta_0_hat + beta_1_hat * 8
# 8 is min the set of cars$speed in fact
8 %in% unique(cars$speed)
# lm function - linear model
stop_dist_model <- lm(dist ~ speed, data = cars)
plot(dist ~ speed, data = cars,
    xlab = "Speed (in Miles Per Hour)",
    ylab = "Stopping Distance (in Feet)",
    main = "Stopping Distance vs Speed",
    pch = 20,
    cex = 1,
    col = "grey")
abline(stop_dist_model, lwd = 3, col = "darkorange")
