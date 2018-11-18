#plotting functions
# install.packages('plot3D') # to install this package uncomment the line
library(plot3D)
sinx <- function(x) sin(x)
curve(sinx, -pi, pi)
# 3D Plot of Half of a Torus
par(mar = c(2, 2, 2, 2))
par(mfrow = c(1, 1))
R <- 3
r <- 2
x <- seq(0, 2*pi,length.out=50)
y <- seq(0, pi,length.out=50)
M <- mesh(x, y)

alpha <- M$x
beta <- M$y


surf3D(x = (R + r*cos(alpha)) * cos(beta),
       y = (R + r*cos(alpha)) * sin(beta),
       z = r * sin(alpha),
       colkey=FALSE,
       bty="b2",
       main="Half of a Torus")

func1 <- function(x) 3*x^4 + 4*x^3
plot(func1, -1.35, .5, axes = FALSE, col = 'blue')
axis(1, pos = 0, at = seq(-2,1,.5))
axis(2, pos = 0, at = seq(-2,2,.5))

func2 <- function(x) sqrt(2*x - x^2)
func3 <- function(x) x^2
plot(func2, 0,2, axes = FALSE, col = 'red', type = 'l', ylim = range(c(0,2)), xlab = 'x', ylab = 'y')
#enable multiplot
par(new = TRUE)
plot(func3, 0,2, axes = FALSE, col = 'blue', type = 'l', ylim = range(c(0,2)), xlab = '', ylab = '')
axis(1, pos = 0, at = seq(0,2,1))
axis(2, pos = 0, at = seq(0,2,1))

#another multiplot
f1 <- function(x) sin(2*x)
f2 <- function(x) sin(3*x)
f3 <- function(x) sin(4*x)
plot(f1, 0,2, axes = TRUE, col = 'red', type = 'l', xlab = 'x', ylab = 'y'); par(new = TRUE)
plot(f2, 0,2, axes = TRUE, col = 'red', type = 'l', xlab = 'x', ylab = 'y'); par(new = TRUE)
plot(f3, 0,2, axes = TRUE, col = 'red', type = 'l', xlab = 'x', ylab = 'y'); par(new = TRUE)

#volumes of solids of revolution around a line
Vol <- function(a, b,fx, f1x, gx, g1x) {
  integrand <- function(x) { (fx(x) - gx(x))^2 * (1 + f1x(x) * g1x(x)) / (g1x(x)^2 + 1)^(3/2); }
  pi * integrate(integrand, lower = a, upper = b)$value;
}
#example 1
fx <- function(x) { x + sin(x) }; f1x <- function(x) { 1 + cos(x) }
gx <- function(x) { x - 2 }; g1x <- function(x) { 1 }
Vol(0, 2*pi, fx, f1x, gx, g1x)

#example 2
fx <- function(x) { x^2 }; f1x <- function(x) { 2*x }
gx <- function(x) { sqrt(2*x-x^2) }; g1x <- function(x) { (1-x)/sqrt(2*x-x^2) }
Vol(0, 1, fx, f1x, gx, g1x)

#remove multiplot
par(new = FALSE)
#intercept
func2 <- function(x) sin(x)
func3 <- function(x) x^2
plot(func2, 0,pi, axes = FALSE, col = 'red', type = 'l', ylim = range(c(0,pi)), xlab = 'x', ylab = 'y')
par(new = TRUE)
plot(func3, 0,pi, axes = FALSE, col = 'blue', type = 'l', ylim = range(c(0,pi)), xlab = '', ylab = '')
axis(1, pos = 0, at = seq(0,pi,1))
axis(2, pos = 0, at = seq(0,pi,1))

#tootSolve package
# install.packages('rootSolve')
library(rootSolve)

# roots for intersection y=sin(x), y=x^2
roots <- multiroot(function(x) { sin(x) - x^2}, start = c(0,2))$root

#example volume 3 parable around sine
fx <- function(x) { x^2 }; f1x <- function(x) { 2*x }
gx <- function(x) { sin(x) }; g1x <- function(x) { cos(x) }
Vol(roots[1], roots[2], fx, f1x, gx, g1x)

#example volume 4 sine around parable
fx <- function(x) { sin(x) }; f1x <- function(x) { cos(x) }
gx <- function(x) { x^2 }; g1x <- function(x) { 2*x }
Vol(roots[1], roots[2], fx, f1x, gx, g1x)
