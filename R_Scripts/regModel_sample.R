# library(UsingR)
data(diamond)
diamond
plot(diamond$carat, diamond$price,
     xlab = "Mass (carat)",
     ylab = "Price (SIN $)",
     bg = "lightblue",
     col = "black", cex = 1.1, pch = 21, frame = F)
fit <- lm(price ~ carat, data = diamond)
abline(fit, lwd = 2)
coef(fit)
fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
coef(fit2)
fit3 <- lm(price ~ I(carat * 10), data = diamond)
coef(fit3)

## predicting price of diamond given carats
newx <- c(.16, .27, .34)
##first way
coef(fit)[1] + coef(fit)[2] * newx
## best way is
predict(fit, newdata = data.frame(carat = newx))

# residuals
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x )
e <- resid(fit)
yhat <- predict(fit)
max(abs(e - (y - yhat)))
## direct computation
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))

## non-linear data
x <- runif(100, -3, 3); y <- x + sin(x) + rnorm(100, sd = .2)
plot(x, y); abline(lm(y ~ x))
# residual plot
plot(x, resid(lm(y ~ x)))
abline(h = 0)

## heteroskedasticity (or heteroschedasticity)
x <- runif(100, 0, 6); y <- x + rnorm(100, mean = 0, sd = .001 * x)
plot(x, y); abline(lm(y ~ x))
#plotting residuals
plot(x, resid(lm(y ~ x)))
abline(h = 0)

#residual variation
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
summary(fit)$sigma # residual standard error
sqrt(sum(resid(fit)^2) / (n - 2)) # how compute residual standard error

# anscombe
plot(anscombe$x1, anscombe$y1); abline(lm(anscombe$y1 ~ anscombe$x1))
plot(anscombe$x2, anscombe$y2); abline(lm(anscombe$y2 ~ anscombe$x2))
plot(anscombe$x3, anscombe$y3); abline(lm(anscombe$y3 ~ anscombe$x3))
plot(anscombe$x4, anscombe$y4); abline(lm(anscombe$y4 ~ anscombe$x4))

# mtcars: 95% confidence for expected mpg at average weight
x <- mtcars$wt; y <- mtcars$mpg
fit <- lm(y ~ x)
predict(fit, data.frame(x = mean(x)), interval = 'confidence')

# mtcars: 95% prediction for a new car weighing 3000 pounds for expected mpg
predict(fit,data.frame(x=3), interval="prediction")

# plotting prediction intervals
# library(UsingR)
data(diamond)
x <- diamond$carat; y <- diamond$price; n <- length(y)
fit <- lm(y ~ x)
plot(x, y, frame = F, xlab = 'Carat', ylab = 'Dollars', pch = 21, col = 'black', bg = 'lightblue', cex = 2)
abline(fit, lwd = 2)
xVals <- seq(min(x), max(x), by = .01)
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
e <- y - beta0 - beta1 * x
sigma <- summary(fit)$sigma # sqrt(sum(e^2) / (n - 2))
ssx <- sum(x - mean(x)^2)
yVals <- coef(fit)[1] + coef(fit) * xVals
se1 <- sigma * sqrt(1 /n + (xVals - mean(x))^2 / ssx)
se2 <- sigma * sqrt(1 + 1 /n + (xVals - mean(x))^2 / ssx)
lines(xVals, yVals + 2 * se1)
lines(xVals, yVals - 2 * se1)
lines(xVals, yVals + 2 * se2)
lines(xVals, yVals - 2 * se2)

## plotting lines
plot(mtcars$mpg,mtcars$hp, type='p'); abline(v = mean(mtcars$mpg), col='purple'); abline(h = mean(mtcars$hp))
