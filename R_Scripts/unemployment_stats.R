setwd('C:\\R_Dev\\Repo') #set working dir

unemp <- read.csv("unemployment_rate_2007_2016.csv")
#print(unemp)
#print(unemp$Year)
plot(unemp$Year, unemp$Germany, main="UNEMPLOYMENT RATE 2007-2016", type="l", col="red", xlab = 'year', ylab = '%', ylim = range(c(3,28)))
#enable multiplot
par(new = TRUE)
plot(unemp$Year, unemp$Italy, main="UNEMPLOYMENT RATE 2007-2016", type="l", col="green", xlab = 'year', ylab = '%', ylim = range(c(3,28)))
par(new = TRUE)
plot(unemp$Year, unemp$Spain, main="UNEMPLOYMENT RATE 2007-2016", type="l", col="cyan", xlab = 'year', ylab = '%', ylim = range(c(3,28)))
par(new = TRUE)
plot(unemp$Year, unemp$France, main="UNEMPLOYMENT RATE 2007-2016", type="l", col="blue", xlab = 'year', ylab = '%', ylim = range(c(3,28)))
par(new = TRUE)
plot(unemp$Year, unemp$United_Kingdom, main="UNEMPLOYMENT RATE 2007-2016", type="l", col="magenta", xlab = 'year', ylab = '%', ylim = range(c(3,28)))
par(new = TRUE)
plot(unemp$Year, unemp$Greece, main="UNEMPLOYMENT RATE 2007-2016", type="l", col="black", xlab = 'year', ylab = '%', ylim = range(c(3,28)))
legend(2007, 28.5, c("ESP", "FRA","GER", "GR", "ITA", "UK"), lty=c(1,1,1,1,1,1), lwd=c(2,2,2,2,2,2), col=c("cyan", "blue", "red", "black", "green", "magenta"))
# regression model
x <- unemp$Year
y <- unemp$Italy
fit <-lm(y ~ x)
# prediction
predict(fit,data.frame(x=2017), interval="prediction")
# confidence
predict(fit,data.frame(x=2017), interval="confidence")
# noise
predict(fit,data.frame(x=2017), interval="none")
# confidence
confint(fit)
