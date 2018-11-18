setwd('C:\\R_Dev\\Repo') #set working dir

hpex <- read.csv("heathPublicExpPerCent.csv")
#print(hpex)
#print(hpex$Year)
plot(hpex$Year, hpex$GER, main="HEALTH PUBLIC EXPENDITURE RATE 1995-2013", type="l", col="black", xlab = 'year', ylab = '%', ylim = range(c(3,28)))
#enable multiplot
par(new = TRUE)
plot(hpex$Year, hpex$ITA, main="HEALTH PUBLIC EXPENDITURE RATE 1995-2013", type="l", col="green", xlab = 'year', ylab = '%', ylim = range(c(3,28)))
par(new = TRUE)
plot(hpex$Year, hpex$FRA, main="HEALTH PUBLIC EXPENDITURE RATE 1995-2013", type="l", col="blue", xlab = 'year', ylab = '%', ylim = range(c(3,28)))
par(new = TRUE)
plot(hpex$Year, hpex$GR, main="HEALTH PUBLIC EXPENDITURE RATE 1995-2013", type="l", col="light blue", xlab = 'year', ylab = '%', ylim = range(c(3,28)))
par(new = TRUE)
plot(hpex$Year, hpex$IRL, main="HEALTH PUBLIC EXPENDITURE RATE 1995-2013", type="l", col="yellow", xlab = 'year', ylab = '%', ylim = range(c(3,28)))
par(new = TRUE)
plot(hpex$Year, hpex$GBR, main="HEALTH PUBLIC EXPENDITURE RATE 1995-2013", type="l", col="red", xlab = 'year', ylab = '%', ylim = range(c(3,28)))

