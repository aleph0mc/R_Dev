setwd('G:\\Documenti\\eBook\\R_statistics_language') #set working dir

gdp_main <-read.csv("GDP_main_countries.csv")
#transpose data frame
tgdp_main <- as.data.frame(t(gdp_main))
tgdp_main <- tgdp_main[-1,]
colnames(tgdp_main) <- colnames(tgdp_main) <- c('FR', 'GER', 'GR', 'IR', 'ITA', 'ESP', 'UK', 'USA')
tgdp_main$Year <- 1971:2019; tgdp_main$Year[tgdp_main$Year > 2014] <- c(2014.1, 2015.1, 2015.2, 2015.3, 2020)
tgdp_main$Position <- tgdp_main$Year - 1970.0
rownames(tgdp_main) <- NULL

#tgdp_main <- tgdp_main[-1, ]
#tgdp_main$myfactor <- factor(row.names(tgdp_main))
#save to csv file
write.table(tgdp_main, file = "gdp_by_country_and_year.csv", sep = ",", col.names = T, row.names = F, qmethod = "double")

gdp_data <- read.csv("gdp_by_country_and_year.csv")

#regression model
# Italy
x <- gdp_data$Year[gdp_data$Year > 2013]; yI <- gdp_data$ITA[gdp_data$Year > 2013]
fit <- lm(yI ~ x)
predict(fit,data.frame(x=2016:2019), interval="prediction")
plot(x, yI, type = 'l', col = 'green', ylim = c(-.7, 2.7), xlab = 'Percent GDP', ylab = 'Year')

# France
yF <- gdp_data$F[gdp_data$Year > 2013]
lines(x, yF, col = 'blue')

# USA
yUSA <- gdp_data$USA[gdp_data$Year > 2013]
lines(x, yUSA, col = 'red')

# Germany
yD <- gdp_data$D[gdp_data$Year > 2013]
lines(x, yD, col = 'yellow')


# UK
yUK <- gdp_data$UK[gdp_data$Year > 2013]
lines(x, yUK, col = 'black')

# read global data
gdTop <- read.csv("global_data_by_top_countries.csv")
gdTop$ID <- 1:10
# barplots
par(mfrow = c(1,2))
barplot(gdTop$GDP_dollars, gdTop$ID, names.arg = gdTop[,1], las = 2, col = 'lightblue', main = 'GDP in dollars')
barplot(gdTop$Jobless_rate_pc, gdTop$ID, names.arg = gdTop[,1], las = 2, col = 'yellow', main = 'Jobless rate')

