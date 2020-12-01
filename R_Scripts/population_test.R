setwd("C:/Projects/GitHub/R_Dev")
library(readr)
pop <- read_delim("Repo/population.dat", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
# plot(Id ~ FracPop, data = pop,
#      xlab = "Years",
#      ylab = "Populatoin (thousand)",
#      main = "Years vs Population",
#      pch = 20,
#      cex = 1,
#      col = "red")
# 
plot(pop$Id, pop$FracPop,
     xlab = "Years",
     ylab = "Populatoin (thousand)",
     main = "Years vs Population",
     pch = 20,
     cex = 1,
     col = "red")
model <- lm(Year ~ FracPop, data = pop)
model$coefficients
model$residuals
model$fitted.values