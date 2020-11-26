# install.packages("readr")
# install.packages("forecast")
# or
# install.packages(c("readr", "forecast"))
setwd("C:/Projects/GitHub/R_Dev")
# co19total <- scan(file.path("c:", "Users/.../Documents/.../co_total.dat"), skip = 1, what = list(total_cases=0))
# or
library(readr)
co19total <- read_csv("Repo/co_total.dat")
co19totalts <- ts(co19total$total_cases, start=c(1))
plot.ts(co19totalts)
library("forecast")
co19totalforecast <- HoltWinters(co19totalts, beta=FALSE, gamma=FALSE)
plot(co19totalforecast)
# co19totalforecast
# co19totalforecast$fitted
# co19totalforecast$SSE
co19totalforecast2 <- forecast:::forecast.HoltWinters(co19totalforecast, h=60)
# co19totalforecast2
plot(forecast(co19totalforecast2))