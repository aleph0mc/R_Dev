# install.packages("readr")
# install.packages("forecast")
# or
# install.packages(c("readr", "forecast"))
setwd("C:/Projects/GitHub/R_Dev")
# co19new <- scan(file.path("c:", "Users/.../Documents/.../co_new.dat"), skip = 1, what = list(new_cases=0))
# or
library(readr)
co19newts <- read_csv("Repo/co_new.dat")
co19newts <- ts(co19new$new_cases, start=c(1))
plot.ts(co19newts)
library(forecast)
co19newforecast <- HoltWinters(co19newts, beta=FALSE, gamma=FALSE)
plot(co19newforecast)
# co19newforecast
# co19newforecast$fitted
# co19newforecast$SSE
co19newforecast2 <- forecast:::forecast.HoltWinters(co19newforecast, h=60)
# co19newforecast2
plot(forecast(co19newforecast2))