library(ggplot2)
mpg
table(mpg$drv)
table(mpg$drv) / nrow(mpg)
plot(hwy ~ displ, data = mpg,
     xlab = "Engine Displacement (in Liters)",
     ylab = "Miles Per Gallon (Highway)",
     main = "MPG (Highway) vs Engine Displacement",
     pch = 20,
     cex = 2,
     col = "dodgerblue")