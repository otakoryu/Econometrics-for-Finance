getwd()
setwd("C:/Users/Adel/Downloads")

library(fpp)
ausair

plot(ausair, main = "Total Annual Air Passengers: 1970-2009", xlab =
       "Year", ylab = "Passengers (million)")
#Upward trend, non linear, exponential growth

air_train = window(ausair, start = 1970, end = 2005)
air_test = window(ausair, start = 2006)

smt1=holt(air_train, exponential = TRUE, initial ="optimal")

summary(smt1)

smt1 = holt(air_train, exponential = TRUE, initial = "optimal", h = 4)
smt1

plot(ausair, xlab="Year", ylab = "Passengers(millions)", main= "Forecasts from Exponential Trend Method")

lines(smt1$fitted, col=2 , type ="o")

lines(smt1$mean, col=3, type ="o")

accuracy(smt1, air_test)

states <- smt1$model$states[,1:2]
colnames(states) <-c("level", "slope")
states
plot(states)

#3
smt2= holt(air_train, exponential =FALSE, intial ="optimal", damped= TRUE)
summary(smt2)

smt2= holt(air_train, exponetial= FALSE, intial ="optimal", damped= TRUE, h=4)
smt2

plot(ausair,xlab="YEar", ylab="PAssengers(m)",main="Froecasts from Additive Damoed Trend Methos")

lines(smt2$fitted , col =2, type ="o")

lines(smt2$mean, col=3,type="o")

accuracy(smt2, air_test)

fit3 = holt(air_train, exponential =FALSE, initial = "optimal", damped
            = TRUE, h = 50)
plot(fit3, type = "o")

fit4 = holt(air_train, exponential =FALSE, initial = "optimal", damped
            = FALSE, h = 50)
plot(fit4, type = "o")

#4 smt5 =