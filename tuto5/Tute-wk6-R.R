

library(forecast); 
library(stats); # the packages used

############ Tutorial question 2 #############

crude.oil <- read.delim("crude.txt", header=T)
attach(crude.oil)
crude.pr <- ts(crude)
crude.rets <- 100*diff(log(crude.pr))
plot(crude.pr, main="Crude oil for the year 2016")
plot(crude.rets, main="Crude oil returns")
is.ts(crude.pr)
is.ts(crude.rets)
chartSeries(crude.rets)

#use 200 as training set and keep 5 as test set

fit.arima <- Arima(crude.rets[1:201], order=c(1,0,1), include.mean=T)
print(fit.arima)
forc.crude = predict(fit.arima, n.ahead=5)
print(forc.crude)
residuals(fit.arima)
fitted(fit.arima)
plot(fit.arima)

coef(fit.arima)

## R calculates the forecasts as follows

fc1 = 0.1703*(1-(-0.6093)) + (-0.6093)*(2.1124) +  0.6999*(1.9346)
fc1

fc2 = 0.1703*(1-(-0.6093)) + (-0.6093)*fc1
fc2

fc3 = 0.1703*(1-(-0.6093)) + (-0.6093)*fc2
fc3

fc4 = 0.1703*(1-(-0.6093)) + (-0.6093)*fc3
fc4

fc5 = 0.1703*(1-(-0.6093)) + (-0.6108313)*fc4
fc5

forecasts <- c(fc1, fc2, fc3, fc4, fc5); 
print(forecasts)

fc <- forecast(fit.arima, h=5)
print(fc)

accuracy(f=c(fc[[4]][[1]], fc[[4]][[2]], fc[[4]][[3]], fc[[4]][[4]], fc[[4]][[5]]), x=tail(crude.rets, 5))

accuracy(f=c(0.34113448, 0.06624718, 0.23372275, 0.13168797, 0.19385284), 
         x=c(-1.41651001, -1.33641163, -0.04016871, -0.24135169, 0.00000000))

accuracy(f=c(0.341005, 0.06628944, 0.23372275, 0.1316864,  0.1936256), 
         x=c(-1.41651001, -1.33641163, -0.04016871, -0.24135169, 0.00000000))

accuracy(forecasts,crude.rets[202:206])

ob<-c(-1.41651001, -1.33641163, -0.04016871, -0.24135169, 0.00000000)
accuracy(forecasts,ob)


############ Tutorial question 3 #############


#### i. white noise #### 

set.seed(600)
whitenoise = rnorm(n=500, m=0, sd=1)
acf(whitenoise) 
pacf(whitenoise)


#### ii AR(2) model ###

library(forecast)

#simulation of model can be done by function below
set.seed(645)
ar2.sim <- arima.sim(list(order=c(2,0,0), ar=c(0.8, 0.15)), n=500)

ar2.sim.plot <- plot(arima.sim(list(order=c(2,0,0), ar=c(0.8, 0.05)), n=500), ylab="x", 
     main=(expression(AR(2)~ ~ ~ phi_1==+.8 ~ ~ ~ phi_2==+.05)))

Acf(ar2.sim, 50)
Pacf(ar2.sim, 50)

#### iii MA(1) model ###

library(forecast)

set.seed(467)
ma1.sim <- arima.sim(list(order=c(0,0,1), ma=c(0.8)), n=500)
Acf(ma1.sim, 50)
Pacf(ma1.sim, 50)


#### vi ARMA(2,1) model ###

library(forecast)

set.seed(540)
arma21.sim <- arima.sim(list(order=c(2,0,1), ar=c(0.75, 0.16), ma=c(0.8)), n=500)
Acf(arma21.sim, 50)
Pacf(arma21.sim, 50)
