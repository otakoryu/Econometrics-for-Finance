# to know the number of observations in the specific data
length(crude.rets)

library(forecast)
arima_au1<-auto.arima(crude.rets[1:201],ic="aic",stepwise=F, trace=F, approximation=F)
arima_au1
plot(arima_au1)

tail(crude.rets[1:201],1) #last value of training set
tail(residuals(fit.arima),1) #last value of residual

train<-crude.rets[1:201]
crude.rets.train<-ts(train)
plot(crude.rets.train)

test<-crude.rets[202:206]
crude.rets.test<-ts(test)
plot(crude.rets.test)

library(tseries)
adf.test(crude.rets.train)

model<-arima(crude.rets.train,order=c(1,0,1),include.mean = T)
model

tsdiag(model)


