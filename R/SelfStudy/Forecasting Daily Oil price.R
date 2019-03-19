#--------------------------
#Using Full training set
#--------------------------
#Making time series objects
read.csv("crude_daily.csv", header=T)
attach(crude_daily)
head(crude_daily);tail(crude_daily)
names(crude)
crude<-ts(crude)
plot(crude)

chartSeries(crude)

#ADF test
library(tseries)
adf.test(crude)
#result returns, p-value is 0.01 which means H0(data is unit root) was rejected


#Conversion into Log return
crude.ret<- 100*diff(crude, lag=1);crude.ret<- as.ts(crude.ret)
plot(crude.ret)

#ADF test
libarary(tseries)
adf.test(crude.ret)

#auto.correl
acf(crude)
acf(crude.ret)

#partial auto correl
acf(crude)
pacf(crude.ret)

#auto model selection for crude.ret
library(forecast)
Fit<-auto.arima(crude.ret,ic="aic",trace=F,stepwise=F,approximation = F,allowdrift=F);Fit
# selected model is ARIMA(4,0,1)


##need to perform Ljung box test to check the non-exsistent of auto-correl of residuals
##statstically;Ho=non-exsistence of residual auto-correl >> should not be rejected 
#diagnosis of residuals
tsdiag(Fit)
#From residual aut-correl plot, can't observe any line that goes behind blue line that means there is significant relation ship in data
#then could say proper model with non-auto-correl in residuals


#Forecast 10 ahead
plot(forecast(Fit, h=10))
abline(h=mean(crude.ret))

#-------------------------------------
#Keep 10% as training set with rolling 
#-------------------------------------

train<-crude.ret["::1-450"]

test<-crude.ret["451-500::"]

#Make function that makes one ahead prediciton with data entry
calcForecast<-function(data){
  model<-Arima(data,order=c(4,0,1))
  return(forecast(model,h=1)$mean)
}

library(zoo)
f_arima<- rollapply(crude.ret,450,calcForecast)
f_arima<-lag(f_arima)


