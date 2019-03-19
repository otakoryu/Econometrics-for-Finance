crude.oil<-read.delim("crude.txt",header=T)
attach(crude.oil)
crude<-ts(crude)
plot(crude)
crude.r<-100*diff(log(crude))
plot(crude.r)
library("quantmod")
chartSeries(crude.r,type = "candles")
candleChart(crude.r)


acf(crude.r)
pacf(crude.r)

library(tseries)
adf.test(crude.r)

length(crude.r)

crude.rt<-crude.r[1:201]
crude.te<-crude.r[202:206]
crude.tr<-ts(crude.rt)
crude.te<-ts(crude.te)
plot(crude.te)

library(forecast)
model1<-arima(crude.tr,order=c(1,0,1), include.mean=T)
summary(model1)
plot(model1)
tsdiag(model1)

#-------------------
##Naive model
f_rw<-rwf(crude.tr,h=5)
f_mean<-mean(crude.tr,h=5)
plot(f_rw)
plot(f_mean)
#--------------------------

library(forecast)
model1_foc<-forecast(model1, h=5)
plot(model1_foc)
accuracy(model1_foc)
accuracy(model1_foc,crude.te[1:5])


library(xts)
data(crude.tr)
crude.tr.test<-as.xts(crude.tr)

my_fc<-function(crude.rets){
  model<-arima(crude.tr,order=c(1,0,1))
  return(forecast(model,h=1))
}


library(zoo)
model2_foc<-rollapplyr(crude.tr,200,my_fc)

library(xts)
data(sample_matrix)
test <- as.xts(sample_matrix)

myFun <- function(x) {
  x$xt <- seq(1-nrow(x),0)
  lm(Close ~ poly(xt,4), data=x)
}
test1 <- rollapplyr(test, width=20, FUN=myFun, by.column=FALSE)

