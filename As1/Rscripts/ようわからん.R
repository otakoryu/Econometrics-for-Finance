length(heatoil.rets.te)
str(heatoil.rets)
any(is.na(heatoil.rets[]))
sum(is.na(ngas.rets[]))

4434-1250

oil.rets.tr<-oil.rets[1:3184]
oil.rets.te<-oil.rets[3185:4434]

#---------------------
##oil forecast
#AR(1)
library(forecast)
modeloilar<-arima(oil.rets.tr,order=c(1,0,0))
model1<-forecast(modeloilar,h=1250)
plot(model1)
plot(modeloilar)

summary(model1)
accuracy(model1,oil.rets.te[1:1250])

#MA(1)
oilma<-arima(oil.rets.tr,order=c(0,0,1))
model2<-forecast(oilma,h=1250)
plot(model2)
plot(oilma)
tsdiag(model2)





#--------------------------------------------------------
my_function<-function(oil.rets){
  model<-arima(oil.rets.tr,order=c(1,0,0))
  return(forecast(model,h=1))
}

rolling<-rollapply(oil.rets,width=3184,FUN = my_function, by=1)


library(zoo)
library(xts)
modeloilar2<-rapply(oil.rets[1:3184],3184,my_function)
print(modeloilar2)

library(forecast)
library(quantmod)
library(xts)
oil.xts<-as.xts(oil,descr='new xts object')
class(oil)
plot(oil)

oil.xts.ret<-100*diff(log(oil))
oil.xts.tr<-oil.xts.ret[1:3184]
oil.xts.te<-oil.xts.ret[3185:4435]
oil.tr<-ts(oil.xts.tr)
oil.te<-ts(oil.xts.te)
ol.xts<-ts(oil.xts.ret)

my_function1<-function(oil.tr){
  model<-arima(oil.tr,order=c(1,0,0))
  return(forecast(model,h=1)$mean)
}
modeloilar2<-rollapply(oil.xts,20,my_function1)
print(modeloilar2)
lenght(modeloilar2)
rolling<-rollapply(oil.xts,width=3184,FUN = my_function1, by=1)
rolling
