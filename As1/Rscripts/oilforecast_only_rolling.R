library(tseries)
library(quantmod)

oilf<-read.table("oilf.txt",header=T)
names(oilf)
attach(oilf)

oil=as.ts(oilf$oilf)
plot(oil)

oil.rets<-100*diff(log(oil))
oil.rets<-ts(oil.rets)
plot(oil.rets)

par(mfrow=c(2,1))
plot(oil)
plot(oil.rets)

oil.tr = as.ts(oil.rets[1:3184])
oil.te = as.ts(oil.rets[3185:4434])

length(oil.te)


#oilAR(1)

library(forecast)
model1<-arima(oil.tr,order=c(1,0,0),method="ML",include.mean = T)
plot(model1)

library(portes);
re_model1 <- portest(model1, test=c("LjungBox"), SquaredQ=FALSE)
tsdiag(model1)
resid.model1<-residuals(model1)
plot(resid.model1)

model1_foc<-forecast(model1,h=1250)
plot(model1_foc)

##rolling----------------------------------

fun1<-function(oil.rets){
  model<-arima(oil.rets,order=c(1,0,0),method="ML",include.mean = T)
  return(forecast(model,h=1)$mean)
}


require(zoo)
rolling<-rollapply(ts(oil.rets),
                   width = 3185,
                   FUN=fun1,
                   align = "right")

length(rolling)
plot(rolling)
print(rolling)
head(rolling,6)
tail(rolling,6)

plot(oil.rets)
par(new=T)
lines(rolling, col="pink", lwd=2)

accuracy (rolling,oil.te[1:1250])


