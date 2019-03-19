##Question A(i)
oil<-read.delim("oilf.txt",header=T)
attach(oil)
names(oil)
oil=as.ts(oil$oilf)
plot(oil)

oil.rets<-100*diff(log(oil))
plot(oil.rets)

oil.tr<-as.ts(oil.rets[1:3184])
oil.te<-as.ts(oil.rets[3185:4434])

plot(oil.tr)
plot(oil.te)

library(tseries)
adf.test(oil.rets)


##---Summary----
summary(oil.rets)
library(e1071)
var(oil.rets)
sd(oil.rets)
skewness(oil.rets)
kurtosis(oil.rets)


##--density plot----
plot(density(oil.rets))
hist(oil.rets,freq=FALSE, xlab="log returns",ylab="probability",
     main="distribution of oil return",
     xlim=c(-15,15),ylim=c(0,0.3),
     col="pink",breaks=100,las=1)

curve(dnorm(x,mean=mean(oil.rets),
            sd=sd(oil.rets)),add=TRUE,
      col="darkblue",lwd=2)

###Normality---------------------------

qqnorm(oil.rets)
qqline(oil.rets,col="red",lwd=3)

library(tseries)  
jarque.bera.test(oil.rets)

library(fBasics)
normalTest(oil.rets,method="jb",na.rm=TRUE)


#oilAR(1)

library(forecast)
oilar1<-arima(oil.tr,order=c(1,0,0),method="ML",include.mean = T)
plot(oilar1)

library(portes);
re_model1 <- portest(oilar1, lags=c(5, 10), test=c("LjungBox"), SquaredQ=FALSE)
tsdiag(oilar1)
resid.model1<-residuals(oilar1)
plot(resid.model1)

#foc without re-estimation
oilar1_foc<-forecast(oilar1,h=1250)
plot(oilar1_foc)

#----rolling window---------------
fun1<-function(x){
  model<-arima(x,order=c(1,0,0))
  return(forecast(model,h=1)$mean)
}
length(oil.tr)

require(zoo)
roll.oilar1<-rollapply(oil.rets,width = 3184,FUN=fun1,align = "right")
length(roll.oilar1)
plot(roll.oilar1)
print(roll.oilar1)
tail(roll.oilar1,2)

plot(oil.rets)
par(new=T)
lines(roll.oilar1, col="pink", lwd=2)

accuracy (roll.oilar1,oil.te[1:1250])



#oilMA(1)
oilma1<-arima(oil.tr,order=c(0,0,1),method="ML",include.mean = T)
plot(oilma1)

#---without restimation-----------------
oilma1_foc<-forecast(oilma1,h=1250)
plot(oilma1_foc)

#-----rolling window------------------
fun2<-function(x){
  model2<-arima(x,order=c(0,0,1))
  return(forecast(model2,h=1)$mean)
}

roll.oilma1<-rollapply(oil.rets,
                   width = 3184,FUN=fun2,
                   align = "right")
plot(roll.oilma1)

plot(oil.rets)
par(new=T)
lines(roll.oilma1,col="pink",lwd=2)

#oilArinma(1,0,1)
oilarima11<-arima(oil.tr,order=c(1,0,1),method="ML",include.mean = T)
plot(oilarima11)
tsdiag(oilarima11)
#-----without re-estimation------
oilarima11_foc<-forecast(oilarima11,h=1250)
plot(oilarima11_foc)

#-----rolling window-----------
fun3<-function(x){
  model3<-arima(x,order=c(1,0,1))
  return(forecast(model3,h=1)$mean)
}

roll.oilarima11<-rollapply(oil.rets,
                    width = 3184,FUN=fun3,
                    align = "right")
plot(roll.oilarima11)

plot(oil.rets)
par(new=T)
lines(roll.oilarima11,col="pink",lwd=2)





#----naive--------------
naivef1<-rwf(oil.tr,h=1250)
plot(naivef1)

#historical mean
mean<-mean(oil.tr)
meanf1<-forecast(mean,h=1250,align="right")
plot(meanf1)
plot(oil.rets)
par(new=T)
lines(mean,col="pink",lwd=2,align="right")
accuracy(mean,oil.te[1:1250])


#SMA
library(TTR)
#20
sma20 <- SMA(oil.rets[3165:4434], 20)    
sma20f <- forecast(sma20, 1250)   
plot(sma20f)
print(sma20f)
length(sma20f)
print(sma20)
head(sma20,80)

#60
model6_60<-SMA(oil.tr,60)
model6_60_foc<-forecast(model6_60,1250)
plot(model6_60_foc)
print(model6_60_foc)

#180
model6_180<-SMA(oil.tr,180)
model6_180_foc<-forecast(model6_180,1250)
plot(model6_180_foc)
print(model6_180_foc)


###SMA---
library(forecast)
require(smooth)
require(Mcomp)
