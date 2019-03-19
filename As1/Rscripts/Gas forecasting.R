ngasf<-read.delim("ngasf.txt",header=T)
attach(ngasf)
names(ngasf)
gas=as.ts(ngasf$ngasf)
plot(gas)

gas.rets<-100*diff(log(gas))
plot(gas.rets)
head(gas.rets)

gas.tr<-as.ts(gas.rets[1:3184])
gas.te<-as.ts(gas.rets[3185:4434])
plot(gas.tr)
plot(gas.te)

library(tseries)
adf.test(gas.rets)

##Summary---------------
summary(gas.rets)
sd(gas.rets)
library(e1071)
skewness(gas.rets)
kurtosis(gas.rets)

##Density plot---------------

plot(density(gas.rets))
hist(gas.rets,freq=FALSE, xlab="",ylab="",main="distribution of gas return",
     xlim=c(-15,15),ylim=c(0,0.3),
     col="pink",breaks=100,las=1)
curve(dnorm(x,mean=mean(gas.rets),sd=sd(gas.rets)),add=TRUE,col="darkblue",lwd=2)

##Normality-----------

qqnorm(gas.rets)
qqline(gas.rets,col="red",lwd=3)

library(tseries)  
jarque.bera.test(gas.rets)

library(fBasics)
normalTest(ngas,method="jb",na.rm=TRUE)




#oilAR(1)

library(forecast)
gasar1<-arima(gas.tr,order=c(1,0,0),method="ML",include.mean = T)
plot(gasar1)

library(portes);
re_model1 <- portest(model1, lags=c(5, 10), test=c("LjungBox"), SquaredQ=FALSE)
tsdiag(model1)
resid.model1<-residuals(model1)
plot(resid.model1)

#foc without re-estimation
gasar1_foc<-forecast(gasar1,h=1)
print(gasar1_foc)

#----rolling window---------------
fun11<-function(x){
  model11<-arima(x,order=c(1,0,0))
  return(forecast(model11,h=1)$mean)
}
length(gas.tr)

require(zoo)
roll.gasar1<-rollapply(gas.rets,width = 3185,FUN=fun11,align = "right")
length(roll.gasar1)
plot(roll.gasar1)
print(roll.gasar1)

plot(gas.rets)
par(new=T)
lines(roll.gasar1, col="pink", lwd=2)

accuracy (roll.gasar1[1:1250],gas.te[1:1250])


#oilMA(1)
gasma1<-arima(gas.tr,order=c(0,0,1),method="ML",include.mean = T)
plot(gasma1)

#---without restimation-----------------
library(forecast)
gasma1_foc<-forecast(gasma1,h=1250)
plot(gasma1_foc)

#-----rolling window------------------
fun22<-function(x){
  model22<-arima(x,order=c(0,0,1))
  return(forecast(model22,h=1)$mean)
}

roll.gasma1<-rollapply(gas.rets,
                       width = 3184,FUN=fun22,
                       align = "right")
plot(roll.gasma1)

plot(gas.rets)
par(new=T)
lines(roll.gasma1,col="pink",lwd=2)

#oilArinma(1,0,1)
gasarima11<-arima(gas.tr,order=c(1,0,1),method="ML",include.mean = T)
plot(gasarima11)
tsdiag(gasarima11)
#-----without re-estimation------
gasarima11_foc<-forecast(gasarima11,h=1250)
plot(gasarima11_foc)

#-----rolling window-----------
fun33<-function(x){
  model33<-arima(x,order=c(1,0,1))
  return(forecast(model33,h=1)$mean)
}

roll.gasarima11<-rollapply(gas.rets,
                           width = 3184,FUN=fun33,
                           align = "right")


plot(roll.gasarima11)

plot(gas.rets)
par(new=T)
lines(roll.gasarima11,col="pink",lwd=2)





#----naive--------------
naivef_gas<-rwf(gas.tr,h=1250)
plot(naivef_gas)

#historical mean
mean_gas<-mean(gas.tr)
meanf_gas<-forecast(mean_gas,h=1250)
plot(meanf_gas)
plot(gas.rets)
par(new=T)
lines(meanf_gas,col="pink",lwd=2,align="right")
accuracy(meanf_gas,oil.te[1:1250])
