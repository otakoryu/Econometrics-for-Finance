heatoil<-read.delim("heatoilf.txt",header=T)
attach(oil)
names(heatoil)
heat=as.ts(heatoil$heatoilf)
plot(heat)

require(xts)
require(TTR)
library(quantmod)
chartSeries(heat.rets,type = "candles")

heat.rets<-100*diff(log(heat))
plot(heat.rets)

heat.tr<-as.ts(heat.rets[1:3184])
heat.te<-as.ts(heat.rets[3185:4434])

plot(heat.tr)
plot(heat.te)

library(tseries)
adf.test(heat.rets)

##Sumaary------------

summary(heatoil.rets)
sd(heatoil.rets)
skewness(heatoil.rets)
kurtosis(heatoil.rets)

##Density plot-------

plot(density(heatoil.rets))
hist(heatoil.rets,freq=FALSE, xlab="",ylab="",main="distribution of heatoil return",
     xlim=c(-15,15),ylim=c(0,0.3),
     col="pink",breaks=100,las=1)
curve(dnorm(x,mean=mean(heatoil.rets),sd=sd(heatoil.rets)),add=TRUE,col="darkblue",lwd=2)

##Normality-----------

qqnorm(heatoil.rets)
qqline(heatoil.rets,col="red",lwd=3)

library(tseries) 
jarque.bera.test(heatoil.rets) 

library(fBasics)
normalTest(heatoil,method="jb",na.rm=TRUE)



#oilAR(1)

library(forecast)
heatar1<-arima(heat.tr,order=c(1,0,0),method="ML",include.mean = T)
plot(heatar1)

library(portes);
re_model1 <- portest(model1, lags=c(5, 10), test=c("LjungBox"), SquaredQ=FALSE)
tsdiag(model1)
resid.model1<-residuals(model1)
plot(resid.model1)

#foc without re-estimation
heatar1_foc<-forecast(heatar1,h=1250)
plot(heatar1_foc)

#----rolling window_AR1---------------
fun111<-function(x){
  model111<-arima(x,order=c(1,0,0))
  return(forecast(model111,h=1)$mean)
}
length(heat.tr)

require(zoo)
roll.heatar1<-rollapply(heat.rets,width = 3184,FUN=fun111,align = "right")
length(roll.heatar1)
plot(roll.heatar1)
print(roll.heatar1)
head(roll.heatar1,6)

plot(heat.rets)
par(new=T)
lines(roll.heatar1, col="pink", lwd=2)

accuracy (roll.heatar1,oil.te[1:1250])


#oilMA(1)
heatma1<-arima(heat.tr,order=c(0,0,1),method="ML",include.mean = T)
plot(heatma1)

#---without restimation-----------------
heatma1_foc<-forecast(heatma1,h=1250)
plot(heatma1_foc)

#-----rolling window------------------
fun222<-function(x){
  model222<-arima(x,order=c(0,0,1))
  return(forecast(model222,h=1)$mean)
}

roll.heatma1<-rollapply(heat.rets,
                       width = 3184,FUN=fun222,
                       align = "right")
plot(roll.heatma1)
head(roll.heatma1,6)

plot(heat.rets)
par(new=T)
lines(roll.heatma1,col="pink",lwd=2)

#oilArinma(1,0,1)
heatarima11<-arima(heat.tr,order=c(1,0,1),method="ML",include.mean = T)
plot(heatarima11)
tsdiag(heatarima11)
#-----without re-estimation------
heatarima11_foc<-forecast(heatarima11,h=1250)
plot(heatarima11_foc)

#-----rolling window-----------
fun333<-function(x){
  model333<-arima(x,order=c(1,0,1))
  return(forecast(model333,h=1)$mean)
}

roll.heatarima11<-rollapply(heat.rets,
                           width = 3184,FUN=fun333,
                           align = "right")
plot(roll.heatarima11)
head(roll.heatarima11,6)

plot(heat.rets)
par(new=T)
lines(roll.heatarima11,col="pink",lwd=2)





#----naive--------------
naivef_heat<-rwf(heat.tr,h=1250)
plot(naivef_heat)

#historical mean
mean_heat<-mean(heat.tr)
meanf_heat<-forecast(mean_heat,h=1250)
plot(meanf_heat)
plot(heat.rets)
par(new=T)
lines(meanf_heat,col="pink",lwd=2,align="right")
accuracy(meanf_heat,heat.te[1:1250])
