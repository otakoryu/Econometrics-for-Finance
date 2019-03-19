ngas<-read.delim("ngasf.txt",header=T)
attach(ngas)
names(ngas)
gas=as.ts(ngas$ngasf)
plot(gas)

gas.rets<-100*diff(log(gas))
plot(gas.rets)

gas.tr<-as.ts(gas.rets[1:3184])
gas.te<-as.ts(gas.rets[3185:4434])

plot(gas.tr)
plot(gas.te)

library(tseries)
adf.test(gas.rets)
library(forecast)
library(quantmod)

##---Summary----
summary(gas.rets)
library(e1071)
var(gas.rets)
sd(gas.rets)
skewness(gas.rets)
kurtosis(gas.rets)


##--density plot----
plot(density(gas.rets))


curve(dnorm(x,mean=mean(gas.rets),
            sd=sd(gas.rets)),add=TRUE,
      col="darkblue",lwd=2)

###Normality---------------------------

qqnorm(gas.rets)
qqline(gas.rets,col="red",lwd=3)

library(tseries)  
jarque.bera.test(gas.rets)

library(fBasics)
normalTest(gas.rets,method="jb",na.rm=TRUE)



##Rolling window with Loop---------------------------------------------------------------
#AR1
gasar1=list()
gasyhatar1=list()
for(i in seq(1,1250,1)){
  gasar1[[i]]<-arima(gas.rets[i:(3183+i)],
                     order=c(1,0,0),method="ML",
                     include.mean=T)
  gasyhatar1[[i]]<-forecast(gasar1[[i]],h=1)$mean
  print(gasyhatar1[[i]])
  print(i)
  
}
rollgasar1=as.numeric(unlist(gasyhatar1))
rollgasar1<-as.ts(rollgasar1)
head(rollgasar1,1)

accuracy(gas.te,rollgasar1)

plot(gas.te)
par(new=T)
lines(rollgasar1,col="red")


#MA1-----------------------------------
gasma1=list()
gasyhatma1=list()
for(i in seq(1,1250,1)){
  gasma1[[i]]<-arima(gas.rets[i:(3183+i)],
                     order=c(0,0,1),method="ML",
                     include.mean=T)
  gasyhatma1[[i]]<-forecast(gasma1[[i]],h=1)$mean
  print(gasyhatma1[[i]])
  print(i)
  
}
rollgasma1=as.numeric(unlist(gasyhatma1))
rollgasma1<-as.ts(rollgasma1)

plot(gas.te)
par(new=T)
lines(rollgasma1,col="red")


accuracy(gas.te,rollgasma1)



#ARIMA101-------------------------------------
gasarima1=list()
gasyhatarima1=list()
for(i in seq(1,1250,1)){
  gasarima1[[i]]<-arima(gas.rets[i:(3183+i)],
                        order=c(1,0,1),method="ML",
                        include.mean=T)
  gasyhatarima1[[i]]<-forecast(gasarima1[[i]],h=1)$mean
  print(gasyhatarima1[[i]])
  print(i)
  
}
rollgasarima1=as.numeric(unlist(gasyhatarima1))
rollgasarima1<-as.ts(rollgasarima1)

plot(gas.te)
par(new=T)
lines(rollgasarima1,col="red")

accuracy(gas.te,rollgasarima1)

#naive-------------------------

naivefgas=vector()
naivefgas<-rwf(gas.tr,h=1250)$mean
plot(naivefgas)
length(naivefgas)

head(naivefgas,10)

#historical mean------------

mean=vector()
for(i in 1:1250){
  mean[[i]]<-mean(gas.tr)
}
mean.fgas=as.ts(mean)
plot(gas.te)
par(new=T)
lines(mean.fgas,col="pink")

accuracy(gas.te,mean.fgas)



#SMA--------------------------------
#------------------------------------
  
#SMA20-----------
gasSMA20<-vector()
for(i in 1:1250){
  gasSMA20[i]<-mean(gas.rets[(3164+i):(3183+i)])
}
gasSMA20<-ts(gasSMA20)
head(gasSMA20,20)

plot(gas.te)
par(new=T)
lines(gasSMA20,col="red")

accuracy(gas.te,gasSMA20)


#SMA60---------------
gasSMA60<-vector()
for(i in 1:1250){
  gasSMA60[i]<-mean(gas.rets[(3124+i):(3183+i)])
}
gasSMA60<-ts(gasSMA60)
head(gasSMA60,20)

plot(gas.te)
par(new=T)
lines(gasSMA60,col="red")

accuracy(gas.te,gasSMA60)


#180----------------
gasSMA180<-vector()
for(i in 1:1250){
  gasSMA180[i]<-mean(gas.rets[(3004+i):(3183+i)])
}
gasSMA180<-ts(gasSMA180)
head(gasSMA180,20)

plot(gas.te)
par(new=T)
lines(gasSMA180,col="red")

##-Accuracy-----------
accuracy(gas.te,rollgasar1)
accuracy(gas.te,rollgasma1)
accuracy(gas.te,rollgasarima1)
accuracy(gas.rets[3185:4434],naivefgas)
accuracy(gas.te,mean.fgas)
accuracy(gas.te,gasSMA20)
accuracy(gas.te,gasSMA60)
accuracy(gas.te,gasSMA180)

