hoilf<-read.delim("heatoilf.txt",header=T)
attach(hoilf)
names(hoilf)
ho=as.ts(hoilf$heatoilf)
plot(ho)

ho.rets<-100*diff(log(ho))
plot(ho.rets)

ho.tr<-as.ts(ho.rets[1:3184])
ho.te<-as.ts(ho.rets[3185:4434])

plot(ho.tr)
plot(ho.te)

library(tseries)
adf.test(ho.rets)
library(forecast)

##---Summary----
summary(ho.rets)
library(e1071)
var(ho.rets)
sd(ho.rets)
skewness(ho.rets)
kurtosis(ho.rets)


##--density plot----
plot(density(ho.rets))


curve(dnorm(x,mean=mean(ho.rets),
            sd=sd(ho.rets)),add=TRUE,
      col="darkblue",lwd=2)

###Normality---------------------------

qqnorm(ho.rets)
qqline(ho.rets,col="red",lwd=3)

library(tseries)  
jarque.bera.test(ho.rets)

library(fBasics)
normalTest(gas.rets,method="jb",na.rm=TRUE)



##Rolling window with Loop---------------------------------------------------------------
#AR1
hoar1=list()
hoyhatar1=list()
for(i in seq(1,1250,1)){
  hoar1[[i]]<-arima(ho.rets[i:(3183+i)],
                     order=c(1,0,0),method="ML",
                     include.mean=T)
  hoyhatar1[[i]]<-forecast(hoar1[[i]],h=1)$mean
  print(hoyhatar1[[i]])
  print(i)
  
}
rollhoar1=as.numeric(unlist(hoyhatar1))
rollhoar1<-as.ts(rollhoar1)
head(rollhoar1,1)


plot(ho.te)
par(new=T)
lines(rollhoar1,col="red")


#MA1-----------------------------------
homa1=list()
hoyhatma1=list()
for(i in seq(1,1250,1)){
  homa1[[i]]<-arima(ho.rets[i:(3183+i)],
                     order=c(0,0,1),method="ML",
                     include.mean=T)
  hoyhatma1[[i]]<-forecast(homa1[[i]],h=1)$mean
  print(hoyhatma1[[i]])
  print(i)
  
}
rollhoma1=as.numeric(unlist(hoyhatma1))
rollhoma1<-as.ts(rollhoma1)

plot(ho.te)
par(new=T)
lines(rollhoma1,col="red")



#ARIMA101-------------------------------------
hoarima1=list()
hoyhatarima1=list()
for(i in seq(1,1250,1)){
  hoarima1[[i]]<-arima(ho.rets[i:(3183+i)],
                        order=c(1,0,1),method="ML",
                        include.mean=T)
  hoyhatarima1[[i]]<-forecast(hoarima1[[i]],h=1)$mean
  print(hoyhatarima1[[i]])
  print(i)
  
}
rollhoarima1=as.numeric(unlist(hoyhatarima1))
rollhoarima1<-as.ts(rollhoarima1)

plot(ho.te)
par(new=T)
lines(rollhoarima1,col="yellow")

accuracy(ho.te,rollhoarima1)

#naive-------------------------

naivefho=vector()
naivefho<-rwf(ho.tr,h=1250)$mean

plot(naivefho)







#historical mean------------

mean=vector()
for(i in 1:1250){
  mean[[i]]<-mean(ho.tr)
}
mean.fho=as.ts(mean)

plot(ho.te)
par(new=T)
lines(mean.fho,col="pink")


#SMA--------------------------------
#------------------------------------

#SMA20-----------
hoSMA20<-vector()
for(i in 1:1250){
  hoSMA20[i]<-mean(ho.rets[(3164+i):(3183+i)])
}
hoSMA20<-ts(hoSMA20)
head(hoSMA20,20)

plot(ho.te)
par(new=T)
lines(hoSMA20,col="red")

accuracy(ho.te,hoSMA20)


#SMA60---------------
hoSMA60<-vector()
for(i in 1:1250){
  hoSMA60[i]<-mean(ho.rets[(3124+i):(3183+i)])
}
hoSMA60<-ts(hoSMA60)
head(hoSMA60,20)

plot(ho.te)
par(new=T)
lines(hoSMA60,col="red")

accuracy(ho.te,hoSMA60)


#180----------------
hoSMA180<-vector()
for(i in 1:1250){
  hoSMA180[i]<-mean(ho.rets[(3004+i):(3183+i)])
}
hoSMA180<-ts(hoSMA180)
head(hoSMA180,20)

plot(ho.te)
par(new=T)
lines(hoSMA180,col="red")


##-Accuracy-----------
accuracy(ho.te,rollhoar1)
accuracy(ho.te,rollhoma1)
accuracy(ho.te,rollhoarima1)
accuracy(ho.rets[3185:4435],naivefho)
accuracy(ho.te,mean.fho)
accuracy(ho.te,hoSMA20)
accuracy(ho.te,hoSMA60)
accuracy(ho.te,hoSMA180)
