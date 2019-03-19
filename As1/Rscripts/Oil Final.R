oil<-read.delim("oilf.txt",header=T)
attach(oil)
names(oil)
oil=as.ts(oil$oilf)
plot(oil)

library(tseries)
adf.test(oil.rets)

oil.rets<-100*diff(log(oil))

library(quantmod)
plot(oil.rets)

oil.tr<-as.ts(oil.rets[1:3184])
oil.te<-as.ts(oil.rets[3185:4434])

f1<-arima(oil.tr,order=c(1,0,0),method="ML",)

plot(oil.tr)
plot(oil.te)

library(tseries)

library(forecast)

##---Summary----
summary(oil.rets)
library(e1071)
var(oil.rets)
sd(oil.rets)
skewness(oil.rets)
kurtosis(oil.rets)


##--density plot----
plot(density(oil.rets))


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



##Rolling window with Loop---------------------------------------------------------------
#AR1
oilar1=list()
oilyhatar1=list()
for(i in seq(1,1250,1)){
  oilar1[[i]]<-arima(oil.rets[i:(3183+i)],
                  order=c(1,0,0),method="ML",
                  include.mean=T)
  oilyhatar1[[i]]<-forecast(oilar1[[i]],h=1)
  print(oilyhatar1[[i]])
  print(i)
  
}
rolloilar1=as.numeric(unlist(oilyhatar1))
rolloilar1<-as.ts(rolloilar1)

oka1=vector()
for(i in seq(1,1250,1)){
  oka1=as.numeric(unlist(oilyhatar1[[i]][["mean"]]))
  print(i)
  print(oka1)
}


head(rolloilar1,2)
accuracy(oil.te,rolloilar1)

plot(oil.te)
par(new=T)
lines(rolloilar1,col="red")




  
#MA1-----------------------------------
oilma1=list()
oilyhatma1=list()
for(i in seq(1,1250,1)){
  oilma1[[i]]<-arima(oil.rets[i:(3183+i)],
                     order=c(0,0,1),method="ML",
                     include.mean=T)
  oilyhatma1[[i]]<-forecast(oilma1[[i]],h=1)$mean
  print(oilyhatma1[[i]])
  print(i)
  
}
rolloilma1=as.numeric(unlist(oilyhatma1))
rolloilma1<-as.ts(rolloilma1)


plot(oil.te)
par(new=T)
lines(rolloilma1,col="red")

accuracy(oil.te,rolloilma1)




#ARIMA101-------------------------------------
oilarima1=list()
oilyhatarima1=list()
for(i in seq(1,1250,1)){
  oilarima1[[i]]<-arima(oil.rets[i:(3183+i)],
                     order=c(1,0,1),method="ML",
                     include.mean=T)
  oilyhatarima1[[i]]<-forecast(oilarima1[[i]],h=1)$mean
  print(oilyhatarima1[[i]])
  print(i)
  
}
rolloilarima1=as.numeric(unlist(oilyhatarima1))
rolloilarima1<-as.ts(rolloilarima1)

plot(oil.te)
par(new=T)
lines(rolloilarima1,col="green")

accuracy(oil.te,rolloilarima1)

#naive-------------------------
naivefoil=vector()
naivefoil<-rwf(oil.tr,h=1250)$mean
plot(naivefoil)
length(naivefoil)
head(naivefoil)

accuracy(oil.te,naivefoil)

#historical mean------------
mean<-mean(oil.tr)
mean=vector()
for(i in 1:1250){
  mean[[i]]<-mean(oil.tr)
}
mean.foil=as.ts(mean)
plot(oil.te)
par(new=T)
lines(mean.foil,col="pink")

accuracy(oil.te,mean.foil)





#SMA--------------------------------
#------------------------------------

#SMA20-----------
oilSMA20<-vector()
for(i in 1:1250){
  oilSMA20[i]<-mean(oil.rets[(3164+i):(3183+i)])
  }
oilSMA20<-ts(oilSMA20)
head(oilSMA20,20)

plot(oil.te)
par(new=T)
lines(oilSMA20,col="red")

accuracy(oil.te,oilSMA20)


#SMA60---------------
oilSMA60<-vector()
for(i in 1:1250){
  oilSMA60[i]<-mean(oil.rets[(3124+i):(3183+i)])
}
oilSMA60<-ts(oilSMA60)
head(oilSMA60,20)

plot(oil.te)
par(new=T)
lines(oilSMA60,col="red")

accuracy(oil.te,oilSMA60)


#180----------------
oilSMA180<-vector()
for(i in 1:1250){
  oilSMA180[i]<-mean(oil.rets[(3004+i):(3183+i)])
}
oilSMA180<-ts(oilSMA180)
head(oilSMA180,20)


plot(oil.te)
par(new=T)
lines(oilSMA180,col="red")

accuracy(oil.te,oilSMA180)

##-Accuracy-----------
accuracy(oil.te,rolloilar1)
accuracy(oil.te,rolloilma1)
accuracy(oil.te,rolloilarima1)
accuracy(oil.rets[3185:4434],naivefoil)
accuracy(oil.te,mean.foil)
accuracy(oil.te,oilSMA20)
accuracy(oil.te,oilSMA60)
accuracy(oil.te,oilSMA180)

