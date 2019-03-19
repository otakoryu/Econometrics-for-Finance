oil<-read.delim("oilf.txt",header=T)
heatoil<-read.delim("heatoilf.txt",header=T)
ngas<-read.delim("ngasf.txt",header=T)
attach(heatoil)
attach(ngas)
attach(oil)

oil<-ts(oilf)
heatoil<-ts(heatoilf)
ngas<-ts(ngasf)


plot(oil)
plot(ngas)
plot(heatoil)

oil.rets<-100*diff(log(oil))
ngas.rets<-100*diff(log(ngas))
heatoil.rets<-100*diff(log(heatoil))
write.table(oil.rets, file="heatoil.rets.rets.txt")

library(quantmod)
plot(oil.rets)
plot(ngas.rets)
plot(heatoil.rets)
candleChart(oil.rets)

summary(oil.rets)
library(e1071)
sd(oil.rets)
skewness(oil.rets)
kurtosis(oil.rets)

summary(ngas.rets)
sd(ngas.rets)
skewness(ngas.rets)
kurtosis(ngas.rets)

summary(heatoil.rets)
sd(heatoil.rets)
skewness(heatoil.rets)
kurtosis(heatoil.rets)


#----------------------------------------------------------


plot(density(oil.rets))
hist(oil.rets,freq=FALSE, xlab="",ylab="",main="distribution of oil return",
     xlim=c(-15,15),ylim=c(0,0.3),
     col="pink",breaks=100,las=1)
curve(dnorm(x,mean=mean(oil.rets),sd=sd(oil.rets)),add=TRUE,col="darkblue",lwd=2)

plot(density(heatoil.rets))
hist(heatoil.rets,freq=FALSE, xlab="",ylab="",main="distribution of heatoil return",
     xlim=c(-15,15),ylim=c(0,0.3),
     col="pink",breaks=100,las=1)
curve(dnorm(x,mean=mean(heatoil.rets),sd=sd(heatoil.rets)),add=TRUE,col="darkblue",lwd=2)

plot(density(ngas.rets))
hist(ngas.rets,freq=FALSE, xlab="",ylab="",main="distribution of ngas return",
     xlim=c(-15,15),ylim=c(0,0.3),
     col="pink",breaks=100,las=1)
curve(dnorm(x,mean=mean(ngas.rets),sd=sd(ngas.rets)),add=TRUE,col="darkblue",lwd=2)


qqnorm(oil.rets)
qqline(oil.rets,col="red",lwd=3)

qqnorm(heatoil.rets)
qqline(heatoil.rets,col="red",lwd=3)

qqnorm(ngas.rets)
qqline(ngas.rets,col="red",lwd=3)

library(tseries)  
jarque.bera.test(oil.rets)
jarque.bera.test(heatoil.rets)    
jarque.bera.test(ngas.rets)
#Null=jointly skewness and kurtosis of zero(distribution is normal)

library(fBasics)
normalTest(oil,method="jb",na.rm=TRUE)
normalTest(heatoil,method="jb",na.rm=TRUE)
normalTest(ngas,method="jb",na.rm=TRUE)
