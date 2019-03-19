getwd()
gas<-read.table("ngasf.txt",header=T)
attach(gas)
names(gas)
gas=as.ts(gas$ngasf)


gas.rets<-100*diff(log(gas))

gas.tr<-as.ts(gas.rets[1:3184])
gas.te<-as.ts(gas.rets[3185:4434])

gas.v<-gas.rets^2
gas.v.tr=as.ts(gas.v[1:3184])
gas.v.te=as.ts(gas.v[3185:4434])

#------------------------------------
##Arch effect
#(i)

archgas5<-lm(
  formula = dyn(gas.v ~ lag(gas.v,1) + lag(gas.v,2 )+lag(gas.v,3)+lag(gas.v,4)
                +lag(gas.v,5)), data = gas.v)
summary(archgas5)
0.03306*length(gas.v)
qchisq(0.05,df=5,lower.tail=F)


archgas10<-lm(formula = dyn(gas.v ~ lag(gas.v,1) + lag(gas.v,2 )+lag(gas.v,3)+lag(gas.v,4)
                           +lag(gas.v,5)+lag(gas.v,6) + lag(gas.v,7 )+lag(gas.v,8)+lag(gas.v,9)
                           +lag(gas.v,10)), data = gas.v)

summary(archgas10)
0.03823*length(gas.v)
qchisq(0.05,df=10,lower.tail=F)



#(ii)
gasv.pacf=pacf(gas.v,10)
gasv.pacf=gasv.pacf[["acf"]]
gasv.pacf

#(iii)
##Ljung-Box test
library(portes)    ######log return=continiously compounded return  
qchisq(0.05,10)
Box.test(gas.v,lag=10,type="L")

##New Impact Curve(iv)

#Standard Garch
ngas.ag11.fit=list()

spec.gas.ag11 <- ugarchspec(variance.model = list(model = "sGARCH", 
                                                 garchOrder = c(1,1)),
                           mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                                             include.mean = TRUE),
                           distribution.model = "norm") # Model specification



ngas.ag11.fit<- ugarchfit(gas.rets, spec = spec.gas.ag11,
                         solver = "hybrid") # Model estimation 

#EGarch
ngas.eag11.fit=list()

spec.gas.eag11<-ugarchspec(variance.model = list(model = "eGARCH", 
                                                garchOrder = c(1,1)),
                          mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                                            include.mean = TRUE),
                          distribution.model = "norm") # Model specification

ngas.eag11.fit<- ugarchfit(gas.rets, spec = spec.gas.eag11,
                          solver = "hybrid") # Model estimation 



ni1=newsimpact(z=NULL,ngas.ag11.fit)
plot(ni1$zx,ni1$zy,ylab=ni1$yexpr,xlab=ni1$xexpr,type="l",main="News Impact Curve for Natural Gas")
ni2=newsimpact(z=NULL,ngas.eag11.fit)
lines(ni2$zx,ni2$zy,lty=2,col=2)
legend("top",leg=c("AR(1)-GARCH(1,1)","AR(1)-EGARCH(1,1)"),lty=c(1,2),col=1:2,bg="white")




##(b)-------------------------------------------
##AR(1)-GARCH(1,1)
gas.ag11.fit=list()
gas.ag11f=list()
spec.gas.ag11 <- ugarchspec(variance.model = list(model = "sGARCH", 
                                                 garchOrder = c(1,1)),
                           mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                                             include.mean = TRUE),
                           distribution.model = "norm") # Model specification


for(i in 1:1250){
  gas.ag11.fit[[i]] <- ugarchfit(gas.rets[i:(3183+i)], 
                                spec = spec.gas.ag11,
                                solver = "hybrid") # Model estimation 
  
  gas.ag11f[[i]] = ugarchforecast(gas.ag11.fit[[i]],n.ahead=1)
  print(i)
}



save(gas.ag11.fit, file="gas.ag11.fit")
save(gas.ag11f, file="gas.ag11f")
load("gas.ag11f")
###############################################################################################
###############################################################################################
###############################################################################################

for(i in seq(1,1250,1)){
  
  gas.fit.s11<-as.numeric(unlist(gas.ag11f[[i]]@forecast[["seriesFor"]]))
  write.table(gas.fit.s11, file="gassgarchfit.txt",
              append=TRUE,
              col.names = FALSE,
              sep = ',',row.names=FALSE)
  
  gas.vol.s11<-as.numeric(unlist(gas.ag11f[[i]]@forecast[["sigmaFor"]]))
  write.table(gas.vol.s11, file="gassgarchvol.txt",
              append=TRUE,
              col.names = FALSE,
              sep = ',',row.names=FALSE)
  
  print(i)
}

###############################################################################################
#################################################################################################
##################################################################################################

gassgarchfit=as.ts(read.table("gassgarchfit.txt",header=FALSE))
gassgarchvol=as.ts(read.table("gassgarchvol.txt",header=FALSE));gassgarchvol=gassgarchvol^2



##-------------------------------------------------------
###GJR(1,1)
gas.gjrg11.fit=list()
gas.gjrg11f=list()
spec.gas.gjrg11 <- ugarchspec(variance.model = list(model = "gjrGARCH", 
                                                   garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                                               include.mean = TRUE),
                             distribution.model = "norm") # Model specification


for(i in 1:1250){
  gas.gjrg11.fit[[i]] <- ugarchfit(gas.rets[i:(3183+i)], 
                                  spec = spec.gas.gjrg11,
                                  solver = "hybrid") # Model estimation 
  
  gas.gjrg11f[[i]] = ugarchforecast(gas.gjrg11.fit[[i]],n.ahead=1)
  
  print(i)
  
}
save(gas.gjrg11.fit, file="gas.gjrg11.fit")
save(gas.gjrg11f, file="gas.gjrg11f")
load("gas.gjrg11f")
#######################################################################################################
######################################################################################################
########################################################################################################
for(i in seq(1,1250,1)){
  
  gas.fit.gjrg11<-as.numeric(unlist(gas.gjrg11f[[i]]@forecast[["seriesFor"]]))
  
  gas.vol.gjrg11<-as.numeric(unlist(gas.gjrg11f[[i]]@forecast[["sigmaFor"]]))
  
  write.table(gas.fit.gjrg11, file="gasgjrgarchfit.txt",
              append=TRUE,
              col.names = FALSE,
              sep = ',',row.names=FALSE)
  
  write.table(gas.vol.gjrg11, file="gasgjrgarchvol.txt",
              append=TRUE,
              col.names = FALSE,
              sep = ',',row.names=FALSE)
  print(i)
  
}
#########################################################################################################
#########################################################################################################
#########################################################################################################

gasgjrgarchfit=as.ts(read.table("gasgjrgarchfit.txt",header=FALSE))
gasgjrgarchvol=as.ts(read.table("gasgjrgarchvol.txt",header=FALSE));gasgjrgarchvol=gasgjrgarchvol^2


length(gasgjrgarchvol)

accuracy(gasgjrgarchvol,gas.v.te)
9.916923^2
##----------------------------------------
####AR(5)
gasar5=list()
gasar5f=vector()
for(i in 1:1250){
  gasar5[[i]]=arima(gas.v[i:(3183+i)],order=c(5,0,0),method="ML",include.mean=TRUE)
  gasar5f[[i]]=forecast(gasar5[[i]],h=1)$mean
  print(i)
}

gasar5f=as.ts(gasar5f)
write.table(gasar5f, file="gasar5f.txt",col.names = FALSE,sep = ',',row.names=FALSE)

gasar5f=read.table("gasar5f.txt",header=FALSE);gasar5f=as.ts(gasar5f)
length(gasar5f)



####OLS(lagged1)
gas.ols=list()
gas.olsf=vector()
for(i in 1:1250){
  gas.ols[[i]]=ar.ols(gas.v[i:(3183+i)], aic = TRUE, order.max = 1, na.action = na.fail,
                     demean = FALSE, intercept = TRUE)
  gas.olsf[[i]]<-forecast(gas.ols[[i]],h=1)$mean
  
  print(i)
  
}
gas.olsf=as.ts(gas.olsf)


####Naive
naivegasf=vector()
for(i in 1:1250){
  naivegasf[[i]]<-rwf(gas.v[i:(3183+i)],h=1)$mean
  print(i)
}
naivegasf=ts(naivegasf)
gas.v[3184]
naivegasf[1]

length(naivegasf)



####Historical Mean
meangasf=vector()
for(i in 1:1250){
  meangasf[[i]]<-mean(gas.v[i:(3183+i)])
}
meangasf=as.ts(meangasf)
head(meangasf)

tail(meangasf)

plot(meangasf)
#########SMA
####SMA20

SMA20gasf<-vector()
for(i in 1:1250){
  SMA20gasf[i]<-mean(gas.v[(3164+i):(3183+i)])
}
SMA20gasf<-ts(SMA20gasf)
head(SMA20gasf,20)



####SMA60
SMA60gasf<-vector()
for(i in 1:1250){
  SMA60gasf[i]<-mean(gas.v[(3124+i):(3183+i)])
}
SMA60gasf<-ts(SMA60gasf)




####180
SMA180gasf<-vector()
for(i in 1:1250){
  SMA180gasf[i]<-mean(gas.v[(3004+i):(3183+i)])
}
SMA180gasf<-ts(SMA180gasf)

9.916923^2
####Accuracy-----------------------------------------------------------
####-----------------------------------------------------------------------

accuracy(gassgarchvol,gas.v.te)
accuracy(gasgjrgarchvol,gas.v.te)  
accuracy(gasar5f,gas.v.te)
accuracy(gas.olsf,gas.v.te)
accuracy(naivegasf,gas.v.te)
accuracy(meangasf,gas.v.te)
accuracy(SMA20gasf,gas.v.te)
accuracy(SMA60gasf,gas.v.te)
accuracy(SMA180gasf,gas.v.te)


14.209^2
14.19398^2
14.42307^2
14.96462^2
19.55034^2
15.08394^2
14.34829^2
14.63775^2
14.85421^2
