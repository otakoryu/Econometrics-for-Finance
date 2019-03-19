require(quantmod);require(xts);require(rugarch);require(aTSA);
require(forecast);library(tseries);library(sos);require(fBasics);
require(stats);require(dyn)
#---------------------------------------
oil<-read.table("oilf.txt",header=T)
attach(oil)
names(oil)
oil=as.ts(oil$oilf)
plot(oil)

oil.rets<-100*diff(log(oil))

oil.tr<-as.ts(oil.rets[1:3184])
oil.te<-as.ts(oil.rets[3185:4434])

oil.v<-oil.rets^2
oil.v.tr=as.ts(oil.v[1:3184])
oil.v.te=as.ts(oil.v[3185:4434])
plot(oil.v.te)
write.csv(oil.v, file="oil.v.csv")

sum(is.na(oil.v))
#------------------------------------
##Arch effect
#(i)

archoil5<-lm(
  formula = dyn(oil.v ~ lag(oil.v,1) + lag(oil.v,2 )+lag(oil.v,3)+lag(oil.v,4)
                +lag(oil.v,5)), data = oil.v)
summary(archoil5)

plot(oil.v)
abline(archoil5)

0.1117*length(oil.v)
qchisq(0.05,df=5)

archoil10<-lm(formula = dyn(oil.v ~ lag(oil.v,1) + lag(oil.v,2 )+lag(oil.v,3)+lag(oil.v,4)
                            +lag(oil.v,5)+lag(oil.v,6) + lag(oil.v,7 )+lag(oil.v,8)+lag(oil.v,9)
                            +lag(oil.v,10)), data = oil.v)

plot(oil.v)
abline(archoil10)

summary(archoil10)
0.1354*length(oil.v)
qchisq(0.05,df=10)



#(ii)
oilv.pacf=pacf(oil.v,10)
oilv.pacf=oilv.pacf[["acf"]]
oilv.pacf

#(iii)
##Ljung-Box test
library(portes)    ######log return=continiously compounded return  
LB.test <- portest(oil.v, lags=c(1,2,3,4,5,6,7,8,9,10), test=c("LjungBox"), SquaredQ=FALSE)
LB.test
qchisq(0.05,10)

Box.test(oil.v,lag=10,type="L")

##or?????
oil.ljung=vector()
pacf(oil.v,lag=10)
oil.ljung=pacf(oil.v,lag=10)

oil.ljung.v=vector()
oil.ljung.v=oil.ljung[["acf"]]^2
sum(oil.ljung.v)*length(oil.v)


sum(is.na(oil.v))


##New Impact Curve(iv)

#Standard Garch
noil.ag11.fit=list()

spec.oil.ag11 <- ugarchspec(variance.model = list(model = "sGARCH", 
                                                  garchOrder = c(1,1)),
                            mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                                              include.mean = TRUE),
                            distribution.model = "norm") # Model specification



noil.ag11.fit<- ugarchfit(oil.rets, spec = spec.oil.ag11,
                          solver = "hybrid") # Model estimation 

#EGarch
noil.eag11.fit=list()

spec.oil.eag11<-ugarchspec(variance.model = list(model = "eGARCH", 
                                                 garchOrder = c(1,1)),
                           mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                                             include.mean = TRUE),
                           distribution.model = "norm") # Model specification

noil.eag11.fit<- ugarchfit(oil.rets, spec = spec.oil.eag11,
                           solver = "hybrid") # Model estimation 

plot(noil.ag11.fit,which=12)
plot(noil.eag11.fit,which=12)


ni1=newsimpact(z=NULL,noil.ag11.fit)
plot(ni1$zx,ni1$zy,ylab=ni1$yexpr,xlab=ni1$xexpr,type="l",main="News Impact Curve for Crude Oil")
ni2=newsimpact(z=NULL,noil.eag11.fit)
lines(ni2$zx,ni2$zy,lty=2,col=2)
legend("top",leg=c("AR(1)-GARCH(1,1)","AR(1)-EGARCH(1,1)"),lty=c(1,2),col=1:2,bg="white")




##(b)-------------------------------------------
##AR(1)-GARCH(1,1)
oil.ag11.fit=list()
oil.ag11f=list()
spec.oil.ag11 <- ugarchspec(variance.model = list(model = "sGARCH", 
                                                  garchOrder = c(1,1)),
                            mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                                              include.mean = TRUE),
                            distribution.model = "norm") # Model specification


for(i in 1:1250){
  oil.ag11.fit[[i]] <- ugarchfit(oil.rets[i:(3183+i)], 
                                 spec = spec.oil.ag11,
                                 solver = "hybrid") # Model estimation 
  
  oil.ag11f[[i]] = ugarchforecast(oil.ag11.fit[[i]],n.ahead=1)
  print(i)
}


save(oil.ag11.fit, file="oil.ag11.fit")
save(oil.ag11f, file="oil.ag11f")
load("oil.ag11f")

###############################################################################################
###############################################################################################
##############################################################################################
oil.fit.s11=vector("numeric", length = 1250)
oil.vol.s11=vector("numeric", length = 1250)
for(i in seq(1,1250,1)){
  
  oil.fit.s11<-as.numeric(unlist(oil.ag11f[[i]]@forecast[["seriesFor"]]))
  write.table(oil.fit.s11, file="oilsgarchfit.txt",
              append=TRUE,
              col.names = FALSE,
              sep = ',',row.names=FALSE)
  
  oil.vol.s11<-as.numeric(unlist(oil.ag11f[[i]]@forecast[["sigmaFor"]]))
  write.table(oil.vol.s11, file="oilsgarchvol.txt",
              append=TRUE,
              col.names = FALSE,
              sep = ',',row.names=FALSE)
  
  print(i)
}
###############################################################################################
#################################################################################################
##################################################################################################

oilsgarchfit=as.ts(read.table("oilsgarchfit.txt",header=FALSE))
oilsgarchvol=as.ts(read.table("oilsgarchvol.txt",header=FALSE));oilsgarchvol=oilsgarchvol^2



plot(oilsgarchvol)

length(oilsgarchvol)
head(oilsgarchfit,5)
head(oilsgarchvol,5)


accuracy(oilsgarchvol,oil.v.te)

plot(oilsgarchvol)
head(oilsgarchvol,20)

##-------------------------------------------------------
###GJR(1,1)
oil.gjrg11.fit=list()
oil.gjrg11f=list()
spec.oil.gjrg11 <- ugarchspec(variance.model = list(model = "gjrGARCH", 
                                                    garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                                                include.mean = TRUE),
                              distribution.model = "norm") # Model specification


for(i in 1:1250){
  oil.gjrg11.fit[[i]] <- ugarchfit(oil.rets[i:(3183+i)], 
                                   spec = spec.oil.gjrg11,
                                   solver = "hybrid") # Model estimation 
  
  oil.gjrg11f[[i]] = ugarchforecast(oil.gjrg11.fit[[i]],n.ahead=1)
  
  print(i)
  
}
save(oil.gjrg11.fit, file="oil.gjrg11.fit")
save(oil.gjrg11f, file="oil.gjrg11f")
load("oil.gjrg11f")
#######################################################################################################
######################################################################################################
########################################################################################################
for(i in seq(1,1250,1)){
  
  oil.fit.gjrg11<-as.numeric(unlist(oil.gjrg11f[[i]]@forecast[["seriesFor"]]))
  
  oil.vol.gjrg11<-as.numeric(unlist(oil.gjrg11f[[i]]@forecast[["sigmaFor"]]))
  
  write.table(oil.fit.gjrg11, file="oilgjrgarchfit.txt",
              append=TRUE,
              col.names = FALSE,
              sep = ',',row.names=FALSE)
  
  write.table(oil.vol.gjrg11, file="oilgjrgarchvol.txt",
              append=TRUE,
              col.names = FALSE,
              sep = ',',row.names=FALSE)
  print(i)

}
#########################################################################################################
#########################################################################################################
#########################################################################################################

oilgjrgarchfit=as.ts(read.table("oilgjrgarchfit.txt",header=FALSE))
oilgjrgarchvol=as.ts(read.table("oilgjrgarchvol.txt",header=FALSE));oilgjrgarchvol=oilgjrgarchvol^2

accuracy(oilgjrgarchvol,oil.v.te)
9.916923^2
##----------------------------------------
####AR(5)
oilar5=list()
oilar5f=vector()
for(i in 1:1250){
  oilar5[[i]]=ar.ols(oil.v[i:(3183+i)], aic = TRUE, order.max = 5, na.action = na.fail,
                     demean = FALSE, intercept = TRUE)
  oilar5f[[i]]<-forecast(oilar5[[i]],h=1)$mean
  print(i)
}

summary(oilar5[[1]])
##############################
oilar5=list()
oilar5f=vector()
for(i in 1:1250){
  oilar5[[i]]=arima(oil.v[i:(3183+i)],order=c(5,0,0),method="ML",include.mean=TRUE)
  oilar5f[[i]]=forecast(oilar5[[i]],h=1)$mean
}


oilar5f=as.ts(oilar5f)
write.table(oilar5f, file="oilar5f.txt",col.names = FALSE,sep = ',',row.names=FALSE)

oilar5f=read.table("oilar5f.txt",header=FALSE);oilar5f=as.ts(oilar5f)
length(oilar5f)
accuracy(oilar5f,oil.v)


####OLS(lagged1)
oilols=list()
oilolsf=vector()
for(i in 1:1250){
  oilols[[i]]=ar.ols(oil.v[i:(3183+i)], aic = TRUE, order.max = 1, na.action = na.fail,
                     demean = FALSE, intercept = TRUE)
  oilolsf[[i]]<-forecast(oilols[[i]],h=1)$mean
  
  print(i)
  
}
oilolsf=as.ts(oilolsf)
oilolsf[[472]]
plot(oilolsf)
accuracy(oilolsf,oil.v.te)

####Naive
naiveoilf=vector()
for(i in 1:1250){
  naiveoilf[[i]]<-rwf(oil.v[i:(3183+i)],h=1)$mean
  print(i)
}
naiveoilf=ts(naiveoilf)
oil.v[3184]
naiveoilf[1]

oil.v[4433]
naiveoilf[1250]


length(naiveoilf)



####Historical Mean
meanoilf=vector()
for(i in 1:1250){
  meanoilf[[i]]<-mean(oil.v[i:(3183+i)])
}
meanoilf=as.ts(meanoilf)
head(meanoilf)

tail(meanoilf)

plot(meanoilf)
#########SMA
####SMA20

SMA20oilf<-vector()
for(i in 1:1250){
  SMA20oilf[i]<-mean(oil.v[(3164+i):(3183+i)])
}
SMA20oilf<-ts(SMA20oilf)
head(SMA20oilf,20)



####SMA60
SMA60oilf<-vector()
for(i in 1:1250){
  SMA60oilf[i]<-mean(oil.v[(3124+i):(3183+i)])
}
SMA60oilf<-ts(SMA60oilf)




####180
SMA180oilf<-vector()
for(i in 1:1250){
  SMA180oilf[i]<-mean(oil.v[(3004+i):(3183+i)])
}
SMA180oilf<-ts(SMA180oilf)

9.916923^2
####Accuracy-----------------------------------------------------------
####-----------------------------------------------------------------------

accuracy(oilsgarchvol,oil.v.te)
accuracy(oilgjrgarchvol,oil.v.te)  ####4.90
accuracy(oilar5f,oil.v.te)
accuracy(oilolsf,oil.v.te)
accuracy(naiveoilf,oil.v.te)
accuracy(meanoilf,oil.v.te)
accuracy(SMA20oilf,oil.v.te)
accuracy(SMA60oilf,oil.v.te)
accuracy(SMA180oilf,oil.v.te)


10.04361^2
9.916923^2
10.39331^2
10.35374^2
12.80027^2
10.77749^2
10.14708^2
10.2991^2
10.42703^2
