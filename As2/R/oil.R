require(quantmod)
require(xts)
require(rugarch)
require(aTSA)
require(forecast)
library(tseries)
library(sos)
require(fBasics)
require(stats)
require(dyn)
#---------------------------------------
oil<-read.delim("oilf.txt",header=T)
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

#------------------------------------
##Arch effect

arimato<-auto.arima(oil.rets,max.p=4,max.q=3,d=0,stepwise=T,trace=T)
arima22<-arima(oil.rets,order=c(2,0,2),include.mean=TRUE,method="ML")
resid<-resid(arima22)
var<-resid^2

var5<-ar(var, aic = TRUE, order.max = 5)
var10<-ar(var, aic = TRUE, order.max = 5)

arch.test(object, output = TRUE)

resid=resid(arima1)

var<-resid^2
ar.oil<-ar(oil.rets,method="ols")
summary(ar.oil)

#---------------------------

linear.oil<-lm(oil.rets~time(oil.rets))
summary(linear.oil)


archoil5<-lm(formula = dyn(oil.v ~ lag(oil.v,1) + lag(oil.v,2 )+lag(oil.v,3)+lag(oil.v,4)
                          +lag(oil.v,5)), data = oil.v)
summary(archoil5)

0.1117*4434
qchisq(.95,df=5)

archoil10<-lm(formula = dyn(oil.v ~ lag(oil.v,1) + lag(oil.v,2 )+lag(oil.v,3)+lag(oil.v,4)
                           +lag(oil.v,5)+lag(oil.v,6) + lag(oil.v,7 )+lag(oil.v,8)+lag(oil.v,9)
                           +lag(oil.v,10)), data = oil.v)
summary(archoil10)
0.1354*4434
qchisq(.95,df=10)

pacf(oil.v,10)

library(portes)    ######log return=continiously compounded return  
LB.test <- portest(oil.v, lags=c(1,2,3,4,5,6,7,8,9,10), test=c("LjungBox"), SquaredQ=FALSE)
LB.test
qchisq(.95,10)

Box.test(oil.v,lag=10,type="L")


noil.ag11.fit=list()

spec.oil.ag11 <- ugarchspec(variance.model = list(model = "sGARCH", 
                                                  garchOrder = c(1,1)),
                            mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                                              include.mean = TRUE),
                            distribution.model = "norm") # Model specification


noil.ag11.fit<- ugarchfit(oil.rets, spec = spec.oil.ag11,
                        solver = "hybrid") # Model estimation 
  
  
noil.eag11.fit=list()

spec.oil.eag11<-ugarchspec(variance.model = list(model = "eGARCH", 
                                                 garchOrder = c(1,1)),
                           mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                                             include.mean = TRUE),
                           distribution.model = "norm") # Model specification

noil.eag11.fit<- ugarchfit(oil.rets, spec = spec.oil.eag11,
                          solver = "hybrid") # Model estimation 



plot(noil.eag11.fit, which=12)# "which=1" ranges from 1 to 12.  
par(new=TRUE)
plot(noil.ag11.fit,which=12,col="red")


y=oil.rets[3:4434]
x=oil.rets[2:4433]
x2=oil.rets[1:4432]
linear.oil<-lm(y~x+x2)
residl=vector()
residl=unlist(linear.oil[["residuals"]])
sql=residl^2
sqly=sql[3:4432]
sqlx=sql[2:4431]
sqlx1=sql[1:4430]
linear.sql=lm(sqly~sqlx+sqlx1)
summary(linear.sql)
0.06292*4434
qchisq(.95, df=2)        # 7 degrees of freedom



sql.l=lm(sql~lag(sql,1)+lag(sql,2)+lag(sql,3)+lag(sql,4)+lag(sql,5))
summary(sql.l)

print(sql)

summary(lm(oil.rets~lag(oil.rets,1)+lag(oil.rets,2)+lag(oil.rets,3)))


ar1=ar.ols(oil.rets,demean=FALSE,aic = TRUE, order.max = 10, na.action = na.fail)

summary(ar1)

resid=vector()
resid<-ar1[["resid"]]

arr=ar.ols(resid,aic = TRUE, order.max = 3, na.action = na.fail)



lag1=lag(oil.rets,k=-1)
lag2=lag(oil.rets,k=-2)
lag3=lag(oil.rets,k=-3)
head(lag1,6)
head(lag2,6)
##-------------------------------------------
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


print(oil.ag11f[[i]]@forecast[["sigmaFor"]])
print(oil.ag11f[[i]]@forecast[["seriesFor"]])



save(oil.ag11.fit, file="oil.ag11.fit")
load("oil.ag11f")
forc.ag11=as.numeric(unlist(oil.ag11f$sigmaFor))

df.eg1 <- as.data.frame(oil.ag11f)


do.call(rbind, lapply(names(oil.ag11f), function(x) data.frame(c(seriesFor=x, oil.ag11f[[x]]))))


require(MASS)


forc11=vector("numeric", length = 1250)
for(i in seq(1,1250,1)){
  forc11<-as.numeric(unlist(oil.ag11f[[i]]@forecast[["sigmaFor"]],recursive=TRUE,use.names = TRUE))
  print(forc11)
  
}

write.table(forc11, file="forc11")

forc11<-as.numeric(unlist(oil.ag11f[[i]]@forecast[["sigmaFor"]]))
forc11<-data.frame(matrix(unlist(oil.ag11f[[i]]@forecast[["sigmaFor"]]), nrow=1250))
forc11<-as.numeric(unlist(oil.ag11f[[i]]@forecast[["sigmaFor"]]))
sigmaoil=data.matrix(forc11)
plot(sigmaoil)

do.call(rbind, iris.list)


for(i in 1:1250){
  mu[[i]]=unlist(as.numeric(fitted(oil.ag11f[[i]]$forecast[["seriesFor"]])))
  sigma[[i]]=unlist(as.numeric(sigma(oil.ag11f[[i]]$forecast[["sigmaFor"]])))
}


oil.fit.s11=vector("numeric", length = 1250)
oil.vol.s11=vector("numeric", length = 1250)

for(i in seq(1,1250,1)){
  
  oil.fit.s11<-as.numeric(unlist(oil.ag11f[[i]]@forecast[["seriesFor"]]))
  oil.vol.s11<-as.numeric(unlist(oil.ag11f[[i]]@forecast[["sigmaFor"]]))
  
  write.table(oil.fit.s11, file="oilsgarchfit.txt",
              append=TRUE,
              col.names = FALSE,
              sep = ',',row.names=FALSE)
  
  write.table(oil.vol.s11, file="oilsgarchvol.txt",
              append=TRUE,
              col.names = FALSE,
              sep = ',',row.names=FALSE)
  print(oil.fit.s11)
  print(oil.vol.s11)
}

oilsgarchfit=as.ts(read.table("oilsgarchfit.txt",header=FALSE))
oilsgarchvol=as.ts(read.table("oilsgarchvol.txt",header=FALSE))



##--------------------------------------------------

forc11=vector("numeric",1250)
for(i in seq(1,1250,1)){
  
  forc11<-as.numeric(unlist(oil.ag11f[[i]]@forecast[["sigmaFor"]]))
  print(forc11)
}

forc11<-data.frame(matrix(unlist(oil.ag11f[[i]]@forecast[["sigmaFor"]]), nrow=1250))


forc11<-as.numeric(unlist(oil.ag11f@forecast[["sigmaFor"]]))
forc11<-data.frame(matrix(unlist(oil.ag11f[[i]]@forecast[["sigmaFor"]]), nrow=1250))





sapply(oil.ag11f, '[[', "sigmaFor")

print(vol1[1250])


oil.ag11f[[5]][c('sigmaFor', 'seriesFor')]
lapply(oilag11f, function (x) x[c('sigmaFor', 'seriesFor')])
oil.ag11f[[1]]@forecast[["sigmaFor"]]
walao<-lapply(oil.ag11f,
              function(x) x[c("sigmaFor","seriesFor")])



oil.ag11f[[2]]


series <- unlist(oil.ag11f) 
sig <- unlist(oil.ag11f$sigmaFor) 

subListExtract(oil.ag11f, sigmaFor, simplify = FALSE, keep.names = TRUE)

##-----------------------------
roll.eg <- ugarchroll(
  spec.oil.ag11, oil.rets,
 
  refit.every = 1, 
  
  n.ahead = 1,
  
  n.start = 3184, 
  refit.window = "moving", solver = "hybrid", keep.coef = TRUE)

show(roll.eg)
save(roll.eg, file="roll.eg")
load("roll.eg")

df.eg <- as.data.frame(roll.eg)

plot(df.eg[, 'Sigma', drop = FALSE]$Sigma)

forc1<-unlist(as.ts(roll.eg@forecast[["density"]][["Mu"]]))
head(forc1,6)

plot(forc1)

vol1<-unlist(roll.eg@forecast[["density"]][["Sigma"]])
plot(vol1)

tail(vol11)
tail(roll.eg)


##-------------------------------------------------------
###GJR(1,1)
oil.gag11.fit=list()
oil.gag11f=list()
spec.oil.gag11 <- ugarchspec(variance.model = list(model = "gjrGARCH", 
                                                   garchOrder = c(1,1)),
                             mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                                               include.mean = TRUE),
                             distribution.model = "norm") # Model specification


for(i in 1:1250){
  oil.gag11.fit[[i]] <- ugarchfit(oil.rets[i:(3183+i)], 
                                 spec = spec.oil.gag11,
                                 solver = "hybrid") # Model estimation 
  
  oil.gag11f[[i]] = ugarchforecast(oil.gag11.fit[[i]],n.ahead=1)
  
  print(i)
  
}
save(oil.gag11f, file="oil.gag11f")
load("oil.gag11f")


oil.fit.g11=vector("numeric", length = 1250)
oil.vol.g11=vector("numeric", length = 1250)

for(i in seq(1,1250,1)){
  
  oil.fit.g11<-as.numeric(unlist(oil.gag11f[[i]]@forecast[["seriesFor"]]))
  oil.vol.g11<-as.numeric(unlist(oil.gag11f[[i]]@forecast[["sigmaFor"]]))
  
  write.table(oil.fit.g11, file="oilgjigarchfit.txt",
              append=TRUE,
              col.names = FALSE,
              sep = ',',row.names=FALSE)
  
  write.table(oil.vol.g11, file="oilgjigarchvol.txt",
              append=TRUE,
              col.names = FALSE,
              sep = ',',row.names=FALSE)
  print(oil.fit.g11)
  print(oil.vol.g11)
}

oilgjigarchfit=as.ts(read.table("oilgjigarchfit.txt",header=FALSE))
oilgjigarchvol=as.ts(read.table("oilgjigarchvol.txt",header=FALSE))


##----------------------------------------
####AR(5)
oilar5=list()
oilar5f=vector()
for(i in 1:1250){
oilar5[[i]]=ar.ols(oil.v[i:(3183+i)], aic = TRUE, order.max = 5, na.action = na.fail,
               demean = FALSE, intercept = TRUE)
oilar5f[[i]]<-forecast(oilar5[[i]],h=1)$mean
print(oilar5f[[i]])
print(i)
}

summary(oilar5[[1]])





####OLS(lagged1)
oilols=list()
oilolsf=vector()
for(i in 1:1250){
  oilols[[i]]=ar.ols(oil.v[i:(3183+i)], aic = TRUE, order.max = 1, na.action = na.fail,
                     demean = FALSE, intercept = TRUE)
  oilolsf[[i]]<-forecast(oilols[[i]],h=1)$mean
  
  print(i)
  
}
oilols[[1]]
plot(oilolsf)




####Naive
naiveoilf=vector()
naiveoilf<-rwf(oil.v.tr,h=1250)$mean

oil.v[3184]
head(naiveoilf,2)




####Historical Mean
meanoilf=vector()
for(i in 1:1250){
  meanoilf[[i]]<-mean(oil.v.tr)
}
meanoilf=as.ts(meanoilf)





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
