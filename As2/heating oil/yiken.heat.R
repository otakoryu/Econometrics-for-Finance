getwd()
ho<-read.table("heatoilf.txt",header=T)
attach(ho)
names(ho)
ho=as.ts(ho$heatoilf)


ho.rets<-100*diff(log(ho))

ho.tr<-as.ts(ho.rets[1:3184])
ho.te<-as.ts(ho.rets[3185:4434])

ho.v<-ho.rets^2
ho.v.tr=as.ts(ho.v[1:3184])
ho.v.te=as.ts(ho.v[3185:4434])

#------------------------------------
##Arch effect
#(i)

archho5<-lm(
  formula = dyn(ho.v ~ lag(ho.v,1) + lag(ho.v,2 )+lag(ho.v,3)+lag(ho.v,4)
                +lag(ho.v,5)), data = ho.v)
summary(archho5)
0.03756*length(ho.v)
qchisq(0.05,df=5,lower.tail=F)

archho10<-lm(formula = dyn(ho.v ~ lag(ho.v,1) + lag(ho.v,2 )+lag(ho.v,3)+lag(ho.v,4)
                            +lag(ho.v,5)+lag(ho.v,6) + lag(ho.v,7 )+lag(ho.v,8)+lag(ho.v,9)
                            +lag(ho.v,10)), data = ho.v)


summary(archho10)
0.04676*length(ho.v)
qchisq(0.05,df=10,lower.tail=F)



#(ii)
hov.pacf=pacf(ho.v,10)
hov.pacf=hov.pacf[["acf"]]
hov.pacf

#(iii)
##Ljung-Box test
library(portes)    ######log return=continiously compounded return  
qchisq(0.05,10)
Box.test(ho.v,lag=10,type="L")

##New Impact Curve(iv)

#Standard Garch
nho.ag11.fit=list()

spec.ho.ag11 <- ugarchspec(variance.model = list(model = "sGARCH", 
                                                  garchOrder = c(1,1)),
                            mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                                              include.mean = TRUE),
                            distribution.model = "norm") # Model specification



nho.ag11.fit<- ugarchfit(ho.rets, spec = spec.ho.ag11,
                          solver = "hybrid") # Model estimation 

#EGarch
nho.eag11.fit=list()

spec.ho.eag11<-ugarchspec(variance.model = list(model = "eGARCH", 
                                                 garchOrder = c(1,1)),
                           mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                                             include.mean = TRUE),
                           distribution.model = "norm") # Model specification

nho.eag11.fit<- ugarchfit(ho.rets, spec = spec.ho.eag11,
                           solver = "hybrid") # Model estimation 



ni1=newsimpact(z=NULL,nho.ag11.fit)
plot(ni1$zx,ni1$zy,ylab=ni1$yexpr,xlab=ni1$xexpr,type="l",main="News Impact Curve for Heating Oil")
ni2=newsimpact(z=NULL,nho.eag11.fit)
lines(ni2$zx,ni2$zy,lty=2,col=2)
legend("top",leg=c("AR(1)-GARCH(1,1)","AR(1)-EGARCH(1,1)"),lty=c(1,2),col=1:2,bg="white")




##(b)-------------------------------------------
##AR(1)-GARCH(1,1)
ho.ag11.fit=list()
ho.ag11f=list()
spec.ho.ag11 <- ugarchspec(variance.model = list(model = "sGARCH", 
                                                  garchOrder = c(1,1)),
                            mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                                              include.mean = TRUE),
                            distribution.model = "norm") # Model specification


for(i in 1:1250){
  ho.ag11.fit[[i]] <- ugarchfit(ho.rets[i:(3183+i)], 
                                 spec = spec.ho.ag11,
                                 solver = "hybrid") # Model estimation 
  
  ho.ag11f[[i]] = ugarchforecast(ho.ag11.fit[[i]],n.ahead=1)
  print(i)
}



save(ho.ag11.fit, file="ho.ag11.fit")
save(ho.ag11f, file="ho.ag11f")
load("ho.ag11f")
###############################################################################################
###############################################################################################
###############################################################################################

for(i in seq(1,1250,1)){
  
  ho.fit.s11<-as.numeric(unlist(ho.ag11f[[i]]@forecast[["seriesFor"]]))
  write.table(ho.fit.s11, file="hosgarchfit.txt",
              append=TRUE,
              col.names = FALSE,
              sep = ',',row.names=FALSE)
  
  ho.vol.s11<-as.numeric(unlist(ho.ag11f[[i]]@forecast[["sigmaFor"]]))
  write.table(ho.vol.s11, file="hosgarchvol.txt",
              append=TRUE,
              col.names = FALSE,
              sep = ',',row.names=FALSE)
  
  print(i)
}

###############################################################################################
#################################################################################################
##################################################################################################

hosgarchfit=as.ts(read.table("hosgarchfit.txt",header=FALSE))
hosgarchvol=as.ts(read.table("hosgarchvol.txt",header=FALSE));hosgarchvol=hosgarchvol^2



##-------------------------------------------------------
###GJR(1,1)
ho.gjrg11.fit=list()
ho.gjrg11f=list()
spec.ho.gjrg11 <- ugarchspec(variance.model = list(model = "gjrGARCH", 
                                                    garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                                                include.mean = TRUE),
                              distribution.model = "norm") # Model specification


for(i in 1:1250){
  ho.gjrg11.fit[[i]] <- ugarchfit(ho.rets[i:(3183+i)], 
                                   spec = spec.ho.gjrg11,
                                   solver = "hybrid") # Model estimation 
  
  ho.gjrg11f[[i]] = ugarchforecast(ho.gjrg11.fit[[i]],n.ahead=1)
  
  print(i)
  
}
save(ho.gjrg11.fit, file="ho.gjrg11.fit")
save(ho.gjrg11f, file="ho.gjrg11f")
load("ho.gjrg11f")
#######################################################################################################
######################################################################################################
########################################################################################################
for(i in seq(1,1250,1)){
  
  ho.fit.gjrg11<-as.numeric(unlist(ho.gjrg11f[[i]]@forecast[["seriesFor"]]))
  
  ho.vol.gjrg11<-as.numeric(unlist(ho.gjrg11f[[i]]@forecast[["sigmaFor"]]))
  
  write.table(ho.fit.gjrg11, file="hogjrgarchfit.txt",
              append=TRUE,
              col.names = FALSE,
              sep = ',',row.names=FALSE)
  
  write.table(ho.vol.gjrg11, file="hogjrgarchvol.txt",
              append=TRUE,
              col.names = FALSE,
              sep = ',',row.names=FALSE)
  print(i)
  
}
#########################################################################################################
#########################################################################################################
#########################################################################################################

hogjrgarchfit=as.ts(read.table("hogjrgarchfit.txt",header=FALSE))
hogjrgarchvol=as.ts(read.table("hogjrgarchvol.txt",header=FALSE));hogjrgarchvol=hogjrgarchvol^2


length(hogjrgarchvol)

accuracy(hogjrgarchvol,oil.v.te)
9.916923^2
##----------------------------------------
####AR(5)
hoar5=list()
hoar5f=vector()
for(i in 1:1250){
  hoar5[[i]]=arima(ho.v[i:(3183+i)],order=c(5,0,0),method="ML",include.mean=TRUE)
  hoar5f[[i]]=forecast(hoar5[[i]],h=1)$mean
  print(i)
}

hoar5f=as.ts(hoar5f)
write.table(hoar5f, file="hoar5f.txt",col.names = FALSE,sep = ',',row.names=FALSE)

hoar5f=read.table("hoar5f.txt",header=FALSE);hoar5f=as.ts(hoar5f)
length(hoar5f)



####OLS(lagged1)
ho.ols=list()
ho.olsf=vector()
for(i in 1:1250){
  ho.ols[[i]]=ar.ols(ho.v[i:(3183+i)], aic = TRUE, order.max = 1, na.action = na.fail,
                     demean = FALSE, intercept = TRUE)
  ho.olsf[[i]]<-forecast(ho.ols[[i]],h=1)$mean
  
  print(i)
  
}
ho.olsf=as.ts(ho.olsf)


####Naive
naivehof=vector()
for(i in 1:1250){
  naivehof[[i]]<-rwf(ho.v[i:(3183+i)],h=1)$mean
  print(i)
}
naiveholf=ts(naivehof)
ho.v[3184]
naivehof[1]

length(naivehof)



####Historical Mean
meanhof=vector()
for(i in 1:1250){
  meanhof[[i]]<-mean(ho.v[i:(3183+i)])
}
meanhof=as.ts(meanhof)
head(meanhof)

tail(meanhof)

plot(meanhof)
#########SMA
####SMA20

SMA20hof<-vector()
for(i in 1:1250){
  SMA20hof[i]<-mean(ho.v[(3164+i):(3183+i)])
}
SMA20hof<-ts(SMA20hof)
head(SMA20hof,20)



####SMA60
SMA60hof<-vector()
for(i in 1:1250){
  SMA60hof[i]<-mean(ho.v[(3124+i):(3183+i)])
}
SMA60hof<-ts(SMA60hof)




####180
SMA180hof<-vector()
for(i in 1:1250){
  SMA180hof[i]<-mean(ho.v[(3004+i):(3183+i)])
}
SMA180hof<-ts(SMA180hof)

9.916923^2
####Accuracy-----------------------------------------------------------
####-----------------------------------------------------------------------

accuracy(hosgarchvol,ho.v.te)
accuracy(hogjrgarchvol,ho.v.te)  
accuracy(hoar5f,ho.v.te)
accuracy(ho.olsf,ho.v.te)
accuracy(naivehof,ho.v.te)
accuracy(meanhof,ho.v.te)
accuracy(SMA20hof,ho.v.te)
accuracy(SMA60hof,ho.v.te)
accuracy(SMA180hof,ho.v.te)






