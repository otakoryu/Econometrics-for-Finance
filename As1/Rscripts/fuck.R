dim(oil.rets)

library(forecast)


###1250elements-------------------
ar1=list()
yhat=list()
for(i in seq(1,1250,1)){
  ar1[[i]]<-arima(oil.rets[i:(3183+i)],
                  order=c(1,0,0),method="ML",
                  include.mean=T)
  yhat[[i]]<-forecast(ar1[[i]],h=1)$mean
  print(yhat[[i]])
  print(i)
  
}

roll=as.numeric(unlist(yhat))
rolloilar1<-as.ts(roll)

list2df(list.object, col1 = "X1", col2 = "X2")

??library(data.table)
lstData <- Map(as.data.frame, lstData)
dfrData <- rbindlist(lstData)

data <- matrix(yhat, nrow = 1250, ncol = 3)
lstData <- rep(list(yhat), 1250)


attach(yhat)
write.table(yhat[,2], file="roll1.txt")

forecast(refit, h=h)$mean
#--------------------------------------------------
lapply(modelset, `[`, c('likelihood', 'fixef'))
#-------------------------------------------------


roll<-ts(lapply(yhat,`[`,c("mean")))

as.numeric(roll)



roll=NULL
for(i in 1:1250){
  as.numeric(yhat[[i]][["mean"]])
}





[[1]][["mean"]]



for(i in 1:1250){
  roll<-as.ts(yhat[[i]]$mean)
  print(i)
}


print(yhat[[2]])


accuracy(oil.te,yhat[[i]])


length(yhat[[i]])

library(forecast)
ar1=vector()
yhat=vector()
for(i in seq(i,3184+i,1)){
  ar1[i]<-arima(oil.rets[i:3184+i],order=c(1,0,0),method="ML",include.mean = T)
  yhat[i]<-forecast(ar1,h=1)
  print(yhat[i])
 }
  

Arima1<-list(1250)
Fcasts1<-list(1250)
fore1<-list(1250)

for(i in 1:1250){
  Arima[[i]]<-arima(oil.rets[i:3183+i],order=c(1,0,0),method="ML",include.mean=T)
  Fcasts1[[i]]<-forecast(Arima1[[i]],h=1)
  fore1[i]<-unlist(Fcasts1[[i]]$mean)
  roll[i]<-as.ts(fore1[[i]])
}



temp.old <- tsdata
temp    <- tsdata
Arma1   <- list(10)
fcasts1 <- list(10)
fore1   <- list(10)
temp    <- list(10)

for(i in 1:10){
  Arma1[[i]]    <-  Arima(temp.old, order=c(2,0,2))
  fcasts1[[i]]  <- forecast(Arma1[[i]], h=1)  
  fore1[i]      <- unlist(fcasts1[[i]]$mean)
  temp[i]    <- as.xts(fore1[[i]], order.by = dates[i])
} 
  

for (i in 1:(nrow(df_all)-w)){
  glm1 <- glmboost(fm, data=df_all[i:(w-1+i), ], center=TRUE, control=boost_control(mstop = 100, trace=TRUE)) 
  ls_yhat[[i]] <- predict(glm1, newdata = df_all[w-1+i,])
}

is.na(roll[["fitted"]])

print(roll[["fitted"]])

mean.data <- vector()
for (i in 1:6) {
  mean.data[[i]] <- apply(MSFT[,i],2,basicStats)
}
mean.data[[3]]