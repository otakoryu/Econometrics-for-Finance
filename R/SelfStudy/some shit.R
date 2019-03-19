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

require(forecast)

#--------------------------------------------------------
oilar1=list()
oilyhatar1=list()
for(i in seq(1,1250,1)){
  oilar1[[i]]<-arima(oil.rets[i:(3183+i)],
                     order=c(1,0,0),method="ML",
                     include.mean=T)
  oilyhatar1[[i]]<-forecast(oilar1[[i]],h=1)$mean
  print(oilyhatar1[[i]])
  print(i)
  
}
rolloilar1=as.numeric(unlist(oilyhatar1))
rolloilar1<-as.ts(rolloilar1)


head(rolloilar1,2)
accuracy(oil.te,rolloilar1)

plot(oil.te)
par(new=T)
lines(rolloilar1,col="red")

x <- matrix(data = NA, nrow=10, ncol=10)
for(i in 0:9){
  for(j in 0:9){
    fit <- arima(oil.rets, order=c(i,0,j))
    acc <- accuracy(fit)
    x[i+1,j+1] <- acc[[5]] # Number 5 indicates the position of MAPE in the accuracy list
    print(i);print(j)
  }
  
}

which(x==min(x), arr.ind=T)
row col
