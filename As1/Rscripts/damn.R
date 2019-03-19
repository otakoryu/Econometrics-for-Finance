
point=NULL
for(i in 1:1250){
  ar1[[i]]<-arima(oil.rets[i:(3183+i)],
                  order=c(1,0,0),method="ML",
                  include.mean=T)
  yhat[[i]]<-forecast(ar1[[i]],h=1)
  point<=as.data.frame(yhat[[i]])
}



Mean=NULL
for(i in 1:1250){
  Mean<-as.numeric(yhat[[i]]$mean)
  print(i)
}

class(Mean)
