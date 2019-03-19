library(quantmod)
nikkei <- getSymbols("^N225", src = "yahoo", from = as.Date("2013-04-04"),
                     to = as.Date("2017-08-18"), auto.assign = FALSE)
nikkei.price <- nikkei$N225.Close


chartSeries(nikkei)
is.na(nikkei.rets)
sum(is.na(nikkei.rets))

nikkei.rets<-100*log(diff(nikkei.price))[-1]
nikkei.rets<- na.spline(nikkei.rets)

plot(nikkei.rets)

library(tseries)
gar1<-garch(nikkei.rets,order=c(1,1),trace=F)
summary(gar1)

Garch=list()
for(i in 1:3){
  for(k in 1:3){
    Garch<-garch(nikkei.rets,order=c(i,k),trace=F,approximation=F)
  }
}

unlist(Garch)
