library(quantmod)

getSymbols("TM", from="2005-01-01", to="2018-08-08")

chartSeries(TM[,4])

TM.rets = diff(log(TM[,4]))*100

plot(TM.rets, main="Dow Jones Industrial Average Returns", las=1)

plot(TM[,4], main="Dow Jones Industrial Average Index", las=1)

basicStats(dji_rets)

par(mfrow=c(2,1))
plot(log(TM[,6]))
plot(TM.rets)


garch11.fit1 <- ugarchfit(sp500_rets[2:1600], spec = spec1, solver = "gosolnp", out.sample=5)

garch11.fit2 <- ugarchfit(sp500_rets[1601:3423], spec = spec1, solver = "gosolnp", out.sample=5)
