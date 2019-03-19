
## Asymmetry in volatility

library(quantmod)  # load the package "quantmod"
library(fBasics)   #  Load the package "fBasics"

getSymbols("^DJI", from="2005-01-01", to="2018-08-08") # get daily DJI data from Yahoo Finance

# write.csv(DJI[,6], "DJI.csv",
#          quote=TRUE, row.names=FALSE) # frcst.crude.csv

chartSeries(DJI[,6]) # <== Column 6 of the object "" in R.

dji_rets = diff(log(DJI[,6]))*100

plot(dji_rets, main="Dow Jones Industrial Average Returns", las=1)

plot(DJI[,6], main="Dow Jones Industrial Average Index", las=1)

basicStats(dji_rets)

par(mfrow=c(2,1))
plot(log(DJI[,6]))
plot(dji_rets)

dev.off()





