
library(quantmod)  # load the package "quantmod"
library(fBasics)   #  Load the package "fBasics"

getSymbols("^KLSE", from="2005-01-01", to="2018-08-08") # format (YYYY-MM-DD)
# retrieve daily KLSE stock data from Yahoo Finance

chartSeries(KLSE[,6]) # <== Column 6 of the object "" in R.

# write.csv(KLSE[,6], "KLSE.csv",
#     quote=TRUE, row.names=FALSE) # frcst.crude.csv

getSymbols("^DJI", from="2005-01-01", to="2018-08-08") # get daily DJI data from Yahoo Finance

# write.csv(DJI[,6], "DJI.csv",
#          quote=TRUE, row.names=FALSE) # frcst.crude.csv

chartSeries(DJI[,6]) # <== Column 6 of the object "" in R.

dji_rets = diff(log(DJI[,6]))*100

plot(dji_rets, main="Dow Jones Industrial Average Returns", las=1)

plot(DJI[,6], main="Dow Jones Industrial Average Index", las=1)

basicStats(dji_rets)

# Some descriptives for downloaded data

mean(DJI[,6])
sd(KLSE[,4], na.rm =TRUE)
median(KLSE[,4])
var(KLSE[,4])
sqrt(var(KLSE[,4]))
summary(KLSE[,4])