
library(quantmod)  #  <= load the package "quantmod"
library(fBasics)   #  <= Load the package "fBasics"

getSymbols("^DJI", from="2005-01-01", to="2015-07-31") # format (YYYY-MM-DD)
# get daily Dow Jones average index data from Yahoo Finance


basicStats(DJI[,4])
basicStats(DJI[,6])

DJIreturns = diff(log(DJI[,6]))*100

basicStats(DJIreturns)


set.seed(1233)
random_data <- rnorm(n=2500, m=5, sd=2.5)
basicStats(random_data)
