

library(quantmod)  #  <= load the package "quantmod"
library(fBasics) 

getSymbols("^DJI", from="2005-01-01", to="2015-07-31") # format (YYYY-MM-DD)
# get daily Dow Jones average index data from Yahoo Finance

DJIreturns = diff(log(DJI[,6]))*100

DJIreturns = ts(DJIreturns)

normalTest(DJIreturns, method="jb", na.rm=TRUE) # <== Perform normality test.


set.seed(1233)
random_data <- rnorm(n=1500, m=5, sd=2.5)


normalTest(random_data, method="jb", na.rm=TRUE) # <== Perform normality test.
