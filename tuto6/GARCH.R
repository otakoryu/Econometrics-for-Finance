
##  R commands for tutorial 7

library(rugarch);library(portes); library(sos); library(forecast); 
library(quantmod); require(fBasics);

getSymbols("^GSPC", from="2005-01-01", to="2018-08-08") # format (YYYY-MM-DD)
# retrieve daily S&P500 index data from Yahoo Finance

sp500_rets = ts(diff(log(GSPC[,4]))*100)

head(sp500_rets, 5)

length(sp500_rets)

na.omit(sp500_rets)

plot(sp500_rets)

########### f) (i) ############

##### Historical Volatility #######

stdev(sp500_rets, na.rm=TRUE); 
var(sp500_rets, na.rm =TRUE)

########### f) (ii) Estimate AR(1)-ARCH(1) model
library(rugarch)
spec1 <- ugarchspec(variance.model = list(model = "sGARCH", 
                    garchOrder = c(1,1)),                         #c(arch term,variance term)
                   mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                  include.mean = TRUE),
                   distribution.model = "norm") # Model specification


arch1.fit <- ugarchfit(sp500_rets[2:3423], spec = spec1, solver = "gosolnp", out.sample=5) # Model estimation 

print(arch1.fit)


#"std" for the student-t 
#"sstd" for the skew-student-t 

########### f) (iii) Estimate AR(1)-GARCH(1,1) model

spec2 <- ugarchspec(variance.model = list(model = "sGARCH", 
                                          garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                                      include.mean = TRUE),
                    distribution.model = "snorm") # Model specification

sgarch.fit <- ugarchfit(sp500_rets[2:3423], spec = spec2, solver = "gosolnp", out.sample=5) # Model estimation 

print(sgarch.fit)

