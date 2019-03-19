
## QQ Plot

dev.off()

library(quantmod)  #  <= load the package "quantmod"
getSymbols("^DJI", from="2005-01-01", to="2015-07-31") # format (YYYY-MM-DD)
# get daily Dow Jones average index data from Yahoo Finance

DJIreturns = diff(log(DJI[,6]))*100

DJIreturns = ts(DJIreturns)

qqnorm(DJIreturns, main = "Normal Q-Q Plot of Dow Jones Average Index returns", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", col="darkblue", las=1)

qqline(DJIreturns, col="red", lty=5, lwd=2)


## QQ Plot of normally distibuted data

dev.off()

set.seed(1233)
random_data <- rnorm(n=1500, m=5, sd=2.5)
qqnorm(random_data, main = "Normal Q-Q Plot of normal data", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", col="darkblue", las=1)
qqline(random_data, col="red", lty=5, lwd=2)

plot(random_data, xlab="", ylab="", type="l", col="red", main="Normally distributed data", las=1)


## QQ plot for Student distribution with prespecified degree of freedom

library(Dowd)

Ra <- rnorm(100)

TQQPlot(Ra, 20)


## Skew Hyperbolic Student t-Distribution Quantile-Quantile and Percent-Percent Plots

library(SkewHyperbolic)

par(mfrow = c(1,2))
param <- c(0,1,0,10)
y <- rskewhyp(500, param = param)
qqskewhyp(y, param = param, main = "Skew Hyperbolic\n Q-Q Plot")
ppskewhyp(y, param = param, main = "Skew Hyperbolic\n P-P Plot")

