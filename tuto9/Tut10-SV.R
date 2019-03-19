

set.seed(1344)

library(rugarch); library(sos); library(forecast); 
library(quantmod); require(stochvol); require(fBasics);
library(stochvol);

options(max.print = 10000)

data = read.table("cimb.csv", header=T) # Load the data
names(data)
close.pr = log(data$cimb)     # Transform the price into log 
close.pr = ts(close.pr)
plot(close.pr)
length(close.pr)

### Compute the demeaned returns a) ####

ret <- 100*logret(close.pr, demean = TRUE)
# par(mfrow = c(2, 1), mar = c(1.9, 1.9, 1.9, 0.5), mgp = c(2, 0.6, 0))
plot(ret, type = "l", main = "CIMB stock returns")


# (b)	Using svsample() command of stochvol package estimate the stochastic 
# volatility model for the full sample. R command lines are as follows:
#how many values of parameters you want to generate==draws
result1 <- svsample(ret, draws = 20000, burnin = 1000, priormu = c(-10, 1), 
                    priorphi = c(20, 1.1), priorsigma = 0.1, thinlatent=1)

summary(result1, showlatent = FALSE)

h1t=apply(result1$latent,2,median)

v1=exp(h1t/2)

ts.plot(v1)
ts.plot(h1t)
length(v1)

# (c)	Using svsample() command of stochvol package estimate the stochastic 
# volatility model for the sample from the 1400th observation to 1000th observation 
# so that we drop the outlier from the sample. R command lines are as follows: 
 
 
result2 <- svsample(ret[1400:2668], priormu = c(-10, 1), draws = 20000, burnin = 1000,
                  priorphi = c(20, 1.1), priorsigma = 0.1, thinlatent=50,thinpara=50)

plot(result2)

summary(result2, showlatent = FALSE)

h2t=apply(result2$latent,2,median)

v2=exp(h2t/2)

ts.plot(v2)

length(v2)

# (d) Estimating EGARCH model

spec1 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(1,1), 
                    arfima = FALSE, include.mean = TRUE),
                    distribution.model = "std")     # Model specification

egarch.fit <- ugarchfit(ret[1400:2768], spec = spec1, solver = "gosolnp", out.sample=100) # Model estimation

print(egarch.fit)

con.st.dev <- sigma(egarch.fit)

con.st.dev <- ts(con.st.dev)

length(con.st.dev)

# (e) Plot in the same graph with different colours:

# Plotting vol series from SV model

ts.plot(v2, main="Volatility series from SV and EGARCH model") 

# Adding EGARCH volatility series in the graph

lines(con.st.dev, col="red", lty=2) 
#huge junp indicates over estimation

###the purpose all the above is to forecast volatility more accurtely


# (f) Forecast from SV model

## Predict 100 steps ahead

sv.forecast <- predict(result2, 100)

summary(sv.forecast)

plot(result2, forecast = sv.forecast)

hf=apply(sv.forecast,2,median)

vf=exp(hf/2)

ts.plot(vf)


# Forecast from EGARCH model

egarch.forecast = ugarchforecast(egarch.fit, n.ahead=100)

vol.egarch = sigma(egarch.forecast)

vol.egarch = ts(vol.egarch)

# Estimating GARCH model

spec2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(1,0), 
                    arfima = FALSE, include.mean = TRUE),
                    distribution.model = "norm") # Model specification

sgarch.fit <- ugarchfit(ret, spec = spec2, solver = "gosolnp", out.sample=0) # Model estimation

con.st.dev1 <- sigma(sgarch.fit)

con.st.dev1 <- ts(con.st.dev1)

length(ret)

con.st.dev1


###### RMSE ############

require(forecast)

f1 = vf^2 # forecasted values
x1 = (tail(ret[1400:2768], 100))^2 # Actual values squared returns
accuracy(f=f1, x=x1, test=NULL, d=NULL, D=NULL)


f2 = vol.egarch^2 # forecasted values
x2 = (tail(ret[1400:2768], 100))^2 # Actual values squared returns
accuracy(f=f2, x=x2, test=NULL, d=NULL, D=NULL)



