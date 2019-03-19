
## R commands for Tutorial-07 
## Week-08

library(rugarch); 
library(sos); 
library(forecast); 
library(quantmod); 
require(fBasics);

data = read.table("data.txt", header=T) # Load the data
names(data)
lpalm = log(data$lpalm)     # Transform the price into log 
ret.palm = 100*diff(lpalm) # Compute the log return
ret.palm = as.ts(ret.palm)
plot(ret.palm, col="blue")
length(ret.palm)

########### Estimate AR(1)-GARCH(1,1) model

spec1 <- ugarchspec(variance.model = list(model = "sGARCH", 
                    garchOrder = c(1,1)),
                   mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                  include.mean = TRUE),
                   distribution.model = "norm") # Model specification

garch11.fit.palm <- ugarchfit(ret.palm, spec = spec1, solver = "gosolnp", out.sample=10) # Model estimation 

print(garch11.fit.palm)



#"std" for the student-t 
#"sstd" for the skew-student-t 

########## Forecasting ##########

forc = ugarchforecast(garch11.fit.palm, n.ahead=10)

forc
forc1=as.numeric(unlist(forc))
plot(forc)

forc1 <- as.data.frame(forc)


resid <- residuals(garch11.fit.palm, standardize=FALSE)


tail(resid,1)
0.1682464^2

X<-0.027095+0.138110*0.168246^2+0.756130*0.435722^2

0.4178^2

y<-0.027095+0.13811*0.1745584+0.7561304*0.1745584

0.4280^2
con.st.dev <- sigma(garch11.fit.palm)
tail(con.st.dev)

tail(ret.palm)

### Manual calculation of sigma_{t+1}, sigma_{t+2}, sigma_{t+3} 

fc.vol.1 <- sqrt(0.027095 + 0.138111*0.16824606^2 + 0.756128*0.4357224^2)
fc.vol.2 <- sqrt(0.027095 + (0.138111+0.756128)*fc1^2)
fc.vol.3 <- sqrt(0.027095 + 0.027095*(0.138111+0.756128) + (0.138111+0.756128)^2*fc1^2)


####### Return forecasts using 

fc.ret.1 <- 0.021346*(1-0.019629) + 0.019629*tail(ret.palm[1:758],1)
fc.ret.2 = 0.021346*(1-0.019629) + 0.019629*fc.ret.1
fc.ret.3 = 0.021346*(1-0.019629) + 0.019629*fc.ret.2
fc.ret.4 = 0.021346*(1-0.019629) + 0.019629*fc.ret.3
fc.ret.5 = 0.021346*(1-0.019629) + 0.019629*fc.ret.4

