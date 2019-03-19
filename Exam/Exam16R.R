
##  R commands for Exam 2016

library(rugarch); library(portes); library(sos); library(forecast); 
library(quantmod); require(fBasics);

data = read.table("data.txt", header=T) # Load the data
names(data)
lpalm = data$lpalm     # Transform the price into log 
ret.palm = 100*diff(lpalm) # Compute the log return
ret.palm = as.ts(ret.palm)
plot(ret.palm, col="blue")


data = read.table("data.txt", header=T) # Load the data
names(data)
lrub = data$lrubber     # Transform the price into log 
ret.rub = 100*diff(lrub) # Compute the log return
ret.rub = as.ts(ret.rub)
plot(ret.rub, col="red")


########## Question 1 ######
#### part a) ######
arima.rub <- auto.arima(ret.rub, d=0, max.p=3, max.q=3, allowmean = TRUE, trace=TRUE)
print(arima.rub)

model01 <- Arima(ret.rub, order = c(1,0,0), include.mean = FALSE, method="ML"); model01
model02 <- Arima(ret.rub, order = c(0,0,1), include.mean = FALSE, method="ML"); model02
model03 <- Arima(ret.rub, order = c(1,0,1), include.mean = FALSE, method="ML"); model03
model04 <- Arima(ret.rub, order = c(2,0,1), include.mean = FALSE, method="ML"); model04
model05 <- Arima(ret.rub, order = c(1,0,2), include.mean = FALSE, method="ML"); model05
model06 <- Arima(ret.rub, order = c(2,0,2), include.mean = FALSE, method="ML"); model06
model07 <- Arima(ret.rub, order = c(3,0,2), include.mean = FALSE, method="ML"); model07
model08 <- Arima(ret.rub, order = c(2,0,3), include.mean = FALSE, method="ML"); model08
model09 <- Arima(ret.rub, order = c(3,0,3), include.mean = FALSE, method="ML"); model09

logLik(model01); logLik(model02); logLik(model03); logLik(model04); logLik(model05); 
logLik(model06); logLik(model07); logLik(model08); logLik(model09);

AIC(model01); AIC(model02); AIC(model03); AIC(model04); AIC(model05); AIC(model06); AIC(model07); AIC(model08); AIC(model09); 

BIC(model01); BIC(model02); BIC(model03); BIC(model04); BIC(model05); BIC(model06); BIC(model07); BIC(model08); BIC(model09);  

length(ret.rub)

#### part b) ######

Acf(ret.rub, plot=F)

Box.test(ret.rub, lag=5,  type="Ljung")
Box.test(ret.rub, lag=10, type="Ljung")
Box.test(ret.rub, lag=20, type="Ljung")

########### f) (i) ############

##### Historical Volatility #######

stdev(ret.palm); var(ret.palm)

########### Estimation of AR(1)-GARCH(1,1) Norm model

spec1 <- ugarchspec(variance.model = list(model = "sGARCH", 
                    garchOrder = c(1,1)),
                   mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                  include.mean = TRUE),
                   distribution.model = "norm") # Model specification

sgarch11.fit.palm1 <- ugarchfit(ret.palm, spec = spec1, solver = "gosolnp", out.sample=5) # Model estimation 

print(sgarch11.fit.palm1)

sgarch.st.resid <- residuals(sgarch11.fit.palm1, standardize=TRUE)

sgarch.st.resid <- as.ts(sgarch.st.resid)

portest.st.res <- portest(sgarch.st.resid, lags=seq(5,15,5), order=0, 
                             test=c("LjungBox"),
                             MonteCarlo=TRUE, NREP=1000, InfiniteVarianceQ=FALSE, 
                             SquaredQ=FALSE, func=list(SimModel=NULL,FitModel=NULL), pkg=NULL, 
                             SetSeed=TRUE)

portest.st.res.SQ <- portest(sgarch.st.resid, lags=seq(5,15,5), order=0, 
                            test=c("LjungBox"),
                            MonteCarlo=TRUE, NREP=1000, InfiniteVarianceQ=FALSE, 
                            SquaredQ=TRUE, func=list(SimModel=NULL,FitModel=NULL), pkg=NULL, 
                            SetSeed=TRUE)
Acf(sgarch.st.resid, main="ACF of standardized residuals")

#"std" for the student-t 
#"sstd" for the skew-student-t 

########## Forecasting ##########

forc = ugarchforecast(sgarch11.fit.palm1, n.ahead=5)

forc

resid <- residuals(sgarch11.fit.palm1, standardize=FALSE)
tail(resid)

con.st.dev <- sigma(sgarch11.fit.palm1)
tail(con.st.dev)

tail(ret.palm)

############ AR(1)-EGARCH(1,1)

####### EGARCH(1,1) form palm oil

spec.egarch.palm <- ugarchspec(variance.model = list(model = "eGARCH", 
                                          garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(0,0), arfima = FALSE, 
                                      include.mean = TRUE),
                    distribution.model = "norm") # Model specification

egarch.fit.palm <- ugarchfit(ret.palm, spec = spec.egarch.palm, solver = "gosolnp", out.sample=5) # Model estimation 

print(egarch.fit.palm)

####### GARCH and EGARCH(1,1) form rubber

spec.garch.rub <- ugarchspec(variance.model = list(model = "sGARCH", 
                                                    garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(2,2), arfima = FALSE, 
                                                include.mean = TRUE),
                              distribution.model = "std") # Model specification

sgarch.fit.rub <- ugarchfit(ret.rub, spec = spec.garch.rub, solver = "gosolnp", out.sample=5) # Model estimation 

print(sgarch.fit.rub)

#"std" for the student-t 
#"sstd" for the skew-student-t 

sgarch.st.resid <- residuals(sgarch.fit.rub, standardize=TRUE) # Obtaining the standardized resids
sgarch.st.resid <- as.ts(sgarch.st.resid)
plot(sgarch.st.resid)
Box.test(sgarch.st.resid,lag=10,type='Ljung')

sgarch.st.resid <- residuals(sgarch.fit.rub, standardize=TRUE) # Obtaining the standardized resids
sgarch.st.residSQ <- sgarch.st.resid^2
sgarch.st.residSQ <- as.ts(sgarch.st.residSQ)
plot(sgarch.st.residSQ)
Box.test(sgarch.st.residSQ,lag=10,type='Ljung')

spec.egarch.rub <- ugarchspec(variance.model = list(model = "eGARCH", 
                                                garchOrder = c(1,1)),
                          mean.model = list(armaOrder = c(1,2), arfima = FALSE, 
                                            include.mean = TRUE),
                          distribution.model = "sstd") # Model specification

egarch.fit.rub <- ugarchfit(ret.rub, spec = spec.egarch.rub, solver = "gosolnp", out.sample=5) # Model estimation 

print(egarch.fit.rub)


egarch.st.resid <- residuals(egarch.fit.rub, standardize=TRUE) # Obtaining the standardized resids
egarch.st.resid <- as.ts(egarch.st.resid)
plot(egarch.st.resid)
Box.test(egarch.st.resid,lag=10,type='Ljung')

egarch.st.resid <- residuals(egarch.fit.rub, standardize=TRUE) # Obtaining the standardized resids
egarch.st.residSQ <- egarch.st.resid^2
egarch.st.residSQ <- as.ts(egarch.st.residSQ)
plot(egarch.st.residSQ)
Box.test(egarch.st.residSQ,lag=10,type='Ljung')

#"std" for the student-t 
#"sstd" for the skew-student-t 

######### NEWS IMPACT CURVES #####

dev.off()

par(mfcol=c(1,2))

ni.palm = newsimpact(z = NULL, egarch.fit.palm)  ## chane the ugarchfit object
plot(ni.palm$zx, ni.palm$zy, ylab=ni.palm$yexpr, xlab=ni.palm$xexpr, type="l", 
     main = "Palm oil - News Impact Curve EGARCH(1,1)")

ni.rub = newsimpact(z = NULL, egarch.fit.rub)  ## chane the ugarchfit object
plot(ni.rub$zx, ni.rub$zy, ylab=ni.rub$yexpr, xlab=ni.rub$xexpr, type="l", 
     main = "Rubber - News Impact Curve EGARCH(1,1)")

min(ni.palm$zy)
min(ni.rub$zy)


##### QQ-plot

qqnorm(ret.palm, main = "Normal Q-Q Plot of palm oil returns", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", col="darkblue", las=1)
qqline(ret.palm, col="red", lty=5, lwd=2)

qqnorm(ret.rub, main = "Normal Q-Q Plot of rubber returns", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", col="darkblue", las=1)
qqline(ret.rub, col="red", lty=5, lwd=2)

library(EnvStats) 

ret.palm <- as.numeric(ret.palm)
ret.rub <- as.numeric(ret.rub)

m1 = mean(ret.palm)
m2 = mean(ret.rub)
sd1 = (var(ret.palm))^0.5
sd2 = (var(ret.rub))^0.5

dev.off()

par(mfcol=c(1,2))

qqPlot(ret.palm, add.line=TRUE, distribution = "norm", param.list = list(mean = m1, sd=sd1))
qqPlot(ret.palm, add.line=TRUE, distribution = "t", param.list = list(df=5.008))

qqPlot(ret.rub, add.line=TRUE, distribution = "norm", param.list = list(mean = m2, sd=sd2))
qqPlot(ret.rub, add.line=TRUE, distribution = "t", param.list = list(df=5.008))

library(SkewHyperbolic)
param <- c(0,2,0,9)
qqskewhyp(ret.palm, param = param, main = "Skew Hyperbolic\n Q-Q Plot")
qqskewhyp(ret.rub, param = param, main = "Skew Hyperbolic Q-Q Plot for lrtn.dji")



