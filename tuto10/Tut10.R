
##  R commands for tutorial 10

library(rugarch); 
library(sos); 
library(forecast); 
library(quantmod); 
require(fBasics);

getSymbols("^GSPC", from="2011-01-03", to="2018-10-08") # format (YYYY-MM-DD)
## get daily S&P500 index
chartSeries(GSPC[,4]) # <== Column 4 of the object "" in R.
write.csv(GSPC[,4], "sp500.csv",  # the file name sp500.csv
         quote=TRUE, row.names=F) # 

data = read.table("sp500.csv", header=T) # Load the data
names(data)
lsp500 = log(data$GSPC.Close)     # Transform the price into log 
ret.sp500 = 100*diff(lsp500) # Compute the log return
ret.sp500 = ts(ret.sp500)
plot(ret.sp500)

############ Q2-a  #############
## Conduct the preliminary analysis on r_t. Compute descriptive statitics: 
## skewness, kurtosis, minimum, maximum etc. Conduct the normality and serial dependence tests 

require(fBasics); 
library(portes);

basicStats(ret.sp500)  ###In order to model garch family, at least we need 500 observations
normalTest(ret.sp500, method="jb", na.rm=TRUE) ##mthe result  of this test indicates that our retuen data is not normally distributed
LB5 <- Box.test(ret.sp500, lag=5, type = c("Ljung-Box"))
LB10 <- Box.test(ret.sp500, lag=10, type = c("Ljung-Box"))
print(LB5); print(LB10);
### we reject the null hypothesis for both, our retuen is not independent.
## GARCH is dependent process, therefore, data must have to some extent of correlation in data. Otherewise, we can't use GARCH family.
## Normally, investigation of lag is pretty enough till 10.

############ Q2-b ############
## Fit ARMA-GARCH(1,0) and ARMA(1,0)-EGARCH(1,1) models assuming normal distribution to the 
## r_t series. Obtain the normal QQ-plot of the standardized residuals, and write down 
## the fitted model. Is the model adequate? Why?

## ARMA(1,0)-GARCH(1,1) model

spec1 <- ugarchspec(variance.model = list(model = "sGARCH", 
                    garchOrder = c(1,1)),
                   mean.model = list(armaOrder = c(1,0), arfima = FALSE, 
                  include.mean = TRUE),
                   distribution.model = "norm") # Model specification

sgarch.fit <- ugarchfit(ret.sp500, spec = spec1, solver = "gosolnp", out.sample=5) # Model estimation 

print(sgarch.fit)

sgarch.st.resid <- residuals(sgarch.fit, standardize=TRUE) # Obtaining the standardized resids
sgarch.st.resid <- ts(sgarch.st.resid)
plot(sgarch.st.resid)
Box.test(sgarch.st.resid,lag=10,type='Ljung')  ### Residuals diagnostic has to be rejecting the NULL all the time, otherwise still have some improvement in model
 sgarch.st.resid <- ts(sgarch.st.resid)
# portest.st.res.1 <- portest(sgarch.st.resid, lags=seq(5,15,5), order=0, 
#                     test=c("LjungBox"),
#                     MonteCarlo=TRUE, NREP=1000, InfiniteVarianceQ=FALSE, 
#                     SquaredQ=TRUE, func=list(SimModel=NULL,FitModel=NULL), pkg=NULL, 
#                     SetSeed=TRUE)
# test=c("gvtest","BoxPierce", "LjungBox","Hosking","LiMcLeod") # Other test options in portes
# print(portest.st.res.1)

acf(sgarch.st.resid)

## ARMA(1,0)-EGARCH(1,1) model

spec2 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(1,0), arfima = TRUE, include.mean = TRUE),
                    distribution.model = "norm") # Model specification

egarch.fit <- ugarchfit(ret.sp500, spec = spec2, solver = "gosolnp", out.sample=5) # Model estimation 

print(egarch.fit)

egarch.st.resid <- residuals(egarch.fit, standardize=TRUE) # Obtaining the standardized resids
plot(egarch.st.resid)
Box.test(egarch.st.resid,lag=10,type='Ljung')
egarch.st.resid <- ts(egarch.st.resid)

portest.st.res.2.variance.model <- portest(egarch.st.resid, lags=seq(5,15,5), order=0, 
                            test=c("LjungBox"),
                            MonteCarlo=TRUE, NREP=1000, InfiniteVarianceQ=FALSE, 
                            SquaredQ=TRUE, func=list(SimModel=NULL,FitModel=NULL), pkg=NULL, 
                            SetSeed=TRUE)

portest.st.res.2.mean.model <- portest(egarch.st.resid, lags=seq(5,15,5), order=0, 
                            test=c("LjungBox"),
                            MonteCarlo=TRUE, NREP=1000, InfiniteVarianceQ=FALSE, 
                            SquaredQ=FALSE, func=list(SimModel=NULL,FitModel=NULL), pkg=NULL, 
                            SetSeed=TRUE)
 #test=c("gvtest","BoxPierce", "LjungBox","Hosking","LiMcLeod") # Other test options in portes

print(portest.st.res.2.variance.model)
print(portest.st.res.2.mean.model)

Acf(egarch.st.resid)


######### QQ-plot for GARCH(1,1) standardised resids #########


qqnorm(sgarch.st.resid, main = "Normal Q-Q Plot standardised resids", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", col="darkblue", las=1)

qqline(sgarch.st.resid, col="red", lty=5, lwd=2)


######### QQ plot for EGARCH(1,1) standardised resids #######

qqnorm(egarch.st.resid, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", col="darkblue", las=1)

qqline(egarch.st.resid, col="red", lty=5, lwd=2)


######## 2c) Useful plots ####### 

plot(sgarch.fit, which=12) # Which from 1 to 12 to plot different graphs

plot(egarch.fit, which=12) #

######### 3) News impact curve #########

# par(mfcol=c(2,1))

dev.off()

ni=newsimpact(z = NULL, sgarch.fit)  ## change the ugarchfit object
plot(ni$zx, ni$zy, ylab=ni$yexpr, xlab=ni$xexpr, type="l", 
     main = "News Impact Curve for GARCH(1,1)")


ni=newsimpact(z = NULL, egarch.fit)
plot(ni$zx, ni$zy, ylab=ni$yexpr, xlab=ni$xexpr, type="l", 
     main = "News Impact Curve for EGARCH(1,1)")


############ 4) TGARCH ########

# Let  a_t=r_t-r??  where  r??  is the sample mean of r.
# Fit a TGARCH(1,1) model with a constant term in the volatility equation 
# to the r_t series. Write down the fitted model.
# Is the leverage effect statistically significant? Why?

data = read.table("sp500.csv", header=T) # Load the data
names(data)
lsp500 = log(data$GSPC.Close)     # Transform the price into log 
ret.sp500 = 100*diff(lsp500) # Compute the log return
ret.sp500 = ts(ret.sp500)
plot(ret.sp500)

spec5 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(1,0), arfima = FALSE, include.mean = TRUE),
                    distribution.model = "std")
#"std" for the student-t, 
#"sstd" for the skew-student-t, 

gjrgarch.fit.spec5 <- ugarchfit(ret.sp500, spec = spec5, solver = "gosolnp", out.sample=5)

print(gjrgarch.fit.spec5)


############# 5) GARCH(1,1) Likelihood ratio test #########

##### Restricted model

igarch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1) ),
                    mean.model = list(armaOrder = c(1,0), arfima = FALSE, include.mean = TRUE),
                    distribution.model = "norm", fixed.pars = list(alpha1=0.05, beta1=0.94))

igarch.fit.rest <- ugarchfit(ret.sp500, spec = igarch, solver = "gosolnp", out.sample=5)

coef(igarch.fit.rest)

##### Unrestricted model


spec7 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(1,0), arfima = FALSE, include.mean = TRUE),
                    distribution.model = "norm")

sgarch.fit.unres <- ugarchfit(ret.sp500, spec = spec7, solver = "gosolnp", out.sample=5)



### LR = -2(Lr-Lu) Chi-sq m)

LR1 = -2*(likelihood(igarch.fit.rest) - likelihood(sgarch.fit.unres))
qchisq(0.05,df=2,lower.tail=F)

# we reject the NULL 

### Chi square with 1 degree of freedom is 3.841 

show(sgarch.fit)

