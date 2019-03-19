
library(quantmod); require(fBasics); library(forecast); library(portes);  # load the package "quantmod"

##### QUESTION 1 #####

##### R output #####

getSymbols("^DJI", from="2005-01-03", to="2015-09-17", src = "yahoo") # format (YYYY-MM-DD)
# Retrieve daily Dow Jones Industrial Average index (DJI) from Yahoo Finance
ldji <- log(DJI[,6])                ## Transform index to logarithmic series.   
lrtn.dji <- diff(log(DJI[,6]))*100  ## Compute the logarithmic returns.
lrtn.dji <- as.ts(lrtn.dji)         ## Create time series object.  
basicStats(lrtn.dji)                ## Compute Summary Statistics

write.csv(lrtn.dji, "lrtn.dji.csv",  # the file name st.res.crude.csv
          quote=TRUE, row.names=FALSE) #

## One sample t test ##

t.test(lrtn.dji)

### Question 1 -a(i): 
### Answer: Since zero lies between UCL and LCL. And based on the t-stat, we fail to reject the null hypothesis 
### p-value is insignificant
### t statistic does not exceed critical value of 1.96. 

### Question 1-a(ii):
### Answer: Yes. t = -0.081069/(sqrt(6/2696)) where -0.081069 skewness coef.
### -1.718459 which is not significant. Fail to Reject the null. 
### so that the hypothesis of symmetric distribution is not rejected.

### Question 1-a(iii):

### Answer: Yes, because Q(5), Q(10), and Q(20) of the squared residuals is 
### with p-value close to zero.

## Squared returns ##
lrtn.djiSQ <- lrtn.dji^2
lrtn.djiSQ <- as.ts(lrtn.djiSQ)
## Ljung-Box Test ##
Box.test(lrtn.djiSQ, lag=5,  type="Ljung")
Box.test(lrtn.djiSQ, lag=10, type="Ljung")
Box.test(lrtn.djiSQ, lag=20, type="Ljung")


### Question 1-a(): Does the daily log return of the Dow Jones index have heavy tails? Why?
### Answer: Yes. t = 10.823588/(sqrt(24/2696)) where 10.823588 is kurtosis coef.
### 114.71 which is highly significant.


### Question 1-b:

acf(lrtn.dji, lag.max = 10, plot=F, na.action = na.pass)
pacf(lrtn.dji, lag.max = 10, plot=TRUE, na.action = na.pass)

1.96*(1/sqrt(length(lrtn.dji))) ## Upper bound
-1.96*(1/sqrt(length(lrtn.dji))) ## Lower bound

## Answer reject the null hypothesis


### Question 1-c:

Box.test(lrtn.dji, lag=5,  type="Ljung")


### Question 1-d:

model1=arima(ldji, order=c(2,1,2))
print(model1)
out.model1<-capture.output(model1)
cat(out.model1,file="Output.Model1.txt",sep="\n",append=TRUE)


##### QUESTION 2 #####


### QUESTION 2-a: 


## QQ plot shows that Student t with 3 df is best suited for the returns

### QUESTION 2-b:

library(rugarch);

write.csv(lrtn.dji, "lrtn.dji.csv",  # the file name st.res.crude.csv
          quote=TRUE, row.names=FALSE) #

lrtn.dji <- read.table(file.choose(), header = TRUE)

#### Standard GARCH model estimation

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                   mean.model = list(armaOrder = c(2,2), arfima = FALSE, include.mean = TRUE),
                   distribution.model = "std")  
#"std" for the student-t, 
#"sstd" for the skew-student-t, 
model2 <- ugarchfit(lrtn.dji, spec = spec, solver = "solnp", out.sample=100)

print(model2)

st.res.model2 <- residuals(model2, standardize=TRUE)

Box.test(st.res.model2, lag=5,  type="Ljung")
Box.test(st.res.model2, lag=10,  type="Ljung")
Box.test(st.res.model2, lag=20,  type="Ljung")

#### EGARCH model estimation

spec <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
                   mean.model = list(armaOrder = c(2,2), arfima = FALSE, include.mean = TRUE),
                   distribution.model = "std")  
#"std" for the student-t, 
#"sstd" for the skew-student-t, 
model3 <- ugarchfit(lrtn.dji, spec = spec, solver = "solnp", out.sample=100)

print(model3)


st.res.model3 <- residuals(model3, standardize=TRUE)
Box.test(st.res.model3, lag=5,  type="Ljung")
Box.test(st.res.model3, lag=10,  type="Ljung")
Box.test(st.res.model3, lag=20,  type="Ljung")




BoxPierceSQ <- BoxPierce(lrtn.dji, lags=seq(5,15,5), SquaredQ=TRUE)
LjungBox(lrtn.dji,lags=seq(5,25,5), order=0, SquaredQ=FALSE)

basicStats(lrtn.gspc)

head(lrtn.dji)
acf(lrtn.dji, plot=TRUE, na.action = na.pass)
pacf(lrtn.dji, na.action = na.pass)

print(DJIhead)
out1<-capture.output(DJIhead)
cat(out1,file="DJIhead.csv",sep="\n",append=TRUE)


auto.arima(ldji)






getSymbols("^GSPC", from="2005-01-03", to="2015-09-17") # format (YYYY-MM-DD)
# get daily S&P500 index data from Yahoo Finance

DJIhead=head(DJI)
tail(DJI)

lgspc = log(GSPC[,6])

lrtn.gspc=diff(log(GSPC[,6]))*100 ## Transform to log returns.


lrtn.gspc <- as.ts(lrtn.gspc)