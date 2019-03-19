

# Unit Root tests library(tseries); library(urca;) 

############### Question 1 #############

###### Loading the data #######
crude.data <- read.table("crude.txt", header=TRUE)
rice.series <- read.delim("rice.txt", header=TRUE)
attach(crude.data)
attach(rice.series)
#why use "attach"==in order to read variables' names

crude <- ts(crude)
rice = ts(rice)
plot(log(rice), type="l", col="blue")
plot(log(crude), type="l", col="red")
library(quantmod)
chartSeries(crude)

###### # By conducting unit root tests identify whether there is 
# a unit root in the rice price series.

library(tseries); #K is the number of lag
adf.test(log(rice), k=0)  # DF test #k=0 means lag is zero which indicates test is gonna be normal dickey-fuller test
adf.test(log(rice), k=trunc((length(log(rice)))^(1/3))) # ADF test
pp.test(log(rice))        # PP test
#output;P-value is insignificant then data is not stationary
#----------------------------
CrudeLogret<-diff(crude)
plot(CrudeLogret)
adf.test(CrudeLogret)
#----------------------------
adf.test(log(crude), k=0)  # DF test
adf.test(log(crude), k=trunc((length(log(crude)))^(1/3))) # ADF test
##the value of dickey-fuller value is pretty small(ex.-10),would be stationary, while if it is larger(ex.-1,-2), most probabry, it'll be non-stationary
adf.test(diff(log(crude)),k=0)
plot(diff(log(crude)))
chartSeries(diff(log(crude)))


############### Question 2 ############

library(portes);    ######log return=continiously compounded return  
LB.test <- portest(diff(log(rice)), lags=c(5, 10), test=c("LjungBox"), SquaredQ=FALSE)
LB.test

##p-value is pretty small, then reject Null,means residuals is not white-noise

Box.test(diff(log(rice)), lag = 5, type = c("Box-Pierce"), fitdf = 0)

library(forecast)
model<-auto.arima(diff(log(rice)),stepwise=F,trace=F,approximation=F)
model
tsdiag(model)
model
model<-Arima(diff(log(rice)),order=c(1,0,0))

plot(model,level=c(50,95,h=30))


LB.test <- portest(diff(log(crude)), lags=c(5, 10), test=c("LjungBox"), SquaredQ=FALSE)
LB.test
Box.test(diff(log(crude)), lag = 5, type = c("Box-Pierce"), fitdf = 0)

#### options c("Ljung-Box","Box-Pierce")

############## Question 3 ############

set.seed(545)
whitenoise = rnorm(n=500, m=0, sd=1)
plot(whitenoise)
plot(density(whitenoise))
LB.WN1 <- Box.test(whitenoise, lag = c(10), type = c("Box-Pierce"), fitdf = 0)
LB.WN2 <- Box.test(whitenoise, lag = c(10), type = c("Ljung-Box"), fitdf = 0)
LB.WN1
LB.WN2

random=cumsum(whitenoise)
LB.RW <- Box.test(random, lag = c(10), type = c("Ljung-Box"), fitdf = 0)
acf(random)
pacf(random)

####### The command lines below use the portes library to test the serial correlation #######

library(portes);
LB.WN <- portest(whitenoise, lags=c(5, 10), test=c("LjungBox"), SquaredQ=FALSE)
LB.WN
plot(whitenoise, type="l")

############# Question 4 ############

## Logged rice series 

library(forecast)
Acf(log(rice))
Pacf(log(rice))

Acf(100*diff(log(rice)))
Pacf(100*diff(log(rice)))

Acf(log(crude))
Pacf(log(crude))

Acf(100*diff(log(crude)))
Pacf(100*diff(log(crude)))

Acf(whitenoise)
Pacf(whitenoise)


############## Question 5 ###########

help(arima)
library(forecast)
model1 <- arima(100*diff(log(rice)), order=c(5,0,0), method="ML", fixed = c(NA,0,0,NA,NA,NA)) #NA=non-restrictions on that lag, 0=no estimation on that lag

                model1
                plot(model1)

model2 <- arima(100*diff(log(rice)), order=c(0,0,4), method="ML", fixed = c(NA,NA,0,NA,NA))
model2
plot(model2)
##Inverse MA roots=characteristics of roots, so in a unit circle, if points are inside boarder, your estimated model is stable/stationary


############# Question 6 ############

#### Model 1 is slightly pereferred 


############# Question 7 ############ 

res.model1 = residuals(model1)
res.model2 = residuals(model2)
tsdiag(model1)
tsdiag(model2)
plot(res.model1)
plot(res.model2)

model1<-Arima(model1,level=80,95,h=30)
plot(forecast(model1))

model2<-Arima(model2,level=80,95,h=30)
plot(forecast(model2))


##portes is used for checking the presense of residuals auto-correl
#this function is used to activate Ljung-box and Box pirece
library(portes);
LB.res.model1 <- portest(res.model1, lags=c(5, 10), test=c("LjungBox"), SquaredQ=FALSE)
LB.res.model1

LB.res.model2 <- portest(res.model2, lags=c(5, 10), test=c("LjungBox"), SquaredQ=FALSE)
LB.res.model2
##both return p-value as larger than 0.05,then fail to reject H0 means non-auto-correl in residuals

plot(res.model1, col="blue")
plot(res.model2, col="pink")

Box.test(res.model1, lag = 5, type = c("Box-Pierce"), fitdf = 0)
Box.test(res.model2, lag = 5, type = c("Box-Pierce"), fitdf = 0)
