
library(tseries)
library(urca)
require(fUnitRoots)

# Engle-Granger procedure with generated data

set.seed (123456)
e1 <- rnorm(100)
e2 <- rnorm(100)

# First, two random walks were created, 
# in which the latter one, y2, has
# been set to 0.6*y1 + e2, 
# where e2 is a white noise process

y1 <- cumsum(e1)   ##generating random walk process
y2 <- cumsum(e2)

# y2 <- -0.6*y1+e2

plot(y1, type="l")
plot(y2, type="l")


require(fUnitRoots)

??adfTest
adfTest(y1,lags=8,type="nc")
adfTest(y1,lags=8,type="c")
adfTest(y1,lags=8,type="ct")

adfTest(diff(y1),lags=8,type="nc")
adfTest(diff(y1),lags=8,type="c")
adfTest(diff(y1),lags=8,type="ct")

adfTest(y2,lags=8,type="nc")
adfTest(y2,lags=8,type="c")
adfTest(y2,lags=8,type="ct")

adfTest(diff(y2),lags=8,type="nc")
adfTest(diff(y2),lags=8,type="c")
adfTest(diff(y2),lags=8,type="ct")

# Hence, the cointegrating vector 

lr.reg <- lm(y2 ~ y1)   ##both varibles are long run variable== this equatrion is long run equation

# First, the long-run equation 
# lr.reg has been estimated by OLS.

error <- residuals(lr.reg)
error.lagged <- error[-c(99,100)]
tail(error, 5)
tail(error.lagged, 5)

# the equilibrium error(stationarity of error) is stored as error, 
# and its lagged version has been created by
# simply dropping the last two entries

require(fUnitRoots)
adfTest(error,lags=8,type="nc")  ### with no-constatnt====error is stationary: random walk 
adfTest(error,lags=8,type="c")  ### stationary with only 10% : random walk with drift
adfTest(error,lags=8,type="ct")  ### with linear trend, it is no longer stationary : random walk with liner trend
#### if error is confirmed to be[error~i(0)] i(0)====== your model's varibales are cointergrated, they dont go far from each other, moving closer in the long run


plot(error, type="l")


ci.lr <- ur.df(error, lags=1, type='none') ##Performs the augmented Dickey-Fuller unit root test

dy1 <- diff(y1)
dy2 <- diff(y2)
diff.dat <- data.frame(embed(cbind(dy1, dy2), 2))   ###embed===generates the lages of dy1, dy2
colnames(diff.dat) <- c('dy1','dy2','dy1.1','dy2.1')
ecm.reg <- lm(dy2 ~ error.lagged + dy1.1 + 
                dy2.1, data=diff.dat) 

summary(ecm.reg)

 # Example from Holden and Perman (1994) 
# Engle-Granger procedure: Long-run relationship of 
# consumption, income, and wealth 
# in the United Kingdom

data(Raotbl3)
attach(Raotbl3)

lc <- ts(lc, start = c(1966,4), end=c(1991,2), frequency=4)
li <- ts(li, start=c(1966,4), end=c(1991,2), frequency=4)
lw <- ts(lw , start=c(1966,4), end=c(1991,2), frequency=4) 

ukcons <- window(cbind(lc, li, lw), start=c(1967,2), end=c(1991,2))

lc.eq <- summary(lm(lc ~ li + lw , data=ukcons))
li.eq <- summary(lm(li ~ lc + lw , data=ukcons)) 
lw.eq <- summary(lm(lw ~ li + lc, data=ukcons))

error.lc <- ts(resid(lc.eq) , start=c(1967,2), end=c(1991,2), frequency=4)
error.li <- ts(resid(li.eq) , start=c(1967,2), end=c(1991,2), frequency=4)    
error.lw <- ts(resid(lw.eq) , start=c(1967,2), end=c(1991,2), frequency=4)    

plot(error.lc)
plot(error.li)
plot(error.lw)

# Given the critical value -3.83 
# Conduct unit root test for the residuals

ci.lc <- ur.df(error.lc, lags=1, type='none')
ci.li <- ur.df(error.li, lags=1, type='none')
ci.lw <- ur.df(error.lw, lags=1, type='none')  ####error term from this equation is not strationary,,,,, taking this into equation does not make sense

# ur.df(y, type = c("none", "drift", "trend"), lags = 1,
#       selectlags = c("Fixed", "AIC", "BIC"))

unitrootTable(trend = c("nc"), statistic=c("n"))

jb.lc <- jarque.bera.test(error.lc)
jb.li <- jarque.bera.test(error.li)
jb.lw <- jarque.bera.test(error.lw)


# Engle-Granger: ECM for consumption and income 
# of the United Kingdom

ukcons2 <- ts(embed(diff(ukcons), dim=2), start=c(1967, 4), freq=4)
colnames(ukcons2) <- c('lc.d','li.d','lw.d','lc.d1','li.d1','lw.d1')

error.ecm1 <- window(lag(error.lc, k=-1), start=c(1967,4), end=c(1991,2))
error.ecm2 <- window(lag(error.li, k=-1), start=c(1967,4), end=c(1991,2))

ecm.eq1 <- lm(lc.d ~ error.ecm1 + lc.d1 + li.d1 + lw.d1, data=ukcons2) 
ecm.eq2 <- lm(li.d ~ error.ecm2 + lc.d1 + li.d1 + lw.d1, data=ukcons2)

summary(ecm.eq1)   ####p-value of error correction is insignifincant then it is not cointergration
summary(ecm.eq2)

