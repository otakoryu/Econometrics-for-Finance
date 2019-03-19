
##  R commands for Tutorial # 3

library(quantmod)  # load the package "quantmod"
require(fBasics)

### Problem 1

getSymbols("MSFT", from="2005-01-03", to="2017-08-07") # format (YYYY-MM-DD)
# get daily Microsoft stock data from Yahoo Finance

getSymbols("^IXIC", from="2005-01-03", to="2017-08-07") # format (YYYY-MM-DD)
# get daily NASDAQ composite index from Yahoo Finance

getSymbols("^DJI", from="2005-01-03", to="2017-08-07") # format (YYYY-MM-DD)
# get daily Dow Jones composite index from Yahoo Finance

head(MSFT)
head(IXIC)
head(DJI)

tail(MSFT)
tail(IXIC)
tail(DJI)

apply(MSFT[,4],2,basicStats)

## Compute the simple returns

sim.ret.msft = 100*(diff(MSFT[,4], lag = 1, differences = 1)/lag(MSFT[,4], k=1))
sim.ret.ixic = 100*(diff(IXIC[,4], lag = 1, differences = 1)/lag(IXIC[,4], k=1))
sim.ret.dji = 100*(diff(DJI[,4], lag = 1, differences = 1)/lag(DJI[,4], k=1))

head(sim.ret.msft)
head(sim.ret.ixic)
head(sim.ret.dji)

sim.ret.msft = ts(sim.ret.msft)
sim.ret.ixic = ts(sim.ret.ixic)
sim.ret.dji = ts(sim.ret.dji)

apply(sim.ret.msft,2,basicStats)
apply(sim.ret.ixic,2,basicStats)
apply(sim.ret.dji,2,basicStats)

d1=density(sim.ret.msft, na.rm = TRUE)

plot(d1$x,d1$y,type='l',xlab='return',ylab='density',main="Empirical density of 
     Microsoft simple returns", las=1)
mu=0.045338; s1 =  1.663011
## mu=mean(sim.ret.msft, na.rm = TRUE); s1 = sd(sim.ret.msft, na.rm = TRUE) # <== compute the sample mean and standard deviation of Mirosoft simple returns.
x=seq(-15,15,0.01) # <= create a sequence of real numbers from -15 to 15 with increment 0.01.
y=dnorm(x,mean=mu,sd=s1) # <= obtain normal density with mean mu and standard deviation s1.
lines(x,y,lty=2) # <= impose a dashed line on the density plot for comparison with normal density.
# <= you can also use different colors in the plot. For example,
lines(x,y,lty=2,col="darkblue") # will plot a darkblue curve.

normalTest(sim.ret.msft,method="jb", na.rm=TRUE)
normalTest(sim.ret.ixic,method="jb", na.rm=TRUE)
normalTest(sim.ret.dji,method="jb", na.rm=TRUE)

lrtn.msft=diff(log(MSFT[,4]))*100 ## Transform to log returns.
lrtn.ixic=diff(log(IXIC[,4]))*100 ## Transform to log returns.
lrtn.dji=diff(log(DJI[,4]))*100

head(lrtn.msft)
head(lrtn.ixic)
head(lrtn.dji)

apply(lrtn.msft,2,basicStats)
apply(lrtn.ixic,2,basicStats)
apply(lrtn.dji,2,basicStats)

t.test(lrtn.msft)
t.test(lrtn.ixic)
t.test(lrtn.dji)

normalTest(lrtn.msft, method="jb", na.rm=TRUE)

par(mfcol=c(1,1))

d2=density(lrtn.msft, na.rm=TRUE)
plot(d2$x,d2$y,type='l', xlim=c(-7, 7), xlab="", ylab="", main="Density Plot", las=1)  #  <== Plot the sample density of log returns
names(d1) #  <== Find out the output variables of the command "density"
mu=0.021107; s1 = 1.704606
# mu=mean(lrtn.msft); s1 = sd(lrtn.msft) # <== compute the sample mean and standard deviation.
x=seq(-10,10,0.01) # <=== create a sequence of real numbers from -0.4 to 0.4 with increment 0.01.
y=dnorm(x,mean=mu,sd=s1) # <=== obtain normal density with mean mu and standard deviation s1.
lines(x,y,lty=2, col="red") # <== impose a dashed line on the density plot for comparison with normal density.

d3=density(lrtn.ixic, na.rm=TRUE)
plot(d3$x,d3$y,xlab='log-rtn', xlim=c(-5,5), ylab='density',type='l',main="S&P Comp 1500", las=1)

d4=density(lrtn.dji, na.rm=TRUE)
plot(d4$x,d4$y,xlab='log-rtn', xlim=c(-5, 5), ylab='density',type='l',main="DJI", las=1)
 

##### Problem 2

mda=read.table("m-pg3indexes.txt", header=T) # Load the data file "m-pg3indexes"
mrtn=mda[,3:6]
lmrtn=log(mrtn+1)

######### Problem 2 part a ########

lpg=lmrtn[,1]
t.test(lpg)

######### Problem 2 part b ########

t1=skewness(lpg)/sqrt(6/length(lpg))
t1

######## Problem 2 part c #######

t2=kurtosis(lpg)/sqrt(24/length(lpg))
t2


##### Problem 3

mda=read.table("m-pg3indexes.txt", header=T)
head(mda)

## Q-a	Compute the simple return series.



## Q-b Compute the sample mean, 
## standard deviation, skewness, excess kurtosis, 
## minimum, and maximum of each simple return series.




## Q-c Obtain the empirical density function of the simple returns of 
## Proctor and Gamble (PG) stock. 
## Are the daily simple returns normally distributed? Why? 
## Perform a normality test to justify your answer.



## Q-d Transform the simple returns to log returns. Compute the sample mean,
## standard deviation, skewness, excess kurtosis, minimum, and maximum 
## of each log return series.



######## Problem 4 ####

 da=read.table("m-exusmal.txt",header=T)
 head(da[,2])
 malus=diff(log(da[,2]))
 basicStats(malus)
 
 plot(da[,2], xlab="", ylab="", main="Monthly MYR-USD exchange rate", 
     type="l", las=1)

 plot(malus, xlab="", ylab="", main="Log returns of monthly MYR-USD exchange rate", 
      type="l", las=1)
 
 d1=density(malus)
 par(mfcol=c(1,1))
 plot(d1$x,d1$y,xlab='log-rtn',ylab='density',
      main='FX - Malaysia vs USA',type='l', las=1)

 t.test(malus)

 
