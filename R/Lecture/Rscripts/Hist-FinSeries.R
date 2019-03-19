
library(quantmod)  # load the package "quantmod"
library(fBasics) #  <== Load the package "fBasics"

getSymbols("^KLSE", from="2005-01-01", to="2015-08-12") # format (YYYY-MM-DD)
# get daily KLSE stock data from Yahoo Finance

chartSeries(KLSE[,6]) # <== Column 6 of the object "" in R.

getSymbols("^DJI", from="2005-01-01", to="2015-08-08") # get daily KLSE stock data from Yahoo Finance

chartSeries(DJI[,6]) # <== Column 6 of the object "" in R.

dji_rets = diff(log(DJI[,6]))*100

plot(dji_rets, main="Dow Jones Industrial Average Returns", las=1)

plot(DJI[,6], main="Dow Jones Industrial Average Index", las=1)



basicStats(dji_rets)

# Some descriptives for generated data

mean(KLSE[,4])
sd(KLSE[,4])
median(KLSE[,4])
var(KLSE[,4])
sqrt(var(KLSE[,4]))
summary(KLSE[,4])


# hist() command creates the histogram

hist(KLSE[,4])

histinfo<-hist(KLSE[,4])
histinfo

# 1. Number of bins can be changed with breaks() option. 
# las=1 rotates y axis values

hist(KLSE[,4], ylab="freq", xlab="random data", 
     main="Histogram of KLSE", breaks=50, las=1)

# 2. Frequency or density using freq=FALSE option.

# With freq
hist(KLSE[,4], ylab="freq", xlab="Index values",     
     main="Histogram of KLSE", freq=TRUE, breaks=50, las=1)
# with density
hist(KLSE[,4], ylab="Density", xlab="random data", 
     main="Density plot", freq=FALSE, breaks=50, las=1)

# plot=FALSE option. If we do not want to plot the histogram we use 
# plot=FALSE option.

 hist1 <- hist(KLSE[,4], ylab="freq", xlab="random data", 
     main="Density plot", freq=FALSE, breaks=50, plot=FALSE)

# Following information can be retrieved

hist1$density
hist1$breaks
hist1$counts
hist1$mids

# 3. Further histogram improvement 
#
# We can make the histogram better looking by adjusting the x-axis, 
# y-axis, axis labels, title, and color with following arguments:
#
set.seed(986)
data <- rnorm(n=1500, m=5, sd=2.5)
#
hist(data, freq=FALSE, xlab="", ylab="", main="Distribution of data", 
     xlim=c(-3,14), ylim=c(0, 0.16), 
     col="lightblue") # xlim=c(-5,14),  ylim=c(0, 1)
#
# you may try col="lightred", col="lightgreen", col="darkblue", col="blue". 

# Normal curve in the histogram

# Finally, we can add a nice normal distribution curve to this plot using 
# the curve() function, in which we specify a normal density function with mean 
# and standard deviation that is equal to the mean and standard deviation 
# of generated data
#
set.seed(986)
data <- rnorm(n=1500, m=5, sd=2.5)
#
hist(data, freq=FALSE, xlab="", ylab="", main="Distribution of data", 
     xlim=c(-3,14), ylim=c(0, 0.16), 
     col="lightblue", las=1) # xlim=c(-5,14),  ylim=c(0, 1)
curve(dnorm(x, mean=mean(data), sd=sd(data)), add=TRUE, col="darkblue", lwd=2)
#
## Example with real data dcrude (log difference of crude oil). 

# We check whether the crude oil log prices are normal or not

hist(KLSE[,4], freq=FALSE, xlab="", ylab="", main="Distribution of data", 
     col="lightblue",  breaks=100, las=1) # xlim=c(-5,14),  ylim=c(0, 1), breaks=100, 

curve(dnorm(x, mean=mean(KLSE[,4]), sd=sd(KLSE[,4])), 
      add=TRUE, col="darkblue", lwd=2)

# We inspect whether the crude oil log price changes are normal or not


klse_rets <- diff(log(KLSE[,6]))*100
 dev.off()

 plot(klse_rets)
 
 hist(klse_rets, freq=FALSE, xlab="", ylab="", main="Distribution of data", 
          col="lightgreen",  breaks=75, las=1) # xlim=c(-5,14),  ylim=c(0, 1),  
                                                #breaks=100, 
curve(dnorm(x, mean=mean(klse_rets), sd=sd(klse_rets)), 
     add=TRUE, col="darkblue", lwd=2)

curve(dnorm(x, mean=mean(klse_rets), sd=sd(klse_rets)), add=TRUE, col="darkblue", lwd=2)


hist(klse_rets,nclass=40) # <== Obtain histogram of IBM simple returns.
d1=density(klse_rets) #  <== Compute density function of ibm log returns
names(d1) #  <== Find out the output variables of the command "density".
plot(d1$x,d1$y,type='l')  #  <== Plot the sample density of log returns
mu=mean(libm); s1 = sd(libm) # <== compute the sample mean and standard deviation of IBM log returns.
x=seq(-0.4,0.4,0.01) # <=== create a sequence of real numbers from -0.4 to 0.4 with increment 0.01.
y=dnorm(x,mean=mu,sd=s1) # <=== obtain normal density with mean mu and standard deviation s1.
lines(x,y,lty=2) # <== impose a dashed line on the density plot for comparison with normal density.
# <== you can also use different colors in the plot. For example,
lines(x,y,col="red") # will plot a red curve.




