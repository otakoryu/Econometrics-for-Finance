
par(mfrow=c(2,1))

arima.sim(list(order=c(2,0,0), ar=C(.85,.75), n=100))
pacf(arima.sim(list(order=c(1,0,0), ar=.85), n=100), ylab="x", main=(expression(AR(1)~ ~ ~ phi==+.85)))

plot(arima.sim(list(order=c(1,0,0), ar=-.85), n=100), ylab="x", main=(expression(AR(1)~ ~ ~ phi==-.85)))


plot(arima.sim(list(order=c(0,0,1), ma=.85), n=100), ylab="x", main=(expression(MA(1)~ ~ ~ phi==+.85)))

plot(arima.sim(list(order=c(0,0,1), ma=-.85), n=100), ylab="x", main=(expression(MA(1)~ ~ ~ phi==-.85)))


white.noise=as.ts(rnorm(400))
plot(white.noise)

random=as.ts(cumsum(white.noise))
plot(random)

require(tseries)
adf.test(random)
arima(random,order=c(1,0,0))

acf(random)
pacf(random)
