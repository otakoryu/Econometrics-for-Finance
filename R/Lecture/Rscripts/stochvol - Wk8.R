
# set.seed(2568)

library(stochvol)
data=readEViews("oil-exr-klci.wf1", as.data.frame = TRUE)
data = read.table("OIL-EXR-KLCI-D.txt", header=T) # Load the data

names(data)
ret <- logret(data$CRUDE, demean = TRUE)
par(mfrow = c(2, 1), mar = c(1.9, 1.9, 1.9, 0.5), mgp = c(2, 0.6, 0))
plot(data$CRUDE, type = "l", main = "Price of 1 MYR in USD")
plot(ret, type = "l", main = "Crude oil returns")

res <- svsample(ret, priormu = c(-10, 1), draws = 1000, burnin = 200,
      priorphi = c(20, 1.1), priorsigma = 0.1)

summary(res, showlatent = FALSE)

volplot(res, show0=TRUE, forecast = 100, 
        dates = data$daily[-1])

par(mfrow = c(3, 1))
paratraceplot(res)

par(mfrow = c(1, 3))
paradensplot(res, showobs = TRUE)

plot(res, forecast=100)

plot(resid(res), type="l")

res1 <- updatesummary(res, quantiles = c(0.5, 0.6), esspara = TRUE,
              esslatent = FALSE)
