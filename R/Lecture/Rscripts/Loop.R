
## Example 1

for (k in c(1,2,3)) {
  print(k)
}

## Example 2
units <- list()
for (i in c("ETW1000", "ETW2410", "ETW3481")) {
  units[i] <- print(i)
}


## Example 3

smpl <- list()
for (i in seq(10,200,5)){
smpl[[i]] <- rnorm(i, 0, 1)
print(smpl[[i]])
print(i)
# plot(smpl[[i]], type="l")  
hist(smpl[[i]])
}

plot(smpl[[110]], type="l")  

## Example 4

losses = read.table("Losses.txt", header=T)
for (i in 1:22) {
  print(data.frame(mean(losses[,i]), sd(losses[,i]) ))
} 

## Example 5

library(fBasics)
losses = read.table("Losses.txt", header=T)
for (i in 1:22) {
  print(basicStats(losses[,i]))
}

## Example 6

library(quantmod)  # load the package "quantmod"
ibrary(fBasics)  # load the package "fBasics"
getSymbols("MSFT", from="2005-01-03", to="2017-08-07") # format (YYYY-MM-DD)
# get daily Microsoft stock data from Yahoo Finance
mean.data <- vector()
for (i in 1:6) {
  mean.data[[i]] <- apply(MSFT[,i],2,basicStats)
}
mean.data[[3]]


