
## write.csv(); write.table() functions

library(quantmod)  # load the package "quantmod"
library(fBasics)   #  Load the package "fBasics"

getSymbols("^KLSE", from="2005-01-01", to="2018-08-08") # format (YYYY-MM-DD)
# retrieve daily KLSE stock data from Yahoo Finance

#specify the exact collum that you want extract from data 
write.csv(KLSE[,c(1,4,6)], file="KLSE.csv")

## write.table()

na.omit(KLSE)

ACF.KLSE = acf(KLSE[,c(1,4,6)], plot=F, na.action = na.pass)

write.table(ACF.KLSE[[1]], file="ACF.KLSE.csv")

## capture.output() and cat() functions
#we could save output of list format

ACF.KLSE.output <- capture.output(ACF.KLSE)
cat(ACF.KLSE.output, file="ACF.KLSE.outpu.txt", sep="\n", append=TRUE)


## Saving the output obtained through the loop

ACF.KLSE <- list()
ACF.KLSE.output <- list()

#by looping fuction, from 1-10, It will calculate acf of 101-110 of lag
for (k in 1:10) {
ACF.KLSE[[k]] = acf(KLSE[,4][k:(100+k)], lag.max = 8, plot=F, na.action = na.pass)
ACF.KLSE.output[[k]] <- capture.output(ACF.KLSE[[k]])
cat(ACF.KLSE.output[[k]], file="ACF.KLSE.output.txt", sep="\n", append=T)
}

