
 library(forecast)
 oils <- read.delim("oils.txt", header=TRUE) # Load the "oils" data.
 attach(oils)
 head(oils); tail(oils);
 names(oils);
 palm <- ts(palm); plot(palm);


 ###### Log Returns or log changes ######

 palm.rets <- 100*diff(palm, lags=1); palm.rets <- as.ts(palm.rets) # Need to change into ts class 
 plot(palm.rets)
 
 ###### Estimate AR(1) model #######

 ar_palm.rets <- arima(palm.rets, order=c(1,0,0), method="ML", optim.method="BFGS")
 ar_foc<-forecast(ar_palm.rets,h=100)
 plot(ar_foc)
 ## Output
 
 ar_palm.rets
 summary(ar_palm.rets) # Output will be printed
 print(ar_palm.rets)  # Output will be printed
 tsdiag(ar_palm.rets)
 ## ACF of residuals
 
 acf(ar_palm.rets$residuals)
 
 AIC(ar_palm.rets)    # AIC is returned 
 
 library(portes);     # portes library is uploaded to check for remaining serial correlation
 
 LjungBox(ar_palm.rets, lags=c(5,10), SquaredQ=FALSE)
 
 ### Characteristic Roots are the inverse of polynomial roots
 
 polyroot(c(1,-0.3235))
 
 ### Inverse of this root is
 
 1/polyroot(c(1,-0.3235))
 
 
 ####### AR(1) model for Daily crude oil prices #####
  
 crude_daily_rets <- read.csv(file.choose(), header=TRUE) # Choose file crude_daily.csv
 
 
 attach(crude_daily_rets)
 head(crudedaily.rets); tail(crudedaily.rets);
 names(crudedaily.rets);
 crudedaily.rets <- as.ts(crudedaily.rets);
 plot(crudedaily.rets, col="red")

 acf(crudedaily.rets, lag.max=50)
 pacf(crudedaily.rets)

 arimacrudedailyrets <- arima(crudedaily.rets, order=c(1,0,0), method="ML", optim.method="BFGS")

 arimacrudedailyrets
 tsdiag(arimacrudedailyrets)
 
 # Polynomial root
 
 polyroot(c(1, 0.0184))
  
 # Inverse of this root is  
 
 1/polyroot(c(1, 0.0184))
 
 
###### Estimate MA(1) model #######
 
 ma_palm.rets <- arima(palm.rets, order=c(0,0,1), method="ML", optim.method="BFGS")
 
 ## Output
 
 ma_palm.rets
 summary(ma_palm.rets) # Output will be printed
 print(ma_palm.rets)  # Output will be printed
 
 ## ACF of residuals
 
 acf(ma_palm.rets$residuals)
 
 AIC(ma_palm.rets)    # AIC is returned 
 
 library(portes);     # portes library is uploaded to check for remaining serial correlation
 
 LjungBox(ma_palm.rets, lags=c(5,10), SquaredQ=FALSE)
 
 
 ######## Compute AR roots using the loops ##########
 
 arroots <- function(object)
 {
   if(class(object) != "Arima" & class(object) != "ar")
     stop("object must be of class Arima or ar")
   if(class(object) == "Arima")
     parvec <- object$model$phi
   else
     parvec <- object$ar
   if(length(parvec) > 0)
   {
     last.nonzero <- max(which(abs(parvec) > 1e-08))
     if (last.nonzero > 0)
       return(structure(list(roots=polyroot(c(1,-parvec[1:last.nonzero])),
                             type="AR"), class='armaroots'))
   }
   return(structure(list(roots=numeric(0),type="AR"),class='armaroots'))
 }
 
 
 
 ######### Compute MA roots ###########
 
 maroots <- function(object)
 {
   if(class(object) != "Arima")
     stop("object must be of class Arima")
   parvec <- object$model$theta
   if(length(parvec) > 0)
   {
     last.nonzero <- max(which(abs(parvec) > 1e-08))
     if (last.nonzero > 0)
       return(structure(list(roots=polyroot(c(1,parvec[1:last.nonzero])),
                             type="MA"), class='armaroots'))
   }
   return(structure(list(roots=numeric(0),type="MA"),class='armaroots'))
 }
 
 plot.armaroots <- function(x, xlab="Real",ylab="Imaginary",
                            main=paste("Inverse roots of",x$type,"characteristic polynomial"),
                            ...)
 {
   oldpar <- par(pty='s')
   on.exit(par(oldpar))
   plot(c(-1,1),c(-1,1),xlab=xlab,ylab=ylab,
        type="n",bty="n",xaxt="n",yaxt="n", main=main, ...)
   axis(1,at=c(-1,0,1),line=0.5,tck=-0.025)
   axis(2,at=c(-1,0,1),label=c("-i","0","i"),line=0.5,tck=-0.025)
   circx <- seq(-1,1,l=501)
   circy <- sqrt(1-circx^2)
   lines(c(circx,circx),c(circy,-circy),col='gray')
   lines(c(-2,2),c(0,0),col='gray') 
   lines(c(0,0),c(-2,2),col='gray')
   if(length(x$roots) > 0) {
     inside <- abs(x$roots) > 1
     points(1/x$roots[inside],pch=19,col='black')
     if(sum(!inside) > 0)
       points(1/x$roots[!inside],pch=19,col='red')
   }
   }
 
 
   library(forecast)
   crude_daily_rets <- read.csv(file.choose(), header=TRUE) # Choose file crude_daily.csv
   attach(crude_daily_rets)
   head(crudedaily.rets); tail(crudedaily.rets);
   names(crudedaily.rets);
   crudedaily.rets <- as.ts(crudedaily.rets);
   # AR Roots
   plot(arroots(Arima(crude_daily_rets$crudedaily.rets,c(1,0,0))))
   # MA Roots
   plot(maroots(Arima(crudedaily.rets,c(0,0,1))))
   
 