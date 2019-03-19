
names()
#you could check the name of varibales
names(crude.data) 
names(rice.series)


##loop
portest1<-list()
for(i in c(5,10,15,25,30)){
  portest1[[i]]<-print(Box.test(whitenoise[(i):(400-i)],lag=c(10),type=c("Ljung-Box")))
}

##[5:405],[10:410],[15:415],[20:420],[25:425],[30:435]


