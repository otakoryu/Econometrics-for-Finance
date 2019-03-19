
## To create the data frame use data.frame() function

options(max.print = 10000000)

units = c("ETW1000", "ETW2020", "ETW3410", "ETW1102", "ETW3420", "ACW1022")
names = c("Bus & Econ Stat", "Survey Methods", "Appl Econometrics", "Bus & Econ stat", 
          "Forecasting", "ACcounting")
Econometrics = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
df1 = data.frame(units, names, Econometrics) ##  stringsAsFactors = TRUE by default
str(df1)  

df2 = data.frame(units, names, Econometrics, stringsAsFactors = FALSE) ## stringsAsFactors = FALSE
str(df2)


my.data <- read.table("Losses.txt", header=T)
str(my.data)
nrow(my.data)
ncol(my.data)

my.data.1 <- my.data[1, ]
my.data.first20 <- my.data[1:20, ]
my.data[c(1,2,3,6), ]

my.data[c(1:5, 20:25, 27,29,30), ]

my.data[c(1:30), ]


library(dplyr)

my.data50to70 <- my.data %>% slice(50:55) ## slice() command under dplyr package 

my.data.rm.first20 <- my.data[-c(1:20), ] ## All of the rows except the row from 1 to 50. 

my.data.subset1 <- subset(my.data, select=c(MA180, MA60)) ## Using the subset command

my.data.subset2 <- subset(my.data, MA180 > 7) ## Using the subset command

attach(my.data)
my.data.subset3 <- subset(my.data$RW, RW > 100) ## Using the subset command
my.data.subset4 <- subset(my.data$RW, RW == 100) ## Using the subset command

## Dealing with the missing values

my.data1 <- read.table("R2LOG.crude.txt", header=T)
names(my.data1)
str(my.data1)
any(is.na(my.data1[]))
sum(is.na(my.data1[]))
sum(is.na(my.data1$Cons_sGARCH_M_ghyp))
sum(is.na(my.data1$ES180_Model))


missing.val = vector()   ## This loop returns the number of missing values in each column
for (i in seq(1,22,1)) {
  missing.val[i] = sum(is.na(my.data1[,i]))
print(missing.val[i])
  }

colSums(is.na(my.data1[]))  ## We can see the number of 
                            ## missing values in each column

my.data.clean1 = na.omit(my.data1) ## Omitting NAs
my.data.clean2 = my.data1[complete.cases(my.data1),] ## Alternative way of omitting
nrow(my.data1)
nrow(my.data.clean1)
nrow(my.data.clean2)



