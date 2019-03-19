
## Lists
##it allows to extact specific data from output
list1 <- list(887,"abc", 5, list("ETW1102", "ETW1000", "ETW1010"))
vec <- c(7, 6, 3, 4, 10)
mat <- matrix(seq(5,10,1), ncol=2)
mat

mylist <- list(obj1=list1, obj2=vec, obj3=mat)
mylist
mylist$obj3
#$means that you'll extract specific columm data from the specific data fram

#str=structure
str(mylist)

mylist[[1]][[4]][[3]]
#return== 3rd character in the 4th list

mylist[[3]][,1]

mylist[[1]][[4]][[1]]

X<-data.frame(A=c(1,2,3),B=c("X","Y","Z"))
X
X$A
X$B
X$B[3]
X$A[2]
X$B[2]
X[2,2]
G<-list(c(1,2,3),c("X","Y","Z"),data.frame(A=c(1,2,3),B=c("X","Y","Z")))
G
G[1]
G[[3]]
G[3]
G[[3]][[1]][[2]]
