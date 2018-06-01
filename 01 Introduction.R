

          ###############################################
          # Statistical Machine Learning Summer Workshop
          # 06/01/2018
          # Introduction of R programming 
          # Instructor: Cen Wu, Guotao Chu and Fei Zhou
          # Department of Statistics, Kansas State University
          ##############################################


# Installation of R packages 
install.packages("Rcpp") 
library("Rcpp")

# Required packages for today's workship 
# Methond 1 
install.packages("Rcpp") 
install.packages("RcppArmadillo") 
install.packages("regnet") 
install.packages("igraph")
install.packages("gplots")
install.packages("leaps")
install.packages("corrplot")
install.packages("glmnet")
install.packages("ncvreg")
install.packages("e1071")

# Methond 2 
install.packages(c("RcppArmadillo" , "regnet", "igraph", "gplots", "leaps","corrplot","glmnet","ncvreg","e1071")) 


# Working Directory 
getwd()
setwd("C:/Users/Administrator/Desktop/workshop") 
getwd() 

# Load dataset into R 
data<-read.csv ("C:/Users/Administrator/Desktop/workshop/Cravens.csv",header=T,sep=",") 
data
head(data)
data$Sales
data[1,]
data[,2:4]

# Assign values to the variable 
x <- 9
x
print(x)

# Numeric operations 
(3*x/7 + 4)^3  
((3-x)^2 + 7)^(-1)


# Math functions   
y <- 11/5              
exp(-y)                  
sqrt(y)                   
abs(y)                   
log(y)  



# Case sensitive
Inf
inf
  
# Incomplete statements
(3.5 - y)^(-3
     )

# Two or more statement on same line 
one <- 10;  two <- 20 
one; two

# Logical operator 
x <- 3.2; y <- 8
x == y
x != y
x <= 7

# Various ways to define a vector 
x <- c(1,3,5,7,9)
y <- seq(from=0, to=10, length=5)
z <- seq(from=0, to=10, by=3) 
v <- 1:3
r <- rep(2,6)
x
y
z
v
r

# Attributes of vectors
x; class(x); length(x)
y; class(y); length(y)


# add, subtract, multiply, divide elementwise 
x + y
x - y
x * y
x / y


# Select certain elements of a vector 
x
x[2]             
x[c(2,4)]


# Omit certain elements of a vector 
x
x[-2]
x[-c(2,4)]

# Define a Matrix 
X<-matrix(1:12,nrow=3,ncol=4,byrow=T)
X

# Select certain elements of a Matrix 
X[2,]
X[,3]
X[3,2]

# Multiplication between a Matrix and a vector 
X<-X<-matrix(1:12,nrow=3,ncol=4,byrow=T)
X
b<- c(1,0,2,0)
b
X%*%b

# Save and load R object 
save(X, file="test.RData")
save.image("test1.RData") 
load("test1.RData") 



# R graphics 
x <- rnorm(1000)
y <- rnorm(1000) + x
hist(x, col="lightblue")
plot(x,y)
  
# R graphics 
x <- seq(-10,10,0.1)
y <- dnorm(x,0,1)
plot(x,y)


# Normal Density Curve 
x <- seq(-10,10,0.01)
y1 <- dnorm(x,0,1)
y2 <- dnorm(x,0,2)
y3 <- dnorm(x,0,3)
plot(x,y1,type="l",col="green",lwd=2)
points(x,y2,type="l",col="blue",lwd=2)
points(x,y3,type="l",col="red",lwd=2)

labels <- c("Mean=0, Std=1", "Mean=0, Std=2", "Mean=0, Std=3")
colors <- c("green", col="blue", col="red")
legend("topleft", inset=.05, title="Normal Density",cex=0.8,
  labels, lwd=2, lty=c(1, 1, 1), col=colors)

  
# Normal Density Curve 2 
x <- seq(-10,10,0.01)
y1 <- dnorm(x,-2,2)
y2 <- dnorm(x,0,2)
y3 <- dnorm(x,2,2)
plot(x,y1,type="l",col="green",lwd=2)
points(x,y2,type="l",col="blue",lwd=2)
points(x,y3,type="l",col="red",lwd=2)

labels <- c("Mean=-2, Std=2", "Mean=0, Std=2", "Mean=2, Std=2")
colors <- c("green", col="blue", col="red")
legend("topleft", title="Normal Density",inset=.05,
  labels, lwd=2, lty=c(1, 1, 1), col=colors)




