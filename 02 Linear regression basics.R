
          ###############################################
          # Statistical Machine Learning Summer Workshop
          # 06/01/2018
          # LASSO and Extensions 
          # Instructor: Cen Wu, Guotao Chu and Fei Zhou
          # Department of Statistics, Kansas State University
          ##############################################

rm(list=ls(all=TRUE))
setwd("C:\\Users\\Fei\\Dropbox\\Wu group\\Machine Learning workshop")

load("data.RData") 
y<-data$Sales
x<-data[,2:9]
x<-data.matrix(x)

res <- cor(data)
round(res,2)
library(corrplot)
corrplot(res, type = "upper", 
         tl.col = "black", tl.srt = 45)
plot(x[,1],y,xlab = "Accounts",ylab = "Sales",xlim = c(10,270),ylim = c(255,7000),las=1,pch=16,cex.lab=1.5,col="purple")
fit1 <- lm(Sales~Accounts,data=data)
abline(fit1,col="red",lwd=2)
fit1$coefficients
abline(-1000,40,col="green",lwd=2)
abline(2000,10,col="blue",lwd=2)

pairs(data, pch = 19,cex.labels=1.5,col="purple")

## variable selection

fit0 <- lm(Sales~1,data=data) #fit the null model with intercept only

full <- lm(Sales~.,data=data) #fit the full model

## forward selection
step(fit0, scope=list(lower=fit0, upper=full), direction="forward")

## backward elimination
step(full, data=data, direction="backward")

## stepwise regression
step(fit0, scope = list(upper=full), data=data, direction="both")

##Best subset
library(leaps)
regfit.full = regsubsets(Sales~.,data=data)
coef(regfit.full ,6)


