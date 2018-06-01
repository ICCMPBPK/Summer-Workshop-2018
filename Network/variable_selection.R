

          ###############################################
          # Statistical Machine Learning Summer Workshop
          # 06/01/2018
          # Network Analysis 
          # Instructor: Cen Wu, Guotao Chu and Fei Zhou
          # A special Thanks to Jie Ren 
          # Department of Statistics, Kansas State University
          ##############################################

# install.packages("Rcpp","RcppArmadillo","regnet") 

install.packages("Rcpp") 
install.packages("RcppArmadillo") 
install.packages("regnet") 

rm(list=ls(all=TRUE))

library("regnet")

setwd("C:/Users/wucen/Desktop/Workshop/Network")

load(file ="./SKCM_GENEs.RData")
load(file ="./SKCM_clinic.RData")
clc$gender = as.numeric(as.character(clc$gender)); head(clc)
X=as.matrix(cbind(clc[,-1], G)); X[1:5, 1:9]
hist(clc$breslow, main = "breslow", xlab="")
Y = log(clc$breslow); 
hist(Y, main = "log(breslow)", xlab="")


resp = "continuous"; 
clv = (1:2)    # we don't penalize age and gender
alpha.i = 0.5  # initialize coefficient vector using elastic net

################ Network #############################

b.rgn = regnet(X, Y, response=resp, penalty="network", lamb.1=0.06, lamb.2=10, clv =clv, alpha.i=alpha.i)
sum(b.rgn!=0)

round((b.rgn[b.rgn!=0]), digits = 5)

save(b.rgn, file = "./coeff_network.RData")

################ LASSO #############################
b.rgn = regnet(X, Y, response=resp, penalty="lasso", lamb.1=0.05, clv =clv, alpha.i=alpha.i)
sum(b.rgn!=0)

round((b.rgn[b.rgn!=0]), digits = 5)

as.numeric(b.rgn[1:6])

save(b.rgn, file = "./coeff_lasso.RData")
