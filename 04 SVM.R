
          ###############################################
          # Statistical Machine Learning Summer Workshop
          # 06/01/2018
          # Support Vector Machine
          # Instructor: Cen Wu, Guotao Chu and Fei Zhou
          # Department of Statistics, Kansas State University
          ##############################################

          install.packages("e1071")
          library(e1071)

          set.seed(10)

          n <- 8 # number of data points for each class
          p <- 2 # dimension 

          # Generate data points with binary (+1/-1) class labels 
          xc1 <- matrix(rnorm(n*p,mean=0,sd=1),n,p)
          xc2 <- matrix(rnorm(n*p,mean=3,sd=1),n,p)
          x <- rbind(xc1,xc2)

          # Code the response as a categorial variable to perform SVM-based classification
          y <- matrix(as.factor(c(rep(1,n),rep(-1,n))))
          dat <- data.frame(x=x,y=y)

          # Data Visualation

          plot(x,col=ifelse(y>0,"purple", "green"), pch = 19, cex = 1.2, lwd = 2, xlab = "x1", ylab = "x2", cex.lab = 1.3)
          legend("bottomleft", c("+1","-1"),col=c("purple", "green"),pch=c(19, 19),text.col=c("purple", "green"), cex = 1)


          # For separable case, large C leads to exact solution
          # For non-separable case, large C leads to over-fitted SVM
          # For fixed cost:
          svm.fit <- svm(y ~ ., data = dat, type='C-classification', kernel='linear',scale=FALSE, cost = 1000)

          summary(svm.fit)
          svm.fit$index
          # The 1st(2nd) feature is ploted on y-axis (x-axis)
          w <- t(svm.fit$coefs) %*% svm.fit$SV
          b <- -svm.fit$rho
          # b <- -(max(x[y == -1, ] %*% t(w)) + min(x[y == 1, ] %*% t(w)))/2

          abline(a= -b/w[1,2], b=-w[1,1]/w[1,2], col="black", lty=1, lwd = 2)

          # some sub-optimal hyperplanes
          # abline(a= -(b-0.2)/w[1,2], b=-(w[1,1] +0.2)/w[1,2], col="black", lty=1, lwd = 2)
          # abline(a= -(b+0.1)/w[1,2], b=-(w[1,1] -0.4)/w[1,2], col="black", lty=1, lwd = 2)

          abline(a= (-b-1)/w[1,2], b=-w[1,1]/w[1,2], col="black", lty=3, lwd = 2.5)
          abline(a= (-b+1)/w[1,2], b=-w[1,1]/w[1,2], col="black", lty=3, lwd = 2.5)
          points(x[svm.fit$index, ], col="black", cex=3) 

         

          # non-separable case

          set.seed(50)

          n <- 10 # number of data points for each class
          p <- 2 # dimension

          # Generate data points with binary (+1/-1) class labels 
          xc1 <- matrix(rnorm(n*p,mean=0,sd=1),n,p)
          xc2 <- matrix(rnorm(n*p,mean=1.5,sd=1),n,p)
          x <- rbind(xc1,xc2)
          y <- matrix(as.factor(c(rep(1,n),rep(-1,n))))

          # Data Visualation

          plot(x,col=ifelse(y>0,"purple", "green"), pch = 19, cex = 1.2, lwd = 2, xlab = "x1", ylab = "x2", cex.lab = 1.5)
          legend("topright", c("+1","-1"),col=c("purple", "green"),pch=c(19, 19),text.col=c("purple", "green"), cex = 1.5)


          # Now experiment with different amount of costs. What does it imply?   
          svm.fit <- svm(y ~ ., data = data.frame(x, y), type='C-classification', kernel='linear',scale=FALSE, cost = 0.1) # cost =0.1

          w <- t(svm.fit$coefs) %*% svm.fit$SV
          b <- -svm.fit$rho

          abline(a= -b/w[1,2], b=-w[1,1]/w[1,2], col="black", lty=1, lwd = 2)
          abline(a= (-b-1)/w[1,2], b=-w[1,1]/w[1,2], col="black", lty=3, lwd = 2)
          abline(a= (-b+1)/w[1,2], b=-w[1,1]/w[1,2], col="black", lty=3, lwd = 2)
          points(x[svm.fit$index, ], col="black", cex=3) 































