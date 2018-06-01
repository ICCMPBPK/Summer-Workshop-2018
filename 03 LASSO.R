

          ###############################################
          # Statistical Machine Learning Summer Workshop
          # 06/01/2018
          # LASSO and Extensions 
          # Instructor: Cen Wu, Guotao Chu and Fei Zhou
          # Department of Statistics, Kansas State University
          ##############################################


          # a simulated examples on LASSO and Ridge 
          # The glmnet package is from: 
          # Friedman, J., Hastie, T. and Tibshirani, R. (2010). Regularization paths 
          # for generalized linear models via coordinate descent. Journal of statistical software, 33(1)

          rm(list=ls(all=TRUE))
          library(glmnet)
          library(MASS)

          n = 200; p = 300;      
          sig = matrix(0,p,p)    

          for (i in 1: p)
             {
                   for (j in 1: p)
                    {
                      sig[i,j] = 0.5^abs(i-j)
                    }           

            }

          x = mvrnorm(n,rep(0,p),sig)
          error = rnorm(n,0,1)   
          y = 2.5*x[,3]+5*x[,10] + error 

          lasso.cv <- cv.glmnet(x,y,alpha=1,nfolds=5)   
          alpha <- lasso.cv$lambda.min  # lambda in the notes
          # alpha <- 0.5
          lasso.fit <- glmnet(x,y,family="gaussian",alpha=1,nlambda=100)
          coef0 <- as.vector(predict(lasso.fit, s=alpha, type="coefficients"))[-1]
          fix(coef0)

          # The solution path 
          y = -1.5*x[,1]+2.5*x[,3]+5*x[,10]-2*x[,12] + error 
          lasso.cv <- cv.glmnet(x,y,alpha=1,nfolds=5)   
          alpha <- lasso.cv$lambda.min  # lambda in the notes
          lasso.fit <- glmnet(x,y,family="gaussian",alpha=1,nlambda=100)
          coef0 <- as.vector(predict(lasso.fit, s=alpha, type="coefficients"))[-1]
          plot(lasso.fit)
          plot(lasso.fit, xvar = "lambda", label = TRUE)
          plot(lasso.fit, xvar = "dev", label = TRUE)
          plot(lasso.fit, xvar = "norm", label = TRUE) 


          # The ncvreg package is from: 
          # Breheny, P. and Huang, J. (2011) Coordinate descent algorithms for nonconvex penalized regression, with 
          # applications to biological feature selection. The Annals of Applied Statistics, 5(1), p.232.

          library(ncvreg)
          lasso.fit2 <- ncvreg(x,y,penalty=c("lasso"))
          plot(lasso.fit2)


          # Compare LASSO, SCAD and MCP

          rm(list=ls(all=TRUE))
          library(glmnet)
          library(ncvreg)
          library(MASS)

          n = 200; p = 300;      
          sig = matrix(0,p,p)    

          for (i in 1: p)
             {
                   for (j in 1: p)
                    {
                      sig[i,j] = 0.5^abs(i-j)
                    }           

            }

          x = mvrnorm(n,rep(0,p),sig)
          error = rnorm(n,0,1)   
          beta = matrix(c(-1.5,0,2.5,0,0,5,0,-2,rep(0, p-8)))
          y = -1.5*x[,1]+2.5*x[,3]+5*x[,6]-2*x[,8] + error 

          # LASSO
          lasso.cv <- cv.glmnet(x,y,alpha=1,nfolds=5)   
          alpha <- lasso.cv$lambda.min  # lambda in the notes
          lasso.fit <- glmnet(x,y,family="gaussian",alpha=1,nlambda=100)
          coef.lasso <- as.vector(predict(lasso.fit, s=alpha, type="coefficients"))[-1]
          # fix(coef.lasso)
          # sum(coef.lasso!=0)
          sum((coef.lasso-beta)^2)

          # SCAD
          scad.fit = cv.ncvreg(x, y, penalty="SCAD", gamma = 3.7)
          coef.scad = scad.fit$fit$beta[, scad.fit$min][-1]
          plot(scad.fit)
          # fix(coef.scad)
          sum((coef.scad-beta)^2)


          # MCP
          mcp.fit = cv.ncvreg(x, y, penalty="MCP", gamma = 2.5)
          coef.mcp = mcp.fit$fit$beta[, mcp.fit$min][-1]
          plot(mcp.fit)
          # fix(coef.mcp)
          sum((coef.mcp-beta)^2)


          ## motivation of elastic net  
          ## when predictors are highly correlated

          set.seed(100)
          n = 200
          sig = matrix(c(1,0.999,0,0,0.999,1,0,0,0,0,1,0,0,0,0,1),4,4,byrow=T)


          x = mvrnorm(n,rep(0,4),sig)
          error = rnorm(n,0,1)   
          y = 1.0*x[,1]+1.0*x[,2] + error 
          #y = 1.0*x[,1]+1.0*x[,3] + error 


          # LASSO only chooses one of the predictors while pushing all effects on that one

          lasso.cv <- cv.glmnet(x,y,alpha=1,nfolds=5)   
          alpha <- lasso.cv$lambda.min
          lasso.fit <- glmnet(x,y,family="gaussian",alpha=1,nlambda=100)
          coef.lasso <- as.vector(predict(lasso.fit, s=alpha, type="coefficients"))[-1]
          coef.lasso 

          # a potential solution

          elanet.cv <- cv.glmnet(x,y,alpha=0.5,nfolds=5)   
          alpha <- elanet.cv$lambda.min
          elanet.fit <- glmnet(x,y,family="gaussian",alpha=0.5,nlambda=100)
          coef.elanet <- as.vector(predict(elanet.fit, s=alpha, type="coefficients"))[-1]
          coef.elanet 



