# This code replicates key results in the paper 
# "Decision making and uncertainty quantification
# for individualized treatments
# using Bayesian Additive Regression Trees"
# Logan et al 2019

# The code evaluates BART-based individualized treatment rules (ITRs)
# across various simulation settings, replicating Figures 2 and 3.
library(BART)
library(speff2trial)

#----  Figure 3 Evaluation of Estimation----

#---- Replicating Fig3 single small training set----
set.seed(1)
n_train=500; n_test=2000

p <- 3  # no of covariates
X_train <- matrix(runif(n_train * p,-1.5,1.5), ncol = p)  # X1, X2, X3 ~ Unif(-1.5,1.5)
A_train <- rbinom(n_train, 1, 0.5)  #  A ~ Bernoulli(0.5)

p_Y = 1 / (
  1 + exp(
    -0.1 - 0.2 * X_train[,1] + 0.2 * X_train[,2] - 0.1 * X_train[,3] + 0.5 * X_train[,1]^2 +
      A_train * (-0.5 - 0.5 * X_train[,1] - X_train[,2] - 0.3 * as.numeric(X_train[,3] > 0.5) + 0.5 * X_train[,1]^2)
  )
) #p(Y=1|A,X)

Y_train <- rbinom(n_train, 1, p_Y)  #  Y ~ Bernoulli(p_Y)

# train BART ITR
bart_model <- pbart(x.train = cbind(X_train, A_train), 
                    y.train = Y_train,                 
                    sparse = FALSE)                    

# generate test data
X_test <- matrix(runif(n_test * p,-1.5,1.5), ncol = p) # X1, X2, X3 ~ Unif(-1.5,1.5)

p_Y_A1 = 1 / (
  1 + exp(
    -0.1 - 0.2 * X_test[,1] + 0.2 * X_test[,2] - 0.1 * X_test[,3] + 0.5 * X_test[,1]^2 +
      1 * (-0.5 - 0.5 * X_test[,1] - X_test[,2] - 0.3 * as.numeric(X_test[,3] > 0.5) + 0.5 * X_test[,1]^2)
  )
) #p(Y=1|A,X)

p_Y_A0 = 1 / (
  1 + exp(
    -0.1 - 0.2 * X_test[,1] + 0.2 * X_test[,2] - 0.1 * X_test[,3] + 0.5 * X_test[,1]^2 
  )
) #p(Y=1|A,X)


pred_A0 <- predict(bart_model, newdata = cbind(X_test, 0))$prob.test.mean  # prediction when A=0
pred_A1 <- predict(bart_model, newdata = cbind(X_test, 1))$prob.test.mean  # prediction when A=1


plot(p_Y_A1,pred_A1,xlim = c(0,1),ylim = c(0,1),pch=16,xlab="True outcome for trt1",ylab="Posterior mean for trt1");abline(a=0,b=1)
plot(p_Y_A0,pred_A0,xlim = c(0,1),ylim = c(0,1),pch=16,xlab="True outcome for trt0",ylab="Posterior mean for trt0");abline(a=0,b=1)
plot(p_Y_A1-p_Y_A0, pred_A1-pred_A0,xlim = c(-0.5,0.5),ylim = c(-0.5,0.5),pch=16,xlab="True trt difference",ylab="Posterior mean trt.diff.");abline(a=0,b=1)


# ----Replicating Fig3g 400 small training sets----
set.seed(1)
n_train=500; n_test=2000

p <- 3  # no of covariates

Result_3g<-matrix(NA,nrow = 2000,ncol=400)


# generate test data
X_test <- matrix(runif(n_test * p,-1.5,1.5), ncol = p) # X1, X2, X3 ~ Unif(-1.5,1.5)

p_Y_A1 = 1 / (
  1 + exp(
    -0.1 - 0.2 * X_test[,1] + 0.2 * X_test[,2] - 0.1 * X_test[,3] + 0.5 * X_test[,1]^2 +
      1 * (-0.5 - 0.5 * X_test[,1] - X_test[,2] - 0.3 * as.numeric(X_test[,3] > 0.5) + 0.5 * X_test[,1]^2)
  )
) #p(Y=1|A,X)

p_Y_A0 = 1 / (
  1 + exp(
    -0.1 - 0.2 * X_test[,1] + 0.2 * X_test[,2] - 0.1 * X_test[,3] + 0.5 * X_test[,1]^2 
  )
) #p(Y=1|A,X)


for (v in 1:400){
  X_train <- matrix(runif(n_train * p,-1.5,1.5), ncol = p)  # X1, X2, X3 ~ Unif(-1.5,1.5)
  A_train <- rbinom(n_train, 1, 0.5)  #  A ~ Bernoulli(0.5)
  
  p_Y = 1 / (
    1 + exp(
      -0.1 - 0.2 * X_train[,1] + 0.2 * X_train[,2] - 0.1 * X_train[,3] + 0.5 * X_train[,1]^2 +
        A_train * (-0.5 - 0.5 * X_train[,1] - X_train[,2] - 0.3 * as.numeric(X_train[,3] > 0.5) + 0.5 * X_train[,1]^2)
    )
  ) #p(Y=1|A,X)
  
  Y_train <- rbinom(n_train, 1, p_Y)  #  Y ~ Bernoulli(p_Y)
  
  # train BART ITR
  bart_model <- pbart(x.train = cbind(X_train, A_train), 
                      y.train = Y_train,                 
                      sparse = FALSE)   
  pred_A0 <- predict(bart_model, newdata = cbind(X_test, 0))$prob.test.mean  # prediction when A=0
  pred_A1 <- predict(bart_model, newdata = cbind(X_test, 1))$prob.test.mean  # prediction when A=1
  
  Result_3g[,v]<-pred_A1-pred_A0
  
}

plot(p_Y_A1-p_Y_A0, rowMeans(Result_3g),xlim = c(-0.5,0.5),ylim = c(-0.5,0.5),pch=16,xlab="True trt difference",ylab="Posterior mean trt.diff.");abline(a=0,b=1)


# ----Replicating Fig3bdf single large training set----
set.seed(1)
n_train=5000; n_test=2000

p <- 3  # no of covariates
X_train <- matrix(runif(n_train * p,-1.5,1.5), ncol = p)  # X1, X2, X3 ~ Unif(-1.5,1.5)
A_train <- rbinom(n_train, 1, 0.5)  #  A ~ Bernoulli(0.5)

p_Y = 1 / (
  1 + exp(
    -0.1 - 0.2 * X_train[,1] + 0.2 * X_train[,2] - 0.1 * X_train[,3] + 0.5 * X_train[,1]^2 +
      A_train * (-0.5 - 0.5 * X_train[,1] - X_train[,2] - 0.3 * as.numeric(X_train[,3] > 0.5) + 0.5 * X_train[,1]^2)
  )
) #p(Y=1|A,X)

Y_train <- rbinom(n_train, 1, p_Y)  #  Y ~ Bernoulli(p_Y)

# train BART ITR
bart_model <- pbart(x.train = cbind(X_train, A_train), 
                    y.train = Y_train,                 
                    sparse = FALSE)                    

# generate test data
X_test <- matrix(runif(n_test * p,-1.5,1.5), ncol = p) # X1, X2, X3 ~ Unif(-1.5,1.5)

p_Y_A1 = 1 / (
  1 + exp(
    -0.1 - 0.2 * X_test[,1] + 0.2 * X_test[,2] - 0.1 * X_test[,3] + 0.5 * X_test[,1]^2 +
      1 * (-0.5 - 0.5 * X_test[,1] - X_test[,2] - 0.3 * as.numeric(X_test[,3] > 0.5) + 0.5 * X_test[,1]^2)
  )
) #p(Y=1|A,X)

p_Y_A0 = 1 / (
  1 + exp(
    -0.1 - 0.2 * X_test[,1] + 0.2 * X_test[,2] - 0.1 * X_test[,3] + 0.5 * X_test[,1]^2 
  )
) #p(Y=1|A,X)


pred_A0 <- predict(bart_model, newdata = cbind(X_test, 0))$prob.test.mean  # prediction when A=0
pred_A1 <- predict(bart_model, newdata = cbind(X_test, 1))$prob.test.mean  # prediction when A=1


plot(p_Y_A1,pred_A1,xlim = c(0,1),ylim = c(0,1),pch=16,xlab="True outcome for trt1",ylab="Posterior mean for trt1");abline(a=0,b=1)
plot(p_Y_A0,pred_A0,xlim = c(0,1),ylim = c(0,1),pch=16,xlab="True outcome for trt0",ylab="Posterior mean for trt0");abline(a=0,b=1)
plot(p_Y_A1-p_Y_A0, pred_A1-pred_A0,xlim = c(-0.5,0.5),ylim = c(-0.5,0.5),pch=16,xlab="True trt difference",ylab="Posterior mean trt.diff.");abline(a=0,b=1)



# ----Replicating Fig3h 400 large training sets ----
set.seed(1)
n_train=5000; n_test=2000

p <- 3  # no of covariates

Result_3h<-matrix(NA,nrow = 2000,ncol=400)


# generate test data
X_test <- matrix(runif(n_test * p,-1.5,1.5), ncol = p) # X1, X2, X3 ~ Unif(-1.5,1.5)

p_Y_A1 = 1 / (
  1 + exp(
    -0.1 - 0.2 * X_test[,1] + 0.2 * X_test[,2] - 0.1 * X_test[,3] + 0.5 * X_test[,1]^2 +
      1 * (-0.5 - 0.5 * X_test[,1] - X_test[,2] - 0.3 * as.numeric(X_test[,3] > 0.5) + 0.5 * X_test[,1]^2)
  )
) #p(Y=1|A,X)

p_Y_A0 = 1 / (
  1 + exp(
    -0.1 - 0.2 * X_test[,1] + 0.2 * X_test[,2] - 0.1 * X_test[,3] + 0.5 * X_test[,1]^2 
  )
) #p(Y=1|A,X)


for (v in 1:400){
  X_train <- matrix(runif(n_train * p,-1.5,1.5), ncol = p)  # X1, X2, X3 ~ Unif(-1.5,1.5)
  A_train <- rbinom(n_train, 1, 0.5)  #  A ~ Bernoulli(0.5)
  
  p_Y = 1 / (
    1 + exp(
      -0.1 - 0.2 * X_train[,1] + 0.2 * X_train[,2] - 0.1 * X_train[,3] + 0.5 * X_train[,1]^2 +
        A_train * (-0.5 - 0.5 * X_train[,1] - X_train[,2] - 0.3 * as.numeric(X_train[,3] > 0.5) + 0.5 * X_train[,1]^2)
    )
  ) #p(Y=1|A,X)
  
  Y_train <- rbinom(n_train, 1, p_Y)  #  Y ~ Bernoulli(p_Y)
  
  # train BART ITR
  bart_model <- pbart(x.train = cbind(X_train, A_train), 
                      y.train = Y_train,                 
                      sparse = FALSE)   
  pred_A0 <- predict(bart_model, newdata = cbind(X_test, 0))$prob.test.mean  # prediction when A=0
  pred_A1 <- predict(bart_model, newdata = cbind(X_test, 1))$prob.test.mean  # prediction when A=1
  
  Result_3h[,v]<-pred_A1-pred_A0
  
}

plot(p_Y_A1-p_Y_A0, rowMeans(Result_3h),xlim = c(-0.5,0.5),ylim = c(-0.5,0.5),pch=16,xlab="True trt difference",ylab="Posterior mean trt.diff.");abline(a=0,b=1)


# ---- Replicate Fig2 Comparison----
#setup
n_train <- 500
n_test <- 2000

m_values <- c(80, 200)
k_values <- c(0.2, 0.8, 2.0)

Result_K<-data.frame(BARTd=rep(NA,7),BARTcv=rep(NA,7))

#helper function to make folds
nfold <- 5  
make_folds <- function(n, k) {
  sample(rep(1:k, length.out = n))
}

# ----K1-BARTd----
set.seed(1)  
p <- 3  # no of covariates
# generate test set
X_test <- matrix(rnorm(n_test * p), ncol = p)    
p_Y_A0 <- 1-plogis(0.3 + 0.2 * X_test[,1] - 0.2 * X_test[,2] - 0.2 * X_test[,3])
p_Y_A1 <- 1-plogis(0.3 + 0.2 * X_test[,1] - 0.2 * X_test[,2] - 0.2 * X_test[,3] + 
                     1 * (-0.1 - 2 * X_test[,1] - 0.7 * X_test[,2] - 0.1 * X_test[,3]))
Val_opt <-mean(pmax(p_Y_A0,p_Y_A1))

Val_BARTd<-numeric(50)
for(v in 1:50){
  
  X_train <- matrix(rnorm(n_train * p), ncol = p)  # X1, X2, X3 ~ N(0,1)
  A_train <- rbinom(n_train, 1, 0.5)  #  A ~ Bernoulli(0.5)
  
  #K1 formula
  p_Y <- 1-plogis(0.3 + 0.2 * X_train[,1] - 0.2 * X_train[,2] - 0.2 * X_train[,3] + 
                    A_train * (-0.1 - 2 * X_train[,1] - 0.7 * X_train[,2] - 0.1 * X_train[,3]))
  
  Y_train <- rbinom(n_train, 1, p_Y)  #  Y ~ Bernoulli(p_Y)
  
  # train BART ITR
  bart_model <- pbart(x.train = cbind(X_train, A_train), 
                      y.train = Y_train,                 
                      sparse = FALSE)                    
  
  
  # BART prediction
  pred_A0 <- predict(bart_model, newdata = cbind(X_test, 0))$prob.test.mean  # A=0 prediction
  pred_A1 <- predict(bart_model, newdata = cbind(X_test, 1))$prob.test.mean  # A=1 prediction
  #value function
  Val_BARTd[v]<-mean(pmax(pred_A1,pred_A0))
}

Result_K$BARTd[1]<-mean(Val_BARTd)/Val_opt

# ---- K1-BARTcv----


Val_BARTcv <- numeric(50)

for(v in 1:50){
  
  X_train <- matrix(rnorm(n_train * p), ncol = p)
  A_train <- rbinom(n_train, 1, 0.5)
  
  
  p_Y <- 1 - plogis(0.3 + 0.2 * X_train[,1] - 0.2 * X_train[,2] - 0.2 * X_train[,3] + 
                      A_train * (-0.1 - 2 * X_train[,1] - 0.7 * X_train[,2] - 0.1 * X_train[,3]))
  
  Y_train <- rbinom(n_train, 1, p_Y)
  
  
  folds <- make_folds(n_train, nfold)
  
  best_m  <- NA
  best_k  <- NA
  best_cv <- -Inf  
  
  
  for(m_val in m_values){
    for(k_val in k_values){
      cv_performance <- numeric(nfold)
      
      for(f in 1:nfold){
        idx_train <- which(folds != f)
        idx_valid <- which(folds == f)
        
        bart_cv <- pbart(
          x.train = cbind(X_train[idx_train, ], A_train[idx_train]),
          y.train = Y_train[idx_train],
          
          ntree   = m_val,
          k       = k_val,
          sparse  = FALSE 
        )
        
        # on valid 
        pred_valid_A0 <- predict(bart_cv, newdata = cbind(X_train[idx_valid,], 0))$prob.test.mean
        pred_valid_A1 <- predict(bart_cv, newdata = cbind(X_train[idx_valid,], 1))$prob.test.mean
        
        # value function of f-th
        cv_performance[f] <- mean(pmax(pred_valid_A0, pred_valid_A1))
      }
      
      # value function 
      mean_cv_perf <- mean(cv_performance)
      
      # update if better
      if(mean_cv_perf > best_cv){
        best_cv <- mean_cv_perf
        best_m  <- m_val
        best_k  <- k_val
      }
    }
  }
  
  
  bart_model_cv_final <- pbart(
    x.train = cbind(X_train, A_train),
    y.train = Y_train,
    ntree   = best_m,
    k       = best_k,
    sparse  = FALSE
  )
  
  
  pred_A0 <- predict(bart_model_cv_final, newdata = cbind(X_test, 0))$prob.test.mean
  pred_A1 <- predict(bart_model_cv_final, newdata = cbind(X_test, 1))$prob.test.mean
  
  Val_BARTcv[v] <- mean(pmax(pred_A0, pred_A1))
}


Result_K$BARTcv[1] <- mean(Val_BARTcv) / Val_opt




# ----K4-BARTd----

set.seed(1)  
p <- 2  # no of covariates
# generate test set
X_test <-matrix(runif(n_test * p, min=-1.5, max=1.5), ncol = p)  # X1X2 Unif
#K4 formula
p_Y_A0 <- 1-exp(-exp(2 - 1.5*X_test[,1]^2 -1.5*X_test[,2]^2 + 3*X_test[,1]*X_test[,2] ))
p_Y_A1 <- 1-exp(-exp(2 - 1.5*X_test[,1]^2 -1.5*X_test[,2]^2 + 3*X_test[,1]*X_test[,2] + 
                       1*(-0.1-X_test[,1]+X_test[,2])))
Val_opt <-mean(pmax(p_Y_A0,p_Y_A1))

Val_BARTd<-numeric(50)
for(v in 1:50){
  
  X_train <- matrix(runif(n_train * p, min=-1.5, max=1.5), ncol = p)  # X1X2 Unif
  A_train <- rbinom(n_train, 1, 0.5)  #  A ~ Bernoulli(0.5)
  
  #K4 formula
  p_Y <-  1-exp(-exp(2 - 1.5*X_train[,1]^2 -1.5*X_train[,2]^2 + 3*X_train[,1]*X_train[,2] + 
                       A_train*(-0.1-X_train[,1]+X_train[,2])))
  
  Y_train <- rbinom(n_train, 1, p_Y)  #  Y ~ Bernoulli(p_Y)
  
  # train BART ITR
  bart_model <- pbart(x.train = cbind(X_train, A_train), 
                      y.train = Y_train,                 
                      sparse = FALSE)                    
  
  
  
  
  # BART prediction
  pred_A0 <- predict(bart_model, newdata = cbind(X_test, 0))$prob.test.mean  # A=0 prediction
  pred_A1 <- predict(bart_model, newdata = cbind(X_test, 1))$prob.test.mean  # A=1 prediction
  
  Val_BARTd[v]<-mean(pmax(pred_A1,pred_A0))
}

Result_K$BARTd[4]<-mean(Val_BARTd)/Val_opt



# ----K4-BARTcv----
set.seed(1)  
p <- 2  # no of covariates
# generate test set
X_test <-matrix(runif(n_test * p, min=-1.5, max=1.5), ncol = p)  # X1X2 Unif
#K4 formula
p_Y_A0 <- 1-exp(-exp(2 - 1.5*X_test[,1]^2 -1.5*X_test[,2]^2 + 3*X_test[,1]*X_test[,2] ))
p_Y_A1 <- 1-exp(-exp(2 - 1.5*X_test[,1]^2 -1.5*X_test[,2]^2 + 3*X_test[,1]*X_test[,2] + 
                       1*(-0.1-X_test[,1]+X_test[,2])))
Val_opt <-mean(pmax(p_Y_A0,p_Y_A1))

Val_BARTcv <- numeric(50)

for(v in 1:50){
  
  X_train <- matrix(runif(n_train * p, min=-1.5, max=1.5), ncol = p)  # X1X2 Unif
  A_train <- rbinom(n_train, 1, 0.5)
  
  #K4 formula
  p_Y <-  1-exp(-exp(2 - 1.5*X_train[,1]^2 -1.5*X_train[,2]^2 + 3*X_train[,1]*X_train[,2] + 
                       A_train*(-0.1-X_train[,1]+X_train[,2])))
  
  Y_train <- rbinom(n_train, 1, p_Y)
  
  
  folds <- make_folds(n_train, nfold)
  
  best_m  <- NA
  best_k  <- NA
  best_cv <- -Inf  
  
  
  for(m_val in m_values){
    for(k_val in k_values){
      cv_performance <- numeric(nfold)
      
      for(f in 1:nfold){
        idx_train <- which(folds != f)
        idx_valid <- which(folds == f)
        
        bart_cv <- pbart(
          x.train = cbind(X_train[idx_train, ], A_train[idx_train]),
          y.train = Y_train[idx_train],
          
          ntree   = m_val,
          k       = k_val,
          sparse  = FALSE 
        )
        
        # on valid 
        pred_valid_A0 <- predict(bart_cv, newdata = cbind(X_train[idx_valid,], 0))$prob.test.mean
        pred_valid_A1 <- predict(bart_cv, newdata = cbind(X_train[idx_valid,], 1))$prob.test.mean
        
        # value function of f-th
        cv_performance[f] <- mean(pmax(pred_valid_A0, pred_valid_A1))
      }
      
      # value function 
      mean_cv_perf <- mean(cv_performance)
      
      # update if better
      if(mean_cv_perf > best_cv){
        best_cv <- mean_cv_perf
        best_m  <- m_val
        best_k  <- k_val
      }
    }
  }
  
  
  bart_model_cv_final <- pbart(
    x.train = cbind(X_train, A_train),
    y.train = Y_train,
    ntree   = best_m,
    k       = best_k,
    sparse  = FALSE
  )
  
  
  pred_A0 <- predict(bart_model_cv_final, newdata = cbind(X_test, 0))$prob.test.mean
  pred_A1 <- predict(bart_model_cv_final, newdata = cbind(X_test, 1))$prob.test.mean
  
  Val_BARTcv[v] <- mean(pmax(pred_A0, pred_A1))
}


Result_K$BARTcv[4] <- mean(Val_BARTcv) / Val_opt



# ---- K6-BARTd----
set.seed(1)  
p <- 2  # no of covariates
# generate test set
X_test <- matrix(rnorm(n_test * p), ncol = p)    

#K6 
p_Y_A0 <- 1-plogis(0.1 - 0.2*X_test[,1] + 0.2*X_test[,2] - X_test[,1]*X_test[,2] )
p_Y_A1 <- 1-plogis(0.1 - 0.2*X_test[,1] + 0.2*X_test[,2] - X_test[,1]*X_test[,2] + 
                     1 * (-0.5 - X_test[,1] + X_test[,2] +3*X_test[,1]*X_test[,2]))
Val_opt <-mean(pmax(p_Y_A0,p_Y_A1))

Val_BARTd<-numeric(50)
for(v in 1:50){
  
  X_train <- matrix(rnorm(n_train * p), ncol = p)  # X1, X2 N(0,1)
  A_train <- rbinom(n_train, 1, 0.5)  #  A ~ Bernoulli(0.5)
  
  #K6 formula
  p_Y <- 1-plogis(0.1 - 0.2*X_train[,1] + 0.2*X_train[,2] - X_train[,1]*X_train[,2] + 
                    A_train * (-0.5 - X_train[,1] + X_train[,2] +3*X_train[,1]*X_train[,2]))
  
  Y_train <- rbinom(n_train, 1, p_Y)  #  Y ~ Bernoulli(p_Y)
  
  # train BART ITR
  bart_model <- pbart(x.train = cbind(X_train, A_train), 
                      y.train = Y_train,                 
                      sparse = FALSE)                    
  
  
  
  
  # BART prediction
  pred_A0 <- predict(bart_model, newdata = cbind(X_test, 0))$prob.test.mean  # A=0 prediction
  pred_A1 <- predict(bart_model, newdata = cbind(X_test, 1))$prob.test.mean  # A=1 prediction
  
  Val_BARTd[v]<-mean(pmax(pred_A1,pred_A0))
}

Result_K$BARTd[6]<-mean(Val_BARTd)/Val_opt

#----K6-BARTcv----
set.seed(1)  
p <- 2  # no of covariates
# generate test set
X_test <- matrix(rnorm(n_test * p), ncol = p)    

#K6 
p_Y_A0 <- 1-plogis(0.1 - 0.2*X_test[,1] + 0.2*X_test[,2] - X_test[,1]*X_test[,2] )
p_Y_A1 <- 1-plogis(0.1 - 0.2*X_test[,1] + 0.2*X_test[,2] - X_test[,1]*X_test[,2] + 
                     1 * (-0.5 - X_test[,1] + X_test[,2] +3*X_test[,1]*X_test[,2]))
Val_opt <-mean(pmax(p_Y_A0,p_Y_A1))







Val_BARTcv <- numeric(50)

for(v in 1:50){
  
  X_train <- matrix(rnorm(n_train * p), ncol = p)
  A_train <- rbinom(n_train, 1, 0.5)
  
  #K6 formula
  p_Y <- 1-plogis(0.1 - 0.2*X_train[,1] + 0.2*X_train[,2] - X_train[,1]*X_train[,2] + 
                    A_train * (-0.5 - X_train[,1] + X_train[,2] +3*X_train[,1]*X_train[,2]))
  
  Y_train <- rbinom(n_train, 1, p_Y)
  
  
  folds <- make_folds(n_train, nfold)
  
  best_m  <- NA
  best_k  <- NA
  best_cv <- -Inf  
  
  
  for(m_val in m_values){
    for(k_val in k_values){
      cv_performance <- numeric(nfold)
      
      for(f in 1:nfold){
        idx_train <- which(folds != f)
        idx_valid <- which(folds == f)
        
        bart_cv <- pbart(
          x.train = cbind(X_train[idx_train, ], A_train[idx_train]),
          y.train = Y_train[idx_train],
          
          ntree   = m_val,
          k       = k_val,
          sparse  = FALSE 
        )
        
        # on valid 
        pred_valid_A0 <- predict(bart_cv, newdata = cbind(X_train[idx_valid,], 0))$prob.test.mean
        pred_valid_A1 <- predict(bart_cv, newdata = cbind(X_train[idx_valid,], 1))$prob.test.mean
        
        # value function of f-th
        cv_performance[f] <- mean(pmax(pred_valid_A0, pred_valid_A1))
      }
      
      # value function 
      mean_cv_perf <- mean(cv_performance)
      
      # update if better
      if(mean_cv_perf > best_cv){
        best_cv <- mean_cv_perf
        best_m  <- m_val
        best_k  <- k_val
      }
    }
  }
  
  
  bart_model_cv_final <- pbart(
    x.train = cbind(X_train, A_train),
    y.train = Y_train,
    ntree   = best_m,
    k       = best_k,
    sparse  = FALSE
  )
  
  
  pred_A0 <- predict(bart_model_cv_final, newdata = cbind(X_test, 0))$prob.test.mean
  pred_A1 <- predict(bart_model_cv_final, newdata = cbind(X_test, 1))$prob.test.mean
  
  Val_BARTcv[v] <- mean(pmax(pred_A0, pred_A1))
}


Result_K$BARTcv[6] <- mean(Val_BARTcv) / Val_opt







# ---- K3-BARTd ----
set.seed(1)  
p <- 3  # no of covariates
# generate test set
X_test <- matrix(rnorm(n_test * p), ncol = p)    
p_Y_A0 <- 1-exp(-exp(-0.7 - 0.2*X_test[,1] -0.2*X_test[,2] + 0.1*X_test[,3] ))
p_Y_A1 <- 1-exp(-exp(-0.7 - 0.2*X_test[,1] -0.2*X_test[,2] + 0.1*X_test[,3] + 
                       1*(0.1 + 2*X_test[,1] -X_test[,2] -0.3*X_test[,3])))
Val_opt <-mean(pmax(p_Y_A0,p_Y_A1))

Val_BARTd<-numeric(50)
for(v in 1:50){
  
  X_train <- matrix(rnorm(n_train * p), ncol = p)  # X1, X2, X3 ~ N(0,1)
  A_train <- rbinom(n_train, 1, 0.5)  #  A ~ Bernoulli(0.5)
  
  #K3 formula
  p_Y <- 1-exp(-exp(-0.7 - 0.2*X_train[,1] -0.2*X_train[,2] + 0.1*X_train[,3] + 
                      A_train*(0.1 + 2*X_train[,1] -X_train[,2] -0.3*X_train[,3])))
  
  Y_train <- rbinom(n_train, 1, p_Y)  #  Y ~ Bernoulli(p_Y)
  
  # train BART ITR
  bart_model <- pbart(x.train = cbind(X_train, A_train), 
                      y.train = Y_train,                 
                      sparse = FALSE)                    
  
  
  
  
  # BART prediction
  pred_A0 <- predict(bart_model, newdata = cbind(X_test, 0))$prob.test.mean  # A=0 prediction
  pred_A1 <- predict(bart_model, newdata = cbind(X_test, 1))$prob.test.mean  # A=1 prediction
  
  Val_BARTd[v]<-mean(pmax(pred_A1,pred_A0))
}

Result_K$BARTd[3]<-mean(Val_BARTd)/Val_opt

#---- K3BARTcv----
set.seed(1)  
p <- 3  # no of covariates
# generate test set
X_test <- matrix(rnorm(n_test * p), ncol = p)    
p_Y_A0 <- 1-exp(-exp(-0.7 - 0.2*X_test[,1] -0.2*X_test[,2] + 0.1*X_test[,3] ))
p_Y_A1 <- 1-exp(-exp(-0.7 - 0.2*X_test[,1] -0.2*X_test[,2] + 0.1*X_test[,3] + 
                       1*(0.1 + 2*X_test[,1] -X_test[,2] -0.3*X_test[,3])))
Val_opt <-mean(pmax(p_Y_A0,p_Y_A1))

Val_BARTcv <- numeric(50)

for(v in 1:50){
  
  X_train <- matrix(rnorm(n_train * p), ncol = p)
  A_train <- rbinom(n_train, 1, 0.5)
  
  #K3 formula
  p_Y <- 1-exp(-exp(-0.7 - 0.2*X_train[,1] -0.2*X_train[,2] + 0.1*X_train[,3] + 
                      A_train*(0.1 + 2*X_train[,1] -X_train[,2] -0.3*X_train[,3])))
  
  Y_train <- rbinom(n_train, 1, p_Y)
  
  
  folds <- make_folds(n_train, nfold)
  
  best_m  <- NA
  best_k  <- NA
  best_cv <- -Inf  
  
  
  for(m_val in m_values){
    for(k_val in k_values){
      cv_performance <- numeric(nfold)
      
      for(f in 1:nfold){
        idx_train <- which(folds != f)
        idx_valid <- which(folds == f)
        
        bart_cv <- pbart(
          x.train = cbind(X_train[idx_train, ], A_train[idx_train]),
          y.train = Y_train[idx_train],
          
          ntree   = m_val,
          k       = k_val,
          sparse  = FALSE 
        )
        
        # on valid 
        pred_valid_A0 <- predict(bart_cv, newdata = cbind(X_train[idx_valid,], 0))$prob.test.mean
        pred_valid_A1 <- predict(bart_cv, newdata = cbind(X_train[idx_valid,], 1))$prob.test.mean
        
        # value function of f-th
        cv_performance[f] <- mean(pmax(pred_valid_A0, pred_valid_A1))
      }
      
      # value function 
      mean_cv_perf <- mean(cv_performance)
      
      # update if better
      if(mean_cv_perf > best_cv){
        best_cv <- mean_cv_perf
        best_m  <- m_val
        best_k  <- k_val
      }
    }
  }
  
  
  bart_model_cv_final <- pbart(
    x.train = cbind(X_train, A_train),
    y.train = Y_train,
    ntree   = best_m,
    k       = best_k,
    sparse  = FALSE
  )
  
  
  pred_A0 <- predict(bart_model_cv_final, newdata = cbind(X_test, 0))$prob.test.mean
  pred_A1 <- predict(bart_model_cv_final, newdata = cbind(X_test, 1))$prob.test.mean
  
  Val_BARTcv[v] <- mean(pmax(pred_A0, pred_A1))
}


Result_K$BARTcv[3] <- mean(Val_BARTcv) / Val_opt




#----Plot----
library(ggplot2)
library(dplyr)

# create dataframe
df <- data.frame(
  Scenario = factor(c("K1", "K3", "K4", "K6"), levels = c("K1", "K3", "K4", "K6")),
  BARTd = c(92.91, 92.16, 97.02, 86.03),
  BARTcv = c(99.35, 99.24, 99.34, 95.24)
)

# long format
df_long <- df %>%
  tidyr::pivot_longer(cols = c(BARTd, BARTcv), names_to = "Method", values_to = "Value")


ggplot(df_long, aes(x = Scenario, y = Value / 100, color = Method, group = Method)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("BARTd" = "red", "BARTcv" = "black")) +
  labs(
    y = "Fraction of optimal Value",
    x = "Scenario",
    color = "Method"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "#f0f0f0"),
    legend.position = "right"
  ) +
  ylim(0.82, 1.0)





