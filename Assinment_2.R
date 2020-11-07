#ASSIGNMENT: ELASTIC NET REGULARIZATION
#NAME: HYEJIN KIM

#MM algorithm
#Parameters: X, y, eps, lambda, alpha

#function starts
elastic_net <- function (X , y , eps , lambda , alpha ) {
  
  #initialization 
  n <- nrow(X)
  beta_k1 <- rep(0, ncol(X))
  XtX <- crossprod(X,X); Xty <- t(X) %*% y; yty <- crossprod(y,y)
  k <- 1 ; L_diff <- 1; L_beta_k <- 2; L_beta_k1 <- 1
  
  D <- function (beta) diag(1 / max(abs(beta) , eps), ncol(X), ncol(X)) #can I use the pre-defined alpha and lambda here?
  c <- function (beta) (yty / 2*n + (0.5) * lambda * alpha * sum(abs(beta)))
  
  
  #while loop
  while (k == 1 | L_diff/L_beta_k > eps) {
    k <- k + 1
    L_beta_k <- L_beta_k1                                    
    beta_k <- beta_k1
    
    D_new <- D(beta_k)
    c_new <- c(beta_k)
    A <- XtX/n + diag(lambda * (1 - alpha), ncol(X), ncol(X)) + lambda * alpha * D_new
    
    beta_k1 <- solve (A, Xty/n) #
    
    L_beta_k1 <- (0.5)* (t(beta_k1) %*% ((A) %*% beta_k1)) - (t(beta_k1) %*% Xty)/n + c_new

    L_diff <- L_beta_k - L_beta_k1
    
    print (list ("k" = k, "L_beta_k1" = L_beta_k1, "L_diff" = L_diff, "beta_k1" = beta_k1))
  }
  return (beta_k1)
}

#K-fold cross-validation
k_cv <- function (X , y, k_fold , eps , lambdas , alpha ){
  eps <- eps; alpha <- alpha
  n <- nrow(X)
  m <- n/(k_fold) #But what if n/k is not a whole number?
  RMSE <- matrix(NA, nrow = length(lambdas), ncol = 2)
  MSE <- matrix(NA, nrow = k_fold, ncol = 1)
  for (i in 1:length(lambdas)){ #???
    lambda <- lambdas[i]
    RMSE[i,1] <- lambdas[i]
    for (k in 1: k_fold){
      if (k == 1){
        X_train <- X[ -(1 : m) , ] ; y_train <- y[ -(1 : m) , ]
        X_test <- X[ (1 : m) , ] ;  y_test <- y[ (1 : m) , ]
        
      } else{ 
        X_train <- X[-((k - 1) * m : (k * m) ), ]; y_train <- y[-((k - 1) * m : k * m ), ]
        X_test <- X[ (k - 1) * m : (k * m) , ]; y_test <- y[(k - 1) * m : (k * m) , ]
      }
      beta_k1 <- elastic_net(X_train , y_train , eps , lambda , alpha )
      y_hat <- X_test %*% beta_k1
      y_diff <- y_test - y_hat
      MSE[k,1] <- (1/n)*(t(y_diff) %*% y_diff)
      RMSE[i,2] <- ((1/k_fold)*sum(MSE))^(0.5) 
    }
    
  }

}
dim(X)
dim(y2)
as.matrix(y)
y2 <- as.matrix(y)
#Main
load("supermarket1996.RData")
data<-supermarket1996
colnames(data)
data<-data[,-which(colnames(data) == c("STORE", "CITY", "ZIP", "GROCCOUP_sum", "SHPINDX"))]
colnames(data[,3:46]) 
X <- as.matrix(data[,3:46])
y <- as.matrix(data$GROCERY_sum)
results <- k_cv (X , y, 11 , 1e-8 , (0.001:100) , 0.5 )
X[ (( 11- 1) * 7 : 11 * 7 ), ]
#Purchase behavior & availabbility of time
#Purchase behavior and income
  
Xty <- crossprod(X,y)
X  
y  
