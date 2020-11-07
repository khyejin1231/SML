#ASSIGNMENT: ELASTIC NET REGULARIZATION
#NAME: HYEJIN KIM

#MM algorithm
#Parameters: X, y, eps, lambda, alpha

#function starts
elastic_net <- function (X , y , eps , lambda , alpha ) {
  
  #initialization 
 
  beta_k1 <- rep(0, ncol(x))
  XtX <- crossprod(X,X); Xty <- crossprod(X,y); yty <- crossprod(y,y)
  k <- 1
  
  D <- function (beta) diag(1 / max(abs(beta) , eps, ncol(X), ncol(X))) #can I use the pre-defined alpha and lambda here?
  c <- function (beta) yty / 2*n + (0.5) * lambda * alpha * sum(abs(beta))
  L_beta <- function (beta) (1/2)* t(beta) %*% ((A) %*% beta) - (t(beta) %*% Xty)/n + c
    
  #while loop
  while (k == 1 | L_diff/L_beta_k > eps) {
    k <- k + 1
    L_beta_k <- L_beta(beta_k1)                                     
    beta_k <- beta_k1
    
    D_new <- D(beta_k)
    
    A <- XtX/n + diag(lambda * (1 - alpha), ncol(X), ncol(X)) + lambda * alpha * D_new
    
    beta_k1 <- solve (A, Xty/n)
    
    L_beta_k1 <- L_beta(beta_k1)
    
    L_diff <- L_beta_k - L_beta_k1
    
    print (list ("k" = k, "L_beta_k1" = L_beta_k1, "L_diff" = L_diff, "beta_k1" = beta_k1))
  }
  return (beta_k1)
}

#K-fold cross-validation
k_cv <- function (X , y, k_fold , eps , lambdas , alpha ){
  X <- data.matrix(X); y <- data.matrix(y)
  eps <- eps; alpha <- alpha
  n = nrow(X)
  m <- n/(k_fold) #But what if n/k is not a whole number?
  RMSE <- NULL
  MSE <- NULL
  for (i in 1:length(lambdas)){ #???
    lambda <- lambdas[i]
    RMSE[,1] <- lambdas[i]
    for (k in 1: k_fold){
      if (k == 1){
        X_train <- X[ -(1 : m) , ] ; y_train <- y[ -(1 : m) , ]
        X_test <- X[ (1 : m) , ] ;  y_test <- y[ (1 : m) , ]
        
      } else{ 
        X_train <- X[ -((k-1) * m : k * m ), ]; y_train <- y[-((k-1) * m : k * m ), ]
        X_test <- X[ ((k-1) * m : k * m ), ]; y_test <- y[((k-1) * m : k * m ), ]
      }
      elastic_net(X_train , y_train , eps , lambda , alpha )
      y_hat <- X_test %*% beta_k1
      MSE[k,1] <- (1/n)*((y_test - y_hat) %*% (y_test - y_hat)) #ceossprod?
      RMSE[i,2] <- ((1/k_fold)*sum(MSE))^(0.5) 
    }
    
  }

}

#Main
load("supermarket1996.RData")
data<-supermarket1996
colnames(data)
data<-data[,-which(colnames(data) == c("STORE", "CITY", "ZIP", "GROCCOUP_sum", "SHPINDX"))]
colnames(data[,3:46]) 
X <- data[,3:46]
y <- data$GROCERY_sum
results <- k_cv (X , y, 11 , 1e-8 , 0.001:100 , 0.5 )

#Purchase behavior & availabbility of time
#Purchase behavior and income
  
  
  
  