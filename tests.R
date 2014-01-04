#' Tests from reference paper.
#' No, low, and high overfit.  
#' So far hese work occasionally, but the low and high usually result in PBO=1

# mock Sharpe ratio, one percent daily reference
# as used for reference test case reproduction
require(PerformanceAnalytics,quietly=TRUE)
sharpe = function(x,rf=0.01/260) {
  return(mean(Return.excess(x,Rf=rf)) / sd(x-rf))
}


##### test setup TC1 full overfit
N = 200
T = 2000
S = 16
sr_base = 0


# scaling and centering
mu_base = sr_base/(260.0)
sigma_base = 1.00/(260.0)**0.5

M <- data.frame(matrix(NA,T,N,byrow=TRUE,dimnames=list(1:T,1:N)),
                check.names=FALSE)

for ( i in 1:N ) {
  M[,i] <- rnorm(T,mean=0,sd=1)
  M[,i] = M[,i] * sigma_base / sd(M[,i]) # re-scale
  M[,i] = M[,i] + mu_base - mean(M[,i]) # re-center
}

##### test setup TC2 high overfit
sr_case = 1
mu_case = sr_case/(260.0)
sigma_case = sigma_base

# same as TC1, but overwrite Nth case (column)
i = N
M[,i] <- rnorm(T,mean=0,sd=1)
M[,i] = M[,i] * sigma_case / sd(M[,i]) # re-scale
M[,i] = M[,i] + mu_case - mean(M[,i]) # re-center

##### test setup TC3 low overfit
sr_case = 2
mu_case = sr_case/(260.0)
sigma_case = sigma_base

# same as TC1, but overwrite Nth case (column)
i = N
M[,i] <- rnorm(T,mean=0,sd=1)
M[,i] = M[,i] * sigma_case / sd(M[,i]) # re-scale
M[,i] = M[,i] + mu_case - mean(M[,i]) # re-center
