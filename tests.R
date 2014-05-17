#' Tests from reference paper.
#' No, low, and high overfit.  

# mock Sharpe ratio
# as used for reference test case reproduction
require(PerformanceAnalytics,quietly=TRUE)
sharpe <- function(x,rf=0.03/252) {
  sr <- apply(x,2,function(col) {
    er = col - rf
    return(mean(er)/sd(er))
  })
  return(sr)
}



##### corner case only one study
N <- 1
T <- 1000
S <- 8

M <- data.frame(matrix(rnorm(N*T,mean=0,sd=1),
                       nrow=T,ncol=N,byrow=TRUE,
                       dimnames=list(1:T,1:N)),
                check.names=FALSE)

mypbo <- pbo(M,S,Omega,1)

##### test setup TC1 full overfit
N <- 100
T <- 1000
S <- 8

sr_base <- 0
mu_base <- sr_base/(252.0)
sigma_base <- 1.00/(252.0)**0.5

M <- data.frame(matrix(rnorm(N*T,mean=0,sd=1),
                       nrow=T,ncol=N,byrow=TRUE,
                       dimnames=list(1:T,1:N)),
                check.names=FALSE)

# M <- scale(M) # scale and center
for ( i in 1:N ) {
  M[,i] <- rnorm(T,mean=0,sd=1)
  M[,i] = M[,i] * sigma_base / sd(M[,i]) # re-scale
  M[,i] = M[,i] + mu_base - mean(M[,i]) # re-center
}

##### parallel
require(doParallel)
cluster <- makeCluster(detectCores())
registerDoParallel(cluster)
p_pbo <- pbo(M,S,f=Omega,threshold=1,allow_parallel=TRUE)
stopCluster(cluster)

##### test setup TC2 high overfit
sr_case <- 1
mu_case <- sr_case/(252.0)
sigma_case <- sigma_base

M <- data.frame(matrix(rnorm(N*T,mean=0,sd=1),
                       nrow=T,ncol=N,byrow=TRUE,
                       dimnames=list(1:T,1:N)),
                check.names=FALSE)

# same as TC1, but overwrite Nth case (column)
i <- N
M[,i] <- rnorm(T,mean=0,sd=1)
M[,i] <- M[,i] * sigma_case / sd(M[,i]) # re-scale
M[,i] <- M[,i] + mu_case - mean(M[,i]) # re-center

# M <- scale(M) # scale and center
mypbo <- pbo(M,S,sharpe,0)




##### test setup TC3 low overfit
sr_case <- 2
mu_case <- sr_case/(252.0)
sigma_case <- sigma_base

# same as TC1, but overwrite Nth case (column)
M <- data.frame(matrix(rnorm(N*T,mean=0,sd=1),
                       nrow=T,ncol=N,byrow=TRUE,
                       dimnames=list(1:T,1:N)),
                check.names=FALSE)
i <- N
M[,i] <- rnorm(T,mean=0,sd=1)
M[,i] <- M[,i] * sigma_case / sd(M[,i]) # re-scale
M[,i] <- M[,i] + mu_case - mean(M[,i]) # re-center

# M <- scale(M) # scale and center
mypbo <- pbo(M,S,sharpe,0)


