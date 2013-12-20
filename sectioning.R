require(utils)
require(PerformanceAnalytics)

N = 10  # studies
T = 1600 # sample returns
S = 8 # partition count
CS = combn(S,S/2) # combinations
SN = T / S # partition size
IDs = 1:S # partition identifiers

M <- data.frame(matrix(NA,T,N,byrow=TRUE,dimnames=list(1:T,1:N)),
                check.names=FALSE)
for ( i in 1:N ) M[,i] <- rt(T,6) / 100
#plist <- mapply(function(col) exp(cumsum(M[,col])), 1:N)
#olist <- mapply(Omega,M)

# build partitions

# results lists
is_results = list()
oos_results = list()

# for each combination
for ( cs in 1:ncol(CS) ) {
  
  # partition indices
  IS_I = CS[,cs]
  
  # in-sample indices
  is_indices = as.vector(sapply(IS_I,function(i) {
    start = SN * i - SN + 1
    end = start + SN - 1
    start:end
  }))
  
  # out-of-sample indices
  os_indices = which(1:T != is_indices)
  
  # training and test sets (in sample, out of sample)
  # choosing not to reassign row names of each to 1:(T/2)
  J = M[is_indices,]
  J_hat = M[os_indices,]
  
  # compute Omega ratio over the N strategies in each subset
  omega = mapply(Omega,J)
  omega_hat = mapply(Omega,J_hat)
  
  is_results = rbind(is_results,omega)
  oos_results = rbind(oos_results,omega_hat)
}

rownames(is_results) <- 1:ncol(CS)
rownames(oos_results) <- 1:ncol(CS)
