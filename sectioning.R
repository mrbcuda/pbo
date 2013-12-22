require(utils)
require(PerformanceAnalytics)

N = 100  # studies
T = 1600 # sample returns
S = 8 # partition count
CS = combn(S,S/2) # combinations
SN = T / S # partition size
IDs = 1:S # partition identifiers
INF_SUB = 6 # replace Inf with something for plotting

M <- data.frame(matrix(NA,T,N,byrow=TRUE,dimnames=list(1:T,1:N)),
                check.names=FALSE)
for ( i in 1:N ) M[,i] <- rt(T,6) / 100
#plist <- mapply(function(col) exp(cumsum(M[,col])), 1:N)
#olist <- mapply(Omega,M)

# build partitions

# results lists
#is_results = list()
#oos_results = list()
cs_results = list()

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
  J_bar = M[os_indices,]
  
  # compute Omega ratio over the N strategies in each subset
  # could use for R any summary statistic e.g. SharpeRatio
  R = mapply(Omega,J)
  R_bar = mapply(Omega,J_bar)
  
  # compute n* by argmax over R vector
  n_star = which.max(R)
  
  # rank of n*th result from OOS performance; converted to (0,1) interval
  omega_bar_c = rank(R_bar)[n_star] / N
  
  # logit
  # note the value can be Inf
  lambda_c = log(omega_bar_c / (1 - omega_bar_c))
  
  # save the results ??
  #is_results = rbind(is_results,R)
  #oos_results = rbind(oos_results,R_bar)
  cs_results = rbind(cs_results,list(R,R_bar,n_star,omega_bar_c,lambda_c))
}

colnames(cs_results) <- c("R","R_bar","n*","omega_bar","lambda")
rownames(cs_results) <- 1:ncol(CS)

#rownames(is_results) <- 1:ncol(CS)
#rownames(oos_results) <- 1:ncol(CS)

lambda = as.numeric(cs_results[,"lambda"])
lambda[which(lambda==Inf)] = INF_SUB
hist(lambda,xlim=c(-INF_SUB,INF_SUB),main="f(x) Rank Logits")

