
#' Perform probability of backtest overfitting computation via CSCV.
#' @param M a \eqn{TxN} data frame of returns, where \eqn{T} is the samples per study and \eqn{N} is the number of studies.
#' @param S the number of subsets of \eqn{M} for CSCV combinations; must evenly divide \eqn{M} 
#' @param F the function to evaluate a study's performance; required
#' @param threshold the performance metric threshold (e.g. 0 for Sharpe, 1 for Omega)
#' @param inf_sub infinity substitution value for reasonable plotting
#' @param allow_parallel whether to enable parallel processing, default FALSE
#' @return object of class 'pbo' containing list of PBO calculation results and settings
pbo <- function(M,S=4,F=NA,threshold=0,inf_sub=6,allow_parallel=FALSE) {
  stopifnot(is.function(F))
  require(utils,quietly=TRUE)
  
  T <- nrow(M)             # samples per study
  N <- ncol(M)             # studies
  CS <- combn(S,S/2)       # combinations
  SN <- T / S              # partition size
  test_config <- bquote(N == .(N) ~~ T == .(T) ~~ S == .(S))
  
  # initialize results lists
  cs_results <- list()
  
  # a worker iteration function for a single case
  cs_compute <- function(cs) {
    # partition indices
    IS_I <- CS[,cs]
    
    # in-sample indices
    is_indices <- as.vector(sapply(IS_I,function(i) {
      start <- SN * i - SN + 1
      end <- start + SN - 1
      start:end
    }))
    
    # out-of-sample indices
    os_indices <- setdiff(1:T,is_indices) 
    
    # training and test sets (in sample, out of sample)
    # choosing not to reassign row names of each to 1:(T/2)
    # after R don't need to save J or J_bar so could skip this assignment
    J <- M[is_indices,]
    J_bar <- M[os_indices,]
    
    # compute performance over the N strategies in each subset
    # could use for R any summary statistic e.g. SharpeRatio or Omega
    R <- mapply(F,J)
    R_bar <- mapply(F,J_bar)
    
    # compute n* by argmax over R vector
    n_star <- which.max(R)
    n_max_oos <- which.max(R_bar)
    
    # rank of n*th result from OOS performance; converted to (0,1) interval
    os_rank <- rank(R_bar)[n_star]
    omega_bar_c <- os_rank / length(R_bar)
    
    # logit
    # note the value can be Inf
    lambda_c <- log(omega_bar_c / (1 - omega_bar_c))
    
    list(R,R_bar,n_star,n_max_oos,os_rank,omega_bar_c,lambda_c)
  }

  # for each partition combination
  cs_results <- NULL
  if ( allow_parallel ) {
    require(foreach,quietly=TRUE)
    cs_results <- foreach ( cs=1:ncol(CS),
                .combine=rbind,
                .multicombine=TRUE) %dopar%
      cs_compute(cs)
  } else {
    for ( cs in 1:ncol(CS) ) {
      cs_results <- rbind(cs_results,cs_compute(cs))
    }
  }
  
  colnames(cs_results) <- c("R","R_bar","n*","n_max_oos","os_rank","omega_bar","lambda")
  rownames(cs_results) <- 1:ncol(CS)
  
  lambda <- as.numeric(cs_results[,"lambda"])
  lambda[which(lambda==Inf)] <- inf_sub # for plotting
  
  # probability of backtest overfit
  # using MC test count approach of lambda count
  phi <- sum(ifelse(lambda<=0,1,0))/ncol(CS)
  # -- alternative might use relative frequency sum 
  # rf <- as.data.frame( table(lambda) / ncol(CS), stringAsFactors=FALSE)
  # phi <- sum(ifelse(rf$lambda <= 0, rf$Freq, 0))
  # -- alternative might use density fit
  # d <- density(lambda,kernel="rectangular")
  
  # performance degradation
  rn_pairs <- as.data.frame(do.call(rbind,lapply(1:ncol(CS),function(i) {
    n <- cs_results[[i,3]]
    r <- cs_results[[i,1]]
    rb <- cs_results[[i,2]]
    return(c(r[n],rb[n]))
  })))
  colnames(rn_pairs) <- c("Rn","Rbn")
  
  # linear fit to pairs, extract results for plot annotations
  linear_fit <- lm(rn_pairs)
  m <- signif(as.numeric(linear_fit$coefficients[1]),digits=5) # slope
  b <- signif(as.numeric(linear_fit$coefficients[2]),digits=5) # intercept
  ar2 <- signif(summary(linear_fit)$adj.r.squared,digits=2) # adj R-squared
  
  # probability out-of-sample below threshold
  p_oos_bt <- signif(length(which(rn_pairs$Rbn<threshold)) / 
                       nrow(rn_pairs),digits=3)
  rv = list(
    results=cs_results,
    combos=CS,
    lambda=lambda,
    phi=phi,
    rn_pairs=rn_pairs,
    func=as.character(substitute(F)),
    slope=m,
    intercept=b,
    ar2=ar2,
    threshold=threshold,
    below_threshold=p_oos_bt,
    test_config=test_config,
    inf_sub=inf_sub)
  class(rv) <- "pbo"
  rv
}
