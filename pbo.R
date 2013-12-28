#' @title Probability of backtest overfitting
#' @docType data
#' Implement algorithms for computing the probability of 
#' backtest overfitting and related measures, based on the approach
#' specified in Bailey et al., September 2013.
#'  
#' @author Matt Barry <mrb@softisms.com>
#' @source See Bailey, David H. and Borwein, Jonathan M. and 
#' Lopez de Prado, Marcos and Zhu, Qiji Jim, The Probability of Back-Test Overfitting 
#' (September 1, 2013). Available at SSRN: http://ssrn.com/abstract=2326253 or 
#' http://dx.doi.org/10.2139/ssrn.2326253
#' 
require(PerformanceAnalytics,quietly=TRUE) # for performance measure

##### test setup t100
# populating the matrix M with samples from an installed distribution
# see also some reference tests in the tests file

N <- 200                 # studies, alternative configurations
T <- 3200                # sample returns
S <- 8                   # partition count

M <- data.frame(matrix(NA,T,N,byrow=TRUE,dimnames=list(1:T,1:N)),
                check.names=FALSE)
for ( i in 1:N ) M[,i] <- rt(T,10) / 100

my_pbo = pbo(M,S,F=Omega,threshold=1)

#' PBO performs probability of backtest overfitting computation via CSCV.
#' @param M a TxN matrix of returns
#' @param S the number of subsets of M for CSCV combinations 
#' @param threshold the performance metric threshold (e.g. 0 for Sharpe, 1 for Omega)
#' @return list of PBO calculation results and settings
pbo <- function(M,S=4,F=SharpeRatio,threshold=0,inf_sub=6) {
  require(utils,quietly=TRUE)
  
  T <- nrow(M)             # samples per study
  N <- ncol(M)             # studies
  CS <- combn(S,S/2)       # combinations
  SN <- T / S              # partition size
  test_config <- bquote(N == .(N) ~~ T == .(T) ~~ S == .(S))
  
  # initialize results lists
  cs_results <- list()
  
  # for each partition combination
  for ( cs in 1:ncol(CS) ) {
    
    # partition indices
    IS_I <- CS[,cs]
    
    # in-sample indices
    is_indices <- as.vector(sapply(IS_I,function(i) {
      start <- SN * i - SN + 1
      end <- start + SN - 1
      start:end
    }))
    
    # out-of-sample indices
    os_indices <- which(1:T != is_indices)
    
    # training and test sets (in sample, out of sample)
    # choosing not to reassign row names of each to 1:(T/2)
    # after R don't need to save J or J_bar so could skip this assignment
    J <- M[is_indices,]
    J_bar <- M[os_indices,]
    
    # compute performance over the N strategies in each subset
    # could use for R any summary statistic e.g. SharpeRatio or Omega
    R <- mapply(F,J) # mapply(sharpe,J) 
    R_bar <- mapply(F,J_bar) # mapply(sharpe,J_bar)
    
    # compute n* by argmax over R vector
    n_star <- which.max(R)
    
    # rank of n*th result from OOS performance; converted to (0,1) interval
    omega_bar_c <- rank(R_bar)[n_star] / length(R_bar)
    
    # logit
    # note the value can be Inf
    lambda_c <- log(omega_bar_c / (1 - omega_bar_c))
    
    # save the results
    cs_results <- rbind(cs_results,list(R,R_bar,n_star,omega_bar_c,lambda_c))
  }
  
  colnames(cs_results) <- c("R","R_bar","n*","omega_bar","lambda")
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
  return(list(
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
    inf_sub=inf_sub))
}

#' Draws an annotated histogram of rank logits.
#' @param data a list as returned by pbo()
#' @param main plot title, default 'Rank Logits'
#' @param col_bar histogram bar fill color
#' @param col_line density plot line color
#' @param hist_type histogram type as used in 'histogram' function
#' @param show_pbo whether to show the PBO value annotation, default TRUE
#' @param show_regions whether to show the overfit region annotations, default TRUE
#' @param show_config whether to show the study dimension annotations, default TRUE
#' @param ... other parameters passed to 'histogram' or 'densityplot'
pbo_logit <- function(data,
                      main="Rank Logits",
                      col_bar="#cc99cc",
                      col_line="#3366cc",
                      hist_type="density",
                      show_pbo=TRUE,
                      show_regions=TRUE,
                      show_config=TRUE,
                      ...) 
{
  require(lattice,quietly=TRUE)
  require(grid,quietly=TRUE)
  
  # plot rank logit with PBO annotation
  histogram(data$lambda,
            xlim=c(-data$inf_sub,data$inf_sub),
            xlab=expression(lambda[c]),
            main=main,
            type=hist_type,
            panel = function(x, ...){
              panel.histogram(x, col=col_bar,...)
              panel.densityplot(x, col=col_line, ...)
              panel.abline(v=0,type="l",lty=3)
              xa <- unit(0, "npc") + unit(2, "mm") 
              ya <- unit(1, "npc") - unit(2, "mm") 
              if (show_pbo) {
                grid.text(label = bquote(PBO == .(round(data$phi,digits=3))), 
                          x = xa, 
                          y = ya, 
                          just = "left") 
              }
              if (show_config) {
                grid.text(label = data$test_config,
                          x = unit(1.0,"npc") - unit(2,"mm"),
                          y=ya,
                          just = "right")
              }
              if (show_regions) {
                grid.text(label = "Less overfit",
                          x = unit(0.5,"npc") + unit(2,"mm"), 
                          y=ya,
                          just="left")
                grid.text(label = "More overfit",
                          x = unit(0.5,"npc") - unit(2,"mm"), 
                          y=ya,
                          just="right")
              }
            }
  )
}

#' Draws an annotated plot of performance degradation and probability of loss.
#' @param data a list as returned by pbo()
#' @param main plot title, default 'OOS Performance Degradation'
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param col_bar histogram bar fill color
#' @param col_line density plot line color
#' @param show_eqn whether to show the line equation annotation, default TRUE
#' @param show_threshold whether to show the probability of loss annotation, default TRUE
#' @param show_config whether to show the study dimension annotations, default TRUE
#' @param ... other parameters passed to 'xyplot' or 'lmline'
pbo_degradation <- function(data,
                            main="OOS Performance Degradation",
                            xlab="R (OOS)",
                            ylab="R (IS)",
                            col_bar="#cc99cc",
                            col_line="#3366cc",
                            show_eqn=TRUE,
                            show_threshold=TRUE,
                            show_config=TRUE,
                            ...) 
{
  require(lattice,quietly=TRUE)
  require(grid,quietly=TRUE)
  
  # plot Rn pairs
  cloud_span <- c(signif(min(data$rn_pairs),-3),
                  signif(max(data$rn_pairs),3)) # axis range
  
  xyplot(data$rn_pairs$Rbn ~ data$rn_pairs$Rn,
         xlab = xlab,
         ylab = ylab,
         main = main,
         xlim = cloud_span,
         ylim = cloud_span,
         panel = function(x, ...){
           panel.xyplot(x,col=col_bar,...)
           panel.lmline(x,col=col_line,...)
           panel.abline(v=data$threshold,type="l",lty=3)
           panel.abline(h=data$threshold,type="l",lty=3)    
           panel.rug(x,col=col_bar,...)
           ya <- unit(1, "npc") - unit(3, "mm") 
           if ( show_eqn ) {
             grid.text(label = bquote(R_OOS == .(data$intercept) (R_IS) + .(data$slope) + err ~~ AdjR^2 == .(data$ar2)), 
                       x = unit(0, "npc") + unit(3, "mm"), 
                       y = ya, 
                       just = "left",
                       gp=gpar(col=col_line))
           }
           if (show_config) {
             grid.text(label = data$test_config,
                       x = unit(1,"npc") - unit(3,"mm"),
                       y = ya,
                       just="right")
           }
           if (show_threshold) {
             if ( data$threshold == 1 ) { # ugly but ifelse won't work on bquote
               grid.text(label = bquote(P(R_OOS<1) ==  .(data$below_threshold)),
                         x = unit(1, "npc") - unit(3, "mm"), 
                         y = ya - unit(10,"mm"), 
                         just = "right",
                         gp=gpar(col=col_bar)) 
             } else {
               grid.text(label = bquote(P(R_OOS<0) ==  .(data$below_threshold)),
                         x = unit(1, "npc") - unit(3, "mm"), 
                         y = ya - unit(10,"mm"), 
                         just = "right",
                         gp=gpar(col=col_bar))
             }
           }
         }
  )
}



#' Draws an annotated plot of stochastic dominance.
#' @param data a list as returned by pbo()
#' @param main plot title, default 'Stochastic Dominance'
#' @param ylab_left Y-axis label for 1st order
#' @param ylab_right Y-axis lable for 2nd order
#' @param col_sd1 line color for 1st order
#' @param col_sd2 line color for 2nd order
#' @param show_config whether to show the study dimension annotations, default TRUE
#' @param show_prob whether to show the probability equation annotation, default TRUE
#' @param ... other parameters passed to 'xyplot'
pbo_dominance <- function(data,
                          main="Stochastic Dominance",
                          ylab_left="Frequency",
                          ylab_right="2nd Ord. Stochastic Dominance",
                          col_sd1="#3366cc",
                          col_sd2="#339999",
                          increment=0.01,
                          show_config=TRUE,
                          show_prob=TRUE,
                          ...) 
{
  require(lattice,quietly=TRUE)
  require(grid,quietly=TRUE)
  require(latticeExtra,quietly=TRUE)
  
  # uses n* items from R-bar for one line, and all n items from R-bar for the other line
  # create cumulative distribution functions for each data set, then generate samples to plot
  y <- seq(min(data$rn_pairs$Rbn),
           max(data$rn_pairs$Rbn),
           increment) # reasonable R range for evaluation
  erbn <- ecdf(data$rn_pairs$Rbn) # optimized
  erb <- ecdf(sapply(1:ncol(data$combos),
                     function(i) data$results[[i,2]])) # non-optimized (all)
  sorted <- data.frame(cbind(sort(erbn(y)),sort(erb(y))))
  sorted$sd2 <- sorted$X2 - sorted$X1 
  colnames(sorted) <- c("Rbn","Rb","SD2")
  
  xyplot(Rbn + Rb ~ y, 
         data = sorted,
         type="l",
         auto.key = 
           list(x = 0, 
                y = 1, 
                text= c("Optimized (L)",
                        "Non-Optimized (L)",
                        "SD2 (R)"),
                border = FALSE, 
                points = FALSE,
                lines = TRUE),
         main=main,
         ylab=ylab_left,
         xlab=expression(bar(R)[n^textstyle("*")] ~~ plain(vs.) ~~ bar(R)),
         panel = function(x, ...){
           panel.xyplot(x,...)
           panel.abline(v=data$threshold,type="l",lty=3)
           if (show_prob) {
             grid.text(label = expression(paste(italic(Prob), 
                                                group("[",bar(R)[n^textstyle("*")] >= x,"]"),
                                                " > ",
                                                italic(Prob),
                                                group("[",bar(R) >= x,"]"))), 
                       x = unit(0, "npc") + unit(3, "mm"), 
                       y = unit(0, "npc") + unit(3, "mm"), 
                       just = "left",
                       gp=gpar(col=col_sd1))
           }
           if (show_config) {
             grid.text(label=data$test_config,
                       x = unit(1,"npc") - unit(3,"mm"),
                       y = unit(1,"npc") - unit(3,"mm"),
                       just="right")
           }
         }
  ) + as.layer(xyplot(SD2 ~ y,
                      data=sorted,
                      type="l",
                      lty=4,
                      col=col_sd2,
                      ylab=ylab_right,
                      panel = function(x, ...){
                        panel.xyplot(x,...)
                        panel.abline(h=0,type="l",lty=3)
                        grid.text(label = expression(italic(SD2) >= 0), 
                                  x = unit(1, "npc") - unit(3, "mm"), 
                                  y = unit(0, "npc") + unit(3, "mm"), 
                                  just = "right",
                                  gp=gpar(col=col_sd2)) 
                      }
  ),
               y.same=FALSE,
               outside=TRUE,
  )
}

  