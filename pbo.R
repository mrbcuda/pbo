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
require(utils,quietly=TRUE)
require(PerformanceAnalytics,quietly=TRUE) # for performance measure

##### profile

INF_SUB = 6             # replace Inf with something for plotting
BAR_COLOR = "#cc99cc"   # plot bar fill color
LINE_COLOR = "#3366cc"  # plot line color
TEST_COLOR = "#339999"  # plot test line color

##### test setup t100
# populating the matrix M with samples from an installed distribution
# see also some reference tests in the tests file

N = 200                 # studies, alternative configurations
T = 3200                # sample returns
S = 8                   # partition count

M <- data.frame(matrix(NA,T,N,byrow=TRUE,dimnames=list(1:T,1:N)),
                check.names=FALSE)
for ( i in 1:N ) M[,i] <- rt(T,10) / 100


##### derived setup

CS = combn(S,S/2)       # combinations
SN = T / S              # partition size
test_config = bquote(N == .(N) ~~ T == .(T) ~~ S == .(S))

##### analyze


# initialize results lists
cs_results = list()

# for each partition combination
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
  # after R don't need to save J or J_bar so could skip this assignment
  J = M[is_indices,]
  J_bar = M[os_indices,]
  
  # compute Omega ratio over the N strategies in each subset
  # could use for R any summary statistic e.g. SharpeRatio
  R = mapply(Omega,J) # mapply(sharpe,J) 
  R_bar = mapply(Omega,J_bar) # mapply(sharpe,J_bar)
  
  # compute n* by argmax over R vector
  n_star = which.max(R)
  
  # rank of n*th result from OOS performance; converted to (0,1) interval
  omega_bar_c = rank(R_bar)[n_star] / length(R_bar)
  
  # logit
  # note the value can be Inf
  lambda_c = log(omega_bar_c / (1 - omega_bar_c))
  
  # save the results
  cs_results = rbind(cs_results,list(R,R_bar,n_star,omega_bar_c,lambda_c))
}

colnames(cs_results) <- c("R","R_bar","n*","omega_bar","lambda")
rownames(cs_results) <- 1:ncol(CS)

lambda = as.numeric(cs_results[,"lambda"])
lambda[which(lambda==Inf)] = INF_SUB

# probability of backtest overfit
# using MC test count approach of lambda count
phi = sum(ifelse(lambda<=0,1,0))/ncol(CS)
# -- alternative might use relative frequency sum 
# rf = as.data.frame( table(lambda) / ncol(CS), stringAsFactors=FALSE)
# phi = sum(ifelse(rf$lambda <= 0, rf$Freq, 0))
# -- alternative might use density fit
# d = density(lambda,kernel="rectangular")

# performance degradation
rn_pairs = as.data.frame(do.call(rbind,lapply(1:ncol(CS),function(i) {
  n = cs_results[[i,3]]
  r = cs_results[[i,1]]
  rb = cs_results[[i,2]]
  return(c(r[n],rb[n]))
})))
colnames(rn_pairs) <- c("Rn","Rbn")

# linear fit to pairs, extract results for plot annotations
linear_fit = lm(rn_pairs)
m = signif(as.numeric(linear_fit$coefficients[1]),digits=5) # slope
b = signif(as.numeric(linear_fit$coefficients[2]),digits=5) # intercept
ar2 = signif(summary(linear_fit)$adj.r.squared,digits=2) # adj R-squared
  
# probability R(OOS) < 1 for Omega, ratio of <1 to total
# we're using Omega so threshold 1 rather than 0 for Sharpe
threshold = 1

p_oos_lt1 = signif(length(which(rn_pairs$Rbn<threshold)) / 
                     nrow(rn_pairs),digits=3)

#################### GRAPHICS
  
require(lattice,quietly=TRUE)
require(grid,quietly=TRUE)

# plot rank logit with PBO annotation
histogram(lambda,
          xlim=c(-INF_SUB,INF_SUB),
          xlab=expression(lambda[c]),
          main="Rank Logits",
          type="density",
          panel = function(x, ...){
            panel.histogram(x, col=BAR_COLOR,...)
            panel.densityplot(x, col=LINE_COLOR, ...)
            panel.abline(v=0,type="l",lty=3)
            xa <- unit(0, "npc") + unit(2, "mm") 
            ya <- unit(1, "npc") - unit(2, "mm") 
            grid.text(label = bquote(PBO == .(round(phi,digits=3))), 
                      x = xa, 
                      y = ya, 
                      just = "left") 
            grid.text(label = test_config,
                      x = unit(1.0,"npc") - unit(2,"mm"),
                      y=ya,
                      just = "right")
            grid.text(label = "Less overfit",
                      x = unit(0.5,"npc") + unit(2,"mm"), 
                      y=ya,
                      just="left")
            grid.text(label = "More overfit",
                      x = unit(0.5,"npc") - unit(2,"mm"), 
                      y=ya,
                      just="right")          
          }
)

# performance degradation and probability of loss

# plot Rn pairs
cloud_span = c(signif(min(rn_pairs),-3),
               signif(max(rn_pairs),3)) # axis range

xyplot(rn_pairs$Rbn ~ rn_pairs$Rn,
       xlab = "R (IS)",
       ylab = "R (OOS)",
       main = "OOS Performance Degradation",
       xlim = cloud_span,
       ylim = cloud_span,
       panel = function(x, ...){
         panel.xyplot(x,col=BAR_COLOR,...)
         panel.lmline(x,col=LINE_COLOR,...)
         panel.abline(v=threshold,type="l",lty=3)
         panel.abline(h=threshold,type="l",lty=3)    
         panel.rug(x,col=BAR_COLOR,...)
         ya <- unit(1, "npc") - unit(3, "mm") 
         grid.text(label = bquote(R_OOS == .(b) (R_IS) + .(m) + err ~~ AdjR^2 == .(ar2)), 
                   x = unit(0, "npc") + unit(3, "mm"), 
                   y = ya, 
                   just = "left",
                   gp=gpar(col=LINE_COLOR)) 
         grid.text(label = test_config,
                   x = unit(1,"npc") - unit(3,"mm"),
                   y = ya,
                   just="right")
         if ( threshold == 1 ) { # ugly but ifelse won't work on bquote
           grid.text(label = bquote(P(R_OOS<1) ==  .(p_oos_lt1)),
                     x = unit(1, "npc") - unit(3, "mm"), 
                     y = ya - unit(10,"mm"), 
                     just = "right",
                     gp=gpar(col=BAR_COLOR)) 
         } else {
           grid.text(label = bquote(P(R_OOS<0) ==  .(p_oos_lt1)),
                     x = unit(1, "npc") - unit(3, "mm"), 
                     y = ya - unit(10,"mm"), 
                     just = "right",
                     gp=gpar(col=BAR_COLOR))
         }
       }
)

# stochastic dominance
# uses n* items from R-bar for one line, and all n items from R-bar for the other line
# create cumulative distribution functions for each data set, then generate samples to plot
y = seq(min(rn_pairs$Rbn),max(rn_pairs$Rbn),0.01) # reasonable R range for evaluation
erbn = ecdf(rn_pairs$Rbn) # optimized
erb = ecdf(sapply(1:ncol(CS),function(i) cs_results[[i,2]])) # non-optimized (all)
sorted = data.frame(cbind(sort(erbn(y)),sort(erb(y))))
sorted$sd2 = sorted$X2 - sorted$X1 
colnames(sorted) <- c("Rbn","Rb","SD2")

xyplot(Rbn + Rb ~ y, 
       data = sorted,
       type="l",
       auto.key = 
         list(x = 0, 
              y = 1, 
              text=c("Optimized","Non-Optimized"),
              border = FALSE, 
              points = FALSE,
              lines = TRUE),
       main="Stochastic Dominance",
       ylab="Frequency",
       xlab=expression(bar(R)[n^textstyle("*")] ~~ plain(vs.) ~~ bar(R)),
       panel = function(x, ...){
         panel.xyplot(x,...)
         panel.abline(v=threshold,type="l",lty=3)
         grid.text(label = expression(paste(italic(Prob), 
                                            group("[",bar(R)[n^textstyle("*")] >= x,"]"),
                                            " > ",
                                            italic(Prob),
                                            group("[",bar(R) >= x,"]"))), 
                   x = unit(1, "npc") - unit(3, "mm"), 
                   y = unit(0, "npc") + unit(3, "mm"), 
                   just = "right",
                   gp=gpar(col=LINE_COLOR)) 
         grid.text(label=test_config,
                   x = unit(1,"npc") - unit(3,"mm"),
                   y = unit(1,"npc") - unit(3,"mm"),
                   just="right")
       }
)

# optional: stochastic dominance with SD1 and SD2 overlaid
require(latticeExtra,quietly=TRUE)

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
       main="Stochastic Dominance",
       ylab="Frequency",
       xlab=expression(bar(R)[n^textstyle("*")] ~~ plain(vs.) ~~ bar(R)),
       panel = function(x, ...){
         panel.xyplot(x,...)
         panel.abline(v=threshold,type="l",lty=3)
         grid.text(label = expression(paste(italic(Prob), 
                                            group("[",bar(R)[n^textstyle("*")] >= x,"]"),
                                            " > ",
                                            italic(Prob),
                                            group("[",bar(R) >= x,"]"))), 
                   x = unit(0, "npc") + unit(3, "mm"), 
                   y = unit(0, "npc") + unit(3, "mm"), 
                   just = "left",
                   gp=gpar(col=LINE_COLOR)) 
         grid.text(label=test_config,
                   x = unit(1,"npc") - unit(3,"mm"),
                   y = unit(1,"npc") - unit(3,"mm"),
                   just="right")
       }
) + as.layer(xyplot(SD2 ~ y,
                    data=sorted,
                    type="l",
                    lty=4,
                    col=TEST_COLOR,
                    ylab="2nd Ord. Stochastic Dominance",
                    panel = function(x, ...){
                      panel.xyplot(x,...)
                      panel.abline(h=0,type="l",lty=3)
                      grid.text(label = expression(italic(SD2) >= 0), 
                                x = unit(1, "npc") - unit(3, "mm"), 
                                y = unit(0, "npc") + unit(3, "mm"), 
                                just = "right",
                                gp=gpar(col=TEST_COLOR)) 
                    }
),
             y.same=FALSE,
             outside=TRUE,
)


