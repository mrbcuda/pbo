
#' Draws an annotated plot of stochastic dominance.
#' @param data a list as returned by pbo()
#' @param main plot title, default 'Stochastic Dominance'
#' @param ylab_left Y-axis label for 1st order
#' @param ylab_right Y-axis lable for 2nd order
#' @param col_sd1 line color for 1st order
#' @param col_sd2 line color for 2nd order
#' @param increment step increment value for the cumulative distribution estimation
#' @param show_config whether to show the study dimension annotations, default TRUE
#' @param show_prob whether to show the probability equation annotation, default TRUE
#' @param show_grid whether to show the background grid
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
                          show_grid=FALSE,
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
           if (show_grid) {
             panel.grid(-1,-1,col="lightgray",...)
           }
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
         },
         ...
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

  