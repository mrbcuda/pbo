
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
#' @param show_rug whether to show the scatter rug near the axes
#' @param show_grid whether to show the background grid
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
                            show_rug=TRUE,
                            show_grid=FALSE,
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
           if (show_grid) {
             panel.grid(-1,-1,col="lightgray",...)
           }
           panel.lmline(x,col=col_line,...)
           panel.abline(v=data$threshold,type="l",lty=3)
           panel.abline(h=data$threshold,type="l",lty=3)    
           if ( show_rug ) {
             panel.rug(x,col=col_bar,...)
           }
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

