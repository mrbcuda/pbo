
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
