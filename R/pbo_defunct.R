
#' Defunct - draws an annotated histogram of rank logits.
# #' @rdname pbo-defunct
#' @param ... all arguments ignored
#' @seealso 'histogram.pbo'
pbo_logit <- function(...) {
  .Defunct(new="histogram",package="pbo")
}

#' Defunct - draws an annotated dot plot of study selection sorted by selection frequency.
#' @param ... all arguments ignored
#' @seealso 'dotplot.pbo'
pbo_performance_dot <- function(...) {
  .Defunct(new="dotplot",package="pbo")
}
  
#' Defunct - draws a scatter XY plot showing selected study by CSCV case, with out-of-sample 
#' rank exceeding a given threshold.
#' @param ... all arguments ignored
#' @seealso 'xyplot.pbo'
pbo_performance_cases <- function(...) {
  .Defunct(new="xyplot",package="pbo",msg="plot type 'cscv'")
}

#' Defunct - draws an annotated plot of performance degradation and probability of loss.
#' @param ... all arguments ignored
#' @seealso 'xyplot.pbo'
pbo_degradation <- function(...) {
  .Defunct(new="xyplot",package="pbo",msg="plot type 'degradation'")
}

#' Defunct - draws an annotated plot of stochastic dominance.
#' @param ... all arguments ignored
#' @seealso 'xyplot.pbo'
pbo_dominance <- function(...) {
  .Defunct(new="xyplot",package="pbo",msg="plot type 'dominance'")
}
                          
#' Defunct - draws a scatter XY plot of study selection pairs with OOS performance 
#' exceeding a given threshold.
#' @param ... all arguments ignored
#' @seealso 'xyplot.pbo'
pbo_performance_pairs <- function(...) {
  .Defunct(new="xyplot",package="pbo",msg="plot type 'pairs'")
}

#' Defunct - draws a scatter XY plot showing OOS performance rank by CSCV case, 
#' with out-of-sample rank exceeding a given threshold.
#' @param ... all arguments ignored
#' @seealso 'xyplot.pbo'
pbo_performance_ranks <- function(...) {
  .Defunct(new="xyplot",package="pbo",msg="plot type 'ranks'")
}

#' Defunct - draws an annotated XY plot of study selection sorted by selection frequency.
#' @param ... all arguments ignored
#' @seealso 'xyplot.pbo'
pbo_performance_xy <- function(...) {
  .Defunct(new="xyplot",package="pbo",msg="plot type 'selection'")
}