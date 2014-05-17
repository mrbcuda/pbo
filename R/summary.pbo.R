#' @title Summary of backtest overfitting analysis
#' @description Provides a fitted summary of the results in the \code{pbo} object.
#' @details The summary reports the performance evaluation function name and 
#' its threshold level.  The report then extracts from the PBO object and prints
#' the probability of backtest overfit \code{p_bo}, the slope of the performance
#' degration linear regression \code{slope}, the adjusted R-squared of that
#' regression \code{ar^2}, and the proportion of cases leading to losses
#' \code{p_loss}.
#' @param object an object of class \code{pbo} usually acquired by  
#' running \code{\link[pbo]{pbo}}.
#' @param ... additional parameters ignored
#' @keywords probability backtest overfitting PBO summary
#' @seealso pbo
#' @export
summary.pbo <- function(object,...) {
  writeLines(c(paste("Performance function",
                     object$func,
                     "with threshold",
                     object$threshold),
               ""))
  results <- c(object$phi,object$slope,object$ar2,object$below_threshold)
  names(results) <- c("p_bo","slope","ar^2","p_loss")
  results
}