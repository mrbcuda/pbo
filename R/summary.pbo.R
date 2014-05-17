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