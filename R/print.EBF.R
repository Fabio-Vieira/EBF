#' @title print.EBF generic function to print the results of the EBF function
#'
#' @param x an object of class EBF
#' @param ... Additional arguments passed to or from other methods.
#' @method print EBF
#'
#' @export
print.EBF <- function(x, ...){
  cat("----------------Empirical Bayes factor----------------")
  cat("\n")
  cat("\n")
  if(x$integral){
    cat("EBF computed using", x$samples, "samples to approximate the denominator")
  }
  cat("\n")
  cat("\n")
  print(x$EBF)
  cat("\n")
  cat("\n")
  cat("Model contained", x$j, "groups and", x$q, "random effects.")
}
