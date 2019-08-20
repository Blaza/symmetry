#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param k the tuning parameter for the Laplace transform
#' @return The value of the test statistic given by the formula:
#'         <to be added>
#' @e(X)amples
#' set.seed(1)
#' X <- rnorm(50)
#' T1(X, 5)
#' @e(X)port
T1 <- function(X, k) {
  if (!is.numeric((X)) && !is.logical((X))) {
    warning("Argument is not numeric or logical: returning NA")
    return(NA)
  }
  T1_Cpp(X, k);
}
