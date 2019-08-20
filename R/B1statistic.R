#' Calculate \eqn{\sqrt{b_1}} test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @return The value of the test statistic given by the formula:
#' < to be added >
#' @e(X)amples
#' set.seed(1)
#' X <- rnorm(50)
#' B1(X)
#' @e(X)port
B1 <- function(X) {
  if (!is.numeric((X)) && !is.logical((X))) {
    warning("Argument is not numeric or logical: returning NA")
    return(NA)
  }
  B1_Cpp(X)
}
