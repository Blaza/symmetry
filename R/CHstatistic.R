#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @return The value of the test statistic given by the formula:
#'         < to be added >
#' @e(X)amples
#' set.seed(1)
#' X <- rnorm(50)
#' CH(X)
#' @e(X)port
CH <- function(X) {
  if (!is.numeric((X)) && !is.logical((X))) {
    warning("Argument is not numeric or logical: returning NA")
    return(NA)
  }
  CH_Cpp(X)
}
