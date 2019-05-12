#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param mu the estimate of the location parameter
#' @return The value of the test statistic given by the formula:
#'         < to be added >
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' CH(X, 2)
#' @export
CH <- function(X, mu = 0) {
  CH_Cpp(X - mu)
}
