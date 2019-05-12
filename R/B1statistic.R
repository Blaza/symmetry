#' Calculate \eqn{\sqrt{b_1}} test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param mu the estimate of the location parameter
#' @return The value of the test statistic given by the formula:
#' < to be added >
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' B1(X, 2)
#' @export
B1 <- function(X, mu = 0) {
  B1_Cpp(X - mu)
}
