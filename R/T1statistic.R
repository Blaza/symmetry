#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param k the tuning parameter for the Laplace transform
#' @return The value of the test statistic given by the formula:
#'         <to be added>
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' T1(X, 5)
#' @export
T1 <- function(X, k) {
  T1_Cpp(X, k);
}
