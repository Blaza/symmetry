#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param a the tuning parameter for the Laplace transform
#' @return The value of the test statistic given by the formula: \deqn{
#'   \frac{1}{n^4} \sum_{i,j,k,l=1}^n \frac{1}{a+|X_i - X_j|+|X_k -
#'   X_l|}-\frac{1}{a+|X_i - X_j|+|X_k + X_l|} - \frac{1}{a+|X_i + X_j|+|X_k -
#'   X_l|} +\frac{1}{a+|X_i + X_j|+|X_k + X_l|} }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' L1(X)
#' @export
L1 <- function(X, a) {
  L1_Cpp(X, a);
}
