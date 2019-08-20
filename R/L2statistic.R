#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param k the tuning parameter for the Laplace transform
#' @return The value of the test statistic given by the formula: \deqn{\begin{array}{rl}
#'   \frac{1}{n^4} \sum_{i,j,k,l=1}^n &\left(\frac{1}{k+|X_i - X_j|+|X_k -
#'   X_l|}-\frac{1}{k+|X_i - X_j|+|X_k + X_l|}\right. \\ &-\left.\frac{1}{k+|X_i + X_j|+|X_k -
#'   X_l|} +\frac{1}{k+|X_i + X_j|+|X_k + X_l|}\right)\end{array} }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' L2(X, 5)
#' @export
L2 <- function(X, k) {
  if (!is.numeric((X)) && !is.logical((X))) {
    warning("Argument is not numeric or logical: returning NA")
    return(NA)
  }
  L2_Cpp(X, k);
}
