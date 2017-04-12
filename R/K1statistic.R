#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param k the value of parameter 'k' used in the formula
#' @return The value of the test statistic given by the formula:
#'         \eqn{ \sup\limits_{t>0}\left|\frac{1}{\binom{n}{2k}}
#'         \sum\limits_{\mathcal{I}_{2k}} I\{|X_{(k),X_{i_1},\ldots,X_{i_{2k}}}|
#'         < t\}- I\{|X_{(k+1),X_{i_1},\ldots,X_{i_{2k}}}| < t \right|\} }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' K1(X, 2)
#' @export
K1 <- function(X, k) {
  K1_Cpp(X, k)
}

