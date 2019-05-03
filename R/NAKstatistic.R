#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param k the value of parameter 'k' used in the formula
#' @param mu the estimate of the location parameter
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \sup_{t>0}\left|\frac{1}{{n\choose{k}}}
#'         \sum_{\mathcal{I}_{k}} I\{|X_{(1),X_{i_1},\ldots,X_{i_{k}}}-\mu|
#'         < t\}- I\{|X_{(k),X_{i_1},\ldots,X_{i_{k}}}-\mu| < t \}\right| }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' NAK(X, 2)
#' X <- rnorm(50, 1)
#' NAK(X, 2, 1)
#' @export
NAK <- function(X, k, mu = 0) {
  n <- length(X)
  sqrt(n) * NAK_Cpp(X - mu, k)
}

