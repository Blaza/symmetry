#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param k the value of parameter 'k' used in the formula
#' @param mu the estimate of the location parameter
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \sup_{t>0}\left|\frac{1}{{n\choose{2k}}}
#'         \sum_{\mathcal{I}_{2k}} I\{-(X_{(k+1),X_{i_1},\ldots,X_{i_{2k}}}-\mu)
#'         < t\}- I\{X_{(k+1),X_{i_1},\ldots,X_{i_{2k}}}-\mu < t \}\right| }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' MK(X, 2)
#' X <- rnorm(50, 1)
#' MK(X, 2, 1)
#' @export
MK <- function(X, k, mu = 0) {
  MK_Cpp(X - mu, k)
}

