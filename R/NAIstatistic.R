#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param k the value of parameter 'k' used in the formula
#' @param mu the estimate of the location parameter
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \frac{1}{n {n\choose{k}}} \sum_{\mathcal{I}_{k}}
#'         \sum_{i_{k+1}=1}^n I\{|X_{(1),X_{i_1},\ldots,X_{i_{k}}} - \mu| <
#'         |X_{i_{k+1}} - \mu|\}- I\{|X_{(k),X_{i_1},\ldots,X_{i_{k}}} -
#'         \mu| < |X_{i_{k+1}} - \mu|\} }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' NAI(X, 2)
#' X <- rnorm(50, 1)
#' NAI(X, 2, 1)
#' @export
NAI <- function(X, k, mu = 0) {
  NAI_Cpp(X - mu, k)
}
