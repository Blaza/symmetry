#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param mu the estimate of the location parameter
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \frac{1}{n {n\choose{2k}}} \sum_{\mathcal{I}_{2k}}
#'         \sum_{i_{2k+1}=1}^n I\{|X_{(k),X_{i_1},\ldots,X_{i_{2k}}} - \mu| <
#'         |X_{i_{2k+1}} - \mu|\}- I\{|X_{(k+1),X_{i_1},\ldots,X_{i_{2k}}} -
#'         \mu| < |X_{i_{2k+1}} - \mu|\} }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' CH(X, 2)
#' @export
CH <- function(X, mu = 0) {
  CH_Cpp(X - mu)
}
