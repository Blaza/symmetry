#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \sup_{t>0}\left|\frac{1}{n\choose{2}} \sum_{\mathcal{I}_{2}}
#'         \left( \frac12 I\{|X_{i_1}| <
#'         t\} + \frac12 I\{|X_{i_2}| <
#'         t\} - I\{|X_{(2),X_{i_1},X_{i_2}}| < t\}\right)\right| }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' BHK(X)
#' @export
BHK <- function(X) {
  BHK_Cpp(X)
}

