#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param mu the estimate of the location parameter
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \frac{1}{n {n\choose{2}}} \sum_{\mathcal{I}_{2}}
#'         \sum_{i_{3}=1}^n \left( \frac12 I\{|X_{i_1} - \mu| <
#'         |X_{i_{3}} - \mu|\} + \frac12 I\{|X_{i_2} - \mu| <
#'         |X_{i_{3}} - \mu|\} - I\{|X_{(2),X_{i_1},X_{i_2}} -
#'         \mu| < |X_{i_{3}} - \mu|\}\right) }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' BHI(X)
#' X <- rnorm(50, 1)
#' BHI(X, 1)
#' @export
BHI <- function(X, mu = 0) {
  BHI_Cpp(X, mu)
}
