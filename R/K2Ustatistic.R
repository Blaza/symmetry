#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \sup_{t>0}\frac{1}{ {n\choose 2}} \left|
#'         \sum_{1\leq i < j \leq n }
#'         I\{|X_i - X_j| < t\}- I\{|X_i + X_j| < t\}\right| }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' K2U(X)
#' @export
K2U <- function(X) {
  K2U_Cpp(X)
}
