#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \frac{1}{n\choose4} \sum_{1\leq i<j<a<b\leqn} I\{|X_i - X_j| <
#'         X_a+X_b\}- I\{|X_i + X_j| < X_a+X_b\}}
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' I2U(X)
#' @export
I2U <- function(X) {
  I2U_Cpp(X);
}
