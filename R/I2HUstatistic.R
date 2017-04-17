#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \frac{1}{n^2{n\choose2}} \sum_{1\leq i < j \leq n}\sum_{a,b=1}^n
#'         I\{|X_i - X_j| < X_a+X_b\}- I\{|X_i + X_j| < X_a+X_b\} }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' I2HU(X)
#' @export
I2HU <- function(X) {
  I2HU_Cpp(X);
}
