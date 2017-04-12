#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \frac{1}{n^4} \sum\limits_{i,j,a,b=1}^n I\{|X_i - X_j| <
#'         X_a+X_b\}- I\{|X_i + X_j| < X_a+X_b\}}
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' I2(X)
#' @export
I2 <- function(X) {
  I2_Cpp(X);
}
