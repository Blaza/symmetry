#' @template test_stat
#' @templateVar name The Miao, Gel and Gastwirth test statistic
#' @templateVar cite Miao2006
#' @templateVar formula \deqn{\begin{array}{rl} \frac{1}{n^4} \sum_{i,j,m,l=1}^n &\left(\frac{1}{k+|X_i - X_j|+|X_m - X_l|}-\frac{1}{k+|X_i - X_j|+|X_m + X_l|}\right. \\ &-\left.\frac{1}{k+|X_i + X_j|+|X_m - X_l|} +\frac{1}{k+|X_i + X_j|+|X_m + X_l|}\right)\end{array} }
#' @export
L2 <- function(X, k) {
  if (!is.numeric((X)) && !is.logical((X))) {
    warning("Argument is not numeric or logical: returning NA")
    return(NA)
  }
  L2_Cpp(X, k);
}
