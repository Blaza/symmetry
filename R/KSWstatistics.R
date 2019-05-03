#' Calculate Kolmogorov Smirnov test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param mu the estimate of the location parameter
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \sup_t\left|F_n(t+\mu)-(1-F_n(\mu-t))\right| }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' KS(X)
#' @export
KS <- function(X, mu = 0) {
  # using [['D']] to get a number instead of a named vector
  KS_Cpp(X-mu)
}

#' Calculate Signed test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param mu the estimate of the location parameter
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \frac1n \sum_{i=1}^nI\{X_i - \mu > 0\} - \frac12}
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' SGN(X)
#' @export
SGN <- function(X, mu = 0) {
  SGN_Cpp(X - mu)
}

#' Calculate Wilcoxon test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param mu the estimate of the location parameter
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \frac{1}{{n\choose2}} \sum_{1\leq i<j\leq n}
#'         I\{X_i+X_j - 2\mu> 0\} - \frac12 }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' WCX(X)
#' @export
WCX <- function(X, mu = 0) {
  WCX_Cpp(X - mu)
}
