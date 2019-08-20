#' Calculate Kolmogorov Smirnov test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \sup_t\left|F_n(t)-(1-F_n(-t))\right| }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' KS(X)
#' @export
KS <- function(X) {
  if (!is.numeric((X)) && !is.logical((X))) {
    warning("Argument is not numeric or logical: returning NA")
    return(NA)
  }
  KS_Cpp(X)
}

#' Calculate Signed test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \frac1n \sum_{i=1}^nI\{X_i > 0\} - \frac12}
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' SGN(X)
#' @export
SGN <- function(X) {
  if (!is.numeric((X)) && !is.logical((X))) {
    warning("Argument is not numeric or logical: returning NA")
    return(NA)
  }
  SGN_Cpp(X)
}

#' Calculate Wilcoxon test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \frac{1}{{n\choose2}} \sum_{1\leq i<j\leq n}
#'         I\{X_i+X_j> 0\} - \frac12 }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' WCX(X)
#' @export
WCX <- function(X) {
  if (!is.numeric((X)) && !is.logical((X))) {
    warning("Argument is not numeric or logical: returning NA")
    return(NA)
  }
  WCX_Cpp(X)
}
