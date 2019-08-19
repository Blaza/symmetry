#' Calculate Mira test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @return The value of the test statistic given by the formula:
#' < to be added >
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' M(X)
#' @export
M <- function(X) {
  M_Cpp(X)
}
