#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @return The value of the test statistic given by the formula:
#'         < to be added >
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' CH(X)
#' @export
CH <- function(X) {
  CH_Cpp(X)
}
