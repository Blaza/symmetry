#' Calculate Cabilio–Masaro test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @return The value of the test statistic given by the formula:
#'         <to be added>
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' CM(X)
#' @export
CM <- function(X) {
  CM_Cpp(X)
}
