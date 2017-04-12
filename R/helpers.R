#' Calculate the power of a test
#'
#' This function calculates the power of a test given the null and alternative
#' T values and the significance level.
#'
#' @param t0 the vector of null T values
#' @param t1 the vector of alternative T values
#' @param alpha the significance level
#' @export
test_power <- function(t0, t1, alpha=0.05) {
  1 - ecdf(t1)(quantile(t0, 1-alpha))
}

