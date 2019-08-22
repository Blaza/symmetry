#' Available test statistics for symmetry tests
#'
#' The list of implemented test statistics and their functions
#'
#' Below is a list of the implemented test statistics in the package. Each
#' statistic is listed by it's name, a code string (e.g. 'B1', CM','MOI') and
#' the formula of the statistic which is evaluated. The code string is used as
#' an argument to the \link{symmetry_test} function. Some statistics depend on a
#' parameter 'k' which can be seen from the formulas and is also passed as an
#' argument.
#'
#' Each statistic is implemented as a function with the same name as the code
#' string, so the name of the function is passed as the argument "stat" to the
#' \link{symmetry_test} function
#'
#' @param X the numeric vector for which to calculate the test statistic
#' @param k the 'k' parameter in the formula (if applicable)
#' @return The value of the test statistic.
#' @references \insertAllCited{}
#' @aliases TestStatistics
#' @name TestStatistics
NULL
