#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param k the value of parameter 'k' used in the formula
#' @param t the value with which to compare in indicator
#' @param H whether to calculate H or G
#' @return The value of the statistics given by the formula:
#'         \deqn{ H = \frac{1}{{n\choose{2k+1}}} \sum_{\mathcal{I}_{2k+1}}
#'         I\{-X_{(k+1),X_{i_1},\ldots,X_{i_{2k+1}}} < t\} }
#'         \deqn{ G = \frac{1}{{n\choose{2k+1}}} \sum_{\mathcal{I}_{2k+1}}
#'         I\{X_{(k+1),X_{i_1},\ldots,X_{i_{2k+1}}} < t\} }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' HG(X, 2)
#' @export
HG <- function(X, k, t, H = TRUE) {
  sqrt(n) * HG_Cpp(X, k, t, H)
}
