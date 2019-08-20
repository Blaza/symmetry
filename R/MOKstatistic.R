#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param k the value of parameter 'k' used in the formula
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \sup_{t>0}\left|\frac{1}{{n\choose{2k}}}
#'         \sum_{\mathcal{I}_{2k}} I\{|X_{(k),X_{i_1},\ldots,X_{i_{2k}}}|
#'         < t\}- I\{|X_{(k+1),X_{i_1},\ldots,X_{i_{2k}}}| < t \}\right| }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' MOK(X, 2)
#' @export
MOK <- function(X, k) {
  if (!is.numeric((X)) && !is.logical((X))) {
    warning("Argument is not numeric or logical: returning NA")
    return(NA)
  }
  MOK_Cpp(X, k)
}

