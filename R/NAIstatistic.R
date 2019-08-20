#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param k the value of parameter 'k' used in the formula
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \frac{1}{n {n\choose{k}}} \sum_{\mathcal{I}_{k}}
#'         \sum_{i_{k+1}=1}^n I\{|X_{(1),X_{i_1},\ldots,X_{i_{k}}}| <
#'         |X_{i_{k+1}}|\}- I\{|X_{(k),X_{i_1},\ldots,X_{i_{k}}}| < |X_{i_{k+1}}|\} }
#' @e(X)amples
#' set.seed(1)
#' X <- rnorm(50)
#' NAI(X, 2)
#' @e(X)port
NAI <- function(X, k) {
  if (!is.numeric((X)) && !is.logical((X))) {
    warning("Argument is not numeric or logical: returning NA")
    return(NA)
  }
  NAI_Cpp(X, k)
}
