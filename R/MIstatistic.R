#' @template test_stat
#' @templateVar name The Mira test statistic
#' @templateVar cite Mira1999
#' @templateVar formula \deqn{ \frac{1}{n {n\choose{2k+1}}} \sum_{\mathcal{I}_{2k}} \sum_{i_{2k+1}=1}^n I\{-(X_{(k+1),X_{i_1},\ldots,X_{i_{2k}}}| < X_{i_{2k+1}}\}- I\{X_{(k+1),X_{i_1},\ldots,X_{i_{2k}}} < X_{i_{2k+1}}\} }
#' @export
MI <- function(X, k) {
  if (!is.numeric((X)) && !is.logical((X))) {
    warning("Argument is not numeric or logical: returning NA")
    return(NA)
  }
  MI_Cpp(X, k)
}
