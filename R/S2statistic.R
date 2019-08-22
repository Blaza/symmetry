#' @template test_stat
#' @templateVar name Allison \eqn{T_2} statistic
#' @templateVar cite Klar2012
#' @export
S2 <- function(X, k) {
  if (!is.numeric((X)) && !is.logical((X))) {
    warning("Argument is not numeric or logical: returning NA")
    return(NA)
  }
  S2_Cpp(X, k);
}
