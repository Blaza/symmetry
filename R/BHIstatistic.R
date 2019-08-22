#' @template test_stat
#' @templateVar name The Cabilio–Masaro test statistic
#' @templateVar cite Cabilio1996
#' @export
BHI <- function(X) {
  if (!is.numeric((X)) && !is.logical((X))) {
    warning("Argument is not numeric or logical: returning NA")
    return(NA)
  }
  BHI_Cpp(X)
}
