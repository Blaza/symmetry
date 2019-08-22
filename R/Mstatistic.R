#' @template test_stat
#' @templateVar name The Mira test statistic
#' @templateVar cite Mira1999
#' @templateVar formula to be added
#' @export
M <- function(X) {
  if (!is.numeric((X)) && !is.logical((X))) {
    warning("Argument is not numeric or logical: returning NA")
    return(NA)
  }
  M_Cpp(X)
}
