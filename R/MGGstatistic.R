#' @template test_stat
#' @templateVar name The Miao, Gel and Gastwirth test statistic
#' @templateVar cite Miao2006
#' @export
MGG <- function(X) {
  if (!is.numeric((X)) && !is.logical((X))) {
    warning("Argument is not numeric or logical: returning NA")
    return(NA)
  }
  MGG_Cpp(X)
}
