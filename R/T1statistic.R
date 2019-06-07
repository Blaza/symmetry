#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param k the tuning parameter for the Laplace transform
#' @return The value of the test statistic given by the formula:
#'         <to be added>
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' T1(X, 5)
#' @export
T1 <- function(X, k) {
  T1_Cpp(X, k);
}

T1_R <- function(X, a) {
  n <- length(X)
  ks <- seq_along(X)
  ukn <- ((ks - 1)/(n-1))^(n-1) - ((n-ks)/(n-1))^(n-1)
  Xs <- sort(X)
  TS <- 0

  for (i in ks){
    for (j in ks) {
      TS <- TS + ukn[i]*ukn[j]*2*a/(a^2+(abs(Xs[j]) - abs(Xs[i]))^2)
    }
  }

  n*TS
}
