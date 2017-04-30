#' Azzalini skew logistic distribution
#'
#' Generates random numbers from the skew logistic distribution
#'
#' @inheritParams sn::rsc
#' @return Vector of random numbers from Azzalini skew logistic distribution.
#' @export
rsl <- function(n=1, xi=0, omega=1, alpha=0, dp=NULL)
{
  if(!is.null(dp)) {
    if(!missing(alpha))
      stop("You cannot set both 'dp' and component parameters")
    xi <- dp[1]
    omega <- dp[2]
    alpha <- dp[3]
  }
  u1 <- rlogis(n)
  u2 <- rlogis(n)
  id <- (u2 > alpha*u1)
  u1[id] <- (-u1[id])
  z <- u1
  y <- xi+omega*z
  attr(y, "family") <- "SL"
  attr(y, "parameters") <- c(xi,omega,alpha)
  return(y)
}
