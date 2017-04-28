#' Simulate the distribution of a test statistic
#'
#' Simulates the distribution of the specified test statistic under the given
#' null distribution.
#'
#' The dist argument is a list which must contain a field called "name" which
#' determines which distribution to use (e.g. "norm", "unif", "exp", etc.) and,
#' if needed, the parameters for the distribution. The name must be such that
#' the function "r"+name exists ("rnorm", "runif", "rexp", etc). Further
#' parameters are passed to that function.
#'
#' The TS argument is a list which must contain a field called "name" which
#' specifies which test statistic function to use for each sample. The name can
#' be "I1", "K1", "I2", "K2" for statistics implemented by us, or any other
#' statistic for which an R function exists (e.g. "mean", "var", etc.).
#'
#' @param N the number of simulations to do
#' @param n the sample size for each simulation
#' @param dist a list which specifies the null distribution (see details)
#' @param TS a list which specifies the test statistic to use (see details)
#' @return A vector of size N, each element being the value of the statistic
#'         TS on simulated samples of size n.
#' @examples
#' Tvalues(1000, 50, list(name='norm'), list(name='I1', k=2))
#' Tvalues(1000, 50, list(name='unif', min=-1, max=1), list(name='I2'))
#' Tvalues(1000, 50, list(name='logis', loc=0.5, sca=1), list(name='K1', k=2))
#' Tvalues(1000, 50, list(name='exp'), list(name='K2'))
#' @export
Tvalues <- function(N, n, dist=list(), TS=list()) {
  if(!is.list(dist) && is.character(dist))
    dist <- list(name = dist)

  rdist <- paste0("r", dist$name)
  distparams <- dist[ which(names(dist) != "name") ]

  samples <- matrix(do.call(rdist, c(N * n, distparams)), ncol = n)

  if(!is.list(TS) && is.character(TS))
    TS <- list(name = TS)

  TSparams <- TS[ which(names(TS) != "name") ]
  apply(samples, 1, function(x) do.call(TS$name, c(list(x), TSparams)))
}

#' Simulate the distribution of a test statistic in parallel
#'
#' This is just a parallel version of the \code{\link{Tvalues}} function, all
#' arguments apply for this function. See \code{\link{Tvalues}}.
#'
#' @inheritParams Tvalues
#' @param freecores how many cores to leave unused (0 for maximum use of cpu)
#' @param clust a cluster to use for parallel
#' @return A vector of size N, each element being the value of the statistic
#'         TS on simulated samples of size n.
#' @examples
#' parTvalues(1000, 50, list(name='norm'), list(name='I1', k=2))
#' parTvalues(1000, 50, list(name='unif', min=-1, max=1), list(name='I2'))
#' parTvalues(1000, 50, list(name='logis', loc=0.5), list(name='K1', k=2))
#' parTvalues(1000, 50, list(name='exp'), list(name='K2'))
#' @export
parTvalues <- function(N, n, dist=list(), TS=list(), freecores=0, clust=NULL) {
  if(!is.list(dist) && is.character(dist))
    dist <- list(name = dist)

  rdist <- paste0("r", dist$name)
  distparams <- dist[ which(names(dist) != "name") ]

  samples <- matrix(do.call(rdist, c(N * n, distparams)), ncol = n)

  if(!is.list(TS) && is.character(TS))
    TS <- list(name = TS)

  TSparams <- TS[ which(names(TS) != "name") ]

  no_cores <- detectCores() - freecores
  # Initiate cluster
  cl <- if(is.null(clust)) makeCluster(no_cores) else clust

  clusterExport(cl = cl, list("TSparams"), envir = environment())
  clusterEvalQ(cl, library(symmetry))

  Tvals <- parRapply(cl, samples,
                     function(x) do.call(TS$name, c(list(x), TSparams)))

  if(is.null(clust)) {
    stopCluster(cl)
  }

  Tvals
}

