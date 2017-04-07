#' @useDynLib symmetry
#' @importFrom Rcpp sourceCpp
#' @import parallel
#' @import RcppDE


#' @export
Tvalues <- function(N, n, distname, distparams=list(),
                    TSname, TSparams=list()) {

  rdist <- paste0("r", distname)

  samples <- matrix(do.call(rdist, c(N * n, distparams)), ncol = n)

  apply(samples, 1, function(x) do.call(TSname, c(list(X = x), TSparams)))
}

#' @export
parTvalues <- function(N, n, distname, distparams=list(),
                    TSname, TSparams=list(), freecores=0) {

  rdist <- paste0("r", distname)

  samples <- matrix(do.call(rdist, c(N * n, distparams)), ncol = n)

  no_cores <- detectCores() - freecores
  # Initiate cluster
  cl <- makeCluster(no_cores)

  clusterExport(cl = cl, list("TSparams"), envir = environment())
  clusterEvalQ(cl, library(iterpc))
  clusterEvalQ(cl, library(foreach))
  clusterEvalQ(cl, library(symmetry))

  Tvals <- parRapply(cl, samples,
                     function(x) do.call(TSname, c(list(X = x), TSparams)))

  stopCluster(cl)

  Tvals
}
