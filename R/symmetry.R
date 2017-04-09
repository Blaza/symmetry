#' @useDynLib symmetry
#' @importFrom Rcpp sourceCpp
#' @import parallel


#' @export
Tvalues <- function(N, n, dist=list(), TS=list()) {
  rdist <- paste0("r", dist$name)
  distparams <- dist[ which(names(dist) != 'name') ]

  samples <- matrix(do.call(rdist, c(N * n, distparams)), ncol = n)

  TSparams <- TS[ which(names(TS) != 'name') ]
  apply(samples, 1, function(x) do.call(TS$name, c(list(X = x), TSparams)))
}

#' @export
parTvalues <- function(N, n, dist=list(), TS=list(), freecores=0) {
  rdist <- paste0("r", dist$name)
  distparams <- dist[ which(names(dist) != 'name') ]

  samples <- matrix(do.call(rdist, c(N * n, distparams)), ncol = n)

  TSparams <- TS[ which(names(TS) != 'name') ]

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
