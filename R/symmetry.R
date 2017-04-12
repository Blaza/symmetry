#' @useDynLib symmetry
#' @importFrom Rcpp sourceCpp
#' @import parallel


#' @export
Tvalues <- function(N, n, dist=list(), TS=list()) {
  if(!is.list(dist) && is.character(dist))
    dist <- list(name = dist)

  rdist <- paste0("r", dist$name)
  distparams <- dist[ which(names(dist) != 'name') ]

  samples <- matrix(do.call(rdist, c(N * n, distparams)), ncol = n)

  if(!is.list(TS) && is.character(TS))
    TS <- list(name = TS)

  TSparams <- TS[ which(names(TS) != 'name') ]
  apply(samples, 1, function(x) do.call(TS$name, c(list(x), TSparams)))
}

#' @export
parTvalues <- function(N, n, dist=list(), TS=list(), freecores=0) {
  if(!is.list(dist) && is.character(dist))
    dist <- list(name = dist)

  rdist <- paste0("r", dist$name)
  distparams <- dist[ which(names(dist) != 'name') ]

  samples <- matrix(do.call(rdist, c(N * n, distparams)), ncol = n)

  if(!is.list(TS) && is.character(TS))
    TS <- list(name = TS)

  TSparams <- TS[ which(names(TS) != 'name') ]

  no_cores <- detectCores() - freecores
  # Initiate cluster
  cl <- makeCluster(no_cores)

  clusterExport(cl = cl, list("TSparams"), envir = environment())
  clusterEvalQ(cl, library(symmetry))

  Tvals <- parRapply(cl, samples,
                     function(x) do.call(TS$name, c(list(x), TSparams)))

  stopCluster(cl)

  Tvals
}
