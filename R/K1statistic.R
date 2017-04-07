K1_fun <- function(X, k, t, chunk_size) {
  n <- length(X)

  Xs <- sort(X)

  index_set <- iterpc::iterpc(n, 2*k)
  IS_iter <- iterpc::iter_wrapper(index_set, chunk_size)

  chunk_sums <- foreach(comb = IS_iter, .combine = c,
                        .packages = c("symmetry")) %do% {
    sum(abs(Xs[comb[ ,k]]) < t) - sum(abs(Xs[comb[ ,k+1]]) < t)
  }

  abs(sum(chunk_sums)) / choose(n, 2*k)
}

#' @export
K1 <- function(X, k, chunk_size=-1L) {
  n <- length(X)
  if(chunk_size < 0) chunk_size <- best_chunk_size(n, 2*k)

  fun <- function(t) K1_fun(X, k, t, chunk_size)

  aX <- sort(abs(X))
  # potential maximum points
  pts <- c(rbind(aX, aX + c(diff(aX)/2, 0)))

  max(sapply(pts,fun))
}

