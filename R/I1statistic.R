#' @export
I1 <- function(X, k, chunk_size=-1L) {
  n <- length(X)
  if(chunk_size < 0) chunk_size <- best_chunk_size(n, 2*k)
  Xs <- sort(X)

  indicator_set <- iterpc::iterpc(n, 2*k)
  IS_iter <- iterpc::iter_wrapper(indicator_set, chunk_size)

  inner_sums <- foreach(comb = IS_iter, .combine = c) %do% {
    k_col <- abs(Xs[comb[ ,k]])
    kp1_col <- abs(Xs[comb[ ,k+1]])

    k_col_mat <- matrix(rep(k_col, n), ncol = n)
    kp1_col_mat <- matrix(rep(kp1_col, n), ncol = n)

    abs_X_mat <- matrix(rep(abs(X), length(k_col)), ncol = n, byrow = TRUE)

    sum( k_col_mat < abs_X_mat ) - sum( kp1_col_mat < abs_X_mat )
  }

  sum(inner_sums) / (n * choose(n, 2*k))
}

#' @export
I1C <- function(X, k, chunk_size=-1L) {
  n <- length(X)
  if(chunk_size < 0) chunk_size <- best_chunk_size(n, 2*k)

  index_set <- iterpc::iterpc(n, 2*k)
  IS_iter <- iterpc::iter_wrapper(index_set, chunk_size)

  chunk_sums <- foreach(comb = IS_iter, .combine = c,
                        .packages = c("symmetry")) %do% {
    chunk_sum(comb, X, k)
  }

  sum(chunk_sums) / (n * choose(n, 2*k))
}

#' @export
I1C1 <- function(X, k) {
  I1_Cpp(X, k)
}
