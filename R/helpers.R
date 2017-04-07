best_chunk_size <- function(n, k) {
  nCk <- choose(n, k)
  ifelse(nCk > 2e5, round(nCk / 5), nCk)
}

#' @export
test_power <- function(t0, t1, alpha=0.05) {
  1 - ecdf(t1)(quantile(t0, 1-alpha))
}

