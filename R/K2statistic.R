#' @export
K2 <- function(X) {
  n <- length(X)
  combs <- iterpc::getall(iterpc::iterpc(n, 2, ordered = TRUE, replace = TRUE))
  print(dim(combs))
  Xminus <- abs(X[combs[ , 1]] - X[combs[ , 2]])
  Xplus <- abs(X[combs[ , 1]] + X[combs[ , 2]])

  m_ecdf <- ecdf(Xminus)
  p_ecdf <- ecdf(Xplus)

  min_t <- min(c(Xminus, Xplus))
  max_t <- max(c(Xminus, Xplus))

  optimize(function(t) abs(m_ecdf(t) - p_ecdf(t)),
           c(max(0, min_t), max_t), maximum = TRUE)$objective
}
