#' @export
K2 <- function(X) {
  sample_matrix <- K2_get_samples(X)

  X_minus <- sample_matrix['minus', ]
  X_plus <- sample_matrix['plus', ]

  m_ecdf <- ecdf(X_minus)
  p_ecdf <- ecdf(X_plus)

  pot_max_points <- unique(sample_matrix)

  max(abs(m_ecdf(pot_max_points) - p_ecdf(pot_max_points)))
}
