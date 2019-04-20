#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \sup_{t>0}\frac{1}{n^2} \left| \sum_{i,j=1}^n
#'         I\{|X_i - X_j| < t\}- I\{|X_i + X_j| < t\}\right| }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' K2(X)
#' @export
K2 <- function(X) {
  sample_matrix <- K2_get_samples(X)

  X_minus <- sample_matrix['minus', ]
  X_plus <- sample_matrix['plus', ]

  m_ecdf <- ecdf(X_minus)
  p_ecdf <- ecdf(X_plus)

  pot_max_points <- unique(as.vector(sample_matrix))

  n * max(abs(m_ecdf(pot_max_points) - p_ecdf(pot_max_points)))
}
