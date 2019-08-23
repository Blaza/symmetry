asymptotic_distributions <- list(
  MOI = function(k) {
    sigma_k <- choose(2*k, k)^2 * (sqrt(pi) * gamma(k+1.5)*gamma(2*k+1) +
                                   2*gamma(2*k + 1.5) * (gamma(k+1.5) -
                                           sqrt(pi)*gamma(k+1))) /
      (2^(4*k-1)*(2*k+1)^2*gamma(k+1.5)*gamma(2*k+1.5))
    sigma <- sqrt((2 * k + 1)^2 * sigma_k)
    function(q) pnorm(q, 0, sigma)
  },
  M = pnorm,
  CM = pnorm,
  MGG = pnorm,
  MI = function(k) {
    sigma_k <- choose(2*k, k)^2 * (sqrt(pi) * gamma(k+1.5)*gamma(2*k+1) +
                                     2*gamma(2*k + 1.5) * (gamma(k+1.5) -
                                                             sqrt(pi)*gamma(k+1))) /
      (2^(4*k-1)*(2*k+1)^2*gamma(k+1.5)*gamma(2*k+1.5))
    sigma <- sqrt((2 * k + 1)^2 * sigma_k)
    function(q) pnorm(q, 0, sigma)
  }
)
