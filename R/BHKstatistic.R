#' Calculate _ test statistic (see 'Value' for formula)
#'
#' @param X the sample for which to calculate the statistic
#' @param k the value of parameter 'k' used in the formula
#' @param mu the estimate of the location parameter
#' @return The value of the test statistic given by the formula:
#'         \deqn{ \sup_{t>0}\left|\frac{1}{n\choose{2}} \sum_{\mathcal{I}_{2}}
#'         \left( \frac12 I\{|X_{i_1} - \mu| <
#'         t} + \frac12 I\{|X_{i_2} - \mu| <
#'         t\} - I\{|X_{(2),X_{i_1},X_{i_2}} -
#'         \mu| < t\}\right)\right| }
#' @examples
#' set.seed(1)
#' X <- rnorm(50)
#' BHK(X, 2)
#' X <- rnorm(50, 1)
#' BHK(X, 2, 1)
#' @export
BHK <- function(X, mu = 0) {BHK_Cpp(X - mu)
}

