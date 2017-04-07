#' @export
K1 <- function(X, k) {
  saX <- sort(abs(X))
  # potential maximum points
  pts <- c(saX[1], c(rbind(saX[-1] - diff(saX)/2, saX[-1])))

  K1_Cpp(X, pts, k)
}

