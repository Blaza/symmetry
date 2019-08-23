#' @useDynLib symmetry
#' @importFrom stats coef fitted model.matrix residuals rnorm rlogis
#' @importFrom utils tail
#' @importFrom Rdpack reprompt
#' @import parallel
#' @import Rcpp
#'
NULL

#' symmetry: A package which implements tests for symmetry of IID data, linear
#' models and GARCH models
#'
#' The package contains a large number of tests for symmetry (and their
#' bootstrap variants), which can be used to test the symmetry of IID samples or
#' of model residuals. Currently, the supported models are linear models and
#' GARCH models (fitted with the fGarch package). The tests are implemented
#' using Rcpp which ensures great performance.
#'
#' To see the available tests, see \link{TestStatistics}
#'
#' For documentation on how to perform the tests, see \link{symmetry_test}
#'
#' @docType package
#' @name symmetry
NULL
