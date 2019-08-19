#include <Rcpp.h>
#include <cstdint>
#include <cmath>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export]]
double MI_Cpp(const NumericVector & X, double k) {
  int n = X.size();
  NumericVector Xs = clone(X).sort();
  NumericVector mXs = -Xs;

  IntegerVector larger_than_minus(n);
  IntegerVector smaller_than_plus(n);

  for (int i = 0; i < n; i++) {
    larger_than_minus[i] = sum(Xs[i] > mXs);
    smaller_than_plus[i] = sum(Xs[i] < Xs);
  }

  double T = 0;

  // We're using 1-indexing
  for (int j = k; j <= n - k + 1; j++) {
    T += (larger_than_minus[j - 1] - smaller_than_plus[j - 1]) *
      Rf_choose(n - j, k) *
      Rf_choose(j - 1, k);
  }

  return sqrt(n) * T / (n * Rf_choose(n, 2 * k + 1));
}

