#include <Rcpp.h>
#include <cstdint>
#include <cmath>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export]]
double MK_Cpp(const NumericVector & X, double k) {
  int n = X.size();
  NumericVector Xs = clone(X).sort();

  double T2, T3;
  NumericVector T2_vec(2*n), T3_vec(2*n);
  NumericVector smaller1(2*n), smaller2(2*n);

  // We're using 1-indexing for j
  for (int j = k; j <= n - k + 1; j++) {
    // populate vector of indicators of smaller elements
    for (int i = 0; i < n; i++) {
      smaller1[i] = Xs[i] < Xs[j - 1] ? 1 : 0;
      smaller1[n + i] = -Xs[i] < Xs[j - 1] ? 1 : 0;
      smaller2[i] = Xs[i] < -Xs[j - 1] ? 1 : 0;
      smaller2[n + i] = -Xs[i] < -Xs[j - 1] ? 1 : 0;
    }

    T2 = Rf_choose(n - j, k) *
      Rf_choose(j - 1, k);

    T2_vec = T2_vec + T2 * smaller1;

    T3 = Rf_choose(n - j, k) *
      Rf_choose(j - 1, k);
    T3_vec = T3_vec + T3 * smaller2;
  }

  NumericVector D_vec = abs(T2_vec - T3_vec);

  double D_max = max(D_vec);
  return sqrt(n) * D_max / Rf_choose(n, 2 * k + 1);
}

