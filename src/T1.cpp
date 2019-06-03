#include <Rcpp.h>
#include <cstdint>
#include <cmath>
#include <helpers.h>
using namespace Rcpp;


// [[Rcpp::export]]
double T1_Cpp(const NumericVector& X, double a) {
  int n = X.size();
  NumericVector aXs = abs(clone(X).sort());
  double TS_sum = 0;
  int i,j;
  double sqdiff;

  for (i = 0; i < n; i++) {
    TS_sum += ukn(i, n) * ukn(i, n) * 2.0/a;
  }

  for(i = 0; i < n; i++) {
    for(j = 0; j < i; j++) {
      // non diagonal are counted twice
      sqdiff = (aXs[i] - aXs[j]) * (aXs[i] - aXs[j]);
      TS_sum += 2 * ukn(i,n) * ukn(j,n) * 2 * a / (a*a + sqdiff);
    }
  }

  double TS_value = n * TS_sum;

  return TS_value;
}

