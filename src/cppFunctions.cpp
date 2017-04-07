#include <Rcpp.h>
#include "combinations.h"
using namespace Rcpp;

// [[Rcpp::export]]
int chunk_sum(IntegerMatrix comb, NumericVector xs, NumericVector x, int k){
  int chunksize = comb.nrow();
  int n = x.size();

  int sum = 0;

  int j = 0;
  double axi;
  for(int i = 0; i < n; i++){
    axi = std::abs(x[i]);
    for(j = 0; j < chunksize; j++){
      sum += (std::abs(xs[comb(j, k-1) - 1]) < axi);
      sum -= (std::abs(xs[comb(j, k) - 1]) < axi);
    }
  }

  return sum;
}
