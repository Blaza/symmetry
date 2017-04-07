#include <Rcpp.h>
#include "combinations.h"
using namespace Rcpp;

//// [[Rcpp::export]]
//int chunk_sum(NumericVector kc, NumericVector kc1, NumericVector x){
  //int chunksize = kc.size();
  //int n = x.size();

  //int sum = 0;
  //int ind = 0;
  //int ind1 = 0;
  //for(int i = 0; i < n; i++){
    //for(int j = 0; j < chunksize; j++){
      //ind = kc[j] < x[i] ? 1 : 0;
      //ind1 = kc1[j] < x[i] ? 1 : 0;

      //sum += ind - ind1;
    //}
  //}

  //return sum;
//}

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
