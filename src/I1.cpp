// Implementation of I1 test statistic using the super fast combinations
// algorithm provided by Howard Hinnant, as seen here:
// https://howardhinnant.github.io/combinations/combinations.html
#include <Rcpp.h>
#include "combinations.h"
using namespace Rcpp;


class TSs {
    std::int64_t Tsum;
    const NumericVector aX;
    int k, i, n;
    double aXk, aXk1;
  public:
    explicit TSs(const NumericVector& X, int _n, int _k)
                : n(_n), aX(abs(X)), k(_k), Tsum(0) {}

    template <class It>
        bool operator()(It Xcomb, It end_ptr) { // called for each permutation
            aXk = std::abs(Xcomb[k-1]);
            aXk1 = std::abs(Xcomb[k]);

            for(i = 0; i < n; i++) {
                Tsum += aXk < aX[i];
                Tsum -= aXk1 < aX[i];
            }

            return false;  // Don't break out of the loop
        }

    operator std::int64_t() const {return Tsum;}
};


// [[Rcpp::export]]
double I1_Cpp(const NumericVector& X, int k) {
    int n = X.size();
    NumericVector Xs = clone(X);
    std::sort(Xs.begin(), Xs.end());

    std::int64_t TS_sum = for_each_combination(Xs.begin(),
                                               Xs.begin() + 2*k,
                                               Xs.end(),
                                               TSs(X, n, k));

    double TS_value = TS_sum / (n * Rf_choose(n, 2*k));

    return TS_value;
}
  //int n = x.size();

  //NumericVector xs = clone(x);
  //std::sort(xs.begin(), xs.end());

  //int sum = 0;

  //int j = 0;
  //double axi;
  //for(int i = 0; i < n; i++){
    //axi = std::abs(x[i]);
    //for(j = 0; j < chunksize; j++){
      //sum += (std::abs(xs[comb(j, k-1) - 1]) < axi);
      //sum -= (std::abs(xs[comb(j, k) - 1]) < axi);
    //}
  //}
