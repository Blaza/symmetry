// Implementation of NAI test statistic using the super fast combinations
// algorithm provided by Howard Hinnant, as seen here:
// https://howardhinnant.github.io/combinations/combinations.html
#include <Rcpp.h>
#include <cstdint>
#include <cmath>
#include <algorithm>
#include "combinations.h"
using namespace Rcpp;
using std::int64_t;


class NAI {
    int64_t Tsum;
    const NumericVector aX;
    int k, i, n;
    double aX1, aXk;
  public:
    explicit NAI(const NumericVector& X, int _n, int _k)
                : n(_n), aX(abs(X)), k(_k), Tsum(0) {}

    template <class It>
        bool operator()(It Xcomb, It end_ptr) { // called for each combination
            aX1 = std::abs(Xcomb[0]);
            aXk = std::abs(Xcomb[k-1]);

            for(i = 0; i < n; i++) {
                Tsum += aX1 < aX[i];
                Tsum -= aXk < aX[i];
            }

            return false;  // Don't break out of the loop
        }

    operator int64_t() const {return Tsum;}
};


// [[Rcpp::export]]
double NAI_Cpp(const NumericVector& X, int k) {
    int n = X.size();
    NumericVector Xs = clone(X);
    std::sort(Xs.begin(), Xs.end());

    int64_t TS_sum = for_each_combination(Xs.begin(),
                                          Xs.begin() + k,
                                          Xs.end(),
                                          NAI(X, n, k));

    double TS_value = TS_sum / (n * Rf_choose(n, k));

    return TS_value;
}

