// Implementation of MOI test statistic using the super fast combinations
// algorithm provided by Howard Hinnant, as seen here:
// https://howardhinnant.github.io/combinations/combinations.html
#include <Rcpp.h>
#include <cstdint>
#include <cmath>
#include <algorithm>
#include "combinations.h"
using namespace Rcpp;
using std::int64_t;


class MOI {
    int64_t Tsum;
    const NumericVector aX;
    int k, i, n;
    double aXk, aXk1;
  public:
    explicit MOI(const NumericVector& X, int _n, int _k)
                : n(_n), aX(abs(X)), k(_k), Tsum(0) {}

    template <class It>
        bool operator()(It Xcomb, It end_ptr) { // called for each combination
            aXk = std::abs(Xcomb[k-1]);
            aXk1 = std::abs(Xcomb[k]);

            for(i = 0; i < n; i++) {
                Tsum += aXk < aX[i];
                Tsum -= aXk1 < aX[i];
            }

            return false;  // Don't break out of the loop
        }

    operator int64_t() const {return Tsum;}
};


// [[Rcpp::export]]
double MOI_Cpp(const NumericVector& X, double k_in) {
    int n = X.size();
    NumericVector Xs = clone(X);
    std::sort(Xs.begin(), Xs.end());
    int k = std::round(k_in);
    int64_t TS_sum = for_each_combination(Xs.begin(),
                                          Xs.begin() + 2*k,
                                          Xs.end(),
                                          MOI(X, n, k));

    double TS_value = TS_sum / (n * Rf_choose(n, 2*k)) * sqrt(n);

    return TS_value;
}

