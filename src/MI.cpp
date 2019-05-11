// Implementation of MI test statistic using the super fast combinations
// algorithm provided by Howard Hinnant, as seen here:
// https://howardhinnant.github.io/combinations/combinations.html
#include <Rcpp.h>
#include <cstdint>
#include <cmath>
#include <algorithm>
#include "combinations.h"
using namespace Rcpp;
using std::int64_t;


class MI {
    int64_t Tsum;
    const NumericVector X;
    int k, i, n;
  public:
    explicit MI(const NumericVector& _X, int _n, int _k)
                : X(_X), n(_n), k(_k), Tsum(0) {}

    template <class It>
        bool operator()(It Xcomb, It end_ptr) { // called for each combination
            for(i = 0; i < n; i++) {
              Tsum += (-Xcomb[k]) < X[i];
              Tsum -= Xcomb[k] < X[i];
            }

            return false;  // Don't break out of the loop
        }

    operator int64_t() const {return Tsum;}
};


// [[Rcpp::export]]
double MI_Cpp(const NumericVector& X, double k_in) {
    int n = X.size();
    NumericVector Xs = clone(X);
    std::sort(Xs.begin(), Xs.end());
    int k = std::round(k_in);
    int64_t TS_sum = for_each_combination(Xs.begin(),
                                          Xs.begin() + 2*k+1,
                                          Xs.end(),
                                          MI(X, n, k));

    double TS_value = TS_sum / (n * Rf_choose(n, 2*k+1)) * sqrt(n);

    return TS_value;
}
