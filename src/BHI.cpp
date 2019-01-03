// Implementation of BHI test statistic using the super fast combinations
// algorithm provided by Howard Hinnant, as seen here:
// https://howardhinnant.github.io/combinations/combinations.html
#include <Rcpp.h>
#include <cstdint>
#include <cmath>
#include <algorithm>
#include "combinations.h"
#include <test_stats.h>
using namespace Rcpp;
using std::int64_t;


class BHI {
    double Tsum;
    const NumericVector aX;
    int i, n;
    double aX1, aX2;
  public:
    explicit BHI(const NumericVector& X, int _n)
                : n(_n), aX(abs(X)), Tsum(0) {}

    template <class It>
        bool operator()(It Xcomb, It end_ptr) { // called for each combination
            aX1 = std::abs(Xcomb[0]);
            aX2 = std::abs(Xcomb[1]);

            for(i = 0; i < n; i++) {
                Tsum += 0.5 * ((aX1 < aX[i]) - (aX2 < aX[i]));
            }

            return false;  // Don't break out of the loop
        }

    operator double() const {return Tsum;}
};


// [[Rcpp::export]]
double BHI_Cpp(const NumericVector& X) {
    int n = X.size();
    NumericVector Xs = clone(X);
    std::sort(Xs.begin(), Xs.end());

    double TS_sum = for_each_combination(Xs.begin(),
                                         Xs.begin() + 2,
                                         Xs.end(),
                                         BHI(X, n));

    double TS_value = TS_sum / (n * Rf_choose(n, 2));

    return TS_value;
}

