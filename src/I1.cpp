// Implementation of I1 test statistic using the super fast combinations
// algorithm provided by Howard Hinnant, as seen here:
// https://howardhinnant.github.io/combinations/combinations.html
#include <Rcpp.h>
#include "combinations.h"
using namespace Rcpp;
using std::int64_t;


class I1 {
    int64_t Tsum;
    const NumericVector aX;
    int k, i, n;
    double aXk, aXk1;
  public:
    explicit I1(const NumericVector& X, int _n, int _k)
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

    operator int64_t() const {return Tsum;}
};


// [[Rcpp::export]]
double I1_Cpp(const NumericVector& X, int k) {
    int n = X.size();
    NumericVector Xs = clone(X);
    std::sort(Xs.begin(), Xs.end());

    int64_t TS_sum = for_each_combination(Xs.begin(),
                                               Xs.begin() + 2*k,
                                               Xs.end(),
                                               I1(X, n, k));

    double TS_value = TS_sum / (n * Rf_choose(n, 2*k));

    return TS_value;
}

