// Implementation of K1 test statistic using the super fast combinations
// algorithm provided by Howard Hinnant, as seen here:
// https://howardhinnant.github.io/combinations/combinations.html
#include <Rcpp.h>
#include <cstdint>
#include <cmath>
#include <algorithm>
#include <vector>
#include "combinations.h"
using namespace Rcpp;
using std::int64_t;


class K1 {
    std::vector<int64_t> sums;
    const NumericVector pts;
    int k, i, pn;
    double aXk, aXk1, mu;

  public:
    explicit K1(const NumericVector& _pts, int _pn, int _k, double _mu)
                : pn(_pn), k(_k), mu(_mu), pts(_pts), sums(_pn, 0) {}

    template <class It>
        bool operator()(It Xcomb, It end_ptr) { // called for each permutation
            aXk = std::abs(Xcomb[k-1] - mu);
            aXk1 = std::abs(Xcomb[k] - mu);

            for(i = 0; i < pn; i++) {
                sums[i] += aXk < pts[i];
                sums[i] -= aXk1 < pts[i];
            }

            return false;  // Don't break out of the loop
        }

    std::vector<int64_t> get_sums() { return sums; }
};


// [[Rcpp::export]]
double K1_Cpp(const NumericVector& X, int k, double mu) {
    int n = X.size();
    NumericVector Xs = clone(X);
    std::sort(Xs.begin(), Xs.end());

    NumericVector pts = abs(X); // potential points for maximum

    K1 TS = for_each_combination(Xs.begin(),
                                 Xs.begin() + 2*k,
                                 Xs.end(),
                                 K1(pts, n, k, mu));
    std::vector<std::int64_t> sums_stdvec = TS.get_sums();
    NumericVector sums(sums_stdvec.begin(), sums_stdvec.end());

    double max_sum = max(abs(sums));
    double TS_value = max_sum / Rf_choose(n, 2*k);

    return TS_value;
}

