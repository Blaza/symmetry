// Implementation of NAK test statistic using the super fast combinations
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


class NAK {
    std::vector<int64_t> sums;
    const NumericVector pts;
    int k, i, pn;
    double aX1, aXk;

  public:
    explicit NAK(const NumericVector& _pts, int _pn, int _k)
                : pn(_pn), k(_k), pts(_pts), sums(_pn, 0) {}

    template <class It>
        bool operator()(It Xcomb, It end_ptr) { // called for each permutation
            aX1 = std::abs(Xcomb[0]);
            aXk = std::abs(Xcomb[k-1]);

            for(i = 0; i < pn; i++) {
                sums[i] += aX1 < pts[i];
                sums[i] -= aXk < pts[i];
            }

            return false;  // Don't break out of the loop
        }

    std::vector<int64_t> get_sums() { return sums; }
};


// [[Rcpp::export]]
double NAK_Cpp(const NumericVector& X, int k) {
    int n = X.size();
    NumericVector Xs = clone(X);
    std::sort(Xs.begin(), Xs.end());

    NumericVector pts = abs(X); // potential points for maximum

    NAK TS = for_each_combination(Xs.begin(),
                                  Xs.begin() + k,
                                  Xs.end(),
                                  NAK(pts, n, k));
    std::vector<std::int64_t> sums_stdvec = TS.get_sums();
    NumericVector sums(sums_stdvec.begin(), sums_stdvec.end());

    double max_sum = max(abs(sums));
    double TS_value = max_sum / Rf_choose(n, k);

    return TS_value;
}

