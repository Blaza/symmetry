// Implementation of MK test statistic using the super fast combinations
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


class MK {
    std::vector<int64_t> sums;
    const NumericVector pts;
    int k, i, pn;
    double aXk, aXk1;

  public:
    explicit MK(const NumericVector& _pts, int _pn, int _k)
                : pn(_pn), k(_k), pts(_pts), sums(_pn, 0) {}

    template <class It>
        bool operator()(It Xcomb, It end_ptr) { // called for each permutation
            for(i = 0; i < pn; i++) {
              sums[i] += (-Xcomb[k]) < pts[i];
              sums[i] -= Xcomb[k] < pts[i];
            }

            return false;  // Don't break out of the loop
        }

    std::vector<int64_t> get_sums() { return sums; }
};


// [[Rcpp::export]]
double MK_Cpp(const NumericVector& X, double k_in) {
    int n = X.size();
    NumericVector Xs = clone(X);
    std::sort(Xs.begin(), Xs.end());
    int k = std::round(k_in);

    NumericVector pts = X; // potential points for maximum

    MK TS = for_each_combination(Xs.begin(),
                                 Xs.begin() + 2*k+1,
                                 Xs.end(),
                                 MK(pts, n, k));
    std::vector<std::int64_t> sums_stdvec = TS.get_sums();
    NumericVector sums(sums_stdvec.begin(), sums_stdvec.end());

    double max_sum = max(abs(sums));
    double TS_value = max_sum / Rf_choose(n, 2*k+1) * sqrt(n);

    return TS_value;
}

