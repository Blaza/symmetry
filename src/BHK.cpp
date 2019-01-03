// Implementation of BHK test statistic using the super fast combinations
// algorithm provided by Howard Hinnant, as seen here:
// https://howardhinnant.github.io/combinations/combinations.html
#include <Rcpp.h>
#include <cstdint>
#include <cmath>
#include <algorithm>
#include <vector>
#include "combinations.h"
#include <test_stats.h>
using namespace Rcpp;
using std::int64_t;


class BHK {
    std::vector<double> sums;
    const NumericVector pts;
    int k, i, pn;
    double aX1, aX2;

  public:
    explicit BHK(const NumericVector& _pts, int _pn)
                : pn(_pn), pts(_pts), sums(_pn, 0) {}

    template <class It>
        bool operator()(It Xcomb, It end_ptr) { // called for each permutation
            aX1 = std::abs(Xcomb[0]);
            aX2 = std::abs(Xcomb[1]);

            for(i = 0; i < pn; i++) {
              sums[i] += 0.5 * ((aX1 < pts[i]) - (aX2 < pts[i]));
            }


            return false;  // Don't break out of the loop
        }

    std::vector<double> get_sums() { return sums; }
};


// [[Rcpp::export]]
double BHK_Cpp(const NumericVector& X) {
    int n = X.size();
    NumericVector Xs = clone(X);
    std::sort(Xs.begin(), Xs.end());

    NumericVector pts = abs(X); // potential points for maximum

    BHK TS = for_each_combination(Xs.begin(),
                                  Xs.begin() + 2,
                                  Xs.end(),
                                  BHK(pts, n));
    std::vector<double> sums_stdvec = TS.get_sums();
    NumericVector sums(sums_stdvec.begin(), sums_stdvec.end());

    double max_sum = max(abs(sums));
    double TS_value = max_sum / Rf_choose(n, 2);

    return TS_value;
}

