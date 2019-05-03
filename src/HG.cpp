// Implementation of HG test statistic using the super fast combinations
// algorithm provided by Howard Hinnant, as seen here:
// https://howardhinnant.github.io/combinations/combinations.html
#include <Rcpp.h>
#include <cstdint>
#include <cmath>
#include <algorithm>
#include "combinations.h"
using namespace Rcpp;
using std::int64_t;


class HG {
    int64_t Tsum;
    const NumericVector X;
    int k, i, n;
    double t;
    bool H;
  public:
    explicit HG(const NumericVector& _X, int _n, int _k, double _t, bool _H)
                : n(_n), X(_X), k(_k), Tsum(0), t(_t), H(_H) {}

    template <class It>
        bool operator()(It Xcomb, It end_ptr) { // called for each combination
            Tsum += ( (H ? -1 : 1) * Xcomb[k] < t);

            return false;  // Don't break out of the loop
        }

    operator int64_t() const {return Tsum;}
};


// [[Rcpp::export]]
double HG_Cpp(const NumericVector& X, int k, double t, bool H) {
    int n = X.size();
    NumericVector Xs = clone(X);
    std::sort(Xs.begin(), Xs.end());

    int64_t TS_sum = for_each_combination(Xs.begin(),
                                          Xs.begin() + 2*k+1,
                                          Xs.end(),
                                          HG(X, n, k, t, H));

    double TS_value = TS_sum / Rf_choose(n, 2*k+1);

    return sqrt(n) * TS_value;
}

