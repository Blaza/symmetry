#include <Rcpp.h>
#include <cstdint>
#include <cmath>
using namespace Rcpp;
using std::int64_t;

// [[Rcpp::export]]
double I2HUA_Cpp(const NumericVector& X) {
    int n = X.size();
    int64_t TS_sum = 0;
    int i,j,k,l;
    short mult;
    double aXimXj, aXipXj, aXkpXl;
    for(i = 0; i < n; i++) {
        for(j = 0; j < i; j++) {
            aXimXj = std::abs(X[i] - X[j]);
            aXipXj = std::abs(X[i] + X[j]);

            for(k = 0; k < n; k++) {
                for(l = 0; l <= k; l++) {
                    // diagonal elements counted once and non diagonal - twice
                    mult = ( k == l ? 1 : 2 );
                    aXkpXl = std::abs(X[k] + X[l]);

                    TS_sum += mult * (aXimXj < aXkpXl);
                    TS_sum -= mult * (aXipXj < aXkpXl);
                }
            }
        }
    }

    double TS_value = (double)TS_sum / (n*n*Rf_choose(n,2));

    return TS_value;
}

