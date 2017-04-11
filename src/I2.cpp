#include <Rcpp.h>
#include <cstdint>
#include <cmath>
using namespace Rcpp;
using std::int64_t;

// [[Rcpp::export]]
double I2_Cpp(const NumericVector& X) {
    int n = X.size();
    int64_t TS_sum = 0;
    int i,j,k,l;
    short mult1, mult2;
    double aXimXj, aXipXj, aXkpXl;
    for(i = 0; i < n; i++) {
        for(j = 0; j <= i; j++) {
            // diagonal elements will be counted once and non diagonal - twice
            mult1 = ( i == j ? 1 : 2 );

            aXimXj = std::abs(X[i] - X[j]);
            aXipXj = std::abs(X[i] + X[j]);

            for(k = 0; k < n; k++) {
                for(l = 0; l <= k; l++) {
                    // diagonal elements counted once and non diagonal - twice
                    mult2 = ( k == l ? 1 : 2 );

                    TS_sum += mult1 * mult2 * (aXimXj < X[k] + X[l]);
                    TS_sum -= mult1 * mult2 * (aXipXj < X[k] + X[l]);
                }
            }
        }
    }

    double TS_value = (double)TS_sum / pow(n, 4);

    return TS_value;
}

