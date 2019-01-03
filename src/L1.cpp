#include <Rcpp.h>
#include <cstdint>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
double L1_Cpp(const NumericVector& X, double a) {
    int n = X.size();
    double TS_sum = 0, sum_elem;
    int i,j,k,l;
    short mult, mult1, mult2;
    double aXimXj, aXipXj, aXkmXl, aXkpXl;
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

                    mult = mult1 * mult2;

                    aXkmXl = std::abs(X[k] - X[l]);
                    aXkpXl = std::abs(X[k] + X[l]);

                    sum_elem = 0;
                    sum_elem += 1 / (a + aXimXj + aXkmXl);
                    sum_elem += 1 / (a + aXipXj + aXkpXl);
                    sum_elem -= 1 / (a + aXimXj + aXkpXl);
                    sum_elem -= 1 / (a + aXipXj + aXkmXl);

                    TS_sum += mult * sum_elem;
                }
            }
        }
    }

    double TS_value = (double)TS_sum / pow(n, 4);

    return TS_value;
}

