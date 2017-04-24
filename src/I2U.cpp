#include <Rcpp.h>
#include <cstdint>
#include <cmath>
using namespace Rcpp;
using std::int64_t;

// [[Rcpp::export]]
double I2U_Cpp(const NumericVector& X) {
    int n = X.size();
    int64_t TS_sum = 0;
    int i,j,k,l;
    double aXimXj, aXipXj, aXkpXl;
    for(i = 0; i < n; i++) {
        for(j = 0; j < i; j++) {
            aXimXj = std::abs(X[i] - X[j]);
            aXipXj = std::abs(X[i] + X[j]);

            for(k = 0; k < j; k++) {
                for(l = 0; l < k; l++) {
                    TS_sum += (aXimXj < X[k] + X[l]);
                    TS_sum -= (aXipXj < X[k] + X[l]);
                }
            }
        }
    }

    double TS_value = (double)TS_sum / Rf_choose(n, 4);

    return TS_value;
}

