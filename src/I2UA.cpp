#include <Rcpp.h>
#include <cstdint>
#include <cmath>
using namespace Rcpp;
using std::int64_t;

// [[Rcpp::export]]
double I2UA_Cpp(const NumericVector& X) {
    int n = X.size();
    int64_t TS_sum = 0;
    int i,j,a,b;
    double aXimXj, aXipXj, aXapXb;
    for(i = 0; i < n; i++) {
        for(j = i+1; j < n; j++) {
            aXimXj = std::abs(X[i] - X[j]);
            aXipXj = std::abs(X[i] + X[j]);

            for(a = j+1; a < n; a++) {
                for(b = a+1; b < n; b++) {
                    aXapXb = std::abs(X[a] + X[b]);
                    TS_sum += (aXimXj < aXapXb);
                    TS_sum -= (aXipXj < aXapXb);
                }
            }
        }
    }

    double TS_value = (double)TS_sum / Rf_choose(n, 4);

    return TS_value;
}

