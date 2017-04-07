#include <Rcpp.h>
using namespace Rcpp;
using std::int64_t;

// [[Rcpp::export]]
double I2_Cpp(const NumericVector& X) {
    int n = X.size();
    int64_t TS_sum = 0;
    int i,j,k,l;
    double aXimXj, aXipXj;
    for(i = 0; i < n; i++) {
        for(j = 0; j < n; j++) {
            aXimXj = std::abs(X[i] - X[j]);
            aXipXj = std::abs(X[i] + X[j]);
            for(k = 0; j < n; j++) {
                for(l = 0; l < n; l++) {
                    TS_sum += aXimXj < X[k]+X[l];
                    TS_sum -= aXipXj < X[k]+X[l];
                }
            }
        }
    }

    Rcout << TS_sum <<std::endl;
    double TS_value = (double)TS_sum / pow(n, 4);

    return TS_value;
}

