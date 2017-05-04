#include <Rcpp.h>
#include <cstdint>
#include <algorithm>
#include <cmath>
using namespace Rcpp;
using std::int64_t;

// [[Rcpp::export]]
double I2UAS_Cpp(const NumericVector& X) {
    int n = X.size();
    int64_t TS_sum = 0;
    int i,j,a,b;
    double Xijab[4];
    double aXimXj, aXipXj, aXapXb;
    for(i = 0; i < n; i++) {
        for(j = i+1; j < n; j++) {
            for(a = j+1; a < n; a++) {
                for(b = a+1; b < n; b++) {
                    Xijab[0] = X[i];
                    Xijab[1] = X[j];
                    Xijab[2] = X[a];
                    Xijab[3] = X[b];

                    aXimXj = std::abs(Xijab[0] - Xijab[1]);
                    aXipXj = std::abs(Xijab[0] + Xijab[1]);
                    aXapXb = std::abs(Xijab[2] + Xijab[3]);
                    TS_sum += (aXimXj < aXapXb);
                    TS_sum -= (aXipXj < aXapXb);

                    aXimXj = std::abs(Xijab[0] - Xijab[2]);
                    aXipXj = std::abs(Xijab[0] + Xijab[2]);
                    aXapXb = std::abs(Xijab[1] + Xijab[3]);
                    TS_sum += (aXimXj < aXapXb);
                    TS_sum -= (aXipXj < aXapXb);

                    aXimXj = std::abs(Xijab[0] - Xijab[3]);
                    aXipXj = std::abs(Xijab[0] + Xijab[3]);
                    aXapXb = std::abs(Xijab[2] + Xijab[1]);
                    TS_sum += (aXimXj < aXapXb);
                    TS_sum -= (aXipXj < aXapXb);

                    aXimXj = std::abs(Xijab[1] - Xijab[2]);
                    aXipXj = std::abs(Xijab[1] + Xijab[2]);
                    aXapXb = std::abs(Xijab[0] + Xijab[3]);
                    TS_sum += (aXimXj < aXapXb);
                    TS_sum -= (aXipXj < aXapXb);

                    aXimXj = std::abs(Xijab[3] - Xijab[1]);
                    aXipXj = std::abs(Xijab[3] + Xijab[1]);
                    aXapXb = std::abs(Xijab[2] + Xijab[0]);
                    TS_sum += (aXimXj < aXapXb);
                    TS_sum -= (aXipXj < aXapXb);

                    aXimXj = std::abs(Xijab[2] - Xijab[3]);
                    aXipXj = std::abs(Xijab[2] + Xijab[3]);
                    aXapXb = std::abs(Xijab[0] + Xijab[1]);
                    TS_sum += (aXimXj < aXapXb);
                    TS_sum -= (aXipXj < aXapXb);
                }
            }
        }
    }

    double TS_value = (double)TS_sum / (Rf_choose(n, 4) * 6);

    return TS_value;
}

