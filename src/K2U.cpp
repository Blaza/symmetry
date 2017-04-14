#include <Rcpp.h>
#include <cstdint>
#include <cmath>
#include <unordered_set>
using namespace Rcpp;
using std::int64_t;


// [[Rcpp::export]]
NumericMatrix K2U_get_samples(const NumericVector& X) {
    int n = X.size();
    int i, j, count = 0;

    NumericMatrix sample_matrix(2, Rf_choose(n, 2));
    rownames(sample_matrix) = CharacterVector::create("minus", "plus");

    for(i = 0; i < n; i++) {
        for(j = 0; j < i; j++) {
            sample_matrix(0, count) = std::abs(X[i] - X[j]);
            sample_matrix(1, count) = std::abs(X[i] + X[j]);
            count++;
        }
    }

    return sample_matrix;
}

