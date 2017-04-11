#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix K2_get_samples(const NumericVector& X) {
    int n = X.size();
    int i, j, count = 0;

    NumericMatrix sample_matrix(2, n*n);
    rownames(sample_matrix) = CharacterVector::create("minus", "plus");

    for(i = 0; i < n; i++) {
        for(j = 0; j < n; j++) {
            sample_matrix(0, count) = std::abs(X[i] - X[j]);
            sample_matrix(1, count) = std::abs(X[i] + X[j]);
            count++;
        }
    }

    return sample_matrix;
}

