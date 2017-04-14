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

// [[Rcpp::export]]
double K2U_Cpp(const NumericVector& X) {
    int n = X.size();
    int i, j;
    int64_t sum;
    int64_t max = -1; // will be positive as we take sup|...|
    short mult;
    double abs_minus, abs_plus;

    NumericMatrix mat_minus(n, n);
    NumericMatrix mat_plus(n, n);
    std::unordered_set<double> pot_pts = {0};

    for(i = 0; i < n; i++) {
        for(j = 0; j < i; j++) {
            abs_minus = std::abs(X[i] - X[j]);
            abs_plus = std::abs(X[i] + X[j]);

            pot_pts.insert(abs_minus);
            pot_pts.insert(abs_plus);

            mat_minus(i, j) = abs_minus;
            mat_plus(i, j) = abs_plus;
        }
    }

    for(auto it = pot_pts.begin(); it != pot_pts.end(); ++it) {
        sum = 0;
        for(i = 0; i < n; i++) {
            for(j = 0; j < i; j++) {
                sum += (mat_minus(i, j) < *it);
                sum -= (mat_plus(i, j) < *it);
            }
        }
        max = std::abs(sum) > max ? std::abs(sum) : max;
    }

    return (double)max / Rf_choose(n, 2);
}

