#ifndef symmetry_test_stats
#define symmetry_test_stats

using namespace Rcpp;

double I1_Cpp(const NumericVector& X, int k);
double K1_Cpp(const NumericVector& X, int k);
double NAI_Cpp(const NumericVector& X, int k);
double NAK_Cpp(const NumericVector& X, int k);
double BHI_Cpp(const NumericVector& X);
double BHK_Cpp(const NumericVector& X);

#endif
