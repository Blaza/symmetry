#ifndef symmetry_test_stats
#define symmetry_test_stats

using namespace Rcpp;

double I1_Cpp(const NumericVector& X, double k);
double K1_Cpp(const NumericVector& X, double k);
double NAI_Cpp(const NumericVector& X, double k);
double NAK_Cpp(const NumericVector& X, double k);
double BHI_Cpp(const NumericVector& X);
double BHK_Cpp(const NumericVector& X);
double CM_Cpp(const NumericVector& X);
double L1_Cpp(const NumericVector& X, double a);
double CH_Cpp(const NumericVector& X);
double KS_Cpp(const NumericVector& X);
#endif
