#ifndef symmetry_test_stats
#define symmetry_test_stats

using namespace Rcpp;

double MOI_Cpp(const NumericVector& X, double k);
double MOK_Cpp(const NumericVector& X, double k);
double K2_Cpp(const NumericVector& X);
double K2U_Cpp(const NumericVector& X);
double NAI_Cpp(const NumericVector& X, double k);
double NAK_Cpp(const NumericVector& X, double k);
double BHI_Cpp(const NumericVector& X);
double BHK_Cpp(const NumericVector& X);
double CM_Cpp(const NumericVector& X);
double M_Cpp(const NumericVector& X);
double MGG_Cpp(const NumericVector& X);
double B1_Cpp(const NumericVector& X);
double T1_Cpp(const NumericVector& X, double a);
double T2_Cpp(const NumericVector& X, double a);
double S1_Cpp(const NumericVector& X, double a);
double S2_Cpp(const NumericVector& X, double a);
double CH_Cpp(const NumericVector& X);
double KS_Cpp(const NumericVector& X);
double SGN_Cpp(const NumericVector& X);
double WCX_Cpp(const NumericVector& X);
#endif
