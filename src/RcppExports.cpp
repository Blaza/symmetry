// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// chunk_sum
int chunk_sum(const IntegerMatrix comb, const NumericVector x, int k);
RcppExport SEXP symmetry_chunk_sum(SEXP combSEXP, SEXP xSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerMatrix >::type comb(combSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(chunk_sum(comb, x, k));
    return rcpp_result_gen;
END_RCPP
}
// I1_Cpp
double I1_Cpp(const NumericVector& X, int k);
RcppExport SEXP symmetry_I1_Cpp(SEXP XSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(I1_Cpp(X, k));
    return rcpp_result_gen;
END_RCPP
}
// I2_Cpp
double I2_Cpp(const NumericVector& X);
RcppExport SEXP symmetry_I2_Cpp(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(I2_Cpp(X));
    return rcpp_result_gen;
END_RCPP
}
// K1_Cpp
double K1_Cpp(const NumericVector& X, int k);
RcppExport SEXP symmetry_K1_Cpp(SEXP XSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(K1_Cpp(X, k));
    return rcpp_result_gen;
END_RCPP
}
// K2_get_samples
NumericMatrix K2_get_samples(const NumericVector& X);
RcppExport SEXP symmetry_K2_get_samples(SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(K2_get_samples(X));
    return rcpp_result_gen;
END_RCPP
}
