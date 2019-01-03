#include <RcppArmadillo.h>
#include <test_stats.h>
#include <string>
#include <functional>

using namespace Rcpp;

std::function<double (const NumericVector&)>
  get_ts_fun(std::string stat, int k = 0) {

  if (stat == "BHI") {
    return BHI_Cpp;
  }
  if (stat == "BHK") {
    return BHK_Cpp;
  }
  if (stat == "I1") {
    return bind(I1_Cpp, std::placeholders::_1, k);
  }
  if (stat == "K1") {
    return bind(K1_Cpp, std::placeholders::_1, k);
  }
  if (stat == "NAI") {
    return bind(NAI_Cpp, std::placeholders::_1, k);
  }
  if (stat == "NAK") {
    return bind(NAK_Cpp, std::placeholders::_1, k);
  }
  return NULL;
}

// [[Rcpp::export]]
NumericVector randomize_sign_old(const NumericVector& X) {
  int n = X.size();
  NumericVector res(X);
  LogicalVector negative = runif(n, -1, 1) < 0;
  for (int i = 0; i < n; i++) {
    if(negative[i]) {
      res[i] = -X[i];
    }
  }
  return res;
}

// [[Rcpp::export]]
NumericVector randomize_sign(const NumericVector& X, double mu) {
  int n = X.size();
  NumericVector res(X - mu);
  LogicalVector negative = runif(n, -1, 1) < 0;
  for (int i = 0; i < n; i++) {
    if(negative[i]) {
      res[i] = -res[i];
    }
  }
  return res;
}


// [[Rcpp::export]]
NumericVector sample_with_replacement(NumericVector x, int n) {
  return x[floor(runif(n, 0, x.size()))];
}

// [[Rcpp::export]]
NumericVector reflect_sample(const NumericVector& X, double mu, int n) {
  NumericVector reflected(2*n);
  for (int i = 0; i < n; i++) {
    reflected[i] = X[i];
  }
  for (int i = 0; i < n; i++) {
    reflected[i + n] = 2*mu - X[i];
  }
  return reflected;
}


// [[Rcpp::export]]
NumericVector reflected_boot(const NumericVector& X, double mu) {
  int n = X.size();
  return sample_with_replacement(reflect_sample(X, mu, n), n);
}

std::function<NumericVector (const NumericVector&, double)>
  get_null_fun(std::string null_method) {

    if (null_method == "sign") {
      return randomize_sign;
    }
    if (null_method == "reflect") {
      return reflected_boot;
    }
    return NULL;
  }


// [[Rcpp::export]]
double trimmed_mean(const NumericVector& X, double alpha = 0) {
  if (alpha == 0) {
    return mean(X);
  }

  int n = X.size();

  NumericVector Xs(X);
  std::sort(Xs.begin(), Xs.end());

  int trim, lwr, upr;

  if (alpha < 0.5) {
    trim = floor(n * alpha);
    lwr = trim;
    upr = n - trim - 1;
  } else {
    trim = floor(n * 0.5);
    lwr = n - trim - 1;
    upr = trim;
  }

  return mean(Xs[Range(lwr, upr)]);
}


// [[Rcpp::export]]
NumericVector boot_sample(const NumericVector& X, double trim_alpha,
                          int B, std::string null_method,
                          std::string stat, int k = 0) {
  auto ts_fun = get_ts_fun(stat, k);
  auto null_sample_fun = get_null_fun(null_method);

  double mu = trimmed_mean(X, trim_alpha);

  double mu_sym;
  NumericVector X_sym;

  NumericVector boot_sample(B);

  for (int i = 0; i < B; i++) {
    X_sym = null_sample_fun(X, mu);
    mu_sym = trimmed_mean(X_sym, trim_alpha);
    boot_sample[i] = ts_fun(X_sym - mu_sym);
  }

  return boot_sample;
}

// Get regression residuals given model matrix and response vector
// The code is a stripped version of Rcpp gallery example:
// http://gallery.rcpp.org/articles/fast-linear-model-with-armadillo/
// [[Rcpp::export]]
NumericVector lm_resid(const arma::mat& X, NumericVector& yr) {
  arma::colvec y(yr.begin(), yr.size(), false);

  arma::colvec coef = arma::solve(X, y);    // fit model y ~ X
  arma::colvec res  = y - X*coef;           // residuals

  return NumericVector(res.begin(), res.end());
}

// [[Rcpp::export]]
NumericVector boot_sample_lm(const arma::mat& model_matrix,
                             const NumericVector& fitted,
                             const NumericVector& residuals,
                             int B, std::string null_method,
                             std::string stat, int k = 0) {
  auto ts_fun = get_ts_fun(stat, k);
  auto null_sample_fun = get_null_fun(null_method);

  NumericVector boot_resid;
  NumericVector new_resid;
  NumericVector boot_y;

  NumericVector boot_sample(B);

  for (int i = 0; i < B; i++) {
    boot_resid = null_sample_fun(residuals, 0);
    boot_y = fitted + boot_resid;
    new_resid = lm_resid(model_matrix, boot_y);
    boot_sample[i] = ts_fun(new_resid);
  }

  return boot_sample;
}
