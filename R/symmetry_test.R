#' @export
symmetry_test <- function(x, ...) {
  UseMethod("symmetry_test", x)
}

#' @export
symmetry_test.default <- function(x, stat, mu = 0,
                                  simulate_p_value = FALSE, N=1000,
                                  bootstrap = FALSE, B = 100,
                                  boot_method = "sign", trim = 0, k = 0,
                                  q = 8/9) {
  xname <- deparse(substitute(x))

  stat_fun <- match.fun(stat, descend = FALSE)
  pass_k <- "k" %in% names(formals(stat))
  if (pass_k && k == 0)
    stop("Argument 'k' not specified.")

  MU <- NULL

  if (bootstrap) {
    boot <- if (boot_method != "mn") {
      boot_sample(x, trim, B, boot_method, stat, k)
    } else {
      mn_boot_sample(x, trim, B, stat, k, q)
    }

    MU <- c(mu = mean(x, trim = trim))
    xc <- x - MU
    tval <- if(pass_k) stat_fun(xc, k = k) else stat_fun(xc)

    pval <- mean(abs(boot) >= abs(tval))

    METHOD <- c("Symmetry test",
                "Null hypothesis: Data is symmetric")
    params <- c("B" = B)

  } else if(simulate_p_value){
    n <- length(x)
    normals <- matrix(rnorm(n*N, mu), ncol=n)
    apply_fun <- if(pass_k) {
      function(x) stat_fun(x, k, mu)
    } else {
      function(x) stat_fun(x, mu)
    }
    null_distrib <- apply(normals, 1, apply_fun)

    tval <- if(pass_k) stat_fun(x, k = k, mu = mu) else stat_fun(x, mu = mu)

    pval <- mean(abs(null_distrib) >= abs(tval))

    METHOD <- c("Symmetry test",
                paste("Null hypothesis: Data is symmetric around", mu))
    params <- c("N" = N)

  } else {
  }

  names(tval) <- stat
  if(pass_k) params <- c(k=k, params)

  obj <- list(method = METHOD,
              statistic = tval,
              parameters = params,
              p.value = pval,
              estimate = MU,
              data.name = xname)
  class(obj) <- "htest"
  obj
}

#' @export
symmetry_test.lm <- function(model, stat, B = 100,
                             boot_method = "sign", k = 0) {
  stat_fun <- match.fun(stat, descend = FALSE)
  pass_k <- "k" %in% names(formals(stat))
  if (pass_k && k == 0)
    stop("Argument 'k' not specified.")

  X <- model.matrix(model)
  yfit <- fitted(model)
  res <- residuals(model)
  boot <- boot_sample_lm(X, yfit, res, B, boot_method, stat, k)
  tval <- if(pass_k) stat_fun(res, k = k) else stat_fun(res)
  names(tval) <- stat
  pval <- mean(abs(boot) >= abs(tval))

  xname <- paste("Residuals from model", deparse(substitute(model)))
  METHOD <- c("Symmetry test of linear model residuals",
              "Null hypothesis: The residuals are symmetric around 0")
  params <- c("B" = B)
  if(pass_k) params <- c(k=k, params)

  obj <- list(method = METHOD,
              statistic = tval,
              parameters = params,
              p.value = pval,
              data.name = xname)
  class(obj) <- "htest"
  obj
}

#' @export
symmetry_test.fGARCH <- function(model, stat, B = 100, burn = 0,
                                boot_method = "sign", k = 0, iid = FALSE) {
  stat_fun <- match.fun(stat, descend = FALSE)

  pass_k <- "k" %in% names(formals(stat))
  if (pass_k && k == 0)
    stop("Argument 'k' not specified.")

  res <- residuals(model, standardize = TRUE)

  null_sample_fun <- switch(boot_method,
                            "sign" = randomize_sign,
                            "reflect" = reflected_boot)

  coefs <- coef(model)
  omega <- coefs["omega"]
  alpha <- coefs[grepl("alpha", names(coefs))]
  beta <- coefs[grepl("beta", names(coefs))]

  ts <- as.numeric(model@data)
  cfit <- as.numeric(fitted(model))

  not_burned <- length(ts) - burn
  if (not_burned <= 0)
    stop("Number of points to burn is larger than the series length")

  boot <- replicate(B, {
    boot_res <- null_sample_fun(res, 0)

    if (!iid) {
      boot_y <- simulate_garch(boot_res, ts, cfit, omega, alpha, beta)

      boot_model <- garchFit(model@formula, boot_y,
                             cond.dist = "QMLE", include.mean = FALSE,
                             trace = FALSE)
      new_res <- tail(residuals(boot_model, standardize = TRUE), not_burned)
    } else {
      new_res <- tail(boot_res, not_burned)
    }
    if(pass_k) stat_fun(new_res, k = k) else stat_fun(new_res)
  })

  res <- tail(res, not_burned)

  tval <- if(pass_k) stat_fun(res, k = k) else stat_fun(res)
  names(tval) <- stat
  pval <- mean(abs(boot) >= abs(tval))

  xname <- paste("Residuals from model", deparse(substitute(model)))
  METHOD <- c("Symmetry test of GARCH model residuals",
              "Null hypothesis: The residuals are symmetric around 0")
  params <- c("B" = B)
  if(pass_k) params <- c(k=k, params)

  obj <- list(method = METHOD,
              statistic = tval,
              parameters = params,
              p.value = pval,
              data.name = xname,
              boot_stats = boot)
  class(obj) <- "htest"
  obj
}



#' @export
symmetry_test_fast <- function(model, stat, B = 100, burn = 0,
                                 boot_method = "sign", k = 0, iid = FALSE) {
  stat_fun <- match.fun(stat, descend = FALSE)

  pass_k <- "k" %in% names(formals(stat))
  if (pass_k && k == 0)
    stop("Argument 'k' not specified.")

  res <- residuals(model, standardize = TRUE)

  null_sample_fun <- switch(boot_method,
                            "sign" = randomize_sign,
                            "reflect" = reflected_boot)

  coefs <- coef(model)
  omega <- coefs["omega"]
  alpha <- coefs[grepl("alpha", names(coefs))]
  beta <- coefs[grepl("beta", names(coefs))]

  ts <- as.numeric(model@data)
  cfit <- as.numeric(fitted(model))

  not_burned <- length(ts) - burn
  if (not_burned <= 0)
    stop("Number of points to burn is larger than the series length")

  boot_res_mat <- garch_boot_residuals(res, B, boot_method)

  if (!iid) {
    boot_y_vec <- simulate_garch_mat(boot_res_mat, ts, cfit, omega, alpha, beta)

    garch_res_mat <- apply(boot_y_vec, 2, function(boot_y) {
      boot_model <- garchFit(model@formula, boot_y,
                             cond.dist = "QMLE", include.mean = FALSE,
                             trace = FALSE)
      residuals(boot_model, standardize = TRUE)
    })

    new_res_mat <- tail(garch_res_mat, not_burned)
  } else {
    new_res_mat <- tail(boot_res_mat, not_burned)
  }

  if (pass_k) {
    boot <- apply_stat(new_res_mat, stat, k = k)
  } else {
    boot <- apply_stat(new_res_mat, stat)
  }

  res <- tail(res, not_burned)

  tval <- if(pass_k) stat_fun(res, k = k) else stat_fun(res)
  names(tval) <- stat
  pval <- mean(abs(boot) >= abs(tval))

  xname <- paste("Residuals from model", deparse(substitute(model)))
  METHOD <- c("Symmetry test of GARCH model residuals",
              "Null hypothesis: The residuals are symmetric around 0")
  params <- c("B" = B)
  if(pass_k) params <- c(k=k, params)

  obj <- list(method = METHOD,
              statistic = tval,
              parameters = params,
              p.value = pval,
              data.name = xname,
              boot_stats = boot)
  class(obj) <- "htest"
  obj
}
