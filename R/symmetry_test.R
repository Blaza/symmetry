symmetry_test <- function(x, ...) {
  UseMethod("symmetry_test", x)
}

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

    pval <- mean(abs(boot) > abs(tval))

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

    pval <- mean(abs(null_distrib) > abs(tval))

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

symmetry_test.lm <- function(model, stat, B = 100,
                             boot_method = "sign", k = NULL) {
  stat_fun <- match.fun(stat, descend = FALSE)
  pass_k <- "k" %in% names(formals(stat))
  if (pass_k && is.null(k))
    stop("Argument 'k' not specified.")

  X <- model.matrix(model)
  yfit <- fitted(model)
  res <- residuals(model)
  boot <- boot_sample_lm(X, yfit, res, B, boot_method, stat, k)
  tval <- if(pass_k) stat_fun(res, k = k) else stat_fun(res)
  names(tval) <- stat
  pval <- mean(abs(boot) > abs(tval))

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

symmetry_test.garch <- function(model, stat, ts = NULL, B = 100,
                                boot_method = "sign", k = NULL) {
  stat_fun <- match.fun(stat, descend = FALSE)
  if (is.null(ts))
    stop("The original time series must be provided")
  pass_k <- "k" %in% names(formals(stat))
  if (pass_k && is.null(k))
    stop("Argument 'k' not specified.")

  cfit <- fitted(model)[, 1]

  ind <- (max(model$order) + 1) : length(cfit)
  cfit <- cfit[ind]
  res <- residuals(model)[ind]

  null_sample_fun <- switch(boot_method,
                            "sign" = randomize_sign,
                            "reflect" = reflected_boot)

  ts_head <- if (!is.null(ts)) ts[-ind] else rep(0, max(model$order))

  coefs <- coefficients(model)
  omega <- coefs["a0"]
  alpha <- coefs[grepl("a[1-9]", names(coefs))]
  beta <- coefs[grepl("b", names(coefs))]

  boot <- replicate(B, {
    boot_res <- null_sample_fun(res, 0)
    boot_y <- simulate_garch(boot_res, ts, omega, alpha, beta)
    new_res <- residuals(garch(boot_y, model$order, trace = FALSE))
    if(pass_k) stat_fun(new_res, k = k) else stat_fun(new_res)
  })

  tval <- if(pass_k) stat_fun(res, k = k) else stat_fun(res)
  names(tval) <- stat
  pval <- mean(abs(boot) > abs(tval))

  xname <- paste("Residuals from model", deparse(substitute(model)))
  METHOD <- c("Symmetry test of GARCH model residuals",
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
