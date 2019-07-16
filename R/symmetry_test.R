#' @export
symmetry_test <- function(x, ...) {
  UseMethod("symmetry_test", x)
}

#' @export
symmetry_test.default <- function(x, stat, mu = NULL,
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
  params <- NULL

  if (bootstrap && is.null(mu)) { # UNKNOWN MEAN BOOTSTRAP
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

  } else if (bootstrap && !is.null(mu)){ # KNOWN MEAN BOOTSTRAP
    boot <- if (boot_method != "mn") {
      boot_sample(x, mu, B, boot_method, stat, k, TRUE)
    } else {
      mn_boot_sample(x, mu, B, stat, k, q, TRUE)
    }

    xc <- x - mu
    tval <- if(pass_k) stat_fun(xc, k = k) else stat_fun(xc)

    pval <- mean(abs(boot) >= abs(tval))

    METHOD <- c("Symmetry test",
                paste("Null hypothesis: Data is symmetric around", mu))
    params <- c("B" = B)

  } else if(simulate_p_value){ # KNOWN MEAN, SIMULATED P VALUE
    if (is.null(mu))
      mu <- 0
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

  } else { # ASYMPTOTIC RESULTS
    if (!stat %in% names(asymptotic_distributions))
      stop("The asymptotic distribution of the chosen test statistic is not available.")
    if (is.null(mu))
      mu <- 0
    tval <- if(pass_k) stat_fun(x, k = k, mu = mu) else stat_fun(x, mu = mu)
    pdist <- asymptotic_distributions[[stat]](k)
    pval <- 2 * (1 - pdist(abs(tval)))
    METHOD <- c("Symmetry test",
                paste("Null hypothesis: Data is symmetric around", mu))
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
symmetry_test.fGARCH <- function(model, stat, B = 100, burn = 0, bootstrap = TRUE,
                                boot_method = "sign", k = 0, iid = !bootstrap) {
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

  tail_res <- tail(res, not_burned)

  tval <- if(pass_k) stat_fun(tail_res, k = k) else stat_fun(tail_res)
  names(tval) <- stat

  params <- c()

  if (bootstrap) {
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

    pval <- mean(abs(boot) >= abs(tval))
    params <- c("B" = B, params)
  } else { # ASYMPTOTIC DISTRUBUTION
    if (!stat %in% names(asymptotic_distributions))
      stop("The asymptotic distribution of the chosen test statistic is not available.")

    pdist <- asymptotic_distributions[[stat]](k)
    pval <- 2 * (1 - pdist(abs(unname(tval))))
  }
  xname <- paste("Residuals from model", deparse(substitute(model)))
  METHOD <- c("Symmetry test of GARCH model residuals",
              "Null hypothesis: The residuals are symmetric around 0")
  if(pass_k) params <- c(k=k, params)

  obj <- list(method = METHOD,
              statistic = tval,
              parameters = params,
              p.value = pval,
              data.name = xname)
  class(obj) <- "htest"
  obj
}
