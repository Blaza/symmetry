symmetry_test <- function(x, ...) {
  UseMethod("symmetry_test", x)
}

symmetry_test.default <- function(x, stat, mu = 0, bootstrap = FALSE,
                                  B = 100, boot_method = "sign",
                                  trim_alpha = 0, k = 0) {
  xname <- deparse(substitute(x))
  if (bootstrap) {
    stat_fun <- match.fun(stat, descend = FALSE)
    pass_k <- "k" %in% names(formals(stat))
    if (pass_k && k == 0)
      stop("Argument 'k' not specified.")

    boot <- boot_sample(x, trim_alpha, B, boot_method, stat, k)

    MU <- c(mu = mean(x, trim = trim_alpha))
    xc <- x - MU
    tval <- if(pass_k) stat_fun(xc, k = k) else stat_fun(xc)
    names(tval) <- stat

    pval <- mean(abs(boot) > abs(tval))

    METHOD <- c("Symmetry test",
                "Null hypothesis: Data is symmetric")
    params <- c("B" = B)
    if(pass_k) params <- c(k=k, params)

  } else {

  }

  obj <- list(method = METHOD,
              statistic = tval,
              parameters = params,
              p.value = pval,
              estimate = MU,
              data.name = xname)
  class(obj) <- "htest"
  obj
}

symmetry_test.lm <- function(m, stat, B = 100, boot_method = "sign", k = NULL) {
  stat_fun <- match.fun(stat, descend = FALSE)
  pass_k <- "k" %in% names(formals(stat))
  if (pass_k && is.null(k))
    stop("Argument 'k' not specified.")

  X <- model.matrix(m)
  yfit <- fitted(m)
  res <- residuals(m)
  boot <- boot_sample_lm(X, yfit, res, B, boot_method, stat, k)
  tval <- if(pass_k) stat_fun(res, k = k) else stat_fun(res)
  names(tval) <- stat
  pval <- mean(abs(boot) > abs(tval))

  xname <- paste("Residuals from model", deparse(substitute(m)))
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
