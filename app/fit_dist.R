fit_dist <- function(data, type = c("count", "severity"), dists = NULL, value_col = NULL, ...) {
  if (is.data.frame(data)) {
    if (is.null(value_col)) stop("When 'data' is a data frame, supply 'value_col'.")
    if (!value_col %in% names(data)) stop(sprintf("Column '%s' not found.", value_col))
    x <- data[[value_col]]
  } else {
    x <- data
  }

  if (!is.numeric(x)) {
    x_numeric <- suppressWarnings(as.numeric(as.character(x)))
    if (all(is.na(x_numeric))) stop("Input data must be numeric or coercible to numeric.")
    x <- x_numeric
  }

  x <- x[!is.na(x)]
  if (length(x) < 2) stop("Need at least two non-missing observations to fit a distribution.")

  type <- match.arg(type)
  if (type == "count") {
    if (any(x < 0)) stop("Count data must be non-negative values.")
    default_dists <- c("zipois", "pois", "nbinom", "zinegbin", "geom")
  } else {
    if (any(x <= 0)) stop("Severity data must be positive values.")
    default_dists <- c("exp", "gamma", "lnorm", "weibull")
  }

  if (is.null(dists)) dists <- default_dists
  if (!is.character(dists) || length(dists) < 1) stop("'dists' must be a character vector of distribution names.")

  fits <- list()
  bic_values <- rep(NA_real_, length(dists))
  names(bic_values) <- dists

  for (dist in dists) {
    if (type == "count" && dist %in% c("zipois", "zinegbin")) {
      if (!requireNamespace("pscl", quietly = TRUE)) {
        warning("Package 'pscl' not installed; skipping zero-inflated fit.")
        bic_values[dist] <- NA_real_
        next
      }
      df <- data.frame(y = as.integer(round(x)))
      fit_obj <- tryCatch({
        if (dist == "zipois") pscl::zeroinfl(y ~ 1 | 1, data = df, dist = "poisson", EM = FALSE)
        else pscl::zeroinfl(y ~ 1 | 1, data = df, dist = "negbin", EM = FALSE)
      }, error = function(e) {
        warning(sprintf("zeroinfl failed for '%s': %s", dist, e$message), call. = FALSE)
        NULL
      })

      if (is.null(fit_obj)) { bic_values[dist] <- NA_real_; next }

      count_coefs <- stats::coef(fit_obj, model = "count")
      zero_coefs <- stats::coef(fit_obj, model = "zero")
      lambda <- as.numeric(exp(as.numeric(count_coefs[1])))
      pi <- as.numeric(stats::plogis(as.numeric(zero_coefs[1])))
      est <- c(pi = pi, lambda = lambda)
      if (!is.null(fit_obj$theta)) est <- c(est, theta = as.numeric(fit_obj$theta))

      loglik <- as.numeric(stats::logLik(fit_obj))
      npar <- length(count_coefs) + length(zero_coefs) + if (!is.null(fit_obj$theta)) 1 else 0
      bic_values[dist] <- -2 * loglik + log(length(x)) * npar
      fits[[dist]] <- list(estimate = est, loglik = loglik, object = fit_obj)
      next
    }

    # standard distributions
    fit <- tryCatch({
      fitdistrplus::fitdist(x, dist, method = "mle", ...)
    }, error = function(e) {
      warning(sprintf("fitdist failed for distribution '%s': %s", dist, e$message), call. = FALSE)
      NULL
    })

    if (is.null(fit)) { bic_values[dist] <- NA_real_; next }

    bic_values[dist] <- -2 * fit$loglik + log(length(x)) * length(fit$estimate)
    fits[[dist]] <- fit
  }

  fits <- fits[!vapply(fits, is.null, logical(1))]
  if (!length(fits)) stop("None of the requested distributions could be fitted.")

  bic_table <- data.frame(
    distribution = names(fits),
    bic = as.numeric(bic_values[names(fits)]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  bic_table <- bic_table[order(bic_table$bic), , drop = FALSE]

  best_dist <- bic_table$distribution[1]
  list(best_fit = fits[[best_dist]], bic_table = bic_table, best_dist = best_dist, fits = fits)
}
