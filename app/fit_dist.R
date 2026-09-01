# =============================================================================
# fit_dist.R — Ajuste de distribuciones de probabilidad (TEF conteos / LM
# severidad) con selección automática por BIC.
#
# Uso:
#   fit_dist(x, type = "count")    -> distribuciones de conteo (TEF)
#   fit_dist(x, type = "severity") -> distribuciones de severidad (LM)
#
# Devuelve: best_fit (objeto fitdistrplus), bic_table (tabla BIC ordenada),
# best_dist (nombre de la distribución ganadora) y fits (todos los ajustes).
# =============================================================================

fit_dist <- function(data, type = c("count", "severity"), dists = NULL, value_col = NULL, ...) {
  # --- Normalizar la entrada: acepta vector numérico o data.frame ------------
  if (is.data.frame(data)) {
    # Si se pasa un data.frame, se exige el nombre de la columna de valores
    if (is.null(value_col)) stop("Cuando 'data' es un data.frame, indica 'value_col'.")
    if (!value_col %in% names(data)) stop(sprintf("Columna '%s' no encontrada.", value_col))
    x <- data[[value_col]]
  } else {
    x <- data
  }

  # --- Coercer a numérico si es necesario ------------------------------------
  if (!is.numeric(x)) {
    x_numeric <- suppressWarnings(as.numeric(as.character(x)))
    if (all(is.na(x_numeric))) stop("Los datos deben ser numéricos o coercibles a numérico.")
    x <- x_numeric
  }

  # --- Limpieza básica y tamaño mínimo ---------------------------------------
  x <- x[!is.na(x)]
  if (length(x) < 2) stop("Se necesitan al menos dos observaciones no faltantes para ajustar una distribución.")

  # --- Definir el conjunto de distribuciones candidatas según el tipo --------
  type <- match.arg(type)
  if (type == "count") {
    # Conteos (TEF): distribuciones de Poisson, binomial negativa y variantes
    # con inflación de ceros (zipois/zinegbin requieren el paquete pscl).
    if (any(x < 0)) stop("Los datos de conteo deben ser no negativos.")
    default_dists <- c("zipois", "pois", "nbinom", "zinegbin", "geom")
  } else {
    # Severidad (LM): distribuciones continuas de soporte positivo
    if (any(x <= 0)) stop("Los datos de severidad deben ser positivos.")
    default_dists <- c("exp", "gamma", "lnorm", "weibull")
  }

  if (is.null(dists)) dists <- default_dists
  if (!is.character(dists) || length(dists) < 1) stop("'dists' debe ser un vector de caracteres con nombres de distribuciones.")

  # --- Ajustar cada distribución y calcular su BIC ---------------------------
  # BIC = -2 * log-verosimilitud + log(n) * k (k = número de parámetros).
  # La distribución con menor BIC es la mejor según el criterio.
  fits <- list()
  bic_values <- rep(NA_real_, length(dists))
  names(bic_values) <- dists

  for (dist in dists) {
    # Distribuciones con inflación de ceros (pscl::zeroinfl)
    if (type == "count" && dist %in% c("zipois", "zinegbin")) {
      if (!requireNamespace("pscl", quietly = TRUE)) {
        warning("El paquete 'pscl' no está instalado; se omite el ajuste con inflación de ceros.")
        bic_values[dist] <- NA_real_
        next
      }
      df <- data.frame(y = as.integer(round(x)))
      fit_obj <- tryCatch({
        if (dist == "zipois") pscl::zeroinfl(y ~ 1 | 1, data = df, dist = "poisson", EM = FALSE)
        else pscl::zeroinfl(y ~ 1 | 1, data = df, dist = "negbin", EM = FALSE)
      }, error = function(e) {
        warning(sprintf("zeroinfl falló para '%s': %s", dist, e$message), call. = FALSE)
        NULL
      })

      if (is.null(fit_obj)) { bic_values[dist] <- NA_real_; next }

      # Extraer parámetros: lambda (media del componente de conteo), pi
      # (probabilidad de cero) y theta (dispersión si es binomial negativa)
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

    # Distribuciones estándar (fitdistrplus::fitdist por máxima verosimilitud)
    fit <- tryCatch({
      fitdistrplus::fitdist(x, dist, method = "mle", ...)
    }, error = function(e) {
      warning(sprintf("fitdist falló para la distribución '%s': %s", dist, e$message), call. = FALSE)
      NULL
    })

    if (is.null(fit)) { bic_values[dist] <- NA_real_; next }

    bic_values[dist] <- -2 * fit$loglik + log(length(x)) * length(fit$estimate)
    fits[[dist]] <- fit
  }

  # --- Resultado: descartar fallos y elegir la mejor por BIC ------------------
  fits <- fits[!vapply(fits, is.null, logical(1))]
  if (!length(fits)) stop("Ninguna de las distribuciones solicitadas pudo ajustarse.")

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
