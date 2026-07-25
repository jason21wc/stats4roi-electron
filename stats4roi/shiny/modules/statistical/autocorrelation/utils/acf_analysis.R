# Autocorrelation / PACF analysis utilities (pure, testable)
# Significant-lag CI matches SPC Individuals Chart Statistics:
#   crit = qnorm((1 + conf.level) / 2) / sqrt(n.used)

#' Drop non-finite values and coerce to numeric vector.
#' @param x Atomic vector or list column of measures.
#' @return Numeric vector with NAs removed (may be empty).
acf_prepare_series <- function(x) {
  if (is.null(x)) {
    return(numeric(0))
  }
  x <- as.numeric(unlist(x, use.names = FALSE))
  x[is.finite(x)]
}

#' Default lag.max as used by stats::acf when lag.max is NULL.
#' @param n Series length after NA drop.
#' @return Integer lag.max (>= 1 when n >= 2).
acf_default_lag_max <- function(n) {
  n <- as.integer(n)[1]
  if (is.na(n) || n < 2L) {
    return(1L)
  }
  as.integer(floor(10 * log10(n)))
}

#' Critical ACF/PACF absolute value for a two-sided confidence band.
#' @param n Effective sample size (n.used from acf/pacf).
#' @param conf.level Confidence level in (0, 1), default 0.95.
#' @return Positive critical value, or NA if n < 2.
acf_critical_value <- function(n, conf.level = 0.95) {
  n <- as.numeric(n)[1]
  conf.level <- as.numeric(conf.level)[1]
  if (is.na(n) || n < 2 || is.na(conf.level) || conf.level <= 0 || conf.level >= 1) {
    return(NA_real_)
  }
  stats::qnorm((1 + conf.level) / 2) / sqrt(n)
}

#' Clamp lag.max to a valid range for a series of length n.
#' @param n Series length.
#' @param lag.max Requested max lag (NULL uses default).
#' @return Integer lag.max in [1, n - 1].
acf_clamp_lag_max <- function(n, lag.max = NULL) {
  n <- as.integer(n)[1]
  if (is.na(n) || n < 2L) {
    return(1L)
  }
  if (is.null(lag.max) || length(lag.max) < 1L || is.na(lag.max[1])) {
    lag.max <- acf_default_lag_max(n)
  } else {
    lag.max <- as.integer(lag.max[1])
  }
  max(1L, min(lag.max, n - 1L))
}

#' Extract lags with |value| above critical value.
#' For ACF, lag 0 is excluded from the "above" list (SPC parity).
#' @param lag Numeric lag vector.
#' @param value Numeric ACF or PACF values.
#' @param crit Critical absolute value.
#' @param exclude_lag0 Logical; if TRUE, drop lag 0 from both lists when selecting above/below.
#' @return List with up/down values and lags (numeric vectors, possibly empty).
acf_significant_lags <- function(lag, value, crit, exclude_lag0 = TRUE) {
  lag <- as.numeric(lag)
  value <- as.numeric(value)
  if (length(lag) == 0L || length(value) == 0L || is.na(crit)) {
    return(list(
      acf_up = numeric(0),
      lag_up = numeric(0),
      acf_low = numeric(0),
      lag_low = numeric(0)
    ))
  }

  above <- which(value > crit)
  below <- which(value < -crit)

  if (exclude_lag0) {
    above <- above[lag[above] != 0]
    below <- below[lag[below] != 0]
  }

  list(
    acf_up = value[above],
    lag_up = lag[above],
    acf_low = value[below],
    lag_low = lag[below]
  )
}

.acf_result <- function(lag, value, n.used, conf.level, type = c("acf", "pacf")) {
  type <- match.arg(type)
  crit <- acf_critical_value(n.used, conf.level)
  # ACF: exclude lag 0 from significant lists (SPC); PACF has no lag 0 in base R output
  sig <- acf_significant_lags(lag, value, crit, exclude_lag0 = (type == "acf"))
  list(
    type = type,
    lag = lag,
    acf = value,
    n.used = as.integer(n.used),
    conf.level = conf.level,
    crit = crit,
    significant = sig
  )
}

#' Compute sample ACF and significant lags (SPC-compatible).
#' @param x Numeric series (will be prepared).
#' @param lag.max Max lag (NULL = stats default).
#' @param conf.level Confidence for critical band (default 0.95).
#' @return List with lag, acf, n.used, crit, significant; or error attribute via list$error.
acf_compute <- function(x, lag.max = NULL, conf.level = 0.95) {
  series <- acf_prepare_series(x)
  n <- length(series)
  if (n < 2L) {
    return(list(
      type = "acf",
      lag = numeric(0),
      acf = numeric(0),
      n.used = n,
      conf.level = conf.level,
      crit = NA_real_,
      significant = list(
        acf_up = numeric(0), lag_up = numeric(0),
        acf_low = numeric(0), lag_low = numeric(0)
      ),
      error = "Need at least 2 non-missing numeric observations."
    ))
  }

  lag.max <- acf_clamp_lag_max(n, lag.max)
  acf_obj <- stats::acf(series, lag.max = lag.max, plot = FALSE)
  lag <- as.numeric(acf_obj$lag)
  value <- as.numeric(acf_obj$acf)
  out <- .acf_result(lag, value, acf_obj$n.used, conf.level, type = "acf")
  out$error <- NULL
  out
}

#' Compute sample PACF and significant lags.
#' @param x Numeric series (will be prepared).
#' @param lag.max Max lag (NULL = stats default).
#' @param conf.level Confidence for critical band (default 0.95).
#' @return List shaped like acf_compute (field `acf` holds PACF values).
pacf_compute <- function(x, lag.max = NULL, conf.level = 0.95) {
  series <- acf_prepare_series(x)
  n <- length(series)
  if (n < 2L) {
    return(list(
      type = "pacf",
      lag = numeric(0),
      acf = numeric(0),
      n.used = n,
      conf.level = conf.level,
      crit = NA_real_,
      significant = list(
        acf_up = numeric(0), lag_up = numeric(0),
        acf_low = numeric(0), lag_low = numeric(0)
      ),
      error = "Need at least 2 non-missing numeric observations."
    ))
  }

  lag.max <- acf_clamp_lag_max(n, lag.max)
  pacf_obj <- stats::pacf(series, lag.max = lag.max, plot = FALSE)
  lag <- as.numeric(pacf_obj$lag)
  value <- as.numeric(pacf_obj$acf)
  out <- .acf_result(lag, value, pacf_obj$n.used, conf.level, type = "pacf")
  out$error <- NULL
  out
}
