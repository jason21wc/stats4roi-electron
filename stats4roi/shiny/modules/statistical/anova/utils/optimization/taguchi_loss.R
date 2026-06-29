# Taguchi loss helpers (normal surrogate) for ANOVA optimization UI.

#' One-sided ppm below LSL for Normal(mu, sigma).
ppm_normal_lower <- function(mu, sigma, lsl) {
  if (length(mu) == 0L) return(numeric(0))
  out <- rep(NA_real_, length(mu))
  ok <- is.finite(mu) & is.finite(sigma) & sigma > 0 & is.finite(lsl)
  if (any(ok)) {
    out[ok] <- stats::pnorm(lsl, mean = mu[ok], sd = sigma[ok]) * 1e6
  }
  out
}

#' One-sided ppm above USL for Normal(mu, sigma).
ppm_normal_upper <- function(mu, sigma, usl) {
  if (length(mu) == 0L) return(numeric(0))
  out <- rep(NA_real_, length(mu))
  ok <- is.finite(mu) & is.finite(sigma) & sigma > 0 & is.finite(usl)
  if (any(ok)) {
    out[ok] <- stats::pnorm(usl, mean = mu[ok], sd = sigma[ok], lower.tail = FALSE) * 1e6
  }
  out
}

#' Exact Taguchi expected loss and ppm with optional one-sided specs.
#'
#' Uses one unified model for all spec configurations:
#' - Two-sided specs: piecewise exact moments at target split
#'     loss_lower = k_l * E[(X - T)^2 I(X <= T)]
#'     loss_upper = k_u * E[(X - T)^2 I(X >  T)]
#' - One-sided specs: active-side full moment
#'     loss_side  = k_side * E[(X - T)^2]
#' with k_l = C_l / (T - LSL)^2 and k_u = C_u / (USL - T)^2.
#' Total loss is the sum of active side losses.
#' ppm remains one-sided tail probability per active spec.
taguchi_side_specific_metrics_normal <- function(
    mu,
    sigma,
    target,
    lsl = NA_real_,
    usl = NA_real_,
    C_l = 1,
    C_u = 1) {
  n <- length(mu)
  if (n == 0L) {
    return(data.frame(
      loss_lower = numeric(0),
      loss_upper = numeric(0),
      expected_loss = numeric(0),
      ppm_lower = numeric(0),
      ppm_upper = numeric(0),
      ppm = numeric(0)
    ))
  }

  C_l <- suppressWarnings(as.numeric(C_l))
  C_u <- suppressWarnings(as.numeric(C_u))
  if (length(C_l) != 1L || !is.finite(C_l) || C_l < 0) C_l <- 0
  if (length(C_u) != 1L || !is.finite(C_u) || C_u < 0) C_u <- 0

  has_lsl <- is.finite(lsl)
  has_usl <- is.finite(usl)
  base_q <- (mu - target)^2 + sigma^2

  loss_l <- rep(NA_real_, n)
  loss_u <- rep(NA_real_, n)
  ppm_l <- rep(NA_real_, n)
  ppm_u <- rep(NA_real_, n)
  has_both <- has_lsl && has_usl

  # Exact split moments around target for X ~ N(mu, sigma^2)
  m <- mu - target
  s <- sigma
  ok <- is.finite(m) & is.finite(s) & s > 0
  left_m2 <- rep(NA_real_, n)
  right_m2 <- rep(NA_real_, n)
  if (any(ok)) {
    z <- -m[ok] / s[ok]
    phi <- stats::dnorm(z)
    Phi <- stats::pnorm(z)
    left_m2[ok] <- ((m[ok]^2 + s[ok]^2) * Phi) - (s[ok] * m[ok] * phi)
    right_m2[ok] <- ((m[ok]^2 + s[ok]^2) * (1 - Phi)) + (s[ok] * m[ok] * phi)
  }

  if (has_lsl) {
    Delta_l <- suppressWarnings(as.numeric(target) - suppressWarnings(as.numeric(lsl)))
    k_l <- if (is.finite(Delta_l) && Delta_l > 0) C_l / (Delta_l^2) else NA_real_
    loss_l <- if (has_both) k_l * left_m2 else k_l * base_q
    ppm_l <- ppm_normal_lower(mu, sigma, lsl)
  }
  if (has_usl) {
    Delta_u <- suppressWarnings(as.numeric(usl) - suppressWarnings(as.numeric(target)))
    k_u <- if (is.finite(Delta_u) && Delta_u > 0) C_u / (Delta_u^2) else NA_real_
    loss_u <- if (has_both) k_u * right_m2 else k_u * base_q
    ppm_u <- ppm_normal_upper(mu, sigma, usl)
  }

  loss_total <- rowSums(cbind(loss_l, loss_u), na.rm = TRUE)
  ppm_total <- rowSums(cbind(ppm_l, ppm_u), na.rm = TRUE)
  if (!has_lsl && !has_usl) {
    loss_total[] <- NA_real_
    ppm_total[] <- NA_real_
  }

  data.frame(
    loss_lower = loss_l,
    loss_upper = loss_u,
    expected_loss = loss_total,
    ppm_lower = ppm_l,
    ppm_upper = ppm_u,
    ppm = ppm_total
  )
}
