# Map predicted dispersion metrics (ADA / ADM / ADMn1) to operational sigma for normal
# loss and ppm. Plan: PR may refine constants vs thesis/Excel.

#' @param metric_value Resolved dispersion metric (non-negative; use tier resolution before calling).
#' @keywords internal
dispersion_metric_to_sigma <- function(metric_value, type = c("ADA", "ADM", "ADMn1")) {
  type <- match.arg(type)
  v <- suppressWarnings(as.numeric(metric_value))
  if (length(v) != 1L || !is.finite(v) || v < 0) {
    return(NA_real_)
  }
  # For Normal(0,sigma^2), E|X-mu| = sigma*sqrt(2/pi) => sigma = ADA * sqrt(pi/2).
  k <- sqrt(pi / 2)
  switch(type,
    ADA = k * v,
    ADM = k * v,
    ADMn1 = k * v
  )
}

#' Long-term normal capability indices at the optimized setting (6-sigma tolerance).
#' Uses lolcat \code{spc.capability.*.simple} (same as SPC Capability tab).
#' @keywords internal
optimizer_capability_measures <- function(mu, sigma, target = NA_real_, lsl = NA_real_, usl = NA_real_) {
  mu <- suppressWarnings(as.numeric(mu))[1]
  sigma <- suppressWarnings(as.numeric(sigma))[1]
  target <- suppressWarnings(as.numeric(target))[1]
  lsl <- suppressWarnings(as.numeric(lsl))[1]
  usl <- suppressWarnings(as.numeric(usl))[1]
  if (!is.finite(mu) || !is.finite(sigma) || sigma <= 0) {
    return(list(cp = NA_real_, cpk = NA_real_, cpm = NA_real_))
  }
  if (!is.finite(lsl) && !is.finite(usl)) {
    return(list(cp = NA_real_, cpk = NA_real_, cpm = NA_real_))
  }
  if (!requireNamespace("lolcat", quietly = TRUE)) {
    return(list(cp = NA_real_, cpk = NA_real_, cpm = NA_real_))
  }
  natural_tol <- 6 * sigma
  cp <- suppressWarnings(lolcat::spc.capability.cp.simple(
    lower.specification = lsl,
    upper.specification = usl,
    process.center = mu,
    process.natural.tolerance = natural_tol
  ))
  cpk <- suppressWarnings(lolcat::spc.capability.cpk.simple(
    lower.specification = lsl,
    upper.specification = usl,
    process.variability = sigma^2,
    process.center = mu,
    n.sigma = 6
  ))
  cpm <- if (is.finite(target)) {
    suppressWarnings(lolcat::spc.capability.cpm.simple(
      lower.specification = lsl,
      upper.specification = usl,
      process.variability = sigma^2,
      process.center = mu,
      nominal.center = target,
      n.sigma = 6
    ))
  } else {
    NA_real_
  }
  list(cp = cp, cpk = cpk, cpm = cpm)
}
