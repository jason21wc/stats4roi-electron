# Distribution Fitting capability indices

source("modules/statistical/spc/server/spc_capability_server.R")

dfit_capability <- function(x, spec, fit, sd_potential) {
  x <- stats::na.omit(as.numeric(x))
  n <- length(x)
  lsl <- spec$lsl
  usl <- spec$usl
  target <- spec$target
  has_lsl <- is.finite(lsl)
  has_usl <- is.finite(usl)
  has_specs <- has_lsl || has_usl

  out <- list(
    cp = NA_real_,
    cpk = NA_real_,
    cpm = NA_real_,
    cp_potential = NA_real_,
    est_pct_below = NA_real_,
    est_pct_above = NA_real_,
    est_pct_out = NA_real_,
    is_normal = FALSE
  )

  if (!has_specs || n < 2L || is.null(fit) || fit$distribution_id == 0L) {
    return(out)
  }

  mean_x <- mean(x)
  sd_x <- stats::sd(x)
  pfun <- fit$pfun

  p_lower <- if (has_lsl) pfun(lsl) else 0
  p_upper <- if (has_usl) 1 - pfun(usl) else 0
  p_lower <- max(0, min(1, p_lower))
  p_upper <- max(0, min(1, p_upper))

  out$est_pct_below <- if (has_lsl) 100 * p_lower else NA_real_
  out$est_pct_above <- if (has_usl) 100 * p_upper else NA_real_
  out$est_pct_out <- if (has_lsl && has_usl) {
    100 * (p_lower + p_upper)
  } else if (has_lsl) {
    out$est_pct_below
  } else {
    out$est_pct_above
  }

  if (is.finite(target)) {
    out$cpm <- unname(lolcat::spc.capability.cpm.simple(
      lower.specification = lsl,
      upper.specification = usl,
      process.variability = sd_x^2,
      process.center = mean_x,
      nominal.center = target,
      n.sigma = 6
    ))
  }

  if (!is.null(fit$nt) && is.finite(fit$nt) && fit$nt > 0 && has_lsl && has_usl) {
    out$cp <- (usl - lsl) / fit$nt
  }

  if (is.finite(sd_potential) && sd_potential > 0 && has_lsl && has_usl) {
    out$cp_potential <- (usl - lsl) / (6 * sd_potential)
  }

  if (fit$distribution_id == 1L) {
    out$is_normal <- TRUE
    out$cpk <- unname(lolcat::spc.capability.cpk.simple(
      lower.specification = lsl,
      upper.specification = usl,
      process.variability = sd_x^2,
      process.center = mean_x,
      n.sigma = 6
    ))
  } else {
    pct_usl <- if (has_usl) out$est_pct_above else 0
    pct_lsl <- if (has_lsl) out$est_pct_below else 0
    out$cpk <- calculate_capability_cpk_nonnormal(pct_usl, pct_lsl, 3)
  }

  out
}
