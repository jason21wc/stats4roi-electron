# Distribution Fitting conformance (observed vs specification limits)

dfit_conformance <- function(x, spec = list(lsl = NA_real_, target = NA_real_, usl = NA_real_)) {
  x <- stats::na.omit(as.numeric(x))
  n <- length(x)
  lsl <- spec$lsl
  target <- spec$target
  usl <- spec$usl

  has_lsl <- is.finite(lsl)
  has_usl <- is.finite(usl)
  has_target <- is.finite(target)

  n_below <- if (has_lsl) sum(x < lsl) else NA_integer_
  n_above <- if (has_usl) sum(x > usl) else NA_integer_
  n_out <- if (has_lsl && has_usl) {
    n_below + n_above
  } else if (has_lsl) {
    n_below
  } else if (has_usl) {
    n_above
  } else {
    NA_integer_
  }

  pct_below <- if (has_lsl && n > 0) 100 * n_below / n else NA_real_
  pct_above <- if (has_usl && n > 0) 100 * n_above / n else NA_real_
  pct_out <- if (n > 0 && (has_lsl || has_usl)) {
    100 * n_out / n
  } else {
    NA_real_
  }

  pct_off_target <- NA_real_
  if (has_target && n > 0) {
    mean_x <- mean(x)
    if (has_lsl && has_usl && usl > lsl) {
      pct_off_target <- 100 * abs(mean_x - target) / (usl - lsl)
    } else if (has_usl) {
      pct_off_target <- 100 * abs(mean_x - target) / abs(usl - target)
    } else if (has_lsl) {
      pct_off_target <- 100 * abs(mean_x - target) / abs(target - lsl)
    }
  }

  list(
    lsl = lsl,
    target = target,
    usl = usl,
    n_below = n_below,
    n_above = n_above,
    n_out = n_out,
    pct_below = pct_below,
    pct_above = pct_above,
    pct_out = pct_out,
    pct_off_target = pct_off_target
  )
}
