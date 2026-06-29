# SPC Capability Calculations Worker Module
# Pure calculation functions for manual-entry capability measures (Cp, Cpk, Cpm).

calculate_capability_cpk_nonnormal <- function(pct_usl, pct_lsl, divisor) {
  max_pct <- max(pct_usl, pct_lsl, na.rm = TRUE)
  if (is.na(max_pct) || max_pct < 0 || max_pct > 100) {
    return(NA_real_)
  }
  if (max_pct >= 100) {
    return(0)
  }
  if (max_pct <= 0) {
    return(Inf)
  }
  z <- stats::qnorm(1 - max_pct / 100)
  z / divisor
}

calculate_capability_measures <- function(
  distribution,
  study,
  usl,
  target,
  lsl,
  mean,
  sd,
  upl,
  lpl,
  pct_usl,
  pct_lsl,
  R
) {
  if (is.na(mean) || is.na(sd) || sd <= 0) {
    return(list(error = "Mean and a positive standard deviation are required."))
  }
  if (is.na(usl) && is.na(lsl)) {
    return(list(error = "At least one specification limit (USL or LSL) is required."))
  }

  is_normal <- as.numeric(distribution) == 1
  is_short_term <- as.numeric(study) == 1
  n_sigma <- if (is_short_term) 8 else 6

  if (is_normal) {
    natural_tol <- if (is_short_term) 8 * sd else 6 * sd
    cpk <- spc.capability.cpk.simple(
      lower.specification = lsl,
      upper.specification = usl,
      process.variability = sd^2,
      process.center = mean,
      n.sigma = n_sigma
    )
  } else {
    if (is.na(upl) || is.na(lpl)) {
      return(list(error = "Upper and Lower Process Limits are required for nonnormal distribution."))
    }
    if (upl <= lpl) {
      return(list(error = "Upper Process Limit must be greater than Lower Process Limit."))
    }
    if (is.na(pct_usl) || is.na(pct_lsl) || pct_usl < 0 || pct_usl > 100 || pct_lsl < 0 || pct_lsl > 100) {
      return(list(error = "Est % Out USL and Est % Out LSL must be between 0 and 100."))
    }
    natural_tol <- upl - lpl
    cpk_divisor <- if (is_short_term) 4 else 3
    cpk <- calculate_capability_cpk_nonnormal(pct_usl, pct_lsl, cpk_divisor)
  }

  cp <- spc.capability.cp.simple(
    lower.specification = lsl,
    upper.specification = usl,
    process.center = mean,
    process.natural.tolerance = natural_tol
  )

  cpm <- spc.capability.cpm.simple(
    lower.specification = lsl,
    upper.specification = usl,
    process.variability = sd^2,
    process.center = mean,
    nominal.center = target,
    n.sigma = n_sigma
  )

  list(
    cp = ro(cp, R),
    cpk = ro(cpk, R),
    cpm = ro(cpm, R),
    error = NULL,
    distribution_label = names(choice_cap_distribution)[as.numeric(distribution)],
    study_label = names(choice_cap_study)[as.numeric(study)]
  )
}
