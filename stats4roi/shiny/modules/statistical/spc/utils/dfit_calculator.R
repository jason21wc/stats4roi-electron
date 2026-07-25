# Distribution calculator — proportion and quantile from fitted distribution

dfit_distribution_calculate <- function(fit, mode, value = NA, proportion = NA, tail = c("lower", "upper")) {
  tail <- match.arg(tail)
  if (is.null(fit) || is.null(fit$pfun) || is.null(fit$qfun)) {
    return(list(error = "Select a distribution first."))
  }
  pfun <- fit$pfun
  qfun <- fit$qfun

  if (mode == "proportion") {
    if (!is.finite(value)) {
      return(list(error = "Enter a numeric value."))
    }
    p_below <- pfun(value)
    p_below <- max(0, min(1, p_below))
    list(
      value = value,
      p_below = 100 * p_below,
      p_above = 100 * (1 - p_below)
    )
  } else if (mode == "quantile") {
    if (!is.finite(proportion) || proportion < 0 || proportion > 100) {
      return(list(error = "Proportion must be between 0 and 100."))
    }
    p <- proportion / 100
    if (tail == "upper") {
      x <- qfun(1 - p, lower.tail = TRUE)
    } else {
      x <- qfun(p, lower.tail = TRUE)
    }
    list(proportion = proportion, tail = tail, value = x)
  } else {
    list(error = "Unknown calculator mode.")
  }
}
