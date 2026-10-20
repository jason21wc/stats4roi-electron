# Exact binomial CIs for proportions of scores above/below a location.
# Uses lolcat Clopper–Pearson via proportion.test.onesample.exact.simple;
# π_below CI is the complement of π_above: rev(1 - ci_above).

#' Exact CIs for π_above and π_below (ties excluded)
#'
#' @param n_above Count of observations strictly above the location
#' @param n_below Count of observations strictly below the location
#' @param conf.level Confidence level in (0, 1)
#' @return Named list: ci_above, ci_below (length-2 numeric), p_above, p_below, n_included
np_location_proportion_cis <- function(n_above, n_below, conf.level = 0.95) {
  n_above <- as.numeric(n_above)[1L]
  n_below <- as.numeric(n_below)[1L]
  conf.level <- as.numeric(conf.level)[1L]

  if (!is.finite(n_above) || !is.finite(n_below) || n_above < 0 || n_below < 0) {
    stop("n_above and n_below must be finite non-negative numbers.", call. = FALSE)
  }
  if (!is.finite(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be in (0, 1).", call. = FALSE)
  }

  n_included <- n_above + n_below
  if (n_included <= 0) {
    return(list(
      ci_above = c(NA_real_, NA_real_),
      ci_below = c(NA_real_, NA_real_),
      p_above = NA_real_,
      p_below = NA_real_,
      n_included = 0
    ))
  }

  p_above <- n_above / n_included
  p_below <- n_below / n_included

  ci_above <- lolcat::proportion.test.onesample.exact.simple(
    sample.proportion = p_above,
    sample.size = n_included,
    null.hypothesis.proportion = 0.5,
    alternative = "two.sided",
    conf.level = conf.level
  )$conf.int

  ci_above <- as.numeric(ci_above)[1:2]
  ci_below <- rev(1 - ci_above)

  list(
    ci_above = ci_above,
    ci_below = ci_below,
    p_above = p_above,
    p_below = p_below,
    n_included = n_included
  )
}
