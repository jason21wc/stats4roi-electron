# Distribution Fitting descriptive statistics

source("modules/statistical/spc/utils/spc_limit_calcs.R")

dfit_sd_potential <- function(x, mr_span = 2L) {
  x <- stats::na.omit(as.numeric(x))
  if (length(x) < 2L) {
    return(NA_real_)
  }
  mr <- MR_span(data = x, span = mr_span)
  mr <- mr[!is.na(mr)]
  if (!length(mr)) {
    return(NA_real_)
  }
  # SPC Individuals + Median MR dispersion (disp_lim_calc == 7): median(MR) / d4
  d4 <- lolcat::spc.constant.calculation.d4(sample.size = mr_span)
  stats::median(mr) / d4
}

dfit_descriptives <- function(x, sd_potential = NULL, mr_span = 2L) {
  x <- stats::na.omit(as.numeric(x))
  n <- length(x)
  if (n == 0L) {
    return(list(
      n = 0L, mean = NA_real_, median = NA_real_, sd = NA_real_,
      sd_potential = NA_real_, low = NA_real_, high = NA_real_, range = NA_real_,
      skewness = NA_real_, kurtosis = NA_real_
    ))
  }
  if (is.null(sd_potential)) {
    sd_potential <- dfit_sd_potential(x, mr_span = mr_span)
  }
  low <- min(x)
  high <- max(x)
  list(
    n = n,
    mean = mean(x),
    median = stats::median(x),
    sd = stats::sd(x),
    sd_potential = sd_potential,
    low = low,
    high = high,
    range = high - low,
    skewness = dfit_sample_skewness(x),
    kurtosis = dfit_sample_kurtosis(x)
  )
}

dfit_sample_skewness <- function(x) {
  x <- stats::na.omit(as.numeric(x))
  if (length(x) < 3L) {
    return(NA_real_)
  }
  result <- lolcat::skewness.test(x = x, conf.level = 0.95, alternative = "two.sided")
  unname(result$estimate["skewness"])
}

dfit_sample_kurtosis <- function(x) {
  x <- stats::na.omit(as.numeric(x))
  if (length(x) < 4L) {
    return(NA_real_)
  }
  result <- lolcat::kurtosis.test(x = x, conf.level = 0.95, alternative = "two.sided")
  unname(result$estimate["kurtosis"])
}
