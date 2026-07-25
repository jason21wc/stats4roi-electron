# Distribution Fitting — distribution fit dispatch
#
# Tail-focused estimation (dfit_tail_fit.R): MLE / tail-weighted regression /
# Johnson SU|SB|SL selected by minimum Anderson–Darling on the fitted CDF.
# R² is retained as a secondary probability-scale display metric.

source("modules/statistical/spc/utils/dfit_constants.R")
source("modules/statistical/spc/utils/dfit_descriptives.R")

dfit_qrayleigh <- function(p, scale, lower.tail = TRUE) {
  p <- pmin(pmax(as.numeric(p), 0), 1)
  if (!lower.tail) {
    p <- 1 - p
  }
  scale * sqrt(-2 * log(1 - p))
}

dfit_prayleigh <- function(x, scale) {
  x <- pmax(as.numeric(x), 0)
  1 - exp(-x^2 / (2 * scale^2))
}

dfit_nonnormal_capability_fn <- function() {
  get("spc.capability.summary.ungrouped.nonnormal.simple.R", envir = asNamespace("lolcat"))
}

dfit_fit_r2 <- function(x, pfun) {
  x <- stats::na.omit(as.numeric(x))
  n <- length(x)
  if (n < 2L) {
    return(NA_real_)
  }
  xs <- sort(x)
  p_emp <- stats::ppoints(n)
  p_theo <- pfun(xs)
  stats::cor(p_emp, p_theo, use = "complete.obs")^2
}

dfit_nt_from_qfun <- function(qfun) {
  nt <- lolcat::natural.tolerance(qfun)
  list(
    lpl = nt$lower.limit,
    upl = nt$upper.limit,
    nt = nt$natural.tolerance
  )
}

dfit_johnson_family_label <- function(transform) {
  tr <- tolower(as.character(transform))
  switch(
    tr,
    su = "Su",
    sb = "Sb",
    sl = "Sl",
    paste0(toupper(substr(tr, 1L, 1L)), substr(tr, 2L, nchar(tr)))
  )
}

dfit_johnson_transform <- function(family) {
  fam <- tolower(if (is.null(family)) "su" else as.character(family))
  switch(
    fam,
    su = lolcat::transform.johnson.su,
    sb = lolcat::transform.johnson.sb,
    sl = lolcat::transform.johnson.sl,
    lolcat::transform.johnson.su
  )
}

dfit_johnson_cdf <- function(v, params) {
  tr <- dfit_johnson_transform(params$family)
  z <- tr(
    v,
    gamma = params$gamma,
    eta = params$eta,
    lambda = params$lambda,
    epsilon = params$epsilon,
    inverse = FALSE
  )
  stats::pnorm(z)
}

dfit_johnson_qinv <- function(p, params, lower.tail = TRUE) {
  z <- stats::qnorm(p, lower.tail = lower.tail)
  tr <- dfit_johnson_transform(params$family)
  tr(
    z,
    gamma = params$gamma,
    eta = params$eta,
    lambda = params$lambda,
    epsilon = params$epsilon,
    inverse = TRUE
  )
}

dfit_nt_for_distribution <- function(x, distribution_id, params) {
  id <- as.integer(distribution_id)
  if (id == 1L) {
    nt <- lolcat::natural.tolerance.normal(x)
    return(list(lpl = nt$lower.limit, upl = nt$upper.limit, nt = nt$natural.tolerance))
  }
  if (id == 2L) {
    nt <- lolcat::natural.tolerance.exp(x)
    return(list(lpl = nt$lower.limit, upl = nt$upper.limit, nt = nt$natural.tolerance))
  }
  if (id == 3L) {
    nt <- lolcat::natural.tolerance.exp.low(x)
    return(list(lpl = nt$lower.limit, upl = nt$upper.limit, nt = nt$natural.tolerance))
  }
  qfun <- dfit_build_qfun(id, params)
  if (is.null(qfun)) {
    return(list(lpl = NA_real_, upl = NA_real_, nt = NA_real_))
  }
  dfit_nt_from_qfun(qfun)
}

dfit_distribution_mean_numeric <- function(fit, x) {
  pfun <- fit$pfun
  x <- stats::na.omit(as.numeric(x))
  if (is.null(pfun) || length(x) < 2L) {
    return(NA_real_)
  }
  xr <- range(x)
  pad <- max(diff(xr) * 0.25, 1e-6)
  grid <- seq(xr[1] - pad, xr[2] + pad, length.out = 400L)
  dx <- grid[2] - grid[1]
  dens <- vapply(grid, function(v) {
    ph <- pfun(v + dx / 2)
    pl <- pfun(v - dx / 2)
    max((ph - pl) / dx, 0)
  }, numeric(1))
  s <- sum(dens)
  if (!is.finite(s) || s <= 0) {
    return(NA_real_)
  }
  sum(grid * dens) / s
}

dfit_distribution_mean <- function(fit, x = NULL) {
  if (is.null(fit) || as.integer(fit$distribution_id) == 0L || is.null(fit$params)) {
    return(NA_real_)
  }
  id <- as.integer(fit$distribution_id)
  p <- fit$params
  mu <- switch(
    id,
    `1` = p$mean,
    `2` = 1 / p$rate,
    `3` = p$min + 1 / p$rate,
    `4` = if (is.finite(p$shape) && is.finite(p$scale) && p$shape > 0) {
      p$scale * exp(lgamma(1 + 1 / p$shape))
    } else {
      NA_real_
    },
    `5` = (p$min + p$max) / 2,
    `6` = NA_real_,
    `7` = p$shape / p$rate,
    `8` = p$min + p$shape / p$rate,
    `9` = {
      sc <- p$max - p$min
      if (sc <= 0) NA_real_ else p$min + sc * p$shape1 / (p$shape1 + p$shape2)
    },
    `10` = p$scale * sqrt(pi / 2),
    `11` = p$min + p$scale * sqrt(pi / 2),
    NA_real_
  )
  if (is.finite(mu)) {
    return(mu)
  }
  dfit_distribution_mean_numeric(fit, x)
}

dfit_build_qfun <- function(distribution_id, params) {
  id <- as.integer(distribution_id)
  switch(
    id,
    `1` = {
      function(p, lower.tail) stats::qnorm(p, mean = params$mean, sd = params$sd, lower.tail = lower.tail)
    },
    `2` = {
      function(p, lower.tail) {
        stats::qexp(p, rate = params$rate, lower.tail = lower.tail)
      }
    },
    `3` = {
      function(p, lower.tail) {
        loc <- params$min
        stats::qexp(p, rate = params$rate, lower.tail = lower.tail) + loc
      }
    },
    `4` = {
      function(p, lower.tail) {
        stats::qweibull(p, shape = params$shape, scale = params$scale, lower.tail = lower.tail)
      }
    },
    `5` = {
      function(p, lower.tail) {
        stats::qunif(p, min = params$min, max = params$max, lower.tail = lower.tail)
      }
    },
    `6` = {
      function(p, lower.tail) {
        dfit_johnson_qinv(p, params, lower.tail = lower.tail)
      }
    },
    `7` = {
      function(p, lower.tail) {
        stats::qgamma(p, shape = params$shape, rate = params$rate, lower.tail = lower.tail)
      }
    },
    `8` = {
      function(p, lower.tail) {
        loc <- params$min
        stats::qgamma(p, shape = params$shape, rate = params$rate, lower.tail = lower.tail) + loc
      }
    },
    `9` = {
      function(p, lower.tail) {
        loc <- params$min
        sc <- params$max - params$min
        stats::qbeta(p, shape1 = params$shape1, shape2 = params$shape2, lower.tail = lower.tail) * sc + loc
      }
    },
    `10` = {
      function(p, lower.tail) {
        dfit_qrayleigh(p, scale = params$scale, lower.tail = lower.tail)
      }
    },
    `11` = {
      function(p, lower.tail) {
        loc <- params$min
        dfit_qrayleigh(p, scale = params$scale, lower.tail = lower.tail) + loc
      }
    },
    NULL
  )
}

dfit_build_pfun <- function(distribution_id, params) {
  id <- as.integer(distribution_id)
  switch(
    id,
    `1` = function(v) stats::pnorm(v, mean = params$mean, sd = params$sd),
    `2` = function(v) stats::pexp(v, rate = params$rate),
    `3` = function(v) {
      loc <- params$min
      stats::pexp(pmax(v - loc, 0), rate = params$rate)
    },
    `4` = function(v) stats::pweibull(v, shape = params$shape, scale = params$scale),
    `5` = function(v) stats::punif(v, min = params$min, max = params$max),
    `6` = function(v) dfit_johnson_cdf(v, params),
    `7` = function(v) stats::pgamma(v, shape = params$shape, rate = params$rate),
    `8` = function(v) {
      loc <- params$min
      stats::pgamma(pmax(v - loc, 0), shape = params$shape, rate = params$rate)
    },
    `9` = function(v) {
      loc <- params$min
      sc <- params$max - params$min
      if (sc <= 0) return(rep(NA_real_, length(v)))
      stats::pbeta(pmin(pmax((v - loc) / sc, 0), 1), shape1 = params$shape1, shape2 = params$shape2)
    },
    `10` = function(v) dfit_prayleigh(v, scale = params$scale),
    `11` = function(v) {
      loc <- params$min
      dfit_prayleigh(pmax(v - loc, 0), scale = params$scale)
    },
    NULL
  )
}

source("modules/statistical/spc/utils/dfit_tail_fit.R")

dfit_estimate_params_raw <- function(x, distribution_id) {
  x <- stats::na.omit(as.numeric(x))
  id <- as.integer(distribution_id)
  if (!length(x) || id == 0L) {
    return(NULL)
  }

  switch(
    id,
    `1` = list(mean = mean(x), sd = stats::sd(x)),
    `2` = list(rate = 1 / mean(x), min = 0),
    `3` = {
      loc <- min(x)
      list(rate = 1 / (mean(x) - loc), min = loc)
    },
    `4` = dfit_weibull_fit(x),
    `5` = list(min = min(x), max = max(x)),
    `6` = dfit_johnson_fit(x),
    `7` = dfit_gamma_fit(x, low = FALSE),
    `8` = dfit_gamma_fit(x, low = TRUE),
    `9` = dfit_beta_fit(x),
    `10` = list(scale = sqrt(mean(x^2) / 2), min = 0),
    `11` = {
      loc <- min(x)
      y <- x - loc
      list(scale = sqrt(mean(y^2) / 2), min = loc)
    },
    NULL
  )
}

dfit_estimate_params <- function(x, distribution_id) {
  raw <- dfit_estimate_params_raw(x, distribution_id)
  if (is.null(raw)) {
    return(NULL)
  }
  raw[setdiff(names(raw), c("fit_method", "fit_ad"))]
}

dfit_apply_overrides <- function(params, overrides, distribution_id) {
  if (is.null(params) || is.null(overrides) || !length(overrides)) {
    return(params)
  }
  fields <- dfit_param_fields(distribution_id)
  for (nm in names(overrides)) {
    if (!nm %in% names(params)) {
      next
    }
    val <- overrides[[nm]]
    if (nm == "family") {
      if (!is.null(val) && nzchar(as.character(val))) {
        params[[nm]] <- as.character(val)
      }
    } else if (!is.null(val) && is.finite(val)) {
      params[[nm]] <- val
    }
  }
  params
}

dfit_fit_distribution <- function(x, distribution_id, overrides = NULL) {
  id <- as.integer(distribution_id)
  if (id == 0L) {
    return(list(distribution_id = 0L, params = NULL, lpl = NA, upl = NA, nt = NA, fit = NA))
  }

  raw <- dfit_estimate_params_raw(x, id)
  fit_method <- if (!is.null(raw$fit_method)) raw$fit_method else NULL
  fit_ad <- if (!is.null(raw$fit_ad)) raw$fit_ad else NA_real_

  params <- if (is.null(raw)) NULL else raw[setdiff(names(raw), c("fit_method", "fit_ad"))]
  params <- dfit_apply_overrides(params, overrides, id)
  if (is.null(params)) {
    return(list(distribution_id = id, params = NULL, error = "Could not estimate distribution parameters."))
  }

  qfun <- dfit_build_qfun(id, params)
  pfun <- dfit_build_pfun(id, params)
  nt_vals <- dfit_nt_for_distribution(x, id, params)
  fit_r2 <- dfit_fit_r2(x, pfun)
  if (!length(overrides) && is.finite(fit_ad)) {
    ad_val <- fit_ad
  } else {
    ad_val <- dfit_ad_statistic(x, pfun)
  }
  tail_q_rmse <- if (!is.null(qfun)) dfit_tail_quantile_rmse(x, qfun) else NA_real_

  out <- list(
    distribution_id = id,
    distribution_label = dfit_distribution_label(id),
    params = params,
    lpl = nt_vals$lpl,
    upl = nt_vals$upl,
    nt = nt_vals$nt,
    fit = fit_r2,
    fit_r2 = fit_r2,
    fit_ad = ad_val,
    fit_tail_q_rmse = tail_q_rmse,
    fit_method = fit_method,
    qfun = qfun,
    pfun = pfun
  )

  if (id == 6L && !is.null(params$family)) {
    out$johnson_family <- params$family
    out$criteria <- params$criteria
  }

  out
}

dfit_parse_carb_data <- function(path) {
  stats::na.omit(scan(path, quiet = TRUE))
}
