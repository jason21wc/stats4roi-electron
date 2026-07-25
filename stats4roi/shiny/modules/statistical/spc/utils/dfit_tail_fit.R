# Tail-focused distribution estimation helpers
#
# Parameter estimates are chosen to minimize Anderson–Darling distance on the
# fitted CDF (tail-sensitive). R² remains a secondary display metric.

# Quantile probabilities used by lolcat::natural.tolerance (per tail)
DFIT_NT_LOWER_P <- 0.00135
DFIT_NT_UPPER_P <- 0.99865

dfit_ad_statistic <- function(x, pfun) {
  x <- stats::na.omit(as.numeric(x))
  x <- sort(x)
  n <- length(x)
  if (n < 3L) {
    return(NA_real_)
  }
  F <- pfun(x)
  F <- pmin(pmax(F, 1e-10), 1 - 1e-10)
  i <- seq_len(n)
  -n - (1 / n) * sum((2 * i - 1) * (log(F) + log(1 - F[n + 1L - i])))
}

dfit_tail_quantile_rmse <- function(x, qfun) {
  x <- stats::na.omit(as.numeric(x))
  n <- length(x)
  if (n < 2L) {
    return(NA_real_)
  }
  probs <- c(
    DFIT_NT_LOWER_P, 0.01, 0.05, 0.5, 0.95, 0.99, DFIT_NT_UPPER_P
  )
  emp <- stats::quantile(x, probs = probs, names = FALSE, type = 7)
  theo <- vapply(probs, function(p) qfun(p, lower.tail = TRUE), numeric(1))
  if (!all(is.finite(theo))) {
    return(NA_real_)
  }
  sqrt(mean((emp - theo)^2))
}

dfit_johnson_params_from_row <- function(row) {
  list(
    family = dfit_johnson_family_label(row$transform),
    gamma = unname(row$gamma),
    eta = unname(row$eta),
    lambda = unname(row$lambda),
    epsilon = unname(row$epsilon),
    criteria = unname(row$mn.over.p.sq),
    z = unname(row$z)
  )
}

dfit_gamma_mom <- function(y, allow_zero = FALSE) {
  y <- stats::na.omit(as.numeric(y))
  y <- if (allow_zero) y[y >= 0] else y[y > 0]
  if (length(y) < 2L) {
    return(NULL)
  }
  m <- mean(y)
  v <- stats::var(y)
  if (!is.finite(v) || v <= 0) {
    return(NULL)
  }
  list(shape = m^2 / v, rate = m / v)
}

dfit_beta_mom <- function(x) {
  x <- stats::na.omit(as.numeric(x))
  if (length(x) < 2L) {
    return(NULL)
  }
  loc <- min(x)
  sc <- max(x) - loc
  if (sc <= 0) {
    return(NULL)
  }
  y <- (x - loc) / sc
  m <- mean(y)
  v <- stats::var(y)
  if (!is.finite(v) || v <= 0) {
    return(NULL)
  }
  common <- m * (1 - m) / v - 1
  if (!is.finite(common) || common <= 0) {
    return(NULL)
  }
  list(
    shape1 = m * common,
    shape2 = (1 - m) * common,
    min = loc,
    max = max(x)
  )
}

dfit_gamma_fit <- function(x, low = FALSE) {
  if (!low) {
    mom <- dfit_gamma_mom(x)
    mle <- dfit_gamma_mle(x)
    return(dfit_pick_params_by_ad(
      x,
      7L,
      list(
        mle = if (is.null(mle)) NULL else c(mle, list(min = 0)),
        mom = if (is.null(mom)) NULL else c(mom, list(min = 0))
      )
    ))
  }
  loc <- min(x)
  y <- x - loc
  mom <- dfit_gamma_mom(y, allow_zero = TRUE)
  mle <- dfit_gamma_mle(y, allow_zero = TRUE)
  dfit_pick_params_by_ad(
    x,
    8L,
    list(
      mle = if (is.null(mle)) NULL else c(mle, list(min = loc)),
      mom = if (is.null(mom)) NULL else c(mom, list(min = loc))
    )
  )
}

dfit_beta_fit <- function(x) {
  dfit_pick_params_by_ad(
    x,
    9L,
    list(
      mle = dfit_beta_mle(x),
      mom = dfit_beta_mom(x)
    )
  )
}

dfit_weibull_mle <- function(x) {
  x <- stats::na.omit(as.numeric(x))
  x <- x[x > 0]
  if (length(x) < 3L) {
    return(NULL)
  }
  fw <- tryCatch(
    MASS::fitdistr(x, "weibull"),
    error = function(e) NULL
  )
  if (is.null(fw)) {
    return(NULL)
  }
  list(
    shape = unname(fw$estimate["shape"]),
    scale = unname(fw$estimate["scale"])
  )
}

dfit_weibull_tail_rr <- function(x) {
  x <- stats::na.omit(as.numeric(x))
  x <- x[x > 0]
  n <- length(x)
  if (n < 3L) {
    return(NULL)
  }
  xs <- sort(x)
  p <- stats::ppoints(n)
  y <- log(-log(1 - p))
  lx <- log(xs)
  w <- 1 / (p * (1 - p))
  fit <- stats::lm(y ~ lx, weights = w)
  shape <- unname(stats::coef(fit)[2])
  scale <- exp(-unname(stats::coef(fit)[1]) / shape)
  if (!is.finite(shape) || shape <= 0 || !is.finite(scale) || scale <= 0) {
    return(NULL)
  }
  list(shape = shape, scale = scale)
}

dfit_pick_params_by_ad <- function(x, distribution_id, candidates) {
  x <- stats::na.omit(as.numeric(x))
  id <- as.integer(distribution_id)
  best <- NULL
  best_ad <- Inf
  best_method <- NULL

  for (method in names(candidates)) {
    params <- candidates[[method]]
    if (is.null(params)) {
      next
    }
    pfun <- dfit_build_pfun(id, params)
    if (is.null(pfun)) {
      next
    }
    ad <- dfit_ad_statistic(x, pfun)
    if (is.finite(ad) && ad < best_ad) {
      best_ad <- ad
      best <- params
      best_method <- method
    }
  }

  if (is.null(best)) {
    return(NULL)
  }
  best$fit_method <- best_method
  best$fit_ad <- best_ad
  best
}

dfit_weibull_fit <- function(x) {
  dfit_pick_params_by_ad(
    x,
    4L,
    list(
      mle = dfit_weibull_mle(x),
      tail_rr = dfit_weibull_tail_rr(x)
    )
  )
}

dfit_gamma_mle <- function(y, allow_zero = FALSE) {
  y <- stats::na.omit(as.numeric(y))
  y <- if (allow_zero) y[y >= 0] else y[y > 0]
  if (length(y) < 3L) {
    return(NULL)
  }
  if (allow_zero && any(y == 0)) {
    y <- y + .Machine$double.eps
  }
  fg <- tryCatch(
    MASS::fitdistr(y, "gamma"),
    error = function(e) NULL
  )
  if (is.null(fg)) {
    return(NULL)
  }
  list(
    shape = unname(fg$estimate["shape"]),
    rate = unname(fg$estimate["rate"])
  )
}

dfit_beta_mle <- function(x) {
  x <- stats::na.omit(as.numeric(x))
  if (length(x) < 3L) {
    return(NULL)
  }
  loc <- min(x)
  sc <- max(x) - loc
  if (sc <= 0) {
    return(NULL)
  }
  y <- (x - loc) / sc
  y <- pmin(pmax(y, 1e-6), 1 - 1e-6)
  fb <- tryCatch(
    MASS::fitdistr(y, "beta", start = list(shape1 = 1, shape2 = 1)),
    error = function(e) NULL
  )
  if (is.null(fb)) {
    return(NULL)
  }
  list(
    shape1 = unname(fb$estimate["shape1"]),
    shape2 = unname(fb$estimate["shape2"]),
    min = loc,
    max = max(x)
  )
}

dfit_johnson_fit <- function(x) {
  x <- stats::na.omit(as.numeric(x))
  if (length(x) < 4L) {
    return(NULL)
  }
  ej <- lolcat::explore.johnson(x)
  ej <- ej[
    is.finite(ej$gamma) & is.finite(ej$eta) & is.finite(ej$lambda),
    ,
    drop = FALSE
  ]
  if (!nrow(ej)) {
    return(NULL)
  }

  candidates <- do.call(
    rbind,
    lapply(split(ej, ej$transform), function(d) {
      head(d[order(d$mn.over.p.sq), , drop = FALSE], 10L)
    })
  )

  best <- NULL
  best_ad <- Inf
  for (i in seq_len(nrow(candidates))) {
    params <- dfit_johnson_params_from_row(candidates[i, , drop = FALSE])
    pfun <- dfit_build_pfun(6L, params)
    if (is.null(pfun)) {
      next
    }
    ad <- dfit_ad_statistic(x, pfun)
    if (is.finite(ad) && ad < best_ad) {
      best_ad <- ad
      best <- params
      best$fit_method <- paste0("johnson_", tolower(params$family))
      best$fit_ad <- best_ad
    }
  }
  best
}
