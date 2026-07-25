# MLE confidence bounds for 2-parameter Weibull (Fisher / LR / Bayesian)

.weibull_nll_par <- function(par, fail, cens) {
  beta <- exp(par[1])
  eta <- exp(par[2])
  if (!is.finite(beta) || !is.finite(eta) || beta <= 0 || eta <= 0) return(1e12)
  lf <- log(beta / eta) + (beta - 1) * log(fail / eta) - (fail / eta)^beta
  lc <- if (length(cens) > 0L) -(cens / eta)^beta else 0
  ll <- sum(lf) + sum(lc)
  if (!is.finite(ll)) return(1e12)
  -ll
}

.weibull_ll_par <- function(par, fail, cens) {
  - .weibull_nll_par(par, fail, cens)
}

.weibull_hessian_fd <- function(fn, par, eps = 1e-5) {
  n <- length(par)
  H <- matrix(0, n, n)
  f0 <- fn(par)
  for (i in seq_len(n)) {
    ei <- rep(0, n)
    ei[i] <- eps
    for (j in seq_len(n)) {
      ej <- rep(0, n)
      ej[j] <- eps
      H[i, j] <- (fn(par + ei + ej) - fn(par + ei - ej) - fn(par - ei + ej) + fn(par - ei - ej)) / (4 * eps * eps)
    }
  }
  H
}

.weibull_mle_covariance <- function(par, fail, cens) {
  H <- .weibull_hessian_fd(
    function(p) .weibull_nll_par(p, fail, cens),
    par
  )
  cov_log <- tryCatch(solve(H), error = function(e) NULL)
  if (is.null(cov_log)) return(NULL)
  beta <- exp(par[1])
  eta <- exp(par[2])
  J <- diag(c(beta, eta))
  J %*% cov_log %*% t(J)
}

.weibull_cov_valid <- function(cov) {
  if (is.null(cov) || !is.matrix(cov) || nrow(cov) != 2L || ncol(cov) != 2L) {
    return(FALSE)
  }
  if (any(!is.finite(cov))) return(FALSE)
  ev <- eigen(cov, symmetric = TRUE, only.values = TRUE)$values
  isTRUE(all(ev > 0))
}

#' Fisher information / delta-method covariance at fixed (beta, eta).
#' Uses the right-censored Weibull log-likelihood (same as MLE), evaluated at the given estimates.
#' @return list(par, cov, loglik, fail, cens, ok, message)
weibull_fisher_at_params <- function(time, suspended, beta, eta) {
  fail <- as.numeric(time)[!as.logical(suspended)]
  cens <- as.numeric(time)[as.logical(suspended)]
  beta <- as.numeric(beta)[1]
  eta <- as.numeric(eta)[1]
  if (!is.finite(beta) || !is.finite(eta) || beta <= 0 || eta <= 0) {
    return(list(
      ok = FALSE,
      message = "Unable to compute confidence bounds.",
      par = NULL, cov = NULL, loglik = NA_real_, fail = fail, cens = cens
    ))
  }
  par <- c(log(beta), log(eta))
  loglik <- .weibull_ll_par(par, fail, cens)
  cov <- .weibull_mle_covariance(par, fail, cens)
  if (!.weibull_cov_valid(cov)) {
    return(list(
      ok = FALSE,
      message = "Unable to compute confidence bounds.",
      par = par, cov = NULL, loglik = loglik, fail = fail, cens = cens
    ))
  }
  list(
    ok = TRUE,
    message = "",
    par = par,
    cov = cov,
    loglik = loglik,
    fail = fail,
    cens = cens
  )
}

.weibull_grad_R <- function(t, beta, eta, R) {
  u <- (t / eta)^beta
  c(
    -R * u * log(t / eta),
    R * beta * u / eta
  )
}

.weibull_grad_blife <- function(R_level, beta, eta, t) {
  a <- -log(R_level)
  c(
    -t * log(a) / beta^2,
    t / eta
  )
}

.weibull_grad_plot_y <- function(t, beta, eta) {
  c(
    log(t / eta),
    -beta / eta
  )
}

.weibull_fisher_interval <- function(g_hat, grad, cov) {
  if (is.null(cov) || any(!is.finite(grad)) || !is.finite(g_hat)) {
    return(c(NA_real_, NA_real_))
  }
  var_g <- as.numeric(t(grad) %*% cov %*% grad)
  if (!is.finite(var_g) || var_g < 0) return(c(NA_real_, NA_real_))
  se <- sqrt(var_g)
  c(g_hat - se, g_hat + se)
}

.weibull_fisher_bounds_z <- function(conf_level) {
  stats::qnorm(0.5 + conf_level / 2)
}

.weibull_apply_fisher <- function(g_hat, grad, cov, conf_level) {
  if (is.null(cov) || any(!is.finite(grad)) || !is.finite(g_hat)) {
    return(c(NA_real_, NA_real_))
  }
  var_g <- as.numeric(t(grad) %*% cov %*% grad)
  if (!is.finite(var_g) || var_g < 0) return(c(NA_real_, NA_real_))
  z <- .weibull_fisher_bounds_z(conf_level)
  se <- sqrt(var_g)
  c(g_hat - z * se, g_hat + z * se)
}

.weibull_profile_ll_beta <- function(beta, eta_fn, fail, cens) {
  if (!is.finite(beta) || beta <= 0) return(-1e10)
  eta <- eta_fn(beta)
  if (!is.finite(eta) || eta <= 0) return(-1e10)
  ll <- .weibull_ll_par(c(log(beta), log(eta)), fail, cens)
  if (!is.finite(ll)) -1e10 else ll
}

.weibull_lr_cutoff <- function(loglik_mle, conf_level) {
  loglik_mle - stats::qchisq(conf_level, df = 1) / 2
}

.weibull_lr_root <- function(f, lo, hi) {
  f_lo <- f(lo)
  f_hi <- f(hi)
  if (!is.finite(f_lo) || !is.finite(f_hi)) return(NA_real_)
  if (f_lo == 0) return(lo)
  if (f_hi == 0) return(hi)
  if (f_lo * f_hi > 0) return(NA_real_)
  tryCatch(
    stats::uniroot(f, c(lo, hi))$root,
    error = function(e) NA_real_
  )
}

.weibull_lr_endpoint <- function(
    q_hat, q_fn, eta_fn, fail, cens, loglik_mle, conf_level,
    lower = TRUE, beta_range = c(1e-4, 50),
    q_min = NULL, q_max = NULL) {
  if (!is.null(q_max) && is.finite(q_max) && q_max > 0.99) {
    q_max <- 0.99999
  }
  cutoff <- .weibull_lr_cutoff(loglik_mle, conf_level)
  profile_at_q <- function(q) {
    if (!is.finite(q)) return(-1e10)
    if (!is.null(q_min) && q <= q_min) return(-1e10)
    if (!is.null(q_max) && q >= q_max) return(-1e10)
    eta_of_beta <- function(beta) eta_fn(beta, q)
    opt <- tryCatch(
      stats::optimize(
        function(b) .weibull_profile_ll_beta(b, eta_of_beta, fail, cens),
        interval = beta_range,
        maximum = TRUE
      ),
      error = function(e) list(objective = -1e10)
    )
    opt$objective
  }
  if (!is.finite(q_hat)) return(NA_real_)
  ll_hat <- profile_at_q(q_hat)
  if (!is.finite(ll_hat) || ll_hat < cutoff) return(NA_real_)
  diff_fn <- function(q) profile_at_q(q) - cutoff

  if (isTRUE(lower)) {
    hi_q <- q_hat
    if (!is.null(q_min)) {
      lo_q <- q_min + (q_hat - q_min) * 0.25
      for (k in seq_len(50L)) {
        if (profile_at_q(lo_q) < cutoff) break
        lo_q <- q_min + (lo_q - q_min) * 0.5
        if (lo_q <= q_min + 1e-8) {
          lo_q <- q_min + 1e-8
          break
        }
      }
    } else {
      step <- max(abs(q_hat), 1)
      lo_q <- q_hat - step
      for (k in seq_len(50L)) {
        if (profile_at_q(lo_q) < cutoff) break
        lo_q <- lo_q - step
      }
    }
    if (profile_at_q(lo_q) >= cutoff || profile_at_q(hi_q) < cutoff) return(NA_real_)
    .weibull_lr_root(diff_fn, lo_q, hi_q)
  } else {
    lo_q <- q_hat
    if (!is.null(q_max)) {
      hi_q <- q_hat + (q_max - q_hat) * 0.25
      for (k in seq_len(50L)) {
        if (profile_at_q(hi_q) < cutoff) break
        hi_q <- q_hat + (hi_q - q_hat) * 1.5
        if (hi_q >= q_max - 1e-6) {
          hi_q <- q_max - 1e-6
          break
        }
      }
    } else {
      step <- max(abs(q_hat), 1)
      hi_q <- q_hat + step
      for (k in seq_len(50L)) {
        if (profile_at_q(hi_q) < cutoff) break
        hi_q <- hi_q + step
      }
    }
    if (profile_at_q(hi_q) >= cutoff || profile_at_q(lo_q) < cutoff) return(NA_real_)
    .weibull_lr_root(diff_fn, lo_q, hi_q)
  }
}

.weibull_mh_draws <- function(fail, cens, beta_hat, eta_hat, n_draw = 4000L, burn = 1000L) {
  log_beta <- log(beta_hat)
  log_eta <- log(eta_hat)
  lb_b <- log(beta_hat / 20)
  ub_b <- log(20 * beta_hat)
  lb_e <- log(eta_hat / 20)
  ub_e <- log(20 * eta_hat)
  step <- c(0.08, 0.08)
  n_total <- burn + n_draw
  draws <- matrix(NA_real_, n_total, 2)
  draws[1, ] <- c(log_beta, log_eta)
  cur_ll <- .weibull_ll_par(draws[1, ], fail, cens)
  acc <- 0L
  for (i in seq_len(n_total - 1L)) {
    prop <- draws[i, ] + stats::rnorm(2) * step
    if (prop[1] < lb_b || prop[1] > ub_b || prop[2] < lb_e || prop[2] > ub_e) {
      draws[i + 1L, ] <- draws[i, ]
      next
    }
    prop_ll <- .weibull_ll_par(prop, fail, cens)
    if (is.finite(prop_ll) && log(stats::runif(1)) < prop_ll - cur_ll) {
      draws[i + 1L, ] <- prop
      cur_ll <- prop_ll
      acc <- acc + 1L
    } else {
      draws[i + 1L, ] <- draws[i, ]
    }
  }
  post <- draws[(burn + 1L):n_total, , drop = FALSE]
  list(
    beta = exp(post[, 1]),
    eta = exp(post[, 2]),
    acceptance = acc / (n_total - 1L)
  )
}

.weibull_bayes_quantile <- function(samples, conf_level) {
  alpha <- 1 - conf_level
  stats::quantile(samples, probs = c(alpha / 2, 1 - alpha / 2), names = FALSE, type = 7)
}

#' Simple beta=1 reference line (slope 1 through fitted midpoint).
weibull_beta1_reference_line <- function(pts, beta, intercept) {
  if (nrow(pts) < 2L || !is.finite(beta) || !is.finite(intercept)) {
    return(NULL)
  }
  ln_min <- min(pts$ln_t, na.rm = TRUE)
  ln_max <- max(pts$ln_t, na.rm = TRUE)
  if (!is.finite(ln_min) || !is.finite(ln_max) || ln_max <= ln_min) return(NULL)
  mid <- (ln_min + ln_max) / 2
  y_mid <- beta * mid + intercept
  data.frame(
    ln_t = c(ln_min, mid, ln_max),
    y = c(y_mid - (mid - ln_min), y_mid, y_mid + (ln_max - mid))
  )
}

#' Confidence / credible bounds for Weibull quantities (Fisher at given beta/eta; LR/Bayes MLE-only).
weibull_confidence_bounds <- function(
    beta, eta, par, cov, loglik, time, suspended,
    method = c("fisher", "lr", "bayes"),
    conf_level = 0.90,
    ln_t_grid = NULL,
    blife_R = c(0.99, 0.95, 0.90, 0.50, 0.10, 0.01),
    eval_time = NA_real_,
    rel_time_grid = NULL,
    seed = NULL) {
  method <- match.arg(method)
  if (method %in% c("lr", "bayes") && (is.null(loglik) || !is.finite(loglik))) {
    return(NULL)
  }
  fail <- as.numeric(time)[!as.logical(suspended)]
  cens <- as.numeric(time)[as.logical(suspended)]

  if (identical(method, "fisher") && !.weibull_cov_valid(cov)) {
    return(NULL)
  }

  if (is.null(ln_t_grid) || length(ln_t_grid) < 1L) {
    ln_t_grid <- seq(log(min(fail)), log(max(fail)), length.out = 10L)
  }
  if (is.null(rel_time_grid)) {
    t_max <- exp((log(-log(1 - 0.9999999)) + beta * log(eta)) / beta)
    if (!is.finite(t_max) || t_max <= 0) t_max <- max(time) * 3
    rel_time_grid <- seq(0, t_max, length.out = 101L)
    rel_time_grid[1] <- 0
  }
  rel_grid_lr <- if (identical(method, "fisher")) {
    rel_time_grid
  } else {
    t_pos <- rel_time_grid[rel_time_grid > 0]
    if (length(t_pos) < 2L) rel_time_grid else unique(c(0, seq(min(t_pos), max(t_pos), length.out = 25L)))
  }

  R_levels <- as.numeric(blife_R)
  R_levels <- R_levels[is.finite(R_levels) & R_levels > 0 & R_levels < 1]

  if (identical(method, "fisher")) {
    z <- .weibull_fisher_bounds_z(conf_level)
    plot_lo <- plot_hi <- rep(NA_real_, length(ln_t_grid))
    for (i in seq_along(ln_t_grid)) {
      t_i <- exp(ln_t_grid[i])
      y_hat <- beta * log(t_i / eta)
      g <- .weibull_grad_plot_y(t_i, beta, eta)
      var_y <- as.numeric(t(g) %*% cov %*% g)
      if (is.finite(var_y) && var_y >= 0) {
        se <- sqrt(var_y)
        plot_lo[i] <- y_hat - z * se
        plot_hi[i] <- y_hat + z * se
      }
    }
    blife_time <- weibull_blife(R_levels, beta, eta)
    blife_lo <- blife_hi <- rep(NA_real_, length(R_levels))
    for (i in seq_along(R_levels)) {
      t_hat <- blife_time[i]
      g <- .weibull_grad_blife(R_levels[i], beta, eta, t_hat)
      intv <- .weibull_apply_fisher(t_hat, g, cov, conf_level)
      blife_lo[i] <- intv[1]
      blife_hi[i] <- intv[2]
    }
    rel_R <- weibull_reliability(rel_grid_lr, beta, eta)
    rel_lo <- rel_hi <- rep(NA_real_, length(rel_grid_lr))
    for (i in seq_along(rel_grid_lr)) {
      t_i <- rel_grid_lr[i]
      if (t_i <= 0) {
        rel_lo[i] <- rel_hi[i] <- 1
        next
      }
      R_hat <- rel_R[i]
      g <- .weibull_grad_R(t_i, beta, eta, R_hat)
      intv <- .weibull_apply_fisher(R_hat, g, cov, conf_level)
      rel_lo[i] <- max(0, min(1, intv[1]))
      rel_hi[i] <- max(0, min(1, intv[2]))
    }
    point_R <- point_R_lo <- point_R_hi <- NA_real_
    if (is.finite(eval_time) && eval_time >= 0) {
      point_R <- weibull_reliability(eval_time, beta, eta)
      g <- .weibull_grad_R(eval_time, beta, eta, point_R)
      intv <- .weibull_apply_fisher(point_R, g, cov, conf_level)
      point_R_lo <- max(0, min(1, intv[1]))
      point_R_hi <- max(0, min(1, intv[2]))
    }
  } else if (identical(method, "lr")) {
    beta_rng <- c(max(1e-3, beta / 50), beta * 50)
    plot_lo <- plot_hi <- rep(NA_real_, length(ln_t_grid))
    for (i in seq_along(ln_t_grid)) {
      t_i <- exp(ln_t_grid[i])
      y_hat <- beta * log(t_i / eta)
      eta_fn <- function(b, y) t_i * exp(-y / b)
      plot_lo[i] <- .weibull_lr_endpoint(
        y_hat,
        q_fn = function(b, e) b * log(t_i / e),
        eta_fn = eta_fn,
        fail, cens, loglik, conf_level,
        lower = TRUE, beta_range = beta_rng
      )
      plot_hi[i] <- .weibull_lr_endpoint(
        y_hat,
        q_fn = function(b, e) b * log(t_i / e),
        eta_fn = eta_fn,
        fail, cens, loglik, conf_level,
        lower = FALSE, beta_range = beta_rng
      )
    }
    blife_time <- weibull_blife(R_levels, beta, eta)
    blife_lo <- blife_hi <- rep(NA_real_, length(R_levels))
    for (i in seq_along(R_levels)) {
      a <- -log(R_levels[i])
      t_hat <- blife_time[i]
      eta_fn <- function(b, t) t / a^(1 / b)
      blife_lo[i] <- .weibull_lr_endpoint(
        t_hat, NULL, eta_fn, fail, cens, loglik, conf_level,
        lower = TRUE, beta_range = beta_rng, q_min = 0
      )
      blife_hi[i] <- .weibull_lr_endpoint(
        t_hat, NULL, eta_fn, fail, cens, loglik, conf_level,
        lower = FALSE, beta_range = beta_rng, q_min = 0
      )
    }
    rel_R <- weibull_reliability(rel_grid_lr, beta, eta)
    rel_lo <- rel_hi <- rep(NA_real_, length(rel_grid_lr))
    for (i in seq_along(rel_grid_lr)) {
      t_i <- rel_grid_lr[i]
      if (t_i <= 0) {
        rel_lo[i] <- rel_hi[i] <- 1
        next
      }
      R_hat <- rel_R[i]
      eta_fn <- function(b, r) t_i / (-log(r))^(1 / b)
      rel_lo[i] <- .weibull_lr_endpoint(
        R_hat, NULL, eta_fn, fail, cens, loglik, conf_level,
        lower = TRUE, beta_range = beta_rng, q_min = 1e-8, q_max = 1 - 1e-8
      )
      rel_hi[i] <- .weibull_lr_endpoint(
        R_hat, NULL, eta_fn, fail, cens, loglik, conf_level,
        lower = FALSE, beta_range = beta_rng, q_min = 1e-8, q_max = 1 - 1e-8
      )
      rel_lo[i] <- max(0, min(1, rel_lo[i]))
      rel_hi[i] <- max(0, min(1, rel_hi[i]))
    }
    point_R <- point_R_lo <- point_R_hi <- NA_real_
    if (is.finite(eval_time) && eval_time >= 0) {
      point_R <- weibull_reliability(eval_time, beta, eta)
      eta_fn <- function(b, r) eval_time / (-log(r))^(1 / b)
      point_R_lo <- .weibull_lr_endpoint(
        point_R, NULL, eta_fn, fail, cens, loglik, conf_level,
        lower = TRUE, beta_range = beta_rng, q_min = 1e-8, q_max = 1 - 1e-8
      )
      point_R_hi <- .weibull_lr_endpoint(
        point_R, NULL, eta_fn, fail, cens, loglik, conf_level,
        lower = FALSE, beta_range = beta_rng, q_min = 1e-8, q_max = 1 - 1e-8
      )
      point_R_lo <- max(0, min(1, point_R_lo))
      point_R_hi <- max(0, min(1, point_R_hi))
    }
  } else {
    if (!is.null(seed)) set.seed(seed)
    mh <- .weibull_mh_draws(fail, cens, beta, eta)
    b_s <- mh$beta
    e_s <- mh$eta
    plot_lo <- plot_hi <- rep(NA_real_, length(ln_t_grid))
    for (i in seq_along(ln_t_grid)) {
      t_i <- exp(ln_t_grid[i])
      y_s <- b_s * log(t_i / e_s)
      q <- .weibull_bayes_quantile(y_s, conf_level)
      plot_lo[i] <- q[1]
      plot_hi[i] <- q[2]
    }
    blife_time <- weibull_blife(R_levels, beta, eta)
    blife_lo <- blife_hi <- rep(NA_real_, length(R_levels))
    for (i in seq_along(R_levels)) {
      t_s <- e_s * (-log(R_levels[i]))^(1 / b_s)
      q <- .weibull_bayes_quantile(t_s, conf_level)
      blife_lo[i] <- q[1]
      blife_hi[i] <- q[2]
    }
    rel_R <- weibull_reliability(rel_grid_lr, beta, eta)
    rel_lo <- rel_hi <- rep(NA_real_, length(rel_grid_lr))
    for (i in seq_along(rel_grid_lr)) {
      t_i <- rel_grid_lr[i]
      if (t_i <= 0) {
        rel_lo[i] <- rel_hi[i] <- 1
        next
      }
      R_s <- exp(-(t_i / e_s)^b_s)
      q <- .weibull_bayes_quantile(R_s, conf_level)
      rel_lo[i] <- max(0, min(1, q[1]))
      rel_hi[i] <- max(0, min(1, q[2]))
    }
    point_R <- point_R_lo <- point_R_hi <- NA_real_
    if (is.finite(eval_time) && eval_time >= 0) {
      point_R <- weibull_reliability(eval_time, beta, eta)
      R_s <- exp(-(eval_time / e_s)^b_s)
      q <- .weibull_bayes_quantile(R_s, conf_level)
      point_R_lo <- max(0, min(1, q[1]))
      point_R_hi <- max(0, min(1, q[2]))
    }
  }

  rel_curve_out <- data.frame(
    time = rel_grid_lr,
    R = rel_R,
    R_lo = rel_lo,
    R_hi = rel_hi,
    stringsAsFactors = FALSE
  )
  if (!identical(rel_grid_lr, rel_time_grid)) {
    pos <- rel_time_grid > 0
    R_lo_full <- R_hi_full <- rep(NA_real_, length(rel_time_grid))
    R_lo_full[!pos] <- 1
    R_hi_full[!pos] <- 1
    if (any(pos) && sum(is.finite(rel_lo)) >= 2L) {
      R_lo_full[pos] <- pmax(
        0, pmin(1, stats::approx(rel_grid_lr, rel_lo, xout = rel_time_grid[pos], rule = 2)$y)
      )
    }
    if (any(pos) && sum(is.finite(rel_hi)) >= 2L) {
      R_hi_full[pos] <- pmax(
        0, pmin(1, stats::approx(rel_grid_lr, rel_hi, xout = rel_time_grid[pos], rule = 2)$y)
      )
    }
    rel_curve_out <- data.frame(
      time = rel_time_grid,
      R = weibull_reliability(rel_time_grid, beta, eta),
      R_lo = R_lo_full,
      R_hi = R_hi_full,
      stringsAsFactors = FALSE
    )
  }

  list(
    plot = data.frame(ln_t = ln_t_grid, y_lower = plot_lo, y_upper = plot_hi),
    blife = data.frame(
      R = R_levels,
      time = weibull_blife(R_levels, beta, eta),
      time_lo = blife_lo,
      time_hi = blife_hi,
      stringsAsFactors = FALSE
    ),
    rel_curve = rel_curve_out,
    point_R = point_R,
    point_R_lo = point_R_lo,
    point_R_hi = point_R_hi,
    method = method,
    conf_level = conf_level
  )
}
