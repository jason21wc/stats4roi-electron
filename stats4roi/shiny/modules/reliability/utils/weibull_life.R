# 2-parameter Weibull life-data analysis
# Parity with docs/reliability/Weibull_Calculator_v1.6.xlsx (RRX / RRY / MLE)

#' Parse suspend flags (Y/N, yes/no, TRUE/FALSE, 1/0) to logical; NA -> FALSE.
weibull_parse_suspend <- function(x) {
  if (is.null(x)) return(logical(0))
  if (is.logical(x)) return(!is.na(x) & x)
  if (is.numeric(x)) return(is.finite(x) & x != 0)
  s <- tolower(trimws(as.character(x)))
  s %in% c("y", "yes", "true", "t", "1", "suspend", "suspended", "censored")
}

#' Clean life data: positive times, duty-cycle correction, suspend flags.
#' @return list(ok, message, time, suspended, n, n_fail)
weibull_prepare_data <- function(times, suspended = NULL, duty_cycle = 1) {
  t_raw <- suppressWarnings(as.numeric(times))
  duty <- as.numeric(duty_cycle)[1]
  if (!is.finite(duty) || duty <= 0) {
    return(list(ok = FALSE, message = "Duty cycle must be a positive number."))
  }
  keep <- is.finite(t_raw) & t_raw > 0
  t_raw <- t_raw[keep]
  if (length(t_raw) < 1L) {
    return(list(ok = FALSE, message = "Need at least one positive failure/censor time."))
  }
  if (is.null(suspended) || length(suspended) == 0L) {
    sus <- rep(FALSE, length(t_raw))
  } else {
    sus_all <- weibull_parse_suspend(suspended)
    if (length(sus_all) == length(keep)) {
      sus <- sus_all[keep]
    } else if (length(sus_all) == length(t_raw)) {
      sus <- sus_all
    } else {
      return(list(ok = FALSE, message = "Suspend column length must match the time column."))
    }
  }
  t_corr <- t_raw / duty
  n <- length(t_corr)
  n_fail <- sum(!sus)
  if (n_fail < 2L) {
    return(list(ok = FALSE, message = "Need at least 2 failures (non-suspended) for Weibull analysis."))
  }
  list(
    ok = TRUE,
    message = "",
    time = t_corr,
    time_raw = t_raw,
    suspended = sus,
    n = as.integer(n),
    n_fail = as.integer(n_fail),
    duty_cycle = duty
  )
}

#' Exact median rank (Excel BETAINV(0.5, j, n-j+1)).
weibull_median_rank <- function(j, n) {
  stats::qbeta(0.5, j, n - j + 1)
}

#' Rank adjustment for right suspensions (spreadsheet Data!F column logic).
#' Returns adjusted ranks for failures only; suspensions get NA.
weibull_adjusted_ranks <- function(time, suspended) {
  n <- length(time)
  sus <- as.logical(suspended)
  ord <- order(time, sus) # ties: failures before suspensions at same time is fine
  sorted_t <- time[ord]
  sorted_sus <- sus[ord]
  sorted_order <- seq_len(n) # 1..n in sorted position (Excel C)
  adj <- rep(NA_real_, n)
  if (!any(sorted_sus)) {
    adj <- as.numeric(sorted_order)
  } else {
    prev_max <- 0
    for (i in seq_len(n)) {
      if (isTRUE(sorted_sus[i])) {
        adj[i] <- NA_real_
      } else {
        # ((n - C + 1) * max_prev + (n + 1)) / ((n - C + 1) + 1)
        c_i <- sorted_order[i]
        adj[i] <- ((n - c_i + 1) * prev_max + (n + 1)) / ((n - c_i + 1) + 1)
        prev_max <- adj[i]
      }
    }
  }
  list(
    order_idx = ord,
    sorted_time = sorted_t,
    sorted_suspended = sorted_sus,
    sorted_order = sorted_order,
    adj_rank = adj
  )
}

#' Plotting positions for failures only (ln t, ln(-ln(1-F))).
weibull_plot_points <- function(time, suspended) {
  rk <- weibull_adjusted_ranks(time, suspended)
  n <- length(time)
  fail <- !rk$sorted_suspended & is.finite(rk$adj_rank)
  j <- rk$adj_rank[fail]
  t_f <- rk$sorted_time[fail]
  F_hat <- weibull_median_rank(j, n)
  # Clamp for numerical safety
  F_hat <- pmin(pmax(F_hat, .Machine$double.eps), 1 - .Machine$double.eps)
  x <- log(t_f)
  y <- log(-log(1 - F_hat))
  data.frame(
    time = t_f,
    rank = j,
    F_hat = F_hat,
    ln_t = x,
    y = y,
    stringsAsFactors = FALSE
  )
}

#' Rank regression on Y (Excel SLOPE/INTERCEPT): y = beta * x + b.
weibull_fit_rry <- function(pts) {
  if (nrow(pts) < 2L) {
    return(list(ok = FALSE, message = "Need at least 2 failure points for rank regression."))
  }
  fit <- stats::lm(y ~ ln_t, data = pts)
  beta <- unname(stats::coef(fit)[["ln_t"]])
  intercept <- unname(stats::coef(fit)[["(Intercept)"]])
  if (!is.finite(beta) || abs(beta) < .Machine$double.eps) {
    return(list(ok = FALSE, message = "Rank regression (RRY) could not estimate beta."))
  }
  eta <- exp(-intercept / beta)
  r2 <- summary(fit)$r.squared
  rho <- suppressWarnings(stats::cor(pts$ln_t, pts$y))
  list(
    ok = TRUE,
    message = "",
    method = "RRY",
    beta = beta,
    eta = eta,
    intercept = intercept,
    slope = beta,
    r_squared = r2,
    rho = rho
  )
}

#' Rank regression on X: x = a + b*y, invert to Weibull beta/eta.
weibull_fit_rrx <- function(pts) {
  if (nrow(pts) < 2L) {
    return(list(ok = FALSE, message = "Need at least 2 failure points for rank regression."))
  }
  fit <- stats::lm(ln_t ~ y, data = pts)
  b <- unname(stats::coef(fit)[["y"]])
  a <- unname(stats::coef(fit)[["(Intercept)"]])
  if (!is.finite(b) || abs(b) < .Machine$double.eps) {
    return(list(ok = FALSE, message = "Rank regression (RRX) could not estimate beta."))
  }
  # y = (x - a)/b = x/b - a/b  => slope_yx = 1/b, intercept_yx = -a/b
  beta <- 1 / b
  intercept_yx <- -a / b
  eta <- exp(-intercept_yx / beta) # = exp(a)
  r2 <- summary(fit)$r.squared
  rho <- suppressWarnings(stats::cor(pts$ln_t, pts$y))
  list(
    ok = TRUE,
    message = "",
    method = "RRX",
    beta = beta,
    eta = eta,
    intercept = intercept_yx,
    slope = beta,
    r_squared = r2,
    rho = rho
  )
}

#' Right-censored 2-parameter Weibull MLE via optim on (log beta, log eta).
weibull_fit_mle <- function(time, suspended) {
  t <- as.numeric(time)
  sus <- as.logical(suspended)
  fail <- t[!sus]
  cens <- t[sus]
  if (length(fail) < 2L) {
    return(list(ok = FALSE, message = "Need at least 2 failures for MLE."))
  }
  # Starting values from RRY on failures only
  pts <- weibull_plot_points(t, sus)
  start <- weibull_fit_rry(pts)
  if (!isTRUE(start$ok)) {
    beta0 <- 1
    eta0 <- mean(fail)
  } else {
    beta0 <- max(start$beta, 0.05)
    eta0 <- max(start$eta, min(fail) * 0.5)
  }

  nll <- function(par) {
    beta <- exp(par[1])
    eta <- exp(par[2])
    if (!is.finite(beta) || !is.finite(eta) || beta <= 0 || eta <= 0) return(1e12)
    # Failures: log f(t) = log(beta/eta) + (beta-1)*log(t/eta) - (t/eta)^beta
    lf <- log(beta / eta) + (beta - 1) * log(fail / eta) - (fail / eta)^beta
    # Suspensions: log R(t) = -(t/eta)^beta
    lc <- if (length(cens) > 0L) -(cens / eta)^beta else 0
    ll <- sum(lf) + sum(lc)
    if (!is.finite(ll)) return(1e12)
    -ll
  }

  opt <- tryCatch(
    stats::optim(
      par = c(log(beta0), log(eta0)),
      fn = nll,
      method = "Nelder-Mead",
      control = list(maxit = 2000, reltol = 1e-12)
    ),
    error = function(e) NULL
  )
  if (is.null(opt) || !is.finite(opt$value)) {
    return(list(ok = FALSE, message = "Weibull MLE optimization failed."))
  }
  par <- opt$par
  beta <- exp(par[1])
  eta <- exp(par[2])
  if (!is.finite(beta) || !is.finite(eta) || beta <= 0 || eta <= 0) {
    return(list(ok = FALSE, message = "Weibull MLE returned non-finite parameters."))
  }
  cov <- .weibull_mle_covariance(par, fail, cens)
  list(
    ok = TRUE,
    message = "",
    method = "MLE",
    beta = beta,
    eta = eta,
    par = par,
    intercept = NA_real_,
    slope = NA_real_,
    r_squared = NA_real_,
    rho = NA_real_,
    loglik = -opt$value,
    cov = cov,
    fail = fail,
    cens = cens
  )
}

#' Reliability R(t) = exp(-(t/eta)^beta)
weibull_reliability <- function(t, beta, eta) {
  t <- as.numeric(t)
  beta <- as.numeric(beta)[1]
  eta <- as.numeric(eta)[1]
  out <- exp(-(t / eta)^beta)
  out[!is.finite(t) | t < 0] <- NA_real_
  out
}

#' PDF f(t)
weibull_pdf <- function(t, beta, eta) {
  t <- as.numeric(t)
  beta <- as.numeric(beta)[1]
  eta <- as.numeric(eta)[1]
  out <- (beta / eta) * (t / eta)^(beta - 1) * exp(-(t / eta)^beta)
  out[!is.finite(t) | t < 0] <- NA_real_
  out[t == 0 & beta < 1] <- Inf
  out[t == 0 & beta == 1] <- beta / eta
  out[t == 0 & beta > 1] <- 0
  out
}

#' B-life / time at reliability R (fraction surviving)
weibull_blife <- function(R, beta, eta) {
  R <- as.numeric(R)
  beta <- as.numeric(beta)[1]
  eta <- as.numeric(eta)[1]
  out <- eta * (-log(R))^(1 / beta)
  out[!is.finite(R) | R <= 0 | R >= 1] <- NA_real_
  out
}

#' Weibull mean eta * Gamma(1 + 1/beta)
weibull_mean <- function(beta, eta) {
  beta <- as.numeric(beta)[1]
  eta <- as.numeric(eta)[1]
  eta * gamma(1 + 1 / beta)
}

#' Weibull median
weibull_median <- function(beta, eta) {
  beta <- as.numeric(beta)[1]
  eta <- as.numeric(eta)[1]
  eta * (log(2))^(1 / beta)
}

#' Failure-rate trend label from beta
weibull_trend_label <- function(beta) {
  beta <- as.numeric(beta)[1]
  if (!is.finite(beta)) return("")
  if (beta < 1) "decreasing (infant mortality)"
  else if (abs(beta - 1) < 1e-12) "constant failure rate"
  else "increasing (wear-out)"
}

#' Characteristic-life guides on the Weibull probability plot.
#' Vertical at ln(eta); optional horizontal at y = 0 (F approx 0.632).
weibull_characteristic_life_guides <- function(eta, ln_t_range, y_range) {
  eta <- as.numeric(eta)[1]
  if (!is.finite(eta) || eta <= 0 ||
      length(ln_t_range) < 2L || length(y_range) < 2L ||
      !all(is.finite(ln_t_range)) || !all(is.finite(y_range))) {
    return(list(vertical = NULL, horizontal = NULL))
  }
  ln_eta <- log(eta)
  ln_pad <- diff(ln_t_range) * 0.02
  y_pad <- diff(y_range) * 0.02
  if (!is.finite(ln_pad) || ln_pad <= 0) ln_pad <- 0.1
  if (!is.finite(y_pad) || y_pad <= 0) y_pad <- 0.1
  ln_lo <- min(ln_t_range[1], ln_eta) - ln_pad
  ln_hi <- max(ln_t_range[2], ln_eta) + ln_pad
  y_lo <- min(y_range[1], 0) - y_pad
  y_hi <- max(y_range[2], 0) + y_pad
  list(
    vertical = data.frame(ln_t = c(ln_eta, ln_eta), y = c(y_lo, y_hi)),
    horizontal = data.frame(ln_t = c(ln_lo, ln_hi), y = c(0, 0))
  )
}

#' Full Weibull analysis for prepared data.
#' @param method one of "RRX", "RRY", "MLE"
weibull_fit <- function(times, suspended = NULL, duty_cycle = 1,
                        method = c("RRY", "RRX", "MLE"),
                        bounds_method = c("none", "fisher", "lr", "bayes"),
                        conf_level = 0.90,
                        show_bounds = TRUE,
                        eval_time = NA_real_,
                        blife_R = c(0.99, 0.95, 0.90, 0.50, 0.10, 0.01),
                        bounds_seed = NULL) {
  method <- match.arg(method)
  bounds_method <- match.arg(bounds_method)
  prep <- weibull_prepare_data(times, suspended, duty_cycle)
  if (!isTRUE(prep$ok)) return(prep)

  pts <- weibull_plot_points(prep$time, prep$suspended)
  if (identical(method, "RRY")) {
    fit <- weibull_fit_rry(pts)
  } else if (identical(method, "RRX")) {
    fit <- weibull_fit_rrx(pts)
  } else {
    fit <- weibull_fit_mle(prep$time, prep$suspended)
  }
  if (!isTRUE(fit$ok)) return(fit)

  beta <- fit$beta
  eta <- fit$eta
  intercept <- if (is.finite(fit$intercept)) fit$intercept else {
    # For MLE, derive intercept on linearized plot: y = beta*ln(t) - beta*ln(eta)
    -beta * log(eta)
  }

  beta1_line <- weibull_beta1_reference_line(pts, beta, intercept)

  ln_grid <- if (nrow(pts) >= 2L) {
    seq(min(pts$ln_t), max(pts$ln_t), length.out = 10L)
  } else {
    log(range(prep$time[prep$time > 0]))
  }

  t_max <- tryCatch(
    exp((log(-log(1 - 0.9999999)) + beta * log(eta)) / beta),
    error = function(e) max(prep$time) * 3
  )
  if (!is.finite(t_max) || t_max <= 0) t_max <- max(prep$time) * 3
  t_grid <- seq(0, t_max, length.out = 101L)
  t_grid[1] <- 0

  R_levels <- as.numeric(blife_R)
  R_levels <- R_levels[is.finite(R_levels) & R_levels > 0 & R_levels < 1]

  point_R <- if (is.finite(eval_time) && eval_time >= 0) {
    weibull_reliability(eval_time, beta, eta)
  } else {
    NA_real_
  }

  bounds_message <- ""
  want_bounds <- isTRUE(show_bounds) && !identical(bounds_method, "none")
  compute_bounds <- FALSE

  if (isTRUE(want_bounds)) {
    if (bounds_method %in% c("lr", "bayes") && !identical(method, "MLE")) {
      bounds_message <- "Likelihood ratio and Bayesian bounds require MLE estimation."
    } else if (identical(bounds_method, "fisher")) {
      if (!identical(method, "MLE") || is.null(fit$cov)) {
        fisher_info <- weibull_fisher_at_params(prep$time, prep$suspended, beta, eta)
        if (isTRUE(fisher_info$ok)) {
          fit$par <- fisher_info$par
          fit$cov <- fisher_info$cov
          fit$loglik <- fisher_info$loglik
          fit$fail <- fisher_info$fail
          fit$cens <- fisher_info$cens
          compute_bounds <- TRUE
        } else {
          bounds_message <- fisher_info$message %||% "Unable to compute confidence bounds."
        }
      } else {
        compute_bounds <- TRUE
      }
    } else if (identical(method, "MLE")) {
      if (!is.null(fit$cov) && !is.null(fit$loglik)) {
        compute_bounds <- TRUE
      } else {
        bounds_message <- "Unable to compute confidence bounds."
      }
    }
  }

  bounds_obj <- NULL
  if (isTRUE(compute_bounds)) {
    bounds_obj <- weibull_confidence_bounds(
      beta = beta,
      eta = eta,
      par = fit$par,
      cov = fit$cov,
      loglik = fit$loglik,
      time = prep$time,
      suspended = prep$suspended,
      method = bounds_method,
      conf_level = conf_level,
      ln_t_grid = ln_grid,
      blife_R = R_levels,
      eval_time = eval_time,
      rel_time_grid = t_grid,
      seed = bounds_seed
    )
  }

  bounds <- if (!is.null(bounds_obj)) bounds_obj$plot else NULL
  blife_df <- if (!is.null(bounds_obj)) {
    bounds_obj$blife
  } else {
    data.frame(
      R = R_levels,
      time = weibull_blife(R_levels, beta, eta),
      time_lo = NA_real_,
      time_hi = NA_real_,
      stringsAsFactors = FALSE
    )
  }

  curve_R <- weibull_reliability(t_grid, beta, eta)
  curve_R_lo <- if (!is.null(bounds_obj)) bounds_obj$rel_curve$R_lo else rep(NA_real_, length(t_grid))
  curve_R_hi <- if (!is.null(bounds_obj)) bounds_obj$rel_curve$R_hi else rep(NA_real_, length(t_grid))

  point_R_lo <- if (!is.null(bounds_obj)) bounds_obj$point_R_lo else NA_real_
  point_R_hi <- if (!is.null(bounds_obj)) bounds_obj$point_R_hi else NA_real_

  out_message <- if (nzchar(bounds_message)) bounds_message else ""

  list(
    ok = TRUE,
    message = out_message,
    method = method,
    bounds_method = if (compute_bounds) bounds_method else "none",
    conf_level = if (compute_bounds) conf_level else NA_real_,
    n = prep$n,
    n_fail = prep$n_fail,
    n_susp = prep$n - prep$n_fail,
    duty_cycle = prep$duty_cycle,
    time = prep$time,
    suspended = prep$suspended,
    plot_points = pts,
    beta = beta,
    eta = eta,
    intercept = intercept,
    slope = beta,
    r_squared = fit$r_squared,
    rho = fit$rho,
    trend = weibull_trend_label(beta),
    beta1_line = beta1_line,
    bounds = bounds,
    bounds_obj = bounds_obj,
    mean = weibull_mean(beta, eta),
    median = weibull_median(beta, eta),
    sample_mean_fail = mean(prep$time[!prep$suspended]),
    blife = blife_df,
    eval_time = eval_time,
    point_R = point_R,
    point_R_lo = point_R_lo,
    point_R_hi = point_R_hi,
    curve_time = t_grid,
    curve_R = curve_R,
    curve_R_lo = curve_R_lo,
    curve_R_hi = curve_R_hi,
    curve_pdf = weibull_pdf(pmax(t_grid, .Machine$double.eps), beta, eta),
    fit_line = {
      xx <- seq(min(pts$ln_t), max(pts$ln_t), length.out = 50L)
      data.frame(ln_t = xx, y = beta * xx + intercept)
    }
  )
}
