# Crow-AMSAA / Duane reliability growth calculations
# Parity with docs/reliability/Growth Analysis 3.0.xls
# Assumes growth_tables.R is sourced first.

#' Clean positive failure times (sorted unique-ish; drop NA/nonpositive).
growth_clean_times <- function(times) {
  t <- suppressWarnings(as.numeric(times))
  t <- t[is.finite(t) & t > 0]
  sort(t)
}

#' Time-terminated unbiased Crow MLE.
#' beta_hat = (n-1) / (n ln T - sum ln t_i); alpha_hat = n / T^beta
growth_fit_time_terminated <- function(times, end_time) {
  t <- growth_clean_times(times)
  T <- as.numeric(end_time)[1]
  n <- length(t)
  if (n < 2L || !is.finite(T) || T <= 0) {
    return(list(ok = FALSE, message = "Need at least 2 failure times and a positive end time T."))
  }
  if (any(t > T)) {
    return(list(ok = FALSE, message = "All failure times must be <= end test time T."))
  }
  denom <- n * log(T) - sum(log(t))
  if (!is.finite(denom) || denom <= 0) {
    return(list(ok = FALSE, message = "Cannot estimate beta (check times and T)."))
  }
  beta <- (n - 1) / denom
  alpha <- n / (T^beta)
  list(
    ok = TRUE,
    message = "",
    mode = "time",
    n = n,
    T = T,
    times = t,
    beta = beta,
    alpha = alpha
  )
}

#' Failure-terminated unbiased Crow MLE.
#' beta_hat = (n-1) / ((n-1) ln t_n - sum_{i=1}^{n-1} ln t_i); alpha = n / t_n^beta
growth_fit_failure_terminated <- function(times) {
  t <- growth_clean_times(times)
  n <- length(t)
  if (n < 2L) {
    return(list(ok = FALSE, message = "Need at least 2 failure times."))
  }
  tn <- t[n]
  denom <- (n - 1) * log(tn) - sum(log(t[seq_len(n - 1L)]))
  if (!is.finite(denom) || denom <= 0) {
    return(list(ok = FALSE, message = "Cannot estimate beta (check failure times)."))
  }
  beta <- (n - 1) / denom
  alpha <- n / (tn^beta)
  list(
    ok = TRUE,
    message = "",
    mode = "failure",
    n = n,
    T = tn,
    times = t,
    beta = beta,
    alpha = alpha
  )
}

#' Grouped Crow score contribution for beta (set sum to 0 for MLE).
.growth_grouped_score <- function(beta, cum_t, n_j) {
  k <- length(cum_t)
  T <- cum_t[k]
  s <- 0
  for (j in seq_len(k)) {
    nj <- n_j[j]
    if (!is.finite(nj) || nj == 0) {
      next
    }
    tj <- cum_t[j]
    if (j == 1L) {
      # n1 * ( (T1^b * ln T1) / T1^b - ln T ) = n1 * (ln T1 - ln T)
      s <- s + nj * (log(tj) - log(T))
    } else {
      tjm1 <- cum_t[j - 1L]
      num <- (tj^beta) * log(tj) - (tjm1^beta) * log(tjm1)
      den <- (tj^beta) - (tjm1^beta)
      if (!is.finite(den) || abs(den) < .Machine$double.eps) {
        return(NA_real_)
      }
      s <- s + nj * (num / den - log(T))
    }
  }
  s
}

#' Grouped MLE via uniroot on Crow score; LSE via ln-ln regression.
growth_fit_grouped <- function(delta_t, n_j, method = c("MLE", "LSE")) {
  method <- match.arg(method)
  dt <- suppressWarnings(as.numeric(delta_t))
  nj <- suppressWarnings(as.numeric(n_j))
  keep <- is.finite(dt) & dt > 0 & is.finite(nj) & nj >= 0
  dt <- dt[keep]
  nj <- nj[keep]
  if (length(dt) < 2L) {
    return(list(ok = FALSE, message = "Need at least 2 intervals with positive operating time."))
  }
  if (sum(nj) < 2) {
    return(list(ok = FALSE, message = "Need at least 2 total failures in the grouped data."))
  }
  cum_t <- cumsum(dt)
  cum_n <- cumsum(nj)
  T <- cum_t[length(cum_t)]
  N <- cum_n[length(cum_n)]

  if (identical(method, "LSE")) {
    # Only rows with positive cumulative failures
    ok_row <- cum_n > 0 & cum_t > 0
    if (sum(ok_row) < 2L) {
      return(list(ok = FALSE, message = "Need at least 2 points with cumulative failures for LSE."))
    }
    x <- log(cum_t[ok_row])
    y <- log(cum_n[ok_row])
    fit <- stats::lm(y ~ x)
    beta <- unname(stats::coef(fit)[["x"]])
    # Excel: alpha = exp(mean(H) - beta * mean(F))
    alpha <- exp(mean(y) - beta * mean(x))
  } else {
    # MLE: solve score(beta) = 0
    f <- function(b) .growth_grouped_score(b, cum_t, nj)
    # Bracket search
    lo <- 0.05
    hi <- 3
    flo <- f(lo)
    fhi <- f(hi)
    if (!is.finite(flo) || !is.finite(fhi)) {
      return(list(ok = FALSE, message = "Grouped MLE score not finite; check data."))
    }
    # Expand bracket if needed
    tries <- 0L
    while (flo * fhi > 0 && tries < 20L) {
      if (abs(flo) < abs(fhi)) {
        lo <- lo / 2
        flo <- f(lo)
      } else {
        hi <- hi * 1.5
        fhi <- f(hi)
      }
      tries <- tries + 1L
      if (!is.finite(flo) || !is.finite(fhi)) {
        return(list(ok = FALSE, message = "Grouped MLE could not bracket a root for beta."))
      }
    }
    if (flo * fhi > 0) {
      return(list(ok = FALSE, message = "Grouped MLE could not find a beta root (score does not change sign)."))
    }
    root <- stats::uniroot(f, interval = c(lo, hi), tol = 1e-8)
    beta <- root$root
    alpha <- N / (T^beta)
  }

  list(
    ok = TRUE,
    message = "",
    mode = "grouped",
    method = method,
    n = as.integer(N),
    n_groups = length(dt),
    T = T,
    delta_t = dt,
    n_j = nj,
    cum_t = cum_t,
    cum_n = cum_n,
    beta = beta,
    alpha = alpha
  )
}

#' Instantaneous intensity, MTBF, and mission reliability at time T.
growth_instantaneous <- function(alpha, beta, T, mission_time = 0) {
  lam <- alpha * beta * (T^(beta - 1))
  mtbf <- if (is.finite(lam) && lam > 0) 1 / lam else NA_real_
  R <- if (is.finite(lam) && is.finite(mission_time) && mission_time >= 0) {
    exp(-lam * mission_time)
  } else {
    NA_real_
  }
  list(lambda = lam, mtbf = mtbf, R = R, T = T)
}

#' Trend test of beta = 1 (chi-square / Laplace form).
#' Returns p-value and label: Random Failures | Growth/Infant Mortality | Wear-out
growth_trend_test <- function(times, T, beta_hat, type_I = 0.1, failure_terminated = FALSE) {
  t <- growth_clean_times(times)
  n <- length(t)
  if (n < 2L || !is.finite(T) || T <= max(t) && failure_terminated) {
    # for failure-term T is t_n
  }
  if (n < 2L) {
    return(list(p = NA_real_, label = "Insufficient data", beta_biased = NA_real_))
  }
  if (failure_terminated) {
    tn <- t[n]
    # sum ln(tn / t_i) for i=1..n; Excel uses sum over all including last (0)
    s <- sum(log(tn / t))
    beta_biased <- n / s
    df <- 2 * (n - 1)
    # Excel: 2*CHIDIST(2n/(n/sum), 2(n-1)) with sum over ln(tn/ti)
    # Using same structure as time-term with df adjustment
    stat <- 2 * n / beta_biased
    p <- 2 * stats::pchisq(stat, df = df, lower.tail = FALSE)
  } else {
    s <- sum(log(T / t))
    beta_biased <- n / s
    df <- 2 * n
    stat <- 2 * n / beta_biased
    p <- 2 * stats::pchisq(stat, df = df, lower.tail = FALSE)
  }
  if (!is.finite(p)) {
    label <- "Insufficient data"
  } else if (p >= type_I) {
    label <- "Random Failures"
  } else if (is.finite(beta_hat) && beta_hat < 1) {
    label <- "Growth/Infant Mortality"
  } else {
    label <- "Wear-out"
  }
  list(p = p, label = label, beta_biased = beta_biased, statistic = stat, df = df)
}

#' Grouped trend vs homogeneous Poisson (Pearson chi-square).
growth_trend_test_grouped <- function(delta_t, n_j, beta_hat, type_I = 0.1) {
  dt <- suppressWarnings(as.numeric(delta_t))
  nj <- suppressWarnings(as.numeric(n_j))
  keep <- is.finite(dt) & dt > 0 & is.finite(nj)
  dt <- dt[keep]
  nj <- nj[keep]
  k <- length(dt)
  N <- sum(nj)
  T <- sum(dt)
  if (k < 2L || N < 1) {
    return(list(p = NA_real_, label = "Insufficient data"))
  }
  expected <- N * dt / T
  chisq <- sum((nj - expected)^2 / expected)
  df <- k - 1
  p <- stats::pchisq(chisq, df = df, lower.tail = FALSE)
  if (!is.finite(p)) {
    label <- "Insufficient data"
  } else if (p >= type_I) {
    label <- "Random Failures"
  } else if (is.finite(beta_hat) && beta_hat < 1) {
    label <- "Growth/Infant Mortality"
  } else {
    label <- "Wear-out"
  }
  list(p = p, label = label, statistic = chisq, df = df)
}

#' Look up CvM critical value for sample size m and Type I alpha.
growth_cvm_critical <- function(m, type_I) {
  m <- as.integer(m)[1]
  type_I <- as.numeric(type_I)[1]
  # Match Excel column pick among {0.2,0.15,0.1,0.05,0.01}
  alphas <- GROWTH_CVM_ALPHAS
  col <- which.min(abs(alphas - type_I))
  # VLOOKUP-style: largest M <= m, else smallest
  idx <- findInterval(m, GROWTH_CVM_M)
  if (idx < 1L) idx <- 1L
  if (idx > length(GROWTH_CVM_M)) idx <- length(GROWTH_CVM_M)
  unname(GROWTH_CVM_CRIT[idx, col])
}

#' Cramer-von Mises GOF for Crow-AMSAA (time- or failure-terminated).
growth_cvm_gof <- function(times, T, beta, type_I = 0.1, failure_terminated = FALSE) {
  t <- growth_clean_times(times)
  n <- length(t)
  if (failure_terminated) {
    m <- n - 1L
    if (m < 2L) {
      return(list(
        statistic = NA_real_, critical = NA_real_, accept = NA,
        test = "Cramér–von Mises",
        message = "Need n >= 3 for failure-terminated CvM."
      ))
    }
    # Excel uses n-1 in denominator and in (2i-1)/(2(n-1)); still sums over all rows
    # but typically last term uses all times — match Excel: MAX(B)-1 for m
    terms <- numeric(n)
    for (i in seq_len(n)) {
      terms[i] <- ((t[i] / T)^beta - (2 * i - 1) / (2 * m))^2
    }
    stat <- 1 / (12 * m) + sum(terms)
  } else {
    m <- n
    if (m < 2L) {
      return(list(
        statistic = NA_real_, critical = NA_real_, accept = NA,
        test = "Cramér–von Mises",
        message = "Need n >= 2 for CvM."
      ))
    }
    terms <- ((t / T)^beta - (2 * seq_len(n) - 1) / (2 * n))^2
    stat <- 1 / (12 * n) + sum(terms)
  }
  crit <- growth_cvm_critical(m, type_I)
  accept <- is.finite(stat) && is.finite(crit) && stat <= crit
  list(
    statistic = stat,
    critical = crit,
    accept = accept,
    test = "Cramér–von Mises",
    message = if (isTRUE(accept)) {
      "Accept: Data do follow a straight line"
    } else if (isFALSE(accept)) {
      "Reject: Data do not follow a straight line"
    } else {
      "CvM not available"
    }
  )
}

#' Grouped chi-square GOF vs power-law expected interval counts.
growth_gof_grouped <- function(fit, type_I = 0.1) {
  if (!isTRUE(fit$ok)) {
    return(list(
      statistic = NA_real_, p = NA_real_, accept = NA,
      test = "Chi-square (grouped)",
      message = fit$message
    ))
  }
  cum_t <- fit$cum_t
  nj <- fit$n_j
  alpha <- fit$alpha
  beta <- fit$beta
  k <- length(cum_t)
  expected <- numeric(k)
  expected[1] <- alpha * (cum_t[1]^beta)
  if (k > 1L) {
    for (j in 2:k) {
      expected[j] <- alpha * (cum_t[j]^beta - cum_t[j - 1]^beta)
    }
  }
  if (any(expected < 5)) {
    return(list(
      statistic = NA_real_,
      p = NA_real_,
      df = k - 2,
      accept = NA,
      test = "Chi-square (grouped)",
      message = "Insufficient Expected Failures",
      expected = expected
    ))
  }
  chisq <- sum((nj - expected)^2 / expected)
  df <- k - 2
  # Excel uses CHIDIST*2 (two-tailed)
  p <- 2 * stats::pchisq(chisq, df = df, lower.tail = FALSE)
  p <- min(1, p)
  accept <- is.finite(p) && p >= type_I
  list(
    statistic = chisq,
    p = p,
    df = df,
    accept = accept,
    test = "Chi-square (grouped)",
    expected = expected,
    message = if (isTRUE(accept)) {
      "Accept: Data follow the power-law model"
    } else {
      "Reject: Data do not follow the power-law model"
    }
  )
}

#' Map Type I and sidedness to AMSAA table alpha key.
growth_ci_alpha_key <- function(type_I, sides = 2L) {
  sides <- as.integer(sides)[1]
  type_I <- as.numeric(type_I)[1]
  # Excel: I27 = TypeI / (sides * 0.5)
  a <- type_I / (sides * 0.5)
  candidates <- c(0.2, 0.1, 0.05, 0.02)
  candidates[which.min(abs(candidates - a))]
}

#' Large-N normal approximation for AMSAA MTBF factors.
growth_amsaa_factors_large_n <- function(n, alpha) {
  n <- as.numeric(n)[1]
  alpha <- as.numeric(alpha)[1]
  z_l <- stats::qnorm(0.5 + (1 - alpha) / 2)
  z_u <- stats::qnorm(0.5 - (1 - alpha) / 2)
  L <- (1 + z_l / sqrt(2 * n))^(-2)
  U <- (1 + z_u / sqrt(2 * n))^(-2)
  c(L = L, U = U)
}

#' Look up AMSAA MTBF CI factors (L, U) for time or failure truncation.
growth_amsaa_factors <- function(n, type_I, sides = 2L, truncation = c("time", "failure")) {
  truncation <- match.arg(truncation)
  n <- as.integer(n)[1]
  a_key <- growth_ci_alpha_key(type_I, sides)
  a_col <- as.character(a_key)
  if (n > 100L) {
    return(growth_amsaa_factors_large_n(n, a_key))
  }
  if (identical(truncation, "time")) {
    Ns <- GROWTH_AMSAA_TIME_N
    Lmat <- GROWTH_AMSAA_TIME_L
    Umat <- GROWTH_AMSAA_TIME_U
  } else {
    Ns <- GROWTH_AMSAA_FAILURE_N
    Lmat <- GROWTH_AMSAA_FAILURE_L
    Umat <- GROWTH_AMSAA_FAILURE_U
  }
  idx <- findInterval(n, Ns)
  if (idx < 1L) idx <- 1L
  if (idx > length(Ns)) idx <- length(Ns)
  # Prefer exact N match when present
  exact <- which(Ns == n)
  if (length(exact) == 1L) idx <- exact
  c(L = unname(Lmat[idx, a_col]), U = unname(Umat[idx, a_col]))
}

#' MTBF and R confidence intervals from AMSAA factors.
growth_mtbf_ci <- function(mtbf, n, type_I, sides = 2L, side_dir = c("both", "lower", "upper"),
                          truncation = c("time", "failure"), mission_time = 0) {
  side_dir <- match.arg(side_dir)
  truncation <- match.arg(truncation)
  fac <- growth_amsaa_factors(n, type_I, sides, truncation)
  mtbf_L <- fac[["L"]] * mtbf
  mtbf_U <- fac[["U"]] * mtbf
  if (identical(sides, 1L) || identical(as.integer(sides), 1L)) {
    if (identical(side_dir, "upper")) mtbf_L <- NA_real_
    if (identical(side_dir, "lower")) mtbf_U <- NA_real_
  }
  R_L <- if (is.finite(mtbf_L) && mtbf_L > 0) exp(-mission_time / mtbf_L) else NA_real_
  R_U <- if (is.finite(mtbf_U) && mtbf_U > 0) exp(-mission_time / mtbf_U) else NA_real_
  list(
    factor_L = fac[["L"]],
    factor_U = fac[["U"]],
    mtbf_L = mtbf_L,
    mtbf_U = mtbf_U,
    lambda_U = if (is.finite(mtbf_L) && mtbf_L > 0) 1 / mtbf_L else NA_real_,
    lambda_L = if (is.finite(mtbf_U) && mtbf_U > 0) 1 / mtbf_U else NA_real_,
    R_L = R_L,
    R_U = R_U
  )
}

#' Duane RGT tradeoff planning (Excel RGT Tradeoffs sheet).
growth_rgt_tradeoffs <- function(growth_rate, T0, mtbf0, mtbf_I, n_systems, hours_per_test) {
  a <- as.numeric(growth_rate)[1]
  T0 <- as.numeric(T0)[1]
  mtbf0 <- as.numeric(mtbf0)[1]
  mtbf_I <- as.numeric(mtbf_I)[1]
  n_systems <- as.numeric(n_systems)[1]
  hours_per_test <- as.numeric(hours_per_test)[1]
  if (!all(is.finite(c(a, T0, mtbf0, mtbf_I, n_systems, hours_per_test)))) {
    return(list(ok = FALSE, message = "All RGT inputs must be numeric."))
  }
  if (a <= 0 || a >= 1) {
    return(list(ok = FALSE, message = "Growth rate α must be between 0 and 1 (exclusive)."))
  }
  if (any(c(T0, mtbf0, mtbf_I, n_systems, hours_per_test) <= 0)) {
    return(list(ok = FALSE, message = "T0, MTBF0, MTBF_I, systems, and hours/test must be positive."))
  }
  mtbf_C <- mtbf_I * (1 - a)
  T_total <- T0 * (mtbf_C / mtbf0)^(1 / a)
  hours_per_system <- T_total / n_systems
  n_tests <- T_total / (hours_per_test * n_systems)
  list(
    ok = TRUE,
    message = "",
    mtbf_C = mtbf_C,
    T_total = T_total,
    hours_per_system = hours_per_system,
    n_tests = n_tests
  )
}

#' Development time T* so that R(tau | T*) = R_target under Crow intensity.
growth_plan_target_time <- function(R_target, mission_time, alpha, beta) {
  R_target <- as.numeric(R_target)[1]
  mission_time <- as.numeric(mission_time)[1]
  alpha <- as.numeric(alpha)[1]
  beta <- as.numeric(beta)[1]
  if (!all(is.finite(c(R_target, mission_time, alpha, beta)))) {
    return(list(ok = FALSE, message = "All growth-plan inputs must be numeric."))
  }
  if (R_target <= 0 || R_target >= 1) {
    return(list(ok = FALSE, message = "Target reliability must be in (0, 1)."))
  }
  if (mission_time <= 0 || alpha <= 0 || beta <= 0 || beta == 1) {
    return(list(ok = FALSE, message = "Mission time, alpha > 0 and beta > 0, beta != 1 required."))
  }
  # T = (ln(R) / (-tau * alpha * beta)) ^ (1/(beta-1))
  base <- log(R_target) / (-mission_time * alpha * beta)
  if (!is.finite(base) || base <= 0) {
    return(list(ok = FALSE, message = "Cannot invert target time for these parameters."))
  }
  T_star <- base^(1 / (beta - 1))
  list(ok = TRUE, message = "", T_star = T_star)
}

#' Development time on the ideal curve for a given reliability level.
#' Same inversion as growth_plan_target_time; used for plan-tracking comparisons.
growth_plan_time_for_R <- function(R, mission_time, alpha, beta) {
  R <- as.numeric(R)[1]
  mission_time <- as.numeric(mission_time)[1]
  alpha <- as.numeric(alpha)[1]
  beta <- as.numeric(beta)[1]
  if (!all(is.finite(c(R, mission_time, alpha, beta)))) return(NA_real_)
  if (R <= 0 || R >= 1 || mission_time <= 0 || alpha <= 0 || beta <= 0 || beta == 1) {
    return(NA_real_)
  }
  base <- log(R) / (-mission_time * alpha * beta)
  if (!is.finite(base) || base <= 0) return(NA_real_)
  base^(1 / (beta - 1))
}

#' Ideal growth-plan curve and optional actual-vs-ideal plan conformance.
#' Monitoring follows the MIL-HDBK-189 idea of checking whether demonstrated
#' reliability is in conformance with the idealized growth curve (exit /
#' milestone threshold concept): compare actual R at development time t to
#' Ideal R(t). This is a plan check, not a Crow confidence interval.
growth_plan_curve <- function(R_target, mission_time, alpha, beta,
                              n_points = 40L, actual_R = NULL) {
  tgt <- growth_plan_target_time(R_target, mission_time, alpha, beta)
  if (!isTRUE(tgt$ok)) {
    return(tgt)
  }
  T_star <- tgt$T_star
  n_points <- as.integer(n_points)[1]
  idx <- seq_len(n_points)
  time <- idx * T_star / n_points
  lambda <- alpha * beta * time^(beta - 1)
  ideal_R <- exp(-lambda * mission_time)

  track <- NULL
  actual <- NULL
  if (!is.null(actual_R)) {
    ar <- suppressWarnings(as.numeric(actual_R))
    ar <- ar[is.finite(ar)]
    if (length(ar) >= 1L) {
      n_act <- min(length(ar), n_points)
      actual <- ar[seq_len(n_act)]
      t_act <- time[seq_len(n_act)]
      r_ideal <- ideal_R[seq_len(n_act)]
      gap <- actual - r_ideal
      status <- ifelse(
        gap < -1e-12, "Behind",
        ifelse(gap > 1e-12, "Ahead", "On track")
      )
      t_for_actual <- vapply(
        actual,
        function(r) growth_plan_time_for_R(r, mission_time, alpha, beta),
        numeric(1)
      )
      track <- data.frame(
        time = t_act,
        ideal_R = r_ideal,
        actual_R = actual,
        gap = gap,
        status = status,
        plan_time_for_actual = t_for_actual,
        stringsAsFactors = FALSE
      )
    }
  }

  list(
    ok = TRUE,
    message = "",
    T_star = T_star,
    time = time,
    ideal_R = ideal_R,
    actual_R = actual,
    track = track
  )
}

#' Fitted cumulative N(t) grid for plotting.
growth_fitted_curve <- function(alpha, beta, T, n_grid = 100L) {
  tt <- seq(from = max(T / n_grid, .Machine$double.eps), to = T, length.out = n_grid)
  data.frame(time = tt, N = alpha * tt^beta)
}
