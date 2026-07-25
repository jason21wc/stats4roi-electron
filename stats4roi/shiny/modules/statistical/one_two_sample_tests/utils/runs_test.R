# Wald–Wolfowitz runs test for randomness
# References:
#   NIST EDA 1.3.5.13 — https://www.itl.nist.gov/div898/handbook/eda/section3/eda35d.htm
#   Wikipedia — https://en.wikipedia.org/wiki/Wald–Wolfowitz_runs_test
#
# Exact inference (Swed–Eisenhart PMF) when n1 <= 10 or n2 <= 10.
# Asymptotic normal Z (no continuity correction) when both n1, n2 > 10.
# Source runs_critical_values.R after runs_exact_pmf is defined (see end of file).

#' Resolve cut point for dichotomizing a numeric sequence
resolve_runs_cutpoint <- function(x, cut_method = c("median", "mean", "custom"), cut_value = NULL) {
  cut_method <- match.arg(cut_method)
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 1L) {
    stop("No finite values available for cut-point calculation.", call. = FALSE)
  }
  if (identical(cut_method, "median")) {
    return(stats::median(x))
  }
  if (identical(cut_method, "mean")) {
    return(mean(x))
  }
  if (is.null(cut_value) || length(cut_value) < 1L || !is.finite(as.numeric(cut_value)[1L])) {
    stop("Custom cut point must be a finite number.", call. = FALSE)
  }
  as.numeric(cut_value)[1L]
}

#' Dichotomize sequence relative to cut; drop NA and values equal to cut
#'
#' @return list(signs, n1, n2, n.equal, n.na, cut)
dichotomize_for_runs <- function(x, cut) {
  x <- as.numeric(x)
  cut <- as.numeric(cut)[1L]
  if (!is.finite(cut)) {
    stop("Cut point must be finite.", call. = FALSE)
  }
  n_na <- sum(!is.finite(x))
  x <- x[is.finite(x)]
  n_equal <- sum(x == cut)
  keep <- x != cut
  x <- x[keep]
  if (length(x) < 2L) {
    stop("Need at least 2 observations after omitting NA and ties at the cut.", call. = FALSE)
  }
  signs <- ifelse(x > cut, "+", "-")
  n1 <- sum(signs == "+")
  n2 <- sum(signs == "-")
  if (n1 < 1L || n2 < 1L) {
    stop("Need at least one observation on each side of the cut.", call. = FALSE)
  }
  list(
    signs = signs,
    n1 = as.integer(n1),
    n2 = as.integer(n2),
    n.equal = as.integer(n_equal),
    n.na = as.integer(n_na),
    cut = cut
  )
}

#' Count runs in a +/- character (or factor) sequence
count_runs <- function(signs) {
  signs <- as.character(signs)
  signs <- signs[!is.na(signs) & nzchar(signs)]
  if (length(signs) < 1L) {
    return(0L)
  }
  as.integer(1L + sum(signs[-1L] != signs[-length(signs)]))
}

#' Exact PMF of number of runs R given n1, n2 (Swed–Eisenhart)
#'
#' @return named numeric vector of P(R = r) for feasible r
runs_exact_pmf <- function(n1, n2) {
  n1 <- as.integer(n1)[1L]
  n2 <- as.integer(n2)[1L]
  if (is.na(n1) || is.na(n2) || n1 < 1L || n2 < 1L) {
    stop("n1 and n2 must be integers >= 1 for the exact runs distribution.", call. = FALSE)
  }
  N <- n1 + n2
  denom <- lchoose(N, n1)
  r_min <- 2L
  r_max <- as.integer(2L * min(n1, n2) + 1L)
  r_vals <- seq.int(r_min, r_max)
  probs <- numeric(length(r_vals))
  names(probs) <- as.character(r_vals)

  for (i in seq_along(r_vals)) {
    r <- r_vals[i]
    if (r %% 2L == 0L) {
      k <- r / 2L
      logp <- log(2) + lchoose(n1 - 1L, k - 1L) + lchoose(n2 - 1L, k - 1L) - denom
      probs[i] <- if (is.finite(logp)) exp(logp) else 0
    } else {
      k <- (r - 1L) / 2L
      t1 <- lchoose(n1 - 1L, k) + lchoose(n2 - 1L, k - 1L) - denom
      t2 <- lchoose(n1 - 1L, k - 1L) + lchoose(n2 - 1L, k) - denom
      p <- 0
      if (is.finite(t1)) p <- p + exp(t1)
      if (is.finite(t2)) p <- p + exp(t2)
      probs[i] <- p
    }
  }
  # Guard tiny numeric drift
  s <- sum(probs)
  if (is.finite(s) && s > 0) {
    probs <- probs / s
  }
  probs
}

#' Exact p-value from runs PMF
runs_exact_pvalue <- function(R, n1, n2, alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)
  R <- as.integer(R)[1L]
  pmf <- runs_exact_pmf(n1, n2)
  r_vals <- as.integer(names(pmf))
  if (!(R %in% r_vals)) {
    stop("Observed number of runs is not possible for the given n1 and n2.", call. = FALSE)
  }
  p_le <- sum(pmf[r_vals <= R])
  p_ge <- sum(pmf[r_vals >= R])
  if (identical(alternative, "less")) {
    return(min(1, p_le))
  }
  if (identical(alternative, "greater")) {
    return(min(1, p_ge))
  }
  min(1, 2 * min(p_le, p_ge))
}

#' Exact critical values for R given n1, n2, alpha, alternative
#'
#' Discrete Type I rate is kept <= alpha (conservative).
#' two.sided: RL = max { r : P(R <= r) <= alpha/2 }; RU = min { r : P(R >= r) <= alpha/2 }
#'
#' @return list(critical.lower, critical.upper) — NA when that side is unused
runs_exact_critical <- function(n1, n2, alpha = 0.05,
                                alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)
  alpha <- as.numeric(alpha)[1L]
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be between 0 and 1.", call. = FALSE)
  }
  pmf <- runs_exact_pmf(n1, n2)
  r_vals <- as.integer(names(pmf))
  cdf <- cumsum(pmf)
  names(cdf) <- names(pmf)
  sf <- rev(cumsum(rev(pmf)))
  names(sf) <- names(pmf)

  lower_tail_crit <- function(a) {
    ok <- which(cdf <= a + 1e-14)
    if (length(ok) < 1L) {
      return(NA_real_)
    }
    as.numeric(r_vals[max(ok)])
  }
  upper_tail_crit <- function(a) {
    ok <- which(sf <= a + 1e-14)
    if (length(ok) < 1L) {
      return(NA_real_)
    }
    as.numeric(r_vals[min(ok)])
  }

  if (identical(alternative, "less")) {
    return(list(critical.lower = lower_tail_crit(alpha), critical.upper = NA_real_))
  }
  if (identical(alternative, "greater")) {
    return(list(critical.lower = NA_real_, critical.upper = upper_tail_crit(alpha)))
  }
  list(
    critical.lower = lower_tail_crit(alpha / 2),
    critical.upper = upper_tail_crit(alpha / 2)
  )
}

# Spot-check cells (reject if R <= RL or R >= RU at alpha = 0.05 two-sided)
RUNS_CRITICAL_SPOTCHECKS_005 <- data.frame(
  n1 = c(5L, 8L, 10L, 4L, 10L),
  n2 = c(5L, 8L, 10L, 6L, 15L),
  RL = c(2L, 4L, 6L, 2L, 7L),
  RU = c(10L, 14L, 16L, 9L, 18L),
  stringsAsFactors = FALSE
)

#' Asymptotic mean and sd of R (NIST / Wikipedia)
runs_expected_moments <- function(n1, n2) {
  n1 <- as.numeric(n1)[1L]
  n2 <- as.numeric(n2)[1L]
  N <- n1 + n2
  mean_r <- (2 * n1 * n2) / N + 1
  var_r <- (2 * n1 * n2 * (2 * n1 * n2 - n1 - n2)) / (N^2 * (N - 1))
  list(mean = mean_r, sd = sqrt(var_r), var = var_r, N = N)
}

#' Feasible range for R given n1, n2
runs_feasible_range <- function(n1, n2) {
  n1 <- as.integer(n1)[1L]
  n2 <- as.integer(n2)[1L]
  if (is.na(n1) || is.na(n2) || n1 < 0L || n2 < 0L) {
    return(c(NA_integer_, NA_integer_))
  }
  if (n1 < 1L || n2 < 1L) {
    return(c(NA_integer_, NA_integer_))
  }
  c(2L, as.integer(2L * min(n1, n2) + 1L))
}

.validate_runs_inputs <- function(n1, n2, runs) {
  n1 <- as.integer(n1)[1L]
  n2 <- as.integer(n2)[1L]
  runs <- as.integer(runs)[1L]
  if (any(is.na(c(n1, n2, runs)))) {
    stop("n1, n2, and runs must be integers.", call. = FALSE)
  }
  if (n1 < 0L || n2 < 0L) {
    stop("n1 and n2 must be >= 0.", call. = FALSE)
  }
  if (n1 < 1L || n2 < 1L) {
    stop("Need at least one observation on each side of the cut (n1 >= 1 and n2 >= 1).", call. = FALSE)
  }
  if ((n1 + n2) < 2L) {
    stop("Need at least 2 dichotomized observations.", call. = FALSE)
  }
  rng <- runs_feasible_range(n1, n2)
  if (is.na(runs) || runs < rng[1L] || runs > rng[2L]) {
    stop(
      sprintf(
        "R = %d is not possible for n1 = %d, n2 = %d (feasible range %d to %d).",
        runs, n1, n2, rng[1L], rng[2L]
      ),
      call. = FALSE
    )
  }
  list(n1 = n1, n2 = n2, runs = runs)
}

#' Runs test from sufficient statistics
runs.test.simple <- function(n1, n2, runs, alternative = c("two.sided", "less", "greater"),
                             conf.level = 0.95) {
  alternative <- match.arg(alternative)
  conf.level <- as.numeric(conf.level)[1L]
  if (!is.finite(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1.", call. = FALSE)
  }
  alpha <- 1 - conf.level
  v <- .validate_runs_inputs(n1, n2, runs)
  n1 <- v$n1
  n2 <- v$n2
  R <- v$runs
  mom <- runs_expected_moments(n1, n2)
  z <- (R - mom$mean) / mom$sd

  use_exact <- (n1 <= 10L) || (n2 <= 10L)
  if (use_exact) {
    p <- runs_exact_pvalue(R, n1, n2, alternative = alternative)
    crit <- runs_exact_critical(n1, n2, alpha = alpha, alternative = alternative)
    method <- "Exact Wald–Wolfowitz Runs Test for Randomness"
    statistic <- c(R = R)
    inference_mode <- "exact"
  } else {
    if (identical(alternative, "less")) {
      p <- stats::pnorm(z)
    } else if (identical(alternative, "greater")) {
      p <- stats::pnorm(z, lower.tail = FALSE)
    } else {
      p <- 2 * stats::pnorm(-abs(z))
    }
    crit <- list(critical.lower = NA_real_, critical.upper = NA_real_)
    method <- "Asymptotic Wald–Wolfowitz Runs Test for Randomness"
    statistic <- c(Z = z)
    inference_mode <- "asymptotic"
  }

  estimate <- c(
    n1 = n1,
    n2 = n2,
    runs = R,
    expected.runs = mom$mean,
    sd.runs = mom$sd,
    Z = z,
    critical.lower = crit$critical.lower,
    critical.upper = crit$critical.upper
  )

  structure(
    list(
      statistic = statistic,
      parameter = c(n1 = n1, n2 = n2),
      p.value = as.numeric(p),
      estimate = estimate,
      null.value = c(R = mom$mean),
      alternative = alternative,
      method = method,
      data.name = sprintf("n1 = %d, n2 = %d, R = %d", n1, n2, R),
      conf.level = conf.level,
      inference_mode = inference_mode,
      critical.lower = crit$critical.lower,
      critical.upper = crit$critical.upper
    ),
    class = "htest"
  )
}

#' Runs test from a numeric sequence
runs.test <- function(x, cut_method = c("median", "mean", "custom"), cut_value = NULL,
                      alternative = c("two.sided", "less", "greater"), conf.level = 0.95) {
  cut_method <- match.arg(cut_method)
  alternative <- match.arg(alternative)
  cut <- resolve_runs_cutpoint(x, cut_method = cut_method, cut_value = cut_value)
  dich <- dichotomize_for_runs(x, cut)
  R <- count_runs(dich$signs)
  out <- runs.test.simple(
    n1 = dich$n1,
    n2 = dich$n2,
    runs = R,
    alternative = alternative,
    conf.level = conf.level
  )
  out$estimate <- c(
    out$estimate,
    n.equal = dich$n.equal,
    cut = dich$cut
  )
  out$cut_method <- cut_method
  out$cut.value <- dich$cut
  out$n.equal <- dich$n.equal
  out$data.name <- deparse(substitute(x))
  out
}
