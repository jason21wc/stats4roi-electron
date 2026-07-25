# Wheeler-style EWMA calculations for individuals and subgroup means.

#' Asymptotic EWMA control limits.
#'
#' UCL/LCL = target +/- L * K * sqrt(alpha / (2 - alpha))
spc_compute_ewma_limits <- function(target, alpha, K, L = 3) {
  target <- as.numeric(target)
  alpha <- as.numeric(alpha)
  K <- as.numeric(K)
  L <- as.numeric(L)

  if (!is.finite(target)) {
    stop("target must be finite.")
  }
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be between 0 and 1 (exclusive).")
  }
  if (!is.finite(K) || K <= 0) {
    stop("K (sigma estimate) must be positive.")
  }
  if (!is.finite(L) || L <= 0) {
    stop("L (standard errors) must be positive.")
  }

  half_width <- L * K * sqrt(alpha / (2 - alpha))
  list(
    target = target,
    alpha = alpha,
    K = K,
    L = L,
    half_width = as.numeric(half_width),
    UCL = as.numeric(target + half_width),
    LCL = as.numeric(target - half_width)
  )
}

#' Build EWMA point table with asymptotic limits and OOC flags.
#' Seed (starting EWMA) as the mean of the first seed_n observations.
#'
#' Falls back to the mean of all finite values, then NA, when the first
#' seed_n values are not finite.
spc_ewma_seed_value <- function(values, seed_n = 5L) {
  values <- as.numeric(values)
  seed_n <- as.integer(seed_n)
  if (!is.finite(seed_n) || seed_n < 1L) seed_n <- 1L
  take <- values[seq_len(min(seed_n, length(values)))]
  take <- take[is.finite(take)]
  if (length(take) == 0L) {
    finite_all <- values[is.finite(values)]
    if (length(finite_all) == 0L) return(NA_real_)
    return(mean(finite_all))
  }
  mean(take)
}

#' Build EWMA point table using the forecast recurrence.
#'
#' The first plotted EWMA equals the seed (mean of the first seed_n points).
#' Thereafter the EWMA is a one-step-ahead forecast:
#'   z_i = alpha * y_{i-1} + (1 - alpha) * z_{i-1}
#' so the value plotted at sample i reflects observations through sample i-1.
#' When seed is NULL it defaults to the mean of the first seed_n observations.
spc_compute_ewma_table <- function(values, target, alpha, K, L = 3, mr = NULL,
                                   seed = NULL, seed_n = 5L) {
  values <- as.numeric(values)
  target <- as.numeric(target)
  alpha <- as.numeric(alpha)
  K <- as.numeric(K)
  L <- as.numeric(L)

  limits <- spc_compute_ewma_limits(target = target, alpha = alpha, K = K, L = L)
  n <- length(values)

  if (n == 0L) {
    return(data.frame(
      Sample = integer(0),
      Value = numeric(0),
      MR = numeric(0),
      Dev = numeric(0),
      EWMA = numeric(0),
      UCL = numeric(0),
      LCL = numeric(0),
      Target = numeric(0),
      OOC = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  if (is.null(mr)) {
    if (!exists("ppa_moving_range", mode = "function")) {
      source("modules/statistical/spc/utils/spc_sigma_from_limits.R", local = FALSE)
    }
    mr <- ppa_moving_range(values, span = 2L)
  }
  mr <- as.numeric(mr)
  if (length(mr) != n) {
    mr <- c(mr, rep(NA_real_, n - length(mr)))[seq_len(n)]
  }

  if (is.null(seed) || !is.finite(seed)) {
    seed <- spc_ewma_seed_value(values, seed_n = seed_n)
  }
  seed <- as.numeric(seed)
  if (!is.finite(seed)) seed <- target

  ewma <- numeric(n)
  ewma[1] <- seed
  z_prev <- seed
  if (n >= 2L) {
    for (i in 2:n) {
      y_prev <- values[i - 1L]
      if (!is.finite(y_prev)) {
        ewma[i] <- z_prev
        next
      }
      ewma[i] <- alpha * y_prev + (1 - alpha) * z_prev
      z_prev <- ewma[i]
    }
  }

  ooc <- is.finite(ewma) & ((ewma > limits$UCL) | (ewma < limits$LCL))

  data.frame(
    Sample = seq_len(n),
    Value = values,
    MR = mr,
    Dev = values - target,
    EWMA = ewma,
    UCL = rep(limits$UCL, n),
    LCL = rep(limits$LCL, n),
    Target = rep(target, n),
    OOC = ooc,
    stringsAsFactors = FALSE
  )
}

#' Prepare EWMA input series — thin alias over CUSUM data prep.
spc_prepare_ewma_data <- function(
  data,
  mode = c("individuals", "means"),
  data_type = 1L,
  ui1,
  ui2 = NULL,
  sets_col = 0L
) {
  if (!exists("spc_prepare_cusum_data", mode = "function")) {
    source("modules/statistical/spc/utils/spc_cusum_calcs.R", local = FALSE)
  }
  spc_prepare_cusum_data(
    data = data,
    mode = mode,
    data_type = data_type,
    ui1 = ui1,
    ui2 = ui2,
    sets_col = sets_col
  )
}

#' Full EWMA analysis result from prepared data + parameters.
spc_analyze_ewma <- function(
  prepared,
  target,
  alpha = 0.2,
  L = 3,
  sigma_method = c("mean", "median"),
  mr_span = 2L,
  seed_n = 5L
) {
  sigma_method <- match.arg(sigma_method)
  if (!exists("spc_estimate_cusum_sigma", mode = "function")) {
    source("modules/statistical/spc/utils/spc_cusum_calcs.R", local = FALSE)
  }

  sigma_est <- spc_estimate_cusum_sigma(
    mode = prepared$mode,
    values = prepared$values,
    ranges = prepared$ranges,
    n = prepared$n,
    sigma_method = sigma_method,
    mr_span = mr_span,
    sets = prepared$sets
  )
  if (!is.finite(sigma_est$K) || sigma_est$K <= 0) {
    stop("Unable to estimate sigma (K). Check data and sigma method.")
  }

  limits <- spc_compute_ewma_limits(
    target = target,
    alpha = alpha,
    K = sigma_est$K,
    L = L
  )

  if (!exists("ppa_moving_range", mode = "function")) {
    source("modules/statistical/spc/utils/spc_sigma_from_limits.R", local = FALSE)
  }
  series_mr <- ppa_moving_range(prepared$values, span = 2L, reset_at = prepared$sets)

  seed <- spc_ewma_seed_value(prepared$values, seed_n = seed_n)

  table <- spc_compute_ewma_table(
    values = prepared$values,
    target = target,
    alpha = alpha,
    K = sigma_est$K,
    L = L,
    mr = series_mr,
    seed = seed,
    seed_n = seed_n
  )

  list(
    prepared = prepared,
    sigma = sigma_est,
    limits = limits,
    table = table,
    target = as.numeric(target),
    seed = as.numeric(seed),
    seed_n = as.integer(seed_n)
  )
}
