# Wheeler-style tabular CUSUM calculations for individuals and subgroup means.

#' Decision interval d (= h dimensionless) from alpha, beta, and k.
#'
#' Formula: d = (2 / delta^2) * ln((1 - beta) / alpha) with delta = 2 * k.
#' When beta = 0 this reduces to ln(1 / alpha) / (2 * k^2).
spc_compute_cusum_decision_limits <- function(alpha, beta = 0, k = 1, K = 1) {
  alpha <- as.numeric(alpha)
  beta <- as.numeric(beta)
  k <- as.numeric(k)
  K <- as.numeric(K)

  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be between 0 and 1 (exclusive).")
  }
  if (!is.finite(beta) || beta < 0 || beta >= 1) {
    stop("beta must be in [0, 1).")
  }
  if (!is.finite(k) || k <= 0) {
    stop("k must be positive.")
  }
  if (!is.finite(K) || K <= 0) {
    stop("K (sigma estimate) must be positive.")
  }

  delta <- 2 * k
  d <- (2 / (delta^2)) * log((1 - beta) / alpha)
  h <- d
  H <- h * K
  list(
    alpha = alpha,
    beta = beta,
    k = k,
    K = K,
    delta = delta,
    d = d,
    h = h,
    H = H,
    shift_size = 2 * k * K
  )
}

#' Estimate K (sigma of the plotted series) for CUSUM.
#'
#' @param mode "individuals" or "means"
#' @param values numeric series for individuals, or subgroup means for means mode
#' @param ranges subgroup ranges (required for means mode)
#' @param n subgroup size (required for means mode); average n if varying
#' @param sigma_method "mean" or "median" (MR for individuals; range for means)
#' @param mr_span moving-range span for individuals (default 2)
#' @param sets optional set labels for resetting MR at set boundaries
spc_estimate_cusum_sigma <- function(
  mode = c("individuals", "means"),
  values,
  ranges = NULL,
  n = NULL,
  sigma_method = c("mean", "median"),
  mr_span = 2L,
  sets = NULL
) {
  mode <- match.arg(mode)
  sigma_method <- match.arg(sigma_method)
  values <- as.numeric(values)

  if (mode == "individuals") {
    if (!exists("ppa_moving_range", mode = "function")) {
      source("modules/statistical/spc/utils/spc_sigma_from_limits.R", local = FALSE)
    }
    if (!exists("spc_normalize_mr_span", mode = "function")) {
      source("modules/statistical/spc/utils/spc_constants.R", local = FALSE)
    }
    span <- spc_normalize_mr_span(mr_span)
    mr <- ppa_moving_range(values, span = span, reset_at = sets)
    finite_mr <- mr[is.finite(mr)]
    if (length(finite_mr) == 0L) {
      return(list(K = NA_real_, stat = NA_real_, mr = mr, method = sigma_method, n = 1L, span = span))
    }
    stat <- if (identical(sigma_method, "mean")) mean(finite_mr) else stats::median(finite_mr)
    divisor <- if (identical(sigma_method, "mean")) {
      lolcat::spc.constant.calculation.d2(sample.size = span)
    } else {
      lolcat::spc.constant.calculation.d4(sample.size = span)
    }
    K <- stat / divisor
    return(list(
      K = as.numeric(K),
      stat = as.numeric(stat),
      mr = mr,
      method = sigma_method,
      n = 1L,
      span = span,
      method_label = if (identical(sigma_method, "mean")) {
        "Average Moving Range"
      } else {
        "Median Moving Range"
      }
    ))
  }

  # means: K = (R-bar or R-tilde / d2 or d4) / sqrt(n) = sigma of subgroup means
  ranges <- as.numeric(ranges)
  n <- as.numeric(n)
  if (length(n) > 1L) n <- mean(n, na.rm = TRUE)
  if (!is.finite(n) || n < 2) {
    stop("Means CUSUM requires subgroup size n >= 2.")
  }
  finite_r <- ranges[is.finite(ranges)]
  if (length(finite_r) == 0L) {
    return(list(K = NA_real_, stat = NA_real_, mr = NULL, method = sigma_method, n = n, span = NA_integer_))
  }
  stat <- if (identical(sigma_method, "mean")) mean(finite_r) else stats::median(finite_r)
  divisor <- if (identical(sigma_method, "mean")) {
    lolcat::spc.constant.calculation.d2(sample.size = n)
  } else {
    lolcat::spc.constant.calculation.d4(sample.size = n)
  }
  sigma_x <- stat / divisor
  K <- sigma_x / sqrt(n)
  list(
    K = as.numeric(K),
    stat = as.numeric(stat),
    sigma_x = as.numeric(sigma_x),
    mr = NULL,
    method = sigma_method,
    n = n,
    span = NA_integer_,
    method_label = if (identical(sigma_method, "mean")) {
      "Average Range"
    } else {
      "Median Range"
    }
  )
}

#' Build full tabular CUSUM table (Cusum, Zi, Si, Ti, Si*, Ti*).
#'
#' Si*/Ti* follow spreadsheet reset semantics: when the previous starred
#' value exceeds the decision interval (+h / -h), the next starred
#' accumulator restarts from 0.
spc_compute_cusum_table <- function(values, target, K, k, h, mr = NULL) {
  values <- as.numeric(values)
  target <- as.numeric(target)
  K <- as.numeric(K)
  k <- as.numeric(k)
  h <- as.numeric(h)

  if (!is.finite(target)) stop("target must be finite.")
  if (!is.finite(K) || K <= 0) stop("K must be positive and finite.")
  if (!is.finite(k) || k <= 0) stop("k must be positive.")
  if (!is.finite(h) || h <= 0) stop("h must be positive.")

  n <- length(values)
  if (n == 0L) {
    return(data.frame(
      Sample = integer(0),
      Value = numeric(0),
      MR = numeric(0),
      Dev = numeric(0),
      Cusum = numeric(0),
      Zi = numeric(0),
      Si = numeric(0),
      Ti = numeric(0),
      Si_star = numeric(0),
      Ti_star = numeric(0),
      highlight_si_ti = logical(0),
      highlight_si_ti_star = logical(0),
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

  dev <- values - target
  zi <- dev / K
  cusum <- cumsum(ifelse(is.finite(dev), dev, 0))

  Si <- Ti <- Si_star <- Ti_star <- numeric(n)
  Si_prev <- Ti_prev <- 0
  Si_s_prev <- Ti_s_prev <- 0

  for (i in seq_len(n)) {
    z <- zi[i]
    if (!is.finite(z)) {
      Si[i] <- Si_prev
      Ti[i] <- Ti_prev
      Si_star[i] <- Si_s_prev
      Ti_star[i] <- Ti_s_prev
      next
    }
    Si[i] <- max(0, z - k + Si_prev)
    Ti[i] <- min(0, z + k + Ti_prev)

    s_base <- if (Si_s_prev >= h) 0 else Si_s_prev
    t_base <- if (Ti_s_prev <= -h) 0 else Ti_s_prev
    Si_star[i] <- max(0, z - k + s_base)
    Ti_star[i] <- min(0, z + k + t_base)

    Si_prev <- Si[i]
    Ti_prev <- Ti[i]
    Si_s_prev <- Si_star[i]
    Ti_s_prev <- Ti_star[i]
  }

  highlight_si_ti <- (Si > h) | (Ti < -h)
  highlight_si_ti_star <- (Si_star > h) | (Ti_star < -h)

  data.frame(
    Sample = seq_len(n),
    Value = values,
    MR = mr,
    Dev = dev,
    Cusum = cusum,
    Zi = zi,
    Si = Si,
    Ti = Ti,
    Si_star = Si_star,
    Ti_star = Ti_star,
    highlight_si_ti = highlight_si_ti,
    highlight_si_ti_star = highlight_si_ti_star,
    stringsAsFactors = FALSE
  )
}

#' Prepare CUSUM input series from a data frame.
#'
#' @return list with values, ranges (means only), n, sets, y_label, mr of series
spc_prepare_cusum_data <- function(
  data,
  mode = c("individuals", "means"),
  data_type = 1L,
  ui1,
  ui2 = NULL,
  sets_col = 0L
) {
  mode <- match.arg(mode)
  data_type <- as.numeric(data_type)
  ui1 <- as.numeric(ui1)
  ui2 <- if (is.null(ui2)) NULL else as.numeric(ui2)
  sets_col <- as.numeric(sets_col)
  if (length(sets_col) == 0L || is.na(sets_col[1L])) sets_col <- 0

  if (identical(mode, "individuals")) {
    col <- ui1[1L]
    values <- as.numeric(data[[col]])
    sets <- if (sets_col > 0) data[[sets_col]] else NULL
    return(list(
      mode = "individuals",
      values = values,
      ranges = NULL,
      n = 1L,
      sets = sets,
      y_label = names(data)[col],
      sample_labels = seq_along(values)
    ))
  }

  # means
  if (identical(data_type, 1)) {
    if (length(ui1) < 2L) stop("Select at least two columns for subgroup means.")
    k_obs <- nrow(data)
    mat <- as.matrix(data[ui1])
    storage.mode(mat) <- "numeric"
    values <- rowMeans(mat, na.rm = TRUE)
    ranges <- apply(mat, 1L, function(row) {
      row <- row[is.finite(row)]
      if (length(row) < 2L) NA_real_ else max(row) - min(row)
    })
    n_i <- apply(mat, 1L, function(row) sum(is.finite(row)))
    n <- mean(n_i[n_i > 0], na.rm = TRUE)
    y_label <- paste(names(data)[ui1], collapse = ", ")
    sample_labels <- seq_len(k_obs)
    sets <- if (sets_col > 0) data[[sets_col]] else NULL
  } else {
    if (is.null(ui2) || length(ui2) < 1L || is.na(ui2[1L])) {
      stop("Select a data column for column-defined subgroups.")
    }
    sample_col <- ui1[1L]
    data_col <- ui2[1L]
    samples <- data[[sample_col]]
    x <- as.numeric(data[[data_col]])
    sets_raw <- if (sets_col > 0) data[[sets_col]] else NULL
    spl <- split(data.frame(x = x, sets = if (is.null(sets_raw)) NA else sets_raw, stringsAsFactors = FALSE), samples)
    sample_labels <- names(spl)
    values <- vapply(spl, function(part) mean(part$x, na.rm = TRUE), numeric(1))
    ranges <- vapply(spl, function(part) {
      row <- part$x[is.finite(part$x)]
      if (length(row) < 2L) NA_real_ else max(row) - min(row)
    }, numeric(1))
    n_i <- vapply(spl, function(part) sum(is.finite(part$x)), numeric(1))
    n <- mean(n_i[n_i > 0], na.rm = TRUE)
    sets <- if (is.null(sets_raw)) {
      NULL
    } else {
      vapply(spl, function(part) {
        s <- part$sets
        s <- s[!is.na(s)]
        if (length(s) == 0L) NA else s[[1L]]
      }, sets_raw[[1L]])
    }
    y_label <- names(data)[data_col]
  }

  list(
    mode = "means",
    values = as.numeric(values),
    ranges = as.numeric(ranges),
    n = as.numeric(n),
    sets = sets,
    y_label = y_label,
    sample_labels = sample_labels
  )
}

#' Full CUSUM analysis result from prepared data + parameters.
spc_analyze_cusum <- function(
  prepared,
  target,
  k = 1,
  alpha = 0.005,
  beta = 0,
  sigma_method = c("mean", "median"),
  mr_span = 2L
) {
  sigma_method <- match.arg(sigma_method)
  mode <- prepared$mode
  sigma_est <- spc_estimate_cusum_sigma(
    mode = mode,
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
  limits <- spc_compute_cusum_decision_limits(
    alpha = alpha,
    beta = beta,
    k = k,
    K = sigma_est$K
  )
  # Prefer series MR for table (means: MR of subgroup means)
  if (!exists("ppa_moving_range", mode = "function")) {
    source("modules/statistical/spc/utils/spc_sigma_from_limits.R", local = FALSE)
  }
  series_mr <- ppa_moving_range(prepared$values, span = 2L, reset_at = prepared$sets)
  table <- spc_compute_cusum_table(
    values = prepared$values,
    target = target,
    K = limits$K,
    k = limits$k,
    h = limits$h,
    mr = series_mr
  )
  list(
    prepared = prepared,
    sigma = sigma_est,
    limits = limits,
    table = table,
    target = as.numeric(target)
  )
}
