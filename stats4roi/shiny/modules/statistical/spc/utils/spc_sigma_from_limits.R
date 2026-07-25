# Estimate process sigma from SPC-style chart/limit configuration.

source_if_missing <- function(path) {
  if (!exists("MR_span", mode = "function")) {
    if (file.exists(path)) source(path, local = FALSE)
  }
}

#' Build MR vector for a numeric series (optionally reset at stream changes).
ppa_moving_range <- function(values, span = 2L, reset_at = NULL) {
  values <- as.numeric(values)
  n <- length(values)
  if (n < 2) {
    return(rep(NA_real_, n))
  }
  if (!exists("MR_span", mode = "function")) {
    source("modules/statistical/spc/utils/spc_limit_calcs.R", local = FALSE)
  }
  mr <- MR_span(data = values, span = as.integer(span))
  if (!is.null(reset_at) && length(reset_at) == n) {
    changes <- c(FALSE, diff(reset_at) != 0)
    if (any(changes)) {
      mr[changes] <- NA_real_
    }
  }
  mr
}

ppa_sigma_loc_lim_label <- function(limit_config) {
  if (!exists("choice_x_bar_limits2")) {
    source("modules/statistical/spc/utils/spc_constants.R", local = FALSE)
  }
  if (isTRUE(limit_config$ind_or_mean)) {
    loc_lim <- as.integer(limit_config$loc_lim %||% 1L)
    idx <- match(loc_lim, choice_x_bar_limits2)
    if (!is.na(idx)) {
      return(unname(names(choice_x_bar_limits2)[idx]))
    }
    return("X-bar location")
  }
  loc_lim <- as.integer(limit_config$loc_lim %||% 7L)
  if (!exists("spc_individuals_loc_lim_choices", mode = "function")) {
    source("modules/statistical/spc/utils/spc_limit_choice_helpers.R", local = FALSE)
  }
  ch <- spc_individuals_loc_lim_choices()
  idx <- match(loc_lim, ch)
  if (!is.na(idx)) {
    return(unname(names(ch)[idx]))
  }
  "Location"
}

ppa_sigma_disp_lim_label <- function(limit_config) {
  if (!exists("ppa_disp_lim_label", mode = "function")) {
    if (file.exists("modules/statistical/spc/utils/ppa_control_chart_limits.R")) {
      source("modules/statistical/spc/utils/ppa_control_chart_limits.R", local = FALSE)
    }
  }
  ppa_disp_lim_label(limit_config$disp_lim %||% 7L)
}

ppa_sigma_method_label <- function(limit_config) {
  if (!exists("spc_sigma_source_rule", mode = "function")) {
    source("modules/statistical/spc/utils/spc_limit_choice_helpers.R", local = FALSE)
  }
  if (identical(spc_sigma_source_rule(limit_config), "location")) {
    return(ppa_sigma_loc_lim_label(limit_config))
  }
  ppa_sigma_disp_lim_label(limit_config)
}

ppa_replicate_row_stats <- function(replicate_matrix) {
  replicate_matrix <- as.matrix(replicate_matrix)
  n_rep <- ncol(replicate_matrix)
  range <- apply(replicate_matrix, 1, function(row) {
    row <- row[is.finite(row)]
    if (length(row) < 2) NA_real_ else max(row) - min(row)
  })
  sd <- apply(replicate_matrix, 1, function(row) {
    row <- row[is.finite(row)]
    if (length(row) < 2) NA_real_ else stats::sd(row)
  })
  var <- apply(replicate_matrix, 1, function(row) {
    row <- row[is.finite(row)]
    if (length(row) < 2) NA_real_ else stats::var(row)
  })
  list(range = range, sd = sd, var = var, n_rep = n_rep)
}

#' Estimate within-row measurement sigma from dispersion-limit statistic.
#'
#' @return list(sigma, stat, method, n_rep)
ppa_estimate_replicates_sigma <- function(replicate_matrix, limit_config = NULL) {
  if (!exists("c6", mode = "function")) {
    source("modules/statistical/spc/utils/spc_limit_calcs.R", local = FALSE)
  }
  if (!exists("spc_replicate_disp_lim_choices", mode = "function")) {
    source("modules/statistical/spc/utils/spc_limit_choice_helpers.R", local = FALSE)
  }
  row_stats <- ppa_replicate_row_stats(replicate_matrix)
  n_rep <- row_stats$n_rep
  disp_lim <- as.integer(limit_config$disp_lim %||% 1L)
  known_sigma <- limit_config$known_sigma

  if (disp_lim == 9L) {
    sig <- as.numeric(known_sigma)
    return(list(
      sigma = sig,
      stat = sig,
      method = "Known \u03c3",
      n_rep = n_rep
    ))
  }

  method_names <- names(spc_replicate_disp_lim_choices())
  method_ids <- unname(spc_replicate_disp_lim_choices())
  method <- method_names[match(disp_lim, method_ids)]
  if (is.na(method)) method <- "Average Range"

  if (disp_lim == 1L) {
    stat <- mean(row_stats$range, na.rm = TRUE)
    sigma <- stat / lolcat::spc.constant.calculation.d2(sample.size = n_rep)
  } else if (disp_lim == 2L) {
    stat <- stats::median(row_stats$range, na.rm = TRUE)
    sigma <- stat / lolcat::spc.constant.calculation.d4(sample.size = n_rep)
  } else if (disp_lim == 3L) {
    stat <- mean(row_stats$sd, na.rm = TRUE)
    sigma <- stat / lolcat::spc.constant.calculation.c4(sample.size = n_rep)
  } else if (disp_lim == 4L) {
    stat <- stats::median(row_stats$sd, na.rm = TRUE)
    sigma <- stat / c6(n_rep)
  } else {
    # Average Variance (default / disp_lim == 5)
    stat <- mean(row_stats$var, na.rm = TRUE)
    sigma <- sqrt(stat)
  }
  list(sigma = sigma, stat = stat, method = method, n_rep = n_rep)
}

ppa_xbar_pts_from_values <- function(values, subgroup, sample_id) {
  df <- data.frame(value = values, sample = sample_id, stringsAsFactors = FALSE)
  if (!is.null(subgroup)) {
    df$subgroup <- subgroup
  }
  do.call(rbind, lapply(split(df, df$sample), function(part) {
    n <- nrow(part)
    data.frame(
      n = n,
      mean = mean(part$value),
      range = if (n > 1) max(part$value) - min(part$value) else NA_real_,
      sd = if (n > 1) stats::sd(part$value) else NA_real_,
      var = if (n > 1) stats::var(part$value) else NA_real_
    )
  }))
}

ppa_sigma_from_individuals_location <- function(values, limit_config) {
  if (!exists("c6", mode = "function")) {
    source("modules/statistical/spc/utils/spc_limit_calcs.R", local = FALSE)
  }
  values <- as.numeric(values)
  span <- as.integer(limit_config$mr_span %||% 2L)
  known_sigma <- limit_config$known_sigma
  loc_lim <- as.integer(limit_config$loc_lim %||% 7L)
  if (loc_lim %in% 1:4L) {
    loc_lim <- loc_lim + 5L
  }
  mr <- ppa_moving_range(values, span = span)
  stat <- switch(
    as.character(loc_lim),
    "6" = mean(na.omit(mr)),
    "7" = median(na.omit(mr)),
    "8" = stats::sd(values, na.rm = TRUE),
    "9" = as.numeric(known_sigma),
    median(na.omit(mr))
  )
  if (is.na(stat)) {
    return(list(sigma = NA_real_, stat = NA_real_, method = ppa_sigma_loc_lim_label(limit_config)))
  }
  sigma <- switch(
    as.character(loc_lim),
    "6" = stat / lolcat::spc.constant.calculation.d2(sample.size = span),
    "7" = stat / lolcat::spc.constant.calculation.d4(sample.size = span),
    "8" = stat,
    "9" = stat,
    stat / lolcat::spc.constant.calculation.d4(sample.size = span)
  )
  list(sigma = sigma, stat = stat, method = ppa_sigma_loc_lim_label(limit_config))
}

ppa_sigma_from_individuals_dispersion <- function(values, limit_config) {
  values <- as.numeric(values)
  span <- as.integer(limit_config$mr_span %||% 2L)
  known_sigma <- limit_config$known_sigma
  disp_lim <- as.integer(limit_config$disp_lim %||% 7L)
  if (!is.null(known_sigma) && !is.na(known_sigma) && disp_lim == 9L) {
    return(list(sigma = as.numeric(known_sigma), stat = known_sigma, method = "Known sigma"))
  }
  mr <- ppa_moving_range(values, span = span)
  stat <- switch(
    as.character(disp_lim),
    "6" = mean(na.omit(mr)),
    "7" = median(na.omit(mr)),
    "8" = stats::sd(values, na.rm = TRUE),
    "9" = as.numeric(known_sigma),
    mean(na.omit(mr))
  )
  if (is.na(stat)) {
    return(list(sigma = NA_real_, stat = NA_real_, method = ppa_sigma_disp_lim_label(limit_config)))
  }
  sigma <- switch(
    as.character(disp_lim),
    "6" = stat / lolcat::spc.constant.calculation.d2(sample.size = span),
    "7" = stat / lolcat::spc.constant.calculation.d4(sample.size = span),
    "8" = stat,
    "9" = stat,
    stat / lolcat::spc.constant.calculation.d4(sample.size = span)
  )
  list(sigma = sigma, stat = stat, method = ppa_sigma_disp_lim_label(limit_config))
}

ppa_sigma_from_xbar_location <- function(pts, limit_config) {
  if (!exists("c6", mode = "function")) {
    source("modules/statistical/spc/utils/spc_limit_calcs.R", local = FALSE)
  }
  n_bar <- mean(pts$n)
  loc_lim <- as.integer(limit_config$loc_lim %||% 1L)
  stat <- switch(
    as.character(loc_lim),
    "1" = mean(pts$range, na.rm = TRUE),
    "2" = stats::median(pts$range, na.rm = TRUE),
    "3" = mean(pts$sd, na.rm = TRUE),
    "4" = stats::median(pts$sd, na.rm = TRUE),
    "5" = mean(pts$var, na.rm = TRUE),
    stats::median(pts$range, na.rm = TRUE)
  )
  sigma <- switch(
    as.character(loc_lim),
    "1" = stat / lolcat::spc.constant.calculation.d2(sample.size = n_bar),
    "2" = stat / lolcat::spc.constant.calculation.d4(sample.size = n_bar),
    "3" = stat / lolcat::spc.constant.calculation.c4(sample.size = n_bar),
    "4" = stat / c6(n_bar),
    "5" = sqrt(stat),
    "6" = {
      n <- n_bar
      sqrt(n) * mean(abs(diff(pts$mean)), na.rm = TRUE) /
        lolcat::spc.constant.calculation.d2(sample.size = 2)
    },
    "7" = {
      n <- n_bar
      sqrt(n) * stats::median(abs(diff(pts$mean)), na.rm = TRUE) /
        lolcat::spc.constant.calculation.d4(sample.size = 2)
    },
    "8" = sqrt(n_bar) * stats::sd(pts$mean, na.rm = TRUE),
    "9" = as.numeric(limit_config$known_sigma),
    stat / lolcat::spc.constant.calculation.d4(sample.size = n_bar)
  )
  list(sigma = sigma, stat = stat, method = ppa_sigma_loc_lim_label(limit_config))
}

ppa_sigma_from_xbar_dispersion <- function(pts, limit_config) {
  if (!exists("c6", mode = "function")) {
    source("modules/statistical/spc/utils/spc_limit_calcs.R", local = FALSE)
  }
  n_bar <- mean(pts$n)
  disp_type <- as.integer(limit_config$disp_type %||% 1L)
  disp_lim <- as.integer(limit_config$disp_lim %||% 1L)
  stat <- NA_real_
  sigma <- NA_real_

  if (disp_type == 1L) {
    stat <- switch(as.character(disp_lim),
      "1" = mean(pts$range, na.rm = TRUE),
      "2" = stats::median(pts$range, na.rm = TRUE),
      "3" = mean(pts$sd, na.rm = TRUE),
      "4" = stats::median(pts$sd, na.rm = TRUE),
      stats::median(pts$range, na.rm = TRUE)
    )
    d <- switch(as.character(disp_lim),
      "1" = lolcat::spc.constant.calculation.d2(sample.size = n_bar),
      "2" = lolcat::spc.constant.calculation.d4(sample.size = n_bar),
      "3" = lolcat::spc.constant.calculation.c4(sample.size = n_bar),
      "4" = c6(n_bar),
      lolcat::spc.constant.calculation.d4(sample.size = n_bar)
    )
    sigma <- if (disp_lim %in% c(8L)) stat else stat / d
  } else if (disp_type == 2L) {
    stat <- switch(as.character(disp_lim),
      "3" = mean(pts$sd, na.rm = TRUE),
      "4" = stats::median(pts$sd, na.rm = TRUE),
      "6" = mean(abs(diff(pts$sd)), na.rm = TRUE),
      "7" = stats::median(abs(diff(pts$sd)), na.rm = TRUE),
      "8" = stats::sd(pts$sd, na.rm = TRUE),
      stats::median(pts$sd, na.rm = TRUE)
    )
    d <- switch(as.character(disp_lim),
      "3" = lolcat::spc.constant.calculation.c4(sample.size = n_bar),
      "4" = c6(n_bar),
      "6" = lolcat::spc.constant.calculation.d2(sample.size = 2),
      "7" = lolcat::spc.constant.calculation.d4(sample.size = 2),
      "8" = 1,
      c6(n_bar)
    )
    sigma <- if (disp_lim == 8L) stat else stat / d
  } else {
    mr <- ppa_moving_range(pts$mean, span = limit_config$mr_span %||% 2L)
    stat <- stats::median(na.omit(mr))
    sigma <- stat / lolcat::spc.constant.calculation.d4(sample.size = limit_config$mr_span %||% 2L)
  }
  list(sigma = sigma, stat = stat, method = ppa_sigma_disp_lim_label(limit_config))
}

#' Estimate sigma for repeated-measures (row means + replicate matrix).
#'
#' Product short-term sigma is estimated from location limits on row means only.
#' Dispersion limits (R/s/s² within row) describe measurement error for monitoring.
spc_estimate_sigma_replicate <- function(values, replicate_matrix, limit_config) {
  values <- as.numeric(values)
  ppa_sigma_from_individuals_location(values, limit_config)
}

#' Estimate sigma for one stream's series using limit configuration.
spc_estimate_sigma <- function(
  values,
  subgroup = NULL,
  sample_id = NULL,
  limit_config,
  replicate_matrix = NULL
) {
  if (!requireNamespace("lolcat", quietly = TRUE)) {
    stop("lolcat package required for spc_estimate_sigma", call. = FALSE)
  }
  if (!exists("spc_sigma_source_rule", mode = "function")) {
    source("modules/statistical/spc/utils/spc_limit_choice_helpers.R", local = FALSE)
  }

  values <- as.numeric(values)
  shape <- limit_config$data_shape %||% NULL
  if (is.null(shape)) {
    shape <- if (!is.null(replicate_matrix)) {
      "replicate"
    } else if (isTRUE(limit_config$ind_or_mean)) {
      "subgroup"
    } else {
      "single"
    }
  }

  if (identical(shape, "replicate") && !is.null(replicate_matrix)) {
    return(spc_estimate_sigma_replicate(values, replicate_matrix, limit_config))
  }

  source_rule <- spc_sigma_source_rule(limit_config)

  if (isTRUE(limit_config$ind_or_mean)) {
    pts <- ppa_xbar_pts_from_values(values, subgroup, sample_id)
    if (identical(source_rule, "location")) {
      return(ppa_sigma_from_xbar_location(pts, limit_config))
    }
    return(ppa_sigma_from_xbar_dispersion(pts, limit_config))
  }

  if (identical(source_rule, "location")) {
    return(ppa_sigma_from_individuals_location(values, limit_config))
  }
  ppa_sigma_from_individuals_dispersion(values, limit_config)
}

spc_estimate_sigma_xbar <- function(values, subgroup, sample_id, limit_config) {
  pts <- ppa_xbar_pts_from_values(values, subgroup, sample_id)
  if (!exists("spc_sigma_source_rule", mode = "function")) {
    source("modules/statistical/spc/utils/spc_limit_choice_helpers.R", local = FALSE)
  }
  if (identical(spc_sigma_source_rule(limit_config), "location")) {
    return(ppa_sigma_from_xbar_location(pts, limit_config))
  }
  ppa_sigma_from_xbar_dispersion(pts, limit_config)
}

#' Location control limits for PPA run chart (backward-compatible wrapper).
ppa_compute_run_chart_limits <- function(prepared, limit_config, R = 4L) {
  if (!exists("ppa_compute_control_chart", mode = "function")) {
    stop("ppa_compute_control_chart is not available; source ppa_control_chart_limits.R", call. = FALSE)
  }
  chart <- ppa_compute_control_chart(prepared, limit_config, R)
  if (is.null(chart)) {
    return(NULL)
  }
  chart$location
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
