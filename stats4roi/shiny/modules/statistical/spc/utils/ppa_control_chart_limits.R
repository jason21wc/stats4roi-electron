# PPA control-chart limits (individuals / X-bar) — location and dispersion decoupled.

ppa_normalize_individuals_loc_lim <- function(loc_lim) {
  loc_lim <- as.integer(loc_lim)
  switch(
    as.character(loc_lim),
    "1" = 6L,
    "2" = 7L,
    "3" = 8L,
    "4" = 9L,
    loc_lim
  )
}

ppa_loc_lim_label <- function(loc_lim) {
  loc_lim <- ppa_normalize_individuals_loc_lim(loc_lim)
  if (loc_lim %in% 1:4) {
    return(names(choice_x_limits)[loc_lim])
  }
  ch <- ppa_individuals_loc_lim_choices
  idx <- match(loc_lim, ch)
  if (is.na(idx)) "Individuals" else names(ch)[idx]
}

ppa_disp_lim_label <- function(disp_lim) {
  disp_lim <- as.integer(disp_lim)
  if (!exists("choice_r_limits2", mode = "object")) {
    if (file.exists("modules/statistical/spc/utils/spc_constants.R")) {
      source("modules/statistical/spc/utils/spc_constants.R", local = FALSE)
    }
  }
  idx <- match(disp_lim, choice_r_limits2)
  if (!is.na(idx)) {
    return(unname(names(choice_r_limits2)[idx]))
  }
  ch <- ppa_mr_disp_lim_choices
  idx <- match(disp_lim, ch)
  if (is.na(idx)) {
    return("Dispersion")
  }
  unname(names(ch)[idx])
}

ppa_individuals_location_stat <- function(values, limit_config) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (length(values) == 0) {
    return(NA_real_)
  }
  span <- as.integer(limit_config$mr_span %||% 2L)
  loc_lim <- ppa_normalize_individuals_loc_lim(limit_config$loc_lim %||% 7L)
  mr <- ppa_moving_range(values, span = span)
  switch(
    as.character(loc_lim),
    "6" = mean(na.omit(mr)),
    "7" = median(na.omit(mr)),
    "8" = stats::sd(values),
    "9" = as.numeric(limit_config$known_sigma),
    NA_real_
  )
}

ppa_mr_dispersion_stat <- function(values, limit_config) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (length(values) < 2) {
    return(NA_real_)
  }
  span <- as.integer(limit_config$mr_span %||% 2L)
  disp_lim <- as.integer(limit_config$disp_lim %||% 7L)
  mr <- ppa_moving_range(values, span = span)
  switch(
    as.character(disp_lim),
    "6" = mean(na.omit(mr)),
    "7" = median(na.omit(mr)),
    "8" = stats::sd(values),
    "9" = as.numeric(limit_config$known_sigma),
    NA_real_
  )
}

ppa_compute_individuals_location_limits <- function(values, limit_config, R = 4L) {
  if (!exists("calculate_x_limits", mode = "function")) {
    stop("calculate_x_limits is not available; source spc_limits_server.R", call. = FALSE)
  }
  loc_lim <- ppa_normalize_individuals_loc_lim(limit_config$loc_lim %||% 7L)
  stat <- ppa_individuals_location_stat(values, limit_config)
  if (is.na(stat)) {
    return(NULL)
  }
  loc_center <- as.integer(limit_config$loc_center %||% 1L)
  std_err <- as.integer(limit_config$std_err %||% 3L)
  n_mr <- as.integer(limit_config$mr_span %||% 2L)
  x_bar <- if (loc_center == 2L) stats::median(values, na.rm = TRUE) else mean(values, na.rm = TRUE)

  # Legacy PPA UI encoding (choice_x_limits 1–4): option 3 uses c4 with MR span.
  raw_loc <- as.integer(limit_config$loc_lim %||% 7L)
  if (raw_loc %in% 1:4) {
    out <- calculate_x_limits(raw_loc, x_bar, stat, n_mr, std_err, R)
    out$method <- names(choice_x_limits)[raw_loc]
    out$stat <- stat
    return(out)
  }

  if (loc_lim %in% c(6L, 7L)) {
    d <- if (loc_lim == 6L) {
      spc.constant.calculation.d2(sample.size = n_mr)
    } else {
      spc.constant.calculation.d4(sample.size = n_mr)
    }
    loc_const <- std_err / d
    return(list(
      UCL = ro(x_bar + loc_const * stat, R),
      LCL = ro(x_bar - loc_const * stat, R),
      centerline = ro(x_bar, R),
      sig_est = ro(stat / d, R),
      method = ppa_loc_lim_label(loc_lim),
      stat = stat
    ))
  }
  if (loc_lim == 8L) {
    return(list(
      UCL = ro(x_bar + std_err * stat, R),
      LCL = ro(x_bar - std_err * stat, R),
      centerline = ro(x_bar, R),
      sig_est = ro(stat, R),
      method = ppa_loc_lim_label(loc_lim),
      stat = stat
    ))
  }
  if (loc_lim == 9L) {
    out <- calculate_x_limits(4L, x_bar, stat, n_mr, std_err, R)
    out$method <- ppa_loc_lim_label(loc_lim)
    out$stat <- stat
    return(out)
  }
  NULL
}

ppa_attach_dispersion_series <- function(dispersion) {
  if (is.null(dispersion)) {
    return(NULL)
  }
  if ((is.null(dispersion$values) || length(dispersion$values) == 0) &&
      !is.null(dispersion$mr)) {
    dispersion$values <- dispersion$mr
    dispersion$y_label <- dispersion$y_label %||% "Moving Range"
  }
  dispersion
}

ppa_control_chart_has_dispersion <- function(control_chart) {
  if (is.null(control_chart) || is.null(control_chart$dispersion)) {
    return(FALSE)
  }
  disp <- ppa_attach_dispersion_series(control_chart$dispersion)
  !is.null(disp$values) && length(disp$values) > 0
}

ppa_compute_mr_dispersion_limits <- function(values, limit_config, R = 4L) {
  if (!exists("c6", mode = "function")) {
    stop("spc_limit_calcs.R must be sourced before MR dispersion limits", call. = FALSE)
  }
  disp_lim <- as.integer(limit_config$disp_lim %||% 7L)
  span <- as.integer(limit_config$mr_span %||% 2L)
  if (disp_lim == 12L) {
    custom <- limit_config$custom_disp
    if (is.null(custom)) {
      return(NULL)
    }
    return(ppa_attach_dispersion_series(list(
      UCL = as.numeric(custom$upper),
      LCL = as.numeric(custom$lower),
      centerline = as.numeric(custom$center),
      stat = NA_real_,
      method = "Custom",
      mr = ppa_moving_range(as.numeric(values), span = span)
    )))
  }
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (length(values) < 2) {
    return(NULL)
  }
  std_err <- as.integer(limit_config$std_err %||% 3L)
  disp_center <- as.integer(limit_config$disp_center %||% 2L)
  mr <- ppa_moving_range(values, span = span)
  stat <- ppa_mr_dispersion_stat(values, limit_config)
  if (is.na(stat)) {
    return(NULL)
  }
  center <- if (disp_center == 2L) median(na.omit(mr)) else mean(na.omit(mr))

  if (disp_lim == 6L) {
    d_low <- spc.constant.calculation.D3(sample.size = span, n.sigma = std_err)
    d_up <- spc.constant.calculation.D4(sample.size = span, n.sigma = std_err)
    disp_low <- stat * d_low + center
    disp_up <- stat * d_up - center
    lcl <- center - disp_low
    if (!is.na(lcl) && lcl < 0) {
      lcl <- 0
    }
    return(ppa_attach_dispersion_series(list(
      UCL = ro(center + disp_up, R),
      LCL = ro(lcl, R),
      centerline = ro(center, R),
      stat = stat,
      method = ppa_disp_lim_label(disp_lim),
      mr = mr
    )))
  }
  if (disp_lim == 7L) {
    d_low <- spc.constant.calculation.D5(sample.size = span, n.sigma = std_err)
    d_up <- spc.constant.calculation.D6(sample.size = span, n.sigma = std_err)
    disp_low <- stat * d_low + center
    disp_up <- stat * d_up - center
    lcl <- center - disp_low
    if (!is.na(lcl) && lcl < 0) {
      lcl <- 0
    }
    return(ppa_attach_dispersion_series(list(
      UCL = ro(center + disp_up, R),
      LCL = ro(lcl, R),
      centerline = ro(center, R),
      stat = stat,
      method = ppa_disp_lim_label(disp_lim),
      mr = mr
    )))
  }
  if (disp_lim == 8L) {
    mr_bar <- stat * spc.constant.calculation.d2(sample.size = span)
    D3 <- spc.constant.calculation.D3(sample.size = span, n.sigma = std_err)
    D4 <- spc.constant.calculation.D4(sample.size = span, n.sigma = std_err)
    disp_low <- mr_bar * D3 + center
    disp_up <- mr_bar * D4 - center
    lcl <- center - disp_low
    if (!is.na(lcl) && lcl < 0) {
      lcl <- 0
    }
    return(ppa_attach_dispersion_series(list(
      UCL = ro(center + disp_up, R),
      LCL = ro(lcl, R),
      centerline = ro(center, R),
      stat = stat,
      method = ppa_disp_lim_label(disp_lim),
      mr = mr
    )))
  }
  if (disp_lim == 9L) {
    mr_bar <- stat * spc.constant.calculation.d2(sample.size = span)
    D3 <- spc.constant.calculation.D3(sample.size = span, n.sigma = std_err)
    D4 <- spc.constant.calculation.D4(sample.size = span, n.sigma = std_err)
    return(ppa_attach_dispersion_series(list(
      UCL = ro(D4 * mr_bar, R),
      LCL = ro(D3 * mr_bar, R),
      centerline = ro(mr_bar, R),
      stat = stat,
      method = ppa_disp_lim_label(disp_lim),
      mr = mr
    )))
  }
  NULL
}

ppa_ooc_rule_label <- function(rule_name) {
  switch(
    rule_name,
    "outside.limits" = "Outside control limits",
    "runs" = "Run on one side of centerline",
    "trends" = "Trend",
    "alternating" = "Alternating pattern",
    "zone.a" = "Zone A or beyond",
    "consecutive.zone.c" = "Consecutive points in zone C",
    "consecutive.zone.ab" = "Consecutive points beyond zone B",
    rule_name
  )
}

ppa_location_ooc_reason <- function(violations, index) {
  if (is.null(violations) || is.null(violations$rule.results)) {
    return("")
  }
  hits <- names(violations$rule.results)[
    vapply(violations$rule.results, function(r) isTRUE(r[index]), logical(1))
  ]
  if (length(hits) == 0) {
    return("")
  }
  paste(vapply(hits, ppa_ooc_rule_label, character(1)), collapse = "; ")
}

ppa_dispersion_series_for_ooc <- function(dispersion) {
  if (is.null(dispersion)) {
    return(NULL)
  }
  if (!is.null(dispersion$values) && length(dispersion$values) > 0) {
    return(as.numeric(dispersion$values))
  }
  if (!is.null(dispersion$mr) && length(dispersion$mr) > 0) {
    return(as.numeric(dispersion$mr))
  }
  NULL
}

ppa_dispersion_ooc_reason <- function(dispersion, index) {
  label <- tolower(dispersion$y_label %||% "dispersion")
  series <- ppa_dispersion_series_for_ooc(dispersion)
  if (is.null(series) || index < 1L || index > length(series)) {
    return(paste(label, "out of control"))
  }
  val <- series[[index]]
  if (!is.finite(val)) {
    return(paste(label, "out of control"))
  }
  ucl <- dispersion$UCL
  lcl <- dispersion$LCL
  if (length(ucl) > 1L) ucl <- ucl[[min(index, length(ucl))]]
  if (length(lcl) > 1L) lcl <- lcl[[min(index, length(lcl))]]
  if (is.finite(ucl) && val > ucl) {
    return(paste(label, "above UCL"))
  }
  if (is.finite(lcl) && val < lcl) {
    return(paste(label, "below LCL"))
  }
  paste(label, "out of control")
}

ppa_mr_ooc_reason <- function(dispersion, index) {
  # Back-compat wrapper for means-chart MR hover wording.
  if (is.null(dispersion)) {
    return("Moving range out of control")
  }
  if (is.null(dispersion$y_label) && !is.null(dispersion$mr)) {
    dispersion$y_label <- "Moving range"
  }
  ppa_dispersion_ooc_reason(dispersion, index)
}

ppa_point_ooc_reason <- function(control_chart, index) {
  if (is.null(control_chart)) {
    return("")
  }
  is_xbar <- identical(control_chart$chart_type, "xbar")
  is_replicate <- identical(control_chart$chart_type, "replicate")
  loc_flag <- isTRUE(control_chart$location_ooc[index])
  # Means-chart hover: Case 3 uses background MR-of-means, not within-row dispersion OOC.
  mr_means_flag <- if (is_replicate) {
    isTRUE((control_chart$mr_means_ooc %||% control_chart$mr_point_ooc)[index])
  } else {
    FALSE
  }
  disp_flag <- if (is_replicate) {
    mr_means_flag
  } else {
    isTRUE(control_chart$mr_ooc[index])
  }
  if (!loc_flag && !disp_flag) {
    return("")
  }
  parts <- character(0)
  if (loc_flag) {
    loc_reason <- ppa_location_ooc_reason(control_chart$location_violations, index)
    if (nzchar(loc_reason)) {
      parts <- c(parts, loc_reason)
    }
  }
  if (disp_flag && !is_xbar) {
    if (is_replicate) {
      parts <- c(parts, ppa_mr_ooc_reason(control_chart$mr_means_dispersion, index))
    } else {
      mr_point_ooc <- control_chart$mr_point_ooc %||% ppa_mr_ooc_on_individuals(control_chart$mr_ooc)
      if (isTRUE(mr_point_ooc[index])) {
        parts <- c(parts, ppa_mr_ooc_reason(control_chart$dispersion, index))
      }
    }
  }
  paste(parts, collapse = "; ")
}

ppa_xbar_location_stat <- function(pts, loc_lim) {
  switch(
    as.character(as.integer(loc_lim)),
    "1" = mean(pts$range, na.rm = TRUE),
    "2" = stats::median(pts$range, na.rm = TRUE),
    "3" = mean(pts$sd, na.rm = TRUE),
    "4" = stats::median(pts$sd, na.rm = TRUE),
    "5" = mean(pts$var, na.rm = TRUE),
    stats::median(pts$sd, na.rm = TRUE)
  )
}

ppa_xbar_location_limits_per_point <- function(n, loc_lim, stat, centerline, std_err, R = 4L) {
  n <- as.numeric(n)
  loc_lim <- as.integer(loc_lim)
  std_err <- as.numeric(std_err)
  ucl <- rep(NA_real_, length(n))
  lcl <- rep(NA_real_, length(n))
  for (i in seq_along(n)) {
    ni <- n[[i]]
    if (loc_lim == 1L) {
      a <- spc.constant.calculation.A2(sample.size = ni, n.sigma = std_err)
      ucl[[i]] <- centerline + a * stat
      lcl[[i]] <- centerline - a * stat
    } else if (loc_lim == 2L) {
      a <- spc.constant.calculation.A4(sample.size = ni, n.sigma = std_err)
      ucl[[i]] <- centerline + a * stat
      lcl[[i]] <- centerline - a * stat
    } else if (loc_lim == 3L) {
      a <- spc.constant.calculation.A3(sample.size = ni, n.sigma = std_err)
      ucl[[i]] <- centerline + a * stat
      lcl[[i]] <- centerline - a * stat
    } else if (loc_lim == 4L) {
      loc_const <- std_err / (c6(ni) * sqrt(ni))
      ucl[[i]] <- centerline + loc_const * stat
      lcl[[i]] <- centerline - loc_const * stat
    } else {
      lim <- calculate_x_bar_limits(loc_lim, centerline, stat, ni, std_err, R)
      ucl[[i]] <- lim$UCL
      lcl[[i]] <- lim$LCL
    }
  }
  list(
    UCL = ucl,
    LCL = lcl,
    centerline = rep(centerline, length(n))
  )
}

ppa_spc_dispersion_limits <- function(pts, limit_config, R = 4L) {
  disp_type <- as.integer(limit_config$disp_type %||% 1L)
  disp_lim <- as.integer(limit_config$disp_lim %||% 1L)
  disp_center <- as.integer(limit_config$disp_center %||% 2L)
  std_err <- as.integer(limit_config$std_err %||% 3L)
  n_vec <- pts$n
  values <- switch(
    as.character(disp_type),
    "1" = pts$range,
    "2" = pts$sd,
    "3" = pts$var,
    pts$range
  )
  n_bar <- mean(n_vec)

  if (disp_type == 2L && disp_lim == 4L) {
    stat_group <- stats::median(pts$sd, na.rm = TRUE)
    sig_est <- stat_group / c6(n_bar)
    centerline <- if (disp_center == 2L) stats::median(pts$sd, na.rm = TRUE) else mean(pts$sd, na.rm = TRUE)
    ucl <- vapply(n_vec, function(ni) {
      mean_est <- sig_est * spc.constant.calculation.c4(sample.size = ni)
      spc.constant.calculation.B4(sample.size = ni, n.sigma = std_err) * mean_est
    }, numeric(1))
    lcl <- vapply(n_vec, function(ni) {
      mean_est <- sig_est * spc.constant.calculation.c4(sample.size = ni)
      spc.constant.calculation.B3(sample.size = ni, n.sigma = std_err) * mean_est
    }, numeric(1))
    method <- "Median Standard Deviation"
    y_label <- "Std. Dev"
  } else if (disp_type == 2L && disp_lim == 3L) {
    stat_group <- mean(pts$sd, na.rm = TRUE)
    sig_est <- stat_group / spc.constant.calculation.c4(sample.size = n_bar)
    centerline <- if (disp_center == 2L) stats::median(pts$sd, na.rm = TRUE) else mean(pts$sd, na.rm = TRUE)
    ucl <- vapply(n_vec, function(ni) {
      mean_est <- sig_est * spc.constant.calculation.c4(sample.size = ni)
      spc.constant.calculation.B4(sample.size = ni, n.sigma = std_err) * mean_est
    }, numeric(1))
    lcl <- vapply(n_vec, function(ni) {
      mean_est <- sig_est * spc.constant.calculation.c4(sample.size = ni)
      spc.constant.calculation.B3(sample.size = ni, n.sigma = std_err) * mean_est
    }, numeric(1))
    method <- "Average Standard Deviation"
    y_label <- "Std. Dev"
  } else if (disp_type == 1L && disp_lim %in% c(1L, 2L)) {
    stat_group <- if (disp_lim == 2L) stats::median(pts$range, na.rm = TRUE) else mean(pts$range, na.rm = TRUE)
    centerline <- if (disp_center == 2L) stats::median(pts$range, na.rm = TRUE) else mean(pts$range, na.rm = TRUE)
    if (disp_lim == 2L) {
      ucl <- vapply(n_vec, function(ni) {
        spc.constant.calculation.D6(sample.size = ni, n.sigma = std_err) * stat_group
      }, numeric(1))
      lcl <- vapply(n_vec, function(ni) {
        spc.constant.calculation.D5(sample.size = ni, n.sigma = std_err) * stat_group
      }, numeric(1))
      method <- "Median Range"
    } else {
      ucl <- vapply(n_vec, function(ni) {
        spc.constant.calculation.D4(sample.size = ni, n.sigma = std_err) * stat_group
      }, numeric(1))
      lcl <- vapply(n_vec, function(ni) {
        spc.constant.calculation.D3(sample.size = ni, n.sigma = std_err) * stat_group
      }, numeric(1))
      method <- "Average Range"
    }
    y_label <- "Range"
  } else {
    return(ppa_compute_subgroup_dispersion_chart(pts, limit_config, R))
  }

  list(
    UCL = ro(ucl, R),
    LCL = ro(lcl, R),
    centerline = ro(centerline, R),
    stat = stat_group,
    method = method,
    values = values,
    y_label = y_label
  )
}

ppa_summarize_limits <- function(per_point, R = 4L) {
  list(
    UCL = ro(per_point$UCL[[1]], R),
    LCL = ro(per_point$LCL[[1]], R),
    centerline = ro(per_point$centerline[[1]], R)
  )
}

ppa_point_ooc_reasons <- function(control_chart) {
  if (is.null(control_chart)) {
    return(character(0))
  }
  n <- length(control_chart$point_ooc)
  vapply(seq_len(n), function(i) ppa_point_ooc_reason(control_chart, i), character(1))
}

ppa_evaluate_location_ooc <- function(values, limits, return_violations = FALSE) {
  empty <- rep(FALSE, length(values))
  if (is.null(limits) || is.null(values)) {
    if (isTRUE(return_violations)) {
      return(NULL)
    }
    return(empty)
  }
  if (!exists("spc.controlviolation.evaluate.rules", mode = "function")) {
    stop("spc.controlviolation.evaluate.rules is not available; source spc_zones_classify_override.R", call. = FALSE)
  }
  values <- as.numeric(values)
  n <- length(values)
  rules <- spc.rulesets.outside.limits()
  ucl <- limits$UCL
  lcl <- limits$LCL
  cl <- limits$centerline
  if (length(ucl) == 1) ucl <- rep(ucl, n)
  if (length(lcl) == 1) lcl <- rep(lcl, n)
  if (length(cl) == 1) cl <- rep(cl, n)
  vio <- spc.controlviolation.evaluate.rules(
    control.rules = rules,
    chart.series = values,
    center.line = cl,
    control.limits.ucl = ucl,
    control.limits.lcl = lcl,
    zone.a.upper = ucl,
    zone.ab.upper = ucl,
    zone.bc.upper = cl,
    zone.a.lower = lcl,
    zone.ab.lower = lcl,
    zone.bc.lower = cl
  )
  if (isTRUE(return_violations)) {
    return(vio)
  }
  as.logical(vio$rule.results[["outside.limits"]])
}

ppa_evaluate_mr_ooc <- function(mr, limits) {
  if (is.null(limits) || is.null(mr)) {
    return(rep(FALSE, length(mr)))
  }
  mr <- as.numeric(mr)
  ucl <- limits$UCL
  lcl <- limits$LCL
  n <- length(mr)
  if (length(ucl) == 1) ucl <- rep(ucl, n)
  if (length(lcl) == 1) lcl <- rep(lcl, n)
  ok <- is.finite(mr)
  ooc <- rep(FALSE, n)
  if (!any(ok)) {
    return(ooc)
  }
  ooc[ok] <- (is.finite(ucl[ok]) & mr[ok] > ucl[ok]) |
    (is.finite(lcl[ok]) & mr[ok] < lcl[ok])
  ooc
}

ppa_limit_scalar <- function(x) {
  if (length(x) == 0) {
    return(NA_real_)
  }
  if (length(x) == 1) {
    return(x[[1]])
  }
  x[[1]]
}

#' Align MR out-of-control flags to individual chart indices.
ppa_mr_ooc_on_individuals <- function(mr_ooc) {
  mr_ooc <- as.logical(mr_ooc)
  n <- length(mr_ooc)
  if (n == 0) {
    return(logical(0))
  }
  out <- rep(FALSE, n)
  if (n > 1) {
    out[seq(2, n)] <- mr_ooc[seq(2, n)]
  }
  out
}

ppa_compute_replicate_dispersion_limits <- function(replicate_matrix, limit_config, R = 4L) {
  if (!exists("ppa_estimate_replicates_sigma", mode = "function")) {
    source("modules/statistical/spc/utils/spc_sigma_from_limits.R", local = FALSE)
  }
  if (!exists("calculate_r_limits", mode = "function")) {
    source("modules/statistical/spc/server/spc_limits_server.R", local = FALSE)
  }
  # calculate_*_limits may call withMathJax for UI labels; allow headless use.
  if (!exists("withMathJax", mode = "function", envir = .GlobalEnv)) {
    assign("withMathJax", function(x) x, envir = .GlobalEnv)
  }
  replicate_matrix <- as.matrix(replicate_matrix)
  disp_type <- as.integer(limit_config$disp_type %||% 1L)
  if (disp_type == 4L) {
    disp_type <- 1L
  }
  disp_lim <- as.integer(limit_config$disp_lim %||% 1L)
  std_err <- as.integer(limit_config$std_err %||% 3L)
  row_stats <- ppa_replicate_row_stats(replicate_matrix)
  n_rep <- row_stats$n_rep
  values <- switch(
    as.character(disp_type),
    "1" = row_stats$range,
    "2" = row_stats$sd,
    "3" = row_stats$var,
    row_stats$range
  )
  y_label <- switch(
    as.character(disp_type),
    "1" = "Range",
    "2" = "Std. Dev",
    "3" = "Variance",
    "Range"
  )

  est <- ppa_estimate_replicates_sigma(replicate_matrix, limit_config)
  lim_select <- if (disp_lim == 9L) 6L else as.integer(disp_lim)
  raw_stat <- if (disp_lim == 9L) {
    as.numeric(limit_config$known_sigma)
  } else {
    est$stat
  }
  if (!is.finite(raw_stat)) {
    return(NULL)
  }

  if (disp_type == 1L) {
    lim <- calculate_r_limits(lim_select, raw_stat, n_rep, std_err, R)
  } else if (disp_type == 2L) {
    lim <- calculate_s_limits(lim_select, raw_stat, n_rep, std_err, R)
  } else {
    sig_est <- est$sigma
    mean_est <- sig_est^2
    p_low <- stats::pnorm(-std_err, 0, 1)
    p_high <- stats::pnorm(std_err, 0, 1)
    df <- max(n_rep - 1, 1)
    factor_low <- stats::qchisq(p = p_low, df = df) / df
    factor_up <- stats::qchisq(p = p_high, df = df) / df
    lim <- list(
      UCL = ro(factor_up * mean_est, R),
      LCL = ro(max(0, factor_low * mean_est), R),
      centerline = ro(mean_est, R),
      sig_est = ro(sig_est, R)
    )
  }

  list(
    UCL = lim$UCL,
    LCL = lim$LCL,
    centerline = lim$centerline,
    stat = raw_stat,
    method = est$method,
    values = values,
    y_label = y_label,
    sig_est = lim$sig_est
  )
}

ppa_compute_replicate_control_chart <- function(prepared, limit_config, R = 4L) {
  values <- as.numeric(prepared$response)
  replicate_matrix <- attr(prepared, "replicate_matrix")
  location <- ppa_compute_individuals_location_limits(values, limit_config, R)
  dispersion <- if (!is.null(replicate_matrix)) {
    ppa_compute_replicate_dispersion_limits(replicate_matrix, limit_config, R)
  } else {
    NULL
  }

  # Background MR of means: flag OOC on the means chart without a dedicated MR chart.
  mr_cfg <- modifyList(limit_config, list(
    disp_lim = 7L,
    disp_type = 4L,
    disp_center = as.integer(limit_config$loc_center %||% 2L)
  ))
  mr_means_dispersion <- ppa_compute_mr_dispersion_limits(values, mr_cfg, R)
  mr_means_ooc_raw <- if (!is.null(mr_means_dispersion$mr)) {
    ppa_evaluate_mr_ooc(mr_means_dispersion$mr, mr_means_dispersion)
  } else {
    rep(FALSE, length(values))
  }
  mr_means_ooc <- ppa_mr_ooc_on_individuals(mr_means_ooc_raw)

  loc_vio <- ppa_evaluate_location_ooc(values, location, return_violations = TRUE)
  loc_ooc <- as.logical(loc_vio$rule.results[["outside.limits"]])
  disp_ooc <- if (!is.null(dispersion) && !is.null(dispersion$values)) {
    ppa_evaluate_mr_ooc(dispersion$values, dispersion)
  } else {
    rep(FALSE, length(values))
  }

  list(
    location = location,
    dispersion = dispersion,
    location_violations = loc_vio,
    location_ooc = loc_ooc,
    mr_ooc = disp_ooc,
    mr_point_ooc = mr_means_ooc,
    mr_means_ooc = mr_means_ooc,
    mr_means_dispersion = mr_means_dispersion,
    point_ooc = loc_ooc | mr_means_ooc,
    chart_type = "replicate"
  )
}

ppa_compute_control_chart <- function(prepared, limit_config, R = 4L) {
  shape <- limit_config$data_shape %||% NULL
  if (is.null(shape) && !is.null(attr(prepared, "replicate_matrix"))) {
    shape <- "replicate"
  }
  if (
    identical(shape, "replicate") ||
      (!isTRUE(limit_config$ind_or_mean) && !is.null(attr(prepared, "replicate_matrix")))
  ) {
    return(ppa_compute_replicate_control_chart(prepared, limit_config, R))
  }
  values <- as.numeric(prepared$response)
  if (isTRUE(limit_config$ind_or_mean)) {
    return(ppa_compute_xbar_control_chart(prepared, limit_config, R))
  }
  location <- ppa_compute_individuals_location_limits(values, limit_config, R)
  dispersion <- ppa_compute_mr_dispersion_limits(values, limit_config, R)
  loc_vio <- ppa_evaluate_location_ooc(values, location, return_violations = TRUE)
  loc_ooc <- as.logical(loc_vio$rule.results[["outside.limits"]])
  mr_ooc <- if (!is.null(dispersion$mr)) {
    ppa_evaluate_mr_ooc(dispersion$mr, dispersion)
  } else {
    rep(FALSE, length(values))
  }
  mr_point_ooc <- ppa_mr_ooc_on_individuals(mr_ooc)
  list(
    location = location,
    dispersion = dispersion,
    location_violations = loc_vio,
    location_ooc = loc_ooc,
    mr_ooc = mr_ooc,
    mr_point_ooc = mr_point_ooc,
    point_ooc = loc_ooc | mr_point_ooc,
    chart_type = "individuals"
  )
}

ppa_compute_xbar_control_chart <- function(prepared, limit_config, R = 4L) {
  pts <- ppa_build_subgroup_points(prepared)
  if (is.null(pts) || nrow(pts) == 0) {
    return(NULL)
  }
  loc_center <- as.integer(limit_config$loc_center %||% 1L)
  std_err <- as.integer(limit_config$std_err %||% 3L)
  loc_lim <- as.integer(limit_config$loc_lim %||% 1L)
  vals <- as.numeric(prepared$response)
  centerline <- if (loc_center == 2L) {
    stats::median(pts$mean, na.rm = TRUE)
  } else {
    mean(vals, na.rm = TRUE)
  }
  stat <- ppa_xbar_location_stat(pts, loc_lim)
  per_point <- ppa_xbar_location_limits_per_point(pts$n, loc_lim, stat, centerline, std_err, R)
  location <- ppa_summarize_limits(per_point, R)
  location$method <- "X-bar"
  location$stat <- ro(stat, R)
  loc_vio <- ppa_evaluate_location_ooc(pts$mean, per_point, return_violations = TRUE)
  loc_ooc <- as.logical(loc_vio$rule.results[["outside.limits"]])

  dispersion <- ppa_spc_dispersion_limits(pts, limit_config, R)
  disp_ooc <- if (!is.null(dispersion$values)) {
    ppa_evaluate_mr_ooc(dispersion$values, dispersion)
  } else {
    rep(FALSE, nrow(pts))
  }

  list(
    location = location,
    dispersion = dispersion,
    location_per_point = per_point,
    location_violations = loc_vio,
    location_ooc = loc_ooc,
    mr_ooc = disp_ooc,
    mr_point_ooc = disp_ooc,
    point_ooc = loc_ooc,
    chart_type = "xbar",
    subgroup_means = pts$mean,
    subgroup_points = pts
  )
}

ppa_compute_subgroup_dispersion_chart <- function(pts, limit_config, R = 4L) {
  if (!exists("calculate_r_limits", mode = "function")) {
    stop("calculate_r_limits is not available; source spc_limits_server.R", call. = FALSE)
  }
  disp_type <- as.integer(limit_config$disp_type %||% 1L)
  disp_lim <- as.integer(limit_config$disp_lim %||% 1L)
  std_err <- as.integer(limit_config$std_err %||% 3L)
  n_bar <- mean(pts$n)

  if (disp_type == 1L) {
    values <- pts$range
    stat <- switch(
      as.character(disp_lim),
      "1" = mean(values, na.rm = TRUE),
      "2" = median(values, na.rm = TRUE),
      "3" = mean(pts$sd, na.rm = TRUE),
      "4" = median(pts$sd, na.rm = TRUE),
      median(values, na.rm = TRUE)
    )
    lim <- calculate_r_limits(min(disp_lim, 4L), stat, n_bar, std_err, R)
    method <- names(choice_r_limits2)[match(disp_lim, choice_r_limits2)]
    return(list(
      UCL = lim$UCL,
      LCL = lim$LCL,
      centerline = lim$centerline,
      stat = stat,
      method = if (is.na(method)) "Range" else method,
      values = values,
      y_label = "Range"
    ))
  }

  if (disp_type == 2L) {
    values <- pts$sd
    stat <- switch(
      as.character(disp_lim),
      "1" = mean(values, na.rm = TRUE),
      "2" = median(values, na.rm = TRUE),
      "3" = mean(values, na.rm = TRUE),
      "4" = median(values, na.rm = TRUE),
      median(values, na.rm = TRUE)
    )
    lim <- calculate_s_limits(min(disp_lim, 4L), stat, n_bar, std_err, R)
    method <- names(choice_r_limits2)[match(disp_lim, choice_r_limits2)]
    return(list(
      UCL = lim$UCL,
      LCL = lim$LCL,
      centerline = lim$centerline,
      stat = stat,
      method = if (is.na(method)) "Std. Dev" else method,
      values = values,
      y_label = "Std. Dev"
    ))
  }

  values <- pts$var
  stat <- mean(values, na.rm = TRUE)
  if (!exists("calculate_var_limits", mode = "function")) {
    return(NULL)
  }
  lim <- calculate_var_limits(min(disp_lim, 5L), stat, n_bar, std_err, R)
  list(
    UCL = lim$UCL,
    LCL = lim$LCL,
    centerline = lim$centerline,
    stat = stat,
    method = "Variance",
    values = values,
    y_label = "Variance"
  )
}

ppa_format_control_limits_html <- function(chart, R = 4) {
  if (is.null(chart) || is.null(chart$location)) {
    return("")
  }
  loc <- chart$location
  loc_rows <- sprintf(
    paste0(
      "<tr><td><b>Location (", "%s", ")</b></td><td></td></tr>",
      "<tr><td>UCL</td><td>%s</td></tr>",
      "<tr><td>Centerline</td><td>%s</td></tr>",
      "<tr><td>LCL</td><td>%s</td></tr>"
    ),
    loc$method %||% "Individuals",
    ro(loc$UCL, R),
    ro(loc$centerline, R),
    ro(loc$LCL, R)
  )
  disp_rows <- ""
  if (!is.null(chart$dispersion)) {
    disp <- chart$dispersion
    disp_title <- disp$y_label %||% disp$method %||% "Dispersion"
    disp_rows <- sprintf(
      paste0(
        "<tr><td><b>", "%s", " (", "%s", ")</b></td><td></td></tr>",
        "<tr><td>UCL</td><td>%s</td></tr>",
        "<tr><td>Centerline</td><td>%s</td></tr>",
        "<tr><td>LCL</td><td>%s</td></tr>"
      ),
      disp_title,
      disp$method %||% "",
      ro(ppa_limit_scalar(disp$UCL), R),
      ro(ppa_limit_scalar(disp$centerline), R),
      ro(ppa_limit_scalar(disp$LCL), R)
    )
  }
  sprintf(
    paste0(
      "<h5>Control limits</h5>",
      "<table class='table table-condensed' style='width:auto'>",
      "%s%s",
      "</table>"
    ),
    loc_rows,
    disp_rows
  )
}
