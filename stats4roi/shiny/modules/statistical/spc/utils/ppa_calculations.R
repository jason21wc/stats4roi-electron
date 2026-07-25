# Process Performance Analysis — core calculations

ppa_spec_width <- function(spec) {
  lsl <- spec$lsl
  usl <- spec$usl
  if (!is.na(lsl) && !is.na(usl)) {
    return(usl - lsl)
  }
  if (!is.na(usl) && !is.na(spec$target)) {
    return(2 * abs(usl - spec$target))
  }
  if (!is.na(lsl) && !is.na(spec$target)) {
    return(2 * abs(spec$target - lsl))
  }
  if (!is.na(usl)) {
    return(Inf)
  }
  if (!is.na(lsl)) {
    return(Inf)
  }
  NA_real_
}

ppa_has_target <- function(spec) {
  !is.null(spec$target) && !is.na(spec$target)
}

ppa_pooled_within_stream_variance <- function(x, stream) {
  stream <- as.character(stream)
  split_vals <- split(x, stream)
  ns <- vapply(split_vals, length, numeric(1))
  if (length(ns) == 0 || sum(ns) < 2) {
    return(list(variance = NA_real_, sd = NA_real_))
  }
  vars <- vapply(split_vals, function(v) {
    if (length(v) < 2) return(NA_real_)
    stats::var(v)
  }, numeric(1))
  ok <- !is.na(vars) & ns > 1
  if (!any(ok)) {
    return(list(variance = NA_real_, sd = NA_real_))
  }
  pooled <- sum((ns[ok] - 1) * vars[ok]) / sum(ns[ok] - 1)
  list(variance = pooled, sd = sqrt(pooled))
}

ppa_stream_potential_sd_col_label <- function(limit_config) {
  if (is.numeric(limit_config)) {
    limit_config <- list(disp_lim = as.integer(limit_config))
  }
  if (!is.list(limit_config)) {
    limit_config <- list(disp_lim = 7L)
  }
  if (!exists("spc_sigma_source_rule", mode = "function")) {
    source("modules/statistical/spc/utils/spc_limit_choice_helpers.R", local = FALSE)
  }
  rule <- spc_sigma_source_rule(limit_config)
  lim <- if (identical(rule, "dispersion")) {
    as.integer(limit_config$disp_lim %||% 7L)
  } else {
    as.integer(limit_config$loc_lim %||% if (isTRUE(limit_config$ind_or_mean)) 1L else 7L)
  }
  # "12" retained for dispersion Custom only (location Custom is not offered in PPA).
  ind_map <- c(
    "6" = "Std(AMR)",
    "7" = "Std(MMR)",
    "8" = "Std",
    "9" = "Std(Known)",
    "12" = "Std(Custom)"
  )
  xbar_map <- c(
    "1" = "Std(AvgR)",
    "2" = "Std(MedR)",
    "3" = "Std(AvgS)",
    "4" = "Std(MedS)",
    "5" = "Std(AvgVar)",
    "6" = "Std(AMR)",
    "7" = "Std(MMR)",
    "8" = "Std",
    "9" = "Std(Known)",
    "12" = "Std(Custom)"
  )
  use_xbar_map <- isTRUE(limit_config$ind_or_mean) ||
    (identical(rule, "dispersion") && lim < 6L)
  m <- if (use_xbar_map) xbar_map else ind_map
  label <- m[[as.character(lim)]]
  if (is.null(label) || is.na(label)) "Std(potential)" else label
}

ppa_format_stream_table_display <- function(stream_table, limit_config) {
  if (is.null(stream_table) || nrow(stream_table) == 0) {
    return(NULL)
  }
  ord <- ppa_order_stream_levels(stream_table$stream)
  tbl <- stream_table[match(ord, stream_table$stream), , drop = FALSE]
  pot_lab <- ppa_stream_potential_sd_col_label(limit_config)
  out <- data.frame(
    Group = tbl$stream,
    n = as.integer(tbl$n),
    Mean = tbl$mean,
    Low = tbl$low,
    High = tbl$high,
    Range = tbl$range,
    Std = tbl$std,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  out[[pot_lab]] <- tbl$std_potential
  out
}

#' Short calc tag for Case 3 repeated-measures sigma (footer label).
ppa_replicate_sd_calc_label <- function(limit_config) {
  disp_lim <- as.integer(limit_config$disp_lim %||% 1L)
  switch(
    as.character(disp_lim),
    "1" = "R-bar",
    "2" = "R-tilde",
    "3" = "s-bar",
    "4" = "s-tilde",
    "5" = "Avg s\u00b2",
    "9" = "Known \u03c3",
    "R-bar"
  )
}

ppa_format_stream_table_footer_html <- function(
  sd_within,
  sd_potential,
  limit_config,
  R = 4,
  sd_repeated_measures = NULL
) {
  pot_lab <- ppa_stream_potential_sd_col_label(limit_config)
  if (!exists("ppa_sigma_method_label", mode = "function")) {
    source("modules/statistical/spc/utils/spc_sigma_from_limits.R", local = FALSE)
  }
  method <- ppa_sigma_method_label(limit_config)
  fmt <- function(x) format(round(as.numeric(x), R), nsmall = R, trim = TRUE)
  rep_line <- ""
  if (!is.null(sd_repeated_measures) && is.finite(as.numeric(sd_repeated_measures))) {
    calc_lab <- ppa_replicate_sd_calc_label(limit_config)
    rep_line <- sprintf(
      "<br/><strong>Std Dev of repeated measures (%s)</strong> = %s",
      calc_lab,
      fmt(sd_repeated_measures)
    )
  }
  sprintf(
    paste0(
      "<p style='margin-top:0.75em'><strong>SQRT(MSW)</strong> = Avg Std Dev within = %s",
      "<br/><strong>AVG %s</strong> = Avg Std Dev from %s = %s",
      "%s</p>"
    ),
    fmt(sd_within),
    pot_lab,
    method,
    fmt(sd_potential),
    rep_line
  )
}

ppa_stream_summary <- function(prepared, limit_config) {
  streams <- ppa_order_stream_levels(levels(prepared$stream))
  out <- lapply(streams, function(st) {
    idx <- prepared$stream == st
    vals <- prepared$response[idx]
    sg <- prepared$subgroup_id[idx]
    smp <- prepared$sample_id[idx]
    rep_mat <- attr(prepared, "replicate_matrix")
    if (!is.null(rep_mat)) {
      rep_mat <- rep_mat[idx, , drop = FALSE]
    }
    sig <- spc_estimate_sigma(
      values = vals,
      subgroup = if (all(is.na(sg))) NULL else sg,
      sample_id = smp,
      limit_config = limit_config,
      replicate_matrix = rep_mat
    )
  })
  sigmas <- vapply(out, function(x) x$sigma, numeric(1))
  stats <- vapply(out, function(x) x$stat, numeric(1))
  ns <- vapply(streams, function(st) sum(prepared$stream == st), numeric(1))
  means <- vapply(streams, function(st) mean(prepared$response[prepared$stream == st]), numeric(1))
  sds <- vapply(streams, function(st) stats::sd(prepared$response[prepared$stream == st]), numeric(1))
  lows <- vapply(streams, function(st) min(prepared$response[prepared$stream == st]), numeric(1))
  highs <- vapply(streams, function(st) max(prepared$response[prepared$stream == st]), numeric(1))

  data.frame(
    stream = streams,
    n = ns,
    mean = means,
    low = lows,
    high = highs,
    range = highs - lows,
    std = sds,
    std_potential = sigmas,
    stat_disp = stats,
    stringsAsFactors = FALSE
  )
}

ppa_aggregate_sigma_potential <- function(stream_table) {
  ns <- stream_table$n
  sigs <- stream_table$std_potential
  ok <- !is.na(sigs) & ns > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  sum(ns[ok] * sigs[ok]) / sum(ns[ok])
}

ppa_performance_indices <- function(mean, sd_overall, sd_within_stream, sd_potential, spec) {
  if (!requireNamespace("lolcat", quietly = TRUE)) {
    stop("lolcat required", call. = FALSE)
  }
  lsl <- spec$lsl
  usl <- spec$usl
  target <- spec$target
  has_target <- ppa_has_target(spec)
  one_sided <- (is.na(usl) || is.na(lsl)) && !(is.na(usl) && is.na(lsl))

  cap <- lolcat::spc.capability.summary.normal.simple(
    stat.lsl = lsl,
    stat.target = if (has_target) target else NA,
    stat.usl = usl,
    process.center = mean,
    process.variability.estimate = sd_potential^2,
    process.variability.overall = sd_overall^2,
    process.n.upper = 0,
    process.n.lower = 0,
    process.n = 1
  )

  pp <- cap$value[4]
  ppk <- cap$value[5]
  ppm <- if (has_target) cap$value[6] else NA_real_
  cp_pot <- cap$value[1]

  if (one_sided && (is.na(pp) || is.nan(pp))) {
    pp <- ppk
  }
  if (one_sided && (is.na(cp_pot) || is.nan(cp_pot))) {
    cp_pot <- if (!is.na(lsl) && is.na(usl)) {
      lolcat::spc.capability.cpL.simple(
        lower.specification = lsl,
        process.center = mean,
        process.variability = sd_potential^2,
        n.sigma = 6
      )
    } else {
      lolcat::spc.capability.cpU.simple(
        upper.specification = usl,
        process.center = mean,
        process.variability = sd_potential^2,
        n.sigma = 6
      )
    }
  }

  pp_stream <- NA_real_
  if (!is.na(sd_within_stream) && sd_within_stream > 0) {
    if (!is.na(lsl) && !is.na(usl)) {
      pp_stream <- lolcat::spc.capability.cp.simple(
        lower.specification = lsl,
        upper.specification = usl,
        process.center = mean,
        process.natural.tolerance = 6 * sd_within_stream
      )
    } else if (!is.na(lsl) && is.na(usl)) {
      pp_stream <- lolcat::spc.capability.cpL.simple(
        lower.specification = lsl,
        process.center = mean,
        process.variability = sd_within_stream^2,
        n.sigma = 6
      )
    } else if (is.na(lsl) && !is.na(usl)) {
      pp_stream <- lolcat::spc.capability.cpU.simple(
        upper.specification = usl,
        process.center = mean,
        process.variability = sd_within_stream^2,
        n.sigma = 6
      )
    }
  }

  if (!has_target) {
    ppm <- NA_real_
  }

  list(
    ppm = ppm,
    pp = pp,
    ppk = ppk,
    pp_stream = pp_stream,
    cp_potential = cp_pot,
    cap_summary = cap
  )
}

ppa_diagnostics_pct <- function(mean, spec, stream_means) {
  width <- ppa_spec_width(spec)
  pct_off <- NA_real_
  if (ppa_has_target(spec) && is.finite(width) && width > 0) {
    pct_off <- 100 * abs(mean - spec$target) / width
  }
  pct_stream <- NA_real_
  if (length(stream_means) > 0 && is.finite(width) && width > 0) {
    pct_stream <- 100 * (max(stream_means) - min(stream_means)) / width
  }
  list(pct_off_target = pct_off, pct_stream_loss = pct_stream)
}

ppa_nonconforming <- function(x, spec) {
  above <- if (!is.na(spec$usl)) sum(x > spec$usl) else 0L
  below <- if (!is.na(spec$lsl)) sum(x < spec$lsl) else 0L
  n <- length(x)
  total <- above + below
  ppm <- if (n > 0) 1e6 * total / n else NA_real_
  list(
    n = n,
    above_usl = above,
    below_lsl = below,
    total_out = total,
    ppm = ppm
  )
}

ppa_variance_components <- function(mean, sd_overall, sd_within_stream, sd_potential, spec) {
  s2_overall <- sd_overall^2
  s2_within <- sd_within_stream^2
  s2_pot <- sd_potential^2
  s2_off <- if (ppa_has_target(spec)) (mean - spec$target)^2 else 0
  s2_stream <- max(s2_overall - s2_within, 0)
  s2_time <- max(s2_within - s2_pot, 0)
  flags <- list(time_floored = s2_within < s2_pot)

  if (ppa_has_target(spec)) {
    tau2 <- s2_off + s2_stream + s2_time + s2_pot
    components <- c(
      "Off-target" = s2_off,
      "Process stream" = s2_stream,
      "Time (control)" = s2_time,
      "Potential" = s2_pot
    )
  } else {
    tau2 <- s2_stream + s2_time + s2_pot
    components <- c(
      "Process stream" = s2_stream,
      "Time (control)" = s2_time,
      "Potential" = s2_pot
    )
  }

  pct <- if (tau2 > 0) 100 * components / tau2 else rep(0, length(components))
  list(
    tau2 = tau2,
    components = components,
    pct = pct,
    flags = flags
  )
}

ppa_nested_stream_variance <- function(prepared, stream_factors) {
  if (length(stream_factors) < 2) {
    return(NULL)
  }
  df <- prepared
  levels_list <- list()
  var_total <- stats::var(df$response)

  parent_var <- var_total
  between <- list()
  for (i in seq_along(stream_factors)) {
    cols <- stream_factors[seq_len(i)]
    within_var <- {
      keys <- interaction(df[cols], drop = TRUE)
      ppa_pooled_within_stream_variance(df$response, keys)$variance
    }
    between[[stream_factors[[i]]]] <- max(parent_var - within_var, 0)
    parent_var <- within_var
    levels_list[[i]] <- within_var
  }
  list(between = between, var_within_innermost = parent_var)
}

ppa_measurement_error_variance <- function(
  replicate_matrix,
  n_replicates = NULL,
  limit_config = NULL
) {
  if (is.null(replicate_matrix) || ncol(replicate_matrix) < 2) {
    return(NULL)
  }
  if (!exists("ppa_estimate_replicates_sigma", mode = "function")) {
    source("modules/statistical/spc/utils/spc_sigma_from_limits.R", local = FALSE)
  }
  nr <- ncol(replicate_matrix)
  if (is.null(n_replicates)) n_replicates <- nr
  est <- ppa_estimate_replicates_sigma(replicate_matrix, limit_config)
  s_rep <- est$sigma
  s2_rep <- s_rep^2
  s2_meas <- s2_rep / n_replicates
  list(
    s_replicates = s_rep,
    variance_measurement = s2_meas,
    n_replicates = n_replicates,
    method = est$method,
    stat = est$stat
  )
}

ppa_descriptives_combined <- function(x) {
  list(
    n = length(x),
    mean = mean(x),
    sd = stats::sd(x),
    low = min(x),
    q1 = stats::quantile(x, 0.25),
    median = stats::median(x),
    q3 = stats::quantile(x, 0.75),
    high = max(x),
    skewness = .ppa_skewness(x),
    kurtosis = .ppa_kurtosis(x)
  )
}

.ppa_skewness <- function(x) {
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  s <- stats::sd(x)
  if (s == 0) return(NA_real_)
  mean((x - m)^3) / s^3
}

.ppa_kurtosis <- function(x) {
  n <- length(x)
  if (n < 4) return(NA_real_)
  m <- mean(x)
  s <- stats::sd(x)
  if (s == 0) return(NA_real_)
  mean((x - m)^4) / s^4 - 3
}

ppa_analyze <- function(prepared, spec, limit_config, stream_factors = NULL) {
  x <- prepared$response
  x_desc <- ppa_all_response_values(prepared)
  mean <- mean(x)
  sd_overall <- stats::sd(x)
  pooled <- ppa_pooled_within_stream_variance(x, prepared$stream)
  sd_within <- pooled$sd

  stream_table <- ppa_stream_summary(prepared, limit_config)
  sd_potential <- ppa_aggregate_sigma_potential(stream_table)

  indices <- ppa_performance_indices(mean, sd_overall, sd_within, sd_potential, spec)
  diag <- ppa_diagnostics_pct(mean, spec, stream_table$mean)
  nc <- ppa_nonconforming(x, spec)
  variance <- ppa_variance_components(mean, sd_overall, sd_within, sd_potential, spec)

  nested <- NULL
  if (!is.null(stream_factors) && length(stream_factors) > 1) {
    nested <- ppa_nested_stream_variance(prepared, stream_factors)
    if (!is.null(nested)) {
      tau2 <- variance$tau2
      nested_pct <- if (tau2 > 0) 100 * unlist(nested$between) / tau2 else unlist(nested$between) * 0
      nested$pct <- nested_pct
    }
  }

  measurement <- NULL
  reps_mat <- attr(prepared, "replicate_matrix")
  if (!is.null(reps_mat)) {
    measurement <- ppa_measurement_error_variance(
      reps_mat,
      ncol(reps_mat),
      limit_config = limit_config
    )
    if (!is.null(measurement) && "Potential" %in% names(variance$components)) {
      s2_pot <- variance$components["Potential"]
      tau2 <- variance$tau2
      measurement$variance_potential <- unname(s2_pot)
      measurement$variance_measurement <- unname(measurement$variance_measurement)
      measurement$variance_product <- max(unname(s2_pot) - measurement$variance_measurement, 0)
      if (tau2 > 0) {
        measurement$pct_of_tau2 <- 100 * measurement$variance_measurement / tau2
        measurement$pct_product_of_tau2 <- 100 * measurement$variance_product / tau2
      } else {
        measurement$pct_of_tau2 <- NA_real_
        measurement$pct_product_of_tau2 <- NA_real_
      }
      measurement$pct_of_potential <- if (unname(s2_pot) > 0) {
        100 * measurement$variance_measurement / unname(s2_pot)
      } else {
        NA_real_
      }
    }
  }

  descriptives <- ppa_descriptives_combined(x_desc)

  list(
    descriptives = descriptives,
    stream_table = stream_table,
    indices = indices,
    diagnostics = diag,
    nonconforming = nc,
    variance = variance,
    nested_variance = nested,
    measurement_error = measurement,
    sd_overall = sd_overall,
    sd_within_stream = sd_within,
    sd_potential = sd_potential
  )
}

ppa_capability_index_from_tau2 <- function(tau2_rem, center, spec, use_ppm = TRUE) {
  if (!requireNamespace("lolcat", quietly = TRUE)) {
    stop("lolcat required", call. = FALSE)
  }
  tau2_rem <- max(as.numeric(tau2_rem), 0)
  center <- as.numeric(center)
  target <- spec$target
  s2_off_center <- if (ppa_has_target(spec)) (center - target)^2 else 0
  s2_var <- max(tau2_rem - s2_off_center, 0)
  cap <- lolcat::spc.capability.summary.normal.simple(
    stat.lsl = spec$lsl,
    stat.target = if (ppa_has_target(spec)) target else NA,
    stat.usl = spec$usl,
    process.center = center,
    process.variability.estimate = s2_var,
    process.variability.overall = s2_var,
    process.n.upper = 0,
    process.n.lower = 0,
    process.n = 1
  )
  idx <- if (use_ppm) cap$value[6] else cap$value[5]
  as.numeric(idx)
}

ppa_opportunity_analysis_data <- function(result, spec) {
  variance <- result$variance
  indices <- result$indices
  mean <- result$descriptives$mean
  has_target <- ppa_has_target(spec)
  comps <- variance$components
  tau2 <- variance$tau2
  base_label <- if (has_target) "Ppm" else "Ppk"
  base <- if (has_target) indices$ppm else indices$ppk

  s2_off <- if (has_target) unname(comps["Off-target"]) else 0
  s2_stream <- unname(comps["Process stream"])
  s2_time <- unname(comps["Time (control)"])

  scenarios <- if (has_target) {
    list(
      Target = list(remove = s2_off, center = spec$target),
      Stream = list(remove = s2_stream, center = mean),
      `Target+Stream` = list(remove = s2_off + s2_stream, center = spec$target),
      Control = list(remove = s2_time, center = mean)
    )
  } else {
    list(
      Stream = list(remove = s2_stream, center = mean),
      Control = list(remove = s2_time, center = mean)
    )
  }

  scenario_names <- names(scenarios)
  rows <- lapply(scenario_names, function(nm) {
    sc <- scenarios[[nm]]
    tau2_rem <- max(tau2 - sc$remove, 0)
    new_index <- ppa_capability_index_from_tau2(
      tau2_rem,
      sc$center,
      spec,
      use_ppm = has_target
    )
    opportunity <- max(new_index - base, 0)
    data.frame(
      scenario = nm,
      segment = c(base_label, "Opportunity"),
      value = c(base, opportunity),
      total = new_index,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$scenario <- factor(out$scenario, levels = scenario_names)
  out$segment <- factor(
    out$segment,
    levels = c(base_label, "Opportunity")
  )
  out
}

ppa_performance_step_data <- function(indices, has_target = TRUE) {
  stages <- c(
    if (has_target) "Ppm (Current State)" else "Ppk (Current State)",
    "Pp (First Get On Target)",
    "Pp (stream) (Next Align Streams)",
    "Cp (potential) (Finally Achieve Control)"
  )
  data.frame(
    stage = stages,
    value = c(
      if (has_target) indices$ppm else indices$ppk,
      indices$pp,
      indices$pp_stream,
      indices$cp_potential
    ),
    stringsAsFactors = FALSE
  )
}

#' @rdname ppa_performance_step_data
ppa_stacked_bar_data <- ppa_performance_step_data

#' Category order matching variance-components text (top → bottom on charts).
ppa_variance_component_order <- function(has_off_target = TRUE) {
  if (isTRUE(has_off_target)) {
    c("Off-target", "Potential", "Process stream", "Time (control)")
  } else {
    c("Potential", "Process stream", "Time (control)")
  }
}

#' Build plottable variance-component rows with explicit left→right stack geometry.
ppa_variance_bar_data <- function(variance, nested = NULL, measurement = NULL) {
  comps <- variance$components
  pct <- variance$pct
  has_off_target <- "Off-target" %in% names(comps)
  category_order <- ppa_variance_component_order(has_off_target)
  category_order <- category_order[category_order %in% names(comps)]

  hierarchy <- character(0)
  if (!is.null(nested) && length(nested$between) > 0) {
    hierarchy <- names(nested$between)
  }

  rows <- list()
  for (comp in category_order) {
    if (
      identical(comp, "Potential") &&
        !is.null(measurement) &&
        is.finite(measurement$variance_measurement)
    ) {
      segs <- data.frame(
        segment = c("Measurement error", "Potential less measurement"),
        variance = c(
          measurement$variance_measurement,
          measurement$variance_product
        ),
        pct = c(measurement$pct_of_tau2, measurement$pct_product_of_tau2),
        stringsAsFactors = FALSE
      )
    } else if (identical(comp, "Process stream") && length(hierarchy) > 0) {
      segs <- data.frame(
        segment = hierarchy,
        variance = unname(unlist(nested$between[hierarchy], use.names = FALSE)),
        pct = unname(as.numeric(nested$pct[hierarchy])),
        stringsAsFactors = FALSE
      )
    } else {
      segs <- data.frame(
        segment = comp,
        variance = unname(comps[[comp]]),
        pct = unname(pct[[comp]]),
        stringsAsFactors = FALSE
      )
    }
    segs$component <- comp
    segs$xmin <- c(0, cumsum(segs$pct)[-nrow(segs)])
    segs$xmax <- cumsum(segs$pct)
    segs$xmid <- (segs$xmin + segs$xmax) / 2
    segs$total_pct <- sum(segs$pct)
    rows[[length(rows) + 1L]] <- segs
  }

  plot_df <- do.call(rbind, rows)
  rownames(plot_df) <- NULL
  plot_df$component <- factor(plot_df$component, levels = rev(category_order))

  nested_df <- NULL
  if (length(hierarchy) > 0) {
    nested_df <- data.frame(
      component = hierarchy,
      variance = unname(unlist(nested$between[hierarchy], use.names = FALSE)),
      pct = unname(as.numeric(nested$pct[hierarchy])),
      stringsAsFactors = FALSE
    )
    nested_df$component <- factor(nested_df$component, levels = rev(hierarchy))
  }

  totals <- data.frame(
    component = category_order,
    pct = unname(pct[category_order]),
    stringsAsFactors = FALSE
  )

  list(
    primary = totals,
    nested = nested_df,
    nested_hierarchy = hierarchy,
    plot = plot_df,
    totals = totals,
    category_order = category_order,
    has_measurement = !is.null(measurement) && is.finite(measurement$variance_measurement)
  )
}
