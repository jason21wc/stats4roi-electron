# PPA graphics builders

ppa_stream_axis_label <- function(stream_factors) {
  if (length(stream_factors) == 0) {
    return("Stream")
  }
  paste(stream_factors, collapse = " | ")
}

ppa_run_chart_filter_choices <- function(prepared, stream_factors) {
  choices <- c(`All data` = "all")
  if (length(stream_factors) == 0 || is.null(prepared$stream)) {
    return(choices)
  }
  levels <- ppa_order_stream_levels(levels(prepared$stream))
  c(choices, stats::setNames(levels, levels))
}

ppa_prepared_for_run_chart <- function(prepared, selection) {
  if (is.null(selection) || !nzchar(selection) || identical(selection, "all")) {
    return(prepared)
  }
  idx <- as.character(prepared$stream) == selection
  subset <- prepared[idx, , drop = FALSE]
  rep_mat <- attr(prepared, "replicate_matrix")
  if (!is.null(rep_mat)) {
    attr(subset, "replicate_matrix") <- rep_mat[idx, , drop = FALSE]
    attr(subset, "all_response_values") <- as.vector(t(rep_mat[idx, , drop = FALSE]))
  }
  subset
}

ppa_apply_theme <- function(p, base_size = 11, angle_x = NULL) {
  p <- p + ggplot2::theme_gray(base_size = base_size)
  if (!is.null(angle_x) && angle_x != 0) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angle_x, hjust = 1))
  }
  p
}

ppa_palette_color <- function(colors, index) {
  pal <- colors$palette
  if (is.null(pal) || length(pal) < index) {
    return("#000000")
  }
  unname(pal[[index]])
}

#' Fill colors from the user palette, skipping black and dark gray.
ppa_stacked_bar_fills <- function(palette, n) {
  pal <- unname(palette)
  if (length(pal) == 0) {
    return(rep("#888888", n))
  }
  skip <- tolower(pal) %in% c("#000000", "#9e9e9e")
  pal <- pal[!skip]
  if (length(pal) == 0) {
    pal <- unname(palette)
  }
  if (length(pal) < n) {
    pal <- rep(pal, length.out = n)
  }
  pal[seq_len(n)]
}

#' Spec and target reference lines — matches SPC Variables chart styling.
ppa_add_spec_lines <- function(
  p,
  spec,
  colors,
  orientation = c("vertical", "horizontal"),
  show = TRUE
) {
  if (!isTRUE(show)) {
    return(p)
  }
  orientation <- match.arg(orientation)
  spec_color <- ppa_palette_color(colors, 6)
  # SPC location charts use palette[4] (blue in R4) for the data series; target matches that.
  target_color <- ppa_palette_color(colors, 4)
  if (orientation == "vertical") {
    if (!is.na(spec$lsl)) {
      p <- p + ggplot2::geom_vline(
        xintercept = spec$lsl,
        color = spec_color,
        linetype = "twodash",
        linewidth = 0.7
      )
    }
    if (!is.na(spec$usl)) {
      p <- p + ggplot2::geom_vline(
        xintercept = spec$usl,
        color = spec_color,
        linetype = "twodash",
        linewidth = 0.7
      )
    }
    if (ppa_has_target(spec)) {
      p <- p + ggplot2::geom_vline(
        xintercept = spec$target,
        color = target_color,
        linetype = "solid",
        linewidth = 0.7
      )
    }
  } else {
    if (!is.na(spec$lsl)) {
      p <- p + ggplot2::geom_hline(
        yintercept = spec$lsl,
        color = spec_color,
        linetype = "twodash",
        linewidth = 0.7
      )
    }
    if (!is.na(spec$usl)) {
      p <- p + ggplot2::geom_hline(
        yintercept = spec$usl,
        color = spec_color,
        linetype = "twodash",
        linewidth = 0.7
      )
    }
    if (ppa_has_target(spec)) {
      p <- p + ggplot2::geom_hline(
        yintercept = spec$target,
        color = target_color,
        linetype = "solid",
        linewidth = 0.7
      )
    }
  }
  p
}

#' Control limits — matches SPC Variables centerline and UCL/LCL styling.
ppa_add_control_limits <- function(p, control_limits, colors) {
  if (is.null(control_limits)) {
    return(p)
  }
  control_color <- ppa_palette_color(colors, 2)
  center_color <- ppa_palette_color(colors, 3)
  ucl <- ppa_limit_scalar(control_limits$UCL)
  lcl <- ppa_limit_scalar(control_limits$LCL)
  cl <- ppa_limit_scalar(control_limits$centerline)
  if (!is.na(ucl)) {
    p <- p + ggplot2::geom_hline(
      yintercept = ucl,
      color = control_color,
      linetype = 5
    )
  }
  if (!is.na(lcl)) {
    p <- p + ggplot2::geom_hline(
      yintercept = lcl,
      color = control_color,
      linetype = 5
    )
  }
  if (!is.na(cl)) {
    p <- p + ggplot2::geom_hline(
      yintercept = cl,
      color = center_color,
      linetype = "solid"
    )
  }
  p
}

ppa_plot_histogram <- function(
  x,
  spec,
  colors,
  xlab = "Value",
  title = "Histogram (All)",
  base_size = 11,
  show_spec_limits = TRUE
) {
  df <- data.frame(x = x)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(
      bins = 30,
      fill = unname(colors$col_fill_highlight),
      color = unname(colors$col_plot_line),
      alpha = 0.75
    ) +
    ggplot2::labs(title = title, x = xlab, y = "Count")
  p <- ppa_add_spec_lines(p, spec, colors, "vertical", show = show_spec_limits)
  ppa_apply_theme(p, base_size)
}

ppa_plot_box_by_stream <- function(
  prepared,
  stream_factors,
  spec,
  colors,
  ylab = "Value",
  title = "Box Plot by Stream",
  base_size = 11,
  show_spec_limits = TRUE
) {
  xlab <- ppa_stream_axis_label(stream_factors)
  if (length(stream_factors) == 1 && stream_factors[[1]] %in% names(prepared)) {
    x_vals <- prepared[[stream_factors[[1]]]]
  } else {
    x_vals <- prepared$stream
  }
  x_char <- as.character(x_vals)
  ord <- ppa_order_stream_levels(x_char)
  df <- data.frame(
    response = prepared$response,
    x = factor(x_char, levels = ord),
    stringsAsFactors = FALSE
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = response)) +
    ggplot2::geom_boxplot(fill = unname(colors$col_fill_highlight), outlier.alpha = 0.4) +
    ggplot2::labs(title = title, x = xlab, y = ylab)
  p <- ppa_add_spec_lines(p, spec, colors, "horizontal", show = show_spec_limits)
  ppa_apply_theme(p, base_size, angle_x = 45)
}

ppa_run_chart_hover_data <- function(prepared, control_chart = NULL) {
  is_xbar <- !is.null(control_chart) && identical(control_chart$chart_type, "xbar")
  if (is_xbar) {
    pts <- control_chart$subgroup_points
    per <- control_chart$location_per_point
    df <- data.frame(
      idx = seq_len(nrow(pts)),
      response = pts$mean,
      x_label = as.character(pts$sample_id),
      measure = pts$mean,
      facet = "X-bar",
      stringsAsFactors = FALSE
    )
    if (!is.null(per)) {
      df$UCL <- per$UCL
      df$LCL <- per$LCL
      df$centerline <- per$centerline
    } else if (!is.null(control_chart$location)) {
      df$UCL <- control_chart$location$UCL
      df$LCL <- control_chart$location$LCL
      df$centerline <- control_chart$location$centerline
    } else {
      df$UCL <- NA_real_
      df$LCL <- NA_real_
      df$centerline <- NA_real_
    }
    df$ooc_reason <- vapply(seq_len(nrow(df)), function(i) {
      if (isTRUE(control_chart$location_ooc[i])) {
        ppa_location_ooc_reason(control_chart$location_violations, i)
      } else {
        ""
      }
    }, character(1))
    return(df)
  }
  df <- prepared
  df$idx <- seq_len(nrow(df))
  df$x_label <- as.character(df$sample_id)
  df$measure <- df$response
  df$facet <- "Individuals"
  limits <- if (!is.null(control_chart)) control_chart$location else NULL
  if (!is.null(limits)) {
    df$UCL <- limits$UCL
    df$LCL <- limits$LCL
    df$centerline <- limits$centerline
    df$ooc_reason <- ppa_point_ooc_reasons(control_chart)
  } else {
    df$UCL <- NA_real_
    df$LCL <- NA_real_
    df$centerline <- NA_real_
    df$ooc_reason <- rep("", nrow(df))
  }
  df
}

ppa_plot_run_chart <- function(
  prepared,
  spec,
  colors,
  title = "Run Chart",
  ylab = "Value",
  control_limits = NULL,
  control_chart = NULL,
  base_size = 11,
  show_spec_limits = TRUE
) {
  if (!is.null(control_chart) && identical(control_chart$chart_type, "xbar")) {
    pts <- control_chart$subgroup_points
    df <- data.frame(
      response = pts$mean,
      idx = seq_len(nrow(pts)),
      stringsAsFactors = FALSE
    )
    point_ooc <- control_chart$location_ooc
  } else {
    df <- prepared
    df$idx <- seq_len(nrow(df))
    point_ooc <- if (!is.null(control_chart)) control_chart$point_ooc else NULL
  }
  data_color <- ppa_palette_color(colors, 4)
  ooc_color <- ppa_palette_color(colors, 2)
  limits <- if (!is.null(control_chart)) control_chart$location else control_limits
  p <- ggplot2::ggplot(df, ggplot2::aes(x = idx, y = response)) +
    ggplot2::geom_line(alpha = 0.6, color = data_color) +
    ggplot2::geom_point(size = 1, color = data_color) +
    ggplot2::labs(title = title, x = "Sample", y = ylab)
  p <- ppa_add_spec_lines(p, spec, colors, "horizontal", show = show_spec_limits)
  p <- ppa_add_control_limits(p, limits, colors)
  if (!is.null(point_ooc) && any(point_ooc, na.rm = TRUE)) {
    df$ooc_y <- ifelse(point_ooc, df$response, NA_real_)
    p <- p + ggplot2::geom_point(
      data = df,
      ggplot2::aes(x = idx, y = ooc_y),
      color = ooc_color,
      shape = 8,
      size = 2,
      na.rm = TRUE,
      inherit.aes = FALSE
    )
  }
  ppa_apply_theme(p, base_size)
}

ppa_dispersion_chart_hover_data <- function(prepared, control_chart) {
  if (is.null(control_chart) || is.null(control_chart$dispersion)) {
    return(NULL)
  }
  disp <- control_chart$dispersion
  if (exists("ppa_attach_dispersion_series", mode = "function")) {
    disp <- ppa_attach_dispersion_series(disp)
  } else if ((is.null(disp$values) || length(disp$values) == 0) && !is.null(disp$mr)) {
    disp$values <- disp$mr
    disp$y_label <- disp$y_label %||% "Moving Range"
  }
  values <- disp$values
  if (is.null(values) || length(values) == 0) {
    return(NULL)
  }
  n <- length(values)
  x_label <- if (
    identical(control_chart$chart_type, "xbar") &&
      !is.null(control_chart$subgroup_points)
  ) {
    as.character(control_chart$subgroup_points$sample_id)
  } else if (!is.null(prepared) && !is.null(prepared$sample_id)) {
    as.character(prepared$sample_id)
  } else {
    as.character(seq_len(n))
  }
  if (length(x_label) != n) {
    x_label <- as.character(seq_len(n))
  }
  ucl <- disp$UCL %||% NA_real_
  lcl <- disp$LCL %||% NA_real_
  centerline <- disp$centerline %||% NA_real_
  if (length(ucl) == 1L) ucl <- rep(ucl, n)
  if (length(lcl) == 1L) lcl <- rep(lcl, n)
  if (length(centerline) == 1L) centerline <- rep(centerline, n)
  disp_ooc <- control_chart$mr_ooc %||% rep(FALSE, n)
  if (length(disp_ooc) != n) {
    disp_ooc <- rep(FALSE, n)
  }
  ooc_reason <- vapply(seq_len(n), function(i) {
    if (isTRUE(disp_ooc[[i]])) {
      ppa_dispersion_ooc_reason(disp, i)
    } else {
      ""
    }
  }, character(1))
  data.frame(
    idx = seq_len(n),
    value = as.numeric(values),
    x_label = x_label,
    measure = as.numeric(values),
    facet = disp$y_label %||% "Dispersion",
    UCL = as.numeric(ucl)[seq_len(n)],
    LCL = as.numeric(lcl)[seq_len(n)],
    centerline = as.numeric(centerline)[seq_len(n)],
    ooc_reason = ooc_reason,
    stringsAsFactors = FALSE
  )
}

ppa_plot_dispersion_chart <- function(
  control_chart,
  colors,
  title = NULL,
  base_size = 11
) {
  if (is.null(control_chart) || is.null(control_chart$dispersion)) {
    return(NULL)
  }
  disp <- control_chart$dispersion
  if (exists("ppa_attach_dispersion_series", mode = "function")) {
    disp <- ppa_attach_dispersion_series(disp)
  } else if ((is.null(disp$values) || length(disp$values) == 0) && !is.null(disp$mr)) {
    disp$values <- disp$mr
    disp$y_label <- disp$y_label %||% "Moving Range"
  }
  values <- disp$values
  if (is.null(values) || length(values) == 0) {
    return(NULL)
  }
  df <- data.frame(idx = seq_along(values), value = values)
  ylab <- disp$y_label %||% "Dispersion"
  if (is.null(title)) {
    title <- sprintf("%s Chart", ylab)
  }
  data_color <- ppa_palette_color(colors, 4)
  ooc_color <- ppa_palette_color(colors, 2)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = idx, y = value)) +
    ggplot2::geom_line(alpha = 0.6, color = data_color) +
    ggplot2::geom_point(size = 1, color = data_color) +
    ggplot2::labs(title = title, x = "Sample", y = ylab)
  p <- ppa_add_control_limits(p, disp, colors)
  disp_ooc <- control_chart$mr_ooc
  if (!is.null(disp_ooc) && any(disp_ooc, na.rm = TRUE)) {
    df$ooc_y <- ifelse(disp_ooc, df$value, NA_real_)
    p <- p + ggplot2::geom_point(
      data = df,
      ggplot2::aes(x = idx, y = ooc_y),
      color = ooc_color,
      shape = 8,
      size = 2,
      na.rm = TRUE,
      inherit.aes = FALSE
    )
  }
  ppa_apply_theme(p, base_size)
}

ppa_plot_opportunity_analysis <- function(
  bar_data,
  colors,
  has_target = TRUE,
  base_size = 11
) {
  base_label <- if (has_target) "Ppm" else "Ppk"
  scenario_order <- levels(bar_data$scenario)
  n_scenarios <- length(scenario_order)
  bar_data$scenario <- factor(bar_data$scenario, levels = scenario_order)
  bar_data$segment <- factor(bar_data$segment, levels = c(base_label, "Opportunity"))
  base_rows <- bar_data[bar_data$segment == base_label, , drop = FALSE]
  opp_rows <- bar_data[bar_data$segment == "Opportunity", , drop = FALSE]
  base_rows$y0 <- 0
  base_rows$y1 <- base_rows$value
  opp_rows$y0 <- base_rows$value[match(as.character(opp_rows$scenario), as.character(base_rows$scenario))]
  opp_rows$y1 <- opp_rows$y0 + opp_rows$value
  stack_df <- rbind(base_rows, opp_rows)
  stack_df$x <- as.numeric(stack_df$scenario)
  totals <- aggregate(value ~ scenario, data = bar_data, FUN = sum)
  totals$x <- as.numeric(totals$scenario)
  fills <- ppa_stacked_bar_fills(colors$palette, 2)
  fill_vals <- c(
    stats::setNames(fills[1], base_label),
    stats::setNames(fills[min(2, length(fills))], "Opportunity")
  )
  text_size <- max(2.5, base_size / 3.5)
  p <- ggplot2::ggplot(stack_df) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = x - 0.35,
        xmax = x + 0.35,
        ymin = y0,
        ymax = y1,
        fill = segment
      ),
      color = "black",
      linewidth = 0.3
    ) +
    ggplot2::geom_text(
      data = totals,
      ggplot2::aes(x = x, y = value, label = sprintf("%.2f", value)),
      inherit.aes = FALSE,
      vjust = -0.3,
      size = text_size
    ) +
    ggplot2::scale_fill_manual(values = fill_vals, breaks = c(base_label, "Opportunity")) +
    ggplot2::scale_x_continuous(
      breaks = seq_len(n_scenarios),
      labels = scenario_order,
      limits = c(0.5, n_scenarios + 0.5)
    ) +
    ggplot2::labs(
      title = "Opportunity Analysis",
      subtitle = "Stacked opportunity if each improvement is pursued first (current performance at base)",
      x = NULL,
      y = "Performance",
      fill = NULL
    ) +
    ggplot2::expand_limits(y = max(totals$value) * 1.12)
  ppa_apply_theme(p, base_size)
}

ppa_plot_performance_steps <- function(
  bar_data,
  colors,
  has_target = TRUE,
  base_size = 11
) {
  stage_order <- as.character(bar_data$stage)
  bar_data$stage <- factor(bar_data$stage, levels = stage_order)
  n_stages <- nrow(bar_data)
  fills <- ppa_stacked_bar_fills(colors$palette, n_stages)
  names(fills) <- stage_order
  text_size <- max(2.5, base_size / 3.5)
  p <- ggplot2::ggplot(bar_data, ggplot2::aes(x = stage, y = value, fill = stage)) +
    ggplot2::geom_col(width = 0.7, color = NA) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", value)),
      vjust = -0.3,
      size = text_size
    ) +
    ggplot2::scale_fill_manual(values = fills, guide = "none") +
    ggplot2::labs(
      title = if (has_target) "Process Performance Analysis" else "Process Performance (Ppk)",
      subtitle = "Each bar shows performance if prior improvements are achieved (left to right)",
      x = NULL,
      y = "Performance"
    ) +
    ggplot2::expand_limits(y = max(bar_data$value) * 1.12)
  ppa_apply_theme(p, base_size, angle_x = 25)
}

#' @rdname ppa_plot_performance_steps
ppa_plot_stacked_performance <- ppa_plot_performance_steps

ppa_normalize_hex <- function(cols) {
  cols <- as.character(unname(cols))
  ok <- !is.na(cols) & nzchar(cols)
  out <- rep(NA_character_, length(cols))
  if (any(ok)) {
    out[ok] <- toupper(grDevices::rgb(t(grDevices::col2rgb(cols[ok])), maxColorValue = 255))
  }
  out
}

#' Take up to n unused palette colors (hex-normalized uniqueness).
ppa_take_unused_colors <- function(pool, used, n) {
  if (n <= 0L) {
    return(character(0))
  }
  pool <- unname(as.character(pool))
  used_hex <- unique(stats::na.omit(ppa_normalize_hex(used)))
  nest <- character(0)
  for (col in pool) {
    hx <- ppa_normalize_hex(col)
    if (is.na(hx) || hx %in% used_hex) {
      next
    }
    nest <- c(nest, col)
    used_hex <- c(used_hex, hx)
    if (length(nest) >= n) {
      break
    }
  }
  nest
}

#' Fallback nest colors when the Settings palette is exhausted.
ppa_nested_fallback_colors <- function(n, anchor = "#F18F01") {
  if (n <= 0L) {
    return(character(0))
  }
  # Distinct warm/cool accents that sit apart from typical cool Settings palettes.
  base <- c("#8E44AD", "#D35400", "#1ABC9C", "#9B59B6", "#E67E22", "#2980B9", "#C0392B")
  if (n <= length(base)) {
    return(base[seq_len(n)])
  }
  grDevices::hcl(
    h = seq(15, 345, length.out = n),
    c = 70,
    l = 45
  )
}

#' Shared fill map for variance-component and process-stream charts.
#'
#' Main blocks (Off-target / Potential / Process stream / Time) and measurement
#' sub-segments are reserved first from the Settings palette. Nested process-
#' stream factor colors are then chosen from remaining unused palette colors
#' (same map in both charts). Falls back only after the palette is exhausted.
ppa_variance_fill_map <- function(colors = NULL, hierarchy = character(0)) {
  hierarchy <- as.character(hierarchy)
  pal <- NULL
  if (is.list(colors) && !is.null(colors$palette)) {
    pal <- unname(colors$palette)
  } else if (!is.null(colors)) {
    pal <- unname(colors)
  }
  if (is.null(pal) || length(pal) == 0) {
    pal <- c("#2E86AB", "#28A745", "#F18F01", "#C73E1D", "#6C757D", "#8E44AD", "#16A085")
  }
  usable <- ppa_stacked_bar_fills(pal, length(pal))
  main_keys <- c(
    "Off-target",
    "Potential",
    "Process stream",
    "Time (control)",
    "Measurement error",
    "Potential less measurement"
  )
  main_cols <- ppa_stacked_bar_fills(usable, length(main_keys))
  fills <- stats::setNames(main_cols, main_keys)

  n_h <- length(hierarchy)
  if (n_h > 0L) {
    nest_cols <- ppa_take_unused_colors(usable, fills, n_h)
    if (length(nest_cols) < n_h) {
      nest_cols <- c(
        nest_cols,
        ppa_take_unused_colors(
          ppa_nested_fallback_colors(n_h * 2L, fills[["Process stream"]]),
          c(fills, nest_cols),
          n_h - length(nest_cols)
        )
      )
    }
    if (length(nest_cols) < n_h) {
      # Absolute last resort: generate enough hex colors even if some collide.
      nest_cols <- c(nest_cols, ppa_nested_fallback_colors(n_h - length(nest_cols)))
    }
    fills <- c(fills, stats::setNames(nest_cols[seq_len(n_h)], hierarchy))
  }
  fills
}

#' Variance components chart (horizontal bars; text category order top → bottom).
#'
#' Process-stream / potential sub-stacks use explicit xmin/xmax so left→right
#' order always matches hierarchy / measurement order (not ggplot fill stacking).
ppa_plot_variance_bars <- function(
  bar_data,
  colors = NULL,
  title = "Variance Components",
  base_size = 11
) {
  df <- bar_data$plot
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  hierarchy <- as.character(bar_data$nested_hierarchy %||% character(0))
  category_order <- as.character(
    bar_data$category_order %||% ppa_variance_component_order(
      "Off-target" %in% as.character(df$component)
    )
  )
  # Discrete y: first level at bottom → reverse so text order is top → bottom.
  y_levels <- rev(category_order)
  df$component <- factor(as.character(df$component), levels = y_levels)
  df$fill_key <- as.character(df$segment)

  fill_vals <- ppa_variance_fill_map(colors, hierarchy)
  text_size <- max(2.5, base_size / 3.5)

  p <- ggplot2::ggplot(df) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = as.numeric(component) - 0.35,
        ymax = as.numeric(component) + 0.35,
        fill = fill_key
      ),
      color = "grey20",
      linewidth = 0.2
    ) +
    ggplot2::scale_fill_manual(values = fill_vals, guide = "none", na.translate = FALSE) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(y_levels),
      labels = y_levels,
      limits = c(0.4, length(y_levels) + 0.6)
    )

  # End labels for unstacked main blocks.
  simple <- df[as.character(df$segment) == as.character(df$component), , drop = FALSE]
  if (nrow(simple) > 0) {
    p <- p + ggplot2::geom_text(
      data = simple,
      ggplot2::aes(
        x = xmax,
        y = as.numeric(component),
        label = sprintf("%.1f%%", pct)
      ),
      inherit.aes = FALSE,
      hjust = -0.1,
      size = text_size
    )
  }

  # In-segment labels + total for stacked components.
  stacked <- df[as.character(df$segment) != as.character(df$component), , drop = FALSE]
  if (nrow(stacked) > 0) {
    p <- p + ggplot2::geom_text(
      data = stacked,
      ggplot2::aes(
        x = xmid,
        y = as.numeric(component),
        label = sprintf("%.1f%%", pct)
      ),
      inherit.aes = FALSE,
      hjust = 0.5,
      size = text_size,
      color = "white"
    )
    totals <- unique(stacked[, c("component", "total_pct"), drop = FALSE])
    totals$component <- factor(as.character(totals$component), levels = y_levels)
    p <- p + ggplot2::geom_text(
      data = totals,
      ggplot2::aes(
        x = total_pct,
        y = as.numeric(component),
        label = sprintf("%.1f%%", total_pct)
      ),
      inherit.aes = FALSE,
      hjust = -0.1,
      size = text_size,
      color = "black"
    )
  }

  x_max <- max(df$xmax, na.rm = TRUE)
  p <- p +
    ggplot2::labs(title = title, x = "% of total variance about target", y = NULL) +
    ggplot2::expand_limits(x = x_max * 1.15)
  ppa_apply_theme(p, base_size)
}

#' Process-stream factor breakdown (hierarchy outermost → innermost, top → bottom).
ppa_plot_nested_variance_bars <- function(
  bar_data,
  colors = NULL,
  title = "Process Stream Breakdown",
  base_size = 11
) {
  df <- bar_data$nested
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  hierarchy <- as.character(bar_data$nested_hierarchy %||% as.character(df$component))
  y_levels <- rev(hierarchy)
  df$component <- factor(as.character(df$component), levels = y_levels)
  fill_vals <- ppa_variance_fill_map(colors, hierarchy)
  text_size <- max(2.5, base_size / 3.5)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = pct, y = component, fill = component)) +
    ggplot2::geom_col(orientation = "y", width = 0.7, color = "grey20", linewidth = 0.2) +
    ggplot2::scale_y_discrete(limits = y_levels) +
    ggplot2::scale_fill_manual(values = fill_vals, guide = "none") +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", pct)),
      hjust = -0.1,
      size = text_size
    ) +
    ggplot2::labs(title = title, x = "% of total variance", y = NULL) +
    ggplot2::expand_limits(x = max(df$pct, na.rm = TRUE) * 1.15)
  ppa_apply_theme(p, base_size)
}
