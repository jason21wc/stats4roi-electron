# Distribution Fitting ggplot builders

source("modules/statistical/spc/utils/dfit_constants.R")

dfit_fmt_caption <- function(x, digits = 4L) {
  if (is.null(x) || length(x) == 0L || !is.finite(x)) {
    return("")
  }
  format(round(x, digits), nsmall = min(digits, 6L), trim = TRUE)
}

dfit_fit_caption <- function(fit, decimals = 4L) {
  if (is.null(fit) || as.integer(fit$distribution_id) == 0L) {
    return(NULL)
  }
  label <- fit$distribution_label
  if (is.null(label) || !nzchar(label)) {
    label <- dfit_distribution_label(fit$distribution_id)
  }
  param_bits <- character()
  params <- fit$params
  if (!is.null(params)) {
    fields <- dfit_param_fields(fit$distribution_id)
    for (nm in names(fields)) {
      val <- params[[nm]]
      if (is.null(val)) {
        next
      }
      if (nm == "family") {
        txt <- as.character(val)
      } else if (is.finite(val)) {
        txt <- dfit_fmt_caption(val, decimals)
      } else {
        next
      }
      if (nzchar(txt)) {
        param_bits <- c(param_bits, paste0(fields[[nm]], " = ", txt))
      }
    }
  }
  if (!length(param_bits)) {
    return(label)
  }
  paste0(label, ": ", paste(param_bits, collapse = ", "))
}

dfit_add_plot_caption <- function(plot, caption) {
  if (is.null(caption) || !nzchar(caption)) {
    return(plot)
  }
  plot +
    ggplot2::labs(caption = caption) +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 9))
}

dfit_empty_plot <- function(message = "Select measurement data to begin.") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = message, size = 5) +
    ggplot2::theme_void()
}

dfit_hist_binwidth <- function(x, bins = 15L, bin_width = NULL) {
  if (!is.null(bin_width) && is.finite(bin_width) && bin_width > 0) {
    return(bin_width)
  }
  xr <- range(x, na.rm = TRUE)
  if (!is.finite(diff(xr)) || diff(xr) <= 0) {
    return(1)
  }
  diff(xr) / bins
}

dfit_ensure_ppa_graphics <- function() {
  if (!exists("ppa_add_spec_lines", mode = "function")) {
    source("modules/statistical/spc/utils/ppa_calculations.R")
    source("modules/statistical/spc/utils/ppa_graphics.R")
  }
}

dfit_spec_line_defs <- function(spec, colors) {
  dfit_ensure_ppa_graphics()
  spec_color <- ppa_palette_color(colors, 6)
  target_color <- ppa_palette_color(colors, 4)
  defs <- list()
  if (!is.na(spec$lsl)) {
    defs <- c(defs, list(list(
      name = "LSL", value = spec$lsl, color = spec_color,
      linetype = "twodash", linewidth = 0.7
    )))
  }
  if (ppa_has_target(spec)) {
    defs <- c(defs, list(list(
      name = "Target", value = spec$target, color = target_color,
      linetype = "solid", linewidth = 0.7
    )))
  }
  if (!is.na(spec$usl)) {
    defs <- c(defs, list(list(
      name = "USL", value = spec$usl, color = spec_color,
      linetype = "twodash", linewidth = 0.7
    )))
  }
  defs
}

dfit_nt_line_defs <- function(fit, x, colors) {
  if (is.null(fit) || as.integer(fit$distribution_id) == 0L) {
    return(list())
  }
  if (!exists("dfit_distribution_mean", mode = "function")) {
    source("modules/statistical/spc/utils/dfit_distribution_fit.R")
  }
  dfit_ensure_ppa_graphics()
  limit_color <- ppa_palette_color(colors, 3)
  mean_color <- ppa_palette_color(colors, 2)
  mu <- dfit_distribution_mean(fit, x)
  list(
    list(name = "LPL", value = fit$lpl, color = limit_color, linetype = "longdash", linewidth = 1.1),
    list(name = "UPL", value = fit$upl, color = limit_color, linetype = "longdash", linewidth = 1.1),
    list(name = "Mean", value = mu, color = mean_color, linetype = "dotted", linewidth = 1.1)
  )
}

dfit_draw_vertical_reference_lines <- function(p, line_defs, decimals = 4L) {
  if (is.null(line_defs) || !length(line_defs)) {
    return(p)
  }
  label_rows <- list()
  for (item in line_defs) {
    if (!is.finite(item$value)) {
      next
    }
    p <- p + ggplot2::geom_vline(
      xintercept = item$value,
      color = item$color,
      linetype = item$linetype,
      linewidth = item$linewidth
    )
    label_rows <- c(label_rows, list(data.frame(
      x = item$value,
      label = paste0(item$name, " = ", dfit_fmt_caption(item$value, decimals)),
      color = item$color,
      stringsAsFactors = FALSE
    )))
  }
  if (!length(label_rows)) {
    return(p)
  }
  label_df <- do.call(rbind, label_rows)
  label_df$vjust <- 1.05 + 0.22 * (seq_len(nrow(label_df)) - 1L)
  p +
    ggplot2::geom_text(
      data = label_df,
      ggplot2::aes(x = x, y = Inf, label = label, color = I(color), vjust = vjust),
      inherit.aes = FALSE,
      hjust = 0.5,
      size = 3,
      fontface = "bold"
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.18)))
}

dfit_add_reference_lines <- function(
  p,
  spec,
  fit,
  x,
  colors,
  show_spec_limits = FALSE,
  show_nt_limits = FALSE,
  decimals = 4L
) {
  if (is.null(colors)) {
    return(p)
  }
  line_defs <- list()
  if (isTRUE(show_spec_limits)) {
    line_defs <- c(line_defs, dfit_spec_line_defs(spec, colors))
  }
  if (isTRUE(show_nt_limits)) {
    line_defs <- c(line_defs, dfit_nt_line_defs(fit, x, colors))
  }
  dfit_draw_vertical_reference_lines(p, line_defs, decimals)
}

dfit_density_overlay <- function(fit, x, n = 200L, scale_to_counts = FALSE, bin_width = NULL, sample_n = NULL) {
  if (is.null(fit) || is.null(fit$pfun) || fit$distribution_id == 0L) {
    return(NULL)
  }
  xr <- range(x, na.rm = TRUE)
  pad <- diff(xr) * 0.05
  if (!is.finite(pad) || pad <= 0) pad <- 0.1
  grid <- seq(xr[1] - pad, xr[2] + pad, length.out = n)
  dx <- grid[2] - grid[1]
  dens <- rep(NA_real_, length(grid))
  for (i in seq_along(grid)) {
    p_hi <- fit$pfun(grid[i] + dx / 2)
    p_lo <- fit$pfun(grid[i] - dx / 2)
    dens[i] <- (p_hi - p_lo) / dx
  }
  y <- pmax(dens, 0)
  if (scale_to_counts && !is.null(sample_n) && sample_n > 0 && !is.null(bin_width) && bin_width > 0) {
    y <- y * sample_n * bin_width
  }
  data.frame(x = grid, y = y)
}

dfit_plot_histogram_density <- function(
  x,
  fit = NULL,
  spec = list(lsl = NA_real_, target = NA_real_, usl = NA_real_),
  style = c("histogram", "density"),
  bins = 15L,
  bin_width = NULL,
  bin_center = NULL,
  fill_color = "#1F77B4",
  line_color = "#000000",
  overlay_color = "#FF7F0E",
  xlab = "Value",
  title = NULL,
  show_spec_limits = FALSE,
  show_nt_limits = FALSE,
  colors = NULL,
  decimals = 4L
) {
  style <- match.arg(style)
  x <- stats::na.omit(as.numeric(x))
  n <- length(x)
  df <- data.frame(x = x)
  bw <- dfit_hist_binwidth(x, bins = bins, bin_width = bin_width)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = if (style == "histogram") "Count" else "Density"
    )

  if (style == "histogram") {
    bw_arg <- if (!is.null(bin_width) && is.finite(bin_width) && bin_width > 0) {
      bin_width
    } else {
      NULL
    }
    center_arg <- if (!is.null(bin_center) && is.finite(bin_center)) {
      bin_center
    } else {
      NULL
    }
    p <- p + ggplot2::geom_histogram(
      binwidth = bw_arg,
      bins = bins,
      center = center_arg,
      fill = fill_color,
      color = line_color
    )
  } else {
    p <- p +
      ggplot2::geom_density(fill = fill_color, alpha = 0.35, color = line_color) +
      ggplot2::geom_rug(color = line_color, alpha = 0.5)
  }

  overlay <- dfit_density_overlay(
    fit,
    x,
    scale_to_counts = style == "histogram",
    bin_width = bw,
    sample_n = n
  )
  if (!is.null(overlay)) {
    p <- p + ggplot2::geom_line(
      data = overlay,
      ggplot2::aes(x = x, y = y),
      color = overlay_color,
      linewidth = 1,
      inherit.aes = FALSE
    )
  }

  if (isTRUE(show_spec_limits) || isTRUE(show_nt_limits)) {
    p <- dfit_add_reference_lines(
      p, spec, fit, x, colors,
      show_spec_limits = show_spec_limits,
      show_nt_limits = show_nt_limits,
      decimals = decimals
    )
  }
  p + ggplot2::theme_bw()
}

dfit_plot_qq <- function(x, fit, color = "#1F77B4", title = NULL) {
  x <- stats::na.omit(as.numeric(x))
  n <- length(x)
  if (n < 2L || is.null(fit) || is.null(fit$qfun)) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }
  xs <- sort(x)
  p_theo <- fit$qfun(stats::ppoints(n), lower.tail = TRUE)
  df <- data.frame(sample = xs, theoretical = p_theo)
  ggplot2::ggplot(df, ggplot2::aes(x = theoretical, y = sample)) +
    ggplot2::geom_point(color = color) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
    ggplot2::labs(title = title, x = "Theoretical Quantiles", y = "Sample Quantiles") +
    ggplot2::theme_bw()
}

dfit_plot_pp <- function(x, fit, color = "#1F77B4", title = NULL) {
  x <- stats::na.omit(as.numeric(x))
  n <- length(x)
  if (n < 2L || is.null(fit) || is.null(fit$pfun)) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }
  xs <- sort(x)
  p_emp <- stats::ppoints(n)
  p_theo <- fit$pfun(xs)
  df <- data.frame(empirical = p_emp, theoretical = p_theo)
  ggplot2::ggplot(df, ggplot2::aes(x = theoretical, y = empirical)) +
    ggplot2::geom_point(color = color) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
    ggplot2::labs(title = title, x = "Theoretical Probability", y = "Empirical Probability") +
    ggplot2::theme_bw()
}

dfit_plot_probability <- function(
  x,
  fit,
  color = "#1F77B4",
  overlay_color = "#FF7F0E",
  title = NULL,
  spec = list(lsl = NA_real_, target = NA_real_, usl = NA_real_),
  show_spec_limits = FALSE,
  show_nt_limits = FALSE,
  colors = NULL,
  decimals = 4L
) {
  x <- stats::na.omit(as.numeric(x))
  n <- length(x)
  if (n < 2L || is.null(fit) || is.null(fit$pfun)) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }
  xs <- sort(x)
  p_emp <- stats::ppoints(n)
  p_theo <- fit$pfun(xs)
  df <- data.frame(value = xs, probability = p_theo, empirical = p_emp)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = value)) +
    ggplot2::geom_point(ggplot2::aes(y = empirical), color = color) +
    ggplot2::geom_line(ggplot2::aes(y = probability), color = overlay_color, linewidth = 1) +
    ggplot2::labs(title = title, x = "Value", y = "Probability") +
    ggplot2::theme_bw()
  if (isTRUE(show_spec_limits) || isTRUE(show_nt_limits)) {
    p <- dfit_add_reference_lines(
      p, spec, fit, x, colors,
      show_spec_limits = show_spec_limits,
      show_nt_limits = show_nt_limits,
      decimals = decimals
    )
  }
  p
}

dfit_build_plot <- function(
  x,
  fit,
  spec,
  plot_type = c("histogram", "density", "qq", "pp", "probability"),
  bins = 15L,
  bin_width = NULL,
  bin_center = NULL,
  fill_color = "#1F77B4",
  line_color = "#000000",
  overlay_color = "#FF7F0E",
  point_color = "#1F77B4",
  show_spec_limits = FALSE,
  decimals = 4L,
  colors = NULL,
  show_nt_limits = FALSE
) {
  plot_type <- match.arg(plot_type)
  caption <- dfit_fit_caption(fit, decimals)
  p <- switch(
    plot_type,
    histogram = dfit_plot_histogram_density(
      x, fit, spec, style = "histogram", bins = bins,
      bin_width = bin_width, bin_center = bin_center,
      fill_color = fill_color, line_color = line_color, overlay_color = overlay_color,
      show_spec_limits = show_spec_limits, show_nt_limits = show_nt_limits,
      colors = colors, decimals = decimals
    ),
    density = dfit_plot_histogram_density(
      x, fit, spec, style = "density", bins = bins,
      bin_width = bin_width, bin_center = bin_center,
      fill_color = fill_color, line_color = line_color, overlay_color = overlay_color,
      show_spec_limits = show_spec_limits, show_nt_limits = show_nt_limits,
      colors = colors, decimals = decimals
    ),
    qq = dfit_plot_qq(x, fit, color = point_color),
    pp = dfit_plot_pp(x, fit, color = point_color),
    probability = dfit_plot_probability(
      x, fit, color = point_color, overlay_color = overlay_color,
      spec = spec, show_spec_limits = show_spec_limits,
      show_nt_limits = show_nt_limits, colors = colors, decimals = decimals
    )
  )
  dfit_add_plot_caption(p, caption)
}
