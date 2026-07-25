# ggplot builders for EWMA charts and companion Shewhart chart.

spc_ewma_palette_color <- function(colors, index, fallback = "black") {
  if (is.null(colors) || length(colors) < index || is.na(colors[[index]])) {
    return(fallback)
  }
  colors[[index]]
}

#' Plot EWMA series with asymptotic limits and OOC highlights.
spc_plot_ewma <- function(
  result,
  colors = NULL,
  base_size = 11,
  title = "EWMA Chart"
) {
  tab <- result$table
  if (is.null(tab) || nrow(tab) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::annotate("text", x = 0, y = 0, label = "No EWMA data to plot.")
    )
  }

  # Palette: 4 = data line, 2 = limits / upper OOC, 1 = centerline, 6 = lower OOC
  data_col <- spc_ewma_palette_color(colors, 4L, "#2297E6")
  lim_col <- spc_ewma_palette_color(colors, 2L, "#DF536B")
  cl_col <- spc_ewma_palette_color(colors, 1L, "#000000")
  dn_col <- spc_ewma_palette_color(colors, 6L, "#CD0BBC")

  ucl <- result$limits$UCL
  lcl <- result$limits$LCL
  target <- result$target

  tab$upper_y <- ifelse(tab$OOC & tab$EWMA > ucl, tab$EWMA, NA_real_)
  tab$lower_y <- ifelse(tab$OOC & tab$EWMA < lcl, tab$EWMA, NA_real_)

  alpha <- result$limits$alpha
  subtitle <- paste0(
    "\u03b1 = ", format(round(alpha, 4), nsmall = 4, trim = TRUE),
    "; OOC = points outside asymptotic limits only"
  )

  ggplot2::ggplot(tab, ggplot2::aes(x = Sample, y = EWMA)) +
    ggplot2::geom_hline(yintercept = target, color = cl_col) +
    ggplot2::geom_hline(yintercept = ucl, color = lim_col, linetype = 5) +
    ggplot2::geom_hline(yintercept = lcl, color = lim_col, linetype = 5) +
    ggplot2::geom_line(color = data_col, alpha = 0.8, na.rm = TRUE) +
    ggplot2::geom_point(color = data_col, size = 1.6, na.rm = TRUE) +
    ggplot2::geom_point(
      ggplot2::aes(y = upper_y),
      fill = lim_col,
      color = "black",
      shape = 24,
      size = 3,
      stroke = 0.4,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = lower_y),
      fill = dn_col,
      color = "black",
      shape = 25,
      size = 3,
      stroke = 0.4,
      na.rm = TRUE
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Sample",
      y = "EWMA"
    ) +
    ggplot2::theme_bw(base_size = base_size)
}

#' Simple companion Shewhart chart forced to the EWMA target centerline.
spc_plot_ewma_companion <- function(
  result,
  colors = NULL,
  base_size = 11,
  n_sigma = 3
) {
  tab <- result$table
  if (is.null(tab) || nrow(tab) == 0L) {
    return(NULL)
  }
  target <- result$target
  K <- result$limits$K
  data_col <- spc_ewma_palette_color(colors, 4L, "#2297E6")
  cl_col <- spc_ewma_palette_color(colors, 1L, "#000000")
  lim_col <- spc_ewma_palette_color(colors, 2L, "#DF536B")

  ucl <- target + n_sigma * K
  lcl <- target - n_sigma * K
  mode_label <- if (identical(result$prepared$mode, "means")) "X-bar" else "X"
  title <- paste0("Companion ", mode_label, " Chart (centerline = target)")

  ggplot2::ggplot(tab, ggplot2::aes(x = Sample, y = Value)) +
    ggplot2::geom_line(color = data_col, alpha = 0.7, na.rm = TRUE) +
    ggplot2::geom_point(color = data_col, size = 1.5, na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = target, color = cl_col) +
    ggplot2::geom_hline(yintercept = ucl, color = lim_col, linetype = 5) +
    ggplot2::geom_hline(yintercept = lcl, color = lim_col, linetype = 5) +
    ggplot2::labs(title = title, x = "Sample", y = result$prepared$y_label %||% "Value") +
    ggplot2::theme_bw(base_size = base_size)
}

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
