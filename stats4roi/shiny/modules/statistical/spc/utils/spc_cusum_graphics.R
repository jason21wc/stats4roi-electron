# ggplot builders for CUSUM charts and a lightweight companion Shewhart chart.

spc_cusum_palette_color <- function(colors, index, fallback = "black") {
  if (is.null(colors) || length(colors) < index || is.na(colors[[index]])) {
    return(fallback)
  }
  colors[[index]]
}

#' Human-readable label for the highlight-series radio choice.
spc_cusum_highlight_label <- function(highlight = c("si_ti", "si_ti_star")) {
  highlight <- match.arg(highlight)
  if (identical(highlight, "si_ti_star")) {
    "Highlighted signals: Si* and Ti* (reset partial sums)"
  } else {
    "Highlighted signals: Si and Ti"
  }
}

#' Plot cumulative sum with optional Si/Ti or Si*/Ti* signal highlights.
spc_plot_cusum <- function(
  result,
  highlight = c("si_ti", "si_ti_star"),
  colors = NULL,
  base_size = 11,
  show_decision_band = FALSE,
  title = "CUSUM Chart"
) {
  highlight <- match.arg(highlight)
  tab <- result$table
  if (is.null(tab) || nrow(tab) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::annotate("text", x = 0, y = 0, label = "No CUSUM data to plot.")
    )
  }

  # Palette roles (from get_distribution_colors): 4 = data/mean line,
  # 2 = highlight, 6 = control-chart line. Avoid 3 (green) next to 4 (blue).
  data_col <- spc_cusum_palette_color(colors, 4L, "#2297E6")
  up_col <- spc_cusum_palette_color(colors, 2L, "#DF536B")
  dn_col <- spc_cusum_palette_color(colors, 6L, "#CD0BBC")

  flag <- if (identical(highlight, "si_ti_star")) {
    tab$highlight_si_ti_star
  } else {
    tab$highlight_si_ti
  }
  Si <- if (identical(highlight, "si_ti_star")) tab$Si_star else tab$Si
  Ti <- if (identical(highlight, "si_ti_star")) tab$Ti_star else tab$Ti
  h <- result$limits$h

  tab$upper_y <- ifelse(flag & Si > h, tab$Cusum, NA_real_)
  tab$lower_y <- ifelse(flag & Ti < -h, tab$Cusum, NA_real_)

  # Distinct shapes ensure upper vs lower signals stay readable in any palette.
  p <- ggplot2::ggplot(tab, ggplot2::aes(x = Sample, y = Cusum)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
    ggplot2::geom_line(color = data_col, alpha = 0.8, na.rm = TRUE) +
    ggplot2::geom_point(color = data_col, size = 1.6, na.rm = TRUE) +
    ggplot2::geom_point(
      ggplot2::aes(y = upper_y),
      fill = up_col,
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
      subtitle = spc_cusum_highlight_label(highlight),
      x = "Sample",
      y = "CUSUM (cumulative deviation from target)"
    ) +
    ggplot2::theme_bw(base_size = base_size)

  if (isTRUE(show_decision_band) && is.finite(result$limits$H)) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = c(result$limits$H, -result$limits$H),
        linetype = 5,
        color = up_col,
        alpha = 0.5
      )
  }
  p
}

#' Simple companion Shewhart chart forced to the CUSUM target centerline.
spc_plot_cusum_companion <- function(
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
  # Data line = 4 (blue), centerline = 1 (plot line / black), limits = 2 (red).
  # Using 1 for the centerline avoids the blue/green adjacency of index 3.
  data_col <- spc_cusum_palette_color(colors, 4L, "#2297E6")
  cl_col <- spc_cusum_palette_color(colors, 1L, "#000000")
  lim_col <- spc_cusum_palette_color(colors, 2L, "#DF536B")

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
