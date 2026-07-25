# ggplot helpers for autocorrelation module

#' Minimal theme consistent with EDA plot helpers.
acf_plot_theme <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "grey90"),
      panel.grid.minor = ggplot2::element_line(color = "grey95"),
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14),
      plot.title = ggplot2::element_text(size = 16, hjust = 0.5)
    )
}

#' Run-sequence (index vs measure) plot.
#' @param series Numeric vector (prepared).
#' @param colors Color list from get_distribution_colors() or palette vector.
#' @param ylab Y-axis label.
acf_run_sequence_plot <- function(series, colors = NULL, ylab = "Measure") {
  series <- acf_prepare_series(series)
  if (length(series) < 1L) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No data to plot") +
        ggplot2::theme_void()
    )
  }
  line_col <- if (!is.null(colors) && !is.null(colors$col_plot_line)) {
    colors$col_plot_line
  } else if (!is.null(colors) && length(colors) >= 1) {
    colors[[1]]
  } else {
    "steelblue"
  }
  df <- data.frame(index = seq_along(series), measure = series)
  ggplot2::ggplot(df, ggplot2::aes(x = index, y = measure)) +
    ggplot2::geom_line(color = line_col, linewidth = 0.7) +
    ggplot2::geom_point(color = line_col, size = 1.5) +
    ggplot2::labs(x = "Observation order", y = ylab, title = "Run Sequence") +
    acf_plot_theme()
}

#' Bar plot of ACF or PACF with ±critical bands.
#' @param result Result from acf_compute() or pacf_compute().
#' @param omit_lag0 If TRUE and type is acf, drop lag 0 from the plot.
#' @param colors Color list or palette.
#' @param title Plot title override.
acf_correlation_plot <- function(result, omit_lag0 = TRUE, colors = NULL, title = NULL) {
  if (is.null(result) || length(result$lag) < 1L) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No autocorrelation to plot") +
        ggplot2::theme_void()
    )
  }

  lag <- as.numeric(result$lag)
  value <- as.numeric(result$acf)
  if (isTRUE(omit_lag0) && identical(result$type, "acf")) {
    keep <- lag != 0
    lag <- lag[keep]
    value <- value[keep]
  }

  bar_col <- if (!is.null(colors) && !is.null(colors$col_fill_highlight)) {
    colors$col_fill_highlight
  } else if (!is.null(colors) && length(colors) >= 2) {
    colors[[2]]
  } else {
    "steelblue"
  }
  band_col <- if (!is.null(colors) && !is.null(colors$col_mean_line)) {
    colors$col_mean_line
  } else if (!is.null(colors) && length(colors) >= 4) {
    colors[[4]]
  } else {
    "firebrick"
  }

  if (is.null(title)) {
    title <- if (identical(result$type, "pacf")) "Partial Autocorrelation (PACF)" else "Autocorrelation (ACF)"
  }

  df <- data.frame(lag = lag, value = value)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = lag, y = value)) +
    ggplot2::geom_col(fill = bar_col, width = 0.2) +
    ggplot2::geom_hline(yintercept = 0, color = "grey40") +
    ggplot2::labs(x = "Lag", y = if (identical(result$type, "pacf")) "PACF" else "ACF", title = title) +
    acf_plot_theme()

  crit <- result$crit
  if (!is.null(crit) && is.finite(crit)) {
    p <- p +
      ggplot2::geom_hline(yintercept = c(-crit, crit), linetype = "dashed", color = band_col)
  }
  p
}
