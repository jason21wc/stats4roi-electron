# Helpers for EDA Intervals CI plot

# Wrap a plot title to the available device width and reserve top margin for it
format_interval_plot_title <- function(title, plot_width_px, font_size = 11) {
  wrap_width <- max(20L, floor(plot_width_px / (font_size * 0.55)))
  wrapped <- paste(strwrap(title, width = wrap_width), collapse = "\n")
  line_count <- length(strsplit(wrapped, "\n", fixed = TRUE)[[1]])
  top_margin_pt <- font_size * (1.6 + 1.1 * line_count)

  list(
    title = wrapped,
    top_margin_pt = top_margin_pt
  )
}

# Normalize intervals table output for ggplot CI plot
prepare_interval_plot_data <- function(intervals_df, data_type, param = c("Mean", "SD")) {
  param <- match.arg(param)
  if (is.null(intervals_df) || nrow(intervals_df) == 0) {
    return(NULL)
  }

  if (data_type == 1) {
    if (param == "Mean") {
      plot_df <- data.frame(
        group = intervals_df$Column,
        estimate = intervals_df$Mean,
        ci_low = intervals_df$Mean_L,
        ci_high = intervals_df$Mean_U,
        stringsAsFactors = FALSE
      )
    } else {
      plot_df <- data.frame(
        group = intervals_df$Column,
        estimate = intervals_df$SD,
        ci_low = intervals_df$SD_L,
        ci_high = intervals_df$SD_U,
        stringsAsFactors = FALSE
      )
    }
  } else if (data_type == 2) {
    metric_cols <- c("n", "CI_low", "Mean", "CI_high", "SD_low", "SD", "SD_high")
    factor_cols <- setdiff(names(intervals_df), metric_cols)
    group <- apply(intervals_df[, factor_cols, drop = FALSE], 1, function(row) {
      paste(row, collapse = ", ")
    })
    if (param == "Mean") {
      plot_df <- data.frame(
        group = group,
        estimate = intervals_df$Mean,
        ci_low = intervals_df$CI_low,
        ci_high = intervals_df$CI_high,
        stringsAsFactors = FALSE
      )
    } else {
      plot_df <- data.frame(
        group = group,
        estimate = intervals_df$SD,
        ci_low = intervals_df$SD_low,
        ci_high = intervals_df$SD_high,
        stringsAsFactors = FALSE
      )
    }
  } else {
    return(NULL)
  }

  plot_df <- plot_df[stats::complete.cases(plot_df[, c("estimate", "ci_low", "ci_high")]), , drop = FALSE]
  if (nrow(plot_df) == 0) {
    return(NULL)
  }
  plot_df$group <- factor(plot_df$group, levels = plot_df$group)
  plot_df$x_pos <- as.numeric(plot_df$group)
  plot_df
}

# Expand plot data for nearPoints hover (point + CI line endpoints)
prepare_interval_plot_hover_data <- function(plot_df, param = c("Mean", "SD")) {
  param <- match.arg(param)
  if (is.null(plot_df) || nrow(plot_df) == 0) {
    return(NULL)
  }

  base_row <- data.frame(
    x_pos = plot_df$x_pos,
    group = as.character(plot_df$group),
    estimate = plot_df$estimate,
    ci_low = plot_df$ci_low,
    ci_high = plot_df$ci_high,
    param_label = param,
    stringsAsFactors = FALSE
  )

  rbind(
    transform(base_row, y_hover = estimate),
    transform(base_row, y_hover = ci_low),
    transform(base_row, y_hover = ci_high)
  )
}

# Find the nearest interval row from plot hover coordinates (data space)
find_nearest_interval_hover_point <- function(hover_data, hover) {
  if (is.null(hover_data) || nrow(hover_data) == 0) {
    return(NULL)
  }
  if (is.null(hover$x) || is.null(hover$y)) {
    return(NULL)
  }

  x_range <- diff(range(hover_data$x_pos, na.rm = TRUE))
  y_range <- diff(range(hover_data$y_hover, na.rm = TRUE))
  if (!is.finite(x_range) || x_range == 0) {
    x_range <- 1
  }
  if (!is.finite(y_range) || y_range == 0) {
    y_range <- 1
  }

  dist <- ((hover_data$x_pos - hover$x) / x_range)^2 +
    ((hover_data$y_hover - hover$y) / y_range)^2
  idx <- which.min(dist)
  if (dist[idx] > 0.2) {
    return(NULL)
  }

  hover_data[idx, , drop = FALSE]
}
