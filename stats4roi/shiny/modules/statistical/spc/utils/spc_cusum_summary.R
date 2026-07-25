# HTML parameter summary for CUSUM Limit/Parameter Summary panel.

spc_cusum_fmt <- function(x, digits = 4L) {
  if (is.null(x) || length(x) == 0L || !is.finite(x[[1L]])) {
    return("")
  }
  format(round(as.numeric(x[[1L]]), digits), nsmall = digits, trim = TRUE, scientific = FALSE)
}

spc_cusum_row_html <- function(label, value) {
  paste0(
    "<tr><td style='padding:4px 8px;border:1px solid #ddd;'>", label,
    "</td><td style='padding:4px 8px;border:1px solid #ddd;text-align:left;'>",
    value, "</td></tr>"
  )
}

#' Build bordered HTML summary for a CUSUM analysis result.
spc_build_cusum_summary_html <- function(result, digits = 4L) {
  if (is.null(result)) {
    return("<p>No CUSUM results.</p>")
  }
  lim <- result$limits
  sigma <- result$sigma
  mode_label <- if (identical(result$prepared$mode, "means")) {
    "CUSUM for Means"
  } else {
    "CUSUM for Individuals"
  }
  R <- as.integer(digits)

  out <- "<table style='border-collapse:collapse;width:100%;max-width:520px;'>"
  out <- paste0(out, "<tr><th colspan='2' style='text-align:left;padding:6px;'>CUSUM Parameters</th></tr>")
  out <- paste0(out, spc_cusum_row_html("Chart", mode_label))
  out <- paste0(out, spc_cusum_row_html("Target", spc_cusum_fmt(result$target, R)))
  out <- paste0(out, spc_cusum_row_html("k", spc_cusum_fmt(lim$k, R)))
  out <- paste0(out, spc_cusum_row_html("\u03b1 (alpha)", spc_cusum_fmt(lim$alpha, max(R, 4L))))
  out <- paste0(out, spc_cusum_row_html("\u03b2 (beta)", spc_cusum_fmt(lim$beta, max(R, 4L))))
  out <- paste0(out, spc_cusum_row_html("Sigma method", sigma$method_label %||% sigma$method))
  if (identical(result$prepared$mode, "individuals")) {
    out <- paste0(out, spc_cusum_row_html("MR span", as.character(sigma$span)))
  } else {
    out <- paste0(out, spc_cusum_row_html("Subgroup n", spc_cusum_fmt(sigma$n, 2L)))
    out <- paste0(out, spc_cusum_row_html("Range statistic", spc_cusum_fmt(sigma$stat, R)))
  }
  out <- paste0(out, "<tr><th colspan='2' style='text-align:left;padding:6px;'>Derived Constants</th></tr>")
  out <- paste0(out, spc_cusum_row_html("Estimated \u03c3 (K)", spc_cusum_fmt(lim$K, R)))
  out <- paste0(out, spc_cusum_row_html("Detect a shift of", spc_cusum_fmt(lim$shift_size, R)))
  out <- paste0(out, spc_cusum_row_html("K", spc_cusum_fmt(lim$K, R)))
  out <- paste0(out, spc_cusum_row_html("d", spc_cusum_fmt(lim$d, R)))
  out <- paste0(out, spc_cusum_row_html("H", spc_cusum_fmt(lim$H, R)))
  out <- paste0(out, spc_cusum_row_html("h", spc_cusum_fmt(lim$h, R)))

  n_sig <- sum(result$table$highlight_si_ti, na.rm = TRUE)
  n_sig_star <- sum(result$table$highlight_si_ti_star, na.rm = TRUE)
  out <- paste0(out, "<tr><th colspan='2' style='text-align:left;padding:6px;'>Signals</th></tr>")
  out <- paste0(out, spc_cusum_row_html("Points with |Si| or |Ti| > h", as.character(n_sig)))
  out <- paste0(out, spc_cusum_row_html("Points with |Si*| or |Ti*| > h", as.character(n_sig_star)))
  out <- paste0(out, "</table>")
  out
}

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
