# HTML parameter summary for EWMA Limit/Parameter Summary panel.

spc_ewma_fmt <- function(x, digits = 4L) {
  if (is.null(x) || length(x) == 0L || !is.finite(x[[1L]])) {
    return("")
  }
  format(round(as.numeric(x[[1L]]), digits), nsmall = digits, trim = TRUE, scientific = FALSE)
}

spc_ewma_row_html <- function(label, value) {
  paste0(
    "<tr><td style='padding:4px 8px;border:1px solid #ddd;'>", label,
    "</td><td style='padding:4px 8px;border:1px solid #ddd;text-align:left;'>",
    value, "</td></tr>"
  )
}

#' Build bordered HTML summary for an EWMA analysis result.
spc_build_ewma_summary_html <- function(result, digits = 4L) {
  if (is.null(result)) {
    return("<p>No EWMA results.</p>")
  }
  lim <- result$limits
  sigma <- result$sigma
  mode_label <- if (identical(result$prepared$mode, "means")) {
    "EWMA for Means"
  } else {
    "EWMA for Individuals"
  }
  R <- as.integer(digits)

  out <- "<table style='border-collapse:collapse;width:100%;max-width:520px;'>"
  out <- paste0(out, "<tr><th colspan='2' style='text-align:left;padding:6px;'>EWMA Parameters</th></tr>")
  out <- paste0(out, spc_ewma_row_html("Chart", mode_label))
  out <- paste0(out, spc_ewma_row_html("Target", spc_ewma_fmt(result$target, R)))
  out <- paste0(out, spc_ewma_row_html("\u03b1 (weight on current point)", spc_ewma_fmt(lim$alpha, max(R, 4L))))
  if (!is.null(result$seed) && is.finite(result$seed)) {
    seed_label <- paste0("Seed (mean of first ", as.integer(result$seed_n %||% 5), " points)")
    out <- paste0(out, spc_ewma_row_html(seed_label, spc_ewma_fmt(result$seed, R)))
  }
  out <- paste0(out, spc_ewma_row_html("Standard errors (L)", spc_ewma_fmt(lim$L, 2L)))
  out <- paste0(out, spc_ewma_row_html("Sigma method", sigma$method_label %||% sigma$method))
  if (identical(result$prepared$mode, "individuals")) {
    out <- paste0(out, spc_ewma_row_html("MR span", as.character(sigma$span)))
  } else {
    out <- paste0(out, spc_ewma_row_html("Subgroup n", spc_ewma_fmt(sigma$n, 2L)))
    out <- paste0(out, spc_ewma_row_html("Range statistic", spc_ewma_fmt(sigma$stat, R)))
  }
  out <- paste0(out, "<tr><th colspan='2' style='text-align:left;padding:6px;'>Derived Constants</th></tr>")
  out <- paste0(out, spc_ewma_row_html("Estimated \u03c3 (K)", spc_ewma_fmt(lim$K, R)))
  out <- paste0(out, spc_ewma_row_html("Half-width", spc_ewma_fmt(lim$half_width, R)))
  out <- paste0(out, spc_ewma_row_html("UCL", spc_ewma_fmt(lim$UCL, R)))
  out <- paste0(out, spc_ewma_row_html("LCL", spc_ewma_fmt(lim$LCL, R)))

  n_ooc <- sum(result$table$OOC, na.rm = TRUE)
  out <- paste0(out, "<tr><th colspan='2' style='text-align:left;padding:6px;'>Signals</th></tr>")
  out <- paste0(out, spc_ewma_row_html("Points outside limits", as.character(n_ooc)))
  out <- paste0(out, "</table>")
  out
}

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
