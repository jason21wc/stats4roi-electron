# Optimizer report snapshot and ZIP export (pure R; no graphics devices).

#' Sanitize a label for use in export filenames.
#' @keywords internal
mf_safe_export_slug <- function(x) {
  x <- as.character(x)[1]
  if (!nzchar(x)) return("response")
  slug <- gsub("[^A-Za-z0-9._-]+", "_", x)
  slug <- gsub("_+", "_", slug)
  slug <- gsub("^_|_$", "", slug)
  if (!nzchar(slug)) "response" else slug
}

#' Minimal HTML escaping for report output.
#' @keywords internal
mf_html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

#' Format numeric values for HTML report text.
#' @keywords internal
mf_report_fmt_num <- function(x, digits = 6L) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.finite(x), format(signif(x, digits)), "NA")
}

#' Convert a data frame to a simple HTML table.
#' @keywords internal
mf_df_to_html_table <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 1L) {
    return("<p><em>No data available.</em></p>")
  }
  hdr <- paste0(
    "<tr>",
    paste0("<th>", mf_html_escape(names(df)), "</th>", collapse = ""),
    "</tr>"
  )
  body <- apply(df, 1, function(row) {
    paste0(
      "<tr>",
      paste0("<td>", mf_html_escape(as.character(row)), "</td>", collapse = ""),
      "</tr>"
    )
  })
  paste0(
    "<table>",
    "<thead>", hdr, "</thead>",
    "<tbody>", paste(body, collapse = ""), "</tbody>",
    "</table>"
  )
}

#' Inline SVG normal distribution plot (no R graphics device).
#' @keywords internal
mf_normal_distribution_svg <- function(
    mu,
    sigma,
    target = NA_real_,
    lsl = NA_real_,
    usl = NA_real_,
    title = "",
    width = 640,
    height = 360) {
  mu <- suppressWarnings(as.numeric(mu))[1]
  sigma <- suppressWarnings(as.numeric(sigma))[1]
  target <- suppressWarnings(as.numeric(target))[1]
  lsl <- suppressWarnings(as.numeric(lsl))[1]
  usl <- suppressWarnings(as.numeric(usl))[1]

  pad_left <- 56
  pad_right <- 20
  pad_top <- 40
  pad_bottom <- 44
  plot_w <- width - pad_left - pad_right
  plot_h <- height - pad_top - pad_bottom

  if (!is.finite(mu) || !is.finite(sigma) || sigma <= 0) {
    return(sprintf(
      '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" role="img" aria-label="Distribution unavailable"><text x="20" y="40">Distribution unavailable</text></svg>',
      width, height
    ))
  }

  refs <- c(mu - 4 * sigma, mu + 4 * sigma)
  if (is.finite(target)) refs <- c(refs, target)
  if (is.finite(lsl)) refs <- c(refs, lsl)
  if (is.finite(usl)) refs <- c(refs, usl)
  xlo <- min(refs, na.rm = TRUE)
  xhi <- max(refs, na.rm = TRUE)
  span <- xhi - xlo
  pad_x <- if (is.finite(span) && span > 0) 0.05 * span else 1
  xlo <- xlo - pad_x
  xhi <- xhi + pad_x

  xs <- seq(xlo, xhi, length.out = 200L)
  ys <- stats::dnorm(xs, mean = mu, sd = sigma)
  ymax <- max(ys, na.rm = TRUE)
  if (!is.finite(ymax) || ymax <= 0) ymax <- 1

  x_map <- function(x) pad_left + (x - xlo) / (xhi - xlo) * plot_w
  y_base <- pad_top + plot_h
  y_map <- function(y) pad_top + (1 - y / ymax) * plot_h

  coords <- paste0(x_map(xs), ",", y_map(ys))
  area_path <- paste0(
    "M ", x_map(xs[[1L]]), ",", y_base, " L ",
    paste(coords, collapse = " L "),
    " L ", x_map(xs[[length(xs)]]), ",", y_base, " Z"
  )

  vline <- function(x, color, dash) {
    if (!is.finite(x)) return("")
    xi <- x_map(x)
    sprintf(
      '<line x1="%.2f" y1="%d" x2="%.2f" y2="%d" stroke="%s" stroke-width="2" stroke-dasharray="%s"/>',
      xi, pad_top, xi, y_base, color, dash
    )
  }

  title_el <- if (nzchar(title)) {
    sprintf(
      '<text x="%d" y="24" font-size="14" font-weight="600">%s</text>',
      pad_left, mf_html_escape(title)
    )
  } else {
    ""
  }

  paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="', width, '" height="', height,
    '" viewBox="0 0 ', width, " ", height, '" role="img" aria-label="', mf_html_escape(title), '">',
    '<rect width="100%" height="100%" fill="#ffffff"/>',
    title_el,
    '<path d="', area_path, '" fill="#9ecae1" fill-opacity="0.35"/>',
    '<polyline points="', paste(coords, collapse = " "), '" fill="none" stroke="#2c7fb8" stroke-width="2"/>',
    vline(target, "#2ca25f", "6,4"),
    vline(lsl, "#de2d26", "2,4"),
    vline(usl, "#de2d26", "2,4"),
    '<text x="', pad_left, '" y="', height - 12, '" font-size="11">Output</text>',
    "</svg>"
  )
}

#' Extract per-response distribution details from an optimizer result.
#' @keywords internal
optimizer_report_response_details <- function(opt_result) {
  if (is.null(opt_result) || !isTRUE(opt_result$ok)) {
    return(list())
  }
  if (!is.null(opt_result$aggregate) && is.list(opt_result$aggregate) &&
      !is.null(opt_result$aggregate$details) && length(opt_result$aggregate$details) >= 1L) {
    return(opt_result$aggregate$details)
  }
  list(list(
    response = "response",
    mu = opt_result$mu,
    sigma = opt_result$sigma,
    target = NA_real_,
    lsl = NA_real_,
    usl = NA_real_,
    expected_loss = if (!is.null(opt_result$metrics)) opt_result$metrics$expected_loss[1] else NA_real_,
    ppm = if (!is.null(opt_result$metrics)) opt_result$metrics$ppm[1] else NA_real_
  ))
}

#' Build a portable snapshot for optimizer report export.
#' @keywords internal
build_optimizer_report_snapshot <- function(
    opt_result,
    confirmation = NULL,
    loss_grids = NULL,
    metadata = list()) {
  list(
    generated_at = Sys.time(),
    metadata = metadata,
    opt_result = opt_result,
    confirmation = confirmation,
    loss_grids = loss_grids
  )
}

#' Write optimizer detail rows as a data frame.
#' @keywords internal
optimizer_detail_export_table <- function(opt_result) {
  detail_rows <- build_optimizer_detail_rows(opt_result)
  data.frame(
    Metric = detail_rows$metrics,
    Value = detail_rows$values,
    stringsAsFactors = FALSE
  )
}

#' Write optimum factor settings as a data frame.
#' @keywords internal
optimizer_optimum_settings_table <- function(opt_result) {
  if (is.null(opt_result) || !isTRUE(opt_result$ok) || is.null(opt_result$par)) {
    return(data.frame(Note = "No optimum settings available.", stringsAsFactors = FALSE))
  }
  data.frame(
    Variable = names(opt_result$par),
    Value = as.numeric(opt_result$par),
    stringsAsFactors = FALSE
  )
}

#' Write CSV helper with a fallback note row.
#' @keywords internal
mf_write_csv_or_note <- function(path, tbl, empty_note) {
  if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) < 1L) {
    utils::write.csv(data.frame(Note = empty_note), path, row.names = FALSE)
  } else {
    utils::write.csv(tbl, path, row.names = FALSE)
  }
}

#' Build optimizer summary HTML paragraph.
#' @keywords internal
optimizer_report_summary_html <- function(opt_result) {
  if (is.null(opt_result) || !isTRUE(opt_result$ok)) {
    return("<p>No successful optimizer result is available.</p>")
  }
  parts <- c(
    paste0(
      "Optimizer converged (code ", opt_result$convergence, "). ",
      "Objective value: ", mf_report_fmt_num(opt_result$value), "."
    )
  )
  if (!is.null(opt_result$optimize_target) && identical(opt_result$optimize_target, "total_cost")) {
    parts <- c(parts, paste0(" Optimized on total cost with volume=", mf_report_fmt_num(opt_result$volume), "."))
  }
  if (!is.null(opt_result$optimization_mode) && nzchar(as.character(opt_result$optimization_mode))) {
    parts <- c(parts, paste0(" Continuous optimization mode: ", mf_html_escape(opt_result$optimization_mode), "."))
  }
  if (!is.null(opt_result$blocked_factors) && length(opt_result$blocked_factors) > 0L) {
    parts <- c(parts, paste0(" Blocked factors: ", mf_html_escape(paste(opt_result$blocked_factors, collapse = ", ")), "."))
  }
  paste0("<p>", paste(parts, collapse = ""), "</p>")
}

#' Write a standalone HTML optimizer report (no pandoc/rmarkdown/graphics).
#' @keywords internal
write_optimizer_report_html <- function(snapshot, path) {
  opt <- snapshot$opt_result
  details <- snapshot$plot_details
  if (is.null(details)) {
    details <- optimizer_report_response_details(opt)
  }

  plot_sections <- if (length(details) < 1L) {
    "<p><em>No distribution plots available.</em></p>"
  } else {
    paste(vapply(seq_along(details), function(i) {
      di <- details[[i]]
      resp <- if (!is.null(di$response)) as.character(di$response) else paste0("Response ", i)
      svg <- if (!is.null(di$plot_svg) && nzchar(di$plot_svg)) {
        paste0('<div class="plot-wrap">', di$plot_svg, "</div>")
      } else {
        ""
      }
      paste0(
        "<h3>", mf_html_escape(resp), "</h3>",
        svg,
        "<ul>",
        "<li>Predicted mean: ", mf_report_fmt_num(di$mu), "</li>",
        "<li>Predicted sigma: ", mf_report_fmt_num(di$sigma), "</li>",
        "<li>Expected loss: ", mf_report_fmt_num(di$expected_loss), "</li>",
        "<li>PPM: ", mf_report_fmt_num(di$ppm), "</li>",
        "</ul>"
      )
    }, character(1)), collapse = "\n")
  }

  loss_grid_note <- if (is.null(snapshot$loss_grids) || length(snapshot$loss_grids) < 1L) {
    "<p><em>No loss grids were included in this export.</em></p>"
  } else {
    items <- vapply(names(snapshot$loss_grids), function(rn) {
      slug <- mf_safe_export_slug(rn)
      paste0("<li><code>loss_grid_", mf_html_escape(slug), ".csv</code> (", mf_html_escape(rn), ")</li>")
    }, character(1))
    paste0("<p>Per-response loss grids are included as CSV files in this archive:</p><ul>", paste(items, collapse = ""), "</ul>")
  }

  html <- c(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<head>",
    "<meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>Optimizer Report</title>",
    "<style>",
    "body{font-family:system-ui,-apple-system,Segoe UI,sans-serif;margin:2em;line-height:1.5;color:#222;}",
    "table{border-collapse:collapse;margin:1em 0;width:100%;max-width:960px;}",
    "th,td{border:1px solid #ccc;padding:6px 10px;text-align:left;}",
    "th{background:#f5f5f5;}",
    "h2{margin-top:2em;border-bottom:1px solid #ddd;padding-bottom:0.25em;}",
    ".plot-wrap{max-width:720px;margin:0.5em 0;}",
    ".plot-wrap svg{max-width:100%;height:auto;display:block;}",
    "code{background:#f5f5f5;padding:0.1em 0.35em;border-radius:3px;}",
    "</style>",
    "</head>",
    "<body>",
    "<h1>Optimizer Report</h1>",
    paste0("<p>Generated: ", mf_html_escape(format(snapshot$generated_at, "%Y-%m-%d %H:%M:%S %Z")), "</p>"),
    "<h2>Summary</h2>",
    optimizer_report_summary_html(opt),
    "<h2>Optimum factor settings</h2>",
    if (!is.null(opt) && isTRUE(opt$ok)) {
      mf_df_to_html_table(optimizer_optimum_settings_table(opt))
    } else {
      "<p><em>No optimum settings available.</em></p>"
    },
    "<h2>Optimizer detail metrics</h2>",
    if (!is.null(opt) && isTRUE(opt$ok)) {
      mf_df_to_html_table(optimizer_detail_export_table(opt))
    } else {
      "<p><em>No optimizer detail metrics available.</em></p>"
    },
    "<h2>Bounds used</h2>",
    if (!is.null(opt) && isTRUE(opt$ok) && is.data.frame(opt$bounds_used)) {
      mf_df_to_html_table(opt$bounds_used)
    } else {
      "<p><em>No bounds information available.</em></p>"
    },
    "<h2>Confirmation experiment settings</h2>",
    mf_df_to_html_table(snapshot$confirmation),
    "<h2>Estimated distributions at optimum</h2>",
    plot_sections,
    "<h2>Loss grids</h2>",
    loss_grid_note,
    "</body></html>"
  )

  writeLines(html, path, useBytes = TRUE)
  invisible(path)
}

#' Attach inline SVG plot markup to response detail rows.
#' @keywords internal
optimizer_report_attach_plot_svgs <- function(details) {
  if (length(details) < 1L) return(details)
  for (i in seq_along(details)) {
    di <- details[[i]]
    resp_name <- if (!is.null(di$response) && nzchar(as.character(di$response))) {
      as.character(di$response)
    } else {
      paste0("response_", i)
    }
    di$plot_svg <- mf_normal_distribution_svg(
      mu = di$mu,
      sigma = di$sigma,
      target = di$target,
      lsl = di$lsl,
      usl = di$usl,
      title = paste0("Estimated Distribution at Optimum for ", resp_name)
    )
    details[[i]] <- di
  }
  details
}

#' Write optimizer report assets to a directory (CSVs + HTML only).
#' @return Character vector of files written (absolute paths).
#' @keywords internal
write_optimizer_report_assets <- function(snapshot, out_dir) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  opt_result <- snapshot$opt_result
  written <- character(0)

  detail_path <- file.path(out_dir, "optimizer_detail.csv")
  mf_write_csv_or_note(
    detail_path,
    if (!is.null(opt_result) && isTRUE(opt_result$ok)) optimizer_detail_export_table(opt_result) else NULL,
    "No successful optimizer result to export."
  )
  written <- c(written, detail_path)

  optimum_path <- file.path(out_dir, "optimum_settings.csv")
  mf_write_csv_or_note(
    optimum_path,
    if (!is.null(opt_result) && isTRUE(opt_result$ok)) optimizer_optimum_settings_table(opt_result) else NULL,
    "No optimum settings available."
  )
  written <- c(written, optimum_path)

  bounds_path <- file.path(out_dir, "bounds_used.csv")
  bounds_tbl <- if (!is.null(opt_result) && isTRUE(opt_result$ok)) opt_result$bounds_used else NULL
  mf_write_csv_or_note(bounds_path, bounds_tbl, "No bounds information available.")
  written <- c(written, bounds_path)

  conf_path <- file.path(out_dir, "confirmation_settings.csv")
  mf_write_csv_or_note(
    conf_path,
    snapshot$confirmation,
    "No confirmation settings to export."
  )
  written <- c(written, conf_path)

  loss_grids <- snapshot$loss_grids
  if (!is.null(loss_grids) && length(loss_grids) >= 1L) {
    for (rn in names(loss_grids)) {
      entry <- loss_grids[[rn]]
      tbl <- if (is.list(entry) && !is.null(entry$table)) entry$table else entry
      if (is.list(entry) && !isTRUE(entry$ok) && !is.null(entry$message)) {
        tbl <- data.frame(Note = as.character(entry$message), stringsAsFactors = FALSE)
      }
      slug <- mf_safe_export_slug(rn)
      grid_path <- file.path(out_dir, paste0("loss_grid_", slug, ".csv"))
      mf_write_csv_or_note(grid_path, tbl, paste0("No loss grid available for ", rn, "."))
      written <- c(written, grid_path)
    }
  }

  details <- optimizer_report_attach_plot_svgs(optimizer_report_response_details(opt_result))
  snapshot$plot_details <- details

  html_path <- file.path(out_dir, "report.html")
  write_optimizer_report_html(snapshot, html_path)
  written <- c(written, html_path)

  unique(written)
}

#' Zip files under a directory without changing the working directory.
#' @keywords internal
mf_zip_directory <- function(zipfile, dir_path) {
  files_abs <- list.files(dir_path, recursive = TRUE, full.names = TRUE)
  if (length(files_abs) < 1L) {
    stop("No files to zip.")
  }
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zipr(zipfile = zipfile, files = files_abs, root = dir_path)
    return(invisible(zipfile))
  }
  utils::zip(zipfile = zipfile, files = files_abs, flags = "-j")
  invisible(zipfile)
}

#' Write a minimal error archive instead of aborting the download handler.
#' @keywords internal
write_optimizer_report_error_zip <- function(file, message) {
  tmpdir <- tempfile("optimizer_report_error_")
  dir.create(tmpdir)
  writeLines(as.character(message), file.path(tmpdir, "export_error.txt"))
  mf_zip_directory(file, tmpdir)
  unlink(tmpdir, recursive = TRUE)
  invisible(file)
}

#' Write a ZIP archive containing HTML report and CSV files.
#' @keywords internal
write_optimizer_report_zip <- function(file, snapshot) {
  tmpdir <- tempfile("optimizer_report_")
  dir.create(tmpdir)
  write_optimizer_report_assets(snapshot, tmpdir)
  mf_zip_directory(file, tmpdir)
  unlink(tmpdir, recursive = TRUE)
  invisible(file)
}
