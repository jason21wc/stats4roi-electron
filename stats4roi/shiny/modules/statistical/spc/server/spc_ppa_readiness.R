# PPA readiness checks

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

ppa_readiness_checks <- function(prepared, spec, mappings = list()) {
  checks <- list()

  checks$response <- list(
    ok = length(ppa_normalize_response_cols(mappings$response_cols)) > 0,
    label = "At least one response column selected"
  )
  sample_ok <- !is.null(mappings$sample_id) && (
    identical(mappings$sample_id, ppa_sample_time_order_id()) ||
      nzchar(mappings$sample_id)
  )
  checks$sample <- list(
    ok = sample_ok,
    label = "Sample / time column selected"
  )
  checks$stream <- list(
    ok = !is.null(mappings$stream_factors) && length(mappings$stream_factors) > 0,
    label = "At least one process stream factor"
  )
  checks$spec <- list(
    ok = (!is.na(spec$lsl) || !is.na(spec$usl)),
    label = "At least one specification limit"
  )
  checks$numeric <- list(
    ok = is.null(prepared) || (nrow(prepared) > 0 && all(is.finite(prepared$response))),
    label = "Response values are numeric"
  )
  checks$n <- list(
    ok = is.null(prepared) || nrow(prepared) >= 10,
    label = "Sufficient observations (n >= 10)"
  )

  ok <- all(vapply(checks, function(x) isTRUE(x$ok), logical(1)))
  list(ok = ok, checks = checks)
}

ppa_render_readiness_html <- function(readiness, R = 2) {
  rows <- vapply(readiness$checks, function(ch) {
    icon <- if (isTRUE(ch$ok)) "\u2713" else "\u2717"
    sprintf("<tr><td>%s</td><td>%s</td></tr>", icon, ch$label)
  }, character(1))
  HTML(paste0(
    "<h4>Readiness</h4><table>",
    paste(rows, collapse = ""),
    "</table>"
  ))
}

ppa_build_limit_config <- function(input, prefix = "ppa", data_shape = NULL) {
  loc_lim <- as.integer(input[[paste0(prefix, "_loc_lim")]] %||% 7L)
  disp_lim <- as.integer(input[[paste0(prefix, "_disp_lim")]] %||% 7L)
  custom_disp <- NULL
  if (disp_lim == 12L) {
    custom_disp <- list(
      upper = input[[paste0(prefix, "_custom_disp_upper")]],
      center = input[[paste0(prefix, "_custom_disp_center")]],
      lower = input[[paste0(prefix, "_custom_disp_lower")]]
    )
  }
  shape <- data_shape %||% "single"
  defaults <- ppa_default_limit_config(shape)
  ind_or_mean <- isTRUE(defaults$ind_or_mean)
  loc_type <- as.integer(defaults$loc_type)
  cfg <- list(
    data_shape = shape,
    ind_or_mean = ind_or_mean,
    loc_type = as.integer(input[[paste0(prefix, "_loc_type")]] %||% loc_type),
    disp_type = as.integer(input[[paste0(prefix, "_disp_type")]] %||% defaults$disp_type),
    loc_lim = loc_lim,
    disp_lim = disp_lim,
    loc_center = as.integer(input[[paste0(prefix, "_loc_center")]] %||% defaults$loc_center),
    disp_center = as.integer(input[[paste0(prefix, "_disp_center")]] %||% defaults$disp_center),
    mr_span = as.integer(input[[paste0(prefix, "_mr_span")]] %||% defaults$mr_span),
    known_sigma = input[[paste0(prefix, "_known_sigma")]],
    custom_disp = custom_disp,
    std_err = as.integer(input[[paste0(prefix, "_std_err")]] %||% 3L)
  )
  if (identical(shape, "subgroup")) {
    cfg$ind_or_mean <- TRUE
    cfg$loc_type <- 1L
  } else {
    cfg$ind_or_mean <- FALSE
    cfg$loc_type <- 2L
  }
  cfg
}

ppa_apply_limit_defaults_to_inputs <- function(session, data_shape, prefix = "ppa") {
  defs <- ppa_default_limit_config(data_shape)
  updateSelectInput(session, paste0(prefix, "_loc_type"), selected = defs$loc_type)
  updateSelectInput(session, paste0(prefix, "_disp_type"), selected = defs$disp_type)
  updateSelectInput(session, paste0(prefix, "_loc_center"), selected = defs$loc_center)
  updateSelectInput(session, paste0(prefix, "_disp_center"), selected = defs$disp_center)
  updateNumericInput(session, paste0(prefix, "_mr_span"), value = defs$mr_span)
  updateSelectInput(session, paste0(prefix, "_std_err"), selected = defs$std_err)
  if (identical(data_shape, "single")) {
    updateSelectInput(
      session,
      paste0(prefix, "_loc_lim"),
      choices = spc_individuals_loc_lim_choices(),
      selected = defs$loc_lim
    )
    updateSelectInput(
      session,
      paste0(prefix, "_disp_lim"),
      choices = spc_individuals_mr_disp_lim_choices(),
      selected = defs$disp_lim
    )
    return(invisible(NULL))
  }
  updateSelectInput(
    session,
    paste0(prefix, "_loc_lim"),
    choices = if (identical(data_shape, "subgroup")) {
      spc_xbar_loc_lim_choices()
    } else {
      spc_individuals_loc_lim_choices()
    },
    selected = defs$loc_lim
  )
  disp_ch <- if (identical(data_shape, "replicate")) {
    spc_replicate_disp_lim_choices()
  } else {
    spc_disp_lim_choices_for_type(defs$disp_type, replicate_means = FALSE)
  }
  updateSelectInput(
    session,
    paste0(prefix, "_disp_lim"),
    choices = disp_ch,
    selected = defs$disp_lim
  )
  invisible(NULL)
}

ppa_normalize_response_cols <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }
  x <- as.character(x)
  x <- x[nzchar(x)]
  unique(x)
}

ppa_collect_mappings <- function(input, ns = NULL) {
  response_cols <- ppa_normalize_response_cols(input$ppa_response)
  stream_factors <- input$ppa_stream_factors
  if (is.null(stream_factors)) {
    stream_factors <- character(0)
  }
  shape_info <- ppa_resolve_measure_shape(
    response_cols,
    input$ppa_multiple_measures
  )
  list(
    response_cols = response_cols,
    response = shape_info$response,
    sample_id = input$ppa_sample_id %||% ppa_sample_time_order_id(),
    stream_factors = stream_factors,
    data_shape = shape_info$data_shape,
    measure_cols = shape_info$measure_cols,
    multiple_measures = input$ppa_multiple_measures
  )
}

ppa_prepare_from_inputs <- function(data, mappings) {
  if (is.null(data) || nrow(data) == 0) return(NULL)
  if (!".ppa_row_id" %in% names(data)) {
    data$.ppa_row_id <- seq_len(nrow(data))
  }
  ppa_prepare_data(
    data = data,
    response = mappings$response,
    measure_cols = mappings$measure_cols,
    sample_id = mappings$sample_id,
    stream_factors = mappings$stream_factors,
    data_shape = mappings$data_shape
  )
}

ppa_spec_from_inputs <- function(input, ns = NULL) {
  list(
    lsl = input$ppa_lsl,
    usl = input$ppa_usl,
    target = input$ppa_target
  )
}

ppa_format_indices_html <- function(result, spec, R = 4) {
  idx <- result$indices
  nc <- result$nonconforming
  diag <- result$diagnostics
  has_target <- ppa_has_target(spec)

  rows <- c(
    if (has_target) sprintf("<tr><td>Ppm</td><td>%s</td></tr>", ro(idx$ppm, R)) else "",
    sprintf("<tr><td>Ppk</td><td>%s</td></tr>", ro(idx$ppk, R)),
    sprintf("<tr><td>Pp</td><td>%s</td></tr>", ro(idx$pp, R)),
    sprintf("<tr><td>Pp (stream)</td><td>%s</td></tr>", ro(idx$pp_stream, R)),
    sprintf("<tr><td>Cp (potential)</td><td>%s</td></tr>", ro(idx$cp_potential, R))
  )
  if (has_target) {
    rows <- c(rows, sprintf(
      "<tr><td>%% Off-Target</td><td>%s%%</td></tr>",
      ro(diag$pct_off_target, R)
    ))
  }
  rows <- c(rows, sprintf(
    "<tr><td>%% Stream Loss</td><td>%s%%</td></tr>",
    ro(diag$pct_stream_loss, R)
  ))
  rows <- c(rows, sprintf(
    "<tr><td>Nonconforming</td><td>%d (%.0f ppm)</td></tr>",
    nc$total_out, nc$ppm
  ))

  paste0("<h4>Performance Measures</h4><table>", paste(rows, collapse = ""), "</table>")
}

.ppa_fmt_num <- function(x, digits = 4) {
  format(round(as.numeric(x), digits), nsmall = digits, trim = TRUE)
}

.ppa_fmt_int <- function(x) {
  format(as.integer(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

.ppa_html_pre_block <- function(title, lines) {
  paste0(
    "<h4>", title, "</h4>",
    "<pre style='margin:0 0 1em 0; white-space:pre; font-family:Consolas,Monaco,monospace; font-size:13px;'>",
    paste(lines, collapse = "\n"),
    "</pre>"
  )
}

ppa_format_descriptives_html <- function(descriptives, R = 4) {
  lines <- c(
    sprintf("           n = %s", .ppa_fmt_int(descriptives$n)),
    sprintf("        Mean = %s", .ppa_fmt_num(descriptives$mean, R)),
    sprintf("    Std. Dev = %s", .ppa_fmt_num(descriptives$sd, R)),
    sprintf("         Low = %s", .ppa_fmt_num(descriptives$low, R)),
    sprintf("          Q1 = %s", .ppa_fmt_num(as.numeric(descriptives$q1), R)),
    sprintf("      Median = %s", .ppa_fmt_num(descriptives$median, R)),
    sprintf("          Q3 = %s", .ppa_fmt_num(as.numeric(descriptives$q3), R)),
    sprintf("        High = %s", .ppa_fmt_num(descriptives$high, R)),
    sprintf("    Skewness = %s", .ppa_fmt_num(descriptives$skewness, 3)),
    sprintf("    Kurtosis = %s", .ppa_fmt_num(descriptives$kurtosis, 3))
  )
  .ppa_html_pre_block("Descriptives", lines)
}

ppa_format_variance_components_html <- function(
  variance,
  spec,
  R = 4,
  measurement = NULL,
  nested = NULL
) {
  has_target <- ppa_has_target(spec)
  comps <- variance$components
  pct <- variance$pct
  tau2 <- variance$tau2

  var_line <- function(label, value, percent) {
    sprintf(
      "%45s = %s %7s",
      label,
      .ppa_fmt_num(value, R),
      sprintf("%6.2f%%", round(as.numeric(percent), 2))
    )
  }

  total_label <- if (has_target) "Total Variance About Target" else "Total Variance"
  lines <- list(var_line(total_label, tau2, 100))

  key_order <- if (has_target) {
    c("Off-target", "Potential", "Process stream", "Time (control)")
  } else {
    c("Potential", "Process stream", "Time (control)")
  }
  display <- c(
    "Off-target" = "Off-target Variance",
    "Potential" = "Potential Variance",
    "Process stream" = "Process Stream Variance",
    "Time (control)" = "Time(Control) Variance"
  )

  for (key in key_order) {
    if (!key %in% names(comps)) {
      next
    }
    lines[[length(lines) + 1]] <- var_line(display[[key]], comps[[key]], pct[[key]])
    if (identical(key, "Potential") && !is.null(measurement)) {
      lines[[length(lines) + 1]] <- var_line(
        "Measurement Error within Potential",
        measurement$variance_measurement,
        measurement$pct_of_tau2
      )
      lines[[length(lines) + 1]] <- var_line(
        "Potential Variance Less Measurement",
        measurement$variance_product,
        measurement$pct_product_of_tau2
      )
    }
    if (
      identical(key, "Process stream") &&
        !is.null(nested) &&
        length(nested$between) > 0
    ) {
      # Hierarchy order: outermost → innermost
      for (fac in names(nested$between)) {
        lines[[length(lines) + 1]] <- var_line(
          paste("Process Stream Variance Between", fac),
          nested$between[[fac]],
          nested$pct[[fac]]
        )
      }
    }
  }

  .ppa_html_pre_block("Variance Components", unlist(lines))
}
