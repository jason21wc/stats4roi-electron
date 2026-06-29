# Shiny UI builders for optimization tab (no server reactives).

#' Fixed-width Loss / optimization layout: sidebar does not grow with viewport.
#' @keywords internal
optimizer_loss_layout_css <- function() {
  shiny::tags$style(shiny::HTML("
    .optimizer-loss-layout > .row {
      display: flex;
      flex-wrap: nowrap;
      margin-left: 0;
      margin-right: 0;
    }
    .optimizer-loss-layout .optimizer-loss-sidebar {
      flex: 0 0 280px;
      max-width: 280px;
      min-width: 260px;
      overflow-x: visible;
      padding-right: 12px;
    }
    .optimizer-loss-layout .optimizer-loss-main {
      flex: 1 1 auto;
      min-width: 0;
      max-width: 100%;
      overflow-x: auto;
      padding-left: 12px;
    }
    .optimizer-loss-layout .optimizer-loss-sidebar .form-group > label,
    .optimizer-loss-layout .optimizer-loss-sidebar .control-label {
      display: block;
      white-space: nowrap;
    }
    .optimizer-loss-layout .optimizer-loss-sidebar .optimizer-continuous-factor-label {
      white-space: normal;
      margin-bottom: 4px;
    }
    .optimizer-loss-layout .optimizer-loss-sidebar .optimizer-continuous-factor-picker .bootstrap-select,
    .optimizer-loss-layout .optimizer-loss-sidebar .optimizer-continuous-factor-picker .bootstrap-select > .dropdown-toggle {
      width: 100% !important;
      max-width: 100%;
    }
    .optimizer-loss-layout .optimizer-loss-sidebar .help-block,
    .optimizer-loss-layout .optimizer-loss-sidebar .shiny-help-block {
      white-space: normal;
    }
  "))
}

#' DT options for optimizer result tables (centered headers and cells).
#' @keywords internal
optimizer_dt_options <- function(extra = list()) {
  base <- list(
    dom = "t",
    paging = FALSE,
    columnDefs = list(list(className = "dt-center", targets = "_all"))
  )
  if (length(extra) < 1L) return(base)
  modifyList(base, extra)
}

#' Blocked-factor picker (marginalize / exclude from optimization search).
#' @keywords internal
optimizer_blocked_factor_picker <- function(ns, input_id) {
  shiny::tagList(
    shiny::tags$label(
      class = "control-label optimizer-continuous-factor-label",
      optimizer_label_with_help(
        "Blocked factors",
        paste(
          "Factors to exclude from optimization.",
          "Blocked factors stay in the ANOVA model on the Results tab, but their terms are dropped from the optimization prediction equations and the optimizer does not search over their settings.",
          "Predictions are made at the average over blocked levels (marginal model).",
          sep = " "
        )
      )
    ),
    shiny::tags$div(
      class = "optimizer-continuous-factor-picker",
      shinyWidgets::pickerInput(
        ns(input_id),
        label = NULL,
        choices = character(0),
        selected = character(0),
        multiple = TRUE,
        width = "100%",
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `none-selected-text` = "Select blocked factors (optional)"
        )
      )
    )
  )
}

#' Continuous-factor picker with label stacked above the control (sidebar-friendly).
#' @keywords internal
optimizer_continuous_factor_picker <- function(ns, input_id) {
  shiny::tagList(
    shiny::tags$label(class = "control-label optimizer-continuous-factor-label", "Continuous factors"),
    shiny::tags$div(
      class = "optimizer-continuous-factor-picker",
      shinyWidgets::pickerInput(
        ns(input_id),
        label = NULL,
        choices = character(0),
        selected = character(0),
        multiple = TRUE,
        width = "100%",
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `none-selected-text` = "Select continuous factors"
        )
      )
    )
  )
}

#' Hover help icon (browser tooltip via title attribute).
#' @keywords internal
optimizer_label_with_help <- function(label, help_text) {
  ht <- gsub('"', "&quot;", as.character(help_text), fixed = TRUE)
  htmltools::HTML(paste0(
    label,
    " <span class='glyphicon glyphicon-question-sign' style='cursor:help;color:#337ab7;margin-left:4px;' title=\"",
    ht,
    "\"></span>"
  ))
}

#' @keywords internal
optimizer_help_icon <- function(help_text) {
  ht <- gsub('"', "&quot;", as.character(help_text), fixed = TRUE)
  htmltools::HTML(paste0(
    "<span class='glyphicon glyphicon-question-sign' style='cursor:help;color:#337ab7;' title=\"",
    ht,
    "\"></span>"
  ))
}

#' @keywords internal
build_optimizer_normal_plot <- function(mu, sigma, target, lsl = NA_real_, usl = NA_real_, title = "") {
  mu <- suppressWarnings(as.numeric(mu))[1]
  sigma <- suppressWarnings(as.numeric(sigma))[1]
  target <- suppressWarnings(as.numeric(target))[1]
  lsl <- suppressWarnings(as.numeric(lsl))[1]
  usl <- suppressWarnings(as.numeric(usl))[1]

  if (!is.finite(mu) || !is.finite(sigma) || sigma <= 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::ggtitle(if (nzchar(title)) title else "Optimizer distribution") +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Distribution unavailable (sigma is not finite/positive).")
    )
  }

  refs <- c(mu - 4 * sigma, mu + 4 * sigma)
  if (is.finite(target)) refs <- c(refs, target)
  if (is.finite(lsl)) refs <- c(refs, lsl)
  if (is.finite(usl)) refs <- c(refs, usl)
  lo <- min(refs, na.rm = TRUE)
  hi <- max(refs, na.rm = TRUE)
  span <- hi - lo
  pad <- if (is.finite(span) && span > 0) 0.05 * span else 1
  lo <- lo - pad
  hi <- hi + pad
  x <- seq(lo, hi, length.out = 500L)
  d <- data.frame(x = x, dens = stats::dnorm(x, mean = mu, sd = sigma))

  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = dens)) +
    ggplot2::geom_line(color = "#2c7fb8", linewidth = 1) +
    ggplot2::geom_area(fill = "#9ecae1", alpha = 0.35) +
    ggplot2::labs(
      title = if (nzchar(title)) title else "Optimized normal distribution",
      x = "Output",
      y = "Density"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::coord_cartesian(xlim = c(lo, hi), clip = "off")

  if (is.finite(target)) {
    p <- p + ggplot2::geom_vline(xintercept = target, color = "#2ca25f", linetype = "dashed", linewidth = 0.9)
  }
  if (is.finite(lsl)) p <- p + ggplot2::geom_vline(xintercept = lsl, color = "#de2d26", linetype = "dotted", linewidth = 0.9)
  if (is.finite(usl)) p <- p + ggplot2::geom_vline(xintercept = usl, color = "#de2d26", linetype = "dotted", linewidth = 0.9)
  p
}

#' Dispersion resolution audit rows for optimizer detail/export tables.
#' @keywords internal
optimizer_dispersion_audit_rows <- function(r) {
  if (is.null(r)) {
    return(list(metrics = character(0), values = c()))
  }
  mets <- c("Dispersion EMM", "Effective dispersion", "Dispersion tier")
  emm <- if (!is.null(r$disp_pred)) r$disp_pred else NA_real_
  eff <- if (!is.null(r$disp_effective)) r$disp_effective else emm
  dr <- r$dispersion_resolution
  tier_lbl <- if (!is.null(dr) && !is.null(dr$audit_label)) as.character(dr$audit_label)[1] else NA_character_
  vals <- c(emm, eff, tier_lbl)
  if (!is.null(dr) && is.finite(suppressWarnings(as.numeric(dr$cell_raw)))) {
    mets <- c(mets, "Cell dispersion")
    vals <- c(vals, as.numeric(dr$cell_raw))
  }
  if (!is.null(dr) && is.finite(suppressWarnings(as.numeric(dr$delta_used)))) {
    mets <- c(mets, "Resolution delta used")
    vals <- c(vals, as.numeric(dr$delta_used))
  }
  list(metrics = mets, values = vals)
}

#' Build optimizer detail/export metric rows (factors, per-response audit when aggregated).
#' @keywords internal
build_optimizer_detail_rows <- function(r) {
  if (is.null(r) || !isTRUE(r$ok)) {
    return(list(metrics = character(0), values = character(0)))
  }
  mets <- names(r$par)
  vals <- as.numeric(r$par)
  per_response <- !is.null(r$aggregate) && is.list(r$aggregate) &&
    !is.null(r$aggregate$details) && length(r$aggregate$details) >= 1L
  if (!per_response) {
    mets <- c(mets, "Predicted mean", "Predicted sigma")
    vals <- c(vals, r$mu, r$sigma)
    disp_audit <- optimizer_dispersion_audit_rows(r)
    mets <- c(mets, disp_audit$metrics)
    vals <- c(vals, disp_audit$values)
  }
  if (!is.null(r$optimize_target)) {
    mets <- c(mets, "Optimization target", "Production volume", "Unit settings cost", "Total settings cost")
    vals <- c(vals, as.character(r$optimize_target), as.numeric(r$volume), as.numeric(r$unit_setting_cost), as.numeric(r$total_setting_cost))
  }
  if (!is.null(r$objective_breakdown) && is.list(r$objective_breakdown)) {
    ob <- r$objective_breakdown
    mets <- c(
      mets,
      "Objective breakdown: weighted base loss sum",
      "Objective breakdown: weighted loss sum",
      "Objective breakdown: weighted PPM sum",
      "Objective breakdown: objective loss",
      "Objective breakdown: unit settings cost",
      "Objective breakdown: total settings cost",
      "Objective breakdown: final objective"
    )
    vals <- c(
      vals,
      as.numeric(ob$weighted_base_loss_sum),
      as.numeric(ob$weighted_loss_sum),
      as.numeric(ob$weighted_ppm_sum),
      as.numeric(ob$objective_loss),
      as.numeric(ob$unit_setting_cost),
      as.numeric(ob$total_setting_cost),
      as.numeric(ob$final_objective)
    )
  }
  if (per_response) {
    mets <- c(mets, "Aggregated responses", "Total expected Taguchi losses (all responses)", "Total PPM (all responses)")
    vals <- c(vals, as.integer(r$aggregate$response_count), r$aggregate$total_expected_loss, r$aggregate$total_ppm)
    det_rows <- optimizer_response_detail_rows(r)
    mets <- c(mets, det_rows$metrics)
    vals <- c(vals, det_rows$values)
  }
  if (!is.null(r$par_snapped) && nrow(r$par_snapped) == 1L) {
    snap_parts <- vapply(names(r$par_snapped), function(nm) {
      paste0(nm, "=", as.character(r$par_snapped[[nm]][1L]))
    }, character(1))
    mets <- c(mets, "Factor levels used for prediction (nearest design levels)")
    vals <- c(vals, paste(snap_parts, collapse = "; "))
  }
  if (!is.null(r$optimization_mode) && nzchar(as.character(r$optimization_mode))) {
    mets <- c(mets, "Continuous optimization mode")
    vals <- c(vals, as.character(r$optimization_mode))
  }
  if (!per_response) {
    mets <- c(mets, "Loss lower", "Loss upper", "Total expected Taguchi losses", "PPM lower", "PPM upper", "PPM total")
    met_vals <- if (!is.null(r$metrics) && is.list(r$metrics)) r$metrics else list()
    vals <- c(
      vals,
      suppressWarnings(as.numeric(met_vals$loss_lower))[1],
      suppressWarnings(as.numeric(met_vals$loss_upper))[1],
      suppressWarnings(as.numeric(met_vals$expected_loss))[1],
      suppressWarnings(as.numeric(met_vals$ppm_lower))[1],
      suppressWarnings(as.numeric(met_vals$ppm_upper))[1],
      suppressWarnings(as.numeric(met_vals$ppm))[1]
    )
  }
  cap_rows <- optimizer_capability_result_rows(r$capability)
  mets <- c(mets, cap_rows$metrics)
  vals <- c(vals, cap_rows$values)
  list(metrics = mets, values = vals)
}

#' @keywords internal
optimizer_capability_result_rows <- function(capability) {
  if (is.null(capability) || !is.list(capability)) {
    return(list(metrics = character(0), values = numeric(0)))
  }
  list(
    metrics = c("Cp", "Cpk", "Cpm"),
    values = c(
      if (!is.null(capability$cp)) capability$cp else NA_real_,
      if (!is.null(capability$cpk)) capability$cpk else NA_real_,
      if (!is.null(capability$cpm)) capability$cpm else NA_real_
    )
  )
}

#' @keywords internal
build_optimizer_summary_table_ui <- function(
    mu,
    sigma,
    expected_loss,
    ppm,
    volume,
    unit_setting_cost,
    target = NA_real_,
    lsl = NA_real_,
    usl = NA_real_,
    digits = 4L) {
  mu <- suppressWarnings(as.numeric(mu))[1]
  sigma <- suppressWarnings(as.numeric(sigma))[1]
  expected_loss <- suppressWarnings(as.numeric(expected_loss))[1]
  ppm <- suppressWarnings(as.numeric(ppm))[1]
  volume <- suppressWarnings(as.numeric(volume))[1]
  unit_setting_cost <- suppressWarnings(as.numeric(unit_setting_cost))[1]
  if (!is.finite(volume) || volume <= 0) volume <- 1
  if (!is.finite(unit_setting_cost)) unit_setting_cost <- 0
  if (!is.finite(ppm)) ppm <- NA_real_
  if (!is.finite(expected_loss)) expected_loss <- NA_real_

  total_loss_at_volume <- if (is.finite(expected_loss)) expected_loss * volume else NA_real_
  total_nonconforming_at_volume <- if (is.finite(ppm)) (ppm / 1e6) * volume else NA_real_
  loss_u <- if (is.finite(expected_loss)) expected_loss else NA_real_
  settings_u <- if (is.finite(unit_setting_cost)) unit_setting_cost else NA_real_
  total_cost_at_volume <- if (is.finite(loss_u) || is.finite(settings_u)) {
    volume * ((if (is.finite(loss_u)) loss_u else 0) + (if (is.finite(settings_u)) settings_u else 0))
  } else {
    NA_real_
  }

  fmt <- function(x) {
    if (!is.finite(suppressWarnings(as.numeric(x)))) return("NA")
    format(round(as.numeric(x), digits = as.integer(digits)), nsmall = as.integer(digits), trim = TRUE, scientific = FALSE)
  }

  cap <- optimizer_capability_measures(mu = mu, sigma = sigma, target = target, lsl = lsl, usl = usl)

  rows <- list(
    c("Estimated \u03bc", fmt(mu)),
    c("Estimated \u03c3", fmt(sigma)),
    c("Cp", fmt(cap$cp)),
    c("Cpk", fmt(cap$cpk)),
    c("Cpm", fmt(cap$cpm)),
    c("Setting Cost", fmt(unit_setting_cost)),
    c("Taguchi Loss per Unit", fmt(expected_loss)),
    c("Total Taguchi Loss at Volume", fmt(total_loss_at_volume)),
    c("Total cost at volume (Taguchi loss + part cost)", fmt(total_cost_at_volume)),
    c("Total ppm Nonconforming", fmt(ppm)),
    c("Total Nonconforming at Volume", fmt(total_nonconforming_at_volume))
  )

  shiny::tags$table(
    class = "table table-condensed table-striped",
    shiny::tags$thead(
      shiny::tags$tr(
        shiny::tags$th(style = "text-align:center; width: 55%;", "Metric"),
        shiny::tags$th(style = "text-align:center;", "Value")
      )
    ),
    shiny::tags$tbody(
      lapply(rows, function(rw) {
        shiny::tags$tr(
          shiny::tags$td(style = "text-align:center;", rw[[1]]),
          shiny::tags$td(style = "text-align:center;", rw[[2]])
        )
      })
    )
  )
}

#' @keywords internal
build_optimizer_settings_table_ui <- function(par, digits = 4L, display_values = NULL) {
  if (is.null(par) || length(par) < 1L) {
    return(shiny::tags$p(class = "text-muted", "No optimum factor settings."))
  }
  pv <- suppressWarnings(as.numeric(par))
  pn <- names(par)
  if (length(pv) < 1L) {
    return(shiny::tags$p(class = "text-muted", "No optimum factor settings."))
  }
  if (is.null(pn) || length(pn) != length(pv)) pn <- rep.int("", length(pv))
  fmt <- function(x) {
    if (!is.finite(suppressWarnings(as.numeric(x)))) return("NA")
    format(round(as.numeric(x), digits = as.integer(digits)), nsmall = as.integer(digits), trim = TRUE, scientific = FALSE)
  }
  shiny::tagList(
    shiny::tags$h5(style = "margin-top: 0.5em; margin-bottom: 0.35em;", "Optimum factor settings"),
    shiny::tags$table(
      class = "table table-condensed table-striped",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th(style = "text-align:center; width: 55%;", "Setting"),
          shiny::tags$th(style = "text-align:center;", "Value")
        )
      ),
      shiny::tags$tbody(
        lapply(seq_along(pv), function(i) {
          lab <- pn[i]
          if (is.null(lab) || !nzchar(as.character(lab))) lab <- paste0("Factor ", i)
          show_val <- if (!is.null(display_values) && lab %in% names(display_values)) {
            as.character(display_values[[lab]])
          } else {
            fmt(pv[i])
          }
          shiny::tags$tr(
            shiny::tags$td(style = "text-align:center;", as.character(lab)),
            shiny::tags$td(style = "text-align:center;", show_val)
          )
        })
      )
    )
  )
}

#' Actual process-value inputs for continuous factors on the Loss optimizer tab.
#' @keywords internal
build_loss_opt_actual_values_ui <- function(ns, cont_vars, data, model_factor_names, input = NULL) {
  cont_vars <- unique(as.character(cont_vars))
  cont_vars <- cont_vars[nzchar(cont_vars)]
  if (length(cont_vars) < 1L) return(NULL)
  shiny::tags$div(
    class = "loss-opt-actual-values",
    shiny::tags$p(
      class = "text-muted",
      "Optional: enter actual process values for continuous factors (comma-separated, in coded-level order) to display settings as coded (actual), like the Results EMM generator."
    ),
    shiny::tagList(lapply(cont_vars, function(fnm) {
      coded_levels <- multifactor_factor_numeric_levels(data, fnm)
      level_hint <- if (length(coded_levels) > 0L) paste(coded_levels, collapse = ", ") else "n/a"
      prefill <- ""
      if (!is.null(input) && !is.null(model_factor_names)) {
        idx <- match(fnm, model_factor_names)
        if (!is.na(idx)) {
          gkey <- paste0("ems_factor_actual", idx)
          if (!is.null(input[[gkey]]) && nzchar(trimws(as.character(input[[gkey]])))) {
            prefill <- as.character(input[[gkey]])
          }
        }
      }
      safe <- make.names(fnm)
      shiny::textInput(
        inputId = ns(paste0("loss_mf_opt_actual__", safe)),
        label = paste0("Actual values for ", fnm, " (level order: ", level_hint, ")"),
        value = prefill,
        placeholder = if (length(coded_levels) == 2L) "e.g. 8, 12" else "e.g. 8, 12, 16",
        width = "100%"
      )
    }))
  )
}

#' Static single-response Taguchi economics (Loss sidebar; always in DOM).
#' @keywords internal
loss_mf_static_economics_ui <- function(ns) {
  shiny::tagList(
    shiny::tags$p(
      class = "text-muted",
      style = "margin-bottom: 0.5em;",
      shiny::tags$strong("Taguchi economics")
    ),
    shiny::numericInput(ns("loss_mf_target"), "Target (T)", value = NA_real_, width = "100%"),
    shiny::numericInput(
      ns("loss_mf_C_l"),
      "Lower-side cost at LSL (C_lower)",
      value = 1,
      min = 0,
      step = 0.01,
      width = "100%"
    ),
    shiny::numericInput(
      ns("loss_mf_C_u"),
      "Upper-side cost at USL (C_upper)",
      value = 1,
      min = 0,
      step = 0.01,
      width = "100%"
    ),
    shiny::numericInput(ns("loss_mf_lsl"), "LSL", value = NA_real_, width = "100%"),
    shiny::numericInput(ns("loss_mf_usl"), "USL", value = NA_real_, width = "100%")
  )
}

#' Single-response Taguchi economics inputs (Loss tab sidebar).
#' @keywords internal
build_single_response_economics_ui <- function(ns, response_name, economics, input_prefix = "loss_mf") {
  econ <- if (is.null(economics)) opt_economics_defaults() else economics
  shiny::tagList(
    shiny::tags$p(
      class = "text-muted",
      style = "margin-bottom: 0.5em;",
      shiny::tags$strong("Taguchi economics")
    ),
    shiny::tags$p(class = "text-muted", style = "margin-bottom: 0.5em;", shiny::tags$strong(response_name)),
    shiny::numericInput(ns(paste0(input_prefix, "_target")), "Target (T)", value = econ$target, width = "100%"),
    shiny::numericInput(
      ns(paste0(input_prefix, "_C_l")),
      "Lower-side cost at LSL (C_lower)",
      value = econ$C_l,
      min = 0,
      step = 0.01,
      width = "100%"
    ),
    shiny::numericInput(
      ns(paste0(input_prefix, "_C_u")),
      "Upper-side cost at USL (C_upper)",
      value = econ$C_u,
      min = 0,
      step = 0.01,
      width = "100%"
    ),
    shiny::numericInput(ns(paste0(input_prefix, "_lsl")), "LSL", value = econ$lsl, width = "100%"),
    shiny::numericInput(ns(paste0(input_prefix, "_usl")), "USL", value = econ$usl, width = "100%")
  )
}

#' Per-response economics panel for Loss tab (multi-response).
#' @keywords internal
build_per_response_economics_ui <- function(
    ns,
    response_names,
    economics_by_name,
    input_prefix = "loss_mf_econ__",
    selected_tab = NULL) {
  if (length(response_names) < 2L) return(NULL)
  tabs <- lapply(seq_along(response_names), function(i) {
    rn <- response_names[[i]]
    safe_id <- gsub("[^A-Za-z0-9_]", "_", rn)
    econ <- if (!is.null(economics_by_name[[rn]])) economics_by_name[[rn]] else opt_economics_defaults()
    shiny::tabPanel(
      title = rn,
      value = rn,
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::numericInput(ns(paste0(input_prefix, safe_id, "_target")), "Target (T)", value = econ$target, width = "100%")
        ),
        shiny::column(
          4,
          shiny::numericInput(ns(paste0(input_prefix, safe_id, "_lsl")), "LSL", value = econ$lsl, width = "100%")
        ),
        shiny::column(
          4,
          shiny::numericInput(ns(paste0(input_prefix, safe_id, "_usl")), "USL", value = econ$usl, width = "100%")
        )
      ),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::numericInput(ns(paste0(input_prefix, safe_id, "_C_l")), "C_l (loss at LSL)", value = econ$C_l, width = "100%")
        ),
        shiny::column(
          6,
          shiny::numericInput(ns(paste0(input_prefix, safe_id, "_C_u")), "C_u (loss at USL)", value = econ$C_u, width = "100%")
        )
      )
    )
  })
  tab_args <- c(list(id = ns("loss_mf_economics_tabs")), tabs)
  if (!is.null(selected_tab) && nzchar(as.character(selected_tab)[1L])) {
    tab_args$selected <- as.character(selected_tab)[1L]
  }
  shiny::tagList(
    shiny::tags$p(
      class = "text-muted",
      style = "margin-bottom: 0.5em;",
      shiny::tags$strong("Taguchi economics (per response)")
    ),
    do.call(shiny::tabsetPanel, tab_args)
  )
}

#' Collect per-response economics from Shiny inputs.
#' @keywords internal
collect_per_response_economics <- function(input, response_names, input_prefix = "loss_mf_econ__") {
  out <- list()
  for (rn in response_names) {
    safe_id <- gsub("[^A-Za-z0-9_]", "_", rn)
    out[[rn]] <- opt_economics_from_scalars(
      target = input[[paste0(input_prefix, safe_id, "_target")]],
      lsl = input[[paste0(input_prefix, safe_id, "_lsl")]],
      usl = input[[paste0(input_prefix, safe_id, "_usl")]],
      C_l = input[[paste0(input_prefix, safe_id, "_C_l")]],
      C_u = input[[paste0(input_prefix, safe_id, "_C_u")]]
    )
  }
  out
}

#' Tier-3-only resolution controls (sidebar); hidden unless policy 3 is active.
#' @keywords internal
build_tier3_resolution_controls_ui <- function(
    ns,
    response_name,
    ctx,
    confidence,
    input_prefix = "loss_mf_disp_delta__") {
  if (is.null(ctx)) ctx <- list(delta_min = NA_real_, n_mean = NA_real_, delta_recommended = NA_real_)
  safe <- gsub("[^A-Za-z0-9_]", "_", response_name)
  delta_val <- if (is.finite(ctx$delta_user)) ctx$delta_user else ctx$delta_recommended
  if (!is.finite(delta_val)) delta_val <- 0
  help_txt <- paste(
    "Policy 3 uses a worst-case gauge-resolution bound:",
    paste0("P(max - min < Delta_min) = ", confidence, " (Results confidence),"),
    "with n_bar mean replicates per cell for the recommended prefill.",
    sep = " "
  )
  shiny::tagList(
    shiny::tags$p(
      class = "text-muted",
      style = "margin-bottom: 0.35em;",
      optimizer_label_with_help("Gauge resolution prior (Policy 3)", help_txt)
    ),
    shiny::tags$p(
      class = "text-muted",
      style = "font-size: 0.9em; margin-bottom: 0.25em;",
      shiny::tags$strong(response_name)
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::tags$p(
          class = "text-muted",
          style = "margin: 0;",
          "Delta_min:",
          if (is.finite(ctx$delta_min)) signif(ctx$delta_min, 4) else "n/a"
        )
      ),
      shiny::column(
        6,
        shiny::tags$p(
          class = "text-muted",
          style = "margin: 0;",
          "n_bar:",
          if (is.finite(ctx$n_mean)) signif(ctx$n_mean, 3) else "n/a"
        )
      )
    ),
    shiny::numericInput(
      ns(paste0(input_prefix, safe)),
      "Resolution delta",
      value = delta_val,
      min = 0,
      step = 0.0001,
      width = "100%"
    ),
    if (is.finite(ctx$delta_recommended)) {
      shiny::tags$p(
        class = "text-muted",
        style = "font-size: 0.85em;",
        paste0("Recommended delta: ", signif(ctx$delta_recommended, 4))
      )
    } else {
      shiny::tags$p(
        class = "text-warning",
        style = "font-size: 0.85em;",
        "Constant response: set resolution delta manually."
      )
    }
  )
}

#' @keywords internal
build_single_resolution_prior_ui <- function(
    ns,
    response_name,
    ctx,
    confidence,
    input_prefix = "loss_mf_disp_delta__") {
  build_tier3_resolution_controls_ui(ns, response_name, ctx, confidence, input_prefix = input_prefix)
}

#' Per-response policy lines for the Loss tab main panel.
#' @keywords internal
build_dispersion_policy_messages_ui <- function(policy_lines) {
  if (length(policy_lines) < 1L) {
    return(NULL)
  }
  shiny::tags$div(
    class = "loss-dispersion-policy",
    shiny::tags$p(
      class = "text-muted",
      style = "margin-bottom: 0.35em;",
      shiny::tags$strong("Dispersion policy by response")
    ),
    shiny::tags$ul(
      style = "padding-left: 1.2em; margin-bottom: 0.5em;",
      lapply(policy_lines, function(line) {
        shiny::tags$li(
          class = if (grepl("Policy 3", line, fixed = TRUE)) "text-warning" else "text-muted",
          style = "font-size: 0.92em; margin-bottom: 0.2em;",
          line
        )
      })
    )
  )
}

#' Per-response tier-3 controls (multi-response); only responses that need policy 3.
#' @keywords internal
build_per_response_resolution_prior_ui <- function(
    ns,
    response_names,
    ctx_by_name,
    confidence,
    tier3_flags,
    selected_tab = NULL,
    input_prefix = "loss_mf_disp_delta__") {
  tier3_names <- response_names[isTRUE(tier3_flags[response_names])]
  if (length(tier3_names) < 1L) {
    return(NULL)
  }
  if (length(tier3_names) == 1L) {
    rn <- tier3_names[[1L]]
    return(build_tier3_resolution_controls_ui(
      ns,
      rn,
      if (!is.null(ctx_by_name[[rn]])) ctx_by_name[[rn]] else list(),
      confidence,
      input_prefix = input_prefix
    ))
  }
  tabs <- lapply(tier3_names, function(rn) {
    ctx <- if (!is.null(ctx_by_name[[rn]])) ctx_by_name[[rn]] else list()
    shiny::tabPanel(
      title = rn,
      value = rn,
      build_tier3_resolution_controls_ui(ns, rn, ctx, confidence, input_prefix = input_prefix)
    )
  })
  tab_args <- c(list(id = ns("loss_mf_resolution_tabs")), tabs)
  if (!is.null(selected_tab) && nzchar(as.character(selected_tab)[1L]) && selected_tab %in% tier3_names) {
    tab_args$selected <- as.character(selected_tab)[1L]
  } else {
    tab_args$selected <- tier3_names[[1L]]
  }
  shiny::tagList(
    shiny::tags$p(class = "text-muted", style = "margin-bottom: 0.35em;", shiny::tags$strong("Gauge resolution prior (Policy 3 only)")),
    do.call(shiny::tabsetPanel, tab_args)
  )
}

#' Wide top-N loss table across multiple responses (factor settings + per-response metrics).
#' @return Data frame, or \code{NULL} when fewer than two responses succeed.
#' @keywords internal
assemble_multiresponse_loss_top_table <- function(results_by_response, n_top = 5L) {
  if (is.null(results_by_response) || length(results_by_response) < 2L) {
    return(NULL)
  }
  metric_cols <- c(
    "method", "mu_pred", "se_mu", "disp_pred", "disp_emm", "disp_cell",
    "disp_effective", "disp_tier", "delta_used", "sigma",
    "loss_lower", "loss_upper", "expected_loss",
    "ppm_lower", "ppm_upper", "ppm", "n_obs"
  )
  ok_names <- names(results_by_response)[vapply(results_by_response, function(r) {
    isTRUE(r$ok) && is.data.frame(r$table) && nrow(r$table) >= 1L
  }, logical(1))]
  if (length(ok_names) < 2L) {
    return(NULL)
  }
  factor_cols_list <- lapply(ok_names, function(rn) {
    tbl <- results_by_response[[rn]]$table
    setdiff(names(tbl), metric_cols)
  })
  factor_cols <- sort(unique(unlist(factor_cols_list)))
  if (is.null(factor_cols) || length(factor_cols) < 1L) {
    return(NULL)
  }
  pad_factor_cols <- function(tbl, fc) {
    if (!is.data.frame(tbl) || length(fc) < 1L) return(tbl)
    miss <- setdiff(fc, names(tbl))
    if (length(miss) > 0L) {
      for (vn in miss) tbl[[vn]] <- NA
    }
    tbl
  }
  base_rn <- ok_names[[1L]]
  merged <- pad_factor_cols(results_by_response[[base_rn]]$table, factor_cols)
  merged <- merged[, c(factor_cols, "mu_pred", "sigma", "expected_loss"), drop = FALSE]
  names(merged)[names(merged) == "mu_pred"] <- paste0("Mean (", base_rn, ")")
  names(merged)[names(merged) == "sigma"] <- paste0("Sigma (", base_rn, ")")
  names(merged)[names(merged) == "expected_loss"] <- paste0("Loss (", base_rn, ")")
  for (rn in ok_names[-1L]) {
    sub <- pad_factor_cols(results_by_response[[rn]]$table, factor_cols)
    keep_cols <- intersect(c(factor_cols, "mu_pred", "sigma", "expected_loss"), names(sub))
    sub <- sub[, keep_cols, drop = FALSE]
    names(sub)[names(sub) == "mu_pred"] <- paste0("Mean (", rn, ")")
    names(sub)[names(sub) == "sigma"] <- paste0("Sigma (", rn, ")")
    names(sub)[names(sub) == "expected_loss"] <- paste0("Loss (", rn, ")")
    merged <- merge(merged, sub, by = factor_cols, all = TRUE, sort = FALSE)
  }
  loss_cols <- grep("^Loss \\(", names(merged), value = TRUE)
  if (length(loss_cols) < 1L) {
    return(NULL)
  }
  merged$`Total loss (all responses)` <- rowSums(merged[, loss_cols, drop = FALSE], na.rm = TRUE)
  merged <- merged[order(merged$`Total loss (all responses)`, na.last = TRUE), , drop = FALSE]
  if (nrow(merged) > as.integer(n_top)) {
    merged <- merged[seq_len(as.integer(n_top)), , drop = FALSE]
  }
  merged
}

#' Human-readable notes for responses excluded from a multi-response loss table.
#' @keywords internal
loss_multiresponse_assembly_notes <- function(results_by_response) {
  if (is.null(results_by_response) || length(results_by_response) < 1L) {
    return(character(0))
  }
  ok_names <- names(results_by_response)[vapply(results_by_response, function(r) {
    isTRUE(r$ok) && is.data.frame(r$table) && nrow(r$table) >= 1L
  }, logical(1))]
  failed <- setdiff(names(results_by_response), ok_names)
  if (length(failed) < 1L) {
    return(character(0))
  }
  vapply(failed, function(rn) {
    r <- results_by_response[[rn]]
    msg <- if (!is.null(r$message) && nzchar(as.character(r$message)[1])) {
      as.character(r$message)[1]
    } else {
      "Loss grid could not be computed."
    }
    paste0(rn, ": ", msg)
  }, character(1))
}

#' Response names and picker choices for Loss economics editor.
#' @keywords internal
loss_economics_picker_choices <- function(d, dids) {
  dids <- suppressWarnings(as.integer(dids))
  dids <- dids[is.finite(dids)]
  if (length(dids) < 1L) return(stats::setNames(character(0), character(0)))
  labels <- vapply(dids, function(di) {
    if (!is.null(d) && di >= 1L && di <= ncol(d)) names(d)[di] else as.character(di)
  }, character(1))
  stats::setNames(as.character(dids), labels)
}

#' Resolve selectInput value for Loss economics picker (values are column indices).
#' @keywords internal
loss_economics_picker_selected <- function(choices, current_did = NULL) {
  vals <- unname(choices)
  if (length(vals) < 1L) return("")
  cur <- suppressWarnings(as.integer(current_did)[1L])
  cur_chr <- as.character(cur)
  if (length(cur_chr) == 1L && nzchar(cur_chr) && is.finite(cur) && cur_chr %in% vals) {
    return(cur_chr)
  }
  vals[[1L]]
}

#' Read resolution delta for a response (session, then static input).
#' @keywords internal
mf_get_resolution_delta_for_response <- function(session, input, response_key, response_name = NULL) {
  stored <- opt_session_get_resolution_delta(session, response_key)
  if (!is.null(stored) && is.finite(stored)) return(stored)
  if (!is.null(response_name)) {
    legacy <- mf_get_resolution_delta_user(input, response_name)
    if (!is.null(legacy) && is.finite(legacy)) return(legacy)
  }
  if (!is.null(input$loss_mf_disp_delta)) {
    val <- suppressWarnings(as.numeric(input$loss_mf_disp_delta))
    if (length(val) == 1L && is.finite(val)) return(val)
  }
  NULL
}

#' Modal numeric input id for factor-level cost editor.
#' @keywords internal
modal_cost_input_id <- function(cost_id_prefix, factor_name, idx) {
  paste0(cost_id_prefix, gsub("[^A-Za-z0-9_]", "_", factor_name), "__", as.integer(idx))
}

#' Build modal UI for per-factor level unit costs (multifactor / oneway loss tabs).
#' @keywords internal
build_factor_cost_modal_ui <- function(
    d,
    factor_names,
    cont_vars,
    existing_costs_tbl,
    ns,
    cost_id_prefix = "loss_mf_cost__") {
  factor_names <- unique(as.character(factor_names))
  factor_names <- factor_names[!is.na(factor_names) & nzchar(factor_names)]
  cont_vars <- unique(as.character(cont_vars))
  if (length(cont_vars) < 1L) cont_vars <- character(0)
  ex_map <- list()
  if (!is.null(existing_costs_tbl) && nrow(existing_costs_tbl) > 0L) {
    for (i in seq_len(nrow(existing_costs_tbl))) {
      ex_map[[paste0(existing_costs_tbl$factor[[i]], ":", existing_costs_tbl$level[[i]])]] <- existing_costs_tbl$cost[[i]]
    }
  }
  meta <- list()
  blocks <- list()
  for (fn in factor_names) {
    lv <- suppressWarnings(as.numeric(as.character(d[[fn]])))
    lv <- sort(unique(lv[is.finite(lv)]))
    if (length(lv) < 1L) next
    mode_lbl <- if (isTRUE(fn %in% cont_vars)) {
      "Continuous (linear interpolation)"
    } else {
      "Discrete (exact entered levels)"
    }
    items <- list(shiny::tags$h5(paste0(fn, " — ", mode_lbl)))
    for (j in seq_along(lv)) {
      id <- modal_cost_input_id(cost_id_prefix, fn, j)
      key <- paste0(fn, ":", lv[[j]])
      def <- suppressWarnings(as.numeric(ex_map[[key]]))
      if (length(def) != 1L || !is.finite(def)) def <- NA_real_
      meta[[length(meta) + 1L]] <- list(id = id, factor = fn, level = lv[[j]])
      items[[length(items) + 1L]] <- shiny::numericInput(
        inputId = ns(id),
        label = paste0("Level ", lv[[j]]),
        value = def,
        min = 0,
        step = 0.01,
        width = "100%"
      )
    }
    blocks[[length(blocks) + 1L]] <- do.call(shiny::tagList, c(items, list(shiny::tags$hr())))
  }
  list(
    meta = meta,
    ui = if (length(blocks) > 0L) {
      shiny::tagList(blocks)
    } else {
      shiny::tags$p("No numeric tested levels found for selected factors.")
    }
  )
}

#' Taguchi loss grid disclaimer with MathJax (C_l, C_u subscripts and related symbols).
#' @param kind `"multifactor"` or `"oneway"`.
#' @keywords internal
taguchi_loss_disclaimer_ui <- function(kind = c("multifactor", "oneway")) {
  kind <- match.arg(kind)
  if (identical(kind, "oneway")) {
    return(shiny::tags$p(
      class = "text-muted",
      "Normal surrogate; side-specific one-sided loss/ppm with independent ",
      withMathJax("$C_l/C_u$"),
      ". Single-factor reduced model with exact normal second-moment loss scaling."
    ))
  }
  shiny::tags$p(
    class = "text-muted",
    "Normal surrogate; side-specific one-sided loss/ppm with independent ",
    withMathJax("$C_l/C_u$"),
    ". Uses exact normal second moment ",
    withMathJax("$((\\mu-T)^2 + \\sigma^2)$"),
    " with side-specific scaling; total loss is the sum of active-side contributions; ",
    withMathJax("$\\sigma$"),
    " from predicted dispersion ",
    withMathJax("$(\\sqrt{\\pi/2} \\times \\mathrm{metric})$"),
    "; refine in Specify if needed."
  )
}
