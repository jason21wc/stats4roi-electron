# Distribution Fitting server wiring

source("modules/statistical/spc/utils/dfit_analyze.R")
source("modules/statistical/spc/utils/dfit_graphics.R")
source("modules/statistical/spc/utils/dfit_calculator.R")
source("modules/distributions/distribution_testing/functions/exponential_test_functions.R")

dfit_fmt <- function(x, digits = 4L) {
  if (is.null(x) || length(x) == 0L || !is.finite(x)) {
    return("")
  }
  format(round(x, digits), nsmall = digits, trim = TRUE)
}

dfit_row_html <- function(label, value) {
  paste0(
    "<tr><td style='padding:4px 8px;border:1px solid #ddd;'>", label,
    "</td><td style='padding:4px 8px;border:1px solid #ddd;text-align:right;'>",
    value, "</td></tr>"
  )
}

source("modules/statistical/spc/utils/dfit_gof_helpers.R")

dfit_build_summary_html <- function(result, decimals = 4L, gof_rows = NULL) {
  if (is.null(result)) {
    return("<p>No analysis results.</p>")
  }
  d <- result$descriptives
  c <- result$conformance
  f <- result$fit
  cap <- result$capability
  R <- decimals

  out <- "<table style='border-collapse:collapse;width:100%;max-width:520px;'>"
  out <- paste0(out, "<tr><th colspan='2' style='text-align:left;padding:6px;'>Descriptives</th></tr>")
  out <- paste0(out, dfit_row_html("n", d$n))
  out <- paste0(out, dfit_row_html("Mean", dfit_fmt(d$mean, R)))
  out <- paste0(out, dfit_row_html("Median", dfit_fmt(d$median, R)))
  out <- paste0(out, dfit_row_html("Std Dev", dfit_fmt(d$sd, R)))
  out <- paste0(out, dfit_row_html("Std Dev(pot)", dfit_fmt(d$sd_potential, R)))
  out <- paste0(out, dfit_row_html("Low", dfit_fmt(d$low, R)))
  out <- paste0(out, dfit_row_html("High", dfit_fmt(d$high, R)))
  out <- paste0(out, dfit_row_html("Range", dfit_fmt(d$range, R)))

  if (is.finite(c$lsl) || is.finite(c$usl)) {
    out <- paste0(out, "<tr><th colspan='2' style='text-align:left;padding:6px;'>Conformance</th></tr>")
    if (is.finite(c$lsl)) out <- paste0(out, dfit_row_html("LSL", dfit_fmt(c$lsl, R)))
    if (is.finite(c$target)) out <- paste0(out, dfit_row_html("Target", dfit_fmt(c$target, R)))
    if (is.finite(c$usl)) out <- paste0(out, dfit_row_html("USL", dfit_fmt(c$usl, R)))
    if (is.finite(c$n_below)) out <- paste0(out, dfit_row_html("n Below Spec", c$n_below))
    if (is.finite(c$n_above)) out <- paste0(out, dfit_row_html("n Above Spec", c$n_above))
    if (is.finite(c$n_out)) out <- paste0(out, dfit_row_html("Total n Out of Spec", c$n_out))
    if (is.finite(c$pct_below)) out <- paste0(out, dfit_row_html("% Below Spec", dfit_fmt(c$pct_below, 3)))
    if (is.finite(c$pct_above)) out <- paste0(out, dfit_row_html("% Above Spec", dfit_fmt(c$pct_above, 3)))
    if (is.finite(c$pct_out)) out <- paste0(out, dfit_row_html("% Out of Spec", dfit_fmt(c$pct_out, 3)))
    if (is.finite(c$pct_off_target)) {
      out <- paste0(out, dfit_row_html("% Off-Target", dfit_fmt(c$pct_off_target, 2)))
    }
  }

  if (!is.null(f) && f$distribution_id > 0L) {
    out <- paste0(out, "<tr><th colspan='2' style='text-align:left;padding:6px;'>", f$distribution_label, "</th></tr>")
    out <- paste0(out, dfit_row_html("LPL", dfit_fmt(f$lpl, R)))
    out <- paste0(out, dfit_row_html("UPL", dfit_fmt(f$upl, R)))
    out <- paste0(out, dfit_row_html("NT", dfit_fmt(f$nt, R)))
    out <- paste0(out, dfit_row_html("Fit (R²)", dfit_fmt(f$fit_r2, 3)))
    if (is.finite(f$fit_ad)) {
      out <- paste0(out, dfit_row_html("Fit (AD)", dfit_fmt(f$fit_ad, 3)))
    }
    if (f$distribution_id != 1L) {
      out <- paste0(out, dfit_row_html("Skewness (D'Agostino)", dfit_fmt(d$skewness, R)))
      out <- paste0(out, dfit_row_html("Kurtosis (D'Agostino)", dfit_fmt(d$kurtosis, R)))
    }

    params <- f$params
    if (!is.null(params)) {
      fields <- dfit_param_fields(f$distribution_id)
      for (nm in names(fields)) {
        if (!is.null(params[[nm]]) && is.finite(params[[nm]])) {
          out <- paste0(out, dfit_row_html(fields[[nm]], dfit_fmt(params[[nm]], R)))
        }
      }
      if (!is.null(f$criteria)) {
        out <- paste0(out, dfit_row_html("Criteria", dfit_fmt(f$criteria, R)))
      }
    }

    if (is.finite(cap$est_pct_below) || is.finite(cap$est_pct_above)) {
      out <- paste0(out, "<tr><th colspan='2' style='text-align:left;padding:6px;'>Capability</th></tr>")
      if (is.finite(cap$est_pct_below)) {
        out <- paste0(out, dfit_row_html("Est %Below Spec", dfit_fmt(cap$est_pct_below, 3)))
      }
      if (is.finite(cap$est_pct_above)) {
        out <- paste0(out, dfit_row_html("Est %Above Spec", dfit_fmt(cap$est_pct_above, 3)))
      }
      if (is.finite(cap$est_pct_out)) {
        out <- paste0(out, dfit_row_html("Est %Out of Spec", dfit_fmt(cap$est_pct_out, 3)))
      }
      if (is.finite(cap$cpm)) out <- paste0(out, dfit_row_html("Cpm", dfit_fmt(cap$cpm, R)))
      cpk_label <- if (isTRUE(cap$is_normal)) "Cpk" else "Cpk(E)"
      if (is.finite(cap$cpk)) out <- paste0(out, dfit_row_html(cpk_label, dfit_fmt(cap$cpk, R)))
      if (is.finite(cap$cp)) out <- paste0(out, dfit_row_html("Cp", dfit_fmt(cap$cp, R)))
      if (is.finite(cap$cp_potential)) {
        out <- paste0(out, dfit_row_html("Cp(potential)", dfit_fmt(cap$cp_potential, R)))
      }
    }
  }

  out <- dfit_append_gof_rows_html(out, gof_rows)
  paste0(out, "</table>")
}

source("modules/config/architectural_patterns.R")

register_spc_dfit_server <- function(
  input,
  output,
  session,
  filtered_data,
  reactive_color_palette
) {
  ns <- session$ns

  dfit_active_x <- reactive({
    data <- filtered_data()
    req(data)
    dfit_column_values(data, input$dfit_column)
  })

  dfit_spec <- reactive({
    list(
      lsl = input$dfit_lsl,
      target = input$dfit_target,
      usl = input$dfit_usl
    )
  })

  dfit_overrides <- reactive({
    dist_id <- as.integer(input$dfit_distribution)
    fields <- dfit_param_fields(dist_id)
    if (!length(fields)) {
      return(NULL)
    }
    ov <- list()
    for (nm in names(fields)) {
      val <- input[[paste0("dfit_param_", nm)]]
      if (nm == "family") {
        if (!is.null(val) && nzchar(as.character(val))) {
          ov[[nm]] <- as.character(val)
        }
      } else if (!is.null(val) && is.finite(val)) {
        ov[[nm]] <- val
      }
    }
    if (!length(ov)) NULL else ov
  })

  dfit_result <- reactive({
    x <- dfit_active_x()
    req(length(x) >= 2L)
    dfit_analyze(
      x,
      spec = dfit_spec(),
      distribution_id = as.integer(input$dfit_distribution),
      overrides = dfit_overrides()
    )
  })

  output$dfit_column <- renderUI({
    data <- filtered_data()
    req(data)
    choices <- seq_len(ncol(data))
    names(choices) <- names(data)
    pickerInput(
      inputId = ns("dfit_column"),
      label = "Measurement column",
      choices = choices,
      multiple = FALSE,
      options = list(`live-search` = TRUE)
    )
  })

  output$dfit_sd_pot_display <- renderText({
    x <- dfit_active_x()
    if (length(x) < 2L) {
      return("—")
    }
    dfit_fmt(dfit_sd_potential(x), 4L)
  })

  output$dfit_readiness <- renderUI({
    x <- dfit_active_x()
    col <- input$dfit_column
    if (is.null(col) || !length(col) || is.na(as.numeric(col))) {
      return(tags$p(class = "text-muted", "Select measurement data to begin."))
    }
    if (length(x) < 2L) {
      return(tags$p(class = "text-warning", "Need at least 2 observations."))
    }
    NULL
  })

  dfit_sync_fitted_params <- function() {
    dist_id <- as.integer(input$dfit_distribution)
    if (dist_id == 0L) {
      return()
    }
    x <- dfit_active_x()
    if (length(x) < 2L) {
      return()
    }
    fit <- dfit_fit_distribution(x, dist_id)
    if (is.null(fit$params)) {
      return()
    }
    for (nm in names(fit$params)) {
      id <- paste0("dfit_param_", nm)
      if (nm == "family") {
        updateSelectInput(session, id, selected = fit$params[[nm]])
      } else if (!is.null(fit$params[[nm]]) && is.finite(fit$params[[nm]])) {
        updateNumericInput(session, id, value = fit$params[[nm]])
      }
    }
  }

  output$dfit_param_overrides <- renderUI({
    dist_id <- as.integer(input$dfit_distribution)
    if (dist_id == 0L) {
      return(NULL)
    }
    fields <- dfit_param_fields(dist_id)
    if (!length(fields)) {
      return(NULL)
    }
    x <- dfit_active_x()
    fitted <- if (length(x) >= 2L) {
      dfit_fit_distribution(x, dist_id)
    } else {
      NULL
    }
    inputs <- lapply(names(fields), function(nm) {
      if (nm == "family") {
        selected <- "Su"
        if (!is.null(fitted$params[[nm]]) && nzchar(as.character(fitted$params[[nm]]))) {
          selected <- as.character(fitted$params[[nm]])
        }
        return(selectInput(
          ns(paste0("dfit_param_", nm)),
          fields[[nm]],
          choices = choice_dfit_johnson_family,
          selected = selected,
          width = "100%"
        ))
      }
      val <- NA
      if (!is.null(fitted$params[[nm]]) && is.finite(fitted$params[[nm]])) {
        val <- fitted$params[[nm]]
      }
      numericInput(
        ns(paste0("dfit_param_", nm)),
        fields[[nm]],
        value = val,
        width = "120px"
      )
    })
    tagList(inputs, actionLink(ns("dfit_param_reset"), "Reset to fitted"))
  })

  observeEvent(input$dfit_distribution, {
    dfit_sync_fitted_params()
  }, ignoreInit = TRUE)

  observeEvent(input$dfit_column, {
    dfit_sync_fitted_params()
  }, ignoreInit = TRUE)

  observeEvent(input$dfit_param_reset, {
    dfit_sync_fitted_params()
  }, ignoreInit = TRUE)

  dfit_plot_colors <- reactive({
    pal <- reactive_color_palette()
    if (is.null(pal) || !length(pal)) {
      pal <- c("#000000", "#1F77B4", "#FF7F0E", "#2CA02C", "#9467BD", "#D62728")
    }
    cols <- get_distribution_colors(pal)
    c(cols, list(
      palette = pal,
      fill = cols$col_fill_highlight,
      line = cols$col_plot_line,
      overlay = if (length(pal) >= 3L) pal[[3]] else "#FF7F0E",
      point = if (length(pal) >= 2L) pal[[2]] else pal[[1]]
    ))
  })

  dfit_plot_gg <- reactive({
    x <- dfit_active_x()
    if (length(x) < 2L) {
      return(dfit_empty_plot())
    }
    r <- dfit_result()
    plot_tab <- input$dfit_plot_tab %||% "histogram"
    bins <- input$dfit_bins
    if (is.null(bins) || is.na(bins)) {
      bins <- 15L
    }
    bw <- input$dfit_bin_width
    if (!shiny::isTruthy(bw)) {
      bw <- NULL
    }
    bc <- input$dfit_bin_center
    if (!is.numeric(bc)) {
      bc <- NULL
    }
    colors <- dfit_plot_colors()
    dfit_build_plot(
      r$x,
      r$fit,
      r$spec,
      plot_type = plot_tab,
      bins = bins,
      bin_width = bw,
      bin_center = bc,
      fill_color = colors$fill,
      line_color = colors$line,
      overlay_color = colors$overlay,
      point_color = colors$point,
      show_spec_limits = isTRUE(input$dfit_show_specs),
      show_nt_limits = isTRUE(input$dfit_show_nt_limits),
      decimals = input$dfit_decimals %||% 4L,
      colors = colors
    )
  })

  output$dfit_plot <- renderPlot({
    print(dfit_plot_gg())
  })

  dfit_plot_width <- reactive(900 * 8)
  dfit_plot_height <- reactive(420 * 8)
  downloadServer("dfit_plot", dfit_plot_gg, height = dfit_plot_height, width = dfit_plot_width)

  output$dfit_calc_out <- renderUI({
    if (length(dfit_active_x()) < 2L) {
      return(HTML("<p class='text-muted'>Select measurement data to use the calculator.</p>"))
    }
    r <- dfit_result()
    if (r$fit$distribution_id == 0L) {
      return(HTML("<p class='text-muted'>Select a distribution to use the calculator.</p>"))
    }
    res <- if (input$dfit_calc_mode == "proportion") {
      dfit_distribution_calculate(r$fit, "proportion", value = input$dfit_calc_value)
    } else {
      dfit_distribution_calculate(
        r$fit, "quantile",
        proportion = input$dfit_calc_prop,
        tail = input$dfit_calc_tail
      )
    }
    if (!is.null(res$error)) {
      return(HTML(paste0("<p class='text-muted'>", res$error, "</p>")))
    }
    if (input$dfit_calc_mode == "proportion") {
      HTML(paste0(
        "<p>P(X &lt; ", dfit_fmt(res$value, 4L), ") = ", dfit_fmt(res$p_below, 3L), "%<br/>",
        "P(X &gt; ", dfit_fmt(res$value, 4L), ") = ", dfit_fmt(res$p_above, 3L), "%</p>"
      ))
    } else {
      HTML(paste0(
        "<p>", input$dfit_calc_tail, " tail ", dfit_fmt(res$proportion, 3L),
        "% → value = ", dfit_fmt(res$value, 4L), "</p>"
      ))
    }
  })

  dfit_exp_sim_results <- reactiveVal(NULL)

  dfit_exp_context <- reactive({
    list(
      x_digest = digest::digest(dfit_active_x()),
      dist_id = as.integer(input$dfit_distribution),
      tests = input$dfit_exp_tests %||% character(0)
    )
  })

  observeEvent(dfit_exp_context(), {
    dfit_exp_sim_results(NULL)
  }, ignoreInit = TRUE)

  output$dfit_exp_tests_ui <- renderUI({
    dist_id <- as.integer(input$dfit_distribution)
    if (!dist_id %in% c(2L, 3L)) {
      return(NULL)
    }
    tagList(
      checkboxGroupInput(
        ns("dfit_exp_tests"),
        "Exponential tests",
        choices = c(
          "Shapiro-Wilk Exponential" = "sw_exp",
          "MVP Exponential" = "mvp_exp",
          "Anderson-Darling Exponential" = "ad_exp"
        )
      ),
      uiOutput(ns("dfit_exp_simulate_ui")),
      helpText(
        "Anderson-Darling and Shapiro-Wilk (n ≤ 100) run immediately. ",
        "MVP and Shapiro-Wilk (n > 100) use Start Simulation."
      )
    )
  })

  output$dfit_exp_simulate_ui <- renderUI({
    tests <- input$dfit_exp_tests %||% character(0)
    x <- dfit_active_x()
    needs_sim <- dfit_exp_needs_simulation(tests, length(x))
    has_run <- !is.null(isolate(dfit_exp_sim_results()))
    btn_class <- if (needs_sim && !has_run) {
      "btn-sm btn-success"
    } else {
      "btn-sm btn-default"
    }
    tagList(
      actionButton(
        ns("dfit_exp_simulate"),
        "Start Simulation",
        class = btn_class,
        disabled = !needs_sim || has_run
      ),
      if (needs_sim && !has_run) {
        helpText("Simulation required for selected test(s).")
      } else if (has_run) {
        helpText("Simulation complete. Change data or tests to run again.")
      } else {
        helpText("Select a test that requires simulation.")
      }
    )
  })

  observeEvent(input$dfit_exp_simulate, {
    tests <- input$dfit_exp_tests %||% character(0)
    x <- dfit_active_x()
    if (!dfit_exp_needs_simulation(tests, length(x))) {
      return()
    }
    R <- input$dfit_decimals %||% 4L
    dist_id <- as.integer(input$dfit_distribution)
    rows <- dfit_exp_simulation_gof_rows(x, dist_id, tests, R, session = session)
    dfit_exp_sim_results(rows)
  }, ignoreInit = TRUE)

  dfit_gof_rows <- reactive({
    R <- input$dfit_decimals %||% 4L
    dist_id <- as.integer(input$dfit_distribution)
    x <- dfit_active_x()
    rows <- list()

    if (dist_id == 1L) {
      tests <- input$dfit_normality_tests
      rows <- c(rows, dfit_normality_gof_rows(x, tests, R))
    }

    if (dist_id %in% c(2L, 3L)) {
      tests <- input$dfit_exp_tests %||% character(0)
      rows <- c(rows, dfit_exp_instant_gof_rows(x, dist_id, tests, R))
      sim_rows <- dfit_exp_sim_results()
      if (!is.null(sim_rows)) {
        rows <- c(rows, sim_rows)
      }
    }

    rows
  })

  output$dfit_summary_table <- renderUI({
    req(length(dfit_active_x()) >= 2L)
    HTML(dfit_build_summary_html(
      dfit_result(),
      decimals = input$dfit_decimals %||% 4L,
      gof_rows = dfit_gof_rows()
    ))
  })

  list(result = dfit_result)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
