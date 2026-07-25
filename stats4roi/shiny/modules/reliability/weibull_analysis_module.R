# 2-parameter Weibull life-data analysis for stats4ROI
# Uses working data from File | Import Data (safe_filtered_data).

library(shiny)
library(ggplot2)
library(lolcat)

source("modules/config/global_config.R")
source("modules/config/global_data_invalidation.R")
source("modules/reliability/utils/weibull_bounds.R")
source("modules/reliability/utils/weibull_life.R")

.weibull_hex <- function(col, fallback = "#4C78A8") {
  if (is.null(col) || length(col) < 1L || is.na(col[1])) return(fallback)
  rgb <- grDevices::col2rgb(col[1])
  sprintf("#%02X%02X%02X", rgb[1], rgb[2], rgb[3])
}

.weibull_numeric_cols <- function(data) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0 || ncol(data) == 0) {
    return(character(0))
  }
  names(data)[vapply(data, is.numeric, logical(1))]
}

.weibull_all_cols <- function(data) {
  if (is.null(data) || !is.data.frame(data) || ncol(data) == 0) {
    return(character(0))
  }
  names(data)
}

.weibull_bounds_method_help <- function(method, estimation_method = "MLE") {
  fisher_txt <- if (estimation_method %in% c("RRX", "RRY")) {
    "Fisher matrix at rank-regression estimates (ReliaSoft hybrid): delta method on the censored-data likelihood evaluated at the RRX/RRY point estimates."
  } else {
    "Fisher matrix (delta method): fast; best for larger samples."
  }
  switch(
    as.character(method),
    "fisher" = fisher_txt,
    "lr" = "Likelihood ratio: profile likelihood; preferred for small samples (MLE only).",
    "bayes" = "Bayesian credible intervals with uniform noninformative priors on β and η (MLE only).",
    ""
  )
}

.weibull_bounds_label <- function(conf_level, method) {
  pct <- round(conf_level * 100, 1)
  mlab <- switch(
    as.character(method),
    "fisher" = "Fisher",
    "lr" = "LR",
    "bayes" = "Bayes",
    "Bounds"
  )
  sprintf("%s%% %s", format(pct, trim = TRUE), mlab)
}

.weibull_method_help <- function(method) {
  switch(
    as.character(method),
    "RRX" = paste0(
      "RRX (rank regression on X): preferred when uncertainty is mainly in the times-to-failure ",
      "(typical reliability tests). Often a good default for small, mostly complete samples."
    ),
    "RRY" = paste0(
      "RRY (rank regression on Y): regresses the transformed probability (Y) on ln(t) (X) via least squares ",
      "on the Weibull probability plot. Use when uncertainty is mainly in the failure probability ",
      "(plotting position) at known times."
    ),
    "MLE" = paste0(
      "MLE (maximum likelihood): uses each suspension time in the likelihood. Prefer when there is ",
      "substantial right censoring or larger samples; can be biased for very small complete samples."
    ),
    ""
  )
}

create_weibull_analysis_ui <- function(id) {
  ns <- NS(id)

  tabsetPanel(
    id = ns("weibull_tabs"),
    tabPanel(
      title = "Analyze",
      tagList(
        h3("Weibull Analysis"),
        br(),
        tags$div(
          style = "padding: 8px 12px; margin-bottom: 12px; background: #f5f5f5; border: 1px solid #ddd;",
          strong("Status: "),
          textOutput(ns("status_text"), inline = TRUE)
        ),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              ns("col_time"),
              "Time-to-failure column",
              choices = character(0),
              selected = NULL
            ),
            selectInput(
              ns("col_suspend"),
              "Suspend / censored column (optional)",
              choices = c("(none)" = ""),
              selected = ""
            ),
            helpText(
              "Load data under File | Import Data. Suspend: Y/N, 1/0, or TRUE/FALSE."
            ),
            numericInput(ns("duty_cycle"), "Duty cycle", value = 1, min = 0.0001, step = 0.1),
            textInput(ns("time_units"), "Time units", value = "hours"),
            selectInput(
              ns("method"),
              "Estimation method",
              choices = c(
                "RRX (rank regression on X)" = "RRX",
                "RRY (rank regression on Y)" = "RRY",
                "MLE (maximum likelihood)" = "MLE"
              ),
              selected = "RRY"
            ),
            uiOutput(ns("method_help")),
            checkboxInput(ns("show_bounds"), "Show confidence bounds", value = TRUE),
            checkboxInput(ns("show_beta1_line"), "Show β = 1 reference line", value = TRUE),
            checkboxInput(ns("show_char_life"), "Show characteristic life (η)", value = TRUE),
            helpText(
              "Characteristic life η is the time at which ",
              "F(η) = 1 − e^{−1} ≈ 0.632."
            ),
            uiOutput(ns("bounds_method_ui")),
            uiOutput(ns("bounds_method_help")),
            numericInput(
              ns("conf_level_pct"),
              "Confidence level (%)",
              value = 90,
              min = 50,
              max = 99.9,
              step = 0.1
            ),
            numericInput(ns("eval_time"), "Evaluate R at time", value = 150, min = 0, step = 1),
            textInput(
              ns("blife_levels"),
              "B-life reliability levels (comma-separated)",
              value = "0.99, 0.95, 0.90, 0.50, 0.10, 0.01"
            ),
            helpText(
              "Corrected time = raw time / duty cycle. ",
              "Probability plot uses exact median ranks; suspensions use adjusted ranks."
            )
          ),
          mainPanel(
            tabsetPanel(
              id = ns("plot_tabs"),
              tabPanel(
                title = "Probability",
                plotOutput(ns("prob_plot"), height = "360px"),
                downloadUI(ns("prob_plot_dl"))
              ),
              tabPanel(
                title = "Reliability",
                plotOutput(ns("rel_plot"), height = "360px"),
                downloadUI(ns("rel_plot_dl"))
              ),
              tabPanel(
                title = "PDF",
                plotOutput(ns("pdf_plot"), height = "360px"),
                downloadUI(ns("pdf_plot_dl"))
              )
            ),
            tags$hr(style = "border-color: black;"),
            h4("Results"),
            uiOutput(ns("results"))
          )
        )
      )
    ),
    tabPanel(
      title = "About",
      fluidRow(
        column(
          width = 10,
          h3("Weibull Analysis"),
          tags$p(
            "Two-parameter Weibull life-data analysis for complete or right-censored ",
            "(suspended) times-to-failure. Uses the current working data from File | Import Data ",
            "(after rename/transform/filter)."
          ),
          tags$p(
            tags$strong("Model:"),
            withMathJax(
              "$F(t) = 1 - e^{-(t/\\eta)^{\\beta}}$, $R(t) = e^{-(t/\\eta)^{\\beta}}$, $f(t) = \\frac{\\beta}{\\eta}\\left(\\frac{t}{\\eta}\\right)^{\\beta-1} e^{-(t/\\eta)^{\\beta}}$."
            )
          ),
          tags$p(
            withMathJax(
              "$\\beta < 1$: decreasing failure rate (infant mortality); ",
              "$\\beta = 1$: constant (exponential); ",
              "$\\beta > 1$: increasing (wear-out)."
            )
          ),
          tags$p(
            tags$strong("Methods:"),
            " RRX and RRY are rank-regression fits on the Weibull probability plot ",
            "(exact median ranks). MLE maximizes the right-censored Weibull likelihood. ",
            "Optional confidence bounds on the probability plot, B-life times, and ",
            "reliability R(t): Fisher matrix (delta method) is available for RRX, RRY, and MLE ",
            "(for RRX/RRY, Fisher information is evaluated at the rank-regression estimates). ",
            "Likelihood ratio and Bayesian credible intervals (uniform noninformative priors on β and η) ",
            "are available for MLE only."
          ),
          tags$p("References:"),
          tags$ul(
            tags$li(
              tags$a(
                href = "https://help.reliasoft.com/reference/life_data_analysis/lda/parameter_estimation.html",
                target = "_blank",
                "ReliaSoft — Parameter Estimation (RRX, RRY, MLE)"
              )
            ),
            tags$li(
              tags$a(
                href = "https://help.reliasoft.com/reference/life_data_analysis/lda/confidence_bounds.html",
                target = "_blank",
                "ReliaSoft — Confidence Bounds (Fisher, LR, Bayesian)"
              )
            ),
            tags$li(
              tags$a(
                href = "https://www.quanterion.com/models-commonly-used-to-measure-reliability-growth/",
                target = "_blank",
                "Quanterion — Reliability models overview"
              )
            )
          )
        )
      )
    )
  )
}

create_weibull_analysis_server <- function(id, filtered_data, color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    colors <- reactive({
      get_distribution_colors(color_palette())
    })

    status_msg <- reactiveVal("Load data under File | Import Data.")

    `%||%` <- function(a, b) {
      if (is.null(a) || length(a) == 0L) return(b)
      if (is.character(a) && length(a) == 1L && !nzchar(a)) return(b)
      a
    }

    register_module(
      "weibull_analysis_module",
      ui_reset = function() {
        status_msg("Load data under File | Import Data.")
        tryCatch(
          updateSelectInput(session, "col_time", choices = character(0), selected = character(0)),
          error = function(e) NULL
        )
        tryCatch(
          updateSelectInput(session, "col_suspend", choices = c("(none)" = ""), selected = ""),
          error = function(e) NULL
        )
      },
      validation = function(data, selections) {
        if (is.null(data) || nrow(data) == 0) {
          return(list(valid = FALSE, message = "No data available for Weibull Analysis"))
        }
        list(valid = TRUE, message = "")
      }
    )

    # Keep column pickers in sync with working data
    observe({
      data <- tryCatch(filtered_data(), error = function(e) NULL)
      num_cols <- .weibull_numeric_cols(data)
      all_cols <- .weibull_all_cols(data)

      sel_time <- isolate(input$col_time)
      if (is.null(sel_time) || !nzchar(as.character(sel_time)) || !sel_time %in% num_cols) {
        sel_time <- if (length(num_cols) > 0) num_cols[[1]] else NULL
      }
      updateSelectInput(session, "col_time", choices = num_cols, selected = sel_time)

      sus_choices <- c("(none)" = "", stats::setNames(all_cols, all_cols))
      sel_sus <- isolate(input$col_suspend)
      if (is.null(sel_sus) || (!identical(sel_sus, "") && !sel_sus %in% all_cols)) {
        sel_sus <- ""
      }
      updateSelectInput(session, "col_suspend", choices = sus_choices, selected = sel_sus)
    })

    output$status_text <- renderText(status_msg())
    output$method_help <- renderUI({
      helpText(.weibull_method_help(input$method))
    })
    output$bounds_method_ui <- renderUI({
      method <- input$method %||% "RRY"
      if (identical(method, "MLE")) {
        selectInput(
          ns("bounds_method"),
          "Bound method",
          choices = c(
            "Fisher matrix" = "fisher",
            "Likelihood ratio" = "lr",
            "Bayesian" = "bayes"
          ),
          selected = isolate(input$bounds_method %||% "fisher")
        )
      } else {
        tagList(
          tags$p(
            style = "margin-bottom: 4px;",
            tags$strong("Bound method:"),
            " Fisher matrix (rank-regression estimates)"
          )
        )
      }
    })

    output$bounds_method_help <- renderUI({
      method <- input$method %||% "RRY"
      bounds_m <- if (identical(method, "MLE")) {
        input$bounds_method %||% "fisher"
      } else {
        "fisher"
      }
      helpText(.weibull_bounds_method_help(bounds_m, estimation_method = method))
    })

    .parse_blife_levels <- function(txt) {
      parts <- strsplit(as.character(txt %||% ""), "[,;\\s]+")[[1]]
      vals <- suppressWarnings(as.numeric(parts))
      vals <- vals[is.finite(vals) & vals > 0 & vals < 1]
      if (length(vals) < 1L) c(0.99, 0.95, 0.90, 0.50, 0.10, 0.01) else vals
    }

    fit_obj <- shiny::debounce(reactive({
      method <- input$method %||% "RRY"
      duty <- as.numeric(input$duty_cycle)
      is_mle <- identical(method, "MLE")
      show_b <- isTRUE(input$show_bounds)
      bounds_m <- if (is_mle) {
        input$bounds_method %||% "fisher"
      } else if (show_b) {
        "fisher"
      } else {
        "none"
      }
      conf_pct <- as.numeric(input$conf_level_pct)
      conf_lvl <- if (is.finite(conf_pct) && conf_pct >= 50 && conf_pct <= 99.9) conf_pct / 100 else 0.90
      eval_t <- as.numeric(input$eval_time)
      blife_R <- .parse_blife_levels(input$blife_levels)

      .do_fit <- function(times, sus) {
        weibull_fit(
          times, suspended = sus, duty_cycle = duty, method = method,
          bounds_method = bounds_m, conf_level = conf_lvl,
          show_bounds = show_b, eval_time = eval_t, blife_R = blife_R
        )
      }

      data <- tryCatch(filtered_data(), error = function(e) NULL)
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
        status_msg("Load data under File | Import Data.")
        return(list(ok = FALSE, message = "No working data available. Load data under File | Import Data first."))
      }
      col_t <- input$col_time
      if (is.null(col_t) || !nzchar(as.character(col_t)) || !col_t %in% names(data)) {
        status_msg("Select a numeric time column from working data (File | Import Data).")
        return(list(ok = FALSE, message = "Select a time column from working data."))
      }
      times <- data[[col_t]]
      sus <- NULL
      col_s <- input$col_suspend
      if (!is.null(col_s) && nzchar(as.character(col_s)) && col_s %in% names(data)) {
        sus <- data[[col_s]]
      }
      out <- .do_fit(times, sus)

      if (isTRUE(out$ok)) {
        msg <- sprintf(
          "OK — n = %d (%d failures, %d suspended); method = %s.",
          out$n, out$n_fail, out$n_susp, out$method
        )
        if (nzchar(out$message %||% "")) {
          msg <- paste0(msg, " ", out$message)
        }
        status_msg(msg)
      } else {
        status_msg(out$message %||% "Unable to fit Weibull model.")
      }
      out
    }), 350)

    .fmt_plain <- function(x, digits = 4) {
      format(round(as.numeric(x), digits), scientific = FALSE, trim = TRUE, big.mark = ",")
    }

    output$results <- renderUI({
      fit <- fit_obj()
      if (!isTRUE(fit$ok)) {
        return(HTML(paste0("<p>", htmltools::htmlEscape(fit$message %||% ""), "</p>")))
      }
      units <- htmltools::htmlEscape(as.character(input$time_units %||% "hours"))
      td <- "padding: 8px 12px; border: 1px solid #ddd;"
      td_r <- "padding: 8px 12px; border: 1px solid #ddd; text-align: right;"
      th <- "padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5; text-align: left;"
      th_r <- "padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5; text-align: right;"

      rho_row <- ""
      if (is.finite(fit$rho)) {
        rho_row <- paste0(
          "<tr><td style='", td, "'>", withMathJax("$\\rho$ (correlation)"), "</td>",
          "<td style='", td_r, "'>", sprintf("%.4f", fit$rho), "</td></tr>"
        )
      }
      r2_row <- ""
      if (is.finite(fit$r_squared)) {
        r2_row <- paste0(
          "<tr><td style='", td, "'>", withMathJax("$R^2$"), "</td>",
          "<td style='", td_r, "'>", sprintf("%.4f", fit$r_squared), "</td></tr>"
        )
      }

      blife_rows <- ""
      has_blife_ci <- !is.null(fit$blife) && nrow(fit$blife) > 0L &&
        any(is.finite(fit$blife$time_lo) | is.finite(fit$blife$time_hi))
      if (!is.null(fit$blife) && nrow(fit$blife) > 0L) {
        for (i in seq_len(nrow(fit$blife))) {
          time_cell <- paste0(.fmt_plain(fit$blife$time[i], 2), " ", units)
          if (has_blife_ci && (is.finite(fit$blife$time_lo[i]) || is.finite(fit$blife$time_hi[i]))) {
            time_cell <- paste0(
              time_cell,
              " [", .fmt_plain(fit$blife$time_lo[i], 2),
              ", ", .fmt_plain(fit$blife$time_hi[i], 2), "]"
            )
          }
          blife_rows <- paste0(
            blife_rows,
            "<tr><td style='", td, "'>", sprintf("R = %.2f", fit$blife$R[i]), "</td>",
            "<td style='", td_r, "'>", time_cell, "</td></tr>"
          )
        }
      }

      point_row <- ""
      if (is.finite(fit$eval_time) && is.finite(fit$point_R)) {
        r_cell <- sprintf("%.6f", fit$point_R)
        if (is.finite(fit$point_R_lo) && is.finite(fit$point_R_hi)) {
          r_cell <- paste0(r_cell, sprintf(" [%.6f, %.6f]", fit$point_R_lo, fit$point_R_hi))
        }
        point_row <- paste0(
          "<tr><td style='", td, "'>",
          withMathJax(sprintf("$R(%.4g)$", fit$eval_time)), "</td>",
          "<td style='", td_r, "'>", r_cell, "</td></tr>"
        )
      }

      blife_th <- if (has_blife_ci) {
        paste0(
          "<tr><th style='", th, "'>Reliability R</th>",
          "<th style='", th_r, "'>Time [lower, upper]</th></tr>"
        )
      } else {
        paste0(
          "<tr><th style='", th, "'>Reliability R</th>",
          "<th style='", th_r, "'>Time</th></tr>"
        )
      }

      withMathJax(HTML(paste0(
        "<table style='border-collapse: collapse; margin-bottom: 12px;'>",
        "<tr><td style='", td, "'>", withMathJax("$\\beta$ (shape)"), "</td>",
        "<td style='", td_r, "'>", sprintf("%.6f", fit$beta), "</td></tr>",
        "<tr><td style='", td, "'>", withMathJax("$\\eta$ (scale)"), "</td>",
        "<td style='", td_r, "'>", .fmt_plain(fit$eta, 4), " ", units, "</td></tr>",
        "<tr><td style='", td, "'>Method</td>",
        "<td style='", td_r, "'>", htmltools::htmlEscape(fit$method), "</td></tr>",
        "<tr><td style='", td, "'>Failure-rate trend</td>",
        "<td style='", td_r, "'>", htmltools::htmlEscape(fit$trend), "</td></tr>",
        rho_row, r2_row,
        "<tr><td style='", td, "'>Sample mean (failures)</td>",
        "<td style='", td_r, "'>", .fmt_plain(fit$sample_mean_fail, 2), " ", units, "</td></tr>",
        "<tr><td style='", td, "'>Weibull mean</td>",
        "<td style='", td_r, "'>", .fmt_plain(fit$mean, 2), " ", units, "</td></tr>",
        "<tr><td style='", td, "'>Weibull median</td>",
        "<td style='", td_r, "'>", .fmt_plain(fit$median, 2), " ", units, "</td></tr>",
        point_row,
        "</table>",
        "<p><b>B-life / time at reliability R</b></p>",
        "<table style='border-collapse: collapse;'>",
        blife_th,
        blife_rows,
        "</table>"
      )))
    })

    prob_plot_obj <- reactive({
      fit <- fit_obj()
      req(isTRUE(fit$ok))
      cols <- colors()
      pts <- fit$plot_points
      line_col <- .weibull_hex(cols$col_plot_line)
      pt_col <- .weibull_hex(cols$col_point_of_interest_line)
      bnd_col <- .weibull_hex(cols$col_mean_line)
      ref_col <- .weibull_hex(cols$col_fill_highlight)

      bnd_label <- if (!is.null(fit$bounds_obj)) {
        .weibull_bounds_label(fit$bounds_obj$conf_level, fit$bounds_obj$method)
      } else {
        "Bounds"
      }

      p <- ggplot2::ggplot() +
        ggplot2::geom_point(
          data = pts,
          ggplot2::aes(x = .data$ln_t, y = .data$y, color = "Data"),
          size = 2.5
        ) +
        ggplot2::geom_line(
          data = fit$fit_line,
          ggplot2::aes(x = .data$ln_t, y = .data$y, color = "Fit"),
          linewidth = 1
        )

      color_vals <- c(
        "Data" = pt_col,
        "Fit" = line_col
      )

      ln_t_vals <- c(pts$ln_t, fit$fit_line$ln_t)
      y_vals <- c(pts$y, fit$fit_line$y)

      if (!is.null(fit$bounds) && nrow(fit$bounds) > 0L &&
          any(is.finite(fit$bounds$y_lower) | is.finite(fit$bounds$y_upper))) {
        bdf <- fit$bounds
        bdf$bound_key <- bnd_label
        ln_t_vals <- c(ln_t_vals, bdf$ln_t)
        y_vals <- c(y_vals, bdf$y_lower, bdf$y_upper)
        p <- p +
          ggplot2::geom_line(
            data = bdf,
            ggplot2::aes(x = .data$ln_t, y = .data$y_lower, color = .data$bound_key),
            linetype = "dashed", linewidth = 0.8
          ) +
          ggplot2::geom_line(
            data = bdf,
            ggplot2::aes(x = .data$ln_t, y = .data$y_upper, color = .data$bound_key),
            linetype = "dashed", linewidth = 0.8
          )
        color_vals[[bnd_label]] <- bnd_col
      }

      if (isTRUE(input$show_beta1_line) && !is.null(fit$beta1_line)) {
        ln_t_vals <- c(ln_t_vals, fit$beta1_line$ln_t)
        y_vals <- c(y_vals, fit$beta1_line$y)
        p <- p + ggplot2::geom_line(
          data = fit$beta1_line,
          ggplot2::aes(x = .data$ln_t, y = .data$y, color = "β = 1"),
          linetype = "dotdash", linewidth = 0.9
        )
        color_vals[["β = 1"]] <- ref_col
      }

      if (isTRUE(input$show_char_life)) {
        char_guides <- weibull_characteristic_life_guides(
          fit$eta,
          ln_t_range = range(ln_t_vals, na.rm = TRUE),
          y_range = range(y_vals, na.rm = TRUE)
        )
        if (!is.null(char_guides$vertical)) {
          p <- p + ggplot2::geom_line(
            data = char_guides$vertical,
            ggplot2::aes(x = .data$ln_t, y = .data$y, color = "η (F≈0.632)"),
            linetype = "dashed", linewidth = 0.9
          )
          color_vals[["η (F≈0.632)"]] <- pt_col
        }
        if (!is.null(char_guides$horizontal)) {
          p <- p + ggplot2::geom_line(
            data = char_guides$horizontal,
            ggplot2::aes(x = .data$ln_t, y = .data$y, color = "η (F≈0.632)"),
            linetype = "dotted", linewidth = 0.6, alpha = 0.65
          )
        }
      }

      p +
        ggplot2::scale_color_manual(values = color_vals) +
        ggplot2::labs(
          title = "Weibull Probability Plot",
          x = "ln(Time)",
          y = "ln(ln(1/(1 − F̂)))",
          color = NULL
        ) +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(legend.position = "bottom")
    })

    rel_plot_obj <- reactive({
      fit <- fit_obj()
      req(isTRUE(fit$ok))
      cols <- colors()
      df <- data.frame(
        time = fit$curve_time,
        R = fit$curve_R,
        R_lo = fit$curve_R_lo,
        R_hi = fit$curve_R_hi
      )
      has_ribbon <- any(is.finite(df$R_lo) | is.finite(df$R_hi))
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$time, y = .data$R))
      if (has_ribbon) {
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = .data$R_lo, ymax = .data$R_hi),
          fill = .weibull_hex(cols$col_mean_line),
          alpha = 0.25,
          colour = NA
        )
      }
      p +
        ggplot2::geom_line(color = .weibull_hex(cols$col_plot_line), linewidth = 1.1) +
        ggplot2::labs(
          title = "Reliability vs Time",
          x = sprintf("Time (%s)", input$time_units %||% "hours"),
          y = "Reliability R(t)"
        ) +
        ggplot2::coord_cartesian(ylim = c(0, 1)) +
        ggplot2::theme_bw(base_size = 13)
    })

    pdf_plot_obj <- reactive({
      fit <- fit_obj()
      req(isTRUE(fit$ok))
      cols <- colors()
      df <- data.frame(time = fit$curve_time, f = fit$curve_pdf)
      df <- df[is.finite(df$f) & df$time > 0, , drop = FALSE]
      ggplot2::ggplot(df, ggplot2::aes(x = .data$time, y = .data$f)) +
        ggplot2::geom_line(color = .weibull_hex(cols$col_plot_line), linewidth = 1.1) +
        ggplot2::labs(
          title = "Weibull PDF",
          x = sprintf("Time (%s)", input$time_units %||% "hours"),
          y = "f(t)"
        ) +
        ggplot2::theme_bw(base_size = 13)
    })

    output$prob_plot <- renderPlot(prob_plot_obj())
    output$rel_plot <- renderPlot(rel_plot_obj())
    output$pdf_plot <- renderPlot(pdf_plot_obj())

    downloadServer("prob_plot_dl", prob_plot_obj, height = reactive(360 * 4), width = reactive(640 * 4))
    downloadServer("rel_plot_dl", rel_plot_obj, height = reactive(360 * 4), width = reactive(640 * 4))
    downloadServer("pdf_plot_dl", pdf_plot_obj, height = reactive(360 * 4), width = reactive(640 * 4))
  })
}
