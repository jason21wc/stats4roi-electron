# Crow-AMSAA / Duane Reliability Growth Analysis module for stats4ROI
# Uses working data from File | Import Data (safe_filtered_data).

library(shiny)
library(ggplot2)
library(shinyWidgets)
library(lolcat)

source("modules/config/global_config.R")
source("modules/config/global_data_invalidation.R")
source("modules/reliability/utils/growth_tables.R")
source("modules/reliability/utils/growth_amsaa.R")

create_growth_analysis_ui <- function(id) {
  ns <- NS(id)

  tabsetPanel(
    id = ns("growth_tabs"),
    tabPanel(
      title = "Analyze",
      tagList(
        h3("Crow–AMSAA Growth Analysis"),
        br(),
        tags$div(
          style = "padding: 8px 12px; margin-bottom: 12px; background: #f5f5f5; border: 1px solid #ddd;",
          strong("Status: "),
          textOutput(ns("analyze_status_text"), inline = TRUE)
        ),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              ns("data_mode"),
              "Data type",
              choices = c(
                "Time-terminated" = "time",
                "Failure-terminated" = "failure",
                "Grouped / interval" = "grouped"
              ),
              selected = "time"
            ),
            selectInput(
              ns("col_time"),
              "Failure / interval time column",
              choices = character(0),
              selected = NULL
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'grouped'", ns("data_mode")),
              selectInput(
                ns("col_failures"),
                "Failures-in-interval column",
                choices = character(0),
                selected = NULL
              ),
              selectInput(
                ns("grouped_method"),
                "Estimation method",
                choices = c("MLE", "LSE"),
                selected = "LSE"
              )
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'time'", ns("data_mode")),
              numericInput(
                ns("end_time"),
                "End test time T",
                value = NULL,
                min = 0,
                step = 1
              ),
              helpText("Required for time-terminated analysis. T must be at least the last failure time.")
            ),
            textInput(ns("time_units"), "Time units", value = "hours"),
            selectInput(
              ns("type_I"),
              "Type I (α) for goodness-of-fit",
              choices = c(0.01, 0.05, 0.1, 0.15, 0.2),
              selected = 0.1
            ),
            numericInput(
              ns("mission_time"),
              label = withMathJax("Mission time $\\tau$ (for $R_\\tau$)"),
              value = 60, min = 0, step = 1
            ),
            selectInput(
              ns("conf_interval"),
              "Confidence for Intervals",
              choices = c(
                "80%" = 0.80,
                "90%" = 0.90,
                "95%" = 0.95,
                "98%" = 0.98
              ),
              selected = 0.90
            ),
            selectInput(
              ns("ci_sides"),
              "Interval sidedness",
              choices = c("2-sided" = 2, "1-sided" = 1),
              selected = 1
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == '1'", ns("ci_sides")),
              selectInput(
                ns("ci_dir"),
                "1-sided direction",
                choices = c("Lower" = "lower", "Upper" = "upper"),
                selected = "lower"
              )
            ),
            helpText(
              withMathJax(paste0(
                "Type I ($\\alpha$) is used for Cramér–von Mises / $\\chi^2$ GOF and the $\\beta = 1$ trend test. ",
                "Confidence for Intervals is used for $\\mathrm{MTBF}_T$, $\\lambda_T$, and $R_\\tau$ limits (default 90%)."
              ))
            ),
            helpText(
              "Results update as you change settings. Load data under File | Import Data. ",
              "For time- or failure-terminated modes, select the cumulative failure-time column. ",
              "For grouped data, select interval operating time and failures per interval."
            )
          ),
          mainPanel(
            plotOutput(ns("analyze_plot"), height = "360px"),
            fluidRow(
              column(8, downloadUI(ns("analyze_plot_dl"))),
              column(
                4,
                tags$div(
                  style = "padding-top: 6px;",
                  prettySwitch(
                    inputId = ns("log_log"),
                    label = "Log–log scales",
                    value = TRUE,
                    status = "success",
                    fill = TRUE
                  )
                )
              )
            ),
            tags$hr(style = "border-color: black;"),
            h4("Results"),
            uiOutput(ns("analyze_results"))
          )
        )
      )
    ),
    tabPanel(
      title = "RGT Tradeoffs",
      tagList(
        h3("Duane RGT Tradeoffs"),
        br(),
        sidebarLayout(
          sidebarPanel(
            numericInput(
              ns("rgt_a"),
              label = withMathJax("Growth rate $\\alpha$"),
              value = 0.2, min = 0.01, max = 0.99, step = 0.01
            ),
            numericInput(
              ns("rgt_T0"),
              label = withMathJax("Initial interval $T_0$"),
              value = 48, min = 0, step = 1
            ),
            numericInput(
              ns("rgt_mtbf0"),
              label = withMathJax("Initial $\\mathrm{MTBF}_0$"),
              value = 1000, min = 0, step = 1
            ),
            numericInput(
              ns("rgt_mtbfI"),
              label = withMathJax("Goal $\\mathrm{MTBF}_I$ (instantaneous)"),
              value = 10000, min = 0, step = 1
            ),
            numericInput(ns("rgt_systems"), "Number of systems", value = 64, min = 1, step = 1),
            numericInput(ns("rgt_hours"), "Hours per test", value = 48, min = 0, step = 1),
            helpText(
              withMathJax(
                "Estimate total test time and number of tests to reach an instantaneous $\\mathrm{MTBF}$ goal."
              )
            )
          ),
          mainPanel(
            h4("Results"),
            uiOutput(ns("rgt_results"))
          )
        )
      )
    ),
    tabPanel(
      title = "Growth Plan",
      tagList(
        h3("Reliability Growth Plan"),
        br(),
        sidebarLayout(
          sidebarPanel(
            numericInput(ns("plan_R"), "Target reliability", value = 0.975, min = 0.01, max = 0.999, step = 0.001),
            numericInput(ns("plan_tau"), "Mission hours τ", value = 100, min = 0, step = 1),
            numericInput(ns("plan_alpha"), "Crow α (scale)", value = 0.0066, min = 0, step = 0.0001),
            numericInput(ns("plan_beta"), "Crow β (shape)", value = 0.72, min = 0.01, step = 0.01),
            checkboxInput(ns("plan_use_fit"), "Use last Analyze fit for α, β", value = FALSE),
            selectInput(
              ns("plan_actual_col"),
              "Actual reliability column (optional)",
              choices = c("(none)" = ""),
              selected = ""
            ),
            helpText(
              "Ideal reliability path to a target R and target development time T*. ",
              "Optional actual-R values are compared to Ideal R at the same development time ",
              "(plan conformance / milestone check, per MIL-HDBK-189)."
            )
          ),
          mainPanel(
            plotOutput(ns("plan_plot"), height = "420px"),
            downloadUI(ns("plan_plot_dl")),
            tags$hr(style = "border-color: black;"),
            h4("Results"),
            uiOutput(ns("plan_summary"))
          )
        )
      )
    ),
    tabPanel(
      title = "About",
      fluidRow(
        column(
          width = 10,
          h3("Reliability Growth Analysis"),
          tags$p(
            "Crow–AMSAA (power-law NHPP) tracking and Duane planning. Analysis uses the current working data ",
            "from File | Import Data (after rename/transform/filter)."
          ),
          tags$p(
            tags$strong("Model:"),
            withMathJax(
              "$N(t) = \\alpha t^{\\beta}$, $\\lambda(t) = \\alpha\\beta t^{\\beta-1}$, $\\mathrm{MTBF}(t) = 1/\\lambda(t)$, $R_\\tau \\approx \\exp(-\\lambda_T\\,\\tau)$."
            )
          ),
          tags$p(
            withMathJax("$\\beta < 1$ indicates reliability growth; $\\beta = 1$ random failures; $\\beta > 1$ wear-out.")
          ),
          tags$p(
            tags$strong("Growth Plan:"),
            " Shows the idealized reliability path and target development time T*. ",
            "Actual reliability (if provided) is checked against Ideal R at the same development time ",
            "(plan conformance / milestone-threshold idea from MIL-HDBK-189). ",
            "Crow confidence intervals on demonstrated reliability belong on the Analyze tab."
          ),
          tags$p("References:"),
          tags$ul(
            tags$li(
              tags$a(
                href = "https://www.quanterion.com/models-commonly-used-to-measure-reliability-growth/",
                target = "_blank",
                "Quanterion — Models Commonly Used to Measure Reliability Growth"
              )
            ),
            tags$li(
              tags$a(
                href = "https://help.reliasoft.com/reference/reliability_growth_and_repairable_system_analysis/rg_rsa/crow-amsaa_nhpp.html",
                target = "_blank",
                "ReliaSoft — Crow-AMSAA NHPP"
              )
            ),
            tags$li(
              tags$a(
                href = "https://apps.dtic.mil/sti/tr/pdf/ADA381985.pdf",
                target = "_blank",
                "DTIC ADA381985 (AMSAA technical report)"
              )
            ),
            tags$li(
              tags$a(
                href = "https://www.acqnotes.com/Attachments/Mil%20HDBK%20189A%20-%20Reliability%20Growth%20Management.pdf",
                target = "_blank",
                "MIL-HDBK-189A — Reliability Growth Management (idealized curve and thresholds)"
              )
            )
          )
        )
      )
    )
  )
}

.growth_hex <- function(col, fallback = "#4C78A8") {
  if (is.null(col) || length(col) < 1L || is.na(col[1])) return(fallback)
  rgb <- grDevices::col2rgb(col[1])
  sprintf("#%02X%02X%02X", rgb[1], rgb[2], rgb[3])
}

.growth_numeric_cols <- function(data) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0 || ncol(data) == 0) {
    return(character(0))
  }
  names(data)[vapply(data, is.numeric, logical(1))]
}

create_growth_analysis_server <- function(id, filtered_data, color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    colors <- reactive({
      get_distribution_colors(color_palette())
    })

    last_fit <- reactiveVal(NULL)
    analyze_status <- reactiveVal("Load data under File | Import Data.")

    `%||%` <- function(a, b) {
      if (is.null(a) || length(a) == 0L) return(b)
      if (is.character(a) && length(a) == 1L && !nzchar(a)) return(b)
      a
    }

    .is_finite1 <- function(x) {
      length(x) == 1L && is.finite(x)
    }

    .growth_ci_bracket <- function(lo, hi, digits = 6) {
      paste0(
        "[",
        if (.is_finite1(lo)) sprintf(paste0("%.", digits, "g"), lo) else "—",
        ", ",
        if (.is_finite1(hi)) sprintf(paste0("%.", digits, "g"), hi) else "—",
        "]"
      )
    }

    # Clear results / selections when File | Import (or working data) changes
    register_module(
      "growth_analysis_module",
      ui_reset = function() {
        last_fit(NULL)
        analyze_status("Load data under File | Import Data.")
        tryCatch(updateSelectInput(session, "col_time", choices = character(0), selected = character(0)), error = function(e) NULL)
        tryCatch(updateSelectInput(session, "col_failures", choices = character(0), selected = character(0)), error = function(e) NULL)
        tryCatch(
          updateSelectInput(session, "plan_actual_col", choices = c("(none)" = ""), selected = ""),
          error = function(e) NULL
        )
        tryCatch(updateNumericInput(session, "end_time", value = NULL), error = function(e) NULL)
        tryCatch(updateCheckboxInput(session, "plan_use_fit", value = FALSE), error = function(e) NULL)
        tryCatch(updatePrettySwitch(session, "log_log", value = TRUE), error = function(e) NULL)
      },
      validation = function(data, selections) {
        if (is.null(data) || nrow(data) == 0) {
          return(list(valid = FALSE, message = "No data available for Growth Analysis"))
        }
        list(valid = TRUE, message = "")
      }
    )

    # Keep column pickers in sync with working data
    observe({
      data <- tryCatch(filtered_data(), error = function(e) NULL)
      cols <- .growth_numeric_cols(data)

      sel_time <- isolate(input$col_time)
      if (is.null(sel_time) || !nzchar(as.character(sel_time)) || !sel_time %in% cols) {
        sel_time <- if (length(cols) > 0) cols[[1]] else NULL
      }
      updateSelectInput(session, "col_time", choices = cols, selected = sel_time)

      sel_fail <- isolate(input$col_failures)
      if (is.null(sel_fail) || !nzchar(as.character(sel_fail)) || !sel_fail %in% cols) {
        sel_fail <- if (length(cols) > 1) cols[[2]] else if (length(cols) > 0) cols[[1]] else NULL
      }
      updateSelectInput(session, "col_failures", choices = cols, selected = sel_fail)

      plan_choices <- c("(none)" = "", stats::setNames(cols, cols))
      sel_plan <- isolate(input$plan_actual_col)
      if (is.null(sel_plan) || (!identical(sel_plan, "") && !sel_plan %in% cols)) {
        sel_plan <- ""
      }
      updateSelectInput(session, "plan_actual_col", choices = plan_choices, selected = sel_plan)
    })

    output$analyze_status_text <- renderText({
      analyze_status()
    })

    # Debounce numeric typing so intermediate values (e.g. "1" while entering 10000) settle
    analyze_specs <- debounce(reactive({
      list(
        mode = input$data_mode,
        col_time = input$col_time,
        col_failures = input$col_failures,
        grouped_method = input$grouped_method,
        end_time = input$end_time,
        type_I = input$type_I,
        conf_interval = input$conf_interval,
        mission_time = input$mission_time,
        ci_sides = input$ci_sides,
        ci_dir = input$ci_dir,
        time_units = input$time_units
      )
    }), 350)

    analysis_result <- reactive({
      specs <- analyze_specs()
      tryCatch({
        data <- filtered_data()
        if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
          stop("No working data available. Load data under File | Import Data first.")
        }
        mode <- specs$mode
        type_I <- as.numeric(specs$type_I)
        conf_int <- as.numeric(specs$conf_interval)
        if (!is.finite(conf_int) || conf_int <= 0 || conf_int >= 1) {
          conf_int <- 0.90
        }
        type_I_ci <- 1 - conf_int
        mission <- as.numeric(specs$mission_time)
        sides <- as.integer(specs$ci_sides)
        side_dir <- if (identical(sides, 1L)) specs$ci_dir else "both"
        col_time <- specs$col_time

        if (is.null(col_time) || !nzchar(as.character(col_time)) || !col_time %in% names(data)) {
          stop("Select a numeric failure / interval time column.")
        }
        if (!is.numeric(data[[col_time]])) {
          stop("Time column must be numeric.")
        }

        if (identical(mode, "grouped")) {
          col_f <- specs$col_failures
          if (is.null(col_f) || !nzchar(as.character(col_f)) || !col_f %in% names(data)) {
            stop("Select a numeric failures-in-interval column.")
          }
          if (!is.numeric(data[[col_f]])) {
            stop("Failures column must be numeric.")
          }
          fit <- growth_fit_grouped(data[[col_time]], data[[col_f]], method = specs$grouped_method)
          if (!isTRUE(fit$ok)) stop(fit$message)
          inst <- growth_instantaneous(fit$alpha, fit$beta, fit$T, mission)
          trend <- growth_trend_test_grouped(fit$delta_t, fit$n_j, fit$beta, type_I)
          gof <- growth_gof_grouped(fit, type_I)
          ci <- growth_mtbf_ci(
            inst$mtbf, fit$n, type_I_ci, sides, side_dir,
            truncation = "time", mission_time = mission
          )
        } else if (identical(mode, "failure")) {
          fit <- growth_fit_failure_terminated(data[[col_time]])
          if (!isTRUE(fit$ok)) stop(fit$message)
          inst <- growth_instantaneous(fit$alpha, fit$beta, fit$T, mission)
          trend <- growth_trend_test(fit$times, fit$T, fit$beta, type_I, failure_terminated = TRUE)
          gof <- growth_cvm_gof(fit$times, fit$T, fit$beta, type_I, failure_terminated = TRUE)
          ci <- growth_mtbf_ci(
            inst$mtbf, fit$n, type_I_ci, sides, side_dir,
            truncation = "failure", mission_time = mission
          )
        } else {
          end_t <- suppressWarnings(as.numeric(specs$end_time))
          if (length(end_t) < 1L || !is.finite(end_t) || end_t <= 0) {
            stop("Enter End test time T for time-terminated analysis.")
          }
          fit <- growth_fit_time_terminated(data[[col_time]], end_t)
          if (!isTRUE(fit$ok)) stop(fit$message)
          inst <- growth_instantaneous(fit$alpha, fit$beta, fit$T, mission)
          trend <- growth_trend_test(fit$times, fit$T, fit$beta, type_I, failure_terminated = FALSE)
          gof <- growth_cvm_gof(fit$times, fit$T, fit$beta, type_I, failure_terminated = FALSE)
          ci <- growth_mtbf_ci(
            inst$mtbf, fit$n, type_I_ci, sides, side_dir,
            truncation = "time", mission_time = mission
          )
        }

        c(fit, list(
          inst = inst, trend = trend, gof = gof, ci = ci,
          type_I = type_I, conf_interval = conf_int, mission = mission
        ))
      }, error = function(e) {
        list(ok = FALSE, message = conditionMessage(e))
      })
    })

    observe({
      fit <- analysis_result()
      if (isTRUE(fit$ok)) {
        last_fit(fit)
        analyze_status(sprintf("Live fit: n = %d, T = %.4g.", fit$n, fit$T))
      } else {
        last_fit(NULL)
        analyze_status(fit$message %||% "Waiting for valid inputs.")
      }
    })

    output$analyze_results <- renderUI({
      fit <- analysis_result()
      if (is.null(fit) || !isTRUE(fit$ok)) {
        msg <- if (!is.null(fit$message)) fit$message else "Results will appear here when inputs are valid."
        return(HTML(paste0("<p>", htmltools::htmlEscape(msg), "</p>")))
      }
      units <- analyze_specs()$time_units
      if (is.null(units) || length(units) == 0L || !nzchar(as.character(units)[1])) {
        units <- "time units"
      } else {
        units <- as.character(units)[1]
      }
      gof_test <- fit$gof$test %||% "Goodness-of-fit"
      conf_pct <- if (.is_finite1(fit$conf_interval)) {
        sprintf("%.0f%%", 100 * fit$conf_interval)
      } else {
        "CI"
      }
      withMathJax(HTML(paste0(
        "<table>",
        "<tr><td>", withMathJax("$\\beta=$"), "</td><td>&nbsp;&nbsp;</td><td>",
        sprintf("%.6f", fit$beta), "</td></tr>",
        "<tr><td>", withMathJax("$\\alpha=$"), "</td><td>&nbsp;&nbsp;</td><td>",
        sprintf("%.6g", fit$alpha), "</td></tr>",
        "<tr><td>", withMathJax("$T=$"), "</td><td>&nbsp;&nbsp;</td><td>",
        sprintf("%.6g", fit$T), " ", htmltools::htmlEscape(units), "</td></tr>",
        "<tr><td>", withMathJax("$n=$"), "</td><td>&nbsp;&nbsp;</td><td>",
        fit$n, "</td></tr>",
        "<tr><td>", withMathJax("$\\lambda_T=$"), "</td><td>&nbsp;&nbsp;</td><td>",
        sprintf("%.6g", fit$inst$lambda), "</td></tr>",
        "<tr><td>", withMathJax("$\\lambda_T$"), " ", conf_pct, " CI</td><td>&nbsp;&nbsp;</td><td>",
        .growth_ci_bracket(fit$ci$lambda_L, fit$ci$lambda_U, 6), "</td></tr>",
        "<tr><td>", withMathJax("$\\mathrm{MTBF}_T=$"), "</td><td>&nbsp;&nbsp;</td><td>",
        sprintf("%.6f", fit$inst$mtbf), "</td></tr>",
        "<tr><td>", withMathJax("$\\mathrm{MTBF}_T$"), " ", conf_pct, " CI</td><td>&nbsp;&nbsp;</td><td>",
        .growth_ci_bracket(fit$ci$mtbf_L, fit$ci$mtbf_U, 4), "</td></tr>",
        "<tr><td>", withMathJax("$R_\\tau=$"), "</td><td>&nbsp;&nbsp;</td><td>",
        sprintf("%.6f", fit$inst$R),
        " (", withMathJax("$\\tau=$"), " ", sprintf("%.4g", fit$mission), ")</td></tr>",
        "<tr><td>", withMathJax("$R_\\tau$"), " ", conf_pct, " CI</td><td>&nbsp;&nbsp;</td><td>",
        .growth_ci_bracket(fit$ci$R_L, fit$ci$R_U, 6), "</td></tr>",
        "<tr><td>Trend</td><td>&nbsp;&nbsp;</td><td>", htmltools::htmlEscape(fit$trend$label %||% ""),
        if (.is_finite1(fit$trend$p)) {
          paste0(" (", withMathJax("$p=$"), " ", sprintf("%.4f", fit$trend$p), ")")
        } else {
          ""
        },
        "</td></tr>",
        "<tr><td>GOF (", htmltools::htmlEscape(as.character(gof_test)[1]), ")</td><td>&nbsp;&nbsp;</td><td>",
        htmltools::htmlEscape(fit$gof$message %||% ""),
        if (.is_finite1(fit$gof$statistic)) {
          extra <- if (.is_finite1(fit$gof$critical)) {
            sprintf(", crit = %.4g", fit$gof$critical)
          } else if (.is_finite1(fit$gof$p)) {
            sprintf(", p = %.4g", fit$gof$p)
          } else if (.is_finite1(fit$gof$df)) {
            sprintf(", df = %s", as.character(fit$gof$df))
          } else {
            ""
          }
          sprintf(" [stat = %.4g%s]", fit$gof$statistic, extra)
        } else {
          ""
        },
        "</td></tr>",
        "</table>"
      )))
    })

    analyze_plot_obj <- reactive({
      fit <- analysis_result()
      req(isTRUE(fit$ok))
      cols <- colors()
      line_col <- .growth_hex(cols$col_plot_line)
      pt_col <- .growth_hex(cols$col_point_of_interest_line)
      curve <- growth_fitted_curve(fit$alpha, fit$beta, fit$T)

      if (identical(fit$mode, "grouped")) {
        pts <- data.frame(time = fit$cum_t, N = fit$cum_n)
      } else {
        pts <- data.frame(time = fit$times, N = seq_along(fit$times))
      }

      a_lab <- signif(fit$alpha, 4)
      b_lab <- round(fit$beta, 4)

      p <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = curve,
          ggplot2::aes(x = .data$time, y = .data$N),
          color = line_col,
          linewidth = 1
        ) +
        ggplot2::geom_point(
          data = pts,
          ggplot2::aes(x = .data$time, y = .data$N),
          color = pt_col,
          size = 2.5
        ) +
        ggplot2::labs(
          title = "Cumulative Failures vs Time",
          subtitle = bquote(
            N(t) == alpha * t^{beta} ~
              (alpha == .(a_lab) * "," ~ beta == .(b_lab))
          ),
          x = sprintf("Cumulative time (%s)", input$time_units),
          y = "Cumulative failures"
        ) +
        ggplot2::theme_bw(base_size = 13)

      if (isTRUE(input$log_log)) {
        p <- p + ggplot2::scale_x_log10() + ggplot2::scale_y_log10()
      }
      p
    })

    output$analyze_plot <- renderPlot({
      analyze_plot_obj()
    })

    downloadServer(
      "analyze_plot_dl",
      analyze_plot_obj,
      height = reactive(360 * 4),
      width = reactive(640 * 4)
    )

    # ----- RGT Tradeoffs -----
    output$rgt_results <- renderUI({
      out <- growth_rgt_tradeoffs(
        input$rgt_a, input$rgt_T0, input$rgt_mtbf0,
        input$rgt_mtbfI, input$rgt_systems, input$rgt_hours
      )
      if (!isTRUE(out$ok)) {
        return(HTML(paste0("<p>", htmltools::htmlEscape(out$message), "</p>")))
      }
      fmt_plain <- function(x, digits = 4) {
        format(round(as.numeric(x), digits), scientific = FALSE, trim = TRUE, big.mark = ",")
      }
      withMathJax(HTML(paste0(
        "<table>",
        "<tr><td>", withMathJax("$\\mathrm{MTBF}_C$ (cumulative)"), "</td><td>&nbsp;&nbsp;</td><td>",
        fmt_plain(out$mtbf_C), "</td></tr>",
        "<tr><td>", withMathJax("Total test time $T$"), "</td><td>&nbsp;&nbsp;</td><td>",
        fmt_plain(out$T_total), "</td></tr>",
        "<tr><td>Hours per system</td><td>&nbsp;&nbsp;</td><td>",
        fmt_plain(out$hours_per_system), "</td></tr>",
        "<tr><td><b>Number of tests</b></td><td>&nbsp;&nbsp;</td><td><b>",
        fmt_plain(out$n_tests, digits = 2), "</b></td></tr>",
        "</table>",
        "</br></br>",
        withMathJax(
          "$\\mathrm{MTBF}_C = \\mathrm{MTBF}_I\\,(1-\\alpha)$"
        ),
        "<br/>",
        withMathJax(
          "$T = T_0\\left(\\mathrm{MTBF}_C / \\mathrm{MTBF}_0\\right)^{1/\\alpha}$"
        ),
        "</p>"
      )))
    })

    # ----- Growth Plan -----
    observe({
      if (isTRUE(input$plan_use_fit)) {
        fit <- last_fit()
        if (!is.null(fit) && isTRUE(fit$ok)) {
          updateNumericInput(session, "plan_alpha", value = round(fit$alpha, 6))
          updateNumericInput(session, "plan_beta", value = round(fit$beta, 6))
        }
      }
    })

    plan_curve_obj <- reactive({
      actual <- NULL
      col <- input$plan_actual_col
      if (!is.null(col) && nzchar(as.character(col))) {
        data <- tryCatch(filtered_data(), error = function(e) NULL)
        if (!is.null(data) && col %in% names(data) && is.numeric(data[[col]])) {
          actual <- data[[col]]
        }
      }
      growth_plan_curve(
        R_target = input$plan_R,
        mission_time = input$plan_tau,
        alpha = input$plan_alpha,
        beta = input$plan_beta,
        n_points = 40L,
        actual_R = actual
      )
    })

    output$plan_summary <- renderUI({
      curve <- plan_curve_obj()
      if (!isTRUE(curve$ok)) {
        return(HTML(paste0("<p>", htmltools::htmlEscape(curve$message), "</p>")))
      }
      fmt_plain <- function(x, digits = 4) {
        format(round(as.numeric(x), digits), scientific = FALSE, trim = TRUE, big.mark = ",")
      }
      th <- "padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5; text-align: left;"
      th_r <- "padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5; text-align: right;"
      td <- "padding: 8px 12px; border: 1px solid #ddd;"
      td_r <- "padding: 8px 12px; border: 1px solid #ddd; text-align: right;"

      parts <- paste0(
        "<table style='border-collapse: collapse; margin-bottom: 8px;'>",
        "<tr><td style='", td, "'><b>Target development time T*</b></td>",
        "<td style='", td_r, "'>", fmt_plain(curve$T_star, digits = 2), "</td></tr>",
        "<tr><td style='", td, "'><b>Target reliability</b></td>",
        "<td style='", td_r, "'>", sprintf("%.4f", as.numeric(input$plan_R)), "</td></tr>",
        "</table>"
      )

      track <- curve$track
      if (!is.null(track) && nrow(track) > 0L) {
        latest <- track[nrow(track), , drop = FALSE]
        status_col <- switch(
          as.character(latest$status),
          Behind = "#a94442",
          Ahead = "#3c763d",
          "#333333"
        )
        t_plan <- latest$plan_time_for_actual
        plan_time_note <- if (is.finite(t_plan)) {
          paste0(
            " At this actual reliability, the ideal plan expected about ",
            fmt_plain(t_plan, digits = 2),
            " development time (current time ",
            fmt_plain(latest$time, digits = 2),
            "; target T* ",
            fmt_plain(curve$T_star, digits = 2),
            ")."
          )
        } else {
          ""
        }
        parts <- paste0(
          parts,
          "<p style='margin-top:12px;'><b>Latest status: </b>",
          "<span style='color:", status_col, ";font-weight:bold;'>",
          htmltools::htmlEscape(as.character(latest$status)),
          "</span>",
          " (gap = Actual − Ideal = ",
          sprintf("%+.4f", latest$gap),
          ").",
          htmltools::htmlEscape(plan_time_note),
          "</p>",
          "<p style='margin-top:8px;'><b>Plan conformance</b> ",
          "(Actual vs Ideal R at the same development time)</p>",
          "<table style='border-collapse: collapse;'>",
          "<tr>",
          "<th style='", th, "'>Time</th>",
          "<th style='", th_r, "'>Ideal R</th>",
          "<th style='", th_r, "'>Actual R</th>",
          "<th style='", th_r, "'>Gap</th>",
          "<th style='", th, "'>Status</th>",
          "</tr>"
        )
        for (i in seq_len(nrow(track))) {
          row <- track[i, , drop = FALSE]
          parts <- paste0(
            parts,
            "<tr>",
            "<td style='", td, "'>", fmt_plain(row$time, digits = 2), "</td>",
            "<td style='", td_r, "'>", sprintf("%.4f", row$ideal_R), "</td>",
            "<td style='", td_r, "'>", sprintf("%.4f", row$actual_R), "</td>",
            "<td style='", td_r, "'>", sprintf("%+.4f", row$gap), "</td>",
            "<td style='", td, "'>", htmltools::htmlEscape(as.character(row$status)), "</td>",
            "</tr>"
          )
        }
        parts <- paste0(parts, "</table>")
      } else {
        parts <- paste0(
          parts,
          "<p style='margin-top:12px;'>Select an actual reliability column to compare progress to the ideal plan.</p>"
        )
      }
      HTML(parts)
    })

    plan_plot_obj <- reactive({
      curve <- plan_curve_obj()
      req(isTRUE(curve$ok))
      cols <- colors()
      r_target <- as.numeric(input$plan_R)
      if (!is.finite(r_target)) r_target <- NA_real_

      df <- data.frame(
        time = curve$time,
        Ideal = curve$ideal_R
      )
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$time)) +
        ggplot2::geom_line(ggplot2::aes(y = .data$Ideal, color = "Ideal R"), linewidth = 1.1)

      if (is.finite(r_target)) {
        p <- p + ggplot2::geom_hline(
          ggplot2::aes(yintercept = r_target, color = "Target R"),
          linetype = "dashed",
          linewidth = 0.9
        )
      }

      if (!is.null(curve$track) && nrow(curve$track) > 0L) {
        adf <- curve$track
        p <- p +
          ggplot2::geom_line(
            data = adf,
            ggplot2::aes(x = .data$time, y = .data$actual_R, color = "Actual R"),
            linewidth = 1.0
          ) +
          ggplot2::geom_point(
            data = adf,
            ggplot2::aes(x = .data$time, y = .data$actual_R, color = "Actual R"),
            size = 2.5
          )
      }

      y_vals <- df$Ideal
      if (is.finite(r_target)) y_vals <- c(y_vals, r_target)
      if (!is.null(curve$actual_R)) {
        y_vals <- c(y_vals, curve$actual_R)
      }
      y_vals <- y_vals[is.finite(y_vals)]
      y_min <- if (length(y_vals) > 0L) min(y_vals) else 0

      color_vals <- c(
        "Ideal R" = .growth_hex(cols$col_plot_line),
        "Target R" = .growth_hex(cols$col_mean_line, fallback = "#E45756")
      )
      if (!is.null(curve$track) && nrow(curve$track) > 0L) {
        color_vals <- c(
          color_vals,
          "Actual R" = .growth_hex(cols$col_point_of_interest_line)
        )
      }

      p +
        ggplot2::scale_color_manual(
          values = color_vals,
          breaks = names(color_vals)
        ) +
        ggplot2::labs(
          title = "Reliability Growth Plan",
          x = "Development time",
          y = "Reliability R(τ)",
          color = NULL
        ) +
        ggplot2::coord_cartesian(ylim = c(y_min, 1)) +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(legend.position = "bottom")
    })

    output$plan_plot <- renderPlot({
      plan_plot_obj()
    })

    downloadServer(
      "plan_plot_dl",
      plan_plot_obj,
      height = reactive(420 * 4),
      width = reactive(640 * 4)
    )
  })
}
