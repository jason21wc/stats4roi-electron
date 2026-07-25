# Autocorrelation server logic (runs inside coordinator moduleServer namespace)

setup_autocorrelation_server <- function(input, output, session, filtered_data, reactive_color_palette) {
  `%||%` <- function(a, b) if (is.null(a) || (is.character(a) && !nzchar(a))) b else a

  analysis_rv <- reactiveVal(NULL)
  last_error <- reactiveVal(NULL)

  # Keep measure choices in sync with working data
  observe({
    data <- tryCatch(filtered_data(), error = function(e) NULL)
    cols <- character(0)
    if (!is.null(data) && is.data.frame(data) && nrow(data) > 0) {
      cols <- names(data)[vapply(data, is.numeric, logical(1))]
    }
    selected <- isolate(input$acf_measure)
    if (is.null(selected) || !nzchar(as.character(selected)) || !selected %in% cols) {
      selected <- if (length(cols) > 0) cols[[1]] else NULL
    }
    updateSelectInput(session, "acf_measure", choices = cols, selected = selected)
  })

  colors_reactive <- reactive({
    pal <- tryCatch(reactive_color_palette(), error = function(e) NULL)
    if (is.list(pal) && !is.null(pal$col_plot_line)) {
      return(pal)
    }
    if (!is.null(pal) && (is.character(pal) || is.vector(pal))) {
      return(get_distribution_colors(pal))
    }
    get_distribution_colors(get_color_palette())
  })

  observeEvent(input$acf_analyze, {
    last_error(NULL)
    analysis_rv(NULL)

    out <- tryCatch({
      data <- filtered_data()
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
        stop("No working data available. Load data under File first.")
      }
      measure <- input$acf_measure
      if (is.null(measure) || !nzchar(as.character(measure)) || !measure %in% names(data)) {
        stop("Select a numeric measure column.")
      }
      if (!is.numeric(data[[measure]])) {
        stop("Measure must be numeric.")
      }

      series <- acf_prepare_series(data[[measure]])
      if (length(series) < 2L) {
        stop("Need at least 2 non-missing numeric observations.")
      }

      lag.max <- input$acf_lag_max
      if (length(lag.max) < 1 || is.null(lag.max) || is.na(lag.max)) {
        lag.max <- NULL
      }

      conf <- input$acf_conf
      if (is.null(conf) || is.na(conf)) {
        conf <- 0.95
      }

      acf_res <- acf_compute(series, lag.max = lag.max, conf.level = conf)
      pacf_res <- pacf_compute(series, lag.max = lag.max, conf.level = conf)
      if (!is.null(acf_res$error)) {
        stop(acf_res$error)
      }

      list(
        ok = TRUE,
        series = series,
        measure = as.character(measure),
        acf = acf_res,
        pacf = pacf_res,
        omit_lag0 = isTRUE(input$acf_omit_lag0),
        decimals = if (is.null(input$acf_decimals) || is.na(input$acf_decimals)) 4L else as.integer(input$acf_decimals)
      )
    }, error = function(e) {
      last_error(conditionMessage(e))
      NULL
    })

    if (!is.null(out)) {
      analysis_rv(out)
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  output$acf_status_text <- renderText({
    err <- last_error()
    res <- analysis_rv()
    clicks <- input$acf_analyze
    if (is.null(clicks)) clicks <- 0L

    if (!is.null(err)) {
      return(paste0("Error: ", err))
    }
    if (clicks < 1 || is.null(res)) {
      return("Select a measure and click Analyze.")
    }
    paste0(
      "n = ", res$acf$n.used,
      " | ACF_crit = ", format(round(res$acf$crit, res$decimals), nsmall = res$decimals),
      " | Confidence = ", res$acf$conf.level,
      " | Measure = ", res$measure
    )
  })

  run_plot <- reactive({
    res <- analysis_rv()
    req(!is.null(res))
    acf_run_sequence_plot(res$series, colors = colors_reactive(), ylab = res$measure)
  })

  acf_plot_r <- reactive({
    res <- analysis_rv()
    req(!is.null(res))
    acf_correlation_plot(res$acf, omit_lag0 = res$omit_lag0, colors = colors_reactive())
  })

  pacf_plot_r <- reactive({
    res <- analysis_rv()
    req(!is.null(res))
    acf_correlation_plot(res$pacf, omit_lag0 = FALSE, colors = colors_reactive())
  })

  output$acf_run_sequence <- renderPlot({
    res <- analysis_rv()
    validate(need(!is.null(res), last_error() %||% "Click Analyze to generate plots."))
    run_plot()
  })

  output$acf_plot <- renderPlot({
    res <- analysis_rv()
    validate(need(!is.null(res), last_error() %||% "Click Analyze to generate plots."))
    acf_plot_r()
  })

  output$pacf_plot <- renderPlot({
    res <- analysis_rv()
    validate(need(!is.null(res), last_error() %||% "Click Analyze to generate plots."))
    pacf_plot_r()
  })

  .sig_table_html <- function(title, lag, value, crit_label, decimals) {
    if (length(lag) == 0) {
      return(paste0(
        "<u>", title, "</u>",
        "<table><tr><td>", crit_label, "</td></tr>",
        "<tr><td>No significant autocorrelation found</td></tr></table><br/>"
      ))
    }
    rows <- vapply(seq_along(lag), function(i) {
      paste0(
        "<tr><td>Lag = ", lag[i], "</td><td>Value = ",
        format(round(value[i], decimals), nsmall = decimals),
        "</td></tr>"
      )
    }, character(1))
    paste0(
      "<u>", title, "</u>",
      "<table><tr><td>", crit_label, "</td></tr>",
      paste(rows, collapse = ""),
      "</table><br/>"
    )
  }

  output$acf_sig_summary <- renderUI({
    res <- analysis_rv()
    if (is.null(res)) {
      return(NULL)
    }
    d <- res$decimals
    crit <- res$acf$crit
    crit_fmt <- format(round(crit, d), nsmall = d)

    HTML(paste(
      .sig_table_html(
        "ACF greater than ACF<sub>crit</sub>",
        res$acf$significant$lag_up,
        res$acf$significant$acf_up,
        paste0("ACF<sub>crit</sub> = ", crit_fmt, " &nbsp; Confidence = ", res$acf$conf.level),
        d
      ),
      .sig_table_html(
        "ACF less than -ACF<sub>crit</sub>",
        res$acf$significant$lag_low,
        res$acf$significant$acf_low,
        paste0("-ACF<sub>crit</sub> = ", format(round(-crit, d), nsmall = d)),
        d
      ),
      .sig_table_html(
        "PACF greater than PACF<sub>crit</sub>",
        res$pacf$significant$lag_up,
        res$pacf$significant$acf_up,
        paste0("PACF<sub>crit</sub> = ", format(round(res$pacf$crit, d), nsmall = d)),
        d
      ),
      .sig_table_html(
        "PACF less than -PACF<sub>crit</sub>",
        res$pacf$significant$lag_low,
        res$pacf$significant$acf_low,
        paste0("-PACF<sub>crit</sub> = ", format(round(-res$pacf$crit, d), nsmall = d)),
        d
      ),
      sep = "\n"
    ))
  })

  plot_width <- reactive(400 * 4)
  plot_height <- reactive(280 * 4)
  downloadServer("dl_acf_run_sequence", run_plot, height = plot_height, width = plot_width)
  downloadServer("dl_acf_plot", acf_plot_r, height = plot_height, width = plot_width)
  downloadServer("dl_pacf_plot", pacf_plot_r, height = plot_height, width = plot_width)

  invisible(NULL)
}
