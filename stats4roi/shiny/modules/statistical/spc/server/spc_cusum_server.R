# CUSUM tab server — data prep UI, analysis reactive, plots, summary, DT.

register_spc_cusum_server <- function(input, output, session, filtered_data, reactive_color_palette) {
  ns <- session$ns

  output$spc_cusum_sigma_method_ui <- renderUI({
    ind <- spc_is_switch_on(input$spc_cusum_ind_or_mean)
    if (isTRUE(ind)) {
      radioButtons(
        ns("spc_cusum_sigma_method"),
        "Estimate \u03c3 from",
        choices = c(
          "Average Moving Range" = "mean",
          "Median Moving Range" = "median"
        ),
        selected = input$spc_cusum_sigma_method %||% "mean"
      )
    } else {
      radioButtons(
        ns("spc_cusum_sigma_method"),
        "Estimate \u03c3 from",
        choices = c(
          "Average Range" = "mean",
          "Median Range" = "median"
        ),
        selected = input$spc_cusum_sigma_method %||% "mean"
      )
    }
  })

  output$spc_cusum_UI1 <- renderUI({
    data <- filtered_data()
    req(data)
    choices <- seq_len(ncol(data))
    names(choices) <- names(data)
    data_type <- input$spc_cusum_data_type
    ind_chart <- spc_is_switch_on(input$spc_cusum_ind_or_mean)

    if (isTRUE(ind_chart)) {
      return(pickerInput(
        inputId = ns("spc_cusum_UI1"),
        label = "Select Data Column",
        multiple = FALSE,
        options = list(`actions-box` = TRUE),
        choices = choices
      ))
    }
    if (identical(as.numeric(data_type), 1)) {
      return(pickerInput(
        inputId = ns("spc_cusum_UI1"),
        label = "Select Data Columns",
        multiple = TRUE,
        options = list(`actions-box` = TRUE),
        choices = choices
      ))
    }
    tagList(
      pickerInput(
        inputId = ns("spc_cusum_UI1"),
        label = "Select Sample Column",
        multiple = FALSE,
        options = list(`actions-box` = TRUE),
        choices = choices
      ),
      helpText("Rows with the same sample value are combined into one subgroup.")
    )
  })

  output$spc_cusum_UI2 <- renderUI({
    data <- filtered_data()
    req(data)
    ind_chart <- spc_is_switch_on(input$spc_cusum_ind_or_mean)
    if (isTRUE(ind_chart)) return(NULL)
    if (identical(as.numeric(input$spc_cusum_data_type), 1)) return(NULL)

    choices <- seq_len(ncol(data))
    names(choices) <- names(data)
    UI1 <- input$spc_cusum_UI1
    req(UI1)
    fact_selected <- as.numeric(unlist(strsplit(x = as.character(UI1), split = "\\s+")))
    temp <- seq_len(length(choices))
    temp <- temp[-fact_selected]
    choices <- choices[temp]
    pickerInput(
      inputId = ns("spc_cusum_UI2"),
      label = "Select Data Column",
      multiple = FALSE,
      options = list(`actions-box` = TRUE),
      choices = choices
    )
  })

  output$spc_cusum_set <- renderUI({
    data <- filtered_data()
    req(data)
    choices <- seq_len(ncol(data))
    names(choices) <- names(data)
    UI1 <- as.numeric(input$spc_cusum_UI1)
    UI2 <- as.numeric(input$spc_cusum_UI2)
    req(UI1)
    ind_chart <- spc_is_switch_on(input$spc_cusum_ind_or_mean)
    data_type <- as.numeric(input$spc_cusum_data_type)

    if (isTRUE(ind_chart) || identical(data_type, 1)) {
      fact_selected <- c(UI1)
    } else {
      req(UI2)
      fact_selected <- c(UI1, UI2)
    }
    fact_selected <- fact_selected[is.finite(fact_selected)]
    temp <- seq_len(length(choices))
    temp <- setdiff(temp, fact_selected)
    choices <- c("None" = 0, choices[temp])
    pickerInput(
      inputId = ns("spc_cusum_set"),
      label = "Sets Column (optional)",
      multiple = FALSE,
      choices = choices,
      selected = 0
    )
  })

  cusum_result <- reactive({
    data <- filtered_data()
    req(data)
    UI1 <- as.numeric(input$spc_cusum_UI1)
    req(UI1)
    target <- as.numeric(input$spc_cusum_target)
    validate(need(is.finite(target), "Enter a CUSUM target."))

    k <- as.numeric(input$spc_cusum_k)
    alpha <- as.numeric(input$spc_cusum_alpha)
    beta <- as.numeric(input$spc_cusum_beta)
    if (is.null(beta) || length(beta) == 0L || is.na(beta)) beta <- 0
    validate(need(is.finite(k) && k > 0, "k must be positive."))
    validate(need(is.finite(alpha) && alpha > 0 && alpha < 1, "alpha must be between 0 and 1."))
    validate(need(is.finite(beta) && beta >= 0 && beta < 1, "beta must be in [0, 1)."))

    ind_chart <- spc_is_switch_on(input$spc_cusum_ind_or_mean)
    mode <- if (isTRUE(ind_chart)) "individuals" else "means"
    data_type <- as.numeric(input$spc_cusum_data_type %||% 1)
    UI2 <- as.numeric(input$spc_cusum_UI2)
    sets_col <- as.numeric(input$spc_cusum_set %||% 0)
    sigma_method <- input$spc_cusum_sigma_method %||% "mean"
    mr_span <- as.numeric(input$spc_cusum_mr_span %||% 2)

    prepared <- tryCatch(
      spc_prepare_cusum_data(
        data = data,
        mode = mode,
        data_type = data_type,
        ui1 = UI1,
        ui2 = UI2,
        sets_col = sets_col
      ),
      error = function(e) {
        validate(need(FALSE, conditionMessage(e)))
        NULL
      }
    )
    req(prepared)
    validate(need(length(prepared$values) >= 2L, "Need at least 2 observations for CUSUM."))

    tryCatch(
      spc_analyze_cusum(
        prepared = prepared,
        target = target,
        k = k,
        alpha = alpha,
        beta = beta,
        sigma_method = sigma_method,
        mr_span = mr_span
      ),
      error = function(e) {
        validate(need(FALSE, conditionMessage(e)))
        NULL
      }
    )
  })

  cusum_plot_gg <- reactive({
    result <- cusum_result()
    req(result)
    colors <- reactive_color_palette()
    if (is.null(colors) || length(colors) < 4) colors <- palette.colors(8)
    font_size <- as.numeric(input$spc_cusum_font_size %||% 11)
    if (!is.finite(font_size)) font_size <- 11
    highlight <- input$spc_cusum_highlight %||% "si_ti"
    spc_plot_cusum(
      result = result,
      highlight = highlight,
      colors = colors,
      base_size = font_size,
      show_decision_band = isTRUE(input$spc_cusum_show_H)
    )
  })

  companion_plot_gg <- reactive({
    result <- cusum_result()
    req(result)
    colors <- reactive_color_palette()
    if (is.null(colors) || length(colors) < 4) colors <- palette.colors(8)
    font_size <- as.numeric(input$spc_cusum_font_size %||% 11)
    if (!is.finite(font_size)) font_size <- 11
    spc_plot_cusum_companion(result, colors = colors, base_size = font_size)
  })

  output$cusum_chart_out <- renderPlot({
    cusum_plot_gg()
  })

  output$cusum_companion_out <- renderPlot({
    companion_plot_gg()
  })

  cusum_w <- reactive(400 * 8)
  cusum_h <- reactive(200 * 8)
  companion_h <- reactive(160 * 8)
  downloadServer("cusum_chart_out", cusum_plot_gg, height = cusum_h, width = cusum_w)
  downloadServer("cusum_companion_out", companion_plot_gg, height = companion_h, width = cusum_w)

  output$cusum_summary_out <- renderUI({
    result <- cusum_result()
    req(result)
    HTML(spc_build_cusum_summary_html(result, digits = input$spc_cusum_decimals %||% 4L))
  })

  output$cusum_table_out <- DT::renderDataTable({
    result <- cusum_result()
    req(result)
    R <- as.integer(input$spc_cusum_decimals %||% 4L)
    tab <- result$table
    use_star <- identical(input$spc_cusum_highlight %||% "si_ti", "si_ti_star")
    signal_flag <- if (use_star) tab$highlight_si_ti_star else tab$highlight_si_ti
    out <- data.frame(
      Sample = tab$Sample,
      Value = round(tab$Value, R),
      MR = round(tab$MR, R),
      Dev = round(tab$Dev, R),
      Cusum = round(tab$Cusum, R),
      Zi = round(tab$Zi, R),
      Si = round(tab$Si, R),
      Ti = round(tab$Ti, R),
      `Si*` = round(tab$Si_star, R),
      `Ti*` = round(tab$Ti_star, R),
      Signal = ifelse(signal_flag, "Yes", ""),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    DT::datatable(out, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })

  output$cusum_hover_info <- renderUI({
    hover <- input$cusum_plot_hover
    result <- cusum_result()
    req(hover, result)
    tab <- result$table
    point <- nearPoints(
      tab,
      hover,
      xvar = "Sample",
      yvar = "Cusum",
      maxpoints = 1,
      threshold = 10
    )
    if (nrow(point) == 0L) return(NULL)
    R <- as.integer(input$spc_cusum_decimals %||% 4L)
    wellPanel(
      style = paste0(
        "position:absolute; z-index:100; background-color:rgba(245,245,245,0.92); ",
        "left:", hover$coords_css$x + 10, "px; top:", hover$coords_css$y + 10, "px;"
      ),
      HTML(paste0(
        "<b>Sample:</b> ", point$Sample[1], "<br/>",
        "<b>Value:</b> ", spc_cusum_fmt(point$Value[1], R), "<br/>",
        "<b>Cusum:</b> ", spc_cusum_fmt(point$Cusum[1], R), "<br/>",
        "<b>Si:</b> ", spc_cusum_fmt(point$Si[1], R), "<br/>",
        "<b>Ti:</b> ", spc_cusum_fmt(point$Ti[1], R), "<br/>",
        "<b>Si*:</b> ", spc_cusum_fmt(point$Si_star[1], R), "<br/>",
        "<b>Ti*:</b> ", spc_cusum_fmt(point$Ti_star[1], R)
      ))
    )
  })

  invisible(list(result = cusum_result))
}

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
