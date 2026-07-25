# EWMA tab server — data prep UI, analysis reactive, plots, summary, DT.

register_spc_ewma_server <- function(input, output, session, filtered_data, reactive_color_palette) {
  ns <- session$ns

  output$spc_ewma_sigma_method_ui <- renderUI({
    ind <- spc_is_switch_on(input$spc_ewma_ind_or_mean)
    if (isTRUE(ind)) {
      radioButtons(
        ns("spc_ewma_sigma_method"),
        "Estimate \u03c3 from",
        choices = c(
          "Average Moving Range" = "mean",
          "Median Moving Range" = "median"
        ),
        selected = input$spc_ewma_sigma_method %||% "mean"
      )
    } else {
      radioButtons(
        ns("spc_ewma_sigma_method"),
        "Estimate \u03c3 from",
        choices = c(
          "Average Range" = "mean",
          "Median Range" = "median"
        ),
        selected = input$spc_ewma_sigma_method %||% "mean"
      )
    }
  })

  output$spc_ewma_UI1 <- renderUI({
    data <- filtered_data()
    req(data)
    choices <- seq_len(ncol(data))
    names(choices) <- names(data)
    data_type <- input$spc_ewma_data_type
    ind_chart <- spc_is_switch_on(input$spc_ewma_ind_or_mean)

    if (isTRUE(ind_chart)) {
      return(pickerInput(
        inputId = ns("spc_ewma_UI1"),
        label = "Select Data Column",
        multiple = FALSE,
        options = list(`actions-box` = TRUE),
        choices = choices
      ))
    }
    if (identical(as.numeric(data_type), 1)) {
      return(pickerInput(
        inputId = ns("spc_ewma_UI1"),
        label = "Select Data Columns",
        multiple = TRUE,
        options = list(`actions-box` = TRUE),
        choices = choices
      ))
    }
    tagList(
      pickerInput(
        inputId = ns("spc_ewma_UI1"),
        label = "Select Sample Column",
        multiple = FALSE,
        options = list(`actions-box` = TRUE),
        choices = choices
      ),
      helpText("Rows with the same sample value are combined into one subgroup.")
    )
  })

  output$spc_ewma_UI2 <- renderUI({
    data <- filtered_data()
    req(data)
    ind_chart <- spc_is_switch_on(input$spc_ewma_ind_or_mean)
    if (isTRUE(ind_chart)) return(NULL)
    if (identical(as.numeric(input$spc_ewma_data_type), 1)) return(NULL)

    choices <- seq_len(ncol(data))
    names(choices) <- names(data)
    UI1 <- input$spc_ewma_UI1
    req(UI1)
    fact_selected <- as.numeric(unlist(strsplit(x = as.character(UI1), split = "\\s+")))
    temp <- seq_len(length(choices))
    temp <- temp[-fact_selected]
    choices <- choices[temp]
    pickerInput(
      inputId = ns("spc_ewma_UI2"),
      label = "Select Data Column",
      multiple = FALSE,
      options = list(`actions-box` = TRUE),
      choices = choices
    )
  })

  output$spc_ewma_set <- renderUI({
    data <- filtered_data()
    req(data)
    choices <- seq_len(ncol(data))
    names(choices) <- names(data)
    UI1 <- as.numeric(input$spc_ewma_UI1)
    UI2 <- as.numeric(input$spc_ewma_UI2)
    req(UI1)
    ind_chart <- spc_is_switch_on(input$spc_ewma_ind_or_mean)
    data_type <- as.numeric(input$spc_ewma_data_type)

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
      inputId = ns("spc_ewma_set"),
      label = "Sets Column (optional)",
      multiple = FALSE,
      choices = choices,
      selected = 0
    )
  })

  ewma_result <- reactive({
    data <- filtered_data()
    req(data)
    UI1 <- as.numeric(input$spc_ewma_UI1)
    req(UI1)
    target <- as.numeric(input$spc_ewma_target)
    validate(need(is.finite(target), "Enter an EWMA target."))

    alpha <- as.numeric(input$spc_ewma_alpha)
    L <- as.numeric(input$spc_ewma_L %||% 3)
    validate(need(is.finite(alpha) && alpha > 0 && alpha < 1, "alpha must be between 0 and 1."))
    validate(need(is.finite(L) && L > 0, "L must be positive."))

    ind_chart <- spc_is_switch_on(input$spc_ewma_ind_or_mean)
    mode <- if (isTRUE(ind_chart)) "individuals" else "means"
    data_type <- as.numeric(input$spc_ewma_data_type %||% 1)
    UI2 <- as.numeric(input$spc_ewma_UI2)
    sets_col <- as.numeric(input$spc_ewma_set %||% 0)
    sigma_method <- input$spc_ewma_sigma_method %||% "mean"
    mr_span <- as.numeric(input$spc_ewma_mr_span %||% 2)
    seed_n <- as.numeric(input$spc_ewma_seed_n %||% 5)
    if (!is.finite(seed_n) || seed_n < 1) seed_n <- 5

    prepared <- tryCatch(
      spc_prepare_ewma_data(
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
    validate(need(length(prepared$values) >= 2L, "Need at least 2 observations for EWMA."))

    tryCatch(
      spc_analyze_ewma(
        prepared = prepared,
        target = target,
        alpha = alpha,
        L = L,
        sigma_method = sigma_method,
        mr_span = mr_span,
        seed_n = seed_n
      ),
      error = function(e) {
        validate(need(FALSE, conditionMessage(e)))
        NULL
      }
    )
  })

  ewma_plot_gg <- reactive({
    result <- ewma_result()
    req(result)
    colors <- reactive_color_palette()
    if (is.null(colors) || length(colors) < 4) colors <- palette.colors(8)
    font_size <- as.numeric(input$spc_ewma_font_size %||% 11)
    if (!is.finite(font_size)) font_size <- 11
    spc_plot_ewma(
      result = result,
      colors = colors,
      base_size = font_size
    )
  })

  companion_plot_gg <- reactive({
    result <- ewma_result()
    req(result)
    colors <- reactive_color_palette()
    if (is.null(colors) || length(colors) < 4) colors <- palette.colors(8)
    font_size <- as.numeric(input$spc_ewma_font_size %||% 11)
    if (!is.finite(font_size)) font_size <- 11
    spc_plot_ewma_companion(result, colors = colors, base_size = font_size)
  })

  output$ewma_chart_out <- renderPlot({
    ewma_plot_gg()
  })

  output$ewma_companion_out <- renderPlot({
    companion_plot_gg()
  })

  ewma_w <- reactive(400 * 8)
  ewma_h <- reactive(200 * 8)
  companion_h <- reactive(160 * 8)
  downloadServer("ewma_chart_out", ewma_plot_gg, height = ewma_h, width = ewma_w)
  downloadServer("ewma_companion_out", companion_plot_gg, height = companion_h, width = ewma_w)

  output$ewma_summary_out <- renderUI({
    result <- ewma_result()
    req(result)
    HTML(spc_build_ewma_summary_html(result, digits = input$spc_ewma_decimals %||% 4L))
  })

  output$ewma_table_out <- DT::renderDataTable({
    result <- ewma_result()
    req(result)
    R <- as.integer(input$spc_ewma_decimals %||% 4L)
    tab <- result$table
    out <- data.frame(
      Sample = tab$Sample,
      Value = round(tab$Value, R),
      MR = round(tab$MR, R),
      Dev = round(tab$Dev, R),
      EWMA = round(tab$EWMA, R),
      UCL = round(tab$UCL, R),
      LCL = round(tab$LCL, R),
      Target = round(tab$Target, R),
      OOC = ifelse(tab$OOC, "Yes", ""),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    DT::datatable(out, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })

  output$ewma_hover_info <- renderUI({
    hover <- input$ewma_plot_hover
    result <- ewma_result()
    req(hover, result)
    tab <- result$table
    point <- nearPoints(
      tab,
      hover,
      xvar = "Sample",
      yvar = "EWMA",
      maxpoints = 1,
      threshold = 10
    )
    if (nrow(point) == 0L) return(NULL)
    R <- as.integer(input$spc_ewma_decimals %||% 4L)
    wellPanel(
      style = paste0(
        "position:absolute; z-index:100; background-color:rgba(245,245,245,0.92); ",
        "left:", hover$coords_css$x + 10, "px; top:", hover$coords_css$y + 10, "px;"
      ),
      HTML(paste0(
        "<b>Sample:</b> ", point$Sample[1], "<br/>",
        "<b>Value:</b> ", spc_ewma_fmt(point$Value[1], R), "<br/>",
        "<b>EWMA:</b> ", spc_ewma_fmt(point$EWMA[1], R), "<br/>",
        "<b>UCL:</b> ", spc_ewma_fmt(point$UCL[1], R), "<br/>",
        "<b>LCL:</b> ", spc_ewma_fmt(point$LCL[1], R), "<br/>",
        "<b>OOC:</b> ", if (isTRUE(point$OOC[1])) "Yes" else "No"
      ))
    )
  })

  invisible(list(result = ewma_result))
}

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
