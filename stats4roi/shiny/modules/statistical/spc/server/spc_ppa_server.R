# Process Performance Analysis — server registration for SPC coordinator

register_spc_ppa_server <- function(input, output, session, filtered_data, reactive_color_palette) {
  ns <- session$ns

  observeEvent(input$ppa_multiple_measures_help, {
    showModal(modalDialog(
      title = "Multiple Measures Represent",
      easyClose = TRUE,
      footer = modalButton("Close"),
      HTML(paste0(
        "<p><strong>Sample Items:</strong> each selected column is a measure within ",
        "the same sample (e.g. cavity s1&ndash;s4); within-sample variation follows ",
        "your dispersion chart.</p>",
        "<p><strong>Repeated Measures:</strong> columns are remeasurements of the same ",
        "unit; the average is used for process capability and spread among columns ",
        "estimates measurement error.</p>"
      ))
    ))
  }, ignoreInit = TRUE)

  ppa_empty_message_dt <- function(message) {
    DT::datatable(
      data.frame(Message = message),
      rownames = FALSE,
      options = list(dom = "t", ordering = FALSE)
    )
  }

  ppa_numeric_cols <- reactive({
    data <- filtered_data()
    req(data)
    names(data)[vapply(data, function(x) is.numeric(x) || all(!is.na(suppressWarnings(as.numeric(x)))), logical(1))]
  })

  output$ppa_response_ui <- renderUI({
    cols <- ppa_numeric_cols()
    pickerInput(
      ns("ppa_response"),
      "Response column(s)",
      choices = cols,
      selected = if (length(cols)) cols[1] else NULL,
      multiple = TRUE,
      options = list(`live-search` = TRUE, `actions-box` = TRUE)
    )
  })

  output$ppa_sample_ui <- renderUI({
    data <- filtered_data()
    req(data)
    cols <- names(data)
    sample_choices <- c(
      "Observations in Time Order" = ppa_sample_time_order_id(),
      cols
    )
    pickerInput(
      ns("ppa_sample_id"),
      "Sample / time column",
      choices = sample_choices,
      selected = ppa_sample_time_order_id(),
      options = list(`live-search` = TRUE)
    )
  })

  stream_factor_order <- reactiveVal(character(0))

  observeEvent(input$ppa_stream_factors, {
    sel <- input$ppa_stream_factors %||% character(0)
    prev <- isolate(stream_factor_order())
    stream_factor_order(c(intersect(prev, sel), setdiff(sel, prev)))
  }, ignoreNULL = FALSE)

  observeEvent(input$ppa_stream_move_up, {
    fac <- input$ppa_stream_move_factor
    ord <- isolate(stream_factor_order())
    i <- match(fac, ord)
    if (!is.na(i) && i > 1) {
      ord[c(i - 1, i)] <- ord[c(i, i - 1)]
      stream_factor_order(ord)
    }
  })

  observeEvent(input$ppa_stream_move_down, {
    fac <- input$ppa_stream_move_factor
    ord <- isolate(stream_factor_order())
    i <- match(fac, ord)
    if (!is.na(i) && i < length(ord)) {
      ord[c(i, i + 1)] <- ord[c(i + 1, i)]
      stream_factor_order(ord)
    }
  })

  output$ppa_stream_ui <- renderUI({
    data <- filtered_data()
    req(data)
    cols <- names(data)
    pickerInput(
      ns("ppa_stream_factors"),
      "Process stream factor(s)",
      choices = cols,
      multiple = TRUE,
      options = list(`live-search` = TRUE, `actions-box` = TRUE)
    )
  })

  output$ppa_stream_order_ui <- renderUI({
    ord <- stream_factor_order()
    if (length(ord) < 2) {
      return(helpText("Select two or more stream factors to set nesting order (outermost first)."))
    }
    tagList(
      helpText("Hierarchy outermost \u2192 innermost:"),
      tags$ol(lapply(ord, function(f) tags$li(f))),
      selectInput(
        ns("ppa_stream_move_factor"),
        "Reorder factor",
        choices = ord,
        selected = ord[1]
      ),
      tags$div(
        actionButton(ns("ppa_stream_move_up"), "Move up", class = "btn-sm"),
        actionButton(ns("ppa_stream_move_down"), "Move down", class = "btn-sm")
      )
    )
  })

  ppa_data_shape_r <- reactive({
    mappings <- ppa_mappings()
    mappings$data_shape %||% "single"
  })

  # Shape-only: remapping response columns must not recreate limit pickers
  # (that resets selections and can fight updateSelectInput).
  output$ppa_chart_limits_ui <- renderUI({
    shape <- ppa_data_shape_r()
    switch(
      shape,
      subgroup = create_spc_xbar_chart_limits_ui(ns, "ppa"),
      replicate = create_spc_replicate_chart_limits_ui(ns, "ppa"),
      create_spc_individuals_chart_limits_ui(ns, "ppa")
    )
  })

  observeEvent(ppa_data_shape_r(), {
    shape <- ppa_data_shape_r()
    isolate(ppa_apply_limit_defaults_to_inputs(session, shape, "ppa"))
  }, ignoreInit = TRUE)

  # Rebuild on shape only. Dispersion-type changes use updateSelectInput below —
  # do not bind selection to input$ppa_disp_lim (that recreates the widget and
  # flaps between shape defaults vs type defaults, e.g. Median vs Average MR).
  output$ppa_disp_lim_ui <- renderUI({
    shape <- ppa_data_shape_r()
    if (!shape %in% c("subgroup", "replicate")) {
      return(NULL)
    }
    defs <- ppa_default_limit_config(shape)
    disp_type <- isolate(as.integer(input$ppa_disp_type %||% defs$disp_type))
    ch <- if (identical(shape, "replicate")) {
      spc_replicate_disp_lim_choices()
    } else {
      spc_disp_lim_choices_for_type(disp_type, replicate_means = FALSE)
    }
    selectInput(
      ns("ppa_disp_lim"),
      label = "Dispersion limit calculation",
      choices = ch,
      selected = defs$disp_lim
    )
  })

  observeEvent(input$ppa_disp_type, {
    shape <- isolate(ppa_data_shape_r())
    if (!shape %in% c("subgroup", "replicate")) {
      return()
    }
    disp_type <- suppressWarnings(as.numeric(input$ppa_disp_type))
    req(!is.na(disp_type))
    type_def <- spc_default_limits_for_disp_type(disp_type)$disp_lim
    ch <- if (identical(shape, "replicate")) {
      spc_replicate_disp_lim_choices()
    } else {
      spc_disp_lim_choices_for_type(disp_type, replicate_means = FALSE)
    }
    # Prefer current selection when still valid (avoids wiping shape defaults
    # such as Median Range when apply_defaults also updates disp_type).
    selected <- spc_resolve_limit_selection(
      isolate(input$ppa_disp_lim),
      type_def,
      ch
    )
    updateSelectInput(session, "ppa_disp_lim", choices = ch, selected = selected)
  }, ignoreInit = TRUE)

  output$ppa_custom_disp_limits_ui <- renderUI({
    disp_lim <- suppressWarnings(as.integer(input$ppa_disp_lim))
    req(!is.na(disp_lim), disp_lim == 12L)
    ppa_custom_disp_limits_ui_content(ns, "ppa")
  })

  output$ppa_known_sigma_ui <- renderUI({
    loc_lim <- suppressWarnings(as.integer(input$ppa_loc_lim))
    disp_lim <- suppressWarnings(as.integer(input$ppa_disp_lim))
    if (is.na(loc_lim)) loc_lim <- 0L
    if (is.na(disp_lim)) disp_lim <- 0L
    req(loc_lim == 9L || disp_lim == 9L)
    ppa_known_sigma_ui_content(ns, "ppa")
  })

  ppa_mappings <- reactive({
    mappings <- ppa_collect_mappings(input, ns)
    ord <- stream_factor_order()
    if (length(ord) > 0) {
      mappings$stream_factors <- ord
    }
    mappings
  })

  ppa_spec_reactive <- reactive({
    target <- input$ppa_target
    if (is.null(target) || length(target) == 0 || is.na(target)) {
      target <- NA_real_
    }
    list(
      lsl = input$ppa_lsl,
      usl = input$ppa_usl,
      target = target
    )
  })

  ppa_prepared <- reactive({
    data <- filtered_data()
    req(data)
    mappings <- ppa_mappings()
    tryCatch(ppa_prepare_from_inputs(data, mappings), error = function(e) NULL)
  })

  ppa_limit_config <- reactive({
    mappings <- ppa_mappings()
    shape <- mappings$data_shape %||% "single"
    ppa_build_limit_config(input, "ppa", data_shape = shape)
  })

  ppa_result <- reactive({
    prepared <- ppa_prepared()
    spec <- ppa_spec_reactive()
    mappings <- ppa_mappings()
    req(prepared, spec)
    readiness <- ppa_readiness_checks(prepared, spec, mappings)
    req(readiness$ok)
    ppa_analyze(
      prepared = prepared,
      spec = spec,
      limit_config = ppa_limit_config(),
      stream_factors = mappings$stream_factors
    )
  })

  output$ppa_readiness_out <- renderUI({
    prepared <- ppa_prepared()
    spec <- ppa_spec_reactive()
    mappings <- ppa_mappings()
    readiness <- ppa_readiness_checks(prepared, spec, mappings)
    ppa_render_readiness_html(readiness, R = input$ppa_decimals %||% 4)
  })

  output$ppa_summary_out <- renderUI({
    result <- ppa_result()
    req(result)
    R <- as.integer(input$ppa_decimals %||% 4)
    HTML(ppa_format_indices_html(result, ppa_spec_reactive(), R))
  })

  output$ppa_descriptives_out <- renderUI({
    result <- ppa_result()
    req(result)
    R <- as.integer(input$ppa_decimals %||% 4)
    HTML(ppa_format_descriptives_html(result$descriptives, R))
  })

  output$ppa_variance_components_out <- renderUI({
    result <- ppa_result()
    req(result)
    R <- as.integer(input$ppa_decimals %||% 4)
    HTML(ppa_format_variance_components_html(
      result$variance,
      ppa_spec_reactive(),
      R,
      measurement = result$measurement_error,
      nested = result$nested_variance
    ))
  })

  outputOptions(output, "ppa_descriptives_out", suspendWhenHidden = FALSE)
  outputOptions(output, "ppa_variance_components_out", suspendWhenHidden = FALSE)

  output$ppa_stream_table <- DT::renderDT({
    prepared <- ppa_prepared()
    spec <- ppa_spec_reactive()
    mappings <- ppa_mappings()
    if (is.null(prepared)) {
      return(ppa_empty_message_dt("Select response and process stream columns."))
    }
    readiness <- ppa_readiness_checks(prepared, spec, mappings)
    if (!isTRUE(readiness$ok)) {
      failed <- vapply(readiness$checks, function(ch) {
        if (!isTRUE(ch$ok)) ch$label else NA_character_
      }, character(1))
      msg <- paste(na.omit(failed), collapse = "; ")
      return(ppa_empty_message_dt(msg))
    }
    result <- ppa_result()
    if (is.null(result) || nrow(result$stream_table) == 0) {
      return(ppa_empty_message_dt("Process stream analysis is not available for the current selections."))
    }
    R <- as.integer(input$ppa_decimals %||% 4)
    display <- ppa_format_stream_table_display(result$stream_table, ppa_limit_config())
    num_cols <- setdiff(names(display), c("Group", "n"))
    dt <- DT::datatable(
      display,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "tip",
        ordering = FALSE,
        columnDefs = list(list(className = "dt-right", targets = seq(1, ncol(display) - 1)))
      ),
      rownames = FALSE,
      class = "compact stripe hover"
    )
    DT::formatRound(dt, columns = num_cols, digits = R)
  })

  output$ppa_stream_table_footer <- renderUI({
    prepared <- ppa_prepared()
    spec <- ppa_spec_reactive()
    mappings <- ppa_mappings()
    if (is.null(prepared)) {
      return(NULL)
    }
    readiness <- ppa_readiness_checks(prepared, spec, mappings)
    if (!isTRUE(readiness$ok)) {
      return(NULL)
    }
    result <- ppa_result()
    req(result)
    R <- as.integer(input$ppa_decimals %||% 4)
    sd_rep <- result$measurement_error$s_replicates %||% NULL
    HTML(ppa_format_stream_table_footer_html(
      result$sd_within_stream,
      result$sd_potential,
      ppa_limit_config(),
      R,
      sd_repeated_measures = sd_rep
    ))
  })

  outputOptions(output, "ppa_stream_table", suspendWhenHidden = FALSE)
  outputOptions(output, "ppa_stream_table_footer", suspendWhenHidden = FALSE)

  ppa_colors <- reactive({
    pal <- reactive_color_palette()
    c(get_distribution_colors(pal), list(palette = pal))
  })

  ppa_base_font <- reactive({
    fs <- suppressWarnings(as.numeric(input$ppa_font_size))
    if (is.na(fs)) 11 else fs
  })

  ppa_plot_opts <- function() {
    list(
      base_size = ppa_base_font(),
      show_spec_limits = isTRUE(input$ppa_show_spec_limits)
    )
  }

  output$ppa_stacked_bar <- renderPlot({
    result <- ppa_result()
    req(result)
    spec <- ppa_spec_reactive()
    opts <- ppa_plot_opts()
    bar_data <- ppa_performance_step_data(result$indices, ppa_has_target(spec))
    ppa_plot_performance_steps(
      bar_data,
      ppa_colors(),
      ppa_has_target(spec),
      opts$base_size
    )
  })

  output$ppa_opportunity_bar <- renderPlot({
    result <- ppa_result()
    req(result)
    spec <- ppa_spec_reactive()
    opts <- ppa_plot_opts()
    if (!ppa_has_target(spec) && is.na(result$indices$ppk)) {
      return(NULL)
    }
    bar_data <- ppa_opportunity_analysis_data(result, spec)
    ppa_plot_opportunity_analysis(
      bar_data,
      ppa_colors(),
      ppa_has_target(spec),
      opts$base_size
    )
  })

  output$ppa_variance_bar <- renderPlot({
    result <- ppa_result()
    req(result)
    opts <- ppa_plot_opts()
    bar_data <- ppa_variance_bar_data(result$variance, result$nested_variance, result$measurement_error)
    ppa_plot_variance_bars(bar_data, colors = ppa_colors(), base_size = opts$base_size)
  })

  output$ppa_nested_variance_bar <- renderPlot({
    result <- ppa_result()
    req(result)
    opts <- ppa_plot_opts()
    bar_data <- ppa_variance_bar_data(result$variance, result$nested_variance, result$measurement_error)
    p <- ppa_plot_nested_variance_bars(bar_data, colors = ppa_colors(), base_size = opts$base_size)
    if (is.null(p)) {
      plot.new()
      text(0.5, 0.5, "No nested stream breakdown", cex = opts$base_size / 11)
    } else {
      p
    }
  })

  output$ppa_histogram_filter_ui <- renderUI({
    prepared <- ppa_prepared()
    mappings <- ppa_mappings()
    req(prepared)
    choices <- ppa_run_chart_filter_choices(prepared, mappings$stream_factors %||% character(0))
    selected <- input$ppa_histogram_stream %||% "all"
    if (!selected %in% unname(choices)) {
      selected <- "all"
    }
    selectInput(
      ns("ppa_histogram_stream"),
      "Histogram by",
      choices = choices,
      selected = selected
    )
  })

  output$ppa_histogram <- renderPlot({
    prepared <- ppa_prepared()
    spec <- ppa_spec_reactive()
    mappings <- ppa_mappings()
    req(prepared, spec)
    opts <- ppa_plot_opts()
    selection <- input$ppa_histogram_stream %||% "all"
    subset <- ppa_prepared_for_run_chart(prepared, selection)
    xlab <- mappings$response %||% "Value"
    title <- if (identical(selection, "all")) {
      "Histogram (All data)"
    } else {
      sprintf("Histogram (%s)", selection)
    }
    ppa_plot_histogram(
      subset$response,
      spec,
      ppa_colors(),
      xlab = xlab,
      title = title,
      base_size = opts$base_size,
      show_spec_limits = opts$show_spec_limits
    )
  })

  output$ppa_boxplot <- renderPlot({
    prepared <- ppa_prepared()
    spec <- ppa_spec_reactive()
    mappings <- ppa_mappings()
    req(prepared, spec)
    opts <- ppa_plot_opts()
    ylab <- mappings$response %||% "Value"
    ppa_plot_box_by_stream(
      prepared,
      mappings$stream_factors %||% character(0),
      spec,
      ppa_colors(),
      ylab = ylab,
      base_size = opts$base_size,
      show_spec_limits = opts$show_spec_limits
    )
  })

  output$ppa_run_chart_filter_ui <- renderUI({
    prepared <- ppa_prepared()
    mappings <- ppa_mappings()
    req(prepared)
    choices <- ppa_run_chart_filter_choices(prepared, mappings$stream_factors %||% character(0))
    selected <- input$ppa_run_chart_stream %||% "all"
    if (!selected %in% unname(choices)) {
      selected <- "all"
    }
    selectInput(
      ns("ppa_run_chart_stream"),
      "Run chart by",
      choices = choices,
      selected = selected
    )
  })

  ppa_run_chart_context <- reactive({
    prepared <- ppa_prepared()
    spec <- ppa_spec_reactive()
    req(prepared, spec)
    selection <- input$ppa_run_chart_stream %||% "all"
    subset <- ppa_prepared_for_run_chart(prepared, selection)
    control_chart <- NULL
    if (!isTRUE(input$ppa_run_chart) && nrow(subset) > 0) {
      R <- as.integer(input$ppa_decimals %||% 4)
      control_chart <- tryCatch(
        ppa_compute_control_chart(subset, ppa_limit_config(), R),
        error = function(e) NULL
      )
    }
    list(
      subset = subset,
      control_chart = control_chart,
      hover_data = if (nrow(subset) > 0) {
        ppa_run_chart_hover_data(subset, control_chart)
      } else {
        NULL
      }
    )
  })

  output$ppa_run_chart <- renderPlot({
    prepared <- ppa_prepared()
    spec <- ppa_spec_reactive()
    mappings <- ppa_mappings()
    req(prepared, spec)
    opts <- ppa_plot_opts()
    ctx <- ppa_run_chart_context()
    subset <- ctx$subset
    ylab <- mappings$response %||% "Value"
    selection <- input$ppa_run_chart_stream %||% "all"
    title <- if (identical(selection, "all")) {
      "Run Chart (All data)"
    } else {
      sprintf("Run Chart (%s)", selection)
    }
    ppa_plot_run_chart(
      subset,
      spec,
      ppa_colors(),
      title = title,
      ylab = ylab,
      control_chart = ctx$control_chart,
      base_size = opts$base_size,
      show_spec_limits = opts$show_spec_limits
    )
  })

  output$ppa_run_chart_hover_info <- renderUI({
    hover <- input$ppa_run_chart_hover
    if (is.null(hover)) {
      return(NULL)
    }
    dat <- ppa_run_chart_context()$hover_data
    if (is.null(dat) || nrow(dat) == 0) {
      return(NULL)
    }
    point <- nearPoints(
      dat,
      hover,
      xvar = "idx",
      yvar = "response",
      threshold = 5,
      maxpoints = 1,
      addDist = TRUE
    )
    if (nrow(point) == 0) {
      return(NULL)
    }
    R <- as.integer(input$ppa_decimals %||% 4)
    left_px <- hover$coords_css$x
    top_px <- hover$coords_css$y
    style <- paste0(
      "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;"
    )
    has_limits <- !is.na(point$UCL[1])
    ooc_reason <- point$ooc_reason[1] %||% ""
    wellPanel(
      style = style,
      p(HTML(paste0(
        "<span style='display:block; text-transform:capitalize; text-align:center'>", point$facet, "</span>",
        "<b> Point: </b>", point$x_label, "<br/>",
        "<b> Measure: </b>", ro(point$measure, R), "<br/>",
        if (has_limits) paste0(
          "<b> UCL: </b>", ro(point$UCL, R), "<br/>",
          "<b> Centerline: </b>", ro(point$centerline, R), "<br/>",
          "<b> LCL: </b>", ro(point$LCL, R), "<br/>"
        ) else "",
        if (nzchar(ooc_reason)) paste0("<b> OOC: </b>", ooc_reason) else ""
      )))
    )
  })

  output$ppa_dispersion_section <- renderUI({
    if (isTRUE(input$ppa_run_chart)) {
      return(NULL)
    }
    ctx <- ppa_run_chart_context()
    if (is.null(ctx$control_chart) || !ppa_control_chart_has_dispersion(ctx$control_chart)) {
      return(NULL)
    }
    tagList(
      div(
        style = "position: relative;",
        plotOutput(
          ns("ppa_dispersion_chart"),
          height = "260px",
          hover = hoverOpts(ns("ppa_dispersion_chart_hover"), delay = 100, delayType = "debounce")
        ),
        uiOutput(ns("ppa_dispersion_chart_hover_info"), style = "pointer-events: none;")
      ),
      ppa_plot_download_row(ns, "ppa_dispersion_chart")
    )
  })

  output$ppa_dispersion_chart <- renderPlot({
    if (isTRUE(input$ppa_run_chart)) {
      return(NULL)
    }
    ctx <- ppa_run_chart_context()
    req(ctx$control_chart)
    ppa_plot_dispersion_chart(
      ctx$control_chart,
      ppa_colors(),
      base_size = ppa_base_font()
    )
  })

  output$ppa_dispersion_chart_hover_info <- renderUI({
    hover <- input$ppa_dispersion_chart_hover
    if (is.null(hover)) {
      return(NULL)
    }
    ctx <- ppa_run_chart_context()
    dat <- ppa_dispersion_chart_hover_data(ctx$subset, ctx$control_chart)
    if (is.null(dat) || nrow(dat) == 0) {
      return(NULL)
    }
    point <- nearPoints(
      dat,
      hover,
      xvar = "idx",
      yvar = "value",
      threshold = 5,
      maxpoints = 1,
      addDist = TRUE
    )
    if (nrow(point) == 0) {
      return(NULL)
    }
    R <- as.integer(input$ppa_decimals %||% 4)
    left_px <- hover$coords_css$x
    top_px <- hover$coords_css$y
    style <- paste0(
      "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;"
    )
    has_limits <- !is.na(point$UCL[1])
    ooc_reason <- point$ooc_reason[1] %||% ""
    wellPanel(
      style = style,
      p(HTML(paste0(
        "<span style='display:block; text-transform:capitalize; text-align:center'>", point$facet, "</span>",
        "<b> Point: </b>", point$x_label, "<br/>",
        "<b> Measure: </b>", ro(point$measure, R), "<br/>",
        if (has_limits) paste0(
          "<b> UCL: </b>", ro(point$UCL, R), "<br/>",
          "<b> Centerline: </b>", ro(point$centerline, R), "<br/>",
          "<b> LCL: </b>", ro(point$LCL, R), "<br/>"
        ) else "",
        if (nzchar(ooc_reason)) paste0("<b> OOC: </b>", ooc_reason) else ""
      )))
    )
  })

  output$ppa_run_chart_limits_out <- renderUI({
    if (isTRUE(input$ppa_run_chart)) {
      return(NULL)
    }
    ctx <- ppa_run_chart_context()
    if (is.null(ctx$control_chart)) {
      return(NULL)
    }
    R <- as.integer(input$ppa_decimals %||% 4)
    HTML(ppa_format_control_limits_html(ctx$control_chart, R))
  })

  ppa_plot_h <- reactive(300 * 4)
  ppa_plot_w <- reactive(400 * 4)
  downloadServer("ppa_stacked_bar", reactive({
    result <- ppa_result()
    req(result)
    opts <- ppa_plot_opts()
    bar_data <- ppa_performance_step_data(result$indices, ppa_has_target(ppa_spec_reactive()))
    ppa_plot_performance_steps(
      bar_data,
      ppa_colors(),
      ppa_has_target(ppa_spec_reactive()),
      opts$base_size
    )
  }), width = ppa_plot_w, height = reactive(360 * 4))
  downloadServer("ppa_opportunity_bar", reactive({
    result <- ppa_result()
    spec <- ppa_spec_reactive()
    req(result)
    opts <- ppa_plot_opts()
    if (!ppa_has_target(spec) && is.na(result$indices$ppk)) {
      return(NULL)
    }
    bar_data <- ppa_opportunity_analysis_data(result, spec)
    ppa_plot_opportunity_analysis(
      bar_data,
      ppa_colors(),
      ppa_has_target(spec),
      opts$base_size
    )
  }), width = ppa_plot_w, height = reactive(340 * 4))
  downloadServer("ppa_histogram", reactive({
    prepared <- ppa_prepared()
    spec <- ppa_spec_reactive()
    mappings <- ppa_mappings()
    req(prepared, spec)
    opts <- ppa_plot_opts()
    selection <- input$ppa_histogram_stream %||% "all"
    subset <- ppa_prepared_for_run_chart(prepared, selection)
    xlab <- mappings$response %||% "Value"
    title <- if (identical(selection, "all")) {
      "Histogram (All data)"
    } else {
      sprintf("Histogram (%s)", selection)
    }
    ppa_plot_histogram(
      subset$response,
      spec,
      ppa_colors(),
      xlab = xlab,
      title = title,
      base_size = opts$base_size,
      show_spec_limits = opts$show_spec_limits
    )
  }), width = ppa_plot_w, height = ppa_plot_h)
  downloadServer("ppa_boxplot", reactive({
    prepared <- ppa_prepared()
    spec <- ppa_spec_reactive()
    mappings <- ppa_mappings()
    req(prepared, spec)
    opts <- ppa_plot_opts()
    ylab <- mappings$response %||% "Value"
    ppa_plot_box_by_stream(
      prepared,
      mappings$stream_factors %||% character(0),
      spec,
      ppa_colors(),
      ylab = ylab,
      base_size = opts$base_size,
      show_spec_limits = opts$show_spec_limits
    )
  }), width = ppa_plot_w, height = reactive(320 * 4))
  downloadServer("ppa_run_chart", reactive({
    prepared <- ppa_prepared()
    spec <- ppa_spec_reactive()
    mappings <- ppa_mappings()
    req(prepared, spec)
    opts <- ppa_plot_opts()
    ctx <- ppa_run_chart_context()
    subset <- ctx$subset
    ylab <- mappings$response %||% "Value"
    selection <- input$ppa_run_chart_stream %||% "all"
    title <- if (identical(selection, "all")) {
      "Run Chart (All data)"
    } else {
      sprintf("Run Chart (%s)", selection)
    }
    ppa_plot_run_chart(
      subset,
      spec,
      ppa_colors(),
      title = title,
      ylab = ylab,
      control_chart = ctx$control_chart,
      base_size = opts$base_size,
      show_spec_limits = opts$show_spec_limits
    )
  }), width = ppa_plot_w, height = ppa_plot_h)
  downloadServer("ppa_dispersion_chart", reactive({
    if (isTRUE(input$ppa_run_chart)) {
      return(NULL)
    }
    ctx <- ppa_run_chart_context()
    req(ctx$control_chart)
    ppa_plot_dispersion_chart(
      ctx$control_chart,
      ppa_colors(),
      base_size = ppa_plot_opts()$base_size
    )
  }), width = ppa_plot_w, height = reactive(260 * 4))
  downloadServer("ppa_variance_bar", reactive({
    result <- ppa_result()
    req(result)
    opts <- ppa_plot_opts()
    bar_data <- ppa_variance_bar_data(result$variance, result$nested_variance, result$measurement_error)
    ppa_plot_variance_bars(bar_data, colors = ppa_colors(), base_size = opts$base_size)
  }), width = ppa_plot_w, height = ppa_plot_h)

  invisible(NULL)
}
