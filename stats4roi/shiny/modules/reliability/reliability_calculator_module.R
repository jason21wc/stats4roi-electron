# Interactive series-of-parallels reliability calculator for stats4ROI
# Excel formula parity with docs/reliability/ReliabilityCalculator2.xls

library(shiny)
library(rhandsontable)
library(lolcat)

source("modules/config/global_config.R")
source("modules/reliability/utils/reliability_calc.R")

create_reliability_calculator_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Reliability Calculator"),
    br(),
    sidebarLayout(
      sidebarPanel(
        helpText(
          "Each row is a serial stage. Columns P1, P2, … are parallel components ",
          "within that stage. Leave cells blank for unused slots."
        ),
        tags$label("Stages (serial)"),
        fluidRow(
          column(6, actionButton(ns("add_stage"), "Add", width = "100%")),
          column(6, actionButton(ns("remove_stage"), "Remove", width = "100%"))
        ),
        br(),
        tags$label("Parallel components"),
        fluidRow(
          column(6, actionButton(ns("add_parallel"), "Add", width = "100%")),
          column(6, actionButton(ns("remove_parallel"), "Remove", width = "100%"))
        ),
        br(),
        actionButton(ns("reset_grid"), "Reset grid", width = "100%"),
        br(), br(),
        textOutput(ns("status_msg"))
      ),
      mainPanel(
        rHandsontableOutput(ns("rel_table"), height = "420px"),
        tags$hr(style = "border-color: black;"),
        h4("Results"),
        htmlOutput(ns("system_results")),
        br(),
        helpText(
          "Row reliability = 1 − ∏(1 − Rⱼ) for filled parallel slots. ",
          "System reliability = product of all row reliabilities. ",
          "Enter component reliabilities between 0 and 1."
        )
      )
    )
  )
}

.create_reliability_hex <- function(col) {
  if (is.null(col) || length(col) < 1L || is.na(col[1])) {
    return("#E8F4FC")
  }
  rgb <- grDevices::col2rgb(col[1])
  sprintf("#%02X%02X%02X", rgb[1], rgb[2], rgb[3])
}

.create_reliability_renderer <- function(n_parallels, input_bg, result_bg) {
  sprintf(
    paste0(
      "function(instance, td, row, col, prop, value, cellProperties) {",
      "  Handsontable.renderers.NumericRenderer.apply(this, arguments);",
      "  if (value === null || value === '' || typeof value === 'undefined') {",
      "    td.innerHTML = '';",
      "  }",
      "  var nPar = %d;",
      "  if (col >= nPar) {",
      "    td.style.background = '%s';",
      "    td.style.fontWeight = '600';",
      "  } else {",
      "    td.style.background = '%s';",
      "  }",
      "  td.style.textAlign = 'right';",
      "}"
    ),
    as.integer(n_parallels),
    result_bg,
    input_bg
  )
}

create_reliability_calculator_server <- function(id, color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    colors <- reactive({
      get_distribution_colors(color_palette())
    })

    grid_state <- reactiveVal(empty_reliability_grid(
      RELIABILITY_DEFAULT_STAGES,
      RELIABILITY_DEFAULT_PARALLELS
    ))
    status_text <- reactiveVal("")
    table_tick <- reactiveVal(0L)

    .set_status <- function(msg) status_text(msg)
    .bump_table <- function() table_tick(isolate(table_tick()) + 1L)

    output$status_msg <- renderText({
      status_text()
    })

    output$system_results <- renderUI({
      grid <- grid_state()
      rows <- compute_row_reliabilities(grid)
      sys_r <- system_reliability(rows)
      HTML(paste0(
        "<table>",
        "<tr><td><b>System reliability</b></td><td>&nbsp;&nbsp;</td><td>",
        sprintf("%.6f", sys_r), "</td></tr>",
        "<tr><td>Stages × parallel slots</td><td>&nbsp;&nbsp;</td><td>",
        nrow(grid), " × ", ncol(grid), "</td></tr>",
        "</table>"
      ))
    })

    output$rel_table <- rhandsontable::renderRHandsontable({
      table_tick()
      grid <- grid_state()
      disp <- reliability_display_table(grid)
      for (j in seq_len(ncol(disp) - 1L)) {
        disp[[j]] <- ifelse(is.na(disp[[j]]), NA_real_, disp[[j]])
      }
      n_par <- ncol(grid)
      input_bg <- .create_reliability_hex(colors()$col_fill)
      result_bg <- .create_reliability_hex(colors()$col_fill_highlight)
      input_rgb <- grDevices::col2rgb(input_bg)
      input_bg_light <- sprintf(
        "#%02X%02X%02X",
        min(255L, as.integer(input_rgb[1] * 0.25 + 255 * 0.75)),
        min(255L, as.integer(input_rgb[2] * 0.25 + 255 * 0.75)),
        min(255L, as.integer(input_rgb[3] * 0.25 + 255 * 0.75))
      )
      result_rgb <- grDevices::col2rgb(result_bg)
      result_bg_light <- sprintf(
        "#%02X%02X%02X",
        min(255L, as.integer(result_rgb[1] * 0.35 + 255 * 0.65)),
        min(255L, as.integer(result_rgb[2] * 0.35 + 255 * 0.65)),
        min(255L, as.integer(result_rgb[3] * 0.35 + 255 * 0.65))
      )

      renderer <- .create_reliability_renderer(n_par, input_bg_light, result_bg_light)

      hot <- rhandsontable::rhandsontable(
        disp,
        rowHeaderWidth = 80,
        stretchH = "all",
        height = 400
      ) %>%
        rhandsontable::hot_table(
          rowHeaders = TRUE,
          colHeaders = TRUE,
          highlightCol = TRUE,
          highlightRow = TRUE
        ) %>%
        rhandsontable::hot_cols(format = "0.0000", renderer = renderer) %>%
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)

      for (j in seq_len(n_par)) {
        hot <- hot %>% rhandsontable::hot_col(j, readOnly = FALSE, halign = "htRight")
      }
      hot <- hot %>%
        rhandsontable::hot_col(
          n_par + 1L,
          readOnly = TRUE,
          halign = "htRight",
          format = "0.000000"
        )
      hot$x$rowHeaders <- rownames(disp)
      hot$x$colHeaders <- colnames(disp)
      hot
    })

    observeEvent(input$rel_table, {
      df <- tryCatch(
        rhandsontable::hot_to_r(input$rel_table),
        error = function(e) NULL
      )
      if (is.null(df)) return()
      if ("Row reliability" %in% names(df)) {
        df <- df[, names(df) != "Row reliability", drop = FALSE]
      }
      current <- grid_state()
      if (nrow(df) != nrow(current) || ncol(df) != ncol(current)) return()

      new_grid <- matrix(NA_real_, nrow = nrow(df), ncol = ncol(df))
      for (i in seq_len(nrow(df))) {
        for (j in seq_len(ncol(df))) {
          check <- validate_reliability_cell(df[i, j])
          if (!isTRUE(check$ok)) {
            .set_status(sprintf("Stage %d, P%d: %s", i, j, check$message))
            .bump_table()
            return()
          }
          new_grid[i, j] <- check$value
        }
      }
      rownames(new_grid) <- paste("Stage", seq_len(nrow(new_grid)))
      colnames(new_grid) <- paste0("P", seq_len(ncol(new_grid)))
      if (!identical(new_grid, current)) {
        grid_state(new_grid)
        .set_status("")
      }
    }, ignoreInit = TRUE)

    observeEvent(input$add_stage, {
      grid <- grid_state()
      res <- resize_reliability_grid(grid, nrow(grid) + 1L, ncol(grid))
      if (!res$ok) {
        .set_status(res$message)
      } else {
        grid_state(res$grid)
        .set_status("")
        .bump_table()
      }
    })

    observeEvent(input$remove_stage, {
      grid <- grid_state()
      res <- resize_reliability_grid(grid, nrow(grid) - 1L, ncol(grid))
      if (!res$ok) {
        .set_status(res$message)
      } else {
        grid_state(res$grid)
        .set_status("")
        .bump_table()
      }
    })

    observeEvent(input$add_parallel, {
      grid <- grid_state()
      res <- resize_reliability_grid(grid, nrow(grid), ncol(grid) + 1L)
      if (!res$ok) {
        .set_status(res$message)
      } else {
        grid_state(res$grid)
        .set_status("")
        .bump_table()
      }
    })

    observeEvent(input$remove_parallel, {
      grid <- grid_state()
      res <- resize_reliability_grid(grid, nrow(grid), ncol(grid) - 1L)
      if (!res$ok) {
        .set_status(res$message)
      } else {
        grid_state(res$grid)
        .set_status("")
        .bump_table()
      }
    })

    observeEvent(input$reset_grid, {
      grid_state(empty_reliability_grid(
        RELIABILITY_DEFAULT_STAGES,
        RELIABILITY_DEFAULT_PARALLELS
      ))
      .set_status("")
      .bump_table()
    })
  })
}
