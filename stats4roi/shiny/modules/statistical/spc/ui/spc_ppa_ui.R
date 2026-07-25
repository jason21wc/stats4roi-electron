# Process Performance Analysis tab UI

ppa_plot_download_row <- function(ns, id) {
  fluidRow(
    column(3, downloadButtonUI(ns(id))),
    column(3, tags$div(class = "inline", downloadSelectUI(ns(id))))
  )
}

create_spc_ppa_ui_internal <- function(ns) {
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Response"),
      uiOutput(ns("ppa_response_ui")),
      conditionalPanel(
        condition = sprintf(
          "input['%s'] != null && input['%s'].length > 1",
          ns("ppa_response"),
          ns("ppa_response")
        ),
        tags$div(
          tags$span("Multiple Measures Represent: "),
          actionLink(
            ns("ppa_multiple_measures_help"),
            label = NULL,
            icon = icon("circle-question"),
            title = "Help"
          )
        ),
        radioButtons(
          inputId = ns("ppa_multiple_measures"),
          label = NULL,
          choices = c(
            "Sample Items" = "sample_items",
            "Repeated Measures" = "repeated_measures"
          ),
          selected = "sample_items"
        )
      ),
      hr(),
      h4("2. Process structure"),
      uiOutput(ns("ppa_sample_ui")),
      uiOutput(ns("ppa_stream_ui")),
      uiOutput(ns("ppa_stream_order_ui")),
      hr(),
      h4("3. Specifications"),
      tags$div(
        class = "inline",
        numericInput(ns("ppa_lsl"), "LSL:", value = NA, width = "100px"),
        numericInput(ns("ppa_target"), "Target:", value = NA, width = "100px"),
        numericInput(ns("ppa_usl"), "USL:", value = NA, width = "100px")
      ),
      helpText("Leave Target blank to report Ppk only (no Ppm)."),
      checkboxInput(ns("ppa_show_spec_limits"), "Show spec limits on exploratory charts", TRUE),
      hr(),
      h4("4. Chart types & limits"),
      uiOutput(ns("ppa_chart_limits_ui")),
      hr(),
      h4("5. Display"),
      numericInput(ns("ppa_decimals"), "Decimals", value = 4, min = 0, max = 9, width = "100px"),
      numericInput(ns("ppa_font_size"), "Base font size", value = 11, min = 8, max = 20, width = "100px"),
      checkboxInput(ns("ppa_run_chart"), "Run charts only (no control limits)", FALSE),
      hr(),
      tags$p(
        style = "font-size: 0.9em; color: #555;",
        "Process Performance Analysis based on ",
        tags$a(
          href = "https://roi-ally.com/index.php/en/publications/white-papers/performance-analysis-for-process-improvement",
          target = "_blank",
          rel = "noopener noreferrer",
          "Performance Analysis for Process Improvement"
        ),
        " (Petrovich, ASQ Quality Congress, May 1998)."
      )
    ),
    mainPanel(
      width = 9,
      uiOutput(ns("ppa_readiness_out")),
      tabsetPanel(
        tabPanel(
          "Exploratory",
          br(),
          htmlOutput(ns("ppa_descriptives_out")),
          uiOutput(ns("ppa_histogram_filter_ui")),
          plotOutput(ns("ppa_histogram"), height = "300px"),
          ppa_plot_download_row(ns, "ppa_histogram"),
          plotOutput(ns("ppa_boxplot"), height = "320px"),
          ppa_plot_download_row(ns, "ppa_boxplot"),
          uiOutput(ns("ppa_run_chart_filter_ui")),
          div(
            style = "position: relative;",
            plotOutput(
              ns("ppa_run_chart"),
              height = "300px",
              hover = hoverOpts(ns("ppa_run_chart_hover"), delay = 100, delayType = "debounce")
            ),
            uiOutput(ns("ppa_run_chart_hover_info"), style = "pointer-events: none;")
          ),
          ppa_plot_download_row(ns, "ppa_run_chart"),
          uiOutput(ns("ppa_dispersion_section")),
          htmlOutput(ns("ppa_run_chart_limits_out"))
        ),
        tabPanel(
          "Summary",
          br(),
          htmlOutput(ns("ppa_summary_out")),
          plotOutput(ns("ppa_stacked_bar"), height = "360px"),
          ppa_plot_download_row(ns, "ppa_stacked_bar"),
          plotOutput(ns("ppa_opportunity_bar"), height = "340px"),
          ppa_plot_download_row(ns, "ppa_opportunity_bar")
        ),
        tabPanel(
          "Variance",
          br(),
          htmlOutput(ns("ppa_variance_components_out")),
          plotOutput(ns("ppa_variance_bar"), height = "320px"),
          plotOutput(ns("ppa_nested_variance_bar"), height = "280px"),
          fluidRow(
            column(3, downloadButtonUI(ns("ppa_variance_bar"))),
            column(3, tags$div(class = "inline", downloadSelectUI(ns("ppa_variance_bar"))))
          )
        ),
        tabPanel(
          "By stream",
          br(),
          h4("Process Stream Analysis"),
          DT::DTOutput(ns("ppa_stream_table")),
          htmlOutput(ns("ppa_stream_table_footer"))
        )
      )
    )
  )
}
