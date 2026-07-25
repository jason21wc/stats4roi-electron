# Autocorrelation module UI (setup + results)

create_autocorrelation_ui_internal <- function(ns) {
  tagList(
    h3("Autocorrelation Analysis"),
    br(),
    # Persistent status line — always bound (debugging aid + user feedback)
    tags$div(
      style = "padding: 8px 12px; margin-bottom: 12px; background: #f5f5f5; border: 1px solid #ddd;",
      strong("Status: "),
      textOutput(ns("acf_status_text"), inline = TRUE)
    ),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = ns("acf_measure"),
          label = "Measure",
          choices = character(0),
          selected = NULL
        ),
        numericInput(
          inputId = ns("acf_conf"),
          label = "Confidence",
          value = 0.95,
          min = 0.5,
          max = 0.999,
          step = 0.01,
          width = "100px"
        ),
        numericInput(
          inputId = ns("acf_lag_max"),
          label = "Max lag (empty = default)",
          value = NA,
          min = 1,
          step = 1,
          width = "160px"
        ),
        prettySwitch(
          inputId = ns("acf_omit_lag0"),
          label = "Omit lag 0 on ACF plot",
          value = TRUE,
          status = "success",
          fill = TRUE
        ),
        numericInput(
          inputId = ns("acf_decimals"),
          label = "Decimals",
          value = 4,
          min = 0,
          max = 9,
          step = 1,
          width = "75px"
        ),
        actionButton(
          inputId = ns("acf_analyze"),
          label = "Analyze",
          class = "btn-primary"
        ),
        br(), br(),
        helpText(
          "Sequence uses row order of the working data after removing missing values in the selected measure."
        )
      ),
      mainPanel(
        plotOutput(ns("acf_run_sequence"), height = "280px"),
        fluidRow(
          column(3, downloadButtonUI(ns("dl_acf_run_sequence"))),
          column(3, tags$div(class = "inline", downloadSelectUI(ns("dl_acf_run_sequence"))))
        ),
        tags$hr(style = "border-color: black;"),
        plotOutput(ns("acf_plot"), height = "280px"),
        fluidRow(
          column(3, downloadButtonUI(ns("dl_acf_plot"))),
          column(3, tags$div(class = "inline", downloadSelectUI(ns("dl_acf_plot"))))
        ),
        tags$hr(style = "border-color: black;"),
        plotOutput(ns("pacf_plot"), height = "280px"),
        fluidRow(
          column(3, downloadButtonUI(ns("dl_pacf_plot"))),
          column(3, tags$div(class = "inline", downloadSelectUI(ns("dl_pacf_plot"))))
        ),
        tags$hr(style = "border-color: black;"),
        h4("Significant lags"),
        uiOutput(ns("acf_sig_summary"))
      )
    )
  )
}
