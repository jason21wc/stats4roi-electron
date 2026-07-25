# CUSUM tab UI — Variables-style sidebar + plot / summary main panel.

library(shiny)
library(shinyWidgets)
library(DT)

create_spc_cusum_ui_internal <- function(ns) {
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Set Up Chart"),
      tags$div(
        materialSwitch(
          inputId = ns("spc_cusum_ind_or_mean"),
          label = "Mean",
          value = FALSE,
          inline = TRUE
        ),
        tags$span("Individuals")
      ),
      conditionalPanel(
        condition = paste0("input['", ns("spc_cusum_ind_or_mean"), "']==0"),
        radioButtons(
          inputId = ns("spc_cusum_data_type"),
          label = "Subgroups are:",
          choices = c(
            "A row across multiple columns" = 1,
            "Defined by a column" = 2
          )
        )
      ),
      uiOutput(ns("spc_cusum_UI1")),
      uiOutput(ns("spc_cusum_UI2")),
      uiOutput(ns("spc_cusum_set")),
      hr(),
      h4("2. CUSUM Parameters"),
      dropdown(
        tags$h4("Parameters"),
        numericInput(ns("spc_cusum_target"), "Target", value = NA, width = "120px"),
        numericInput(
          ns("spc_cusum_k"),
          "k (usually 0.5 to 1)",
          value = 1,
          min = 0.05,
          step = 0.05,
          width = "120px"
        ),
        numericInput(
          ns("spc_cusum_alpha"),
          "\u03b1 (alpha)",
          value = 0.005,
          min = 1e-6,
          max = 0.999,
          step = 0.001,
          width = "120px"
        ),
        numericInput(
          ns("spc_cusum_beta"),
          "\u03b2 (beta)",
          value = 0,
          min = 0,
          max = 0.999,
          step = 0.001,
          width = "120px"
        ),
        uiOutput(ns("spc_cusum_sigma_method_ui")),
        conditionalPanel(
          condition = paste0("input['", ns("spc_cusum_ind_or_mean"), "']==1"),
          numericInput(
            ns("spc_cusum_mr_span"),
            "Span of MR",
            value = 2,
            min = 2,
            step = 1,
            width = "100px"
          )
        ),
        circle = TRUE,
        status = "danger",
        icon = icon("fas fa-chart-line"),
        width = "280px",
        tooltip = tooltipOptions(title = "Click to set CUSUM parameters")
      ),
      helpText("Never use a CUSUM chart without also reviewing a control chart."),
      hr(),
      h4("3. Display"),
      numericInput(
        ns("spc_cusum_font_size"),
        "Base Font Size",
        value = 11,
        min = 1,
        step = 1,
        width = "75px"
      ),
      numericInput(
        ns("spc_cusum_decimals"),
        "Decimals",
        value = 4,
        min = 0,
        step = 1,
        width = "75px"
      )
    ),
    mainPanel(
      width = 9,
      h3("CUSUM Chart"),
      div(
        style = "position:relative",
        plotOutput(
          ns("cusum_chart_out"),
          height = "360px",
          hover = hoverOpts(ns("cusum_plot_hover"), delay = 100, delayType = "debounce")
        ),
        uiOutput(ns("cusum_hover_info"), style = "pointer-events: none")
      ),
      HTML("<p style='text-align:right;'><small>Mouse over for point information</small></p>"),
      fluidRow(
        column(3, downloadButtonUI(ns("cusum_chart_out"))),
        column(3, tags$div(class = "inline", downloadSelectUI(ns("cusum_chart_out"))))
      ),
      br(),
      fluidRow(
        column(
          width = 2,
          dropdown(
            tags$h4("Chart Options"),
            radioButtons(
              inputId = ns("spc_cusum_highlight"),
              label = "Highlight points using",
              choices = c(
                "Si and Ti" = "si_ti",
                "Si* and Ti*" = "si_ti_star"
              ),
              selected = "si_ti"
            ),
            checkboxInput(
              ns("spc_cusum_show_companion"),
              "Show companion control chart",
              value = TRUE
            ),
            checkboxInput(
              ns("spc_cusum_show_H"),
              "Show \u00b1H reference lines on CUSUM",
              value = FALSE
            ),
            circle = TRUE,
            status = "success",
            icon = icon("gear"),
            width = "280px",
            tooltip = tooltipOptions(title = "Click to modify chart")
          )
        ),
        column(
          width = 3,
          prettySwitch(
            inputId = ns("spc_cusum_show_summary"),
            label = "Parameter Summary",
            value = TRUE,
            status = "success",
            fill = TRUE
          )
        ),
        column(
          width = 3,
          prettySwitch(
            inputId = ns("spc_cusum_show_data"),
            label = "Show plot data",
            value = FALSE,
            status = "success",
            fill = TRUE
          )
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("spc_cusum_show_summary"), "']==1"),
        uiOutput(ns("cusum_summary_out"))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("spc_cusum_show_companion"), "']==1"),
        hr(style = "border-top: 1px solid #000000;"),
        plotOutput(ns("cusum_companion_out"), height = "280px"),
        fluidRow(
          column(3, downloadButtonUI(ns("cusum_companion_out"))),
          column(3, tags$div(class = "inline", downloadSelectUI(ns("cusum_companion_out"))))
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("spc_cusum_show_data"), "']==1"),
        hr(style = "border-top: 1px solid #000000;"),
        h4("Plot Data"),
        DT::dataTableOutput(ns("cusum_table_out"))
      )
    )
  )
}
