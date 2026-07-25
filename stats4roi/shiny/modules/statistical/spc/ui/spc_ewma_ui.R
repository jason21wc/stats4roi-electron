# EWMA tab UI — Variables-style sidebar + plot / summary main panel.

library(shiny)
library(shinyWidgets)
library(DT)

create_spc_ewma_ui_internal <- function(ns) {
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Set Up Chart"),
      tags$div(
        materialSwitch(
          inputId = ns("spc_ewma_ind_or_mean"),
          label = "Mean",
          value = FALSE,
          inline = TRUE
        ),
        tags$span("Individuals")
      ),
      conditionalPanel(
        condition = paste0("input['", ns("spc_ewma_ind_or_mean"), "']==0"),
        radioButtons(
          inputId = ns("spc_ewma_data_type"),
          label = "Subgroups are:",
          choices = c(
            "A row across multiple columns" = 1,
            "Defined by a column" = 2
          )
        )
      ),
      uiOutput(ns("spc_ewma_UI1")),
      uiOutput(ns("spc_ewma_UI2")),
      uiOutput(ns("spc_ewma_set")),
      hr(),
      h4("2. EWMA Parameters"),
      dropdown(
        tags$h4("Parameters"),
        numericInput(ns("spc_ewma_target"), "Target", value = NA, width = "120px"),
        numericInput(
          ns("spc_ewma_alpha"),
          "\u03b1 (weight on current point)",
          value = 0.2,
          min = 0.01,
          max = 0.99,
          step = 0.05,
          width = "160px"
        ),
        selectInput(
          ns("spc_ewma_L"),
          "Standard Errors (L)",
          choices = c(2, 3),
          selected = 3,
          width = "100px"
        ),
        numericInput(
          ns("spc_ewma_seed_n"),
          "Seed = mean of first N points",
          value = 5,
          min = 1,
          step = 1,
          width = "180px"
        ),
        uiOutput(ns("spc_ewma_sigma_method_ui")),
        conditionalPanel(
          condition = paste0("input['", ns("spc_ewma_ind_or_mean"), "']==1"),
          numericInput(
            ns("spc_ewma_mr_span"),
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
        width = "300px",
        tooltip = tooltipOptions(title = "Click to set EWMA parameters")
      ),
      helpText("Never use an EWMA chart without also reviewing a control chart. Judge stability by points outside limits only."),
      hr(),
      h4("3. Display"),
      numericInput(
        ns("spc_ewma_font_size"),
        "Base Font Size",
        value = 11,
        min = 1,
        step = 1,
        width = "75px"
      ),
      numericInput(
        ns("spc_ewma_decimals"),
        "Decimals",
        value = 4,
        min = 0,
        step = 1,
        width = "75px"
      )
    ),
    mainPanel(
      width = 9,
      h3("EWMA Chart"),
      div(
        style = "position:relative",
        plotOutput(
          ns("ewma_chart_out"),
          height = "360px",
          hover = hoverOpts(ns("ewma_plot_hover"), delay = 100, delayType = "debounce")
        ),
        uiOutput(ns("ewma_hover_info"), style = "pointer-events: none")
      ),
      HTML("<p style='text-align:right;'><small>Mouse over for point information</small></p>"),
      fluidRow(
        column(3, downloadButtonUI(ns("ewma_chart_out"))),
        column(3, tags$div(class = "inline", downloadSelectUI(ns("ewma_chart_out"))))
      ),
      br(),
      fluidRow(
        column(
          width = 2,
          dropdown(
            tags$h4("Chart Options"),
            checkboxInput(
              ns("spc_ewma_show_companion"),
              "Show companion control chart",
              value = TRUE
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
            inputId = ns("spc_ewma_show_summary"),
            label = "Parameter Summary",
            value = TRUE,
            status = "success",
            fill = TRUE
          )
        ),
        column(
          width = 3,
          prettySwitch(
            inputId = ns("spc_ewma_show_data"),
            label = "Show plot data",
            value = FALSE,
            status = "success",
            fill = TRUE
          )
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("spc_ewma_show_summary"), "']==1"),
        uiOutput(ns("ewma_summary_out"))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("spc_ewma_show_companion"), "']==1"),
        hr(style = "border-top: 1px solid #000000;"),
        plotOutput(ns("ewma_companion_out"), height = "280px"),
        fluidRow(
          column(3, downloadButtonUI(ns("ewma_companion_out"))),
          column(3, tags$div(class = "inline", downloadSelectUI(ns("ewma_companion_out"))))
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("spc_ewma_show_data"), "']==1"),
        hr(style = "border-top: 1px solid #000000;"),
        h4("Plot Data"),
        DT::dataTableOutput(ns("ewma_table_out"))
      )
    )
  )
}
