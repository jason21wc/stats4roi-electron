# Distribution Fitting tab UI

library(shiny)
library(shinyWidgets)

source("modules/statistical/spc/utils/dfit_constants.R")

create_spc_dfit_ui_internal <- function(ns) {
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Data"),
      uiOutput(ns("dfit_column")),
      hr(),
      h4("2. Specifications"),
      tags$div(
        id = "inline1",
        class = "inline",
        numericInput(ns("dfit_lsl"), "LSL:", value = NA, width = "75px"),
        numericInput(ns("dfit_target"), "Target:", value = NA, width = "75px"),
        numericInput(ns("dfit_usl"), "USL:", value = NA, width = "75px")
      ),
      helpText("Enter at least one spec limit for conformance and capability."),
      checkboxInput(
        ns("dfit_show_specs"),
        "Show spec limits on chart",
        value = FALSE
      ),
      hr(),
      h4("3. Potential variability"),
      verbatimTextOutput(ns("dfit_sd_pot_display")),
      helpText("Median moving range estimator (span 2)."),
      hr(),
      h4("4. Distribution"),
      selectInput(
        inputId = ns("dfit_distribution"),
        label = "Distribution",
        choices = choice_dfit_distribution,
        selected = 0L
      ),
      conditionalPanel(
        condition = paste0("input['", ns("dfit_distribution"), "'] != '0'"),
        checkboxInput(
          ns("dfit_show_nt_limits"),
          "Show natural tolerance limits on chart",
          value = FALSE
        ),
        helpText("LPL, UPL, and distribution mean (long-dash / dotted).")
      ),
      numericInput(
        ns("dfit_decimals"),
        "Decimals",
        value = 4,
        min = 0,
        max = 9,
        step = 1,
        width = "75px"
      ),
      conditionalPanel(
        condition = paste0("input['", ns("dfit_distribution"), "'] == '1'"),
        checkboxGroupInput(
          ns("dfit_normality_tests"),
          "Normality tests",
          choices = c(
            "Anderson-Darling" = "ad",
            "Shapiro-Wilk" = "sw",
            "Lin-Mudholkar" = "lm",
            "Skewness (D'Agostino)" = "skew",
            "Kurtosis (D'Agostino)" = "kurt"
          ),
          selected = c("ad", "sw", "lm", "skew", "kurt")
        )
      ),
      uiOutput(ns("dfit_exp_tests_ui"))
    ),
    mainPanel(
      width = 9,
      h3("Distribution Fitting"),
      uiOutput(ns("dfit_readiness")),
      tabsetPanel(
        id = ns("dfit_plot_tab"),
        type = "tabs",
        tabPanel("Histogram", value = "histogram"),
        tabPanel("Density + rug", value = "density"),
        tabPanel("Q-Q", value = "qq"),
        tabPanel("P-P", value = "pp"),
        tabPanel("Probability", value = "probability")
      ),
      br(),
      fluidRow(
        column(3, downloadButtonUI(ns("dfit_plot"))),
        column(3, downloadSelectUI(ns("dfit_plot"))),
        column(
          2,
          conditionalPanel(
            condition = paste0("input['", ns("dfit_plot_tab"), "'] == 'histogram'"),
            dropdown(
              tags$h4("Histogram options"),
              fluidRow(
                column(
                  5,
                  numericInput(
                    ns("dfit_bin_width"),
                    "Bin Width",
                    value = NULL,
                    min = 0,
                    step = 0.01,
                    width = "100%"
                  )
                ),
                column(
                  2,
                  br(),
                  p(style = "text-align: center; font-weight: bold;", "OR")
                ),
                column(
                  5,
                  numericInput(
                    ns("dfit_bins"),
                    "# of Bins",
                    value = NULL,
                    min = 1,
                    step = 1,
                    width = "100%"
                  )
                )
              ),
              conditionalPanel(
                condition = paste0(
                  "input['", ns("dfit_bin_width"), "'] > 0 && input['",
                  ns("dfit_bins"), "'] > 0"
                ),
                p(
                  style = "text-align: center; color: red;",
                  "Bin width overrides selecting the number of bins."
                )
              ),
              numericInput(
                ns("dfit_bin_center"),
                "Center of a Bin",
                value = NULL,
                width = "100%"
              ),
              circle = TRUE,
              status = "success",
              icon = icon("gear"),
              width = "280px",
              tooltip = tooltipOptions(title = "Histogram bin settings")
            )
          )
        ),
        column(
          2,
          conditionalPanel(
            condition = paste0("input['", ns("dfit_distribution"), "'] != '0'"),
            dropdown(
              tags$h4("Customize Parameters"),
              uiOutput(ns("dfit_param_overrides")),
              circle = TRUE,
              status = "success",
              icon = icon("sliders"),
              width = "280px",
              right = TRUE,
              tooltip = tooltipOptions(title = "Customize fitted parameters")
            )
          )
        ),
        column(
          2,
          conditionalPanel(
            condition = paste0("input['", ns("dfit_distribution"), "'] != '0'"),
            dropdown(
              tags$h4("Distribution calculator"),
              radioButtons(
                ns("dfit_calc_mode"),
                "Calculate",
                choices = c(
                  "Proportion from value" = "proportion",
                  "Value from proportion" = "quantile"
                )
              ),
              conditionalPanel(
                condition = paste0("input['", ns("dfit_calc_mode"), "'] == 'proportion'"),
                numericInput(ns("dfit_calc_value"), "Value", value = NA, width = "100%")
              ),
              conditionalPanel(
                condition = paste0("input['", ns("dfit_calc_mode"), "'] == 'quantile'"),
                numericInput(ns("dfit_calc_prop"), "Proportion (%)", value = NA, min = 0, max = 100, width = "100%"),
                radioButtons(
                  ns("dfit_calc_tail"),
                  "Tail",
                  choices = c("Lower" = "lower", "Upper" = "upper")
                )
              ),
              tags$div(style = "max-width: 260px; word-wrap: break-word;", htmlOutput(ns("dfit_calc_out"))),
              circle = TRUE,
              status = "success",
              icon = icon("calculator"),
              width = "280px",
              right = TRUE,
              tooltip = tooltipOptions(title = "Distribution calculator")
            )
          )
        )
      ),
      plotOutput(ns("dfit_plot"), height = "420px"),
      hr(),
      conditionalPanel(
        condition = paste0("input['", ns("dfit_distribution"), "'] != '0'"),
        actionButton(
          ns("dfit_send_nt_spc"),
          "Send LPL/UPL to SPC Individuals custom limits",
          class = "btn-sm btn-primary"
        ),
        helpText(
          "Sets the Variables chart custom location limits (LCL = LPL, UCL = UPL, ",
          "centerline = mean) and selects Custom limit calculation."
        )
      ),
      htmlOutput(ns("dfit_summary_table"))
    )
  )
}
