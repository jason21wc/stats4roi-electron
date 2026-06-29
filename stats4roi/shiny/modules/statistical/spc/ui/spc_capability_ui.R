# SPC Capability Calculations UI
# UI components for the Capability Calculations tab within the SPC module.
#
# NOTE: Business logic is implemented in coordinator/workers; this file is UI only.

library(shiny)

create_spc_capability_ui_internal <- function(ns) {
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = ns("cap_dist"),
        label = "Distribution",
        choices = choice_cap_distribution,
        selected = 1
      ),
      radioButtons(
        inputId = ns("cap_study"),
        label = "Study",
        choices = choice_cap_study,
        selected = 2
      ),
      numericInput(
        inputId = ns("cap_decimals"),
        label = "Decimals",
        value = 4,
        min = 0,
        max = 9,
        step = 1,
        width = "75px"
      )
    ),
    mainPanel(
      h3("Capability Calculations"),
      br(),
      h4("Enter Statistics"),
      fluidRow(
        column(
          width = 6,
          tags$div(
            id = "inline1",
            class = "inline",
            numericInput(
              inputId = ns("cap_usl"),
              label = "USL:",
              value = NA,
              width = "75px"
            ),
            numericInput(
              inputId = ns("cap_target"),
              label = "Target:",
              value = NA,
              width = "75px"
            ),
            numericInput(
              inputId = ns("cap_lsl"),
              label = "LSL:",
              value = NA,
              width = "75px"
            )
          )
        ),
        column(
          width = 6,
          tags$div(
            id = "inline1",
            class = "inline",
            numericInput(
              inputId = ns("cap_mean"),
              label = withMathJax("$\\bar{X}$:"),
              value = 0,
              width = "75px"
            ),
            numericInput(
              inputId = ns("cap_sd"),
              label = withMathJax("$s$:"),
              value = 1,
              min = 0,
              width = "75px"
            )
          )
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("cap_dist"), "']==2"),
        fluidRow(
          column(
            width = 12,
            tags$div(
              id = "inline1",
              class = "inline",
              numericInput(
                inputId = ns("cap_upl"),
                label = "Upper Process Limit:",
                value = NA,
                width = "75px"
              ),
              numericInput(
                inputId = ns("cap_lpl"),
                label = "Lower Process Limit:",
                value = NA,
                width = "75px"
              ),
              numericInput(
                inputId = ns("cap_pct_usl"),
                label = "Est % Out USL:",
                value = 0,
                min = 0,
                max = 100,
                width = "75px"
              ),
              numericInput(
                inputId = ns("cap_pct_lsl"),
                label = "Est % Out LSL:",
                value = 0,
                min = 0,
                max = 100,
                width = "75px"
              )
            )
          )
        )
      ),
      br(),
      h4("Results"),
      htmlOutput(ns("capability_calc_out"))
    )
  )
}
