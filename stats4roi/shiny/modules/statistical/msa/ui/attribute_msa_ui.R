# Discrete (Attribute) MSA UI

library(shiny)
library(shinyWidgets)

create_attribute_msa_ui_internal <- function(ns) {
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = ns("msa_d_data_type"),
        label = "How is the data configured?",
        choices = c("Columns are Appraisers" = 1)
      ),
      radioButtons(
        inputId = ns("msa_d_level"),
        label = "Data Level",
        choices = c("Nominal or Ordinal" = 1)
      ),
      uiOutput(ns("msa_d_1")),
      checkboxInput(inputId = ns("msa_d_internal"), label = "Repeated Measures?", value = FALSE),
      uiOutput(ns("d_repeat_sel")),
      checkboxInput(inputId = ns("msa_d_standard"), label = "Set Standard?"),
      conditionalPanel(
        condition = paste0("input['", ns("msa_d_standard"), "'] == 1"),
        uiOutput(ns("msa_d_standard_ID"))
      ),
      radioButtons(
        inputId = ns("msa_d_type"),
        label = "Assessor Selection",
        choices = c("Fixed (Light's)" = 1)
      ),
      actionButton(ns("msa_d_go"), "Perform Analysis", icon = icon("check"), style = "color: white; background-color:green;")
    ),
    mainPanel(
      h3("Measurement System Analysis - Discrete Data"),
      fluidRow(
        column(7),
        column(3, numericInput(inputId = ns("conf_msa_d"), label = "Confidence", value = 0.95, min = 0, max = 1, step = 0.05, width = "75px")),
        column(2, numericInput(inputId = ns("deci_msa_d"), label = "Decimals", value = 4, min = 0, max = 9, step = 1, width = "75px"))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("msa_d_xtab"), "'] == 1"),
        uiOutput(ns("msa_xtab"))
      ),
      htmlOutput(ns("msa_out_d"))
    )
  )
}
