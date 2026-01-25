# Normality Tests UI Component
# Refactored from original app.R lines 1283-1309

create_normality_tests_ui <- function(ns) {
  tagList(
    h3("Normality Tests"),
    sidebarLayout(
      sidebarPanel(
        numericInput(
          inputId = ns("conf_eda"),
          label = "Confidence",
          value = 0.95,
          min = 0,
          max = 1,
          step = 0.05,
          width = "75px"
        ),
        numericInput(
          inputId = ns("decimals_desc"),
          label = "Decimals",
          value = 5,
          min = 0,
          step = 1,
          width = "75px"
        ),
        prettySwitch(
          inputId = ns("auto_norm"),
          label = "Auto Select Normality Test?",
          value = TRUE,
          status = "success",
          fill = TRUE
        ),
        conditionalPanel(
          condition = "input.auto_norm == 0",
          ns = ns,
          checkboxGroupButtons(
            inputId = ns("norm_test"),
            label = "Select Tests",
            choices = c(
              "Anderson-Darling (n < 25)" = "stat.ad.test=2",
              "Shapiro-Wilk (n < 25)" = "stat.sw.test=2",
              "Skewness and Kurtosis (n > 25)" = "stat.skew.test=2, stat.kurt.test=2"
            ),
            justified = TRUE,
            direction = "vertical"
          )
        ),
        uiOutput(ns("eda_data_list"))
      ),
      mainPanel(
        fluidRow(
          textOutput(ns("norm_test_output")),
          conditionalPanel(
            condition = "input.eda_data_type==2",
            ns = ns,
            uiOutput(ns("sel_data_name"))
          ),
          DTOutput(ns("eda_desc_out"))
        )
      )
    )
  )
}
