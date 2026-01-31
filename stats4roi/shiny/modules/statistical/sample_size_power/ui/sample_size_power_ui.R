# =============================================================================
# SAMPLE SIZE AND POWER UI
# =============================================================================
# UI layout for Sample Size/Power tab. Dynamic parameter inputs (s_sizeUI1-4)
# are rendered on demand via renderUI in the coordinator (on-demand architecture).

create_sample_size_power_ui_internal <- function(ns) {
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = ns("sample_size_type"),
        label = "Calculate the sample size or power for:",
        choices = c("Means" = 1, "Standard Deviations" = 2, "Proportions (binomial)" = 3, "Rates (Poisson)" = 4, "ANOVA" = 5, "Correlations" = 6)
      ),
      radioButtons(
        inputId = ns("sample_size_mode"),
        label = "Calculate for",
        choices = c("Hypothesis Test" = 1, "Estimation" = 2),
        selected = 1
      ),
      conditionalPanel(
        condition = "input.sample_size_mode == 1",
        ns = ns,
        uiOutput(ns("s_size_tests"))
      ),
      conditionalPanel(
        condition = "input.sample_size_mode == 2",
        ns = ns,
        checkboxInput(
          inputId = ns("s_size_more_info"),
          label = "More information about selected calculation",
          value = FALSE
        )
      )
    ),
    mainPanel(
      fluidRow(
        conditionalPanel(
          condition = "!(input.sample_size_type == 5 || input.sample_size_mode == 2)",
          ns = ns,
          selectInput(
            inputId = ns("one_or_two_size"),
            label = "Alternative is:",
            choices = c("Equal to the null" = "two.sided", "Less Than the null" = "less", "Greater Than the null" = "greater"),
            width = "150px",
            selected = 2
          )
        ),
        conditionalPanel(
          condition = "input.sample_size_mode == 1",
          ns = ns,
          checkboxInput(
            inputId = ns("power_s"),
            label = "Power",
            value = FALSE
          )
        )
      ),
      tags$div(
        id = "inline1",
        class = "inline",
        fluidRow(
          column(6, numericInput(
            inputId = ns("s_size_alpha"),
            label = withMathJax("$$\\alpha:{ }$$"),
            value = 0.05,
            min = 0,
            max = 1,
            step = 0.05,
            width = "150px"
          )),
          column(6,
            conditionalPanel(
              condition = "input.sample_size_mode == 2",
              ns = ns,
              numericInput(
                inputId = ns("s_size_sigfig"),
                label = "CI Width SigFigs",
                value = 5,
                min = 1,
                max = 9,
                step = 1,
                width = "150px"
              )
            )
          )
        ),
        fluidRow(
          column(6,
            conditionalPanel(
              condition = "input.sample_size_mode == 1",
              ns = ns,
              conditionalPanel(
                condition = "input.power_s == 0",
                ns = ns,
                numericInput(
                  inputId = ns("s_size_beta"),
                  label = withMathJax("$$\\beta:{ }$$"),
                  value = 0.1,
                  min = 0,
                  max = 1,
                  step = 0.05,
                  width = "150px"
                )
              ),
              conditionalPanel(
                condition = "input.power_s == 1",
                ns = ns,
                numericInput(
                  inputId = ns("s_size_n"),
                  label = withMathJax("$$n:{ }$$"),
                  value = 10,
                  min = 0,
                  step = 1,
                  width = "150px"
                )
              )
            )
          ),
          column(3, uiOutput(ns("s_sizeUI3")))
        ),
        fluidRow(
          column(6, uiOutput(ns("s_sizeUI1"))),
          column(6, uiOutput(ns("s_sizeUI4")))
        ),
        fluidRow(
          column(6, uiOutput(ns("s_sizeUI2")))
        )
      ),
      htmlOutput(ns("pretty_ssize"))
    )
  )
}
