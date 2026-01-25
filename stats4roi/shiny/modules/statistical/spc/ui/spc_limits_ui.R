# SPC Limits Calculations UI
# UI components for the Limits Calculations tab within the SPC module.
#
# NOTE: Business logic is implemented in coordinator/workers; this file is UI only.

library(shiny)

create_spc_limits_ui_internal <- function(ns) {
  tabsetPanel(
    tabPanel(
      title = withMathJax("$\\bar{X}$"),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = ns("x_bar_lim_stat"),
            label = "Select Method",
            choices = choice_x_bar_limits
          )
        ),
        mainPanel(
          h3("Control Limit Calculations"),
          br(),
          h4("Enter Statistics"),
          fluidRow(
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("x_bar_lim_x_bar"),
                  label = withMathJax("$\\overline{\\overline{X}}:$"),
                  value = 0,
                  width = "75px"
                ),
                uiOutput(ns("x_bar_lim_value")),
                numericInput(
                  inputId = ns("x_bar_lim_n"),
                  label = withMathJax("$n:$"),
                  value = 5,
                  min = 1,
                  step = 1,
                  width = "75px"
                )
              )
            ),
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("x_bar_lim_sterr"),
                  label = "Standard Errors",
                  value = 3,
                  min = 1,
                  width = "75px"
                ),
                br(),
                numericInput(
                  inputId = ns("x_bar_lim_decimals"),
                  label = "Decimals",
                  value = 4,
                  min = 0,
                  max = 9,
                  step = 1,
                  width = "75px"
                )
              )
            )
          ),
          br(),
          h4("Results"),
          htmlOutput(ns("x_bar_lim_out"))
        )
      )
    ),
    tabPanel(
      title = withMathJax("$X$"),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = ns("x_lim_stat"),
            label = "Select Method",
            choices = choice_x_limits
          )
        ),
        mainPanel(
          h3("Control Limit Calculations"),
          br(),
          h4("Enter Statistics"),
          fluidRow(
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("x_lim_x_bar"),
                  label = withMathJax("$\\bar{X}:$"),
                  value = 0,
                  width = "75px"
                ),
                uiOutput(ns("x_lim_value")),
                uiOutput(ns("x_lim_n_UI"))
              )
            ),
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("x_lim_sterr"),
                  label = "Standard Errors",
                  value = 3,
                  min = 1,
                  width = "75px"
                ),
                br(),
                numericInput(
                  inputId = ns("x_lim_decimals"),
                  label = "Decimals",
                  value = 4,
                  min = 0,
                  max = 9,
                  step = 1,
                  width = "75px"
                )
              )
            )
          ),
          br(),
          h4("Results"),
          htmlOutput(ns("x_lim_out"))
        )
      )
    ),
    tabPanel(
      title = withMathJax("$R$"),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = ns("r_lim_stat"),
            label = "Select Method",
            choices = choice_r_limits
          )
        ),
        mainPanel(
          h3("Control Limit Calculations"),
          br(),
          h4("Enter Statistics"),
          fluidRow(
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                uiOutput(ns("r_lim_value")),
                numericInput(
                  inputId = ns("r_lim_n"),
                  label = withMathJax("$n:$"),
                  value = 5,
                  width = "75px"
                )
              )
            ),
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("r_lim_sterr"),
                  label = "Standard Errors",
                  value = 3,
                  min = 1,
                  width = "75px"
                ),
                br(),
                numericInput(
                  inputId = ns("r_lim_decimals"),
                  label = "Decimals",
                  value = 4,
                  min = 0,
                  max = 9,
                  step = 1,
                  width = "75px"
                )
              )
            )
          ),
          br(),
          h4("Results"),
          htmlOutput(ns("r_lim_out"))
        )
      )
    ),
    tabPanel(
      title = withMathJax("$s$"),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = ns("s_lim_stat"),
            label = "Select Method",
            choices = choice_r_limits,
            selected = 3
          )
        ),
        mainPanel(
          h3("Control Limit Calculations"),
          br(),
          h4("Enter Statistics"),
          fluidRow(
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                uiOutput(ns("s_lim_value")),
                numericInput(
                  inputId = ns("s_lim_n"),
                  label = withMathJax("$n:$"),
                  value = 5,
                  width = "75px"
                )
              )
            ),
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("s_lim_sterr"),
                  label = "Standard Errors",
                  value = 3,
                  min = 1,
                  width = "75px"
                ),
                br(),
                numericInput(
                  inputId = ns("s_lim_decimals"),
                  label = "Decimals",
                  value = 4,
                  min = 0,
                  max = 9,
                  step = 1,
                  width = "75px"
                )
              )
            )
          ),
          br(),
          h4("Results"),
          htmlOutput(ns("s_lim_out"))
        )
      )
    ),
    tabPanel(
      title = withMathJax("$s^{2}$"),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = ns("s2_lim_stat"),
            label = "Select Method",
            choices = choice_r_limits,
            selected = 5
          )
        ),
        mainPanel(
          h3("Control Limit Calculations"),
          br(),
          h4("Enter Statistics"),
          fluidRow(
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                uiOutput(ns("s2_lim_value")),
                numericInput(
                  inputId = ns("s2_lim_n"),
                  label = withMathJax("$n:$"),
                  value = 5,
                  width = "75px"
                )
              )
            ),
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("s2_lim_sterr"),
                  label = "Standard Errors",
                  value = 3,
                  min = 1,
                  width = "75px"
                ),
                br(),
                numericInput(
                  inputId = ns("s2_lim_decimals"),
                  label = "Decimals",
                  value = 4,
                  min = 0,
                  max = 9,
                  step = 1,
                  width = "75px"
                )
              )
            )
          ),
          br(),
          h4("Results"),
          htmlOutput(ns("s2_lim_out"))
        )
      )
    ),
    tabPanel(
      title = withMathJax("$p$"),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = ns("p_lim_stat"),
            label = "Select Method",
            choices = c("Exact" = 1, "Approximate" = 2)
          )
        ),
        mainPanel(
          h3("Control Limit Calculations"),
          br(),
          h4("Enter Statistics"),
          fluidRow(
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("p_lim_value"),
                  label = withMathJax("$\\bar{p}:$"),
                  value = 0.5,
                  min = 0,
                  max = 1,
                  width = "75px"
                ),
                numericInput(
                  inputId = ns("p_lim_n"),
                  label = withMathJax("$n:$"),
                  value = 10,
                  width = "75px"
                )
              )
            ),
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("p_lim_sterr"),
                  label = "Standard Errors",
                  value = 3,
                  min = 1,
                  width = "75px"
                ),
                br(),
                numericInput(
                  inputId = ns("p_lim_decimals"),
                  label = "Decimals",
                  value = 4,
                  min = 0,
                  max = 9,
                  step = 1,
                  width = "75px"
                )
              )
            )
          ),
          br(),
          h4("Results"),
          htmlOutput(ns("p_lim_out"))
        )
      )
    ),
    tabPanel(
      title = withMathJax("$np$"),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = ns("np_lim_stat"),
            label = "Select Method",
            choices = c("Exact" = 1, "Approximate" = 2)
          )
        ),
        mainPanel(
          h3("Control Limit Calculations"),
          br(),
          h4("Enter Statistics"),
          fluidRow(
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("np_lim_value"),
                  label = withMathJax("$\\overline{np}:$"),
                  value = 5,
                  min = 0,
                  width = "75px"
                ),
                numericInput(
                  inputId = ns("np_lim_n"),
                  label = withMathJax("$n:$"),
                  value = 10,
                  width = "75px"
                )
              )
            ),
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("np_lim_sterr"),
                  label = "Standard Errors",
                  value = 3,
                  min = 1,
                  width = "75px"
                ),
                br(),
                numericInput(
                  inputId = ns("np_lim_decimals"),
                  label = "Decimals",
                  value = 4,
                  min = 0,
                  max = 9,
                  step = 1,
                  width = "75px"
                )
              )
            )
          ),
          br(),
          h4("Results"),
          htmlOutput(ns("np_lim_out"))
        )
      )
    ),
    tabPanel(
      title = withMathJax("$c$"),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = ns("c_lim_stat"),
            label = "Select Method",
            choices = c("Exact" = 1, "Approximate" = 2)
          )
        ),
        mainPanel(
          h3("Control Limit Calculations"),
          br(),
          h4("Enter Statistics"),
          fluidRow(
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("c_lim_value"),
                  label = withMathJax("$\\bar{c}:$"),
                  value = 5,
                  min = 0,
                  width = "75px"
                )
              )
            ),
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("c_lim_sterr"),
                  label = "Standard Errors",
                  value = 3,
                  min = 1,
                  width = "75px"
                ),
                br(),
                numericInput(
                  inputId = ns("c_lim_decimals"),
                  label = "Decimals",
                  value = 4,
                  min = 0,
                  max = 9,
                  step = 1,
                  width = "75px"
                )
              )
            )
          ),
          br(),
          h4("Results"),
          htmlOutput(ns("c_lim_out"))
        )
      )
    ),
    tabPanel(
      title = withMathJax("$u$"),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = ns("u_lim_stat"),
            label = "Select Method",
            choices = c("Exact" = 1, "Approximate" = 2)
          )
        ),
        mainPanel(
          h3("Control Limit Calculations"),
          br(),
          h4("Enter Statistics"),
          fluidRow(
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("u_lim_value"),
                  label = withMathJax("$\\bar{u}:$"),
                  value = 0.5,
                  min = 0,
                  width = "75px"
                ),
                numericInput(
                  inputId = ns("u_lim_n"),
                  label = withMathJax("$n:$"),
                  value = 10,
                  width = "75px"
                )
              )
            ),
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("u_lim_sterr"),
                  label = "Standard Errors",
                  value = 3,
                  min = 1,
                  width = "75px"
                ),
                br(),
                numericInput(
                  inputId = ns("u_lim_decimals"),
                  label = "Decimals",
                  value = 4,
                  min = 0,
                  max = 9,
                  step = 1,
                  width = "75px"
                )
              )
            )
          ),
          br(),
          h4("Results"),
          htmlOutput(ns("u_lim_out"))
        )
      )
    ),
    tabPanel(
      title = withMathJax("$\\kappa$"),
      sidebarLayout(
        sidebarPanel(
          h5("SPC Limit Calculations"),
          uiOutput(ns("kappa_limits_kappa")),
          uiOutput(ns("kappa_limits_var")),
          tags$hr(style = "border-color: black;"),
          h5("Discrete Capability Calculations"),
          numericInput(
            inputId = ns("kappa_cap_po"),
            label = "Desired P\u2080",
            value = 0.998,
            min = 0,
            max = 1
          ),
          numericInput(
            inputId = ns("kappa_cap_cp"),
            label = "Highest CP",
            value = NA,
            min = 0,
            max = 1
          )
        ),
        mainPanel(
          h3("Control Limit Calculations"),
          br(),
          h4("Enter Statistics"),
          fluidRow(
            column(width = 4),
            column(
              width = 4,
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("kappa_sterr"),
                  label = "Standard Errors",
                  value = 2,
                  min = 1,
                  width = "75px"
                ),
                br(),
                numericInput(
                  inputId = ns("kappa_decimals"),
                  label = "Decimals",
                  value = 4,
                  min = 0,
                  max = 9,
                  step = 1,
                  width = "75px"
                )
              )
            )
          ),
          br(),
          h4("Results"),
          htmlOutput(ns("kappa_lim_out")),
          htmlOutput(ns("kappa_crit_out"))
        )
      )
    )
  )
}
