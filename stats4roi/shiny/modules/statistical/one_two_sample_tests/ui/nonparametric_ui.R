# =========================================================================
# NONPARAMETRIC UI MODULE
# =========================================================================
# UI for Nonparametric tab - handles both "Enter Statistics" and "Use Data" modes
# Part of One- and Two-Sample Tests module

create_nonparametric_one_two_sample_ui <- function(ns) {
  tabsetPanel(
    # Enter Statistics Tab
    tabPanel(
      title = "Enter Statistics",
      h3("Nonparametric Tests"),
      br(),
      sidebarLayout(
        sidebarPanel(
          numericInput(
            inputId = ns("conf_np"),
            label = "Confidence",
            value = 0.95,
            min = 0,
            max = 1,
            step = 0.01,
            width = "75px"
          ),
          selectInput(
            inputId = ns("one_or_two_np"),
            label = "One Sample or Two?",
            choices = c("One-Sample" = 1, "Two Sample Independent" = 2, "Two-Sample Dependent" = 3)
          ),
          uiOutput(ns("np_tests")),
          numericInput(
            inputId = ns("np_decimals"),
            label = "Decimals",
            value = 4,
            min = 0,
            step = 1,
            width = "75px"
          ),
          checkboxInput(
            inputId = ns("np_more_info"),
            label = "More information about selected test",
            value = FALSE
          )
        ),
        mainPanel(
          br(),
          fluidRow(
            column(5, uiOutput(ns("np_alt")))
          ),
          fluidRow(
            column(6,
              tags$div(
                id = "inline1",
                class = "inline",
                uiOutput(ns("npUI1")),
                HTML("</br>"),
                uiOutput(ns("npUI3")),
                HTML("</br>"),
                uiOutput(ns("npUI5"))
              )
            ),
            column(6,
              tags$div(
                id = "inline1",
                class = "inline",
                uiOutput(ns("npUI2")),
                HTML("</br>"),
                uiOutput(ns("npUI4")),
                HTML("</br>"),
                uiOutput(ns("npUI6")),
                HTML("</br>")
              )
            )
          ),
          br(),
          h4("Results"),
          uiOutput(ns("pretty_nonparametric"))
        )
      )
    ),
    
    # Use Data Tab
    tabPanel(
      title = "Use Data",
      sidebarLayout(
        sidebarPanel(
          numericInput(
            inputId = ns("conf_np_data"),
            label = "Confidence",
            value = 0.95,
            min = 0,
            max = 1,
            step = 0.01,
            width = "75px"
          ),
          radioButtons(
            inputId = ns("data_type_np"),
            label = "How is the data configured?",
            choices = c("Data in Columns" = 1, "Use Reference Column" = 2)
          ),
          conditionalPanel(
            condition = paste0("input['", ns("data_type_np"), "'] == 1"),
            uiOutput(ns("data_choice_column_np"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("data_type_np"), "'] == 2"),
            uiOutput(ns("data_choice_ref_np")),
            uiOutput(ns("data_choice_data_np")),
            uiOutput(ns("data_choice_g1_np")),
            uiOutput(ns("data_choice_g2_np"))
          ),
          uiOutput(ns("ind_or_dep_np_data")),
          uiOutput(ns("np_tests_data")),
          uiOutput(ns("np_mc_pass")),
          numericInput(
            inputId = ns("decimal_np_data"),
            label = "Decimals",
            value = 4,
            min = 0,
            max = 9,
            step = 1,
            width = "75px"
          ),
          checkboxInput(
            inputId = ns("np_more_info_data"),
            label = "More information about selected test",
            value = FALSE
          )
        ),
        mainPanel(
          br(),
          fluidRow(
            column(5, uiOutput(ns("np_alt_data"))),
            column(5,
              conditionalPanel(
                condition = paste0("input['", ns("np_tests_data"), "'] == 3"),
                p("This calculation can take a long time with large sample sizes"),
                actionBttn(
                  inputId = ns("np_data_u_go"),
                  label = "Start Calculation",
                  icon = icon("traffic-light"),
                  style = "material-flat",
                  color = "success"
                )
              )
            )
          ),
          br(),
          fluidRow(
            column(6,
              tags$div(
                id = "inline1",
                class = "inline",
                uiOutput(ns("npUI1_data")),
                HTML("</br>"),
                uiOutput(ns("npUI3_data")),
                HTML("</br>"),
                uiOutput(ns("npUI5_data"))
              )
            ),
            column(6,
              tags$div(
                id = "inline1",
                class = "inline",
                uiOutput(ns("npUI2_data")),
                HTML("</br>"),
                uiOutput(ns("npUI4_data")),
                HTML("</br>"),
                uiOutput(ns("npUI6_data")),
                HTML("</br>")
              )
            )
          ),
          br(),
          h4("Results"),
          uiOutput(ns("pretty_nonparametric_data"))
        )
      )
    )
  )
}
