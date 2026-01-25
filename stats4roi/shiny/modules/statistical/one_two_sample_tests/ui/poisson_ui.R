# =========================================================================
# POISSON UI MODULE
# =========================================================================
# UI for Poisson tab - handles both "Enter Statistics" and "Use Data" modes
# Part of One- and Two-Sample Tests module

create_poisson_one_two_sample_ui <- function(ns) {
  tabsetPanel(
    # Enter Statistics Tab
    tabPanel(
      title = "Enter Statistics",
      h3("Poisson Rate Testing - Enter Statistics"),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = ns("one_or_two_poi"),
            label = "One or Two Sample?",
            choices = c("One Sample" = 1, "Two Sample" = 2)
          ),
          numericInput(
            inputId = ns("conf_poi"),
            label = "Confidence",
            value = 0.95,
            min = 0,
            max = 1,
            step = 0.01,
            width = "75px"
          ),
          numericInput(
            inputId = ns("decimal_poi"),
            label = "Decimals",
            value = 4,
            min = 0,
            max = 9,
            step = 1,
            width = "75px"
          )
        ),
        mainPanel(
          fluidRow(
            conditionalPanel(
              condition = paste0("input['", ns("one_or_two_poi"), "'] == 1"),
              column(
                width = 6,
                selectInput(
                  inputId = ns("alt_poi"),
                  label = "Alternative hypothesis for rates",
                  choices = NULL  # Will be set in coordinator
                )
              )
            ),
            conditionalPanel(
              condition = paste0("input['", ns("one_or_two_poi"), "'] == 2"),
              column(
                width = 6,
                selectInput(
                  inputId = ns("alt_poi_2"),
                  label = "Alternative hypothesis for rates",
                  choices = NULL  # Will be set in coordinator
                )
              )
            )
          ),
          hr(),
          h3("Enter Statistics and Parameters"),
          fluidRow(
            column(
              width = 3,
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("poi_samp"),
                  label = withMathJax("$$c_{1}:{ }$$"),
                  value = 1,
                  min = 0,
                  step = 1,
                  width = "150px"
                )
              ),
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("n_samp_poi"),
                  label = withMathJax("$$n_{1}:{ }$$"),
                  value = 10,
                  min = 1,
                  step = 1,
                  width = "150px"
                )
              )
            ),
            column(
              width = 3,
              conditionalPanel(
                condition = paste0("input['", ns("one_or_two_poi"), "'] == 1"),
                tags$div(
                  id = "inline1",
                  class = "inline",
                  numericInput(
                    inputId = ns("poi0"),
                    label = withMathJax("$$\\lambda_{0}:{ }$$"),
                    value = 2.5,
                    min = 0,
                    width = "150px"
                  )
                )
              ),
              conditionalPanel(
                condition = paste0("input['", ns("one_or_two_poi"), "'] == 2"),
                tags$div(
                  id = "inline1",
                  class = "inline",
                  numericInput(
                    inputId = ns("poi2"),
                    label = withMathJax("$$c_{2}:{ }$$"),
                    value = 3,
                    min = 0,
                    step = 1,
                    width = "150px"
                  )
                ),
                tags$div(
                  id = "inline1",
                  class = "inline",
                  numericInput(
                    inputId = ns("n_samp_poi_2"),
                    label = withMathJax("$$n_{2}:{ }$$"),
                    value = 10,
                    min = 0,
                    step = 1,
                    width = "150px"
                  )
                )
              )
            )
          ),
          br(),
          h4("Results"),
          uiOutput(ns("pretty_poi_stat"))
        )
      )
    ),
    
    # Use Data Tab
    tabPanel(
      title = "Use Data",
      h3("Poisson Rate Testing - Use Data"),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = ns("data_type_poi"),
            label = "How is the data configured?",
            choices = c("Data in Columns" = 1, "Use Reference Column" = 2)
          ),
          conditionalPanel(
            condition = paste0("input['", ns("data_type_poi"), "'] == 1"),
            uiOutput(ns("data_choice_column_poi"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("data_type_poi"), "'] == 2"),
            uiOutput(ns("data_choice_ref_poi")),
            uiOutput(ns("data_choice_data_poi")),
            uiOutput(ns("data_choice_g1_poi")),
            uiOutput(ns("data_choice_g2_poi"))
          ),
          numericInput(
            inputId = ns("conf_poi_data"),
            label = "Confidence",
            value = 0.95,
            min = 0,
            max = 1,
            step = 0.01,
            width = "75px"
          ),
          numericInput(
            inputId = ns("decimal_poi_d"),
            label = "Decimals",
            value = 4,
            min = 0,
            max = 9,
            step = 1,
            width = "75px"
          )
        ),
        mainPanel(
          fluidRow(
            column(6, uiOutput(ns("alt_poi_data")))
          ),
          hr(),
          h3("Statistics from Data"),
          fluidRow(
            column(3, uiOutput(ns("poi_test_data_ui1"))),
            column(3, uiOutput(ns("poi_test_data_ui2")))
          ),
          fluidRow(
            column(3, uiOutput(ns("poi_test_data_ui3"))),
            column(3, uiOutput(ns("poi_test_data_ui4")))
          ),
          br(),
          h4("Results"),
          htmlOutput(ns("pretty_poi_stat_data"))
        )
      )
    )
  )
}
