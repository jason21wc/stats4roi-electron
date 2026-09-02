# =========================================================================
# PROPORTIONS UI MODULE
# =========================================================================
# UI for Proportions tab - handles both "Enter Statistics" and "Use Data" modes
# Part of One- and Two-Sample Tests module

create_proportions_ui <- function(ns) {
  tabsetPanel(
    # Enter Statistics Tab
    tabPanel(
      title = "Enter Statistics",
      h3("Proportion Testing - Enter Statistics"),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = ns("one_or_two_p"),
            label = "One or Two Sample?",
            choices = c("One Sample" = 1, "Two Sample" = 2)
          ),
          numericInput(
            inputId = ns("conf_p"),
            label = "Confidence",
            value = 0.95,
            min = 0,
            max = 1,
            step = 0.01,
            width = "75px"
          ),
          numericInput(
            inputId = ns("decimal_p"),
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
              condition = paste0("input['", ns("one_or_two_p"), "'] == 1"),
              column(
                width = 6,
                selectInput(
                  inputId = ns("alt_p"),
                  label = "Alternative hypothesis for proportions",
                  choices = NULL  # Will be set in coordinator
                )
              )
            ),
            conditionalPanel(
              condition = paste0("input['", ns("one_or_two_p"), "'] == 2"),
              column(
                width = 6,
                selectInput(
                  inputId = ns("alt_p2"),
                  label = "Alternative hypothesis for proportions",
                  choices = NULL  # Will be set in coordinator
                )
              )
            )
          ),
          hr(),
          h3("Enter Statistics and Parameters"),
          checkboxInput(
            inputId = ns("prop_enter_counts"),
            label = "Enter Counts",
            value = FALSE
          ),
          fluidRow(
            class = "sample-size-input-row",
            column(
              width = 6,
              tags$div(
                class = "inline sample-size-param",
                numericInput(
                  inputId = ns("p_samp"),
                  label = withMathJax("$$p_{1}$$"),
                  value = 0.1,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              tags$div(
                class = "inline sample-size-param",
                numericInput(
                  inputId = ns("n_samp_p"),
                  label = withMathJax("$$n_{1}$$"),
                  value = 10,
                  min = 1,
                  step = 1,
                  width = "150px"
                )
              )
            ),
            column(
              width = 6,
              conditionalPanel(
                condition = paste0("input['", ns("one_or_two_p"), "'] == 1"),
                tags$div(
                  class = "inline sample-size-param",
                  numericInput(
                    inputId = ns("p0"),
                    label = withMathJax("$$\\pi_{0}:{ }$$"),
                    value = 0.5,
                    min = 0,
                    max = 1,
                    width = "150px"
                  )
                )
              ),
              conditionalPanel(
                condition = paste0("input['", ns("one_or_two_p"), "'] == 2"),
                tags$div(
                  class = "inline sample-size-param",
                  numericInput(
                    inputId = ns("p2"),
                    label = withMathJax("$$p_{2}:{ }$$"),
                    value = 0.5,
                    min = 0,
                    max = 1,
                    width = "150px"
                  )
                ),
                tags$div(
                  class = "inline sample-size-param",
                  numericInput(
                    inputId = ns("n_samp_p_2"),
                    label = withMathJax("$$n_{2}:{ }$$"),
                    value = 10,
                    min = 1,
                    step = 1,
                    width = "150px"
                  )
                )
              )
            )
          ),
          br(),
          h4("Results"),
          uiOutput(ns("pretty_prop_stat"))
        )
      )
    ),
    
    # Use Data Tab
    tabPanel(
      title = "Use Data",
      h3("Proportion Testing - Use Data"),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = ns("data_type_bi"),
            label = "How is the data configured?",
            choices = c("Data in Columns" = 1, "Use Reference Column" = 2)
          ),
          conditionalPanel(
            condition = paste0("input['", ns("data_type_bi"), "'] == 1"),
            uiOutput(ns("data_choice_column_bi"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("data_type_bi"), "'] == 2"),
            uiOutput(ns("data_choice_ref_bi")),
            uiOutput(ns("data_choice_data_bi")),
            uiOutput(ns("data_choice_g1_bi")),
            uiOutput(ns("data_choice_g2_bi"))
          ),
          # Single uiOutput — must not appear in both conditionalPanels (duplicate output IDs)
          uiOutput(ns("ots_bi_group_assignment")),
          uiOutput(ns("data_bi_success1")),
          uiOutput(ns("data_bi_success2")),
          numericInput(
            inputId = ns("conf_bi_data"),
            label = "Confidence",
            value = 0.95,
            min = 0,
            max = 1,
            step = 0.01,
            width = "75px"
          ),
          numericInput(
            inputId = ns("decimal_bi_d"),
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
            column(6, uiOutput(ns("alt_bi_data")))
          ),
          hr(),
          h3("Statistics from Data"),
          fluidRow(
            class = "sample-size-input-row",
            column(6, tags$div(class = "inline sample-size-param", uiOutput(ns("bi_test_data_ui1")))),
            column(6, tags$div(class = "inline sample-size-param", uiOutput(ns("bi_test_data_ui2"))))
          ),
          fluidRow(
            class = "sample-size-input-row",
            column(6, tags$div(class = "inline sample-size-param", uiOutput(ns("bi_test_data_ui3")))),
            column(6, tags$div(class = "inline sample-size-param", uiOutput(ns("bi_test_data_ui4"))))
          ),
          br(),
          h4("Results"),
          htmlOutput(ns("pretty_prop_stat_data"))
        )
      )
    )
  )
}
