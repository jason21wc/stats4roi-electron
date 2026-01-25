# =========================================================================
# CORRELATION AND ASSOCIATION UI MODULE
# =========================================================================
# UI for Correlation and Association tab - handles "Enter Statistics", "Use Data", and "Scatterplot" modes

create_correlation_association_ui_internal <- function(ns) {
  tabsetPanel(
    # Enter Statistics Tab
    tabPanel(
      title = "Enter Statistics",
      h3("Correlation and Association"),
      br(),
      sidebarLayout(
        sidebarPanel(
          numericInput(
            inputId = ns("conf_corr"),
            label = "Confidence",
            value = 0.95,
            min = 0,
            max = 1,
            step = 0.01,
            width = "75px"
          ),
          selectInput(
            inputId = ns("one_or_two_corr"),
            label = "One Sample or Two?",
            choices = c("One-Sample" = 1, "Two Sample Independent" = 2, "Two-Sample Dependent" = 3)
          ),
          uiOutput(ns("corr_tests")),
          numericInput(
            inputId = ns("decimal_corr"),
            label = "Decimals",
            value = 4,
            min = 0,
            max = 9,
            step = 1,
            width = "75px"
          ),
          checkboxInput(
            inputId = ns("corr_more_info"),
            label = "More information about selected test",
            value = FALSE
          )
        ),
        mainPanel(
          fluidRow(
            column(5, uiOutput(ns("corr_alt")))
          ),
          fluidRow(
            column(6,
              tags$div(
                id = "inline1",
                class = "inline",
                uiOutput(ns("corrUI1")),
                HTML("</br>"),
                uiOutput(ns("corrUI3")),
                HTML("</br>"),
                uiOutput(ns("corrUI5"))
              )
            ),
            column(6,
              tags$div(
                id = "inline1",
                class = "inline",
                uiOutput(ns("corrUI2")),
                HTML("</br>"),
                uiOutput(ns("corrUI4")),
                HTML("</br>"),
                uiOutput(ns("corrUI6")),
                HTML("</br>")
              )
            )
          ),
          br(),
          h4("Results"),
          uiOutput(ns("pretty_corr"))
        )
      )
    ),
    
    # Use Data Tab
    tabPanel(
      title = "Use Data",
      h3("Correlation and Association"),
      br(),
      sidebarLayout(
        sidebarPanel(
          numericInput(
            inputId = ns("conf_corr_data"),
            label = "Confidence",
            value = 0.95,
            min = 0,
            max = 1,
            step = 0.01,
            width = "75px"
          ),
          selectInput(
            inputId = ns("one_or_two_corr_data"),
            label = "One Sample or Two?",
            choices = c("One-Sample" = 1, "Two Sample Independent" = 2, "Two-Sample Dependent" = 3)
          ),
          uiOutput(ns("corr_tests_data")),
          numericInput(
            inputId = ns("decimal_corr_data"),
            label = "Decimals",
            value = 4,
            min = 0,
            max = 9,
            step = 1,
            width = "75px"
          ),
          checkboxInput(
            inputId = ns("corr_more_info_data"),
            label = "More information about selected test",
            value = FALSE
          ),
          checkboxInput(
            inputId = ns("corr_data_info"),
            label = "Data Structure Requirements",
            value = FALSE
          ),
          uiOutput(ns("corr_data_info_text"))
        ),
        mainPanel(
          fluidRow(
            column(5, uiOutput(ns("data_config")))
          ),
          fluidRow(
            column(3, uiOutput(ns("data_choice_column_corr_1"))),
            column(3, uiOutput(ns("data_choice_column_corr_2"))),
            column(3, uiOutput(ns("data_choice_column_corr_3"))),
            column(3, uiOutput(ns("data_choice_column_corr_4")))
          ),
          fluidRow(
            conditionalPanel(
              condition = paste0("input['", ns("corr_tests_data"), "'] == 4 || ",
                               "input['", ns("corr_tests_data"), "'] == 7 || ",
                               "input['", ns("corr_tests_data"), "'] == 8 || ",
                               "input['", ns("corr_tests_data"), "'] == 9 || ",
                               "input['", ns("corr_tests_data"), "'] == 10 || ",
                               "input['", ns("corr_tests_data"), "'] == 11 || ",
                               "input['", ns("corr_tests_data"), "'] == 12"),
              verbatimTextOutput(ns("corr_xtab"))
            )
          ),
          fluidRow(
            column(5, uiOutput(ns("corr_alt_data")))
          ),
          fluidRow(
            column(6,
              tags$div(
                id = "inline1",
                class = "inline",
                uiOutput(ns("corrUI1_data")),
                HTML("</br>"),
                uiOutput(ns("corrUI3_data")),
                HTML("</br>"),
                uiOutput(ns("corrUI5_data"))
              )
            ),
            column(6,
              tags$div(
                id = "inline1",
                class = "inline",
                uiOutput(ns("corrUI2_data")),
                HTML("</br>"),
                uiOutput(ns("corrUI4_data")),
                HTML("</br>")
              )
            )
          ),
          br(),
          h4("Results"),
          uiOutput(ns("pretty_corr_data"))
        )
      )
    ),
    
    # Scatterplot Tab
    tabPanel(
      title = "Scatterplot",
      h3("Scatterplot"),
      br(),
      sidebarLayout(
        sidebarPanel(
          numericInput(
            inputId = ns("conf_scatter"),
            label = "Confidence",
            value = 0.95,
            min = 0,
            max = 1,
            step = 0.01,
            width = "75px"
          ),
          numericInput(
            inputId = ns("decimal_scat"),
            label = "Decimals",
            value = 4,
            min = 0,
            max = 9,
            step = 1,
            width = "75px"
          ),
          uiOutput(ns("scat_x")),
          uiOutput(ns("scat_y")),
          pickerInput(
            inputId = ns("curve_fit"),
            label = "Fit Model",
            choices = curve_fit_choice,
            options = list(title = "None Selected")
          ),
          uiOutput(ns("model_ci")),
          uiOutput(ns("point_ci")),
          prettySwitch(
            inputId = ns("y_x_line"),
            label = "y = x Line?",
            value = FALSE,
            status = "success",
            fill = TRUE
          )
        ),
        mainPanel(
          div(
            style = "position:relative",
            plotOutput(ns("scatterplot"), hover = hoverOpts(ns("scat_hover"), delay = 100, delayType = "debounce")),
            uiOutput(ns("hover_info_scat"), style = "pointer-events: none")
          ),
          HTML("<p style='text-align:right;'><small>Mouse over for point information</small></p>"),
          fluidRow(
            column(3, downloadButtonUI(ns("scatterplot"))),
            column(3,
              tags$div(
                id = "inline1",
                class = "inline",
                downloadSelectUI(ns("scatterplot"))
              )
            ),
            column(6,
              tags$div(
                id = "inline1",
                class = "inline",
                numericInput(
                  inputId = ns("scat_font_size"),
                  label = "Base Font Size",
                  value = 11,
                  min = 1,
                  step = 1,
                  width = "75px"
                )
              )
            )
          ),
          tags$hr(style = "border-color: black;"),
          uiOutput(ns("scatter_plot_stats"))
        )
      )
    )
  )
}
