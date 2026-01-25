# =========================================================================
# MEANS AND DISPERSION UI MODULE
# =========================================================================
# UI for Means and Dispersion tab - handles both "Enter Statistics" and "Use Data" modes
# Part of One- and Two-Sample Tests module

create_means_dispersion_ui <- function(ns) {
  tabsetPanel(
    # Enter Statistics Tab
    tabPanel(
      title = "Enter Statistics",
      h3("Mean and Dispersion Testing - Enter Statistics"),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = ns("one_or_two"),
            label = "One or Two Sample?",
            choices = c("One Sample" = 1, "Two Sample" = 2)
          ),
          uiOutput(ns("md_sig_known")),
          uiOutput(ns("md_t_dep_stat")),
          uiOutput(ns("md_t_type_stat")),
          numericInput(
            inputId = ns("conf"),
            label = "Confidence",
            value = 0.95,
            min = 0,
            max = 1,
            step = 0.01,
            width = "75px"
          ),
          numericInput(
            inputId = ns("decimal"),
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
            column(6, uiOutput(ns("alt_md"))),
            column(6,
              uiOutput(ns("onesample_md")),
              uiOutput(ns("md_dep_or_indep"))
            )
          ),
          hr(),
          h3("Enter Statistics and Parameters"),
          fluidRow(
            column(3, tags$div(id = "inline1", class = "inline", uiOutput(ns("mdUI1")))),
            column(3, tags$div(id = "inline1", class = "inline", uiOutput(ns("mdUI2"))))
          ),
          fluidRow(
            column(3, tags$div(id = "inline1", class = "inline", uiOutput(ns("mdUI3")))),
            column(3, tags$div(id = "inline1", class = "inline", uiOutput(ns("mdUI4"))))
          ),
          fluidRow(
            column(3, tags$div(id = "inline1", class = "inline", uiOutput(ns("mdUI5")))),
            column(3, tags$div(id = "inline1", class = "inline", uiOutput(ns("mdUI6"))))
          ),
          br(),
          h4("Results"),
          htmlOutput(ns("pretty_md"))
        )
      )
    ),
    
    # Use Data Tab
    tabPanel(
      title = "Use data",
      h3("Mean and Dispersion Testing - Data"),
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = ns("data_type_md"),
            label = "How is the data configured?",
            choices = c("Data in Columns" = 1, "Use Reference Column" = 2)
          ),
          conditionalPanel(
            condition = paste0("input['", ns("data_type_md"), "'] == 1"),
            uiOutput(ns("data_choice_column"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("data_type_md"), "'] == 2"),
            uiOutput(ns("data_choice_ref")),
            uiOutput(ns("data_choice_data")),
            uiOutput(ns("data_choice_g1")),
            uiOutput(ns("data_choice_g2"))
          ),
          uiOutput(ns("md_data_test_selection")),
          uiOutput(ns("md_t_dep")),
          uiOutput(ns("md_t_type")),
          numericInput(
            inputId = ns("conf_m_d_data"),
            label = "Confidence",
            value = 0.95,
            min = 0,
            max = 1,
            step = 0.01,
            width = "75px"
          ),
          numericInput(
            inputId = ns("decimal_m_d_d"),
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
            column(6, uiOutput(ns("alt_mean_data"))),
            column(6,
              uiOutput(ns("md_data_indep")),
              uiOutput(ns("md_data_one_samp_var"))
            )
          ),
          fluidRow(
            column(6, tags$div(id = "inline1", class = "inline", uiOutput(ns("md_data_UI1")))),
            column(6, tags$div(id = "inline1", class = "inline", uiOutput(ns("md_data_UI2"))))
          ),
          fluidRow(
            column(6, tags$div(id = "inline1", class = "inline", uiOutput(ns("md_data_UI3")))),
            column(6, tags$div(id = "inline1", class = "inline", uiOutput(ns("md_data_UI4"))))
          ),
          fluidRow(
            column(6, tags$div(id = "inline1", class = "inline", uiOutput(ns("md_data_UI5")))),
            column(6, tags$div(id = "inline1", class = "inline", uiOutput(ns("md_data_UI6"))))
          ),
          br(),
          h4("Results"),
          uiOutput(ns("pretty_md_data"))
        )
      )
    )
  )
}
