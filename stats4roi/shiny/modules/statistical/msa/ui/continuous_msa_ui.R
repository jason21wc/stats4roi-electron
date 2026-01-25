# Continuous MSA UI (Interval/Ratio)

library(shiny)
library(DT)
library(shinyWidgets)

create_continuous_msa_ui_internal <- function(ns) {
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Set up the analysis and press:"),
      actionButton(ns("msa_c_go"), "Perform Analysis", icon = icon("check"), style = "color: white; background-color:green;"),
      br(),
      br(),
      radioButtons(
        inputId = ns("msa_data_type"),
        label = "How is the data configured?",
        choices = c("Use reference columns" = 2)
      ),
      uiOutput(ns("msa_UI1")),
      uiOutput(ns("msa_UI2")),
      uiOutput(ns("msa_UI3")),
      uiOutput(ns("msa_UI4")),
      fluidRow(
        column(
          6,
          radioButtons(
            inputId = ns("msa_level"),
            label = "Data Level",
            choices = c("Interval/Ratio" = 1)
          )
        ),
        column(
          6,
          conditionalPanel(
            condition = paste0("input['", ns("msa_level"), "'] == 1"),
            radioButtons(
              inputId = ns("msa_type"),
              label = "What type of Study?",
              choices = c("Potential" = 1, "Short-Term" = 2, "Long-Term" = 3)
            ),
            checkboxInput(inputId = ns("msa_details"), label = "Info about study?")
          )
        )
      ),
      tags$hr(style = "border-color: black;"),
      fluidRow(
        column(
          6,
          checkboxInput(inputId = ns("msa_stand"), label = "Check Linearity and Bias?", value = FALSE)
        ),
        column(
          6,
          conditionalPanel(
            condition = paste0("input['", ns("msa_stand"), "'] == 1"),
            uiOutput(ns("msa_UI5"))
          )
        )
      ),
      tags$hr(style = "border-color: black;"),
      conditionalPanel(
        condition = paste0("input['", ns("msa_level"), "'] == 1"),
        radioButtons(
          inputId = ns("msa_calc"),
          label = "Calculation",
          choices = c("Range" = 1, "Standard Deviation" = 2, "ANOVA" = 3)
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("msa_level"), "'] == 1"),
        fluidRow(
          column(2),
          column(
            8,
            tags$div(
              materialSwitch(inputId = ns("msa_sigmas"), label = "5.15\u03c3", value = FALSE, inline = TRUE),
              tags$span("6\u03c3")
            )
          ),
          column(2)
        ),
        tags$div(
          materialSwitch(inputId = ns("msa_range_b"), label = "Spec Range", value = FALSE, inline = TRUE),
          tags$span("Enter Specs")
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("msa_range_b"), "'] == 1"),
        fluidRow(
          column(6, numericInput(inputId = ns("msa_LSL"), label = "LSL", value = NA)),
          column(6, numericInput(inputId = ns("msa_USL"), label = "USL", value = NA))
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("msa_range_b"), "'] == 0"),
        numericInput(inputId = ns("msa_range"), label = "Spec Range", value = NA)
      ),
      HTML("<p><b>Optional:</b> Enter specs or a spec range, the average of the process and the standard devation of the process <i>without measurement error</i> to see if there is a danger of misclassification and to get the number of distinct categories and discrimination ratio for the process.</p>"),
      fluidRow(
        column(6, numericInput(inputId = ns("proc_mean"), label = withMathJax("$\\mu_{process}$"), value = NULL)),
        column(6, numericInput(inputId = ns("proc_std"), label = withMathJax("$\\sigma_{process}$"), value = NULL))
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = paste0("input['", ns("msa_level"), "'] == 1"),
        h3("Measurement System Analysis - Interval/Ratio Data")
      ),
      conditionalPanel(
        condition = paste0("input['", ns("msa_level"), "'] == 2"),
        h3("Measurement System Analysis - Nominal Data")
      ),
      fluidRow(
        column(
          7,
          materialSwitch(inputId = ns("msa_diagnostic"), label = "Show Detailed Diagnostics?", value = FALSE, status = "success"),
          materialSwitch(inputId = ns("msa_graphs"), label = "Show Graphs?", value = TRUE, status = "success")
        ),
        column(3, numericInput(inputId = ns("conf_msa"), label = "Confidence", value = 0.95, min = 0, max = 1, step = 0.05, width = "75px")),
        column(2, numericInput(inputId = ns("deci_msa"), label = "Decimals", value = 4, min = 0, max = 9, step = 1, width = "75px"))
      ),
      htmlOutput(ns("msa_out")),
      conditionalPanel(
        condition = paste0("input['", ns("msa_graphs"), "'] == 1"),
        h4("Summary Graphs"),
        conditionalPanel(
          condition = paste0("input['", ns("msa_type"), "'] == 3"),
          checkboxInput(inputId = ns("msd_lt_axis"), label = "Match y axis?", value = TRUE),
          uiOutput(ns("lt_plot")),
          fluidRow(
            column(3, downloadButtonUI(ns("msalt"))),
            column(6, tags$div(id = "inline1", class = "inline", downloadSelectUI(ns("msalt"))))
          ),
          tags$hr(style = "border-color: black;"),
          uiOutput(ns("lt_plot_2")),
          fluidRow(
            column(3, downloadButtonUI(ns("msa_lt_overall"))),
            column(6, tags$div(id = "inline1", class = "inline", downloadSelectUI(ns("msa_lt_overall"))))
          ),
          tags$hr(style = "border-color: black;")
        ),
        uiOutput(ns("msa_cchart1")),
        fluidRow(
          column(3, downloadButtonUI(ns("msacchart1"))),
          column(6, tags$div(id = "inline1", class = "inline", downloadSelectUI(ns("msacchart1"))))
        ),
        tags$hr(style = "border-color: black;"),
        conditionalPanel(
          condition = paste0("input['", ns("msa_stand"), "'] == 1"),
          uiOutput(ns("msa_linearity_2")),
          fluidRow(
            column(3, downloadButtonUI(ns("linearityPlot"))),
            column(3, tags$div(id = "inline1", class = "inline", downloadSelectUI(ns("linearityPlot")))),
            column(3, checkboxInput(inputId = ns("msa_violin_line"), label = "Add violins?", value = FALSE)),
            column(3, checkboxInput(inputId = ns("msa_jitter_line"), label = "Add jitter?", value = FALSE))
          ),
          fluidRow(
            numericInput(
              inputId = ns("msa_as_measured"),
              label = "Enter Single As-Measured Reading to Estimate True Value",
              value = NA,
              width = "100%"
            ),
            DTOutput(ns("msa_linear_est"))
          ),
          tags$hr(style = "border-color: black;")
        ),
        plotOutput(ns("msascatter")),
        fluidRow(
          column(3, downloadButtonUI(ns("msascatter"))),
          column(6, tags$div(id = "inline1", class = "inline", downloadSelectUI(ns("msascatter"))))
        ),
        tags$hr(style = "border-color: black;"),
        uiOutput(ns("msa_cchart0")),
        fluidRow(
          column(3, downloadButtonUI(ns("msachart0"))),
          column(3, tags$div(id = "inline1", class = "inline", downloadSelectUI(ns("msachart0")))),
          column(2, checkboxInput(inputId = ns("msa_violin"), label = "Add violins?", value = FALSE)),
          column(2, checkboxInput(inputId = ns("msa_jitter"), label = "Add jitter?", value = FALSE)),
          column(2, checkboxInput(inputId = ns("norm_chart0"), label = "Normalize?", value = FALSE))
        ),
        tags$hr(style = "border-color: black;"),
        uiOutput(ns("msa_box")),
        fluidRow(
          column(3, downloadButtonUI(ns("msabox"))),
          column(3, tags$div(id = "inline1", class = "inline", downloadSelectUI(ns("msabox")))),
          column(4),
          column(2, checkboxInput(inputId = ns("norm_box"), label = "Normalize?", value = FALSE))
        ),
        tags$hr(style = "border-color: black;"),
        plotOutput(ns("msavarcompgraph")),
        fluidRow(
          column(3, downloadButtonUI(ns("msavarcompgraph"))),
          column(6, tags$div(id = "inline1", class = "inline", downloadSelectUI(ns("msavarcompgraph"))))
        ),
        tags$hr(style = "border-color: black;"),
        h4("Estimation of Specification Misclassification"),
        plotOutput(ns("msadangerzone")),
        fluidRow(
          column(3, downloadButtonUI(ns("msadangerzone"))),
          column(6, tags$div(id = "inline1", class = "inline", downloadSelectUI(ns("msadangerzone"))))
        ),
        tags$hr(style = "border-color: black;")
      )
    )
  )
}
