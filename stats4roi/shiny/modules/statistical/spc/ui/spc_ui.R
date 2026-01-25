# SPC UI (Control Charts)
# Recreates the SPC tab layout from app_monolithic.R.
#
# NOTE: Business logic is implemented in coordinator/workers; this file is UI only.

library(shiny)
library(DT)
library(shinyWidgets)

# Source limits UI
source("modules/statistical/spc/ui/spc_limits_ui.R")

create_spc_ui_internal <- function(ns) {
  tabPanel(
    title = "SPC",
    h3("Control Charts"),
    tabsetPanel(
      tabPanel(
        title = "Variables",
        sidebarLayout(
          sidebarPanel(
            h4("1. Set Up Chart"),
            tags$div(
              materialSwitch(inputId = ns("spc_var_ind_or_mean"), label = "Mean", value = FALSE, inline = TRUE),
              tags$span("Individuals")
            ),
            conditionalPanel(
              condition = paste0("input['", ns("spc_var_ind_or_mean"), "']==0"),
              radioButtons(
                inputId = ns("spc_var_data_type"),
                label = "Subgroups are:",
                choices = c("A row across multiple columns" = 1, "Defined by a column" = 2)
              )
            ),
            uiOutput(ns("spc_var_UI1")),
            uiOutput(ns("spc_var_UI2")),
            uiOutput(ns("spc_var_set")),
            checkboxInput(inputId = ns("spc_runchart"), label = "Plot as run chart (no limits)?", value = FALSE),
            uiOutput(ns("spc_run_type")),
            conditionalPanel(
              condition = paste0("input['", ns("spc_runchart"), "']==0"),
              h4("2. Select Chart Types and Limits"),
              dropdown(
                fluidRow(
                  column(
                    6,
                    uiOutput(ns("spc_var_loc_type")),
                    conditionalPanel(
                      condition = paste0("input['", ns("spc_var_loc_lim"), "']<11"),
                      selectInput(inputId = ns("spc_x_loc"), label = "Location Centerline", choices = choice_x_centerline)
                    ),
                    conditionalPanel(
                      condition = paste0("input['", ns("spc_var_disp_type"), "']==4"),
                      numericInput(inputId = ns("spc_mr_span"), label = "Span of MR", value = 2, min = 2, step = 1)
                    ),
                    uiOutput(ns("spc_var_loc_lim")),
                    conditionalPanel(
                      condition = paste0("input['", ns("spc_var_loc_lim"), "']==12"),
                      hr(style = "border-top: 1px solid #000000;"),
                      h4("Custom Location Limits"),
                      numericInput(inputId = ns("custom.x.upper"), label = "UCL", value = 0),
                      numericInput(inputId = ns("custom.x.center"), label = "Centerline", value = 0),
                      numericInput(inputId = ns("custom.x.lower"), label = "LCL", value = 0)
                    )
                  ),
                  column(
                    6,
                    uiOutput(ns("spc_var_disp_type")),
                    conditionalPanel(
                      condition = paste0(
                        "input['", ns("spc_var_disp_lim"), "']<11 && input['", ns("spc_var_disp_lim"), "']!=9"
                      ),
                      selectInput(inputId = ns("spc_x_disp"), label = "Dispersion Centerline", choices = choice_x_centerline)
                    ),
                    uiOutput(ns("spc_var_disp_lim")),
                    conditionalPanel(
                      condition = paste0("input['", ns("spc_var_disp_lim"), "']==12"),
                      hr(style = "border-top: 1px solid #000000;"),
                      h4("Custom Dispersion Limits"),
                      numericInput(inputId = ns("custom.disp.upper"), label = "UCL", value = 0),
                      numericInput(inputId = ns("custom.disp.center"), label = "Centerline", value = 0),
                      numericInput(inputId = ns("custom.disp.lower"), label = "LCL", value = 0)
                    ),
                    style = "border-left: 1px solid"
                  )
                ),
                conditionalPanel(
                  condition = paste0(
                    "input['", ns("spc_var_loc_lim"), "']==9 || input['", ns("spc_var_disp_lim"), "']==9"
                  ),
                  numericInput(inputId = ns("known_sig_x"), label = "Enter \u03c3", value = 1, min = 0)
                ),
                selectInput(inputId = ns("std_err_x"), label = "Standard Errors", choices = c(2, 3), selected = 3),
                circle = TRUE,
                status = "danger",
                icon = icon("fas fa-chart-line"),
                width = "300px",
                tooltip = tooltipOptions(title = "Click to select charts and limits")
              )
            )
          ),
          mainPanel(
            div(
              style = "position:relative",
              plotOutput(
                ns("xchartout"),
                hover = hoverOpts(ns("plot_hover"), delay = 100, delayType = "debounce")
              ),
              uiOutput(ns("hover_info_x"), style = "pointer-events: none")
            ),
            HTML("<p style='text-align:right;'><small>Mouse over for point information</small></p>"),
            fluidRow(
              column(3, downloadButtonUI(ns("xchartout"))),
              column(3, tags$div(id = "inline1", class = "inline", downloadSelectUI(ns("xchartout")))),
              column(
                6,
                tags$div(
                  id = "inline1",
                  class = "inline",
                  numericInput(inputId = ns("spc_font_size"), label = "Base Font Size", value = 11, min = 1, step = 1, width = "75px")
                )
              )
            ),
            tags$hr(style = "border-color: black;"),
            br(),
            fluidRow(
              column(
                width = 2,
                conditionalPanel(
                  condition = paste0("input['", ns("spc_runchart"), "']==0"),
                  dropdown(
                    tags$h4("Chart Options"),
                    uiOutput(ns("ooc_rules_x_ui")),
                    uiOutput(ns("ooc_rules_disp_ui")),
                    selectInput(inputId = ns("run_length_x"), label = "Run Length", choices = c(8, 9), selected = 8),
                    checkboxGroupButtons(
                      inputId = ns("x_chart_options"),
                      label = "Graph Features",
                      choices = c(
                        "Connect Points" = 1,
                        "Control Limits" = 2,
                        "Center Line" = 3,
                        "Show OOC Points" = 4,
                        "Show OOC Labels" = 5,
                        "Show Zones" = 6
                      ),
                      direction = "vertical",
                      selected = c(1, 2, 3, 4)
                    ),
                    circle = TRUE,
                    status = "success",
                    icon = icon("gear"),
                    width = "300px",
                    tooltip = tooltipOptions(title = "Click to modify chart")
                  )
                )
              ),
              column(width = 3, uiOutput(ns("spc_analysis"))),
              column(
                width = 3,
                prettySwitch(
                  inputId = ns("x_spc_show_data"),
                  label = "Show plot data",
                  value = FALSE,
                  status = "success",
                  fill = TRUE
                )
              ),
              column(3, numericInput(inputId = ns("spc_x_anova_decimals"), label = "Decimals", value = 4, min = 0, step = 1, width = "50px"))
            ),
            conditionalPanel(
              condition = paste0("input['", ns("x_spc_show_anova"), "']==1"),
              tags$div(
                id = "inline1",
                class = "inline",
                fluidRow(
                  column(6, uiOutput(ns("limit_analysis_x"))),
                  column(
                    6,
                    p("Enter specs for Capability Analysis"),
                    numericInput(inputId = ns("spc_x_USL"), label = "USL: ", value = NA, width = "75px"),
                    numericInput(inputId = ns("spc_x_target"), label = "Target: ", value = NA, width = "75px"),
                    numericInput(inputId = ns("spc_x_LSL"), label = "LSL: ", value = NA, width = "75px")
                  )
                )
              ),
              hr(style = "border-top: 1px solid #000000;"),
              uiOutput(ns("capability_analysis_x")),
              hr(style = "border-top: 1px solid #000000;"),
              fluidRow(
                column(8, h4("Oneway Random Effects ANOVA")),
                column(4, numericInput(inputId = ns("spc_x_anova_conf"), label = "Confidence", value = 0.95, min = 0, max = 1, width = "75px"))
              ),
              htmlOutput(ns("spc_anova"))
            ),
            conditionalPanel(
              condition = paste0("input['", ns("x_spc_show_data"), "']==1"),
              hr(style = "border-top: 1px solid #000000;"),
              h4("Plot Data"),
              DT::dataTableOutput(ns("xbar_table_out"))
            )
          )
        )
      ),
      tabPanel(
        title = "Attributes",
        sidebarLayout(
          sidebarPanel(
            h4("1. Set Up Chart"),
            radioButtons(
              inputId = ns("spc_att_data_type"),
              label = "Observations are:",
              choices = c("Each row" = 1)
            ),
            uiOutput(ns("spc_att_UI1")),
            checkboxInput(inputId = ns("att_const_n"), label = "Constant n?", value = FALSE),
            uiOutput(ns("spc_att_UI2")),
            conditionalPanel(
              condition = paste0(
                "input['", ns("spc_att_loc"), "']!=3 && input['", ns("spc_att_loc_lim"), "']!=8"
              ),
              uiOutput(ns("spc_att_set"))
            ),
            h4("2. Select Chart Types and Limits"),
            dropdown(
              fluidRow(
                uiOutput(ns("spc_att_loc_type")),
                conditionalPanel(
                  condition = paste0("input['", ns("spc_att_loc_lim"), "']<6"),
                  uiOutput(ns("spc_att_loc"))
                ),
                uiOutput(ns("spc_att_loc_lim")),
                conditionalPanel(
                  condition = paste0("input['", ns("spc_att_loc_lim"), "']==8"),
                  hr(style = "border-top: 1px solid #000000;"),
                  h4("Custom Limits"),
                  numericInput(inputId = ns("custom.att.upper"), label = "UCL", value = 0),
                  numericInput(inputId = ns("custom.att.center"), label = "Centerline", value = 0),
                  numericInput(inputId = ns("custom.att.lower"), label = "LCL", value = 0)
                ),
                uiOutput(ns("known_param_att")),
                selectInput(inputId = ns("std_err_att"), label = "Standard Errors", choices = c(2, 3), selected = 3)
              ),
              circle = TRUE,
              status = "danger",
              icon = icon("fas fa-chart-line"),
              width = "300px",
              tooltip = tooltipOptions(title = "Click to select charts and limits")
            )
          ),
          mainPanel(
            div(
              style = "position:relative",
              plotOutput(
                ns("attchartout"),
                hover = hoverOpts(ns("plot_hover_att"), delay = 100, delayType = "debounce")
              ),
              uiOutput(ns("hover_info_att"), style = "pointer-events: none")
            ),
            fluidRow(
              column(3, downloadButtonUI(ns("attchartout"))),
              column(3, tags$div(id = "inline1", class = "inline", downloadSelectUI(ns("attchartout")))),
              column(
                6,
                tags$div(
                  id = "inline1",
                  class = "inline",
                  numericInput(
                    inputId = ns("att_spc_font_size"),
                    label = "Base Font Size",
                    value = 11,
                    min = 1,
                    step = 1,
                    width = "75px"
                  )
                )
              )
            ),
            br(),
            fluidRow(
              column(
                width = 2,
                dropdown(
                  tags$h4("Chart Options"),
                  uiOutput(ns("ooc_rules_att_ui")),
                  selectInput(inputId = ns("run_length_att"), label = "Run Length", choices = c(8, 9), selected = 8),
                  checkboxGroupButtons(
                    inputId = ns("att_chart_options"),
                    label = "Graph Features",
                    choices = c(
                      "Connect Points" = 1,
                      "Control Limits" = 2,
                      "Center Line" = 3,
                      "Show OOC Points" = 4,
                      "Show OOC Labels" = 5,
                      "Show Zones" = 6
                    ),
                    direction = "vertical",
                    selected = c(1, 2, 3, 4)
                  ),
                  circle = TRUE,
                  status = "success",
                  icon = icon("gear"),
                  width = "300px",
                  tooltip = tooltipOptions(title = "Click to modify chart")
                )
              ),
              column(
                width = 3,
                prettySwitch(
                  inputId = ns("att_spc_show_analysis"),
                  label = "Limit Summary",
                  value = FALSE,
                  status = "success",
                  fill = TRUE
                )
              ),
              column(
                width = 3,
                prettySwitch(
                  inputId = ns("att_spc_show_data"),
                  label = "Show plot data",
                  value = FALSE,
                  status = "success",
                  fill = TRUE
                )
              ),
              column(
                3,
                numericInput(
                  inputId = ns("spc_att_decimals"),
                  label = "Decimals",
                  value = 4,
                  min = 0,
                  step = 1,
                  width = "75px"
                )
              )
            ),
            conditionalPanel(
              condition = paste0("input['", ns("att_spc_show_analysis"), "']==1"),
              uiOutput(ns("limit_analysis_att"))
            ),
            conditionalPanel(
              condition = paste0("input['", ns("att_spc_show_data"), "']==1"),
              hr(style = "border-top: 1px solid #000000;"),
              h4("Plot Data"),
              DT::dataTableOutput(ns("att_table_out"))
            )
          )
        )
      ),
      tabPanel(
        title = "Limits Calculations",
        create_spc_limits_ui_internal(ns)
      )
    )
  )
}

