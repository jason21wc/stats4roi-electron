# Oneway ANOVA UI Module
# UI for Oneway ANOVA tab with ANOVA and Post-hoc sub-tabs

create_oneway_anova_ui_internal <- function(ns) {
  navlistPanel(
    widths = c(2, 10),
    # ANOVA Tab
    tabPanel(
      "ANOVA",
      sidebarLayout(
        sidebarPanel(
          numericInput(
            inputId = ns("conf_ow"),
            label = "Confidence",
            value = 0.95,
            min = 0,
            max = 1,
            step = 0.01,
            width = "75px"
          ),
          numericInput(
            inputId = ns("decimal_ow"),
            label = "Decimals",
            value = 4,
            min = 0,
            max = 9,
            step = 1,
            width = "75px"
          ),
          uiOutput(ns("factor_ow")),
          uiOutput(ns("data_ow")),
          radioGroupButtons(
            inputId = ns("type_ow"),
            label = "Factor and Test Type",
            choices = c("Fixed - Welch" = 4, "Fixed - Fisher" = 1, "Random" = 2, "Kruskal-Wallis" = 3)
          ),
          conditionalPanel(
            condition = "input.type_ow != 3",
            ns = ns,
            prettySwitch(
              inputId = ns("disp_ow"),
              label = "Show Dispersion Tests?",
              status = "success",
              fill = TRUE,
              value = TRUE
            )
          )
        ),
        mainPanel(
          h4("Oneway ANOVA"),
          htmlOutput(ns("ow_table")),
          # Always render DT table - server returns empty table when not Kruskal-Wallis
          # This avoids DT initialization timing issues with conditional rendering
          DTOutput(ns("KW_out")),
          conditionalPanel(
            condition = "input.disp_ow == 1",
            ns = ns,
            htmlOutput(ns("ow_disp"))
          )
        )
      )
    ),
    # Post-hoc Tab
    tabPanel(
      "Post-hoc",
      sidebarLayout(
        sidebarPanel(
          numericInput(
            inputId = ns("decimal_ow_ph"),
            label = "Decimals",
            value = 4,
            min = 0,
            max = 9,
            step = 1,
            width = "75px"
          ),
          conditionalPanel(
            condition = "input.type_ow != 2",
            ns = ns,
            prettySwitch(
              inputId = ns("lines_ow_ph"),
              label = "Add Lines?",
              value = TRUE,
              status = "success",
              fill = TRUE
            )
          ),
          uiOutput(ns("ow_ph_plot_type")),
          uiOutput(ns("ow_ph_type")),
          conditionalPanel(
            condition = "input.type_ow != 2",
            ns = ns,
            checkboxInput(
              inputId = ns("ow_ph_details"),
              label = "Post-hoc Details?",
              value = FALSE
            )
          )
        ),
        mainPanel(
          h4("Oneway ANOVA Post-hoc"),
          h5("Confidence set on Oneway ANOVA tab"),
          plotOutput(ns("plotow")),
          fluidRow(
            column(3, downloadButtonUI(ns("plotow"))),
            column(3, tags$div(id = "inline1", class = "inline", downloadSelectUI(ns("plotow")))),
            column(6, tags$div(id = "inline1", class = "inline", numericInput(
              inputId = ns("ow_font_size"),
              label = "Base Font Size",
              value = 11,
              min = 1,
              step = 1,
              width = "75px"
            )))
          ),
          tags$hr(style = "border-color: black;"),
          DTOutput(ns("ow_ph_out_tab")),
          verbatimTextOutput(ns("ow_ph_details"))
        )
      )
    )
  )
}
