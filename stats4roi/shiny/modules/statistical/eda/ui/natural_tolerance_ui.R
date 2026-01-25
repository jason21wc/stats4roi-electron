# =========================================================================
# NATURAL TOLERANCE UI MODULE
# =========================================================================
# UI for natural tolerance analysis tab
# Part of EDA module - handles natural tolerance calculations

create_natural_tolerance_ui <- function(ns) {
  
  tagList(
    h4("Natural Tolerance"),
    tabsetPanel(
      tabPanel(
        "Use Data",
        sidebarLayout(
          sidebarPanel(
            numericInput(
              inputId = ns("decimals_nt_data"),
              label = "Decimals",
              value = 4,
              min = 0,
              step = 1,
              width = "75px"
            ),
            pickerInput(
              inputId = ns("dist_nt_data"),
              label = "Select a Distribution",
              choices = c(
                "Normal" = 1,
                "Exponential(Low)" = 2,
                "Exponential(0)" = 3
              )
            ),
            uiOutput(ns("nt_data_list"))
          ), # end sidebarpanel
          mainPanel(
            h4("Natural Tolerance"),
            DTOutput(ns("nt_out_data"))
          ) # end mainpanel
        ) # end sidebarlayout
      ),
      tabPanel(
        "Enter Statistics",
        sidebarLayout(
          sidebarPanel(
            numericInput(
              inputId = ns("decimals_nt"),
              label = "Decimals",
              value = 4,
              min = 0,
              step = 1,
              width = "75px"
            ),
            pickerInput(
              inputId = ns("dist_nt"),
              label = "Select a Distribution",
              choices = c(
                "Normal" = 1,
                "Exponential" = 2,
                "Binomial" = 3,
                "Poisson" = 4,
                "χ²" = 5
              )
            ),
            uiOutput(ns("nt_UI1")),
            uiOutput(ns("nt_UI2"))
          ), # end sidebarpanel
          mainPanel(
            h4("Natural Tolerance"),
            DTOutput(ns("nt_out_simple"))
          ) # end mainpanel
        ) # end sidebarlayout
      ) # end tabpanel
    ) # end tabsetpanel
  )
}