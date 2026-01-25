# =========================================================================
# INTERVALS UI MODULE
# =========================================================================
# UI for intervals analysis tab
# Part of EDA module - handles confidence and credible interval calculations

create_intervals_ui <- function(ns) {
  
  tagList(
    h4("Estimate Intervals"),
    sidebarLayout(
      sidebarPanel(
        numericInput(
          inputId = ns("conf_ci"),
          label = "Confidence",
          value = 0.95,
          min = 0,
          max = 1,
          step = 0.05,
          width = "75px"
        ),
        numericInput(
          inputId = ns("decimals_ci"),
          label = "Decimals",
          value = 5,
          min = 0,
          step = 1,
          width = "75px"
        ),
        radioGroupButtons(
          inputId = ns("interval_type"),
          label = "Interval Type",
          choices = c(
            "Confidence" = 1,
            "Credible" = 2
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("interval_type"), "'] == 2"),
          radioGroupButtons(
            inputId = ns("interval_b_type"),
            label = "Credible Interval Type",
            choices = c(
              "Highest Density Interval" = "HDI",
              "Equal-tailed Interval" = "ETI"
            ),
            direction = "vertical"
          )
        ),
        uiOutput(ns("ci_data_list")),
        checkboxInput(
          inputId = ns("ci_info"),
          label = "About CIs",
          value = FALSE
        )
      ), # end sidebarpanel
      mainPanel(
        HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", paste0(#waiting spinner
          '<span data-display-if="',
          '$(&#39;html&#39;).attr(&#39;class&#39;)==&#39;shiny-busy&#39;',
          '">',
          '<i class="fa fa-spinner fa-spin-pulse fa-fw" style="color: #337ab7; font-size:50px !important;" ></i>',
          '</span>'
        )),
        uiOutput(ns("interval_title")),
        DTOutput(ns("ci_out"))
      ) # end mainpanel
    ) # end sidebarlayout
  )
}