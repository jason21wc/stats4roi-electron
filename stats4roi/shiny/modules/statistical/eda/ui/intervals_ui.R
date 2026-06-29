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
          inputId = ns("ci_show_plot"),
          label = "Show interval plot?",
          value = FALSE
        ),
        checkboxInput(
          inputId = ns("ci_info"),
          label = "About CIs",
          value = FALSE
        )
      ), # end sidebarpanel
      mainPanel(
        conditionalPanel(
          condition = paste0("input['", ns("ci_show_plot"), "'] == true"),
          tags$div(
            id = "inline1",
            class = "inline",
            noUiSliderInput(
              inputId = ns("ci_plot_width"),
              label = "Width",
              min = 200,
              max = 1600,
              inline = TRUE,
              width = "300px",
              value = 400,
              step = 100,
              format = wNumbFormat(decimals = 0, suffix = "px")
            )
          ),
          fluidRow(
            column(1,
              noUiSliderInput(
                inputId = ns("ci_plot_height"),
                label = "Height",
                min = 200,
                max = 1600,
                value = 400,
                step = 100,
                orientation = "vertical",
                width = "100px",
                height = "300px",
                format = wNumbFormat(decimals = 0, suffix = "px")
              )
            ),
            column(9,
              div(
                style = "position:relative; display:inline-block; overflow:visible;",
                plotOutput(
                  ns("ci_plot"),
                  width = "auto",
                  height = "auto",
                  hover = hoverOpts(ns("ci_hover"), delay = 100, delayType = "debounce")
                ),
                uiOutput(ns("hover_info_ci"), style = "pointer-events: none;")
              ),
              HTML("<p style='text-align:right;'><small>Mouse over for interval information</small></p>")
            ),
            column(2,
              radioButtons(
                inputId = ns("ci_plot_param"),
                label = "Parameter",
                choices = c("Mean" = "Mean", "SD" = "SD"),
                selected = "Mean"
              )
            )
          ),
          fluidRow(
            column(3,
              dropdown(
                tags$h4("Chart Options"),
                textInput(
                  inputId = ns("ci_plot_title"),
                  label = "Chart Title",
                  value = ""
                ),
                textInput(
                  inputId = ns("ci_plot_xlab"),
                  label = "X-axis Label",
                  value = ""
                ),
                textInput(
                  inputId = ns("ci_plot_ylab"),
                  label = "Y-axis Label",
                  value = ""
                ),
                circle = TRUE,
                status = "success",
                icon = icon("gear"),
                width = "300px",
                tooltip = tooltipOptions(title = "Click to modify chart")
              )
            ),
            column(2,
              numericInput(
                inputId = ns("ci_font_size"),
                label = "Base Font Size",
                value = 11,
                min = 1,
                step = 1,
                width = "75px"
              )
            ),
            column(3,
              downloadButtonUI(ns("ciplot"))
            ),
            column(3,
              tags$div(id = "inline1", class = "inline",
                downloadSelectUI(ns("ciplot"))
              )
            ),
            column(1)
          )
        ),
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
