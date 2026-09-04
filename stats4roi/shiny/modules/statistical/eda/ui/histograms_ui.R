# Histograms UI Component
# Following distribution testing pattern

create_histograms_ui <- function(ns) {
  tagList(
    h3("Histograms"),
    sidebarLayout(
      sidebarPanel(
        h4("Histogram Options"),
        # Data selection UI (rendered in main module)
        uiOutput(ns("hist_data_list")),
        # Independent variable list (for factor analysis)
        uiOutput(ns("hist_indep_list")),
        # Multiple data choice
        uiOutput(ns("mult_data")),
        # Histogram type selection
        radioButtons(
          inputId = ns("hist_type"),
          label = "Plot Style:",
          choices = c(
            "Histogram" = 1,
            "Frequency Polygon" = 2,
            "Kernel Density (bw='sj')" = 3
          ),
          selected = 1
        ),
        tags$hr(style="border-color: black;"),
        tags$p(style="font-weight:700;", "Chart Modifications:"),
        # Normal curve option
        checkboxInput(
          inputId = ns("norm_curve"),
          label = "Normal Curve?",
          value = FALSE
        ),
        # Y-axis mode toggle (only for histograms without normal overlay)
        conditionalPanel(
          condition = "input.hist_type == 1 && input.norm_curve == 0",
          ns = ns,
          materialSwitch(
            inputId = ns("hist_freq_y_axis"),
            label = "Frequency (relative) Y-axis?",
            value = FALSE,
            status = "success"
          )
        ),
        # Draw specifications option
        checkboxInput(
          inputId = ns("hist_specs"),
          label = "Draw specs?",
          value = FALSE
        ),
        # Specifications controls (conditional)
        conditionalPanel(
          condition = "input.hist_specs==1",
          ns = ns,
          tags$div(id="inline1", class="inline",
            numericInput(
              inputId = ns("hist_LSL"),
              label = "LSL",
              value = NA,
              width = "75px"
            )
          ),
          tags$div(id="inline1", class="inline",
            numericInput(
              inputId = ns("hist_target"),
              label = "Target",
              value = NA,
              width = "75px"
            )
          ),
          tags$div(id="inline1", class="inline",
            numericInput(
              inputId = ns("hist_USL"),
              label = "USL",
              value = NA,
              width = "75px"
            )
          )
        ),
        # Bin controls (conditional on plot type)
        conditionalPanel(
          condition = "input.hist_type != 3",
          ns = ns,
          fluidRow(
            column(5,
              numericInput(
                inputId = ns("hist_bin_w"),
                label = "Bin Width",
                min = 1,
                value = NULL,
                width = "75px"
              )
            ),
            column(2,
              br(),
              p(style="text-align: center; font-weight: bold;", "OR")
            ),
            column(5,
              numericInput(
                inputId = ns("hist_bins"),
                label = "# of Bins",
                min = 1,
                value = NULL,
                width = "75px"
              )
            )
          ),
          conditionalPanel(
            condition = "input.hist_bin_w > 0 & input.hist_bins > 0",
            ns = ns,
            p(style="text-align: center; color: red;", "Bin width overrides selecting the number of bins.")
          ),
          numericInput(
            inputId = ns("hist_center"),
            label = "Center of a Bin",
            min = 0,
            value = NULL,
            width = "150px"
          )
        ),
        # Density-specific controls (conditional)
        conditionalPanel(
          condition = "input.hist_type==3",
          ns = ns,
          checkboxInput(
            inputId = ns("hist_extend_d"),
            label = "Extend Density?",
            value = FALSE
          ),
          checkboxInput(
            inputId = ns("hist_rug"),
            label = "Add Rug Plot?",
            value = TRUE
          )
        ),
      ),
      mainPanel(
        # Plot controls
        tags$div(
          id = "inline1", 
          class = "inline",
          noUiSliderInput(
            inputId = ns("hist_width"),
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
              inputId = ns("hist_height"),
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
          column(11,
            plotOutput(ns("hist_plot"), width = "auto", height = "auto")
          )
        ),
        fluidRow(
          column(3,
            dropdown(
              tags$h4("Chart Options"),
              textInput(inputId = ns("hist_title"), label = "Chart Title", value = NA),
              textInput(inputId = ns("hist_x_lab"), label = "X-axis Label", value = NA),
              checkboxInput(inputId = ns("hist_big"), label = "Larger font?", value = FALSE),
              circle = TRUE, 
              status = "success", 
              icon = icon("gear"), 
              width = "300px", 
              tooltip = tooltipOptions(title = "Click to modify chart")
            )
          ),
          column(3,
            downloadButtonUI(ns("histogram"))
          ),
          column(3,
            tags$div(id = "inline1", class = "inline", downloadSelectUI(ns("histogram")))
          )
        ),
        # Frequency distribution toggle (conditional)
        conditionalPanel(
          condition = "input.hist_type != 3",
          ns = ns,
          materialSwitch(
            inputId = ns("hist_freq_dist"),
            label = "Show frequency distribution?",
            value = FALSE,
            status = "success"
          )
        ),
        # Frequency distribution table (conditional)
        conditionalPanel(
          condition = "input.hist_freq_dist == true",
          ns = ns,
          h4("Frequency Distribution Table"),
          numericInput(
            inputId = ns("freq_dist_dec"),
            label = "Decimals",
            value = 5,
            min = 0,
            max = 9,
            step = 1,
            width = "75px"
          ),
          uiOutput(ns("hist_panel_select")),
          DTOutput(ns("hist_freq_table"))
        )
      )
    )
  )
}
