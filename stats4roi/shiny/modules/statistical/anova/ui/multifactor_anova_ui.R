# Multi-Factor ANOVA UI Module
# UI for Multi-Factor ANOVA tab with Set Up and Results sub-tabs

create_multifactor_anova_ui_internal <- function(ns) {
  navlistPanel(
    id = ns("mw_anova"),
    widths = c(2, 10),
    # Set Up Tab
    tabPanel(
      "Set Up",
      value = ns("mw_su"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          HTML("<p style='color:red; text-align:center'><u>Important: Make sure factors are numeric and not set as the factor class.</u></p>"),
          uiOutput(ns("ems_factors")),
          uiOutput(ns("ems_data"))
        ),
        mainPanel(
          width = 9,
          h3("Create Design"),
          checkboxInput(
            inputId = ns("ems_show_mixed_nest"),
            label = "Select random and nested effects?",
            value = FALSE
          ),
          conditionalPanel(
            condition = paste0("input['", ns("ems_show_mixed_nest"), "'] == 1"),
            fluidRow(
              column(6, uiOutput(ns("ems_rand_select"))),
              column(6, uiOutput(ns("ems_nest_select")))
            )
          ),
          fluidRow(
            column(6,
              uiOutput(ns("ems_pool")),
              uiOutput(ns("ems_primary"))
            ),
            column(6,
              uiOutput(ns("ems_ems_a"))
            )
          )
        )
      )
    ),
    # Results Tab
    tabPanel(
      "Results",
      navlistPanel(
        widths = c(2, 10),
        # ANOVA Sub-tab
        tabPanel(
          "ANOVA",
          mainPanel(
            width = 12,
            fluidRow(
              column(6, numericInput(inputId = ns("ems_conf"), label = "Confidence", value = 0.95, min = 0, max = 1, width = "75px")),
              column(6, numericInput(inputId = ns("ems_dec"), label = "Decimals", value = 4, min = 0, max = 9, width = "75px"))
            ),
            fluidRow(
              h4("Results"),
              fluidRow(
                column(4,
                  tags$div(
                    materialSwitch(inputId = ns("ems_disp"), label = "Means", value = FALSE, inline = TRUE, status = "primary"),
                    tags$span("Dispersion")
                  )
                ),
                column(4,
                  conditionalPanel(
                    condition = paste0("input['", ns("ems_disp"), "'] == 1"),
                    radioGroupButtons(inputId = ns("ems_disp_type"), label = "Dispersion Test", choices = c("ADA" = 1, "ADM" = 2, "ADM(n-1)" = 3))
                  )
                ),
                column(4,
                  checkboxInput(inputId = ns("ems_show_pool"), label = "Show Pooled Effects?", value = FALSE)
                )
              ),
              HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", paste0(
                '<span data-display-if="',
                '$(&#39;html&#39;).attr(&#39;class&#39;)==&#39;shiny-busy&#39;',
                '">',
                '<i class="fa fa-spinner fa-spin-pulse fa-fw" style="color: #337ab7; font-size:50px !important;" ></i>',
                '</span>'
              )),
              conditionalPanel(
                condition = paste0("input['", ns("ems_pool"), "'] == 0"),
                h4("ANOVA Source Table"),
                htmlOutput(ns("ems_table"))
              ),
              conditionalPanel(
                condition = paste0("input['", ns("ems_pool"), "'] != 0"),
                h4("Pooled ANOVA Source Table"),
                htmlOutput(ns("ems_table_pooled"))
              )
            )
          )
        ),
        # Graphs Sub-tab
        tabPanel(
          "Graphs",
          mainPanel(
            width = 12,
            numericInput(inputId = ns("ems_target"), label = "Target", value = NA, width = "100px"),
            plotOutput(outputId = ns("emssigeffects"), width = "auto", height = 400),
            fluidRow(
              column(6, downloadButtonUI(ns("emssigeffects"))),
              column(6, tags$div(id = "inline1", class = "inline", downloadSelectUI(ns("emssigeffects"))))
            ),
            hr(),
            checkboxInput(inputId = ns("ems_show_coeffs"), label = "Show Reduced Model Coefficients?", value = FALSE),
            # DTOutput always present to avoid DT initialization issues when shown/hidden
            DTOutput(ns("multi_coeff_est")),
            hr(),
            checkboxInput(inputId = ns("ems_show_optimum"), label = "Show Settings with Estimated Average Closest to Target?", value = FALSE),
            conditionalPanel(
              condition = paste0("input['", ns("ems_show_optimum"), "'] == 1"),
              tags$div(id = "inline1", class = "inline", numericInput(inputId = ns("multi_response_tol"), label = "Range of Averages Around Target to Display", value = 0)),
              NULL
            ),
            # DTOutput always present to avoid DT initialization issues when shown/hidden
            DTOutput(ns("multi_response_target")),
            hr(),
            checkboxInput(inputId = ns("ems_show_box"), label = "Show Main Effects Boxplots", value = FALSE),
            conditionalPanel(
              condition = paste0("input['", ns("ems_show_box"), "'] == 1"),
              # Match monolithic: allow server-side dynamic height
              plotOutput(ns("emsmaineffects"), width = "auto", height = "auto"),
              fluidRow(
                column(6, downloadButtonUI(ns("emsmaineffects"))),
                column(6, tags$div(id = "inline1", class = "inline", downloadSelectUI(ns("emsmaineffects"))))
              )
            ),
            hr(),
            uiOutput(ns("ems_int_sel")),
            uiOutput(ns("emsinteractions")),
            fluidRow(
              column(6, downloadButton(outputId = ns("ems_interaction_dl"), label = "Download all as zip")),
              column(6, tags$div(id = "inline1", class = "inline",
                pickerInput(inputId = ns("ems_interaction_dl_format"),
                  label = "Format: ",
                  choices = c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"),
                  selected = "svg",
                  width = "75px")
              ))
            )
          )
        ),
        # Post-hocs Sub-tab
        tabPanel(
          "Post-hocs",
          fluidPage(
            pickerInput(inputId = ns("ems_ph_select"),
              label = "Select fixed effect post-hoc",
              choices = list(
                "All pair-wise, equal variance" = c("Tukey" = 1, "Bonferroni Procedure" = 2, "Holm's Method" = 3),
                "All pair-wise, unequal variance" = c("Games-Howell" = 4, "Bonferroni Procedure" = 5, "Holm's Method" = 6)
              ),
              options = list(title = "Select test"),
              selected = NULL
            ),
            uiOutput(ns("ems_ph_selection")),
            fluidRow(
              prettyCheckboxGroup(inputId = ns("ems_ph_plot_options"), label = "Select Options", choices = c("Show Confidence Intervals" = "CIs", "Show Prediction Intervals" = "PIs", "Plot Horizontal?" = "hor"), selected = c("CIs", "PIs"), inline = TRUE, status = "success")
            ),
            plotOutput(ns("emsphplot"), width = "auto", height = 400),
            fluidRow(
              column(4, downloadButtonUI(ns("emsphplot"))),
              column(4, tags$div(id = "inline1", class = "inline", downloadSelectUI(ns("emsphplot")))),
              column(4, tags$div(id = "inline1", class = "inline", numericInput(inputId = ns("ph_font_size"), label = "Base Font Size", value = 11, min = 1, step = 1, width = "75px")))
            ),
            hr(),
            htmlOutput(ns("ems_ph_out"))
          )
        )
      )
    )
  )
}
