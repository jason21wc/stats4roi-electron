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
            condition = "parseInt(input.type_ow, 10) != 3 && input.ow_disp_analysis != true",
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
          fluidRow(
            column(
              12,
              conditionalPanel(
                condition = "parseInt(input.type_ow, 10) != 3",
                ns = ns,
                tags$div(
                  materialSwitch(
                    inputId = ns("ow_disp_analysis"),
                    label = "Means",
                    value = FALSE,
                    inline = TRUE,
                    status = "primary"
                  ),
                  tags$span("Dispersion")
                )
              )
            )),
            fluidRow(
            column(
              12,
              conditionalPanel(
                condition = "parseInt(input.type_ow, 10) != 3 && (input.ow_disp_analysis == true || input.ow_disp_analysis == 1)",
                ns = ns,
                radioGroupButtons(
                  inputId = ns("ow_disp_type"),
                  label = "Dispersion Test",
                  choices = c("ADA" = 1, "ADM" = 2, "ADM(n-1)" = 3),
                  selected = 1
                )
              )
            )
          ),
          htmlOutput(ns("ow_table")),
          # Always render DT table - server returns empty table when not Kruskal-Wallis
          # This avoids DT initialization timing issues with conditional rendering
          DTOutput(ns("KW_out")),
          conditionalPanel(
            condition = "parseInt(input.type_ow, 10) != 3 && input.disp_ow == true && input.ow_disp_analysis != true",
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
            ),
            checkboxInput(
              inputId = ns("ow_ph_homogeneous"),
              label = "Show homogeneous subsets?",
              value = FALSE
            )
          )
        ),
        mainPanel(
          h4("Oneway ANOVA Post-hoc"),
          h5("Confidence set on Oneway ANOVA tab"),
          tags$p(tags$em("Omnibus and post-hoc follow settings on the Oneway ANOVA tab.")),
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
          conditionalPanel(
            condition = "input.type_ow != 2 && input.ow_ph_homogeneous == true",
            ns = ns,
            DTOutput(ns("ow_ph_homogeneous"))
          ),
          verbatimTextOutput(ns("ow_ph_details"))
        )
      )
    ),
    # Loss / optimization — must be a direct navlistPanel child (not wrapped in conditionalPanel).
    tabPanel(
      "Loss / optimization",
      optimizer_loss_layout_css(),
      div(
        class = "optimizer-loss-layout",
        fluidRow(
          column(
            3,
            class = "optimizer-loss-sidebar",
            numericInput(ns("loss_ow_target"), "Target (T)", value = 0, width = "100%"),
            numericInput(ns("loss_ow_dec"), "Decimals", value = 4, min = 0, max = 9, step = 1, width = "75px"),
            numericInput(ns("loss_ow_C_l"), "Lower-side cost at LSL (C_lower)", value = 1, min = 0, step = 0.01, width = "100%"),
            numericInput(ns("loss_ow_C_u"), "Upper-side cost at USL (C_upper)", value = 1, min = 0, step = 0.01, width = "100%"),
            numericInput(ns("loss_ow_lsl"), "LSL", value = NA, width = "100%"),
            numericInput(ns("loss_ow_usl"), "USL", value = NA, width = "100%"),
            uiOutput(ns("loss_ow_resolution_prior_panel")),
            selectInput(
              ns("loss_ow_opt_target"),
              "Final optimization objective",
              choices = c(
                "Taguchi loss only" = "taguchi_loss",
                "Total Cost (per unit loss + setting cost) \u00d7 volume" = "total_cost"
              ),
              selected = "taguchi_loss"
            ),
            numericInput(ns("loss_ow_opt_volume"), "Production volume", value = 1, min = 1, step = 1, width = "100%"),
            checkboxInput(
              ns("loss_ow_opt_use_factor_costs"),
              label = optimizer_label_with_help(
                "Use factor-level unit costs",
                "A popup cost editor opens when this checkbox is enabled. Continuous factors are interpreted by linear interpolation across entered levels. Discrete factors use only entered level costs."
              ),
              value = FALSE
            ),
            conditionalPanel(
              condition = sprintf("input['%s']", ns("loss_ow_opt_use_factor_costs")),
              textOutput(ns("loss_ow_opt_costs_summary"))
            ),
            optimizer_continuous_factor_picker(ns, "loss_ow_opt_continuous"),
            tags$div(
              style = "margin-top: 0.5em;",
              actionButton(ns("loss_ow_opt_run"), "Run Optimizer", class = "btn-primary"),
              optimizer_help_icon(
                paste(
                  "Taguchi loss is computed around the target (not only outside specs).",
                  "With two specs, loss is split lower/upper; with one spec, active-side loss uses full off-target deviation.",
                  "PPM is always calculated from spec-tail probabilities.",
                  "Select continuous factors above; unselected factors are treated as discrete coded levels.",
                  "Total-cost mode adds unit settings cost multiplied by Production volume.",
                  "Sigma follows the dispersion metric selected on the Oneway ANOVA tab (Dispersion test analysis).",
                  sep = " "
                )
              )
            )
          ),
          column(
            12 - 3,
            class = "optimizer-loss-main",
            conditionalPanel(
              condition = "parseInt(input.type_ow, 10) != 1 && parseInt(input.type_ow, 10) != 4",
              ns = ns,
              tags$p(
                class = "text-warning",
                "Loss / optimization is available only for fixed-effects Oneway ANOVA (Fisher or Welch). Select one of those test types on the ANOVA tab."
              )
            ),
            htmlOutput(ns("loss_ow_msg")),
            tags$h4("Top 5 lowest-loss observed discrete settings"),
            tags$p(
              class = "text-muted",
              "This table ranks only observed experimental settings by total Taguchi loss per unit."
            ),
            tags$p(
              class = "text-muted",
              style = "font-size: 0.92em; margin-top: -0.2em;",
              tags$strong("Dispersion metric for \u03c3:"),
              textOutput(ns("loss_ow_disp_metric_note"), inline = TRUE)
            ),
            uiOutput(ns("loss_ow_dispersion_policy")),
            DTOutput(ns("loss_ow_tab")),
            tags$hr(),
            htmlOutput(ns("loss_ow_opt_msg")),
            checkboxInput(ns("loss_ow_opt_show_details"), "Show detailed optimizer output", value = FALSE),
            DTOutput(ns("loss_ow_opt_bounds_tab")),
            DTOutput(ns("loss_ow_opt_tab")),
            uiOutput(ns("loss_ow_opt_dist_cards")),
            downloadButton(ns("loss_ow_opt_export"), "Export optimizer result (CSV)")
          )
        )
      )
    )
  )
}
