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
          ),
        )
      )
    ),
    # Results Tab
    tabPanel(
      "Results",
      navlistPanel(
        id = ns("mw_results_nav"),
        widths = c(2, 10),
        # ANOVA Sub-tab
        tabPanel(
          "ANOVA",
          mainPanel(
            width = 12,
            uiOutput(ns("ems_data_active")),
            fluidRow(
              column(6, numericInput(inputId = ns("ems_conf"), label = "Confidence", value = 0.95, min = 0, max = 1, width = "75px")),
              column(6, numericInput(inputId = ns("ems_dec"), label = "Decimals", value = 4, min = 0, max = 9, width = "75px"))
            ),
            fluidRow(
              column(
                12,
                tags$div(
                  style = "display:flex; gap:0.5em; flex-wrap:wrap; margin-bottom:0.6em;",
                  conditionalPanel(
                    condition = paste0("input['", ns("ems_disp"), "'] == 0"),
                    uiOutput(ns("ems_finalize_means_btn"))
                  ),
                  conditionalPanel(
                    condition = paste0("input['", ns("ems_disp"), "'] == 1"),
                    uiOutput(ns("ems_finalize_dispersion_btn"))
                  )
                ),
                NULL
              )
            ),
            fluidRow(
              column(
                12,
                tags$div(
                  style = "display:flex; gap:0.5em; flex-wrap:wrap; align-items:flex-start; margin-bottom:0.35em;",
                  actionButton(ns("ems_recalc_anova"), "Apply pooling & refresh ANOVA", class = "btn-primary btn-sm"),
                  actionButton(ns("ems_reset_effect_model"), "Reset pooling to Set Up model", class = "btn-default btn-sm")
                ),
                htmlOutput(ns("ems_anova_effect_pending"))
              )
            ),
            fluidRow(
              h4("Results"),
              fluidRow(
                column(3,
                  tags$div(
                    materialSwitch(inputId = ns("ems_disp"), label = "Means", value = FALSE, inline = TRUE, status = "primary"),
                    tags$span("Dispersion")
                  )
                ),
                column(3,
                  conditionalPanel(
                    condition = paste0("input['", ns("ems_disp"), "'] == 1"),
                    radioGroupButtons(inputId = ns("ems_disp_type"), label = "Dispersion Test", choices = c("ADA" = 1, "ADM" = 2, "ADM(n-1)" = 3))
                  )
                ),
                column(3,
                  checkboxInput(inputId = ns("ems_show_pool"), label = "Show Pooled Effects?", value = FALSE)
                ),
                column(3,
                  checkboxInput(inputId = ns("ems_show_rfc"), label = "Show %RFC?", value = FALSE)
                )
              ),
            fluidRow(
              column(
                12,
                tags$p(
                  class = "text-muted",
                  style = "font-size:0.88em; margin:0.15em 0 0.35em 0;",
                  "The experiment is defined once in Set Up; refine mean and dispersion models separately for each response here. ",
                  "Reported ANOVA is for analysis; the reduced prediction equations used in Loss may differ after pooling random/block effects for prediction."
                )
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
                condition = paste0("!input['", ns("ems_show_pool"), "']"),
                h4("ANOVA Source Table"),
                htmlOutput(ns("ems_table"))
              ),
              conditionalPanel(
                condition = paste0("input['", ns("ems_show_pool"), "']"),
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
            uiOutput(ns("ems_nested_section")),
            hr(),
            checkboxInput(inputId = ns("ems_show_coeffs"), label = "Show Reduced Model Coefficients?", value = FALSE),
            # DTOutput always present to avoid DT initialization issues when shown/hidden
            DTOutput(ns("multi_coeff_est")),
            hr(),
            uiOutput(ns("ems_graphs_optimum_section")),
            uiOutput(ns("ems_graphs_optimum_details")),
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
    ),
    # Loss / optimization (MVP: continuous normal, grid over ref_grid)
    tabPanel(
      "Loss / optimization",
      value = ns("mw_loss"),
      optimizer_loss_layout_css(),
      div(
        class = "optimizer-loss-layout",
        fluidRow(
          column(
            3,
            class = "optimizer-loss-sidebar",
            tags$p(
              class = "text-muted",
              style = "margin-bottom: 0.5em;",
              tags$strong("Taguchi economics")
            ),
            numericInput(ns("loss_mf_dec"), "Decimals", value = 4, min = 0, max = 9, step = 1, width = "75px"),
            optimizer_blocked_factor_picker(ns, "loss_mf_opt_blocked"),
            selectInput(
              ns("loss_mf_econ_active"),
              "Response to edit",
              choices = c("Select responses on Set Up" = ""),
              selected = ""
            ),
            tags$p(
              class = "text-muted",
              style = "font-size: 0.9em; margin-top: -0.35em; margin-bottom: 0.5em;",
              "Enter economics for each selected response: switch here, edit fields, repeat for all responses, then Calculate."
            ),
            textOutput(ns("loss_mf_econ_active_label")),
            numericInput(ns("loss_mf_lsl"), "LSL", value = NA_real_, width = "100%"),
            numericInput(ns("loss_mf_target"), "Target (T)", value = 0, width = "100%"),
            numericInput(ns("loss_mf_usl"), "USL", value = NA_real_, width = "100%"),
            numericInput(
              ns("loss_mf_C_l"),
              "Lower-side cost at LSL (C_lower)",
              value = 1,
              min = 0,
              step = 0.01,
              width = "100%"
            ),
            numericInput(
              ns("loss_mf_C_u"),
              "Upper-side cost at USL (C_upper)",
              value = 1,
              min = 0,
              step = 0.01,
              width = "100%"
            ),
            tags$div(
              style = "display: none;",
              numericInput(ns("loss_mf_tier3_flag"), label = NULL, value = 0, min = 0, max = 1, step = 1)
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] > 0", ns("loss_mf_tier3_flag")),
              tags$p(
                class = "text-muted",
                style = "margin-top: 0.75em; margin-bottom: 0.35em;",
                optimizer_label_with_help(
                  "Gauge resolution prior (Policy 3)",
                  paste(
                    "Shown only when dispersion EMM and observed cell metrics are unavailable.",
                    "Prefill uses the Normal range bound at Results confidence."
                  )
                )
              ),
              textOutput(ns("loss_mf_disp_delta_context")),
              numericInput(
                ns("loss_mf_disp_delta"),
                "Resolution delta",
                value = 0,
                min = 0,
                step = 0.0001,
                width = "100%"
              ),
              textOutput(ns("loss_mf_disp_delta_recommended"))
            ),
            selectInput(
              ns("loss_mf_opt_target"),
              "Final optimization objective",
              choices = c(
                "Taguchi loss only" = "taguchi_loss",
                "Total Cost (per unit loss + setting cost) \u00d7 volume" = "total_cost"
              ),
              selected = "taguchi_loss"
            ),
            numericInput(ns("loss_mf_opt_volume"), "Production volume", value = 1, min = 1, step = 1, width = "100%"),
            checkboxInput(
              ns("loss_mf_opt_use_factor_costs"),
              label = optimizer_label_with_help(
                "Use factor-level unit costs",
                "A popup cost editor opens when this checkbox is enabled. Continuous factors are interpreted by linear interpolation across entered levels. Discrete factors use only entered level costs."
              ),
              value = FALSE
            ),
            conditionalPanel(
              condition = sprintf("input['%s']", ns("loss_mf_opt_use_factor_costs")),
              textOutput(ns("loss_mf_opt_costs_summary"))
            ),
            optimizer_continuous_factor_picker(ns, "loss_mf_opt_continuous"),
            uiOutput(ns("loss_mf_opt_actual_values_panel")),
            tags$div(
              style = "margin-top: 0.5em;",
              actionButton(ns("loss_mf_opt_run"), "Run Optimizer", class = "btn-primary"),
              optimizer_help_icon(
                paste(
                  "Taguchi loss is computed around the target (not only outside specs).",
                  "With two specs, loss is split lower/upper; with one spec, active-side loss uses full off-target deviation.",
                  "PPM is always calculated from spec-tail probabilities.",
                  "Select blocked factors near the top to drop them from optimization predictions and search (e.g. nuisance blocks).",
                  "Select continuous factors below; unselected non-blocked factors are treated as discrete coded levels.",
                  "Total-cost mode adds unit settings cost multiplied by Production volume.",
                  "Sigma follows the dispersion metric selected on the Results ANOVA tab (Dispersion test analysis).",
                  "With multiple responses, use Response to edit to enter Taguchi economics per response.",
                  sep = " "
                )
              )
            )
          ),
          column(
            12 - 3,
            class = "optimizer-loss-main",
            htmlOutput(ns("loss_mf_msg")),
            htmlOutput(ns("loss_mf_optimization_checklist")),
            tags$div(
              style = "margin: 0.5em 0 1em 0;",
              actionButton(
                ns("loss_mf_calc_run"),
                "Calculate loss grid",
                class = "btn-primary"
              ),
              optimizer_help_icon(
                paste(
                  "Enter Taguchi economics (target, at least one spec limit, and C_l/C_u) for every selected response,",
                  "then calculate once. Policy 3 resolution delta controls appear only when needed after calculation.",
                  sep = " "
                )
              )
            ),
            tags$h4("Top 5 lowest-loss observed discrete settings"),
            tags$p(
              class = "text-muted",
              "This table ranks observed experimental settings by predicted Taguchi losses only. This table does not consider per-unit settings costs, combinations not run, or continuous factors. For that, set up and run the optimizer."
            ),
            tags$p(
              class = "text-muted",
              style = "font-size: 0.92em; margin-top: -0.2em;",
              tags$strong("Dispersion metric for \u03c3:"),
              textOutput(ns("loss_mf_disp_metric_note"), inline = TRUE)
            ),
            uiOutput(ns("loss_mf_dispersion_policy")),
            DTOutput(ns("loss_mf_tab")),
            tags$hr(),
            htmlOutput(ns("loss_mf_opt_msg")),
            htmlOutput(ns("loss_mf_preflight")),
            checkboxInput(ns("loss_mf_opt_show_details"), "Show detailed optimizer output", value = FALSE),
            DTOutput(ns("loss_mf_opt_bounds_tab")),
            DTOutput(ns("loss_mf_opt_tab")),
            uiOutput(ns("loss_mf_opt_dist_cards")),
            tags$hr(),
            tags$h4("Confirmation Experiment Settings"),
            DTOutput(ns("loss_mf_confirmation_tab")),
            downloadButton(ns("loss_mf_confirmation_export"), "Export confirmation settings (CSV)"),
            downloadButton(ns("loss_mf_opt_export"), "Export Full Optimizer Results")
          )
        )
      )
    )
  )
}
