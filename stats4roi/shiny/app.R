# stats4ROI_mod.R - Modular Application
# This is the main application file that uses the modular components
# Original app.R (40,000+ lines) is preserved for reference

# Core packages only at startup. Specialty packages (tidyverse, propagate, emmeans,
# nlme, car, bayestestR, bayesboot, agop, ggh4x) load inside the modules that need them.
library(shiny)
library(DT)
library(datamods)
library(dplyr)
library(readr)
library(ggplot2)
library(lolcat)
source("modules/statistical/spc/utils/spc_zones_classify_override.R")
library(shinyWidgets)
library(svglite)
library(rhandsontable)

# Source global configuration and deferred-loading infrastructure first
source("modules/config/global_config.R")
source("modules/config/global_data_invalidation.R")
source("modules/config/architectural_patterns.R")
source("modules/config/deferred_module_loader.R")
# Keep deployment file discovery in sync with deferred module paths
deferred_deployment_source_manifest()

# Eager: data pipeline (needed for File menu and all analyses)
source("modules/data/data_import_module.R")
source("modules/data/data_modification_module.R")
source("modules/data/working_data_module.R")
source("modules/data/dynamic_filtering_module.R")
source("modules/data/data_transformation_module.R")

# Feature modules (Distributions, Reliability, EDA, SPC, ANOVA, …) are sourced
# on first navbar visit or via idle prefetch — see deferred_module_loader.R

# UI
ui <- fluidPage(
  # Suppress synchronous XMLHttpRequest warnings
  tags$head(
    tags$script(HTML("
      // Suppress synchronous XMLHttpRequest warnings
      (function() {
        // Override console.warn to filter out XMLHttpRequest warnings
        var originalWarn = console.warn;
        console.warn = function() {
          var args = Array.prototype.slice.call(arguments);
          var message = args.join(' ');
          if (message.indexOf('Synchronous XMLHttpRequest') === -1) {
            originalWarn.apply(console, args);
          }
        };
        
        // Also override XMLHttpRequest.open to force async
        var originalOpen = XMLHttpRequest.prototype.open;
        XMLHttpRequest.prototype.open = function(method, url, async, user, password) {
          if (async === false) {
            async = true;
          }
          return originalOpen.apply(this, arguments);
        };
        
        // Defensive JavaScript for ShinyWidgets and DataTables
        (function() {
          // Override console.error to catch and ignore ShinyWidgets and DataTables errors
          var originalError = console.error;
          console.error = function() {
            var args = Array.prototype.slice.call(arguments);
            var message = args.join(' ');
            if (message.indexOf('can\\'t access property') === -1 && 
                message.indexOf('$table.DataTable is not a function') === -1) {
              originalError.apply(console, args);
            }
          };
          
          // Add global error handler for ShinyWidgets and DataTables
          window.addEventListener('error', function(e) {
            if (e.message && (e.message.indexOf('can\\'t access property') !== -1 || 
                              e.message.indexOf('$table.DataTable is not a function') !== -1)) {
              e.preventDefault();
              console.log('ShinyWidgets/DataTables error caught and ignored:', e.message);
              return false;
            }
          });
          
          // DataTables defensive initialization
          $(document).ready(function() {
            // Wait for DataTables to be available and override renderValue
            var checkDataTables = function() {
              if (typeof $.fn.DataTable !== 'undefined') {
                console.log('DataTables library loaded successfully');
                
                // Override HTMLWidgets datatables renderValue to handle timing issues
                if (typeof HTMLWidgets !== 'undefined' && HTMLWidgets.widgets) {
                  var dtWidget = HTMLWidgets.widgets['datatables'];
                  if (dtWidget) {
                    var originalRenderValue = dtWidget.renderValue;
                    dtWidget.renderValue = function(el, x, instance) {
                      try {
                        // Ensure DataTables is fully loaded
                        if (typeof $.fn.DataTable === 'undefined') {
                          console.log('DataTables not ready, retrying in 100ms');
                          setTimeout(function() {
                            dtWidget.renderValue(el, x, instance);
                          }, 100);
                          return;
                        }
                        originalRenderValue.call(this, el, x, instance);
                      } catch (e) {
                        console.log('DataTables render error caught and ignored:', e.message);
                        // Try again after a short delay
                        setTimeout(function() {
                          try {
                            originalRenderValue.call(this, el, x, instance);
                          } catch (e2) {
                            console.log('DataTables retry failed, ignoring:', e2.message);
                          }
                        }, 200);
                      }
                    };
                  }
                }
              } else {
                // DataTables not ready yet, check again
                setTimeout(checkDataTables, 50);
              }
            };
            checkDataTables();
          });
          // Redraw DataTables when their tab is shown (fixes rows not drawn when table initializes in hidden tab)
          $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"], a[data-bs-toggle=\"tab\"]', function() {
            var href = $(this).attr('href');
            if (!href || href.indexOf('#') === -1) return;
            var $pane = $(href);
            if (!$pane.length) return;
            setTimeout(function() {
              var tableIds = ['w_data', 'dynamic_filtering-table'];
              tableIds.forEach(function(id) {
                if ($pane.find('#' + id).length === 0) return;
                var $container = $('#' + id);
                var $table = $container.find('table');
                if ($table.length && $.fn.DataTable && $table.DataTable) {
                  try {
                    var api = $table.DataTable();
                    if (api && api.columns) {
                      api.columns.adjust().draw(false);
                    }
                  } catch (e) {}
                }
              });
            }, 100);
          });
        })();
      })();
    ")),
    # MathJax support
    withMathJax(),
    # MathJax configuration for inline math
    tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$']]}
            });
            </script >
            ")),
    # CSS for inline form layout
    tags$head(tags$style(HTML("div#inlin label { width: 15%; }
                               div#inlin input { display: inline-block; width: 85%;}"))),
    tags$head(
      tags$style(type="text/css", "#inlin label{ display: table-cell; text-align: left; vertical-align: middle; }
                                   #inlin .form-group { display: table-row;}")
    ),
    # CSS for .inline class - use class attribute (.inline instead of #inline)
    tags$head(
      tags$style(type="text/css", ".inline label{ display: table-cell; text-align: left !important; vertical-align: middle; }
                                   .inline .form-group { display: table-row;}")
    ),
    # Sample Size/Power: fixed input width (label table-cell layout otherwise sizes inputs by label length)
    tags$head(
      tags$style(type="text/css", "
        .sample-size-param.inline label,
        .inline.sample-size-param label {
          display: table-cell;
          text-align: left !important;
          vertical-align: middle;
          min-width: 88px;
          padding-right: 8px;
          white-space: nowrap;
        }
        .sample-size-param.inline .form-group,
        .inline.sample-size-param .form-group {
          display: table-row;
        }
        .sample-size-param .shiny-input-container {
          display: table-cell;
          width: 150px !important;
          min-width: 150px !important;
          max-width: 150px !important;
          vertical-align: middle;
        }
        .sample-size-param input[type='number'] {
          width: 150px !important;
          min-width: 150px !important;
          max-width: 150px !important;
          box-sizing: border-box;
        }
        .sample-size-input-row > [class*='col-'] {
          margin-bottom: 8px;
        }
        @media (max-width: 992px) {
          .sample-size-input-row > [class*='col-'] {
            width: 100%;
            float: none;
          }
        }
      ")
    ),
    # CSS for tables (match monolithic app defaults)
    tags$head(tags$style(HTML("
                            th,td {padding: 2px !important;
                                    min-width:60px;}
                            td {text-align: right;}
                            th {text-align: center;
                                    border-bottom: 1px solid black;}
                            table.dataTable.eda-table thead th,
                            table.dataTable.eda-table thead td {
                              text-align: center !important;
                            }
                            table.dataTable.eda-table tbody td {
                              text-align: right !important;
                            }
                            table {border-collapse: separate !important;}
                            
                            "))),
    tags$head(tags$style(HTML("
      #stats4roi-init-overlay {
        position: fixed;
        inset: 0;
        z-index: 200000;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        gap: 1.25rem;
        background: rgba(248, 249, 250, 0.92);
        backdrop-filter: blur(2px);
        -webkit-backdrop-filter: blur(2px);
        pointer-events: all;
      }
      #stats4roi-init-overlay .stats4roi-init-spinner {
        width: 3rem;
        height: 3rem;
        border: 0.35rem solid rgba(13, 110, 253, 0.2);
        border-top-color: #0d6efd;
        border-radius: 50%;
        animation: stats4roi-spin 0.85s linear infinite;
      }
      @keyframes stats4roi-spin {
        to { transform: rotate(360deg); }
      }
      #stats4roi-init-overlay .stats4roi-init-text {
        margin: 0;
        font-size: 1.15rem;
        font-weight: 600;
        color: #212529;
      }
    "))),
    tags$head(tags$script(HTML("
      (function() {
        function hideStats4roiInitOverlay() {
          var el = document.getElementById('stats4roi-init-overlay');
          if (!el || el.getAttribute('data-hidden') === '1') return;
          el.setAttribute('data-hidden', '1');
          el.style.opacity = '0';
          el.style.transition = 'opacity 0.25s ease';
          setTimeout(function() {
            if (el.parentNode) el.parentNode.removeChild(el);
          }, 280);
        }
        function bindHide() {
          if (typeof jQuery === 'undefined') {
            setTimeout(bindHide, 30);
            return;
          }
          var $ = jQuery;
          if ($(document).data('stats4roi-init-bound')) return;
          $(document).data('stats4roi-init-bound', true);
          $(document).on('shiny:connected', function() {
            $(document).one('shiny:idle', function() {
              requestAnimationFrame(function() {
                requestAnimationFrame(hideStats4roiInitOverlay);
              });
            });
          });
          setTimeout(hideStats4roiInitOverlay, 120000);
        }
        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', bindHide);
        } else {
          bindHide();
        }
      })();
    ")))
  ),
  useSweetAlert(),
  
  titlePanel(title = div(img(src = "roi-stat.svg", width = "40px"), "stats4ROI v4.36"), windowTitle = "stats4ROI"),
  navbarPage(
    id = "main_navbar",
    title = NULL,
    # Welcome Page - First tabPanel is the default
    tabPanel(
      title = "Welcome to stats4ROI!",
      value = "welcome",
      tags$img(src = "roi-stat.svg", width = "200px", style = "float:left; margin:10px"),
      h2("Welcome to stats4ROI!"),
      HTML("<a href='https://www.roi-ally.com' target='_blank'>The ROI Alliance</a> provides this friendly graphic front end to give access to the powerful statistical program R without having to know a line of code and with the assurance that the packages used work correctly. We hope this means that you can spend time learning when and how to use statistics rather than how to script in R. Don't let that stop you from learning R when you are ready!"),
      br(),
      br(),
      HTML("stats4ROI author Steve Ouellette has a Master's of Engineering, has been a Certified Management Consultant<sup>\U24C7</sup> and has been consulting since 1996. He has taught basic and advanced statistics and management principles at the University of Colorado Boulder."),
      br(),
      br(),
      HTML("<a href='https://six-sigma-online.com/' target='_blank'>Put your statistics to work with Six Sigma Black Belt training.</a>"),
      br(),
      HTML("<a href='https://www.roi-ally.com/index.php/en/contact-us' target='_blank'>Contact us</a> for management or statistical training and consulting."),
      HTML("<br><br>stats4ROI is absolutely free, but if you want to buy us a beverage, <a href='https://www.roi-ally.com/index.php/en/services/how-we-can-help/roistat-software'>click the <b>Donate</b> button on the home page.</a> We appreciate it!"),
      HTML("<a href='https://www.amazon.com/dp/B0CPNQ566Y' target='_blank'><img src='3d_ddl_cover_s.png' style='float:right'></a>"),
      HTML("<a href='https://www.amazon.com/dp/B08T48JCZC' target='_blank'><img src='book_cover_perspective.png' style='float:right;height:126px'></a>"),
      hr(),
      h3("stats4ROI News"),
      HTML("<a href='https://www.roi-ally.com/index.php/en/roistat-blog-feed' target='_blank'>Click here to see the current version of stats4ROI!</a>")
    ),
    navbarMenu(
      title = "Concept Simulators",
      deferred_tab_panel("Central Limit Theorem", "clt_sim", "deferred_ui_clt_sim"),
      deferred_tab_panel("Power & Sample Size", "power_sim", "deferred_ui_power_sim"),
      deferred_tab_panel("How ANOVA Works", "anova_sim", "deferred_ui_anova_sim")
    ),
    navbarMenu(
      title = "File",
      tabPanel(
        title = "Import Data",
        h2("Import Data"),
        fluidRow(
          column(
            width = 4,
            checkboxGroupInput(
              inputId = "from",
              label = "Select where the data file is located then click \"Open Import Window\" below:",
              choices = c("file", "copypaste", "googlesheets", "url", "dropbox", "onedrive"),
              selected = c("file", "copypaste")
            ),
            actionButton("launch_modal", "Open import window"),
            p("Your original file is only accessed here. Nothing is changed in the original file.")
          ),
          column(
            width = 8,
            tags$b("Imported data:"),
            verbatimTextOutput(outputId = "name"),
            verbatimTextOutput(outputId = "data")
          )
        )
      ),
      tabPanel(
        title = "Select, Rename, and Convert Variables",
        h3("Select, rename and convert variables"),
        fluidRow(
          column(
            width = 6,
            update_variables_ui("vars")
          ),
          column(
            width = 6,
            tags$b("Original data:"),
            verbatimTextOutput("original"),
            verbatimTextOutput("original_str"),
            tags$b("Modified data:"),
            verbatimTextOutput("modified"),
            verbatimTextOutput("modified_str")
          )
        )
      ),
      tabPanel(
        title = "Transform Data",
        create_data_transformation_ui("transform")
      ),
      tabPanel(
        title = "Dynamically Filter Data",
        create_dynamic_filtering_ui("dynamic_filtering")
      ),
      tabPanel(
        title = "Current Working Data",
        h3("Current Working Data"),
        p("This is the current table used by statistical analysis modules: imported data plus any transforms or randomly generated columns (after dynamic filters, when applied). Download saves this same table as CSV."),
        downloadButton(outputId = "download_working_data", label = "Download current data (CSV)"),
        br(), br(),
        DT::dataTableOutput(outputId = "w_data")
      )
    ),
    navbarMenu(
      title = "Edit",
      tabPanel(
        title = "Settings",
        h3("Settings"),
        selectInput(
          inputId = "color_pal",
          label = "Select a color palette",
          choices = c("R4",
                      "ggplot2",
                      "Accent",
                      "Okabe-Ito",
                      "Dark 2",
                      "Paired",
                      "Pastel 1",
                      "Pastel 2",
                      "Set 1",
                      "Set 2",
                      "Set 3",
                      "Tableau 10",
                      "Classic Tableau",
                      "Polychrome 36",
                      "Alphabet"),
          selected = "R4"
        ),
        HTML("From <a href='https://developer.r-project.org/Blog/public/2019/11/21/a-new-palette-for-r/'><b>A New palette() for R</b> (2019) by Achim Zeileis, Paul Murrell, Martin Maechler, Deepayan Sarkar:</a>  <ul>
<li><code> 'R4 '</code> is the new default palette (same as <code> 'default '</code>, starting from R version 4.0.0).</li>
<li><code> 'Okabe-Ito '</code> is a well-established palette introduced by
<a href= 'http://jfly.iam.u-tokyo.ac.jp/color/ '>Masataka Okabe &amp; Kei Ito</a> that is well-suited for
color vision deficiencies.</li>
<li><code> 'Accent '</code>, <code> 'Dark 2 '</code>, <code> 'Paired '</code>, <code> 'Pastel 1 '</code>, <code> 'Pastel 2 '</code>,
<code> 'Set 1 '</code>, <code> 'Set 2 '</code>, and <code> 'Set 3 '</code> are all palettes from the
popular <a href= 'http://ColorBrewer2.org/ '>ColorBrewer</a> color sets for cartography
(by Mark A. Harrower &amp; Cynthia A. Brewer).</li>
<li><code> 'ggplot2 '</code> is based on the default (hue-based) color scale introduced by Hadley Wickham
in <a href= 'https://CRAN.R-project.org/package=ggplot2 '>ggplot2</a>.</li>
<li><code> 'Tableau 10 '</code> and <code> 'Classic Tableau '</code> are default palettes (by Maureen Stone &amp; Cristy Miller)
from the popular <a href= 'https://www.tableau.com/about/blog/2016/7/colors-upgrade-tableau-10-56782 '>Tableau</a> visualization software.</li>
<li><code> 'Polychrome 36 '</code> and <code> 'Alphabet '</code> are large sets of distinguishable
colors from the <a href= 'https://CRAN.R-project.org/package=Polychrome '>Polychrome</a> package
(by Kevin R. Coombes &amp; Guy Brock).</li>
</ul>"),
        plotOutput("swatch")
      )
    ),
    navbarMenu(
      title = "Distributions",
      deferred_tab_panel("Binomial", "binomial", "deferred_ui_binomial"),
      deferred_tab_panel("Normal", "normal", "deferred_ui_normal"),
      deferred_tab_panel("Poisson", "poisson", "deferred_ui_poisson"),
      deferred_tab_panel("Hypergeometric", "hypergeometric", "deferred_ui_hypergeometric"),
      deferred_tab_panel("Geometric", "geometric", "deferred_ui_geometric"),
      deferred_tab_panel("Exponential", "exponential", "deferred_ui_exponential"),
      deferred_tab_panel("Weibull", "weibull", "deferred_ui_weibull"),
      deferred_tab_panel("F Distribution", "f_distribution", "deferred_ui_f_distribution"),
      deferred_tab_panel("Critical Values", "critical_values", "deferred_ui_critical_values"),
      deferred_tab_panel("Distribution Testing", "distribution_testing", "deferred_ui_distribution_testing")
    ),
    navbarMenu(
      title = "Reliability",
      deferred_tab_panel("Reliability Calculator", "reliability_calc", "deferred_ui_reliability_calc"),
      deferred_tab_panel("Growth Analysis", "growth_analysis", "deferred_ui_growth_analysis"),
      deferred_tab_panel("Weibull Analysis", "weibull_analysis", "deferred_ui_weibull_analysis")
    ),
    deferred_tab_panel("Sample Size/Power", "sample_size_power", "deferred_ui_sample_size_power"),
    deferred_tab_panel("EDA", "eda", "deferred_ui_eda"),
    navbarMenu(
      title = "One- and Two-Sample Tests",
      deferred_tab_panel("Test Reference", "ots_test_reference", "deferred_ui_ots_test_reference"),
      deferred_tab_panel("Means and Dispersion", "ots_means_dispersion", "deferred_ui_ots_means_dispersion"),
      deferred_tab_panel("Proportion", "ots_proportion", "deferred_ui_ots_proportion"),
      deferred_tab_panel("Poisson", "ots_poisson", "deferred_ui_ots_poisson"),
      deferred_tab_panel("Nonparametric", "ots_nonparametric", "deferred_ui_ots_nonparametric")
    ),
    deferred_tab_panel("Correlation and Association", "correlation_association", "deferred_ui_correlation_association"),
    deferred_tab_panel("Autocorrelation", "autocorrelation", "deferred_ui_autocorrelation"),
    deferred_tab_panel("SPC/Capability", "spc", "deferred_ui_spc"),
    navbarMenu(
      title = "MSA",
      deferred_tab_panel("Continuous", "msa_continuous", "deferred_ui_msa_continuous"),
      deferred_tab_panel("Discrete", "msa_discrete", "deferred_ui_msa_discrete")
    ),
    deferred_tab_panel("Crosstabs", "crosstabs", "deferred_ui_crosstabs"),
    navbarMenu(
      title = "ANOVA",
      deferred_tab_panel("Oneway", "anova_oneway", "deferred_ui_anova_oneway"),
      deferred_tab_panel("Multi-Factor", "anova_multifactor", "deferred_ui_anova_multifactor")
    ),
    deferred_tab_panel("DOE Screening Design", "doe_orthogonal", "deferred_ui_doe_orthogonal")
  ),
  tags$div(
    id = "stats4roi-init-overlay",
    role = "status",
    `aria-live` = "polite",
    `aria-busy` = "true",
    tags$div(class = "stats4roi-init-spinner"),
    tags$p("Initializing Program", class = "stats4roi-init-text")
  )
)

# Server
server <- function(input, output, session) {
  # Fresh per-session deferred server/UI flags (source cache stays process-wide).
  if (exists("reset_deferred_session_state", mode = "function")) {
    reset_deferred_session_state()
  }

  session$onSessionEnded(function() {
    stopApp()
    # Quit this R process when: non-interactive (Electron/portable/batch), or when we are
    # RStudio's background "Run App" process (rstudioapi not available = no IDE in this process).
    # That way the background R session exits and does not stay as a zombie (~600 MB).
    quit_background <- !interactive() ||
      (requireNamespace("rstudioapi", quietly = TRUE) && !rstudioapi::isAvailable())
    if (quit_background) {
      q("no")
    }
  })

  # Global settings are now handled by global_config.R
  # No need to duplicate here - they're already set when the config is sourced

  # Data Import (replicating app.R lines 3779-3815)
  # Import modal (replicating app.R lines 3779-3786)
  observeEvent(input$launch_modal, {
    req(input$from)
    import_modal(
      id = "modal_import",
      from = input$from,
      title = "Import data to be used in application"
    )
  })
  
  # Import server (replicating app.R lines 3788-3795)
  imported <- import_server("modal_import",
                            return_class = "data.frame",
                            read_fns = list(
                              dat = function(file) {
                                readr::read_delim(file = file)
                              }
                            )
  )
  
  # Import outputs (replicating app.R lines 3797-3815)
  output$name <- renderPrint({
    req(imported$name())
    imported$name()
  })
  
  output$data <- renderPrint({
    req(imported$data())
    imported$data()
  })
  
  # Data modification (replicating app.R lines 3836-3853)
  updated_data <- update_variables_server(
    id = "vars",
    data = reactive({
      req(imported$data())
      data <- imported$data()
      # Ensure data is a data.frame
      if (!is.data.frame(data)) {
        return(NULL)
      }
      # Ensure data has proper column names and types for datamods
      if (ncol(data) == 0) {
        return(NULL)
      }
      # Convert any character columns to factors if needed for better type conversion
      for (i in 1:ncol(data)) {
        if (is.character(data[[i]])) {
          data[[i]] <- as.factor(data[[i]])
        }
      }
      data
    })
  )
  
  # Modification outputs (replicating app.R lines 3841-3853)
  output$original <- renderPrint({
    req(imported$data())
    imported$data()
  })
  
  output$original_str <- renderPrint({
    req(imported$data())
    str(imported$data())
  })
  
  output$modified <- renderPrint({
    req(updated_data())
    updated_data()
  })
  
  output$modified_str <- renderPrint({
    req(updated_data())
    str(updated_data())
  })
  
  
  # Working data (replicating app.R lines 3817-3831)
  # Create a signal for new data imports
  new_data_signal <- reactive({
    data <- imported$data()
    req(data)
    digest::digest(data)
  })
  
  working_data_result <- create_working_data_server("working_data", reactive(imported$data()), reactive(updated_data()), new_data_signal)

  # Data transformation module - adds transform columns to working data (pass-through when none applied)
  transform_result <- create_data_transformation_server("transform", working_data_result$data)

  # Dynamic filtering module - receives transformed data
  dynamic_filtering_result <- create_dynamic_filtering_server("dynamic_filtering", transform_result$data)
  
  # Current working data output (same pattern as deployment: static output + renderDT with data)
  # Shows the same data used by analyses (filtered + transformed / generated)
  output$w_data <- DT::renderDT({
    df <- safe_filtered_data()
    if (is.null(df) || !is.data.frame(df)) {
      return(data.frame())
    }
    df
  }, options = list(lengthMenu = c(5, 10, 50), pageLength = 10))

  # Safe filtered data reactive (filtered data when available, else transformed/generated)
  # datamods filter may return an empty 0-col frame before any data exists — prefer transform then.
  safe_filtered_data <- reactive({
    transformed <- transform_result$data()
    filtered <- tryCatch({
      dynamic_filtering_result$filtered()
    }, error = function(e) NULL)
    if (!is.null(filtered) && is.data.frame(filtered) && ncol(filtered) > 0L) {
      return(filtered)
    }
    transformed
  })

  # Download current data (transformed + filtered) as CSV
  output$download_working_data <- downloadHandler(
    filename = function() {
      paste0("stats4ROI_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      df <- safe_filtered_data()
      if (is.null(df) || !is.data.frame(df) || (nrow(df) == 0L && ncol(df) == 0L)) {
        readr::write_csv(data.frame(), file)
      } else {
        readr::write_csv(df, file)
      }
    }
  )
  
  # Working data for global invalidation (analyses use safe_filtered_data)
  working_data <- reactive({
    working_data_result$data()
  })

  # Set up global data invalidation system
  # This ensures ALL data-driven UI elements are reset when new data is loaded
  setup_global_data_invalidation(session, working_data)

  # Color palette for modules (Settings tab)
  reactive_color_palette <- reactive({
    pal <- input$color_pal
    if (is.null(pal)) pal <- "R4"  # Default if not yet selected
    get_color_palette(pal)
  })

  deferred_deps <- list(
    filtered_data = safe_filtered_data,
    color_palette = reactive_color_palette
  )

  # Activate feature modules on first navbar visit (source + server + UI)
  observeEvent(input$main_navbar, {
    activate_deferred_tab(input$main_navbar, output, deferred_deps, session)
  }, ignoreNULL = TRUE)

  # After first flush, prefetch deferred module sources in the background
  session$onFlushed(function() {
    prefetch_deferred_modules(session)
  }, once = TRUE)

  # Color swatch plot (Settings tab demo)
  output$swatch <- renderPlot({
    pal <- input$color_pal
    if (is.null(pal)) pal <- "R4"

    fake_data <- data.frame(
      x1 = rnorm(50), x2 = rnorm(50), x3 = rnorm(50), x4 = rnorm(50),
      x5 = rnorm(50), x6 = rnorm(50), x7 = rnorm(50), x8 = rnorm(50)
    )

    boxplot(x = fake_data, col = get_color_palette(pal))
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
