# stats4ROI_mod.R - Modular Application
# This is the main application file that uses the modular components
# Original app.R (40,000+ lines) is preserved for reference

library(shiny)
library(DT)
library(datamods)
library(dplyr)
library(readr)
library(ggplot2)
library(lolcat)
source("modules/statistical/spc/utils/spc_zones_classify_override.R")
library(agop)
library(shinyWidgets)
library(ggh4x)
library(emmeans)
library(nlme)
library(tidyverse)
# Shiny-compatible fork required: https://github.com/ProfessorPeregrine/propagate (CRAN version prints messages and fails in modules)
library(propagate)
propagate_desc <- packageDescription("propagate")
if (is.null(propagate_desc$RemoteUsername) || propagate_desc$RemoteUsername != "ProfessorPeregrine") {
  warning(
    "propagate does not appear to be the Shiny fork from ProfessorPeregrine. ",
    "You may see 'predictNLS: Propagating predictor value...' messages and scatterplot fit errors. ",
    "Fix: remove.packages('propagate'); remotes::install_github('ProfessorPeregrine/propagate')"
  )
}
library(svglite)
library(car)
library(bayestestR)
library(bayesboot)
library(rhandsontable)

# Source global configuration first
source("modules/config/global_config.R")
source("modules/config/global_data_invalidation.R")
source("modules/config/architectural_patterns.R")


# Source all modules
source("modules/data/data_import_module.R")
source("modules/data/data_modification_module.R")
# Note: data_filtering_module.R not needed - using datamods package directly
source("modules/data/working_data_module.R")
source("modules/data/dynamic_filtering_module.R")
source("modules/data/data_transformation_module.R")
# Crosstabs module (now modular)
source("modules/statistical/crosstabs/crosstabs_module.R")
# ANOVA module (now modular)
source("modules/statistical/anova/anova_module.R")

# Source distribution modules
source("modules/distributions/base_distribution_module.R")
source("modules/distributions/binomial_module.R")
source("modules/distributions/normal_module.R")
source("modules/distributions/poisson_module.R")
source("modules/distributions/hypergeometric_module.R")
source("modules/distributions/geometric_module.R")
source("modules/distributions/exponential_module.R")
source("modules/distributions/weibull_module.R")
source("modules/distributions/f_distribution_module.R")
source("modules/distributions/critical_values_module.R")

# Source statistical modules
source("modules/distributions/distribution_testing/distribution_testing_module.R")
source("modules/statistical/sample_size_power/sample_size_power_module.R")
source("modules/statistical/eda/eda_module.R")
source("modules/statistical/one_two_sample_tests/one_two_sample_tests_module.R")
source("modules/statistical/correlation_association/correlation_association_module.R")  # Re-adding step by step
source("modules/statistical/autocorrelation/autocorrelation_module.R")
# SPC module (to be implemented for parity with monolithic app)
source("modules/statistical/spc/spc_module.R")
# MSA module (to be implemented for parity with monolithic app)
source("modules/statistical/msa/msa_module.R")
source("modules/statistical/doe_orthogonal/doe_orthogonal_module.R")

# Reliability (series-of-parallels calculator + Crow-AMSAA growth + Weibull)
# Calculator is manual grid; Growth / Weibull use working data from File | Import.
source("modules/reliability/reliability_calculator_module.R")
source("modules/reliability/growth_analysis_module.R")
source("modules/reliability/weibull_analysis_module.R")

# Concept simulators (teaching demos; no working data)
source("modules/learning/simulators/clt_simulator_module.R")
source("modules/learning/simulators/power_simulator_module.R")
source("modules/learning/simulators/anova_simulator_module.R")

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
  
  titlePanel(title = div(img(src = "roi-stat.svg", width = "40px"), "stats4ROI v4.3"), windowTitle = "stats4ROI"),
  navbarPage(
    title = NULL,
    # Welcome Page - First tabPanel is the default
    tabPanel(
      title = "Welcome to stats4ROI!",
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
      tabPanel(
        title = "Central Limit Theorem",
        create_clt_simulator_ui("clt_sim")
      ),
      tabPanel(
        title = "Power & Sample Size",
        create_power_simulator_ui("power_sim")
      ),
      tabPanel(
        title = "How ANOVA Works",
        create_anova_simulator_ui("anova_sim")
      )
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
        p("This is the filtered data that will be used by the statistical analysis modules."),
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
      tabPanel(
        title = "Binomial",
        create_binomial_ui("binomial")
      ),
      tabPanel(
        title = "Normal",
        create_normal_ui("normal")
      ),
      tabPanel(
        title = "Poisson",
        create_poisson_ui("poisson")
      ),
      tabPanel(
        title = "Hypergeometric",
        create_hypergeometric_ui("hypergeometric")
      ),
      tabPanel(
        title = "Geometric",
        create_geometric_ui("geometric")
      ),
      tabPanel(
        title = "Exponential",
        create_exponential_ui("exponential")
      ),
      tabPanel(
        title = "Weibull",
        create_weibull_ui("weibull")
      ),
      tabPanel(
        title = "F Distribution",
        create_f_distribution_ui("f_distribution")
      ),
      tabPanel(
        title = "Critical Values",
        create_critical_values_ui("critical_values")
      ),
      tabPanel(
        title = "Distribution Testing",
        create_distribution_testing_ui("distribution_testing")
      )
    ),
    navbarMenu(
      title = "Reliability",
      tabPanel(
        title = "Reliability Calculator",
        create_reliability_calculator_ui("reliability_calc")
      ),
      tabPanel(
        title = "Growth Analysis",
        create_growth_analysis_ui("growth_analysis")
      ),
      tabPanel(
        title = "Weibull Analysis",
        create_weibull_analysis_ui("weibull_analysis")
      )
    ),
    tabPanel(
      title = "Sample Size/Power",
      create_sample_size_power_ui("sample_size_power")
    ),
    create_eda_ui("eda"),
    create_one_two_sample_tests_ui("one_two_sample_tests"),  # Re-adding step by step
    create_correlation_association_ui("correlation_association"),
    create_autocorrelation_ui("autocorrelation"),
    create_spc_ui("spc"),
    create_msa_ui("msa"),
    create_crosstabs_ui("crosstabs"),
    create_anova_ui("anova"),
    create_doe_orthogonal_ui("doe_orthogonal")
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

  # Data filtering (replicating app.R lines 3857-3895)
  # Data reactive for filtering - use transformed data (or working data when no transforms)
  data <- reactive({
    transform_result$data()
  })

  # Filter server (replicating app.R lines 3862-3868)
  # Use transformed data for filtering
  res_filter <- list(
    filtered = data,
    code = reactive("No filtering applied"),
    expr = reactive("No filtering applied")
  )

  # Progress bar update (replicating app.R lines 3870-3875)
  observeEvent(res_filter$filtered(), {
    tryCatch({
      updateProgressBar(
        session = session, id = "pbar",
        value = nrow(res_filter$filtered()), total = nrow(data())
      )
    }, error = function(e) {
      # Silently ignore progress bar errors
      cat("Progress bar update error:", e$message, "\n")
    })
  })

  # Dynamic filtering module - receives transformed data
  dynamic_filtering_result <- create_dynamic_filtering_server("dynamic_filtering", transform_result$data)
  
  # Current working data output (same pattern as deployment: static output + renderDT with data)
  # Shows the same data used by analyses (filtered + transformed)
  output$w_data <- DT::renderDT({
    req(safe_filtered_data())
    safe_filtered_data()
  }, options = list(lengthMenu = c(5, 10, 50), pageLength = 10))

  # Safe filtered data reactive (filtered data when available, else working data)
  safe_filtered_data <- reactive({
    tryCatch({
      dynamic_filtering_result$filtered()
    }, error = function(e) {
      working_data_result$data()
    })
  })

  # Download current data (transformed + filtered) as CSV
  output$download_working_data <- downloadHandler(
    filename = function() {
      paste0("stats4ROI_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      df <- safe_filtered_data()
      if (is.null(df) || nrow(df) == 0) {
        df <- data.frame(Message = "No data to export.")
      }
      readr::write_csv(df, file)
    }
  )
  
  # Global working data reactive for use by other modules
  # This provides the filtered data (with filters applied when available)
  working_data <- reactive({
    working_data_result$data()
  })
  
  # Store working data in reactive values for module access
  # This replicates how the original app makes filtered data available
  values <- reactiveValues(working_data = NULL, color_palette = NULL)
  
  observe({
    values$working_data <- working_data()
  })
  
  # Set up global data invalidation system
  # This ensures ALL data-driven UI elements are reset when new data is loaded
  setup_global_data_invalidation(session, working_data)
  
  # Settings functionality (replicating app.R lines 16095-16106)
  # Color palette management using global system
  pal_col <- reactiveVal("R4")  # Default color palette
  
  # Create reactive color palette that updates when user changes selection
  reactive_color_palette <- reactive({
    pal <- input$color_pal
    if (is.null(pal)) pal <- "R4"  # Default if not yet selected
    get_color_palette(pal)
  })
  
  # Initialize color palette immediately
  values$color_palette <- get_color_palette("R4")

  # Concept simulators (no working data)
  create_clt_simulator_server("clt_sim", reactive_color_palette)
  create_power_simulator_server("power_sim", reactive_color_palette)
  create_anova_simulator_server("anova_sim", reactive_color_palette)
  
  # Distribution modules (replicating app.R distribution functionality)
  # Binomial distribution - use reactive color palette
  create_binomial_server("binomial", reactive_color_palette)
  
  # Normal distribution - use reactive color palette
  create_normal_server("normal", reactive_color_palette)
  
  # Poisson distribution - use reactive color palette
  create_poisson_server("poisson", reactive_color_palette)
  
  # Hypergeometric distribution - use reactive color palette
  create_hypergeometric_server("hypergeometric", reactive_color_palette)
  
  # Geometric distribution - use reactive color palette
  create_geometric_server("geometric", reactive_color_palette)
  
  # Exponential distribution - use reactive color palette
  create_exponential_server("exponential", reactive_color_palette)
  
  # Weibull distribution - use reactive color palette
  create_weibull_server("weibull", reactive_color_palette)
  
  # F-distribution - use reactive color palette
  create_f_distribution_server("f_distribution", reactive_color_palette)
  
  # Critical Values - use reactive color palette
  create_critical_values_server("critical_values", reactive_color_palette)
  
  # Reliability Calculator
  create_reliability_calculator_server("reliability_calc", reactive_color_palette)

  # Reliability Growth Analysis (uses working data)
  create_growth_analysis_server("growth_analysis", safe_filtered_data, reactive_color_palette)

  # Weibull life-data analysis (uses working data)
  create_weibull_analysis_server("weibull_analysis", safe_filtered_data, reactive_color_palette)

  # Sample Size and Power Analysis
  create_sample_size_power_server("sample_size_power", reactive_color_palette)
  
  # EDA Module
  create_eda_server("eda", safe_filtered_data, reactive_color_palette)
  
  # One- and Two-Sample Tests Module
  create_one_two_sample_tests_server("one_two_sample_tests", safe_filtered_data, reactive_color_palette)
  
  # Correlation and Association Module
  create_correlation_association_server("correlation_association", safe_filtered_data, reactive_color_palette)

  # Autocorrelation Module
  create_autocorrelation_server("autocorrelation", safe_filtered_data, reactive_color_palette)
  
  # SPC Module
  create_spc_server("spc", safe_filtered_data, reactive_color_palette)

  # MSA Module
  create_msa_server("msa", safe_filtered_data, reactive_color_palette)

  # Crosstabs Module
  create_crosstabs_server("crosstabs", safe_filtered_data, reactive_color_palette)
  create_anova_server("anova", safe_filtered_data, reactive_color_palette)
  create_doe_orthogonal_server("doe_orthogonal", reactive_color_palette)

  # Distribution Testing
  create_distribution_testing_server("distribution_testing", safe_filtered_data, reactive_color_palette)
  
  # Color swatch plot (replicating app.R lines 16095-16106)
  output$swatch <- renderPlot({
    pal <- input$color_pal
    if (is.null(pal)) pal <- "R4"  # Default if not yet selected
    
    # Update global color palette
    pal_col(pal)
    
    # Plot a demo with the selected palette
    fake_data <- data.frame(
      x1 = rnorm(50), x2 = rnorm(50), x3 = rnorm(50), x4 = rnorm(50),
      x5 = rnorm(50), x6 = rnorm(50), x7 = rnorm(50), x8 = rnorm(50)
    )
    
    # Use global color system
    color_palette <- get_color_palette(pal)
    boxplot(x = fake_data, col = color_palette)
    
    # Store colors for use by other modules
    values$color_palette <- color_palette
    values$col_mean_line <- color_palette[3]
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
