# Correlation and Association Module
# This module follows the three-tier architecture with proper coordinator-worker separation
# and integration with global systems.

# =============================================================================
# IMPORTS
# =============================================================================
library(shiny)
library(DT)
library(lolcat)
library(shinyWidgets)

# Source global systems
source("modules/config/global_config.R")
source("modules/config/global_data_invalidation.R")

# Source function overrides (must be after library(lolcat) to override)
# TODO: Remove this override when lolcat package is updated with the fix
# See: modules/statistical/correlation_association/utils/OVERRIDE_REMOVAL_GUIDE.md for removal instructions
source("modules/statistical/correlation_association/utils/cor_kendall_tau_simple_override.R")

# Source worker modules
source("modules/statistical/correlation_association/server/correlation_association_server.R")
source("modules/statistical/correlation_association/server/scatterplot_server.R")

# =============================================================================
# CHOICE VECTORS (from original app)
# =============================================================================
# Choices for statistics entry correlation tests alternate hyp
choice_corr_alt_text <- c(
  "\U1D70C of sample is not equal to \U1D70C\U2080", "\U1D70C of sample is less than \U1D70C\U2080", "\U1D70C of sample is greater than \U1D70C\U2080",
  "\U1D70C\U209B is not equal to 0", "\U1D70C\U209B is less than 0", "\U1D70C\U209B is greater than 0",
  "\U1D70F is not equal to 0", "\U1D70F is less than 0", "\U1D70F is greater than 0",
  "\U1D711 is not equal to 0", "", "",
  "\U1D70C\U2081\U2082 is not equal to \U1D70C\U2083\U2084", "\U1D70C\U2081\U2082 is less than \U1D70C\U2083\U2084", "\U1D70C\U2081\U2082 is greater than \U1D70C\U2083\U2084",
  "\U1D6FE\U2081 is not equal to \U1D6FE\U2082", "\U1D6FE\U2081 is less than \U1D6FE\U2082", "\U1D6FE\U2081 is greater than \U1D6FE\U2082",
  "\U1D73F\U2081 is not equal to \U1D73F\U2082", "\U1D73F\U2081 is less than \U1D73F\U2082", "\U1D73F\U2081 is greater than \U1D73F\U2082",
  "\U1D70C\U2081\U2083 is not equal to \U1D70C\U2082\U2083", "\U1D70C\U2081\U2083 is less than \U1D70C\U2082\U2083", "\U1D70C\U2081\U2083 is greater than \U1D70C\U2082\U2083",
  "\U1D73F is not equal to 0", "\U1D73F is less than 0", "\U1D73F is greater than 0"
)

# Curve fit choices for scatterplot (from original app)
curve_fit_choice <- c(
  "None" = 0,
  "Linear: y = A + Bx" = 1,
  "Exponential: y = Ae(Bx)" = 2,
  "Logarithmic: A + B ln x" = 3,
  "Proportional: y = Ax" = 4,
  "Power: y = Ax^B" = 5,
  "Inverse: y = A + B/x" = 6,
  "S: y=e(A + B/x)" = 14,
  "Compound: y = AB^x" = 7,
  "Growth: y = e(A + Bx)" = 8,
  "Loess: Locally Weighted Regression" = 9,
  "Quadratic: y = A + Bx + Cx²" = 10,
  "Cubic: y = A + Bx + Cx² + Dx³" = 11,
  "4th Order: y = A + Bx + Cx² + Dx³ + Ex^4" = 12,
  "5th Order: y = A + Bx + Cx² + Dx³ + Ex^4 + Fx^5" = 13
)

# Choices for data entry correlation tests alternate hyp
choice_corr_alt_text_data <- c(
  "\U1D70C of sample is not equal to \U1D70C\U2080", "\U1D70C of sample is less than \U1D70C\U2080", "\U1D70C of sample is greater than \U1D70C\U2080",
  "\U1D70C\U209B is not equal to 0", "\U1D70C\U209B is less than 0", "\U1D70C\U209B is greater than 0",
  "\U1D70F is not equal to 0", "\U1D70F is less than 0", "\U1D70F is greater than 0",
  "V is not equal to 0", "", "",
  "\U1D70C is not equal to 0", "\U1D70C is less than 0", "\U1D70C is greater than 0",
  "\U1D70C is not equal to 0", "\U1D70C is less than 0", "\U1D70C is greater than 0",
  "True Q is not equal to 0", "True Q is less than 0", "True Q is greater than 0",
  "True G is not equal to \U03B3\U2080", "True G is less than \U03B3\U2080", "True G is greater than \U03B3\U2080",
  "\U1D70C\U209C is not equal to 0", "\U1D70C\U209C is less than 0", "\U1D70C\U209C is greater than 0",
  "\U1D73F is not equal to 0", "\U1D73F is less than 0", "\U1D73F is greater than 0",
  "W' is not equal to 0", "W' is less than 0", "W' is greater than 0",
  "J' is not equal to 0", "J' is less than 0", "J' is greater than 0",
  "\U1D70C\U2081\U2082 is not equal to \U1D70C\U2083\U2084", "\U1D70C\U2081\U2082 is less than \U1D70C\U2083\U2084", "\U1D70C\U2081\U2082 is greater than \U1D70C\U2083\U2084",
  "\U1D6FE\U2081 is not equal to \U1D6FE\U2082", "\U1D6FE\U2081 is less than \U1D6FE\U2082", "\U1D6FE\U2081 is greater than \U1D6FE\U2082",
  "\U1D73F\U2081 is not equal to \U1D73F\U2082", "\U1D73F\U2081 is less than \U1D73F\U2082", "\U1D73F\U2081 is greater than \U1D73F\U2082",
  "J'\U2081 is not equal to J'\U2082", "J'\U2081 is less than J'\U2082", "J'\U2081 is greater than J'\U2082",
  "\U1D70C\U2081\U2083 is not equal to \U1D70C\U2082\U2083", "\U1D70C\U2081\U2083 is less than \U1D70C\U2082\U2083", "\U1D70C\U2081\U2083 is greater than \U1D70C\U2082\U2083"
)

# Source sub-module UI components
source("modules/statistical/correlation_association/ui/correlation_association_ui.R")

# =============================================================================
# COORDINATOR UI FUNCTION
# =============================================================================
create_correlation_association_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Correlation and Association",
    create_correlation_association_ui_internal(ns)
  )
}

# =============================================================================
# COORDINATOR SERVER FUNCTION
# =============================================================================
create_correlation_association_server <- function(id, filtered_data, reactive_color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # =========================================================================
    # REGISTER MODULE WITH GLOBAL DATA INVALIDATION SYSTEM
    # =========================================================================
    register_module("correlation_association_module", 
      ui_reset = function() {
        # Reset all Correlation and Association UI elements to defaults
        # TODO: Add specific resets as needed
      },
      validation = function(data, selections) {
        # Validate Correlation and Association selections
        if (is.null(data) || nrow(data) == 0) {
          return(list(valid = FALSE, message = "No data available for Correlation and Association analysis"))
        }
        return(list(valid = TRUE, message = ""))
      }
    )
    
    # =========================================================================
    # SUB-MODULE SERVER CALLS (Worker Module)
    # =========================================================================
    correlation_association_worker_result <- create_correlation_association_worker(
      "correlation_association_worker",
      filtered_data,
      reactive({
        list(
          # Enter Statistics mode inputs
          conf_corr = input$conf_corr,
          corr_tests = input$corr_tests,
          alt_corr = input$alt_corr,
          stat_corr1 = input$stat_corr1,
          stat_corr2 = input$stat_corr2,
          stat_corr3 = input$stat_corr3,
          stat_corr4 = input$stat_corr4,
          n_corr = input$n_corr,
          stat_corr6 = input$stat_corr6,
          decimal_corr = input$decimal_corr,
          # Use Data mode inputs
          conf_corr_data = input$conf_corr_data,
          decimal_corr_data = input$decimal_corr_data,
          corr_tests_data = input$corr_tests_data,
          alt_corr_data = input$alt_corr_data,
          corr_data_selected_columns_1 = input$corr_data_selected_columns_1,
          corr_data_selected_columns_2 = input$corr_data_selected_columns_2,
          corr_data_selected_columns_3 = input$corr_data_selected_columns_3,
          corr_data_selected_columns_4 = input$corr_data_selected_columns_4,
          corr_data_type = input$corr_data_type,
          corr_UI1_data = input$corr_UI1_data,
          corr_UI2_data = input$corr_UI2_data,
          corr_UI3_data = input$corr_UI3_data,
          corr_UI4_data = input$corr_UI4_data,
          corr_UI5_data = input$corr_UI5_data,
          corr_UI6_data = input$corr_UI6_data
        )
      })
    )
    
    # =========================================================================
    # ENTER STATISTICS MODE - UI RENDERING FUNCTIONS
    # =========================================================================
    
    # Render test selector based on "One Sample or Two?"
    output$corr_tests <- renderUI({
      one_or_two_corr <- input$one_or_two_corr
      req(one_or_two_corr)
      
      if (one_or_two_corr == 1) {
        # One sample test
        corr_test_choice <- c(1, 2, 3, 4)
        names(corr_test_choice) <- c("Pearson r", "Spearman Rank r\U209B", "Kendall's \U1D70F", "\U1D719 / Cramers V")
      } else if (one_or_two_corr == 2) {
        # Two sample independent
        corr_test_choice <- c(5, 6, 7)
        names(corr_test_choice) <- c("Pearson r", "Goodman and Kruskal's \U1D6FE", "Cohen's \U1D73F")
      } else if (one_or_two_corr == 3) {
        # Two sample dependent
        corr_test_choice <- c(8, 9)
        names(corr_test_choice) <- c("Pearson r", "\U1D73F Agreement (2x2)")
      }
      
      radioButtons(inputId = ns("corr_tests"), label = "Select Test", choices = corr_test_choice)
    })
    
    # Render alternative hypothesis selector
    output$corr_alt <- renderUI({
      corr_tests <- input$corr_tests
      req(corr_tests)
      
      choice_corr_alt <- c("two.sided", "less", "greater")
      if (corr_tests == 4) {
        choice_corr_alt <- c("two.sided", "", "")
      }
      index <- (3 * (as.numeric(corr_tests) - 1)) + 1
      
      names(choice_corr_alt) <- choice_corr_alt_text[c(seq(index, index + 2))]
      
      selectInput(inputId = ns("alt_corr"), label = "Alternative Hypothesis", choices = choice_corr_alt)
    })
    
    # Render dynamic UI inputs (corrUI1 through corrUI6)
    output$corrUI1 <- renderUI({
      corr_tests <- input$corr_tests
      req(corr_tests)
      
      if (corr_tests == 1) {
        numericInput(inputId = ns("stat_corr1"), label = withMathJax("$$r:{ }$$"), value = 0.5, width = "750px")
      } else if (corr_tests == 2) {
        numericInput(inputId = ns("stat_corr1"), label = withMathJax("$$r_{s}:{ }$$"), value = 0.5, width = "750px")
      } else if (corr_tests == 3) {
        numericInput(inputId = ns("stat_corr1"), label = "Concordant Pairs", value = 395, width = "750px")
      } else if (corr_tests == 4) {
        numericInput(inputId = ns("stat_corr1"), label = withMathJax("$$n_{11}:{ }$$"), value = 6, width = "750px")
      } else if (corr_tests == 5) {
        numericInput(inputId = ns("stat_corr1"), label = withMathJax("$$r_{12}:{ }$$"), value = 0.5, width = "750px")
      } else if (corr_tests == 6) {
        numericInput(inputId = ns("stat_corr1"), label = withMathJax("$$\\gamma_{1:{ }}$$"), value = 0.8, width = "750px")
      } else if (corr_tests == 7) {
        numericInput(inputId = ns("stat_corr1"), label = withMathJax("$$\\kappa_{1}:{ }$$"), value = 0.701, width = "750px")
      } else if (corr_tests == 8) {
        numericInput(inputId = ns("stat_corr1"), label = withMathJax("$$r_{13}:{ }$$"), value = 0.312, width = "750px")
      } else if (corr_tests == 9) {
        HTML("<p style='text-align:center'><b>Assessor 2 Pass</b></p>")
      }
    })
    
    output$corrUI2 <- renderUI({
      corr_tests <- input$corr_tests
      req(corr_tests)
      
      if (corr_tests == 1) {
        numericInput(inputId = ns("stat_corr2"), label = withMathJax("$$\\rho_{0}:{ }$$"), value = 0.5, width = "750px")
      } else if (corr_tests == 2) {
        NULL
      } else if (corr_tests == 3) {
        numericInput(inputId = ns("stat_corr2"), label = "Discordant Pairs", value = 40, width = "750px")
      } else if (corr_tests == 4) {
        numericInput(inputId = ns("stat_corr2"), label = withMathJax("$$n_{21}:{ }$$"), value = 2, width = "750px")
      } else if (corr_tests == 5) {
        numericInput(inputId = ns("stat_corr2"), label = withMathJax("$$r_{34}:{ }$$"), value = 0.3, width = "750px")
      } else if (corr_tests == 6) {
        numericInput(inputId = ns("stat_corr2"), label = withMathJax("$$\\gamma_{2}:{ }$$"), value = 0.3, width = "750px")
      } else if (corr_tests == 7) {
        numericInput(inputId = ns("stat_corr2"), label = withMathJax("$$\\kappa_{2:{ }}$$"), value = 0.469, width = "750px")
      } else if (corr_tests == 8) {
        numericInput(inputId = ns("stat_corr2"), label = withMathJax("$$r_{23}:{ }$$"), value = 0.737, width = "750px")
      } else if (corr_tests == 9) {
        HTML("<p style='text-align:center'><b>Assessor 2 Fail</b></p>")
      }
    })
    
    output$corrUI3 <- renderUI({
      corr_tests <- input$corr_tests
      req(corr_tests)
      
      if (corr_tests == 1 || corr_tests == 2) {
        NULL
      } else if (corr_tests == 3) {
        numericInput(inputId = ns("stat_corr3"), label = "Count Ties X", value = 0, width = "750px")
      } else if (corr_tests == 4) {
        numericInput(inputId = ns("stat_corr3"), label = withMathJax("$$n_{12}:{ }$$"), value = 1, width = "750px")
      } else if (corr_tests == 5) {
        NULL
      } else if (corr_tests == 6) {
        numericInput(inputId = ns("stat_corr3"), label = withMathJax("$$\\sigma_{SE1}:{ }$$"), value = 0.1, width = "750px")
      } else if (corr_tests == 7) {
        numericInput(inputId = ns("stat_corr3"), label = withMathJax("$$se_{\\kappa1}:{ }$$"), value = 0.063, width = "750px")
      } else if (corr_tests == 8) {
        numericInput(inputId = ns("stat_corr3"), label = withMathJax("$$r_{12}:{ }$$"), value = 0.51, width = "750px")
      } else if (corr_tests == 9) {
        numericInput(inputId = ns("stat_corr3"), label = "Assessor 1 Pass", value = 150, min = 0, step = 1)
      }
    })
    
    output$corrUI4 <- renderUI({
      corr_tests <- input$corr_tests
      req(corr_tests)
      
      if (corr_tests == 1 || corr_tests == 2) {
        NULL
      } else if (corr_tests == 3) {
        numericInput(inputId = ns("stat_corr4"), label = "Count Ties Y", value = 0, width = "750px")
      } else if (corr_tests == 4) {
        numericInput(inputId = ns("stat_corr4"), label = withMathJax("$$n_{22}:{ }$$"), value = 3, width = "750px")
      } else if (corr_tests == 5) {
        numericInput(inputId = ns("stat_corr4"), label = withMathJax("$$n_{34}:{ }$$"), value = 10, width = "750px")
      } else if (corr_tests == 6) {
        numericInput(inputId = ns("stat_corr4"), label = withMathJax("$$\\sigma_{SE2}:{ }$$"), value = 0.02, width = "750px")
      } else if (corr_tests == 7) {
        numericInput(inputId = ns("stat_corr4"), label = withMathJax("$$se_{\\kappa2}:{ }$$"), value = 0.007, width = "750px")
      } else if (corr_tests == 8) {
        NULL
      } else if (corr_tests == 9) {
        numericInput(inputId = ns("stat_corr4"), label = "Assessor 1 Pass", value = 6, min = 0, step = 1)
      }
    })
    
    output$corrUI5 <- renderUI({
      corr_tests <- input$corr_tests
      req(corr_tests)
      
      if (corr_tests == 1 || corr_tests == 2 || corr_tests == 3) {
        numericInput(inputId = ns("n_corr"), label = withMathJax("$$n:{ }$$"), value = 10, min = 1, step = 1)
      } else if (corr_tests == 4) {
        NULL
      } else if (corr_tests == 5) {
        numericInput(inputId = ns("n_corr"), label = withMathJax("$$n_{12}:{ }$$"), value = 10, min = 1, step = 1)
      } else if (corr_tests == 6 || corr_tests == 7) {
        NULL
      } else if (corr_tests == 8) {
        numericInput(inputId = ns("n_corr"), label = withMathJax("$$n:{ }$$"), value = 20, min = 1, step = 1)
      } else if (corr_tests == 9) {
        numericInput(inputId = ns("n_corr"), label = "Assessor 1 Fail", value = 14, min = 0, step = 1)
      }
    })
    
    output$corrUI6 <- renderUI({
      corr_tests <- input$corr_tests
      req(corr_tests)
      
      if (corr_tests == 9) {
        numericInput(inputId = ns("stat_corr6"), label = "Assessor 1 Fail", value = 30, min = 0, step = 1)
      } else {
        NULL
      }
    })
    
    # =========================================================================
    # ENTER STATISTICS MODE - RESULTS RENDERING
    # =========================================================================
    
    # Reactive for Enter Statistics results - round at rendering stage
    corr_stat_results_reactive <- reactive({
      corr_tests <- input$corr_tests
      conf_corr <- input$conf_corr
      alt <- input$alt_corr
      R <- input$decimal_corr
      
      req(corr_tests, conf_corr, alt, R)
      
      # Get results from server (unrounded)
      results <- correlation_association_worker_result$corr_stat_out()
      req(results)
      
      # Round at rendering stage
      ro(results, R)
    })
    
    output$pretty_corr <- renderUI({
      resultsR <- corr_stat_results_reactive()
      
      # Handle NULL or character results
      if (is.null(resultsR)) {
        return(HTML("<p>No results available. Please check your inputs.</p>"))
      }
      
      if (is.character(resultsR)) {
        return(HTML(paste("<p>", resultsR, "</p>")))
      }
      
      conf <- input$conf_corr
      alt <- input$alt_corr
      corr_tests <- input$corr_tests
      R <- input$decimal_corr
      
      req(conf, alt, corr_tests, R)
      
      if (alt == "two.sided") {
        alt_num <- 1
      } else if (alt == "less") {
        alt_num <- 2
      } else if (alt == "greater") {
        alt_num <- 3
      }
      
      beta_statement <- "Power to reject the null if the observed difference was real = "
      
      # Test 1: Pearson r one-sample
      if (corr_tests == 1) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{0}=$"), resultsR$parameter), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r^2 = $"), ro(resultsR$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), resultsR$estimate[3]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho :$")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste(if (resultsR$parameter == 0) {"t = "} else {"z = "}, resultsR$statistic), 
          if (resultsR$parameter == 0) {paste("df = ", resultsR$estimate[2])} else {""}, "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          paste(beta_statement, 100 * resultsR$estimate[8], "%")
        ))
      }
      # Test 2: Spearman rank
      else if (corr_tests == 2) {
        HTML(c(
          paste("<b>One-Sample ", resultsR$method, " Test</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r_s = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{s}=$"), resultsR$parameter), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r_{s}^2 = $"), ro(resultsR$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), resultsR$estimate[2] + 2), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho_{s} :$")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("t = ", resultsR$statistic), "</td>",
          "<td>", paste("df = ", resultsR$estimate[2]), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 3: Kendall's tau
      else if (corr_tests == 3) {
        UI3 <- input$stat_corr3
        UI4 <- input$stat_corr4
        req(UI3, UI4)
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste("Concordant Pairs: ", resultsR$estimate[3]), "</td>",
          "<td>", "</td>",
          "<td>", paste("Discordant Pairs: ", resultsR$estimate[4]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("Count Ties X: ", UI3), "</td>",
          "<td>", "</td>",
          "<td>", paste("Count Ties Y: ", UI4), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$\\tau = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), resultsR$estimate[2]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("z = ", resultsR$statistic), "</td>",
          "<td>", "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 4: Cramer's V/Phi
      else if (corr_tests == 4) {
        UI1 <- input$stat_corr1
        UI2 <- input$stat_corr2
        UI3 <- input$stat_corr3
        UI4 <- input$stat_corr4
        req(UI1, UI2, UI3, UI4)
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{11} =$"), UI1), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{21} =$"), UI2), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{12} =$"), UI3), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{22} =$"), UI4), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$V = $"), resultsR[["statistic"]][["V"]]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\chi^2 =$"), resultsR[["estimate"]][["chi.square"]]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "<td>", "</td>",
          "<td>", "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 5: Pearson r two-sample independent
      else if (corr_tests == 5) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r_{12} = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$r_{34}=$"), resultsR$estimate[7]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r_{12}^2 = $"), resultsR$estimate[6]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$r_{34}^2 = $"), resultsR$estimate[12]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{12} = $"), resultsR$estimate[2]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{34} = $"), resultsR$estimate[8]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho_{12} :$")), "</td>",
          "<td>", resultsR$estimate[4], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[5], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{34} :$")), "</td>",
          "<td>", resultsR$estimate[10], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[11], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("z = ", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 6: Goodman-Kruskal gamma two-sample independent
      else if (corr_tests == 6) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$\\gamma_{1} = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\gamma_{2}=$"), resultsR$estimate[3]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$\\sigma_{SE_{1}} = $"), resultsR$estimate[2]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\sigma_{SE_{2}} = $"), resultsR$estimate[4]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence"), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("z = ", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 7: Cohen's kappa two-sample independent
      else if (corr_tests == 7) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$\\kappa_{1} = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\kappa_{2}=$"), resultsR$estimate[3]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$SE_{\\kappa_{1}} = $"), resultsR$estimate[2]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$SE_{\\kappa_{2}} = $"), resultsR$estimate[4]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence"), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("z = ", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 8: Pearson r two-sample dependent
      else if (corr_tests == 8) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r_{13} = $"), resultsR$estimate[3]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$r_{23}=$"), resultsR$estimate[7]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r_{12} = $"), resultsR$estimate[11]), "</td>",
          "<td>", "</td>",
          "<td>", "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), 3 + resultsR$estimate[2]), "</td>",
          "<td>", "</td>",
          "<td>", "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r_{13} - r_{23} = $")), resultsR$estimate[1], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho_{13} - \\rho_{23}:$")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{13} :$")), "</td>",
          "<td>", resultsR$estimate[4], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[5], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{23} :$")), "</td>",
          "<td>", resultsR$estimate[8], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[9], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{12} :$")), "</td>",
          "<td>", resultsR$estimate[12], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[13], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("t = ", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 9: Cohen's kappa one-sample
      else if (corr_tests == 9) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{total} = $"), resultsR[["estimate"]][["n"]]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("Proportion Agreement =", resultsR[["estimate"]][["p_o"]]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("Proportion Chance Agreement =", resultsR[["estimate"]][["p_c"]]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$\\kappa_{max} =$"), resultsR[["estimate"]][["kappa.max"]]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$\\kappa = $"), resultsR[["estimate"]][["kappa"]]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval"), "</td>",
          "<td>", resultsR$conf.int[1], " to ", resultsR$conf.int[2], "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("z = ", resultsR[["statistic"]][["z"]]), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Placeholder for tests not yet implemented
      else {
        HTML(paste("<p>Results rendering for test", corr_tests, "to be completed</p>"))
      }
    })
    
    # =========================================================================
    # USE DATA MODE - UI RENDERING FUNCTIONS
    # =========================================================================
    
    # Render test selector for Use Data mode
    output$corr_tests_data <- renderUI({
      one_or_two_corr_data <- input$one_or_two_corr_data
      req(one_or_two_corr_data)
      
      if (one_or_two_corr_data == 1) {
        # One sample test
        corr_test_choice <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
        names(corr_test_choice) <- c(
          "Pearson r", "Spearman Rank r\U209B", "Kendall's \U1D70F", "Cramers V",
          "Biserial r", "Point-Biserial r", "Yule's Q", "Goodman and Kruskal's \U1D6FE",
          "Tetrachoric r", "Cohen's \U1D73F", "Kendall's W", "J-index"
        )
      } else if (one_or_two_corr_data == 2) {
        # Two sample independent
        corr_test_choice <- c(13, 14, 15, 16)
        names(corr_test_choice) <- c("Pearson r", "Goodman and Kruskal's \U1D6FE", "Cohen's \U1D73F", "J-index")
      } else if (one_or_two_corr_data == 3) {
        # Two sample dependent
        corr_test_choice <- c(17)
        names(corr_test_choice) <- c("Pearson r")
      }
      
      radioButtons(inputId = ns("corr_tests_data"), label = "Select Test", choices = corr_test_choice)
    })
    
    # Render alternative hypothesis selector for Use Data mode
    output$corr_alt_data <- renderUI({
      corr_tests <- input$corr_tests_data
      req(corr_tests)
      
      choice_corr_alt <- c("two.sided", "less", "greater")
      if (corr_tests == 4) {
        choice_corr_alt <- c("two.sided", "", "")
      }
      index <- (3 * (as.numeric(corr_tests) - 1)) + 1
      
      names(choice_corr_alt) <- choice_corr_alt_text_data[c(seq(index, index + 2))]
      
      selectInput(inputId = ns("alt_corr_data"), label = "Alternative Hypothesis", choices = choice_corr_alt)
    })
    
    # Render data configuration selector
    output$data_config <- renderUI({
      test <- input$corr_tests_data
      data <- filtered_data()
      
      req(data, test)
      
      if (test == 4 || test == 7 || test == 8 || test == 9 || test == 10 || test == 12 || test == 16) {
        radioButtons(
          inputId = ns("corr_data_type"),
          label = "How is your data configured?",
          choices = c("Independent" = 1, "Frequency" = 2),
          inline = TRUE
        )
      } else if (test == 11) {
        radioButtons(
          inputId = ns("corr_data_type"),
          label = "How is your data configured?",
          choices = c("Independent" = 1, "Tabular" = 2),
          inline = TRUE
        )
      } else {
        NULL
      }
    })
    
    # Render column selectors (1-4)
    output$data_choice_column_corr_1 <- renderUI({
      data <- filtered_data()
      req(data)
      
      choices <- seq(1:ncol(data))
      names(choices) <- names(data)
      test <- input$corr_tests_data
      req(test)
      
      if (test == 1) {
        selectInput(inputId = ns("corr_data_selected_columns_1"), label = "X Data", choices = choices)
      } else if (test == 2 || test == 3) {
        selectInput(inputId = ns("corr_data_selected_columns_1"), label = "X\U2081 Data", choices = choices)
      } else if (test == 4 || test == 7 || test == 8 || test == 9) {
        selectInput(inputId = ns("corr_data_selected_columns_1"), label = "Row Data", choices = choices)
      } else if (test == 5) {
        selectInput(inputId = ns("corr_data_selected_columns_1"), label = "Dichotomized Data", choices = choices)
      } else if (test == 6) {
        selectInput(inputId = ns("corr_data_selected_columns_1"), label = "Dichotomous Data", choices = choices)
      } else if (test == 10) {
        selectInput(inputId = ns("corr_data_selected_columns_1"), label = "Rater 1", choices = choices)
      } else if (test == 11) {
        selectInput(inputId = ns("corr_data_selected_columns_2"), label = "Items Being Ranked", choices = choices)
      } else if (test == 12) {
        selectInput(inputId = ns("corr_data_selected_columns_2"), label = "Actual", choices = choices)
      } else if (test == 13 || test == 17) {
        selectInput(inputId = ns("corr_data_selected_columns_1"), label = "X\U2081", choices = choices)
      } else if (test == 14 || test == 15 || test == 16) {
        selectInput(inputId = ns("corr_data_selected_columns_1"), label = "Group", choices = choices)
      }
    })
    
    output$data_choice_column_corr_2 <- renderUI({
      data <- filtered_data()
      req(data)
      
      choices <- seq(1:ncol(data))
      names(choices) <- names(data)
      test <- input$corr_tests_data
      data_type <- input$corr_data_type
      req(test)
      
      if (test == 1) {
        selectInput(inputId = ns("corr_data_selected_columns_2"), label = "Y Data", choices = choices)
      } else if (test == 2 || test == 3) {
        selectInput(inputId = ns("corr_data_selected_columns_2"), label = "X\U2082 Data", choices = choices)
      } else if (test == 4 || test == 7 || test == 8 || test == 9) {
        selectInput(inputId = ns("corr_data_selected_columns_2"), label = "Column Data", choices = choices)
      } else if (test == 5 || test == 6) {
        selectInput(inputId = ns("corr_data_selected_columns_2"), label = "Continuous Data", choices = choices)
      } else if (test == 10) {
        selectInput(inputId = ns("corr_data_selected_columns_2"), label = "Rater 2", choices = choices)
      } else if (test == 11) {
        if (data_type == 1) {
          selectInput(inputId = ns("corr_data_selected_columns_2"), label = "Judges", choices = choices)
        } else if (data_type == 2) {
          selectizeInput(
            inputId = ns("corr_data_selected_columns_2"),
            label = "Select Judges",
            multiple = TRUE,
            choices = choices
          )
        }
      } else if (test == 12) {
        selectInput(inputId = ns("corr_data_selected_columns_2"), label = "Predicted", choices = choices)
      } else if (test == 13) {
        selectInput(inputId = ns("corr_data_selected_columns_2"), label = "Y\U2081", choices = choices)
      } else if (test == 14) {
        selectInput(inputId = ns("corr_data_selected_columns_2"), label = "Ordinal Data 1", choices = choices)
      } else if (test == 15) {
        selectInput(inputId = ns("corr_data_selected_columns_2"), label = "Judge 1", choices = choices)
      } else if (test == 16) {
        selectInput(inputId = ns("corr_data_selected_columns_2"), label = "Predicted", choices = choices)
      } else if (test == 17) {
        selectInput(inputId = ns("corr_data_selected_columns_2"), label = "X\U2082", choices = choices)
      }
    })
    
    output$data_choice_column_corr_3 <- renderUI({
      data <- filtered_data()
      test <- input$corr_tests_data
      data_type <- input$corr_data_type
      
      req(data, test)
      if (is.null(data_type)) data_type <- 0
      
      output <- NULL
      
      if ((test > 3) && (data_type == 2)) {
        choices <- seq(1:ncol(data))
        names(choices) <- names(data)
        output <- selectInput(inputId = ns("corr_data_selected_columns_3"), label = "Weight", choices = choices)
      } else if ((test == 10 && data_type == 2)) {
        choices <- seq(1:ncol(data))
        names(choices) <- names(data)
        output <- selectInput(inputId = ns("corr_data_selected_columns_3"), label = "Weight", choices = choices)
      } else if (test == 11) {
        if (data_type == 1) {
          choices <- seq(1:ncol(data))
          names(choices) <- names(data)
          output <- selectInput(inputId = ns("corr_data_selected_columns_3"), label = "Ratings", choices = choices)
        } else if (data_type == 2) {
          output <- NULL
        }
      } else if ((test == 12) && (data_type == 2)) {
        choices <- seq(1:ncol(data))
        names(choices) <- names(data)
        output <- selectInput(inputId = ns("corr_data_selected_columns_3"), label = "Weight", choices = choices)
      } else if (test == 13) {
        choices <- seq(1:ncol(data))
        names(choices) <- names(data)
        output <- selectInput(inputId = ns("corr_data_selected_columns_3"), label = "X\U2082", choices = choices)
      } else if (test == 14) {
        choices <- seq(1:ncol(data))
        names(choices) <- names(data)
        output <- selectInput(inputId = ns("corr_data_selected_columns_3"), label = "Ordinal Data 2", choices = choices)
      } else if (test == 15) {
        choices <- seq(1:ncol(data))
        names(choices) <- names(data)
        output <- selectInput(inputId = ns("corr_data_selected_columns_3"), label = "Judge 2", choices = choices)
      } else if (test == 16) {
        choices <- seq(1:ncol(data))
        names(choices) <- names(data)
        output <- selectInput(inputId = ns("corr_data_selected_columns_3"), label = "Actual", choices = choices)
      } else if (test == 17) {
        choices <- seq(1:ncol(data))
        names(choices) <- names(data)
        output <- selectInput(inputId = ns("corr_data_selected_columns_3"), label = "X\U2083 (Common variable)", choices = choices)
      }
      
      output
    })
    
    output$data_choice_column_corr_4 <- renderUI({
      data <- filtered_data()
      test <- input$corr_tests_data
      data_type <- input$corr_data_type
      
      req(data, test)
      output <- NULL
      
      if (test == 13) {
        choices <- seq(1:ncol(data))
        names(choices) <- names(data)
        output <- selectInput(inputId = ns("corr_data_selected_columns_4"), label = "Y\U2082", choices = choices)
      } else if (test == 14) {
        choices <- seq(1:ncol(data))
        names(choices) <- names(data)
        output <- selectInput(inputId = ns("corr_data_selected_columns_4"), label = "Weight", choices = choices)
      } else if ((test == 16 && data_type == 2)) {
        choices <- seq(1:ncol(data))
        names(choices) <- names(data)
        output <- selectInput(inputId = ns("corr_data_selected_columns_4"), label = "Weight", choices = choices)
      }
      
      output
    })
    
    # Crosstab generation reactive for Use Data mode
    crosstab_corr_data <- reactive({
      data <- filtered_data()
      req(data)
      
      data_type <- input$corr_data_type
      corr_tests <- input$corr_tests_data
      corr_data_1 <- input$corr_data_selected_columns_1
      corr_data_2 <- input$corr_data_selected_columns_2
      corr_data_3 <- input$corr_data_selected_columns_3
      corr_data_4 <- input$corr_data_selected_columns_4
      
      req(data_type, corr_tests, corr_data_1, corr_data_2)
      
      x1 <- data[, as.numeric(corr_data_1)]
      x2 <- data[, as.numeric(corr_data_2)]
      x3 <- if (!is.null(corr_data_3)) data[, as.numeric(corr_data_3)] else NULL
      x4 <- if (!is.null(corr_data_4)) data[, as.numeric(corr_data_4)] else NULL
      
      output <- NULL
      
      # If independent format
      if (data_type == 1 && corr_tests != 11) {
        output <- transform.independent.format.to.xt(
          x_row = x1,
          x_col = x2,
          na.rm = TRUE,
          x_row_name = names(data[as.numeric(corr_data_1)]),
          x_col_name = names(data[as.numeric(corr_data_2)])
        )
      }
      # If weighted format
      else if ((data_type == 2 && corr_tests != 11) || (corr_tests == 11 && data_type == 1)) {
        req(x3)
        output <- transform.independent.format.to.xt(
          x_row = x1,
          x_col = x2,
          weight = x3,
          na.rm = TRUE,
          x_row_name = names(data[as.numeric(corr_data_1)]),
          x_col_name = names(data[as.numeric(corr_data_2)])
        )
      }
      # If tabular
      else if (data_type == 2 && corr_tests == 11) {
        selected_judges <- as.numeric(unlist(strsplit(x = corr_data_2, split = "\\s+")))
        items <- as.numeric(corr_data_1)
        output <- data[, c(selected_judges)]
      }
      
      output
    })
    
    # Render crosstab display
    output$corr_xtab <- renderPrint({
      crosstab_corr_data()
    })
    
    # Render dynamic UI outputs for Use Data mode (corrUI1_data through corrUI6_data)
    output$corrUI1_data <- renderUI({
      corr_tests <- input$corr_tests_data
      corr_data_1 <- input$corr_data_selected_columns_1
      corr_data_2 <- input$corr_data_selected_columns_2
      data <- filtered_data()
      R <- input$decimal_corr_data
      
      req(data, corr_tests)
      
      if (corr_tests == 1) {
        numericInput(inputId = ns("corr_UI1_data"), label = withMathJax("$$\\rho_{0}:{ }$$"), value = 0, width = "750px")
      } else if (corr_tests == 8) {
        numericInput(inputId = ns("corr_UI1_data"), label = withMathJax("$$\\gamma_{0}:{ }$$"), value = 0, width = "750px")
      } else {
        NULL
      }
    })
    
    output$corrUI2_data <- renderUI({
      # Most tests don't need UI2_data
      NULL
    })
    
    output$corrUI3_data <- renderUI({
      # Most tests don't need UI3_data
      NULL
    })
    
    output$corrUI4_data <- renderUI({
      # Most tests don't need UI4_data
      NULL
    })
    
    output$corrUI5_data <- renderUI({
      # Most tests don't need UI5_data
      NULL
    })
    
    # Data structure requirements info
    output$corr_data_info_text <- renderUI({
      corr_data_info <- input$corr_data_info
      corr_tests <- input$corr_tests_data
      req(corr_data_info, corr_tests)
      
      if (corr_data_info == FALSE) {
        HTML("The data for correlation and association tests needs to be structured differently depending on the test. Check the box above to get more information about the data structure for the selected test.")
      } else {
        if (corr_tests == 1 || corr_tests == 2 || corr_tests == 3 || corr_tests == 5 || corr_tests == 6) {
          HTML("Two columns of data, each row being a paired observation.")
        } else if (corr_tests == 4 || corr_tests == 7 || corr_tests == 8 || corr_tests == 9) {
          HTML("<ul><li>Independent - Two columns of data, each row representing an observation.</li><li>Frequency - Three columns of data, one column the row identifier, one column the column identifier, and one column the weight for each combination.</li></ul>")
        } else if (corr_tests == 10) {
          HTML("<ul><li>Independent - Two columns of data, one for each judge, with each line representing their rating for an item.</li><li>Frequency - Three columns of data, one column is Judge 1's assessment, one column is Judge 2's assessment, and one column the weight for each combination.</li></ul>")
        } else if (corr_tests == 11) {
          HTML("<ul><li>Independent - Three columns of data, one identifying the item being rated, one the rater and one for their rating for that item.</li><li>Tabular - Four or more columns of data, one column is the item being rated and each additional column is a rater and their rating for each item. Raters can be added or taken out in the Select Judges box.</li></ul>")
        } else if (corr_tests == 12) {
          HTML("<ul><li>Independent - Two columns of data, one column being the predicted result and one column being the actual result. Each row is a unique test/result pair.</li><li>Frequency - Three columns of data, one column for the predicted result, one column for the actual result, and one column the weight for each combination.</li></ul>")
        } else if (corr_tests == 13) {
          HTML("Independent - Four columns of data: a column of x and a column with their corresponding y, a third column of the second X and a fourth of its y.")
        } else if (corr_tests == 14) {
          HTML("Four columns of data: one column the group designator, two columns of the row and column designator, and a column of the weight for each combination.")
        } else if (corr_tests == 15) {
          HTML("Three columns of data: one column the group designator, a column for each of the two judges. Each line represents a particular item.")
        } else if (corr_tests == 16) {
          HTML("<ul><li>Independent - three columns of data, one column being the group, one the predicted result and one being the actual result. Each row is a unique test/result pair within the group.</li><li>Frequency - Four columns of data, one column for the group, one for the predicted result within each group, one for the actual result within each group, and one column the weight for each combination.</li></ul>")
        } else if (corr_tests == 17) {
          HTML("Three columns of data: two columns of observations and a third column of the variable in common between them.")
        } else {
          HTML("Data structure information for this test.")
        }
      }
    })
    
    # =========================================================================
    # USE DATA MODE - RESULTS RENDERING
    # =========================================================================
    
    # Reactive for Use Data results - round at rendering stage
    corr_data_results_reactive <- reactive({
      corr_tests_data <- input$corr_tests_data
      data_type <- input$corr_data_type
      data <- filtered_data()
      conf <- input$conf_corr_data
      alt <- input$alt_corr_data
      R <- input$decimal_corr_data
      
      # Only require data_type for tests that need it
      tests_needing_data_type <- c(4, 7, 8, 9, 10, 11, 12, 14, 15, 16)
      if (!is.null(corr_tests_data) && length(corr_tests_data) > 0 && corr_tests_data %in% tests_needing_data_type) {
        req(corr_tests_data, data_type, data, conf, alt, R)
      } else if (!is.null(corr_tests_data) && length(corr_tests_data) > 0) {
        req(corr_tests_data, data, conf, alt, R)
      } else {
        return(NULL)
      }
      
      # Get results from server (unrounded)
      results <- correlation_association_worker_result$corr_data_out()
      req(results)
      
      # Round at rendering stage
      ro(results, R)
    })
    
    output$pretty_corr_data <- renderUI({
      resultsR <- corr_data_results_reactive()
      
      # Handle NULL or character results
      if (is.null(resultsR)) {
        return(HTML("<p>No results available. Please check your inputs.</p>"))
      }
      
      if (is.character(resultsR)) {
        return(HTML(paste("<p>", resultsR, "</p>")))
      }
      
      conf <- input$conf_corr_data
      alt <- input$alt_corr_data
      corr_tests <- input$corr_tests_data
      R <- input$decimal_corr_data
      UI1 <- input$corr_UI1_data
      UI3 <- input$corr_UI3_data
      UI4 <- input$corr_UI4_data
      
      req(conf, alt, corr_tests, R)
      
      if (alt == "two.sided") {
        alt_num <- 1
      } else if (alt == "less") {
        alt_num <- 2
      } else if (alt == "greater") {
        alt_num <- 3
      }
      
      beta_statement <- "Power to reject the null if the observed difference was real = "
      
      # Test 1: One-sample Pearson r
      if (corr_tests == 1) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{0}=$"), resultsR$parameter), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r^2 = $"), ro(resultsR$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), resultsR$estimate[3]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho :$")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_data[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste(if (resultsR$parameter == 0) {"t = "} else {"z = "}, resultsR$statistic), 
          if (resultsR$parameter == 0) {paste("df = ", resultsR$estimate[2])} else {""}, "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          paste(beta_statement, 100 * resultsR$estimate[8], "%")
        ))
      }
      # Test 2: One-sample Spearman
      else if (corr_tests == 2) {
        HTML(c(
          paste("<b>One-Sample ", resultsR$method, " Test</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r_s = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{s}=$"), resultsR$parameter), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r_{s}^2 = $"), ro(resultsR$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), resultsR$estimate[2] + 2), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho_{s} :$")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_data[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("t = ", resultsR$statistic), "</td>",
          "<td>", paste("df = ", resultsR$estimate[2]), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 3: Kendall's tau
      else if (corr_tests == 3) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste("Concordant Pairs: ", resultsR$estimate[3]), "</td>",
          "<td>", "</td>",
          "<td>", paste("Discordant Pairs: ", resultsR$estimate[4]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("Count Ties X: ", UI3), "</td>",
          "<td>", "</td>",
          "<td>", paste("Count Ties Y: ", UI4), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$\\tau = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), resultsR$estimate[2]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_data[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("z = ", resultsR$statistic), "</td>",
          "<td>", "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 4: Cramer's V
      else if (corr_tests == 4) {
        xtab <- crosstab_corr_data()
        req(xtab)
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$V =$"), resultsR$statistic), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$E_{min} =$"), resultsR$estimate[3]), "</td>",
          "<td>", if (resultsR$estimate[3] < 5) {paste("<p style='color:red;'> ***Warning ", withMathJax("$E_{min}<5$"), "***</p>")}, "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_data[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste(withMathJax("$\\chi^2 =$"), resultsR$estimate[1]), "</td>",
          "<td>", paste("df = ", (nrow(xtab) - 1) * (ncol(xtab) - 1)), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 5: Biserial correlation
      else if (corr_tests == 5) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r_{bi} = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{bi0}=$"), resultsR$parameter), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r_{bi}^2 = $"), ro(resultsR$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), resultsR$estimate[3]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_data[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("z = ", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 6: Point-biserial correlation
      else if (corr_tests == 6) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r_{pbi} = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{pbi0}=$"), resultsR$parameter), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r_{pbi}^2 = $"), ro(resultsR$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1} = $"), resultsR$estimate[5]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{2} = $"), resultsR$estimate[8]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho_{pbi} :$")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_data[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("t = ", resultsR$statistic), "</td>",
          "<td>", paste("df = ", resultsR$estimate[2]), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 7: Yule's Q
      else if (corr_tests == 7) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$Q = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_data[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("z = ", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 8: Goodman-Kruskal gamma one-sample
      else if (corr_tests == 8) {
        req(UI1)
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$G = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\gamma_{0}=$"), UI1), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{subj} = $"), resultsR$estimate[3]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{con} = $"), resultsR$estimate[4]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{dis} = $"), resultsR$estimate[5]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\gamma :$")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_data[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("z = ", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 9: Tetrachoric correlation
      else if (corr_tests == 9) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r_{t} = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{t0}=$"), resultsR$parameter), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), resultsR$estimate[5]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_data[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("z = ", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 10: Cohen's kappa one-sample
      else if (corr_tests == 10) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$\\kappa = $"), resultsR[["estimate"]][["kappa"]]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\kappa_{0}=$"), resultsR[["parameter"]][["null hypothesis kappa"]]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$\\kappa_{max} = $"), resultsR[["estimate"]][["kappa.max"]]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{agree} = $"), resultsR[["estimate"]][["n.agree"]]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{disagree}=$"), resultsR[["estimate"]][["n.disagree"]]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\kappa :$")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_data[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("z = ", resultsR[["statistic"]][["z"]]), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 11: Kendall's coefficient of concordance
      else if (corr_tests == 11) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$W = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$W'=$"), UI1), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{subj} = $"), resultsR$estimate[2]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{raters} = $"), resultsR$estimate[3]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_data[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste(withMathJax("$\\chi^2=$"), resultsR$statistic), "</td>",
          "<td>", paste("df = ", resultsR$estimate[2] - 1), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 12: J-index one-sample
      else if (corr_tests == 12) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$J = $"), resultsR$statistic), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$J'=0$")), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$J :$")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_data[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("J = ", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 13: Pearson r two-sample independent
      else if (corr_tests == 13) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r_{12} = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$r_{34}=$"), resultsR$estimate[7]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r_{12}^2 = $"), resultsR$estimate[6]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$r_{34}^2 = $"), resultsR$estimate[12]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{12} = $"), resultsR$estimate[2]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{34} = $"), resultsR$estimate[8]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho_{12} :$")), "</td>",
          "<td>", resultsR$estimate[4], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[5], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{34} :$")), "</td>",
          "<td>", resultsR$estimate[10], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[11], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_data[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("z = ", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 14: Goodman-Kruskal gamma two-sample independent
      else if (corr_tests == 14) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$G_{1} = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$G_{2}=$"), resultsR$estimate[3]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$\\sigma_{SE_{1}} = $"), resultsR$estimate[2]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\sigma_{SE_{2}} = $"), resultsR$estimate[4]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence"), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_data[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("z = ", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 15: Cohen's kappa two-sample independent
      else if (corr_tests == 15) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$\\kappa_{1} = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\kappa_{2}=$"), resultsR$estimate[3]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$SE_{\\kappa_{1}} = $"), resultsR$estimate[2]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$SE_{\\kappa_{2}} = $"), resultsR$estimate[4]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence"), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_data[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("z = ", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 16: J-index two-sample
      else if (corr_tests == 16) {
        HTML(c(
          paste("<b> Two-Sample ", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$J_{1} = $"), resultsR$estimate[3]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$J_{2} = $"), resultsR$estimate[7]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$J_{diff} = $"), resultsR$estimate[1]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$J_{diff} :$")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$J_{1} :$")), "</td>",
          "<td>", resultsR$estimate[5], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[6], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$J_{2} :$")), "</td>",
          "<td>", resultsR$estimate[9], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[10], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_data[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("z = ", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 17: Pearson r two-sample dependent
      else if (corr_tests == 17) {
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r_{13} = $"), resultsR$estimate[3]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$r_{23}=$"), resultsR$estimate[7]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r_{12} = $"), resultsR$estimate[11]), "</td>",
          "<td>", "</td>",
          "<td>", "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), 3 + resultsR$estimate[2]), "</td>",
          "<td>", "</td>",
          "<td>", "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r_{13} - r_{23} = $")), resultsR$estimate[1], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho_{13} - \\rho_{23}:$")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{13} :$")), "</td>",
          "<td>", resultsR$estimate[4], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[5], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{23} :$")), "</td>",
          "<td>", resultsR$estimate[8], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[9], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{12} :$")), "</td>",
          "<td>", resultsR$estimate[12], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[13], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_data[3 * (as.numeric(corr_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("t = ", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Placeholder for tests not yet implemented
      else {
        HTML(paste("<p>Results rendering for test", corr_tests, "to be completed</p>"))
      }
    })
    
    # =========================================================================
    # MORE INFORMATION MODALS
    # =========================================================================
    
    # More info observeEvent for Enter Statistics mode
    observeEvent(input$corr_more_info, {
      if (!input$corr_more_info) return()
      
      corr_tests <- input$corr_tests
      req(corr_tests)
      
      if (corr_tests == 1) {
        title <- "One-Sample Pearson r Test"
        text_out <- HTML("The one-sample Pearson r test is used with interval or ratio data to determine if the linear correlation between two variables could be the same as \U1D70C\U2080. If \U1D70C\U2080 = 0, it is testing to see if a linear relationship between the variables is significant at all. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Pearson_correlation_coefficient'>Wikipedia</a>")
      } else if (corr_tests == 2) {
        title <- "One-Sample Spearman r\U209B Test"
        text_out <- HTML("The one-sample Spearman r\U209B is a nonparametric test used when the data are not interval or ratio or cannot be assumed to be linearly related, to determine of there is a significant monotonic relationship between two variables. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient'>Wikipedia</a>")
      } else if (corr_tests == 3) {
        title <- "One-Sample Kendall's \U1D70F Test"
        text_out <- HTML("The one-sample Kendall's \U1D70F nonparametric test is used with ordinal data to determine if there is a significant association between two variables. It does not require that the relationship is monotonic like the Spearman r\U209B. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Kendall_rank_correlation_coefficient'>Wikipedia</a>")
      } else if (corr_tests == 4) {
        title <- "One-Sample Pearson \U1D719 Test"
        text_out <- HTML("The one-sample Pearson \U1D719 coefficient (also known as Matthews Correlation Coefficient when it is used in predicting binary classifications) is used to determine if there is a significant association between two binary variables.</br></br>For more information see <a href='https://en.wikipedia.org/wiki/Phi_coefficient'>Wikipedia</a>")
      } else if (corr_tests == 5) {
        title <- "Two-Sample Pearson r Test"
        text_out <- HTML("The two-sample Pearson r test is used with two sets of interval or ratio data to determine if the linear correlation between the first set and the linear correlation between the second set are the same. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Pearson_correlation_coefficient'>Wikipedia</a>")
      } else if (corr_tests == 6) {
        title <- "Two-Sample Goodman and Kruskall \U1D6FE Test"
        text_out <- HTML("The two-sample Goodman and Kruskall \U1D6FE test is used with two sets of ordinal data to determine if the association between the first set and the association between the second set are the same. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Goodman_and_Kruskal%27s_gamma'>Wikipedia</a>")
      } else if (corr_tests == 7) {
        title <- "Two-Sample Cohen \U1D73F Test"
        text_out <- HTML("The two-sample Cohen \U1D73F test is used compare two rater \U1D73F scores to determine if the two raters can be said to be equally reliable. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Cohen%27s_kappa'>Wikipedia</a>")
      } else if (corr_tests == 8) {
        title <- "Two-Sample Dependent Pearson r Test"
        text_out <- HTML("The two-sample dependent Pearson r test is used with overlapping sets of interval or ratio data to determine if the linear correlation between the first and third variable is the same as the linear correlation between the second and third variable. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Pearson_correlation_coefficient'>Wikipedia</a>")
      } else if (corr_tests == 9) {
        title <- "Cohen \U1D73F Test"
        text_out <- HTML("The Cohen \U1D73F test is used determine the level of agreement above that of chance between two judges across the same items. It can also determine agreement with the real value for one judge.</br></br>For more information see <a href='https://en.wikipedia.org/wiki/Cohen%27s_kappa'>Wikipedia</a>")
      } else {
        title <- "Test Information"
        text_out <- HTML("Information about this test.")
      }
      
      sendSweetAlert(
        title = title,
        text = HTML(text_out),
        html = TRUE,
        showCloseButton = TRUE,
        btn_labels = "Close",
        type = "info"
      )
      updateCheckboxInput(inputId = "corr_more_info", value = FALSE)
    })
    
    # More info observeEvent for Use Data mode
    observeEvent(input$corr_more_info_data, {
      if (!input$corr_more_info_data) return()
      
      corr_tests <- input$corr_tests_data
      req(corr_tests)
      
      if (corr_tests == 1) {
        title <- "One-Sample Pearson r Test"
        text_out <- HTML("The one-sample Pearson r test is used with interval or ratio data to determine if the linear correlation between two variables could be the same as \U1D70C\U2080. If \U1D70C\U2080 = 0, it is testing to see if a linear relationship between the variables is significant at all. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Pearson_correlation_coefficient'>Wikipedia</a>")
      } else if (corr_tests == 2) {
        title <- "One-Sample Spearman r\U209B Test"
        text_out <- HTML("The one-sample Spearman r\U209B is a nonparametric test used when the data are not interval or ratio or cannot be assumed to be linearly related, to determine of there is a significant monotonic relationship between two variables. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient'>Wikipedia</a>")
      } else if (corr_tests == 3) {
        title <- "One-Sample Kendall's \U1D70F Test"
        text_out <- HTML("The one-sample Kendall's \U1D70F nonparametric test is used with ordinal data to determine if there is a significant association between two variables. It does not require that the relationship is monotonic like the Spearman r\U209B. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Kendall_rank_correlation_coefficient'>Wikipedia</a>")
      } else if (corr_tests == 4) {
        title <- "One-Sample Pearson \U1D719 Test"
        text_out <- HTML("The one-sample Pearson \U1D719 coefficient is a special case of Cram\U00E9r's V when you have two nominal variables with two categories, so the more general V is used here. It is used to determine if there is a significant association between two nominal variables with two or more categories.</br></br>For more information see <a href='https://en.wikipedia.org/wiki/Phi_coefficient'>Wikipedia</a>")
      } else if (corr_tests == 5) {
        title <- "Biserial Correlation Coefficient Test"
        text_out <- HTML("The biserial correlation coefficient is used to estimate the correlation between two underlying continuous variables that have both been artificially reduced to dichotomous measures.</br></br>")
      } else if (corr_tests == 6) {
        title <- "Point-Biserial Correlation Coefficient Test"
        text_out <- HTML("The point-biserial correlation coefficient is used to measure the relationship between one continuous variables and one genuinely dichotomous variable.</br></br>For more information see <a href='https://en.wikipedia.org/wiki/Point-biserial_correlation_coefficient'>Wikipedia</a>")
      } else if (corr_tests == 7) {
        title <- "Yule's Q Test"
        text_out <- HTML("Yule's Q is a special case of Goodman and Kruskal's \U1D6FE measure of rank correlation when you have a 2x2 matrix. It is used to measure the strength of association of two dichotomous variables. Yule's Q can have a slightly different interpretation than \U1D6FE since the sign depends on how the researcher sets up the matrix. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Goodman_and_Kruskal%27s_gamma'>Wikipedia</a>")
      } else if (corr_tests == 8) {
        title <- "One-Sample Goodman and Kruskall \U1D6FE Test"
        text_out <- HTML("The one-sample Goodman and Kruskall \U1D6FE test is used to determine the strength of association between two ordinal variables based on the rank correlations. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Goodman_and_Kruskal%27s_gamma'>Wikipedia</a>")
      } else if (corr_tests == 9) {
        title <- "Tetrachoric Correlation Coefficient Test"
        text_out <- HTML("The tetrachoric correlation coefficient is used to estimate the linear relationship between two underlying continuous normally distributed variables that have been artificially reduced to dichotomous variables. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Polychoric_correlation'>Wikipedia</a>")
      } else if (corr_tests == 10) {
        title <- "Cohen's \U1D73F Test"
        text_out <- HTML("Cohen's \U1D73F test is used determine the level of agreement above that of chance between two judges across the same items. It can also determine agreement with the real value for one judge.</br></br>For more information see <a href='https://en.wikipedia.org/wiki/Cohen%27s_kappa'>Wikipedia</a>")
      } else if (corr_tests == 11) {
        title <- "Kendall's Coefficient of Concordance Test"
        text_out <- HTML("Kendall's coefficient of concordance is a measure of agreement among judges' ordinal ratings. It does not measure if the judges give the same rating, rather that units assessed as high by one judge tend to be rated high by the others.</br></br>For more information see <a href='https://en.wikipedia.org/wiki/Kendall%27s_W'>Wikipedia</a>")
      } else if (corr_tests == 12) {
        title <- "J-index of Predictive Efficiency Test"
        text_out <- HTML("The J-index of predictive efficiency is used when a dichotomous test is used to predict a dichotomous outcome, like a pass/fail test predicting failure in the field.</br></br>For more information see <a href='https://en.wikipedia.org/wiki/Youden%27s_J_statistic'>Wikipedia</a>")
      } else if (corr_tests == 13) {
        title <- "Two-Sample Pearson r Test"
        text_out <- HTML("The two-sample Pearson r test is used with two sets of interval or ratio data to determine if the linear correlation between the first set and the linear correlation between the second set are the same. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Pearson_correlation_coefficient'>Wikipedia</a>")
      } else if (corr_tests == 14) {
        title <- "Two-Sample Goodman and Kruskall \U1D6FE Test"
        text_out <- HTML("The two-sample Goodman and Kruskall \U1D6FE test is used with two sets of ordinal data to determine if the association between the first set and the association between the second set are the same. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Goodman_and_Kruskal%27s_gamma'>Wikipedia</a>")
      } else if (corr_tests == 15) {
        title <- "Two-Sample Cohen \U1D73F Test"
        text_out <- HTML("The two-sample Cohen \U1D73F test is used compare two rater \U1D73F scores to determine if the two raters can be said to be equally reliable. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Cohen%27s_kappa'>Wikipedia</a>")
      } else if (corr_tests == 16) {
        title <- "Two-Sample J-index of Predictive Efficiency"
        text_out <- HTML("The two-sample J-index of predictive efficiency is used to compare how efficiently two dichotomous tests are at predicting a dichotomous outcome.</br></br>For more information see <a href='https://en.wikipedia.org/wiki/Youden%27s_J_statistic'>Wikipedia</a>")
      } else if (corr_tests == 17) {
        title <- "Two-Sample Dependent Pearson r Test"
        text_out <- HTML("The two-sample dependent Pearson r test is used with overlapping sets of interval or ratio data to determine if the linear correlation between the first and third variable is the same as the linear correlation between the second and third variable. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Pearson_correlation_coefficient'>Wikipedia</a>")
      } else {
        title <- "Test Information"
        text_out <- HTML("Information about this test.")
      }
      
      sendSweetAlert(
        title = title,
        text = HTML(text_out),
        html = TRUE,
        showCloseButton = TRUE,
        btn_labels = "Close",
        type = "info"
      )
      updateCheckboxInput(inputId = "corr_more_info_data", value = FALSE)
    })
    
    # =========================================================================
    # SCATTERPLOT TAB - WORKER MODULE CALL
    # =========================================================================
    
    # Create input values reactive for scatterplot
    scatterplot_input_values <- reactive({
      list(
        scat_x_sel = input$scat_x_sel,
        scat_y_sel = input$scat_y_sel,
        curve_fit = input$curve_fit,
        conf_scatter = input$conf_scatter,
        scat_ci = input$scat_ci,
        point_ci = input$point_ci,
        y_x_line = input$y_x_line,
        decimal_scat = input$decimal_scat,
        scat_font_size = input$scat_font_size
      )
    })
    
    # Call scatterplot worker module
    scatterplot_worker_result <- create_scatterplot_worker(
      "scatterplot_worker",
      filtered_data,
      scatterplot_input_values,
      choice_corr_alt_text  # Pass choice vector for statistics display
    )
    
    # =========================================================================
    # SCATTERPLOT TAB - UI RENDERING FUNCTIONS
    # =========================================================================
    
    # Render scatterplot column selectors
    output$scat_x <- renderUI({
      data <- filtered_data()
      req(data)
      choices <- seq(1:ncol(data))
      names(choices) <- names(data)
      pickerInput(
        inputId = ns("scat_x_sel"),
        label = "Select x",
        choices = choices,
        options = list(title = "Select x")
      )
    })
    
    output$scat_y <- renderUI({
      data <- filtered_data()
      req(data)
      choices <- seq(1:ncol(data))
      names(choices) <- names(data)
      pickerInput(
        inputId = ns("scat_y_sel"),
        label = "Select y",
        choices = choices,
        options = list(title = "Select y")
      )
    })
    
    # Dynamic UI for model CI (only shown when curve fit is selected)
    output$model_ci <- renderUI({
      fit <- input$curve_fit
      if (is.null(fit) || fit == 0 || fit == "") {
        NULL
      } else {
        prettySwitch(
          inputId = ns("scat_ci"),
          label = "CI for Model",
          value = TRUE,
          status = "success",
          fill = TRUE
        )
      }
    })
    
    # Dynamic UI for point CI (only shown when curve fit is selected)
    output$point_ci <- renderUI({
      fit <- input$curve_fit
      if (is.null(fit) || fit == 0 || fit == "") {
        NULL
      } else {
        prettySwitch(
          inputId = ns("point_ci"),
          label = "CI for Points",
          value = TRUE,
          status = "success",
          fill = TRUE
        )
      }
    })
    
    # =========================================================================
    # SCATTERPLOT TAB - PLOT RENDERING
    # =========================================================================
    
    # Render scatterplot
    output$scatterplot <- renderPlot({
      scatterplot_worker_result$scatter_plot()
    })
    
    # Download server for scatterplot
    scat_width <- reactive(400 * 4)
    scat_height <- reactive(400 * 4)
    downloadServer("scatterplot", scatterplot_worker_result$scatter_plot,
                   height = scat_height, width = scat_width)
    
    # =========================================================================
    # SCATTERPLOT TAB - HOVER INFORMATION
    # =========================================================================
    
    output$hover_info_scat <- renderUI({
      req(input$scat_x_sel, input$scat_y_sel)
      R <- input$decimal_scat
      hover <- input$scat_hover
      if (is.null(hover)) {
        return(NULL)
      }
      
      data <- scatterplot_worker_result$scat_dat()
      req(data)
      
      # Use nearPoints to find the closest point
      point <- nearPoints(df = data, coordinfo = hover, xvar = names(data)[1], yvar = names(data)[2])
      if (nrow(point) == 0) {
        return(NULL)
      }
      
      # Calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$coords_css$x
      top_px <- hover$coords_css$y
      
      # Create style property for tooltip
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # Actual tooltip created as wellPanel
      # Get ro function from global config
      ro <- get_global_config()$ro
      
      # Format point values (point is a single-row data frame from nearPoints)
      point_x_val <- if (nrow(point) > 0) point[[1]] else NULL
      point_y_val <- if (nrow(point) > 0) point[[2]] else NULL
      
      if (is.null(point_x_val) || is.null(point_y_val)) {
        return(NULL)
      }
      
      wellPanel(
        style = style,
        p(HTML(paste0(
          if ("facet" %in% names(point) && !is.null(point$facet)) {
            paste0("<span style='display:block; text-transform:capitalize; text-align:center'>", point$facet, "</span>")
          } else {
            ""
          },
          "<b> ", names(point)[1], ": </b>", ro(point_x_val, R), "<br/>",
          "<b> ", names(point)[2], ": </b>", ro(point_y_val, R), "<br/>"
        )))
      )
    })
    
    # =========================================================================
    # SCATTERPLOT TAB - STATISTICS DISPLAY
    # =========================================================================
    
    output$scatter_plot_stats <- renderUI({
      scatterplot_worker_result$scatter_plot_stats()
    })
    
  })
}
