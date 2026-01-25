# One- and Two-Sample Tests Module
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

# =============================================================================
# CHOICE VECTORS (from original app)
# =============================================================================
choice_sd_alt_1 <- c("two.sided", "less", "greater")
names(choice_sd_alt_1) <- c("\U03C3 of sample is not equal to \U03C3\U2080", 
                           "\U03C3 of sample is less than \U03C3\U2080", 
                           "\U03C3 of sample is greater than \U03C3\U2080")

choice_mean_alt_1 <- c("two.sided", "less", "greater")
names(choice_mean_alt_1) <- c("\U03BC of sample is not equal to \U03BC\U2080", 
                             "\U03BC of sample is less than \U03BC\U2080", 
                             "\U03BC of sample is greater than \U03BC\U2080")

choice_mean_alt_2 <- c("two.sided", "less", "greater")
names(choice_mean_alt_2) <- c("\U03BC\U2081 is not equal to \U03BC\U2082", 
                              "\U03BC\U2081 is less than \U03BC\U2082", 
                              "\U03BC\U2081 is greater than \U03BC\U2082")

choice_mean_alt_3 <- c("two.sided", "less", "greater")
names(choice_mean_alt_3) <- c("\U0394 of sample is not equal to \U0394", 
                              "\U0394 of sample is less than \U0394", 
                              "\U0394 of sample is greater than \U0394")

beta_statement <- "Power to reject the null if the observed difference was real = "

choice_prop_alt_1 <- c("two.sided", "less", "greater")
names(choice_prop_alt_1) <- c("\U03C0 of sample is not equal to \U03C0\U2080",
                             "\U03C0 of sample is less than \U03C0\U2080",
                             "\U03C0 of sample is greater than \U03C0\U2080")

choice_prop_alt_2 <- c("two.sided", "less", "greater")
names(choice_prop_alt_2) <- c("\U03C0\U2081 is not equal to \U03C0\U2082",
                             "\U03C0\U2081 is less than \U03C0\U2082",
                             "\U03C0\U2081 is greater than \U03C0\U2082")

# Temporarily commented out to test if Unicode characters cause issues
# choice_poi_alt_1 <- c("two.sided", "less", "greater")
# names(choice_poi_alt_1) <- c("\U03BB of sample is not equal to \U03BB\U2080",
#                             "\U03BB of sample is less than \U03BB\U2080",
#                             "\U03BB of sample is greater than \U03BB\U2080")
# 
# choice_poi_alt_2 <- c("two.sided", "less", "greater")
# names(choice_poi_alt_2) <- c("\U03BB\U2081 is not equal to \U03BB\U2082",
#                             "\U03BB\U2081 is less than \U03BB\U2082",
#                             "\U03BB\U2081 is greater than \U03BB\U2082")
choice_poi_alt_1 <- c("two.sided", "less", "greater")
names(choice_poi_alt_1) <- c("lambda of sample is not equal to lambda0",
                            "lambda of sample is less than lambda0",
                            "lambda of sample is greater than lambda0")

choice_poi_alt_2 <- c("two.sided", "less", "greater")
names(choice_poi_alt_2) <- c("lambda1 is not equal to lambda2",
                            "lambda1 is less than lambda2",
                            "lambda1 is greater than lambda2")

# Choices for alternative hypothesis nonparametric
choice_np_alt_text <- c(
  "M of sample is not equal to M\U2080", "M of sample is less than M\U2080", "M of sample is greater than M\U2080",
  "M of sample is not equal to M\U2080", "M of sample is less than M", "M of sample is greater than M",
  "M\U2081 is not equal to M\U2082", "M\U2081 is less than M\U2082", "M\U2081 is greater than M\U2082",
  "M\U2081 is not equal to M\U2082", "M\U2081 is less than M\U2082", "M\U2081 of sample is greater than M\U2082",
  "x\U2099 - y\U2099 is not equal to 0", "x\U2099 - y\U2099 is less than 0", "x\U2099 - y\U2099 is greater than 0",
  "x\U2099 - y\U2099 is not equal to 0", "x\U2099 - y\U2099 is less than 0", "x\U2099 - y\U2099 is greater than 0",
  "Pass\U2081 Fail\U2082 is not equal to Fail\U2081 Pass\U2082", "Pass\U2081 Fail\U2082 is less than Fail\U2081 Pass\U2082", "Pass\U2081 Fail\U2082 is greater than Fail\U2081 Pass\U2082"
)

# Source sub-module UI components - Adding back step by step
source("modules/statistical/one_two_sample_tests/ui/test_reference_ui.R")
source("modules/statistical/one_two_sample_tests/ui/means_dispersion_ui.R")
source("modules/statistical/one_two_sample_tests/ui/proportions_ui.R")
source("modules/statistical/one_two_sample_tests/ui/poisson_ui.R")
source("modules/statistical/one_two_sample_tests/ui/nonparametric_ui.R")

# Source sub-module server components
source("modules/statistical/one_two_sample_tests/server/means_dispersion_server.R")
source("modules/statistical/one_two_sample_tests/server/proportions_server.R")
source("modules/statistical/one_two_sample_tests/server/poisson_server.R")
source("modules/statistical/one_two_sample_tests/server/nonparametric_server.R")

# =============================================================================
# COORDINATOR UI FUNCTION
# =============================================================================
create_one_two_sample_tests_ui <- function(id) {
  ns <- NS(id)
  
  navbarMenu(
    title = "One- and Two-Sample Tests",
    
    # Test Reference Tab - Step 1: Simplest component
    tabPanel(
      title = "Test Reference",
      create_test_reference_ui(ns)
    ),
    
    # Means and Dispersion Tab - Step 2
    tabPanel(
      title = "Means and Dispersion",
      create_means_dispersion_ui(ns)
    ),
    
    # Proportion Tab - Step 3
    tabPanel(
      title = "Proportion",
      create_proportions_ui(ns)
    ),
    
    # Poisson Tab - Step 4
    tabPanel(
      title = "Poisson",
      create_poisson_one_two_sample_ui(ns)
    ),
    
    # Nonparametric Tab - Step 5
    tabPanel(
      title = "Nonparametric",
      create_nonparametric_one_two_sample_ui(ns)
    )
    # 
    # # Proportion Tab - Step 3 (commented out for now)
    # tabPanel(
    #   title = "Proportion",
    #   create_proportions_ui(ns)
    # ),
    # 
    # # Poisson Tab - Step 4 (commented out for now)
    # tabPanel(
    #   title = "Poisson",
    #   create_poisson_ui(ns)
    # ),
    # 
    # # Nonparametric Tab - Step 5 (commented out for now)
    # tabPanel(
    #   title = "Nonparametric",
    #   create_nonparametric_ui(ns)
    # )
  )
}

# =============================================================================
# COORDINATOR SERVER FUNCTION
# =============================================================================
create_one_two_sample_tests_server <- function(id, filtered_data, reactive_color_palette) {
  moduleServer(id, function(input, output, session) {
    
    # Get namespace function
    ns <- session$ns
    
    # =========================================================================
    # REGISTER MODULE WITH GLOBAL DATA INVALIDATION SYSTEM
    # =========================================================================
    register_module("one_two_sample_tests_module", 
      ui_reset = function(session) {
        # Reset all One- and Two-Sample Tests UI elements to defaults
        # TODO: Add specific resets as tabs are implemented
      },
      validation = function(data, selections) {
        # Validate One- and Two-Sample Tests selections
        if (is.null(data) || nrow(data) == 0) {
          return(list(valid = FALSE, message = "No data available for One- and Two-Sample Tests analysis"))
        }
        return(list(valid = TRUE, message = ""))
      }
    )
    
    # =========================================================================
    # SUB-MODULE SERVER CALLS
    # =========================================================================
    # Means and Dispersion server
    means_dispersion_result <- create_means_dispersion_server(
      "means_dispersion",
      filtered_data,
      reactive({
        list(
          conf = input$conf,
          md_UI1 = input$md_UI1,
          md_UI2 = input$md_UI2,
          md_UI3 = input$md_UI3,
          md_UI4 = input$md_UI4,
          md_UI5 = input$md_UI5,
          md_UI6 = input$md_UI6,
          md_alt = input$md_alt,
          t_type = input$t_type,
          md_test_num = md_test_num(),
          decimal = input$decimal,
          # Use Data mode inputs
          conf_m_d_data = input$conf_m_d_data,
          decimal_m_d_d = input$decimal_m_d_d,
          md_data_selected_columns = input$md_data_selected_columns,
          md_data_UI1 = input$md_data_UI1,
          md_data_UI2 = input$md_data_UI2,
          md_data_UI3 = input$md_data_UI3,
          md_data_UI4 = input$md_data_UI4,
          one_or_two_md_data = input$one_or_two_md_data,
          t_type_dat = input$t_type_dat,
          data_type_md = input$data_type_md,
          data_choice_ref = input$data_choice_ref,
          data_choice_data = input$data_choice_data,
          data_choice_g1 = input$data_choice_g1,
          data_choice_g2 = input$data_choice_g2,
          data_md_test_num = data_md_test_num(),
          md_t_dep_type = input$md_t_dep_type,
          md_t_dep_type_stat = input$md_t_dep_type_stat
        )
      })
    )
    
    # =========================================================================
    # TEST NUMBER CALCULATION REACTIVES
    # =========================================================================
    # Generate test number for Enter Statistics mode
    md_test_num <- reactive({
      one_or_two <- input$one_or_two
      sigma_known <- input$sigma_known
      dep_or_indep <- input$dep_or_indep
      one_samp_var <- input$one_samp_var
      
      req(one_or_two, sigma_known)
      if (one_or_two == 2) req(dep_or_indep)
      if (one_or_two == 1) req(!is.null(one_samp_var))
      
      output <- NULL
      
      # One sample z
      if (one_or_two == 1 && sigma_known == 2) {
        output <- 1
      }
      # One sample t
      else if (one_or_two == 1 && sigma_known == 1 && one_samp_var == FALSE) {
        output <- 2
      }
      # One sample var
      else if (one_or_two == 1 && sigma_known == 1 && one_samp_var == TRUE) {
        output <- 3
      }
      # Two-sample z - ind
      if (one_or_two == 2 && sigma_known == 2 && dep_or_indep == 1) {
        output <- 4
      }
      # Two-sample t - ind
      else if (one_or_two == 2 && sigma_known == 1 && dep_or_indep == 1) {
        output <- 5
      }
      # Two-sample z - dep
      else if (one_or_two == 2 && sigma_known == 2 && dep_or_indep == 2) {
        output <- 6
      }
      # Two-sample t - dep
      else if (one_or_two == 2 && sigma_known == 1 && dep_or_indep == 2) {
        output <- 7
      }
      
      output
    })
    
    # Generate test number for Use Data mode
    data_md_test_num <- reactive({
      type <- input$data_type_md
      md_data_selected_columns <- input$md_data_selected_columns
      sigma_known_data <- input$sigma_known_data
      dep_or_indep_data <- input$dep_or_indep_data
      one_samp_var_data <- input$one_samp_var_data
      
      # Determine if one or two sample based on number of columns selected
      if (type == 1) {
        # Column mode - check number of columns
        if (is.null(md_data_selected_columns) || length(md_data_selected_columns) == 0) {
          return(NULL)
        }
        is_one_sample <- length(md_data_selected_columns) == 1
      } else {
        # Reference column mode - always two sample (has g1 and g2)
        is_one_sample <- FALSE
      }
      
      req(sigma_known_data)
      if (!is_one_sample) req(dep_or_indep_data)
      if (is_one_sample) req(!is.null(one_samp_var_data))
      
      output_data_md_test_num <- NULL
      
      if (is_one_sample) {
        # One-sample z
        if (sigma_known_data == 2) {
          output_data_md_test_num <- 1
        }
        # One-sample t
        else if (sigma_known_data == 1 && one_samp_var_data == FALSE) {
          output_data_md_test_num <- 2
        }
        # One-sample var
        else if (sigma_known_data == 1 && one_samp_var_data == TRUE) {
          output_data_md_test_num <- 3
        }
      } else {
        # Two-sample z - indep
        if (sigma_known_data == 2 && dep_or_indep_data == 1) {
          output_data_md_test_num <- 4
        }
        # Two-sample t - indep
        else if (sigma_known_data == 1 && dep_or_indep_data == 1) {
          output_data_md_test_num <- 5
        }
        # Two-sample z - dep (not implemented)
        else if (sigma_known_data == 2 && dep_or_indep_data == 2) {
          output_data_md_test_num <- 6
        }
        # Two-sample t - dep
        else if (sigma_known_data == 1 && dep_or_indep_data == 2) {
          output_data_md_test_num <- 7
        }
      }
      
      output_data_md_test_num
    })
    
    # =========================================================================
    # RENDER WORKER OUTPUTS (coordinator handles all rendering)
    # =========================================================================
    # Note: Due to the complexity of Means and Dispersion, most UI rendering
    # is handled directly in the coordinator. The worker only provides calculation results.
    
    # Render Enter Statistics results
    output$pretty_md <- renderUI({
      results <- means_dispersion_result$mean_out()
      
      # Handle NULL or character results
      if (is.null(results)) {
        return(HTML("<p>Please enter all required values and select test options.</p>"))
      }
      
      if (is.character(results)) {
        return(HTML(paste("<p>", results, "</p>")))
      }
      
      conf <- input$conf
      UI1 <- input$md_UI1
      UI2 <- input$md_UI2
      UI3 <- input$md_UI3
      UI4 <- input$md_UI4
      UI5 <- input$md_UI5
      UI6 <- input$md_UI6
      alt <- input$md_alt
      R <- input$decimal
      md_test_num <- md_test_num()
      
      # Check required inputs
      if (is.null(alt) || is.null(md_test_num)) {
        return(HTML("<p>Please select all required options.</p>"))
      }
      
      # Convert alternative to number for choice vector indexing
      if (alt == "two.sided") {
        alt_num <- 1
      } else if (alt == "less") {
        alt_num <- 2
      } else if (alt == "greater") {
        alt_num <- 3
      }
      
      output_html <- NULL
      
      # Test 1: One-sample z-test
      if (md_test_num == 1) {
        output_html <- HTML(c(
          paste("<b>", results$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$\\bar{X} = $"), results$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\mu_{0}=$"), results$parameter), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$\\sigma = $"), ro(UI3, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), UI5), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\mu :$")), "</td>",
          "<td>", results$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", results$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", names(choice_mean_alt_1[alt_num]), ": "), "</td>",
          "<td>", paste("z = ", results$statistic, "&emsp;"), "</td>",
          "<td>", paste("p = ", results$p.value, if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          paste(beta_statement, 100 * results$estimate[4], "%")
        ))
      }
      # Test 2: One-sample t-test
      else if (md_test_num == 2) {
        output_html <- HTML(c(
          paste("<b>", results$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$\\bar{X} = $"), results$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\mu_{0}=$"), results$parameter), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$s = $"), ro(UI3, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), UI5), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\mu :$")), "</td>",
          "<td>", results$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", results$conf.int[2], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\sigma :$")), "</td>",
          "<td>", results$estimate[7], "</td>",
          "<td>", " to ", "</td>",
          "<td>", results$estimate[9], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", names(choice_mean_alt_1[alt_num]), ": "), "</td>",
          "<td>", paste("t = ", results$statistic, "&emsp;"), "</td>",
          "<td>", "df =", results$estimate[3], "</td>",
          "<td>", paste("p = ", results$p.value, if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          paste(beta_statement, 100 * results$estimate[10], "%")
        ))
      }
      # Test 3: One-sample variance test
      else if (md_test_num == 3) {
        output_html <- HTML(c(
          paste("<b>", results$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$s = $"), ro(UI1, R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\sigma_{0}=$"), ro(UI2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), results$estimate[3]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\sigma :$")), "</td>",
          "<td>", ro(results$conf.int[1]^0.5, R), "</td>",
          "<td>", " to ", "</td>",
          "<td>", ro(results$conf.int[2]^0.5, R), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", names(choice_sd_alt_1[alt_num]), ": "), "</td>",
          "<td>", paste(withMathJax("$\\chi^2 = $"), results$statistic), "</td>",
          "<td>", paste("df = ", results$estimate[2]), "</td>",
          "<td>", paste("p = ", results$p.value, if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          paste(beta_statement, 100 * results$estimate[4], "%")
        ))
      }
      # Test 4: Two-sample z-test independent
      else if (md_test_num == 4) {
        output_html <- HTML(c(
          paste("<b>", results$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$\\bar{X}_{1} = $"), results$estimate[3]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\bar{X}_{2}=$"), results$estimate[7]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$\\sigma_{1} = $"), ro(UI3, R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\sigma_{2} = $"), ro(UI4, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1} = $"), UI5), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{2} = $"), UI6), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\mu_{1} :$")), "</td>",
          "<td>", results$estimate[4], "</td>",
          "<td>", " to ", "</td>",
          "<td>", results$estimate[5], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\mu_{2} :$")), "</td>",
          "<td>", results$estimate[8], "</td>",
          "<td>", " to ", "</td>",
          "<td>", results$estimate[9], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\mu_{1}-\\mu_{2} :$")), "</td>",
          "<td>", results$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", results$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", names(choice_mean_alt_2[alt_num]), ": "), "</td>",
          "<td>", paste("z = ", results$statistic, "&emsp;"), "</td>",
          "<td>", paste("p = ", results$p.value, if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 5: Two-sample t-test independent
      else if (md_test_num == 5) {
        output_html <- HTML(c(
          paste("<b>", results$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$\\bar{X}_{1} = $"), results$estimate[4]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\bar{X}_{2}=$"), results$estimate[14]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$s_{1} = $"), ro(UI3, R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$s_{2} = $"), ro(UI4, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1} = $"), UI5), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{2} = $"), UI6), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\mu_{1} :$")), "</td>",
          "<td>", results$estimate[5], "</td>",
          "<td>", " to ", "</td>",
          "<td>", results$estimate[6], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\mu_{2} :$")), "</td>",
          "<td>", results$estimate[15], "</td>",
          "<td>", " to ", "</td>",
          "<td>", results$estimate[16], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\sigma_{1} :$")), "</td>",
          "<td>", results$estimate[12], "</td>",
          "<td>", " to ", "</td>",
          "<td>", results$estimate[13], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\sigma_{2} :$")), "</td>",
          "<td>", results$estimate[22], "</td>",
          "<td>", " to ", "</td>",
          "<td>", results$estimate[23], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\mu_{1}-\\mu_{2} :$")), "</td>",
          "<td>", results$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", results$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", withMathJax("$\\sigma_{1}^2=\\sigma_{2}^2$"), ": "), "</td>",
          "<td>", paste("F = "), results$estimate[25], "</td>",
          "<td>", "df =", results$estimate[26], "/", results$estimate[27], "</td>",
          "<td>", paste("p = ", results$estimate[28], if (results$estimate[28] < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("Test for ", names(choice_mean_alt_2[alt_num]), ": "), "</td>",
          "<td>", paste("t = ", results$statistic), "</td>",
          "<td>", "df =", results$estimate[3], "</td>",
          "<td>", paste("p = ", results$p.value, if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 6: Two-sample z dependent (not applicable)
      else if (md_test_num == 6) {
        output_html <- NULL
      }
      # Test 7: Two-sample t-test dependent
      else if (md_test_num == 7) {
        t_dep_type <- input$md_t_dep_type_stat
        req(t_dep_type)
        
        if (t_dep_type == 1) {
          # d-bar type
          output_html <- HTML(c(
            paste("<b>", results$method, "</b>"),
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste(withMathJax("$\\bar{D} = $"), results[["estimate"]][["sample.mean"]]), "</td>",
            "<td>", "</td>",
            "<td>", paste(withMathJax("$\\Delta=$"), results[["parameter"]][["null hypothesis mean"]]), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste(withMathJax("$s_{D} = $"), results[["estimate"]][["sd"]]), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste(withMathJax("$n = $"), results[["estimate"]][["df"]] + 1), "</td>",
            "</tr>",
            "</table>",
            "<table>",
            "<tr>",
            "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
            "<td>", paste(withMathJax("$\\Delta :$")), "</td>",
            "<td>", results[["conf.int"]][1], "</td>",
            "<td>", " to ", "</td>",
            "<td>", results[["conf.int"]][2], "</td>",
            "</tr>",
            "<tr>",
            "<td>", "</td>",
            "<td>", paste(withMathJax("$\\sigma_{D} :$")), "</td>",
            "<td>", results[["estimate"]][["sd.lowerci"]], "</td>",
            "<td>", " to ", "</td>",
            "<td>", results[["estimate"]][["sd.upperci"]], "</td>",
            "</tr>",
            "</table>",
            "<br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\bar{D}$ Test for ", names(choice_mean_alt_3[alt_num]), ": "), "</td>",
            "<td>", paste("t = ", results[["statistic"]][["t statistic"]]), "</td>",
            "<td>", paste("df =", results[["estimate"]][["df"]]), "</td>",
            "<td>", paste("p = ", results[["p.value"]], if (results[["p.value"]] < 1 - conf) {"*"}), "</td>",
            "</tr>",
            "</table>"
          ))
        } else if (t_dep_type == 2) {
          # Mean difference type
          req(UI4, UI5, UI6)
          
          # Correlation test
          corr_test <- cor.pearson.r.onesample.simple(
            sample.r = UI6,
            sample.size = UI5,
            null.hypothesis.rho = 0,
            conf.level = conf
          )
          corr_test <- ro(corr_test, R)
          
          output_html <- HTML(c(
            paste("<b>", results$method, "</b>"),
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste(withMathJax("$\\bar{X}_{1} = $"), results$estimate[5]), "</td>",
            "<td>", "</td>",
            "<td>", paste(withMathJax("$\\bar{X}_{2}=$"), results$estimate[14]), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste(withMathJax("$s_{1} = $"), ro(UI3, R)), "</td>",
            "<td>", "</td>",
            "<td>", paste(withMathJax("$s_{2} = $"), ro(UI4, R)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste(withMathJax("$n = $"), UI5), "</td>",
            "<td>", "</td>",
            "<td>", paste(withMathJax("$r_{xy} = $"), UI6), "</td>",
            "</tr>",
            "</table>",
            "<table>",
            "<tr>",
            "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
            "<td>", paste(withMathJax("$\\mu_{1} :$")), "</td>",
            "<td>", results$estimate[6], "</td>",
            "<td>", " to ", "</td>",
            "<td>", results$estimate[7], "</td>",
            "</tr>",
            "<tr>",
            "<td>", "</td>",
            "<td>", paste(withMathJax("$\\mu_{2} :$")), "</td>",
            "<td>", results$estimate[15], "</td>",
            "<td>", " to ", "</td>",
            "<td>", results$estimate[16], "</td>",
            "</tr>",
            "<tr>",
            "<td>", "</td>",
            "<td>", paste(withMathJax("$\\sigma_{1} :$")), "</td>",
            "<td>", results$estimate[12], "</td>",
            "<td>", " to ", "</td>",
            "<td>", results$estimate[13], "</td>",
            "</tr>",
            "<tr>",
            "<td>", "</td>",
            "<td>", paste(withMathJax("$\\sigma_{2} :$")), "</td>",
            "<td>", results$estimate[21], "</td>",
            "<td>", " to ", "</td>",
            "<td>", results$estimate[22], "</td>",
            "</tr>",
            "<tr>",
            "<td>", "</td>",
            "<td>", paste(withMathJax("$\\mu_{1}-\\mu_{2} :$")), "</td>",
            "<td>", results$conf.int[1], "</td>",
            "<td>", " to ", "</td>",
            "<td>", results$conf.int[2], "</td>",
            "</tr>",
            "</table>",
            "<table>",
            "<tr>",
            "<td>", paste("Test for ", withMathJax("$\\rho=0$"), ": "), "</td>",
            "<td>", paste("t = "), corr_test$statistic, "</td>",
            "<td>", "df =", corr_test$estimate[2], "</td>",
            "<td>", paste("p = ", corr_test$p.value, if (corr_test$p.value < 1 - conf) {"*"}), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("Dependent test for ", withMathJax("$\\sigma_{1}^2=\\sigma_{2}^2$"), ": "), "</td>",
            "<td>", paste("t = "), results$estimate[24], "</td>",
            "<td>", "df =", results$estimate[25], "</td>",
            "<td>", paste("p = ", results$estimate[26], if (results$estimate[26] < 1 - conf) {"*"}), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("Test for ", names(choice_mean_alt_2[alt_num]), ": "), "</td>",
            "<td>", paste("t = ", results$statistic), "</td>",
            "<td>", "df =", results$estimate[3], "</td>",
            "<td>", paste("p = ", results$p.value, if (results$p.value < 1 - conf) {"*"}), "</td>",
            "</tr>",
            "</table>"
          ))
        }
      }
      
      output_html
    })
    
    # Render Use Data results
    output$pretty_md_data <- renderUI({
      results <- means_dispersion_result$m_d_data_out()
      
      # Handle NULL or character results
      if (is.null(results)) {
        return(HTML("<p>Please select data columns and enter all required values.</p>"))
      }
      
      if (is.character(results)) {
        return(HTML(paste("<p>", results, "</p>")))
      }
      
      conf <- input$conf_m_d_data
      UI1 <- input$md_data_UI1
      UI2 <- input$md_data_UI2
      UI3 <- input$md_data_UI3
      UI4 <- input$md_data_UI4
      UI5 <- input$md_data_UI5
      UI6 <- input$md_data_UI6
      alt <- input$one_or_two_md_data
      R <- input$decimal_m_d_d
      test_num <- data_md_test_num()
      md_data_selected_columns <- input$md_data_selected_columns
      data <- filtered_data()
      type <- input$data_type_md
      
      # Check required inputs
      if (is.null(alt) || is.null(test_num)) {
        return(HTML("<p>Please select all required options.</p>"))
      }
      
      # Convert alternative to number
      if (alt == "two.sided") {
        alt_num <- 1
      } else if (alt == "less") {
        alt_num <- 2
      } else if (alt == "greater") {
        alt_num <- 3
      }
      
      # Handle results structure - test 5 returns a list with main, levene, adm_n1
      # Other tests return the results directly
      # Note: Rounding only occurs during table rendering, not here
      if (is.list(results) && "main" %in% names(results)) {
        # Test 5 - extract components (all unrounded - rounding happens during table rendering)
        main_results <- results$main
        levene <- results$levene
        adm_n1 <- results$adm_n1
        # Create rounded version of main results for display
        resultsR <- ro(main_results, R)
      } else {
        # Other tests - use results directly (unrounded)
        main_results <- results
        levene <- NULL
        adm_n1 <- NULL
        # Create rounded version for display
        resultsR <- ro(results, R)
      }
      
      output_html <- NULL
      
      # Test 1: One-sample z-test
      if (test_num == 1) {
        output_html <- HTML(c(
          paste("<b>", results$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$\\bar{X} = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\mu_{0}=$"), resultsR$parameter), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$\\sigma = $"), ro(UI3, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), resultsR$estimate[2]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\mu :$")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", names(choice_mean_alt_1[alt_num]), ": "), "</td>",
          "<td>", paste("z = ", resultsR$statistic, "&emsp;"), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          paste(beta_statement, 100 * resultsR$estimate[4], "%")
        ))
      }
      # Test 2: One-sample t-test
      else if (test_num == 2) {
        output_html <- HTML(c(
          paste("<b>", results$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$\\bar{X} = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\mu_{0}=$"), resultsR$parameter), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$s = $"), resultsR$estimate[8]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), resultsR$estimate[3] + 1), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\mu :$")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\sigma :$")), "</td>",
          "<td>", resultsR$estimate[7], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[9], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", names(choice_mean_alt_1[alt_num]), ": "), "</td>",
          "<td>", paste("t = ", resultsR$statistic, "&emsp;"), "</td>",
          "<td>", "df =", resultsR$estimate[3], "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          paste(beta_statement, 100 * resultsR$estimate[10], "%")
        ))
      }
      # Test 3: One-sample variance test
      else if (test_num == 3) {
        output_html <- HTML(c(
          paste("<b>", results$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$s = $"), ro(results$estimate[1]^0.5, R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\sigma_{0}=$"), ro(UI2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), ro(results$estimate[3], R)), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\sigma :$")), "</td>",
          "<td>", ro(results$conf.int[1]^0.5, R), "</td>",
          "<td>", " to ", "</td>",
          "<td>", ro(results$conf.int[2]^0.5, R), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", names(choice_sd_alt_1[alt_num]), ": "), "</td>",
          "<td>", paste(withMathJax("$\\chi^2 = $"), ro(results$statistic, R)), "</td>",
          "<td>", paste("df = ", ro(results$estimate[2], R)), "</td>",
          "<td>", paste("p = ", ro(results$p.value, R), if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          paste(beta_statement, 100 * ro(results$estimate[4], R), "%")
        ))
      }
      # Test 4: Two-sample z-test independent
      else if (test_num == 4) {
        if (type == 1) {
          group1_name <- names(data)[as.numeric(md_data_selected_columns[1])]
          group2_name <- names(data)[as.numeric(md_data_selected_columns[2])]
        } else {
          group1_name <- paste("Group 1 = ", input$data_choice_g1)
          group2_name <- paste("Group 2 = ", input$data_choice_g2)
        }
        
        output_html <- HTML(c(
          paste("<b>", results$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr><td style='border-bottom:1px solid #000'>", group1_name, "</td><td style='border-bottom:1px solid #000'></td>",
          "<td style='border-bottom:1px solid #000'>", group2_name, "</td></tr>",
          "<tr>",
          "<td>", paste(withMathJax("$\\bar{X}_{1} = $"), resultsR$estimate[3]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\bar{X}_{2}=$"), resultsR$estimate[7]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$\\sigma_{1} = $"), ro(UI3, R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\sigma_{2} = $"), ro(UI4, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1} = $"), resultsR$estimate[6]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{2} = $"), resultsR$estimate[10]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\mu_{1} :$")), "</td>",
          "<td>", resultsR$estimate[4], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[5], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\mu_{2} :$")), "</td>",
          "<td>", resultsR$estimate[8], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[9], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\mu_{1}-\\mu_{2} :$")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", names(choice_mean_alt_2[alt_num]), ": "), "</td>",
          "<td>", paste("z = ", resultsR$statistic, "&emsp;"), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 5: Two-sample t-test independent
      else if (test_num == 5) {
        req(data, type)
        
        # Get group names for display
        if (type == 1) {
          req(md_data_selected_columns)
          group1_name <- names(data)[as.numeric(md_data_selected_columns[1])]
          group2_name <- names(data)[as.numeric(md_data_selected_columns[2])]
        } else {
          group1_name <- paste("Group 1 = ", input$data_choice_g1)
          group2_name <- paste("Group 2 = ", input$data_choice_g2)
        }
        
        output_html <- HTML(c(
          paste("<b>", results$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr><td style='border-bottom:1px solid #000'>", group1_name, "</td><td style='border-bottom:1px solid #000'></td>",
          "<td style='border-bottom:1px solid #000'>", group2_name, "</td></tr>",
          "<tr>",
          "<td>", paste(withMathJax("$\\bar{X}_{1} = $"), resultsR$estimate[4]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\bar{X}_{2}=$"), resultsR$estimate[14]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$s_{1} = $"), resultsR$estimate[11]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$s_{2} = $"), resultsR$estimate[21]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1} = $"), resultsR$estimate[7]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{2} = $"), resultsR$estimate[17]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\mu_{1} :$")), "</td>",
          "<td>", resultsR$estimate[5], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[6], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\mu_{2} :$")), "</td>",
          "<td>", resultsR$estimate[15], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[16], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\sigma_{1} :$")), "</td>",
          "<td>", resultsR$estimate[12], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[13], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\sigma_{2} :$")), "</td>",
          "<td>", resultsR$estimate[22], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[23], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\mu_{1}-\\mu_{2} :$")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Tests for ", withMathJax("$\\sigma_{1}^2=\\sigma_{2}^2$"), ": "), "</td>",
          "</tr>",
          "<tr>",
          "<td align='right'>", "F-test", "</td>",
          "<td>", paste("F = "), resultsR$estimate[25], "</td>",
          "<td>", "df =", resultsR$estimate[26], "/", resultsR$estimate[27], "</td>",
          "<td>", paste("p = ", resultsR$estimate[28], if (resultsR$estimate[28] < 1 - conf) {"*"}), "</td>",
          "</tr>",
          if (!is.null(levene)) c(
            "<tr>",
            "<td align='right'>", "Levene's Test (ADA)", "</td>",
            "<td>", paste("t = ", ro(levene$statistic, R)), "</td>",
            "<td>", paste("df = ", ro(levene$estimate[3], R)), "</td>",
            "<td>", paste("p = ", ro(levene$p.value, R), if (levene$p.value < 1 - conf) {"*"}), "</td>",
            "</tr>"
          ),
          if (!is.null(adm_n1)) c(
            "<tr>",
            "<td align='right'>", paste("ADM", withMathJax("$_{n-1} $")), "</td>",
            "<td>", paste("t = ", ro(adm_n1$statistic, R)), "</td>",
            "<td>", paste("df = ", ro(adm_n1$estimate[3], R)), "</td>",
            "<td>", paste("p = ", ro(adm_n1$p.value, R), if (adm_n1$p.value < 1 - conf) {"*"}), "</td>",
            "</tr>"
          ),
          "<tr>",
          "<td>", paste("Test for ", names(choice_mean_alt_2[alt_num]), ": "), "</td>",
          "<td>", paste("t = ", resultsR$statistic), "</td>",
          "<td>", "df =", resultsR$estimate[3], "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 7: Two-sample t-test dependent
      else if (test_num == 7) {
        dep_t_type <- input$md_t_dep_type
        req(dep_t_type)
        
        if (dep_t_type == 1) {
          # d-bar type
          output_html <- HTML(c(
            paste("<b>", results$method, "</b>"),
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste(withMathJax("$\\bar{D} = $"), resultsR[["estimate"]][["sample.mean"]]), "</td>",
            "<td>", "</td>",
            "<td>", paste(withMathJax("$\\Delta=$"), resultsR[["parameter"]][["null hypothesis mean"]]), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste(withMathJax("$s_{D} = $"), resultsR[["estimate"]][["sd"]]), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste(withMathJax("$n = $"), resultsR[["estimate"]][["df"]] + 1), "</td>",
            "</tr>",
            "</table>",
            "<table>",
            "<tr>",
            "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
            "<td>", paste(withMathJax("$\\Delta :$")), "</td>",
            "<td>", resultsR[["conf.int"]][1], "</td>",
            "<td>", " to ", "</td>",
            "<td>", resultsR[["conf.int"]][2], "</td>",
            "</tr>",
            "<tr>",
            "<td>", "</td>",
            "<td>", paste(withMathJax("$\\sigma_{D} :$")), "</td>",
            "<td>", resultsR[["estimate"]][["sd.lowerci"]], "</td>",
            "<td>", " to ", "</td>",
            "<td>", resultsR[["estimate"]][["sd.upperci"]], "</td>",
            "</tr>",
            "</table>",
            "<br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\bar{D}$ Test for ", names(choice_mean_alt_3[alt_num]), ": "), "</td>",
            "<td>", paste("t = ", resultsR[["statistic"]][["t statistic"]]), "</td>",
            "<td>", paste("df =", resultsR[["estimate"]][["df"]]), "</td>",
            "<td>", paste("p = ", resultsR[["p.value"]], if (resultsR[["p.value"]] < 1 - conf) {"*"}), "</td>",
            "</tr>",
            "</table>"
          ))
        } else if (dep_t_type == 2) {
          # Mean difference type - similar to test 5 but with correlation test
          if (type == 1) {
            group1_name <- names(data)[as.numeric(md_data_selected_columns[1])]
            group2_name <- names(data)[as.numeric(md_data_selected_columns[2])]
          } else {
            group1_name <- paste("Group 1 = ", input$data_choice_g1)
            group2_name <- paste("Group 2 = ", input$data_choice_g2)
          }
          
          # Correlation test - calculate from actual data
          if (type == 1) {
            req(md_data_selected_columns, length(md_data_selected_columns) >= 2, data)
            ave_data <- na.omit(data.frame(g1 = data[, as.numeric(md_data_selected_columns[1])], 
                                             g2 = data[, as.numeric(md_data_selected_columns[2])]))
            req(nrow(ave_data) > 0)
            rho_val <- cor(ave_data$g1, ave_data$g2)
            n_val <- nrow(ave_data)
          } else {
            ref_col <- as.numeric(input$data_choice_ref)
            data_col <- as.numeric(input$data_choice_data)
            g1_col <- input$data_choice_g1
            g2_col <- input$data_choice_g2
            req(ref_col, data_col, g1_col, g2_col, data,
                data_col > 0, data_col <= ncol(data),
                ref_col > 0, ref_col <= ncol(data))
            temp1 <- data[[data_col]][which(data[[ref_col]] == g1_col)]
            temp2 <- data[[data_col]][which(data[[ref_col]] == g2_col)]
            req(length(temp1) > 0, length(temp2) > 0,
                is.numeric(temp1) || is.logical(temp1),
                is.numeric(temp2) || is.logical(temp2))
            temp <- na.omit(data.frame(temp1, temp2))
            req(nrow(temp) > 0)
            rho_val <- cor(temp$temp1, temp$temp2)
            n_val <- nrow(temp)
          }
          
          corr_test <- cor.pearson.r.onesample.simple(
            sample.r = rho_val,
            sample.size = n_val,
            null.hypothesis.rho = 0,
            conf.level = conf
          )
          corr_test <- ro(corr_test, R)
          
          output_html <- HTML(c(
            paste("<b>", results$method, "</b>"),
            "<br><br>",
            "<table>",
            "<tr><td style='border-bottom:1px solid #000'>", group1_name, "</td><td style='border-bottom:1px solid #000'></td>",
            "<td style='border-bottom:1px solid #000'>", group2_name, "</td></tr>",
            "<tr>",
            "<td>", paste(withMathJax("$\\bar{X}_{1} = $"), resultsR$estimate[5]), "</td>",
            "<td>", "</td>",
            "<td>", paste(withMathJax("$\\bar{X}_{2}=$"), resultsR$estimate[14]), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste(withMathJax("$s_{1} = $"), resultsR$estimate[11]), "</td>",
            "<td>", "</td>",
            "<td>", paste(withMathJax("$s_{2} = $"), resultsR$estimate[21]), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste(withMathJax("$n = $"), n_val), "</td>",
            "<td>", "</td>",
            "<td>", paste(withMathJax("$r_{xy} = $"), ro(rho_val, R)), "</td>",
            "</tr>",
            "</table>",
            "<table>",
            "<tr>",
            "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
            "<td>", paste(withMathJax("$\\mu_{1} :$")), "</td>",
            "<td>", resultsR$estimate[6], "</td>",
            "<td>", " to ", "</td>",
            "<td>", resultsR$estimate[7], "</td>",
            "</tr>",
            "<tr>",
            "<td>", "</td>",
            "<td>", paste(withMathJax("$\\mu_{2} :$")), "</td>",
            "<td>", resultsR$estimate[15], "</td>",
            "<td>", " to ", "</td>",
            "<td>", resultsR$estimate[16], "</td>",
            "</tr>",
            "<tr>",
            "<td>", "</td>",
            "<td>", paste(withMathJax("$\\mu_{1}-\\mu_{2} :$")), "</td>",
            "<td>", resultsR$conf.int[1], "</td>",
            "<td>", " to ", "</td>",
            "<td>", resultsR$conf.int[2], "</td>",
            "</tr>",
            "</table>",
            "<table>",
            "<tr>",
            "<td>", paste("Test for ", withMathJax("$\\rho=0$"), ": "), "</td>",
            "<td>", paste("t = "), corr_test$statistic, "</td>",
            "<td>", "df =", corr_test$estimate[2], "</td>",
            "<td>", paste("p = ", corr_test$p.value, if (corr_test$p.value < 1 - conf) {"*"}), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("Test for ", names(choice_mean_alt_2[alt_num]), ": "), "</td>",
            "<td>", paste("t = ", resultsR$statistic), "</td>",
            "<td>", "df =", resultsR$estimate[3], "</td>",
            "<td>", paste("p = ", resultsR$p.value, if (results$p.value < 1 - conf) {"*"}), "</td>",
            "</tr>",
            "</table>"
          ))
        }
      }
      
      output_html
    })
    
    # =========================================================================
    # MEANS AND DISPERSION UI RENDERING FUNCTIONS
    # =========================================================================
    
    # One-sample variance checkbox
    output$onesample_md <- renderUI({
      one_or_two <- input$one_or_two
      sigma_known <- input$sigma_known
      req(one_or_two, sigma_known)
      
      if (one_or_two == 1 && sigma_known == 1) {
        checkboxInput(
          inputId = ns("one_samp_var"),
          label = "One-sample test for variance?",
          value = FALSE
        )
      } else {
        NULL
      }
    })
    
    # Alternative hypothesis selection
    output$alt_md <- renderUI({
      md_test_num <- md_test_num()
      req(md_test_num)
      
      if (md_test_num > 0 && md_test_num < 3) {
        selectInput(
          inputId = ns("md_alt"),
          label = "Alternative Hypothesis for Means",
          choices = choice_mean_alt_1
        )
      } else if (md_test_num == 3) {
        selectInput(
          inputId = ns("md_alt"),
          label = "Alternative Hypothesis for Variance",
          choices = choice_sd_alt_1
        )
      } else if (md_test_num > 3 && md_test_num < 7) {
        selectInput(
          inputId = ns("md_alt"),
          label = "Alternative Hypothesis for Means",
          choices = choice_mean_alt_2
        )
      } else if (md_test_num == 7) {
        dep_t_type <- input$md_t_dep_type_stat
        req(dep_t_type)
        if (dep_t_type == 1) {
          selectInput(
            inputId = ns("md_alt"),
            label = "Alternative Hypothesis for Means",
            choices = choice_mean_alt_3
          )
        } else if (dep_t_type == 2) {
          selectInput(
            inputId = ns("md_alt"),
            label = "Alternative Hypothesis for Means",
            choices = choice_mean_alt_2
          )
        }
      }
    })
    
    # Dependent/Independent selection
    output$md_dep_or_indep <- renderUI({
      one_or_two <- input$one_or_two
      req(one_or_two)
      
      if (one_or_two == 2) {
        selectInput(
          inputId = ns("dep_or_indep"),
          label = "Independent or Dependent Test?",
          choices = c("Independent" = 1, "Dependent" = 2)
        )
      } else {
        NULL
      }
    })
    
    # Sigma known selection
    output$md_sig_known <- renderUI({
      req(input$one_or_two)
      if (input$one_or_two == 1) {
        dep_or_indep <- 1
      } else {
        dep_or_indep <- input$dep_or_indep
      }
      
      req(dep_or_indep)
      
      if (dep_or_indep == 1) {
        radioButtons(
          inputId = ns("sigma_known"),
          label = "Is σ definitively known?",
          choices = c("No, use t" = 1, "Yes, use z" = 2)
        )
      } else {
        radioButtons(
          inputId = ns("sigma_known"),
          label = "Is σ definitively known?",
          choices = c("No, use t" = 1)
        )
      }
    })
    
    # Dependent test type selection
    output$md_t_dep_stat <- renderUI({
      test_num <- md_test_num()
      req(test_num)
      
      if (test_num == 7) {
        radioButtons(
          inputId = ns("md_t_dep_type_stat"),
          label = "Dependency by",
          choices = c("Nature (D̄)" = 1, "Design (Mean Difference)" = 2)
        )
      } else {
        NULL
      }
    })
    
    # T-test type selection
    output$md_t_type_stat <- renderUI({
      test_num <- md_test_num()
      req(test_num)
      
      if (test_num == 5) {
        radioButtons(
          inputId = ns("t_type"),
          label = "Select:",
          choices = c(
            "Welch (recommended)" = "no",
            "Student (unknown but equal variance)" = "yes",
            "Choose based on variance test (not recommended)" = "auto"
          )
        )
      } else {
        NULL
      }
    })
    
    # Dynamic UI inputs - mdUI1 through mdUI6
    output$mdUI1 <- renderUI({
      md_test_num <- md_test_num()
      req(md_test_num)
      
      if (md_test_num == 1 || md_test_num == 2) {
        numericInput(
          inputId = ns("md_UI1"),
          label = withMathJax("$$\\bar{X}:{ }$$"),
          value = 10,
          width = "150px"
        )
      } else if (md_test_num == 3) {
        numericInput(
          inputId = ns("md_UI1"),
          label = withMathJax("$$s:{ }$$"),
          value = 1,
          min = 0,
          step = 1,
          width = "150px"
        )
      } else if (md_test_num > 3 && md_test_num < 7) {
        numericInput(
          inputId = ns("md_UI1"),
          label = withMathJax("$$\\bar{X}_{1}:{ }$$"),
          value = 10,
          width = "150px"
        )
      } else if (md_test_num == 7) {
        dep_t_type <- input$md_t_dep_type_stat
        req(dep_t_type)
        if (dep_t_type == 1) {
          numericInput(
            inputId = ns("md_UI1"),
            label = withMathJax("$$\\bar{D}:{ }$$"),
            value = 1,
            width = "150px"
          )
        } else if (dep_t_type == 2) {
          numericInput(
            inputId = ns("md_UI1"),
            label = withMathJax("$$\\bar{X}_{1}:{ }$$"),
            value = 10,
            width = "150px"
          )
        }
      }
    })
    
    output$mdUI2 <- renderUI({
      md_test_num <- md_test_num()
      req(md_test_num)
      
      if (md_test_num == 1 || md_test_num == 2) {
        numericInput(
          inputId = ns("md_UI2"),
          label = withMathJax("$$\\mu_{0}:{ }$$"),
          value = 12,
          width = "150px"
        )
      } else if (md_test_num == 3) {
        numericInput(
          inputId = ns("md_UI2"),
          label = withMathJax("$$\\sigma_{0}:{ }$$"),
          value = 12,
          width = "150px"
        )
      } else if (md_test_num > 3 && md_test_num < 7) {
        numericInput(
          inputId = ns("md_UI2"),
          label = withMathJax("$$\\bar{X}_{2}:{ }$$"),
          value = 12,
          width = "150px"
        )
      } else if (md_test_num == 7) {
        dep_t_type <- input$md_t_dep_type_stat
        req(dep_t_type)
        if (dep_t_type == 1) {
          numericInput(
            inputId = ns("md_UI2"),
            label = withMathJax("$$\\Delta:{ }$$"),
            value = 0,
            width = "150px"
          )
        } else if (dep_t_type == 2) {
          numericInput(
            inputId = ns("md_UI2"),
            label = withMathJax("$$\\bar{X}_{2}:{ }$$"),
            value = 12,
            width = "150px"
          )
        }
      }
    })
    
    output$mdUI3 <- renderUI({
      md_test_num <- md_test_num()
      req(md_test_num)
      
      if (md_test_num == 1) {
        numericInput(
          inputId = ns("md_UI3"),
          label = withMathJax("$$\\sigma:{ }$$"),
          value = 12,
          width = "150px"
        )
      } else if (md_test_num == 2) {
        numericInput(
          inputId = ns("md_UI3"),
          label = withMathJax("$$s:{ }$$"),
          value = 12,
          width = "150px"
        )
      } else if (md_test_num == 3) {
        numericInput(
          inputId = ns("md_UI3"),
          label = withMathJax("$$n:{ }$$"),
          value = 12,
          width = "150px"
        )
      } else if (md_test_num == 4 || md_test_num == 6) {
        numericInput(
          inputId = ns("md_UI3"),
          label = withMathJax("$$\\sigma_{1}:{ }$$"),
          value = 12,
          width = "150px"
        )
      } else if (md_test_num == 5) {
        numericInput(
          inputId = ns("md_UI3"),
          label = withMathJax("$$s_{1}:{ }$$"),
          value = 12,
          width = "150px"
        )
      } else if (md_test_num == 7) {
        dep_t_type <- input$md_t_dep_type_stat
        req(dep_t_type)
        if (dep_t_type == 1) {
          numericInput(
            inputId = ns("md_UI3"),
            label = withMathJax("$$s_{D}:{ }$$"),
            value = 1,
            width = "150px"
          )
        } else if (dep_t_type == 2) {
          numericInput(
            inputId = ns("md_UI3"),
            label = withMathJax("$$s_{1}:{ }$$"),
            value = 12,
            width = "150px"
          )
        }
      }
    })
    
    output$mdUI4 <- renderUI({
      md_test_num <- md_test_num()
      req(md_test_num)
      
      if (md_test_num > 0 && md_test_num < 4) {
        NULL
      } else if (md_test_num == 4 || md_test_num == 6) {
        numericInput(
          inputId = ns("md_UI4"),
          label = withMathJax("$$\\sigma_{2}:{ }$$"),
          value = 12,
          width = "150px"
        )
      } else if (md_test_num == 5) {
        numericInput(
          inputId = ns("md_UI4"),
          label = withMathJax("$$s_{2}:{ }$$"),
          value = 12,
          width = "150px"
        )
      } else if (md_test_num == 7) {
        dep_t_type <- input$md_t_dep_type_stat
        req(dep_t_type)
        if (dep_t_type == 1) {
          NULL
        } else if (dep_t_type == 2) {
          numericInput(
            inputId = ns("md_UI4"),
            label = withMathJax("$$s_{2}:{ }$$"),
            value = 12,
            width = "150px"
          )
        }
      }
    })
    
    output$mdUI5 <- renderUI({
      md_test_num <- md_test_num()
      req(md_test_num)
      
      if (md_test_num == 1 || md_test_num == 2) {
        numericInput(
          inputId = ns("md_UI5"),
          label = withMathJax("$$n:{ }$$"),
          value = 12,
          width = "150px"
        )
      } else if (md_test_num == 3) {
        NULL
      } else if (md_test_num > 3 && md_test_num < 6) {
        numericInput(
          inputId = ns("md_UI5"),
          label = withMathJax("$$n_{1}:{ }$$"),
          value = 12,
          width = "150px"
        )
      } else if (md_test_num > 5 && md_test_num < 8) {
        numericInput(
          inputId = ns("md_UI5"),
          label = withMathJax("$$n:{ }$$"),
          value = 12,
          width = "150px"
        )
      }
    })
    
    output$mdUI6 <- renderUI({
      md_test_num <- md_test_num()
      req(md_test_num)
      
      if (md_test_num > 0 && md_test_num < 4) {
        NULL
      } else if (md_test_num > 3 && md_test_num < 6) {
        numericInput(
          inputId = ns("md_UI6"),
          label = withMathJax("$$n_{2}:{ }$$"),
          value = 12,
          width = "150px"
        )
      } else if (md_test_num > 5 && md_test_num < 7) {
        numericInput(
          inputId = ns("md_UI6"),
          label = withMathJax("$$r_{xy}:{ }$$"),
          value = 0.5,
          width = "150px"
        )
      } else if (md_test_num == 7) {
        dep_t_type <- input$md_t_dep_type_stat
        req(dep_t_type)
        if (dep_t_type == 1) {
          NULL
        } else if (dep_t_type == 2) {
          numericInput(
            inputId = ns("md_UI6"),
            label = withMathJax("$$r_{xy}:{ }$$"),
            value = 0.5,
            width = "150px"
          )
        }
      }
    })
    
    # =========================================================================
    # MEANS AND DISPERSION "USE DATA" TAB UI RENDERING FUNCTIONS
    # =========================================================================
    
    # Data selection - columns mode
    output$data_choice_column <- renderUI({
      data <- filtered_data()
      req(data, ncol(data) > 0)
      
      choices <- seq_len(ncol(data))
      names(choices) <- names(data)
      
      checkboxGroupInput(
        inputId = ns("md_data_selected_columns"),
        label = "Analyze which column(s)?",
        choices = choices
      )
    })
    
    # Data selection - reference column mode
    output$data_choice_ref <- renderUI({
      data <- filtered_data()
      req(data, ncol(data) > 0)
      
      choices <- seq_len(ncol(data))
      names(choices) <- names(data)
      
      selectInput(
        inputId = ns("data_choice_ref"),
        label = "Select Factor",
        multiple = FALSE,
        choices = choices
      )
    })
    
    # Data selection - data column (excluding reference)
    output$data_choice_data <- renderUI({
      data <- filtered_data()
      ref <- input$data_choice_ref
      req(data, ncol(data) > 0, ref)
      
      choices <- seq_len(ncol(data))
      names(choices) <- names(data)
      
      # Remove the selected reference column
      fact_selected <- as.numeric(ref)
      temp <- seq_along(choices)
      temp <- temp[-fact_selected]
      choices <- choices[temp]
      
      selectInput(
        inputId = ns("data_choice_data"),
        label = "Select Data",
        multiple = FALSE,
        choices = choices
      )
    })
    
    # Group 1 selection
    output$data_choice_g1 <- renderUI({
      data <- filtered_data()
      ref <- input$data_choice_ref
      req(data, ref)
      
      factor_col <- as.numeric(ref)
      factor_values <- unique(na.omit(data[[factor_col]]))
      
      selectInput(
        inputId = ns("data_choice_g1"),
        label = "Group 1",
        choices = factor_values
      )
    })
    
    # Group 2 selection
    output$data_choice_g2 <- renderUI({
      data <- filtered_data()
      ref <- input$data_choice_ref
      factor_g1 <- input$data_choice_g1
      req(data, ref, factor_g1)
      
      factor_col <- as.numeric(ref)
      factor_values <- unique(na.omit(data[[factor_col]]))
      
      # Remove group 1 from choices
      factor_values <- factor_values[factor_values != factor_g1]
      
      selectInput(
        inputId = ns("data_choice_g2"),
        label = "Group 2",
        choices = factor_values
      )
    })
    
    # Test selection UI (sigma known/unknown)
    output$md_data_test_selection <- renderUI({
      data <- filtered_data()
      type <- input$data_type_md
      req(data, type)
      
      if (type == 1) {
        # Column mode
        dep_col <- input$md_data_selected_columns
        dep_or_indep <- input$dep_or_indep_data
        
        if (length(dep_col) == 1) {
          # One sample
          radioButtons(
            inputId = ns("sigma_known_data"),
            label = "Is σ definitively known?",
            choices = c("No, use t" = 1, "Yes, use z" = 2)
          )
        } else {
          # Two sample
          req(dep_or_indep)
          if (dep_or_indep == 2) {
            # Dependent - no sigma selection
            return(NULL)
          }
          radioButtons(
            inputId = ns("sigma_known_data"),
            label = "Is σ definitively known?",
            choices = c("No, use t" = 1, "Yes, use z" = 2)
          )
        }
      } else {
        # Reference column mode
        dep_or_indep <- input$dep_or_indep_data
        req(dep_or_indep)
        
        if (dep_or_indep == 2) {
          # Dependent - no sigma selection
          return(NULL)
        } else {
          radioButtons(
            inputId = ns("sigma_known_data"),
            label = "Is σ definitively known?",
            choices = c("No, use t" = 1, "Yes, use z" = 2)
          )
        }
      }
    })
    
    # Independent/Dependent selection for data mode
    output$md_data_indep <- renderUI({
      data <- filtered_data()
      type <- input$data_type_md
      req(data, type)
      
      if (type == 1) {
        # Column mode
        dep_col <- input$md_data_selected_columns
        if (length(dep_col) > 1) {
          selectInput(
            inputId = ns("dep_or_indep_data"),
            label = "Independent or Dependent Test?",
            choices = c("Independent" = 1, "Dependent" = 2)
          )
        } else {
          NULL
        }
      } else {
        # Reference column mode - always show
        selectInput(
          inputId = ns("dep_or_indep_data"),
          label = "Independent or Dependent Test?",
          choices = c("Independent" = 1, "Dependent" = 2)
        )
      }
    })
    
    # One-sample variance checkbox for data mode
    output$md_data_one_samp_var <- renderUI({
      type <- input$data_type_md
      md_data_selected_columns <- input$md_data_selected_columns
      sigma_known_data <- input$sigma_known_data
      
      # Determine if one sample
      if (type == 1) {
        if (is.null(md_data_selected_columns) || length(md_data_selected_columns) != 1) {
          return(NULL)
        }
        is_one_sample <- TRUE
      } else {
        is_one_sample <- FALSE
      }
      
      req(sigma_known_data)
      
      if (is_one_sample && sigma_known_data == 1) {
        checkboxInput(
          inputId = ns("one_samp_var_data"),
          label = "One-sample test for variance?",
          value = FALSE
        )
      } else {
        NULL
      }
    })
    
    # Alternative hypothesis for data mode
    output$alt_mean_data <- renderUI({
      test_num <- data_md_test_num()
      req(test_num)
      
      if (test_num == 1) {
        selectInput(
          inputId = ns("one_or_two_md_data"),
          label = "Alternative Hypothesis for Means",
          choices = choice_mean_alt_1
        )
      } else if (test_num == 2) {
        selectInput(
          inputId = ns("one_or_two_md_data"),
          label = "Alternative Hypothesis for Means",
          choices = choice_mean_alt_1
        )
      } else if (test_num == 3) {
        selectInput(
          inputId = ns("one_or_two_md_data"),
          label = "Alternative Hypothesis for Means",
          choices = choice_sd_alt_1
        )
      } else if (test_num == 4 || test_num == 5 || test_num == 6) {
        selectInput(
          inputId = ns("one_or_two_md_data"),
          label = "Alternative Hypothesis for Means",
          choices = choice_mean_alt_2
        )
      } else if (test_num == 7) {
        dep_t_type <- input$md_t_dep_type
        req(dep_t_type)
        if (dep_t_type == 1) {
          selectInput(
            inputId = ns("one_or_two_md_data"),
            label = "Alternative Hypothesis for Means",
            choices = choice_mean_alt_3
          )
        } else if (dep_t_type == 2) {
          selectInput(
            inputId = ns("one_or_two_md_data"),
            label = "Alternative Hypothesis for Means",
            choices = choice_mean_alt_2
          )
        }
      }
    })
    
    # Dependent test type for data mode
    output$md_t_dep <- renderUI({
      test_num <- data_md_test_num()
      req(test_num)
      
      if (test_num == 7) {
        radioButtons(
          inputId = ns("md_t_dep_type"),
          label = "Dependency by",
          choices = c("Nature (D̄)" = 1, "Design (Mean Difference)" = 2)
        )
      } else {
        NULL
      }
    })
    
    # T-test type for data mode
    output$md_t_type <- renderUI({
      test_num <- data_md_test_num()
      req(test_num)
      
      if (test_num == 5) {
        radioButtons(
          inputId = ns("t_type_dat"),
          label = "Select:",
          choices = c(
            "Welch (recommended)" = "no",
            "Student (unknown but equal variance)" = "yes",
            "Choose based on variance test (not recommended)" = "auto"
          )
        )
      } else {
        NULL
      }
    })
    
    # Dynamic UI inputs for data mode - md_data_UI1 through md_data_UI6
    # These calculate and display statistics from the data
    output$md_data_UI1 <- renderUI({
      test_num <- data_md_test_num()
      md_data_selected_columns <- input$md_data_selected_columns
      data <- filtered_data()
      decimal_m_d_d <- input$decimal_m_d_d
      type <- input$data_type_md
      ref_col <- as.numeric(input$data_choice_ref)
      data_col <- as.numeric(input$data_choice_data)
      g1_num <- input$data_choice_g1
      g2_num <- input$data_choice_g2
      
      req(data, test_num)
      
      if (test_num == 1) {
        # One-sample z-test - show sample mean
        req(md_data_selected_columns, length(md_data_selected_columns) >= 1, data)
        average <- ro(mean(x = na.omit(data[, as.numeric(md_data_selected_columns[1])])), decimal_m_d_d)
        label <- withMathJax(paste("$\\bar{X} = $", average))
        HTML(paste(label))
      } else if (test_num == 2) {
        # One-sample t-test - show sample mean
        req(md_data_selected_columns, length(md_data_selected_columns) >= 1, data)
        average <- ro(mean(x = na.omit(data[, as.numeric(md_data_selected_columns[1])])), decimal_m_d_d)
        label <- withMathJax(paste("$\\bar{X} = $", average))
        HTML(paste(label))
      } else if (test_num == 3) {
        # One-sample var test - show sample std
        req(md_data_selected_columns, length(md_data_selected_columns) >= 1, data)
        std <- ro(sd(x = na.omit(data[, as.numeric(md_data_selected_columns[1])])), decimal_m_d_d)
        label <- withMathJax(paste("$s = $", std))
        HTML(paste(label))
      } else if (test_num == 4 || test_num == 5) {
        # Two-sample independent - show group 1 mean
        if (type == 1) {
          req(md_data_selected_columns, length(md_data_selected_columns) >= 1, data)
          average <- ro(mean(x = na.omit(data[, as.numeric(md_data_selected_columns[1])])), decimal_m_d_d)
        } else if (type == 2) {
          req(data_col, ref_col, g1_num, data, 
              data_col > 0, data_col <= ncol(data),
              ref_col > 0, ref_col <= ncol(data))
          g1_data <- data[[data_col]][which(data[[ref_col]] == g1_num)]
          req(length(g1_data) > 0, is.numeric(g1_data) || is.logical(g1_data))
          average <- ro(mean(x = na.omit(g1_data)), decimal_m_d_d)
        }
        label <- withMathJax(paste("$\\bar{X}_{1} = $", average))
        HTML(paste(label))
      } else if (test_num == 7) {
        dep_t_type <- input$md_t_dep_type
        req(dep_t_type)
        
        if (type == 1) {
          req(md_data_selected_columns, length(md_data_selected_columns) >= 2, data)
          ave_data <- na.omit(data.frame(g1 = data[, as.numeric(md_data_selected_columns[1])], 
                                           g2 = data[, as.numeric(md_data_selected_columns[2])]))
          req(nrow(ave_data) > 0)
          if (dep_t_type == 1) {
            average <- ro(mean(x = ave_data$g2 - ave_data$g1), decimal_m_d_d)
            label <- withMathJax(paste("$\\bar{D} = $", average))
          } else if (dep_t_type == 2) {
            average <- ro(mean(x = ave_data$g1), decimal_m_d_d)
            label <- withMathJax(paste("$\\bar{X}_{1} = $", average))
          }
          HTML(paste(label))
        } else {
          req(data_col, ref_col, g1_num, g2_num, data,
              data_col > 0, data_col <= ncol(data),
              ref_col > 0, ref_col <= ncol(data))
          temp1 <- data[[data_col]][which(data[[ref_col]] == g1_num)]
          temp2 <- data[[data_col]][which(data[[ref_col]] == g2_num)]
          req(length(temp1) > 0, length(temp2) > 0,
              is.numeric(temp1) || is.logical(temp1),
              is.numeric(temp2) || is.logical(temp2))
          if (length(temp1) != length(temp2)) {
            return("Need equal length groups for dependent test")
          }
          temp <- na.omit(data.frame(temp1, temp2))
          req(nrow(temp) > 0)
          if (dep_t_type == 1) {
            average <- ro(mean(temp$temp1 - temp$temp2), decimal_m_d_d)
            label <- withMathJax(paste("$\\bar{D} = $", average))
          } else if (dep_t_type == 2) {
            average <- ro(mean(temp$temp1), decimal_m_d_d)
            label <- withMathJax(paste("$\\bar{X}_{1} = $", average))
          }
          HTML(paste(label))
        }
      }
    })
    
    output$md_data_UI2 <- renderUI({
      test_num <- data_md_test_num()
      md_data_selected_columns <- input$md_data_selected_columns
      data <- filtered_data()
      decimal_m_d_d <- input$decimal_m_d_d
      type <- input$data_type_md
      ref_col <- as.numeric(input$data_choice_ref)
      data_col <- as.numeric(input$data_choice_data)
      g1_num <- input$data_choice_g1
      g2_num <- input$data_choice_g2
      
      req(test_num)
      
      if (test_num == 1 || test_num == 2) {
        # One-sample tests - parameter input
        numericInput(
          inputId = ns("md_data_UI2"),
          label = withMathJax("$\\mu_{0}:{ }$"),
          value = 3
        )
      } else if (test_num == 3) {
        # One-sample var test - parameter input
        numericInput(
          inputId = ns("md_data_UI2"),
          label = withMathJax("$\\sigma_{0}:{ }$"),
          value = 3
        )
      } else if (test_num == 4 || test_num == 5) {
        # Two-sample independent - show group 2 mean
        if (type == 1) {
          req(md_data_selected_columns, length(md_data_selected_columns) >= 2, data)
          average <- ro(mean(x = na.omit(data[, as.numeric(md_data_selected_columns[2])])), decimal_m_d_d)
        } else if (type == 2) {
          req(data_col, ref_col, g2_num, data,
              data_col > 0, data_col <= ncol(data),
              ref_col > 0, ref_col <= ncol(data))
          g2_data <- data[[data_col]][which(data[[ref_col]] == g2_num)]
          req(length(g2_data) > 0, is.numeric(g2_data) || is.logical(g2_data))
          average <- ro(mean(x = na.omit(g2_data)), decimal_m_d_d)
        }
        label <- withMathJax(paste("$\\bar{X}_{2} = $", average))
        HTML(paste(label))
      } else if (test_num == 7) {
        dep_t_type <- input$md_t_dep_type
        req(dep_t_type)
        
        if (dep_t_type == 1) {
          # d-bar type - parameter input
          numericInput(
            inputId = ns("md_data_UI2"),
            label = withMathJax("$\\Delta:{ }$"),
            value = 0
          )
        } else if (dep_t_type == 2) {
          # Mean difference type - show group 2 mean
          if (type == 1) {
            ave_data <- na.omit(data.frame(g1 = data[, as.numeric(md_data_selected_columns[1])], 
                                           g2 = data[, as.numeric(md_data_selected_columns[2])]))
            average <- ro(mean(x = ave_data$g2), decimal_m_d_d)
            label <- withMathJax(paste("$\\bar{X}_{2} = $", average))
            HTML(paste(label))
          } else {
            req(data_col, ref_col, g1_num, g2_num)
            temp1 <- data[[data_col]][which(data[[ref_col]] == g1_num)]
            temp2 <- data[[data_col]][which(data[[ref_col]] == g2_num)]
            temp <- na.omit(data.frame(temp1, temp2))
            average <- ro(mean(temp$temp2), decimal_m_d_d)
            label <- withMathJax(paste("$\\bar{X}_{2} = $", average))
            HTML(paste(label))
          }
        }
      }
    })
    
    output$md_data_UI3 <- renderUI({
      test_num <- data_md_test_num()
      md_data_selected_columns <- input$md_data_selected_columns
      data <- filtered_data()
      decimal_m_d_d <- input$decimal_m_d_d
      type <- input$data_type_md
      ref_col <- as.numeric(input$data_choice_ref)
      data_col <- as.numeric(input$data_choice_data)
      g1_num <- input$data_choice_g1
      g2_num <- input$data_choice_g2
      
      req(test_num)
      
      if (test_num == 1) {
        # One-sample z-test - parameter input
        req(md_data_selected_columns, length(md_data_selected_columns) >= 1, data)
        numericInput(
          inputId = ns("md_data_UI3"),
          label = withMathJax("$\\sigma_{0}:{ }$"),
          value = ro(sd(na.omit(data[, as.numeric(md_data_selected_columns[1])])), decimal_m_d_d)
        )
      } else if (test_num == 2) {
        # One-sample t-test - show sample std
        req(md_data_selected_columns, length(md_data_selected_columns) >= 1, data)
        std <- ro(sd(x = na.omit(data[, as.numeric(md_data_selected_columns[1])])), decimal_m_d_d)
        label <- withMathJax(paste("$s = $", std))
        HTML(paste(label))
      } else if (test_num == 3) {
        # One-sample var test - show sample size
        req(md_data_selected_columns, length(md_data_selected_columns) >= 1, data)
        n <- length(na.omit(data[, as.numeric(md_data_selected_columns[1])]))
        label <- withMathJax(paste("$n = $", n))
        HTML(paste(label))
      } else if (test_num == 4) {
        # Two-sample z independent - parameter input for sigma1
        if (type == 1) {
          req(md_data_selected_columns, length(md_data_selected_columns) >= 1, data)
          std <- ro(sd(x = na.omit(data[, as.numeric(md_data_selected_columns[1])])), decimal_m_d_d)
        } else if (type == 2) {
          req(data_col, ref_col, g1_num, data,
              data_col > 0, data_col <= ncol(data),
              ref_col > 0, ref_col <= ncol(data))
          g1_data <- data[[data_col]][which(data[[ref_col]] == g1_num)]
          req(length(g1_data) > 0, is.numeric(g1_data) || is.logical(g1_data))
          std <- ro(sd(x = na.omit(g1_data)), decimal_m_d_d)
        }
        numericInput(
          inputId = ns("md_data_UI3"),
          label = withMathJax("$\\sigma_{1}:{ }$"),
          value = std
        )
      } else if (test_num == 5) {
        # Two-sample t independent - show group 1 std
        if (type == 1) {
          req(md_data_selected_columns, length(md_data_selected_columns) >= 1)
          std <- ro(sd(x = na.omit(data[, as.numeric(md_data_selected_columns[1])])), decimal_m_d_d)
        } else if (type == 2) {
          req(data_col, ref_col, g1_num, data,
              data_col > 0, data_col <= ncol(data),
              ref_col > 0, ref_col <= ncol(data))
          g1_data <- data[[data_col]][which(data[[ref_col]] == g1_num)]
          req(length(g1_data) > 0, is.numeric(g1_data) || is.logical(g1_data))
          std <- ro(sd(x = na.omit(g1_data)), decimal_m_d_d)
        }
        label <- withMathJax(paste("$s_{1} = $", std))
        HTML(paste(label))
      } else if (test_num == 7) {
        dep_t_type <- input$md_t_dep_type
        req(dep_t_type)
        
        if (type == 1) {
          req(md_data_selected_columns, length(md_data_selected_columns) >= 2, data)
          ave_data <- na.omit(data.frame(g1 = data[, as.numeric(md_data_selected_columns[1])], 
                                           g2 = data[, as.numeric(md_data_selected_columns[2])]))
          req(nrow(ave_data) > 0)
          if (dep_t_type == 1) {
            std <- ro(sd(x = ave_data$g1 - ave_data$g2), decimal_m_d_d)
            label <- withMathJax(paste("$s_{D} = $", std))
          } else if (dep_t_type == 2) {
            std <- ro(sd(x = ave_data$g1), decimal_m_d_d)
            label <- withMathJax(paste("$s_{1} = $", std))
          }
          HTML(paste(label))
        } else {
          req(data_col, ref_col, g1_num, g2_num)
          temp1 <- data[[data_col]][which(data[[ref_col]] == g1_num)]
          temp2 <- data[[data_col]][which(data[[ref_col]] == g2_num)]
          if (length(temp1) != length(temp2)) {
            return("Need equal length groups for dependent test")
          }
          temp <- na.omit(data.frame(temp1, temp2))
          if (dep_t_type == 1) {
            std <- ro(sd(x = temp$temp1 - temp$temp2), decimal_m_d_d)
            label <- withMathJax(paste("$s_{D} = $", std))
          } else if (dep_t_type == 2) {
            std <- ro(sd(x = temp$temp1), decimal_m_d_d)
            label <- withMathJax(paste("$s_{1} = $", std))
          }
          HTML(paste(label))
        }
      }
    })
    
    output$md_data_UI4 <- renderUI({
      test_num <- data_md_test_num()
      md_data_selected_columns <- input$md_data_selected_columns
      data <- filtered_data()
      decimal_m_d_d <- input$decimal_m_d_d
      type <- input$data_type_md
      ref_col <- as.numeric(input$data_choice_ref)
      data_col <- as.numeric(input$data_choice_data)
      g1_num <- input$data_choice_g1
      g2_num <- input$data_choice_g2
      
      req(test_num)
      
      if (test_num > 0 && test_num < 4) {
        NULL
      } else if (test_num == 4) {
        # Two-sample z independent - parameter input for sigma2
        if (type == 1) {
          std <- ro(sd(x = na.omit(data[, as.numeric(md_data_selected_columns[2])])), decimal_m_d_d)
        } else if (type == 2) {
          req(data_col, ref_col, g2_num, data,
              data_col > 0, data_col <= ncol(data),
              ref_col > 0, ref_col <= ncol(data))
          g2_data <- data[[data_col]][which(data[[ref_col]] == g2_num)]
          req(length(g2_data) > 0, is.numeric(g2_data) || is.logical(g2_data))
          std <- ro(sd(x = na.omit(g2_data)), decimal_m_d_d)
        }
        numericInput(
          inputId = ns("md_data_UI4"),
          label = withMathJax("$\\sigma_{2}:{ }$"),
          value = std
        )
      } else if (test_num == 5) {
        # Two-sample t independent - show group 2 std
        if (type == 1) {
          req(md_data_selected_columns, length(md_data_selected_columns) >= 2, data)
          std <- ro(sd(x = na.omit(data[, as.numeric(md_data_selected_columns[2])])), decimal_m_d_d)
        } else if (type == 2) {
          req(data_col, ref_col, g2_num, data,
              data_col > 0, data_col <= ncol(data),
              ref_col > 0, ref_col <= ncol(data))
          g2_data <- data[[data_col]][which(data[[ref_col]] == g2_num)]
          req(length(g2_data) > 0, is.numeric(g2_data) || is.logical(g2_data))
          std <- ro(sd(x = na.omit(g2_data)), decimal_m_d_d)
        }
        label <- withMathJax(paste("$s_{2} = $", std))
        HTML(paste(label))
      } else if (test_num == 7) {
        dep_t_type <- input$md_t_dep_type
        req(dep_t_type)
        
        if (dep_t_type == 1) {
          NULL
        } else if (dep_t_type == 2) {
          if (type == 1) {
            ave_data <- na.omit(data.frame(g1 = data[, as.numeric(md_data_selected_columns[1])], 
                                             g2 = data[, as.numeric(md_data_selected_columns[2])]))
            std <- ro(sd(x = ave_data$g2), decimal_m_d_d)
            label <- withMathJax(paste("$s_{2} = $", std))
            HTML(paste(label))
          } else {
            req(data_col, ref_col, g1_num, g2_num, data,
                data_col > 0, data_col <= ncol(data),
                ref_col > 0, ref_col <= ncol(data))
            temp1 <- data[[data_col]][which(data[[ref_col]] == g1_num)]
            temp2 <- data[[data_col]][which(data[[ref_col]] == g2_num)]
            req(length(temp1) > 0, length(temp2) > 0,
                is.numeric(temp1) || is.logical(temp1),
                is.numeric(temp2) || is.logical(temp2))
            temp <- na.omit(data.frame(temp1, temp2))
            req(nrow(temp) > 0)
            std <- ro(sd(x = temp$temp2), decimal_m_d_d)
            label <- withMathJax(paste("$s_{2} = $", std))
            HTML(paste(label))
          }
        }
      }
    })
    
    output$md_data_UI5 <- renderUI({
      test_num <- data_md_test_num()
      md_data_selected_columns <- input$md_data_selected_columns
      data <- filtered_data()
      decimal_m_d_d <- input$decimal_m_d_d
      type <- input$data_type_md
      ref_col <- as.numeric(input$data_choice_ref)
      data_col <- as.numeric(input$data_choice_data)
      g1_num <- input$data_choice_g1
      g2_num <- input$data_choice_g2
      
      req(test_num)
      
      if (test_num == 1 || test_num == 2) {
        # One-sample tests - show sample size
        req(md_data_selected_columns, length(md_data_selected_columns) >= 1, data)
        n <- length(na.omit(data[, as.numeric(md_data_selected_columns[1])]))
        label <- withMathJax(paste("$n = $", n))
        HTML(paste(label))
      } else if (test_num == 3) {
        NULL
      } else if (test_num == 4 || test_num == 5) {
        # Two-sample independent - show group 1 size
        if (type == 1) {
          req(md_data_selected_columns, length(md_data_selected_columns) >= 1, data)
          n <- length(na.omit(data[, as.numeric(md_data_selected_columns[1])]))
        } else if (type == 2) {
          req(data_col, ref_col, g1_num, data,
              data_col > 0, data_col <= ncol(data),
              ref_col > 0, ref_col <= ncol(data))
          g1_data <- data[[data_col]][which(data[[ref_col]] == g1_num)]
          req(length(g1_data) > 0)
          n <- length(na.omit(g1_data))
        }
        label <- withMathJax(paste("$n_{1} = $", n))
        HTML(paste(label))
      } else if (test_num == 7) {
        # Two-sample dependent - show sample size
        if (type == 1) {
          req(md_data_selected_columns, length(md_data_selected_columns) >= 2)
          ave_data <- na.omit(data.frame(g1 = data[, as.numeric(md_data_selected_columns[1])], 
                                           g2 = data[, as.numeric(md_data_selected_columns[2])]))
          n <- length(ave_data$g1)
          label <- withMathJax(paste("$n = $", n))
          HTML(paste(label))
        } else {
          req(data_col, ref_col, g1_num, g2_num, data,
              data_col > 0, data_col <= ncol(data),
              ref_col > 0, ref_col <= ncol(data))
          temp1 <- data[[data_col]][which(data[[ref_col]] == g1_num)]
          temp2 <- data[[data_col]][which(data[[ref_col]] == g2_num)]
          req(length(temp1) > 0, length(temp2) > 0)
          if (length(temp1) != length(temp2)) {
            return("Need equal length groups for dependent test")
          }
          n <- nrow(na.omit(data.frame(temp1, temp2)))
          label <- withMathJax(paste("$n = $", n))
          HTML(paste(label))
        }
      }
    })
    
    output$md_data_UI6 <- renderUI({
      test_num <- data_md_test_num()
      md_data_selected_columns <- input$md_data_selected_columns
      data <- filtered_data()
      decimal_m_d_d <- input$decimal_m_d_d
      type <- input$data_type_md
      ref_col <- as.numeric(input$data_choice_ref)
      data_col <- as.numeric(input$data_choice_data)
      g1_num <- input$data_choice_g1
      g2_num <- input$data_choice_g2
      
      req(test_num)
      
      if (test_num > 0 && test_num < 4) {
        NULL
      } else if (test_num == 4 || test_num == 5) {
        # Two-sample independent - show group 2 size
        if (type == 1) {
          req(md_data_selected_columns, length(md_data_selected_columns) >= 2, data)
          n <- length(na.omit(data[, as.numeric(md_data_selected_columns[2])]))
        } else if (type == 2) {
          req(data_col, ref_col, g2_num, data,
              data_col > 0, data_col <= ncol(data),
              ref_col > 0, ref_col <= ncol(data))
          g2_data <- data[[data_col]][which(data[[ref_col]] == g2_num)]
          req(length(g2_data) > 0)
          n <- length(na.omit(g2_data))
        }
        label <- withMathJax(paste("$n_{2} = $", n))
        HTML(paste(label))
      } else if (test_num == 7) {
        dep_t_type <- input$md_t_dep_type
        req(dep_t_type)
        
        if (dep_t_type == 1) {
          NULL
        } else if (dep_t_type == 2) {
          # Mean difference type - show correlation
          if (type == 1) {
            req(md_data_selected_columns, length(md_data_selected_columns) >= 2)
            ave_data <- na.omit(data.frame(g1 = data[, as.numeric(md_data_selected_columns[1])], 
                                             g2 = data[, as.numeric(md_data_selected_columns[2])]))
            req(nrow(ave_data) > 0)
            rho <- ro(cor(ave_data$g1, ave_data$g2), decimal_m_d_d)
            label <- withMathJax(paste("$r_{xy} = $", rho))
            HTML(paste(label))
          } else {
            req(data_col, ref_col, g1_num, g2_num, data,
                data_col > 0, data_col <= ncol(data),
                ref_col > 0, ref_col <= ncol(data))
            temp1 <- data[[data_col]][which(data[[ref_col]] == g1_num)]
            temp2 <- data[[data_col]][which(data[[ref_col]] == g2_num)]
            req(length(temp1) > 0, length(temp2) > 0,
                is.numeric(temp1) || is.logical(temp1),
                is.numeric(temp2) || is.logical(temp2))
            temp <- na.omit(data.frame(temp1, temp2))
            req(nrow(temp) > 0)
            rho <- ro(cor(temp$temp1, temp$temp2), decimal_m_d_d)
            label <- withMathJax(paste("$r_{xy} = $", rho))
            HTML(paste(label))
          }
        }
      }
    })
    
    # =========================================================================
    # PROPORTIONS MODULE
    # =========================================================================
    # Proportions server
    proportions_result <- create_proportions_server(
      "proportions",
      filtered_data,
      reactive({
        list(
          # Enter Statistics mode inputs
          decimal_p = input$decimal_p,
          alt_p = input$alt_p,
          alt_p2 = input$alt_p2,
          conf_p = input$conf_p,
          one_or_two_p = input$one_or_two_p,
          p_samp = input$p_samp,
          n_samp_p = input$n_samp_p,
          n_samp_p_2 = input$n_samp_p_2,
          p0 = input$p0,
          p2 = input$p2,
          # Use Data mode inputs
          decimal_bi_d = input$decimal_bi_d,
          conf_bi_data = input$conf_bi_data,
          data_type_bi = input$data_type_bi,
          bi_data_selected_columns = input$bi_data_selected_columns,
          bi_data_success1 = input$bi_data_success1,
          bi_data_success2 = input$bi_data_success2,
          alt_p_bi = input$alt_p_bi,
          data_choice_ref_bi = input$data_choice_ref_bi,
          data_choice_data_bi = input$data_choice_data_bi,
          data_choice_g1_bi = input$data_choice_g1_bi,
          data_choice_g2_bi = input$data_choice_g2_bi,
          bi_test_data_ui2 = input$bi_test_data_ui2
        )
      })
    )
    
    # Set choice vectors for alternative hypothesis selectors
    observe({
      updateSelectInput(session, "alt_p", choices = choice_prop_alt_1)
      updateSelectInput(session, "alt_p2", choices = choice_prop_alt_2)
    })
    
    # Render Enter Statistics results
    output$pretty_prop_stat <- renderUI({
      results <- proportions_result$prop_out()
      one_or_two_p <- input$one_or_two_p
      alt_p <- input$alt_p
      alt_p2 <- input$alt_p2
      R <- input$decimal_p
      conf <- input$conf_p
      
      if (is.null(results)) {
        return(HTML("<p>No results available. Please check your inputs.</p>"))
      }
      
      if (is.character(results)) {
        return(HTML(paste("<p>", results, "</p>")))
      }
      
      # Round results for display
      resultsR <- ro(results, R)
      
      # Determine alternative hypothesis number
      if (alt_p == "two.sided") {
        alt_num <- 1
      } else if (alt_p == "less") {
        alt_num <- 2
      } else if (alt_p == "greater") {
        alt_num <- 3
      } else {
        alt_num <- 1
      }
      
      if (alt_p2 == "two.sided") {
        alt_num_p2 <- 1
      } else if (alt_p2 == "less") {
        alt_num_p2 <- 2
      } else if (alt_p2 == "greater") {
        alt_num_p2 <- 3
      } else {
        alt_num_p2 <- 1
      }
      
      if (one_or_two_p == 1) {
        # One-sample proportion test
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$p =$"), resultsR$statistic), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\pi_{0} =$"), resultsR$parameter), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n= $"), resultsR$estimate[2]), "</td>",
          "<td>", "</td>",
          "<td>", "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", withMathJax("$\\pi: $")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", "to", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Exact test for ", names(choice_prop_alt_1[alt_num]), ": "), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          paste(beta_statement, 100 * resultsR$estimate[4], "%")
        ))
      } else if (one_or_two_p == 2) {
        # Two-sample proportion test
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$p_{1} =$"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$p_{2} =$"), resultsR$estimate[7]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1}= $"), resultsR$estimate[2]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{2}= $"), resultsR$estimate[8]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", withMathJax("$\\pi_{1}: $")), "</td>",
          "<td>", resultsR$estimate[5], "</td>",
          "<td>", "to", "</td>",
          "<td>", resultsR$estimate[6], "</td>",
          "</tr>",
          "<tr>",
          "<td align='right'>", paste(withMathJax("$\\pi_{2}: $")), "</td>",
          "<td>", resultsR$estimate[11], "</td>",
          "<td>", "to", "</td>",
          "<td>", resultsR$estimate[12], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Exact test for ", names(choice_prop_alt_2[alt_num_p2]), ": "), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
    })
    
    # Render Use Data results
    output$pretty_prop_stat_data <- renderUI({
      results <- proportions_result$prop_data_out()
      data_type <- input$data_type_bi
      alt <- input$alt_p_bi
      R <- input$decimal_bi_d
      conf <- input$conf_bi_data
      data <- filtered_data()
      bi_data_selected_columns <- input$bi_data_selected_columns
      
      if (is.null(results)) {
        return(HTML("<p>No results available. Please check your inputs.</p>"))
      }
      
      if (is.character(results)) {
        return(HTML(paste("<p>", results, "</p>")))
      }
      
      # Round results for display
      resultsR <- ro(results, R)
      
      # Determine alternative hypothesis number
      if (alt == "two.sided") {
        alt_num <- 1
      } else if (alt == "less") {
        alt_num <- 2
      } else if (alt == "greater") {
        alt_num <- 3
      } else {
        alt_num <- 1
      }
      
      # Determine number of samples
      if (data_type == 1) {
        data_columns <- as.numeric(bi_data_selected_columns)
        if (!isTruthy(data_columns)) {
          return(NULL)
        }
        samples <- length(data_columns)
        
        if (samples == 1) {
          # One-sample - get group names
          group1_name <- names(data)[data_columns[1]]
        } else if (samples == 2) {
          group1_name <- names(data)[data_columns[1]]
          group2_name <- names(data)[data_columns[2]]
        }
      } else {
        samples <- 2
        group1_name <- paste("Group 1 = ", input$data_choice_g1_bi)
        group2_name <- paste("Group 2 = ", input$data_choice_g2_bi)
      }
      
      if (samples == 1) {
        # One-sample proportion test
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$p =$"), resultsR[["statistic"]][["p"]]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\pi_{0} =$"), resultsR[["parameter"]][["null hypothesis proportion"]]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n= $"), resultsR[["estimate"]][["sample.size"]]), "</td>",
          "<td>", "</td>",
          "<td>", "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", withMathJax("$\\pi: $")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", "to", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Exact test for ", names(choice_prop_alt_1[alt_num]), ": "), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          paste(beta_statement, 100 * resultsR[["estimate"]][["power"]], "%")
        ))
      } else if (samples == 2) {
        # Two-sample proportion test
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr><td style='border-bottom:1px solid #000'>", group1_name, "</td><td style='border-bottom:1px solid #000'></td>",
          "<td style='border-bottom:1px solid #000'>", group2_name, "</td></tr>",
          "<tr>",
          "<td>", paste(withMathJax("$p_{1} =$"), resultsR[["estimate"]][["sample.prop.g1"]]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$p_{2} =$"), resultsR[["estimate"]][["sample.prop.g2"]]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1}= $"), resultsR[["estimate"]][["sample.size.g1"]]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{2}= $"), resultsR[["estimate"]][["sample.size.g2"]]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", withMathJax("$\\pi_{1}: $")), "</td>",
          "<td>", resultsR[["estimate"]][["p.g1.lowerci"]], "</td>",
          "<td>", "to", "</td>",
          "<td>", resultsR[["estimate"]][["p.g1.upperci"]], "</td>",
          "</tr>",
          "<tr>",
          "<td align='right'>", paste(withMathJax("$\\pi_{2}: $")), "</td>",
          "<td>", resultsR[["estimate"]][["p.g2.lowerci"]], "</td>",
          "<td>", "to", "</td>",
          "<td>", resultsR[["estimate"]][["p.g2.upperci"]], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Exact test for ", names(choice_prop_alt_2[alt_num]), ": "), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
    })
    
    # =========================================================================
    # PROPORTIONS UI RENDERING FUNCTIONS
    # =========================================================================
    
    # Column selection for data mode
    output$data_choice_column_bi <- renderUI({
      data <- filtered_data()
      req(data)
      
      choices <- seq(1, ncol(data))
      names(choices) <- names(data)
      
      checkboxGroupInput(
        inputId = ns("bi_data_selected_columns"),
        label = "Analyze which column(s)?",
        choices = choices
      )
    })
    
    # Reference column selection
    output$data_choice_ref_bi <- renderUI({
      data <- filtered_data()
      req(data)
      
      choices <- seq(1, ncol(data))
      names(choices) <- names(data)
      
      selectInput(
        inputId = ns("data_choice_ref_bi"),
        label = "Select Factor",
        choices = choices
      )
    })
    
    # Data column selection (when using reference)
    output$data_choice_data_bi <- renderUI({
      data <- filtered_data()
      ref_col <- input$data_choice_ref_bi
      req(data, ref_col)
      
      choices <- seq(1, ncol(data))
      names(choices) <- names(data)
      
      # Remove the selected reference column
      ref_selected <- as.numeric(ref_col)
      temp <- seq(1, length(choices))
      temp <- temp[-ref_selected]
      choices <- choices[temp]
      
      selectInput(
        inputId = ns("data_choice_data_bi"),
        label = "Select Data",
        choices = choices
      )
    })
    
    # Group 1 selection
    output$data_choice_g1_bi <- renderUI({
      data <- filtered_data()
      ref_col <- input$data_choice_ref_bi
      req(data, ref_col)
      
      factor_levels <- unique(na.omit(data[[as.numeric(ref_col)]]))
      
      selectInput(
        inputId = ns("data_choice_g1_bi"),
        label = "Group 1",
        choices = factor_levels
      )
    })
    
    # Group 2 selection
    output$data_choice_g2_bi <- renderUI({
      data <- filtered_data()
      ref_col <- input$data_choice_ref_bi
      g1 <- input$data_choice_g1_bi
      req(data, ref_col, g1)
      
      factor_levels <- unique(na.omit(data[[as.numeric(ref_col)]]))
      
      # Remove the selected group 1
      temp <- factor_levels[factor_levels != g1]
      
      selectInput(
        inputId = ns("data_choice_g2_bi"),
        label = "Group 2",
        choices = temp
      )
    })
    
    # Alternative hypothesis selector
    output$alt_bi_data <- renderUI({
      data_type <- input$data_type_bi
      req(data_type)
      
      if (data_type == 1) {
        # Columns mode
        bi_data_selected_columns <- input$bi_data_selected_columns
        if (!isTruthy(bi_data_selected_columns)) {
          return(HTML("<p>Select at least one data column</p>"))
        }
        samples <- length(bi_data_selected_columns)
      } else {
        # Reference mode
        factor_col <- input$data_choice_ref_bi
        if (!isTruthy(factor_col)) {
          return(HTML("<p>Select factor column</p>"))
        }
        samples <- 2
      }
      
      if (samples > 2) {
        return(HTML("<p>Select at most two columns</p>"))
      }
      
      if (samples == 1) {
        selectInput(
          inputId = ns("alt_p_bi"),
          label = "Alternative hypothesis for proportions",
          choices = choice_prop_alt_1
        )
      } else if (samples == 2) {
        selectInput(
          inputId = ns("alt_p_bi"),
          label = "Alternative hypothesis for proportions",
          choices = choice_prop_alt_2
        )
      }
    })
    
    # Success value selector for group 1
    output$data_bi_success1 <- renderUI({
      data <- filtered_data()
      data_type <- input$data_type_bi
      req(data, data_type)
      
      if (data_type == 1) {
        # Columns mode
        data_columns <- as.numeric(input$bi_data_selected_columns)
        if (length(data_columns) > 2 || !isTruthy(data_columns)) {
          return(NULL)
        }
        options <- na.omit(unique(data[[data_columns[1]]]))
        if (length(options) == 1) {
          options <- c(options, paste0("Not ", options))
        }
        col_name <- names(data)[data_columns[1]]
      } else {
        # Reference mode
        data_col <- as.numeric(input$data_choice_data_bi)
        req(data_col)
        options <- na.omit(unique(data[[data_col]]))
        if (length(options) == 1) {
          options <- c(options, paste0("Not ", options))
        }
        col_name <- names(data)[data_col]
      }
      
      selectInput(
        inputId = ns("bi_data_success1"),
        label = paste0("What Indicates 'Success' in ", col_name, "?"),
        choices = options,
        multiple = FALSE
      )
    })
    
    # Success value selector for group 2
    output$data_bi_success2 <- renderUI({
      data <- filtered_data()
      data_type <- input$data_type_bi
      req(data, data_type)
      
      if (data_type == 1) {
        # Columns mode
        data_columns <- as.numeric(input$bi_data_selected_columns)
        if (length(data_columns) > 2 || !isTruthy(data_columns) || length(data_columns) == 1) {
          return(NULL)
        }
        options <- na.omit(unique(data[[data_columns[2]]]))
        if (length(options) == 1) {
          options <- c(options, paste0("Not ", options))
        }
        col_name <- names(data)[data_columns[2]]
      } else {
        # Reference mode - not used
        return(NULL)
      }
      
      selectInput(
        inputId = ns("bi_data_success2"),
        label = paste0("What Indicates 'Success' in ", col_name, "?"),
        choices = options,
        multiple = FALSE
      )
    })
    
    # Dynamic UI elements showing statistics from data
    output$bi_test_data_ui1 <- renderUI({
      data <- filtered_data()
      data_type <- input$data_type_bi
      decimal_bi_d <- input$decimal_bi_d
      req(data, data_type, decimal_bi_d, nrow(data) > 0, ncol(data) > 0)
      
      if (data_type == 1) {
        # Columns mode
        data_columns <- as.numeric(input$bi_data_selected_columns)
        if (!isTruthy(data_columns) || length(data_columns) == 0) {
          return(NULL)
        }
        req(all(data_columns > 0), all(data_columns <= ncol(data)))
        samples <- length(data_columns)
        
        if (samples == 1) {
          # One-sample - show sample proportion
          x <- na.omit(data[[data_columns[1]]])
          success1 <- input$bi_data_success1
          req(success1, length(x) > 0)
          count <- sum(x == success1, na.rm = TRUE)
          n <- length(x)
          req(n > 0)
          p_val <- count / n
          label <- withMathJax(paste("$p = $", ro(p_val, decimal_bi_d)))
          HTML(paste(label))
        } else if (samples == 2) {
          # Two-sample - show group 1 proportion
          success1 <- input$bi_data_success1
          req(success1)
          x <- na.omit(data[[data_columns[1]]])
          req(length(x) > 0)
          count <- sum(x == success1, na.rm = TRUE)
          n <- length(x)
          req(n > 0)
          p_val <- count / n
          label <- withMathJax(paste("$p_{1} = $", ro(p_val, decimal_bi_d)))
          HTML(paste(label))
        }
      } else {
        # Reference mode - show group 1 proportion
        ref_col <- as.numeric(input$data_choice_ref_bi)
        data_col <- as.numeric(input$data_choice_data_bi)
        g1 <- input$data_choice_g1_bi
        success1 <- input$bi_data_success1
        req(ref_col, data_col, g1, success1,
            ref_col > 0, ref_col <= ncol(data),
            data_col > 0, data_col <= ncol(data))
        
        bi_data <- data.frame(ref = data[[ref_col]], data = data[[data_col]])
        g1_data <- bi_data$data[bi_data$ref == g1]
        req(length(g1_data) > 0)
        count <- sum(g1_data == success1, na.rm = TRUE)
        n <- sum(!is.na(g1_data))
        req(n > 0)
        p_val <- count / n
        label <- withMathJax(paste("$p_{1} = $", ro(p_val, decimal_bi_d)))
        HTML(paste(label))
      }
    })
    
    output$bi_test_data_ui2 <- renderUI({
      data <- filtered_data()
      data_type <- input$data_type_bi
      req(data, data_type, nrow(data) > 0, ncol(data) > 0)
      
      if (data_type == 1) {
        # Columns mode
        data_columns <- as.numeric(input$bi_data_selected_columns)
        if (!isTruthy(data_columns)) {
          return(NULL)
        }
        req(all(data_columns > 0), all(data_columns <= ncol(data)))
        samples <- length(data_columns)
        
        if (samples == 1) {
          # One-sample - parameter input for null hypothesis
          numericInput(
            inputId = ns("bi_test_data_ui2"),
            label = withMathJax("$\\pi_{0}:{ }$"),
            value = 0.5,
            min = 0,
            max = 1,
            width = "75px"
          )
        } else if (samples == 2) {
          # Two-sample - show group 2 proportion
          decimal_bi_d <- input$decimal_bi_d
          success2 <- input$bi_data_success2
          req(success2, decimal_bi_d)
          x <- na.omit(data[[data_columns[2]]])
          req(length(x) > 0)
          count <- sum(x == success2, na.rm = TRUE)
          n <- length(x)
          req(n > 0)
          p_val <- count / n
          label <- withMathJax(paste("$p_{2} = $", ro(p_val, decimal_bi_d)))
          HTML(paste(label))
        }
      } else {
        # Reference mode - show group 2 proportion
        ref_col <- as.numeric(input$data_choice_ref_bi)
        data_col <- as.numeric(input$data_choice_data_bi)
        g2 <- input$data_choice_g2_bi
        success1 <- input$bi_data_success1
        decimal_bi_d <- input$decimal_bi_d
        req(ref_col, data_col, g2, success1, decimal_bi_d,
            ref_col > 0, ref_col <= ncol(data),
            data_col > 0, data_col <= ncol(data))
        
        bi_data <- data.frame(ref = data[[ref_col]], data = data[[data_col]])
        g2_data <- bi_data$data[bi_data$ref == g2]
        req(length(g2_data) > 0)
        count <- sum(g2_data == success1, na.rm = TRUE)
        n <- sum(!is.na(g2_data))
        req(n > 0)
        p_val <- count / n
        label <- withMathJax(paste("$p_{2} = $", ro(p_val, decimal_bi_d)))
        HTML(paste(label))
      }
    })
    
    output$bi_test_data_ui3 <- renderUI({
      data <- filtered_data()
      data_type <- input$data_type_bi
      req(data, data_type, nrow(data) > 0, ncol(data) > 0)
      
      if (data_type == 1) {
        # Columns mode
        data_columns <- as.numeric(input$bi_data_selected_columns)
        if (!isTruthy(data_columns)) {
          return(NULL)
        }
        req(all(data_columns > 0), all(data_columns <= ncol(data)))
        samples <- length(data_columns)
        
        if (samples == 1) {
          # One-sample - show sample size
          x <- na.omit(data[[data_columns[1]]])
          req(length(x) > 0)
          n <- length(x)
          label <- withMathJax(paste("$n = $", n))
          HTML(paste(label))
        } else if (samples == 2) {
          # Two-sample - show group 1 size
          x <- na.omit(data[[data_columns[1]]])
          req(length(x) > 0)
          n <- length(x)
          label <- withMathJax(paste("$n_{1} = $", n))
          HTML(paste(label))
        }
      } else {
        # Reference mode - show group 1 size
        ref_col <- as.numeric(input$data_choice_ref_bi)
        data_col <- as.numeric(input$data_choice_data_bi)
        g1 <- input$data_choice_g1_bi
        req(ref_col, data_col, g1,
            ref_col > 0, ref_col <= ncol(data),
            data_col > 0, data_col <= ncol(data))
        
        bi_data <- data.frame(ref = data[[ref_col]], data = data[[data_col]])
        g1_data <- bi_data$data[bi_data$ref == g1]
        req(length(g1_data) > 0)
        n <- sum(!is.na(g1_data))
        label <- withMathJax(paste("$n_{1} = $", n))
        HTML(paste(label))
      }
    })
    
    output$bi_test_data_ui4 <- renderUI({
      data <- filtered_data()
      data_type <- input$data_type_bi
      req(data, data_type, nrow(data) > 0, ncol(data) > 0)
      
      if (data_type == 1) {
        # Columns mode
        data_columns <- as.numeric(input$bi_data_selected_columns)
        if (!isTruthy(data_columns) || length(data_columns) == 1) {
          return(NULL)
        }
        req(all(data_columns > 0), all(data_columns <= ncol(data)))
        samples <- length(data_columns)
        
        if (samples == 2) {
          # Two-sample - show group 2 size
          x <- na.omit(data[[data_columns[2]]])
          req(length(x) > 0)
          n <- length(x)
          label <- withMathJax(paste("$n_{2} = $", n))
          HTML(paste(label))
        }
      } else {
        # Reference mode - show group 2 size
        ref_col <- as.numeric(input$data_choice_ref_bi)
        data_col <- as.numeric(input$data_choice_data_bi)
        g2 <- input$data_choice_g2_bi
        req(ref_col, data_col, g2,
            ref_col > 0, ref_col <= ncol(data),
            data_col > 0, data_col <= ncol(data))
        
        bi_data <- data.frame(ref = data[[ref_col]], data = data[[data_col]])
        g2_data <- bi_data$data[bi_data$ref == g2]
        req(length(g2_data) > 0)
        n <- sum(!is.na(g2_data))
        label <- withMathJax(paste("$n_{2} = $", n))
        HTML(paste(label))
      }
    })
    
    # =========================================================================
    # POISSON MODULE
    # =========================================================================
    # Poisson server
    poisson_result <- create_poisson_one_two_sample_server(
      "poisson",
      filtered_data,
      reactive({
        list(
          # Enter Statistics mode inputs
          decimal_poi = input$decimal_poi,
          alt_poi = input$alt_poi,
          alt_poi_2 = input$alt_poi_2,
          conf_poi = input$conf_poi,
          one_or_two_poi = input$one_or_two_poi,
          poi_samp = input$poi_samp,
          n_samp_poi = input$n_samp_poi,
          n_samp_poi_2 = input$n_samp_poi_2,
          poi0 = input$poi0,
          poi2 = input$poi2,
          # Use Data mode inputs
          decimal_poi_d = input$decimal_poi_d,
          conf_poi_data = input$conf_poi_data,
          data_type_poi = input$data_type_poi,
          poi_data_selected_columns = input$poi_data_selected_columns,
          alt_poi_data = input$alt_poi_data,
          data_choice_ref_poi = input$data_choice_ref_poi,
          data_choice_data_poi = input$data_choice_data_poi,
          data_choice_g1_poi = input$data_choice_g1_poi,
          data_choice_g2_poi = input$data_choice_g2_poi,
          poi_test_data_ui2 = input$poi_test_data_ui2
        )
      })
    )
    
    # Set choice vectors for alternative hypothesis selectors
    observe({
      updateSelectInput(session, "alt_poi", choices = choice_poi_alt_1)
      updateSelectInput(session, "alt_poi_2", choices = choice_poi_alt_2)
    })
    
    # Render Enter Statistics results
    output$pretty_poi_stat <- renderUI({
      results <- poisson_result$poi_out()
      one_or_two_poi <- input$one_or_two_poi
      alt_poi <- input$alt_poi
      alt_poi_2 <- input$alt_poi_2
      R <- input$decimal_poi
      conf <- input$conf_poi
      n1 <- input$n_samp_poi
      n2 <- input$n_samp_poi_2
      c1 <- input$poi_samp
      c2 <- input$poi2
      
      if (is.null(results)) {
        return(HTML("<p>No results available. Please check your inputs.</p>"))
      }
      
      if (is.character(results)) {
        return(HTML(paste("<p>", results, "</p>")))
      }
      
      # Round results for display
      resultsR <- ro(results, R)
      
      # Determine alternative hypothesis number
      if (alt_poi == "two.sided") {
        alt_num <- 1
      } else if (alt_poi == "less") {
        alt_num <- 2
      } else if (alt_poi == "greater") {
        alt_num <- 3
      } else {
        alt_num <- 1
      }
      
      if (alt_poi_2 == "two.sided") {
        alt_num_p2 <- 1
      } else if (alt_poi_2 == "less") {
        alt_num_p2 <- 2
      } else if (alt_poi_2 == "greater") {
        alt_num_p2 <- 3
      } else {
        alt_num_p2 <- 1
      }
      
      if (one_or_two_poi == 1) {
        # One-sample Poisson test
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$c =$"), resultsR$statistic), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\lambda_{0} =$"), resultsR$null.value), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n= $"), resultsR$parameter), "</td>",
          "<td>", "</td>",
          "<td>", "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", withMathJax("$\\lambda: $")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", "to", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Exact test for ", names(choice_poi_alt_1[alt_num]), ": "), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      } else if (one_or_two_poi == 2) {
        # Two-sample Poisson test
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$c_{1} =$"), c1), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$c_{2} =$"), c2), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1}= $"), n1), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{2}= $"), n2), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", withMathJax("$\\lambda_{1}: $")), "</td>",
          "<td>", resultsR$estimate[4], "</td>",
          "<td>", "to", "</td>",
          "<td>", resultsR$estimate[5], "</td>",
          "</tr>",
          "<tr>",
          "<td align='right'>", paste(withMathJax("$\\lambda_{2}: $")), "</td>",
          "<td>", resultsR$estimate[6], "</td>",
          "<td>", "to", "</td>",
          "<td>", resultsR$estimate[7], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Exact test for ", names(choice_poi_alt_2[alt_num_p2]), ": "), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
    })
    
    # Render Use Data results
    output$pretty_poi_stat_data <- renderUI({
      results <- poisson_result$poi_data_out()
      alt <- input$alt_poi_data
      R <- input$decimal_poi_d
      conf <- input$conf_poi_data
      data <- filtered_data()
      data_type <- input$data_type_poi
      poi_data_selected_columns <- input$poi_data_selected_columns
      
      if (is.null(results)) {
        return(HTML("<p>No results available. Please check your inputs.</p>"))
      }
      
      if (is.character(results)) {
        return(HTML(paste("<p>", results, "</p>")))
      }
      
      # Round results for display
      resultsR <- ro(results, R)
      
      # Determine alternative hypothesis number
      if (alt == "two.sided") {
        alt_num <- 1
      } else if (alt == "less") {
        alt_num <- 2
      } else if (alt == "greater") {
        alt_num <- 3
      } else {
        alt_num <- 1
      }
      
      # Determine number of samples and get group names
      if (data_type == 1) {
        sel_col <- as.numeric(poi_data_selected_columns)
        if (!isTruthy(sel_col)) {
          return(NULL)
        }
        req(all(sel_col > 0), all(sel_col <= ncol(data)))
        samples <- length(sel_col)
        
        if (samples == 1) {
          # One-sample - get column name
          group1_name <- names(data)[sel_col[1]]
        } else if (samples == 2) {
          group1_name <- names(data)[sel_col[1]]
          group2_name <- names(data)[sel_col[2]]
        }
      } else {
        samples <- 2
        g1 <- input$data_choice_g1_poi
        g2 <- input$data_choice_g2_poi
        req(g1, g2)
        group1_name <- paste("Group 1 = ", g1)
        group2_name <- paste("Group 2 = ", g2)
      }
      
      if (samples == 1) {
        # One-sample Poisson test
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$c =$"), resultsR$statistic), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\lambda_{0} =$"), resultsR$null.value), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n= $"), resultsR$parameter), "</td>",
          "<td>", "</td>",
          "<td>", "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", withMathJax("$\\lambda: $")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", "to", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Exact test for ", names(choice_poi_alt_1[alt_num]), ": "), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      } else if (samples == 2) {
        # Two-sample Poisson test - need to get counts and n from data
        if (data_type == 1) {
          sel_col <- as.numeric(poi_data_selected_columns)
          req(all(sel_col > 0), all(sel_col <= ncol(data)))
          poi_data <- data[sel_col]
          count1 <- sum(na.omit(poi_data[[1]]))
          n1 <- nrow(na.omit(poi_data[1]))
          count2 <- sum(na.omit(poi_data[[2]]))
          n2 <- nrow(na.omit(poi_data[2]))
        } else {
          ref_col <- as.numeric(input$data_choice_ref_poi)
          data_col <- as.numeric(input$data_choice_data_poi)
          g1_id <- input$data_choice_g1_poi
          g2_id <- input$data_choice_g2_poi
          req(ref_col, data_col, g1_id, g2_id,
              ref_col > 0, ref_col <= ncol(data),
              data_col > 0, data_col <= ncol(data))
          
          poi_data1 <- data.frame(
            group = data[[ref_col]][data[[ref_col]] == g1_id],
            count = data[[data_col]][data[[ref_col]] == g1_id]
          )
          poi_data2 <- data.frame(
            group = data[[ref_col]][data[[ref_col]] == g2_id],
            count = data[[data_col]][data[[ref_col]] == g2_id]
          )
          req(length(poi_data1$count) > 0, length(poi_data2$count) > 0)
          count1 <- sum(na.omit(poi_data1$count))
          n1 <- length(na.omit(poi_data1$count))
          count2 <- sum(na.omit(poi_data2$count))
          n2 <- length(na.omit(poi_data2$count))
        }
        
        HTML(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr><td style='border-bottom:1px solid #000'>", group1_name, "</td><td style='border-bottom:1px solid #000'></td>",
          "<td style='border-bottom:1px solid #000'>", group2_name, "</td></tr>",
          "<tr>",
          "<td>", paste(withMathJax("$c_{1} =$"), count1), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$c_{2} =$"), count2), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1}= $"), n1), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{2}= $"), n2), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", withMathJax("$\\lambda_{1}: $")), "</td>",
          "<td>", resultsR[["estimate"]][["g1.lambda.lowerci"]], "</td>",
          "<td>", "to", "</td>",
          "<td>", resultsR[["estimate"]][["g1.lambda.upperci"]], "</td>",
          "</tr>",
          "<tr>",
          "<td align='right'>", paste(withMathJax("$\\lambda_{2}: $")), "</td>",
          "<td>", resultsR[["estimate"]][["g2.lambda.lowerci"]], "</td>",
          "<td>", "to", "</td>",
          "<td>", resultsR[["estimate"]][["g2.lambda.upperci"]], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Exact test for ", names(choice_poi_alt_2[alt_num]), ": "), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
    })
    
    # =========================================================================
    # POISSON UI RENDERING FUNCTIONS
    # =========================================================================
    
    # Column selection for data mode
    output$data_choice_column_poi <- renderUI({
      data <- filtered_data()
      req(data)
      
      choices <- seq(1, ncol(data))
      names(choices) <- names(data)
      
      checkboxGroupInput(
        inputId = ns("poi_data_selected_columns"),
        label = "Analyze which column(s)?",
        choices = choices
      )
    })
    
    # Reference column selection
    output$data_choice_ref_poi <- renderUI({
      data <- filtered_data()
      req(data)
      
      choices <- seq(1, ncol(data))
      names(choices) <- names(data)
      
      selectInput(
        inputId = ns("data_choice_ref_poi"),
        label = "Select Factor",
        choices = choices
      )
    })
    
    # Data column selection (when using reference)
    output$data_choice_data_poi <- renderUI({
      data <- filtered_data()
      ref_col <- input$data_choice_ref_poi
      req(data, ref_col)
      
      choices <- seq(1, ncol(data))
      names(choices) <- names(data)
      
      # Remove the selected reference column
      ref_selected <- as.numeric(ref_col)
      temp <- seq(1, length(choices))
      temp <- temp[-ref_selected]
      choices <- choices[temp]
      
      selectInput(
        inputId = ns("data_choice_data_poi"),
        label = "Select Data",
        choices = choices
      )
    })
    
    # Group 1 selection
    output$data_choice_g1_poi <- renderUI({
      data <- filtered_data()
      ref_col <- input$data_choice_ref_poi
      req(data, ref_col)
      
      factor_levels <- unique(na.omit(data[[as.numeric(ref_col)]]))
      
      selectInput(
        inputId = ns("data_choice_g1_poi"),
        label = "Group 1",
        choices = factor_levels
      )
    })
    
    # Group 2 selection
    output$data_choice_g2_poi <- renderUI({
      data <- filtered_data()
      ref_col <- input$data_choice_ref_poi
      g1 <- input$data_choice_g1_poi
      req(data, ref_col, g1)
      
      factor_levels <- unique(na.omit(data[[as.numeric(ref_col)]]))
      
      # Remove the selected group 1
      temp <- factor_levels[factor_levels != g1]
      
      selectInput(
        inputId = ns("data_choice_g2_poi"),
        label = "Group 2",
        choices = temp
      )
    })
    
    # Alternative hypothesis selector
    output$alt_poi_data <- renderUI({
      data_type <- input$data_type_poi
      req(data_type)
      
      if (data_type == 1) {
        # Columns mode
        poi_data_selected_columns <- input$poi_data_selected_columns
        if (!isTruthy(poi_data_selected_columns)) {
          return(HTML("<p>Select at least one data column</p>"))
        }
        samples <- length(poi_data_selected_columns)
      } else {
        # Reference mode
        factor_col <- input$data_choice_ref_poi
        if (!isTruthy(factor_col)) {
          return(HTML("<p>Select factor column</p>"))
        }
        samples <- 2
      }
      
      if (samples > 2) {
        return(HTML("<p>Select at most two columns</p>"))
      }
      
      if (samples == 1) {
        selectInput(
          inputId = ns("alt_poi_data"),
          label = "Alternative hypothesis for rates",
          choices = choice_poi_alt_1
        )
      } else if (samples == 2) {
        selectInput(
          inputId = ns("alt_poi_data"),
          label = "Alternative hypothesis for rates",
          choices = choice_poi_alt_2
        )
      }
    })
    
    # Dynamic UI elements showing statistics from data
    output$poi_test_data_ui1 <- renderUI({
      data <- filtered_data()
      data_type <- input$data_type_poi
      decimal_poi_d <- input$decimal_poi_d
      req(data, data_type, decimal_poi_d, nrow(data) > 0, ncol(data) > 0)
      
      if (data_type == 1) {
        # Columns mode
        sel_col <- as.numeric(input$poi_data_selected_columns)
        if (!isTruthy(sel_col) || length(sel_col) == 0) {
          return(NULL)
        }
        req(all(sel_col > 0), all(sel_col <= ncol(data)))
        samples <- length(sel_col)
        
        poi_data <- data[sel_col]
        count <- sum(na.omit(poi_data[[1]]))
        n <- nrow(na.omit(poi_data[1]))
        req(n > 0)
        lambda_val <- count / n
        
        if (samples == 2) {
          label <- withMathJax(paste("$\\lambda_{1} = $", ro(lambda_val, decimal_poi_d)))
        } else {
          label <- withMathJax(paste("$\\lambda = $", ro(lambda_val, decimal_poi_d)))
        }
        HTML(paste(label))
      } else {
        # Reference mode - show group 1 lambda
        ref_col <- as.numeric(input$data_choice_ref_poi)
        data_col <- as.numeric(input$data_choice_data_poi)
        g1_id <- input$data_choice_g1_poi
        req(ref_col, data_col, g1_id,
            ref_col > 0, ref_col <= ncol(data),
            data_col > 0, data_col <= ncol(data))
        
        poi_data <- data.frame(
          group = data[[ref_col]][data[[ref_col]] == g1_id],
          count = data[[data_col]][data[[ref_col]] == g1_id]
        )
        names(poi_data) <- c("Group", "Count")
        
        count <- sum(na.omit(poi_data$Count))
        n <- length(na.omit(poi_data$Count))
        req(n > 0)
        lambda_val <- count / n
        label <- withMathJax(paste("$\\lambda_{1} = $", ro(lambda_val, decimal_poi_d)))
        HTML(paste(label))
      }
    })
    
    output$poi_test_data_ui2 <- renderUI({
      data <- filtered_data()
      data_type <- input$data_type_poi
      decimal_poi_d <- input$decimal_poi_d
      req(data, data_type, decimal_poi_d, nrow(data) > 0, ncol(data) > 0)
      
      if (data_type == 1) {
        # Columns mode
        sel_col <- as.numeric(input$poi_data_selected_columns)
        if (!isTruthy(sel_col)) {
          return(NULL)
        }
        req(all(sel_col > 0), all(sel_col <= ncol(data)))
        samples <- length(sel_col)
        
        if (samples == 1) {
          # One-sample - parameter input for null hypothesis
          numericInput(
            inputId = ns("poi_test_data_ui2"),
            label = withMathJax("$\\lambda_{0}:{ }$"),
            value = 0.5,
            min = 0,
            width = "75px"
          )
        } else if (samples == 2) {
          # Two-sample - show group 2 lambda
          poi_data <- data[sel_col]
          count <- sum(na.omit(poi_data[[2]]))
          n <- nrow(na.omit(poi_data[2]))
          req(n > 0)
          lambda_val <- count / n
          label <- withMathJax(paste("$\\lambda_{2} = $", ro(lambda_val, decimal_poi_d)))
          HTML(paste(label))
        }
      } else {
        # Reference mode - show group 2 lambda
        ref_col <- as.numeric(input$data_choice_ref_poi)
        data_col <- as.numeric(input$data_choice_data_poi)
        g2_id <- input$data_choice_g2_poi
        req(ref_col, data_col, g2_id,
            ref_col > 0, ref_col <= ncol(data),
            data_col > 0, data_col <= ncol(data))
        
        poi_data <- data.frame(
          group = data[[ref_col]][data[[ref_col]] == g2_id],
          count = data[[data_col]][data[[ref_col]] == g2_id]
        )
        names(poi_data) <- c("Group", "Count")
        
        count <- sum(na.omit(poi_data$Count))
        n <- length(na.omit(poi_data$Count))
        req(n > 0)
        lambda_val <- count / n
        label <- withMathJax(paste("$\\lambda_{2} = $", ro(lambda_val, decimal_poi_d)))
        HTML(paste(label))
      }
    })
    
    output$poi_test_data_ui3 <- renderUI({
      data <- filtered_data()
      data_type <- input$data_type_poi
      decimal_poi_d <- input$decimal_poi_d
      req(data, data_type, decimal_poi_d, nrow(data) > 0, ncol(data) > 0)
      
      if (data_type == 1) {
        # Columns mode
        sel_col <- as.numeric(input$poi_data_selected_columns)
        if (!isTruthy(sel_col)) {
          return(NULL)
        }
        req(all(sel_col > 0), all(sel_col <= ncol(data)))
        samples <- length(sel_col)
        
        poi_data <- data[sel_col]
        n <- nrow(na.omit(poi_data[1]))
        req(n > 0)
        
        if (samples == 2) {
          label <- withMathJax(paste("$n_{1} = $", n))
        } else {
          label <- withMathJax(paste("$n = $", n))
        }
        HTML(paste(label))
      } else {
        # Reference mode - show group 1 size
        ref_col <- as.numeric(input$data_choice_ref_poi)
        data_col <- as.numeric(input$data_choice_data_poi)
        g1_id <- input$data_choice_g1_poi
        req(ref_col, data_col, g1_id,
            ref_col > 0, ref_col <= ncol(data),
            data_col > 0, data_col <= ncol(data))
        
        poi_data <- data.frame(
          group = data[[ref_col]][data[[ref_col]] == g1_id],
          count = data[[data_col]][data[[ref_col]] == g1_id]
        )
        names(poi_data) <- c("Group", "Count")
        
        n <- length(na.omit(poi_data$Count))
        label <- withMathJax(paste("$n_{1} = $", n))
        HTML(paste(label))
      }
    })
    
    output$poi_test_data_ui4 <- renderUI({
      data <- filtered_data()
      data_type <- input$data_type_poi
      decimal_poi_d <- input$decimal_poi_d
      req(data, data_type, decimal_poi_d, nrow(data) > 0, ncol(data) > 0)
      
      if (data_type == 1) {
        # Columns mode
        sel_col <- as.numeric(input$poi_data_selected_columns)
        if (!isTruthy(sel_col) || length(sel_col) == 1) {
          return(NULL)
        }
        req(all(sel_col > 0), all(sel_col <= ncol(data)))
        samples <- length(sel_col)
        
        if (samples == 2) {
          # Two-sample - show group 2 size
          poi_data <- data[sel_col]
          n <- nrow(na.omit(poi_data[2]))
          req(n > 0)
          label <- withMathJax(paste("$n_{2} = $", n))
          HTML(paste(label))
        }
      } else {
        # Reference mode - show group 2 size
        ref_col <- as.numeric(input$data_choice_ref_poi)
        data_col <- as.numeric(input$data_choice_data_poi)
        g2_id <- input$data_choice_g2_poi
        req(ref_col, data_col, g2_id,
            ref_col > 0, ref_col <= ncol(data),
            data_col > 0, data_col <= ncol(data))
        
        poi_data <- data.frame(
          group = data[[ref_col]][data[[ref_col]] == g2_id],
          count = data[[data_col]][data[[ref_col]] == g2_id]
        )
        names(poi_data) <- c("Group", "Count")
        
        n <- length(na.omit(poi_data$Count))
        label <- withMathJax(paste("$n_{2} = $", n))
        HTML(paste(label))
      }
    })
    
    # =========================================================================
    # NONPARAMETRIC MODULE
    # =========================================================================
    
    # Nonparametric server
    # Note: Mann-Whitney U button tracking is handled inside the server module
    nonparametric_result <- create_nonparametric_server(
      "nonparametric",
      filtered_data,
      reactive({
        list(
          # Enter Statistics mode inputs
          conf_np = input$conf_np,
          np_decimals = input$np_decimals,
          choice_np_alt_stat = input$choice_np_alt_stat,
          np_tests = input$np_tests,
          npUI1 = input$npUI1,
          npUI2 = input$npUI2,
          npUI3 = input$npUI3,
          npUI4 = input$npUI4,
          npUI5 = input$npUI5,
          npUI6 = input$npUI6,
          # Use Data mode inputs
          conf_np_data = input$conf_np_data,
          decimal_np_data = input$decimal_np_data,
          data_type_np = input$data_type_np,
          np_data_selected_columns = input$np_data_selected_columns,
          np_tests_data = input$np_tests_data,
          choice_np_alt_data = input$choice_np_alt_stat_data,
          data_choice_ref_np = input$data_choice_ref_np,
          data_choice_data_np = input$data_choice_data_np,
          data_choice_g1_np = input$data_choice_g1_np,
          data_choice_g2_np = input$data_choice_g2_np,
          np_data_UI2 = input$np_data_UI2,
          np_mc_pass = input$np_mc_pass,
          np_data_u_go = input$np_data_u_go
        )
      })
    )
    
    # =========================================================================
    # ENTER STATISTICS MODE - UI RENDERING FUNCTIONS
    # =========================================================================
    
    output$np_tests <- renderUI({
      one_or_two_np <- input$one_or_two_np
      req(one_or_two_np)
      
      if (one_or_two_np == 1) {
        np_test_choice <- c(1, 2)
        names(np_test_choice) <- c("Sign Test for Location", "Wilcoxon Test for Location")
      } else if (one_or_two_np == 2) {
        np_test_choice <- c(3, 4)
        names(np_test_choice) <- c("Wilcoxon-Mann-Whitney U", "Two-Sample Median")
      } else if (one_or_two_np == 3) {
        np_test_choice <- c(5, 6, 7)
        names(np_test_choice) <- c("Dependent Sign Test", "Dependent Wilcoxon Signed Ranks", "McNemar's Test of Change")
      }
      
      radioButtons(inputId = ns("np_tests"), label = "Select Test", choices = np_test_choice)
    })
    
    # More info observeEvent - using sweetalert
    observeEvent(input$np_more_info, {
      np_more_info <- input$np_more_info
      np_tests <- input$np_tests
      req(np_more_info, np_tests)
      
      if (!np_more_info) return()
      
      if (np_tests == 1) {
        title <- "One-Sample Sign Test for Location"
        text_out <- HTML("The one-sample sign test for location is simply a test to see if the number of observations above the location of interest is equal to the number below that location. You can get exactly the same answer by testing the proportion above or below some number against a null hypothesis of π = 0.5. Ties are not counted. It has the same assumptions as the one-sample binomial test, namely that the samples are randomly drawn from a population with a constant probability (in this case of being above or below some location) and are independent. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Sign_test'>Wikipedia</a>")
      } else if (np_tests == 2) {
        title <- "One-Sample Wilcoxon Signed Ranks Test"
        text_out <- HTML("The one-sample Wilcoxon Signed Ranks Test for location is an alternative to the one-sample t-test when the normality assumption of the t-test cannot be assumed. The data must be ordinal and independent. To calculate the sum of the ranks, calculate the difference of each observation from the selected location, drop any observations of zero difference, take the absolute value of the differences, and get the average rank of these for each observation. Sum the average ranks for those observations falling above and below the selected location. The smaller of the two will be used to generate a z-score and p-value. </br></br>For more information see <a href='https://nyuwinthrop.org/wp-content/uploads/2019/08/wilcoxon-sign-rank-test-one-sample.pdf'>NYU Winthrop</a>")
      } else if (np_tests == 3) {
        title <- "Two-Sample Wilcoxon-Mann-Whitney U Test"
        text_out <- HTML("The two-sample Wilcoxon-Mann-Whitney U Test for equality of distributions with particular sensitivity to location. It is an alternative to the two-sample t-test when the normality assumption of the t-test cannot be assumed. The data must be ordinal and independent with approximately the same dispersion. To calculate the sum of the ranks for each group assign an average rank to all the observations from both groups. Sum the average ranks for each group. A U statistic is calculated for each group and the smaller one is used to generate a z-score and p-value. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test'>Wikipedia</a>")
      } else if (np_tests == 4) {
        title <- "Two-Sample Median Test"
        text_out <- HTML("The two-sample median test assesses whether the median of two samples are equal. It does have lower power than the Wilcoxon-Mann-Whitney test, so that test is preferred unless the dispersions are not approximately equal or if one or more observation exceeds the measurement device scale (pegs the dial). </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Median_test'>Wikipedia</a>")
      } else if (np_tests == 5) {
        title <- "Two-Sample Dependent Sign Test"
        text_out <- HTML("The two-sample dependent sign test is based on the binomial distribution like the one-sample sign test. It only assumes the data are independent, comes from the same population, and are at least ordinal. For each paired value, the difference is either positive, negative, or equal. If there is no difference between the dependent observations, it should be a 50-50 chance that one is above the other, and that is assessed with an exact binomial test.</br></br>For more information see <a href='https://en.wikipedia.org/wiki/Sign_test'>Wikipedia</a>")
      } else if (np_tests == 6) {
        title <- "Two-Sample Dependent Wilcoxon Signed Ranks Test"
        text_out <- HTML("The two-sample dependent Wilcoxon Signed Ranks test is an alternative to the paired t-test when the assumption of normally distributed difference cannot be assumed. To calculate the sum of the ranks, calculate the difference between each pair of observations, drop any observations of zero difference, take the absolute value of the differences, and get the average rank of these for each observation. Sum up the ranks that came from a positive difference and sum up the ranks that came from a negative difference. The negative sum will be subtracted from the positive sum to generate the W statistic which will be used to generate a z-score and p-value.</br></br>For more information see <a href='https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test'>Wikipedia</a>")
      } else if (np_tests == 7) {
        title <- "McNemar's Test of Change"
        text_out <- HTML("McNemar's Test of Change is used on paired dichotomous data. It is commonly used to determine if there is a difference is caused by some treatment. The same units are classified before the intervention, then afterward. The test only concerns the two cells P1F2 and F1P2 and is a binomial test to determine of the probability of these events has changed. If the probabilities are significantly different, the conclusion is that the treatment has caused a some change, for good or bad. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/McNemar%27s_test'>Wikipedia</a>")
      }
      
      sendSweetAlert(session = session, title = title, text = text_out, html = TRUE, 
                     showCloseButton = TRUE, btn_labels = "Close", type = "info")
      updateCheckboxInput(inputId = "np_more_info", value = FALSE)
    })
    
    # More info observeEvent for Use Data tab - using sweetalert
    observeEvent(input$np_more_info_data, {
      np_more_info_data <- input$np_more_info_data
      np_tests_data <- input$np_tests_data
      req(np_more_info_data, np_tests_data)
      
      if (!np_more_info_data) return()
      
      if (np_tests_data == 1) {
        title <- "One-Sample Sign Test for Location"
        text_out <- HTML("The one-sample sign test for location is simply a test to see if the number of observations above the location of interest is equal to the number below that location. You can get exactly the same answer by testing the proportion above or below some number against a null hypothesis of π = 0.5. Ties are not counted. It has the same assumptions as the one-sample binomial test, namely that the samples are randomly drawn from a population with a constant probability (in this case of being above or below some location) and are independent. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Sign_test'>Wikipedia</a>")
      } else if (np_tests_data == 2) {
        title <- "One-Sample Wilcoxon Signed Ranks Test"
        text_out <- HTML("The one-sample Wilcoxon Signed Ranks Test for location is an alternative to the one-sample t-test when the normality assumption of the t-test cannot be assumed. The data must be ordinal and independent. To calculate the sum of the ranks, calculate the difference of each observation from the selected location, drop any observations of zero difference, take the absolute value of the differences, and get the average rank of these for each observation. Sum the average ranks for those observations falling above and below the selected location. The smaller of the two will be used to generate a z-score and p-value. </br></br>For more information see <a href='https://nyuwinthrop.org/wp-content/uploads/2019/08/wilcoxon-sign-rank-test-one-sample.pdf'>NYU Winthrop</a>")
      } else if (np_tests_data == 3) {
        title <- "Two-Sample Wilcoxon-Mann-Whitney U Test"
        text_out <- HTML("The two-sample Wilcoxon-Mann-Whitney U Test for equality of distributions with particular sensitivity to location. It is an alternative to the two-sample t-test when the normality assumption of the t-test cannot be assumed. The data must be ordinal and independent with approximately the same dispersion. To calculate the sum of the ranks for each group assign an average rank to all the observations from both groups. Sum the average ranks for each group. A U statistic is calculated for each group and the smaller one is used to generate a z-score and p-value. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test'>Wikipedia</a>")
      } else if (np_tests_data == 4) {
        title <- "Two-Sample Median Test"
        text_out <- HTML("The two-sample median test assesses whether the median of two samples are equal. It does have lower power than the Wilcoxon-Mann-Whitney test, so that test is preferred unless the dispersions are not approximately equal or if one or more observation exceeds the measurement device scale (pegs the dial). </br></br>For more information see <a href='https://en.wikipedia.org/wiki/Median_test'>Wikipedia</a>")
      } else if (np_tests_data == 5) {
        title <- "Two-Sample Dependent Sign Test"
        text_out <- HTML("The two-sample dependent sign test is based on the binomial distribution like the one-sample sign test. It only assumes the data are independent, comes from the same population, and are at least ordinal. For each paired value, the difference is either positive, negative, or equal. If there is no difference between the dependent observations, it should be a 50-50 chance that one is above the other, and that is assessed with an exact binomial test.</br></br>For more information see <a href='https://en.wikipedia.org/wiki/Sign_test'>Wikipedia</a>")
      } else if (np_tests_data == 6) {
        title <- "Two-Sample Dependent Wilcoxon Signed Ranks Test"
        text_out <- HTML("The two-sample dependent Wilcoxon Signed Ranks test is an alternative to the paired t-test when the assumption of normally distributed difference cannot be assumed. To calculate the sum of the ranks, calculate the difference between each pair of observations, drop any observations of zero difference, take the absolute value of the differences, and get the average rank of these for each observation. Sum up the ranks that came from a positive difference and sum up the ranks that came from a negative difference. The negative sum will be subtracted from the positive sum to generate the W statistic which will be used to generate a z-score and p-value.</br></br>For more information see <a href='https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test'>Wikipedia</a>")
      } else if (np_tests_data == 7) {
        title <- "McNemar's Test of Change"
        text_out <- HTML("McNemar's Test of Change is used on paired dichotomous data. It is commonly used to determine if there is a difference is caused by some treatment. The same units are classified before the intervention, then afterward. The test only concerns the two cells P1F2 and F1P2 and is a binomial test to determine of the probability of these events has changed. If the probabilities are significantly different, the conclusion is that the treatment has caused a some change, for good or bad. </br></br>For more information see <a href='https://en.wikipedia.org/wiki/McNemar%27s_test'>Wikipedia</a>")
      }
      
      sendSweetAlert(session = session, title = title, text = text_out, html = TRUE, 
                     showCloseButton = TRUE, btn_labels = "Close", type = "info")
      updateCheckboxInput(inputId = "np_more_info_data", value = FALSE)
    })
    
    output$np_alt <- renderUI({
      np_tests <- input$np_tests
      req(np_tests)
      
      choice_np_alt <- c("two.sided", "less", "greater")
      names(choice_np_alt) <- c(
        choice_np_alt_text[as.numeric(np_tests) * 3 - 2],
        choice_np_alt_text[as.numeric(np_tests) * 3 - 1],
        choice_np_alt_text[as.numeric(np_tests) * 3]
      )
      
      selectInput(inputId = ns("choice_np_alt_stat"), label = "Alternative Hypothesis", choices = choice_np_alt)
    })
    
    # npUI1 through npUI6 rendering functions - following pattern from monolithic app
    output$npUI1 <- renderUI({
      np_tests <- input$np_tests
      req(np_tests)
      
      if (np_tests == 1) {
        numericInput(inputId = ns("npUI1"), label = withMathJax("$n_{above}:{ }$"), value = 2, min = 0, step = 1)
      } else if (np_tests == 2) {
        numericInput(inputId = ns("npUI1"), label = withMathJax("$S^{+}:{ }$"), value = 3, min = 0, step = 1)
      } else if (np_tests == 3) {
        numericInput(inputId = ns("npUI1"), label = withMathJax("$S_{1}:{ }$"), value = 32, min = 0, step = 1)
      } else if (np_tests == 4) {
        numericInput(inputId = ns("npUI1"), label = withMathJax("$n_{1 \\;above}:{ }$"), value = 1, min = 0, step = 1)
      } else if (np_tests == 5) {
        numericInput(inputId = ns("npUI1"), label = withMathJax("$n^+:{ }$"), value = 8, min = 0, step = 1)
      } else if (np_tests == 6) {
        numericInput(inputId = ns("npUI1"), label = withMathJax("$S^{+}:{ }$"), value = 27, min = 0, step = 1)
      } else if (np_tests == 7) {
        HTML("<p style='text-align:center'><b>Pass 2</b></p>")
      }
    })
    
    output$npUI2 <- renderUI({
      np_tests <- input$np_tests
      req(np_tests)
      
      if (np_tests == 1) {
        NULL
      } else if (np_tests == 2) {
        numericInput(inputId = ns("npUI2"), label = withMathJax("$S^{-}:{ }$"), value = 150, min = 0, step = 1)
      } else if (np_tests == 3) {
        numericInput(inputId = ns("npUI2"), label = withMathJax("$S_{2}:{ }$"), value = 46, min = 0, step = 1)
      } else if (np_tests == 4) {
        numericInput(inputId = ns("npUI2"), label = withMathJax("$n_{2 \\;above}:{ }$"), value = 5, min = 0, step = 1)
      } else if (np_tests == 5 || np_tests == 6) {
        NULL
      } else if (np_tests == 7) {
        HTML("<p style='text-align:center'><b>Fail 2</b></p>")
      }
    })
    
    output$npUI3 <- renderUI({
      np_tests <- input$np_tests
      req(np_tests)
      
      if (np_tests == 1) {
        numericInput(inputId = ns("npUI3"), label = withMathJax("$n_{equal}:{ }$"), value = 0, min = 0, step = 1)
      } else if (np_tests == 2) {
        numericInput(inputId = ns("npUI3"), label = withMathJax("$n_{Adj.}:{ }$"), value = 17, min = 1, step = 1)
      } else if (np_tests == 3) {
        numericInput(inputId = ns("npUI3"), label = withMathJax("$n_{1}:{ }$"), value = 6, min = 0, step = 1)
      } else if (np_tests == 4) {
        numericInput(inputId = ns("npUI3"), label = withMathJax("$n_{1 \\;equal}:{ }$"), value = 0, min = 0, step = 1)
      } else if (np_tests == 5) {
        numericInput(inputId = ns("npUI3"), label = withMathJax("$n^=:{ }$"), value = 0, min = 0, step = 1)
      } else if (np_tests == 6) {
        numericInput(inputId = ns("npUI3"), label = withMathJax("$S^{-}:{ }$"), value = 18, min = 0, step = 1)
      } else if (np_tests == 7) {
        numericInput(inputId = ns("npUI3"), label = "Pass 1", value = 56, min = 0, step = 1)
      }
    })
    
    output$npUI4 <- renderUI({
      np_tests <- input$np_tests
      req(np_tests)
      
      if (np_tests %in% c(1, 2)) {
        NULL
      } else if (np_tests == 3) {
        numericInput(inputId = ns("npUI4"), label = withMathJax("$n_{2}:{ }$"), value = 6, min = 0, step = 1)
      } else if (np_tests == 4) {
        numericInput(inputId = ns("npUI4"), label = withMathJax("$n_{2 \\;equal}:{ }$"), value = 0, min = 0, step = 1)
      } else if (np_tests %in% c(5, 6)) {
        NULL
      } else if (np_tests == 7) {
        numericInput(inputId = ns("npUI4"), label = "Pass 1", value = 4, min = 0, step = 1)
      }
    })
    
    output$npUI5 <- renderUI({
      np_tests <- input$np_tests
      req(np_tests)
      
      if (np_tests == 1) {
        numericInput(inputId = ns("npUI5"), label = withMathJax("$n_{below}:{ }$"), value = 8, min = 0, step = 1)
      } else if (np_tests %in% c(2, 3)) {
        NULL
      } else if (np_tests == 4) {
        numericInput(inputId = ns("npUI5"), label = withMathJax("$n_{1 \\;below}:{ }$"), value = 5, min = 0, step = 1)
      } else if (np_tests == 5) {
        numericInput(inputId = ns("npUI5"), label = withMathJax("$n^-:{ }$"), value = 2, min = 0, step = 1)
      } else if (np_tests == 6) {
        numericInput(inputId = ns("npUI5"), label = withMathJax("$n:{ }$"), value = 10, min = 1, step = 1)
      } else if (np_tests == 7) {
        numericInput(inputId = ns("npUI5"), label = "Fail 1", value = 56, min = 1, step = 1)
      }
    })
    
    output$npUI6 <- renderUI({
      np_tests <- input$np_tests
      req(np_tests)
      
      if (np_tests %in% c(1, 2, 3, 5, 6)) {
        NULL
      } else if (np_tests == 4) {
        numericInput(inputId = ns("npUI6"), label = withMathJax("$n_{2 \\;below}:{ }$"), value = 1, min = 0, step = 1)
      } else if (np_tests == 7) {
        numericInput(inputId = ns("npUI6"), label = "Fail 1", value = 4, min = 1, step = 1)
      }
    })
    
    # =========================================================================
    # ENTER STATISTICS MODE - RESULTS RENDERING
    # =========================================================================
    
    output$pretty_nonparametric <- renderUI({
      results <- nonparametric_result$np_stat_out()
      req(results)
      
      conf <- input$conf_np
      R <- input$np_decimals
      alt <- input$choice_np_alt_stat
      np_tests <- input$np_tests
      npUI1 <- input$npUI1
      npUI2 <- input$npUI2
      npUI3 <- input$npUI3
      npUI4 <- input$npUI4
      npUI5 <- input$npUI5
      npUI6 <- input$npUI6
      
      req(conf, R, alt, np_tests)
      
      # Round results at rendering stage
      resultsR <- ro(results, R)
      
      if (alt == "two.sided") {
        alt_num <- 1
      } else if (alt == "less") {
        alt_num <- 2
      } else if (alt == "greater") {
        alt_num <- 3
      }
      
      # Test 1: One-sample sign test
      if (np_tests == 1) {
        HTML(c(
          paste("<b>Sign Test for Location</b><br>", "<b>", "Method: ", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{above} = $"), npUI1), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{equal} = $"), npUI3), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{below} = $"), npUI5), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$p_{below} = $"), resultsR$statistic), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{observed} = $"), npUI1 + npUI3 + npUI5), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{included} = $"), resultsR$estimate[2]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for proportion below = ", resultsR$parameter), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", withMathJax("$\\pi_{below} :$")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_np_alt_text[3 * (as.numeric(np_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste(withMathJax("$p_{below} = $"), resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          "<table><tr><td>", beta_statement, resultsR$estimate[4], "</td></tr>",
          "</table>"
        ))
      }
      # Test 2: One-sample Wilcoxon signed ranks test
      else if (np_tests == 2) {
        HTML(c(
          paste("<b>", "Method: ", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$S^{+} = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$S^{-} = $"), resultsR$estimate[2]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{Adj.} = $"), resultsR$estimate[3]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence"), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_np_alt_text[3 * (as.numeric(np_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("W = ", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 3: Two-sample Mann-Whitney U test
      else if (np_tests == 3) {
        group1_name <- "Group 1"
        group2_name <- "Group 2"
        HTML(c(
          paste("<b>", "Method: ", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr><td style='border-bottom:1px solid #000'>", group1_name, "</td><td style='border-bottom:1px solid #000'></td>",
          "<td style='border-bottom:1px solid #000'>", group2_name, "</td></tr>",
          "<tr>",
          "<td>", paste(withMathJax("$S_{1} = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$S_{2} = $"), resultsR$estimate[2]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1} = $"), resultsR$estimate[3]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{2} = $"), resultsR$estimate[4]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence"), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_np_alt_text[3 * (as.numeric(np_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("U = ", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 4: Two-sample median test (Mood's)
      else if (np_tests == 4) {
        HTML(c(
          paste("<b>", "Two-Sample Median Test<br>Method: ", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1\\; above} = $"), resultsR$estimate[3]), "</td>",
          "<td>", paste(withMathJax("$p_{1\\; above} = $"), resultsR$estimate[1]), "</td>",
          "<td>", paste(withMathJax("$n_{2\\; above} = $"), resultsR$estimate[9]), "</td>",
          "<td>", paste(withMathJax("$p_{2\\; above} = $"), resultsR$estimate[7]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1\\; equal} = $"), npUI3), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{2\\; equal} = $"), npUI4, "</td>"),
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1\\; below} = $"), resultsR$estimate[4]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{2\\; below} = $"), resultsR$estimate[10]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1\\; total\\; inc.} = $"), resultsR$estimate[2]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{2\\; total\\; inc.} = $"), resultsR$estimate[8]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", withMathJax("$\\pi_{1 \\;above} :$")), "</td>",
          "<td>", resultsR$estimate[5], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[6], "</td>",
          "</tr>",
          "<tr>",
          "<td align='right'>", paste(withMathJax("$\\pi_{2 \\;above} :$")), "</td>",
          "<td>", resultsR$estimate[11], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[12], "</td>",
          "</tr>",
          "<tr>",
          "<td align='right'>", "Odds Ratio:", "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_np_alt_text[3 * (as.numeric(np_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 5: Two-sample dependent sign test
      else if (np_tests == 5) {
        HTML(c(
          paste("<b>Two-Sample Dependent Sign Test for Location</b><br>", "<b>", "Method: ", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$n^{+} = $"), npUI1), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$p^{+} = $"), resultsR$statistic), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n^{=} = $"), npUI3), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n^{-} = $"), npUI5), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{observed} = $"), npUI1 + npUI3 + npUI5), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{included} = $"), resultsR$estimate[2]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for the probability that the difference in each pair is positive = ", resultsR$parameter), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", withMathJax("$\\pi^{+} :$")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_np_alt_text[3 * (as.numeric(np_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste(withMathJax("$p^{+} = $"), resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          "<table><tr>,<td>", beta_statement, resultsR$estimate[4], "</td></tr>",
          "</table>"
        ))
      }
      # Test 6: Two-sample dependent Wilcoxon signed ranks test
      else if (np_tests == 6) {
        HTML(c(
          paste("<b>", "Method: Dependent ", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$S^{+} = $"), resultsR$estimate[1]), "</td>",
          "</tr>",
          "<td>", paste(withMathJax("$S^{-} = $"), resultsR$estimate[2]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), resultsR$estimate[3]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence"), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_np_alt_text[3 * (as.numeric(np_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("W = ", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 7: McNemar's test of change
      else if (np_tests == 7) {
        HTML(c(
          paste("<b>", "Method: ", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$P_{1}F_{2} = $"), resultsR$estimate[1]), "</td><td></td>",
          "<td>", paste(withMathJax("$p(b) = $"), resultsR$estimate[2]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$F_{1}P_{2} = $"), resultsR$estimate[3]), "</td><td></td>",
          "<td>", paste(withMathJax("$p(c) = $"), resultsR$estimate[4]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence"), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_np_alt_text[3 * (as.numeric(np_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
    })
    
    # =========================================================================
    # USE DATA MODE - UI RENDERING FUNCTIONS
    # =========================================================================
    
    # Column selection for Use Data mode
    output$data_choice_column_np <- renderUI({
      data <- filtered_data()
      req(data)
      
      choices <- seq(1:ncol(data))
      names(choices) <- names(data)
      
      checkboxGroupInput(
        inputId = ns("np_data_selected_columns"),
        label = "Analyze which column(s)?",
        choices = choices
      )
    })
    
    # Number of selected columns reactive
    num_selected_columns_np_data <- reactive({
      np_data_selected_columns <- input$np_data_selected_columns
      if (is.null(np_data_selected_columns)) return(0)
      length(np_data_selected_columns)
    })
    
    # Independent or Dependent selection
    output$ind_or_dep_np_data <- renderUI({
      columns <- num_selected_columns_np_data()
      type <- input$data_type_np
      req(type)
      
      if (columns == 2 && type == 1) {
        selectInput(
          inputId = ns("dep_or_indep_np_data"),
          label = "Independent or Dependent Test?",
          choices = c("Independent" = 1, "Dependent" = 2)
        )
      } else if (type == 2) {
        selectInput(
          inputId = ns("dep_or_indep_np_data"),
          label = "Independent Only",
          choices = c("Independent" = 1)
        )
      }
    })
    
    # Test selection for Use Data mode
    output$np_tests_data <- renderUI({
      num_selected_columns <- num_selected_columns_np_data()
      dep_or_indep <- input$dep_or_indep_np_data
      type <- input$data_type_np
      req(type, num_selected_columns)
      
      if (num_selected_columns == 1 && type == 1) {
        np_test_choice <- c(1, 2)
        names(np_test_choice) <- c("Sign Test for Location", "Wilcoxon Test for Location")
      } else if (num_selected_columns == 2 || type == 2) {
        req(dep_or_indep)
        if (dep_or_indep == 1) {
          np_test_choice <- c(3, 4)
          names(np_test_choice) <- c("Wilcoxon-Mann-Whitney U", "Two-Sample Median")
        } else if (dep_or_indep == 2) {
          np_test_choice <- c(5:7)
          names(np_test_choice) <- c("Dependent Sign Test", "Dependent Wilcoxon Signed Ranks", "McNemar's Test of Change")
        }
      }
      
      if (type == 1 && (num_selected_columns < 1 || num_selected_columns > 2)) {
        h3("Select one or two columns of data")
      } else {
        radioButtons(
          inputId = ns("np_tests_data"),
          label = "Select Test",
          choices = np_test_choice
        )
      }
    })
    
    # Reference column mode UI functions (similar to other modules)
    output$data_choice_ref_np <- renderUI({
      data <- filtered_data()
      req(data)
      choices <- seq(1:ncol(data))
      names(choices) <- names(data)
      selectInput(
        inputId = ns("data_choice_ref_np"),
        label = "Select Factor",
        multiple = FALSE,
        choices = choices
      )
    })
    
    output$data_choice_data_np <- renderUI({
      data <- filtered_data()
      req(data)
      choices <- seq(1:ncol(data))
      names(choices) <- names(data)
      factor <- input$data_choice_ref_np
      req(factor)
      
      fact_selected <- as.numeric(factor)
      temp <- seq(1:length(choices))
      temp <- temp[-fact_selected]
      choices <- choices[temp]
      
      selectInput(
        inputId = ns("data_choice_data_np"),
        label = "Select Data",
        multiple = FALSE,
        choices = choices
      )
    })
    
    output$data_choice_g1_np <- renderUI({
      data <- filtered_data()
      req(data)
      ref <- input$data_choice_ref_np
      req(ref)
      
      factor <- unique(data[[as.numeric(ref)]])
      selectInput(inputId = ns("data_choice_g1_np"), label = "Group 1", choices = factor)
    })
    
    output$data_choice_g2_np <- renderUI({
      data <- filtered_data()
      req(data)
      ref <- input$data_choice_ref_np
      factor_g1 <- input$data_choice_g1_np
      req(ref, factor_g1)
      
      factor <- unique(data[[as.numeric(ref)]])
      temp <- factor[-which(x = factor == factor_g1)]
      selectInput(inputId = ns("data_choice_g2_np"), label = "Group 2", choices = temp)
    })
    
    # McNemar's pass value selection
    output$np_mc_pass <- renderUI({
      test <- input$np_tests_data
      req(test)
      if (test != 7) return()
      
      columns <- as.numeric(input$np_data_selected_columns)
      data <- filtered_data()
      req(columns, data, test)
      
      if (length(columns) > 2 || !isTruthy(columns)) {
        return(HTML("Select one or two columns"))
      }
      
      cats <- unique(c(
        na.omit(unique(data[[columns[1]]])),
        na.omit(unique(data[[columns[2]]]))
      ))
      num_cat <- length(cats)
      
      if (num_cat > 2) {
        return(HTML("Needs binomial data"))
      }
      
      selectInput(inputId = ns("np_mc_pass"), label = "Pass Indicated by", choices = cats)
    })
    
    # Alternative hypothesis for Use Data mode
    output$np_alt_data <- renderUI({
      np_tests <- input$np_tests_data
      req(np_tests)
      
      choice_np_alt <- c("two.sided", "less", "greater")
      names(choice_np_alt) <- c(
        choice_np_alt_text[as.numeric(np_tests) * 3 - 2],
        choice_np_alt_text[as.numeric(np_tests) * 3 - 1],
        choice_np_alt_text[as.numeric(np_tests) * 3]
      )
      
      selectInput(
        inputId = ns("choice_np_alt_stat_data"),
        label = "Alternative Hypothesis",
        choices = choice_np_alt
      )
    })
    
    # Dynamic UI for Use Data mode (npUI1_data through npUI6_data)
    # These display calculated statistics from the data
    # Implementation follows pattern from monolithic app - showing medians, counts, etc.
    
    output$npUI1_data <- renderUI({
      np_tests <- input$np_tests_data
      np_data_selected_columns <- input$np_data_selected_columns
      data_type <- input$data_type_np
      data <- filtered_data()
      
      req(np_tests, data)
      
      if (np_tests == 1) {
        # One-sample sign test - show median
        req(np_data_selected_columns)
        x <- data[, as.numeric(np_data_selected_columns[1])]
        HTML(paste(withMathJax("$\\widetilde{X}_{data} = $"), median(x, na.rm = TRUE)))
      } else if (np_tests == 2) {
        # One-sample Wilcoxon signed ranks test - show median
        req(np_data_selected_columns)
        x <- data[, as.numeric(np_data_selected_columns[1])]
        HTML(paste(withMathJax("$\\widetilde{X}_{data} = $"), median(x, na.rm = TRUE)))
      } else if (np_tests == 3 || np_tests == 4) {
        # Two-sample tests - show group 1 median
        if (data_type == 1) {
          req(np_data_selected_columns)
          x1 <- data[, as.numeric(np_data_selected_columns[1])]
        } else if (data_type == 2) {
          ref_col <- as.numeric(input$data_choice_ref_np)
          data_col <- as.numeric(input$data_choice_data_np)
          req(ref_col, data_col)
          x1 <- data[[data_col]][which(data[[ref_col]] == input$data_choice_g1_np)]
        }
        HTML(paste(withMathJax("$\\widetilde{X}_{1} =$"), median(na.omit(x1))))
      } else if (np_tests == 5) {
        # Two-sample dependent sign test
        req(np_data_selected_columns, length(np_data_selected_columns) >= 2)
        x1 <- data[, as.numeric(np_data_selected_columns[1])]
        x2 <- data[, as.numeric(np_data_selected_columns[2])]
        temp <- median.test.twosample.dependent.signtest(g1 = x1, g2 = x2)
        temp <- temp$estimate[3]
        HTML(paste(withMathJax("$n^{+} =$"), temp))
      } else if (np_tests == 6) {
        # Two-sample dependent Wilcoxon signed ranks test
        req(np_data_selected_columns, length(np_data_selected_columns) >= 2)
        x1 <- data[, as.numeric(np_data_selected_columns[1])]
        x2 <- data[, as.numeric(np_data_selected_columns[2])]
        len <- length(x2)
        n <- seq(1:len)
        n_plus <- 0
        for (val in n) {
          if (x1[val] > x2[val]) {
            n_plus <- n_plus + 1
          }
        }
        HTML(paste(withMathJax("$n^{+} =$"), n_plus))
      } else if (np_tests == 7) {
        # McNemar's test
        pass <- input$np_mc_pass
        req(np_data_selected_columns, data, pass, length(np_data_selected_columns) >= 2)
        x1 <- data[, as.numeric(np_data_selected_columns[1])]
        x2 <- data[, as.numeric(np_data_selected_columns[2])]
        cats <- unique(c(na.omit(unique(data[[as.numeric(np_data_selected_columns[1])]])), 
                         na.omit(unique(data[[as.numeric(np_data_selected_columns[2])]]))))
        cats <- c(cats[cats == pass], cats[cats != pass])
        mctable <- table(factor(x1, levels = cats, labels = c("Pass", "Fail")),
                         factor(x2, levels = cats, labels = c("Pass", "Fail")))
        a <- mctable[1, 1]
        HTML(paste0(withMathJax("$P_{1}P_{2}=$"), a))
      }
    })
    
    output$npUI2_data <- renderUI({
      np_tests <- input$np_tests_data
      data_type <- input$data_type_np
      data <- filtered_data()
      
      req(np_tests)
      
      if (np_tests == 1) {
        # One-sample sign test - input for M0
        numericInput(inputId = ns("np_data_UI2"), label = withMathJax("$M_0{ }$"), value = 2, min = 0, step = 1)
      } else if (np_tests == 2) {
        # One-sample Wilcoxon signed ranks test - input for M0
        numericInput(inputId = ns("np_data_UI2"), label = withMathJax("$M_0:{ }$"), value = 3, min = 0, step = 1)
      } else if (np_tests == 3 || np_tests == 4) {
        # Two-sample tests - show group 2 median
        req(data_type)
        if (data_type == 1) {
          np_data_selected_columns <- input$np_data_selected_columns
          req(np_data_selected_columns, data, length(np_data_selected_columns) >= 2)
          x2 <- data[, as.numeric(np_data_selected_columns[2])]
        } else if (data_type == 2) {
          ref_col <- as.numeric(input$data_choice_ref_np)
          data_col <- as.numeric(input$data_choice_data_np)
          req(ref_col, data_col)
          x2 <- data[[data_col]][which(data[[ref_col]] == input$data_choice_g2_np)]
        }
        HTML(paste(withMathJax("$\\widetilde{X}_{2} =$"), median(na.omit(x2))))
      } else if (np_tests == 5 || np_tests == 6) {
        NULL
      } else if (np_tests == 7) {
        # McNemar's test
        req(data_type)
        if (data_type == 1) {
          np_data_selected_columns <- input$np_data_selected_columns
        } else {
          # For data_type == 2 (Reference Column mode), McNemar's test may not be applicable
          # or needs different handling - return NULL for now
          return(NULL)
        }
        pass <- input$np_mc_pass
        req(np_data_selected_columns, data, pass, length(np_data_selected_columns) >= 2)
        x1 <- data[, as.numeric(np_data_selected_columns[1])]
        x2 <- data[, as.numeric(np_data_selected_columns[2])]
        cats <- unique(c(na.omit(unique(data[[as.numeric(np_data_selected_columns[1])]])), 
                         na.omit(unique(data[[as.numeric(np_data_selected_columns[2])]]))))
        cats <- c(cats[cats == pass], cats[cats != pass])
        mctable <- table(factor(x1, levels = cats, labels = c("Pass", "Fail")),
                         factor(x2, levels = cats, labels = c("Pass", "Fail")))
        b <- mctable[1, 2]
        HTML(paste0(withMathJax("$P_{1}F_{2}=$"), b))
      }
    })
    
    output$npUI3_data <- renderUI({
      np_tests <- input$np_tests_data
      data_type <- input$data_type_np
      data <- filtered_data()
      np_data_selected_columns <- input$np_data_selected_columns
      
      req(np_tests, data)
      
      if (np_tests == 1) {
        # One-sample sign test - show sample size
        req(np_data_selected_columns)
        HTML(paste(withMathJax("$n =$"), length(data[, as.numeric(np_data_selected_columns[1])])))
      } else if (np_tests == 2) {
        # One-sample Wilcoxon signed ranks test - show sample size
        req(np_data_selected_columns)
        HTML(paste(withMathJax("$n =$"), length(data[, as.numeric(np_data_selected_columns[1])])))
      } else if (np_tests == 3 || np_tests == 4) {
        # Two-sample tests - show group 1 sample size
        if (data_type == 1) {
          req(np_data_selected_columns)
          x1 <- data[, as.numeric(np_data_selected_columns[1])]
        } else if (data_type == 2) {
          ref_col <- as.numeric(input$data_choice_ref_np)
          data_col <- as.numeric(input$data_choice_data_np)
          req(ref_col, data_col)
          x1 <- data[[data_col]][which(data[[ref_col]] == input$data_choice_g1_np)]
        }
        HTML(paste(withMathJax("$n_1 =$"), length(na.omit(x1))))
      } else if (np_tests == 5) {
        # Two-sample dependent sign test
        req(np_data_selected_columns, length(np_data_selected_columns) >= 2)
        x1 <- data[, as.numeric(np_data_selected_columns[1])]
        x2 <- data[, as.numeric(np_data_selected_columns[2])]
        temp <- median.test.twosample.dependent.signtest(g1 = x1, g2 = x2)
        n_incl <- temp$estimate[2]
        sample_s <- length(data[, as.numeric(np_data_selected_columns[2])])
        HTML(paste(withMathJax("$n^{=} =$"), sample_s - n_incl))
      } else if (np_tests == 6) {
        # Two-sample dependent Wilcoxon signed ranks test
        req(np_data_selected_columns, length(np_data_selected_columns) >= 2)
        x1 <- data[, as.numeric(np_data_selected_columns[1])]
        x2 <- data[, as.numeric(np_data_selected_columns[2])]
        len <- length(x2)
        n <- seq(1:len)
        n_minus <- 0
        for (val in n) {
          if (x1[val] < x2[val]) {
            n_minus <- n_minus + 1
          }
        }
        HTML(paste(withMathJax("$n^{-} =$"), n_minus))
      } else if (np_tests == 7) {
        # McNemar's test
        pass <- input$np_mc_pass
        req(np_data_selected_columns, data, pass, length(np_data_selected_columns) >= 2)
        x1 <- data[, as.numeric(np_data_selected_columns[1])]
        x2 <- data[, as.numeric(np_data_selected_columns[2])]
        cats <- unique(c(na.omit(unique(data[[as.numeric(np_data_selected_columns[1])]])), 
                         na.omit(unique(data[[as.numeric(np_data_selected_columns[2])]]))))
        cats <- c(cats[cats == pass], cats[cats != pass])
        mctable <- table(factor(x1, levels = cats, labels = c("Pass", "Fail")),
                         factor(x2, levels = cats, labels = c("Pass", "Fail")))
        c <- mctable[2, 1]
        HTML(paste0(withMathJax("$F_{1}P_{2}=$"), c))
      }
    })
    
    output$npUI4_data <- renderUI({
      np_tests <- input$np_tests_data
      data_type <- input$data_type_np
      data <- filtered_data()
      np_data_selected_columns <- input$np_data_selected_columns
      
      req(np_tests, data)
      
      if (np_tests == 3 || np_tests == 4) {
        # Two-sample tests - show group 2 sample size
        if (data_type == 1) {
          req(np_data_selected_columns, length(np_data_selected_columns) >= 2)
          x2 <- data[, as.numeric(np_data_selected_columns[2])]
        } else if (data_type == 2) {
          ref_col <- as.numeric(input$data_choice_ref_np)
          data_col <- as.numeric(input$data_choice_data_np)
          req(ref_col, data_col)
          x2 <- data[[data_col]][which(data[[ref_col]] == input$data_choice_g2_np)]
        }
        HTML(paste(withMathJax("$n_2 =$"), length(na.omit(x2))))
      } else if (np_tests == 5 || np_tests == 6) {
        NULL
      } else if (np_tests == 7) {
        # McNemar's test
        pass <- input$np_mc_pass
        req(np_data_selected_columns, data, pass, length(np_data_selected_columns) >= 2)
        x1 <- data[, as.numeric(np_data_selected_columns[1])]
        x2 <- data[, as.numeric(np_data_selected_columns[2])]
        cats <- unique(c(na.omit(unique(data[[as.numeric(np_data_selected_columns[1])]])), 
                         na.omit(unique(data[[as.numeric(np_data_selected_columns[2])]]))))
        cats <- c(cats[cats == pass], cats[cats != pass])
        mctable <- table(factor(x1, levels = cats, labels = c("Pass", "Fail")),
                         factor(x2, levels = cats, labels = c("Pass", "Fail")))
        d <- mctable[2, 2]
        HTML(paste0(withMathJax("$F_{1}F_{2}=$"), d))
      }
    })
    
    output$npUI5_data <- renderUI({
      # Currently not used for one-sample tests, but placeholder for consistency
      NULL
    })
    
    output$npUI6_data <- renderUI({
      # Currently not used for one-sample tests, but placeholder for consistency
      NULL
    })
    
    # =========================================================================
    # USE DATA MODE - RESULTS RENDERING (with special handling for Mann-Whitney U)
    # =========================================================================
    
    # Reactive for Use Data results - button click handling is in server module
    np_data_results_reactive <- reactive({
      np_tests_data <- input$np_tests_data
      data_type <- input$data_type_np
      data <- filtered_data()
      conf <- input$conf_np_data
      alt <- input$choice_np_alt_stat_data
      R <- input$decimal_np_data
      
      req(np_tests_data, data_type, data, conf, alt, R)
      
      # Get results from server (button click check is handled in server module)
      results <- nonparametric_result$np_data_out()
      req(results)
      
      # Round at rendering stage
      ro(results, R)
    })
    
    output$pretty_nonparametric_data <- renderUI({
      resultsR <- np_data_results_reactive()
      
      # Handle NULL or character results (following pattern from other modules)
      if (is.null(resultsR)) {
        return(HTML("<p>No results available. Please check your inputs.</p>"))
      }
      
      if (is.character(resultsR)) {
        return(HTML(paste("<p>", resultsR, "</p>")))
      }
      
      conf <- input$conf_np_data
      alt <- input$choice_np_alt_stat_data
      np_tests <- input$np_tests_data
      data_type <- input$data_type_np
      data <- filtered_data()
      np_data_selected_columns <- input$np_data_selected_columns
      
      req(conf, alt, np_tests, data_type, data)
      
      # For Reference Column mode, we don't need np_data_selected_columns
      # For Data in Columns mode, we do need it (but only for certain tests)

      if (alt == "two.sided") {
        alt_num <- 1
      } else if (alt == "less") {
        alt_num <- 2
      } else if (alt == "greater") {
        alt_num <- 3
      }
      
      # Test 1: One-sample sign test
      if (np_tests == 1) {
        req(np_data_selected_columns)
        len <- length(data[[as.numeric(np_data_selected_columns[1])]])
        HTML(c(
          paste("<b>Sign Test for Location</b><br>", "<b>", "Method: ", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{above} = $"), resultsR$estimate[3]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$p_{above} = $"), resultsR$statistic), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{equal} = $"), len - resultsR$estimate[2]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{below} = $"), resultsR$estimate[2] - resultsR$estimate[3]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{observed} = $"), len), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{included} = $"), resultsR$estimate[2]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for proportion above = ", resultsR$parameter), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", withMathJax("$\\pi_{above} :$")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_np_alt_text[3 * (as.numeric(np_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste(withMathJax("$p_{above} = $"), resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table></br>",
          "<table><tr><td>", beta_statement, resultsR$estimate[4], "</td></tr>",
          "</table>"
        ))
      }
      # Test 3: Two-sample Mann-Whitney U test
      else if (np_tests == 3) {
        # Validate results structure
        req(resultsR$method, resultsR$estimate, resultsR$statistic, resultsR$p.value)
        req(length(resultsR$estimate) >= 4)
        
        # Determine group names based on data mode
        if (data_type == 1) {
          # Data in Columns mode
          group1_name <- if (!is.null(np_data_selected_columns) && length(np_data_selected_columns) >= 1) {
            names(data)[as.numeric(np_data_selected_columns[1])]
          } else {
            "Group 1"
          }
          group2_name <- if (!is.null(np_data_selected_columns) && length(np_data_selected_columns) >= 2) {
            names(data)[as.numeric(np_data_selected_columns[2])]
          } else {
            "Group 2"
          }
        } else {
          # Reference Column mode
          ref_col <- as.numeric(input$data_choice_ref_np)
          g1_num <- input$data_choice_g1_np
          g2_num <- input$data_choice_g2_np
          
          # Ensure inputs are available
          req(ref_col, g1_num, g2_num, !is.null(ref_col), !is.null(g1_num), !is.null(g2_num))
          req(ref_col > 0, ref_col <= ncol(data))
          
          group1_name <- paste0(names(data)[ref_col], " = ", g1_num)
          group2_name <- paste0(names(data)[ref_col], " = ", g2_num)
        }
        
        HTML(c(
          paste("<b>", "Method: ", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr><td style='border-bottom:1px solid #000'>", group1_name, "</td><td style='border-bottom:1px solid #000'></td>",
          "<td style='border-bottom:1px solid #000'>", group2_name, "</td></tr>",
          "<tr>",
          "<td>", paste(withMathJax("$S_{1} = $"), resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$S_{2} = $"), resultsR$estimate[2]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1} = $"), resultsR$estimate[3]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{2} = $"), resultsR$estimate[4]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence"), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_np_alt_text[3 * (as.numeric(np_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("U = ", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Test 4: Two-sample median test (Mood's)
      else if (np_tests == 4) {
        # Validate results structure (median test doesn't have statistic field)
        req(resultsR$method, resultsR$estimate, resultsR$p.value, resultsR$conf.int)
        req(length(resultsR$estimate) >= 12)
        
        # Determine group names and calculate lengths based on data mode
        if (data_type == 1) {
          # Data in Columns mode
          req(np_data_selected_columns, length(np_data_selected_columns) >= 2)
          selected_cols <- as.numeric(np_data_selected_columns)
          len1 <- length(na.omit(data[, selected_cols[1]]))
          len2 <- length(na.omit(data[, selected_cols[2]]))
          group1_name <- names(data)[selected_cols[1]]
          group2_name <- names(data)[selected_cols[2]]
        } else {
          # Reference Column mode
          ref_col <- as.numeric(input$data_choice_ref_np)
          data_col <- as.numeric(input$data_choice_data_np)
          g1_num <- input$data_choice_g1_np
          g2_num <- input$data_choice_g2_np
          
          req(ref_col, data_col, g1_num, g2_num)
          req(!is.null(ref_col), !is.null(data_col), !is.null(g1_num), !is.null(g2_num))
          req(ref_col > 0, ref_col <= ncol(data), data_col > 0, data_col <= ncol(data))
          
          len1 <- length(na.omit(data[[data_col]][which(data[[ref_col]] == g1_num)]))
          len2 <- length(na.omit(data[[data_col]][which(data[[ref_col]] == g2_num)]))
          group1_name <- paste0(names(data)[ref_col], " = ", g1_num)
          group2_name <- paste0(names(data)[ref_col], " = ", g2_num)
        }
        
        html_result <- HTML(c(
          paste("<b>", "Two-Sample Median Test<br>Method: ", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr><td style='border-bottom:1px solid #000'>", group1_name, "</td><td style='border-bottom:1px solid #000'></td>",
          "<td style='border-bottom:1px solid #000'>", group2_name, "</td></tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1\\; above} = $"), resultsR$estimate[3]), "</td><td></td>",
          "<td>", paste(withMathJax("$p_{1\\; above} = $"), resultsR$estimate[1]), "</td>",
          "<td>", paste(withMathJax("$n_{2\\; above} = $"), resultsR$estimate[9]), "</td><td></td>",
          "<td>", paste(withMathJax("$p_{2\\; above} = $"), resultsR$estimate[7]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1\\; equal} = $"), len1 - resultsR$estimate[2]), "</td><td></td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{2\\; equal} = $"), len2 - resultsR$estimate[8]), "</td><td></td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1\\; below} = $"), resultsR$estimate[4]), "</td><td></td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{2\\; below} = $"), resultsR$estimate[10]), "</td><td></td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{1\\; total\\; inc.} = $"), resultsR$estimate[2]), "</td><td></td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{2\\; total\\; inc.} = $"), resultsR$estimate[8]), "</td><td></td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", withMathJax("$\\pi_{1 \\;above} :$")), "</td>",
          "<td>", resultsR$estimate[5], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[6], "</td>",
          "</tr>",
          "<tr>",
          "<td align='right'>", paste(withMathJax("$\\pi_{2 \\;above} :$")), "</td>",
          "<td>", resultsR$estimate[11], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[12], "</td>",
          "</tr>",
          "<tr>",
          "<td align='right'>", "Odds Ratio:", "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_np_alt_text[3 * (as.numeric(np_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
        
        html_result
      }
      # Test 7: McNemar's test of change
      else if (np_tests == 7) {
        # Validate results structure
        req(resultsR$method, resultsR$estimate, resultsR$p.value)
        req(length(resultsR$estimate) >= 4)
        
        # Get column names and pass value for display
        if (data_type == 1) {
          # Data in Columns mode
          req(np_data_selected_columns, length(np_data_selected_columns) >= 2)
          test1_name <- names(data)[as.numeric(np_data_selected_columns[1])]
          test2_name <- names(data)[as.numeric(np_data_selected_columns[2])]
        } else {
          # Reference Column mode - McNemar's test typically uses two columns
          # This case may not be applicable, but set defaults
          test1_name <- "Test 1"
          test2_name <- "Test 2"
        }
        pass_value <- input$np_mc_pass
        
        HTML(c(
          paste("<b>", "Method: ", resultsR$method, "</b>"),
          "<br>Test 1 is column ", test1_name, "<br>",
          "Test 2 is column ", test2_name, "<br>",
          "Pass is indicated by ", pass_value, "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$P_{1}F_{2} = $"), resultsR$estimate[1]), "</td><td></td>",
          "<td>", paste(withMathJax("$p(b) = $"), resultsR$estimate[2]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$F_{1}P_{2} = $"), resultsR$estimate[3]), "</td><td></td>",
          "<td>", paste(withMathJax("$p(c) = $"), resultsR$estimate[4]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence"), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_np_alt_text[3 * (as.numeric(np_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
      # Additional test renderings would follow similar pattern
      else {
        HTML(paste("<p>Results rendering for test", np_tests, "to be completed</p>"))
      }
    })
    
  })
}

# =============================================================================
# ARCHITECTURAL PATTERNS USED
# =============================================================================
# 1. Coordinator-Worker Separation: This coordinator manages UI and data flow
# 2. Explicit Data Flow: Data passed as parameters to worker modules
# 3. Namespace Management: All UI rendering happens in coordinator
# 4. Global System Integration: Uses global data invalidation and config
# 5. Template Compliance: Follows established patterns from working modules
# 6. No Over-Engineering: Simple reactive functions, no complex observers
