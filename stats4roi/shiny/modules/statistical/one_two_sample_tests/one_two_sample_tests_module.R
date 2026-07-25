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

# HTML table row for matched-pairs variance test (when var.test.* present in estimate)
md_matched_pairs_var_test_row <- function(estimate, conf, p_for_sig = NULL) {
  if (is.null(estimate) || !all(c("var.test.t", "var.test.df", "var.test.p") %in% names(estimate))) {
    return(character(0))
  }
  if (is.null(p_for_sig)) {
    p_for_sig <- estimate[["var.test.p"]]
  }
  star <- if (length(p_for_sig) == 1L && !is.na(p_for_sig) && p_for_sig < 1 - conf) "*" else ""
  c(
    "<tr>",
    "<td>", "Matched Pairs t-test for ", "$\\sigma^2_1 = \\sigma^2_2$", ": ", "</td>",
    "<td>", paste("t = "), estimate[["var.test.t"]], "</td>",
    "<td>", "df =", estimate[["var.test.df"]], "</td>",
    "<td>", paste("p = ", estimate[["var.test.p"]], star), "</td>",
    "</tr>"
  )
}

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
  "Pass\U2081 Fail\U2082 is not equal to Fail\U2081 Pass\U2082", "Pass\U2081 Fail\U2082 is less than Fail\U2081 Pass\U2082", "Pass\U2081 Fail\U2082 is greater than Fail\U2081 Pass\U2082",
  "Sequence is not random", "Too few runs (clustering)", "Too many runs (oscillation)"
)

source("modules/statistical/one_two_sample_tests/ots_group_utils.R")
source("modules/statistical/one_two_sample_tests/utils/runs_test.R")

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
      ui_reset = function() {
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
          md_data_col_g1 = input$md_data_col_g1,
          md_data_col_g2 = input$md_data_col_g2,
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
      sigma_known_data <- input$sigma_known_data
      dep_or_indep_data <- input$dep_or_indep_data
      one_samp_var_data <- input$one_samp_var_data
      
      if (type == 1) {
        two_col <- ots_column_mode_ready(
          col_g1 = input$md_data_col_g1,
          col_g2 = input$md_data_col_g2,
          two_sample = TRUE
        )
        one_col <- ots_column_mode_ready(col_g1 = input$md_data_col_g1, two_sample = FALSE)
        if (!one_col) {
          return(NULL)
        }
        is_one_sample <- !two_col
      } else {
        if (!ots_reference_mode_ready(
          input$data_choice_ref,
          input$data_choice_data,
          input$data_choice_g1,
          input$data_choice_g2,
          two_sample = TRUE
        )) {
          return(NULL)
        }
        is_one_sample <- FALSE
      }
      
      if (is.null(sigma_known_data)) return(NULL)
      if (!is_one_sample && is.null(dep_or_indep_data)) return(NULL)
      if (is_one_sample && is.null(one_samp_var_data)) return(NULL)
      
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
    
    md_data_groups <- reactive({
      data <- filtered_data()
      req(data, input$data_type_md)
      test_num <- data_md_test_num()
      two_sample <- !is.null(test_num) && test_num %in% c(4L, 5L, 6L, 7L)
      ots_groups_from_inputs(
        data,
        mode = input$data_type_md,
        col_g1 = input$md_data_col_g1,
        col_g2 = input$md_data_col_g2,
        ref_col = input$data_choice_ref,
        data_col = input$data_choice_data,
        level_g1 = input$data_choice_g1,
        level_g2 = input$data_choice_g2,
        two_sample = two_sample
      )
    })
    
    # =========================================================================
    # RENDER WORKER OUTPUTS (coordinator handles all rendering)
    # =========================================================================
    # Note: Due to the complexity of Means and Dispersion, most UI rendering
    # is handled directly in the coordinator. The worker only provides calculation results.
    
    # Render Enter Statistics results
    output$pretty_md <- renderUI({
      md_test_num()
      input$md_alt
      input$conf
      input$md_UI1
      input$md_UI2
      input$md_UI3
      input$md_UI4
      input$md_UI5
      input$md_UI6
      input$t_type
      input$md_t_dep_type_stat
      
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
            ots_dbar_pairing_note_html(),
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
            "<td>", "$\\Delta :$", "</td>",
            "<td>", results[["conf.int"]][1], "</td>",
            "<td>", " to ", "</td>",
            "<td>", results[["conf.int"]][2], "</td>",
            "</tr>",
            "<tr>",
            "<td>", "</td>",
            "<td>", "$\\sigma_{D} :$", "</td>",
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
          # Mean difference type (match monolithic: block stale n2 in md_UI6 until r in [-1, 1])
          req(UI4, UI5, UI6 <= 1, UI6 >= -1)
          
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
            md_matched_pairs_var_test_row(results$estimate, conf),
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
      # Read dynamic UI inputs to establish dependencies (may be NULL until rendered)
      test_num <- data_md_test_num()
      type <- input$data_type_md
      input$sigma_known_data
      input$dep_or_indep_data
      input$one_samp_var_data
      input$one_or_two_md_data
      input$t_type_dat
      input$md_t_dep_type
      input$md_data_col_g1
      input$md_data_col_g2
      input$data_choice_ref
      input$data_choice_data
      input$data_choice_g1
      input$data_choice_g2
      input$conf_m_d_data
      data <- filtered_data()
      
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
      groups <- md_data_groups()
      
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
      main_results <- results
      levene <- NULL
      adm_n1 <- NULL
      if (is.list(results) && "main" %in% names(results)) {
        main_results <- results$main
        levene <- results$levene
        adm_n1 <- results$adm_n1
        resultsR <- ro(main_results, R)
      } else {
        resultsR <- ro(results, R)
      }
      method_label <- main_results$method
      
      output_html <- NULL
      if (is.null(groups) || is.null(groups$g1)) {
        return(HTML("<p>Group assignment is not ready. Check factor, response, and level selections.</p>"))
      }
      
      # Test 1: One-sample z-test
      if (test_num == 1) {
        output_html <- ots_html_flatten(c(
          paste("<b>", method_label, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", ots_mj_paste_stat("\\bar{X}", groups$g1, resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste0("$\\mu_{0} = $", resultsR$parameter), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("\\sigma", groups$g1, ro(UI3, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("n", groups$g1, resultsR$estimate[2]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", ots_mj_paste_stat("\\mu", groups$g1), "</td>",
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
        output_html <- ots_html_flatten(c(
          paste("<b>", method_label, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", ots_mj_paste_stat("\\bar{X}", groups$g1, resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste0("$\\mu_{0} = $", resultsR$parameter), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("s", groups$g1, resultsR$estimate[8]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("n", groups$g1, resultsR$estimate[3] + 1), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", ots_mj_paste_stat("\\mu", groups$g1), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", ots_mj_paste_stat("\\sigma", groups$g1), "</td>",
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
        output_html <- ots_html_flatten(c(
          paste("<b>", method_label, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", ots_mj_paste_stat("s", groups$g1, ro(results$estimate[1]^0.5, R)), "</td>",
          "<td>", "</td>",
          "<td>", paste0("$\\sigma_{0} = $", ro(UI2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("n", groups$g1, ro(results$estimate[3], R)), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", ots_mj_paste_stat("\\sigma", groups$g1), "</td>",
          "<td>", ro(results$conf.int[1]^0.5, R), "</td>",
          "<td>", " to ", "</td>",
          "<td>", ro(results$conf.int[2]^0.5, R), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", names(choice_sd_alt_1[alt_num]), ": "), "</td>",
          "<td>", paste0("$\\chi^2 = $", ro(results$statistic, R)), "</td>",
          "<td>", paste("df = ", ro(results$estimate[2], R)), "</td>",
          "<td>", paste("p = ", ro(results$p.value, R), if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          paste(beta_statement, 100 * ro(results$estimate[4], R), "%")
        ))
      }
      # Test 4: Two-sample z-test independent
      else if (test_num == 4) {
        if (is.null(groups$g2)) {
          return(HTML("<p>Select Group 2 level for this two-sample test.</p>"))
        }
        output_html <- ots_html_flatten(c(
          paste("<b>", method_label, "</b>"),
          "<br><br>",
          "<table>",
          "<tr><td style='border-bottom:1px solid #000'>", groups$g1$name, "</td><td style='border-bottom:1px solid #000'></td>",
          "<td style='border-bottom:1px solid #000'>", groups$g2$name, "</td></tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("\\bar{X}", groups$g1, resultsR$estimate[3]), "</td>",
          "<td>", "</td>",
          "<td>", ots_mj_paste_stat("\\bar{X}", groups$g2, resultsR$estimate[7]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("\\sigma", groups$g1, ro(UI3, R)), "</td>",
          "<td>", "</td>",
          "<td>", ots_mj_paste_stat("\\sigma", groups$g2, ro(UI4, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("n", groups$g1, resultsR$estimate[6]), "</td>",
          "<td>", "</td>",
          "<td>", ots_mj_paste_stat("n", groups$g2, resultsR$estimate[10]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", ots_mj_paste_stat("\\mu", groups$g1), "</td>",
          "<td>", resultsR$estimate[4], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[5], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", ots_mj_paste_stat("\\mu", groups$g2), "</td>",
          "<td>", resultsR$estimate[8], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[9], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(ots_mj_diff_label("\\mu", groups$g1, groups$g2)), "</td>",
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
        if (is.null(groups$g2)) {
          return(HTML("<p>Select Group 2 level for this two-sample test.</p>"))
        }
        output_html <- ots_html_flatten(c(
          paste("<b>", method_label, "</b>"),
          "<br><br>",
          "<table>",
          "<tr><td style='border-bottom:1px solid #000'>", groups$g1$name, "</td><td style='border-bottom:1px solid #000'></td>",
          "<td style='border-bottom:1px solid #000'>", groups$g2$name, "</td></tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("\\bar{X}", groups$g1, resultsR$estimate[4]), "</td>",
          "<td>", "</td>",
          "<td>", ots_mj_paste_stat("\\bar{X}", groups$g2, resultsR$estimate[14]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("s", groups$g1, resultsR$estimate[11]), "</td>",
          "<td>", "</td>",
          "<td>", ots_mj_paste_stat("s", groups$g2, resultsR$estimate[21]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("n", groups$g1, resultsR$estimate[7]), "</td>",
          "<td>", "</td>",
          "<td>", ots_mj_paste_stat("n", groups$g2, resultsR$estimate[17]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", ots_mj_paste_stat("\\mu", groups$g1), "</td>",
          "<td>", resultsR$estimate[5], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[6], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", ots_mj_paste_stat("\\mu", groups$g2), "</td>",
          "<td>", resultsR$estimate[15], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[16], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", ots_mj_paste_stat("\\sigma", groups$g1), "</td>",
          "<td>", resultsR$estimate[12], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[13], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", ots_mj_paste_stat("\\sigma", groups$g2), "</td>",
          "<td>", resultsR$estimate[22], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[23], "</td>",
          "</tr>",
          "<tr>",
          "<td>", "</td>",
          "<td>", paste(ots_mj_diff_label("\\mu", groups$g1, groups$g2)), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Tests for ", ots_mj_var_ratio_label(groups$g1, groups$g2), ": "), "</td>",
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
            "<td align='right'>", "ADM", "$_{n-1}$", "</td>",
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
        if (is.null(dep_t_type)) {
          return(HTML("<p>Select dependent t-test type.</p>"))
        }
        
        if (dep_t_type == 1) {
          if (is.null(groups$g2)) {
            return(HTML("<p>Select Group 2 level for this dependent test.</p>"))
          }
          # d-bar type
          output_html <- ots_html_flatten(c(
            paste("<b>", method_label, "</b>"),
            "<br><br>",
            ots_dbar_pairing_note_html(groups$g1, groups$g2),
            "<table>",
            "<tr>",
            "<td>", paste0("$\\bar{D} = $", resultsR[["estimate"]][["sample.mean"]]), "</td>",
            "<td>", "</td>",
            "<td>", paste0("$\\Delta = $", resultsR[["parameter"]][["null hypothesis mean"]]), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste0("$s_{D} = $", resultsR[["estimate"]][["sd"]]), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste0("$n = $", resultsR[["estimate"]][["df"]] + 1), "</td>",
            "</tr>",
            "</table>",
            "<table>",
            "<tr>",
            "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
            "<td>", "$\\Delta :$", "</td>",
            "<td>", resultsR[["conf.int"]][1], "</td>",
            "<td>", " to ", "</td>",
            "<td>", resultsR[["conf.int"]][2], "</td>",
            "</tr>",
            "<tr>",
            "<td>", "</td>",
            "<td>", "$\\sigma_{D} :$", "</td>",
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
          if (is.null(groups$g2)) {
            return(HTML("<p>Select Group 2 level for this two-sample test.</p>"))
          }
          paired <- ots_paired_frame(groups$g1$x, groups$g2$x)
          if (is.null(paired)) {
            return(HTML("<p>Need equal-length groups for dependent test.</p>"))
          }
          rho_val <- cor(paired$g1, paired$g2)
          n_val <- nrow(paired)
          
          corr_test <- cor.pearson.r.onesample.simple(
            sample.r = rho_val,
            sample.size = n_val,
            null.hypothesis.rho = 0,
            conf.level = conf
          )
          corr_test <- ro(corr_test, R)
          
          output_html <- ots_html_flatten(c(
            paste("<b>", method_label, "</b>"),
            "<br><br>",
            "<table>",
            "<tr><td style='border-bottom:1px solid #000'>", groups$g1$name, "</td><td style='border-bottom:1px solid #000'></td>",
            "<td style='border-bottom:1px solid #000'>", groups$g2$name, "</td></tr>",
            "<tr>",
            "<td>", ots_mj_paste_stat("\\bar{X}", groups$g1, resultsR$estimate[5]), "</td>",
            "<td>", "</td>",
            "<td>", ots_mj_paste_stat("\\bar{X}", groups$g2, resultsR$estimate[14]), "</td>",
            "</tr>",
            "<tr>",
            "<td>", ots_mj_paste_stat("s", groups$g1, resultsR$estimate[11]), "</td>",
            "<td>", "</td>",
            "<td>", ots_mj_paste_stat("s", groups$g2, resultsR$estimate[21]), "</td>",
            "</tr>",
            "<tr>",
            "<td>", ots_mj_paste_stat("n", groups$g1, n_val), "</td>",
            "<td>", "</td>",
            "<td>", paste0("$r_{xy} = $", ro(rho_val, R)), "</td>",
            "</tr>",
            "</table>",
            "<table>",
            "<tr>",
            "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
            "<td>", ots_mj_paste_stat("\\mu", groups$g1), "</td>",
            "<td>", resultsR$estimate[6], "</td>",
            "<td>", " to ", "</td>",
            "<td>", resultsR$estimate[7], "</td>",
            "</tr>",
            "<tr>",
            "<td>", "</td>",
            "<td>", ots_mj_paste_stat("\\mu", groups$g2), "</td>",
            "<td>", resultsR$estimate[15], "</td>",
            "<td>", " to ", "</td>",
            "<td>", resultsR$estimate[16], "</td>",
            "</tr>",
            "<tr>",
            "<td>", "</td>",
            "<td>", paste(ots_mj_diff_label("\\mu", groups$g1, groups$g2)), "</td>",
            "<td>", resultsR$conf.int[1], "</td>",
            "<td>", " to ", "</td>",
            "<td>", resultsR$conf.int[2], "</td>",
            "</tr>",
            "</table>",
            "<table>",
            "<tr>",
            "<td>", "Test for ", "$\\rho=0$", ": ", "</td>",
            "<td>", paste("t = "), corr_test$statistic, "</td>",
            "<td>", "df =", corr_test$estimate[2], "</td>",
            "<td>", paste("p = ", corr_test$p.value, if (corr_test$p.value < 1 - conf) {"*"}), "</td>",
            "</tr>",
            md_matched_pairs_var_test_row(
              resultsR$estimate, conf,
              p_for_sig = results$estimate[["var.test.p"]]
            ),
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
      
      if (is.null(output_html)) {
        if (is.null(test_num)) {
          return(HTML("<p>Please complete test options above.</p>"))
        }
        return(HTML("<p>Results display is not available for this test configuration.</p>"))
      }
      ots_results_mathjax_wrap(output_html, ns("md_data_mj"))
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
          min = -1,
          max = 1,
          step = 0.01,
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
            min = -1,
            max = 1,
            step = 0.01,
            width = "150px"
          )
        }
      }
    })
    
    # =========================================================================
    # MEANS AND DISPERSION "USE DATA" TAB UI RENDERING FUNCTIONS
    # =========================================================================
    
    # Data selection - columns mode (explicit Group 1 / Group 2 assignment)
    output$data_choice_column <- renderUI({
      data <- filtered_data()
      req(data, ncol(data) > 0)
      
      choices <- ots_column_choices(data)
      
      tagList(
        selectInput(
          inputId = ns("md_data_col_g1"),
          label = "Group 1 column",
          choices = c("Select column..." = "", choices),
          selected = ""
        ),
        selectInput(
          inputId = ns("md_data_col_g2"),
          label = "Group 2 column (leave blank for one-sample)",
          choices = c("—" = "", choices),
          selected = ""
        ),
        tags$p(
          class = "help-block text-muted",
          style = "font-size: 0.85em; margin-top: 0;",
          "For dependent tests, row i in Group 1 is paired with row i in Group 2."
        )
      )
    })
    
    output$ots_md_group_assignment <- renderUI({
      data <- filtered_data()
      req(data, input$data_type_md)
      test_num <- data_md_test_num()
      two_sample <- !is.null(test_num) && test_num %in% c(4L, 5L, 6L, 7L)
      groups <- ots_groups_from_inputs(
        data,
        mode = input$data_type_md,
        col_g1 = input$md_data_col_g1,
        col_g2 = input$md_data_col_g2,
        ref_col = input$data_choice_ref,
        data_col = input$data_choice_data,
        level_g1 = input$data_choice_g1,
        level_g2 = input$data_choice_g2,
        two_sample = two_sample
      )
      ots_group_assignment_html(groups)
    })
    
    # Data selection - reference column mode
    output$data_choice_ref <- renderUI({
      data <- filtered_data()
      req(data, ncol(data) > 0)
      
      choices <- seq_len(ncol(data))
      names(choices) <- names(data)
      
      selectInput(
        inputId = ns("data_choice_ref"),
        label = "Factor column",
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
        label = "Response column",
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
      ref_name <- names(data)[factor_col]
      
      selectInput(
        inputId = ns("data_choice_g1"),
        label = paste0("Group 1 level (", ref_name, ")"),
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
      ref_name <- names(data)[factor_col]
      
      # Remove group 1 from choices
      factor_values <- factor_values[factor_values != factor_g1]
      
      selectInput(
        inputId = ns("data_choice_g2"),
        label = paste0("Group 2 level (", ref_name, ")"),
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
        two_col <- ots_column_mode_ready(
          col_g1 = input$md_data_col_g1,
          col_g2 = input$md_data_col_g2,
          two_sample = TRUE
        )
        dep_or_indep <- input$dep_or_indep_data
        
        if (!two_col) {
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
        two_col <- ots_column_mode_ready(
          col_g1 = input$md_data_col_g1,
          col_g2 = input$md_data_col_g2,
          two_sample = TRUE
        )
        if (two_col) {
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
      sigma_known_data <- input$sigma_known_data
      
      # Determine if one sample
      if (type == 1) {
        two_col <- ots_column_mode_ready(
          col_g1 = input$md_data_col_g1,
          col_g2 = input$md_data_col_g2,
          two_sample = TRUE
        )
        if (two_col) {
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
      groups <- md_data_groups()
      decimal_m_d_d <- input$decimal_m_d_d
      req(test_num, groups)
      
      if (test_num %in% c(1L, 2L)) {
        average <- ro(mean(na.omit(groups$g1$x)), decimal_m_d_d)
        ots_stat_value_html("\\bar{X}", groups$g1, average)
      } else if (test_num == 3L) {
        std <- ro(sd(na.omit(groups$g1$x)), decimal_m_d_d)
        withMathJax(paste("$s = $", std))
      } else if (test_num %in% c(4L, 5L)) {
        req(groups$g2)
        average <- ro(mean(na.omit(groups$g1$x)), decimal_m_d_d)
        ots_stat_value_html("\\bar{X}", groups$g1, average)
      } else if (test_num == 7L) {
        dep_t_type <- input$md_t_dep_type
        req(dep_t_type, groups$g2)
        paired <- ots_paired_frame(groups$g1$x, groups$g2$x)
        if (is.null(paired)) {
          return("Need equal length groups for dependent test")
        }
        if (dep_t_type == 1L) {
          average <- ro(mean(paired$g1 - paired$g2), decimal_m_d_d)
          withMathJax(paste("$\\bar{D} = $", average))
        } else {
          average <- ro(mean(paired$g1), decimal_m_d_d)
          ots_stat_value_html("\\bar{X}", groups$g1, average)
        }
      }
    })
    
    output$md_data_UI2 <- renderUI({
      test_num <- data_md_test_num()
      groups <- md_data_groups()
      decimal_m_d_d <- input$decimal_m_d_d
      req(test_num)
      
      if (test_num %in% c(1L, 2L)) {
        numericInput(
          inputId = ns("md_data_UI2"),
          label = withMathJax("$\\mu_{0}:{ }$"),
          value = 3,
          width = "150px"
        )
      } else if (test_num == 3L) {
        numericInput(
          inputId = ns("md_data_UI2"),
          label = withMathJax("$\\sigma_{0}:{ }$"),
          value = 3,
          width = "150px"
        )
      } else if (test_num %in% c(4L, 5L)) {
        req(groups, groups$g2)
        average <- ro(mean(na.omit(groups$g2$x)), decimal_m_d_d)
        ots_stat_value_html("\\bar{X}", groups$g2, average)
      } else if (test_num == 7L) {
        dep_t_type <- input$md_t_dep_type
        req(dep_t_type)
        if (dep_t_type == 1L) {
          numericInput(
            inputId = ns("md_data_UI2"),
            label = withMathJax("$\\Delta:{ }$"),
            value = 0,
            width = "150px"
          )
        } else {
          req(groups, groups$g2)
          average <- ro(mean(na.omit(groups$g2$x)), decimal_m_d_d)
          ots_stat_value_html("\\bar{X}", groups$g2, average)
        }
      }
    })
    
    output$md_data_UI3 <- renderUI({
      test_num <- data_md_test_num()
      groups <- md_data_groups()
      decimal_m_d_d <- input$decimal_m_d_d
      req(test_num, groups)
      
      if (test_num == 1L) {
        std <- ro(sd(na.omit(groups$g1$x)), decimal_m_d_d)
        numericInput(
          inputId = ns("md_data_UI3"),
          label = withMathJax("$\\sigma_{0}:{ }$"),
          value = std,
          width = "150px"
        )
      } else if (test_num == 2L) {
        std <- ro(sd(na.omit(groups$g1$x)), decimal_m_d_d)
        withMathJax(paste("$s = $", std))
      } else if (test_num == 3L) {
        n <- length(na.omit(groups$g1$x))
        withMathJax(paste("$n = $", n))
      } else if (test_num == 4L) {
        std <- ro(sd(na.omit(groups$g1$x)), decimal_m_d_d)
        sig_lab <- paste0("$\\sigma_{\\text{", ots_tex_name(groups$g1$label_key), "}}:{ }$")
        numericInput(
          inputId = ns("md_data_UI3"),
          label = withMathJax(sig_lab),
          value = std,
          width = "150px"
        )
      } else if (test_num == 5L) {
        std <- ro(sd(na.omit(groups$g1$x)), decimal_m_d_d)
        ots_s_sub_html(1L, std, group = groups$g1)
      } else if (test_num == 7L) {
        dep_t_type <- input$md_t_dep_type
        req(dep_t_type, groups$g2)
        paired <- ots_paired_frame(groups$g1$x, groups$g2$x)
        if (is.null(paired)) {
          return("Need equal length groups for dependent test")
        }
        if (dep_t_type == 1L) {
          std <- ro(sd(paired$g1 - paired$g2), decimal_m_d_d)
          withMathJax(paste("$s_{D} = $", std))
        } else {
          std <- ro(sd(paired$g1), decimal_m_d_d)
          ots_s_sub_html(1L, std, group = groups$g1)
        }
      }
    })
    
    output$md_data_UI4 <- renderUI({
      test_num <- data_md_test_num()
      groups <- md_data_groups()
      decimal_m_d_d <- input$decimal_m_d_d
      req(test_num)
      
      if (test_num %in% c(1L, 2L, 3L)) {
        NULL
      } else if (test_num == 4L) {
        req(groups, groups$g2)
        std <- ro(sd(na.omit(groups$g2$x)), decimal_m_d_d)
        sig_lab <- paste0("$\\sigma_{\\text{", ots_tex_name(groups$g2$label_key), "}}:{ }$")
        numericInput(
          inputId = ns("md_data_UI4"),
          label = withMathJax(sig_lab),
          value = std,
          width = "150px"
        )
      } else if (test_num == 5L) {
        req(groups, groups$g2)
        std <- ro(sd(na.omit(groups$g2$x)), decimal_m_d_d)
        ots_s_sub_html(2L, std, group = groups$g2)
      } else if (test_num == 7L) {
        dep_t_type <- input$md_t_dep_type
        req(dep_t_type, dep_t_type == 2L, groups, groups$g2)
        std <- ro(sd(na.omit(groups$g2$x)), decimal_m_d_d)
        ots_s_sub_html(2L, std, group = groups$g2)
      }
    })
    
    output$md_data_UI5 <- renderUI({
      test_num <- data_md_test_num()
      groups <- md_data_groups()
      req(test_num, groups)
      
      if (test_num %in% c(1L, 2L)) {
        n <- length(na.omit(groups$g1$x))
        withMathJax(paste("$n = $", n))
      } else if (test_num == 3L) {
        NULL
      } else if (test_num %in% c(4L, 5L)) {
        n <- length(na.omit(groups$g1$x))
        ots_n_sub_html(1L, n, group = groups$g1)
      } else if (test_num == 7L) {
        req(groups$g2)
        paired <- ots_paired_frame(groups$g1$x, groups$g2$x)
        if (is.null(paired)) {
          return("Need equal length groups for dependent test")
        }
        n <- nrow(paired)
        withMathJax(paste("$n = $", n))
      }
    })
    
    output$md_data_UI6 <- renderUI({
      test_num <- data_md_test_num()
      groups <- md_data_groups()
      decimal_m_d_d <- input$decimal_m_d_d
      req(test_num)
      
      if (test_num %in% c(1L, 2L, 3L)) {
        NULL
      } else if (test_num %in% c(4L, 5L)) {
        req(groups, groups$g2)
        n <- length(na.omit(groups$g2$x))
        ots_n_sub_html(2L, n, group = groups$g2)
      } else if (test_num == 7L) {
        dep_t_type <- input$md_t_dep_type
        req(dep_t_type, dep_t_type == 2L, groups, groups$g2)
        paired <- ots_paired_frame(groups$g1$x, groups$g2$x)
        req(paired)
        rho <- ro(cor(paired$g1, paired$g2), decimal_m_d_d)
        withMathJax(paste("$r_{xy} = $", rho))
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
          prop_enter_counts = isTRUE(input$prop_enter_counts),
          # Use Data mode inputs
          decimal_bi_d = input$decimal_bi_d,
          conf_bi_data = input$conf_bi_data,
          data_type_bi = input$data_type_bi,
          bi_data_col_g1 = input$bi_data_col_g1,
          bi_data_col_g2 = input$bi_data_col_g2,
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
    
    # Proportions Enter Statistics: toggle numeric inputs between p and success counts x
    observe({
      use_counts <- isTRUE(input$prop_enter_counts)
      n1 <- input$n_samp_p
      n2 <- input$n_samp_p_2
      one_two <- input$one_or_two_p
      max1 <- if (is.null(n1) || !is.finite(n1) || n1 < 1) 1e6 else n1
      
      if (use_counts) {
        updateNumericInput(session, "p_samp",
                           label = withMathJax("$$np_{1}$$"),
                           min = 0, max = max1, step = 1)
        if (!is.null(one_two) && one_two == 2) {
          max2 <- if (is.null(n2) || !is.finite(n2) || n2 < 1) 1e6 else n2
          updateNumericInput(session, "p2",
                             label = withMathJax("$$np_{2}$$"),
                             min = 0, max = max2, step = 1)
        }
      } else {
        updateNumericInput(session, "p_samp",
                           label = withMathJax("$$p_{1}$$"),
                           min = 0, max = 1, step = 0.01)
        if (!is.null(one_two) && one_two == 2) {
          updateNumericInput(session, "p2",
                             label = withMathJax("$$p_{2}$$"),
                             min = 0, max = 1, step = 0.01)
        }
      }
    })
    
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
    
    bi_data_groups <- reactive({
      data <- filtered_data()
      req(data, input$data_type_bi)
      two_col <- ots_column_mode_ready(
        col_g1 = input$bi_data_col_g1,
        col_g2 = input$bi_data_col_g2,
        two_sample = TRUE
      )
      ots_groups_from_inputs(
        data,
        mode = input$data_type_bi,
        col_g1 = input$bi_data_col_g1,
        col_g2 = input$bi_data_col_g2,
        ref_col = input$data_choice_ref_bi,
        data_col = input$data_choice_data_bi,
        level_g1 = input$data_choice_g1_bi,
        level_g2 = input$data_choice_g2_bi,
        two_sample = if (input$data_type_bi == 1) two_col else TRUE
      )
    })
    
    # Render Use Data results
    output$pretty_prop_stat_data <- renderUI({
      results <- proportions_result$prop_data_out()
      data_type <- input$data_type_bi
      alt <- input$alt_p_bi
      R <- input$decimal_bi_d
      conf <- input$conf_bi_data
      data <- filtered_data()
      groups <- bi_data_groups()
      
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
      
      two_col <- if (data_type == 1) {
        ots_column_mode_ready(
          col_g1 = input$bi_data_col_g1,
          col_g2 = input$bi_data_col_g2,
          two_sample = TRUE
        )
      } else {
        TRUE
      }
      samples <- if (two_col) 2L else 1L
      req(groups)
      
      if (samples == 1) {
        req(groups$g1)
        # One-sample proportion test
        return(ots_results_mathjax_wrap(ots_html_flatten(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", ots_mj_paste_stat("p", groups$g1, resultsR[["statistic"]][["p"]]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\pi_{0} =$"), resultsR[["parameter"]][["null hypothesis proportion"]]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("n", groups$g1, resultsR[["estimate"]][["sample.size"]]), "</td>",
          "<td>", "</td>",
          "<td>", "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", ots_mj_paste_stat("\\pi", groups$g1)), "</td>",
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
        )), ns("prop_data_mj")))
      } else if (samples == 2) {
        req(groups$g2)
        return(ots_results_mathjax_wrap(ots_html_flatten(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr><td style='border-bottom:1px solid #000'>", groups$g1$name, "</td><td style='border-bottom:1px solid #000'></td>",
          "<td style='border-bottom:1px solid #000'>", groups$g2$name, "</td></tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("p", groups$g1, resultsR[["estimate"]][["sample.prop.g1"]]), "</td>",
          "<td>", "</td>",
          "<td>", ots_mj_paste_stat("p", groups$g2, resultsR[["estimate"]][["sample.prop.g2"]]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("n", groups$g1, resultsR[["estimate"]][["sample.size.g1"]]), "</td>",
          "<td>", "</td>",
          "<td>", ots_mj_paste_stat("n", groups$g2, resultsR[["estimate"]][["sample.size.g2"]]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", ots_mj_paste_stat("\\pi", groups$g1)), "</td>",
          "<td>", resultsR[["estimate"]][["p.g1.lowerci"]], "</td>",
          "<td>", "to", "</td>",
          "<td>", resultsR[["estimate"]][["p.g1.upperci"]], "</td>",
          "</tr>",
          "<tr>",
          "<td align='right'>", paste(ots_mj_paste_stat("\\pi", groups$g2)), "</td>",
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
        )), ns("prop_data_mj")))
      }
    })
    
    # =========================================================================
    # PROPORTIONS UI RENDERING FUNCTIONS
    # =========================================================================
    
    output$data_choice_column_bi <- renderUI({
      data <- filtered_data()
      req(data, ncol(data) > 0)
      choices <- ots_column_choices(data)
      tagList(
        selectInput(
          inputId = ns("bi_data_col_g1"),
          label = "Group 1 column",
          choices = c("Select column..." = "", choices),
          selected = ""
        ),
        selectInput(
          inputId = ns("bi_data_col_g2"),
          label = "Group 2 column (leave blank for one-sample)",
          choices = c("—" = "", choices),
          selected = ""
        )
      )
    })
    
    output$ots_bi_group_assignment <- renderUI({
      data <- filtered_data()
      req(data, input$data_type_bi)
      two_col <- if (input$data_type_bi == 1) {
        ots_column_mode_ready(
          col_g1 = input$bi_data_col_g1,
          col_g2 = input$bi_data_col_g2,
          two_sample = TRUE
        )
      } else {
        ots_reference_mode_ready(
          input$data_choice_ref_bi,
          input$data_choice_data_bi,
          input$data_choice_g1_bi,
          input$data_choice_g2_bi,
          two_sample = TRUE
        )
      }
      groups <- ots_groups_from_inputs(
        data,
        mode = input$data_type_bi,
        col_g1 = input$bi_data_col_g1,
        col_g2 = input$bi_data_col_g2,
        ref_col = input$data_choice_ref_bi,
        data_col = input$data_choice_data_bi,
        level_g1 = input$data_choice_g1_bi,
        level_g2 = input$data_choice_g2_bi,
        two_sample = two_col
      )
      ots_group_assignment_html(groups)
    })
    
    # Reference column selection
    output$data_choice_ref_bi <- renderUI({
      data <- filtered_data()
      req(data)
      
      choices <- ots_column_choices(data)
      
      selectInput(
        inputId = ns("data_choice_ref_bi"),
        label = "Factor column",
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
        label = "Response column",
        choices = choices
      )
    })
    
    # Group 1 selection
    output$data_choice_g1_bi <- renderUI({
      data <- filtered_data()
      ref_col <- input$data_choice_ref_bi
      req(data, ref_col)
      
      factor_col <- as.numeric(ref_col)
      factor_levels <- unique(na.omit(data[[factor_col]]))
      ref_name <- names(data)[factor_col]
      
      selectInput(
        inputId = ns("data_choice_g1_bi"),
        label = paste0("Group 1 level (", ref_name, ")"),
        choices = factor_levels
      )
    })
    
    # Group 2 selection
    output$data_choice_g2_bi <- renderUI({
      data <- filtered_data()
      ref_col <- input$data_choice_ref_bi
      g1 <- input$data_choice_g1_bi
      req(data, ref_col, g1)
      
      factor_col <- as.numeric(ref_col)
      factor_levels <- unique(na.omit(data[[factor_col]]))
      ref_name <- names(data)[factor_col]
      temp <- factor_levels[factor_levels != g1]
      
      selectInput(
        inputId = ns("data_choice_g2_bi"),
        label = paste0("Group 2 level (", ref_name, ")"),
        choices = temp
      )
    })
    
    # Alternative hypothesis selector
    output$alt_bi_data <- renderUI({
      data_type <- input$data_type_bi
      req(data_type)
      
      if (data_type == 1) {
        two_col <- ots_column_mode_ready(
          col_g1 = input$bi_data_col_g1,
          col_g2 = input$bi_data_col_g2,
          two_sample = TRUE
        )
        one_col <- ots_column_mode_ready(col_g1 = input$bi_data_col_g1, two_sample = FALSE)
        if (!one_col) {
          return(HTML("<p>Select Group 1 column</p>"))
        }
        samples <- if (two_col) 2L else 1L
      } else {
        if (!isTruthy(input$data_choice_ref_bi)) {
          return(HTML("<p>Select factor column</p>"))
        }
        samples <- 2L
      }
      
      if (samples == 1L) {
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
      
      groups <- bi_data_groups()
      if (data_type == 1) {
        if (!ots_column_mode_ready(col_g1 = input$bi_data_col_g1, two_sample = FALSE)) {
          return(NULL)
        }
        req(groups)
        options <- na.omit(unique(groups$g1$x))
        if (length(options) == 1) {
          options <- c(options, paste0("Not ", options))
        }
        col_name <- groups$g1$name
      } else {
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
        if (!ots_column_mode_ready(
          col_g1 = input$bi_data_col_g1,
          col_g2 = input$bi_data_col_g2,
          two_sample = TRUE
        )) {
          return(NULL)
        }
        groups <- bi_data_groups()
        req(groups, groups$g2)
        options <- na.omit(unique(groups$g2$x))
        if (length(options) == 1) {
          options <- c(options, paste0("Not ", options))
        }
        col_name <- groups$g2$name
      } else {
        groups <- bi_data_groups()
        req(groups, groups$g2)
        options <- na.omit(unique(groups$g2$x))
        if (length(options) == 1) {
          options <- c(options, paste0("Not ", options))
        }
        col_name <- groups$g2$name
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
      groups <- bi_data_groups()
      decimal_bi_d <- input$decimal_bi_d
      success1 <- input$bi_data_success1
      req(groups, success1, decimal_bi_d)
      x <- na.omit(groups$g1$x)
      req(length(x) > 0)
      p_val <- ro(sum(x == success1, na.rm = TRUE) / length(x), decimal_bi_d)
      if (is.null(groups$g2)) {
        withMathJax(paste("$p = $", p_val))
      } else {
        ots_stat_value_html("p", groups$g1, p_val)
      }
    })
    
    output$bi_test_data_ui2 <- renderUI({
      groups <- bi_data_groups()
      req(groups)
      if (is.null(groups$g2)) {
        numericInput(
          inputId = ns("bi_test_data_ui2"),
          label = withMathJax("$\\pi_{0}:{ }$"),
          value = 0.5,
          min = 0,
          max = 1,
          width = "150px"
        )
      } else {
        decimal_bi_d <- input$decimal_bi_d
        success2 <- input$bi_data_success2
        req(success2, decimal_bi_d)
        x <- na.omit(groups$g2$x)
        req(length(x) > 0)
        p_val <- ro(sum(x == success2, na.rm = TRUE) / length(x), decimal_bi_d)
        ots_stat_value_html("p", groups$g2, p_val)
      }
    })
    
    output$bi_test_data_ui3 <- renderUI({
      groups <- bi_data_groups()
      req(groups)
      x <- na.omit(groups$g1$x)
      req(length(x) > 0)
      n <- length(x)
      if (is.null(groups$g2)) {
        withMathJax(paste("$n = $", n))
      } else {
        ots_n_sub_html(1L, n, group = groups$g1)
      }
    })
    
    output$bi_test_data_ui4 <- renderUI({
      groups <- bi_data_groups()
      req(groups, groups$g2)
      x <- na.omit(groups$g2$x)
      req(length(x) > 0)
      ots_n_sub_html(2L, length(x), group = groups$g2)
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
          poi_data_col_g1 = input$poi_data_col_g1,
          poi_data_col_g2 = input$poi_data_col_g2,
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
      poi_sel_cols <- ots_col_vector(input$poi_data_col_g1, input$poi_data_col_g2)
      
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
      
      two_col <- length(poi_sel_cols) >= 2L
      samples <- if (two_col) 2L else 1L
      groups <- ots_groups_from_inputs(
        data,
        mode = data_type,
        col_g1 = input$poi_data_col_g1,
        col_g2 = input$poi_data_col_g2,
        ref_col = input$data_choice_ref_poi,
        data_col = input$data_choice_data_poi,
        level_g1 = input$data_choice_g1_poi,
        level_g2 = input$data_choice_g2_poi,
        two_sample = two_col
      )
      req(groups)
      
      if (samples == 1) {
        req(groups$g1)
        # One-sample Poisson test
        return(ots_results_mathjax_wrap(ots_html_flatten(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", ots_mj_paste_stat("c", groups$g1, resultsR$statistic), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\lambda_{0} =$"), resultsR$null.value), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("n", groups$g1, resultsR$parameter), "</td>",
          "<td>", "</td>",
          "<td>", "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", ots_mj_paste_stat("\\lambda", groups$g1)), "</td>",
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
        )), ns("poi_data_mj")))
      } else if (samples == 2) {
        req(groups$g2)
        g1_x <- na.omit(groups$g1$x)
        g2_x <- na.omit(groups$g2$x)
        count1 <- sum(g1_x)
        n1 <- length(g1_x)
        count2 <- sum(g2_x)
        n2 <- length(g2_x)
        
        return(ots_results_mathjax_wrap(ots_html_flatten(c(
          paste("<b>", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr><td style='border-bottom:1px solid #000'>", groups$g1$name, "</td><td style='border-bottom:1px solid #000'></td>",
          "<td style='border-bottom:1px solid #000'>", groups$g2$name, "</td></tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("c", groups$g1, count1), "</td>",
          "<td>", "</td>",
          "<td>", ots_mj_paste_stat("c", groups$g2, count2), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("n", groups$g1, n1), "</td>",
          "<td>", "</td>",
          "<td>", ots_mj_paste_stat("n", groups$g2, n2), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", ots_mj_paste_stat("\\lambda", groups$g1)), "</td>",
          "<td>", resultsR[["estimate"]][["g1.lambda.lowerci"]], "</td>",
          "<td>", "to", "</td>",
          "<td>", resultsR[["estimate"]][["g1.lambda.upperci"]], "</td>",
          "</tr>",
          "<tr>",
          "<td align='right'>", paste(ots_mj_paste_stat("\\lambda", groups$g2)), "</td>",
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
        )), ns("poi_data_mj")))
      }
    })
    
    # =========================================================================
    # POISSON UI RENDERING FUNCTIONS
    # =========================================================================
    
    output$data_choice_column_poi <- renderUI({
      data <- filtered_data()
      req(data, ncol(data) > 0)
      choices <- ots_column_choices(data)
      tagList(
        selectInput(
          inputId = ns("poi_data_col_g1"),
          label = "Group 1 column",
          choices = c("Select column..." = "", choices),
          selected = ""
        ),
        selectInput(
          inputId = ns("poi_data_col_g2"),
          label = "Group 2 column (leave blank for one-sample)",
          choices = c("—" = "", choices),
          selected = ""
        )
      )
    })
    
    output$ots_poi_group_assignment <- renderUI({
      data <- filtered_data()
      req(data, input$data_type_poi)
      two_col <- if (input$data_type_poi == 1) {
        ots_column_mode_ready(
          col_g1 = input$poi_data_col_g1,
          col_g2 = input$poi_data_col_g2,
          two_sample = TRUE
        )
      } else {
        ots_reference_mode_ready(
          input$data_choice_ref_poi,
          input$data_choice_data_poi,
          input$data_choice_g1_poi,
          input$data_choice_g2_poi,
          two_sample = TRUE
        )
      }
      groups <- ots_groups_from_inputs(
        data,
        mode = input$data_type_poi,
        col_g1 = input$poi_data_col_g1,
        col_g2 = input$poi_data_col_g2,
        ref_col = input$data_choice_ref_poi,
        data_col = input$data_choice_data_poi,
        level_g1 = input$data_choice_g1_poi,
        level_g2 = input$data_choice_g2_poi,
        two_sample = two_col
      )
      ots_group_assignment_html(groups)
    })
    
    # Reference column selection
    output$data_choice_ref_poi <- renderUI({
      data <- filtered_data()
      req(data)
      
      selectInput(
        inputId = ns("data_choice_ref_poi"),
        label = "Factor column",
        choices = ots_column_choices(data)
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
        label = "Response column",
        choices = choices
      )
    })
    
    # Group 1 selection
    output$data_choice_g1_poi <- renderUI({
      data <- filtered_data()
      ref_col <- input$data_choice_ref_poi
      req(data, ref_col)
      
      factor_col <- as.numeric(ref_col)
      factor_levels <- unique(na.omit(data[[factor_col]]))
      ref_name <- names(data)[factor_col]
      
      selectInput(
        inputId = ns("data_choice_g1_poi"),
        label = paste0("Group 1 level (", ref_name, ")"),
        choices = factor_levels
      )
    })
    
    # Group 2 selection
    output$data_choice_g2_poi <- renderUI({
      data <- filtered_data()
      ref_col <- input$data_choice_ref_poi
      g1 <- input$data_choice_g1_poi
      req(data, ref_col, g1)
      
      factor_col <- as.numeric(ref_col)
      factor_levels <- unique(na.omit(data[[factor_col]]))
      ref_name <- names(data)[factor_col]
      temp <- factor_levels[factor_levels != g1]
      
      selectInput(
        inputId = ns("data_choice_g2_poi"),
        label = paste0("Group 2 level (", ref_name, ")"),
        choices = temp
      )
    })
    
    # Alternative hypothesis selector
    output$alt_poi_data <- renderUI({
      data_type <- input$data_type_poi
      req(data_type)
      
      if (data_type == 1) {
        # Columns mode
        poi_sel_cols <- ots_col_vector(input$poi_data_col_g1, input$poi_data_col_g2)
        if (!isTruthy(poi_sel_cols)) {
          return(HTML("<p>Select at least one data column</p>"))
        }
        samples <- length(poi_sel_cols)
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
        sel_col <- as.numeric(ots_col_vector(input$poi_data_col_g1, input$poi_data_col_g2))
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
        sel_col <- as.numeric(ots_col_vector(input$poi_data_col_g1, input$poi_data_col_g2))
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
            width = "150px"
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
        sel_col <- as.numeric(ots_col_vector(input$poi_data_col_g1, input$poi_data_col_g2))
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
        sel_col <- as.numeric(ots_col_vector(input$poi_data_col_g1, input$poi_data_col_g2))
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
          np_data_col_g1 = input$np_data_col_g1,
          np_data_col_g2 = input$np_data_col_g2,
          np_tests_data = input$np_tests_data,
          choice_np_alt_data = input$choice_np_alt_stat_data,
          data_choice_ref_np = input$data_choice_ref_np,
          data_choice_data_np = input$data_choice_data_np,
          data_choice_g1_np = input$data_choice_g1_np,
          data_choice_g2_np = input$data_choice_g2_np,
          np_data_UI2 = input$np_data_UI2,
          np_mc_pass = input$np_mc_pass,
          np_data_u_go = input$np_data_u_go,
          np_runs_cut_method = input$np_runs_cut_method,
          np_runs_cut_value = input$np_runs_cut_value
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
        np_test_choice <- c(1, 2, 8)
        names(np_test_choice) <- c("Sign Test for Location", "Wilcoxon Test for Location", "Runs Test for Randomness")
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
      } else if (np_tests == 8) {
        title <- "Runs Test for Randomness"
        text_out <- HTML("The Wald–Wolfowitz runs test assesses whether a two-valued sequence is produced randomly (elements mutually independent given the counts of each type). A numeric series is dichotomized about a cut point (median, mean, or a custom value in Use Data); values equal to the cut are omitted. Too few runs suggest clustering; too many suggest oscillation. Exact Swed–Eisenhart p-values and critical values are used when either count is ≤ 10; otherwise the NIST normal approximation for the number of runs is used (no continuity correction).</br></br>For more information see <a href='https://www.itl.nist.gov/div898/handbook/eda/section3/eda35d.htm'>NIST</a> and <a href='https://en.wikipedia.org/wiki/Wald%E2%80%93Wolfowitz_runs_test'>Wikipedia</a>")
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
      } else if (np_tests_data == 8) {
        title <- "Runs Test for Randomness"
        text_out <- HTML("The Wald–Wolfowitz runs test assesses whether a two-valued sequence is produced randomly (elements mutually independent given the counts of each type). Values above the cut are coded +, below are coded −, and values equal to the cut are omitted. <b>Row order is the sequence order</b> — reordering the data changes the test. Choose the cut as the median, mean, or a custom number. Exact inference is used when either side has ≤ 10 observations; otherwise the NIST normal approximation is used.</br></br>For more information see <a href='https://www.itl.nist.gov/div898/handbook/eda/section3/eda35d.htm'>NIST</a> and <a href='https://en.wikipedia.org/wiki/Wald%E2%80%93Wolfowitz_runs_test'>Wikipedia</a>")
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
        numericInput(inputId = ns("npUI1"), label = withMathJax("$n_{above}:{ }$"), value = 2, min = 0, step = 1, width = "150px")
      } else if (np_tests == 2) {
        numericInput(inputId = ns("npUI1"), label = withMathJax("$S^{+}:{ }$"), value = 3, min = 0, step = 1, width = "150px")
      } else if (np_tests == 3) {
        numericInput(inputId = ns("npUI1"), label = withMathJax("$S_{1}:{ }$"), value = 32, min = 0, step = 1, width = "150px")
      } else if (np_tests == 4) {
        numericInput(inputId = ns("npUI1"), label = withMathJax("$n_{1 \\;above}:{ }$"), value = 1, min = 0, step = 1, width = "150px")
      } else if (np_tests == 5) {
        numericInput(inputId = ns("npUI1"), label = withMathJax("$n^+:{ }$"), value = 8, min = 0, step = 1, width = "150px")
      } else if (np_tests == 6) {
        numericInput(inputId = ns("npUI1"), label = withMathJax("$S^{+}:{ }$"), value = 27, min = 0, step = 1, width = "150px")
      } else if (np_tests == 7) {
        HTML("<p style='text-align:center'><b>Pass 2</b></p>")
      } else if (np_tests == 8) {
        numericInput(inputId = ns("npUI1"), label = withMathJax("$n_{+}:{ }$"), value = 13, min = 0, step = 1, width = "150px")
      }
    })
    
    output$npUI2 <- renderUI({
      np_tests <- input$np_tests
      req(np_tests)
      
      if (np_tests == 1) {
        NULL
      } else if (np_tests == 2) {
        numericInput(inputId = ns("npUI2"), label = withMathJax("$S^{-}:{ }$"), value = 150, min = 0, step = 1, width = "150px")
      } else if (np_tests == 3) {
        numericInput(inputId = ns("npUI2"), label = withMathJax("$S_{2}:{ }$"), value = 46, min = 0, step = 1, width = "150px")
      } else if (np_tests == 4) {
        numericInput(inputId = ns("npUI2"), label = withMathJax("$n_{2 \\;above}:{ }$"), value = 5, min = 0, step = 1, width = "150px")
      } else if (np_tests == 5 || np_tests == 6) {
        NULL
      } else if (np_tests == 7) {
        HTML("<p style='text-align:center'><b>Fail 2</b></p>")
      } else if (np_tests == 8) {
        numericInput(inputId = ns("npUI2"), label = withMathJax("$n_{-}:{ }$"), value = 8, min = 0, step = 1, width = "150px")
      }
    })
    
    output$npUI3 <- renderUI({
      np_tests <- input$np_tests
      req(np_tests)
      
      if (np_tests == 1) {
        numericInput(inputId = ns("npUI3"), label = withMathJax("$n_{equal}:{ }$"), value = 0, min = 0, step = 1, width = "150px")
      } else if (np_tests == 2) {
        numericInput(inputId = ns("npUI3"), label = withMathJax("$n_{Adj.}:{ }$"), value = 17, min = 1, step = 1, width = "150px")
      } else if (np_tests == 3) {
        numericInput(inputId = ns("npUI3"), label = withMathJax("$n_{1}:{ }$"), value = 6, min = 0, step = 1, width = "150px")
      } else if (np_tests == 4) {
        numericInput(inputId = ns("npUI3"), label = withMathJax("$n_{1 \\;equal}:{ }$"), value = 0, min = 0, step = 1, width = "150px")
      } else if (np_tests == 5) {
        numericInput(inputId = ns("npUI3"), label = withMathJax("$n^=:{ }$"), value = 0, min = 0, step = 1, width = "150px")
      } else if (np_tests == 6) {
        numericInput(inputId = ns("npUI3"), label = withMathJax("$S^{-}:{ }$"), value = 18, min = 0, step = 1, width = "150px")
      } else if (np_tests == 7) {
        numericInput(inputId = ns("npUI3"), label = "Pass 1", value = 56, min = 0, step = 1, width = "150px")
      } else if (np_tests == 8) {
        numericInput(inputId = ns("npUI3"), label = withMathJax("$R:{ }$"), value = 6, min = 2, step = 1, width = "150px")
      }
    })
    
    output$npUI4 <- renderUI({
      np_tests <- input$np_tests
      req(np_tests)
      
      if (np_tests %in% c(1, 2)) {
        NULL
      } else if (np_tests == 3) {
        numericInput(inputId = ns("npUI4"), label = withMathJax("$n_{2}:{ }$"), value = 6, min = 0, step = 1, width = "150px")
      } else if (np_tests == 4) {
        numericInput(inputId = ns("npUI4"), label = withMathJax("$n_{2 \\;equal}:{ }$"), value = 0, min = 0, step = 1, width = "150px")
      } else if (np_tests %in% c(5, 6)) {
        NULL
      } else if (np_tests == 7) {
        numericInput(inputId = ns("npUI4"), label = "Pass 1", value = 4, min = 0, step = 1, width = "150px")
      }
    })
    
    output$npUI5 <- renderUI({
      np_tests <- input$np_tests
      req(np_tests)
      
      if (np_tests == 1) {
        numericInput(inputId = ns("npUI5"), label = withMathJax("$n_{below}:{ }$"), value = 8, min = 0, step = 1, width = "150px")
      } else if (np_tests %in% c(2, 3)) {
        NULL
      } else if (np_tests == 4) {
        numericInput(inputId = ns("npUI5"), label = withMathJax("$n_{1 \\;below}:{ }$"), value = 5, min = 0, step = 1, width = "150px")
      } else if (np_tests == 5) {
        numericInput(inputId = ns("npUI5"), label = withMathJax("$n^-:{ }$"), value = 2, min = 0, step = 1, width = "150px")
      } else if (np_tests == 6) {
        numericInput(inputId = ns("npUI5"), label = withMathJax("$n:{ }$"), value = 10, min = 1, step = 1, width = "150px")
      } else if (np_tests == 7) {
        numericInput(inputId = ns("npUI5"), label = "Fail 1", value = 56, min = 1, step = 1, width = "150px")
      }
    })
    
    output$npUI6 <- renderUI({
      np_tests <- input$np_tests
      req(np_tests)
      
      if (np_tests %in% c(1, 2, 3, 5, 6)) {
        NULL
      } else if (np_tests == 4) {
        numericInput(inputId = ns("npUI6"), label = withMathJax("$n_{2 \\;below}:{ }$"), value = 1, min = 0, step = 1, width = "150px")
      } else if (np_tests == 7) {
        numericInput(inputId = ns("npUI6"), label = "Fail 1", value = 4, min = 1, step = 1, width = "150px")
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
      # Test 8: Runs test for randomness
      else if (np_tests == 8) {
        is_exact <- identical(resultsR$inference_mode, "exact")
        crit_row <- if (is_exact) {
          c(
            "<tr>",
            "<td>", paste(withMathJax("$R_{L} = $"), resultsR$estimate[["critical.lower"]]), "</td>",
            "<td>", "</td>",
            "<td>", paste(withMathJax("$R_{U} = $"), resultsR$estimate[["critical.upper"]]), "</td>",
            "</tr>"
          )
        } else {
          character(0)
        }
        stat_label <- if (is_exact) {
          paste(withMathJax("$R = $"), resultsR$estimate[["runs"]])
        } else {
          paste(withMathJax("$Z = $"), resultsR$estimate[["Z"]])
        }
        HTML(c(
          paste("<b>Runs Test for Randomness</b><br>", "<b>", "Method: ", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$n_{+} = $"), resultsR$estimate[["n1"]]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$n_{-} = $"), resultsR$estimate[["n2"]]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$R = $"), resultsR$estimate[["runs"]]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\bar{R} = $"), resultsR$estimate[["expected.runs"]]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$s_{R} = $"), resultsR$estimate[["sd.runs"]]), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$Z = $"), resultsR$estimate[["Z"]], if (is_exact) " (large-sample approx.)"), "</td>",
          "</tr>",
          crit_row,
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence; inference: ", resultsR$inference_mode), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_np_alt_text[3 * (as.numeric(np_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", stat_label, "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        ))
      }
    })
    
    # =========================================================================
    # USE DATA MODE - UI RENDERING FUNCTIONS
    # =========================================================================
    
    output$data_choice_column_np <- renderUI({
      data <- filtered_data()
      req(data, ncol(data) > 0)
      choices <- ots_column_choices(data)
      tagList(
        selectInput(
          inputId = ns("np_data_col_g1"),
          label = "Group 1 column",
          choices = c("Select column..." = "", choices),
          selected = ""
        ),
        selectInput(
          inputId = ns("np_data_col_g2"),
          label = "Group 2 column (leave blank for one-sample)",
          choices = c("—" = "", choices),
          selected = ""
        ),
        tags$p(
          class = "help-block text-muted",
          style = "font-size: 0.85em; margin-top: 0;",
          "For dependent tests, row i in Group 1 is paired with row i in Group 2."
        )
      )
    })
    
    output$ots_np_group_assignment <- renderUI({
      req(filtered_data(), input$data_type_np)
      ots_group_assignment_html(np_data_groups())
    })

    np_data_groups <- reactive({
      data <- filtered_data()
      req(data, input$data_type_np)
      np_tests <- input$np_tests_data
      # Semantic one- vs two-sample flag (not input-readiness). Passing readiness
      # as two_sample made one-column Sign/Wilcoxon resolve as two-sample and
      # return NULL when Group 2 was blank — blank results despite valid calc.
      two_sample_np <- !is.null(np_tests) && as.integer(np_tests)[1L] %in% c(3L, 4L, 5L, 6L, 7L)
      ots_groups_from_inputs(
        data,
        mode = input$data_type_np,
        col_g1 = input$np_data_col_g1,
        col_g2 = input$np_data_col_g2,
        ref_col = input$data_choice_ref_np,
        data_col = input$data_choice_data_np,
        level_g1 = input$data_choice_g1_np,
        level_g2 = input$data_choice_g2_np,
        two_sample = two_sample_np
      )
    })
    
    # Number of selected columns reactive
    num_selected_columns_np_data <- reactive({
      np_sel_cols <- ots_col_vector(input$np_data_col_g1, input$np_data_col_g2)
      if (is.null(np_sel_cols)) return(0)
      length(np_sel_cols)
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
        np_test_choice <- c(1, 2, 8)
        names(np_test_choice) <- c("Sign Test for Location", "Wilcoxon Test for Location", "Runs Test for Randomness")
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
      
      columns <- as.numeric(ots_col_vector(input$np_data_col_g1, input$np_data_col_g2))
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
      groups <- np_data_groups()
      R <- input$decimal_np_data
      data <- filtered_data()
      np_sel_cols <- ots_col_vector(input$np_data_col_g1, input$np_data_col_g2)
      
      req(np_tests, data)
      
      if (np_tests %in% c(1L, 2L)) {
        req(groups, groups$g1)
        med <- median(na.omit(groups$g1$x))
        if (!is.null(R)) med <- ro(med, R)
        ots_stat_value_html("\\widetilde{X}", groups$g1, med)
      } else if (np_tests == 8L) {
        req(groups, groups$g1)
        cut_method <- input$np_runs_cut_method
        cut_value <- input$np_runs_cut_value
        if (is.null(cut_method)) cut_method <- "median"
        x1 <- na.omit(groups$g1$x)
        req(length(x1) > 0, is.numeric(x1))
        cut <- tryCatch(
          resolve_runs_cutpoint(x1, cut_method = as.character(cut_method)[1L], cut_value = cut_value),
          error = function(e) NA_real_
        )
        dich <- tryCatch(dichotomize_for_runs(x1, cut), error = function(e) NULL)
        if (is.null(dich)) {
          HTML("<p class='help-block'>Need observations on both sides of the cut.</p>")
        } else {
          n1_disp <- dich$n1
          if (!is.null(R)) {
            cut <- ro(cut, R)
          }
          tagList(
            HTML(paste0(withMathJax("$n_{+} = $"), n1_disp)),
            HTML(paste0("<br/>", withMathJax("$\\mathrm{cut} = $"), cut))
          )
        }
      } else if (np_tests %in% c(3L, 4L)) {
        req(groups, groups$g1)
        med <- median(na.omit(groups$g1$x))
        if (!is.null(R)) med <- ro(med, R)
        ots_stat_value_html("\\widetilde{X}", groups$g1, med)
      } else if (np_tests == 5L) {
        req(groups, groups$g2)
        temp <- median.test.twosample.dependent.signtest(
          g1 = groups$g1$x,
          g2 = groups$g2$x
        )
        n_plus <- temp$estimate[3]
        if (!is.null(R)) n_plus <- ro(n_plus, R)
        ots_mj_mod_stat_html("n", groups$g1, "^{+}", n_plus)
      } else if (np_tests == 6L) {
        req(groups, groups$g2)
        paired <- ots_paired_frame(groups$g1$x, groups$g2$x)
        n_plus <- if (is.null(paired)) 0L else sum(paired$g1 > paired$g2)
        ots_mj_mod_stat_html("n", groups$g1, "^{+}", n_plus)
      } else if (np_tests == 7L) {
        pass <- input$np_mc_pass
        req(np_sel_cols, data, pass, length(np_sel_cols) >= 2)
        x1 <- data[, as.numeric(np_sel_cols[1])]
        x2 <- data[, as.numeric(np_sel_cols[2])]
        cats <- unique(c(
          na.omit(unique(data[[as.numeric(np_sel_cols[1])]])),
          na.omit(unique(data[[as.numeric(np_sel_cols[2])]]))
        ))
        cats <- c(cats[cats == pass], cats[cats != pass])
        mctable <- table(
          factor(x1, levels = cats, labels = c("Pass", "Fail")),
          factor(x2, levels = cats, labels = c("Pass", "Fail"))
        )
        HTML(paste0(withMathJax("$P_{1}P_{2}=$"), mctable[1, 1]))
      }
    })
    
    output$npUI2_data <- renderUI({
      np_tests <- input$np_tests_data
      groups <- np_data_groups()
      R <- input$decimal_np_data
      data <- filtered_data()
      np_sel_cols <- ots_col_vector(input$np_data_col_g1, input$np_data_col_g2)
      
      req(np_tests)
      
      if (np_tests == 1L) {
        numericInput(
          inputId = ns("np_data_UI2"),
          label = withMathJax("$M_0{ }$"),
          value = 2,
          min = 0,
          step = 1,
          width = "150px"
        )
      } else if (np_tests == 2L) {
        numericInput(
          inputId = ns("np_data_UI2"),
          label = withMathJax("$M_0:{ }$"),
          value = 3,
          min = 0,
          step = 1,
          width = "150px"
        )
      } else if (np_tests == 8L) {
        tagList(
          radioButtons(
            inputId = ns("np_runs_cut_method"),
            label = "Cut point",
            choices = c("Median" = "median", "Mean" = "mean", "Custom" = "custom"),
            selected = "median"
          ),
          conditionalPanel(
            condition = paste0("input['", ns("np_runs_cut_method"), "'] == 'custom'"),
            numericInput(
              inputId = ns("np_runs_cut_value"),
              label = "Custom cut value",
              value = 0,
              width = "150px"
            )
          ),
          tags$p(
            class = "help-block text-muted",
            style = "font-size: 0.85em;",
            "Values equal to the cut are omitted. Row order is the sequence order."
          )
        )
      } else if (np_tests %in% c(3L, 4L)) {
        req(groups, groups$g2)
        med <- median(na.omit(groups$g2$x))
        if (!is.null(R)) med <- ro(med, R)
        ots_stat_value_html("\\widetilde{X}", groups$g2, med)
      } else if (np_tests %in% c(5L, 6L)) {
        NULL
      } else if (np_tests == 7L) {
        if (as.integer(input$data_type_np)[1L] != 1L) {
          return(NULL)
        }
        pass <- input$np_mc_pass
        req(np_sel_cols, data, pass, length(np_sel_cols) >= 2)
        x1 <- data[, as.numeric(np_sel_cols[1])]
        x2 <- data[, as.numeric(np_sel_cols[2])]
        cats <- unique(c(
          na.omit(unique(data[[as.numeric(np_sel_cols[1])]])),
          na.omit(unique(data[[as.numeric(np_sel_cols[2])]]))
        ))
        cats <- c(cats[cats == pass], cats[cats != pass])
        mctable <- table(
          factor(x1, levels = cats, labels = c("Pass", "Fail")),
          factor(x2, levels = cats, labels = c("Pass", "Fail"))
        )
        HTML(paste0(withMathJax("$P_{1}F_{2}=$"), mctable[1, 2]))
      }
    })
    
    output$npUI3_data <- renderUI({
      np_tests <- input$np_tests_data
      groups <- np_data_groups()
      data <- filtered_data()
      np_sel_cols <- ots_col_vector(input$np_data_col_g1, input$np_data_col_g2)
      
      req(np_tests, data)
      
      if (np_tests %in% c(1L, 2L)) {
        req(groups, groups$g1)
        ots_n_sub_html(group = groups$g1, value = length(na.omit(groups$g1$x)))
      } else if (np_tests == 8L) {
        req(groups, groups$g1)
        cut_method <- input$np_runs_cut_method
        cut_value <- input$np_runs_cut_value
        if (is.null(cut_method)) cut_method <- "median"
        x1 <- na.omit(groups$g1$x)
        req(length(x1) > 0, is.numeric(x1))
        cut <- tryCatch(
          resolve_runs_cutpoint(x1, cut_method = as.character(cut_method)[1L], cut_value = cut_value),
          error = function(e) NA_real_
        )
        dich <- tryCatch(dichotomize_for_runs(x1, cut), error = function(e) NULL)
        if (is.null(dich)) {
          NULL
        } else {
          R_obs <- count_runs(dich$signs)
          HTML(paste0(
            withMathJax("$n_{-} = $"), dich$n2, "<br/>",
            withMathJax("$n_{=} = $"), dich$n.equal, " (omitted)<br/>",
            withMathJax("$R = $"), R_obs
          ))
        }
      } else if (np_tests %in% c(3L, 4L)) {
        req(groups, groups$g1)
        ots_n_sub_html(group = groups$g1, value = length(na.omit(groups$g1$x)))
      } else if (np_tests == 5L) {
        req(groups, groups$g2)
        temp <- median.test.twosample.dependent.signtest(
          g1 = groups$g1$x,
          g2 = groups$g2$x
        )
        n_incl <- temp$estimate[2]
        n_equal <- length(na.omit(groups$g2$x)) - n_incl
        ots_mj_mod_stat_html("n", groups$g1, "^{=}", n_equal)
      } else if (np_tests == 6L) {
        req(groups, groups$g2)
        paired <- ots_paired_frame(groups$g1$x, groups$g2$x)
        n_minus <- if (is.null(paired)) 0L else sum(paired$g1 < paired$g2)
        ots_mj_mod_stat_html("n", groups$g1, "^{-}", n_minus)
      } else if (np_tests == 7L) {
        pass <- input$np_mc_pass
        req(np_sel_cols, data, pass, length(np_sel_cols) >= 2)
        x1 <- data[, as.numeric(np_sel_cols[1])]
        x2 <- data[, as.numeric(np_sel_cols[2])]
        cats <- unique(c(
          na.omit(unique(data[[as.numeric(np_sel_cols[1])]])),
          na.omit(unique(data[[as.numeric(np_sel_cols[2])]]))
        ))
        cats <- c(cats[cats == pass], cats[cats != pass])
        mctable <- table(
          factor(x1, levels = cats, labels = c("Pass", "Fail")),
          factor(x2, levels = cats, labels = c("Pass", "Fail"))
        )
        HTML(paste0(withMathJax("$F_{1}P_{2}=$"), mctable[2, 1]))
      }
    })
    
    output$npUI4_data <- renderUI({
      np_tests <- input$np_tests_data
      groups <- np_data_groups()
      data <- filtered_data()
      np_sel_cols <- ots_col_vector(input$np_data_col_g1, input$np_data_col_g2)
      
      req(np_tests, data)
      
      if (np_tests %in% c(3L, 4L)) {
        req(groups, groups$g2)
        ots_n_sub_html(group = groups$g2, value = length(na.omit(groups$g2$x)))
      } else if (np_tests %in% c(5L, 6L)) {
        NULL
      } else if (np_tests == 7L) {
        pass <- input$np_mc_pass
        req(np_sel_cols, data, pass, length(np_sel_cols) >= 2)
        x1 <- data[, as.numeric(np_sel_cols[1])]
        x2 <- data[, as.numeric(np_sel_cols[2])]
        cats <- unique(c(
          na.omit(unique(data[[as.numeric(np_sel_cols[1])]])),
          na.omit(unique(data[[as.numeric(np_sel_cols[2])]]))
        ))
        cats <- c(cats[cats == pass], cats[cats != pass])
        mctable <- table(
          factor(x1, levels = cats, labels = c("Pass", "Fail")),
          factor(x2, levels = cats, labels = c("Pass", "Fail"))
        )
        HTML(paste0(withMathJax("$F_{1}F_{2}=$"), mctable[2, 2]))
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
      data <- filtered_data()
      np_sel_cols <- ots_col_vector(input$np_data_col_g1, input$np_data_col_g2)
      np_groups <- np_data_groups()
      
      req(conf, alt, np_tests, data)
      
      # For Reference Column mode, we don't need np_sel_cols
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
        req(np_groups, np_groups$g1)
        len <- length(na.omit(np_groups$g1$x))
        return(ots_results_mathjax_wrap(ots_html_flatten(c(
          paste("<b>Sign Test for Location</b><br>", "<b>", "Method: ", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", ots_mj_mod_stat("n", np_groups$g1, "\\; above", resultsR$estimate[3]), "</td>",
          "<td>", "</td>",
          "<td>", ots_mj_mod_stat("p", np_groups$g1, "\\; above", resultsR$statistic), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_mod_stat("n", np_groups$g1, "\\; equal", len - resultsR$estimate[2]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_mod_stat("n", np_groups$g1, "\\; below", resultsR$estimate[2] - resultsR$estimate[3]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_mod_stat("n", np_groups$g1, "\\; observed", len), "</td>",
          "<td>", "</td>",
          "<td>", ots_mj_mod_stat("n", np_groups$g1, "\\; included", resultsR$estimate[2]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for proportion above = ", resultsR$parameter), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", ots_mj_mod_stat("\\pi", np_groups$g1, "\\; above")), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_np_alt_text[3 * (as.numeric(np_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", ots_mj_mod_stat("p", np_groups$g1, "\\; above", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table></br>",
          "<table><tr><td>", beta_statement, resultsR$estimate[4], "</td></tr>",
          "</table>"
        )), ns("np_data_mj")))
      }
      # Test 2: One-sample Wilcoxon signed ranks test
      else if (np_tests == 2) {
        req(np_groups, np_groups$g1)
        req(resultsR$method, resultsR$estimate, resultsR$statistic, resultsR$p.value)
        len <- length(na.omit(np_groups$g1$x))
        return(ots_results_mathjax_wrap(ots_html_flatten(c(
          paste("<b>", "Method: ", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste("$S^{+} =$", resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", paste("$S^{-} =$", resultsR$estimate[2]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("n", np_groups$g1, len), "</td>",
          "<td>", "</td>",
          "<td>", paste("$n_{Adj.} =$", resultsR$estimate[3]), "</td>",
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
        )), ns("np_data_mj")))
      }
      # Test 8: Runs test for randomness
      else if (np_tests == 8) {
        req(resultsR$method, resultsR$estimate, resultsR$p.value, resultsR$inference_mode)
        req(np_groups, np_groups$g1)
        is_exact <- identical(resultsR$inference_mode, "exact")
        cut_method_label <- if (!is.null(resultsR$cut_method)) resultsR$cut_method else input$np_runs_cut_method
        cut_val <- resultsR$estimate[["cut"]]
        n_eq <- resultsR$estimate[["n.equal"]]
        crit_row <- if (is_exact) {
          c(
            "<tr>",
            "<td>", paste("$R_{L} =$", resultsR$estimate[["critical.lower"]]), "</td>",
            "<td>", "</td>",
            "<td>", paste("$R_{U} =$", resultsR$estimate[["critical.upper"]]), "</td>",
            "</tr>"
          )
        } else {
          character(0)
        }
        stat_label <- if (is_exact) {
          paste("$R =$", resultsR$estimate[["runs"]])
        } else {
          paste("$Z =$", resultsR$estimate[["Z"]])
        }
        return(ots_results_mathjax_wrap(ots_html_flatten(c(
          paste("<b>Runs Test for Randomness</b><br>", "<b>", "Method: ", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste("Cut method:", cut_method_label), "</td>",
          "<td>", "</td>",
          "<td>", paste("$\\mathrm{cut} =$", cut_val), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("$n_{+} =$", resultsR$estimate[["n1"]]), "</td>",
          "<td>", "</td>",
          "<td>", paste("$n_{-} =$", resultsR$estimate[["n2"]]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("$n_{=} =$", n_eq, "(omitted)"), "</td>",
          "<td>", "</td>",
          "<td>", paste("$R =$", resultsR$estimate[["runs"]]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("$\\bar{R} =$", resultsR$estimate[["expected.runs"]]), "</td>",
          "<td>", "</td>",
          "<td>", paste("$s_{R} =$", resultsR$estimate[["sd.runs"]]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("$Z =$", resultsR$estimate[["Z"]], if (is_exact) "(large-sample approx.)"), "</td>",
          "</tr>",
          crit_row,
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence; inference: ", resultsR$inference_mode), "</td>",
          "</tr>",
          "<tr>",
          "<td>", "Note: row order is the sequence order.", "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_np_alt_text[3 * (as.numeric(np_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", stat_label, "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>"
        )), ns("np_data_mj")))
      }
      # Test 3: Two-sample Mann-Whitney U test
      else if (np_tests == 3) {
        # Validate results structure
        req(resultsR$method, resultsR$estimate, resultsR$statistic, resultsR$p.value)
        req(length(resultsR$estimate) >= 4)
        
        req(np_groups, np_groups$g2)
        
        return(ots_results_mathjax_wrap(ots_html_flatten(c(
          paste("<b>", "Method: ", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr><td style='border-bottom:1px solid #000'>", np_groups$g1$name, "</td><td style='border-bottom:1px solid #000'></td>",
          "<td style='border-bottom:1px solid #000'>", np_groups$g2$name, "</td></tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("S", np_groups$g1, resultsR$estimate[1]), "</td>",
          "<td>", "</td>",
          "<td>", ots_mj_paste_stat("S", np_groups$g2, resultsR$estimate[2]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_paste_stat("n", np_groups$g1, resultsR$estimate[3]), "</td>",
          "<td>", "</td>",
          "<td>", ots_mj_paste_stat("n", np_groups$g2, resultsR$estimate[4]), "</td>",
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
        )), ns("np_data_mj")))
      }
      # Test 4: Two-sample median test (Mood's)
      else if (np_tests == 4) {
        # Validate results structure (median test doesn't have statistic field)
        req(resultsR$method, resultsR$estimate, resultsR$p.value, resultsR$conf.int)
        req(length(resultsR$estimate) >= 12)
        
        req(np_groups, np_groups$g2)
        len1 <- length(na.omit(np_groups$g1$x))
        len2 <- length(na.omit(np_groups$g2$x))
        
        return(ots_results_mathjax_wrap(ots_html_flatten(c(
          paste("<b>", "Two-Sample Median Test<br>Method: ", resultsR$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr><td style='border-bottom:1px solid #000'>", np_groups$g1$name, "</td><td style='border-bottom:1px solid #000'></td>",
          "<td style='border-bottom:1px solid #000'>", np_groups$g2$name, "</td></tr>",
          "<tr>",
          "<td>", ots_mj_mod_stat("n", np_groups$g1, "\\; above", resultsR$estimate[3]), "</td><td></td>",
          "<td>", ots_mj_mod_stat("p", np_groups$g1, "\\; above", resultsR$estimate[1]), "</td>",
          "<td>", ots_mj_mod_stat("n", np_groups$g2, "\\; above", resultsR$estimate[9]), "</td><td></td>",
          "<td>", ots_mj_mod_stat("p", np_groups$g2, "\\; above", resultsR$estimate[7]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_mod_stat("n", np_groups$g1, "\\; equal", len1 - resultsR$estimate[2]), "</td><td></td>",
          "<td>", "</td>",
          "<td>", ots_mj_mod_stat("n", np_groups$g2, "\\; equal", len2 - resultsR$estimate[8]), "</td><td></td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_mod_stat("n", np_groups$g1, "\\; below", resultsR$estimate[4]), "</td><td></td>",
          "<td>", "</td>",
          "<td>", ots_mj_mod_stat("n", np_groups$g2, "\\; below", resultsR$estimate[10]), "</td><td></td>",
          "</tr>",
          "<tr>",
          "<td>", ots_mj_mod_stat("n", np_groups$g1, "\\; total\\; inc.", resultsR$estimate[2]), "</td><td></td>",
          "<td>", "</td>",
          "<td>", ots_mj_mod_stat("n", np_groups$g2, "\\; total\\; inc.", resultsR$estimate[8]), "</td><td></td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for", ots_mj_mod_stat("\\pi", np_groups$g1, "\\; above")), "</td>",
          "<td>", resultsR$estimate[5], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$estimate[6], "</td>",
          "</tr>",
          "<tr>",
          "<td align='right'>", paste(ots_mj_mod_stat("\\pi", np_groups$g2, "\\; above")), "</td>",
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
        )), ns("np_data_mj")))
      }
      # Test 5: Two-sample dependent sign test
      else if (np_tests == 5) {
        req(np_groups, np_groups$g2)
        req(resultsR$method, resultsR$estimate, resultsR$statistic, resultsR$p.value, resultsR$conf.int)
        len2 <- length(na.omit(np_groups$g2$x))
        return(ots_results_mathjax_wrap(ots_html_flatten(c(
          paste("<b>Two-Sample Dependent Sign Test for Location</b><br>", "<b>", "Method: ", resultsR$method, "</b>"),
          "<br>",
          paste("Direction assessed by subtracting ", np_groups$g2$name, " from ", np_groups$g1$name),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste("$n^{+} =$", resultsR$estimate[3]), "</td>",
          "<td>", "</td>",
          "<td>", paste("$p^{+} =$", resultsR$statistic), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("$n^{=} =$", len2 - resultsR$estimate[2]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("$n^{-} =$", resultsR$estimate[2] - resultsR$estimate[3]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("$n_{observed} =$", len2), "</td>",
          "<td>", "</td>",
          "<td>", paste("$n_{included} =$", resultsR$estimate[2]), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for the probability that the difference in each pair is positive = ", resultsR$parameter), "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for $\\pi^{+} :$"), "</td>",
          "<td>", resultsR$conf.int[1], "</td>",
          "<td>", " to ", "</td>",
          "<td>", resultsR$conf.int[2], "</td>",
          "</tr>",
          "</table>",
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_np_alt_text[3 * (as.numeric(np_tests) - 1) + alt_num], ": "), "</td>",
          "<td>", paste("$p^{+} =$", resultsR$statistic), "</td>",
          "<td>", paste("p = ", resultsR$p.value, if (!is.na(resultsR$p.value) && resultsR$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          "<table><tr><td>", beta_statement, resultsR$estimate[4], "</td></tr>",
          "</table>"
        )), ns("np_data_mj")))
      }
      # Test 6: Two-sample dependent Wilcoxon signed ranks test
      else if (np_tests == 6) {
        req(np_groups, np_groups$g2)
        req(resultsR$method, resultsR$estimate, resultsR$statistic, resultsR$p.value)
        return(ots_results_mathjax_wrap(ots_html_flatten(c(
          paste("<b>", "Method: Dependent ", resultsR$method, "</b>"),
          "<br>",
          paste("Direction assessed by subtracting ", np_groups$g2$name, " from ", np_groups$g1$name),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste("$S^{+} =$", resultsR$estimate[1]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("$S^{-} =$", resultsR$estimate[2]), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste("$n =$", resultsR$estimate[3]), "</td>",
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
        )), ns("np_data_mj")))
      }
      # Test 7: McNemar's test of change
      else if (np_tests == 7) {
        # Validate results structure
        req(resultsR$method, resultsR$estimate, resultsR$p.value)
        req(length(resultsR$estimate) >= 4)
        
        # Get column names and pass value for display
        if (data_type == 1) {
          # Data in Columns mode
          req(np_sel_cols, length(np_sel_cols) >= 2)
          test1_name <- names(data)[as.numeric(np_sel_cols[1])]
          test2_name <- names(data)[as.numeric(np_sel_cols[2])]
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
