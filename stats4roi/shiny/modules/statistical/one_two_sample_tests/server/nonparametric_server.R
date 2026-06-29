# Nonparametric Server Component
# This worker module handles calculations for Nonparametric tests
# Returns reactive functions with calculation results

# =============================================================================
# IMPORTS
# =============================================================================
library(shiny)
library(lolcat)

# Source global systems
source("modules/config/global_config.R")
source("modules/statistical/one_two_sample_tests/ots_group_utils.R")

# =============================================================================
# WORKER SERVER FUNCTION
# =============================================================================
create_nonparametric_server <- function(id, filtered_data, input_values, mann_whitney_count_func = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================================
    # INPUT EXTRACTION
    # =========================================================================
    inputs <- reactive({
      input_values()
    })
    
    # =========================================================================
    # MANN-WHITNEY U BUTTON TRACKING
    # =========================================================================
    # Track the last processed button click count for Mann-Whitney U test
    # This prevents recalculation until button is clicked again
    last_mann_whitney_count <- reactiveVal(0)
    
    # Cache the last successful results for Mann-Whitney U test
    # This allows results to persist even when button check fails on subsequent reactive invalidations
    cached_mann_whitney_results <- reactiveVal(NULL)
    
    # =========================================================================
    # NP STAT OUT REACTIVE (Enter Statistics mode)
    # =========================================================================
    np_stat_out <- reactive({
      inputs_vals <- inputs()
      req(inputs_vals)
      
      conf <- inputs_vals$conf_np
      R <- inputs_vals$np_decimals
      alt <- inputs_vals$choice_np_alt_stat
      np_tests <- inputs_vals$np_tests
      npUI1 <- inputs_vals$npUI1
      npUI2 <- inputs_vals$npUI2
      npUI3 <- inputs_vals$npUI3
      npUI4 <- inputs_vals$npUI4
      npUI5 <- inputs_vals$npUI5
      npUI6 <- inputs_vals$npUI6
      
      req(np_tests, R, npUI1, alt)
      
      results <- NULL
      
      # Test 1: One-sample sign test
      if (np_tests == 1) {
        req(npUI3)
        results <- median.test.onesample.signtest.simple(
          n.below = npUI1,
          n.equal = npUI3,
          n.above = npUI5,
          alternative = alt,
          conf.level = conf
        )
      }
      # Test 2: One-sample Wilcoxon signed ranks test
      else if (np_tests == 2) {
        req(npUI2, npUI3)
        results <- wilcoxon.signed.ranks.onesample.test.simple(
          sum.ranks.positive = npUI1,
          sum.ranks.negative = npUI2,
          adj.sample.size = npUI3,
          alternative = alt
        )
      }
      # Test 3: Two-sample Mann-Whitney U test
      else if (np_tests == 3) {
        req(npUI2, npUI3, npUI4)
        results <- mann.whitney.u.test.simple(
          sum.ranks.g1 = npUI1,
          sum.ranks.g2 = npUI2,
          sample.size.g1 = npUI3,
          sample.size.g2 = npUI4,
          alternative = alt
        )
      }
      # Test 4: Two-sample median test (Mood's)
      else if (np_tests == 4) {
        req(npUI2, npUI3, npUI4, npUI5)
        results <- median.test.twosample.independent.mood.simple(
          n.below.g1 = npUI5,
          n.equal.g1 = npUI3,
          n.above.g1 = npUI1,
          n.below.g2 = npUI6,
          n.equal.g2 = npUI4,
          n.above.g2 = npUI2,
          alternative = alt,
          conf.level = conf
        )
      }
      # Test 5: Two-sample dependent sign test
      else if (np_tests == 5) {
        req(npUI3, npUI5)
        results <- median.test.twosample.dependent.signtest.simple(
          n.below = npUI5,
          n.equal = npUI3,
          n.above = npUI1,
          alternative = alt,
          conf.level = conf
        )
      }
      # Test 6: Two-sample dependent Wilcoxon signed ranks test
      else if (np_tests == 6) {
        req(npUI3, npUI5)
        results <- wilcoxon.signed.ranks.twosample.test.simple(
          sum.ranks.positive = npUI1,
          sum.ranks.negative = npUI3,
          sample.size = npUI5,
          alternative = alt
        )
      }
      # Test 7: McNemar's test of change
      else if (np_tests == 7) {
        req(npUI4, npUI5)
        results <- proportion.test.mcnemar.simple(
          b = npUI4,
          c = npUI5,
          null.hypothesis.proportion = 0.5,
          alternative = alt,
          conf.level = conf,
          method = "Exact"
        )
      }
      
      # Return unrounded results - rounding happens at rendering stage
      results
    })
    
    # =========================================================================
    # NP DATA OUT REACTIVE (Use Data mode)
    # =========================================================================
    np_data_out <- reactive({
      inputs_vals <- inputs()
      data <- filtered_data()
      req(inputs_vals, data)
      
      R <- inputs_vals$decimal_np_data
      conf <- inputs_vals$conf_np_data
      data_type <- inputs_vals$data_type_np
      np_tests_data <- inputs_vals$np_tests_data
      alt <- inputs_vals$choice_np_alt_data
      
      req(np_tests_data, R, alt, conf, data_type, data)
      
      results <- NULL
      
      two_sample_np <- np_tests_data %in% c(3L, 4L, 5L, 6L, 7L)
      
      if (data_type == 1) {
        if (two_sample_np) {
          if (!ots_column_mode_ready(
            col_g1 = inputs_vals$np_data_col_g1,
            col_g2 = inputs_vals$np_data_col_g2,
            two_sample = TRUE
          )) {
            return(NULL)
          }
        } else if (!ots_column_mode_ready(col_g1 = inputs_vals$np_data_col_g1, two_sample = FALSE)) {
          return(NULL)
        }
        
        groups <- ots_groups_from_inputs(
          data,
          mode = 1L,
          col_g1 = inputs_vals$np_data_col_g1,
          col_g2 = inputs_vals$np_data_col_g2,
          two_sample = two_sample_np
        )
        req(groups)
        x1 <- na.omit(groups$g1$x)
        req(length(x1) > 0, is.numeric(x1) || is.logical(x1))
        x2 <- if (two_sample_np) {
          req(groups$g2)
          x2v <- na.omit(groups$g2$x)
          req(length(x2v) > 0, is.numeric(x2v) || is.logical(x2v))
          x2v
        } else {
          NULL
        }
        
        if (np_tests_data == 1) {
          # One-sample sign test
          location <- inputs_vals$np_data_UI2
          req(location)
          results <- median.test.onesample.signtest(
            x = x1,
            null.hypothesis.location = location,
            alternative = alt,
            conf.level = conf
          )
        } else if (np_tests_data == 2) {
          # One-sample Wilcoxon signed ranks test
          location <- inputs_vals$np_data_UI2
          req(location)
          results <- wilcoxon.signed.ranks.onesample.test(
            x = x1,
            null.hypothesis.location = location,
            alternative = alt
          )
        } else if (np_tests_data == 3) {
          # Two-sample Mann-Whitney U test - requires button click
          button_click <- inputs_vals$np_data_u_go
          current_count <- last_mann_whitney_count()
          
          # Check if button was clicked - if not, return cached results if available
          if (is.null(button_click) || button_click <= current_count) {
            cached <- cached_mann_whitney_results()
            if (!is.null(cached)) {
              return(cached)
            }
            # No cached results and button not clicked - return NULL
            return(NULL)
          }
          
          # Update the count to prevent recalculation until next button click
          last_mann_whitney_count(button_click)
          
          req(!is.null(x2))
          req(length(x1) > 0, length(x2) > 0)
          
          results <- median.test.twosample.independent.mann.whitney(
            g1 = x1,
            g2 = x2,
            alternative = alt
          )
          
          # Cache the results for future use
          cached_mann_whitney_results(results)
        } else if (np_tests_data == 4) {
          # Two-sample median test (Mood's)
          req(!is.null(x2))
          req(length(x1) > 0, length(x2) > 0)
          results <- median.test.twosample.independent.mood(
            g1 = x1,
            g2 = x2,
            alternative = alt,
            conf.level = conf
          )
        } else if (np_tests_data == 5) {
          # Two-sample dependent sign test
          req(!is.null(x2))
          results <- median.test.twosample.dependent.signtest(
            x1 = x1,
            x2 = x2,
            alternative = alt,
            conf.level = conf
          )
        } else if (np_tests_data == 6) {
          # Two-sample dependent Wilcoxon signed ranks test
          req(!is.null(x2))
          results <- wilcoxon.signed.ranks.twosample.test(
            x1 = x1,
            x2 = x2,
            alternative = alt
          )
        } else if (np_tests_data == 7) {
          # McNemar's test of change
          req(!is.null(x2))
          pass1 <- inputs_vals$np_mc_pass
          req(pass1)
          
          # Create 2x2 table for McNemar's test
          b <- sum(x1 == pass1 & x2 != pass1, na.rm = TRUE)
          c <- sum(x1 != pass1 & x2 == pass1, na.rm = TRUE)
          
          results <- proportion.test.mcnemar.simple(
            b = b,
            c = c,
            alternative = alt,
            conf.level = conf
          )
        }
      } else if (data_type == 2) {
        # Use Reference Column mode - factor-based selection
        ref_col <- as.numeric(inputs_vals$data_choice_ref_np)
        data_col <- as.numeric(inputs_vals$data_choice_data_np)
        g1_num <- inputs_vals$data_choice_g1_np
        g2_num <- inputs_vals$data_choice_g2_np
        
        req(ref_col, data_col, g1_num, g2_num, data)
        
        # Validate column indices
        req(ref_col > 0, ref_col <= ncol(data), 
            data_col > 0, data_col <= ncol(data),
            ref_col != data_col)
        
        # Extract groups based on factor values
        req(nrow(data) > 0, 
            length(which(data[[ref_col]] == g1_num)) > 0,
            length(which(data[[ref_col]] == g2_num)) > 0)
        
        g1 <- data[[data_col]][which(data[[ref_col]] == g1_num)]
        g2 <- data[[data_col]][which(data[[ref_col]] == g2_num)]
        
        # Ensure extracted data is numeric
        req(is.numeric(g1) || is.logical(g1), 
            is.numeric(g2) || is.logical(g2))
        
        # Remove NAs
        x1 <- na.omit(g1)
        x2 <- na.omit(g2)
        
        req(length(x1) > 0, length(x2) > 0)
        
        if (np_tests_data == 1) {
          # One-sample sign test - not applicable for Reference Column mode (always two-sample)
          results <- NULL
        } else if (np_tests_data == 2) {
          # One-sample Wilcoxon signed ranks test - not applicable for Reference Column mode
          results <- NULL
        } else if (np_tests_data == 3) {
          # Two-sample Mann-Whitney U test - requires button click
          button_click <- inputs_vals$np_data_u_go
          current_count <- last_mann_whitney_count()
          
          # Check if button was clicked - if not, return cached results if available
          if (is.null(button_click) || button_click <= current_count) {
            cached <- cached_mann_whitney_results()
            if (!is.null(cached)) {
              return(cached)
            }
            # No cached results and button not clicked - return NULL
            return(NULL)
          }
          
          # Update the count to prevent recalculation until next button click
          last_mann_whitney_count(button_click)
          
          results <- median.test.twosample.independent.mann.whitney(
            g1 = x1,
            g2 = x2,
            alternative = alt
          )
          
          # Cache the results for future use
          cached_mann_whitney_results(results)
        } else if (np_tests_data == 4) {
          # Two-sample median test (Mood's)
          results <- median.test.twosample.independent.mood(
            g1 = x1,
            g2 = x2,
            alternative = alt,
            conf.level = conf
          )
        } else if (np_tests_data == 5) {
          # Two-sample dependent sign test
          results <- median.test.twosample.dependent.signtest(
            x1 = x1,
            x2 = x2,
            alternative = alt,
            conf.level = conf
          )
        } else if (np_tests_data == 6) {
          # Two-sample dependent Wilcoxon signed ranks test
          results <- wilcoxon.signed.ranks.twosample.test(
            x1 = x1,
            x2 = x2,
            alternative = alt
          )
        } else if (np_tests_data == 7) {
          # McNemar's test of change
          pass1 <- inputs_vals$np_mc_pass
          req(pass1)
          
          # Create 2x2 table for McNemar's test
          b <- sum(x1 == pass1 & x2 != pass1, na.rm = TRUE)
          c <- sum(x1 != pass1 & x2 == pass1, na.rm = TRUE)
          
          results <- proportion.test.mcnemar.simple(
            b = b,
            c = c,
            alternative = alt,
            conf.level = conf
          )
        }
      }
      
      # Return unrounded results - rounding happens at rendering stage
      results
    })
    
    # =========================================================================
    # RETURN REACTIVE FUNCTIONS
    # =========================================================================
    list(
      np_stat_out = np_stat_out,
      np_data_out = np_data_out
    )
  })
}
