# Means and Dispersion Server Component
# This worker module handles calculations for Means and Dispersion tests
# Returns reactive functions with calculation results

# =============================================================================
# IMPORTS
# =============================================================================
library(shiny)
library(lolcat)

# Source global systems
source("modules/config/global_config.R")

# =============================================================================
# WORKER SERVER FUNCTION
# =============================================================================
create_means_dispersion_server <- function(id, filtered_data, input_values) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================================
    # INPUT EXTRACTION
    # =========================================================================
    # Extract inputs from coordinator
    inputs <- reactive({
      input_values()
    })
    
    # =========================================================================
    # MEAN OUT REACTIVE (Enter Statistics mode)
    # =========================================================================
    mean_out <- reactive({
      inputs_vals <- inputs()
      req(inputs_vals)
      
      conf <- inputs_vals$conf
      UI1 <- inputs_vals$md_UI1
      UI2 <- inputs_vals$md_UI2
      UI3 <- inputs_vals$md_UI3
      UI4 <- inputs_vals$md_UI4
      UI5 <- inputs_vals$md_UI5
      UI6 <- inputs_vals$md_UI6
      md_alt <- inputs_vals$md_alt
      t_type <- inputs_vals$t_type
      md_test_num <- inputs_vals$md_test_num
      R <- inputs_vals$decimal
      
      req(md_test_num, UI1, UI2, UI3, conf, md_alt)
      
      results <- NULL
      
      # One sample z
      if (md_test_num == 1) {
        results <- z.test.onesample.simple(
          sample.mean = UI1,
          known.population.variance = UI3^2,
          sample.size = UI5,
          null.hypothesis.mean = UI2,
          alternative = md_alt,
          conf.level = conf
        )
      }
      # One-sample t
      else if (md_test_num == 2) {
        results <- t.test.onesample.simple(
          sample.mean = UI1,
          sample.variance = UI3^2,
          sample.size = UI5,
          null.hypothesis.mean = UI2,
          alternative = md_alt,
          conf.level = conf
        )
      }
      # One sample var test
      else if (md_test_num == 3) {
        results <- variance.test.onesample.simple(
          sample.variance = UI1^2,
          sample.size = UI3,
          null.hypothesis.variance = UI2^2,
          alternative = md_alt,
          conf.level = conf
        )
      }
      # Two-sample z indep
      else if (md_test_num == 4) {
        req(UI4, UI5, UI6)
        results <- mean.z.test.twosample.independent.simple(
          sample.mean.g1 = UI1,
          variance.g1 = UI3^2,
          sample.size.g1 = UI5,
          sample.mean.g2 = UI2,
          variance.g2 = UI4^2,
          sample.size.g2 = UI6,
          null.hypothesis.difference = 0,
          alternative = md_alt,
          g1.details = TRUE,
          g2.details = TRUE,
          conf.level = conf
        )
      }
      # Two-sample t indep
      else if (md_test_num == 5) {
        req(UI4, UI5, UI6, t_type, md_alt, conf)
        results <- t.test.twosample.independent.simple(
          sample.mean.g1 = UI1,
          sample.variance.g1 = UI3^2,
          sample.size.g1 = UI5,
          sample.mean.g2 = UI2,
          sample.variance.g2 = UI4^2,
          sample.size.g2 = UI6,
          null.hypothesis.difference = 0,
          assume.equal.variances = t_type,
          alternative = md_alt,
          conf.level = conf,
          var.test.conf.level = conf,
          var.test.details = TRUE,
          g1.details = TRUE,
          g2.details = TRUE
        )
      }
      # Two-sample z dep (not applicable)
      else if (md_test_num == 6) {
        results <- "Not Applicable"
      }
      # Two-sample t dep
      else if (md_test_num == 7) {
        dep_t_type <- inputs_vals$md_t_dep_type_stat
        req(dep_t_type)
        if (dep_t_type == 1) {
          req(UI5)
          results <- t.test.twosample.dependent.simple.dbar(
            pair.differences.mean = UI1,
            pair.differences.variance = UI3^2,
            sample.size = UI5,
            null.hypothesis.difference = UI2,
            alternative = md_alt,
            conf.level = conf
          )
        } else if (dep_t_type == 2) {
          req(UI4, UI5, UI6, md_alt, conf)
          results <- t.test.twosample.dependent.simple.meandiff(
            sample.mean.g1 = UI1,
            sample.variance.g1 = UI3^2,
            sample.size = UI5,
            sample.mean.g2 = UI2,
            sample.variance.g2 = UI4^2,
            rho.estimate = UI6,
            null.hypothesis.difference = 0,
            assume.equal.variances = "yes",
            alternative = md_alt,
            conf.level = conf,
            var.test.conf.level = conf,
            var.test.details = TRUE,
            g1.details = TRUE,
            g2.details = TRUE
          )
        }
      }
      
      if (!is.null(results) && !is.character(results)) {
        ro(results, R)
      } else {
        results
      }
    })
    
    # =========================================================================
    # M_D_DATA_OUT REACTIVE (Use Data mode)
    # =========================================================================
    m_d_data_out <- reactive({
      inputs_vals <- inputs()
      data <- filtered_data()
      req(inputs_vals, data)
      
      R <- inputs_vals$decimal_m_d_d
      conf <- inputs_vals$conf_m_d_data
      md_data_selected_columns <- inputs_vals$md_data_selected_columns
      md_data_UI1 <- inputs_vals$md_data_UI1
      md_data_UI2 <- inputs_vals$md_data_UI2
      md_data_UI3 <- inputs_vals$md_data_UI3
      md_data_UI4 <- inputs_vals$md_data_UI4
      one_or_two_md_data <- inputs_vals$one_or_two_md_data
      t_type <- inputs_vals$t_type_dat
      type <- inputs_vals$data_type_md
      ref_col <- as.numeric(inputs_vals$data_choice_ref)
      data_col <- as.numeric(inputs_vals$data_choice_data)
      g1_num <- inputs_vals$data_choice_g1
      g2_num <- inputs_vals$data_choice_g2
      test_num <- inputs_vals$data_md_test_num
      
      req(type, test_num, one_or_two_md_data, data)
      
      results <- NULL
      
      if (type == 1) {
        req(md_data_selected_columns)
        
        # One-sample z
        if (test_num == 1) {
          mean_data <- na.omit(data[, as.numeric(md_data_selected_columns[1])])
          results <- mean.z.test.onesample(
            x = mean_data,
            known.population.variance = md_data_UI3^2,
            null.hypothesis.mean = md_data_UI2,
            alternative = one_or_two_md_data,
            conf.level = conf
          )
        }
        # One-sample t
        else if (test_num == 2) {
          req(md_data_UI2, one_or_two_md_data, conf)
          mean_data <- na.omit(data[, as.numeric(md_data_selected_columns[1])])
          results <- mean.t.test.onesample(
            x = mean_data,
            null.hypothesis.mean = md_data_UI2,
            alternative = one_or_two_md_data,
            conf.level = conf
          )
        }
        # One-sample var
        else if (test_num == 3) {
          var_data <- na.omit(data[, as.numeric(md_data_selected_columns[1])])
          results <- variance.test.onesample(
            g1 = var_data,
            null.hypothesis.variance = md_data_UI2^2,
            alternative = one_or_two_md_data,
            conf.level = conf
          )
        }
        # Two-sample z indep
        else if (test_num == 4) {
          req(md_data_selected_columns)
          mean_data_1 <- na.omit(data[, as.numeric(md_data_selected_columns[1])])
          mean_data_2 <- na.omit(data[, as.numeric(md_data_selected_columns[2])])
          
          mean_g1 <- mean(mean_data_1)
          var_g1 <- var(mean_data_1)
          n_g1 <- length(mean_data_1[!is.na(mean_data_1)])
          
          mean_g2 <- mean(mean_data_2)
          var_g2 <- var(mean_data_2)
          n_g2 <- length(mean_data_2[!is.na(mean_data_2)])
          
          results <- mean.z.test.twosample.independent.simple(
            sample.mean.g1 = mean_g1,
            variance.g1 = var_g1,
            sample.size.g1 = n_g1,
            sample.mean.g2 = mean_g2,
            variance.g2 = var_g2,
            sample.size.g2 = n_g2,
            null.hypothesis.difference = 0,
            alternative = one_or_two_md_data,
            conf.level = conf,
            g1.details = TRUE,
            g2.details = TRUE
          )
        }
        # Two-sample t indep
        else if (test_num == 5) {
          req(md_data_selected_columns)
          mean_data_1 <- na.omit(data[, as.numeric(md_data_selected_columns[1])])
          mean_data_2 <- na.omit(data[, as.numeric(md_data_selected_columns[2])])
          
          # Main t-test
          main_results <- t.test.twosample.independent(
            x1 = mean_data_1,
            x2 = mean_data_2,
            null.hypothesis.difference = 0,
            assume.equal.variances = t_type,
            alternative = one_or_two_md_data,
            conf.level = conf,
            var.test.conf.level = conf,
            var.test.details = TRUE,
            g1.details = TRUE,
            g2.details = TRUE
          )
          
          # Calculate Levene's test (ADA)
          g1_ada <- dispersion.ADA(mean_data_1)
          g2_ada <- dispersion.ADA(mean_data_2)
          levene <- t.test.twosample.independent(g1 = g1_ada, g2 = g2_ada)
          
          # Calculate ADMn-1 test
          g1_adm <- na.omit(dispersion.ADMn1(mean_data_1))
          g2_adm <- na.omit(dispersion.ADMn1(mean_data_2))
          adm_n1 <- t.test.twosample.independent(g1 = g1_adm, g2 = g2_adm, assume.equal.variances = FALSE)
          
          # Return list with main results and additional tests
          results <- list(
            main = main_results,
            levene = levene,
            adm_n1 = adm_n1
          )
        }
        # Two-sample t dep
        else if (test_num == 7) {
          type_dep_t <- inputs_vals$md_t_dep_type
          req(type_dep_t)
          mean_data_1 <- na.omit(data[, as.numeric(md_data_selected_columns[1])])
          mean_data_2 <- na.omit(data[, as.numeric(md_data_selected_columns[2])])
          
          if (length(mean_data_1) != length(mean_data_2)) {
            return("Need equal length groups for dependent test")
          }
          
          if (type_dep_t == 1) {
            req(md_data_UI2)
            results <- t.test.twosample.dependent(
              x1 = mean_data_1,
              x2 = mean_data_2,
              null.hypothesis.difference = md_data_UI2,
              alternative = one_or_two_md_data,
              conf.level = conf
            )
          } else if (type_dep_t == 2) {
            g1_mean <- mean(mean_data_1)
            g2_mean <- mean(mean_data_2)
            g1_sd <- sd(mean_data_1)
            g2_sd <- sd(mean_data_2)
            rho <- cor(mean_data_1, mean_data_2)
            
            results <- t.test.twosample.dependent.simple.meandiff(
              sample.mean.g1 = g1_mean,
              sample.mean.g2 = g2_mean,
              sample.variance.g1 = g1_sd^2,
              sample.variance.g2 = g2_sd^2,
              sample.size = length(mean_data_1),
              rho.estimate = rho,
              null.hypothesis.difference = 0,
              assume.equal.variances = t_type,
              alternative = one_or_two_md_data,
              conf.level = conf,
              var.test.conf.level = conf,
              var.test.details = TRUE,
              g1.details = TRUE,
              g2.details = TRUE
            )
          }
        }
      } else if (type == 2) {
        # Use Reference Column mode - factor-based selection
        req(ref_col, data_col, g1_num, g2_num, data)
        
        # Validate column indices
        req(ref_col > 0, ref_col <= ncol(data), 
            data_col > 0, data_col <= ncol(data),
            ref_col != data_col)
        
        # Extract groups based on factor values
        # Ensure data exists and is valid before extraction
        req(nrow(data) > 0, 
            length(which(data[[ref_col]] == g1_num)) > 0,
            length(which(data[[ref_col]] == g2_num)) > 0)
        
        g1 <- data[[data_col]][which(data[[ref_col]] == g1_num)]
        g2 <- data[[data_col]][which(data[[ref_col]] == g2_num)]
        
        # Ensure extracted data is numeric
        req(is.numeric(g1) || is.logical(g1), 
            is.numeric(g2) || is.logical(g2))
        
        # One-sample z
        if (test_num == 1) {
          mean_data <- na.omit(g1)
          req(length(mean_data) > 0, is.numeric(mean_data))
          results <- mean.z.test.onesample(
            x = mean_data,
            known.population.variance = md_data_UI3^2,
            null.hypothesis.mean = md_data_UI2,
            alternative = one_or_two_md_data,
            conf.level = conf
          )
        }
        # One-sample t
        else if (test_num == 2) {
          req(md_data_UI2, one_or_two_md_data, conf)
          mean_data <- na.omit(g1)
          req(length(mean_data) > 0, is.numeric(mean_data))
          results <- mean.t.test.onesample(
            x = mean_data,
            null.hypothesis.mean = md_data_UI2,
            alternative = one_or_two_md_data,
            conf.level = conf
          )
        }
        # One-sample var
        else if (test_num == 3) {
          var_data <- na.omit(g1)
          req(length(var_data) > 0, is.numeric(var_data))
          results <- variance.test.onesample(
            g1 = var_data,
            null.hypothesis.variance = md_data_UI2^2,
            alternative = one_or_two_md_data,
            conf.level = conf
          )
        }
        # Two-sample z indep
        else if (test_num == 4) {
          mean_data_1 <- na.omit(g1)
          mean_data_2 <- na.omit(g2)
          
          # Ensure we have valid numeric data before calculations
          req(length(mean_data_1) > 0, length(mean_data_2) > 0,
              is.numeric(mean_data_1), is.numeric(mean_data_2))
          
          mean_g1 <- mean(mean_data_1)
          var_g1 <- var(mean_data_1)
          n_g1 <- length(mean_data_1[!is.na(mean_data_1)])
          
          mean_g2 <- mean(mean_data_2)
          var_g2 <- var(mean_data_2)
          n_g2 <- length(mean_data_2[!is.na(mean_data_2)])
          
          results <- mean.z.test.twosample.independent.simple(
            sample.mean.g1 = mean_g1,
            variance.g1 = var_g1,
            sample.size.g1 = n_g1,
            sample.mean.g2 = mean_g2,
            variance.g2 = var_g2,
            sample.size.g2 = n_g2,
            null.hypothesis.difference = 0,
            alternative = one_or_two_md_data,
            conf.level = conf,
            g1.details = TRUE,
            g2.details = TRUE
          )
        }
        # Two-sample t indep
        else if (test_num == 5) {
          req(t_type)
          # For reference column mode, use formula interface if available
          # Otherwise use direct interface
          mean_data_1 <- na.omit(g1)
          mean_data_2 <- na.omit(g2)
          
          # Ensure we have valid numeric data before calculations
          req(length(mean_data_1) > 0, length(mean_data_2) > 0,
              is.numeric(mean_data_1), is.numeric(mean_data_2))
          
          # Main t-test
          if (exists("t.test.twosample.independent.fx")) {
            # Create data frame with factor and data
            temp_data <- data.frame(
              value = c(mean_data_1, mean_data_2),
              group = factor(c(rep(g1_num, length(mean_data_1)), 
                              rep(g2_num, length(mean_data_2))))
            )
            main_results <- t.test.twosample.independent.fx(
              fx = value ~ group,
              data = temp_data,
              assume.equal.variances = t_type,
              alternative = one_or_two_md_data,
              conf.level = conf,
              var.test.conf.level = conf,
              var.test.details = TRUE,
              g1.details = TRUE,
              g2.details = TRUE
            )
          } else {
            # Fall back to direct interface
            main_results <- t.test.twosample.independent(
              x1 = mean_data_1,
              x2 = mean_data_2,
              null.hypothesis.difference = 0,
              assume.equal.variances = t_type,
              alternative = one_or_two_md_data,
              conf.level = conf,
              var.test.conf.level = conf,
              var.test.details = TRUE,
              g1.details = TRUE,
              g2.details = TRUE
            )
          }
          
          # Calculate Levene's test (ADA)
          g1_ada <- dispersion.ADA(x = mean_data_1)
          g2_ada <- dispersion.ADA(x = mean_data_2)
          levene <- t.test.twosample.independent(g1 = g1_ada, g2 = g2_ada)
          
          # Calculate ADMn-1 test
          g1_adm_n1 <- na.omit(dispersion.ADMn1(x = mean_data_1))
          g2_adm_n1 <- na.omit(dispersion.ADMn1(x = mean_data_2))
          adm_n1 <- t.test.twosample.independent(g1 = g1_adm_n1, g2 = g2_adm_n1, assume.equal.variances = FALSE)
          
          # Return list with main results and additional tests
          results <- list(
            main = main_results,
            levene = levene,
            adm_n1 = adm_n1
          )
        }
        # Two-sample t dep
        else if (test_num == 7) {
          type_dep_t <- inputs_vals$md_t_dep_type
          req(type_dep_t)
          
          # Ensure we have valid numeric data
          req(length(g1) > 0, length(g2) > 0,
              is.numeric(g1) || is.logical(g1),
              is.numeric(g2) || is.logical(g2))
          
          if (length(g1) != length(g2)) {
            return("Need equal length groups for dependent test")
          }
          
          if (type_dep_t == 1) {
            req(md_data_UI2)
            results <- t.test.twosample.dependent(
              x1 = g1,
              x2 = g2,
              null.hypothesis.difference = md_data_UI2,
              alternative = one_or_two_md_data,
              conf.level = conf
            )
          } else if (type_dep_t == 2) {
            # Ensure data is numeric before calculations
            req(is.numeric(g1), is.numeric(g2))
            g1_mean <- mean(g1, na.rm = TRUE)
            g2_mean <- mean(g2, na.rm = TRUE)
            g1_sd <- sd(g1, na.rm = TRUE)
            g2_sd <- sd(g2, na.rm = TRUE)
            rho <- cor(g1, g2, use = "complete.obs")
            
            results <- t.test.twosample.dependent.simple.meandiff(
              sample.mean.g1 = g1_mean,
              sample.mean.g2 = g2_mean,
              sample.variance.g1 = g1_sd^2,
              sample.variance.g2 = g2_sd^2,
              sample.size = length(na.omit(g1)),
              rho.estimate = rho,
              null.hypothesis.difference = 0,
              assume.equal.variances = t_type,
              alternative = one_or_two_md_data,
              conf.level = conf,
              var.test.conf.level = conf,
              var.test.details = TRUE,
              g1.details = TRUE,
              g2.details = TRUE
            )
          }
        }
      }
      
      # Return results - rounding pattern:
      # - Main results: rounded in server (consistent with other tests)
      # - ADA/ADM tests (levene, adm_n1): NOT rounded here, rounded only during table rendering
      if (!is.null(results) && !is.character(results)) {
        if (is.list(results) && "main" %in% names(results)) {
          # Test 5 returns a list with main (rounded), levene and adm_n1 (unrounded)
          list(
            main = ro(results$main, R),
            levene = results$levene,  # Unrounded - rounded only during table rendering
            adm_n1 = results$adm_n1   # Unrounded - rounded only during table rendering
          )
        } else {
          # Other tests - round results (consistent with mean_out pattern)
          ro(results, R)
        }
      } else {
        results
      }
    })
    
    # =========================================================================
    # RETURN REACTIVE FUNCTIONS
    # =========================================================================
    list(
      mean_out = mean_out,
      m_d_data_out = m_d_data_out
    )
  })
}
