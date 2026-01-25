# Correlation and Association Server Component
# This worker module handles calculations for Correlation and Association tests
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
create_correlation_association_worker <- function(id, filtered_data, input_values) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================================
    # INPUT EXTRACTION
    # =========================================================================
    inputs <- reactive({
      input_values()
    })
    
    # =========================================================================
    # CORR STAT OUT REACTIVE (Enter Statistics mode)
    # =========================================================================
    corr_stat_out <- reactive({
      inputs_vals <- inputs()
      req(inputs_vals)
      
      conf_corr <- inputs_vals$conf_corr
      corr_tests <- inputs_vals$corr_tests
      corr_hyp <- inputs_vals$alt_corr
      stat_corr1 <- inputs_vals$stat_corr1
      stat_corr2 <- inputs_vals$stat_corr2
      stat_corr3 <- inputs_vals$stat_corr3
      stat_corr4 <- inputs_vals$stat_corr4
      n_corr <- inputs_vals$n_corr
      stat_corr6 <- inputs_vals$stat_corr6
      
      req(corr_tests, conf_corr, corr_hyp)
      
      results <- NULL
      
      # Test 1: Pearson r one-sample
      if (corr_tests == 1) {
        req(stat_corr1, stat_corr2, n_corr)
        if (abs(stat_corr1) > 1) return(NULL)
        results <- cor.pearson.r.onesample.simple(
          sample.r = stat_corr1,
          sample.size = n_corr,
          null.hypothesis.rho = stat_corr2,
          alternative = corr_hyp,
          conf.level = conf_corr
        )
      }
      # Test 2: Spearman rank
      else if (corr_tests == 2) {
        req(stat_corr1, n_corr)
        if (abs(stat_corr1) > 1) return(NULL)
        results <- cor.spearman.rank.simple(
          r_sp = stat_corr1,
          sample.size = n_corr,
          alternative = corr_hyp,
          conf.level = conf_corr
        )
      }
      # Test 3: Kendall's tau
      else if (corr_tests == 3) {
        req(stat_corr1, stat_corr2, stat_corr3, stat_corr4, n_corr)
        results <- cor.kendall.tau.simple(
          count.concordant = stat_corr1,
          count.discordant = stat_corr2,
          ties.x1 = stat_corr3,
          ties.x2 = stat_corr4,
          sample.size = n_corr,
          alternative = corr_hyp,
          conf.level = conf_corr
        )
      }
      # Test 4: Cramer's V/Phi
      else if (corr_tests == 4) {
        req(stat_corr1, stat_corr2, stat_corr3, stat_corr4)
        # Validate inputs are numeric and not NA
        if (any(is.na(c(stat_corr1, stat_corr2, stat_corr3, stat_corr4))) || 
            any(!is.numeric(c(stat_corr1, stat_corr2, stat_corr3, stat_corr4)))) {
          return(NULL)
        }
        M <- matrix(data = c(stat_corr1, stat_corr3, stat_corr2, stat_corr4), nrow = 2, ncol = 2)
        # Validate matrix has no NA values and all values are valid
        if (any(is.na(M)) || any(!is.finite(M))) {
          return(NULL)
        }
        results <- cor.cramer.phi(observed.frequencies = M)
      }
      # Test 5: Pearson r two-sample independent
      else if (corr_tests == 5) {
        req(stat_corr1, stat_corr2, stat_corr4)
        if (abs(stat_corr1) > 1 || abs(stat_corr2) > 1) return(NULL)
        results <- cor.pearson.r.twosample.independent.simple(
          sample.r.g1.g2 = stat_corr1,
          sample.size.g1.g2 = n_corr,
          sample.r.g3.g4 = stat_corr2,
          sample.size.g3.g4 = stat_corr4,
          alternative = corr_hyp,
          conf.level = conf_corr
        )
      }
      # Test 6: Goodman-Kruskal gamma two-sample independent
      else if (corr_tests == 6) {
        req(stat_corr1, stat_corr2, stat_corr3, stat_corr4)
        if (abs(stat_corr1) > 1 || abs(stat_corr2) > 1) return(NULL)
        results <- cor.goodman.kruskal.gamma.twosample.independent.simple(
          gamma.g1 = stat_corr1,
          se.est.gamma.g1 = stat_corr3,
          gamma.g2 = stat_corr2,
          se.est.gamma.g2 = stat_corr4,
          alternative = corr_hyp,
          conf.level = conf_corr
        )
      }
      # Test 7: Cohen's kappa two-sample independent
      else if (corr_tests == 7) {
        req(stat_corr1, stat_corr2, stat_corr3, stat_corr4)
        if (abs(stat_corr1) > 1 || abs(stat_corr2) > 1) return(NULL)
        results <- cor.cohen.kappa.twosample.independent.simple(
          kappa.g1 = stat_corr1,
          se.kappa.g1 = stat_corr3,
          kappa.g2 = stat_corr2,
          se.kappa.g2 = stat_corr4,
          alternative = corr_hyp,
          conf.level = conf_corr
        )
      }
      # Test 8: Pearson r two-sample dependent
      else if (corr_tests == 8) {
        req(stat_corr1, stat_corr2, stat_corr3, n_corr)
        if (abs(stat_corr1) > 1 || abs(stat_corr2) > 1 || abs(stat_corr3) > 1) return(NULL)
        results <- cor.pearson.r.twosample.dependent.simple(
          sample.r.g1.g3 = stat_corr1,
          sample.r.g2.g3 = stat_corr2,
          sample.r.g1.g2 = stat_corr3,
          sample.size = n_corr,
          alternative = corr_hyp,
          conf.level = conf_corr
        )
      }
      # Test 9: Cohen's kappa one-sample
      else if (corr_tests == 9) {
        req(stat_corr3, stat_corr4, n_corr, stat_corr6)
        observed <- matrix(data = c(stat_corr3, stat_corr4, n_corr, stat_corr6), nrow = 2, ncol = 2)
        results <- cor.cohen.kappa.onesample(
          observed.frequencies = observed,
          alternative = corr_hyp,
          conf.level = conf_corr
        )
      }
      
      # Return unrounded results - rounding happens at rendering stage
      results
    })
    
    # =========================================================================
    # CORR DATA OUT REACTIVE (Use Data mode)
    # =========================================================================
    corr_data_out <- reactive({
      inputs_vals <- inputs()
      data <- filtered_data()
      req(inputs_vals, data)
      
      UI1 <- inputs_vals$corr_UI1_data
      UI2 <- inputs_vals$corr_UI2_data
      UI3 <- inputs_vals$corr_UI3_data
      UI4 <- inputs_vals$corr_UI4_data
      UI5 <- inputs_vals$corr_UI5_data
      UI6 <- inputs_vals$corr_UI6_data
      alt <- inputs_vals$alt_corr_data
      conf <- inputs_vals$conf_corr_data
      corr_tests <- inputs_vals$corr_tests_data
      corr_data_1 <- inputs_vals$corr_data_selected_columns_1
      corr_data_2 <- inputs_vals$corr_data_selected_columns_2
      corr_data_3 <- inputs_vals$corr_data_selected_columns_3
      corr_data_4 <- inputs_vals$corr_data_selected_columns_4
      data_type <- inputs_vals$corr_data_type
      
      req(corr_tests, data, alt)
      
      # Extract data columns conditionally based on test requirements
      # Only extract columns that are needed for the specific test
      x1 <- NULL
      x2 <- NULL
      x3 <- NULL
      x4 <- NULL
      
      # Helper function to safely extract column
      extract_column <- function(col_idx, data) {
        if (is.null(col_idx) || col_idx == "" || is.na(col_idx)) {
          return(NULL)
        }
        col_num <- suppressWarnings(as.numeric(col_idx))
        if (is.na(col_num) || col_num < 1 || col_num > ncol(data)) {
          return(NULL)
        }
        return(data[, col_num])
      }
      
      x1 <- extract_column(corr_data_1, data)
      x2 <- extract_column(corr_data_2, data)
      x3 <- extract_column(corr_data_3, data)
      x4 <- extract_column(corr_data_4, data)
      
      results <- NULL
      
      # Test 1: Pearson r one-sample
      if (corr_tests == 1) {
        req(UI1, corr_data_1, corr_data_2, corr_data_1 != corr_data_2, x1, x2)
        
        results <- cor.pearson.r.onesample(
          x = x1,
          y = x2,
          null.hypothesis.rho = UI1,
          alternative = alt,
          conf.level = conf
        )
      }
      # Test 2: Spearman rank
      else if (corr_tests == 2) {
        req(corr_data_1, corr_data_2, corr_data_1 != corr_data_2, x1, x2)
        results <- cor.spearman.rank(
          x1 = x1,
          x2 = x2,
          conf.level = conf,
          alternative = alt
        )
      }
      # Test 3: Kendall's tau
      else if (corr_tests == 3) {
        req(corr_data_1, corr_data_2, corr_data_1 != corr_data_2, x1, x2)
        results <- cor.kendall.tau(
          x1 = x1,
          x2 = x2,
          alternative = alt,
          conf.level = conf
        )
      }
      # Test 4: Cramer's V (uses crosstab)
      else if (corr_tests == 4) {
        req(corr_data_1, corr_data_2)
        data_type <- inputs_vals$corr_data_type
        req(data_type)
        # Generate crosstab from data
        if (data_type == 1) {
          xtab <- transform.independent.format.to.xt(
            x_row = x1,
            x_col = x2,
            na.rm = TRUE
          )
        } else if (data_type == 2) {
          req(x3)
          xtab <- transform.independent.format.to.xt(
            x_row = x1,
            x_col = x2,
            weight = x3,
            na.rm = TRUE
          )
        }
        req(xtab)
        results <- cor.cramer.v(observed.frequencies = xtab)
      }
      # Test 5: Biserial correlation
      else if (corr_tests == 5) {
        req(corr_data_1, corr_data_2, corr_data_1 != corr_data_2, x1, x2)
        temp <- data.frame(x1, x2)
        temp <- na.omit(temp)
        results <- cor.biserial(
          discrete_var = temp$x1,
          continuous_var = temp$x2,
          alternative = alt,
          conf.level = conf
        )
      }
      # Test 6: Point-biserial correlation
      else if (corr_tests == 6) {
        req(corr_data_1, corr_data_2, corr_data_1 != corr_data_2, x1, x2)
        temp <- data.frame(x1, x2)
        temp <- na.omit(temp)
        results <- cor.point.biserial(
          discrete_var = temp$x1,
          continuous_var = temp$x2,
          alternative = alt,
          conf.level = conf
        )
      }
      # Test 7: Yule's Q (uses crosstab)
      else if (corr_tests == 7) {
        req(corr_data_1, corr_data_2)
        data_type <- inputs_vals$corr_data_type
        req(data_type)
        # Generate crosstab from data
        if (data_type == 1) {
          xtab <- transform.independent.format.to.xt(
            x_row = x1,
            x_col = x2,
            na.rm = TRUE
          )
        } else if (data_type == 2) {
          req(x3)
          xtab <- transform.independent.format.to.xt(
            x_row = x1,
            x_col = x2,
            weight = x3,
            na.rm = TRUE
          )
        }
        req(xtab)
        results <- cor.yule.q(x = xtab, alternative = alt)
      }
      # Test 8: Goodman-Kruskal gamma one-sample (uses crosstab)
      else if (corr_tests == 8) {
        req(UI1, corr_data_1, corr_data_2)
        data_type <- inputs_vals$corr_data_type
        req(data_type)
        # Generate crosstab from data
        if (data_type == 1) {
          xtab <- transform.independent.format.to.xt(
            x_row = x1,
            x_col = x2,
            na.rm = TRUE
          )
        } else if (data_type == 2) {
          req(x3)
          xtab <- transform.independent.format.to.xt(
            x_row = x1,
            x_col = x2,
            weight = x3,
            na.rm = TRUE
          )
        }
        req(xtab)
        results <- cor.goodman.kruskal.gamma.onesample(
          x = xtab,
          null.hypothesis.gamma = UI1,
          alternative = alt,
          conf.level = conf
        )
      }
      # Test 9: Tetrachoric correlation (uses crosstab)
      else if (corr_tests == 9) {
        req(corr_data_1, corr_data_2)
        data_type <- inputs_vals$corr_data_type
        req(data_type)
        # Generate crosstab from data
        if (data_type == 1) {
          xtab <- transform.independent.format.to.xt(
            x_row = x1,
            x_col = x2,
            na.rm = TRUE
          )
        } else if (data_type == 2) {
          req(x3)
          xtab <- transform.independent.format.to.xt(
            x_row = x1,
            x_col = x2,
            weight = x3,
            na.rm = TRUE
          )
        }
        req(xtab)
        results <- cor.tetrachoric(x = xtab, alternative = alt, conf.level = conf)
      }
      # Test 10: Cohen's kappa one-sample (uses crosstab)
      else if (corr_tests == 10) {
        req(corr_data_1, corr_data_2)
        data_type <- inputs_vals$corr_data_type
        req(data_type)
        # Generate crosstab from data
        if (data_type == 1) {
          xtab <- transform.independent.format.to.xt(
            x_row = x1,
            x_col = x2,
            na.rm = TRUE
          )
        } else if (data_type == 2) {
          req(x3)
          xtab <- transform.independent.format.to.xt(
            x_row = x1,
            x_col = x2,
            weight = x3,
            na.rm = TRUE
          )
        }
        req(xtab)
        results <- cor.cohen.kappa.onesample(xtab, alternative = alt, conf.level = conf)
      }
      # Test 11: Kendall's coefficient of concordance (uses crosstab)
      else if (corr_tests == 11) {
        req(corr_data_1, corr_data_2)
        data_type <- inputs_vals$corr_data_type
        req(data_type)
        # Generate crosstab from data - special handling for tabular format
        if (data_type == 1) {
          req(x3)
          xtab <- transform.independent.format.to.xt(
            x_row = x1,
            x_col = x2,
            weight = x3,
            na.rm = TRUE
          )
        } else if (data_type == 2) {
          # Tabular format - selected_judges is in corr_data_2 as space-separated string
          selected_judges <- as.numeric(unlist(strsplit(x = corr_data_2, split = "\\s+")))
          items <- as.numeric(corr_data_1)
          xtab <- data[, c(selected_judges)]
        }
        req(xtab)
        results <- cor.kendall.coefficient.concordance(x = xtab, alternative = alt, conf.level = conf)
      }
      # Test 12: J-index one-sample (uses crosstab)
      else if (corr_tests == 12) {
        req(corr_data_1, corr_data_2)
        data_type <- inputs_vals$corr_data_type
        req(data_type)
        # Generate crosstab from data
        if (data_type == 1) {
          xtab <- transform.independent.format.to.xt(
            x_row = x1,
            x_col = x2,
            na.rm = TRUE
          )
        } else if (data_type == 2) {
          req(x3)
          xtab <- transform.independent.format.to.xt(
            x_row = x1,
            x_col = x2,
            weight = x3,
            na.rm = TRUE
          )
        }
        req(xtab)
        results <- cor.j.index.onesample(x = xtab, conf.level = conf, alternative = alt)
      }
      # Test 13: Pearson r two-sample independent
      else if (corr_tests == 13) {
        req(corr_data_1, corr_data_2, corr_data_3, corr_data_4)
        set1 <- na.omit(data.frame(x1, x2))
        set2 <- na.omit(data.frame(x3, x4))
        results <- cor.pearson.r.twosample.independent(
          x1 = set1$x1,
          x2 = set1$x2,
          x3 = set2$x3,
          x4 = set2$x4,
          alternative = alt,
          conf.level = conf
        )
      }
      # Test 14: Goodman-Kruskal gamma two-sample independent (uses crosstab)
      else if (corr_tests == 14) {
        req(corr_data_1, corr_data_2, corr_data_3, corr_data_4)
        data_type <- inputs_vals$corr_data_type
        req(data_type)
        # Split data by group
        data1 <- as.data.frame(data[which(data[, as.numeric(corr_data_1)] == 1), ])
        data2 <- as.data.frame(data[which(data[, as.numeric(corr_data_1)] == 2), ])
        
        # Generate crosstabs for each group
        xtab1 <- transform.independent.format.to.xt(
          x_row = data1[, as.numeric(corr_data_2)],
          x_col = data1[, as.numeric(corr_data_3)],
          weight = data1[, as.numeric(corr_data_4)]
        )
        xtab2 <- transform.independent.format.to.xt(
          x_row = data2[, as.numeric(corr_data_2)],
          x_col = data2[, as.numeric(corr_data_3)],
          weight = data2[, as.numeric(corr_data_4)]
        )
        
        # Calculate gamma for each group
        gamma_out1 <- cor.goodman.kruskal.gamma.onesample(
          x = xtab1,
          null.hypothesis.gamma = 0,
          alternative = alt,
          conf.level = conf
        )
        gamma_out2 <- cor.goodman.kruskal.gamma.onesample(
          x = xtab2,
          null.hypothesis.gamma = 0,
          alternative = alt,
          conf.level = conf
        )
        
        gamma1 <- gamma_out1$estimate[1]
        gamma1_se <- gamma_out1$estimate[2]
        gamma2 <- gamma_out2$estimate[1]
        gamma2_se <- gamma_out2$estimate[2]
        
        results <- cor.goodman.kruskal.gamma.twosample.independent.simple(
          gamma.g1 = gamma1,
          se.est.gamma.g1 = gamma1_se,
          gamma.g2 = gamma2,
          se.est.gamma.g2 = gamma2_se,
          alternative = alt,
          conf.level = conf
        )
      }
      # Test 15: Cohen's kappa two-sample independent (uses crosstab)
      else if (corr_tests == 15) {
        req(corr_data_1, corr_data_2, corr_data_3)
        # Split data by group
        data1 <- as.data.frame(data[which(data[, as.numeric(corr_data_1)] == 1), ])
        data2 <- as.data.frame(data[which(data[, as.numeric(corr_data_1)] == 2), ])
        
        # Generate crosstabs for each group
        xtab1 <- transform.independent.format.to.xt(
          x_row = data1[, as.numeric(corr_data_2)],
          x_col = data1[, as.numeric(corr_data_3)]
        )
        xtab2 <- transform.independent.format.to.xt(
          x_row = data2[, as.numeric(corr_data_2)],
          x_col = data2[, as.numeric(corr_data_3)]
        )
        
        # Calculate kappa for each group
        kappa_out1 <- cor.cohen.kappa.onesample(xtab1, alternative = alt, conf.level = conf)
        kappa_out2 <- cor.cohen.kappa.onesample(xtab2, alternative = alt, conf.level = conf)
        
        kappa1 <- kappa_out1$estimate[1]
        kappa1_se <- kappa_out1$estimate[2]
        kappa2 <- kappa_out2$estimate[1]
        kappa2_se <- kappa_out2$estimate[2]
        
        results <- cor.cohen.kappa.twosample.independent.simple(
          kappa.g1 = kappa1,
          se.kappa.g1 = kappa1_se,
          kappa.g2 = kappa2,
          se.kappa.g2 = kappa2_se,
          alternative = alt,
          conf.level = conf
        )
      }
      # Test 16: J-index two-sample (uses crosstab)
      else if (corr_tests == 16) {
        req(corr_data_1, corr_data_2, corr_data_3)
        data_type <- inputs_vals$corr_data_type
        req(data_type)
        # Split data by group
        data1 <- as.data.frame(data[which(data[, as.numeric(corr_data_1)] == 1), ])
        data2 <- as.data.frame(data[which(data[, as.numeric(corr_data_1)] == 2), ])
        
        # Generate crosstabs for each group
        if (data_type == 1) {
          xtab1 <- transform.independent.format.to.xt(
            x_row = data1[, as.numeric(corr_data_2)],
            x_col = data1[, as.numeric(corr_data_3)]
          )
          xtab2 <- transform.independent.format.to.xt(
            x_row = data2[, as.numeric(corr_data_2)],
            x_col = data2[, as.numeric(corr_data_3)]
          )
        } else if (data_type == 2) {
          req(x4)
          xtab1 <- transform.independent.format.to.xt(
            x_row = data1[, as.numeric(corr_data_2)],
            x_col = data1[, as.numeric(corr_data_3)],
            weight = data1[, as.numeric(corr_data_4)]
          )
          xtab2 <- transform.independent.format.to.xt(
            x_row = data2[, as.numeric(corr_data_2)],
            x_col = data2[, as.numeric(corr_data_3)],
            weight = data2[, as.numeric(corr_data_4)]
          )
        }
        
        req(xtab1, xtab2)
        results <- cor.j.index.twosample(x1 = xtab1, x2 = xtab2, alternative = alt, conf.level = conf)
      }
      # Test 17: Pearson r two-sample dependent
      else if (corr_tests == 17) {
        req(corr_data_1, corr_data_2, corr_data_3)
        results <- cor.pearson.r.twosample.dependent(
          x1 = x1,
          x2 = x2,
          x3 = x3,
          alternative = alt,
          conf.level = conf
        )
      }
      
      # Return unrounded results - rounding happens at rendering stage
      results
    })
    
    # =========================================================================
    # RETURN REACTIVE FUNCTIONS
    # =========================================================================
    list(
      corr_stat_out = corr_stat_out,
      corr_data_out = corr_data_out
    )
  })
}
