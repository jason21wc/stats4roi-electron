# Proportions Server Component
# This worker module handles calculations for Proportion tests
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
create_proportions_server <- function(id, filtered_data, input_values) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================================
    # INPUT EXTRACTION
    # =========================================================================
    inputs <- reactive({
      input_values()
    })
    
    # =========================================================================
    # PROP OUT REACTIVE (Enter Statistics mode)
    # =========================================================================
    prop_out <- reactive({
      inputs_vals <- inputs()
      req(inputs_vals)
      
      decimal_p <- inputs_vals$decimal_p
      alt_p <- inputs_vals$alt_p
      alt_p2 <- inputs_vals$alt_p2
      conf_p <- inputs_vals$conf_p
      one_or_two_p <- inputs_vals$one_or_two_p
      p_samp <- inputs_vals$p_samp
      n_samp_p <- inputs_vals$n_samp_p
      n_samp_p_2 <- inputs_vals$n_samp_p_2
      p0 <- inputs_vals$p0
      p2 <- inputs_vals$p2
      
      if (one_or_two_p == 1) {
        results <- proportion.test.onesample.exact.simple(
          sample.proportion = p_samp,
          sample.size = n_samp_p,
          null.hypothesis.proportion = p0,
          alternative = alt_p,
          conf.level = conf_p
        )
      } else if (one_or_two_p == 2) {
        results <- proportion.test.twosample.exact.simple(
          sample.proportion.g1 = p_samp,
          sample.size.g1 = n_samp_p,
          sample.proportion.g2 = p2,
          sample.size.g2 = n_samp_p_2,
          alternative = alt_p2,
          conf.level = conf_p
        )
      }
      
      # Return unrounded results - rounding happens at rendering stage
      results
    })
    
    # =========================================================================
    # PROP DATA OUT REACTIVE (Use Data mode)
    # =========================================================================
    prop_data_out <- reactive({
      inputs_vals <- inputs()
      data <- filtered_data()
      req(inputs_vals, data)
      
      R <- inputs_vals$decimal_bi_d
      conf <- inputs_vals$conf_bi_data
      data_type <- inputs_vals$data_type_bi
      bi_data_selected_columns <- inputs_vals$bi_data_selected_columns
      success1 <- inputs_vals$bi_data_success1
      success2 <- inputs_vals$bi_data_success2
      alt <- inputs_vals$alt_p_bi
      
      req(alt, R, conf, data_type, data, success1)
      
      results <- NULL
      
      if (data_type == 1) {
        # Data in Columns mode
        data_columns <- as.numeric(bi_data_selected_columns)
        if (!isTruthy(data_columns)) {
          return(NULL)
        }
        samples <- length(data_columns)
        if (samples > 2) {
          return(NULL)
        }
        
        if (samples == 1) {
          # One-sample proportion test
          x <- na.omit(data[[data_columns]])
          results <- proportion.test.onesample.exact(
            x = x,
            success.value = success1,
            null.hypothesis.proportion = inputs_vals$bi_test_data_ui2,
            alternative = alt,
            conf.level = conf
          )
        } else if (samples == 2) {
          # Two-sample proportion test
          req(success2)
          bi_data <- data[data_columns]
          ng1 <- length(na.omit(bi_data[[1]]))
          ng2 <- length(na.omit(bi_data[[2]]))
          count1 <- length(na.omit(bi_data[1][bi_data[1] == success1]))
          count2 <- length(na.omit(bi_data[2][bi_data[2] == success2]))
          pg1 <- count1 / ng1
          pg2 <- count2 / ng2
          
          results <- proportion.test.twosample.exact.simple(
            sample.proportion.g1 = pg1,
            sample.size.g1 = ng1,
            sample.proportion.g2 = pg2,
            sample.size.g2 = ng2,
            alternative = alt,
            conf.level = conf
          )
        }
      } else if (data_type == 2) {
        # Use Reference Column mode
        factor_col <- as.numeric(inputs_vals$data_choice_ref_bi)
        if (!isTruthy(factor_col)) {
          return(NULL)
        }
        factor_levels <- na.omit(unique(data[factor_col]))
        if (nrow(factor_levels) != 2) {
          return("Factor needs exactly two levels")
        }
        data_col <- as.numeric(inputs_vals$data_choice_data_bi)
        
        bi_data <- data.frame(ref = data[factor_col], data = data[data_col])
        names(bi_data) <- c("ref", "data")
        count1 <- length(na.omit(bi_data$data[which(bi_data$ref == factor_levels[1,] & bi_data$data == success1)]))
        count2 <- length(na.omit(bi_data$data[which(bi_data$ref == factor_levels[2,] & bi_data$data == success1)]))
        ng1 <- length(na.omit(bi_data$data[bi_data$ref == factor_levels[1,]]))
        ng2 <- length(na.omit(bi_data$data[bi_data$ref == factor_levels[2,]]))
        pg1 <- count1 / ng1
        pg2 <- count2 / ng2
        
        results <- proportion.test.twosample.exact.simple(
          sample.proportion.g1 = pg1,
          sample.size.g1 = ng1,
          sample.proportion.g2 = pg2,
          sample.size.g2 = ng2,
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
      prop_out = prop_out,
      prop_data_out = prop_data_out
    )
  })
}
