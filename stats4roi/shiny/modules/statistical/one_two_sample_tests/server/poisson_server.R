# Poisson Server Component
# This worker module handles calculations for Poisson rate tests
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
create_poisson_one_two_sample_server <- function(id, filtered_data, input_values) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================================
    # INPUT EXTRACTION
    # =========================================================================
    inputs <- reactive({
      input_values()
    })
    
    # =========================================================================
    # POI OUT REACTIVE (Enter Statistics mode)
    # =========================================================================
    poi_out <- reactive({
      inputs_vals <- inputs()
      req(inputs_vals)
      
      decimal_poi <- inputs_vals$decimal_poi
      alt_poi <- inputs_vals$alt_poi
      alt_poi_2 <- inputs_vals$alt_poi_2
      conf_poi <- inputs_vals$conf_poi
      one_or_two_poi <- inputs_vals$one_or_two_poi
      poi_samp <- inputs_vals$poi_samp
      n_samp_poi <- inputs_vals$n_samp_poi
      n_samp_poi_2 <- inputs_vals$n_samp_poi_2
      poi0 <- inputs_vals$poi0
      poi2 <- inputs_vals$poi2
      
      req(poi_samp, n_samp_poi, conf_poi)
      
      if (one_or_two_poi == 1) {
        req(poi0, alt_poi)
        results <- poisson.test.onesample.simple(
          sample.count = poi_samp,
          sample.size = n_samp_poi,
          null.hypothesis.lambda = poi0,
          alternative = alt_poi,
          conf.level = conf_poi
        )
      } else if (one_or_two_poi == 2) {
        req(poi2, n_samp_poi_2, alt_poi_2)
        results <- poisson.test.twosample.simple(
          sample.count.g1 = poi_samp,
          sample.size.g1 = n_samp_poi,
          sample.count.g2 = poi2,
          sample.size.g2 = n_samp_poi_2,
          alternative = alt_poi_2,
          conf.level = conf_poi
        )
      }
      
      # Return unrounded results - rounding happens at rendering stage
      results
    })
    
    # =========================================================================
    # POI DATA OUT REACTIVE (Use Data mode)
    # =========================================================================
    poi_data_out <- reactive({
      inputs_vals <- inputs()
      data <- filtered_data()
      req(inputs_vals, data)
      
      R <- inputs_vals$decimal_poi_d
      conf <- inputs_vals$conf_poi_data
      data_type <- inputs_vals$data_type_poi
      poi_data_selected_columns <- inputs_vals$poi_data_selected_columns
      alt <- inputs_vals$alt_poi_data
      
      req(alt, R, conf, data_type, data)
      
      results <- NULL
      
      if (data_type == 1) {
        # Data in Columns mode
        sel_col <- as.numeric(poi_data_selected_columns)
        if (!isTruthy(sel_col)) {
          return(NULL)
        }
        req(all(sel_col > 0), all(sel_col <= ncol(data)))
        samples <- length(sel_col)
        if (samples > 2) {
          return(NULL)
        }
        
        poi_data <- data[sel_col]
        count1 <- sum(na.omit(poi_data[[1]]))
        n1 <- nrow(na.omit(poi_data[1]))
        req(n1 > 0)
        
        if (samples == 1) {
          # One-sample Poisson test
          ui2 <- inputs_vals$poi_test_data_ui2
          req(ui2)
          results <- poisson.test.onesample.simple(
            sample.count = count1,
            sample.size = n1,
            null.hypothesis.lambda = ui2,
            alternative = alt,
            conf.level = conf
          )
        } else if (samples == 2) {
          # Two-sample Poisson test
          count2 <- sum(na.omit(poi_data[[2]]))
          n2 <- nrow(na.omit(poi_data[2]))
          req(n2 > 0)
          
          results <- poisson.test.twosample.simple(
            sample.count.g1 = count1,
            sample.size.g1 = n1,
            sample.count.g2 = count2,
            sample.size.g2 = n2,
            alternative = alt,
            conf.level = conf
          )
        }
      } else if (data_type == 2) {
        # Use Reference Column mode
        ref_col <- as.numeric(inputs_vals$data_choice_ref_poi)
        data_col <- as.numeric(inputs_vals$data_choice_data_poi)
        g1_id <- inputs_vals$data_choice_g1_poi
        g2_id <- inputs_vals$data_choice_g2_poi
        req(data_col, ref_col, g1_id, g2_id,
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
        names(poi_data1) <- c("Group", "Count")
        names(poi_data2) <- c("Group", "Count")
        
        count1 <- sum(na.omit(poi_data1$Count))
        n1 <- length(na.omit(poi_data1$Count))
        count2 <- sum(na.omit(poi_data2$Count))
        n2 <- length(na.omit(poi_data2$Count))
        req(n1 > 0, n2 > 0)
        
        results <- poisson.test.twosample.simple(
          sample.count.g1 = count1,
          sample.size.g1 = n1,
          sample.count.g2 = count2,
          sample.size.g2 = n2,
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
      poi_out = poi_out,
      poi_data_out = poi_data_out
    )
  })
}
