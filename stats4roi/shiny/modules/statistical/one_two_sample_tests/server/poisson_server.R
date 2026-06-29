# Poisson Server Component

library(shiny)
library(lolcat)

source("modules/config/global_config.R")
source("modules/statistical/one_two_sample_tests/ots_group_utils.R")

create_poisson_one_two_sample_server <- function(id, filtered_data, input_values) {
  moduleServer(id, function(input, output, session) {
    
    inputs <- reactive({
      input_values()
    })
    
    poi_out <- reactive({
      inputs_vals <- inputs()
      req(inputs_vals)
      
      conf_poi <- inputs_vals$conf_poi
      one_or_two_poi <- inputs_vals$one_or_two_poi
      poi_samp <- inputs_vals$poi_samp
      n_samp_poi <- inputs_vals$n_samp_poi
      n_samp_poi_2 <- inputs_vals$n_samp_poi_2
      poi0 <- inputs_vals$poi0
      poi2 <- inputs_vals$poi2
      
      req(poi_samp, n_samp_poi, conf_poi)
      
      if (one_or_two_poi == 1) {
        req(poi0, inputs_vals$alt_poi)
        poisson.test.onesample.simple(
          sample.count = poi_samp,
          sample.size = n_samp_poi,
          null.hypothesis.lambda = poi0,
          alternative = inputs_vals$alt_poi,
          conf.level = conf_poi
        )
      } else if (one_or_two_poi == 2) {
        req(poi2, n_samp_poi_2, inputs_vals$alt_poi_2)
        poisson.test.twosample.simple(
          sample.count.g1 = poi_samp,
          sample.size.g1 = n_samp_poi,
          sample.count.g2 = poi2,
          sample.size.g2 = n_samp_poi_2,
          alternative = inputs_vals$alt_poi_2,
          conf.level = conf_poi
        )
      }
    })
    
    poi_data_out <- reactive({
      inputs_vals <- inputs()
      data <- filtered_data()
      req(inputs_vals, data)
      
      conf <- inputs_vals$conf_poi_data
      data_type <- inputs_vals$data_type_poi
      alt <- inputs_vals$alt_poi_data
      
      req(alt, conf, data_type, data)
      
      two_col <- ots_column_mode_ready(
        col_g1 = inputs_vals$poi_data_col_g1,
        col_g2 = inputs_vals$poi_data_col_g2,
        two_sample = TRUE
      )
      one_col <- ots_column_mode_ready(col_g1 = inputs_vals$poi_data_col_g1, two_sample = FALSE)
      
      if (data_type == 1L || data_type == 1) {
        if (!one_col && !two_col) {
          return(NULL)
        }
        two_sample <- two_col
      } else {
        if (!ots_reference_mode_ready(
          inputs_vals$data_choice_ref_poi,
          inputs_vals$data_choice_data_poi,
          inputs_vals$data_choice_g1_poi,
          inputs_vals$data_choice_g2_poi,
          two_sample = TRUE
        )) {
          return(NULL)
        }
        two_sample <- TRUE
      }
      
      groups <- ots_groups_from_inputs(
        data,
        mode = data_type,
        col_g1 = inputs_vals$poi_data_col_g1,
        col_g2 = inputs_vals$poi_data_col_g2,
        ref_col = inputs_vals$data_choice_ref_poi,
        data_col = inputs_vals$data_choice_data_poi,
        level_g1 = inputs_vals$data_choice_g1_poi,
        level_g2 = inputs_vals$data_choice_g2_poi,
        two_sample = two_sample
      )
      if (is.null(groups) || is.null(groups$g1)) {
        return(NULL)
      }
      
      g1_x <- na.omit(groups$g1$x)
      count1 <- sum(g1_x)
      n1 <- length(g1_x)
      req(n1 > 0)
      
      if (!two_sample) {
        ui2 <- inputs_vals$poi_test_data_ui2
        req(ui2)
        poisson.test.onesample.simple(
          sample.count = count1,
          sample.size = n1,
          null.hypothesis.lambda = ui2,
          alternative = alt,
          conf.level = conf
        )
      } else {
        req(groups$g2)
        g2_x <- na.omit(groups$g2$x)
        count2 <- sum(g2_x)
        n2 <- length(g2_x)
        req(n2 > 0)
        poisson.test.twosample.simple(
          sample.count.g1 = count1,
          sample.size.g1 = n1,
          sample.count.g2 = count2,
          sample.size.g2 = n2,
          alternative = alt,
          conf.level = conf
        )
      }
    })
    
    list(
      poi_out = poi_out,
      poi_data_out = poi_data_out
    )
  })
}
