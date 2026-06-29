# Proportions Server Component

library(shiny)
library(lolcat)

source("modules/config/global_config.R")
source("modules/statistical/one_two_sample_tests/ots_group_utils.R")

create_proportions_server <- function(id, filtered_data, input_values) {
  moduleServer(id, function(input, output, session) {
    
    inputs <- reactive({
      input_values()
    })
    
    prop_out <- reactive({
      inputs_vals <- inputs()
      req(inputs_vals)
      
      alt_p <- inputs_vals$alt_p
      alt_p2 <- inputs_vals$alt_p2
      conf_p <- inputs_vals$conf_p
      one_or_two_p <- inputs_vals$one_or_two_p
      p_samp <- inputs_vals$p_samp
      n_samp_p <- inputs_vals$n_samp_p
      n_samp_p_2 <- inputs_vals$n_samp_p_2
      p0 <- inputs_vals$p0
      p2 <- inputs_vals$p2
      prop_enter_counts <- isTRUE(inputs_vals$prop_enter_counts)
      
      req(one_or_two_p %in% c(1, 2))
      
      if (prop_enter_counts) {
        validate(
          need(is.numeric(n_samp_p) && is.finite(n_samp_p) && n_samp_p >= 1,
               "Sample size n1 must be a finite number \u2265 1."),
          need(is.numeric(p_samp) && is.finite(p_samp),
               "np1 must be a finite number."),
          need(p_samp >= 0 && p_samp <= n_samp_p,
               "Counts: need 0 \u2264 np1 \u2264 n1."),
          need(abs(p_samp - round(p_samp)) < 1e-9,
               "Counts: np1 must be a whole number.")
        )
        np1 <- as.integer(round(p_samp))
        if (one_or_two_p == 2) {
          validate(
            need(is.numeric(n_samp_p_2) && is.finite(n_samp_p_2) && n_samp_p_2 >= 1,
                 "Sample size n2 must be a finite number \u2265 1."),
            need(is.numeric(p2) && is.finite(p2),
                 "np2 must be a finite number."),
            need(p2 >= 0 && p2 <= n_samp_p_2,
                 "Counts: need 0 \u2264 np2 \u2264 n2."),
            need(abs(p2 - round(p2)) < 1e-9,
                 "Counts: np2 must be a whole number.")
          )
          np2 <- as.integer(round(p2))
        }
      }
      
      if (one_or_two_p == 1) {
        if (prop_enter_counts) {
          results <- proportion.test.onesample.exact.simple(
            np = np1,
            sample.size = n_samp_p,
            sample.proportion = NA,
            null.hypothesis.proportion = p0,
            alternative = alt_p,
            conf.level = conf_p
          )
        } else {
          results <- proportion.test.onesample.exact.simple(
            sample.proportion = p_samp,
            sample.size = n_samp_p,
            null.hypothesis.proportion = p0,
            alternative = alt_p,
            conf.level = conf_p
          )
        }
      } else if (one_or_two_p == 2) {
        if (prop_enter_counts) {
          results <- proportion.test.twosample.exact.simple(
            np.g1 = np1,
            sample.size.g1 = n_samp_p,
            sample.proportion.g1 = NA,
            np.g2 = np2,
            sample.size.g2 = n_samp_p_2,
            sample.proportion.g2 = NA,
            alternative = alt_p2,
            conf.level = conf_p
          )
        } else {
          results <- proportion.test.twosample.exact.simple(
            sample.proportion.g1 = p_samp,
            sample.size.g1 = n_samp_p,
            sample.proportion.g2 = p2,
            sample.size.g2 = n_samp_p_2,
            alternative = alt_p2,
            conf.level = conf_p
          )
        }
      }
      
      results
    })
    
    prop_data_out <- reactive({
      inputs_vals <- inputs()
      data <- filtered_data()
      req(inputs_vals, data)
      
      conf <- inputs_vals$conf_bi_data
      data_type <- inputs_vals$data_type_bi
      success1 <- inputs_vals$bi_data_success1
      success2 <- inputs_vals$bi_data_success2
      alt <- inputs_vals$alt_p_bi
      
      req(alt, conf, data_type, data, success1)
      
      two_col <- ots_column_mode_ready(
        col_g1 = inputs_vals$bi_data_col_g1,
        col_g2 = inputs_vals$bi_data_col_g2,
        two_sample = TRUE
      )
      one_col <- ots_column_mode_ready(col_g1 = inputs_vals$bi_data_col_g1, two_sample = FALSE)
      
      if (data_type == 1L || data_type == 1) {
        if (!one_col && !two_col) {
          return(NULL)
        }
        two_sample <- two_col
      } else {
        if (!ots_reference_mode_ready(
          inputs_vals$data_choice_ref_bi,
          inputs_vals$data_choice_data_bi,
          inputs_vals$data_choice_g1_bi,
          inputs_vals$data_choice_g2_bi,
          two_sample = TRUE
        )) {
          return(NULL)
        }
        two_sample <- TRUE
      }
      
      groups <- ots_groups_from_inputs(
        data,
        mode = data_type,
        col_g1 = inputs_vals$bi_data_col_g1,
        col_g2 = inputs_vals$bi_data_col_g2,
        ref_col = inputs_vals$data_choice_ref_bi,
        data_col = inputs_vals$data_choice_data_bi,
        level_g1 = inputs_vals$data_choice_g1_bi,
        level_g2 = inputs_vals$data_choice_g2_bi,
        two_sample = two_sample
      )
      if (is.null(groups) || is.null(groups$g1)) {
        return(NULL)
      }
      
      if (!two_sample) {
        x <- na.omit(groups$g1$x)
        proportion.test.onesample.exact(
          x = x,
          success.value = success1,
          null.hypothesis.proportion = inputs_vals$bi_test_data_ui2,
          alternative = alt,
          conf.level = conf
        )
      } else {
        req(success2, groups$g2)
        g1_x <- na.omit(groups$g1$x)
        g2_x <- na.omit(groups$g2$x)
        ng1 <- length(g1_x)
        ng2 <- length(g2_x)
        if (ng1 < 1L || ng2 < 1L) {
          return(NULL)
        }
        count1 <- sum(g1_x == success1, na.rm = TRUE)
        count2 <- sum(g2_x == success2, na.rm = TRUE)
        proportion.test.twosample.exact.simple(
          sample.proportion.g1 = count1 / ng1,
          sample.size.g1 = ng1,
          sample.proportion.g2 = count2 / ng2,
          sample.size.g2 = ng2,
          alternative = alt,
          conf.level = conf
        )
      }
    })
    
    list(
      prop_out = prop_out,
      prop_data_out = prop_data_out
    )
  })
}
