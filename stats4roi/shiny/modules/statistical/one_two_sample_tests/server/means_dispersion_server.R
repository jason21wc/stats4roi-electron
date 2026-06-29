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

source("modules/statistical/one_two_sample_tests/ots_group_utils.R")



# =============================================================================

# WORKER SERVER FUNCTION

# =============================================================================

create_means_dispersion_server <- function(id, filtered_data, input_values) {

  moduleServer(id, function(input, output, session) {

    

    inputs <- reactive({

      input_values()

    })

    

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

      

      if (is.null(md_test_num) || is.null(UI1) || is.null(UI2) || is.null(UI3) ||

          is.null(conf) || is.null(md_alt)) {

        return(NULL)

      }

      

      results <- NULL

      

      if (md_test_num == 1) {

        results <- z.test.onesample.simple(

          sample.mean = UI1,

          known.population.variance = UI3^2,

          sample.size = UI5,

          null.hypothesis.mean = UI2,

          alternative = md_alt,

          conf.level = conf

        )

      } else if (md_test_num == 2) {

        results <- t.test.onesample.simple(

          sample.mean = UI1,

          sample.variance = UI3^2,

          sample.size = UI5,

          null.hypothesis.mean = UI2,

          alternative = md_alt,

          conf.level = conf

        )

      } else if (md_test_num == 3) {

        results <- variance.test.onesample.simple(

          sample.variance = UI1^2,

          sample.size = UI3,

          null.hypothesis.variance = UI2^2,

          alternative = md_alt,

          conf.level = conf

        )

      } else if (md_test_num == 4) {

        if (is.null(UI4) || is.null(UI5) || is.null(UI6)) return(NULL)

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

      } else if (md_test_num == 5) {

        if (is.null(UI4) || is.null(UI5) || is.null(UI6) || is.null(t_type)) return(NULL)

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

      } else if (md_test_num == 6) {

        results <- "Not Applicable"

      } else if (md_test_num == 7) {

        dep_t_type <- inputs_vals$md_t_dep_type_stat

        if (is.null(dep_t_type)) return(NULL)

        if (dep_t_type == 1) {

          if (is.null(UI5)) return(NULL)

          results <- t.test.twosample.dependent.simple.dbar(

            pair.differences.mean = UI1,

            pair.differences.variance = UI3^2,

            sample.size = UI5,

            null.hypothesis.difference = UI2,

            alternative = md_alt,

            conf.level = conf

          )

        } else if (dep_t_type == 2) {

          if (is.null(UI4) || is.null(UI5) || is.null(UI6)) return(NULL)

          if (!is.finite(UI6) || UI6 < -1 || UI6 > 1) return(NULL)

          results <- t.test.twosample.dependent.simple.meandiff(

            sample.mean.g1 = UI1,

            sample.variance.g1 = UI3^2,

            sample.size = UI5,

            sample.mean.g2 = UI2,

            sample.variance.g2 = UI4^2,

            rho.estimate = UI6,

            null.hypothesis.difference = 0,

            assume.equal.variances = "no",

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

    

    m_d_data_out <- reactive({

      inputs_vals <- inputs()

      data <- filtered_data()

      if (is.null(inputs_vals) || is.null(data)) {

        return(NULL)

      }

      

      R <- inputs_vals$decimal_m_d_d

      conf <- inputs_vals$conf_m_d_data

      md_data_UI2 <- inputs_vals$md_data_UI2

      md_data_UI3 <- inputs_vals$md_data_UI3

      one_or_two_md_data <- inputs_vals$one_or_two_md_data

      t_type <- inputs_vals$t_type_dat

      type <- inputs_vals$data_type_md

      test_num <- inputs_vals$data_md_test_num

      

      if (is.null(type) || is.null(test_num) || is.null(one_or_two_md_data) || is.null(conf)) {

        return(NULL)

      }

      if (test_num == 5L && is.null(t_type)) {

        return(NULL)

      }

      if (test_num == 7L && is.null(inputs_vals$md_t_dep_type)) {

        return(NULL)

      }

      

      two_sample <- test_num %in% c(4L, 5L, 7L)

      

      if (type == 1L || type == 1) {

        if (two_sample) {

          if (!ots_column_mode_ready(

            col_g1 = inputs_vals$md_data_col_g1,

            col_g2 = inputs_vals$md_data_col_g2,

            two_sample = TRUE

          )) {

            return(NULL)

          }

        } else if (!ots_column_mode_ready(col_g1 = inputs_vals$md_data_col_g1, two_sample = FALSE)) {

          return(NULL)

        }

      } else if (type == 2L || type == 2) {

        if (two_sample) {

          if (!ots_reference_mode_ready(

            inputs_vals$data_choice_ref,

            inputs_vals$data_choice_data,

            inputs_vals$data_choice_g1,

            inputs_vals$data_choice_g2,

            two_sample = TRUE

          )) {

            return(NULL)

          }

        } else if (!ots_reference_mode_ready(

          inputs_vals$data_choice_ref,

          inputs_vals$data_choice_data,

          inputs_vals$data_choice_g1,

          two_sample = FALSE

        )) {

          return(NULL)

        }

      }

      

      groups <- ots_groups_from_inputs(

        data,

        mode = type,

        col_g1 = inputs_vals$md_data_col_g1,

        col_g2 = inputs_vals$md_data_col_g2,

        ref_col = inputs_vals$data_choice_ref,

        data_col = inputs_vals$data_choice_data,

        level_g1 = inputs_vals$data_choice_g1,

        level_g2 = inputs_vals$data_choice_g2,

        two_sample = two_sample

      )

      if (is.null(groups) || is.null(groups$g1)) {

        return(NULL)

      }

      if (two_sample && is.null(groups$g2)) {

        return(NULL)

      }

      

      g1_vec <- groups$g1$x

      g2_vec <- if (two_sample) groups$g2$x else NULL

      

      results <- NULL

      

      if (test_num == 1) {

        mean_data <- na.omit(g1_vec)

        results <- mean.z.test.onesample(

          x = mean_data,

          known.population.variance = md_data_UI3^2,

          null.hypothesis.mean = md_data_UI2,

          alternative = one_or_two_md_data,

          conf.level = conf

        )

      } else if (test_num == 2) {

        req(md_data_UI2, one_or_two_md_data, conf)

        mean_data <- na.omit(g1_vec)

        results <- mean.t.test.onesample(

          x = mean_data,

          null.hypothesis.mean = md_data_UI2,

          alternative = one_or_two_md_data,

          conf.level = conf

        )

      } else if (test_num == 3) {

        var_data <- na.omit(g1_vec)

        results <- variance.test.onesample(

          g1 = var_data,

          null.hypothesis.variance = md_data_UI2^2,

          alternative = one_or_two_md_data,

          conf.level = conf

        )

      } else if (test_num == 4) {

        mean_data_1 <- na.omit(g1_vec)

        mean_data_2 <- na.omit(g2_vec)

        results <- mean.z.test.twosample.independent.simple(

          sample.mean.g1 = mean(mean_data_1),

          variance.g1 = var(mean_data_1),

          sample.size.g1 = length(mean_data_1[!is.na(mean_data_1)]),

          sample.mean.g2 = mean(mean_data_2),

          variance.g2 = var(mean_data_2),

          sample.size.g2 = length(mean_data_2[!is.na(mean_data_2)]),

          null.hypothesis.difference = 0,

          alternative = one_or_two_md_data,

          conf.level = conf,

          g1.details = TRUE,

          g2.details = TRUE

        )

      } else if (test_num == 5) {

        req(t_type, one_or_two_md_data, conf)

        mean_data_1 <- na.omit(g1_vec)

        mean_data_2 <- na.omit(g2_vec)

        req(length(mean_data_1) > 0, length(mean_data_2) > 0)

        

        main_results <- t.test.twosample.independent(

          g1 = mean_data_1,

          g2 = mean_data_2,

          null.hypothesis.difference = 0,

          assume.equal.variances = t_type,

          alternative = one_or_two_md_data,

          conf.level = conf,

          var.test.conf.level = conf,

          var.test.details = TRUE,

          g1.details = TRUE,

          g2.details = TRUE

        )

        

        g1_ada <- dispersion.ADA(mean_data_1)

        g2_ada <- dispersion.ADA(mean_data_2)

        levene <- t.test.twosample.independent(g1 = g1_ada, g2 = g2_ada)

        

        g1_adm <- na.omit(dispersion.ADMn1(mean_data_1))

        g2_adm <- na.omit(dispersion.ADMn1(mean_data_2))

        adm_n1 <- t.test.twosample.independent(g1 = g1_adm, g2 = g2_adm, assume.equal.variances = FALSE)

        

        results <- list(main = main_results, levene = levene, adm_n1 = adm_n1)

      } else if (test_num == 7) {

        type_dep_t <- inputs_vals$md_t_dep_type

        req(type_dep_t)

        mean_data_1 <- na.omit(g1_vec)

        mean_data_2 <- na.omit(g2_vec)

        

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

          results <- t.test.twosample.dependent.simple.meandiff(

            sample.mean.g1 = mean(mean_data_1),

            sample.mean.g2 = mean(mean_data_2),

            sample.variance.g1 = sd(mean_data_1)^2,

            sample.variance.g2 = sd(mean_data_2)^2,

            sample.size = length(mean_data_1),

            rho.estimate = cor(mean_data_1, mean_data_2),

            null.hypothesis.difference = 0,

            assume.equal.variances = "no",

            alternative = one_or_two_md_data,

            conf.level = conf,

            var.test.conf.level = conf,

            var.test.details = TRUE,

            g1.details = TRUE,

            g2.details = TRUE

          )

        }

      }

      

      final_out <- if (!is.null(results) && !is.character(results)) {

        if (is.list(results) && "main" %in% names(results)) {

          list(

            main = ro(results$main, R),

            levene = results$levene,

            adm_n1 = results$adm_n1

          )

        } else {

          ro(results, R)

        }

      } else {

        results

      }

      final_out

    })

    

    list(

      mean_out = mean_out,

      m_d_data_out = m_d_data_out

    )

  })

}


