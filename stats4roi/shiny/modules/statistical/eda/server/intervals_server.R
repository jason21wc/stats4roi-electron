# =========================================================================
# INTERVALS SERVER MODULE
# =========================================================================
# Server logic for intervals analysis tab
# Part of EDA module - handles confidence and credible interval calculations

source("modules/statistical/eda/utils/interval_plot_helpers.R")

# Import required functions from lolcat package
ro <- round.object

# Define pop.sd function (from original app)
pop.sd <- function(x) {
  n <- length(x)
  sd(x) * sqrt((n - 1) / n)
}

create_intervals_server <- function(id, data_source, data_type_reactive, input_values, reactive_color_palette) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================================
    # INPUT VALIDATION AND EXTRACTION
    # =========================================================================
    # Extract inputs with validation (following worker template pattern)
    conf_ci <- reactive({
      input_vals <- input_values()
      conf <- input_vals$conf_ci
      if (is.null(conf) || is.na(conf)) return(0.95)
      conf
    })
    
    decimals_ci <- reactive({
      input_vals <- input_values()
      dec <- input_vals$decimals_ci
      if (is.null(dec) || is.na(dec)) return(5)
      dec
    })
    
    interval_type <- reactive({
      input_vals <- input_values()
      int_type <- input_vals$interval_type
      if (is.null(int_type) || is.na(int_type)) return(1)
      as.numeric(int_type)
    })
    
    interval_b_type <- reactive({
      input_vals <- input_values()
      b_type <- input_vals$interval_b_type
      if (is.null(b_type) || is.na(b_type)) return("HDI")
      b_type
    })
    
    data_list_for_ci <- reactive({
      input_vals <- input_values()
      data_col <- input_vals$data_list_for_ci
      if (is.null(data_col) || is.na(data_col)) return(NULL)
      data_col
    })
    
    ci_info <- reactive({
      input_vals <- input_values()
      info <- input_vals$ci_info
      if (is.null(info) || is.na(info)) return(FALSE)
      info
    })

    ci_show_plot <- reactive({
      input_vals <- input_values()
      show <- input_vals$ci_show_plot
      if (is.null(show)) return(FALSE)
      isTRUE(show)
    })

    ci_plot_param <- reactive({
      input_vals <- input_values()
      param <- input_vals$ci_plot_param
      if (is.null(param) || !param %in% c("Mean", "SD")) return("Mean")
      param
    })

    ci_plot_width <- reactive({
      input_vals <- input_values()
      width_val <- input_vals$ci_plot_width
      if (is.null(width_val)) return(400)
      width_val
    })

    ci_plot_height <- reactive({
      input_vals <- input_values()
      height_val <- input_vals$ci_plot_height
      if (is.null(height_val)) return(400)
      height_val
    })

    ci_font_size <- reactive({
      input_vals <- input_values()
      size <- input_vals$ci_font_size
      if (is.null(size) || is.na(size)) return(11)
      as.numeric(size)
    })

    ci_plot_title <- reactive({
      input_vals <- input_values()
      title <- input_vals$ci_plot_title
      if (is.null(title) || title == "") return(NA_character_)
      title
    })

    ci_plot_xlab <- reactive({
      input_vals <- input_values()
      xlab <- input_vals$ci_plot_xlab
      if (is.null(xlab) || xlab == "") return(NA_character_)
      xlab
    })

    ci_plot_ylab <- reactive({
      input_vals <- input_values()
      ylab <- input_vals$ci_plot_ylab
      if (is.null(ylab) || ylab == "") return(NA_character_)
      ylab
    })

    color_palette <- reactive({
      reactive_color_palette()
    })
    
    # =========================================================================
    # INTERVALS CALCULATION
    # =========================================================================
    
    intervals_data <- reactive({
      data <- data_source()
      req(data)
      
      # Get data type and selections from coordinator
      data_type <- data_type_reactive()
      selections <- input_values()
      
      # Validate data
      if (is.null(data) || nrow(data) == 0) {
        return(data.frame())
      }
      
      names(data) <- make.names(names(data))
      
      # Get input values
      R <- decimals_ci()
      data_col <- data_list_for_ci()
      conf <- conf_ci()
      int_type <- interval_type()
      b_int_type <- interval_b_type()
      
      # Process data based on type
      if (data_type == 1) {
        # Column analysis
        if (is.null(selections$eda_UI1) || length(selections$eda_UI1) == 0) {
          return(data.frame())
        }
        UI1 <- as.numeric(selections$eda_UI1)
        
        if (int_type == 1) {
          # Confidence intervals
          len <- length(UI1)
          loop <- seq(from = 1, to = len)
          mean_ci_l <- NULL
          mean_ci_u <- NULL
          samp_size <- NULL
          means <- NULL
          stdev <- NULL
          stdev_l <- NULL
          stdev_u <- NULL
          
          for (int in loop) {
            x_raw <- data[UI1][, int]
            x_num <- eda_safe_numeric(x_raw)
            x_clean <- x_num[!is.na(x_num)]
            if (length(x_clean) >= 2) {
              tryCatch({
                t_out <- t.test.onesample(x = x_clean, conf.level = conf)
                mean_ci_l <- c(mean_ci_l, t_out[["conf.int"]][1])
                mean_ci_u <- c(mean_ci_u, t_out[["conf.int"]][2])
                df_ <- t_out[["estimate"]][["df"]]
                samp_size <- c(samp_size, 1 + df_)
                means <- c(means, t_out[["estimate"]][["sample.mean"]])
                s_ <- t_out[["estimate"]][["sd"]]
                stdev <- c(stdev, s_)
                stdev_l <- c(stdev_l, t_out[["estimate"]][["sd.lowerci"]])
                stdev_u <- c(stdev_u, t_out[["estimate"]][["sd.upperci"]])
              }, error = function(e) {
                warning("Error in confidence interval calculation for column ", int, ": ", e$message)
                mean_ci_l <<- c(mean_ci_l, NA)
                mean_ci_u <<- c(mean_ci_u, NA)
                samp_size <<- c(samp_size, length(x_clean))
                means <<- c(means, if (length(x_clean) == 1) x_clean[1] else NA)
                stdev <<- c(stdev, NA)
                stdev_l <<- c(stdev_l, NA)
                stdev_u <<- c(stdev_u, NA)
              })
            } else {
              mean_ci_l <- c(mean_ci_l, NA)
              mean_ci_u <- c(mean_ci_u, NA)
              samp_size <- c(samp_size, length(x_clean))
              means <- c(means, if (length(x_clean) == 1) x_clean[1] else NA)
              stdev <- c(stdev, NA)
              stdev_l <- c(stdev_l, NA)
              stdev_u <- c(stdev_u, NA)
            }
          }
          
          output <- data.frame(
            Column = names(data)[UI1],
            n = unname(samp_size),
            Mean_L = unname(mean_ci_l),
            Mean = unname(means),
            Mean_U = unname(mean_ci_u),
            SD_L = unname(stdev_l),
            SD = unname(stdev),
            SD_U = unname(stdev_u)
          )
          
          if (needs_pooled_all_row(length(UI1))) {
            pooled_x <- pool_numeric_vector(data[UI1])
            if (length(pooled_x) >= 2) {
              tryCatch({
                t_out <- t.test.onesample(x = pooled_x, conf.level = conf)
                all_row <- data.frame(
                  Column = POOLED_ALL_LABEL,
                  n = 1 + t_out[["estimate"]][["df"]],
                  Mean_L = t_out[["conf.int"]][1],
                  Mean = t_out[["estimate"]][["sample.mean"]],
                  Mean_U = t_out[["conf.int"]][2],
                  SD_L = t_out[["estimate"]][["sd.lowerci"]],
                  SD = t_out[["estimate"]][["sd"]],
                  SD_U = t_out[["estimate"]][["sd.upperci"]]
                )
                output <- prepend_rows_top(all_row, output)
              }, error = function(e) {
                warning("Error in pooled confidence interval: ", e$message)
              })
            }
          }
          
        } else {
          # Bayesian credible intervals
          num_col <- ncol(data[UI1])
          output <- data.frame(
            Column = names(data)[UI1],
            n = NA,
            Mean_L = NA,
            Mean = NA,
            Mean_U = NA,
            SD_L = NA,
            SD = NA,
            SD_U = NA
          )
          
          for (i in 1:num_col) {
            x_raw <- data[UI1][, i]
            x_num <- eda_safe_numeric(x_raw)
            x_clean <- x_num[!is.na(x_num)]
            if (length(x_clean) >= 2) {
              tryCatch({
                subdata <- data.frame(x = x_clean)
                output$n[i] <- nrow(subdata)
                boot_mean <- bayesboot(subdata[[1]], weighted.mean, use.weights = TRUE)
                boot_sd <- bayesboot(subdata[[1]], pop.sd)
                
                ci_mean <- ci(boot_mean, method = b_int_type, ci = conf)
                ci_std <- ci(boot_sd, method = b_int_type, ci = conf)
                
                output$Mean_L[i] <- ci_mean$CI_low
                output$Mean[i] <- mean(boot_mean$V1)
                output$Mean_U[i] <- ci_mean$CI_high
                output$SD_L[i] <- ci_std$CI_low
                output$SD[i] <- mean(boot_sd$V1)
                output$SD_U[i] <- ci_std$CI_high
              }, error = function(e) {
                warning("Error in credible interval calculation for column ", i, ": ", e$message)
                output$n[i] <<- length(x_clean)
                output$Mean_L[i] <<- NA
                output$Mean[i] <<- if (length(x_clean) == 1) x_clean[1] else NA
                output$Mean_U[i] <<- NA
                output$SD_L[i] <<- NA
                output$SD[i] <<- NA
                output$SD_U[i] <<- NA
              })
            } else {
              output$n[i] <- length(x_clean)
              output$Mean_L[i] <- NA
              output$Mean[i] <- if (length(x_clean) == 1) x_clean[1] else NA
              output$Mean_U[i] <- NA
              output$SD_L[i] <- NA
              output$SD[i] <- NA
              output$SD_U[i] <- NA
            }
          }
          
          if (needs_pooled_all_row(num_col)) {
            pooled_x <- pool_numeric_vector(data[UI1])
            if (length(pooled_x) >= 2) {
              tryCatch({
                subdata <- data.frame(x = pooled_x)
                output$n <- c(NA, output$n)
                output$Mean_L <- c(NA, output$Mean_L)
                output$Mean <- c(NA, output$Mean)
                output$Mean_U <- c(NA, output$Mean_U)
                output$SD_L <- c(NA, output$SD_L)
                output$SD <- c(NA, output$SD)
                output$SD_U <- c(NA, output$SD_U)
                output$Column <- c(POOLED_ALL_LABEL, output$Column)
                
                boot_mean <- bayesboot(subdata[[1]], weighted.mean, use.weights = TRUE)
                boot_sd <- bayesboot(subdata[[1]], pop.sd)
                ci_mean <- ci(boot_mean, method = b_int_type, ci = conf)
                ci_std <- ci(boot_sd, method = b_int_type, ci = conf)
                
                output$n[1] <- nrow(subdata)
                output$Mean_L[1] <- ci_mean$CI_low
                output$Mean[1] <- mean(boot_mean$V1)
                output$Mean_U[1] <- ci_mean$CI_high
                output$SD_L[1] <- ci_std$CI_low
                output$SD[1] <- mean(boot_sd$V1)
                output$SD_U[1] <- ci_std$CI_high
              }, error = function(e) {
                warning("Error in pooled credible interval: ", e$message)
              })
            }
          }
        }
        
      } else if (data_type == 2) {
        # Factor analysis
        if (is.null(selections$eda_UI1) || is.null(selections$eda_UI2) || is.null(data_col)) {
          return(data.frame())
        }
        
        dep_info <- resolve_factor_dependent_column(data, data_col, selections$eda_UI2)
        dep_name <- dep_info$dep_name
        dep_col_index <- dep_info$dep_col_index
        if (is.null(dep_name) || is.na(dep_col_index)) {
          return(data.frame())
        }
        indep <- colnames(data)[as.numeric(unlist(strsplit(x = as.character(selections$eda_UI1), split = "\\s+")))]
        indep_names <- paste(indep, collapse = "+")
        model_text <- formula(paste(dep_name, " ~ ", indep_names))
        combos <- unique(data[as.numeric(selections$eda_UI1)])
        output <- data.frame(combos, "n" = NA, "CI_low" = NA, "Mean" = NA, "CI_high" = NA, "SD_low" = NA, "SD" = NA, "SD_high" = NA)
        
        if (int_type == 1) {
          # Confidence intervals
          for (i in 1:nrow(combos)) {
            tryCatch({
              # Safer factor filtering using Reduce and Map instead of eval(parse)
              filter_condition <- Reduce(`&`, Map(function(col, val) data[[col]] == val, colnames(combos), combos[i, ]))
              subdata <- data[filter_condition, ]
              t_out <- t.test.onesample(eda_safe_numeric(subdata[[dep_name]]), conf.level = conf)
              
              output$CI_low[i] <- t_out[["conf.int"]][1]
              output$CI_high[i] <- t_out[["conf.int"]][2]
              output$n[i] <- t_out[["estimate"]][["df"]] + 1
              output$Mean[i] <- t_out[["estimate"]][["sample.mean"]]
              output$SD[i] <- t_out[["estimate"]][["sd"]]
              output$SD_low[i] <- t_out[["estimate"]][["sd.lowerci"]]
              output$SD_high[i] <- t_out[["estimate"]][["sd.upperci"]]
            }, error = function(e) {
              warning("Error in factor analysis confidence interval calculation for combination ", i, ": ", e$message)
              output$CI_low[i] <<- NA
              output$CI_high[i] <<- NA
              output$n[i] <<- NA
              output$Mean[i] <<- NA
              output$SD[i] <<- NA
              output$SD_low[i] <<- NA
              output$SD_high[i] <<- NA
            })
          }
        } else {
          # Bayesian credible intervals
          for (i in 1:nrow(combos)) {
            tryCatch({
              # Safer factor filtering using Reduce and Map instead of eval(parse)
              filter_condition <- Reduce(`&`, Map(function(col, val) data[[col]] == val, colnames(combos), combos[i, ]))
              subdata <- data[filter_condition, ]
              subdata <- data.frame(na.omit(eda_safe_numeric(subdata[[dep_name]])))
              
              if (nrow(subdata) > 0) {
                output$n[i] <- nrow(subdata)
                boot_mean <- bayesboot(subdata[[1]], weighted.mean, use.weights = TRUE)
                boot_sd <- bayesboot(subdata[[1]], pop.sd)
                
                ci_mean <- ci(boot_mean, method = b_int_type, ci = conf)
                ci_std <- ci(boot_sd, method = b_int_type, ci = conf)
                
                output$CI_low[i] <- ci_mean$CI_low
                output$Mean[i] <- mean(boot_mean$V1)
                output$CI_high[i] <- ci_mean$CI_high
                output$SD_low[i] <- ci_std$CI_low
                output$SD[i] <- mean(boot_sd$V1)
                output$SD_high[i] <- ci_std$CI_high
              } else {
                output$n[i] <- 0
                output$CI_low[i] <- NA
                output$Mean[i] <- NA
                output$CI_high[i] <- NA
                output$SD_low[i] <- NA
                output$SD[i] <- NA
                output$SD_high[i] <- NA
              }
            }, error = function(e) {
              warning("Error in factor analysis credible interval calculation for combination ", i, ": ", e$message)
              output$CI_low[i] <<- NA
              output$Mean[i] <<- NA
              output$CI_high[i] <<- NA
              output$n[i] <<- NA
              output$SD_low[i] <<- NA
              output$SD[i] <<- NA
              output$SD_high[i] <<- NA
            })
          }
        }
        
        if (needs_pooled_all_row(nrow(output))) {
          pooled_x <- na.omit(eda_safe_numeric(data[[dep_name]]))
          all_row <- output[1, , drop = FALSE]
          all_row[1, ] <- NA
          all_row <- label_factor_group_row(all_row, colnames(combos))
          
          if (int_type == 1) {
            tryCatch({
              t_out <- t.test.onesample(pooled_x, conf.level = conf)
              all_row$n <- 1 + t_out[["estimate"]][["df"]]
              all_row$CI_low <- t_out[["conf.int"]][1]
              all_row$Mean <- t_out[["estimate"]][["sample.mean"]]
              all_row$CI_high <- t_out[["conf.int"]][2]
              all_row$SD <- t_out[["estimate"]][["sd"]]
              all_row$SD_low <- t_out[["estimate"]][["sd.lowerci"]]
              all_row$SD_high <- t_out[["estimate"]][["sd.upperci"]]
              output <- prepend_rows_top(all_row, output)
            }, error = function(e) {
              warning("Error in pooled factor confidence interval: ", e$message)
            })
          } else {
            tryCatch({
              subdata <- data.frame(x = pooled_x)
              if (length(pooled_x) > 0) {
                boot_mean <- bayesboot(subdata[[1]], weighted.mean, use.weights = TRUE)
                boot_sd <- bayesboot(subdata[[1]], pop.sd)
                ci_mean <- ci(boot_mean, method = b_int_type, ci = conf)
                ci_std <- ci(boot_sd, method = b_int_type, ci = conf)
                all_row$n <- length(pooled_x)
                all_row$CI_low <- ci_mean$CI_low
                all_row$Mean <- mean(boot_mean$V1)
                all_row$CI_high <- ci_mean$CI_high
                all_row$SD_low <- ci_std$CI_low
                all_row$SD <- mean(boot_sd$V1)
                all_row$SD_high <- ci_std$CI_high
                output <- prepend_rows_top(all_row, output)
              }
            }, error = function(e) {
              warning("Error in pooled factor credible interval: ", e$message)
            })
          }
        }
        
      } else {
        return(data.frame())
      }
      
      # Round output
      output <- ro(output, R)
      output
    })

    ci_plot <- reactive({
      tryCatch({
        if (!ci_show_plot()) {
          return(NULL)
        }

        table_data <- intervals_data()
        data_type <- data_type_reactive()
        plot_df <- prepare_interval_plot_data(table_data, data_type, ci_plot_param())
        if (is.null(plot_df)) {
          return(NULL)
        }

        selections <- input_values()
        param <- ci_plot_param()
        conf <- conf_ci()
        int_type <- interval_type()
        b_int_type <- interval_b_type()
        palette <- color_palette()
        font_size <- ci_font_size()

        if (int_type == 1) {
          base_title <- paste0(
            100 * conf,
            "% Confidence Intervals - Assuming Normality, limits based on the t distribution"
          )
        } else {
          base_title <- paste0(
            "Bayesian Bootstrap ",
            100 * conf,
            "% Credible Intervals - ",
            b_int_type,
            " Method"
          )
        }

        plot_title <- if (isTruthy(ci_plot_title())) {
          ci_plot_title()
        } else {
          paste0(base_title, " (", param, ")")
        }
        title_info <- format_interval_plot_title(
          plot_title,
          ci_plot_width(),
          font_size = font_size
        )

        if (isTruthy(ci_plot_xlab())) {
          xlab <- ci_plot_xlab()
        } else if (data_type == 1) {
          xlab <- "Column"
        } else {
          indep <- colnames(data_source())[as.numeric(unlist(strsplit(
            x = as.character(selections$eda_UI1),
            split = "\\s+"
          )))]
          xlab <- paste(indep, collapse = ", ")
        }

        if (isTruthy(ci_plot_ylab())) {
          ylab <- ci_plot_ylab()
        } else if (data_type == 2 && !is.null(data_list_for_ci())) {
          data <- data_source()
          dep_info <- resolve_factor_dependent_column(
            data,
            data_list_for_ci(),
            selections$eda_UI2
          )
          ylab <- if (!is.null(dep_info$dep_name)) dep_info$dep_name else param
        } else {
          ylab <- param
        }

        ggplot(plot_df, aes(x = x_pos, y = estimate)) +
          geom_linerange(
            aes(ymin = ci_low, ymax = ci_high),
            linewidth = 1.2,
            color = palette[2]
          ) +
          geom_point(size = 5, color = palette[1]) +
          scale_x_continuous(
            breaks = plot_df$x_pos,
            labels = as.character(plot_df$group),
            limits = c(min(plot_df$x_pos) - 0.5, max(plot_df$x_pos) + 0.5)
          ) +
          theme_gray(base_size = font_size) +
          theme(
            plot.title.position = "plot",
            plot.title = element_text(hjust = 0.5, lineheight = 1.05),
            plot.margin = margin(
              t = title_info$top_margin_pt,
              r = 10,
              b = 10,
              l = 10,
              unit = "pt"
            ),
            axis.text.x = element_text(angle = 45, hjust = 1)
          ) +
          labs(title = title_info$title, x = xlab, y = ylab)
      }, error = function(e) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message), size = 5) +
          theme_void()
      })
    })

    ci_plot_hover_data <- reactive({
      if (!ci_show_plot()) {
        return(NULL)
      }

      table_data <- intervals_data()
      data_type <- data_type_reactive()
      plot_df <- prepare_interval_plot_data(table_data, data_type, ci_plot_param())
      prepare_interval_plot_hover_data(plot_df, ci_plot_param())
    })
    
    # =========================================================================
    # RETURN REACTIVE FUNCTIONS
    # =========================================================================
    
    return(list(
      intervals_data = intervals_data,
      ci_plot = ci_plot,
      ci_plot_width = ci_plot_width,
      ci_plot_height = ci_plot_height,
      ci_plot_hover_data = ci_plot_hover_data
    ))
  })
}