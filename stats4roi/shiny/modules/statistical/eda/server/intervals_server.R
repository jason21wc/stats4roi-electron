# =========================================================================
# INTERVALS SERVER MODULE
# =========================================================================
# Server logic for intervals analysis tab
# Part of EDA module - handles confidence and credible interval calculations

# Import required functions from lolcat package
ro <- round.object

# Define pop.sd function (from original app)
pop.sd <- function(x) {
  n <- length(x)
  sd(x) * sqrt((n - 1) / n)
}

create_intervals_server <- function(id, data_source, data_type_reactive, input_values) {
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
            tryCatch({
              t_out <- t.test.onesample(x = data[UI1][, int], conf.level = conf)
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
              samp_size <<- c(samp_size, NA)
              means <<- c(means, NA)
              stdev <<- c(stdev, NA)
              stdev_l <<- c(stdev_l, NA)
              stdev_u <<- c(stdev_u, NA)
            })
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
            tryCatch({
              subdata <- data.frame(na.omit(data[UI1][, i]))
              names(subdata) <- names(data)[UI1][i]
              
              # Create posteriors using bayesboot
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
              output$n[i] <<- NA
              output$Mean_L[i] <<- NA
              output$Mean[i] <<- NA
              output$Mean_U[i] <<- NA
              output$SD_L[i] <<- NA
              output$SD[i] <<- NA
              output$SD_U[i] <<- NA
            })
          }
        }
        
      } else if (data_type == 2) {
        # Factor analysis
        if (is.null(selections$eda_UI1) || is.null(selections$eda_UI2) || is.null(data_col)) {
          return(data.frame())
        }
        
        # Get column names for formula (following descriptives pattern)
        selected_data_cols <- as.numeric(selections$eda_UI2)
        dep_name <- colnames(data)[selected_data_cols[as.numeric(data_col)]]
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
              t_out <- t.test.onesample(subdata[[as.numeric(data_col)]], conf.level = conf)
              
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
              subdata <- data.frame(na.omit(subdata[[as.numeric(data_col)]]))
              
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
        
      } else {
        return(data.frame())
      }
      
      # Round output
      output <- ro(output, R)
      output
    })
    
    # =========================================================================
    # RETURN REACTIVE FUNCTIONS
    # =========================================================================
    
    return(list(
      intervals_data = intervals_data
    ))
  })
}