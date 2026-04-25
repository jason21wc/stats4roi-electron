# Normality Tests Server Component
# This worker module follows the new architectural patterns and returns reactive functions only

# =============================================================================
# IMPORTS
# =============================================================================
library(shiny)
library(DT)
library(dplyr)

# Source global systems
source("modules/config/global_config.R")

# =============================================================================
# WORKER SERVER FUNCTION
# =============================================================================
create_normality_tests_server <- function(id, data_source, data_type_reactive, input_values) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================================
    # INPUT VALIDATION AND EXTRACTION
    # =========================================================================
    # Extract inputs with validation (following worker template pattern)
    decimals <- reactive({
      input_vals <- input_values()
      dec <- input_vals$decimals_desc
      if (is.null(dec) || is.na(dec)) return(5)
      dec
    })
    
    confidence <- reactive({
      input_vals <- input_values()
      conf <- input_vals$conf_eda
      if (is.null(conf) || is.na(conf)) return(0.95)
      conf
    })
    
    auto_norm <- reactive({
      input_vals <- input_values()
      auto <- input_vals$auto_norm
      if (is.null(auto)) return(TRUE)
      auto
    })
    
    norm_test <- reactive({
      input_vals <- input_values()
      test <- input_vals$norm_test
      if (is.null(test) || length(test) == 0) return(character(0))
      test
    })
    
    data_list_for_eda <- reactive({
      input_vals <- input_values()
      data_col <- input_vals$data_list_for_eda
      if (is.null(data_col)) return(NULL)
      data_col
    })
    
    # =========================================================================
    # DATA PROCESSING REACTIVE
    # =========================================================================
    processed_data <- reactive({
      # Get data from coordinator
      data <- data_source()
      req(data)
      
      # Get data type and selections from coordinator
      data_type <- data_type_reactive()
      selections <- input_values()
      
      # Validate data
      if (is.null(data) || nrow(data) == 0) {
        return(data.frame())
      }
      
      # Process data based on type
      if (data_type == 1) {
        # Column analysis
        if (is.null(selections$eda_UI1) || length(selections$eda_UI1) == 0) {
          return(data.frame())
        }
        selected_cols <- as.numeric(selections$eda_UI1)
        result <- data[, selected_cols, drop = FALSE]
      } else if (data_type == 2) {
        # Factor analysis - return full data for formula-based analysis
        if (is.null(selections$eda_UI1) || is.null(selections$eda_UI2)) {
          return(data.frame())
        }
        # For factor analysis, we need the full data because summary.continuous uses formula
        result <- data
      } else {
        return(data.frame())
      }
      
      as.data.frame(result)
    })
    
    # =========================================================================
    # BUSINESS LOGIC REACTIVE
    # =========================================================================
    normality_data <- reactive({
      data <- processed_data()  # Use processed data from coordinator
      data_type <- data_type_reactive()
      selections <- input_values()
      R <- decimals()
      conf <- confidence()
      auto <- auto_norm()
      test <- norm_test()
      data_col <- data_list_for_eda()
      
      if (is.null(data) || nrow(data) == 0) {
        return(list(data = data.frame(), p_columns = character(0), confidence = conf))
      }
      
      # Names already processed by coordinator
      
      # Process data based on type and mode
      
      tryCatch({
        if (data_type == 1) {
        # Column analysis - use original app approach
        # Get original data and column indices (following original app pattern)
        original_data <- data_source()
        names(original_data) <- make.names(names(original_data))
        
        # Convert UI1 to column indices (like original app)
        selected_cols <- as.numeric(selections$eda_UI1)
        norm_dat <- original_data[, selected_cols, drop = FALSE]
        
        # Process data for normality testing
        
        if (auto == TRUE) {
          # Auto mode - use intelligent test selection based on sample size
          # Process each column individually to avoid NA-related naming conflicts
          results_list <- list()
          for (i in 1:ncol(norm_dat)) {
            col_name <- names(norm_dat)[i]
            col_data <- norm_dat[, i, drop = FALSE]
            
            tryCatch({
              # Determine sample size for this column (excluding NAs)
              n_valid <- sum(!is.na(col_data))
              
              # Use intelligent test selection based on sample size
              if (n_valid <= 25) {
                # Small sample: use Anderson-Darling and Shapiro-Wilk
                col_result <- summary.all.variables(data = col_data, stat.sd = T, stat.ad.test = 2, stat.sw.test = 2)
              } else {
                # Large sample: use Skewness and Kurtosis tests
                col_result <- summary.all.variables(data = col_data, stat.sd = T, stat.skew.test = 2, stat.kurt.test = 2)
              }
              
              results_list[[i]] <- col_result
            }, error = function(e) {
              # Create a row with error info for this column
              results_list[[i]] <<- data.frame(
                Variable = col_name,
                n = sum(!is.na(col_data)),
                Error = paste("Could not process:", e$message)
              )
            })
          }
          
          # Combine results with dynamic column structure based on tests actually performed
          if (length(results_list) > 0) {
            # First pass: determine which test columns are actually used
            used_test_columns <- character(0)
            used_statistic_columns <- character(0)
            
            for (i in 1:length(results_list)) {
              result <- results_list[[i]]
              if (!is.null(result) && nrow(result) > 0) {
                # Check which test p-value columns are present in this result
                test_cols <- c("adtest.p", "swtest.p", "g3test.p", "g4test.p")
                present_test_cols <- test_cols[test_cols %in% names(result)]
                used_test_columns <- unique(c(used_test_columns, present_test_cols))
                
                # Check which test statistic columns are present in this result
                stat_cols <- c("adtest.AA", "swtest.W", "g3.skewness", "g4.kurtosis")
                present_stat_cols <- stat_cols[stat_cols %in% names(result)]
                used_statistic_columns <- unique(c(used_statistic_columns, present_stat_cols))
              }
            }
            
            # Create standardized results for each column with only relevant test columns
            standardized_results <- list()
            
            for (i in 1:length(results_list)) {
              col_name <- names(norm_dat)[i]
              result <- results_list[[i]]
              
              # Start with base columns
              std_row <- data.frame(
                Variable = col_name,
                n = if ("n" %in% names(result)) result$n[1] else sum(!is.na(norm_dat[,i])),
                missing = if ("missing" %in% names(result)) result$missing[1] else sum(is.na(norm_dat[,i])),
                mean = if ("mean" %in% names(result)) result$mean[1] else NA,
                sd = if ("sd" %in% names(result)) result$sd[1] else NA
              )
              
              # Add test columns in the specified order: statistic then p-value
              # Anderson-Darling: adtest.AA, adtest.p
              if ("adtest.AA" %in% used_statistic_columns) {
                std_row[["adtest.AA"]] <- if ("adtest.AA" %in% names(result)) result[["adtest.AA"]][1] else NA
              }
              if ("adtest.p" %in% used_test_columns) {
                std_row[["adtest.p"]] <- if ("adtest.p" %in% names(result)) result[["adtest.p"]][1] else NA
              }
              
              # Shapiro-Wilk: swtest.W, swtest.p
              if ("swtest.W" %in% used_statistic_columns) {
                std_row[["swtest.W"]] <- if ("swtest.W" %in% names(result)) result[["swtest.W"]][1] else NA
              }
              if ("swtest.p" %in% used_test_columns) {
                std_row[["swtest.p"]] <- if ("swtest.p" %in% names(result)) result[["swtest.p"]][1] else NA
              }
              
              # Skewness: g3.skewness, g3test.p
              if ("g3.skewness" %in% used_statistic_columns) {
                std_row[["g3.skewness"]] <- if ("g3.skewness" %in% names(result)) result[["g3.skewness"]][1] else NA
              }
              if ("g3test.p" %in% used_test_columns) {
                std_row[["g3test.p"]] <- if ("g3test.p" %in% names(result)) result[["g3test.p"]][1] else NA
              }
              
              # Kurtosis: g4.kurtosis, g4test.p
              if ("g4.kurtosis" %in% used_statistic_columns) {
                std_row[["g4.kurtosis"]] <- if ("g4.kurtosis" %in% names(result)) result[["g4.kurtosis"]][1] else NA
              }
              if ("g4test.p" %in% used_test_columns) {
                std_row[["g4test.p"]] <- if ("g4test.p" %in% names(result)) result[["g4test.p"]][1] else NA
              }
              
              standardized_results[[i]] <- std_row
            }
            
            # Combine the standardized results (all have the same structure now)
            output <- do.call(rbind, standardized_results)
          } else {
            output <- data.frame(Message = "No data to process")
          }
        } else {
          # Manual mode - use selected tests
          if (is.null(test) || length(test) == 0) {
            return(list(data = data.frame(Message = "Please select normality tests"), p_columns = character(0), confidence = conf))
          }
          
          # Build test selection string (following original app logic)
          test_sel <- paste(test, collapse = ",")
          
          # Only add =0 for tests that are NOT already selected
          if (!grepl("stat.ad.test=2", test_sel)) {
            test_sel <- c(test_sel, ",stat.ad.test=0")
            test_sel <- paste(test_sel, collapse = "")
          }
          if (!grepl("stat.sw.test=2", test_sel)) {
            test_sel <- c(test_sel, ",stat.sw.test=0")
            test_sel <- paste(test_sel, collapse = "")
          }
          if (!grepl("stat.skew.test=2", test_sel)) {
            test_sel <- c(test_sel, ",stat.skew.test=0")
            test_sel <- paste(test_sel, collapse = "")
          }
          if (!grepl("stat.kurt.test=2", test_sel)) {
            test_sel <- c(test_sel, ",stat.kurt.test=0")
            test_sel <- paste(test_sel, collapse = "")
          }
          
          # Process each column individually to avoid NA-related naming conflicts
          results_list <- list()
          for (i in 1:ncol(norm_dat)) {
            col_name <- names(norm_dat)[i]
            col_data <- norm_dat[, i, drop = FALSE]
            
            tryCatch({
              # Use original app's manual mode parameters for normality tests
              col_result <- eval(parse(text = paste("summary.all.variables(data = col_data, stat.sd = T,", test_sel, ")")))
              results_list[[i]] <- col_result
            }, error = function(e) {
              # Create a row with error info for this column
              results_list[[i]] <<- data.frame(
                Variable = col_name,
                n = sum(!is.na(col_data)),
                Error = paste("Could not process:", e$message)
              )
            })
          }
          
          # Combine results with dynamic column structure based on tests actually performed
          if (length(results_list) > 0) {
            # First pass: determine which test columns are actually used
            used_test_columns <- character(0)
            used_statistic_columns <- character(0)
            
            for (i in 1:length(results_list)) {
              result <- results_list[[i]]
              if (!is.null(result) && nrow(result) > 0) {
                # Check which test p-value columns are present in this result
                test_cols <- c("adtest.p", "swtest.p", "g3test.p", "g4test.p")
                present_test_cols <- test_cols[test_cols %in% names(result)]
                used_test_columns <- unique(c(used_test_columns, present_test_cols))
                
                # Check which test statistic columns are present in this result
                stat_cols <- c("adtest.AA", "swtest.W", "g3.skewness", "g4.kurtosis")
                present_stat_cols <- stat_cols[stat_cols %in% names(result)]
                used_statistic_columns <- unique(c(used_statistic_columns, present_stat_cols))
              }
            }
            
            # Create standardized results for each column with only relevant test columns
            standardized_results <- list()
            
            for (i in 1:length(results_list)) {
              col_name <- names(norm_dat)[i]
              result <- results_list[[i]]
              
              # Start with base columns
              std_row <- data.frame(
                Variable = col_name,
                n = if ("n" %in% names(result)) result$n[1] else sum(!is.na(norm_dat[,i])),
                missing = if ("missing" %in% names(result)) result$missing[1] else sum(is.na(norm_dat[,i])),
                mean = if ("mean" %in% names(result)) result$mean[1] else NA,
                sd = if ("sd" %in% names(result)) result$sd[1] else NA
              )
              
              # Add test columns in the specified order: statistic then p-value
              # Anderson-Darling: adtest.AA, adtest.p
              if ("adtest.AA" %in% used_statistic_columns) {
                std_row[["adtest.AA"]] <- if ("adtest.AA" %in% names(result)) result[["adtest.AA"]][1] else NA
              }
              if ("adtest.p" %in% used_test_columns) {
                std_row[["adtest.p"]] <- if ("adtest.p" %in% names(result)) result[["adtest.p"]][1] else NA
              }
              
              # Shapiro-Wilk: swtest.W, swtest.p
              if ("swtest.W" %in% used_statistic_columns) {
                std_row[["swtest.W"]] <- if ("swtest.W" %in% names(result)) result[["swtest.W"]][1] else NA
              }
              if ("swtest.p" %in% used_test_columns) {
                std_row[["swtest.p"]] <- if ("swtest.p" %in% names(result)) result[["swtest.p"]][1] else NA
              }
              
              # Skewness: g3.skewness, g3test.p
              if ("g3.skewness" %in% used_statistic_columns) {
                std_row[["g3.skewness"]] <- if ("g3.skewness" %in% names(result)) result[["g3.skewness"]][1] else NA
              }
              if ("g3test.p" %in% used_test_columns) {
                std_row[["g3test.p"]] <- if ("g3test.p" %in% names(result)) result[["g3test.p"]][1] else NA
              }
              
              # Kurtosis: g4.kurtosis, g4test.p
              if ("g4.kurtosis" %in% used_statistic_columns) {
                std_row[["g4.kurtosis"]] <- if ("g4.kurtosis" %in% names(result)) result[["g4.kurtosis"]][1] else NA
              }
              if ("g4test.p" %in% used_test_columns) {
                std_row[["g4test.p"]] <- if ("g4test.p" %in% names(result)) result[["g4test.p"]][1] else NA
              }
              
              standardized_results[[i]] <- std_row
            }
            
            # Combine the standardized results (all have the same structure now)
            output <- do.call(rbind, standardized_results)
          } else {
            output <- data.frame(Message = "No data to process")
          }
        }
        } else if (data_type == 2) {
          # Factor analysis - use summary.continuous with formula (following original app logic)
          if (is.null(data_col)) {
            return(list(
              data = data.frame(Message = "Please select a data column"),
              p_columns = character(0),
              confidence = conf
            ))
          }
          
          names(data) <- make.names(names(data))
          
          # Get column names for formula
          # data_col is the index within the selected data columns (UI2), need to map to original data
          selected_data_cols <- as.numeric(selections$eda_UI2)
          dep_name <- colnames(data)[selected_data_cols[as.numeric(data_col)]]
          indep <- colnames(data)[as.numeric(selections$eda_UI1)]
          indep_names <- paste(indep, collapse = "+")
          model_text <- formula(paste(dep_name, " ~ ", indep_names))
          
          if (auto == TRUE) {
            # Auto mode - use default statistics
            output <- summary.continuous(fx = model_text, data = data, stat.sd = T)
          } else {
            # Manual mode - use selected tests
            if (is.null(test) || length(test) == 0) {
              return(list(
                data = data.frame(Message = "Please select normality tests"),
                p_columns = character(0),
                confidence = conf
              ))
            }
            
            # Build test selection string
            test_sel <- paste(test, collapse = ",")
            if (!grepl("stat.ad.test=2", test_sel)) {
              test_sel <- c(test_sel, ",stat.ad.test=0")
              test_sel <- paste(test_sel, collapse = "")
            }
            if (!grepl("stat.sw.test=2", test_sel)) {
              test_sel <- c(test_sel, ",stat.sw.test=0")
              test_sel <- paste(test_sel, collapse = "")
            }
            if (!grepl("stat.skew.test=2", test_sel)) {
              test_sel <- c(test_sel, ",stat.skew.test=0")
              test_sel <- paste(test_sel, collapse = "")
            }
            if (!grepl("stat.kurt.test=2", test_sel)) {
              test_sel <- c(test_sel, ",stat.kurt.test=0")
              test_sel <- paste(test_sel, collapse = "")
            }
            
            output <- eval(parse(text = paste("summary.continuous(fx=model_text, data = data, stat.sd = T,", test_sel, ")")))
          }
        } else {
          output <- data.frame()
        }
        
        # Identify p-value columns for highlighting (for both auto and manual modes)
        # Use same pattern as original app: columns ending with .p
        p_columns <- names(output)[grepl("\\.p$", names(output))]
        
        result <- list(
          data = output,
          p_columns = p_columns,
          confidence = conf
        )
        }, error = function(e) {
          # Handle errors gracefully
          result <<- list(
            data = data.frame(Error = paste("Calculation error:", e$message)), 
            p_columns = character(0), 
            confidence = conf
          )
        })
      
      # Apply rounding using global function
      result$data <- ro(result$data, R)
      result
    })
    
    # =========================================================================
    # RETURN REACTIVE FUNCTIONS (coordinator will render these)
    # =========================================================================
    list(
      # Data reactives
      normality_data = normality_data,
      processed_data = processed_data,
      
      # Input reactives
      decimals = decimals,
      confidence = confidence,
      auto_norm = auto_norm,
      norm_test = norm_test
    )
  })
}

# =============================================================================
# ARCHITECTURAL PATTERNS USED
# =============================================================================
# 1. Worker Pattern: Contains only business logic, no UI rendering
# 2. Reactive Functions: Returns reactive functions for coordinator to render
# 3. Input Validation: Validates inputs with sensible defaults
# 4. Data Processing: Processes data based on coordinator inputs
# 5. Error Handling: Graceful handling of missing/invalid data
# 6. Template Compliance: Follows established patterns from working modules