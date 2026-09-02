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
          col_specs <- normality_column_specs_list(norm_dat)
          results_list <- list()
          for (k in seq_along(col_specs)) {
            spec <- col_specs[[k]]
            col_name <- spec$name
            col_data <- spec$data
            
            tryCatch({
              x_num <- suppressWarnings(as.numeric(spec$x))
              n_valid <- sum(!is.na(x_num))
              if (n_valid == 0) {
                col_result <- data.frame(
                  Variable = col_name,
                  n = 0L,
                  missing = length(spec$x),
                  stringsAsFactors = FALSE
                )
              } else if (n_valid <= 25) {
                clean_df <- data.frame(setNames(list(x_num[!is.na(x_num)]), col_name), stringsAsFactors = FALSE)
                col_result <- summary.all.variables(
                  data = clean_df,
                  stat.sd = TRUE,
                  stat.ad.test = 2,
                  stat.sw.test = 2
                )
              } else {
                clean_df <- data.frame(setNames(list(x_num[!is.na(x_num)]), col_name), stringsAsFactors = FALSE)
                col_result <- summary.all.variables(
                  data = clean_df,
                  stat.sd = TRUE,
                  stat.skew.test = 2,
                  stat.kurt.test = 2
                )
              }
              results_list[[k]] <- col_result
            }, error = function(e) {
              results_list[[k]] <<- data.frame(
                Variable = col_name,
                n = sum(!is.na(suppressWarnings(as.numeric(spec$x)))),
                Error = paste("Could not process:", e$message)
              )
            })
          }
          
          output <- normality_standardize_column_results(results_list, col_specs)
        } else {
          # Manual mode - use selected tests
          if (is.null(test) || length(test) == 0) {
            return(list(data = data.frame(Message = "Please select normality tests"), p_columns = character(0), confidence = conf))
          }
          
          # Build test selection string (following original app logic)
          test_sel <- build_normality_test_selection(test)
          
          col_specs <- normality_column_specs_list(norm_dat)
          results_list <- list()
          for (k in seq_along(col_specs)) {
            spec <- col_specs[[k]]
            col_name <- spec$name
            col_data <- spec$data
            
            tryCatch({
              x_num <- suppressWarnings(as.numeric(spec$x))
              n_valid <- sum(!is.na(x_num))
              if (n_valid == 0) {
                col_result <- data.frame(
                  Variable = col_name,
                  n = 0L,
                  missing = length(spec$x),
                  stringsAsFactors = FALSE
                )
              } else {
                clean_df <- data.frame(setNames(list(x_num[!is.na(x_num)]), col_name), stringsAsFactors = FALSE)
                col_result <- eval(parse(text = paste(
                  "summary.all.variables(data = clean_df, stat.sd = T,",
                  test_sel,
                  ")"
                )))
              }
              results_list[[k]] <- col_result
            }, error = function(e) {
              results_list[[k]] <<- data.frame(
                Variable = col_name,
                n = sum(!is.na(suppressWarnings(as.numeric(spec$x)))),
                Error = paste("Could not process:", e$message)
              )
            })
          }
          
          output <- normality_standardize_column_results(results_list, col_specs)
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
          
          dep_name_m <- make.names(dep_name)
          dep_x <- suppressWarnings(as.numeric(data[[dep_name_m]]))
          if (sum(!is.na(dep_x)) == 0) {
            return(list(
              data = data.frame(Message = paste0("Selected dependent column '", dep_name, "' contains no valid numeric observations.")),
              p_columns = character(0),
              confidence = conf
            ))
          }
          
          indep <- colnames(data)[as.numeric(selections$eda_UI1)]
          indep_names <- paste(indep, collapse = "+")
          model_text <- formula(paste(dep_name, " ~ ", indep_names))
          
          group_cols <- make.names(indep)
          
          if (auto == TRUE) {
            output <- summary.continuous(fx = model_text, data = data, stat.sd = T)
          } else {
            if (is.null(test) || length(test) == 0) {
              return(list(
                data = data.frame(Message = "Please select normality tests"),
                p_columns = character(0),
                confidence = conf
              ))
            }
            
            test_sel <- build_normality_test_selection(test)
            
            output <- eval(parse(text = paste(
              "summary.continuous(fx=model_text, data = data, stat.sd = T,",
              test_sel,
              ")"
            )))
          }
          
          output <- prepend_normality_factor_all_row(
            output,
            data,
            dep_name,
            group_cols,
            auto,
            test
          )
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