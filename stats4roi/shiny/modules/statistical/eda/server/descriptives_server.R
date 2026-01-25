# Descriptives Server Component
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
create_descriptives_server <- function(id, data_source, data_type_reactive, input_values) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================================
    # INPUT VALIDATION AND EXTRACTION
    # =========================================================================
    # Extract inputs with validation (following worker template pattern)
    decimals <- reactive({
      input_vals <- input_values()
      dec <- input_vals$decimals_desc2
      if (is.null(dec) || is.na(dec)) return(5)
      dec
    })
    
    desc_stats <- reactive({
      input_vals <- input_values()
      stats <- input_vals$desc_stats
      if (is.null(stats) || length(stats) == 0) return(character(0))
      stats
    })
    
    data_list_for_desc <- reactive({
      input_vals <- input_values()
      data_col <- input_vals$data_list_for_desc
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
    # HELPER FUNCTIONS
    # =========================================================================
    build_stats_selection <- function(desc_stats) {
      # Build statistics selection string (following original app logic)
      stats_sel <- paste(desc_stats, collapse = ",")
      if (grepl("stat.mean=T", stats_sel) != T) {
        stats_sel <- c(stats_sel, ",stat.mean=F")
        stats_sel <- paste(stats_sel, collapse = "")
      }
      if (grepl("stat.var=T", stats_sel) != T) {
        stats_sel <- c(stats_sel, ",stat.var=F")
        stats_sel <- paste(stats_sel, collapse = "")
      }
      if (grepl("stat.miss=T", stats_sel) != T) {
        stats_sel <- c(stats_sel, ",stat.miss=F")
        stats_sel <- paste(stats_sel, collapse = "")
      }
      if (grepl("stat.sd=T", stats_sel) != T) {
        stats_sel <- c(stats_sel, ",stat.sd=F")
        stats_sel <- paste(stats_sel, collapse = "")
      }
      if (grepl("stat.skew.test=T", stats_sel) != T) {
        stats_sel <- c(stats_sel, ",stat.skew.test=0")
        stats_sel <- paste(stats_sel, collapse = "")
      }
      if (grepl("stat.kurt.test=T", stats_sel) != T) {
        stats_sel <- c(stats_sel, ",stat.kurt.test=0")
        stats_sel <- paste(stats_sel, collapse = "")
      }
      
      stats_sel <- c(stats_sel, ",stat.ad.test = 0,stat.sw.test = 0,stat.dago.test = 0")
      stats_sel <- paste(stats_sel, collapse = "")
      
      return(stats_sel)
    }
    
    # =========================================================================
    # BUSINESS LOGIC REACTIVE
    # =========================================================================
    descriptives_data <- reactive({
      data <- processed_data()  # Use processed data from coordinator
      data_type <- data_type_reactive()
      selections <- input_values()
      R <- decimals()
      desc_stats <- desc_stats()
      
      if (is.null(data) || nrow(data) == 0) {
        return(data.frame())
      }
      
      # If no statistics selected, return empty table
      if (is.null(desc_stats) || length(desc_stats) == 0) {
        return(data.frame(Message = "Please select statistics to display"))
      }
      
      # Names already processed by coordinator
      
      tryCatch({
        if (data_type == 1) {
          # Column analysis - use original app approach
          # Get original data and column indices (following original app pattern)
          original_data <- data_source()
          names(original_data) <- make.names(names(original_data))
          
          # Convert UI1 to column indices (like original app)
          selected_cols <- as.numeric(selections$eda_UI1)
          desc_dat <- original_data[, selected_cols, drop = FALSE]
          
          # Build statistics selection string using helper function
          stats_sel <- build_stats_selection(desc_stats)
          
          # Execute statistics (following original app logic)
          output <- eval(parse(text = paste("summary.all.variables(data = desc_dat,", stats_sel, ")")))
        } else if (data_type == 2) {
          # Factor analysis - use summary.continuous with formula (following original app logic)
          data_col <- data_list_for_desc()
          if (is.null(data_col)) {
            return(data.frame(Message = "Please select a data column"))
          }
          
          # Get column names for formula
          # data_col is the index within the selected data columns (UI2), need to map to original data
          selected_data_cols <- as.numeric(selections$eda_UI2)
          dep_name <- colnames(data)[selected_data_cols[as.numeric(data_col)]]
          indep <- colnames(data)[as.numeric(selections$eda_UI1)]
          indep_names <- paste(indep, collapse = "+")
          model_text <- formula(paste(dep_name, " ~ ", indep_names))
          
          # Build statistics selection string using helper function
          stats_sel <- build_stats_selection(desc_stats)
          
          # Execute statistics using the selection string (respecting user choices)
          output <- eval(parse(text = paste("summary.continuous(fx=model_text, data = data,", stats_sel, ")")))
        } else {
          output <- data.frame()
        }
      }, error = function(e) {
        # Handle known issues with summary.all.variables()
        error_msg <- e$message
        if (grepl("names do not match previous names", error_msg)) {
          error_msg <- "This error can occur with certain data combinations (e.g., columns with different data types or missing values). This is a known limitation of the statistical function."
        }
        output <<- data.frame(Error = paste("Calculation error:", error_msg))
      })
      
      # Apply rounding using global function
      output <- ro(output, R)
      output
    })
    
    # =========================================================================
    # RETURN REACTIVE FUNCTIONS (coordinator will render these)
    # =========================================================================
    list(
      # Data reactives
      descriptives_data = descriptives_data,
      processed_data = processed_data,
      
      # Input reactives
      decimals = decimals,
      desc_stats = desc_stats
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