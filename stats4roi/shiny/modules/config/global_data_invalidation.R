# Global Data Invalidation System for stats4ROI
# This system ensures ALL data-driven UI elements are reset when new data is loaded
# Applies to: EDA, Distribution Testing, Sample Size/Power, and all other modules

# =============================================================================
# DATA INVALIDATION TRIGGER SYSTEM
# =============================================================================

# Global data invalidation trigger - detects when working data changes
create_global_data_trigger <- function(working_data) {
  reactive({
    data <- working_data()
    if (!is.null(data)) {
      paste0("global_data_", nrow(data), "_", ncol(data), "_", digest::digest(data))
    } else {
      "no_global_data"
    }
  })
}

# Module-specific data trigger
create_module_data_trigger <- function(working_data, module_name) {
  reactive({
    data <- working_data()
    if (!is.null(data)) {
      paste0(module_name, "_data_", nrow(data), "_", ncol(data), "_", digest::digest(data))
    } else {
      paste0(module_name, "_no_data")
    }
  })
}

# DOE Screening Design: fingerprint when any setup control changes (no dataset).
# Same idea as create_global_data_trigger: downstream reactives compare to a
# committed snapshot after "Design the Experiment" to clear stale outputs.
create_doe_orthogonal_setup_trigger <- function(input) {
  reactive({
    n <- suppressWarnings(as.integer(input$n_factors %||% 3L))
    if (length(n) == 0L || is.na(n[1L])) n <- 3L
    n <- as.integer(min(20L, max(2L, n[1L])))
    levs <- vapply(seq_len(n), function(i) as.character(input[[paste0("level_", i)]] %||% "2"), character(1))
    merges <- vapply(seq_len(n), function(i) as.character(input[[paste0("merge_", i)]] %||% ""), character(1))
    parts <- list(
      n = n,
      factor_names_custom = input$factor_names_custom %||% "",
      level_names_custom = input$level_names_custom %||% "",
      interactions_digest = digest::digest(sort(setdiff(input$interactions %||% character(0), "__doe_inter_placeholder__")), algo = "md5"),
      array_choice = input$array_choice %||% "",
      levels_digest = digest::digest(levs, algo = "md5"),
      merges_digest = digest::digest(merges, algo = "md5")
    )
    digest::digest(parts, algo = "md5")
  })
}

# =============================================================================
# MODULAR UI RESET SYSTEM
# =============================================================================

# Global UI reset system - resets ALL data-driven UI elements
reset_all_data_ui_elements <- function(session, working_data) {
  # Track the previous data state to only reset when data actually changes
  previous_data_state <- reactiveVal(NULL)
  
  observeEvent(working_data(), {
    current_data <- working_data()
    current_state <- if (!is.null(current_data)) {
      paste0("global_data_", nrow(current_data), "_", ncol(current_data), "_", digest::digest(current_data))
    } else {
      "no_global_data"
    }
    
    previous_state <- previous_data_state()
    
    # Only reset UI if the data state has actually changed
    if (is.null(previous_state) || current_state != previous_state) {
      # Reset all registered modules (each ui_reset uses its own module session)
      reset_registered_modules()
      
      # Switch to data import tab when new data is loaded
      updateTabsetPanel(session, "main_navbar", selected = "Import Data")
      
      # Update the previous state
      previous_data_state(current_state)
    }
  })
}

# Reset all registered modules.
# Each ui_reset must be a zero-argument function that captures module-local `session`
# from moduleServer (see eda_module). Do not use function(session) { ... } here.
reset_registered_modules <- function() {
  # Get registered modules from global config
  config <- get_global_config()
  registered_modules <- config$get_registered_modules()
  
  # Reset each registered module
  for (module_name in registered_modules) {
    tryCatch({
      module_info <- module_registry[[module_name]]
      if (!is.null(module_info$ui_reset)) {
        module_info$ui_reset()
      }
    }, error = function(e) {
      cat("Error resetting module", module_name, ":", e$message, "\n")
    })
  }
}

# EDA module UI reset (delegates to EDA data-invalidation helpers)
reset_eda_ui_elements <- function(session) {
  helper_path <- "modules/statistical/eda/utils/data_invalidation_helpers.R"
  if (file.exists(helper_path)) {
    source(helper_path, local = FALSE)
    reset_eda_data_driven_ui(session)
  }
}

# Distribution Testing module UI reset
reset_distribution_testing_ui_elements <- function(session) {
  # Reset distribution test type
  updateRadioButtons(session, "dist_test_type", selected = 1)
  
  # Reset data type selection
  updateRadioButtons(session, "dist_test_data_type", selected = 1)
  
  # Reset numeric inputs
  updateNumericInput(session, "dist_test_conf", value = 0.95)
  updateNumericInput(session, "dist_test_decimals", value = 3)
  
  # Reset checkboxes
  updateCheckboxInput(session, "dist_test_zero", value = FALSE)
  updateCheckboxInput(session, "dist_test_info", value = FALSE)
  
  # Reset plot selection (if it exists)
  tryCatch({
    updateRadioGroupButtons(session, "dist_test_plot_select", selected = 1)
  }, error = function(e) {
    # Plot selection might not exist yet, that's okay
  })
  
  # Note: UI1 and UI2 are rendered via uiOutput and will be automatically
  # re-rendered when the module detects data change, so we don't need to
  # manually reset them here
  
}

# Sample Size/Power module UI reset
reset_sample_size_power_ui_elements <- function(session) {
  # Reset sample size/power selections
  updateRadioButtons(session, "ssp_analysis_type", selected = 1)
  updateRadioButtons(session, "ssp_test_type", selected = 1)
  
  # Reset numeric inputs
  updateNumericInput(session, "ssp_alpha", value = 0.05)
  updateNumericInput(session, "ssp_power", value = 0.8)
  updateNumericInput(session, "ssp_effect_size", value = 0.5)
  updateNumericInput(session, "ssp_n", value = 30)

  # Power curve (module inputs)
  updateCheckboxInput(session, "power_curve", value = FALSE)
  updateNumericInput(session, "power_curve_start", value = 0)
  updateNumericInput(session, "power_curve_interval", value = 0.05)
}

# Other data-driven modules reset
reset_other_data_modules <- function(session) {
  # Add any other modules that have data-driven UI elements
  # This is where you would add resets for future modules
  
}

# =============================================================================
# VALIDATION SYSTEM
# =============================================================================

# Global validation system for all data-driven selections
validate_global_data_selections <- function(data, module, selections) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(valid = FALSE, message = "No data available. Please load data first."))
  }
  
  # Use registered module validation if available
  config <- get_global_config()
  registered_modules <- config$get_registered_modules()
  
  if (module %in% registered_modules) {
    module_info <- module_registry[[module]]
    if (!is.null(module_info$validation)) {
      return(module_info$validation(data, selections))
    }
  }
  
  # Fallback to legacy validation
  if (module == "eda") {
    return(validate_eda_selections(data, selections))
  } else if (module == "distribution_testing") {
    return(validate_distribution_testing_selections(data, selections))
  } else if (module == "sample_size_power") {
    return(validate_sample_size_power_selections(data, selections))
  }
  
  return(list(valid = TRUE, message = ""))
}

# Standard validation patterns
validate_data_selection <- function(data, selections) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(valid = FALSE, message = "No data available. Please load data first."))
  }
  
  if (is.null(selections$UI1) || length(selections$UI1) == 0) {
    return(list(valid = FALSE, message = "Please select data columns."))
  }
  
  # Validate column selections
  selected_cols <- as.numeric(selections$UI1)
  if (any(selected_cols > ncol(data) | selected_cols < 1)) {
    return(list(valid = FALSE, message = "Invalid column selection for current data. Please reselect columns."))
  }
  
  return(list(valid = TRUE, message = ""))
}

validate_factor_analysis_selection <- function(data, selections) {
  # First validate basic data selection
  basic_validation <- validate_data_selection(data, selections)
  if (!basic_validation$valid) {
    return(basic_validation)
  }
  
  # Additional validation for factor analysis
  if (is.null(selections$UI2) || length(selections$UI2) == 0) {
    return(list(valid = FALSE, message = "Please select data columns for factor analysis."))
  }
  
  # Validate data column selections
  selected_data_cols <- as.numeric(selections$UI2)
  if (any(selected_data_cols > ncol(data) | selected_data_cols < 1)) {
    return(list(valid = FALSE, message = "Invalid data column selection for factor analysis."))
  }
  
  return(list(valid = TRUE, message = ""))
}

# EDA-specific validation
validate_eda_selections <- function(data, selections) {
  if (!is.null(selections$columns) && length(selections$columns) > 0) {
    selected_cols <- as.numeric(unlist(strsplit(x = selections$columns, split = "\\s+")))
    if (any(selected_cols > ncol(data) | selected_cols < 1)) {
      return(list(valid = FALSE, message = "Invalid column selection for current data. Please reselect columns."))
    }
  }
  return(list(valid = TRUE, message = ""))
}

# Distribution Testing-specific validation
validate_distribution_testing_selections <- function(data, selections) {
  if (!is.null(selections$columns) && length(selections$columns) > 0) {
    selected_cols <- as.numeric(unlist(strsplit(x = selections$columns, split = "\\s+")))
    if (any(selected_cols > ncol(data) | selected_cols < 1)) {
      return(list(valid = FALSE, message = "Invalid column selection for current data. Please reselect columns."))
    }
  }
  
  # Additional validation for distribution testing
  if (!is.null(selections$data_type) && selections$data_type == 2) {
    # Factor analysis mode - need both factor and data columns
    if (is.null(selections$factor_columns) || length(selections$factor_columns) == 0) {
      return(list(valid = FALSE, message = "Please select factor columns for factor analysis."))
    }
    if (is.null(selections$data_columns) || length(selections$data_columns) == 0) {
      return(list(valid = FALSE, message = "Please select data columns for factor analysis."))
    }
  }
  
  return(list(valid = TRUE, message = ""))
}

# Sample Size/Power-specific validation
validate_sample_size_power_selections <- function(data, selections) {
  # Add validation logic for sample size/power selections
  return(list(valid = TRUE, message = ""))
}

# Global error table creator
create_global_error_table <- function(message) {
  DT::datatable(
    data.frame(Message = message), 
    options = list(paging = FALSE, searching = FALSE, info = FALSE)
  )
}

# Global data invalidation observer
# This should be called in the main server function
setup_global_data_invalidation <- function(session, working_data) {
  # Set up the global reset observer
  reset_all_data_ui_elements(session, working_data)
  
}
