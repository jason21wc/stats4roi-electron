# Architectural Patterns for stats4ROI
# This file defines the core architectural patterns and helper functions
# used across all modules in the stats4ROI application.

# =============================================================================
# CORE ARCHITECTURAL PRINCIPLES
# =============================================================================

# 1. Three-Tier Architecture
#    - Main App: Global configuration and module coordination
#    - Coordinator Modules: UI composition, data flow, rendering
#    - Worker Modules: Business logic, calculations, reactive functions

# 2. Explicit Data Flow
#    - Data passed as parameters between modules
#    - No implicit global state dependencies
#    - Clear reactive dependency chains

# 3. Namespace Management
#    - All UI rendering happens in coordinator modules
#    - Worker modules return reactive functions only
#    - No namespace conflicts between modules

# 4. Data Invalidation
#    - Centralized system for handling data changes
#    - All modules register for invalidation
#    - Consistent UI reset behavior

# =============================================================================
# COLOR SYSTEM
# =============================================================================

# Standardized color system across all modules
get_distribution_colors <- function(color_palette) {
  list(
    col_plot_line = color_palette[1],
    col_fill_highlight = color_palette[2], 
    col_point_of_interest_line = color_palette[3],
    col_mean_line = color_palette[4],
    col_fill = color_palette[5],
    col_line_control_chart = color_palette[6]
  )
}

# =============================================================================
# INPUT VALIDATION PATTERNS
# =============================================================================

# Standard input validation with defaults
validate_numeric_input <- function(input_value, default_value, min_val = NULL, max_val = NULL) {
  if (is.null(input_value) || is.na(input_value)) {
    return(default_value)
  }
  
  if (!is.null(min_val) && input_value < min_val) {
    return(min_val)
  }
  
  if (!is.null(max_val) && input_value > max_val) {
    return(max_val)
  }
  
  input_value
}

validate_logical_input <- function(input_value, default_value = FALSE) {
  if (is.null(input_value)) {
    return(default_value)
  }
  input_value
}

validate_character_input <- function(input_value, default_value = "") {
  if (is.null(input_value) || input_value == "") {
    return(default_value)
  }
  input_value
}

# =============================================================================
# DATA PROCESSING PATTERNS
# =============================================================================

# Standard data selection pattern (from distribution testing)
process_data_selection <- function(data, data_type, UI1, UI2) {
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }
  
  if (is.null(UI1) || length(UI1) == 0) {
    return(data.frame())
  }
  
  if (data_type == 1) {
    # Column analysis - only use UI1 selection
    selected <- as.numeric(UI1)
    output <- data[, selected, drop = FALSE]
  } else if (data_type == 2) {
    # Factor analysis - use both UI1 and UI2 selections
    if (is.null(UI2) || length(UI2) == 0) {
      return(data.frame())
    }
    selected <- c(as.numeric(UI1), as.numeric(UI2))
    output <- data[, selected, drop = FALSE]
  } else {
    return(data.frame())
  }
  
  as.data.frame(output)
}

# =============================================================================
# UI RENDERING PATTERNS
# =============================================================================

# Standard UI1 selection (from distribution testing)
create_ui1_selection <- function(ns, data, data_type, label1, label2) {
  choices <- seq_len(ncol(data))
  names(choices) <- names(data)
  
  if (data_type == 1) {
    pickerInput(
      inputId = ns("UI1"),
      label = label1,
      multiple = TRUE,
      options = list(`actions-box` = TRUE),
      choices = choices
    )
  } else if (data_type == 2) {
    pickerInput(
      inputId = ns("UI1"),
      label = label2,
      multiple = TRUE,
      options = list(`actions-box` = TRUE),
      choices = choices
    )
  }
}

# Standard UI2 selection (from distribution testing)
create_ui2_selection <- function(ns, data, data_type, UI1) {
  if (data_type != 2 || is.null(UI1) || length(UI1) == 0) {
    return(NULL)
  }
  
  choices <- seq_len(ncol(data))
  names(choices) <- names(data)
  
  # Exclude selected factors
  fact_selected <- as.numeric(unlist(strsplit(x = UI1, split = "\\s+")))
  temp <- seq_along(choices)
  temp <- temp[-fact_selected]
  choices <- choices[temp]
  
  pickerInput(
    inputId = ns("UI2"),
    label = "Select Data",
    multiple = TRUE,
    options = list(`actions-box` = TRUE),
    choices = choices
  )
}

# =============================================================================
# ERROR HANDLING PATTERNS
# =============================================================================

# Standard error table creation
create_error_table <- function(message) {
  data.frame(
    Error = message,
    stringsAsFactors = FALSE
  )
}

# Standard validation check
validate_data_selection <- function(data, selections) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(valid = FALSE, message = "No data available. Please load data first."))
  }
  
  if (is.null(selections$UI1) || length(selections$UI1) == 0) {
    return(list(valid = FALSE, message = "Please select data columns."))
  }
  
  list(valid = TRUE, message = "")
}

# =============================================================================
# REACTIVE PATTERNS
# =============================================================================

# Standard reactive with validation
create_validated_reactive <- function(input_value, validation_func, default_value) {
  reactive({
    value <- input_value()
    validation_func(value, default_value)
  })
}

# Standard data reactive pattern
create_data_reactive <- function(data_source, data_type_reactive, input_values) {
  reactive({
    data <- data_source()
    req(data)
    
    data_type <- data_type_reactive()
    selections <- input_values()
    
    process_data_selection(data, data_type, selections$UI1, selections$UI2)
  })
}

# =============================================================================
# DOWNLOAD FUNCTIONALITY
# =============================================================================

# Standard download UI
downloadButtonUI <- function(id) {
  downloadButton(NS(id, "dl_plot"))
}

downloadSelectUI <- function(id) {
  pickerInput(NS(id, "format"), 
             label = "Format: ", 
             choices = c("eps","ps","tex","pdf","jpeg","tiff","png","bmp","svg","wmf"),
             selected = "svg",
             width = "75px")
}

# Standard download server
downloadServer <- function(id, plot, height = NULL, width = NULL) {
  moduleServer(id, function(input, output, session) {
    output$dl_plot <- downloadHandler(
      filename = function() {
        file_format <- tolower(input$format)
        paste0(id, ".", file_format)
      },
      content = function(file) {
        ggsave(file, plot = plot(), height = height(), width = width(), 
               units = "px", device = tolower(input$format))
      }
    )
  })
}

# =============================================================================
# MODULE REGISTRATION
# =============================================================================

# Register module for data invalidation
register_module_invalidation <- function(module_name, ui_elements) {
  # This would be called by each module to register its UI elements
  # for global data invalidation
  # Implementation depends on the global invalidation system
}

# =============================================================================
# ARCHITECTURAL COMPLIANCE CHECKING
# =============================================================================

# Check if module follows architectural patterns
check_architectural_compliance <- function(module_code) {
  compliance_issues <- c()
  
  # Check for namespace violations
  if (grepl("renderPlot.*ns\\(", module_code)) {
    compliance_issues <- c(compliance_issues, 
                          "renderPlot should not use ns() - namespace violation")
  }
  
  # Check for observer overuse
  observer_count <- length(grep("observeEvent", module_code))
  if (observer_count > 2) {
    compliance_issues <- c(compliance_issues, 
                          paste("Too many observers (", observer_count, 
                                ") - consider reactive functions"))
  }
  
  # Check for proper return pattern
  if (!grepl("return\\(list\\(", module_code)) {
    compliance_issues <- c(compliance_issues, 
                          "Module should return list of reactive functions")
  }
  
  compliance_issues
}

# =============================================================================
# TEMPLATE UTILITIES
# =============================================================================

# Replace template placeholders
replace_template_placeholders <- function(template_content, replacements) {
  content <- template_content
  
  for (placeholder in names(replacements)) {
    content <- gsub(paste0("\\[", placeholder, "\\]"), replacements[[placeholder]], content)
  }
  
  content
}

# Generate module from template
generate_module_from_template <- function(template_path, output_path, replacements) {
  template_content <- readLines(template_path)
  module_content <- replace_template_placeholders(template_content, replacements)
  writeLines(module_content, output_path)
}
