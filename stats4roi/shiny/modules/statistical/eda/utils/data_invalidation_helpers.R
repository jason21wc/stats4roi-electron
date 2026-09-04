# Data Invalidation Helper Functions
# Centralized system for handling data changes across all EDA modules

if (!exists("DEFAULT_QUANTILE_TYPE", inherits = TRUE)) {
  source("modules/statistical/eda/utils/quantile_types.R")
}

# Create a data invalidation trigger that changes when data changes
create_data_invalidation_trigger <- function(filtered_data) {
  reactive({
    data <- filtered_data()
    if (!is.null(data)) {
      paste0("data_", nrow(data), "_", ncol(data), "_", digest::digest(data))
    } else {
      "no_data"
    }
  })
}

# Run an update* call without failing if the input is not on the current page
safe_eda_update <- function(expr) {
  tryCatch(expr, error = function(e) invisible(NULL))
}

# Default descriptives statistic selection (matches descriptives_ui.R)
eda_default_desc_stats <- function() {
  c("stat.mean=T", "stat.sd=T", "stat.min=T", "stat.max=T", "stat.range=T")
}

# Reset data-driven EDA UI only (preserves Natural Tolerance "Enter Statistics" inputs)
reset_eda_data_driven_ui <- function(session) {
  # Data setup
  safe_eda_update(updatePickerInput(session, "eda_UI1", selected = character(0)))
  safe_eda_update(updatePickerInput(session, "eda_UI2", selected = character(0)))
  safe_eda_update(updateRadioButtons(session, "eda_data_type", selected = 1))
  safe_eda_update(updateTabsetPanel(session, "eda_panel", selected = "eda_data_setup"))
  
  # Descriptives
  safe_eda_update(updateNumericInput(session, "decimals_desc2", value = 5))
  safe_eda_update(updatePickerInput(session, "desc_quantile_type", selected = DEFAULT_QUANTILE_TYPE))
  safe_eda_update(updatePickerInput(session, "desc_stats", selected = eda_default_desc_stats()))
  
  # Normality tests
  safe_eda_update(updateNumericInput(session, "conf_eda", value = 0.95))
  safe_eda_update(updateNumericInput(session, "decimals_desc", value = 5))
  safe_eda_update(updatePrettySwitch(session, "auto_norm", value = TRUE))
  safe_eda_update(updateCheckboxGroupButtons(session, "norm_test", selected = character(0)))
  
  # Histograms / frequency polygons / density
  safe_eda_update(updateRadioButtons(session, "hist_type", selected = 1))
  safe_eda_update(updateCheckboxInput(session, "norm_curve", value = FALSE))
  safe_eda_update(updateCheckboxInput(session, "hist_specs", value = FALSE))
  safe_eda_update(updateNumericInput(session, "hist_LSL", value = NA))
  safe_eda_update(updateNumericInput(session, "hist_target", value = NA))
  safe_eda_update(updateNumericInput(session, "hist_USL", value = NA))
  safe_eda_update(updateNumericInput(session, "hist_bin_w", value = NULL))
  safe_eda_update(updateNumericInput(session, "hist_bins", value = NULL))
  safe_eda_update(updateNumericInput(session, "hist_center", value = NULL))
  safe_eda_update(updateCheckboxInput(session, "hist_extend_d", value = FALSE))
  safe_eda_update(updateCheckboxInput(session, "hist_rug", value = TRUE))
  safe_eda_update(updateCheckboxInput(session, "combine_data_choice", value = FALSE))
  safe_eda_update(updateCheckboxInput(session, "mult_data_choice", value = TRUE))
  safe_eda_update(updateNoUiSliderInput(session, "hist_width", value = 400))
  safe_eda_update(updateNoUiSliderInput(session, "hist_height", value = 400))
  safe_eda_update(updateTextInput(session, "hist_title", value = ""))
  safe_eda_update(updateTextInput(session, "hist_x_lab", value = ""))
  safe_eda_update(updateCheckboxInput(session, "hist_big", value = FALSE))
  safe_eda_update(updateMaterialSwitch(session, "hist_freq_y_axis", value = FALSE))
  safe_eda_update(updateMaterialSwitch(session, "hist_freq_dist", value = FALSE))
  safe_eda_update(updateNumericInput(session, "freq_dist_dec", value = 5))
  
  # Boxplots
  safe_eda_update(updateCheckboxInput(session, "box_violin", value = FALSE))
  safe_eda_update(updateCheckboxInput(session, "notch_box", value = FALSE))
  safe_eda_update(updateNoUiSliderInput(session, "box_width", value = 400))
  safe_eda_update(updateNoUiSliderInput(session, "box_height", value = 400))
  safe_eda_update(updateTextInput(session, "box_title", value = ""))
  safe_eda_update(updateTextInput(session, "box_x_lab", value = ""))
  safe_eda_update(updateTextInput(session, "box_y_lab", value = ""))
  safe_eda_update(updateCheckboxInput(session, "box_big", value = FALSE))
  
  # Quantiles
  safe_eda_update(updateNumericInput(session, "decimals_quant", value = 5))
  safe_eda_update(updatePickerInput(session, "quantile_type", selected = DEFAULT_QUANTILE_TYPE))
  safe_eda_update(updateRadioButtons(session, "quant_sel", selected = 4))
  safe_eda_update(updateNumericInput(session, "quant_cust", value = 4))
  
  # Intervals
  safe_eda_update(updateNumericInput(session, "conf_ci", value = 0.95))
  safe_eda_update(updateNumericInput(session, "decimals_ci", value = 5))
  safe_eda_update(updateRadioButtons(session, "interval_type", selected = 1))
  safe_eda_update(updateRadioButtons(session, "interval_b_type", selected = "HDI"))
  safe_eda_update(updateCheckboxInput(session, "ci_show_plot", value = FALSE))
  safe_eda_update(updateRadioButtons(session, "ci_plot_param", selected = "Mean"))
  safe_eda_update(updateNoUiSliderInput(session, "ci_plot_width", value = 400))
  safe_eda_update(updateNoUiSliderInput(session, "ci_plot_height", value = 400))
  safe_eda_update(updateNumericInput(session, "ci_font_size", value = 11))
  safe_eda_update(updateTextInput(session, "ci_plot_title", value = ""))
  safe_eda_update(updateTextInput(session, "ci_plot_xlab", value = ""))
  safe_eda_update(updateTextInput(session, "ci_plot_ylab", value = ""))
  safe_eda_update(updateCheckboxInput(session, "ci_info", value = FALSE))
  
  # Natural tolerance — Use Data tab only (not Enter Statistics: decimals_nt, dist_nt, UI1_nt, UI2_nt)
  safe_eda_update(updateNumericInput(session, "decimals_nt_data", value = 4))
  safe_eda_update(updatePickerInput(session, "dist_nt_data", selected = 1))
}

# Reset all UI elements when data changes (generic helper)
reset_ui_on_data_change <- function(session, ui_elements) {
  observeEvent(ui_elements$data_trigger(), {
    if (!is.null(ui_elements$eda_reset) && isTRUE(ui_elements$eda_reset)) {
      reset_eda_data_driven_ui(session)
      return(invisible(NULL))
    }
    
    # Reset picker inputs
    if (!is.null(ui_elements$picker_inputs)) {
      for (input_id in ui_elements$picker_inputs) {
        safe_eda_update(updatePickerInput(session, input_id, selected = character(0)))
      }
    }
    
    # Reset numeric inputs
    if (!is.null(ui_elements$numeric_inputs)) {
      for (input_id in names(ui_elements$numeric_inputs)) {
        safe_eda_update(updateNumericInput(session, input_id, value = ui_elements$numeric_inputs[[input_id]]))
      }
    }
    
    # Reset switches
    if (!is.null(ui_elements$switches)) {
      for (input_id in names(ui_elements$switches)) {
        safe_eda_update(updatePrettySwitch(session, input_id, value = ui_elements$switches[[input_id]]))
      }
    }
    
    # Reset radio buttons
    if (!is.null(ui_elements$radio_buttons)) {
      for (input_id in names(ui_elements$radio_buttons)) {
        safe_eda_update(updateRadioButtons(session, input_id, selected = ui_elements$radio_buttons[[input_id]]))
      }
    }
    
    # Reset checkboxes
    if (!is.null(ui_elements$checkboxes)) {
      for (input_id in names(ui_elements$checkboxes)) {
        safe_eda_update(updateCheckboxInput(session, input_id, value = ui_elements$checkboxes[[input_id]]))
      }
    }
    
    # Switch to data setup tab if specified
    if (!is.null(ui_elements$switch_to_tab)) {
      safe_eda_update(updateTabsetPanel(session, ui_elements$tabset_id, selected = ui_elements$switch_to_tab))
    }
  }, ignoreInit = TRUE)
}

# Validate UI selections against current data
validate_ui_selections <- function(data, selections) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(valid = FALSE, message = "No data available. Please load data first."))
  }
  
  # Check column selections
  if (!is.null(selections$columns) && length(selections$columns) > 0) {
    selected_cols <- as.numeric(unlist(strsplit(x = selections$columns, split = "\\s+")))
    if (any(selected_cols > ncol(data) | selected_cols < 1)) {
      return(list(valid = FALSE, message = "Invalid column selection for current data. Please reselect columns."))
    }
  }
  
  return(list(valid = TRUE, message = ""))
}

# Create error table for display
create_error_table <- function(message) {
  DT::datatable(
    data.frame(Message = message),
    options = list(paging = FALSE, searching = FALSE, info = FALSE)
  )
}

# Legacy list used by module template examples
get_default_ui_values <- function() {
  list(
    numeric_inputs = list(
      "conf_eda" = 0.95,
      "decimals_desc" = 5,
      "decimals_desc2" = 5
    ),
    switches = list(
      "auto_norm" = TRUE
    ),
    radio_buttons = list(
      "eda_data_type" = 1
    ),
    checkboxes = list(
      "data_label" = TRUE
    ),
    picker_inputs = c(
      "eda_UI1",
      "eda_UI2"
    )
  )
}
