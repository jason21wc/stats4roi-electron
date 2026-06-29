# MSA (Measurement System Analysis) Module - Coordinator
# Implements the MSA tab from app_monolithic.R using the coordinator/worker architecture.

library(shiny)

# Source global systems
source("modules/config/global_config.R")
source("modules/config/global_data_invalidation.R")

# Source UI components
source("modules/statistical/msa/ui/msa_ui.R")
source("modules/statistical/msa/ui/continuous_msa_ui.R")
source("modules/statistical/msa/ui/attribute_msa_ui.R")

# Source worker modules
source("modules/statistical/msa/server/continuous_msa_server.R")
source("modules/statistical/msa/server/attribute_msa_server.R")
source("modules/statistical/msa/server/msa_linearity_bias_server.R")

# Source MSA helpers/constants
source("modules/statistical/msa/utils/msa_helpers.R")
source("modules/statistical/msa/utils/msa_constants.R")
source("modules/statistical/msa/utils/msa_discrete_helpers.R")

# =============================================================================
# COORDINATOR UI FUNCTION
# =============================================================================
create_msa_ui <- function(id) {
  ns <- NS(id)
  create_msa_ui_internal(ns)
}

# =============================================================================
# COORDINATOR SERVER FUNCTION
# =============================================================================
create_msa_server <- function(id, filtered_data, reactive_color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Register module with global invalidation system (filled in later)
    register_module(
      module_name = "msa",
      ui_reset_function = function() {
        # Continuous MSA resets
        updatePickerInput(session, "msa_UI1", selected = character(0))
        updatePickerInput(session, "msa_UI2", selected = character(0))
        updatePickerInput(session, "msa_UI3", selected = character(0))
        updatePickerInput(session, "msa_UI4", selected = character(0))
        updatePickerInput(session, "msa_UI5", selected = character(0))

        updateRadioButtons(session, "msa_data_type", selected = 2)
        updateRadioButtons(session, "msa_level", selected = 1)
        updateRadioButtons(session, "msa_type", selected = 1)
        updateRadioButtons(session, "msa_calc", selected = 1)
        updateCheckboxInput(session, "msa_details", value = FALSE)
        updateCheckboxInput(session, "msa_stand", value = FALSE)
        updateSwitchInput(session, "msa_sigmas", value = FALSE)
        updateSwitchInput(session, "msa_range_b", value = FALSE)
        updateNumericInput(session, "msa_LSL", value = NA)
        updateNumericInput(session, "msa_USL", value = NA)
        updateNumericInput(session, "msa_range", value = NA)
        updateNumericInput(session, "proc_mean", value = NULL)
        updateNumericInput(session, "proc_std", value = NULL)
        updateSwitchInput(session, "msa_diagnostic", value = FALSE)
        updateSwitchInput(session, "msa_graphs", value = TRUE)
        updateNumericInput(session, "conf_msa", value = 0.95)
        updateNumericInput(session, "deci_msa", value = 4)
        updateCheckboxInput(session, "msd_lt_axis", value = TRUE)
        updateCheckboxInput(session, "msa_violin_line", value = FALSE)
        updateCheckboxInput(session, "msa_jitter_line", value = FALSE)
        updateNumericInput(session, "msa_as_measured", value = NA)
        updateCheckboxInput(session, "msa_violin", value = FALSE)
        updateCheckboxInput(session, "msa_jitter", value = FALSE)
        updateCheckboxInput(session, "norm_chart0", value = FALSE)
        updateCheckboxInput(session, "norm_box", value = FALSE)

        # Discrete MSA resets
        updatePickerInput(session, "msa_d_UI1", selected = character(0))
        updateCheckboxInput(session, "msa_d_internal", value = FALSE)
        updateCheckboxInput(session, "msa_d_standard", value = FALSE)
        updatePickerInput(session, "msa_d_stand_id", selected = character(0))
        updateRadioButtons(session, "msa_d_type", selected = 1)
        updateNumericInput(session, "conf_msa_d", value = 0.95)
        updateNumericInput(session, "deci_msa_d", value = 4)
      },
      validation_function = function(data) {
        if (is.null(data) || nrow(data) == 0) {
          return(list(valid = FALSE, message = "No data available for MSA"))
        }
        list(valid = TRUE, message = "")
      }
    )

    # Continuous MSA worker
    continuous_state <- create_continuous_msa_server(
      id = NULL,
      filtered_data = filtered_data,
      reactive_color_palette = reactive_color_palette
    )

    # Linearity & bias worker
    create_msa_linearity_bias_server(
      id = NULL,
      filtered_data = filtered_data,
      reactive_color_palette = reactive_color_palette,
      continuous_state = continuous_state
    )

    # Attribute MSA worker
    create_attribute_msa_server(
      id = NULL,
      filtered_data = filtered_data,
      reactive_color_palette = reactive_color_palette
    )
  })
}
