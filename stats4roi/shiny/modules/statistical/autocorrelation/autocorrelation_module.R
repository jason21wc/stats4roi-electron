# Autocorrelation Analysis Module
# Stand-alone ACF / PACF / run-sequence diagnostics.
# Shared compute utils are also used by SPC Individuals Chart Statistics.

library(shiny)
library(ggplot2)
library(shinyWidgets)

source("modules/config/global_config.R")
source("modules/config/global_data_invalidation.R")

source("modules/statistical/autocorrelation/utils/acf_analysis.R")
source("modules/statistical/autocorrelation/utils/acf_plots.R")
source("modules/statistical/autocorrelation/ui/autocorrelation_ui.R")
source("modules/statistical/autocorrelation/server/autocorrelation_server.R")

# =============================================================================
# COORDINATOR UI
# =============================================================================
create_autocorrelation_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Autocorrelation",
    value = ns("autocorrelation_tab"),
    create_autocorrelation_ui_internal(ns)
  )
}

# =============================================================================
# COORDINATOR SERVER
# =============================================================================
create_autocorrelation_server <- function(id, filtered_data, reactive_color_palette) {
  moduleServer(id, function(input, output, session) {
    register_module(
      "autocorrelation_module",
      ui_reset = function() {},
      validation = function(data, selections) {
        if (is.null(data) || nrow(data) == 0) {
          return(list(valid = FALSE, message = "No data available for Autocorrelation analysis"))
        }
        list(valid = TRUE, message = "")
      }
    )

    setup_autocorrelation_server(
      input,
      output,
      session,
      filtered_data,
      reactive_color_palette
    )
  })
}
