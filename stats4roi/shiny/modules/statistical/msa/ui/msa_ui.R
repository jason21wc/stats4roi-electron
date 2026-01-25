# MSA UI (Measurement System Analysis)
# Recreates the MSA tab layout from app_monolithic.R.

library(shiny)

create_msa_ui_internal <- function(ns) {
  navbarMenu(
    title = "MSA",
    tabPanel(
      title = "Continuous",
      create_continuous_msa_ui_internal(ns)
    ),
    tabPanel(
      title = "Discrete",
      create_attribute_msa_ui_internal(ns)
    )
  )
}
