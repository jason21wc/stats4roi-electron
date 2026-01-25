# Data Setup UI Component
# Replicated from original app.R lines 1210-1231

create_data_setup_ui <- function(ns) {
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = ns("eda_data_type"),
        label = "Select Data Type",
        choices = c(
          "Analyze columns" = 1,
          "Analyze by factors" = 2
        )
      ),
      uiOutput(ns("eda_UI1")), # choice_column or factor selection
      uiOutput(ns("eda_UI2"))  # data selection when using factors
    ),
    mainPanel(
      # Show selected data
      h2("Sample of Selected Data"),
      dataTableOutput(outputId = ns("eda_selected_data"))
    )
  )
}
