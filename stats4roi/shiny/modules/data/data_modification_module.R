# Data Modification Module for stats4ROI
# This module exactly replicates the variable selection, renaming, and conversion functionality from the original app

# Data Modification UI (replicating app.R lines 500-516)
create_data_modification_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 6,
      update_variables_ui(ns("vars"))
    ),
    column(
      width = 6,
      tags$b("Original data:"),
      verbatimTextOutput(ns("original")),
      verbatimTextOutput(ns("original_str")),
      tags$b("Modified data:"),
      verbatimTextOutput(ns("modified")),
      verbatimTextOutput(ns("modified_str"))
    )
  )
}

# Data Modification Server (replicating app.R lines 3836-3853)
create_data_modification_server <- function(id, imported_data) {
  moduleServer(id, function(input, output, session) {
    
    # Create namespacing function
    ns <- NS(id)
    
    # Update variables server (replicating app.R lines 3836-3839)
    updated_data <- update_variables_server(
      id = ns("vars"),
      data = reactive(imported_data())
    )
    
    # Output functions (replicating app.R lines 3841-3853)
    output$original <- renderPrint({
      imported_data()
    })
    
    output$original_str <- renderPrint({
      str(imported_data())
    })
    
    output$modified <- renderPrint({
      updated_data()
    })
    
    output$modified_str <- renderPrint({
      str(updated_data())
    })
    
    # Return the updated data reactive
    return(list(
      data = updated_data
    ))
  })
}
