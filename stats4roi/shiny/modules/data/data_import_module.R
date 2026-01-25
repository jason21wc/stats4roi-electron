# Data Import Module for stats4ROI
# This module exactly replicates the file loading functionality from the original app
# Uses datamods package (not esquisse) as per original app.R

# Data Import UI (replicating app.R lines 460-478)
create_data_import_ui <- function(id) {
  ns <- NS(id)
  
  # Import choices (from original app)
  choice_import <- c("file", "copypaste", "googlesheets", "url", "dropbox", "onedrive")
  
  fluidRow(
    column(
      width = 4,
      checkboxGroupInput(
        inputId = ns("from"),
        label = "Select where the data file is located then click \"Open Import Window\" below:",
        choices = choice_import,
        selected = c("file", "copypaste")
      ),
      actionButton(ns("launch_modal"), "Open import window"),
      p("Your original file is only accessed here. Nothing is changed in the original file.")
    ),
    column(
      width = 8,
      tags$b("Imported data:"),
      verbatimTextOutput(outputId = ns("name")),
      verbatimTextOutput(outputId = ns("data"))
    )
  )
}

# Data Import Server (replicating app.R lines 3780-3815)
create_data_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Import modal (replicating app.R lines 3780-3786)
    observeEvent(input$launch_modal, {
      req(input$from)
      import_modal(
        id = "modal_import",
        from = input$from,
        title = "Import data to be used in application"
      )
    })
    
    # Import server (replicating app.R lines 3788-3795)
    imported <- import_server("modal_import",
                              return_class = "data.frame",
                              read_fns = list(
                                dat = function(file) {
                                  readr::read_delim(file = file)
                                }
                              )
    )
    
    # Output functions (replicating app.R lines 3797-3815)
    output$name <- renderPrint({
      req(imported$name())
      imported$name()
    })
    
    output$data <- renderPrint({
      req(imported$data())
      imported$data()
    })
    
    # Return the imported data reactive and a signal for new data
    return(list(
      data = reactive(imported$data()),
      name = reactive(imported$name()),
      new_data_signal = reactive({
        req(imported$data())
        TRUE
      })
    ))
  })
}
