# Dynamic Filtering Module for stats4ROI
# This module handles the filtering functionality only when needed

# Dynamic Filtering UI
create_dynamic_filtering_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 3,
      filter_data_ui(ns("filtering"), max_height = "500px")
    ),
    column(
      width = 9,
      progressBar(
        id = ns("pbar"), value = 100,
        total = 100, display_pct = TRUE
      ),
      DT::dataTableOutput(outputId = ns("table")),
      tags$b("Code dplyr:"),
      verbatimTextOutput(outputId = ns("code_dplyr")),
      tags$b("Expression:"),
      verbatimTextOutput(outputId = ns("code")),
      tags$b("Filtered data:"),
      verbatimTextOutput(outputId = ns("res_str"))
    )
  )
}

# Dynamic Filtering Server
create_dynamic_filtering_server <- function(id, working_data) {
  moduleServer(id, function(input, output, session) {
    
    # Get namespace function
    ns <- session$ns
    
    # Data reactive
    data <- reactive({
      working_data()
    })
    
    # Filter server - only initialize when this module is active
    res_filter <- filter_data_server(
      id = "filtering",
      data = data,
      widget_num = "slider",
      widget_date = "slider",
      label_na = "Missing"
    )
    
    # Progress bar update
    observeEvent(res_filter$filtered(), {
      updateProgressBar(
        session = session, id = ns("pbar"),
        value = nrow(res_filter$filtered()), total = nrow(data())
      )
    })
    
    # Output functions
    output$table <- DT::renderDT({
      res_filter$filtered()
    }, options = list(pageLength = 5))
    
    output$code_dplyr <- renderPrint({
      res_filter$code()
    })
    
    output$code <- renderPrint({
      res_filter$expr()
    })
    
    output$res_str <- renderPrint({
      str(res_filter$filtered())
    })
    
    # Return the filter results
    return(list(
      filtered = res_filter$filtered,
      code = res_filter$code,
      expr = res_filter$expr
    ))
  })
}
