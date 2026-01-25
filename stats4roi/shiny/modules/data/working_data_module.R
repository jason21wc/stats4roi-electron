# Working Data Module for stats4ROI
# This module exactly replicates the working_data functionality from the original app

# Working Data Server (replicating app.R lines 3817-3831)
create_working_data_server <- function(id, imported_data, updated_data, new_data_signal) {
  moduleServer(id, function(input, output, session) {
    
    # Global variable for new file tracking (from original app)
    newfile <- reactiveVal(FALSE)
    
    # Observe new data imports
    observeEvent(new_data_signal(), {
      newfile(TRUE)
    })
    
    # Working data function (replicating app.R lines 3817-3831)
    working_data <- reactive({
      data_imp <- imported_data()
      data_up <- updated_data()
      
      if(newfile() || is.null(data_up)){
        output <- data_imp
        newfile(FALSE)
        #plot_data_r<-reactive(x = NULL)
      } else {
        output <- data_up
      }
      
      output
    })
    
    # Return the working data reactive
    return(list(
      data = working_data,
      newfile = newfile
    ))
  })
}
