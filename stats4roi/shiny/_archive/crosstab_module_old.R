library(shiny)
library(rhandsontable)
library(tidyverse)
library(htmlwidgets)

# Enhanced UI function with reset button
create_crosstab_ui <- function(id, 
                               initial_rows = 2, 
                               initial_cols = 2,
                               initial_row_label = "Row",
                               initial_col_label = "Column",
                               type=c("manual","data")
                               ) {
  ns <- NS(id)
  tagList(
    
    if(type[1]=="manual"){
      fluidRow(
        column(4,numericInput(ns("rows"), "Number of Rows", value = initial_rows, min = 1,width = "150px")),
        column(4,textInput(ns("row_label"), "Row Label", value = initial_row_label))
      )
    },
    
    if(type[1]=="manual"){
      fluidRow(
        column(4,numericInput(ns("cols"), "Number of Columns", value = initial_cols, min = 1,width = "150px")),
        column(4,textInput(ns("col_label"), "Column Label", value = initial_col_label)),
        column(2,numericInput(ns("decimals"),label = "Decimals",value = 3,min = 0,max = 9,step = 1,width = "75px")),
        column(2,numericInput(ns("conf"),label = "Confidence",value = 0.95,min = 0,max = 1,step = .05,width = "75px"))
      )
    } else{
      fluidRow(
        column(2,numericInput(ns("decimals"),label = "Decimals",value = 3,min = 0,max = 9,step = 1,width = "75px")),
        column(2,numericInput(ns("conf"),label = "Confidence",value = 0.95,min = 0,max = 1,step = .05,width = "75px"))
      )
    }
    ,
    
    if(type[1]=="manual"){
      h4("Crosstab Data Entry")
    } else{h4("Crosstabs from Data")}
    ,
    
    rHandsontableOutput(ns("crosstab")),
    hr(),
    fluidRow(
      column(4,actionButton(ns("merge"), "Merge Selected",class = "btn-success",title="Select adjacent rows or columns by clicking and dragging across headers, or non-adjacent rows or columns by clicking and holding CTRL")
      ),
      column(4,actionButton(ns("delete_btn"), "Delete Selected", class="btn-danger")),
      column(4,actionButton(ns("reset"), "Reset Table", class = "btn-warning"))
    )

  )
}

# Enhanced server function with return values
create_crosstab_server <- function(id, xt_data, type = c("manual", "data")) {
  type <- match.arg(type)  # Validate and get the type
  
  moduleServer(id, function(input, output, session) {
    table_state <- reactiveVal(NULL)
    
    # Modify the observer to properly initialize the table state from data
    observe({
      req(xt_data)
      
        data <- xt_data()
        req(data)
        
        # Make sure data is a data frame
        if (!is.data.frame(data)) {
          data <- as.data.frame(data)
        }
        
        # Update the table state
        table_state(data)
        
        # If using manual inputs, update them to match the data dimensions
        if (!is.null(input$rows)) {
          updateNumericInput(session, "rows", value = nrow(data))
        }
        if (!is.null(input$cols)) {
          updateNumericInput(session, "cols", value = ncol(data))
        }
 
    })
    
    # Create or update table when dimensions change
    observe({
      req(input$rows, input$cols)
      
      # Get current table if it exists
      current_table <- table_state()
      
      if (is.null(current_table)) {
        # Initial table creation
        row_labels <- paste(input$row_label, 1:input$rows)
        col_labels <- paste(input$col_label, 1:input$cols)
        new_table <- matrix(0, 
                            nrow = input$rows, 
                            ncol = input$cols,
                            dimnames = list(row_labels, col_labels)
        )
        
        table_state(as.data.frame(new_table))
        
      } else {
        # Only handle dimension changes if they're different
        current_row_count <- nrow(current_table)
        current_col_count <- ncol(current_table)
        
        if (input$rows != current_row_count || input$cols != current_col_count) {
          # Get current labels
          current_row_labels <- rownames(current_table)
          current_col_labels <- colnames(current_table)
          
          # Create new table with current labels as base
          new_table <- matrix(0, 
                              nrow = input$rows, 
                              ncol = input$cols,
                              dimnames = list(
                                c(current_row_labels[1:min(current_row_count, input$rows)],
                                  if (input$rows > current_row_count) 
                                    paste(input$row_label, (current_row_count+1):input$rows)),
                                c(current_col_labels[1:min(current_col_count, input$cols)],
                                  if (input$cols > current_col_count) 
                                    paste(input$col_label, (current_col_count+1):input$cols))
                              )
          )
          
          # Copy existing data
          for (i in 1:min(current_row_count, input$rows)) {
            for (j in 1:min(current_col_count, input$cols)) {
              new_table[i, j] <- current_table[i, j]
            }
          }
          
          table_state(as.data.frame(new_table))
        }
      }
    })
    
    # Modify the reset handler
    observeEvent(input$reset, {
      isolate({
        if (type == "manual") {
          # Manual mode - reset to empty table
          row_labels <- paste(input$row_label, 1:input$rows)
          col_labels <- paste(input$col_label, 1:input$cols)
          
          new_table <- matrix(0, 
                            nrow = input$rows, 
                            ncol = input$cols,
                            dimnames = list(row_labels, col_labels))
          
          # Convert to data frame before updating state
          new_table <- as.data.frame(new_table)
          
          # Update the table state
          table_state(new_table)
          
          showNotification("Manual table has been reset", type = "message")
        } else {
          # Data mode - regenerate from source data
          req(xt_data)
          data <- xt_data()
          req(data)
          
          # Update the table state with fresh data
          table_state(data)
          
          showNotification("Table has been reset to original data", type = "message")
        }
      })
    }, ignoreInit = TRUE)
    
    # Modify the label observers to handle all label changes
    observeEvent(input$row_label, {
      current_table <- table_state()
      if (!is.null(current_table)) {
        # Get current row numbers
        row_numbers <- seq_len(nrow(current_table))
        
        # Create new labels
        new_labels <- paste(input$row_label, row_numbers)
        
        # Preserve any merged row names
        current_labels <- rownames(current_table)
        merged_rows <- grep("\\+", current_labels)
        if(length(merged_rows) > 0) {
          new_labels[merged_rows] <- current_labels[merged_rows]
        }
        
        # Update row names
        rownames(current_table) <- new_labels
        table_state(current_table)
      }
    })
    
    observeEvent(input$col_label, {
      current_table <- table_state()
      if (!is.null(current_table)) {
        # Get current column numbers
        col_numbers <- seq_len(ncol(current_table))
        
        # Create new labels
        new_labels <- paste(input$col_label, col_numbers)
        
        # Preserve any merged column names
        current_labels <- colnames(current_table)
        merged_cols <- grep("\\+", current_labels)
        if(length(merged_cols) > 0) {
          new_labels[merged_cols] <- current_labels[merged_cols]
        }
        
        # Update column names
        colnames(current_table) <- new_labels
        table_state(current_table)
      }
    })
    
    # Render editable table
    output$crosstab <- renderRHandsontable({
      req(table_state())
      
      # Store the namespace function result
      ns_string <- session$ns("selection")
      
      # Get current table
      current_table <- table_state()
      
      # Create the rhandsontable with explicit headers
      hot <- rhandsontable(current_table, 
                           rowHeaderWidth = 200,
                           useTypes = FALSE,
                           stretchH = "all") %>%
        hot_table(rowHeaders = TRUE,
                  colHeaders = TRUE) %>%
        hot_cols(format = "0",
                 renderer = "numeric") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      
      # Explicitly set the row and column headers
      hot$x$rowHeaders <- rownames(current_table)
      hot$x$colHeaders <- colnames(current_table)
      
      # Enable selection by setting the afterSelectionEnd callback
      hot$x$afterSelectionEnd <- JS(
        sprintf("function(r1, c1, r2, c2) {
          var selected = this.getSelected();
          var selectedCols = [];
          var selectedRows = [];
          
          for (var i = 0; i < selected.length; i++) {
            var start_row = Math.min(selected[i][0], selected[i][2]);
            var end_row = Math.max(selected[i][0], selected[i][2]);
            var start_col = Math.min(selected[i][1], selected[i][3]);
            var end_col = Math.max(selected[i][1], selected[i][3]);
            
            for (var r = start_row; r <= end_row; r++) {
              selectedRows.push(r);
            }
            for (var c = start_col; c <= end_col; c++) {
              selectedCols.push(c);
            }
          }
          
          console.log('Selection made:', {rows: selectedRows, cols: selectedCols});
          
          Shiny.setInputValue('%s', {
            rows: [...new Set(selectedRows)],
            cols: [...new Set(selectedCols)]
          });
        }", ns_string)
      )
      
      hot
    })
    
    # Handle table edits
    previous_table <- reactiveVal(NULL)
    
    observeEvent(input$crosstab, {
      if (!is.null(input$crosstab)) {
        updated_table <- hot_to_r(input$crosstab)
        prev_table <- previous_table()
        
        # Only process if this is a new change
        if (!identical(updated_table, prev_table)) {
          # Convert to data frame while preserving row names
          rnames <- rownames(updated_table)
          cnames <- colnames(updated_table)
          # Convert all columns to numeric
          updated_table <- as.data.frame(lapply(updated_table, function(x) as.numeric(as.character(x))))
          rownames(updated_table) <- rnames
          colnames(updated_table) <- cnames
          
          # Update both states
          previous_table(updated_table)
          table_state(updated_table)
        }
      }
    }, ignoreInit = TRUE)
    
    # Handle merging
    observeEvent(input$merge, {
      sel <- input$selection
      
      # Add debugging prints
      # print("Selection received:")
      # print(sel)
      # print("Selection structure:")
      # print(str(sel))
      
      if (is.null(sel) || (is.null(sel[["rows"]]) && is.null(sel[["cols"]]))) {
        showNotification("Please select specific rows or columns first", type = "warning")
        return()
      }
      
      # Get current table data
      current_data <- hot_to_r(input$crosstab)
      # print("Current table dimensions:")
      # print(dim(current_data))
      
      n_rows <- nrow(current_data)
      n_cols <- ncol(current_data)
      
      # Extract selected rows and columns, and convert to 1-based R index
      selected_rows <- if (!is.null(sel[["rows"]])) sort(unique(as.numeric(sel[["rows"]]))) + 1 else NULL
      selected_cols <- if (!is.null(sel[["cols"]])) sort(unique(as.numeric(sel[["cols"]]))) + 1 else NULL
      
      # print("Selected rows (after conversion):")
      # print(selected_rows)
      # print("Selected cols (after conversion):")
      # print(selected_cols)
      
      # Determine if all rows or all columns are selected
      full_col_selection <- !is.null(selected_cols) && length(selected_cols)>1 && length(selected_rows) == n_rows
      full_row_selection <- !is.null(selected_rows) && length(selected_rows)>1 && length(selected_cols) == n_cols
      
      # print("Full selections:")
      # print(paste("Full col selection:", full_col_selection))
      # print(paste("Full row selection:", full_row_selection))
      
      if(!full_col_selection & !full_row_selection){
        showNotification("Please select either complete rows or complete columns", type = "warning")
        return()
      }
      
      # Correct the selection
      # If full columns but not all rows, rows are set to NULL
      if (!is.null(selected_cols) && full_col_selection) {
        selected_rows <- NULL
      }
      # If full rows but not all columns, columns are set to NULL
      if (!is.null(selected_rows) && full_row_selection) {
        selected_cols <- NULL
      }
      
      # Print corrected selections for debugging
      #print(list(corrected_rows = selected_rows, corrected_cols = selected_cols))
      
      # Merge selected columns if any
      if (!is.null(selected_cols) && length(selected_cols) > 1) {
        merged_col <- rowSums(current_data[, selected_cols, drop = FALSE])
        
        # Create merged name using the actual column names
        current_col_names <- colnames(current_data)[selected_cols]
        merged_name <- paste(current_col_names, collapse = "+")
        
        # Create new table with non-selected columns and replace with the merged column
        remaining_cols <- setdiff(1:ncol(current_data), selected_cols)
        new_table <- current_data[, remaining_cols, drop = FALSE]
        new_table[[merged_name]] <- merged_col
        
        # Arrange merged column correctly
        # new_table <- new_table[, c(remaining_cols[remaining_cols < min(selected_cols)], 
        #                            merged_name, 
        #                            remaining_cols[remaining_cols > max(selected_cols)])]
        
        # Update state and dimensions
        table_state(new_table)
        updateNumericInput(session, "cols", value = ncol(new_table))
      }
      
      # Merge selected rows if any
      if (!is.null(selected_rows) && length(selected_rows) > 1) {
        merged_row <- colSums(current_data[selected_rows, , drop = FALSE])
        
        # Create new table with non-selected rows and replace with the merged row
        remaining_rows <- setdiff(1:nrow(current_data), selected_rows)
        new_table <- current_data[remaining_rows, , drop = FALSE]
        
        # Create a name for the merged row
        new_row_name <- paste(rownames(current_data)[selected_rows], collapse = "+")
        
        # Bind the merged row
        new_table <- rbind(new_table, setNames(as.list(merged_row), colnames(new_table)))
        rownames(new_table)[nrow(new_table)] <- new_row_name
        
        # Update state and dimensions
        table_state(new_table)
        updateNumericInput(session, "rows", value = nrow(new_table))
      }
    })
    
    # Observe event for the delete button
    observeEvent(input$delete_btn, {
      sel <- input$selection
      
      if (is.null(sel) || (is.null(sel[["rows"]]) && is.null(sel[["cols"]]))) {
        showNotification("Please select rows or columns to delete", type = "warning")
        return()
      }
      
      # Get current table data
      current_data <- hot_to_r(input$crosstab)
      rnames <- rownames(current_data)
      cnames <- colnames(current_data)
      numeric_data <- as.data.frame(lapply(current_data, function(x) as.numeric(as.character(x))))
      rownames(numeric_data) <- rnames
      colnames(numeric_data) <- cnames
      n_rows <- nrow(numeric_data)
      n_cols <- ncol(numeric_data)
      
      # Extract selected rows and columns, convert to 1-based index
      selected_rows <- if (!is.null(sel[["rows"]])) sort(unique(as.numeric(sel[["rows"]]))) + 1 else NULL
      selected_cols <- if (!is.null(sel[["cols"]])) sort(unique(as.numeric(sel[["cols"]]))) + 1 else NULL
      
      # Determine if all rows or all columns are selected
      full_col_selection <- !is.null(selected_cols) && length(selected_cols) < n_cols
      full_row_selection <- !is.null(selected_rows) && length(selected_rows) < n_rows
      
      # Correct the selection
      # If full columns but not all rows, rows are set to NULL
      if (!is.null(selected_cols) && full_col_selection) {
        selected_rows <- NULL
      }
      # If full rows but not all columns, columns are set to NULL
      if (!is.null(selected_rows) && full_row_selection) {
        selected_cols <- NULL
      }
      
      # Delete selected rows or columns if they were fully selected
      if (!is.null(selected_cols) && length(selected_cols) < ncol(numeric_data)) {
        numeric_data <- numeric_data[, -selected_cols, drop = FALSE]
      }
      if (!is.null(selected_rows) && length(selected_rows) < nrow(numeric_data)) {
        numeric_data <- numeric_data[-selected_rows, , drop = FALSE]
      }
      
      # Update state with the modified table
      table_state(numeric_data)
      updateNumericInput(session, "cols", value = ncol(numeric_data))
      updateNumericInput(session, "rows", value = nrow(numeric_data))
    })
    
    # Return reactive values that can be used by the parent app
    list(
      table_data = table_state,  # Return the reactive table state
      get_dimensions = reactive({  # Return current dimensions
        table <- table_state()
        list(
          rows = nrow(table),
          cols = ncol(table)
        )
      }),
      get_labels = reactive({  # Return current labels
        table <- table_state()
        list(
          row_labels = rownames(table),
          col_labels = colnames(table)
        )
      })
    )
  })
}