# Crosstab Table Worker Module
# Contains business logic for crosstab table management
# Also handles rhandsontable rendering (special case for widget inputs)

library(shiny)
library(rhandsontable)

create_crosstab_table_worker <- function(id, xt_data, input_values, type = c("manual", "data")) {
  type <- match.arg(type)  # Validate and get the type
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    table_state <- reactiveVal(NULL)
    previous_table <- reactiveVal(NULL)
    
    # Get input values reactively (for data type or as fallback)
    inputs <- reactive({
      if (type == "manual") {
        # For manual type, use direct inputs (in this module's namespace)
        list(
          rows = input$rows,
          cols = input$cols,
          row_label = input$row_label,
          col_label = input$col_label,
          decimals = input$decimals,
          conf = input$conf
        )
      } else {
        # For data type, use passed input_values
        input_values()
      }
    })
    
    # Initialize table state from external data (for "data" type)
    observe({
      if (type == "data" && !is.null(xt_data)) {
        data <- xt_data()
        req(data)
        
        # Make sure data is a data frame
        if (!is.data.frame(data)) {
          data <- as.data.frame(data)
        }
        
        # Update the table state
        table_state(data)
        
        # For data type, dimensions are determined by the data itself
        # No need to update inputs
      }
    })
    
    # Create or update table when dimensions change (for "manual" type)
    if (type == "manual") {
      observe({
        # For manual type, inputs are in this module's namespace
        # Check if inputs exist before proceeding
        if (is.null(input$rows) || is.null(input$cols)) {
          return()
        }
        
        req(input$rows, input$cols)
        
        rows_val <- input$rows
        cols_val <- input$cols
        inputs_vals <- inputs()
        
        # Get current table if it exists
        current_table <- table_state()
        
        if (is.null(current_table)) {
          # Initial table creation
          row_labels <- paste(inputs_vals$row_label, 1:rows_val)
          col_labels <- paste(inputs_vals$col_label, 1:cols_val)
          new_table <- matrix(0, 
                              nrow = rows_val, 
                              ncol = cols_val,
                              dimnames = list(row_labels, col_labels)
          )
          
          table_state(as.data.frame(new_table))
          
        } else {
          # Only handle dimension changes if they're different
          current_row_count <- nrow(current_table)
          current_col_count <- ncol(current_table)
          
          if (rows_val != current_row_count || cols_val != current_col_count) {
            # Get current labels
            current_row_labels <- rownames(current_table)
            current_col_labels <- colnames(current_table)
            
            # Create new table with current labels as base
            new_table <- matrix(0, 
                                nrow = rows_val, 
                                ncol = cols_val,
                                dimnames = list(
                                  c(current_row_labels[1:min(current_row_count, rows_val)],
                                    if (rows_val > current_row_count) 
                                      paste(inputs_vals$row_label, (current_row_count+1):rows_val)),
                                  c(current_col_labels[1:min(current_col_count, cols_val)],
                                    if (cols_val > current_col_count) 
                                      paste(inputs_vals$col_label, (current_col_count+1):cols_val))
                                )
            )
            
            # Copy existing data
            for (i in 1:min(current_row_count, rows_val)) {
              for (j in 1:min(current_col_count, cols_val)) {
                new_table[i, j] <- current_table[i, j]
              }
            }
            
            table_state(as.data.frame(new_table))
          }
        }
      })
    }
    
    # Handle reset
    observeEvent(input$reset, {
      isolate({
        if (type == "manual") {
          inputs_vals <- inputs()
          rows_val <- input$rows
          cols_val <- input$cols
          
          # Manual mode - reset to empty table
          row_labels <- paste(inputs_vals$row_label, 1:rows_val)
          col_labels <- paste(inputs_vals$col_label, 1:cols_val)
          
          new_table <- matrix(0, 
                              nrow = rows_val, 
                              ncol = cols_val,
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
    
    # Handle label changes (for manual type)
    observeEvent(inputs()$row_label, {
      if (type == "manual") {
        current_table <- table_state()
        if (!is.null(current_table)) {
          inputs_vals <- inputs()
          # Get current row numbers
          row_numbers <- seq_len(nrow(current_table))
          
          # Create new labels
          new_labels <- paste(inputs_vals$row_label, row_numbers)
          
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
      }
    })
    
    observeEvent(inputs()$col_label, {
      if (type == "manual") {
        current_table <- table_state()
        if (!is.null(current_table)) {
          inputs_vals <- inputs()
          # Get current column numbers
          col_numbers <- seq_len(ncol(current_table))
          
          # Create new labels
          new_labels <- paste(inputs_vals$col_label, col_numbers)
          
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
      }
    })
    
    # Handle table edits
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
      
      if (is.null(sel) || (is.null(sel[["rows"]]) && is.null(sel[["cols"]]))) {
        showNotification("Please select specific rows or columns first", type = "warning")
        return()
      }
      
      # Get current table data - try worker namespace first, then fall back to table_state
      current_data <- NULL
      if (!is.null(input$crosstab)) {
        current_data <- hot_to_r(input$crosstab)
      } else {
        # If rhandsontable is rendered in coordinator namespace, use table_state
        current_data <- table_state()
        req(current_data)
      }
      
      n_rows <- nrow(current_data)
      n_cols <- ncol(current_data)
      
      # Extract selected rows and columns, and convert to 1-based R index
      selected_rows <- if (!is.null(sel[["rows"]])) sort(unique(as.numeric(sel[["rows"]]))) + 1 else NULL
      selected_cols <- if (!is.null(sel[["cols"]])) sort(unique(as.numeric(sel[["cols"]]))) + 1 else NULL
      
      # Determine if all rows or all columns are selected
      full_col_selection <- !is.null(selected_cols) && length(selected_cols)>1 && length(selected_rows) == n_rows
      full_row_selection <- !is.null(selected_rows) && length(selected_rows)>1 && length(selected_cols) == n_cols
      
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
        
        # Update state and dimensions
        table_state(new_table)
        if (type == "manual") {
          updateNumericInput(session, "cols", value = ncol(new_table))
        }
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
        if (type == "manual") {
          updateNumericInput(session, "rows", value = nrow(new_table))
        }
      }
    })
    
    # Handle deletion
    observeEvent(input$delete_btn, {
      sel <- input$selection
      
      if (is.null(sel) || (is.null(sel[["rows"]]) && is.null(sel[["cols"]]))) {
        showNotification("Please select rows or columns to delete", type = "warning")
        return()
      }
      
      # Get current table data - try worker namespace first, then fall back to table_state
      current_data <- NULL
      if (!is.null(input$crosstab)) {
        current_data <- hot_to_r(input$crosstab)
      } else {
        # If rhandsontable is rendered in coordinator namespace, use table_state
        current_data <- table_state()
        req(current_data)
      }
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
      if (type == "manual") {
        updateNumericInput(session, "cols", value = ncol(numeric_data))
        updateNumericInput(session, "rows", value = nrow(numeric_data))
      }
    })
    
    # Render editable table (special case - widget needs to be rendered here to access inputs)
    output$crosstab <- rhandsontable::renderRHandsontable({
      table <- table_state()
      req(table)
      
      # Store the namespace function result for selection
      ns_string <- ns("selection")
      
      # Create the rhandsontable with explicit headers
      hot <- rhandsontable::rhandsontable(table, 
                           rowHeaderWidth = 200,
                           useTypes = FALSE,
                           stretchH = "all") %>%
        rhandsontable::hot_table(rowHeaders = TRUE,
                  colHeaders = TRUE) %>%
        rhandsontable::hot_cols(format = "0",
                 renderer = "numeric") %>%
        rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      
      # Right-align all numeric columns individually
      for (col_idx in seq_len(ncol(table))) {
        hot <- hot %>% rhandsontable::hot_col(col_idx, halign = "htRight")
      }
      
      # Explicitly set the row and column headers
      hot$x$rowHeaders <- rownames(table)
      hot$x$colHeaders <- colnames(table)
      
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
          
          Shiny.setInputValue('%s', {
            rows: [...new Set(selectedRows)],
            cols: [...new Set(selectedCols)]
          }, {priority: 'event'});
        }", ns_string)
      )
      
      hot
    })
    
    # Function to update table state from external source (for coordinator to sync edits)
    update_table_state <- function(new_table) {
      if (!is.null(new_table)) {
        # Convert to data frame while preserving row names
        rnames <- rownames(new_table)
        cnames <- colnames(new_table)
        # Convert all columns to numeric
        new_table <- as.data.frame(lapply(new_table, function(x) as.numeric(as.character(x))))
        rownames(new_table) <- rnames
        colnames(new_table) <- cnames
        
        # Update both states
        previous_table(new_table)
        table_state(new_table)
      }
    }
    
    # Return reactive values and output that can be used by the parent app
    list(
      table_data = table_state,  # Return the reactive table state
      update_table_state = update_table_state,  # Function to update table state from coordinator
      get_dimensions = reactive({  # Return current dimensions
        table <- table_state()
        if (is.null(table)) {
          list(rows = 0, cols = 0)
        } else {
          list(
            rows = nrow(table),
            cols = ncol(table)
          )
        }
      }),
      get_labels = reactive({  # Return current labels
        table <- table_state()
        if (is.null(table)) {
          list(row_labels = NULL, col_labels = NULL)
        } else {
          list(
            row_labels = rownames(table),
            col_labels = colnames(table)
          )
        }
      }),
      get_inputs = reactive({  # Return current input values (for coordinator to read)
        if (type == "manual") {
          list(
            rows = if (!is.null(input$rows)) input$rows else 0,
            cols = if (!is.null(input$cols)) input$cols else 0,
            row_label = if (!is.null(input$row_label)) input$row_label else "Row",
            col_label = if (!is.null(input$col_label)) input$col_label else "Column",
            decimals = if (!is.null(input$decimals)) input$decimals else 3,
            conf = if (!is.null(input$conf)) input$conf else 0.95
          )
        } else {
          # For data type, inputs are in this module's namespace
          list(
            decimals = if (!is.null(input$decimals)) input$decimals else 3,
            conf = if (!is.null(input$conf)) input$conf else 0.95
          )
        }
      })
      # Note: output$crosstab is rendered in this worker's namespace
      # The UI references it directly via NS("worker_id", "crosstab")
      # We don't return output objects - they're accessible via namespace
    )
  })
}
