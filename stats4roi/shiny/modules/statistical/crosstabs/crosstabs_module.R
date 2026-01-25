# Crosstabs Module
# This module follows the three-tier architecture with proper coordinator-worker separation
# and integration with global systems.

# =============================================================================
# IMPORTS
# =============================================================================
library(shiny)
library(rhandsontable)
library(lolcat)
library(shinyWidgets)

# Source function overrides (must be after library(lolcat) to override)
# TODO: Remove this override when lolcat package is updated with the fix
# See: modules/statistical/crosstabs/utils/OVERRIDE_REMOVAL_GUIDE.md for removal instructions
source("modules/statistical/crosstabs/utils/cor_bowker_symmetry_override.R")

# Source global systems
source("modules/config/global_config.R")
source("modules/config/global_data_invalidation.R")

# Source worker modules
source("modules/statistical/crosstabs/server/crosstab_table_server.R")
source("modules/statistical/crosstabs/server/crosstab_statistics_server.R")

# Source UI module
source("modules/statistical/crosstabs/ui/crosstabs_ui.R")

# =============================================================================
# COORDINATOR UI FUNCTION
# =============================================================================
create_crosstabs_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Crosstabs",
    value = ns("crosstabs_tab"),  # Add explicit value to ensure proper tab identification
    create_crosstabs_ui_internal(ns)
  )
}

# =============================================================================
# COORDINATOR SERVER FUNCTION
# =============================================================================
create_crosstabs_server <- function(id, filtered_data, reactive_color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # =========================================================================
    # REGISTER MODULE WITH GLOBAL DATA INVALIDATION SYSTEM
    # =========================================================================
    register_module("crosstabs_module", 
      ui_reset = function(session) {
        # Reset all Crosstabs UI elements to defaults
        # TODO: Add specific resets as needed
      },
      validation = function(data, selections) {
        # Validate Crosstabs selections
        if (is.null(data) || nrow(data) == 0) {
          return(list(valid = FALSE, message = "No data available for Crosstabs analysis"))
        }
        return(list(valid = TRUE, message = ""))
      }
    )
    
    # =========================================================================
    # MANUAL TAB - WORKER MODULES
    # =========================================================================
    
    # Create table worker for manual tab
    # Note: Manual tab inputs are in the worker's namespace, so we pass a dummy reactive
    # The worker will access its inputs directly
    crosstab_table_manual <- create_crosstab_table_worker(
      "crosstab_manual",
      xt_data = NULL,
      input_values = reactive(list()),  # Dummy - worker accesses inputs directly
      type = "manual"
    )
    
    # Create statistics input values reactive for manual tab
    manual_stats_input_values <- reactive({
      dims <- crosstab_table_manual$get_dimensions()
      worker_inputs <- crosstab_table_manual$get_inputs()
      list(
        counts = input$xtab_man_counts,
        ev = input$xtab_man_ev,
        stats = input$xtab_man_stats,
        R = worker_inputs$decimals,
        conf = worker_inputs$conf,
        row_label = worker_inputs$row_label,
        col_label = worker_inputs$col_label,
        dims = dims
      )
    })
    
    # Create statistics worker for manual tab
    crosstab_stats_manual <- create_crosstab_statistics_worker(
      "crosstab_stats_manual",
      table_data = crosstab_table_manual$table_data,
      input_values = manual_stats_input_values
    )
    
    # =========================================================================
    # USE DATA TAB - WORKER MODULES
    # =========================================================================
    
    # Create reactive for crosstab data from filtered data
    crosstab_data_reactive <- reactive({
      data <- filtered_data()
      req(data)
      
      col_num <- as.numeric(input$xtab_data_col)
      row_num <- as.numeric(input$xtab_data_row)
      req(col_num, row_num)
      
      tryCatch({
        xtab <- as.data.frame.matrix(table(data[[row_num]], data[[col_num]]))
        
        colnames(xtab) <- sapply(X = names(data)[col_num], 
                                 FUN = function(X) paste0(X, " = ", colnames(xtab)))
        rownames(xtab) <- sapply(X = names(data)[row_num], 
                                FUN = function(X) paste0(X, " = ", rownames(xtab)))
        
        xtab
      }, error = function(e) {
        print(paste("Error creating crosstab:", e$message))
        NULL
      })
    })
    
    # Create table worker for use data tab
    # Note: Use data tab inputs (decimals, conf) are in the worker's namespace
    crosstab_table_data <- create_crosstab_table_worker(
      "crosstab_data",
      xt_data = crosstab_data_reactive,
      input_values = reactive(list()),  # Dummy - worker accesses inputs directly
      type = "data"
    )
    
    # Create statistics input values reactive for use data tab
    data_stats_input_values <- reactive({
      dims <- crosstab_table_data$get_dimensions()
      labels <- crosstab_table_data$get_labels()
      worker_inputs <- crosstab_table_data$get_inputs()
      list(
        counts = input$xtab_dat_counts,
        ev = input$xtab_dat_ev,
        stats = input$xtab_dat_stats,
        R = worker_inputs$decimals,
        conf = worker_inputs$conf,
        row_label = if (!is.null(labels$row_labels) && length(labels$row_labels) > 0) {
          # Extract base label from first row label (remove " = ..." part)
          gsub(" = .*", "", labels$row_labels[1])
        } else {
          "Row"
        },
        col_label = if (!is.null(labels$col_labels) && length(labels$col_labels) > 0) {
          # Extract base label from first col label (remove " = ..." part)
          gsub(" = .*", "", labels$col_labels[1])
        } else {
          "Column"
        },
        dims = dims
      )
    })
    
    # Create statistics worker for use data tab
    crosstab_stats_data <- create_crosstab_statistics_worker(
      "crosstab_stats_data",
      table_data = crosstab_table_data$table_data,
      input_values = data_stats_input_values
    )
    
    # =========================================================================
    # MANUAL TAB - UI RENDERING
    # =========================================================================
    
    # Note: rhandsontable is rendered by the worker in its own namespace
    # The UI references it directly via NS("crosstab_manual", "crosstab")
    
    # Debounced table data for manual tab (prevent update loops)
    counts_debounced_manual <- debounce(reactive(crosstab_table_manual$table_data()), 1000)
    
    # Output reactiveVal for manual tab
    xtab_man_out <- reactiveVal(NULL)
    
    # Observe changes and generate output for manual tab
    observe({
      table <- crosstab_table_manual$table_data()
      dims <- crosstab_table_manual$get_dimensions()
      table_display <- crosstab_stats_manual$table_display()
      stats_output <- crosstab_stats_manual$statistics_output()
      
      req(table, table_display, dims$rows > 1, dims$cols > 1, counts_debounced_manual())
      
      # Convert table_display to HTML (matching original app formatting)
      table_html <- paste0(
        "<table border='1' style='border-collapse: collapse;'>",
        # Add column headers
        "<tr><th style='padding: 5px; text-align: left;'></th>",  # Empty <th> for row names
        paste0("<th style='padding: 5px; text-align: center;'>", colnames(table_display), "</th>", collapse = ""),
        "</tr>",
        # Add table rows with row names
        paste0(
          apply(
            cbind(RowName = rownames(table_display), table_display), 1, function(row) {
              paste0("<tr>", 
                     "<td style='padding: 5px; text-align: left; font-weight: bold;'>", row[1], "</td>",  # Row name
                     paste0("<td style='padding: 5px; text-align: right;'>", row[-1], "</td>", collapse = ""),  # Data cells
                     "</tr>")
            }
          ),
          collapse = ""
        ),
        "</table>"
      )
      
      # Combine table and statistics
      xtab_man_out(HTML(paste0(table_html, stats_output)))
    })
    
    # Render manual tab output
    output$xtab_manual_out <- renderUI({
      if (!is.null(xtab_man_out())) {
        xtab_man_out()
      }
    })
    
    # =========================================================================
    # USE DATA TAB - UI RENDERING
    # =========================================================================
    
    # Render column selectors
    output$xtab_ui_data <- renderUI({
      data <- filtered_data()
      req(data)
      
      choices <- seq(1:ncol(data))
      names(choices) <- names(data)
      
      tagList(
        pickerInput(inputId = ns("xtab_data_col"), label = "Select Columns", 
                   choices = choices, options = list(title = "Select Columns")),
        pickerInput(inputId = ns("xtab_data_row"), label = "Select Rows", 
                   choices = choices, options = list(title = "Select Rows"))
      )
    })
    
    # Render crosstab table UI for use data tab
    output$xtab_ui_data_2 <- renderUI({
      data <- filtered_data()
      col_num <- as.numeric(input$xtab_data_col)
      row_num <- as.numeric(input$xtab_data_row)
      req(data, col_num, row_num)
      
      # Create namespace function for data worker (nested under coordinator)
      ns_data <- function(id) ns(paste0("crosstab_data-", id))
      
      tagList(
        fluidRow(
          column(2, numericInput(ns_data("decimals"), label = "Decimals", 
                                value = 3, min = 0, max = 9, step = 1, width = "75px")),
          column(2, numericInput(ns_data("conf"), label = "Confidence", 
                                value = 0.95, min = 0, max = 1, step = 0.05, width = "75px"))
        ),
        h4("Crosstabs from Data"),
        rHandsontableOutput(ns("crosstab_data_proxy")),
        hr(),
        fluidRow(
          column(4, actionButton(ns_data("merge"), "Merge Selected", class = "btn-success", 
                                 title = "Select adjacent rows or columns by clicking and dragging across headers, or non-adjacent rows or columns by clicking and holding CTRL")),
          column(4, actionButton(ns_data("delete_btn"), "Delete Selected", class = "btn-danger")),
          column(4, actionButton(ns_data("reset"), "Reset Table", class = "btn-warning"))
        )
      )
    })
    
    # Render rhandsontable for use data tab in coordinator namespace
    # This proxies the worker's table data and handles edits
    output$crosstab_data_proxy <- rhandsontable::renderRHandsontable({
      table <- crosstab_table_data$table_data()
      req(table)
      
      # Store the namespace function result for selection (worker's namespace)
      worker_ns <- NS("crosstab_data")
      ns_string <- worker_ns("selection")
      
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
          });
        }", ns_string)
      )
      
      hot
    })
    
    # Handle table edits for use data tab - sync to worker
    observeEvent(input$crosstab_data_proxy, {
      if (!is.null(input$crosstab_data_proxy)) {
        updated_table <- hot_to_r(input$crosstab_data_proxy)
        # Sync to worker's table state
        crosstab_table_data$update_table_state(updated_table)
      }
    }, ignoreInit = TRUE)
    
    # Debounced table data for use data tab (prevent update loops)
    counts_debounced_data <- debounce(reactive(crosstab_table_data$table_data()), 1000)
    
    # Output reactiveVal for use data tab
    xtab_dat_out <- reactiveVal(NULL)
    
    # Observe changes and generate output for use data tab
    observe({
      table <- crosstab_table_data$table_data()
      dims <- crosstab_table_data$get_dimensions()
      table_display <- crosstab_stats_data$table_display()
      stats_output <- crosstab_stats_data$statistics_output()
      
      req(table, table_display, dims$rows > 1, dims$cols > 1, counts_debounced_data())
      
      # Convert table_display to HTML (matching original app formatting)
      table_html <- paste0(
        "<table border='1' style='border-collapse: collapse;'>",
        # Add column headers
        "<tr><th style='padding: 5px; text-align: left;'></th>",  # Empty <th> for row names
        paste0("<th style='padding: 5px; text-align: center;'>", colnames(table_display), "</th>", collapse = ""),
        "</tr>",
        # Add table rows with row names
        paste0(
          apply(
            cbind(RowName = rownames(table_display), table_display), 1, function(row) {
              paste0("<tr>", 
                     "<td style='padding: 5px; text-align: left; font-weight: bold;'>", row[1], "</td>",  # Row name
                     paste0("<td style='padding: 5px; text-align: right;'>", row[-1], "</td>", collapse = ""),  # Data cells
                     "</tr>")
            }
          ),
          collapse = ""
        ),
        "</table>"
      )
      
      # Combine table and statistics
      xtab_dat_out(HTML(paste0(table_html, stats_output)))
    })
    
    # Render use data tab output
    output$xtab_filedat_out <- renderUI({
      if (!is.null(xtab_dat_out())) {
        xtab_dat_out()
      }
    })
  })
}
