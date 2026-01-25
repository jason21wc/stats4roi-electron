# EDA (Exploratory Data Analysis) Module
# This module follows the new three-tier architecture with proper coordinator-worker separation
# and integration with global systems.

# =============================================================================
# IMPORTS
# =============================================================================
library(shiny)
library(DT)
library(ggplot2)
library(lolcat)
library(shinyWidgets)
library(bayestestR)

# Source global systems
source("modules/config/global_config.R")
source("modules/config/global_data_invalidation.R")

# Source sub-module UI components
source("modules/statistical/eda/ui/data_setup_ui.R")
source("modules/statistical/eda/ui/descriptives_ui.R")
source("modules/statistical/eda/ui/normality_tests_ui.R")
source("modules/statistical/eda/ui/histograms_ui.R")
source("modules/statistical/eda/ui/quantiles_ui.R")
source("modules/statistical/eda/ui/intervals_ui.R")
source("modules/statistical/eda/ui/natural_tolerance_ui.R")

# Source sub-module server components
source("modules/statistical/eda/server/descriptives_server.R")
source("modules/statistical/eda/server/normality_tests_server.R")
source("modules/statistical/eda/server/boxplots_server.R")
source("modules/statistical/eda/server/histograms_server.R")
source("modules/statistical/eda/server/quantiles_server.R")
source("modules/statistical/eda/server/intervals_server.R")
source("modules/statistical/eda/server/natural_tolerance_server.R")

# =============================================================================
# COORDINATOR UI FUNCTION
# =============================================================================
create_eda_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "EDA",
    tabsetPanel(
      id = ns("eda_panel"),
      
      # Data Setup Tab
      tabPanel(
        title = "Data Setup", 
        value = "eda_data_setup",
        create_data_setup_ui(ns)
      ),
      
      # Descriptives Tab
      tabPanel(
        title = "Descriptives",
        create_descriptives_ui(ns)
      ),
      
      # Normality Tests Tab
      tabPanel(
        title = "Normality Tests",
        create_normality_tests_ui(ns)
      ),
      
      # Boxplots Tab
      tabPanel(
        title = "Boxplots",
        h3("Boxplots"),
        sidebarLayout(
          sidebarPanel(
            checkboxInput(
              inputId = ns("box_violin"),
              label = "Violin Plot?",
              value = FALSE
            ),
            # Conditionally render notch checkbox
            uiOutput(ns("notch_checkbox")),
            uiOutput(ns("box_data_list"))
          ),
          mainPanel(
            # Plot controls
            tags$div(
              id = "inline1", 
              class = "inline",
              noUiSliderInput(
                inputId = ns("box_width"),
                label = "Width",
                min = 200,
                max = 1600,
                inline = TRUE,
                width = "300px",
                value = 400,
                step = 100,
                format = wNumbFormat(decimals = 0, suffix = "px")
              )
            ),
            fluidRow(
              column(1,
                noUiSliderInput(
                  inputId = ns("box_height"),
                  label = "Height",
                  min = 200,
                  max = 1600,
                  value = 400,
                  step = 100,
                  orientation = "vertical",
                  width = "100px",
                  height = "300px",
                  format = wNumbFormat(decimals = 0, suffix = "px")
                )
              ),
              column(11,
                plotOutput(ns("box_plot"), width = "auto", height = "auto")
              )
            ),
            # Plot customization and controls
            fluidRow(
              column(3,
                dropdown(
                  tags$h4("Chart Options"),
                  textInput(
                    inputId = ns("box_title"),
                    label = "Chart Title",
                    value = ""
                  ),
                  textInput(
                    inputId = ns("box_xlab"),
                    label = "X-axis Label",
                    value = ""
                  ),
                  textInput(
                    inputId = ns("box_ylab"),
                    label = "Y-axis Label",
                    value = ""
                  ),
                  circle = TRUE,
                  status = "success",
                  icon = icon("gear"),
                  width = "300px",
                  tooltip = tooltipOptions(title = "Click to modify chart")
                )
              ),
              column(3,
                downloadButtonUI(ns("boxplot"))
              ),
              column(3,
                tags$div(id="inline1", class="inline",
                  downloadSelectUI(ns("boxplot"))
                )
              ),
              column(6,
                # Additional space for future controls
              )
            )
          )
        )
      ),
      
      # Histograms Tab
      tabPanel(
        title = "Histograms",
        create_histograms_ui(ns)
      ),
      
      # Quantiles Tab
      tabPanel(
        title = "Quantiles",
        create_quantiles_ui(ns)
      ),
      
      # Intervals Tab
      tabPanel(
        title = "Intervals",
        create_intervals_ui(ns)
      ),
      
      # Natural Tolerance Tab
      tabPanel(
        title = "Nat. Tol.",
        create_natural_tolerance_ui(ns)
      )
    )
  )
}

# =============================================================================
# COORDINATOR SERVER FUNCTION
# =============================================================================
create_eda_server <- function(id, filtered_data, reactive_color_palette) {
  moduleServer(id, function(input, output, session) {
    
    # Get namespace function
    ns <- session$ns
    
    # =========================================================================
    # REGISTER MODULE WITH GLOBAL DATA INVALIDATION SYSTEM
    # =========================================================================
    register_module("eda_module", 
      ui_reset = function(session) {
        # Reset all EDA UI elements to defaults
        updatePickerInput(session, "eda_UI1", selected = character(0))
        updatePickerInput(session, "eda_UI2", selected = character(0))
        updateRadioButtons(session, "eda_data_type", selected = 1)
        updateNumericInput(session, "conf_eda", value = 0.95)
        updateNumericInput(session, "decimals_desc", value = 5)
        updateNumericInput(session, "decimals_desc2", value = 5)
        updateSwitchInput(session, "auto_norm", value = TRUE)
        updateSwitchInput(session, "auto_desc", value = TRUE)
      },
      validation = function(data, selections) {
        # Validate EDA selections
        if (is.null(data) || nrow(data) == 0) {
          return(list(valid = FALSE, message = "No data available for EDA analysis"))
        }
        if (is.null(selections$eda_UI1) || length(selections$eda_UI1) == 0) {
          return(list(valid = FALSE, message = "Please select columns or factors for analysis"))
        }
        return(list(valid = TRUE, message = ""))
      }
    )
    
    # =========================================================================
    # DATA TYPE REACTIVE (following distribution testing pattern)
    # =========================================================================
    eda_data_type <- reactive({
      input$eda_data_type
    })
    
    # =========================================================================
    # UI1 SELECTION (following distribution testing pattern exactly)
    # =========================================================================
    output$eda_UI1 <- renderUI({
      data <- filtered_data()
      req(data, ncol(data) > 0)
      
      choices <- seq_len(ncol(data))
      names(choices) <- names(data)
      data_type <- eda_data_type()
      
      if (data_type == 1) {
        # Analyze columns - show column picker
        pickerInput(
          inputId = ns("eda_UI1"),
          label = "Select Columns",
          multiple = TRUE,
          options = list(`actions-box` = TRUE),
          choices = choices
        )
      } else if (data_type == 2) {
        # Analyze by factors - show factor picker
        pickerInput(
          inputId = ns("eda_UI1"),
          label = "Select Factor(s)",
          multiple = TRUE,
          options = list(`actions-box` = TRUE),
          choices = choices
        )
      }
    })
    
    # =========================================================================
    # UI2 SELECTION (following distribution testing pattern exactly)
    # =========================================================================
    output$eda_UI2 <- renderUI({
      data <- filtered_data()
      req(data, ncol(data) > 0)
      
      choices <- seq_len(ncol(data))
      names(choices) <- names(data)
      data_type <- eda_data_type()
      UI1 <- input$eda_UI1
      
      # Only show second UI for factor analysis
      if (data_type == 2 && !is.null(UI1) && length(UI1) > 0) {
        # Analyze by factors - show data column picker (excluding selected factors)
        fact_selected <- as.numeric(UI1)
        temp <- seq_along(choices)
        temp <- temp[-fact_selected]
        choices <- choices[temp]
        
        pickerInput(
          inputId = ns("eda_UI2"),
          label = "Select Data",
          multiple = TRUE,
          options = list(`actions-box` = TRUE),
          choices = choices
        )
      }
    })
    
    # =========================================================================
    # PROCESSED DATA REACTIVE (following distribution testing pattern)
    # =========================================================================
    eda_data <- reactive({
      data <- filtered_data()
      req(data)
      
      # Apply make.names to original data (following original app pattern)
      names(data) <- make.names(names(data))
      
      data_type <- eda_data_type()
      UI1 <- input$eda_UI1
      UI2 <- input$eda_UI2
      
      if (is.null(UI1) || length(UI1) == 0) {
        return(data.frame())
      }
      
      if (data_type == 1) {
        # Column analysis - only use UI1 selection
        selected <- as.numeric(UI1)
        output <- data[, selected, drop = FALSE]
      } else if (data_type == 2) {
        # Factor analysis - return original data, let workers handle grouping
        # Workers will use summary.continuous() with formula for proper grouping
        output <- data
      } else {
        return(data.frame())
      }
      
      as.data.frame(output)
    })
    
    # =========================================================================
    # DATA PREVIEW TABLE (rendered in coordinator)
    # =========================================================================
    output$eda_selected_data <- renderDataTable({
      output <- eda_data()
      req(output)
      
      DT::datatable(output, options = list(lengthMenu = c(5, 10, 50)))
    })
    
    # =========================================================================
    # WORKER MODULE SERVER CALLS
    # =========================================================================
    
    # Descriptives worker
    descriptives_result <- create_descriptives_server("descriptives", 
      reactive(filtered_data()), 
      eda_data_type, 
      reactive({
        list(
          eda_UI1 = input$eda_UI1,
          eda_UI2 = input$eda_UI2,
          desc_stats = input$desc_stats,
          decimals_desc2 = input$decimals_desc2,
          data_list_for_desc = input$data_list_for_desc
        )
      })
    )
    
    # Normality tests worker
    normality_tests_result <- create_normality_tests_server("normality_tests", 
      reactive(filtered_data()), 
      eda_data_type, 
      reactive({
        list(
          eda_UI1 = input$eda_UI1,
          eda_UI2 = input$eda_UI2,
          decimals_desc = input$decimals_desc,
          conf_eda = input$conf_eda,
          auto_norm = input$auto_norm,
          norm_test = input$norm_test,
          data_list_for_eda = input$data_list_for_eda
        )
      })
    )
    
    # Boxplots worker
    boxplots_result <- create_boxplots_server("boxplots", 
      eda_data, 
      filtered_data, 
      eda_data_type, 
      reactive({
        width_val <- if (is.null(input$box_width)) 400 else input$box_width
        height_val <- if (is.null(input$box_height)) 400 else input$box_height
        
        # Handle conditional data_list_for_box (architectural compliance)
        data_list_for_box <- NULL
        if (input$eda_data_type == 2) {  # Factor analysis
          data_list_for_box <- input$data_list_for_box
        }
        
        list(
          eda_UI1 = input$eda_UI1,
          eda_UI2 = input$eda_UI2,
          data_list_for_box = data_list_for_box,
          box_width = width_val,
          box_height = height_val,
          box_violin = input$box_violin,
          notch_box = input$notch_box,
          box_title = input$box_title,
          box_xlab = input$box_xlab,
          box_ylab = input$box_ylab,
          box_big = input$box_big,
          box_outliers = input$box_outliers,
          box_jitter = input$box_jitter
        )
      }), 
      reactive_color_palette
    )
    
    # Histograms worker
    histograms_result <- create_histograms_server("histograms", 
      filtered_data, 
      eda_data_type, 
      reactive({
        list(
          eda_UI1 = input$eda_UI1,
          eda_UI2 = input$eda_UI2,
          data_list_for_hist = input$data_list_for_hist,
          indep_list_hist = input$indep_list_hist,
          hist_panel_filter = input$hist_panel_filter,
          hist_width = if (is.null(input$hist_width)) 400 else input$hist_width,
          hist_height = if (is.null(input$hist_height)) 400 else input$hist_height,
          hist_type = input$hist_type,
          norm_curve = input$norm_curve,
          hist_specs = input$hist_specs,
          hist_LSL = input$hist_LSL,
          hist_target = input$hist_target,
          hist_USL = input$hist_USL,
          hist_bin_w = input$hist_bin_w,
          hist_bins = input$hist_bins,
          hist_center = input$hist_center,
          hist_extend_d = input$hist_extend_d,
          hist_rug = input$hist_rug,
          mult_data_choice = input$mult_data_choice,
          hist_freq_dist = input$hist_freq_dist,
          freq_dist_dec = input$freq_dist_dec,
          hist_title = input$hist_title,
          hist_x_lab = input$hist_x_lab,
          hist_big = input$hist_big
        )
      }), 
      reactive_color_palette
    )
    
    # Quantiles worker
    quantiles_result <- create_quantiles_server("quantiles", 
      reactive(filtered_data()), 
      eda_data_type,
      reactive({
        list(
          eda_UI1 = input$eda_UI1,
          eda_UI2 = input$eda_UI2,
          decimals_quant = input$decimals_quant,
          quant_sel = input$quant_sel,
          quant_cust = input$quant_cust,
          data_list_for_quant = input$data_list_for_quant
        )
      })
    )
    
    # Intervals worker
    intervals_result <- create_intervals_server("intervals", 
      reactive(filtered_data()), 
      eda_data_type,
      reactive({
        list(
          eda_UI1 = input$eda_UI1,
          eda_UI2 = input$eda_UI2,
          conf_ci = input$conf_ci,
          decimals_ci = input$decimals_ci,
          interval_type = input$interval_type,
          interval_b_type = input$interval_b_type,
          data_list_for_ci = input$data_list_for_ci,
          ci_info = input$ci_info
        )
      })
    )
    
    # Natural tolerance worker (for both Use Data and Enter Statistics tabs)
    natural_tolerance_result <- create_natural_tolerance_server("natural_tolerance", 
      reactive(filtered_data()), 
      eda_data_type,
      reactive({
        list(
          # Data-based inputs (for Use Data tab)
          eda_UI1 = input$eda_UI1,
          eda_UI2 = input$eda_UI2,
          decimals_nt_data = input$decimals_nt_data,
          dist_nt_data = input$dist_nt_data,
          data_list_for_nt = input$data_list_for_nt,
          # Enter Statistics inputs
          decimals_nt = input$decimals_nt,
          dist_nt = input$dist_nt,
          UI1_nt = input$UI1_nt,
          UI2_nt = input$UI2_nt
        )
      })
    )
    
    # =========================================================================
    # RENDER WORKER OUTPUTS (coordinator handles all rendering)
    # =========================================================================
    
    # Descriptives data list UI
    output$desc_data_list <- renderUI({
      data <- eda_data()
      data_type <- eda_data_type()
      UI2 <- input$eda_UI2
      
      if (data_type == 1) {
        # Column analysis - no additional data selection needed
        return(NULL)
      } else if (data_type == 2) {
        # Factor analysis - show radio buttons for dependent variable selection
        if (!is.null(UI2) && length(UI2) > 0) {
          # Use the actual selected data columns from UI2
          selected_data_cols <- as.numeric(UI2)
          data_cols <- data[, selected_data_cols, drop = FALSE]
          if (ncol(data_cols) > 0) {
            choices <- seq_len(ncol(data_cols))
            names(choices) <- names(data_cols)
            radioButtons(
              inputId = ns("data_list_for_desc"),
              label = "Select data:",
              choices = choices
            )
          }
        }
      }
    })
    
    # Descriptives table
    output$desc_out <- renderDT({
      result <- descriptives_result$descriptives_data()
      req(result)
      
      DT::datatable(result, options = list(paging = FALSE))
    })
    
    # Normality tests data list UI
    output$eda_data_list <- renderUI({
      data <- eda_data()
      data_type <- eda_data_type()
      UI2 <- input$eda_UI2
      
      if (data_type == 1) {
        return(NULL)
      } else       if (data_type == 2) {
        if (!is.null(UI2) && length(UI2) > 0) {
          # Use the actual selected data columns from UI2
          selected_data_cols <- as.numeric(UI2)
          data_cols <- data[, selected_data_cols, drop = FALSE]
          if (ncol(data_cols) > 0) {
            choices <- seq_len(ncol(data_cols))
            names(choices) <- names(data_cols)
            radioButtons(
              inputId = ns("data_list_for_eda"),
              label = "Select data:",
              choices = choices
            )
          }
        }
      }
    })
    
    # Normality tests selected data name UI
    output$sel_data_name <- renderUI({
      data <- eda_data()
      data_type <- eda_data_type()
      UI2 <- input$eda_UI2
      
      if (data_type == 2) {
        # Use the actual selected data columns from UI2
        selected_data_cols <- as.numeric(UI2)
        data_cols <- data[, selected_data_cols, drop = FALSE]
        data_col <- input$data_list_for_eda
        if (!is.null(data_col) && ncol(data_cols) > 0) {
          h4(paste("Selected data:", names(data_cols)[as.numeric(data_col)]))
        }
      }
    })
    
    # Normality tests table
    output$eda_desc_out <- renderDT({
      result <- normality_tests_result$normality_data()
      req(result)
      
      # Create the table
      table <- DT::datatable(
        result$data,
        options = list(paging = FALSE)
      )
      
      # Apply formatting if p_columns exist
      if (length(result$p_columns) > 0) {
        table <- table %>% 
          formatStyle(
            result$p_columns,
            backgroundColor = styleInterval(
              cuts = 1 - result$confidence,
              values = c('yellow', 'lightgray')
            )
          )
      }
      
      table
    })
    

    # Notch checkbox - only show when violin plot is not selected
    output$notch_checkbox <- renderUI({
      if (is.null(input$box_violin) || !input$box_violin) {
        checkboxInput(
          inputId = ns("notch_box"),
          label = "Use notch?",
          value = FALSE
        )
      } else {
        # Return empty div when violin plot is selected
        div()
      }
    })
    
    # Boxplots data list UI (following descriptives pattern exactly)
    output$box_data_list <- renderUI({
      data <- filtered_data()
      data_type <- input$eda_data_type
      UI1 <- input$eda_UI1
      UI2 <- input$eda_UI2
      
      if (data_type == 1) {
        # Column analysis - no additional data selection needed
        return(NULL)
      } else if (data_type == 2) {
        # Factor analysis - show radio buttons for dependent variable selection
        if (!is.null(UI2) && length(UI2) > 0) {
          # Use the actual selected data columns from UI2
          selected_data_cols <- as.numeric(UI2)
          data_cols <- data[, selected_data_cols, drop = FALSE]
          if (ncol(data_cols) > 0) {
            choices <- seq_len(ncol(data_cols))
            names(choices) <- names(data_cols)
            radioButtons(
              inputId = ns("data_list_for_box"),
              label = "Select data:",
              choices = choices
            )
          }
        }
      }
    })
    
    
    # Boxplots plot
    output$box_plot <- renderPlot({
      plot_result <- boxplots_result$box_plot()
      plot_result
    }, width = boxplots_result$box_width, height = boxplots_result$box_height)
    
    # Boxplots download server (in coordinator, following architectural pattern)
    # Download dimensions (following distribution module pattern)
    download_width <- reactive({
      width_val <- boxplots_result$box_width()
      if (is.null(width_val)) return(400 * 4)  # Default 1600px
      width_val * 4  # Scale up for high-quality download
    })
    
    download_height <- reactive({
      height_val <- boxplots_result$box_height()
      if (is.null(height_val)) return(400 * 4)  # Default 1600px
      height_val * 4  # Scale up for high-quality download
    })
    
    downloadServer("boxplot", boxplots_result$box_plot,
                  width = download_width, 
                  height = download_height)
    
    # Histograms download server (following architectural pattern)
    hist_download_width <- reactive({
      width_val <- histograms_result$hist_width()
      if (is.null(width_val)) return(400 * 4)  # Default 1600px
      width_val * 4  # Scale up for high-quality download
    })
    
    hist_download_height <- reactive({
      height_val <- histograms_result$hist_height()
      if (is.null(height_val)) return(400 * 4)  # Default 1600px
      height_val * 4  # Scale up for high-quality download
    })
    
    downloadServer("histogram", histograms_result$hist_plot,
                  width = hist_download_width, 
                  height = hist_download_height)
    
    # Histograms data list UI
    output$hist_data_list <- renderUI({
      data <- eda_data()
      data_type <- eda_data_type()
      UI1 <- input$eda_UI1
      UI2 <- input$eda_UI2
      
      if (data_type == 1) {
        return(NULL)
      }
      
      if (data_type == 2) {
        if (!is.null(UI2) && length(UI2) > 0) {
          # Use the actual selected data columns from UI2
          selected_data_cols <- as.numeric(UI2)
          data_cols <- data[, selected_data_cols, drop = FALSE]
          if (ncol(data_cols) > 0) {
            choices <- seq_len(ncol(data_cols))
            names(choices) <- names(data_cols)
            radioButtons(
              inputId = ns("data_list_for_hist"),
              label = "Select data:",
              choices = choices
            )
          }
        }
      }
    })
    
    # Histograms independent variable list UI (for factor analysis)
    output$hist_indep_list <- renderUI({
      data <- eda_data()
      data_type <- eda_data_type()
      UI1 <- input$eda_UI1
      
      if (data_type == 1) {
        return(NULL)
      }
      
      if (data_type == 2) {
        if (!is.null(UI1) && length(UI1) > 0) {
          # Create factor combinations
          factors <- as.numeric(UI1)
          factor_data <- data[, factors, drop = FALSE]
          factor_combos <- unique(interaction(factor_data, sep = ", "))
          
          if (length(factor_combos) > 0) {
            pickerInput(
              inputId = ns("indep_list_hist"),
              label = "Select factor combinations:",
              choices = factor_combos,
              selected = factor_combos,  # Select all by default
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectAllText = "Select All",
                deselectAllText = "Deselect All"
              )
            )
          }
        }
      }
    })
    
    # Multiple data choice UI
    output$mult_data <- renderUI({
      data <- eda_data()
      data_type <- eda_data_type()
      UI1 <- input$eda_UI1
      
      if (data_type == 1) {
        # For column analysis, show if multiple columns selected
        if (!is.null(UI1) && length(UI1) > 1) {
          checkboxInput(
            inputId = ns("mult_data_choice"),
            label = "Multiple Data on One Axis",
            value = TRUE
          )
        }
      } else {
        # For factor analysis, always show
        checkboxInput(
          inputId = ns("mult_data_choice"),
          label = "Multiple Data on One Axis",
          value = TRUE
        )
      }
    })
    
    
    # Histograms plot
    output$hist_plot <- renderPlot({
      plot_result <- histograms_result$hist_plot()
      plot_result
    }, width = histograms_result$hist_width, height = histograms_result$hist_height)
    
    # Histograms panel selector for frequency distribution table
    output$hist_panel_select <- renderUI({
      # Use original data (filtered_data) for column names, not processed data (eda_data)
      original_data <- filtered_data()
      data_type <- eda_data_type()
      UI1 <- input$eda_UI1
      UI2 <- input$eda_UI2
      
      req(UI1, data_type, original_data)
      
      # Add safety checks for NA values
      if (any(is.na(UI1))) {
        return(NULL)
      }
      
      if (data_type == 1) {
        # Column analysis - one panel per selected column
        # Convert UI1 to numeric for proper indexing
        UI1_numeric <- as.numeric(UI1)
        
        if (length(UI1_numeric) > 0 && all(UI1_numeric > 0) && all(UI1_numeric <= ncol(original_data))) {
          panel_list <- seq(1, length(UI1_numeric))
          column_names <- names(original_data)[UI1_numeric]  # Use numeric indices
          if (any(is.na(column_names))) {
            return(NULL)
          }
          names(panel_list) <- column_names
        } else {
          return(NULL)
        }
      } else {
        # Factor analysis - one panel per factor combination
        data_col <- input$data_list_for_hist
        req(data_col, input$indep_list_hist)
        
        # Add safety check for data_col
        if (is.na(data_col)) {
          return(NULL)
        }
        
        selected_combos <- input$indep_list_hist
        if (!is.null(selected_combos) && length(selected_combos) > 0 && !any(is.na(selected_combos))) {
          panel_list <- seq(1, length(selected_combos))
          names(panel_list) <- selected_combos
        } else {
          return(NULL)
        }
      }
      
      selectInput(
        inputId = ns("hist_panel_filter"),
        label = "Select Panel to View",
        choices = panel_list
      )
    })
    
    # Histograms frequency distribution table
    output$hist_freq_table <- renderDT({
      table_result <- histograms_result$hist_freq_table()
      req(table_result)
      DT::datatable(table_result, options = list(paging = FALSE))
    })
    
    # Quantiles data list rendering
    output$quant_data_list <- renderUI({
      data_type <- eda_data_type()
      UI1 <- input$eda_UI1
      UI2 <- input$eda_UI2
      
      if (data_type == 1) {
        # Column analysis - no data selection needed
        return(NULL)
      }
      
      if (data_type == 2) {
        # Factor analysis - show data selection
        select <- as.numeric(unlist(strsplit(x = UI2, split = "\\s+")))
        names(UI2) <- names(eda_data())[select]
        radioButtons(
          inputId = ns("data_list_for_quant"),
          label = "Select data:",
          choices = UI2
        )
      }
    })
    
    # Quantiles table
    output$quant_out <- renderDT({
      result <- quantiles_result$quantiles_data()
      req(result)
      DT::datatable(result, options = list(paging = FALSE))
    })
    
    # Intervals data list rendering
    output$ci_data_list <- renderUI({
      data_type <- eda_data_type()
      UI1 <- input$eda_UI1
      UI2 <- input$eda_UI2
      
      if (data_type == 1) {
        # Column analysis - no data selection needed
        return(NULL)
      }
      
      if (data_type == 2) {
        # Factor analysis - show data selection
        select <- as.numeric(unlist(strsplit(x = UI2, split = "\\s+")))
        names(UI2) <- names(eda_data())[select]
        radioButtons(
          inputId = ns("data_list_for_ci"),
          label = "Select data:",
          choices = UI2
        )
      }
    })
    
    # Intervals title rendering
    output$interval_title <- renderUI({
      int_type <- input$interval_type
      conf <- input$conf_ci
      b_type <- input$interval_b_type
      
      if (int_type == 1) {
        title <- paste0(100 * conf, "% Confidence Intervals - Assuming Normality, limits based on the t distribution")
      } else {
        title <- paste0("Bayesian Bootstrap ", 100 * conf, "% Credible Intervals - ", b_type, " Method")
      }
      
      h4(title)
    })
    
    # Intervals table
    output$ci_out <- renderDT({
      result <- intervals_result$intervals_data()
      req(result)
      DT::datatable(result, options = list(paging = FALSE))
    })
    
    # About CIs popup
    observeEvent(input$ci_info, {
      ci_info <- input$ci_info
      if (!ci_info) { return() }
      
      title <- "About Intervals"
      text_out <- HTML("A critical part of data analysis is quantifying how much precision there is in your estimate of a parameter, which can be shown by using an interval around the point estimate. stats4ROI calculates two different types of intervals.<br><br><b><u>Confidence Intervals</u></b> are a range around your point estimate that, if you were to repeat that procedure many times, would capture the true parameter with some level of confidence. This often confuses people, as a confidence interval is <u>not</u> an interval that contains the true value of the parameter with some level of confidence.<br><br><b><u>Credible Intervals</u></b> describes the uncertainty related to your estimate of an unknown parameter (Makowski, et.al, 2019). A credible interval can be said to have a stated chance of containing the true population parameter being estimated and is probably more useful for practical applications. The technique used here is a bootstrapped estimate as introduced by Rubin (1981), so it is non-parametric.<br><br>See <a href='https://easystats.github.io/bayestestR/articles/credible_interval.html'>Credible Intervals</a> for a more in-depth discussion and an explanation as to the two different options for credible interval types.")
      
      sendSweetAlert(
        title = title,
        text = HTML(text_out),
        html = TRUE,
        showCloseButton = TRUE,
        btn_labels = "Close",
        type = "info"
      )
      updateCheckboxInput(inputId = ns("ci_info"), value = FALSE)
    })
    
    # Natural tolerance data list UI (for Use Data tab, type=2) - matching original app lines 18500-18515
    output$nt_data_list <- renderUI({
      data <- filtered_data()
      data_type <- input$eda_data_type  # 1=columns 2=factor
      UI1 <- input$eda_UI1  # columns if column data, factors if factor data
      UI2 <- input$eda_UI2  # selected data column numbers if factors
      
      if (data_type == 1) {
        output <- NULL
      }
      
      if (data_type == 2) {
        select <- as.numeric(unlist(strsplit(x = UI2, split = "\\s+")))
        names(UI2) <- names(data)[select]
        output <- radioButtons(
          inputId = ns("data_list_for_nt"),
          label = "Select data:",
          choices = UI2
        )
      }
      
      output
    })
    
    # Natural tolerance output table (Use Data tab) - matching original app lines 18412-18497
    output$nt_out_data <- renderDT({
      result <- natural_tolerance_result$natural_tolerance_data()
      req(result)
      if (nrow(result) == 0) {
        return(DT::datatable(data.frame(Message = "No data available"), options = list(paging = FALSE)))
      }
      DT::datatable(result, options = list(paging = FALSE))
    })
    
    # Natural tolerance UI1 (Enter Statistics tab) - matching original app lines 18517-18539
    output$nt_UI1 <- renderUI({
      distr <- input$dist_nt
      
      if (distr == 1) {
        output <- numericInput(inputId = ns("UI1_nt"), label = "Mean", value = 0)
      }
      if (distr == 2) {
        output <- numericInput(inputId = ns("UI1_nt"), label = "Mean", value = 50)
      }
      if (distr == 3) {
        output <- numericInput(inputId = ns("UI1_nt"), label = "n", value = 10)
      }
      if (distr == 4) {
        output <- numericInput(inputId = ns("UI1_nt"), label = "lambda", value = 10)
      }
      if (distr == 5) {
        output <- numericInput(inputId = ns("UI1_nt"), label = "df", value = 10)
      }
      
      output
    })
    
    # Natural tolerance UI2 (Enter Statistics tab) - matching original app lines 18541-18561
    output$nt_UI2 <- renderUI({
      distr <- input$dist_nt
      
      if (distr == 1) {
        output <- numericInput(inputId = ns("UI2_nt"), label = "Standard Deviation", value = 1)
      }
      if (distr == 2) {
        output <- numericInput(inputId = ns("UI2_nt"), label = "Minimum", value = 0)
      }
      if (distr == 3) {
        output <- numericInput(inputId = ns("UI2_nt"), label = "Probability", value = 0.5)
      }
      if (distr == 4) {
        output <- NULL
      }
      if (distr == 5) {
        output <- numericInput(inputId = ns("UI2_nt"), label = "ncp", value = 0)
      }
      
      output
    })
    
    # Natural tolerance simple table (Enter Statistics tab) - using DT to match other EDA outputs
    output$nt_out_simple <- renderDT({
      result <- natural_tolerance_result$natural_tolerance_simple()
      req(result)
      if (nrow(result) == 0) {
        return(DT::datatable(data.frame(Message = "No data available"), options = list(paging = FALSE)))
      }
      DT::datatable(result, options = list(paging = FALSE))
    })
    
    # =========================================================================
    # RETURN PROCESSED DATA FOR EXTERNAL USE
    # =========================================================================
    return(list(eda_data = eda_data))
  })
}

# =============================================================================
# ARCHITECTURAL PATTERNS USED
# =============================================================================
# 1. Coordinator-Worker Separation: This coordinator manages UI and data flow
# 2. Explicit Data Flow: Data passed as parameters to worker modules
# 3. Namespace Management: All UI rendering happens in coordinator
# 4. Global System Integration: Uses global data invalidation and config
# 5. Template Compliance: Follows established patterns from working modules
# 6. No Over-Engineering: Simple reactive functions, no complex observers