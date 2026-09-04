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

# Shared EDA table helpers
source("modules/statistical/eda/utils/pooled_all_row.R")
source("modules/statistical/eda/utils/quantile_types.R")
source("modules/statistical/eda/ui/quantile_type_ui.R")
source("modules/statistical/eda/utils/interval_plot_helpers.R")
source("modules/statistical/eda/utils/data_invalidation_helpers.R")
source("modules/statistical/eda/utils/eda_helpers.R")

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
                div(
                  style = "position:relative; display:inline-block; overflow:visible;",
                  plotOutput(
                    ns("box_plot"),
                    width = "auto",
                    height = "auto",
                    hover = hoverOpts(ns("box_hover"), delay = 100, delayType = "debounce")
                  ),
                  uiOutput(ns("hover_info_box"), style = "pointer-events: none;")
                )
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
    # DATA INVALIDATION (new data load clears data-driven UI and stale outputs)
    # =========================================================================
    eda_data_trigger <- create_data_invalidation_trigger(filtered_data)
    
    # Read input$ inside the reactive (not at call time) so values refresh after invalidation
    eda_worker_inputs <- function(builder) {
      reactive({
        eda_data_trigger()
        builder()
      })
    }
    
    # =========================================================================
    # REGISTER MODULE WITH GLOBAL DATA INVALIDATION SYSTEM
    # =========================================================================
    register_module("eda_module", 
      ui_reset = function() {
        reset_eda_data_driven_ui(session)
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
      eda_data_trigger()
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
    output$eda_selected_data <- renderDT({
      eda_data_trigger()
      preview_data <- eda_data()
      req(preview_data)
      eda_datatable(preview_data, options = list(lengthMenu = c(5, 10, 50)))
    })
    
    # =========================================================================
    # WORKER MODULE SERVER CALLS
    # =========================================================================
    
    # Descriptives worker
    descriptives_result <- create_descriptives_server("descriptives", 
      reactive(filtered_data()), 
      eda_data_type, 
      eda_worker_inputs(function() list(
        eda_UI1 = input$eda_UI1,
        eda_UI2 = input$eda_UI2,
        desc_stats = input$desc_stats,
        desc_quantile_type = input$desc_quantile_type,
        decimals_desc2 = input$decimals_desc2,
        data_list_for_desc = input$data_list_for_desc
      ))
    )
    
    # Normality tests worker
    normality_tests_result <- create_normality_tests_server("normality_tests", 
      reactive(filtered_data()), 
      eda_data_type, 
      eda_worker_inputs(function() list(
        eda_UI1 = input$eda_UI1,
        eda_UI2 = input$eda_UI2,
        decimals_desc = input$decimals_desc,
        conf_eda = input$conf_eda,
        auto_norm = input$auto_norm,
        norm_test = input$norm_test,
        data_list_for_eda = input$data_list_for_eda
      ))
    )
    
    # Boxplots worker
    boxplots_result <- create_boxplots_server("boxplots", 
      eda_data, 
      filtered_data, 
      eda_data_type, 
      eda_worker_inputs(function() list(
        eda_UI1 = input$eda_UI1,
        eda_UI2 = input$eda_UI2,
        data_list_for_box = if (as.integer(input$eda_data_type) == 2L) input$data_list_for_box else NULL,
        box_width = if (is.null(input$box_width)) 400 else input$box_width,
        box_height = if (is.null(input$box_height)) 400 else input$box_height,
        box_violin = input$box_violin,
        notch_box = input$notch_box,
        box_title = input$box_title,
        box_xlab = input$box_xlab,
        box_ylab = input$box_ylab,
        box_big = input$box_big,
        box_outliers = input$box_outliers,
        box_jitter = input$box_jitter
      )), 
      reactive_color_palette
    )
    
    # Histograms worker
    histograms_result <- create_histograms_server("histograms", 
      filtered_data, 
      eda_data_type, 
      eda_worker_inputs(function() list(
        eda_UI1 = input$eda_UI1,
        eda_UI2 = input$eda_UI2,
        data_list_for_hist = input$data_list_for_hist,
        indep_list_hist = input$indep_list_hist,
        hist_panel_filter = input$hist_panel_filter,
        hist_width = if (is.null(input$hist_width)) 400 else input$hist_width,
        hist_height = if (is.null(input$hist_height)) 400 else input$hist_height,
        hist_type = input$hist_type,
        norm_curve = input$norm_curve,
        hist_freq_y_axis = input$hist_freq_y_axis,
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
        combine_data_choice = input$combine_data_choice,
        hist_freq_dist = input$hist_freq_dist,
        freq_dist_dec = input$freq_dist_dec,
        hist_title = input$hist_title,
        hist_x_lab = input$hist_x_lab,
        hist_big = input$hist_big
      )), 
      reactive_color_palette
    )
    
    # Quantiles worker
    quantiles_result <- create_quantiles_server("quantiles", 
      reactive(filtered_data()), 
      eda_data_type,
      eda_worker_inputs(function() list(
        eda_UI1 = input$eda_UI1,
        eda_UI2 = input$eda_UI2,
        decimals_quant = input$decimals_quant,
        quantile_type = input$quantile_type,
        quant_sel = input$quant_sel,
        quant_cust = input$quant_cust,
        data_list_for_quant = input$data_list_for_quant
      ))
    )
    
    # Intervals worker
    intervals_result <- create_intervals_server("intervals", 
      reactive(filtered_data()), 
      eda_data_type,
      eda_worker_inputs(function() list(
        eda_UI1 = input$eda_UI1,
        eda_UI2 = input$eda_UI2,
        conf_ci = input$conf_ci,
        decimals_ci = input$decimals_ci,
        interval_type = input$interval_type,
        interval_b_type = input$interval_b_type,
        data_list_for_ci = input$data_list_for_ci,
        ci_info = input$ci_info,
        ci_show_plot = input$ci_show_plot,
        ci_plot_param = input$ci_plot_param,
        ci_plot_width = if (is.null(input$ci_plot_width)) 400 else input$ci_plot_width,
        ci_plot_height = if (is.null(input$ci_plot_height)) 400 else input$ci_plot_height,
        ci_font_size = input$ci_font_size,
        ci_plot_title = input$ci_plot_title,
        ci_plot_xlab = input$ci_plot_xlab,
        ci_plot_ylab = input$ci_plot_ylab
      )),
      reactive_color_palette
    )
    
    # Natural tolerance worker (for both Use Data and Enter Statistics tabs)
    natural_tolerance_result <- create_natural_tolerance_server("natural_tolerance", 
      reactive(filtered_data()), 
      eda_data_type,
      eda_worker_inputs(function() list(
        eda_UI1 = input$eda_UI1,
        eda_UI2 = input$eda_UI2,
        decimals_nt_data = input$decimals_nt_data,
        dist_nt_data = input$dist_nt_data,
        data_list_for_nt = input$data_list_for_nt,
        decimals_nt = input$decimals_nt,
        dist_nt = input$dist_nt,
        UI1_nt = input$UI1_nt,
        UI2_nt = input$UI2_nt
      ))
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
      eda_data_trigger()
      result <- descriptives_result$descriptives_data()
      req(result)
      
      eda_datatable(result, options = list(paging = FALSE))
    })

    output$desc_quantile_type_help <- render_quantile_type_help(
      reactive(normalize_quantile_type(input$desc_quantile_type))
    )

    output$quantile_type_help <- render_quantile_type_help(
      reactive(normalize_quantile_type(input$quantile_type))
    )
    
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
      eda_data_trigger()
      result <- normality_tests_result$normality_data()
      req(result)
      
      # Create the table
      table <- eda_datatable(
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
      eda_data_trigger()
      boxplots_result$box_plot()
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

    # Boxplots hover tooltip (mirrors Intervals hover_info_ci pattern)
    output$hover_info_box <- renderUI({
      # No tooltip in violin mode
      if (isTRUE(input$box_violin)) return(NULL)

      hover <- input$box_hover
      if (is.null(hover)) return(NULL)

      hover_data <- isolate(boxplots_result$box_plot_hover_data())
      if (is.null(hover_data)) return(NULL)

      fence_samples  <- hover_data$fence_samples
      outlier_points <- hover_data$outlier_points

      ro <- get_global_config()$ro
      R  <- 4  # default rounding digits for boxplot hover

      # --- Outlier hit-test takes precedence ---
      if (!is.null(outlier_points) && nrow(outlier_points) > 0) {
        near_outliers <- nearPoints(
          df        = outlier_points,
          coordinfo = hover,
          xvar      = "ID",
          yvar      = "Data",
          threshold = 25,
          maxpoints = 20,
          addDist   = TRUE
        )
        if (nrow(near_outliers) > 0) {
          left_px <- hover$coords_css$x
          top_px  <- hover$coords_css$y
          style <- paste0(
            "position:absolute; z-index:1000; background-color: rgba(245, 245, 245, 0.92); ",
            "left:", left_px + 12, "px; top:", top_px + 12, "px; ",
            "padding:6px 10px; border:1px solid #ccc; border-radius:4px; ",
            "box-shadow:0 1px 4px rgba(0,0,0,0.2); white-space:nowrap;"
          )
          # Show group header + outlier values
          grp <- as.character(near_outliers$ID[1])
          kind_label <- ifelse(near_outliers$outlier_kind == "wild",
                               "Extreme Outlier", "Outlier")
          vals <- paste0(
            "<b>", kind_label, ": </b>", ro(near_outliers$Data, R),
            collapse = "<br/>"
          )
          return(div(
            style = style,
            HTML(paste0(
              "<div style='text-align:center; font-weight:bold; margin-bottom:4px;'>",
              grp, "</div>", vals
            ))
          ))
        }
      }

      # --- Fence / box-whisker hit-test ---
      if (!is.null(fence_samples) && nrow(fence_samples) > 0) {
        near_fence <- nearPoints(
          df        = fence_samples,
          coordinfo = hover,
          xvar      = "ID",
          yvar      = "Data",
          threshold = 25,
          maxpoints = 1,
          addDist   = TRUE
        )
        if (nrow(near_fence) > 0) {
          left_px <- hover$coords_css$x
          top_px  <- hover$coords_css$y
          style <- paste0(
            "position:absolute; z-index:1000; background-color: rgba(245, 245, 245, 0.92); ",
            "left:", left_px + 12, "px; top:", top_px + 12, "px; ",
            "padding:6px 10px; border:1px solid #ccc; border-radius:4px; ",
            "box-shadow:0 1px 4px rgba(0,0,0,0.2); white-space:nowrap;"
          )
          s <- near_fence[1, ]
          return(div(
            style = style,
            HTML(paste0(
              "<div style='text-align:center; font-weight:bold; margin-bottom:4px;'>",
              as.character(s$ID), "</div>",
              "<b>Median: </b>",       ro(s$Median, R),       "<br/>",
              "<b>Q1: </b>",           ro(s$Q1, R),           "<br/>",
              "<b>Q3: </b>",           ro(s$Q3, R),           "<br/>",
              "<b>IQR: </b>",          ro(s$IQR, R),          "<br/>",
              "<b>Inner Fence Lower: </b>", ro(s$Inner_Lower, R), "<br/>",
              "<b>Inner Fence Upper: </b>", ro(s$Inner_Upper, R), "<br/>",
              "<b>Outer Fence Lower: </b>", ro(s$Outer_Lower, R), "<br/>",
              "<b>Outer Fence Upper: </b>", ro(s$Outer_Upper, R)
            ))
          ))
        }
      }

      NULL
    })
    outputOptions(output, "hover_info_box", suspendWhenHidden = FALSE)
    
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
    
    # Histogram/KDE: combine on one plot + shared x-axis across facets (not for frequency polygon)
    output$mult_data <- renderUI({
      data_type <- eda_data_type()
      UI1 <- input$eda_UI1
      hist_type <- input$hist_type
      if (is.null(hist_type)) hist_type <- 1
      
      multiple_series <- FALSE
      if (data_type == 1) {
        multiple_series <- !is.null(UI1) && length(UI1) > 1
      } else {
        indep <- input$indep_list_hist
        multiple_series <- !is.null(indep) && length(indep) > 1
      }
      
      if (!multiple_series || as.numeric(hist_type) == 2) {
        return(NULL)
      }
      
      tagList(
        checkboxInput(
          inputId = ns("combine_data_choice"),
          label = "Combine all selected data on one plot",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.combine_data_choice == false",
          ns = ns,
          checkboxInput(
            inputId = ns("mult_data_choice"),
            label = "Same x-axis on all facet panels",
            value = TRUE
          )
        )
      )
    })
    
    
    # Histograms plot
    output$hist_plot <- renderPlot({
      eda_data_trigger()
      plot_result <- histograms_result$hist_plot()
      plot_result
    }, width = histograms_result$hist_width, height = histograms_result$hist_height)
    
    # Histograms panel selector for frequency distribution table
    output$hist_panel_select <- renderUI({
      if (isTRUE(input$combine_data_choice)) {
        return(NULL)
      }
      if (identical(as.numeric(input$hist_type), 2L)) {
        return(NULL)
      }
      
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
      eda_data_trigger()
      table_result <- histograms_result$hist_freq_table()
      req(table_result)
      eda_datatable(table_result, options = list(paging = FALSE))
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
      eda_data_trigger()
      result <- quantiles_result$quantiles_data()
      req(result)
      eda_datatable(result, options = list(paging = FALSE))
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
    
    # Intervals plot
    output$ci_plot <- renderPlot({
      eda_data_trigger()
      req(input$ci_show_plot)
      intervals_result$ci_plot()
    }, width = intervals_result$ci_plot_width, height = intervals_result$ci_plot_height)

    ci_download_width <- reactive({
      width_val <- intervals_result$ci_plot_width()
      if (is.null(width_val)) return(400 * 4)
      width_val * 4
    })

    ci_download_height <- reactive({
      height_val <- intervals_result$ci_plot_height()
      if (is.null(height_val)) return(400 * 4)
      height_val * 4
    })

    downloadServer("ciplot", intervals_result$ci_plot,
                  width = ci_download_width,
                  height = ci_download_height)

    output$hover_info_ci <- renderUI({
      req(input$ci_show_plot)
      R <- input$decimals_ci
      hover <- input$ci_hover
      if (is.null(hover)) {
        return(NULL)
      }

      hover_data <- isolate(intervals_result$ci_plot_hover_data())
      req(hover_data)

      point <- nearPoints(
        df = hover_data,
        coordinfo = hover,
        xvar = "x_pos",
        yvar = "y_hover",
        threshold = 25,
        maxpoints = 1
      )
      if (nrow(point) == 0) {
        point <- find_nearest_interval_hover_point(hover_data, hover)
      }
      if (is.null(point) || nrow(point) == 0) {
        return(NULL)
      }

      left_px <- hover$coords_css$x
      top_px <- hover$coords_css$y
      style <- paste0(
        "position:absolute; z-index:1000; background-color: rgba(245, 245, 245, 0.92); ",
        "left:", left_px + 12, "px; top:", top_px + 12, "px; ",
        "padding:6px 10px; border:1px solid #ccc; border-radius:4px; ",
        "box-shadow:0 1px 4px rgba(0,0,0,0.2); white-space:nowrap;"
      )

      ro <- get_global_config()$ro
      param_label <- point$param_label[1]

      div(
        style = style,
        HTML(paste0(
          "<div style='text-align:center; font-weight:bold; margin-bottom:4px;'>",
          point$group[1],
          "</div>",
          "<b>", param_label, ": </b>", ro(point$estimate[1], R), "<br/>",
          "<b>CI_low: </b>", ro(point$ci_low[1], R), "<br/>",
          "<b>CI_high: </b>", ro(point$ci_high[1], R)
        ))
      )
    })
    outputOptions(output, "hover_info_ci", suspendWhenHidden = FALSE)

    # Intervals table
    output$ci_out <- renderDT({
      eda_data_trigger()
      result <- intervals_result$intervals_data()
      req(result)
      eda_datatable(result, options = list(paging = FALSE))
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
      eda_data_trigger()
      result <- natural_tolerance_result$natural_tolerance_data()
      req(result)
      if (nrow(result) == 0) {
        return(eda_datatable(data.frame(Message = "No data available"), options = list(paging = FALSE)))
      }
      eda_datatable(result, options = list(paging = FALSE))
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
        return(eda_datatable(data.frame(Message = "No data available"), options = list(paging = FALSE)))
      }
      eda_datatable(result, options = list(paging = FALSE))
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