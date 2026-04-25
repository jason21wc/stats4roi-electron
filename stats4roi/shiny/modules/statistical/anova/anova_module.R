# ANOVA Module
# This module follows the three-tier architecture with proper coordinator-worker separation
# and integration with global systems.

# =============================================================================
# IMPORTS
# =============================================================================
library(shiny)
library(lolcat)
library(DT)
library(ggplot2)
library(shinyWidgets)
library(dplyr)
library(stringr)

# Source global systems
source("modules/config/global_config.R")
source("modules/config/global_data_invalidation.R")

# Source helper functions
source("modules/statistical/anova/utils/anova_helpers.R")
source("modules/statistical/anova/utils/emsanova_roi_overrides.R")

# Source worker modules
source("modules/statistical/anova/server/oneway_anova_server.R")
source("modules/statistical/anova/server/oneway_posthoc_server.R")
source("modules/statistical/anova/server/multifactor_anova_server.R")

# Source UI modules
source("modules/statistical/anova/ui/oneway_anova_ui.R")
source("modules/statistical/anova/ui/multifactor_anova_ui.R")
# Multi-Factor workers will be added as they are completed

# =============================================================================
# COORDINATOR UI FUNCTION
# =============================================================================
create_anova_ui <- function(id) {
  ns <- NS(id)
  
  navbarMenu(
    title = "ANOVA",
    tabPanel(
      title = "Oneway",
      create_oneway_anova_ui_internal(ns)
    ),
    tabPanel(
      title = "Multi-Factor",
      create_multifactor_anova_ui_internal(ns)
    )
  )
}

# =============================================================================
# COORDINATOR SERVER FUNCTION
# =============================================================================
create_anova_server <- function(id, filtered_data, reactive_color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # =========================================================================
    # REGISTER MODULE WITH GLOBAL DATA INVALIDATION SYSTEM
    # =========================================================================
    register_module(
      module_name = "anova",
      ui_reset_function = function(session) {
        # Reset Multi-Factor ANOVA UI elements when data changes
        reset_anova_ui_elements(session, ns)
      },
      validation_function = function(data) {
        # Validate data for ANOVA
        if (is.null(data) || nrow(data) == 0) {
          return(list(valid = FALSE, message = "No data available"))
        }
        list(valid = TRUE, message = "")
      }
    )
    
    # =========================================================================
    # ANOVA UI RESET FUNCTION
    # =========================================================================
    reset_anova_ui_elements <- function(session, ns) {
      # Reset Oneway ANOVA inputs
      # factor_ow and data_ow render pickerInputs with IDs ow_factor and ow_data
      tryCatch({
        updatePickerInput(session, ns("ow_factor"), selected = character(0))
      }, error = function(e) {
        # Will be re-rendered when data changes
      })
      tryCatch({
        updatePickerInput(session, ns("ow_data"), selected = character(0))
      }, error = function(e) {
        # Will be re-rendered when data changes
      })
      updateRadioGroupButtons(session, ns("type_ow"), selected = 1)
      updateNumericInput(session, ns("conf_ow"), value = 0.95)
      updateNumericInput(session, ns("decimal_ow"), value = 4)
      updateNumericInput(session, ns("decimal_ow_ph"), value = 4)
      updateNumericInput(session, ns("ow_font_size"), value = 11)
      updatePrettySwitch(session, ns("disp_ow"), value = TRUE)
      updatePrettySwitch(session, ns("lines_ow_ph"), value = TRUE)
      updateCheckboxInput(session, ns("ow_ph_details"), value = FALSE)
      tryCatch({
        updateRadioButtons(session, ns("ow_ph_type"), selected = NULL)
      }, error = function(e) {
        # May not exist yet (uiOutput)
      })
      
      # Reset Multi-Factor ANOVA column selections
      # factors_ems and data_ems are pickerInputs inside uiOutput, so we can reset them
      tryCatch({
        updatePickerInput(session, ns("factors_ems"), selected = character(0))
      }, error = function(e) {
        # Will be re-rendered when data changes
      })
      tryCatch({
        updatePickerInput(session, ns("data_ems"), selected = character(0))
      }, error = function(e) {
        # Will be re-rendered when data changes
      })
      
      # Reset Multi-Factor ANOVA checkboxes
      updateCheckboxInput(session, ns("ems_show_mixed_nest"), value = FALSE)
      updateCheckboxInput(session, ns("ems_show_pool"), value = FALSE)
      updateCheckboxInput(session, ns("ems_disp"), value = FALSE)
      updateCheckboxInput(session, ns("ems_show_coeffs"), value = FALSE)
      updateCheckboxInput(session, ns("ems_show_optimum"), value = FALSE)
      updateCheckboxInput(session, ns("ems_show_box"), value = FALSE)
      
      # Reset Multi-Factor ANOVA numeric inputs
      updateNumericInput(session, ns("ems_conf"), value = 0.95)
      updateNumericInput(session, ns("ems_dec"), value = 4)
      updateNumericInput(session, ns("ems_target"), value = NA)
      updateNumericInput(session, ns("multi_response_tol"), value = 0)
      updateNumericInput(session, ns("ph_font_size"), value = 11)
      
      # Reset Multi-Factor ANOVA picker inputs
      updatePickerInput(session, ns("ems_pool"), selected = character(0))
      updatePickerInput(session, ns("ems_primary_col"), selected = character(0))
      updatePickerInput(session, ns("ems_ph_select"), selected = NULL)
      updatePickerInput(session, ns("ems_ph_effects"), selected = character(0))
      tryCatch({
        updatePrettyCheckboxGroup(session, ns("ems_ph_plot_options"), selected = c("CIs", "PIs"))
      }, error = function(e) {
        # May not exist yet
      })
      
      # Reset Multi-Factor ANOVA radio buttons/selects
      tryCatch({
        updateRadioGroupButtons(session, ns("ems_disp_type"), selected = 1)
      }, error = function(e) {
        # May not exist if dispersion is not selected
      })
      # Note: ems_ems can be either checkboxInput or radioGroupButtons depending on design balance
      # It's rendered dynamically, so we don't reset it here - it will be re-rendered when data changes
      
      # Reset dynamic f_r_factor and nest_factor inputs (will be cleared when factors_ems is cleared)
      # These are created dynamically, so clearing factors_ems will cause them to be re-rendered empty
      
      # Switch to Multi-Factor ANOVA "Set Up" tab
      # updateTabsetPanel works for both tabsetPanel and navlistPanel
      tryCatch({
        updateTabsetPanel(session, ns("mw_anova"), selected = ns("mw_su"))
      }, error = function(e) {
        # Ignore if tab switching fails (tab may not exist yet)
      })
    }
    
    # =========================================================================
    # ONEWAY ANOVA WORKER
    # =========================================================================
    
    # Create oneway ANOVA worker
    oneway_anova_worker <- create_oneway_anova_worker(
      id = "oneway_anova",
      filtered_data = filtered_data,
      input_values = oneway_inputs
    )
    
    # Render oneway outputs - these are in coordinator namespace
    output$ow_table <- renderUI({
      oneway_anova_worker$anova_table()
    })
    
    # Kruskal-Wallis mean rank table - always render DTOutput, return NULL when not K-W
    # This avoids DT initialization timing issues by always keeping the table visible
    # When not K-W, return NULL so no table is shown
    output$KW_out <- renderDT({
      # Check if Kruskal-Wallis is selected
      type <- input$type_ow
      if (is.null(type) || type != 3) {
        # Return NULL so no table is shown when not K-W
        return(NULL)
      }
      
      # Get Kruskal-Wallis table data
      kw_result <- oneway_anova_worker$kw_table()
      req(kw_result)
      
      # Use explicit DT::datatable() call - this is the proven working pattern from EDA module
      # All working DT tables in EDA use this pattern (descriptives, normality, natural tolerance, etc.)
      DT::datatable(kw_result, options = list(paging = FALSE))
    })
    
    output$ow_disp <- renderUI({
      oneway_anova_worker$dispersion_output()
    })
    
    # Render dynamic UI for factor selection (in coordinator namespace)
    output$factor_ow <- renderUI({
      data <- filtered_data()
      req(data)
      choices <- seq_len(ncol(data))
      names(choices) <- names(data)
      
      pickerInput(
        inputId = ns("ow_factor"),
        label = "Select Factor",
        multiple = FALSE,
        choices = choices
      )
    })
    
    # Render dynamic UI for data selection (in coordinator namespace)
    output$data_ow <- renderUI({
      data <- filtered_data()
      req(data)
      choices <- seq_len(ncol(data))
      names(choices) <- names(data)
      factor_selected <- as.numeric(input$ow_factor)
      req(factor_selected)
      
      # Remove factor column from choices
      temp <- seq_along(choices)
      temp <- temp[-factor_selected]
      choices <- choices[temp]
      
      pickerInput(
        inputId = ns("ow_data"),
        label = "Select Data",
        multiple = FALSE,
        choices = choices
      )
    })
    
    # Update worker inputs to include factor and data selections
    oneway_inputs <- reactive({
      result <- list(
        conf_ow = if (!is.null(input$conf_ow)) input$conf_ow else 0.95,
        decimal_ow = if (!is.null(input$decimal_ow)) input$decimal_ow else 4,
        type_ow = if (!is.null(input$type_ow)) input$type_ow else 1,
        disp_ow = if (!is.null(input$disp_ow)) input$disp_ow else TRUE,
        ow_factor = if (!is.null(input$ow_factor)) input$ow_factor else NULL,
        ow_data = if (!is.null(input$ow_data)) input$ow_data else NULL
      )
      
      result
    })
    
    # =========================================================================
    # ONEWAY POST-HOC WORKER
    # =========================================================================
    # Create input values reactive for post-hoc worker
    oneway_posthoc_inputs <- reactive({
      list(
        conf_ow = if (!is.null(input$conf_ow)) input$conf_ow else 0.95,
        decimal_ow_ph = if (!is.null(input$decimal_ow_ph)) input$decimal_ow_ph else 4,
        type_ow = if (!is.null(input$type_ow)) input$type_ow else 1,
        ow_factor = if (!is.null(input$ow_factor)) input$ow_factor else NULL,
        ow_data = if (!is.null(input$ow_data)) input$ow_data else NULL,
        ow_ph_type = if (!is.null(input$ow_ph_type)) input$ow_ph_type else NULL,
        ow_ph_details = if (!is.null(input$ow_ph_details)) input$ow_ph_details else FALSE,
        lines_ow_ph = if (!is.null(input$lines_ow_ph)) input$lines_ow_ph else TRUE,
        plot_type_ow_ph = if (!is.null(input$plot_type_ow_ph)) input$plot_type_ow_ph else 1,
        ow_font_size = if (!is.null(input$ow_font_size)) input$ow_font_size else 11
      )
    })
    
    # Render dynamic UI for plot type (in coordinator namespace)
    output$ow_ph_plot_type <- renderUI({
      type <- input$type_ow
      req(type)
      
      if (type == 1 || type == 3 || type == 4) {
        radioButtons(
          inputId = ns("plot_type_ow_ph"),
          label = "Choose plot type",
          choices = c("Points Only" = 1, "Violin" = 2, "Boxplot" = 3)
        )
      } else if (type == 2) {
        NULL
      }
    })
    
    # Render dynamic UI for post-hoc test type (in coordinator namespace)
    output$ow_ph_type <- renderUI({
      type <- input$type_ow
      req(type)
      
      if (type == 1 || type == 4) {  # fixed
        if (type == 4) {
          radioButtons(
            inputId = ns("ow_ph_type"),
            label = "Select Post-Hoc",
            choices = c("Tukey (equal variances)" = 1, "Games & Howell(unequal variances)" = 2),
            selected = 2
          )
        } else {
          radioButtons(
            inputId = ns("ow_ph_type"),
            label = "Select Post-Hoc",
            choices = c("Tukey (equal variances)" = 1, "Games & Howell(unequal variances)" = 2)
          )
        }
      } else if (type == 2) {  # random
        NULL
      } else if (type == 3) {  # K-W
        radioButtons(
          inputId = ns("ow_ph_type"),
          label = "Select Post-Hoc",
          choices = c("Wilcoxon-Mann-Whitney U" = 3)
        )
      }
    })
    
    # Create oneway post-hoc worker
    oneway_posthoc_worker <- create_oneway_posthoc_worker(
      id = "oneway_posthoc",
      filtered_data = filtered_data,
      input_values = oneway_posthoc_inputs,
      reactive_color_palette = reactive_color_palette
    )
    
    # Render post-hoc outputs
    # Worker returns DT object, so use renderDT (not renderDataTable)
    output$ow_ph_out_tab <- renderDT({
      oneway_posthoc_worker$posthoc_table()
    })
    
    output$ow_ph_details <- renderPrint({
      oneway_posthoc_worker$posthoc_details()
    })
    
    output$plotow <- renderPlot({
      oneway_posthoc_worker$posthoc_plot()
    })
    
    # Set up download functionality for plot
    plotow_height <- reactive(400 * 4)
    plotow_width <- reactive(400 * 4)
    downloadServer("plotow", oneway_posthoc_worker$posthoc_plot, height = plotow_height, width = plotow_width)
    
    # =========================================================================
    # MULTI-FACTOR ANOVA WORKER
    # =========================================================================
    # Multi-Factor (EMS) ANOVA worker (core logic implemented; rendering in later todos)
    multifactor_core_inputs <- reactive({
      # Collect dynamic f_r_factor and nest_factor inputs
      # These are created dynamically based on number of factors selected
      factors_id <- as.numeric(input$factors_ems)
      f_r_factors <- list()
      nest_factors <- list()
      
      if (!is.null(factors_id) && length(factors_id) > 0) {
        for (i in seq_along(factors_id)) {
          f_r_key <- paste0("f_r_factor", i)
          nest_key <- paste0("nest_factor", i)
          # Access inputs directly (they may not exist yet, so use tryCatch or check)
          if (!is.null(input[[f_r_key]])) {
            f_r_factors[[f_r_key]] <- input[[f_r_key]]
          }
          if (!is.null(input[[nest_key]])) {
            nest_factors[[nest_key]] <- input[[nest_key]]
          }
        }
      }
      
      base_list <- list(
        # Column selections
        factors_ems = input$factors_ems,
        data_ems = input$data_ems,
        
        # Mixed/nested configuration
        ems_show_mixed_nest = isTRUE(input$ems_show_mixed_nest),
        
        # Pooling + primary error
        ems_pool = input$ems_pool,
        ems_primary_col = input$ems_primary_col,
        ems_show_pool = isTRUE(input$ems_show_pool),
        
        # ANOVA settings
        ems_conf = if (!is.null(input$ems_conf)) input$ems_conf else 0.95,
        ems_dec = if (!is.null(input$ems_dec)) input$ems_dec else 4,
        ems_disp = isTRUE(input$ems_disp),
        ems_disp_type = input$ems_disp_type,
        ems_ems = input$ems_ems,
        
        # Graphs settings
        ems_target = input$ems_target,
        ems_show_coeffs = isTRUE(input$ems_show_coeffs),
        ems_show_optimum = isTRUE(input$ems_show_optimum),
        multi_response_tol = input$multi_response_tol,
        ems_show_box = isTRUE(input$ems_show_box),
        ems_interaction_dl_format = input$ems_interaction_dl_format
      )
      
      # Combine base list with dynamic f_r_factor and nest_factor inputs
      c(base_list, f_r_factors, nest_factors)
    })

    multifactor_posthoc_inputs <- reactive({
      list(
        ems_ph_select = input$ems_ph_select,
        ems_ph_plot_options = input$ems_ph_plot_options,
        ph_font_size = input$ph_font_size,
        ems_ph_effects = input$ems_ph_effects
      )
    })
    
    multifactor_worker <- create_multifactor_anova_worker(
      id = "multifactor_anova",
      filtered_data = filtered_data,
      core_input_values = multifactor_core_inputs,
      posthoc_input_values = multifactor_posthoc_inputs,
      reactive_color_palette = reactive_color_palette
    )
    
    # ---------------------------
    # Set Up tab dynamic UI
    # ---------------------------
    output$ems_factors <- renderUI({
      data <- filtered_data()
      req(data)
      choices <- seq_len(ncol(data))
      names(choices) <- names(data)
      
      pickerInput(
        inputId = ns("factors_ems"),
        label = "Select Factors",
        choices = choices,
        multiple = TRUE,
        options = list(title = "Select factor columns")
      )
    })
    
    output$ems_data <- renderUI({
      data <- filtered_data()
      req(data)
      choices <- seq_len(ncol(data))
      names(choices) <- names(data)
      
      pickerInput(
        inputId = ns("data_ems"),
        label = "Select Response",
        choices = choices,
        multiple = FALSE,
        options = list(title = "Select response column")
      )
    })
    
    output$ems_rand_select <- renderUI({
      data <- filtered_data()
      factors_id <- as.numeric(input$factors_ems)
      req(data, factors_id)
      
      factors_names <- names(data)[factors_id]
      if (length(factors_names) < 1) return(NULL)
      
      controls <- lapply(seq_along(factors_names), function(i) {
        radioButtons(
          inputId = ns(paste0("f_r_factor", i)),
          label = paste0("Effect type: ", factors_names[i]),
          choices = c("Fixed" = "F", "Random" = "R"),
          selected = "F",
          inline = TRUE
        )
      })
      
      do.call(tagList, controls)
    })
    
    output$ems_nest_select <- renderUI({
      data <- filtered_data()
      factors_id <- as.numeric(input$factors_ems)
      req(data, factors_id)
      
      factors_names <- names(data)[factors_id]
      if (length(factors_names) < 1) return(NULL)
      if (length(factors_names) < 2) return(NULL)
      
      controls <- lapply(seq_along(factors_names), function(i) {
        other_factors <- setdiff(factors_names, factors_names[i])
        checkboxGroupInput(
          inputId = ns(paste0("nest_factor", i)),
          label = paste0(factors_names[i], " nested within"),
          choices = other_factors,
          selected = NULL,
          inline = TRUE
        )
      })
      
      do.call(tagList, controls)
    })
    
    output$ems_primary <- renderUI({
      data <- filtered_data()
      factors_id <- as.numeric(input$factors_ems)
      data_id <- as.numeric(input$data_ems)
      req(data)
      
      # Match monolithic behavior: dummy columns exclude selected factors + response
      dummy_names <- names(data)
      if (!is.null(factors_id)) {
        dummy_names <- dummy_names[!(dummy_names %in% names(data)[factors_id])]
      }
      if (!is.null(data_id)) {
        dummy_names <- dummy_names[dummy_names != names(data)[data_id]]
      }
      
      pickerInput(
        inputId = ns("ems_primary_col"),
        label = "Dummy Columns (e1 Primary Error)",
        choices = dummy_names,
        multiple = TRUE,
        options = list(title = "Select dummy columns")
      )
    })
    
    output$ems_pool <- renderUI({
      data <- filtered_data()
      factors_id <- as.numeric(input$factors_ems)
      data_id <- as.numeric(input$data_ems)
      req(data, factors_id, data_id)
      
      if (length(factors_id) < 2) return(NULL)
      
      # IMPORTANT: do NOT depend on input$ems_pool here; otherwise the UI re-renders on each click
      # and the picker appears to "reset" (can't multi-select smoothly).
      factors_names <- names(data)[factors_id]
      req(factors_names)
      
      # Generate all main effects + interaction terms (same ordering as monolithic all_effects())
      choices <- unlist(lapply(seq_along(factors_names), function(k) {
        apply(utils::combn(factors_names, k), 2, function(x) paste(x, collapse = ":"))
      }))
      choices <- stringr::str_sort(choices)
      choices <- choices[order(stringr::str_count(choices, ":"))]
      
      pickerInput(
        inputId = ns("ems_pool"),
        label = "Pool / Exclude from model",
        choices = list("Selected effects will be excluded from the model" = choices),
        multiple = TRUE,
        # Preserve existing selections WITHOUT making renderUI reactive to ems_pool.
        # If we depend on input$ems_pool here, the UI re-renders on each click and the dropdown closes.
        selected = shiny::isolate(input$ems_pool),
        options = list(
          "actions-box" = TRUE,
          "title" = "Select effects to exclude",
          "tick-icon" = "glyphicon glyphicon-remove"
        )
      )
    })
    
    output$ems_ems_a <- renderUI({
      data <- filtered_data()
      factors_id <- as.numeric(input$factors_ems)
      data_id <- as.numeric(input$data_ems)
      req(data, factors_id, data_id)
      
      factors_names <- names(data)[factors_id]
      req(factors_names)
      
      EMSflag <- balance_test(factors_names = factors_names, data = data)
      
      if (EMSflag) {
        radioGroupButtons(
          inputId = ns("ems_ems"),
          label = "Unbalanced design - select analysis",
          choices = c("Unweighted" = 1, "Fractional Design odd levels" = 2, "Weighted" = 3),
          direction = "vertical",
          status = "danger",
          checkIcon = list(
            yes = icon("ok", lib = "glyphicon"),
            no = icon("remove", lib = "glyphicon")
          )
        )
      } else {
        checkboxInput(inputId = ns("ems_ems"), label = "Show EMS", value = FALSE)
      }
    })
    
    # ---------------------------
    # Results tab placeholders
    # ---------------------------
    output$ems_table <- renderUI({
      conf <- input$ems_conf
      R <- input$ems_dec
      show_ems <- input$ems_ems
      disp <- isTRUE(input$ems_disp)
      
      req(conf, R)
      
      data <- filtered_data()
      data_id <- as.numeric(input$data_ems)
      factors_id <- as.numeric(input$factors_ems)
      req(data, data_id, factors_id)
      
      aov_out_l <- multifactor_worker$aov_out()
      
      # If any n per cell < 3 and dispersion was requested, worker returns NULL
      if (is.null(aov_out_l)) {
        return(HTML("Not enough samples within cell to calculate dispersion."))
      }
      
      # Propagate worker error strings; odd-level reduced model is a formula string (see ems_pooled)
      if (is.character(aov_out_l) && length(aov_out_l) == 1) {
        if (grepl("can't calculate", aov_out_l, fixed = TRUE)) {
          return(HTML(aov_out_l))
        }
        if (grepl("~", aov_out_l, fixed = TRUE)) {
          return(HTML(paste0(
            "<p>Orthogonal design with odd levels (dummy levels): the unpooled table is completed using the same reduced model on the ",
            "<b>Pooled ANOVA</b> tab. Reduced model: <code>",
            htmltools::htmlEscape(aov_out_l),
            "</code></p>"
          )))
        }
        return(HTML(aov_out_l))
      }
      
      req(is.data.frame(aov_out_l))
      
      # Determine residual row name used by this table
      residual_row <- if ("Residuals" %in% rownames(aov_out_l)) "Residuals" else "Residual"
      
      # Effect size calculations
      sst <- sum(na.omit(aov_out_l$SS))
      msw <- aov_out_l[residual_row, "MS"]
      omega <- 100 * (aov_out_l$SS - (aov_out_l$Df * msw)) / (sst + msw)
      
      # ICC calculations for random effects
      msb <- head(aov_out_l$MS, -1)
      row_name <- rownames(aov_out_l)[-nrow(aov_out_l)]
      J <- rep(NA_real_, length(row_name))
      sum_n <- rep(NA_real_, length(row_name))
      sum_nsq <- rep(NA_real_, length(row_name))
      for (i in seq_along(row_name)) {
        combo <- c(str_split(row_name[i], ":", simplify = TRUE))
        combo <- gsub("\\([^)]+\\)", "", x = combo)
        combo <- trimws(combo)
        combo <- combo[combo != ""]
        if (length(combo) == 0) next
        
        J[i] <- nrow(unique(data[combo]))
        counts <- data %>% count(across(all_of(combo)))
        sum_n[i] <- sum(counts$n)
        sum_nsq[i] <- sum(counts$n^2)
      }
      K_prime <- (1 / (J - 1)) * (sum_n - (sum_nsq / sum_n))
      bcv <- (msb - msw) / K_prime
      ICC <- c(100 * bcv / (bcv + msw))
      
      # Add fixed/random labels via worker eff_types
      effects_f_r <- multifactor_worker$eff_types()
      req(effects_f_r)
      
      aov_out_l$Type <- c(effects_f_r$type, "R")
      aov_out_l$imp <- c(ifelse(effects_f_r$type_code == 1, omega, ICC), "")
      
      # Zero-out insignificant and negative effect sizes
      aov_out_l$imp[as.numeric(sub("<", "", aov_out_l$Pvalue, fixed = TRUE)) >= (1 - conf)] <- 0
      aov_out_l$imp[aov_out_l$imp < 0] <- 0
      aov_out_l$imp <- paste0(as.character(ro(as.numeric(aov_out_l$imp), R)), "%")
      aov_out_l[residual_row, "imp"] <- ""
      
      # Header text
      if (disp) {
        type_name <- c("ADA", "ADM", "ADM<sub>(n-1)</sub>")
        disp_type <- as.numeric(input$ems_disp_type)
        header <- paste0("Dependent Variable: ", names(data)[data_id], "<br>Dispersion Analysis based on ", type_name[disp_type])
      } else {
        header <- paste0("Dependent Variable: ", names(data)[data_id], "<br>Means Analysis")
      }

      # ANOVA Notes (ported behavior)
      notes <- multifactor_worker$anova_notes()
      if (isTruthy(notes)) {
        header <- paste0(header, "<br>ANOVA Notes: ", notes)
      }
      
      out_row <- seq_len(nrow(aov_out_l))
      
      # Column selection + table header HTML
      if (!is.logical(show_ems)) {
        # Unbalanced analysis selection (1/2/3)
        unbal <- as.numeric(show_ems)
        approach <- if (unbal == 1) {
          "Unbalanced Design: Unweighted Analysis"
        } else if (unbal == 2) {
          "Unbalanced Design: Orthogonal design odd levels - Dummy column(s) subtracted from error term"
        } else {
          "Unbalanced Design: Weighted Analysis"
        }
        header <- paste0(header, "<br>", approach)
        out_col <- c("SS", "Df", "MS", "Fvalue", "Pvalue", "Type", "imp")
        html <- paste(header, "<table><tr><th><b>Source</b></th><th><b>SS</b></th><th><b>df</b></th><th><b>MS</b></th><th><b>F</b></th><th><b>p</b></th><th><b>Type(F/R)</b></th><th><b>", withMathJax("$$\\omega^2$$"), " or ICC</b></th>")
      } else if (isTRUE(show_ems)) {
        out_col <- c("SS", "Df", "MS", "Fvalue", "Pvalue", "Type", "imp", "EMS")
        html <- paste(header, "<table><tr><th><b>Source</b></th><th><b>SS</b></th><th><b>df</b></th><th><b>MS</b></th><th><b>F</b></th><th><b>p</b></th><th><b>Type(F/R)</b></th><th><b>", withMathJax("$$\\omega^2$$"), " or ICC</b></th><th><b>EMS</b></th>")
      } else {
        out_col <- c("SS", "Df", "MS", "Fvalue", "Pvalue", "Type", "imp")
        html <- paste(header, "<table><tr><th><b>Source</b></th><th><b>SS</b></th><th><b>df</b></th><th><b>MS</b></th><th><b>F</b></th><th><b>p</b></th><th><b>Type(F/R)</b></th><th><b>", withMathJax("$$\\omega^2$$"), " or ICC</b></th>")
      }
      
      # Match monolithic behavior: round once, then paste values into HTML
      # (monolithic uses `ro <- round.object` from lolcat)
      aov_out_r <- lolcat::round.object(aov_out_l, R)
      
      for (i in out_row) {
        html <- paste(html, "<tr><td><b>", rownames(aov_out_r)[i], "</b></td>")
        for (j in out_col) {
          if (rownames(aov_out_r)[i] %in% c("Residuals", "Residual") && (j %in% c("Fvalue", "Pvalue", "imp"))) {
            html <- paste(html, "<td></td>")
            next
          }
          if ((j == "Pvalue" && (aov_out_r[i, j] == "" || is.na(aov_out_r[i, j])))) {
            html <- paste(html, "<td></td>")
            next
          }
          if (j == "Pvalue" && as.numeric(gsub(pattern = "<", replacement = "", x = aov_out_r[i, j])) < (1 - conf)) {
            # Use paste0 to avoid inserting a space that can wrap the "*" to a new line
            html <- paste0(html, "<td bgcolor='yellow'>", aov_out_r[i, j], "*</td>")
          } else {
            html <- paste(html, "<td>", aov_out_r[i, j], "</td>")
          }
        }
        html <- paste(html, "</tr>")
      }
      
      HTML(paste(html, "</table>"))
    })
    
    output$ems_table_pooled <- renderUI({
      conf <- input$ems_conf
      R <- input$ems_dec
      disp <- isTRUE(input$ems_disp)
      show_ems <- input$ems_ems
      pool_vars <- input$ems_pool
      req(conf, R, pool_vars)
      
      data <- filtered_data()
      data_id <- as.numeric(input$data_ems)
      req(data, data_id)
      
      aov_out_l <- multifactor_worker$ems_pooled()
      if (is.null(aov_out_l)) {
        return(HTML("Not enough samples within cell to calculate dispersion."))
      }
      if (is.character(aov_out_l) && length(aov_out_l) == 1) {
        return(HTML(aov_out_l))
      }
      req(is.data.frame(aov_out_l))
      
      if ("(Intercept)" %in% rownames(aov_out_l)) {
        aov_out_l <- aov_out_l[-which(rownames(aov_out_l) == "(Intercept)"), ]
      }
      
      residual_row <- if ("Within Cells" %in% rownames(aov_out_l)) {
        "Within Cells"
      } else if ("Residuals" %in% rownames(aov_out_l)) {
        "Residuals"
      } else {
        "Residual"
      }
      
      # Effect size calculations (same as monolithic pooled renderer)
      sst <- sum(na.omit(aov_out_l$SS))
      msw <- aov_out_l[nrow(aov_out_l), "MS"]
      omega <- 100 * (aov_out_l$SS - (aov_out_l$Df * msw)) / (sst + msw)
      
      msb <- head(aov_out_l$MS, -1)
      row_name <- rownames(aov_out_l)[-nrow(aov_out_l)]
      J <- rep(NA_real_, length(row_name))
      sum_n <- rep(NA_real_, length(row_name))
      sum_nsq <- rep(NA_real_, length(row_name))
      for (i in seq_along(row_name)) {
        combo <- c(str_split(row_name[i], ":", simplify = TRUE))
        combo <- gsub("\\([^)]+\\)", "", x = combo)
        combo <- trimws(combo)
        combo <- combo[combo != ""]
        if (length(combo) == 0) next
        
        test <- try(nrow(unique(data[combo])), silent = TRUE)
        if (inherits(test, "try-error")) {
          J[i] <- 0
          sum_n[i] <- 0
          sum_nsq[i] <- 0
          next
        }
        
        J[i] <- nrow(unique(data[combo]))
        counts <- data %>% count(across(all_of(combo)))
        sum_n[i] <- sum(counts$n)
        sum_nsq[i] <- sum(counts$n^2)
      }
      K_prime <- (1 / (J - 1)) * (sum_n - (sum_nsq / sum_n))
      bcv <- (msb - msw) / K_prime
      ICC <- c(100 * bcv / (bcv + msw))
      
      effects_f_r <- multifactor_worker$eff_types()
      req(effects_f_r)
      aov_out_l$Type <- c(effects_f_r$type, "R")
      aov_out_l$imp <- c(ifelse(effects_f_r$type_code == 1, omega, ICC), "")
      
      aov_out_l$imp[as.numeric(sub("<", "", aov_out_l$Pvalue, fixed = TRUE)) >= (1 - conf)] <- 0
      aov_out_l$imp[aov_out_l$imp < 0] <- 0
      aov_out_l$imp <- paste0(as.character(ro(as.numeric(aov_out_l$imp), R)), "%")
      aov_out_l[residual_row, "imp"] <- ""
      
      # Header text
      if (disp) {
        type_name <- c("ADA", "ADM", "ADM<sub>(n-1)</sub>")
        disp_type <- as.numeric(input$ems_disp_type)
        header <- paste0("Dependent Variable: ", names(data)[data_id], "<br>Dispersion Analysis based on ", type_name[disp_type])
      } else {
        header <- paste0("Dependent Variable: ", names(data)[data_id], "<br>Means Analysis")
      }

      # ANOVA Notes (ported behavior)
      notes <- multifactor_worker$anova_notes()
      if (isTruthy(notes)) {
        header <- paste0(header, "<br>ANOVA Notes: ", notes)
      }
      
      out_row <- seq_len(nrow(aov_out_l))
      if (is.logical(show_ems) && isTRUE(show_ems)) {
        out_col <- c("SS", "Df", "MS", "Fvalue", "Pvalue", "Type", "imp", "EMS")
        html <- paste(header, "<table><tr><th><b>Source</b></th><th><b>SS</b></th><th><b>df</b></th><th><b>MS</b></th><th><b>F</b></th><th><b>p</b></th><th><b>Type(F/R)</b></th><th><b>", withMathJax("$$\\omega^2$$"), " or ICC</b></th><th><b>EMS</b></th>")
      } else {
        out_col <- c("SS", "Df", "MS", "Fvalue", "Pvalue", "Type", "imp")
        html <- paste(header, "<table><tr><th><b>Source</b></th><th><b>SS</b></th><th><b>df</b></th><th><b>MS</b></th><th><b>F</b></th><th><b>p</b></th><th><b>Type(F/R)</b></th><th><b>", withMathJax("$$\\omega^2$$"), " or ICC</b></th>")
      }
      
      # Match monolithic behavior: round once, then paste values into HTML
      # (monolithic uses `ro <- round.object` from lolcat)
      aov_out_r <- lolcat::round.object(aov_out_l, R)
      for (i in out_row) {
        html <- paste(html, "<tr><td><b>", rownames(aov_out_r)[i], "</b></td>")
        for (j in out_col) {
          if (rownames(aov_out_r)[i] %in% c("Residuals", "Residual", "Within Cells") && (j %in% c("Fvalue", "Pvalue", "imp"))) {
            html <- paste(html, "<td></td>")
            next
          }
          if (j == "Pvalue" && (aov_out_r[i, j] == "" || is.na(aov_out_r[i, j]))) {
            html <- paste(html, "<td></td>")
            next
          }
          if (is.nan(aov_out_r[i, j])) {
            html <- paste(html, "<td>NaN</td>")
            next
          }
          if (j == "Pvalue" && as.numeric(gsub(pattern = "<", replacement = "", x = aov_out_r[i, j])) < (1 - conf)) {
            # Use paste0 to avoid inserting a space that can wrap the "*" to a new line
            html <- paste0(html, "<td bgcolor='yellow'>", aov_out_r[i, j], "*</td>")
          } else {
            html <- paste(html, "<td>", aov_out_r[i, j], "</td>")
          }
        }
        html <- paste(html, "</tr>")
      }
      
      if (isTRUE(input$ems_show_pool)) {
        HTML(paste(html, "</table>", "<br><br><br><b><u>Pooled Effects</u></b><br><br>", paste0(pool_vars, collapse = " || ")))
      } else {
        HTML(paste(html, "</table>"))
      }
    })
    
    output$emssigeffects <- renderPlot({
      multifactor_worker$ems_sig_effects_plot()
    }, width = 800, height = 400)

    # Download significant effects plot
    emssigeffects_height <- reactive(400 * 4)
    emssigeffects_width <- reactive(400 * 8)
    downloadServer("emssigeffects", multifactor_worker$ems_sig_effects_plot, height = emssigeffects_height, width = emssigeffects_width)
    
    output$multi_coeff_est <- renderDT({
      if (!isTRUE(input$ems_show_coeffs)) {
        return(NULL)
      }
      
      model <- multifactor_worker$model_mean_est()
      R <- input$ems_dec
      req(model, R)
      
      coeffs <- ro(data.frame(model$coefficients), R)
      DT::datatable(coeffs, options = list(dom = "t", paging = FALSE))
    })
    
    output$multi_response_target <- renderDT({
      if (!isTRUE(input$ems_show_optimum)) {
        return(NULL)
      }
      
      model <- multifactor_worker$model_mean_est()
      target <- input$ems_target
      R <- input$ems_dec
      req(model, target, R)
      
      emm <- emmeans::emmeans(object = model, specs = formula(paste("~", model[["terms"]][[3]][[2]][2])))
      tol <- input$multi_response_tol
      if (!isTruthy(tol)) tol <- 0
      
      closest <- summary(emm)[which(abs(summary(emm)$emmean - target) <= tol), ]
      closest <- closest[-c((ncol(closest) - 3):ncol(closest))]
      closest <- ro(closest, R)
      DT::datatable(closest, options = list(dom = "t", paging = FALSE), rownames = FALSE)
    })
    
    emsmaineffects_height <- reactive({
      req(input$factors_ems)
      ceiling(length(input$factors_ems) / 3) * 500
    })
    emsmaineffects_width <- reactive(400 * 2)
    
    output$emsmaineffects <- renderPlot({
      req(isTRUE(input$ems_show_box))
      multifactor_worker$ems_main_effects_plot()
    }, width = emsmaineffects_width, height = emsmaineffects_height)
    outputOptions(output, "emsmaineffects", suspendWhenHidden = TRUE)

    # Download main effects plot
    emsmaineffects_download_height <- reactive(emsmaineffects_height() * 5)
    emsmaineffects_download_width <- reactive(emsmaineffects_width() * 5)
    downloadServer(
      "emsmaineffects",
      multifactor_worker$ems_main_effects_plot,
      height = emsmaineffects_download_height,
      width = emsmaineffects_download_width
    )
    
    output$ems_int_sel <- renderUI({
      data <- filtered_data()
      req(data)
      
      # Match monolithic: derive interactions from current model terms (pooled vs not),
      # and avoid re-rendering on every click selection.
      aov_l <- if (!is.null(input$ems_pool) && length(input$ems_pool) > 0) {
        multifactor_worker$ems_pooled()
      } else {
        multifactor_worker$aov_out()
      }
      req(aov_l)
      if (!is.data.frame(aov_l)) return(NULL)
      
      int_sel <- rownames(head(aov_l, -1))
      int_sel <- int_sel[str_count(int_sel, ":") %in% c(1, 2)]
      if (length(int_sel) == 0) return(NULL)

      disp <- isTRUE(input$ems_disp)
      int_title <- if (disp) {
        "Show Two- and Three-Way Interaction Plots for Dispersion (Observed):"
      } else {
        "Show Two- and Three-Way Interaction Plots for Means (Observed):"
      }

      pickerInput(
        inputId = ns("ems_int_selected"),
        label = int_title,
        choices = list("Interactions" = int_sel),
        multiple = TRUE,
        selected = shiny::isolate(input$ems_int_selected),
        options = list(
          "actions-box" = TRUE,
          "title" = "Select effects to include"
        ),
        width = "100%"
      )
    })
    
    output$emsinteractions <- renderUI({
      int_selected <- input$ems_int_selected
      data <- filtered_data()
      req(data, int_selected)

      plot_list <- lapply(seq_along(int_selected), function(i) {
        effect <- int_selected[i]
        if (str_count(effect, ":") == 2) {
          fac <- str_split(effect, ":", simplify = TRUE)[3]
          level <- length(unique(data[[fac]]))
          plotOutput(outputId = ns(paste0("int_plot_", i)), height = 400 * level)
        } else {
          plotOutput(outputId = ns(paste0("int_plot_", i)))
        }
      })
      do.call(tagList, plot_list)
    })

    observeEvent(input$ems_int_selected, {
      int_selected <- input$ems_int_selected
      req(int_selected)

      lapply(seq_along(int_selected), function(i) {
        effect <- int_selected[i]
        output[[paste0("int_plot_", i)]] <- renderPlot({
          multifactor_worker$interaction_plot(effect)
        })
      })
    }, ignoreInit = TRUE)

    # Download all interaction plots as a zip
    output$ems_interaction_dl <- downloadHandler(
      filename = function() {
        paste0("ems_interactions_", Sys.Date(), ".zip")
      },
      content = function(file) {
        int_selected <- input$ems_int_selected
        req(int_selected)
        fmt <- input$ems_interaction_dl_format
        if (!isTruthy(fmt)) fmt <- "svg"

        tmpdir <- tempfile("ems_int_")
        dir.create(tmpdir)

        files <- character(0)
        for (i in seq_along(int_selected)) {
          p <- multifactor_worker$interaction_plot(int_selected[i])
          out_path <- file.path(tmpdir, paste0("int_plot_", i, ".", fmt))
          ggplot2::ggsave(filename = out_path, plot = p, device = fmt, width = 8, height = 4, units = "in")
          files <- c(files, out_path)
        }

        utils::zip(zipfile = file, files = files, flags = "-j")
      }
    )
    
    output$emsphplot <- renderPlot({
      multifactor_worker$posthoc_plot()
    }, width = 800, height = 400)
    outputOptions(output, "emsphplot", suspendWhenHidden = TRUE)

    emsphplot_height <- reactive(400 * 4)
    emsphplot_width <- reactive(400 * 8)
    downloadServer("emsphplot", multifactor_worker$posthoc_plot, height = emsphplot_height, width = emsphplot_width)
    
    output$ems_ph_selection <- renderUI({
      conf <- input$ems_conf
      req(conf)

      aov_out_l <- if (!is.null(input$ems_pool) && length(input$ems_pool) > 0) {
        multifactor_worker$ems_pooled()
      } else {
        multifactor_worker$aov_out()
      }

      req(aov_out_l)
      if (is.character(aov_out_l)) return(NULL)
      if (!is.data.frame(aov_out_l)) return(NULL)

      # Significant effects (exclude residual and intercept)
      sig <- aov_out_l[aov_out_l$Pvalue <= (1 - conf), , drop = FALSE]
      sig_effects <- row.names(sig)
      sig_effects <- sig_effects[!sig_effects %in% c("Residuals", "Residual", "(Intercept)", "Within Cells")]
      if (length(sig_effects) == 0) return(NULL)

      pickerInput(
        inputId = ns("ems_ph_effects"),
        label = "Select effects for post-hoc",
        choices = sig_effects,
        options = list(title = "Select Effect(s)"),
        selected = shiny::isolate(input$ems_ph_effects),
        multiple = TRUE
      )
    })
    
    output$ems_ph_out <- renderUI({
      multifactor_worker$posthoc_out_dt()
    })
    
    # Downloads are wired when plots are implemented in later todos.
    
  })
}
