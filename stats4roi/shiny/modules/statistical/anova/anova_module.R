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
source("modules/statistical/anova/utils/oneway_homogeneous_subsets.R")
source("modules/statistical/anova/utils/emsanova_roi_overrides.R")
source("modules/statistical/anova/utils/data_invalidation_helpers.R")
source("modules/statistical/anova/utils/optimization/dispersion_metric_sigma.R")
source("modules/statistical/anova/utils/optimization/dispersion_resolution_prior.R")
source("modules/statistical/anova/utils/optimization/taguchi_loss.R")
source("modules/statistical/anova/server/optimization/optimization_helpers.R")
source("modules/statistical/anova/utils/optimization/search_strategy.R")
source("modules/statistical/anova/utils/optimization/multiresponse.R")
source("modules/statistical/anova/utils/optimization/taguchi_loss_mvp.R")

# Source worker modules
source("modules/statistical/anova/server/oneway_anova_server.R")
source("modules/statistical/anova/server/oneway_posthoc_server.R")
source("modules/statistical/anova/server/multifactor_anova_server.R")
source("modules/statistical/anova/server/optimization/optimization_session.R")
source("modules/statistical/anova/server/mf_optimization_readiness.R")
source("modules/statistical/anova/server/optimization/optimization_coordinator.R")
source("modules/statistical/anova/server/optimization/optimization_ui_helpers.R")
source("modules/statistical/anova/server/optimization/optimization_report.R")
source("modules/statistical/anova/server/loss/loss_multifactor_server.R")

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

    # Popup-saved factor costs (semicolon string); avoids relying on a textArea input sync.
    loss_ow_saved_factor_costs <- reactiveVal("")
    loss_mf_saved_factor_costs <- reactiveVal("")
    loss_mf_opt_result_val <- reactiveVal(NULL)
    loss_ow_opt_result_val <- reactiveVal(NULL)

    # =========================================================================
    # DATA INVALIDATION (EDA pattern: trigger invalidates workers; register_module resets UI)
    # =========================================================================
    anova_data_trigger <- create_data_invalidation_trigger(filtered_data)

    anova_worker_inputs <- function(builder) {
      reactive({
        anova_data_trigger()
        builder()
      })
    }

    register_module(
      module_name = "anova",
      ui_reset_function = function() {
        reset_anova_data_driven_ui(session, ns, on_after_reset = function() {
          loss_ow_saved_factor_costs("")
          loss_mf_saved_factor_costs("")
          loss_mf_opt_result_val(NULL)
          loss_ow_opt_result_val(NULL)
        })
      },
      validation_function = function(data) {
        if (is.null(data) || nrow(data) == 0) {
          return(list(valid = FALSE, message = "No data available"))
        }
        list(valid = TRUE, message = "")
      }
    )

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

    snapshot_mf_loss_setup_from_inputs <- function() {
      sync_mf_design_setup()
      bumped <- FALSE
      fid <- suppressWarnings(as.numeric(isolate(input$factors_ems)))
      fid <- fid[is.finite(fid)]
      if (length(fid) >= 2L) {
        if (!identical(isolate(mf_loss_setup_cache$factors_ems), fid)) {
          mf_loss_setup_cache$factors_ems <- fid
          bumped <- TRUE
        }
      }
      dem <- suppressWarnings(as.numeric(isolate(input$data_ems)))
      dem <- dem[is.finite(dem)]
      if (length(dem) >= 1L) {
        if (!identical(isolate(mf_loss_setup_cache$data_ems), dem)) {
          mf_loss_setup_cache$data_ems <- dem
          bumped <- TRUE
        }
      }
      act <- suppressWarnings(as.numeric(isolate(input$data_ems_active)))
      act <- act[is.finite(act)]
      if (length(act) >= 1L) {
        act_i <- as.integer(act[[1L]])
        if (!identical(isolate(mf_loss_setup_cache$data_ems_active), act_i)) {
          mf_loss_setup_cache$data_ems_active <- act_i
          bumped <- TRUE
        }
      }
      if (isTRUE(bumped)) {
        mf_loss_setup_rev(isolate(mf_loss_setup_rev()) + 1L)
      }
      invisible(bumped)
    }

    observe({
      d <- filtered_data()
      fid <- suppressWarnings(as.numeric(input$factors_ems))
      fid <- fid[is.finite(fid)]
      bumped <- FALSE
      if (length(fid) >= 2L) {
        if (!identical(isolate(mf_loss_setup_cache$factors_ems), fid)) {
          mf_loss_setup_cache$factors_ems <- fid
          bumped <- TRUE
        }
      }
      dem <- suppressWarnings(as.numeric(input$data_ems))
      dem <- dem[is.finite(dem)]
      if (length(dem) >= 1L) {
        if (!identical(isolate(mf_loss_setup_cache$data_ems), dem)) {
          mf_loss_setup_cache$data_ems <- dem
          bumped <- TRUE
        }
      }
      act <- suppressWarnings(as.numeric(input$data_ems_active))
      act <- act[is.finite(act)]
      if (length(act) >= 1L) {
        act_i <- as.integer(act[[1L]])
        if (!identical(isolate(mf_loss_setup_cache$data_ems_active), act_i)) {
          mf_loss_setup_cache$data_ems_active <- act_i
          bumped <- TRUE
        }
      }
      if (isTRUE(bumped)) {
        mf_loss_setup_rev(isolate(mf_loss_setup_rev()) + 1L)
      }
    })

    mf_resolved_data_ems_r <- reactive({
      mf_loss_setup_rev()
      input$data_ems
      dem <- suppressWarnings(as.numeric(input$data_ems))
      dem <- dem[is.finite(dem)]
      if (length(dem) >= 1L) {
        return(dem)
      }
      cached <- mf_loss_setup_cache$data_ems
      if (!is.null(cached) && length(cached) >= 1L) return(cached)
      NULL
    })

  mf_resolved_factors_ems <- function() {
      fid <- suppressWarnings(as.numeric(isolate(input$factors_ems)))
      fid <- fid[is.finite(fid)]
      if (length(fid) >= 2L) return(fid)
      cached <- isolate(mf_loss_setup_cache$factors_ems)
      if (!is.null(cached) && length(cached) >= 2L) return(cached)
      NULL
    }

    mf_resolved_data_ems <- function() {
      mf_resolved_data_ems_r()
    }

    mf_resolved_active_did <- function() {
      act <- suppressWarnings(as.numeric(isolate(input$data_ems_active)))
      act <- act[is.finite(act)]
      if (length(act) >= 1L) return(as.integer(act[[1L]]))
      cached <- isolate(mf_loss_setup_cache$data_ems_active)
      if (!is.null(cached) && is.finite(cached)) return(as.integer(cached))
      dem <- mf_resolved_data_ems()
      if (!is.null(dem) && length(dem) >= 1L) return(as.integer(dem[[1L]]))
      NULL
    }

    last_mf_opt_picker <- reactiveVal(list(choices = character(0), selected = character(0)))
    last_mf_opt_blocked_picker <- reactiveVal(list(choices = character(0), selected = character(0)))

    observe({
      d <- filtered_data()
      fid <- suppressWarnings(as.numeric(input$factors_ems))
      if (is.null(d) || length(fid) < 1L) {
        st <- list(choices = character(0), selected = character(0))
        if (!identical(isolate(last_mf_opt_picker()), st)) {
          last_mf_opt_picker(st)
          updatePickerInput(session, "loss_mf_opt_continuous", choices = character(0), selected = character(0))
        }
        if (!identical(isolate(last_mf_opt_blocked_picker()), st)) {
          last_mf_opt_blocked_picker(st)
          updatePickerInput(session, "loss_mf_opt_blocked", choices = character(0), selected = character(0))
        }
      } else {
        valid_fid <- fid[fid >= 1L & fid <= ncol(d)]
        fnames <- if (length(valid_fid) > 0L) names(d)[valid_fid] else character(0)
        current <- isolate(input$loss_mf_opt_continuous)
        current <- current[current %in% fnames]
        st <- list(choices = fnames, selected = sort(current))
        if (!identical(isolate(last_mf_opt_picker()), st)) {
          last_mf_opt_picker(st)
          updatePickerInput(
            session, "loss_mf_opt_continuous",
            choices = fnames,
            selected = current
          )
        }
        blocked <- isolate(input$loss_mf_opt_blocked)
        blocked <- blocked[blocked %in% fnames]
        cont <- current
        blocked <- setdiff(blocked, cont)
        stb <- list(choices = fnames, selected = sort(blocked))
        if (!identical(isolate(last_mf_opt_blocked_picker()), stb)) {
          last_mf_opt_blocked_picker(stb)
          updatePickerInput(
            session, "loss_mf_opt_blocked",
            choices = fnames,
            selected = blocked
          )
        }
      }
    })

    observe({
      d <- filtered_data()
      fc <- suppressWarnings(as.numeric(input$ow_factor))
      dc <- suppressWarnings(as.numeric(input$ow_data))
      if (is.null(d) || length(fc) != 1L || length(dc) != 1L || !is.finite(fc) || !is.finite(dc) ||
          fc < 1L || fc > ncol(d) || dc < 1L || dc > ncol(d)) {
        updatePickerInput(session, "loss_ow_opt_continuous", choices = character(0), selected = character(0))
      } else {
        raw_name <- names(d)[fc]
        model_name <- make.names(raw_name)
        current <- input$loss_ow_opt_continuous
        current <- unique(as.character(current))
        current <- current[current %in% c(raw_name, model_name)]
        updatePickerInput(
          session, "loss_ow_opt_continuous",
          choices = stats::setNames(model_name, raw_name),
          selected = current
        )
      }
    })

    loss_cost_modal_state <- reactiveValues(meta = list())
    loss_ow_cost_modal_state <- reactiveValues(meta = list())

    output$loss_mf_opt_costs_summary <- renderText({
      if (!isTRUE(input$loss_mf_opt_use_factor_costs)) return("")
      d <- filtered_data()
      req(d)
      fid <- suppressWarnings(as.numeric(input$factors_ems))
      valid_fid <- fid[fid >= 1L & fid <= ncol(d)]
      fnames <- if (length(valid_fid) > 0L) names(d)[valid_fid] else character(0)
      tbl <- parse_factor_level_costs(loss_mf_saved_factor_costs(), valid_factors = fnames)
      if (nrow(tbl) < 1L) {
        "No factor-level costs entered."
      } else {
        paste0("Saved entries: ", nrow(tbl), " across ", length(unique(tbl$factor)), " factor(s).")
      }
    })

    open_factor_cost_editor <- function() {
      tryCatch({
        d <- filtered_data()
        if (is.null(d) || !is.data.frame(d) || ncol(d) < 1L) {
          showNotification("Cost editor unavailable: no active data/factors.", type = "error")
          return(invisible(NULL))
        }
        fid <- mf_resolved_factors_ems()
        valid_fid <- if (is.null(fid)) numeric(0) else fid[fid >= 1L & fid <= ncol(d)]
        fnames <- if (length(valid_fid) > 0L) names(d)[valid_fid] else character(0)
        if (length(fnames) < 1L) {
          showNotification("Cost editor unavailable: choose factors first.", type = "error")
          return(invisible(NULL))
        }
        cont_vars <- if (!is.null(input$loss_mf_opt_continuous)) unique(as.character(input$loss_mf_opt_continuous)) else character(0)
        existing <- parse_factor_level_costs(isolate(loss_mf_saved_factor_costs()), valid_factors = fnames)
        b <- build_factor_cost_modal_ui(
          d = d, factor_names = fnames, cont_vars = cont_vars, existing_costs_tbl = existing,
          ns = ns, cost_id_prefix = "loss_mf_cost__"
        )
        loss_cost_modal_state$meta <- b$meta
        showModal(modalDialog(
          title = "Factor-level unit costs",
          b$ui,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("loss_mf_opt_save_costs"), "Save costs", class = "btn-primary")
          ),
          size = "l",
          easyClose = TRUE
        ))
      }, error = function(e) {
        showNotification(paste0("Could not open cost editor: ", conditionMessage(e)), type = "error")
      })
    }

    observeEvent(input$loss_mf_opt_use_factor_costs, {
      if (!isTRUE(input$loss_mf_opt_use_factor_costs)) return(invisible(NULL))
      open_factor_cost_editor()
    }, ignoreInit = TRUE)

    observeEvent(input$loss_mf_opt_save_costs, {
      meta <- isolate(loss_cost_modal_state$meta)
      if (length(meta) < 1L) {
        removeModal()
        return(invisible(NULL))
      }
      parts <- character(0)
      for (m in meta) {
        val <- suppressWarnings(as.numeric(input[[m$id]]))
        if (length(val) != 1L || !is.finite(val)) next
        parts <- c(parts, paste0(m$factor, ":", m$level, "=", val))
      }
      if (length(parts) < 1L) {
        showNotification(
          "No numeric costs were read from the popup (all fields empty or non-numeric). Enter a number in each level row, then Save again.",
          type = "warning",
          duration = 8
        )
        loss_mf_saved_factor_costs("")
      } else {
        loss_mf_saved_factor_costs(paste(parts, collapse = "; "))
      }
      removeModal()
    }, ignoreInit = TRUE)

    output$loss_ow_opt_costs_summary <- renderText({
      if (!isTRUE(input$loss_ow_opt_use_factor_costs)) return("")
      d <- filtered_data()
      req(d)
      fc <- suppressWarnings(as.numeric(input$ow_factor))
      dc <- suppressWarnings(as.numeric(input$ow_data))
      if (length(fc) != 1L || length(dc) != 1L || !is.finite(fc) || !is.finite(dc)) return("")
      dat <- d[, c(dc, fc), drop = FALSE]
      dat <- as.data.frame(dat, stringsAsFactors = FALSE)
      names(dat) <- make.names(names(dat))
      xname <- names(dat)[2L]
      tbl <- parse_factor_level_costs(loss_ow_saved_factor_costs(), valid_factors = c(xname))
      if (nrow(tbl) < 1L) {
        "No factor-level costs entered."
      } else {
        paste0("Saved entries: ", nrow(tbl), " across ", length(unique(tbl$factor)), " factor(s).")
      }
    })

    open_factor_cost_editor_ow <- function() {
      tryCatch({
        d <- filtered_data()
        if (is.null(d) || !is.data.frame(d) || ncol(d) < 1L) {
          showNotification("Cost editor unavailable: no active data.", type = "error")
          return(invisible(NULL))
        }
        fc <- suppressWarnings(as.numeric(input$ow_factor))
        dc <- suppressWarnings(as.numeric(input$ow_data))
        if (length(fc) != 1L || length(dc) != 1L || !is.finite(fc) || !is.finite(dc) ||
            fc < 1L || fc > ncol(d) || dc < 1L || dc > ncol(d)) {
          showNotification("Cost editor unavailable: choose response and factor on the Oneway ANOVA tab.", type = "error")
          return(invisible(NULL))
        }
        dat <- d[, c(dc, fc), drop = FALSE]
        dat <- as.data.frame(dat, stringsAsFactors = FALSE)
        names(dat) <- make.names(names(dat))
        xname <- names(dat)[2L]
        d_one <- dat[, 2, drop = FALSE]
        names(d_one) <- xname
        cont_vars <- if (!is.null(input$loss_ow_opt_continuous)) unique(as.character(input$loss_ow_opt_continuous)) else character(0)
        cont_vars <- cont_vars[nzchar(cont_vars)]
        cont_vars <- intersect(cont_vars, xname)
        existing <- parse_factor_level_costs(isolate(loss_ow_saved_factor_costs()), valid_factors = c(xname))
        b <- build_factor_cost_modal_ui(
          d = d_one, factor_names = c(xname), cont_vars = cont_vars, existing_costs_tbl = existing,
          ns = ns, cost_id_prefix = "loss_ow_cost__"
        )
        loss_ow_cost_modal_state$meta <- b$meta
        showModal(modalDialog(
          title = "Factor-level unit costs",
          b$ui,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("loss_ow_opt_save_costs"), "Save costs", class = "btn-primary")
          ),
          size = "l",
          easyClose = TRUE
        ))
      }, error = function(e) {
        showNotification(paste0("Could not open cost editor: ", conditionMessage(e)), type = "error")
      })
    }

    observeEvent(input$loss_ow_opt_use_factor_costs, {
      if (!isTRUE(input$loss_ow_opt_use_factor_costs)) return(invisible(NULL))
      open_factor_cost_editor_ow()
    }, ignoreInit = TRUE)

    observeEvent(input$loss_ow_opt_save_costs, {
      meta <- isolate(loss_ow_cost_modal_state$meta)
      if (length(meta) < 1L) {
        removeModal()
        return(invisible(NULL))
      }
      parts <- character(0)
      for (m in meta) {
        val <- suppressWarnings(as.numeric(input[[m$id]]))
        if (length(val) != 1L || !is.finite(val)) next
        parts <- c(parts, paste0(m$factor, ":", m$level, "=", val))
      }
      if (length(parts) < 1L) {
        showNotification(
          "No numeric costs were read from the popup (all fields empty or non-numeric). Enter a number in each level row, then Save again.",
          type = "warning",
          duration = 8
        )
        loss_ow_saved_factor_costs("")
      } else {
        loss_ow_saved_factor_costs(paste(parts, collapse = "; "))
      }
      removeModal()
    }, ignoreInit = TRUE)
    
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
      anova_data_trigger()
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
      anova_data_trigger()
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
    oneway_inputs <- anova_worker_inputs(function() {
      result <- list(
        conf_ow = if (!is.null(input$conf_ow)) input$conf_ow else 0.95,
        decimal_ow = if (!is.null(input$decimal_ow)) input$decimal_ow else 4,
        type_ow = if (!is.null(input$type_ow)) input$type_ow else 1,
        disp_ow = if (!is.null(input$disp_ow)) input$disp_ow else TRUE,
        ow_disp_analysis = isTRUE(input$ow_disp_analysis),
        ow_disp_type = if (!is.null(input$ow_disp_type)) input$ow_disp_type else 1,
        ow_factor = if (!is.null(input$ow_factor)) input$ow_factor else NULL,
        ow_data = if (!is.null(input$ow_data)) input$ow_data else NULL
      )
      
      result
    })
    
    # =========================================================================
    # ONEWAY POST-HOC WORKER
    # =========================================================================
    # Create input values reactive for post-hoc worker
    oneway_posthoc_inputs <- anova_worker_inputs(function() {
      list(
        conf_ow = if (!is.null(input$conf_ow)) input$conf_ow else 0.95,
        decimal_ow_ph = if (!is.null(input$decimal_ow_ph)) input$decimal_ow_ph else 4,
        type_ow = if (!is.null(input$type_ow)) input$type_ow else 1,
        ow_disp_analysis = isTRUE(input$ow_disp_analysis),
        ow_disp_type = if (!is.null(input$ow_disp_type)) input$ow_disp_type else 1,
        ow_factor = if (!is.null(input$ow_factor)) input$ow_factor else NULL,
        ow_data = if (!is.null(input$ow_data)) input$ow_data else NULL,
        ow_ph_type = if (!is.null(input$ow_ph_type)) input$ow_ph_type else NULL,
        ow_ph_details = if (!is.null(input$ow_ph_details)) input$ow_ph_details else FALSE,
        ow_ph_homogeneous = isTRUE(input$ow_ph_homogeneous),
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
        ph_choices <- if (isTRUE(input$ow_disp_analysis)) {
          c(
            "Tukey (sample sizes equal)" = 1,
            "Games & Howell (sample sizes unequal)" = 2
          )
        } else {
          c(
            "Tukey (equal variances)" = 1,
            "Games & Howell(unequal variances)" = 2
          )
        }
        selected <- input$ow_ph_type
        if (is.null(selected)) {
          selected <- if (type == 4) 2 else NULL
        }
        radioButtons(
          inputId = ns("ow_ph_type"),
          label = "Select Post-Hoc",
          choices = ph_choices,
          selected = selected
        )
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

    output$ow_ph_homogeneous <- renderDT({
      oneway_posthoc_worker$posthoc_homogeneous_subsets()
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
    active_data_ems <- reactive({
      sel <- as.numeric(input$data_ems)
      if (is.null(sel) || length(sel) < 1L || all(!is.finite(sel))) return(NULL)
      sel <- sel[is.finite(sel)]
      act <- suppressWarnings(as.numeric(input$data_ems_active))
      if (!is.null(act) && length(act) >= 1L && is.finite(act[[1]]) && act[[1]] %in% sel) {
        return(as.integer(act[[1]]))
      }
      as.integer(sel[[1]])
    })

    # Per-response retention for Results/Graphs/Post-hoc controls.
    # When active response switches, persist current UI state by response id,
    # then restore previously saved state for the newly active response.
    mf_results_state <- reactiveValues(by_response = list(), last_active = NULL)
    mf_anova_effect_ui_rev <- reactiveVal(0L)

    # Set Up tab (mixed/random/nested) inputs are removed from Shiny input when the
    # navlistPanel switches to Results; persist them here for ANOVA + effect lists.
    mf_design_setup <- reactiveValues(
      show_mixed_nest = FALSE,
      f_r = list(),
      nest = list(),
      n_factors = 0L,
      ems_pool = character(0)
    )
    mf_loss_setup_cache <- reactiveValues(
      factors_ems = NULL,
      data_ems = NULL,
      data_ems_active = NULL
    )
    mf_loss_setup_rev <- reactiveVal(0L)
    loss_mf_results_val <- reactiveVal(NULL)
    mf_loss_tier3_snapshot <- reactiveVal(NULL)
    loss_mf_calculating <- reactiveVal(FALSE)
    mf_loss_tab_rev <- reactiveVal(0L)
    loss_mf_opt_running <- reactiveVal(FALSE)

    # Draft Set Up exclusions (live picker); worker uses mf_setup_pool_committed only.
    mf_setup_pool_committed <- reactiveVal(character(0))

    mf_setup_pool_committed_vec <- function() {
      em <- mf_setup_pool_committed()
      if (is.null(em) || length(em) < 1L) character(0) else as.character(em)
    }

    mf_setup_pool_for_effects <- function(fx) {
      em <- mf_setup_pool_committed_vec()
      if (is.null(fx) || length(fx) < 1L) {
        return(em)
      }
      intersect(em, as.character(fx))
    }

    commit_mf_setup_pool_if_changed <- function() {
      sync_mf_design_setup()
      em <- isolate(mf_design_setup$ems_pool)
      if (is.null(em)) em <- character(0)
      em <- as.character(em)
      prev <- isolate(mf_setup_pool_committed())
      if (is.null(prev)) prev <- character(0)
      if (identical(sort(unique(em)), sort(unique(prev)))) {
        return(invisible(FALSE))
      }
      mf_setup_pool_committed(em)
      mf_effect_pool_store(list())
      mf_applied_pool_rev(isolate(mf_applied_pool_rev()) + 1L)
      invisible(TRUE)
    }

    sync_mf_design_setup <- function() {
      fid <- suppressWarnings(as.numeric(isolate(input$factors_ems)))
      fid <- fid[is.finite(fid) & fid >= 1L]
      if (length(fid) < 1L) {
        mf_design_setup$n_factors <- 0L
        mf_design_setup$show_mixed_nest <- FALSE
        mf_design_setup$f_r <- list()
        mf_design_setup$nest <- list()
        mf_design_setup$ems_pool <- character(0)
        return(invisible(NULL))
      }
      if (!is.null(input$ems_show_mixed_nest)) {
        mf_design_setup$show_mixed_nest <- isTRUE(input$ems_show_mixed_nest)
      }
      nf <- length(fid)
      mf_design_setup$n_factors <- nf
      for (i in seq_len(nf)) {
        fk <- paste0("f_r_factor", i)
        nk <- paste0("nest_factor", i)
        fv <- input[[fk]]
        if (!is.null(fv) && nzchar(as.character(fv)[1])) {
          mf_design_setup$f_r[[fk]] <- as.character(fv)[1]
        }
        nv <- input[[nk]]
        if (!is.null(nv)) {
          mf_design_setup$nest[[nk]] <- as.character(nv)
        }
      }
      invisible(NULL)
    }

    observe({
      input$factors_ems
      input$ems_show_mixed_nest
      sync_mf_design_setup()
      nf <- isolate(mf_design_setup$n_factors)
      if (nf >= 1L) {
        for (i in seq_len(nf)) {
          input[[paste0("f_r_factor", i)]]
          input[[paste0("nest_factor", i)]]
        }
      }
    })

    # Persist draft picker state only; do not wire live input$ems_pool into ANOVA reactives.
    mf_resolved_f_r_types <- function(fid) {
      vapply(seq_along(fid), function(i) {
        x <- isolate(input[[paste0("f_r_factor", i)]])
        if (is.null(x) || length(x) < 1L) {
          cached <- isolate(mf_design_setup$f_r[[paste0("f_r_factor", i)]])
          if (!is.null(cached) && nzchar(as.character(cached)[1L])) {
            return(as.character(cached)[1L])
          }
          return("F")
        }
        xc <- as.character(x)[1]
        if (!nzchar(xc)) "F" else xc
      }, character(1))
    }

    mf_resolved_ems_show_mixed_nest <- function() {
      if (is.null(input$ems_show_mixed_nest)) {
        isTRUE(isolate(mf_design_setup$show_mixed_nest))
      } else {
        isTRUE(input$ems_show_mixed_nest)
      }
    }

    observeEvent(input$ems_pool, {
      mf_design_setup$ems_pool <- if (is.null(input$ems_pool)) {
        character(0)
      } else {
        as.character(input$ems_pool)
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Commit Set Up exclusions when leaving Set Up (Results / Loss / etc.).
    observeEvent(input$mw_anova, {
      snapshot_mf_loss_setup_from_inputs()
      cur <- input$mw_anova
      if (is.null(cur) || identical(cur, ns("mw_su"))) {
        return(invisible(NULL))
      }
      commit_mf_setup_pool_if_changed()
      if (identical(as.character(cur), ns("mw_loss")) || grepl("Loss", as.character(cur), fixed = TRUE)) {
        mf_loss_tab_rev(isolate(mf_loss_tab_rev()) + 1L)
        # Ensure response ids are cached before Loss tab reads them (Set Up inputs are removed).
        snapshot_mf_loss_setup_from_inputs()
      } else {
        loss_mf_results_val(NULL)
        mf_loss_tier3_snapshot(NULL)
      }
    }, ignoreInit = FALSE)

    observeEvent(input$data_ems, {
      snapshot_mf_loss_setup_from_inputs()
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    capture_mf_results_state <- function() {
      list(
        ems_conf = input$ems_conf,
        ems_dec = input$ems_dec,
        ems_disp = isTRUE(input$ems_disp),
        ems_disp_type = input$ems_disp_type,
        ems_show_pool = isTRUE(input$ems_show_pool),
        ems_primary_col = input$ems_primary_col,
        ems_ems = input$ems_ems,
        ems_target = input$ems_target,
        ems_show_coeffs = isTRUE(input$ems_show_coeffs),
        ems_show_optimum = isTRUE(input$ems_show_optimum),
        multi_response_tol = input$multi_response_tol,
        ems_show_box = isTRUE(input$ems_show_box),
        ems_ph_select = input$ems_ph_select,
        ems_ph_effects = input$ems_ph_effects,
        ems_ph_plot_options = input$ems_ph_plot_options
      )
    }

    apply_mf_results_state <- function(st) {
      if (is.null(st) || !is.list(st)) return(invisible(NULL))
      if (!is.null(st$ems_conf)) updateNumericInput(session, ns("ems_conf"), value = st$ems_conf)
      if (!is.null(st$ems_dec)) updateNumericInput(session, ns("ems_dec"), value = st$ems_dec)
      if (!is.null(st$ems_disp)) updateCheckboxInput(session, ns("ems_disp"), value = isTRUE(st$ems_disp))
      if (!is.null(st$ems_show_pool)) updateCheckboxInput(session, ns("ems_show_pool"), value = isTRUE(st$ems_show_pool))
      if (!is.null(st$ems_primary_col)) try(updatePickerInput(session, ns("ems_primary_col"), selected = st$ems_primary_col), silent = TRUE)
      if (!is.null(st$ems_ems)) {
        try(updateRadioGroupButtons(session, ns("ems_ems"), selected = st$ems_ems), silent = TRUE)
        try(updateCheckboxInput(session, ns("ems_ems"), value = isTRUE(st$ems_ems)), silent = TRUE)
      }
      if (!is.null(st$ems_disp_type)) try(updateRadioGroupButtons(session, ns("ems_disp_type"), selected = st$ems_disp_type), silent = TRUE)
      if (!is.null(st$ems_target)) updateNumericInput(session, ns("ems_target"), value = st$ems_target)
      if (!is.null(st$ems_show_coeffs)) updateCheckboxInput(session, ns("ems_show_coeffs"), value = isTRUE(st$ems_show_coeffs))
      if (!is.null(st$ems_show_optimum)) updateCheckboxInput(session, ns("ems_show_optimum"), value = isTRUE(st$ems_show_optimum))
      if (!is.null(st$multi_response_tol)) updateNumericInput(session, ns("multi_response_tol"), value = st$multi_response_tol)
      if (!is.null(st$ems_show_box)) updateCheckboxInput(session, ns("ems_show_box"), value = isTRUE(st$ems_show_box))
      if (!is.null(st$ems_ph_select)) try(updatePickerInput(session, ns("ems_ph_select"), selected = st$ems_ph_select), silent = TRUE)
      if (!is.null(st$ems_ph_effects)) try(updatePickerInput(session, ns("ems_ph_effects"), selected = st$ems_ph_effects), silent = TRUE)
      if (!is.null(st$ems_ph_plot_options)) {
        try(updatePrettyCheckboxGroup(session, ns("ems_ph_plot_options"), selected = st$ems_ph_plot_options), silent = TRUE)
      }
      invisible(NULL)
    }

    observeEvent(active_data_ems(), {
      cur <- active_data_ems()
      if (is.null(cur) || !is.finite(cur)) return()
      cur_key <- as.character(cur)
      prev_key <- isolate(mf_results_state$last_active)

      if (!is.null(prev_key) && nzchar(prev_key)) {
        mf_results_state$by_response[[prev_key]] <- capture_mf_results_state()
      }

      saved <- isolate(mf_results_state$by_response[[cur_key]])
      if (!is.null(saved)) {
        apply_mf_results_state(saved)
      }
      fx <- isolate(current_effect_choices())
      if (length(fx) >= 1L) {
        is_disp <- isTRUE(isolate(input$ems_disp))
        ensure_mf_effect_pool_slot(cur, is_disp, fx)
      }
      mf_results_state$last_active <- cur_key
      mf_anova_effect_ui_rev(isolate(mf_anova_effect_ui_rev()) + 1L)
    }, ignoreInit = FALSE)

    observeEvent(input$data_ems, {
      sel <- suppressWarnings(as.numeric(input$data_ems))
      sel <- sel[is.finite(sel)]
      keep_keys <- as.character(sel)
      old <- isolate(mf_results_state$by_response)
      if (length(old) > 0) {
        mf_results_state$by_response <- old[names(old) %in% keep_keys]
      }
    }, ignoreInit = TRUE)

    effect_toggle_input_id <- function(effect_name, did = NULL, is_dispersion = FALSE) {
      safe <- gsub("[^A-Za-z0-9_]", "_", as.character(effect_name))
      mode_tag <- if (isTRUE(is_dispersion)) "disp" else "means"
      if (is.null(did) || length(did) < 1L || !is.finite(suppressWarnings(as.numeric(did[[1L]])))) {
        return(paste0("ems_keep_effect_", mode_tag, "_", safe))
      }
      paste0("ems_keep_effect_", as.integer(did[[1L]]), "_", mode_tag, "_", safe)
    }

    #' Use checkbox for ANOVA table cells (minimal markup for correct row alignment + Shiny binding).
    mf_effect_keep_checkbox_cell <- function(local_id, checked, ns_fun) {
      as.character(
        tags$input(
          id = ns_fun(local_id),
          type = "checkbox",
          checked = if (isTRUE(checked)) "checked" else NULL
        )
      )
    }

    mf_write_table_checkbox_effects <- function(did, is_dispersion, table_fx) {
      k <- mf_effect_pool_slot_key(did, is_dispersion)
      fx <- as.character(table_fx)
      fx <- fx[!is.na(fx) & nzchar(fx)]
      prev <- isolate(mf_table_checkbox_effects_by_slot[[k]])
      if (!is.null(prev) && identical(sort(prev), sort(fx))) {
        return(invisible(fx))
      }
      mf_table_checkbox_effects_by_slot[[k]] <- fx
      mf_table_checkbox_effects_rev(isolate(mf_table_checkbox_effects_rev()) + 1L)
      invisible(fx)
    }

    mf_read_table_checkbox_effects <- function(did, is_dispersion) {
      k <- mf_effect_pool_slot_key(did, is_dispersion)
      fx <- mf_table_checkbox_effects_by_slot[[k]]
      if (is.null(fx) || length(fx) < 1L) character(0) else as.character(fx)
    }

    mf_pooling_checkbox_effects_from_table <- function(display_row_names, ghost_eff = character(0)) {
      res_nm <- c("Residuals", "Residual", "Within Cells", "(Intercept)")
      setdiff(as.character(display_row_names), c(res_nm, as.character(ghost_eff)))
    }

    mf_effect_pool_key <- function(effect_name) {
      nm <- as.character(effect_name)
      if (length(nm) < 1L || !nzchar(nm[[1L]])) return("")
      if (exists("standardize_interaction", mode = "function")) {
        return(standardize_interaction(nm[[1L]]))
      }
      nm[[1L]]
    }

    mf_table_effect_is_pooled <- function(effect_name, pool_fx) {
      pool_fx <- as.character(pool_fx)
      pool_fx <- pool_fx[!is.na(pool_fx) & nzchar(pool_fx)]
      if (length(pool_fx) < 1L) return(FALSE)
      key <- mf_effect_pool_key(effect_name)
      if (!nzchar(key)) return(FALSE)
      any(vapply(pool_fx, function(p) identical(mf_effect_pool_key(p), key), logical(1)))
    }

    mf_map_table_effect_to_fx <- function(effect_name, fx) {
      effect_name <- as.character(effect_name)
      fx <- as.character(fx)
      if (effect_name %in% fx) return(effect_name)
      if (!exists("standardize_interaction", mode = "function")) return(NA_character_)
      key <- standardize_interaction(effect_name)
      hits <- fx[vapply(fx, function(ae) identical(standardize_interaction(ae), key), logical(1))]
      if (length(hits) >= 1L) hits[[1L]] else NA_character_
    }

    mf_resolve_table_effects_to_fx <- function(table_effects, fx) {
      table_effects <- as.character(table_effects)
      table_effects <- table_effects[nzchar(table_effects)]
      if (length(table_effects) < 1L) return(character(0))
      fx <- as.character(fx)
      out <- character(0)
      for (te in table_effects) {
        mapped <- mf_map_table_effect_to_fx(te, fx)
        out <- c(out, if (!is.na(mapped) && nzchar(mapped)) mapped else te)
      }
      unique(out)
    }

    mf_effect_pool_slot_key <- function(did, is_dispersion_mode) {
      paste0(as.integer(did), "_", if (isTRUE(is_dispersion_mode)) "disp" else "means")
    }

    mf_effect_pool_store <- reactiveVal(list())

    # Bumped only when the worker-facing applied pool changes (not draft-only edits).
    mf_applied_pool_rev <- reactiveVal(0L)
    # Per (response, means|disp) slot: effect names that have a Use checkbox in the rendered table.
    mf_table_checkbox_effects_by_slot <- reactiveValues()
    mf_table_checkbox_effects_rev <- reactiveVal(0L)
    # Bumped when model registry / finalization changes so checklist + commit buttons re-render.
    mf_optimization_model_rev <- reactiveVal(0L)
    bump_mf_optimization_model_rev <- function() {
      mf_optimization_model_rev(isolate(mf_optimization_model_rev()) + 1L)
    }

    mf_model_registry <- reactiveValues(by_response = list())
    mf_model_finalized <- reactiveValues(by_response = list())
    mf_optimization_session <- reactiveVal(opt_session_init())

    get_or_build_mf_fit <- function(
        d, fid, did, conf, aov_disp_l, aov_mean_l, ems_disp_type, ems_show_mixed_nest,
        f_r_types, fit_scope = c("both", "means_only", "dispersion_only"),
        pool_mean = NULL, pool_disp = NULL, available_effects = NULL, force_rebuild = FALSE) {
      fit_scope <- match.arg(fit_scope)
      pool_mean <- if (is.null(pool_mean)) character(0) else as.character(pool_mean)
      pool_disp <- if (is.null(pool_disp)) character(0) else as.character(pool_disp)
      if (!is.null(available_effects)) {
        available_effects <- as.character(available_effects)
        available_effects <- available_effects[nzchar(available_effects)]
      } else {
        available_effects <- NULL
      }
      key <- as.character(as.integer(did))
      sig_mean <- mf_fit_signature(
        did = did, fid = fid, conf = conf, ems_disp_type = ems_disp_type,
        ems_show_mixed_nest = ems_show_mixed_nest, f_r_types = f_r_types,
        pool = pool_mean, ems_disp = FALSE,
        nrow_d = if (is.data.frame(d)) nrow(d) else NA_integer_,
        ncol_d = if (is.data.frame(d)) ncol(d) else NA_integer_
      )
      sig_disp <- mf_fit_signature(
        did = did, fid = fid, conf = conf, ems_disp_type = ems_disp_type,
        ems_show_mixed_nest = ems_show_mixed_nest, f_r_types = f_r_types,
        pool = pool_disp, ems_disp = TRUE,
        nrow_d = if (is.data.frame(d)) nrow(d) else NA_integer_,
        ncol_d = if (is.data.frame(d)) ncol(d) else NA_integer_
      )
      cached <- isolate(mf_model_registry$by_response[[key]])
      cache_hit <- mf_registry_cache_hit(cached, sig_mean, sig_disp, fit_scope)
      if (isTRUE(cache_hit) && !isTRUE(force_rebuild)) {
        return(cached$fit)
      }
      fit <- multifactor_taguchi_mvp_fit_models(
        dat = d,
        factors_id = fid,
        data_id = did,
        conf = conf,
        aov_out_l = aov_disp_l,
        aov_out_mean = aov_mean_l,
        ems_disp_type = ems_disp_type,
        ems_show_mixed_nest = ems_show_mixed_nest,
        f_r_types = f_r_types,
        mean_model_override = NULL,
        fit_scope = fit_scope,
        pool_disp = pool_disp,
        available_effects = available_effects
      )
      if (isTRUE(fit$ok)) {
        mf_model_registry$by_response[[key]] <- mf_registry_store_fit(
          cached, sig_mean, sig_disp, fit_scope, fit
        )
        bump_mf_optimization_model_rev()
      }
      fit
    }

    # Clear per-response / pooling coordinator state when working data changes.
    observeEvent(anova_data_trigger(), {
      mf_results_state$by_response <- list()
      mf_results_state$last_active <- NULL
      mf_effect_pool_store(list())
      mf_applied_pool_rev(0L)
      mf_model_registry$by_response <- list()
      mf_model_finalized$by_response <- list()
      bump_mf_optimization_model_rev()
      mf_anova_effect_ui_rev(isolate(mf_anova_effect_ui_rev()) + 1L)
      mf_design_setup$show_mixed_nest <- FALSE
      mf_design_setup$f_r <- list()
      mf_design_setup$nest <- list()
      mf_design_setup$n_factors <- 0L
      mf_design_setup$ems_pool <- character(0)
      mf_setup_pool_committed(character(0))
      for (slot_k in names(isolate(reactiveValuesToList(mf_table_checkbox_effects_by_slot)))) {
        mf_table_checkbox_effects_by_slot[[slot_k]] <- NULL
      }
      mf_table_checkbox_effects_rev(isolate(mf_table_checkbox_effects_rev()) + 1L)
      loss_mf_opt_result_val(NULL)
      loss_ow_opt_result_val(NULL)
    }, ignoreInit = TRUE)

    write_mf_effect_pool_entry <- function(key, ent, bump_applied = NA) {
      st <- isolate(mf_effect_pool_store())
      prev <- st[[key]]
      if (!is.null(prev)) {
        pa_prev <- sort(unique(as.character(if (is.null(prev$applied)) character(0) else prev$applied)))
        pa_new <- sort(unique(as.character(if (is.null(ent$applied)) character(0) else ent$applied)))
        dr_prev <- sort(unique(as.character(if (is.null(prev$draft)) character(0) else prev$draft)))
        dr_new <- sort(unique(as.character(if (is.null(ent$draft)) character(0) else ent$draft)))
        if (identical(pa_prev, pa_new) && identical(dr_prev, dr_new)) {
          return(invisible(NULL))
        }
      }
      st[[key]] <- ent
      mf_effect_pool_store(st)

      if (isFALSE(bump_applied)) {
        bump_it <- FALSE
      } else if (isTRUE(bump_applied)) {
        bump_it <- TRUE
      } else {
        pa <- if (!is.null(prev) && !is.null(prev$applied)) sort(unique(as.character(prev$applied))) else NULL
        na <- if (!is.null(ent) && !is.null(ent$applied)) sort(unique(as.character(ent$applied))) else character(0)
        bump_it <- is.null(pa) || !identical(pa, na)
      }
      if (isTRUE(bump_it)) mf_applied_pool_rev(isolate(mf_applied_pool_rev()) + 1L)
      invisible(NULL)
    }

    ensure_mf_effect_pool_slot <- function(did, is_dispersion, fx) {
      fx <- as.character(fx)
      if (length(fx) < 1L || is.null(did) || !is.finite(suppressWarnings(as.numeric(did)))) {
        return(invisible(NULL))
      }
      k <- mf_effect_pool_slot_key(did, is_dispersion)
      st <- isolate(mf_effect_pool_store())
      if (!is.null(st[[k]])) return(invisible(NULL))
      setup <- mf_setup_pool_for_effects(fx)
      write_mf_effect_pool_entry(
        k,
        list(applied = setup, draft = setup),
        bump_applied = FALSE
      )
      invisible(NULL)
    }

    mf_applied_pool_without_table_checkbox <- function(did, is_dispersion, fx, toggle_fx) {
      fx <- as.character(fx)
      applied <- applied_effect_pool_for_did_mode(did, is_dispersion, fx)
      setup <- mf_setup_pool_for_effects(fx)
      applied_extra <- setdiff(applied, setup)
      if (length(applied_extra) < 1L) return(character(0))
      toggle_fx <- as.character(toggle_fx)
      toggle_fx <- toggle_fx[!is.na(toggle_fx) & nzchar(toggle_fx)]
      if (length(toggle_fx) < 1L) return(applied_extra)
      toggle_keys <- unique(vapply(toggle_fx, mf_effect_pool_key, character(1)))
      toggle_keys <- toggle_keys[nzchar(toggle_keys)]
      applied_extra[vapply(applied_extra, function(p) {
        pk <- mf_effect_pool_key(p)
        !nzchar(pk) || !(pk %in% toggle_keys)
      }, logical(1))]
    }

    compute_effect_pool_from_checkbox_inputs <- function(fx, setup_pool, did, is_dispersion = FALSE, checkbox_fx = NULL) {
      fx <- as.character(fx)
      setup_pool <- intersect(as.character(setup_pool), fx)
      # Set Up exclusions are always pooled; only track live checkboxes for table rows.
      pooled <- setup_pool
      display_fx <- setdiff(fx, setup_pool)
      toggle_fx <- if (!is.null(checkbox_fx) && length(checkbox_fx) >= 1L) {
        as.character(checkbox_fx)
      } else {
        display_fx
      }
      preserve_applied <- mf_applied_pool_without_table_checkbox(did, is_dispersion, fx, toggle_fx)
      if (length(preserve_applied) >= 1L) {
        pooled <- c(pooled, preserve_applied)
      }
      for (eff in toggle_fx) {
        cb_id <- effect_toggle_input_id(eff, did, is_dispersion)
        vid <- input[[cb_id]]
        fx_eff <- mf_map_table_effect_to_fx(eff, fx)
        pool_nm <- if (!is.na(fx_eff) && nzchar(fx_eff)) fx_eff else eff
        if (!is.null(vid) && !isTRUE(vid)) {
          pooled <- c(pooled, pool_nm)
        }
      }
      unique(intersect(as.character(pooled), fx))
    }

    applied_effect_pool_for_did_mode <- function(did, is_dispersion_mode, fx) {
      fx <- as.character(fx)
      fx <- fx[!is.na(fx) & nzchar(fx)]
      if (length(fx) < 1L) return(character(0))
      setup <- mf_setup_pool_for_effects(fx)
      key <- mf_effect_pool_slot_key(did, is_dispersion_mode)
      ent <- isolate(mf_effect_pool_store())[[key]]
      ap <- if (!is.null(ent) && !is.null(ent$applied)) as.character(ent$applied) else setup
      intersect(ap, fx)
    }

    #' Pooling for dispersion commit — matches Results \code{ems_pooled} / live \code{ems_pool} picker.
    #' @keywords internal
    mf_pool_disp_for_commit <- function(did, fx) {
      fx <- as.character(fx)
      fx <- fx[!is.na(fx) & nzchar(fx)]
      if (length(fx) < 1L) return(character(0))
      if (isTRUE(isolate(input$ems_disp))) {
        pv <- isolate(input$ems_pool)
        if (!is.null(pv) && length(pv) >= 1L) {
          return(intersect(as.character(pv), fx))
        }
      }
      ap <- applied_effect_pool_for_did_mode(did, TRUE, fx)
      if (length(ap) > 0L) return(ap)
      pm <- applied_effect_pool_for_did_mode(did, FALSE, fx)
      if (length(pm) > 0L) return(pm)
      mf_setup_pool_for_effects(fx)
    }

    observeEvent(input$factors_ems, {
      mf_effect_pool_store(list())
      mf_applied_pool_rev(isolate(mf_applied_pool_rev()) + 1L)
      mf_anova_effect_ui_rev(isolate(mf_anova_effect_ui_rev()) + 1L)
      mf_design_setup$ems_pool <- character(0)
      mf_setup_pool_committed(character(0))
    }, ignoreInit = TRUE)

    observeEvent(input$ems_disp, {
      did <- active_data_ems()
      if (!is.null(did) && is.finite(did)) {
        fx <- isolate(current_effect_choices())
        if (length(fx) >= 1L) {
          is_disp <- isTRUE(input$ems_disp)
          k <- mf_effect_pool_slot_key(did, is_disp)
          st <- isolate(mf_effect_pool_store())
          if (is.null(st[[k]])) {
            setup <- mf_setup_pool_for_effects(fx)
            write_mf_effect_pool_entry(
              k,
              list(applied = setup, draft = setup),
              bump_applied = FALSE
            )
          }
        }
      }
      mf_anova_effect_ui_rev(isolate(mf_anova_effect_ui_rev()) + 1L)
    }, ignoreInit = TRUE)

    observeEvent(input$ems_conf, {
      did <- suppressWarnings(as.numeric(active_data_ems()))
      if (!is.finite(did)) {
        return(invisible(NULL))
      }
      key <- as.character(as.integer(did))
      old <- isolate(mf_model_finalized$by_response[[key]])
      if (is.null(old)) {
        old <- list()
      }
      res <- mf_invalidate_finalized_on_conf_change(old, input$ems_conf)
      if (!isTRUE(res$changed)) {
        return(invisible(NULL))
      }
      mf_model_finalized$by_response[[key]] <- res$entry
      bump_mf_optimization_model_rev()
      resp_lbl <- {
        d <- isolate(filtered_data())
        if (is.data.frame(d) && did >= 1L && did <= ncol(d)) names(d)[did] else as.character(did)
      }
      shiny::showNotification(
        paste0(
          "Confidence changed for ", resp_lbl,
          "; means and dispersion models must be re-committed before optimization."
        ),
        type = "warning",
        duration = 6
      )
      invisible(NULL)
    }, ignoreInit = TRUE)

    multifactor_core_inputs <- anova_worker_inputs(function() {
      # Collect dynamic f_r_factor and nest_factor inputs
      # These are created dynamically based on number of factors selected
      factors_id <- input$factors_ems
      if (is.null(factors_id) || length(factors_id) == 0L) {
        factors_id <- numeric(0)
      } else {
        factors_id <- as.numeric(factors_id)
      }

      data <- filtered_data()
      if (length(factors_id) > 0L && !is.null(data)) {
        factors_id <- factors_id[factors_id >= 1L & factors_id <= ncol(data)]
      }
      if (length(factors_id) == 0L) {
        factors_id <- NULL
      }

      f_r_factors <- list()
      nest_factors <- list()
      
      if (!is.null(factors_id) && length(factors_id) > 0) {
        for (i in seq_along(factors_id)) {
          f_r_key <- paste0("f_r_factor", i)
          nest_key <- paste0("nest_factor", i)
          fv <- input[[f_r_key]]
          stored_fr <- isolate(mf_design_setup$f_r[[f_r_key]])
          f_r_factors[[f_r_key]] <- if (!is.null(fv) && nzchar(as.character(fv)[1])) {
            as.character(fv)[1]
          } else if (!is.null(stored_fr) && nzchar(stored_fr)) {
            stored_fr
          } else {
            "F"
          }
          nv <- input[[nest_key]]
          stored_nest <- isolate(mf_design_setup$nest[[nest_key]])
          if (!is.null(nv)) {
            nest_factors[[nest_key]] <- as.character(nv)
          } else if (!is.null(stored_nest)) {
            nest_factors[[nest_key]] <- stored_nest
          }
        }
      }
      
      show_mixed_nest <- if (!is.null(input$ems_show_mixed_nest)) {
        isTRUE(input$ems_show_mixed_nest)
      } else {
        isTRUE(isolate(mf_design_setup$show_mixed_nest))
      }

      base_list <- list(
        # Column selections
        factors_ems = input$factors_ems,
        data_ems = active_data_ems(),
        data_ems_all = input$data_ems,
        
        # Mixed/nested configuration
        ems_show_mixed_nest = show_mixed_nest,
        
        # Pooling + primary error
        ems_pool_setup = mf_setup_pool_committed(),
        ems_pool = current_applied_effect_pool(),
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

    multifactor_anova_cache_key <- reactive({
      # Include data fingerprint so bindCache does not serve stale tables after a new dataset
      # load when the user picks the same column indices as before.
      ci <- multifactor_core_inputs()
      keep <- c(
        "factors_ems", "data_ems", "ems_pool_setup", "ems_pool", "ems_primary_col",
        "ems_show_mixed_nest", "ems_disp", "ems_disp_type", "ems_conf", "ems_dec", "ems_ems"
      )
      dyn <- names(ci)[grepl("^f_r_factor\\d+$|^nest_factor\\d+$", names(ci))]
      list(
        logic_rev = 10L,
        data_fp = anova_data_trigger(),
        cfg = ci[c(keep, dyn)]
      )
    })

    multifactor_posthoc_inputs <- anova_worker_inputs(function() {
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
      reactive_color_palette = reactive_color_palette,
      data_invalidation_trigger = anova_data_trigger
    )

    # Pooled ANOVA is expensive; multiple UI paths used to re-invoke it per flush:
    # - ems_table + ems_table_pooled (conditionalPanel does not suspend by default)
    # - mf_effect_pool_store() updates (draft checkboxes) re-render tables but must not
    #   re-fit ANOVA until Recalculate changes multifactor_core_inputs()$ems_pool.
    ems_anova_pooled_tab <- shiny::bindCache(
      reactive({
        if (isTRUE(input$ems_disp)) {
          multifactor_worker$ems_pooled_dispersion()
        } else {
          multifactor_worker$ems_pooled()
        }
      }),
      multifactor_anova_cache_key()
    )

    ems_anova_source_tab <- shiny::bindCache(
      reactive({
        multifactor_worker$aov_out()
      }),
      multifactor_anova_cache_key()
    )

    #' Effects that have a Use checkbox in the current ANOVA table (not all design ids).
    mf_pooling_table_checkbox_effects <- function(fx, setup_pool, aov_rownames = NULL) {
      fx <- as.character(fx)
      setup_pool <- intersect(as.character(setup_pool), fx)
      display_fx <- setdiff(fx, setup_pool)
      if (is.null(aov_rownames) || length(aov_rownames) < 1L) {
        return(display_fx)
      }
      res_nm <- c("Residuals", "Residual", "Within Cells", "(Intercept)")
      tbl_fx <- setdiff(as.character(aov_rownames), res_nm)
      intersect(display_fx, tbl_fx)
    }

    mf_pooling_checkbox_effects_for_did <- function(did, is_dispersion, fx) {
      mf_table_checkbox_effects_rev()
      mf_read_table_checkbox_effects(did, is_dispersion)
    }

    mf_mixed_nest_active <- function() {
      if (!is.null(input$ems_show_mixed_nest)) {
        isTRUE(input$ems_show_mixed_nest)
      } else {
        isTRUE(isolate(mf_design_setup$show_mixed_nest))
      }
    }

    mf_nest_inputs_for_effects <- function(factors_id) {
      out <- list()
      for (i in seq_along(factors_id)) {
        nest_key <- paste0("nest_factor", i)
        nv <- input[[nest_key]]
        stored_nest <- isolate(mf_design_setup$nest[[nest_key]])
        if (!is.null(nv)) {
          out[[nest_key]] <- as.character(nv)
        } else if (!is.null(stored_nest)) {
          out[[nest_key]] <- stored_nest
        }
      }
      out
    }

    mf_effect_choice_list <- reactive({
      data <- filtered_data()
      fid <- suppressWarnings(as.numeric(input$factors_ems))
      req(data, fid)
      req(length(fid) >= 2L)
      req(all(fid >= 1L & fid <= ncol(data)))
      factors_names <- names(data)[fid]
      mixed <- mf_mixed_nest_active()
      nested_chr <- if (mixed) {
        build_mf_nested_chr(factors_names, mf_nest_inputs_for_effects(fid), TRUE)
      } else {
        NULL
      }
      ae <- ems_design_effect_ids(factors_names, nested_chr)
      req(length(ae) >= 1L)
      as.character(ae)
    })

    current_effect_choices <- reactive({
      ae <- mf_effect_choice_list()
      req(ae)
      as.character(ae)
    })

    current_applied_effect_pool <- reactive({
      mf_applied_pool_rev()
      active_data_ems()
      input$ems_disp
      mf_setup_pool_committed()
      did <- active_data_ems()
      req(did)
      fx <- current_effect_choices()
      req(length(fx) >= 1L)
      applied_effect_pool_for_did_mode(did, isTRUE(input$ems_disp), fx)
    })

    current_effect_display_choices <- reactive({
      effects <- current_effect_choices()
      if (length(effects) < 1L) return(character(0))
      setdiff(effects, current_applied_effect_pool())
    })

    observe({
      did <- active_data_ems()
      req(did)
      fx <- current_effect_choices()
      req(length(fx) >= 1L)
      is_disp <- isTRUE(input$ems_disp)
      k <- mf_effect_pool_slot_key(did, is_disp)
      setup <- mf_setup_pool_for_effects(fx)
      ensure_mf_effect_pool_slot(did, is_disp, fx)
      toggle_fx <- mf_pooling_checkbox_effects_for_did(did, is_disp, fx)
      if (length(toggle_fx) < 1L) return(invisible(NULL))
      for (eff in toggle_fx) input[[effect_toggle_input_id(eff, did, is_disp)]]
      pooled_draft <- compute_effect_pool_from_checkbox_inputs(fx, setup, did, is_disp, checkbox_fx = toggle_fx)
      # After switching means/disp, table checkboxes may not exist yet — do not overwrite draft.
      if (length(toggle_fx) > 0L) {
        bound_cb <- vapply(
          toggle_fx,
          function(eff) !is.null(input[[effect_toggle_input_id(eff, did, is_disp)]]),
          logical(1)
        )
        if (!any(bound_cb)) {
          return(invisible(NULL))
        }
      }
      st <- isolate(mf_effect_pool_store())
      ent <- st[[k]]

      if (is.null(ent)) {
        write_mf_effect_pool_entry(k, list(applied = setup, draft = pooled_draft), bump_applied = FALSE)
        return(invisible(NULL))
      }
      cur_draft <- if (!is.null(ent$draft) && length(ent$draft) >= 1L) as.character(ent$draft) else character(0)
      if (!identical(sort(cur_draft), sort(pooled_draft))) {
        write_mf_effect_pool_entry(k, list(applied = ent$applied, draft = pooled_draft), bump_applied = FALSE)
      }
    })

    observeEvent(input$ems_recalc_anova, {
      did <- active_data_ems()
      req(did)
      fx <- isolate(current_effect_choices())
      req(length(fx) >= 1L)
      setup <- mf_setup_pool_for_effects(fx)
      is_disp <- isTRUE(isolate(input$ems_disp))
      k <- mf_effect_pool_slot_key(did, is_disp)
      ensure_mf_effect_pool_slot(did, is_disp, fx)
      toggle_fx <- mf_pooling_checkbox_effects_for_did(did, is_disp, fx)
      pooled <- compute_effect_pool_from_checkbox_inputs(fx, setup, did, is_disp, checkbox_fx = toggle_fx)
      write_mf_effect_pool_entry(k, list(applied = pooled, draft = pooled), bump_applied = TRUE)
      mf_anova_effect_ui_rev(isolate(mf_anova_effect_ui_rev()) + 1L)
    }, ignoreNULL = TRUE)

    observeEvent(input$ems_reset_effect_model, {
      did <- active_data_ems()
      req(did)
      fx <- isolate(current_effect_choices())
      req(length(fx) >= 1L)
      setup <- mf_setup_pool_for_effects(fx)
      k <- mf_effect_pool_slot_key(did, isTRUE(isolate(input$ems_disp)))
      write_mf_effect_pool_entry(k, list(applied = setup, draft = setup), bump_applied = NA)
      mf_anova_effect_ui_rev(isolate(mf_anova_effect_ui_rev()) + 1L)
    }, ignoreNULL = TRUE)
    
    # ---------------------------
    # Set Up tab dynamic UI
    # ---------------------------
    output$ems_factors <- renderUI({
      anova_data_trigger()
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
    shiny::outputOptions(output, "ems_factors", suspendWhenHidden = FALSE)
    
    output$ems_data <- renderUI({
      anova_data_trigger()
      data <- filtered_data()
      req(data)
      choices <- seq_len(ncol(data))
      names(choices) <- names(data)

      pickerInput(
        inputId = ns("data_ems"),
        label = "Select Response(s) (up to 4)",
        choices = choices,
        multiple = TRUE,
        options = list(
          title = "Select response column(s)",
          "actions-box" = TRUE,
          "max-options" = 4
        )
      )
    })
    shiny::outputOptions(output, "ems_data", suspendWhenHidden = FALSE)

    output$ems_data_active <- renderUI({
      anova_data_trigger()
      data <- filtered_data()
      sel <- as.numeric(input$data_ems)
      req(data, sel)
      sel <- sel[is.finite(sel)]
      if (length(sel) < 2L) return(NULL)
      choices <- sel
      names(choices) <- names(data)[sel]
      current <- suppressWarnings(as.numeric(input$data_ems_active))
      current <- current[is.finite(current)]
      selected_val <- if (length(current) >= 1L && current[[1L]] %in% choices) {
        current[[1L]]
      } else {
        choices[[1L]]
      }
      pickerInput(
        inputId = ns("data_ems_active"),
        label = "Active response for current Results/Loss view",
        choices = choices,
        multiple = FALSE,
        selected = selected_val,
        options = list(title = "Select active response")
      )
    })
    
    output$ems_rand_select <- renderUI({
      anova_data_trigger()
      data <- filtered_data()
      factors_id <- as.numeric(input$factors_ems)
      req(data, factors_id)
      req(all(factors_id >= 1L & factors_id <= ncol(data)))
      
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
    shiny::outputOptions(output, "ems_rand_select", suspendWhenHidden = FALSE)
    
    output$ems_nest_select <- renderUI({
      anova_data_trigger()
      data <- filtered_data()
      factors_id <- as.numeric(input$factors_ems)
      req(data, factors_id)
      req(all(factors_id >= 1L & factors_id <= ncol(data)))
      
      factors_names <- names(data)[factors_id]
      if (length(factors_names) < 1) return(NULL)
      if (length(factors_names) < 2) return(NULL)
      
      controls <- lapply(seq_along(factors_names), function(i) {
        other_factors <- setdiff(factors_names, factors_names[i])
        nest_key <- paste0("nest_factor", i)
        stored <- isolate(mf_design_setup$nest[[nest_key]])
        selected <- if (!is.null(stored)) stored else NULL
        checkboxGroupInput(
          inputId = ns(nest_key),
          label = paste0(factors_names[i], " nested within"),
          choices = other_factors,
          selected = selected,
          inline = TRUE
        )
      })
      
      do.call(tagList, controls)
    })
    shiny::outputOptions(output, "ems_nest_select", suspendWhenHidden = FALSE)
    
    output$ems_primary <- renderUI({
      anova_data_trigger()
      data <- filtered_data()
      factors_id <- as.numeric(input$factors_ems)
      data_id <- as.numeric(active_data_ems())
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
    shiny::outputOptions(output, "ems_primary", suspendWhenHidden = FALSE)
    
    output$ems_pool <- renderUI({
      anova_data_trigger()
      data <- filtered_data()
      factors_id <- as.numeric(input$factors_ems)
      data_id <- as.numeric(active_data_ems())
      req(data, factors_id, data_id)
      req(all(factors_id >= 1L & factors_id <= ncol(data)))

      if (length(factors_id) < 2) return(NULL)

      choices <- mf_effect_choice_list()

      # IMPORTANT: do NOT depend on input$ems_pool here; otherwise the UI re-renders on each
      # click and the picker appears to "reset" (can't multi-select smoothly). ANOVA uses the
      # committed pool when leaving Set Up, not live picker churn (see main branch).
      pool_selected <- intersect(
        as.character(
          shiny::isolate(input$ems_pool) %||%
            shiny::isolate(mf_design_setup$ems_pool) %||%
            character(0)
        ),
        as.character(choices)
      )

      pickerInput(
        inputId = ns("ems_pool"),
        label = "Pool / Exclude from model",
        choices = list("Selected effects will be excluded from the model" = choices),
        multiple = TRUE,
        selected = pool_selected,
        options = list(
          "actions-box" = TRUE,
          "title" = "Select effects to exclude",
          "tick-icon" = "glyphicon glyphicon-remove"
        )
      )
    })
    shiny::outputOptions(output, "ems_pool", suspendWhenHidden = FALSE)

    output$ems_ems_a <- renderUI({
      anova_data_trigger()
      data <- filtered_data()
      factors_id <- as.numeric(input$factors_ems)
      data_id <- as.numeric(active_data_ems())
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

    get_saved_or_current_response_state <- function(did) {
      key <- as.character(as.integer(did))
      st <- isolate(mf_results_state$by_response[[key]])
      if (is.null(st) || !is.list(st)) {
        st <- list()
      }
      active_key <- as.character(suppressWarnings(as.integer(isolate(active_data_ems()))))
      use_live <- identical(key, active_key)
      fx <- isolate(current_effect_choices())
      pm <- applied_effect_pool_for_did_mode(did, FALSE, fx)
      pd <- applied_effect_pool_for_did_mode(did, TRUE, fx)
      disp_cur <- if (use_live) {
        isTRUE(isolate(input$ems_disp))
      } else if (!is.null(st$ems_disp)) {
        isTRUE(st$ems_disp)
      } else {
        isTRUE(isolate(input$ems_disp))
      }
      conf_cur <- if (use_live) {
        isolate(input$ems_conf)
      } else if (!is.null(st$ems_conf)) {
        st$ems_conf
      } else {
        isolate(input$ems_conf)
      }
      disp_type_cur <- if (use_live) {
        isolate(input$ems_disp_type)
      } else if (!is.null(st$ems_disp_type)) {
        st$ems_disp_type
      } else {
        isolate(input$ems_disp_type)
      }
      list(
        ems_conf = conf_cur,
        ems_disp = disp_cur,
        ems_disp_type = disp_type_cur,
        ems_pool_means = pm,
        ems_pool_disp = pd,
        ems_pool = if (disp_cur) pd else pm
      )
    }

    mf_model_readiness <- function(d, fid, dids, active_did) {
      mf_model_readiness_compute(
        d = d,
        fid = fid,
        dids = dids,
        registry_by_response = isolate(mf_model_registry$by_response),
        finalized_by_response = isolate(mf_model_finalized$by_response),
        ems_show_mixed_nest = mf_resolved_ems_show_mixed_nest(),
        f_r_types = mf_resolved_f_r_types(fid),
        get_state_fn = get_saved_or_current_response_state
      )
    }

    mf_optimization_readiness_full <- function(d, fid, dids) {
      mf_optimization_model_rev()
      mf_applied_pool_rev()
      optimization_readiness(
        d = d,
        fid = fid,
        dids = dids,
        registry_by_response = isolate(mf_model_registry$by_response),
        finalized_by_response = isolate(mf_model_finalized$by_response),
        ems_show_mixed_nest = mf_resolved_ems_show_mixed_nest(),
        f_r_types = mf_resolved_f_r_types(fid),
        get_state_fn = get_saved_or_current_response_state,
        session = isolate(mf_optimization_session()),
        pooling_pending_fn = mf_pooling_pending_differs,
        require_economics = length(dids) > 1L
      )
    }

    mf_active_commit_state <- function() {
      mf_optimization_model_rev()
      mf_applied_pool_rev()
      d <- filtered_data()
      did <- suppressWarnings(as.numeric(active_data_ems()))
      fid <- mf_resolved_factors_ems()
      if (is.null(d) || !is.data.frame(d) || is.na(did) || length(fid) < 2L) {
        return(list(mean_ready = FALSE, disp_ready = FALSE))
      }
      rr <- mf_model_readiness(d = d, fid = fid, dids = did, active_did = did)[[as.character(as.integer(did))]]
      if (is.null(rr)) return(list(mean_ready = FALSE, disp_ready = FALSE))
      list(mean_ready = isTRUE(rr$mean_ready), disp_ready = isTRUE(rr$disp_ready))
    }

    output$ems_finalize_means_btn <- renderUI({
      st <- mf_active_commit_state()
      if (isTRUE(st$mean_ready)) {
        actionButton(ns("ems_finalize_means"), "\u2713 Means Model Committed", class = "btn-success btn-sm")
      } else {
        actionButton(ns("ems_finalize_means"), "Commit Means Model for optimization", class = "btn-primary btn-sm")
      }
    })

    output$ems_finalize_dispersion_btn <- renderUI({
      st <- mf_active_commit_state()
      if (isTRUE(st$disp_ready)) {
        actionButton(ns("ems_finalize_dispersion"), "\u2713 Dispersion Model Committed", class = "btn-success btn-sm")
      } else {
        actionButton(ns("ems_finalize_dispersion"), "Commit Dispersion Model for optimization", class = "btn-warning btn-sm")
      }
    })

    observeEvent(input$ems_finalize_means, {
      d <- filtered_data()
      fid <- mf_resolved_factors_ems()
      did <- as.numeric(active_data_ems())
      req(d, fid, did, length(fid) >= 2L)
      if (isTRUE(mf_pooling_pending_differs(did, FALSE))) {
        shiny::showNotification(
          "Pooling has pending changes. Click Apply pooling & refresh ANOVA or Reset pooling to Set Up before committing the means model.",
          type = "warning",
          duration = 8
        )
        return(invisible(NULL))
      }
      st <- get_saved_or_current_response_state(did)
      conf <- if (!is.null(st$ems_conf)) st$ems_conf else 0.95
      ems_disp_type <- if (!is.null(st$ems_disp_type)) as.integer(st$ems_disp_type)[1] else 1L
      pool <- st$ems_pool_means
      aov_mean <- mf_means_anova_for_reduced_model(multifactor_worker, pool)
      f_r_types <- mf_resolved_f_r_types(fid)
      ems_show_mixed <- mf_resolved_ems_show_mixed_nest()
      ae_commit <- tryCatch(as.character(isolate(multifactor_worker$all_effects())), error = function(e) character(0))
      fit <- get_or_build_mf_fit(
        d = d, fid = fid, did = did, conf = conf, aov_disp_l = NULL, aov_mean_l = aov_mean,
        ems_disp_type = ems_disp_type, ems_show_mixed_nest = ems_show_mixed,
        f_r_types = f_r_types, pool_mean = pool, pool_disp = st$ems_pool_disp,
        available_effects = ae_commit,
        fit_scope = "means_only",
        force_rebuild = TRUE
      )
      if (!isTRUE(fit$ok) || is.null(fit$mean_mod)) {
        shiny::showNotification(
          if (nzchar(as.character(fit$message))) as.character(fit$message)[1] else "Could not commit means model.",
          type = "error",
          duration = 8
        )
        return(invisible(NULL))
      }
      key <- as.character(as.integer(did))
      sig <- mf_fit_signature(
        did = did, fid = fid, conf = conf, ems_disp_type = ems_disp_type,
        ems_show_mixed_nest = ems_show_mixed, f_r_types = f_r_types,
        pool = pool, ems_disp = FALSE,
        nrow_d = nrow(d), ncol_d = ncol(d)
      )
      old <- isolate(mf_model_finalized$by_response[[key]])
      if (is.null(old)) old <- list()
      old$means_signature <- sig
      old$means_finalized_at <- as.character(Sys.time())
      mf_model_finalized$by_response[[key]] <- old
      bump_mf_optimization_model_rev()
      resp_lbl <- if (did >= 1L && did <= ncol(d)) names(d)[did] else as.character(did)
      shiny::showNotification(
        paste0("Means model committed for ", resp_lbl, ". Commit dispersion (and repeat per response if multiple outputs are selected)."),
        type = "message",
        duration = 6
      )
    }, ignoreInit = TRUE)

    observeEvent(input$ems_finalize_dispersion, {
      d <- filtered_data()
      fid <- mf_resolved_factors_ems()
      did <- as.numeric(active_data_ems())
      req(d, fid, did, length(fid) >= 2L)
      if (isTRUE(mf_pooling_pending_differs(did, TRUE))) {
        shiny::showNotification(
          "Pooling has pending changes. Click Apply pooling & refresh ANOVA or Reset pooling to Set Up before committing the dispersion model.",
          type = "warning",
          duration = 8
        )
        return(invisible(NULL))
      }
      st <- get_saved_or_current_response_state(did)
      conf <- if (!is.null(st$ems_conf)) st$ems_conf else 0.95
      ems_disp_type <- if (!is.null(st$ems_disp_type)) as.integer(st$ems_disp_type)[1] else 1L
      ae_commit <- tryCatch(as.character(isolate(multifactor_worker$all_effects())), error = function(e) character(0))
      pool <- mf_pool_disp_for_commit(did, ae_commit)
      aov_disp <- mf_disp_anova_for_reduced_model(multifactor_worker, pool)
      aov_mean <- mf_means_anova_for_reduced_model(multifactor_worker, st$ems_pool_means)
      f_r_types <- mf_resolved_f_r_types(fid)
      ems_show_mixed <- mf_resolved_ems_show_mixed_nest()
      ae_commit <- tryCatch(as.character(isolate(multifactor_worker$all_effects())), error = function(e) character(0))
      fit <- get_or_build_mf_fit(
        d = d, fid = fid, did = did, conf = conf, aov_disp_l = aov_disp, aov_mean_l = aov_mean,
        ems_disp_type = ems_disp_type, ems_show_mixed_nest = ems_show_mixed,
        f_r_types = f_r_types, pool_mean = st$ems_pool_means, pool_disp = pool,
        available_effects = ae_commit,
        fit_scope = "dispersion_only",
        force_rebuild = TRUE
      )
      if (!isTRUE(fit$ok) || is.null(fit$disp_mod)) {
        shiny::showNotification(
          if (nzchar(as.character(fit$message))) as.character(fit$message)[1] else "Could not commit dispersion model.",
          type = "error",
          duration = 8
        )
        return(invisible(NULL))
      }
      key <- as.character(as.integer(did))
      sig <- mf_fit_signature(
        did = did, fid = fid, conf = conf, ems_disp_type = ems_disp_type,
        ems_show_mixed_nest = ems_show_mixed, f_r_types = f_r_types,
        pool = pool, ems_disp = TRUE,
        nrow_d = nrow(d), ncol_d = ncol(d)
      )
      old <- isolate(mf_model_finalized$by_response[[key]])
      if (is.null(old)) old <- list()
      old$dispersion_signature <- sig
      old$dispersion_finalized_at <- as.character(Sys.time())
      mf_model_finalized$by_response[[key]] <- old
      bump_mf_optimization_model_rev()
      resp_lbl <- if (did >= 1L && did <= ncol(d)) names(d)[did] else as.character(did)
      shiny::showNotification(
        paste0("Dispersion model committed for ", resp_lbl, "."),
        type = "message",
        duration = 6
      )
    }, ignoreInit = TRUE)
    
    format_factorial_cell_diag_html <- function(cs) {
      if (is.null(cs) || !is.list(cs)) return("")
      if (!is.finite(as.numeric(cs$min_count))) return("")
      paste0(
        " Factorial cells: ", cs$n_cells, "; replicates per cell: min ", cs$min_count,
        ", max ", cs$max_count, " (", cs$n_rows, " rows). ",
        "ADA/ADM require at least 3 observations in every factorial cell. ",
        "If min is 1, a Run, Order, Replicate, Batch, or Block effect is still active. ",
        "Deselect those effects (then click Recalculate ANOVA) so cells represent replicated treatment combinations."
      )
    }

    mf_effective_checkbox_pool_for_table <- function(data_id, is_dispersion) {
      if (is.null(data_id) || length(data_id) < 1L || !is.finite(as.numeric(data_id[[1L]]))) return(character(0))
      data_id <- as.integer(data_id[[1L]])
      fx <- isolate(current_effect_choices())
      if (length(fx) < 1L) return(character(0))
      k <- mf_effect_pool_slot_key(data_id, is_dispersion)
      ent <- mf_effect_pool_store()[[k]]
      if (is.null(ent)) {
        return(mf_setup_pool_for_effects(fx))
      }
      if (length(ent$draft) >= 1L) return(as.character(ent$draft))
      if (!is.null(ent$applied)) return(as.character(ent$applied))
      character(0)
    }

    mf_pooling_pending_differs <- function(data_id, is_dispersion) {
      if (is.null(data_id) || length(data_id) < 1L || !is.finite(as.numeric(data_id[[1L]]))) return(FALSE)
      data_id <- as.integer(data_id[[1L]])
      fx <- isolate(current_effect_choices())
      if (length(fx) < 1L) return(FALSE)
      k <- mf_effect_pool_slot_key(data_id, is_dispersion)
      ent <- isolate(mf_effect_pool_store())[[k]]
      if (is.null(ent)) return(FALSE)
      ap <- if (is.null(ent$applied)) character(0) else as.character(ent$applied)
      dr <- if (is.null(ent$draft)) character(0) else as.character(ent$draft)
      !identical(sort(unique(ap)), sort(unique(dr)))
    }

    mf_strike_pool_pending_effects <- function(data_id, is_dispersion, table_row_names = NULL) {
      if (is.null(data_id) || length(data_id) < 1L || !is.finite(as.numeric(data_id[[1L]]))) return(character(0))
      data_id <- as.integer(data_id[[1L]])
      fx <- current_effect_choices()
      if (length(fx) < 1L) return(character(0))
      ap <- applied_effect_pool_for_did_mode(data_id, is_dispersion, fx)
      dr <- mf_effective_checkbox_pool_for_table(data_id, is_dispersion)
      pending_fx <- unique(as.character(setdiff(dr, ap)))
      if (length(pending_fx) < 1L) return(character(0))
      if (is.null(table_row_names) || length(table_row_names) < 1L) return(pending_fx)
      res_nm <- c("Residuals", "Residual", "Within Cells", "(Intercept)")
      table_eff <- setdiff(as.character(table_row_names), res_nm)
      if (length(table_eff) < 1L) return(character(0))
      pending_keys <- unique(vapply(pending_fx, mf_effect_pool_key, character(1)))
      table_eff[vapply(
        table_eff,
        function(te) {
          k <- mf_effect_pool_key(te)
          nzchar(k) && k %in% pending_keys
        },
        logical(1)
      )]
    }

    mf_results_applied_pool_only <- function(data_id, is_dispersion, fx) {
      fx <- as.character(fx)
      fx <- fx[!is.na(fx) & nzchar(fx)]
      if (length(fx) < 1L) return(character(0))
      ap <- applied_effect_pool_for_did_mode(data_id, is_dispersion, fx)
      setup <- mf_setup_pool_for_effects(fx)
      unique(as.character(setdiff(ap, setup)))
    }

    mf_anova_pooled_ghost_effects <- function(data_id, is_dispersion, fx_all, aov_rownames) {
      results_pool <- mf_results_applied_pool_only(data_id, is_dispersion, fx_all)
      if (length(results_pool) < 1L) return(character(0))
      res_nm <- c("Residuals", "Residual", "Within Cells", "(Intercept)")
      current_eff <- setdiff(as.character(aov_rownames), res_nm)
      if (length(current_eff) < 1L) return(results_pool)
      current_keys <- unique(vapply(current_eff, mf_effect_pool_key, character(1)))
      current_keys <- current_keys[nzchar(current_keys)]
      results_pool[vapply(results_pool, function(p) {
        pk <- mf_effect_pool_key(p)
        nzchar(pk) && !(pk %in% current_keys)
      }, logical(1))]
    }

    mf_anova_table_display_row_names <- function(aov_rownames, ghost_eff) {
      res <- intersect(c("Residuals", "Residual", "Within Cells"), as.character(aov_rownames))
      current_eff <- setdiff(as.character(aov_rownames), res)
      eff <- unique(c(current_eff, ghost_eff))
      c(eff, res)
    }

    mf_anova_applied_pool_active <- function(data_id, is_dispersion, fx_all) {
      if (length(fx_all) < 1L) return(FALSE)
      length(applied_effect_pool_for_did_mode(data_id, is_dispersion, fx_all)) > 0L
    }

    mf_anova_table_r_squared_stats <- function(aov_table, n_obs = NULL) {
      if (is.null(aov_table) || !is.data.frame(aov_table) || !"SS" %in% names(aov_table)) {
        return(NULL)
      }
      rn <- rownames(aov_table)
      if (is.null(rn) || length(rn) < 1L) return(NULL)
      res_nm <- intersect(c("Residuals", "Residual", "Within Cells"), rn)
      if (length(res_nm) != 1L) return(NULL)
      res_nm <- res_nm[[1L]]
      ss <- suppressWarnings(as.numeric(aov_table$SS))
      if (length(ss) != length(rn) || !any(is.finite(ss))) return(NULL)
      ss_res <- ss[match(res_nm, rn)]
      if (!is.finite(ss_res) || ss_res < 0) return(NULL)
      ss_tot <- sum(ss[is.finite(ss) & ss >= 0], na.rm = TRUE)
      if (!is.finite(ss_tot) || ss_tot <= 0) return(NULL)
      r2 <- 1 - ss_res / ss_tot
      if (!is.finite(r2)) return(NULL)
      df_res <- suppressWarnings(as.integer(aov_table[res_nm, "Df"])[1L])
      adj <- NA_real_
      n_obs <- suppressWarnings(as.integer(n_obs)[1L])
      if (is.finite(n_obs) && n_obs > 1L && is.finite(df_res) && df_res > 0L) {
        adj <- 1 - (1 - r2) * (n_obs - 1L) / df_res
      }
      list(r.squared = r2, adj.r.squared = adj)
    }

    mf_html_r2_line_for_ems_table_model <- function(mod, decimals, aov_table = NULL, n_obs = NULL) {
      stats <- mf_anova_table_r_squared_stats(aov_table, n_obs)
      if (is.null(stats) && !is.null(mod)) {
        sm <- tryCatch(summary(mod), error = function(e) NULL)
        if (!is.null(sm) && !is.null(sm$r.squared)) {
          stats <- list(
            r.squared = suppressWarnings(as.numeric(sm$r.squared)[1]),
            adj.r.squared = suppressWarnings(as.numeric(sm$adj.r.squared)[1])
          )
        }
      }
      if (is.null(stats)) return("")
      r2 <- stats$r.squared
      adj <- stats$adj.r.squared
      d <- as.integer(decimals)[1]
      if (!is.finite(d) || d < 0L) d <- 4L
      fmt <- function(x) {
        if (!is.finite(x)) return("NA")
        format(round(x, d), nsmall = d, trim = TRUE, scientific = FALSE)
      }
      paste0(
        "<p class=\"text-muted\" style=\"font-size:0.9em;margin:0.35em 0 0 0;\">",
        "R\u00b2 = ", fmt(r2),
        ", adjusted R\u00b2 = ", fmt(adj),
        "</p>"
      )
    }

    # eff_types() may use unpooled aov_out() when applied_pool is empty, while Results tables
    # always render multifactor_worker$ems_pooled(). Build fixed/random assignments in table row order.
    align_eff_types_to_effect_rows <- function(eff_tbl, effect_row_names) {
      eo <- effect_row_names
      if (is.null(eo) || length(eo) < 1L) return(NULL)
      type_lbl <- vapply(eo, function(nm) {
        if (!is.null(eff_tbl) && is.data.frame(eff_tbl) && nm %in% row.names(eff_tbl)) {
          as.character(eff_tbl[nm, "type"])[1L]
        } else {
          "F"
        }
      }, character(1))
      type_code <- vapply(eo, function(nm) {
        if (!is.null(eff_tbl) && is.data.frame(eff_tbl) && nm %in% row.names(eff_tbl)) {
          as.integer(eff_tbl[nm, "type_code"])[[1L]]
        } else {
          1L
        }
      }, integer(1))
      data.frame(effect = eo, type = type_lbl, type_code = type_code, stringsAsFactors = FALSE, row.names = eo)
    }

    build_eff_types_for_rows_fast <- function(effect_row_names, factors_id, data) {
      eo <- as.character(effect_row_names)
      eo <- eo[!is.na(eo) & nzchar(eo)]
      if (length(eo) < 1L) return(NULL)
      fac_names <- make.names(names(data)[as.numeric(factors_id)])
      fr_map <- setNames(rep("F", length(fac_names)), fac_names)
      for (ii in seq_along(fac_names)) {
        fr <- input[[paste0("f_r_factor", ii)]]
        if (is.null(fr) || !nzchar(as.character(fr)[1])) fr <- "F"
        fr_map[[fac_names[[ii]]]] <- as.character(fr)[1]
      }
      type_lbl <- vapply(eo, function(nm) {
        parts <- unlist(strsplit(gsub("\\([^)]+\\)", "", nm), ":", fixed = TRUE))
        parts <- trimws(parts)
        parts <- parts[nzchar(parts)]
        if (length(parts) < 1L) return("F")
        ptypes <- fr_map[parts]
        if (any(is.na(ptypes))) return("F")
        if (any(ptypes == "R")) "R" else "F"
      }, character(1))
      type_code <- ifelse(type_lbl == "R", 0L, 1L)
      data.frame(effect = eo, type = type_lbl, type_code = type_code, stringsAsFactors = FALSE, row.names = eo)
    }

    # ---------------------------
    # Results tab placeholders
    # ---------------------------
    output$ems_anova_readiness <- renderUI({
      d <- filtered_data()
      req(d, input$factors_ems, input$data_ems)
      fid <- suppressWarnings(as.numeric(input$factors_ems))
      dids <- mf_resolved_data_ems()
      req(length(fid) >= 2L, !is.null(dids), length(dids) >= 1L)
      full_ready <- mf_optimization_readiness_full(d, fid, dids)
      optimization_checklist_ui(full_ready)
    })

    output$ems_anova_effect_pending <- renderUI({
      did <- active_data_ems()
      shiny::req(did)
      mf_effect_pool_store()
      fx <- shiny::isolate(current_effect_choices())
      if (length(fx) < 1L) return(NULL)
      k <- mf_effect_pool_slot_key(did, isTRUE(input$ems_disp))
      ent <- mf_effect_pool_store()[[k]]
      if (is.null(ent)) return(NULL)
      ap <- if (is.null(ent$applied)) character(0) else unique(as.character(ent$applied))
      dr <- if (is.null(ent$draft)) character(0) else unique(as.character(ent$draft))
      if (identical(sort(ap), sort(dr))) return(NULL)
      shiny::tags$p(
        class = "text-warning",
        style = "font-size:0.88em; margin:0.25em 0;",
        shiny::HTML(
          "<strong>Pending pooling changes.</strong> The ANOVA table still reflects the last applied model; ",
          "rows marked with strikethrough will be pooled after you click <em>Apply pooling &amp; refresh ANOVA</em>. ",
          "Use <em>Reset pooling to Set Up model</em> to clear pending and applied pooling to match Set Up (then apply if needed)."
        )
      )
    })

    output$ems_table <- renderUI({
      mf_anova_effect_ui_rev()
      active_data_ems()
      conf <- input$ems_conf
      R <- input$ems_dec
      show_ems <- input$ems_ems
      disp <- isTRUE(input$ems_disp)
      show_rfc <- isTRUE(input$ems_show_rfc)
      rfc_th <- if (show_rfc) ems_anova_rfc_header() else ""
      
      req(conf, R)
      
      data <- filtered_data()
      data_id <- as.numeric(active_data_ems())
      factors_id <- as.numeric(input$factors_ems)
      req(data, data_id, factors_id)
      req(data_id >= 1L && data_id <= ncol(data))
      req(all(factors_id >= 1L & factors_id <= ncol(data)))
      req(length(factors_id) >= 2L)

      checkbox_pool <- mf_effective_checkbox_pool_for_table(data_id, disp)

      fx_all <- current_effect_choices()
      use_pooled_source <- mf_anova_applied_pool_active(data_id, disp, fx_all)

      aov_out_l <- if (use_pooled_source) {
        ems_anova_pooled_tab()
      } else {
        ems_anova_source_tab()
      }
      disp_warning_msg <- NULL
      if (is.null(aov_out_l)) {
        disp_warning_msg <- "Not enough samples within cell to calculate dispersion. Table shown as placeholder so effects can be pooled."
        fx <- current_effect_display_choices()
        if (length(fx) < 1L) {
          fx <- character(0)
        }
        rn <- c(fx, "Residuals")
        aov_out_l <- data.frame(
          SS = rep(NA_real_, length(rn)),
          Df = rep(NA_real_, length(rn)),
          MS = rep(NA_real_, length(rn)),
          Fvalue = rep(NA_real_, length(rn)),
          Pvalue = rep(NA_real_, length(rn)),
          stringsAsFactors = FALSE
        )
        rownames(aov_out_l) <- rn
      }
      if (!is.null(disp_warning_msg) && isTRUE(disp)) {
        cs_tbl <- tryCatch(multifactor_worker$factorial_cell_summary(), error = function(e) NULL)
        dt_tbl <- format_factorial_cell_diag_html(cs_tbl)
        if (nzchar(dt_tbl)) disp_warning_msg <- paste0(disp_warning_msg, dt_tbl)
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

      # If still on unpooled EMSanova (no Set Up exclusions), subset to active effects only when
      # the worker table is larger (avoids NA aliased rows from saturated factorial fits).
      kept_fx <- current_effect_display_choices()
      if (!use_pooled_source && length(kept_fx) >= 1L) {
        rn_all <- rownames(aov_out_l)
        res_nm <- c("Residuals", "Residual", "Within Cells")
        allow <- (rn_all %in% kept_fx) | (rn_all %in% res_nm)
        if ("(Intercept)" %in% rn_all && !("(Intercept)" %in% kept_fx)) {
          allow <- allow & !(rn_all %in% "(Intercept)")
        }
        keep_ord <- rn_all[allow]
        if (length(keep_ord) >= 1L && length(keep_ord) < length(rn_all)) {
          aov_out_l <- aov_out_l[keep_ord, , drop = FALSE]
        }
      }

      # Determine residual row name used by this table
      residual_row <- if ("Residuals" %in% rownames(aov_out_l)) "Residuals" else "Residual"
      
      if (is.null(disp_warning_msg)) {
        # Effect size calculations (intercept excluded from omega^2 / ICC / %RFC metric rows)
        msw <- aov_out_l[residual_row, "MS"]
        omega <- anova_table_omega_squared_values(aov_out_l, residual_row)

        effect_rows <- anova_table_effect_rows(aov_out_l, residual_row)
        metric_rows <- anova_table_metric_rows(aov_out_l, residual_row)

        # ICC calculations for random effects (metric rows only)
        msb <- aov_out_l$MS[metric_rows]
        J <- rep(NA_real_, length(metric_rows))
        sum_n <- rep(NA_real_, length(metric_rows))
        sum_nsq <- rep(NA_real_, length(metric_rows))
        for (i in seq_along(metric_rows)) {
          combo <- c(str_split(metric_rows[i], ":", simplify = TRUE))
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
        names(ICC) <- metric_rows

        effects_f_r <- build_eff_types_for_rows_fast(metric_rows, factors_id, data)
        if (is.null(effects_f_r) || nrow(effects_f_r) < 1L) {
          effects_f_r <- data.frame(
            effect = metric_rows,
            type = rep("F", length(metric_rows)),
            type_code = rep(1L, length(metric_rows)),
            stringsAsFactors = FALSE,
            row.names = metric_rows
          )
        }
        type_before <- rep("F", length(effect_rows))
        names(type_before) <- effect_rows
        type_before[metric_rows] <- effects_f_r$type
        imp_before <- rep(NA_real_, length(effect_rows))
        names(imp_before) <- effect_rows
        imp_before[metric_rows] <- ifelse(effects_f_r$type_code == 1L, omega[metric_rows], ICC)

        pvals <- anova_pvalue_numeric(aov_out_l$Pvalue)
        names(pvals) <- rownames(aov_out_l)
        for (nm in metric_rows) {
          if (!is.na(pvals[nm]) && pvals[nm] >= (1 - conf)) {
            imp_before[nm] <- 0
          }
        }
        imp_before[imp_before < 0] <- 0
        imp_before[is.na(imp_before)] <- 0
        imp_fmt <- rep("", length(effect_rows))
        names(imp_fmt) <- effect_rows
        imp_fmt[metric_rows] <- paste0(as.character(ro(as.numeric(imp_before[metric_rows]), R)), "%")

        aov_out_l$Type <- c(type_before, "R")
        aov_out_l$imp <- c(imp_fmt, "")

        if (show_rfc) {
          rfc_numeric <- compute_percent_rfc(aov_out_l, effects_f_r, conf, residual_rows = residual_row)
          aov_out_l$rfc <- format_anova_rfc_column(rfc_numeric, R, ro_fun = ro)
        }
      } else {
        aov_out_l$Type <- c(rep("F", max(nrow(aov_out_l) - 1L, 0L)), "R")
        aov_out_l$imp <- rep("", nrow(aov_out_l))
      }
      aov_out_for_r2 <- aov_out_l
      aov_out_l <- anova_table_strip_intercept_for_display(aov_out_l)
      t_after_metrics <- Sys.time()
      
      # Header text
      if (disp) {
        type_name <- c("ADA", "ADM", "ADM<sub>(n-1)</sub>")
        disp_type <- as.numeric(input$ems_disp_type)
        header <- paste0("Dependent Variable: ", names(data)[data_id], "<br>Dispersion Analysis based on ", type_name[disp_type])
      } else {
        header <- paste0("Dependent Variable: ", names(data)[data_id], "<br>Means Analysis")
      }

      # ANOVA Notes (ported behavior) — prefer notes bundled on the cached table
      notes <- mf_anova_notes_from_table(aov_out_l, multifactor_worker$anova_notes())
      if (isTruthy(notes)) {
        header <- paste0(header, "<br>ANOVA Notes: ", notes)
      }
      if (isTruthy(disp_warning_msg)) {
        header <- paste0(header, "<br><span style='color:#b45309;'><b>Warning:</b> ", disp_warning_msg, "</span>")
      }
      
      out_row <- seq_len(nrow(aov_out_l))
      cbp <- checkbox_pool
      ghost_eff <- mf_anova_pooled_ghost_effects(data_id, disp, fx_all, rownames(aov_out_l))
      display_row_names <- mf_anova_table_display_row_names(rownames(aov_out_l), ghost_eff)
      table_cb_fx <- mf_pooling_checkbox_effects_from_table(display_row_names, ghost_eff)
      mf_write_table_checkbox_effects(data_id, disp, table_cb_fx)
      strike_eff <- mf_strike_pool_pending_effects(data_id, disp, display_row_names)
      ensure_mf_effect_pool_slot(data_id, disp, fx_all)
      effect_keep_cell <- function(effect_name) {
        if (effect_name %in% c("Residuals", "Residual", "Within Cells", "(Intercept)")) return("")
        if (effect_name %in% ghost_eff) return("")
        id <- effect_toggle_input_id(effect_name, data_id, disp)
        checked <- !mf_table_effect_is_pooled(effect_name, cbp)
        mf_effect_keep_checkbox_cell(id, checked, ns)
      }
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
        out_col <- ems_anova_append_rfc_column(c("SS", "Df", "MS", "Fvalue", "Pvalue", "Type", "imp"), show_rfc)
        html <- paste(
          header,
          "<table><tr><th><b>Use</b></th><th><b>Source</b></th><th><b>SS</b></th><th><b>df</b></th><th><b>MS</b></th><th><b>F</b></th><th><b>p</b></th><th><b>Type(F/R)</b></th><th><b>",
          withMathJax("$$\\omega^2$$"), " or ICC</b></th>",
          rfc_th
        )
      } else if (isTRUE(show_ems)) {
        out_col <- ems_anova_append_rfc_column(c("SS", "Df", "MS", "Fvalue", "Pvalue", "Type", "imp", "EMS"), show_rfc)
        html <- paste(
          header,
          "<table><tr><th><b>Use</b></th><th><b>Source</b></th><th><b>SS</b></th><th><b>df</b></th><th><b>MS</b></th><th><b>F</b></th><th><b>p</b></th><th><b>Type(F/R)</b></th><th><b>",
          withMathJax("$$\\omega^2$$"), " or ICC</b></th>",
          rfc_th,
          "<th><b>EMS</b></th>"
        )
      } else {
        out_col <- ems_anova_append_rfc_column(c("SS", "Df", "MS", "Fvalue", "Pvalue", "Type", "imp"), show_rfc)
        html <- paste(
          header,
          "<table><tr><th><b>Use</b></th><th><b>Source</b></th><th><b>SS</b></th><th><b>df</b></th><th><b>MS</b></th><th><b>F</b></th><th><b>p</b></th><th><b>Type(F/R)</b></th><th><b>",
          withMathJax("$$\\omega^2$$"), " or ICC</b></th>",
          rfc_th
        )
      }
      
      # Match monolithic behavior: round once, then paste values into HTML
      # (monolithic uses `ro <- round.object` from lolcat)
      aov_out_r <- lolcat::round.object(aov_out_l, R)

      for (effect_name in display_row_names) {
        if (effect_name %in% ghost_eff) {
          eff_esc <- htmltools::htmlEscape(effect_name)
          eff_cell <- paste0(
            "<b>", eff_esc,
            "</b> <span class=\"text-muted\" style=\"font-size:0.78em\">(pooled)</span>"
          )
          html <- paste0(
            html,
            "<tr style=\"opacity:0.75;background-color:#f8f9fa;\"><td></td><td>", eff_cell, "</td>"
          )
          for (j in out_col) {
            html <- paste(html, "<td></td>")
          }
          html <- paste(html, "</tr>")
          next
        }
        i <- match(effect_name, rownames(aov_out_r))
        if (is.na(i)) next
        eff_esc <- htmltools::htmlEscape(effect_name)
        if (effect_name %in% strike_eff) {
          eff_cell <- paste0(
            "<span style=\"text-decoration:line-through;opacity:0.82\">", eff_esc,
            "</span> <span class=\"text-muted\" style=\"font-size:0.78em\">(will pool)</span>"
          )
        } else {
          eff_cell <- paste0("<b>", eff_esc, "</b>")
        }
        row_style <- if (effect_name %in% strike_eff) {
          " style=\"text-decoration:line-through;opacity:0.82;background-color:#f8f9fa;\""
        } else {
          ""
        }
        html <- paste0(html, "<tr", row_style, "><td>", effect_keep_cell(effect_name), "</td><td>", eff_cell, "</td>")
        for (j in out_col) {
          if (rownames(aov_out_r)[i] == "(Intercept)" && (j %in% c("imp", "rfc"))) {
            html <- paste(html, "<td></td>")
            next
          }
          if (rownames(aov_out_r)[i] %in% c("Residuals", "Residual") && (j %in% c("Fvalue", "Pvalue", "imp", "rfc"))) {
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

      mod_r2 <- tryCatch(multifactor_worker$aov_model(), error = function(e) NULL)
      r2_html <- mf_html_r2_line_for_ems_table_model(mod_r2, R, aov_table = aov_out_for_r2, n_obs = nrow(data))

      HTML(paste0(paste(html, "</table>"), r2_html))
    })
    
    output$ems_table_pooled <- renderUI({
      mf_anova_effect_ui_rev()
      active_data_ems()
      conf <- input$ems_conf
      R <- input$ems_dec
      disp <- isTRUE(input$ems_disp)
      show_ems <- input$ems_ems
      show_rfc <- isTRUE(input$ems_show_rfc)
      rfc_th <- if (show_rfc) ems_anova_rfc_header() else ""
      req(conf, R)
      fx_req <- isolate(current_effect_choices())
      req(length(fx_req) >= 1L)
      did_req <- active_data_ems()
      req(did_req)
      req(mf_anova_applied_pool_active(did_req, isTRUE(input$ems_disp), fx_req))
      
      data <- filtered_data()
      data_id <- as.numeric(active_data_ems())
      factors_id <- as.numeric(input$factors_ems)
      req(data, data_id, factors_id)
      req(data_id >= 1L && data_id <= ncol(data))
      req(all(factors_id >= 1L & factors_id <= ncol(data)))
      req(length(factors_id) >= 2L)

      checkbox_pool <- mf_effective_checkbox_pool_for_table(data_id, disp)

      aov_out_l <- ems_anova_pooled_tab()
      disp_warning_msg <- NULL
      if (is.null(aov_out_l)) {
        disp_warning_msg <- "Not enough samples within cell to calculate dispersion. Table shown as placeholder so effects can be pooled."
        fx <- current_effect_display_choices()
        if (length(fx) < 1L) {
          fx <- character(0)
        }
        rn <- c(fx, "Residuals")
        aov_out_l <- data.frame(
          SS = rep(NA_real_, length(rn)),
          Df = rep(NA_real_, length(rn)),
          MS = rep(NA_real_, length(rn)),
          Fvalue = rep(NA_real_, length(rn)),
          Pvalue = rep(NA_real_, length(rn)),
          stringsAsFactors = FALSE
        )
        rownames(aov_out_l) <- rn
      }
      if (!is.null(disp_warning_msg) && isTRUE(disp)) {
        cs_p <- tryCatch(multifactor_worker$factorial_cell_summary(), error = function(e) NULL)
        dt_p <- format_factorial_cell_diag_html(cs_p)
        if (nzchar(dt_p)) disp_warning_msg <- paste0(disp_warning_msg, dt_p)
      }
      if (is.character(aov_out_l) && length(aov_out_l) == 1) {
        return(HTML(aov_out_l))
      }
      req(is.data.frame(aov_out_l))
      
      residual_row <- if ("Within Cells" %in% rownames(aov_out_l)) {
        "Within Cells"
      } else if ("Residuals" %in% rownames(aov_out_l)) {
        "Residuals"
      } else {
        "Residual"
      }
      
      if (is.null(disp_warning_msg)) {
        msw <- aov_out_l[residual_row, "MS"]
        omega <- anova_table_omega_squared_values(aov_out_l, residual_row)

        effect_rows <- anova_table_effect_rows(aov_out_l, residual_row)
        metric_rows <- anova_table_metric_rows(aov_out_l, residual_row)

        msb <- aov_out_l$MS[metric_rows]
        J <- rep(NA_real_, length(metric_rows))
        sum_n <- rep(NA_real_, length(metric_rows))
        sum_nsq <- rep(NA_real_, length(metric_rows))
        for (i in seq_along(metric_rows)) {
          combo <- c(str_split(metric_rows[i], ":", simplify = TRUE))
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
        names(ICC) <- metric_rows

        effects_f_r <- build_eff_types_for_rows_fast(metric_rows, factors_id, data)
        if (is.null(effects_f_r) || nrow(effects_f_r) < 1L) {
          effects_f_r <- data.frame(
            effect = metric_rows,
            type = rep("F", length(metric_rows)),
            type_code = rep(1L, length(metric_rows)),
            stringsAsFactors = FALSE,
            row.names = metric_rows
          )
        }

        type_before <- rep("F", length(effect_rows))
        names(type_before) <- effect_rows
        type_before[metric_rows] <- effects_f_r$type
        imp_before <- rep(NA_real_, length(effect_rows))
        names(imp_before) <- effect_rows
        imp_before[metric_rows] <- ifelse(effects_f_r$type_code == 1L, omega[metric_rows], ICC)

        pvals <- anova_pvalue_numeric(aov_out_l$Pvalue)
        names(pvals) <- rownames(aov_out_l)
        for (nm in metric_rows) {
          if (!is.na(pvals[nm]) && pvals[nm] >= (1 - conf)) {
            imp_before[nm] <- 0
          }
        }
        imp_before[imp_before < 0] <- 0
        imp_before[is.na(imp_before)] <- 0
        imp_fmt <- rep("", length(effect_rows))
        names(imp_fmt) <- effect_rows
        imp_fmt[metric_rows] <- paste0(as.character(ro(as.numeric(imp_before[metric_rows]), R)), "%")

        aov_out_l$Type <- c(type_before, "R")
        aov_out_l$imp <- c(imp_fmt, "")

        if (show_rfc) {
          rfc_numeric <- compute_percent_rfc(aov_out_l, effects_f_r, conf, residual_rows = residual_row)
          aov_out_l$rfc <- format_anova_rfc_column(rfc_numeric, R, ro_fun = ro)
        }
      } else {
        aov_out_l$Type <- c(rep("F", max(nrow(aov_out_l) - 1L, 0L)), "R")
        aov_out_l$imp <- rep("", nrow(aov_out_l))
      }
      aov_out_for_r2 <- aov_out_l
      aov_out_l <- anova_table_strip_intercept_for_display(aov_out_l)
      
      # Header text
      if (disp) {
        type_name <- c("ADA", "ADM", "ADM<sub>(n-1)</sub>")
        disp_type <- as.numeric(input$ems_disp_type)
        header <- paste0("Dependent Variable: ", names(data)[data_id], "<br>Dispersion Analysis based on ", type_name[disp_type])
      } else {
        header <- paste0("Dependent Variable: ", names(data)[data_id], "<br>Means Analysis")
      }

      # ANOVA Notes (ported behavior) — prefer notes bundled on the cached table
      notes <- mf_anova_notes_from_table(aov_out_l, multifactor_worker$anova_notes())
      if (isTruthy(notes)) {
        header <- paste0(header, "<br>ANOVA Notes: ", notes)
      }
      if (isTruthy(disp_warning_msg)) {
        header <- paste0(header, "<br><span style='color:#b45309;'><b>Warning:</b> ", disp_warning_msg, "</span>")
      }
      
      out_row <- seq_len(nrow(aov_out_l))
      cbp <- checkbox_pool
      fx_all <- current_effect_choices()
      ghost_eff <- mf_anova_pooled_ghost_effects(data_id, disp, fx_all, rownames(aov_out_l))
      display_row_names <- mf_anova_table_display_row_names(rownames(aov_out_l), ghost_eff)
      table_cb_fx <- mf_pooling_checkbox_effects_from_table(display_row_names, ghost_eff)
      mf_write_table_checkbox_effects(data_id, disp, table_cb_fx)
      strike_eff <- mf_strike_pool_pending_effects(data_id, disp, display_row_names)
      ensure_mf_effect_pool_slot(data_id, disp, fx_all)
      effect_keep_cell <- function(effect_name) {
        if (effect_name %in% c("Residuals", "Residual", "Within Cells", "(Intercept)")) return("")
        if (effect_name %in% ghost_eff) return("")
        id <- effect_toggle_input_id(effect_name, data_id, disp)
        checked <- !mf_table_effect_is_pooled(effect_name, cbp)
        mf_effect_keep_checkbox_cell(id, checked, ns)
      }
      if (is.logical(show_ems) && isTRUE(show_ems)) {
        out_col <- ems_anova_append_rfc_column(c("SS", "Df", "MS", "Fvalue", "Pvalue", "Type", "imp", "EMS"), show_rfc)
        html <- paste(
          header,
          "<table><tr><th><b>Use</b></th><th><b>Source</b></th><th><b>SS</b></th><th><b>df</b></th><th><b>MS</b></th><th><b>F</b></th><th><b>p</b></th><th><b>Type(F/R)</b></th><th><b>",
          withMathJax("$$\\omega^2$$"), " or ICC</b></th>",
          rfc_th,
          "<th><b>EMS</b></th>"
        )
      } else {
        out_col <- ems_anova_append_rfc_column(c("SS", "Df", "MS", "Fvalue", "Pvalue", "Type", "imp"), show_rfc)
        html <- paste(
          header,
          "<table><tr><th><b>Use</b></th><th><b>Source</b></th><th><b>SS</b></th><th><b>df</b></th><th><b>MS</b></th><th><b>F</b></th><th><b>p</b></th><th><b>Type(F/R)</b></th><th><b>",
          withMathJax("$$\\omega^2$$"), " or ICC</b></th>",
          rfc_th
        )
      }
      
      # Match monolithic behavior: round once, then paste values into HTML
      # (monolithic uses `ro <- round.object` from lolcat)
      aov_out_r <- lolcat::round.object(aov_out_l, R)
      for (effect_name in display_row_names) {
        if (effect_name %in% ghost_eff) {
          eff_esc <- htmltools::htmlEscape(effect_name)
          eff_cell <- paste0(
            "<b>", eff_esc,
            "</b> <span class=\"text-muted\" style=\"font-size:0.78em\">(pooled)</span>"
          )
          html <- paste0(
            html,
            "<tr style=\"opacity:0.75;background-color:#f8f9fa;\"><td></td><td>", eff_cell, "</td>"
          )
          for (j in out_col) {
            html <- paste(html, "<td></td>")
          }
          html <- paste(html, "</tr>")
          next
        }
        i <- match(effect_name, rownames(aov_out_r))
        if (is.na(i)) next
        eff_esc <- htmltools::htmlEscape(effect_name)
        if (effect_name %in% strike_eff) {
          eff_cell <- paste0(
            "<span style=\"text-decoration:line-through;opacity:0.82\">", eff_esc,
            "</span> <span class=\"text-muted\" style=\"font-size:0.78em\">(will pool)</span>"
          )
        } else {
          eff_cell <- paste0("<b>", eff_esc, "</b>")
        }
        row_style <- if (effect_name %in% strike_eff) {
          " style=\"text-decoration:line-through;opacity:0.82;background-color:#f8f9fa;\""
        } else {
          ""
        }
        html <- paste0(html, "<tr", row_style, "><td>", effect_keep_cell(effect_name), "</td><td>", eff_cell, "</td>")
        for (j in out_col) {
          if (rownames(aov_out_r)[i] == "(Intercept)" && (j %in% c("imp", "rfc"))) {
            html <- paste(html, "<td></td>")
            next
          }
          if (rownames(aov_out_r)[i] %in% c("Residuals", "Residual", "Within Cells") && (j %in% c("Fvalue", "Pvalue", "imp", "rfc"))) {
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

      mod_r2p <- tryCatch(multifactor_worker$aov_model(), error = function(e) NULL)
      r2_htmlp <- mf_html_r2_line_for_ems_table_model(mod_r2p, R, aov_table = aov_out_for_r2, n_obs = nrow(data))
      pool_show <- applied_effect_pool_for_did_mode(data_id, disp, current_effect_choices())

      if (isTRUE(input$ems_show_pool)) {
        pool_txt <- if (length(pool_show) > 0L) paste0(pool_show, collapse = " || ") else "(none)"
        HTML(paste0(paste(html, "</table>"), r2_htmlp, "<br><br><br><b><u>Pooled Effects</u></b><br><br>", pool_txt))
      } else {
        HTML(paste0(paste(html, "</table>"), r2_htmlp))
      }
    })
    shiny::outputOptions(output, "ems_table", suspendWhenHidden = TRUE)
    shiny::outputOptions(output, "ems_table_pooled", suspendWhenHidden = TRUE)
    
    output$emssigeffects <- renderPlot({
      multifactor_worker$ems_sig_effects_plot()
    }, width = 800, height = 400)
    shiny::outputOptions(output, "emssigeffects", suspendWhenHidden = TRUE)

    # Download significant effects plot
    emssigeffects_height <- reactive(400 * 4)
    emssigeffects_width <- reactive(400 * 8)
    downloadServer("emssigeffects", multifactor_worker$ems_sig_effects_plot, height = emssigeffects_height, width = emssigeffects_width)

    output$ems_nested_section <- renderUI({
      if (!isTRUE(input$ems_show_mixed_nest)) {
        return(NULL)
      }
      effects <- multifactor_worker$significant_nested_effects()
      random_nested <- multifactor_worker$significant_nested_random_effects()
      if (length(effects) == 0 && length(random_nested) == 0) {
        return(NULL)
      }

      random_note <- if (length(random_nested) > 0) {
        tags$p(
          tags$em(
            paste0(
              "Significant random nested effect(s): ",
              paste(random_nested, collapse = ", "),
              ". Stratum F tables and plots are not shown for random nested effects; ",
              "see the ANOVA table (ICC) and interaction plots below."
            )
          )
        )
      } else {
        NULL
      }

      if (length(effects) == 0) {
        return(tagList(
          tags$h4("Significant Nested Effects"),
          random_note
        ))
      }

      tagList(
        tags$h4("Significant Nested Effects"),
        random_note,
        tags$p(
          "Estimated marginal means within each parent stratum. ",
          "The table reports overall EMS ANOVA results and within-stratum F tests ",
          "(numerator from one-way ANOVA holding parents constant; denominator MS from the full nested model residual)."
        ),
        uiOutput(ns("ems_nested_plots")),
        tags$br(),
        DTOutput(ns("ems_nested_stratum_table")),
        fluidRow(
          column(6, downloadButton(outputId = ns("ems_nested_dl"), label = "Download nested plots as zip")),
          column(6, tags$div(
            id = "inline1",
            class = "inline",
            pickerInput(
              inputId = ns("ems_nested_dl_format"),
              label = "Format: ",
              choices = c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"),
              selected = "svg",
              width = "75px"
            )
          ))
        )
      )
    })
    shiny::outputOptions(output, "ems_nested_section", suspendWhenHidden = TRUE)

    output$ems_nested_plots <- renderUI({
      effects <- multifactor_worker$significant_nested_effects()
      if (length(effects) == 0) {
        return(NULL)
      }

      plot_list <- lapply(seq_along(effects), function(i) {
        plotOutput(outputId = ns(paste0("ems_nested_plot_", i)), width = "auto", height = "auto")
      })
      do.call(tagList, plot_list)
    })

    observe({
      effects <- multifactor_worker$significant_nested_effects()
      req(length(effects) > 0)

      for (i in seq_along(effects)) {
        local({
          plot_idx <- i
          plot_effect <- effects[[i]]
          output[[paste0("ems_nested_plot_", plot_idx)]] <- renderPlot(
            {
              multifactor_worker$nested_effect_plot(plot_effect)
            },
            width = 800,
            height = multifactor_worker$nested_effect_plot_height_one(plot_effect)
          )
        })
      }
    })

    output$ems_nested_stratum_table <- renderDT({
      tbl <- multifactor_worker$nested_stratum_table_data()
      req(tbl)
      R <- input$ems_dec

      display <- tbl[, c("Source", "SS", "Df", "MS", "Fvalue", "Pvalue", "Sig", "Notes"), drop = FALSE]
      numeric_cols <- c("SS", "Df", "MS", "Fvalue", "Pvalue")
      for (col in numeric_cols) {
        display[[col]] <- as.numeric(display[[col]])
      }
      display <- ro(display, R)

      footnotes <- attr(tbl, "footnotes")
      caption <- if (length(footnotes) > 0) {
        paste(footnotes, collapse = " ")
      } else {
        ""
      }

      DT::datatable(
        display,
        caption = caption,
        options = list(
          dom = "t",
          paging = FALSE,
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        rownames = FALSE
      )
    })

    output$ems_nested_dl <- downloadHandler(
      filename = function() {
        paste0("ems_nested_effects_", Sys.Date(), ".zip")
      },
      content = function(file) {
        effects <- multifactor_worker$significant_nested_effects()
        req(length(effects) > 0)
        fmt <- input$ems_nested_dl_format
        if (!isTruthy(fmt)) fmt <- "svg"

        tmpdir <- tempfile("ems_nested_")
        dir.create(tmpdir)

        files <- character(0)
        for (i in seq_along(effects)) {
          p <- multifactor_worker$nested_effect_plot(effects[[i]])
          out_path <- file.path(tmpdir, paste0("nested_plot_", i, ".", fmt))
          ggplot2::ggsave(
            filename = out_path,
            plot = p,
            device = fmt,
            width = 8,
            height = multifactor_worker$nested_effect_plot_height_one(effects[[i]]) / 100,
            units = "in"
          )
          files <- c(files, out_path)
        }

        utils::zip(zipfile = file, files = files, flags = "-j")
      }
    )
    
    output$multi_coeff_est <- renderDT({
      if (!isTRUE(input$ems_show_coeffs)) {
        return(NULL)
      }

      model <- multifactor_worker$model_mean_est()
      R <- input$ems_dec
      req(model, R)

      coeffs <- ro(data.frame(model$coefficients), R)
      only_intercept <- nrow(coeffs) == 1L &&
        "(Intercept)" %in% rownames(coeffs)
      cap <- if (isTRUE(only_intercept)) {
        "No significant effects at the current confidence; intercept-only (grand mean) model."
      } else {
        NULL
      }
      DT::datatable(
        coeffs,
        caption = cap,
        options = list(dom = "t", paging = FALSE)
      )
    })

    observe({
      if (!isTRUE(multifactor_worker$has_fixed_factors())) {
        updateCheckboxInput(session, ns("ems_show_optimum"), value = FALSE)
      }
    })

    output$ems_graphs_optimum_section <- renderUI({
      if (!isTRUE(multifactor_worker$has_fixed_factors())) {
        return(NULL)
      }
      checkboxInput(
        inputId = ns("ems_show_optimum"),
        label = "Show Settings with Estimated Average Closest to Target?",
        value = isTRUE(input$ems_show_optimum)
      )
    })
    shiny::outputOptions(output, "ems_graphs_optimum_section", suspendWhenHidden = TRUE)

    output$ems_graphs_optimum_details <- renderUI({
      if (!isTRUE(multifactor_worker$has_fixed_factors()) || !isTRUE(input$ems_show_optimum)) {
        return(NULL)
      }
      tol_val <- suppressWarnings(as.numeric(input$multi_response_tol))
      if (length(tol_val) != 1L || !is.finite(tol_val)) tol_val <- 0
      tagList(
        tags$div(
          id = "inline1",
          class = "inline",
          numericInput(
            inputId = ns("multi_response_tol"),
            label = "Range of Averages Around Target to Display",
            value = tol_val
          )
        ),
        tags$p(
          "Mark factors as continuous to search interpolated settings between observed levels (within the experimental range). ",
          "In mixed models, settings use the reduced fixed-effects model (averaged over random factors)."
        ),
        uiOutput(ns("ems_factor_type_controls"))
      )
    })
    shiny::outputOptions(output, "ems_graphs_optimum_details", suspendWhenHidden = TRUE)

    output$ems_factor_type_controls <- renderUI({
      if (!isTRUE(input$ems_show_optimum)) {
        return(NULL)
      }

      model <- multifactor_worker$model_mean_est()
      if (is.null(model)) {
        return(tags$p(
          style = "color: #666; font-style: italic;",
          "Run the analysis with at least one significant effect to configure factor types."
        ))
      }

      factor_names <- multifactor_model_factor_names(model)
      if (length(factor_names) == 0L) {
        return(tags$p(
          style = "color: #666; font-style: italic;",
          "Intercept-only reduced model (no significant effects at the current confidence)."
        ))
      }

      data <- filtered_data()
      req(data)

      controls <- lapply(seq_along(factor_names), function(i) {
        coded_levels <- multifactor_factor_numeric_levels(data, factor_names[i])
        level_hint <- if (length(coded_levels) > 0L) {
          paste(coded_levels, collapse = ", ")
        } else {
          "n/a"
        }

        fluidRow(
          column(
            4,
            tags$strong(factor_names[i])
          ),
          column(
            8,
            tags$div(
              materialSwitch(
                inputId = ns(paste0("ems_factor_continuous", i)),
                label = "Discrete",
                value = FALSE,
                inline = TRUE,
                status = "primary"
              ),
              tags$span("Continuous")
            ),
            conditionalPanel(
              condition = paste0("input['", ns(paste0("ems_factor_continuous", i)), "'] == 1"),
              textInput(
                inputId = ns(paste0("ems_factor_actual", i)),
                label = paste0(
                  "Actual values for ", factor_names[i],
                  " (comma-separated; level order: ", level_hint, ")"
                ),
                value = "",
                placeholder = if (length(coded_levels) == 2L) "e.g. 8, 12" else "e.g. 8, 12, 16"
              )
            )
          )
        )
      })

      do.call(tagList, controls)
    })
    
    output$multi_response_target <- renderDT({
      if (!isTRUE(input$ems_show_optimum) || !isTRUE(multifactor_worker$has_fixed_factors())) {
        return(NULL)
      }
      
      model <- multifactor_worker$model_mean_est()
      target <- input$ems_target
      R <- input$ems_dec
      req(model, target, R)

      data <- filtered_data()
      req(data)

      factor_names <- multifactor_model_factor_names(model)
      factor_continuous <- stats::setNames(rep(FALSE, length(factor_names)), factor_names)
      for (i in seq_along(factor_names)) {
        key <- paste0("ems_factor_continuous", i)
        if (!is.null(input[[key]])) {
          factor_continuous[[factor_names[i]]] <- isTRUE(input[[key]])
        }
      }

      tol <- input$multi_response_tol
      if (!isTruthy(tol)) tol <- 0

      factor_actual_values <- list()
      cont_levels <- stats::setNames(
        lapply(factor_names, function(f) multifactor_factor_numeric_levels(data, f)),
        factor_names
      )
      for (i in seq_along(factor_names)) {
        f <- factor_names[i]
        if (!isTRUE(factor_continuous[[f]])) {
          next
        }
        key <- paste0("ems_factor_actual", i)
        parsed <- multifactor_parse_actual_values(
          if (!is.null(input[[key]])) input[[key]] else "",
          length(cont_levels[[f]])
        )
        if (!is.null(parsed)) {
          factor_actual_values[[f]] <- parsed
        }
      }

      closest <- multifactor_closest_to_target(
        model = model,
        data = data,
        target = target,
        tol = tol,
        factor_continuous = factor_continuous,
        decimals = R
      )
      closest <- multifactor_apply_actual_value_display(
        result = closest,
        factor_continuous = factor_continuous,
        cont_levels = cont_levels,
        factor_actual_values = factor_actual_values,
        decimals = R
      )
      closest <- multifactor_format_target_table_display(
        result = closest,
        factor_continuous = factor_continuous,
        decimals = R
      )
      DT::datatable(
        closest,
        options = list(
          dom = "t",
          paging = FALSE,
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        rownames = FALSE
      )
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

      # Derive interactions from the active model table (pooled when Set Up excludes effects).
      setup_pool <- mf_setup_pool_committed_vec()
      aov_l <- if (length(setup_pool) > 0L) {
        ems_anova_pooled_tab()
      } else {
        ems_anova_source_tab()
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
    shiny::outputOptions(output, "ems_int_sel", suspendWhenHidden = TRUE)

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
    shiny::outputOptions(output, "emsinteractions", suspendWhenHidden = TRUE)

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
      anova_data_trigger()
      conf <- input$ems_conf
      req(conf)

      setup_pool <- mf_setup_pool_committed_vec()
      aov_out_l <- if (length(setup_pool) > 0L) {
        ems_anova_pooled_tab()
      } else {
        ems_anova_source_tab()
      }

      req(aov_out_l)
      if (is.character(aov_out_l)) return(NULL)
      if (!is.data.frame(aov_out_l)) return(NULL)

      # Significant effects (exclude residual and intercept)
      sig <- aov_out_l[aov_out_l$Pvalue <= (1 - conf), , drop = FALSE]
      sig_effects <- row.names(sig)
      sig_effects <- sig_effects[!sig_effects %in% c("Residuals", "Residual", "(Intercept)", "Within Cells")]
      eff_tbl <- multifactor_worker$eff_types()
      ft <- multifactor_worker$factor_types()
      sig_effects <- multifactor_filter_fixed_anova_effects(
        sig_effects,
        eff_tbl = eff_tbl,
        random_factor_names = if (!is.null(ft)) ft$random else character(0)
      )
      if (length(sig_effects) == 0) return(NULL)

      effect_selected <- intersect(
        as.character(shiny::isolate(input$ems_ph_effects) %||% character(0)),
        as.character(sig_effects)
      )

      pickerInput(
        inputId = ns("ems_ph_effects"),
        label = "Select fixed effects for post-hoc",
        choices = sig_effects,
        options = list(title = "Select Effect(s)"),
        selected = effect_selected,
        multiple = TRUE
      )
    })
    shiny::outputOptions(output, "ems_ph_selection", suspendWhenHidden = TRUE)
    
    output$ems_ph_out <- renderUI({
      multifactor_worker$posthoc_out_dt()
    })

    # -------------------------------------------------------------------------
    # Loss / optimization (Taguchi MVP — multifactor)
    # -------------------------------------------------------------------------
    loss_multifactor_server(
      input = input,
      output = output,
      session = session,
      ns = ns,
      filtered_data = filtered_data,
      multifactor_worker = multifactor_worker,
      active_data_ems = active_data_ems,
      mf_resolved_data_ems = mf_resolved_data_ems,
      mf_resolved_data_ems_r = mf_resolved_data_ems_r,
      mf_resolved_factors_ems = mf_resolved_factors_ems,
      mf_resolved_active_did = mf_resolved_active_did,
      mf_model_registry = mf_model_registry,
      mf_model_finalized = mf_model_finalized,
      mf_optimization_session = mf_optimization_session,
      mf_results_state = mf_results_state,
      loss_mf_opt_result_val = loss_mf_opt_result_val,
      loss_mf_results_val = loss_mf_results_val,
      mf_loss_tier3_snapshot = mf_loss_tier3_snapshot,
      loss_mf_calculating = loss_mf_calculating,
      loss_mf_opt_running = loss_mf_opt_running,
      get_or_build_mf_fit = get_or_build_mf_fit,
      bump_mf_optimization_model_rev = bump_mf_optimization_model_rev,
      mf_optimization_model_rev = mf_optimization_model_rev,
      mf_applied_pool_rev = mf_applied_pool_rev,
      mf_pooling_pending_differs = mf_pooling_pending_differs,
      current_effect_choices = current_effect_choices,
      applied_effect_pool_for_did_mode = applied_effect_pool_for_did_mode,
      get_saved_or_current_response_state = get_saved_or_current_response_state,
      mf_model_readiness = mf_model_readiness,
      mf_optimization_readiness_full = mf_optimization_readiness_full,
      mf_resolved_f_r_types = mf_resolved_f_r_types,
      mf_resolved_ems_show_mixed_nest = mf_resolved_ems_show_mixed_nest,
      mf_loss_tab_rev = mf_loss_tab_rev,
      mf_loss_setup_rev = mf_loss_setup_rev,
      mf_design_setup = mf_design_setup,
      mf_loss_setup_cache = mf_loss_setup_cache,
      loss_mf_saved_factor_costs = loss_mf_saved_factor_costs
    )
    # -------------------------------------------------------------------------
    # Loss / optimization (Taguchi MVP — oneway)
    # -------------------------------------------------------------------------
    ow_loss_dispersion_type_id <- function() {
      if (isTRUE(input$ow_disp_analysis)) {
        dt <- suppressWarnings(as.integer(input$ow_disp_type)[1])
        if (is.na(dt) || dt < 1L || dt > 3L) 1L else dt
      } else {
        1L
      }
    }

    ow_loss_dispersion_type_label <- function() {
      lbl <- c("ADA", "ADM", "ADM(n-1)")
      lbl[ow_loss_dispersion_type_id()]
    }

    ow_loss_fixed_effects_only <- function() {
      type_ow <- suppressWarnings(as.integer(input$type_ow))
      is.finite(type_ow) && type_ow %in% c(1L, 4L)
    }

    observeEvent(input$type_ow, {
      if (!ow_loss_fixed_effects_only()) {
        loss_ow_opt_result_val(NULL)
      }
    }, ignoreInit = TRUE)

    output$loss_ow_disp_metric_note <- renderText({
      lbl <- ow_loss_dispersion_type_label()
      if (isTRUE(input$ow_disp_analysis)) {
        paste0(" ", lbl, " (from Oneway ANOVA \u2192 Dispersion test analysis)")
      } else {
        paste0(" ", lbl, " (enable Dispersion test analysis on the ANOVA tab to change)")
      }
    })

    loss_ow_result <- reactive({
      if (!ow_loss_fixed_effects_only()) {
        return(list(
          ok = FALSE,
          message = "Loss / optimization is available only for fixed-effects Oneway ANOVA (Fisher or Welch).",
          table = NULL,
          disclaimer = ""
        ))
      }
      d <- filtered_data()
      shiny::req(d)
      fc <- as.numeric(input$ow_factor)
      dc <- as.numeric(input$ow_data)
      shiny::req(fc, dc)
      type_ow <- if (!is.null(input$type_ow)) as.numeric(input$type_ow) else 1
      disp_ow <- isTRUE(input$disp_ow)
      ow_loss_disp_type <- ow_loss_dispersion_type_id()
      ow_conf <- if (!is.null(input$conf_ow)) input$conf_ow else 0.95
      resp_name <- if (dc >= 1L && dc <= ncol(d)) names(d)[dc] else as.character(dc)
      delta_user <- mf_get_resolution_delta_user(input, resp_name, input_prefix = "loss_ow_disp_delta__")
      oneway_taguchi_loss_mvp(
        dat = d,
        factor_ow = fc,
        data_col = dc,
        type_ow = type_ow,
        disp_ow = disp_ow,
        ow_loss_disp_type = ow_loss_disp_type,
        target = suppressWarnings(as.numeric(input$loss_ow_target)),
        C_l = suppressWarnings(as.numeric(input$loss_ow_C_l)),
        C_u = suppressWarnings(as.numeric(input$loss_ow_C_u)),
        lsl = suppressWarnings(as.numeric(input$loss_ow_lsl)),
        usl = suppressWarnings(as.numeric(input$loss_ow_usl)),
        ow_conf = ow_conf,
        resolution_delta_user = delta_user
      )
    })

    output$loss_ow_dispersion_policy <- renderUI({
      r <- tryCatch(loss_ow_result(), error = function(e) {
        list(ok = FALSE, message = conditionMessage(e), table = NULL)
      })
      d <- filtered_data()
      dc <- suppressWarnings(as.numeric(input$ow_data))
      rn <- "Response"
      if (!is.null(d) && length(dc) == 1L && is.finite(dc)) {
        dc_i <- as.integer(dc)
        if (dc_i >= 1L && dc_i <= ncol(d)) rn <- names(d)[dc_i]
      }
      summary <- if (isTRUE(r$ok)) summarize_dispersion_policy(r$table) else list(ok = FALSE, message = r$message, uses_tier3 = FALSE)
      line <- format_response_dispersion_policy_line(rn, summary, ow_loss_dispersion_type_label())
      build_dispersion_policy_messages_ui(line)
    })

    output$loss_ow_resolution_prior_panel <- renderUI({
      r <- tryCatch(loss_ow_result(), error = function(e) {
        list(ok = FALSE, message = conditionMessage(e), table = NULL, resolution_ctx = NULL)
      })
      summary <- if (isTRUE(r$ok)) summarize_dispersion_policy(r$table) else list(uses_tier3 = FALSE)
      if (!isTRUE(summary$uses_tier3)) {
        return(NULL)
      }
      d <- filtered_data()
      shiny::req(d)
      fc <- as.numeric(input$ow_factor)
      dc <- as.numeric(input$ow_data)
      shiny::req(fc, dc)
      dat <- d[, c(dc, fc), drop = FALSE]
      dat <- as.data.frame(dat, stringsAsFactors = FALSE)
      names(dat) <- make.names(names(dat))
      yname <- names(dat)[1L]
      ow_conf <- if (!is.null(input$conf_ow)) input$conf_ow else 0.95
      ctx <- if (!is.null(r$resolution_ctx)) {
        r$resolution_ctx
      } else {
        list()
      }
      build_tier3_resolution_controls_ui(ns, yname, ctx, ow_conf, input_prefix = "loss_ow_disp_delta__")
    })

    observeEvent(input$loss_ow_opt_run, {
      if (!ow_loss_fixed_effects_only()) {
        loss_ow_opt_result_val(list(
          ok = FALSE,
          message = "Loss / optimization is available only for fixed-effects Oneway ANOVA (Fisher or Welch)."
        ))
        return(invisible(NULL))
      }
      d <- filtered_data()
      shiny::req(d)
      fc <- as.numeric(input$ow_factor)
      dc <- as.numeric(input$ow_data)
      shiny::req(fc, dc)
      dat <- d[, c(dc, fc), drop = FALSE]
      dat <- as.data.frame(dat, stringsAsFactors = FALSE)
      names(dat) <- make.names(names(dat))
      yname <- names(dat)[1L]
      xname <- names(dat)[2L]
      dat[[yname]] <- suppressWarnings(as.numeric(dat[[yname]]))
      dat[[xname]] <- suppressWarnings(as.numeric(dat[[xname]]))
      if (all(is.na(dat[[xname]]))) {
        loss_ow_opt_result_val(list(ok = FALSE, message = "Optimizer requires a numeric coded factor column."))
        return(invisible(NULL))
      }
      dat <- dat[stats::complete.cases(dat[, c(yname, xname), drop = FALSE]), , drop = FALSE]
      if (nrow(dat) < 5L) {
        loss_ow_opt_result_val(list(ok = FALSE, message = "Not enough complete numeric rows for optimization."))
        return(invisible(NULL))
      }

      cont_vars <- if (!is.null(input$loss_ow_opt_continuous)) unique(as.character(input$loss_ow_opt_continuous)) else character(0)
      cont_vars <- cont_vars[nzchar(cont_vars)]
      cont_vars <- intersect(cont_vars, xname)
      treat_continuous <- length(cont_vars) > 0L

      dat_fit <- dat
      dat_fit[[xname]] <- factor(as.character(dat_fit[[xname]]))

      form_c <- stats::as.formula(paste(yname, "~", xname))
      disp_type <- ow_loss_dispersion_type_label()
      if (disp_type == "ADM(n-1)") disp_type <- "ADMn1"
      if (disp_type == "ADA") {
        dat_fit$.taguchi_disp <- lolcat::compute.group.dispersion.ADA(fx = form_c, data = dat_fit)
      } else if (disp_type == "ADM") {
        dat_fit$.taguchi_disp <- lolcat::compute.group.dispersion.ADM(fx = form_c, data = dat_fit)
      } else {
        dat_fit$.taguchi_disp <- lolcat::compute.group.dispersion.ADMn1(fx = form_c, data = dat_fit)
      }
      form_mean <- stats::as.formula(paste(yname, "~", xname))
      mean_mod <- tryCatch(stats::lm(form_mean, data = dat_fit), error = function(e) NULL)
      disp_mod <- tryCatch(
        stats::lm(stats::as.formula(paste(".taguchi_disp ~", xname)), data = dat_fit, na.action = na.omit),
        error = function(e) NULL
      )
      if (is.null(mean_mod) || is.null(disp_mod)) {
        loss_ow_opt_result_val(list(ok = FALSE, message = "Could not fit mean/dispersion models for optimization."))
        return(invisible(NULL))
      }

      xv <- suppressWarnings(as.numeric(as.character(dat[[xname]])))
      bounds <- stats::setNames(
        list(list(lower = min(xv, na.rm = TRUE), upper = max(xv, na.rm = TRUE))),
        xname
      )

      factor_kinds <- stats::setNames(if (treat_continuous) "continuous" else "discrete", xname)
      ow_conf <- if (!is.null(input$conf_ow)) input$conf_ow else 0.95
      delta_user <- mf_get_resolution_delta_user(input, yname, input_prefix = "loss_ow_disp_delta__")
      resolution_ctx <- build_resolution_context(
        dat = dat_fit,
        response_name = yname,
        cell_factors = xname,
        disp_type = disp_type,
        confidence = ow_conf,
        delta_user = delta_user
      )
      factor_interp <- if (treat_continuous) {
        build_factor_level_interp(mean_mod, disp_mod, dat_fit, cont_vars, resolution_ctx = resolution_ctx)
      } else {
        NULL
      }

      optimize_target <- if (!is.null(input$loss_ow_opt_target)) as.character(input$loss_ow_opt_target)[1] else "taguchi_loss"
      if (!optimize_target %in% c("taguchi_loss", "total_cost")) optimize_target <- "taguchi_loss"
      volume <- suppressWarnings(as.numeric(input$loss_ow_opt_volume))
      if (!is.finite(volume) || volume <= 0) {
        loss_ow_opt_result_val(list(ok = FALSE, message = "Production volume must be a positive number."))
        return(invisible(NULL))
      }
      factor_cost_tbl <- if (isTRUE(input$loss_ow_opt_use_factor_costs)) {
        parse_factor_level_costs(isolate(loss_ow_saved_factor_costs()), valid_factors = c(xname))
      } else {
        data.frame(factor = character(0), level = numeric(0), cost = numeric(0), stringsAsFactors = FALSE)
      }
      unit_setting_cost_fn <- make_unit_setting_cost_fn(factor_cost_tbl, factor_kinds = factor_kinds)

      res <- tryCatch(
        optimize_loss_continuous(
          mean_model = mean_mod,
          disp_model = disp_mod,
          bounds = bounds,
          target = suppressWarnings(as.numeric(input$loss_ow_target)),
          lsl = suppressWarnings(as.numeric(input$loss_ow_lsl)),
          usl = suppressWarnings(as.numeric(input$loss_ow_usl)),
          C_l = suppressWarnings(as.numeric(input$loss_ow_C_l)),
          C_u = suppressWarnings(as.numeric(input$loss_ow_C_u)),
          disp_type = disp_type,
          factor_kinds = factor_kinds,
          optimize_target = optimize_target,
          volume = volume,
          unit_setting_cost_fn = unit_setting_cost_fn,
          factor_interp = factor_interp,
          resolution_ctx = resolution_ctx
        ),
        error = function(e) list(ok = FALSE, message = conditionMessage(e))
      )
      res$bounds_used <- data.frame(
        Variable = names(bounds),
        Lower = vapply(bounds, function(b) as.numeric(b$lower), numeric(1)),
        Upper = vapply(bounds, function(b) as.numeric(b$upper), numeric(1))
      )
      res$plot_target <- suppressWarnings(as.numeric(input$loss_ow_target))
      res$plot_lsl <- suppressWarnings(as.numeric(input$loss_ow_lsl))
      res$plot_usl <- suppressWarnings(as.numeric(input$loss_ow_usl))
      loss_ow_opt_result_val(res)
    }, ignoreInit = TRUE)

    observeEvent(
      list(
        input$loss_ow_target,
        input$loss_ow_dec,
        input$loss_ow_C_l,
        input$loss_ow_C_u,
        input$loss_ow_lsl,
        input$loss_ow_usl,
        input$loss_ow_opt_target,
        input$loss_ow_opt_volume,
        input$loss_ow_opt_use_factor_costs,
        input$loss_ow_opt_factor_costs,
        loss_ow_saved_factor_costs(),
        input$loss_ow_opt_continuous,
        input$ow_disp_analysis,
        input$ow_disp_type,
        input$type_ow
      ),
      {
        loss_ow_opt_result_val(NULL)
      },
      ignoreInit = TRUE
    )

    output$loss_ow_msg <- renderUI({
      r <- tryCatch(loss_ow_result(), error = function(e) {
        list(ok = FALSE, message = conditionMessage(e), disclaimer = "")
      })
      if (!isTRUE(r$ok)) {
        return(shiny::tags$p(class = "text-danger", r$message))
      }
      shiny::tagList(
        if (nzchar(as.character(r$message))) shiny::tags$p(class = "text-warning", r$message),
        taguchi_loss_disclaimer_ui("oneway")
      )
    })

    output$loss_ow_opt_msg <- renderUI({
      r <- loss_ow_opt_result_val()
      req(r)
      if (!isTRUE(r$ok)) {
        return(shiny::tags$p(class = "text-danger", ifelse(is.null(r$message), "Optimization failed.", r$message)))
      }
      shiny::tags$p(
        class = "text-muted",
        paste0(
          "Optimizer converged (code ", r$convergence, "). Objective value: ", signif(r$value, 6), ".",
          if (!is.null(r$optimize_target) && identical(r$optimize_target, "total_cost")) {
            paste0(" Optimized on total cost with volume=", signif(as.numeric(r$volume), 6), ".")
          } else {
            ""
          }
        )
      )
    })

    output$loss_ow_opt_tab <- renderDT({
      if (!isTRUE(input$loss_ow_opt_show_details)) {
        return(DT::datatable(data.frame(), options = list(dom = "t", paging = FALSE), rownames = FALSE))
      }
      r <- loss_ow_opt_result_val()
      req(r)
      if (!isTRUE(r$ok)) {
        return(DT::datatable(data.frame(Note = ifelse(is.null(r$message), "Optimization failed.", r$message)), options = optimizer_dt_options(), rownames = FALSE))
      }
      R <- if (!is.null(input$loss_ow_dec)) input$loss_ow_dec else 4
      mets <- c(names(r$par), "Predicted mean", "Predicted sigma")
      vals <- c(as.numeric(r$par), r$mu, r$sigma)
      disp_audit <- optimizer_dispersion_audit_rows(r)
      mets <- c(mets, disp_audit$metrics)
      vals <- c(vals, disp_audit$values)
      if (!is.null(r$optimize_target)) {
        mets <- c(mets, "Optimization target", "Production volume", "Unit settings cost", "Total settings cost")
        vals <- c(vals, as.character(r$optimize_target), as.numeric(r$volume), as.numeric(r$unit_setting_cost), as.numeric(r$total_setting_cost))
      }
      if (!is.null(r$objective_breakdown) && is.list(r$objective_breakdown)) {
        ob <- r$objective_breakdown
        mets <- c(
          mets,
          "Objective breakdown: weighted base loss sum",
          "Objective breakdown: weighted loss sum",
          "Objective breakdown: weighted PPM sum",
          "Objective breakdown: objective loss",
          "Objective breakdown: unit settings cost",
          "Objective breakdown: total settings cost",
          "Objective breakdown: final objective"
        )
        vals <- c(
          vals,
          as.numeric(ob$weighted_base_loss_sum),
          as.numeric(ob$weighted_loss_sum),
          as.numeric(ob$weighted_ppm_sum),
          as.numeric(ob$objective_loss),
          as.numeric(ob$unit_setting_cost),
          as.numeric(ob$total_setting_cost),
          as.numeric(ob$final_objective)
        )
      }
      if (!is.null(r$par_snapped) && nrow(r$par_snapped) == 1L) {
        snap_parts <- vapply(names(r$par_snapped), function(nm) {
          paste0(nm, "=", as.character(r$par_snapped[[nm]][1L]))
        }, character(1))
        mets <- c(mets, "Factor levels used for prediction (nearest design levels)")
        vals <- c(vals, paste(snap_parts, collapse = "; "))
      }
      mets <- c(mets, "Loss lower", "Loss upper", "Total expected Taguchi losses", "PPM lower", "PPM upper", "PPM total")
      vals <- c(
        vals,
        r$metrics$loss_lower[1],
        r$metrics$loss_upper[1],
        r$metrics$expected_loss[1],
        r$metrics$ppm_lower[1],
        r$metrics$ppm_upper[1],
        r$metrics$ppm[1]
      )
      cap_rows <- optimizer_capability_result_rows(r$capability)
      mets <- c(mets, cap_rows$metrics)
      vals <- c(vals, cap_rows$values)
      tbl <- data.frame(Metric = mets, Value = vals)
      num_idx <- !(grepl("Factor levels", tbl$Metric, fixed = TRUE) |
        grepl("^Optimization target$", tbl$Metric))
      tbl$Value[num_idx] <- round(as.numeric(tbl$Value[num_idx]), digits = as.integer(R))
      DT::datatable(tbl, options = optimizer_dt_options(), rownames = FALSE)
    })

    output$loss_ow_opt_dist_plot <- renderPlot({
      r <- loss_ow_opt_result_val()
      req(r, isTRUE(r$ok))
      d <- filtered_data()
      dc <- suppressWarnings(as.numeric(input$ow_data))
      resp_name <- if (!is.null(d) && is.data.frame(d) && length(dc) == 1L && is.finite(dc) && dc >= 1L && dc <= ncol(d)) {
        as.character(names(d)[dc])
      } else {
        "response"
      }
      plot_target <- if (!is.null(r$plot_target)) r$plot_target else suppressWarnings(as.numeric(input$loss_ow_target))
      plot_lsl <- if (!is.null(r$plot_lsl)) r$plot_lsl else suppressWarnings(as.numeric(input$loss_ow_lsl))
      plot_usl <- if (!is.null(r$plot_usl)) r$plot_usl else suppressWarnings(as.numeric(input$loss_ow_usl))
      build_optimizer_normal_plot(
        mu = r$mu,
        sigma = r$sigma,
        target = plot_target,
        lsl = plot_lsl,
        usl = plot_usl,
        title = paste0("Estimated Distribution at Optimum for ", resp_name)
      )
    }, height = 260)

    output$loss_ow_opt_dist_cards <- renderUI({
      r <- loss_ow_opt_result_val()
      req(r)
      if (!isTRUE(r$ok)) return(NULL)
      R <- if (!is.null(input$loss_ow_dec)) input$loss_ow_dec else 4

      shiny::tagList(
        shiny::tags$hr(),
        build_optimizer_settings_table_ui(r$par, digits = R),
        shiny::plotOutput(outputId = ns("loss_ow_opt_dist_plot"), height = 260),
        shiny::tags$p(
          class = "text-muted",
          style = "margin-top: 0.25em;",
          shiny::tags$span(style = "color:#2ca25f;font-weight:600;", "Dashed green"),
          " = Target | ",
          shiny::tags$span(style = "color:#de2d26;font-weight:600;", "Dotted red"),
          " = LSL/USL"
        ),
        build_optimizer_summary_table_ui(
          mu = r$mu,
          sigma = r$sigma,
          expected_loss = r$metrics$expected_loss[1],
          ppm = r$metrics$ppm[1],
          volume = if (!is.null(r$volume)) r$volume else 1,
          unit_setting_cost = if (!is.null(r$unit_setting_cost)) r$unit_setting_cost else 0,
          target = if (!is.null(r$plot_target)) r$plot_target else suppressWarnings(as.numeric(input$loss_ow_target)),
          lsl = if (!is.null(r$plot_lsl)) r$plot_lsl else suppressWarnings(as.numeric(input$loss_ow_lsl)),
          usl = if (!is.null(r$plot_usl)) r$plot_usl else suppressWarnings(as.numeric(input$loss_ow_usl)),
          digits = R
        )
      )
    })

    output$loss_ow_opt_bounds_tab <- renderDT({
      if (!isTRUE(input$loss_ow_opt_show_details)) {
        return(DT::datatable(data.frame(), options = list(dom = "t", paging = FALSE), rownames = FALSE))
      }
      r <- loss_ow_opt_result_val()
      req(r)
      if (is.null(r$bounds_used)) return(NULL)
      DT::datatable(r$bounds_used, options = optimizer_dt_options(), rownames = FALSE)
    })

    output$loss_ow_opt_export <- downloadHandler(
      filename = function() paste0("loss_ow_optimizer_", Sys.Date(), ".csv"),
      content = function(file) {
        r <- loss_ow_opt_result_val()
        if (is.null(r) || !isTRUE(r$ok)) {
          utils::write.csv(data.frame(Note = "No successful optimizer result to export."), file, row.names = FALSE)
          return()
        }
      mets <- c(names(r$par), "Predicted mean", "Predicted sigma")
      vals <- c(as.numeric(r$par), r$mu, r$sigma)
      disp_audit <- optimizer_dispersion_audit_rows(r)
      mets <- c(mets, disp_audit$metrics)
      vals <- c(vals, disp_audit$values)
        if (!is.null(r$optimize_target)) {
          mets <- c(mets, "Optimization target", "Production volume", "Unit settings cost", "Total settings cost")
          vals <- c(vals, as.character(r$optimize_target), as.numeric(r$volume), as.numeric(r$unit_setting_cost), as.numeric(r$total_setting_cost))
        }
        if (!is.null(r$objective_breakdown) && is.list(r$objective_breakdown)) {
          ob <- r$objective_breakdown
          mets <- c(
            mets,
            "Objective breakdown: weighted base loss sum",
            "Objective breakdown: weighted loss sum",
            "Objective breakdown: weighted PPM sum",
            "Objective breakdown: objective loss",
            "Objective breakdown: unit settings cost",
            "Objective breakdown: total settings cost",
            "Objective breakdown: final objective"
          )
          vals <- c(
            vals,
            as.numeric(ob$weighted_base_loss_sum),
            as.numeric(ob$weighted_loss_sum),
            as.numeric(ob$weighted_ppm_sum),
            as.numeric(ob$objective_loss),
            as.numeric(ob$unit_setting_cost),
            as.numeric(ob$total_setting_cost),
            as.numeric(ob$final_objective)
          )
        }
        if (!is.null(r$par_snapped) && nrow(r$par_snapped) == 1L) {
          snap_parts <- vapply(names(r$par_snapped), function(nm) {
            paste0(nm, "=", as.character(r$par_snapped[[nm]][1L]))
          }, character(1))
          mets <- c(mets, "Factor levels used for prediction (nearest design levels)")
          vals <- c(vals, paste(snap_parts, collapse = "; "))
        }
        mets <- c(mets, "Loss lower", "Loss upper", "Total expected Taguchi losses", "PPM lower", "PPM upper", "PPM total")
        vals <- c(
          vals,
          r$metrics$loss_lower[1],
          r$metrics$loss_upper[1],
          r$metrics$expected_loss[1],
          r$metrics$ppm_lower[1],
          r$metrics$ppm_upper[1],
          r$metrics$ppm[1]
        )
        cap_rows <- optimizer_capability_result_rows(r$capability)
        mets <- c(mets, cap_rows$metrics)
        vals <- c(vals, cap_rows$values)
        tbl <- data.frame(Metric = mets, Value = vals)
        utils::write.csv(tbl, file, row.names = FALSE)
      }
    )

    output$loss_ow_tab <- renderDT({
      r <- tryCatch(loss_ow_result(), error = function(e) {
        list(ok = FALSE, message = conditionMessage(e), table = NULL)
      })
      R <- if (!is.null(input$loss_ow_dec)) input$loss_ow_dec else 4
      if (!isTRUE(r$ok)) {
        return(DT::datatable(
          data.frame(Note = r$message),
          options = optimizer_dt_options(),
          rownames = FALSE
        ))
      }
      tbl <- r$table
      if ("se_mu" %in% names(tbl)) {
        tbl$se_mu <- NULL
      }
      if ("disp_emm" %in% names(tbl) && "disp_pred" %in% names(tbl)) {
        tbl$disp_pred <- NULL
      }
      if ("expected_loss" %in% names(tbl)) {
        tbl$expected_loss <- suppressWarnings(as.numeric(tbl$expected_loss))
        tbl <- tbl[order(tbl$expected_loss, na.last = TRUE), , drop = FALSE]
      }
      if (nrow(tbl) > 5L) {
        tbl <- utils::head(tbl, 5L)
      }
      pretty_names <- c(
        method = "Method",
        mu_pred = "Predicted mean",
        disp_pred = "Dispersion EMM",
        disp_emm = "Dispersion EMM",
        disp_cell = "Cell dispersion",
        disp_effective = "Effective dispersion",
        disp_tier = "Dispersion tier",
        delta_used = "Resolution delta used",
        sigma = "Predicted sigma",
        loss_lower = "Lower-side Taguchi loss",
        loss_upper = "Upper-side Taguchi loss",
        expected_loss = "Total Taguchi loss per unit",
        ppm_lower = "PPM below LSL",
        ppm_upper = "PPM above USL",
        ppm = "Total PPM outside specs"
      )
      for (nm in names(pretty_names)) {
        if (nm %in% names(tbl)) names(tbl)[names(tbl) == nm] <- pretty_names[[nm]]
      }
      num_cols <- names(tbl)[vapply(tbl, is.numeric, logical(1))]
      for (nm in num_cols) {
        tbl[[nm]] <- suppressWarnings(as.numeric(tbl[[nm]]))
        tbl[[nm]] <- round(tbl[[nm]], digits = as.integer(R))
      }
      DT::datatable(
        tbl,
        options = optimizer_dt_options(list(scrollX = TRUE, autoWidth = TRUE)),
        rownames = FALSE,
        class = "cell-border stripe compact"
      )
    })

    # Downloads are wired when plots are implemented in later todos.

  })
}
