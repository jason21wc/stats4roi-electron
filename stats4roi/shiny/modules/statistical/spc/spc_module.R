# SPC (Statistical Process Control) Module - Coordinator
# Implements the SPC tab from app_monolithic.R using the coordinator/worker architecture.
#
# Coordinator responsibilities:
# - UI composition (tab layout)
# - UI rendering (renderUI/renderPlot/renderDT)
# - Data flow coordination between workers
#
# Worker responsibilities (implemented in server/):
# - Business logic only (data prep, calculations), return reactives/data to coordinator

library(shiny)
library(DT)
library(ggplot2)
library(shinyWidgets)

# Source global systems
source("modules/config/global_config.R")
source("modules/config/global_data_invalidation.R")

# Source UI components
source("modules/statistical/spc/ui/spc_ui.R")
source("modules/statistical/spc/ui/spc_limits_ui.R")
source("modules/statistical/spc/ui/spc_capability_ui.R")

# Source worker modules (stubs initially; implemented in later todos)
# source("modules/statistical/spc/server/spc_variables_server.R")
# source("modules/statistical/spc/server/spc_attributes_server.R")
# source("modules/statistical/spc/server/spc_anova_server.R")

# Source SPC constants
source("modules/statistical/spc/utils/spc_constants.R")
source("modules/statistical/spc/utils/spc_limit_calcs.R")

# Source worker modules
source("modules/statistical/spc/server/spc_limits_server.R")
source("modules/statistical/spc/server/spc_capability_server.R")

# =============================================================================
# COORDINATOR UI FUNCTION
# =============================================================================
create_spc_ui <- function(id) {
  ns <- NS(id)
  create_spc_ui_internal(ns)
}

# =============================================================================
# COORDINATOR SERVER FUNCTION
# =============================================================================
create_spc_server <- function(id, filtered_data, reactive_color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # =========================================================================
    # REGISTER MODULE WITH GLOBAL DATA INVALIDATION SYSTEM
    # =========================================================================
    register_module(
      module_name = "spc",
      ui_reset_function = function() {
        # Reset key SPC inputs when data changes
        updatePickerInput(session, "spc_var_UI1", selected = character(0))
        updatePickerInput(session, "spc_var_UI2", selected = character(0))
        updatePickerInput(session, "spc_var_set", selected = 0)
        updatePickerInput(session, "spc_att_UI1", selected = character(0))
        updatePickerInput(session, "spc_att_UI2", selected = character(0))
        updatePickerInput(session, "spc_att_set", selected = 0)

        updateSelectInput(session, "spc_var_data_type", selected = 1)
        updateSelectInput(session, "spc_att_data_type", selected = 1)
        updateSelectInput(session, "spc_var_loc_type", selected = 1)
        updateSelectInput(session, "spc_att_loc_type", selected = 1)
        updateSelectInput(session, "spc_var_loc_lim", selected = 1)
        updateSelectInput(session, "spc_var_disp_lim", selected = 1)
        updateSelectInput(session, "spc_att_loc_lim", selected = 1)
        updateSelectInput(session, "spc_x_loc", selected = 1)
        updateSelectInput(session, "spc_x_disp", selected = 1)
        updateSelectInput(session, "spc_att_loc", selected = 1)

        updateCheckboxInput(session, "spc_runchart", value = FALSE)
        updateCheckboxInput(session, "att_const_n", value = FALSE)
        updateCheckboxInput(session, "x_spc_show_anova", value = FALSE)
        updateCheckboxInput(session, "x_spc_show_data", value = FALSE)
        updateCheckboxInput(session, "att_spc_show_analysis", value = FALSE)
        updateCheckboxInput(session, "att_spc_show_data", value = FALSE)

        # Reset limits calculations inputs
        updateSelectInput(session, "kappa_limits_k", selected = character(0))
        updateSelectInput(session, "kappa_limits_v", selected = character(0))
      },
      validation_function = function(data) {
        if (is.null(data) || nrow(data) == 0) {
          return(list(valid = FALSE, message = "No data available for SPC"))
        }
        list(valid = TRUE, message = "")
      }
    )

    # -------------------------------------------------------------------------
    # Variables chart: dynamic UI (ported from app_monolithic.R)
    # -------------------------------------------------------------------------
    output$spc_var_UI1 <- renderUI({
      data <- filtered_data()
      req(data)

      choices <- seq_len(ncol(data))
      names(choices) <- names(data)

      data_type <- input$spc_var_data_type
      ind_chart <- input$spc_var_ind_or_mean

      if (isTRUE(ind_chart)) {
        return(pickerInput(
          inputId = ns("spc_var_UI1"),
          label = "Select Data Column",
          multiple = FALSE,
          options = list(`actions-box` = TRUE),
          choices = choices
        ))
      }

      if (identical(data_type, 1L) || identical(as.numeric(data_type), 1)) {
        return(pickerInput(
          inputId = ns("spc_var_UI1"),
          label = "Select Data Columns",
          multiple = TRUE,
          options = list(`actions-box` = TRUE),
          choices = choices
        ))
      }

      if (identical(data_type, 2L) || identical(as.numeric(data_type), 2)) {
        return(pickerInput(
          inputId = ns("spc_var_UI1"),
          label = "Select Sample Column",
          multiple = FALSE,
          options = list(`actions-box` = TRUE),
          choices = choices
        ))
      }

      NULL
    })

    output$spc_var_UI2 <- renderUI({
      data <- filtered_data()
      req(data)

      choices <- seq_len(ncol(data))
      names(choices) <- names(data)

      data_type <- input$spc_var_data_type
      UI1 <- input$spc_var_UI1
      req(UI1)
      ind_chart <- input$spc_var_ind_or_mean

      if (isTRUE(ind_chart)) return(NULL)

      if (identical(as.numeric(data_type), 1)) return(NULL)

      if (identical(as.numeric(data_type), 2)) {
        fact_selected <- as.numeric(unlist(strsplit(x = UI1, split = "\\s+")))
        temp <- seq_len(length(choices))
        temp <- temp[-fact_selected]
        choices <- choices[temp]

        return(pickerInput(
          inputId = ns("spc_var_UI2"),
          label = "Select Data Column",
          multiple = FALSE,
          options = list(`actions-box` = TRUE),
          choices = choices
        ))
      }

      NULL
    })

    output$spc_var_set <- renderUI({
      data <- filtered_data()
      req(data)

      choices <- seq_len(ncol(data))
      names(choices) <- names(data)

      data_type <- input$spc_var_data_type
      UI1 <- as.numeric(input$spc_var_UI1)
      UI2 <- as.numeric(input$spc_var_UI2)
      req(UI1)

      ind_chart <- input$spc_var_ind_or_mean

      if (identical(as.numeric(data_type), 1) || isTRUE(ind_chart)) {
        fact_selected <- c(UI1)
        temp <- seq_len(length(choices))
        temp <- temp[-fact_selected]
        choices <- c("None" = 0, choices[temp])
        return(pickerInput(
          inputId = ns("spc_var_set"),
          label = "Select Set Column",
          multiple = FALSE,
          options = list(`actions-box` = TRUE),
          choices = choices,
          selected = 0
        ))
      }

      if (identical(as.numeric(data_type), 2)) {
        req(UI2)
        fact_selected <- c(UI1, UI2)
        temp <- seq_len(length(choices))
        temp <- temp[-fact_selected]
        choices <- c("None" = 0, choices[temp])
        return(pickerInput(
          inputId = ns("spc_var_set"),
          label = "Select Set Column",
          multiple = FALSE,
          options = list(`actions-box` = TRUE),
          choices = choices,
          selected = 0
        ))
      }

      NULL
    })

    output$spc_var_loc_type <- renderUI({
      ind_chart <- input$spc_var_ind_or_mean
      if (isTRUE(ind_chart)) {
        return(selectInput(inputId = ns("spc_var_loc_type"), label = "Location", choices = c("X" = 2)))
      }
      selectInput(inputId = ns("spc_var_loc_type"), label = "Location", choices = c("X-bar" = 1))
    })

    output$spc_var_disp_type <- renderUI({
      x_type <- input$spc_var_loc_type
      req(x_type)
      if (identical(as.numeric(x_type), 1)) {
        return(selectInput(inputId = ns("spc_var_disp_type"), label = "Dispersion", choices = choice_disp_spc[1:3]))
      }
      if (identical(as.numeric(x_type), 2)) {
        return(selectInput(inputId = ns("spc_var_disp_type"), label = "Dispersion", choices = choice_disp_spc[4]))
      }
      NULL
    })

    output$spc_var_loc_lim <- renderUI({
      x_type <- input$spc_var_loc_type
      req(x_type)
      ind_chart <- input$spc_var_ind_or_mean

      if (!isTRUE(ind_chart)) {
        return(selectInput(
          inputId = ns("spc_var_loc_lim"),
          label = "X-bar Limit Calculation",
          choices = choice_x_bar_limits2
        ))
      }

      choice <- c(1:9, 12)
      names(choice) <- c(
        "Average Range",
        "Median Range",
        "Average Standard Deviation",
        "Median Standard Deviation",
        "Average Variance",
        "Average Moving Range of X",
        "Median Moving Range of X",
        "Standard Deviation of X",
        "Known \u03c3",
        "Custom"
      )
      selectInput(
        inputId = ns("spc_var_loc_lim"),
        label = "X Limit Calculation",
        choices = choice[6:10]
      )
    })

    output$spc_var_disp_lim <- renderUI({
      x_type <- input$spc_var_loc_type
      disp_type <- as.numeric(input$spc_var_disp_type)
      req(x_type, disp_type)

      choice_disp_xbar <- c("R", "s", "s\u00b2")
      choice_disp_x <- c("Moving Range")
      choice_r_limits2_local <- choice_r_limits2

      if (disp_type == 2) {
        choice_r_limits2_local <- c(1:9, 12)
        names(choice_r_limits2_local) <- c(
          "Average Range",
          "Median Range",
          "Average Standard Deviation",
          "Median Standard Deviation",
          "Average Variance",
          "Average MR of s",
          "Median MR of s",
          "Standard Deviation of s",
          "Known \u03c3",
          "Custom"
        )
      }

      if (disp_type == 3) {
        choice_r_limits2_local <- c(1:9, 12)
        names(choice_r_limits2_local) <- c(
          "Average Range",
          "Median Range",
          "Average Standard Deviation",
          "Median Standard Deviation",
          "Average Variance",
          "Average MR of s\u00b2",
          "Median MR of s\u00b2",
          "Standard Deviation of s\u00b2",
          "Known \u03c3",
          "Custom"
        )
      }

      if (disp_type == 4) {
        choice_r_limits2_local <- c(1:9, 12)
        names(choice_r_limits2_local) <- c(
          "Average Range",
          "Median Range",
          "Average Standard Deviation",
          "Median Standard Deviation",
          "Average Variance",
          "Average MR",
          "Median MR",
          "Standard Deviation of X",
          "Known \u03c3",
          "Custom"
        )
      }

      if (identical(as.numeric(x_type), 1)) {
        return(selectInput(
          inputId = ns("spc_var_disp_lim"),
          label = paste(choice_disp_xbar[disp_type], " Limit Calculation"),
          choices = choice_r_limits2_local
        ))
      }

      if (identical(as.numeric(x_type), 2)) {
        return(selectInput(
          inputId = ns("spc_var_disp_lim"),
          label = paste(choice_disp_x, " Limit Calculations"),
          choices = choice_r_limits2_local[6:10]
        ))
      }

      NULL
    })

    output$spc_run_type <- renderUI({
      runchart <- input$spc_runchart
      if (!isTRUE(runchart)) return(NULL)

      if (!isTRUE(input$spc_var_ind_or_mean)) {
        return(tags$div(
          tags$p("Run chart of:"),
          materialSwitch(inputId = ns("spc_run_loc"), label = "Means", value = FALSE, inline = TRUE),
          tags$span("Medians")
        ))
      }

      tags$div(
        tags$p("Centerline:"),
        materialSwitch(inputId = ns("spc_run_loc"), label = "Mean", value = FALSE, inline = TRUE),
        tags$span("Median")
      )
    })

    # -------------------------------------------------------------------------
    # Variables chart: plot + plot data + hover
    # (Full SPC limits + OOC logic implemented in later todos; run chart is included.)
    # -------------------------------------------------------------------------
    plot_data_r <- reactiveVal(NULL)
    plot_data_change <- reactiveVal(0)

    x_chart_out <- reactive({
      data <- filtered_data()
      req(data)

      color <- reactive_color_palette()
      if (is.null(color) || length(color) < 4) color <- palette.colors(8)

      data_type <- input$spc_var_data_type
      UI1 <- as.numeric(input$spc_var_UI1)
      UI2 <- as.numeric(input$spc_var_UI2)
      req(UI1)

      runchart <- input$spc_runchart
      ind_chart <- input$spc_var_ind_or_mean
      run_loc <- input$spc_run_loc
      font_size <- as.numeric(input$spc_font_size)
      if (is.na(font_size)) font_size <- 11

      sets <- as.numeric(input$spc_var_set)
      if (is.null(sets) || length(sets) == 0 || all(is.na(sets))) sets <- 0
      if (!is.null(sets) && !is.na(sets) && sets > ncol(data)) sets <- 0

      if (any(is.na(UI1)) || any(UI1 < 1) || any(UI1 > ncol(data))) {
        plot_data_r(NULL)
        return(
          ggplot() +
            theme_void() +
            annotate(
              "text",
              x = 0,
              y = 0,
              label = "Selected column is no longer available.\nPlease reselect variables after loading new data."
            )
        )
      }

      # Data prep (ported from monolithic)
      if (!isTRUE(ind_chart)) {
        if (identical(as.numeric(data_type), 1)) {
          req(length(UI1) > 1)
          k_obs <- nrow(data)
          dep <- lolcat::transform.dependent.format.to.independent.format(data = data[UI1])
          y_lab <- names(data[UI1])
          if (sets == 0) {
            d <- cbind("Data" = dep, "Sample" = seq_len(k_obs))
            names(d) <- c("Name", "Data", "Sample")
          } else {
            d <- cbind("Data" = dep, "Sample" = seq_len(k_obs), "Sets" = sets)
            names(d) <- c("Name", "Data", "Sample", "Sets")
          }
        } else {
          req(UI2)
          if (any(is.na(UI2)) || any(UI2 < 1) || any(UI2 > ncol(data))) {
            plot_data_r(NULL)
            return(
              ggplot() +
                theme_void() +
                annotate(
                  "text",
                  x = 0,
                  y = 0,
                  label = "Selected column is no longer available.\nPlease reselect variables after loading new data."
                )
            )
          }
          k_obs <- nrow(unique(data[UI1]))
          y_lab <- names(data)[UI2]
          if (sets == 0) {
            d <- cbind("Data" = data[UI2], "Sample" = data[UI1])
            names(d) <- c("Data", "Sample")
          } else {
            d <- cbind("Data" = data[UI2], "Sample" = data[UI1], "Sets" = data[sets])
            names(d) <- c("Data", "Sample", "Sets")
          }
        }
      } else {
        k_obs <- nrow(data)
        y_lab <- names(data)[UI1]
        if (sets == 0) {
          d <- cbind("Data" = data[UI1], "Sample" = seq_len(k_obs))
          names(d) <- c("Data", "Sample")
        } else {
          d <- cbind("Data" = data[UI1], "Sample" = seq_len(k_obs), "Sets" = data[sets])
          names(d) <- c("Data", "Sample", "Sets")
        }
      }

      d <- d[order(d$Sample), ]

      # Ensure required columns are atomic numeric vectors (avoid mean/var on non-numeric)
      if (is.data.frame(d$Data)) d$Data <- d$Data[[1]]
      if (is.factor(d$Data)) d$Data <- as.character(d$Data)
      d$Data <- suppressWarnings(as.numeric(d$Data))
      if (all(is.na(d$Data))) {
        plot_data_r(NULL)
        return(
          ggplot() +
            theme_void() +
            annotate(
              "text",
              x = 0,
              y = 0,
              label = "Selected data column is not numeric.\nUse the 'Select, Rename, and Convert Variables' tab to convert it to numeric."
            )
        )
      }

      if (!is.null(d$Sets)) {
        if (is.data.frame(d$Sets)) d$Sets <- d$Sets[[1]]
        if (is.factor(d$Sets)) d$Sets <- as.character(d$Sets)
        d$Sets <- suppressWarnings(as.numeric(d$Sets))
      }

      # Run chart branch (exact monolithic behavior)
      if (isTRUE(runchart)) {
        names(d)[names(d) == "Sample"] <- "x_label"
        names(d)[names(d) == "Data"] <- "measure"

        if (sets == 0) {
          d <- cbind(d, "Sets" = 1)
          if (isTRUE(run_loc)) {
            d <- cbind(d, "loc" = median(d$measure))
            p <- ggplot(data = d, aes(x = x_label, y = measure)) +
              stat_summary(geom = "point", fun = "median", color = color[4]) +
              stat_summary(geom = "line", fun = "median", color = color[4]) +
              geom_hline(aes(yintercept = median(measure)), color = color[3]) +
              theme_gray(base_size = font_size)
            if (isTRUE(ind_chart)) {
              p <- p + ggtitle("Run Chart, Median Centerline") + labs(x = "Samples", y = names(data)[UI1])
            } else {
              p <- p + ggtitle("Run Chart of Medians") + labs(x = "Samples", y = "Data")
            }
          } else {
            d <- cbind(d, "loc" = mean(d$measure))
            p <- ggplot(data = d, aes(x = x_label, y = measure)) +
              stat_summary(geom = "point", fun = "mean", color = color[4]) +
              stat_summary(geom = "line", fun = "mean", color = color[4]) +
              geom_hline(aes(yintercept = mean(measure)), color = color[3]) +
              theme_gray(base_size = font_size)
            if (isTRUE(ind_chart)) {
              p <- p + ggtitle("Run Chart, Mean Centerline") + labs(x = "Samples", y = names(data)[UI1])
            } else {
              p <- p + ggtitle("Run Chart of Means") + labs(x = "Samples", y = "Data")
            }
          }
        } else {
          # Set-based run charts (ported from monolithic; relies on $ partial matching Set/Sets)
          diff_run <- c(0, diff(d$Set))
          diff_set <- 1
          for (i in seq_along(diff_run)) {
            if (diff_run[i] == 0) {
              diff_run[i] <- diff_set
            } else {
              diff_set <- diff_set + 1
              diff_run[i] <- diff_set
            }
          }
          d <- cbind(d, diff_run)

          if (isTRUE(run_loc)) {
            group_medians <- aggregate(d$measure, list(d$Set), FUN = median)
            names(group_medians) <- c("Sets", "loc")
            d <- merge(d, group_medians, sort = FALSE)
            p <- ggplot(data = d, aes(x = x_label, y = measure, group = diff_run)) +
              stat_summary(geom = "point", fun = "median", color = color[4]) +
              stat_summary(geom = "line", fun = "median", color = color[4]) +
              geom_line(aes(y = loc), color = color[3]) +
              theme_gray(base_size = font_size)
            if (isTRUE(ind_chart)) {
              p <- p +
                ggtitle("Run Chart, Median Centerline") +
                labs(x = "Samples", y = names(data)[UI1])
            } else {
              p <- p +
                ggtitle("Run Chart of Medians") +
                labs(x = "Samples", y = "Data")
            }
          } else {
            group_means <- aggregate(d$measure, list(d$Set), FUN = mean)
            names(group_means) <- c("Sets", "loc")
            d <- merge(d, group_means, sort = FALSE)
            p <- ggplot(data = d, aes(x = x_label, y = measure, group = diff_run)) +
              stat_summary(geom = "point", fun = "mean", color = color[4]) +
              stat_summary(geom = "line", fun = "mean", color = color[4]) +
              geom_line(aes(y = loc), color = color[3]) +
              theme_gray(base_size = font_size)
            if (isTRUE(ind_chart)) {
              p <- p +
                ggtitle("Run Chart, Mean Centerline") +
                labs(x = "Samples", y = names(data)[UI1])
            } else {
              p <- p +
                ggtitle("Run Chart of Means") +
                labs(x = "Samples", y = "Data")
            }
          }
        }

        # Hover/plot data payload (run chart)
        meas_fun <- if (isTRUE(run_loc)) median else mean
        hoverdata <- data.frame(
          x_label = sort(unique(d$x_label)),
          n_sample = aggregate(d$measure, list(d$x_label), FUN = length)[2],
          set = aggregate(d$Sets, list(d$x_label), FUN = unique)[2],
          measure = aggregate(d$measure, list(d$x_label), FUN = meas_fun)[2],
          centerline = aggregate(d$loc, list(d$x_label), FUN = meas_fun)[2]
        )
        colnames(hoverdata) <- c("x_label", "n_sample", "set", "measure", "centerline")

        plot_data_r(data.frame(
          x_label = hoverdata$x_label,
          facet = "Run Chart",
          n_sample = hoverdata$n_sample,
          set = hoverdata$set,
          UCL = NA,
          LCL = NA,
          measure = hoverdata$measure,
          est_sig = NA,
          ind_s = NA,
          set_sd = NA,
          centerline = hoverdata$centerline,
          outside = NA,
          runs = NA,
          zone_bc_up = NA,
          zone_bc_low = NA,
          zone_ab_up = NA,
          zone_ab_low = NA,
          zone_a_up = NA,
          zone_a_low = NA,
          trends = NA,
          alternating = NA,
          zone_a = NA,
          consec_c = NA,
          consec_ab = NA,
          zone_a_b = NA
        ))

        return(p)
      }

      # End of run chart branch; compute full SPC control charts (ported from monolithic)
      x_type <- input$spc_var_loc_type
      disp_type <- input$spc_var_disp_type
      span <- input$spc_mr_span
      x_lim_calc <- input$spc_var_loc_lim
      disp_lim_calc <- input$spc_var_disp_lim
      loc_upper_custom <- input$custom.x.upper
      loc_center_custom <- input$custom.x.center
      loc_lower_custom <- input$custom.x.lower
      disp_upper_custom <- input$custom.disp.upper
      disp_center_custom <- input$custom.disp.center
      disp_lower_custom <- input$custom.disp.lower
      std_err <- as.numeric(input$std_err_x)
      known_sig_x <- input$known_sig_x
      loc_center_type <- as.numeric(input$spc_x_loc)
      disp_center_type <- as.numeric(input$spc_x_disp)
      USL <- input$spc_x_USL
      Target <- input$spc_x_target
      LSL <- input$spc_x_LSL

      req(x_type, disp_type, x_lim_calc, disp_lim_calc)

      # Defaults (monolithic selects first choice if present)
      if (is.na(loc_center_type)) loc_center_type <- 1
      if (is.na(disp_center_type)) disp_center_type <- 1
      if (is.na(std_err)) std_err <- 3
      if (is.null(span) || is.na(span)) span <- 2

      run_length <- as.numeric(input$run_length_x)
      if (is.na(run_length)) run_length <- 8
      ooc_rules <- spc.rulesets.nelson.1984.test.1.2.3.4.5.6.7.8()
      ooc_rules$runs <- spc.controlviolation.nelson.1984.test2.runs.create(point.count = run_length)

      st_dev_ind <- sd(d$Data)

      # For sets (match monolithic behavior)
      if (sets != 0) {
        set_id <- unique(d$Sets)
      } else {
        set_id <- 1
        row_num <- nrow(d)
        if (!is.null(d$Sets)) {
          d$Sets <- NULL
        }
        d <- cbind(d, Sets = rep(1, row_num))
        sets <- ncol(d)
      }

      d <- d[order(d$Sample), ]

      # Count observations outside of spec if spec exists (ported; used by capability)
      if (!is.na(USL) || !is.na(Target) || !is.na(LSL)) {
        set_num <- unique(d$Sets)
        above_USL <- below_LSL <- list()
        for (i in set_num) {
          above_USL[[as.character(i)]] <- sum(d$Data[d$Sets == i] > USL)
          below_LSL[[as.character(i)]] <- sum(d$Data[d$Sets == i] < LSL)
        }
      }

      # Calculate the points we are going to use (ported)
      if (as.numeric(x_type) == 1) {
        if (isTRUE(ind_chart)) return(NULL)
        points <- cbind(
          aggregate(Sets ~ Sample, data = d, mean),
          spc.preprocess.data(data = d$Data, sample = d$Sample, stat.n = TRUE, stat.mean = TRUE, stat.range = TRUE, stat.sd = TRUE, stat.var = TRUE)
        )
        points <- points[order(points$g), ]
      }
      if (as.numeric(x_type) == 2) {
        if (!isTRUE(ind_chart)) return(NULL)
        points <- d
        n <- rep(1, k_obs)
        if (as.numeric(input$spc_var_set) != 0) {
          set_change <- c(0, diff(points$Sets))
          MR <- MR_span(data = points$Data, span = span)
          for (i in seq_len(length(set_change))) {
            if (set_change[i] != 0) {
              MR[i] <- NA
            }
          }
        } else {
          MR <- MR_span(data = points$Data, span = span)
        }
        points <- cbind(points, MR, n)
      }

      # Now that we have the points, calculate the limits for each and estimated std
      points_1 <- centerline_1 <- UCL1 <- zone_a_up_1 <- zone_ab_up_1 <- zone_bc_up_1 <- LCL1 <- zone_a_low_1 <- zone_ab_low_1 <- zone_bc_low_1 <- NULL
      points_2 <- centerline_2 <- UCL2 <- zone_a_up_2 <- zone_ab_up_2 <- zone_bc_up_2 <- LCL2 <- zone_a_low_2 <- zone_ab_low_2 <- zone_bc_low_2 <- NULL
      set <- Sample <- sample <- est_sig <- ind_sd <- set_sd <- sd_set <- sig_est <- n_k <- NULL

      sample2 <- NULL

      plot_data <- plot_data_disp <- NULL
      control_vio_x <- control_vio_disp <- NULL

      # Coerce point columns to numeric (guards against list/factor columns)
      if (!is.null(points$range)) points$range <- suppressWarnings(as.numeric(points$range))
      if (!is.null(points$mean)) points$mean <- suppressWarnings(as.numeric(points$mean))
      if (!is.null(points$n)) points$n <- suppressWarnings(as.numeric(points$n))
      if (!is.null(points$sd)) points$sd <- suppressWarnings(as.numeric(points$sd))
      if (!is.null(points$var)) points$var <- suppressWarnings(as.numeric(points$var))

      # --- Location limits (ported from monolithic) ---
      if (as.numeric(x_lim_calc) == 1) { # avg R loc
        for (j in set_id) {
          stat <- mean(points$range[points$Set == j])
          sd_set[j] <- sd(d$Data[d$Set == j])
          Sample <- unique(points$g[points$Set == j])
          if (loc_center_type == 1) centerline_loc <- mean(d$Data[d$Set == j])
          if (loc_center_type == 2) centerline_loc <- median(d$Data[d$Set == j])

          for (i in Sample) {
            n <- points$n[i]
            loc_const <- spc.constant.calculation.A2(sample.size = n, n.sigma = std_err)

            points_1 <- c(points_1, points$mean[i])
            sample <- c(sample, i)
            set <- c(set, j)
            n_k <- c(n_k, n)
            centerline_1 <- c(centerline_1, centerline_loc)
            UCL1 <- c(UCL1, centerline_loc + loc_const * stat)
            LCL1 <- c(LCL1, centerline_loc - loc_const * stat)
            ac1 <- centerline_loc + loc_const * stat - centerline_loc
            zone_a_up_1 <- c(zone_a_up_1, centerline_loc + loc_const * stat)
            zone_ab_up_1 <- c(zone_ab_up_1, centerline_loc + (2 / 3) * ac1)
            zone_bc_up_1 <- c(zone_bc_up_1, centerline_loc + (1 / 3) * ac1)
            zone_a_low_1 <- c(zone_a_low_1, centerline_loc - loc_const * stat)
            zone_ab_low_1 <- c(zone_ab_low_1, centerline_loc - (2 / 3) * ac1)
            zone_bc_low_1 <- c(zone_bc_low_1, centerline_loc - (1 / 3) * ac1)
          }
        }

        plot_data <- cbind(set, sample, n_k, points_1, LCL1, centerline_1, UCL1, zone_a_up_1, zone_ab_up_1, zone_bc_up_1, zone_a_low_1, zone_ab_low_1, zone_bc_low_1)
        plot_data <- as.data.frame(plot_data[order(sample), ])
        control_vio_x <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = plot_data$points_1,
          center.line = plot_data$centerline_1,
          control.limits.ucl = plot_data$UCL1,
          zone.a.upper = plot_data$zone_a_up_1,
          zone.ab.upper = plot_data$zone_ab_up_1,
          zone.bc.upper = plot_data$zone_bc_up_1,
          control.limits.lcl = plot_data$LCL1,
          zone.a.lower = plot_data$zone_a_low_1,
          zone.ab.lower = plot_data$zone_ab_low_1,
          zone.bc.lower = plot_data$zone_bc_low_1
        )
      }

      if (as.numeric(x_lim_calc) == 2) { # median R loc
        for (j in set_id) {
          stat <- median(points$range[points$Set == j])
          sd_set[j] <- sd(d$Data[d$Set == j])
          Sample <- unique(points$g[points$Set == j])
          if (loc_center_type == 1) centerline_loc <- mean(d$Data[d$Set == j])
          if (loc_center_type == 2) centerline_loc <- median(d$Data[d$Set == j])

          for (i in Sample) {
            n <- points$n[i]
            loc_const <- spc.constant.calculation.A4(sample.size = n, n.sigma = std_err)

            points_1 <- c(points_1, points$mean[i])
            sample <- c(sample, i)
            set <- c(set, j)
            n_k <- c(n_k, n)
            centerline_1 <- c(centerline_1, centerline_loc)
            UCL1 <- c(UCL1, centerline_loc + loc_const * stat)
            LCL1 <- c(LCL1, centerline_loc - loc_const * stat)
            ac1 <- centerline_loc + loc_const * stat - centerline_loc
            zone_a_up_1 <- c(zone_a_up_1, centerline_loc + loc_const * stat)
            zone_ab_up_1 <- c(zone_ab_up_1, centerline_loc + (2 / 3) * ac1)
            zone_bc_up_1 <- c(zone_bc_up_1, centerline_loc + (1 / 3) * ac1)
            zone_a_low_1 <- c(zone_a_low_1, centerline_loc - loc_const * stat)
            zone_ab_low_1 <- c(zone_ab_low_1, centerline_loc - (2 / 3) * ac1)
            zone_bc_low_1 <- c(zone_bc_low_1, centerline_loc - (1 / 3) * ac1)
          }
        }

        plot_data <- cbind(set, sample, n_k, points_1, LCL1, centerline_1, UCL1, zone_a_up_1, zone_ab_up_1, zone_bc_up_1, zone_a_low_1, zone_ab_low_1, zone_bc_low_1)
        plot_data <- as.data.frame(plot_data[order(sample), ])
        control_vio_x <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = plot_data$points_1,
          center.line = plot_data$centerline_1,
          control.limits.ucl = plot_data$UCL1,
          zone.a.upper = plot_data$zone_a_up_1,
          zone.ab.upper = plot_data$zone_ab_up_1,
          zone.bc.upper = plot_data$zone_bc_up_1,
          control.limits.lcl = plot_data$LCL1,
          zone.a.lower = plot_data$zone_a_low_1,
          zone.ab.lower = plot_data$zone_ab_low_1,
          zone.bc.lower = plot_data$zone_bc_low_1
        )
      }

      if (as.numeric(x_lim_calc) == 3) { # avg s loc
        for (j in set_id) {
          stat <- mean(points$sd[points$Set == j])
          sd_set[j] <- sd(d$Data[d$Set == j])
          Sample <- unique(points$g[points$Set == j])
          if (loc_center_type == 1) centerline_loc <- mean(d$Data[d$Set == j])
          if (loc_center_type == 2) centerline_loc <- median(d$Data[d$Set == j])

          for (i in Sample) {
            n <- points$n[i]
            loc_const <- spc.constant.calculation.A3(sample.size = n, n.sigma = std_err)

            points_1 <- c(points_1, points$mean[i])
            sample <- c(sample, i)
            set <- c(set, j)
            n_k <- c(n_k, n)
            centerline_1 <- c(centerline_1, centerline_loc)
            UCL1 <- c(UCL1, centerline_loc + loc_const * stat)
            LCL1 <- c(LCL1, centerline_loc - loc_const * stat)
            ac1 <- centerline_loc + loc_const * stat - centerline_loc
            zone_a_up_1 <- c(zone_a_up_1, centerline_loc + loc_const * stat)
            zone_ab_up_1 <- c(zone_ab_up_1, centerline_loc + (2 / 3) * ac1)
            zone_bc_up_1 <- c(zone_bc_up_1, centerline_loc + (1 / 3) * ac1)
            zone_a_low_1 <- c(zone_a_low_1, centerline_loc - loc_const * stat)
            zone_ab_low_1 <- c(zone_ab_low_1, centerline_loc - (2 / 3) * ac1)
            zone_bc_low_1 <- c(zone_bc_low_1, centerline_loc - (1 / 3) * ac1)
          }
        }

        plot_data <- cbind(set, sample, n_k, points_1, LCL1, centerline_1, UCL1, zone_a_up_1, zone_ab_up_1, zone_bc_up_1, zone_a_low_1, zone_ab_low_1, zone_bc_low_1)
        plot_data <- as.data.frame(plot_data[order(sample), ])
        control_vio_x <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = plot_data$points_1,
          center.line = plot_data$centerline_1,
          control.limits.ucl = plot_data$UCL1,
          zone.a.upper = plot_data$zone_a_up_1,
          zone.ab.upper = plot_data$zone_ab_up_1,
          zone.bc.upper = plot_data$zone_bc_up_1,
          control.limits.lcl = plot_data$LCL1,
          zone.a.lower = plot_data$zone_a_low_1,
          zone.ab.lower = plot_data$zone_ab_low_1,
          zone.bc.lower = plot_data$zone_bc_low_1
        )
      }

      if (as.numeric(x_lim_calc) == 4) { # median s loc
        for (j in set_id) {
          stat <- median(points$sd[points$Set == j])
          sd_set[j] <- sd(d$Data[d$Set == j])
          Sample <- unique(points$g[points$Set == j])
          if (loc_center_type == 1) centerline_loc <- mean(d$Data[d$Set == j])
          if (loc_center_type == 2) centerline_loc <- median(d$Data[d$Set == j])

          for (i in Sample) {
            n <- points$n[i]
            loc_const <- std_err / (c6(n) * sqrt(n))

            points_1 <- c(points_1, points$mean[i])
            sample <- c(sample, i)
            set <- c(set, j)
            n_k <- c(n_k, n)
            centerline_1 <- c(centerline_1, centerline_loc)
            UCL1 <- c(UCL1, centerline_loc + loc_const * stat)
            LCL1 <- c(LCL1, centerline_loc - loc_const * stat)
            ac1 <- centerline_loc + loc_const * stat - centerline_loc
            zone_a_up_1 <- c(zone_a_up_1, centerline_loc + loc_const * stat)
            zone_ab_up_1 <- c(zone_ab_up_1, centerline_loc + (2 / 3) * ac1)
            zone_bc_up_1 <- c(zone_bc_up_1, centerline_loc + (1 / 3) * ac1)
            zone_a_low_1 <- c(zone_a_low_1, centerline_loc - loc_const * stat)
            zone_ab_low_1 <- c(zone_ab_low_1, centerline_loc - (2 / 3) * ac1)
            zone_bc_low_1 <- c(zone_bc_low_1, centerline_loc - (1 / 3) * ac1)
          }
        }

        plot_data <- cbind(set, sample, n_k, points_1, LCL1, centerline_1, UCL1, zone_a_up_1, zone_ab_up_1, zone_bc_up_1, zone_a_low_1, zone_ab_low_1, zone_bc_low_1)
        plot_data <- as.data.frame(plot_data[order(sample), ])
        control_vio_x <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = plot_data$points_1,
          center.line = plot_data$centerline_1,
          control.limits.ucl = plot_data$UCL1,
          zone.a.upper = plot_data$zone_a_up_1,
          zone.ab.upper = plot_data$zone_ab_up_1,
          zone.bc.upper = plot_data$zone_bc_up_1,
          control.limits.lcl = plot_data$LCL1,
          zone.a.lower = plot_data$zone_a_low_1,
          zone.ab.lower = plot_data$zone_ab_low_1,
          zone.bc.lower = plot_data$zone_bc_low_1
        )
      }

      if (as.numeric(x_lim_calc) == 5) { # average var loc
        for (j in set_id) {
          stat <- mean(points$var[points$Set == j])
          sd_set[j] <- sd(d$Data[d$Set == j])
          Sample <- unique(points$g[points$Set == j])
          if (loc_center_type == 1) centerline_loc <- mean(d$Data[d$Set == j])
          if (loc_center_type == 2) centerline_loc <- median(d$Data[d$Set == j])

          for (i in Sample) {
            n <- points$n[i]
            loc_const <- std_err / sqrt(n * stat)

            points_1 <- c(points_1, points$mean[i])
            sample <- c(sample, i)
            set <- c(set, j)
            n_k <- c(n_k, n)
            centerline_1 <- c(centerline_1, centerline_loc)
            UCL1 <- c(UCL1, centerline_loc + loc_const * stat)
            LCL1 <- c(LCL1, centerline_loc - loc_const * stat)
            ac1 <- centerline_loc + loc_const * stat - centerline_loc
            zone_a_up_1 <- c(zone_a_up_1, centerline_loc + loc_const * stat)
            zone_ab_up_1 <- c(zone_ab_up_1, centerline_loc + (2 / 3) * ac1)
            zone_bc_up_1 <- c(zone_bc_up_1, centerline_loc + (1 / 3) * ac1)
            zone_a_low_1 <- c(zone_a_low_1, centerline_loc - loc_const * stat)
            zone_ab_low_1 <- c(zone_ab_low_1, centerline_loc - (2 / 3) * ac1)
            zone_bc_low_1 <- c(zone_bc_low_1, centerline_loc - (1 / 3) * ac1)
          }
        }

        plot_data <- cbind(set, sample, n_k, points_1, LCL1, centerline_1, UCL1, zone_a_up_1, zone_ab_up_1, zone_bc_up_1, zone_a_low_1, zone_ab_low_1, zone_bc_low_1)
        plot_data <- as.data.frame(plot_data[order(sample), ])
        control_vio_x <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = plot_data$points_1,
          center.line = plot_data$centerline_1,
          control.limits.ucl = plot_data$UCL1,
          zone.a.upper = plot_data$zone_a_up_1,
          zone.ab.upper = plot_data$zone_ab_up_1,
          zone.bc.upper = plot_data$zone_bc_up_1,
          control.limits.lcl = plot_data$LCL1,
          zone.a.lower = plot_data$zone_a_low_1,
          zone.ab.lower = plot_data$zone_ab_low_1,
          zone.bc.lower = plot_data$zone_bc_low_1
        )
      }

      if (as.numeric(x_lim_calc) == 6) { # MR x loc
        for (j in set_id) {
          if (isTRUE(ind_chart)) {
            stat <- mean(na.omit(points$MR[points$Set == j]))
            Sample <- unique(points$Sample[points$Set == j])
          } else {
            stat <- mean(abs(diff(points$mean[points$Set == j])))
            Sample <- unique(points$g[points$Set == j])
          }
          sd_set[j] <- sd(d$Data[d$Set == j])
          if (loc_center_type == 1) centerline_loc <- mean(d$Data[d$Set == j])
          if (loc_center_type == 2) centerline_loc <- median(d$Data[d$Set == j])

          for (i in Sample) {
            n <- points$n[i]
            loc_const <- std_err / (spc.constant.calculation.d2(sample.size = span))

            if (isTRUE(ind_chart)) {
              points_1 <- c(points_1, points$Data[i])
            } else {
              points_1 <- c(points_1, points$mean[i])
            }

            sample <- c(sample, i)
            set <- c(set, j)
            n_k <- c(n_k, n)
            centerline_1 <- c(centerline_1, centerline_loc)
            UCL1 <- c(UCL1, centerline_loc + loc_const * stat)
            LCL1 <- c(LCL1, centerline_loc - loc_const * stat)
            ac1 <- centerline_loc + loc_const * stat - centerline_loc
            zone_a_up_1 <- c(zone_a_up_1, centerline_loc + loc_const * stat)
            zone_ab_up_1 <- c(zone_ab_up_1, centerline_loc + (2 / 3) * ac1)
            zone_bc_up_1 <- c(zone_bc_up_1, centerline_loc + (1 / 3) * ac1)
            zone_a_low_1 <- c(zone_a_low_1, centerline_loc - loc_const * stat)
            zone_ab_low_1 <- c(zone_ab_low_1, centerline_loc - (2 / 3) * ac1)
            zone_bc_low_1 <- c(zone_bc_low_1, centerline_loc - (1 / 3) * ac1)
          }
        }

        plot_data <- cbind(set, sample, n_k, points_1, LCL1, centerline_1, UCL1, zone_a_up_1, zone_ab_up_1, zone_bc_up_1, zone_a_low_1, zone_ab_low_1, zone_bc_low_1)
        plot_data <- as.data.frame(plot_data[order(sample), ])
        control_vio_x <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = plot_data$points_1,
          center.line = plot_data$centerline_1,
          control.limits.ucl = plot_data$UCL1,
          zone.a.upper = plot_data$zone_a_up_1,
          zone.ab.upper = plot_data$zone_ab_up_1,
          zone.bc.upper = plot_data$zone_bc_up_1,
          control.limits.lcl = plot_data$LCL1,
          zone.a.lower = plot_data$zone_a_low_1,
          zone.ab.lower = plot_data$zone_ab_low_1,
          zone.bc.lower = plot_data$zone_bc_low_1
        )
      }

      if (as.numeric(x_lim_calc) == 7) { # median MR x loc
        for (j in set_id) {
          if (isTRUE(ind_chart)) {
            stat <- median(na.omit(points$MR[points$Set == j]))
            Sample <- unique(points$Sample[points$Set == j])
          } else {
            stat <- median(abs(diff(points$mean[points$Set == j])))
            Sample <- unique(points$g[points$Set == j])
          }
          sd_set[j] <- sd(d$Data[d$Set == j])

          if (loc_center_type == 1) centerline_loc <- mean(d$Data[d$Set == j])
          if (loc_center_type == 2) centerline_loc <- median(d$Data[d$Set == j])

          for (i in Sample) {
            n <- points$n[i]
            loc_const <- std_err / (spc.constant.calculation.d4(sample.size = span))

            if (isTRUE(ind_chart)) {
              points_1 <- c(points_1, points$Data[i])
            } else {
              points_1 <- c(points_1, points$mean[i])
            }

            sample <- c(sample, i)
            set <- c(set, j)
            n_k <- c(n_k, n)
            centerline_1 <- c(centerline_1, centerline_loc)
            UCL1 <- c(UCL1, centerline_loc + loc_const * stat)
            LCL1 <- c(LCL1, centerline_loc - loc_const * stat)
            ac1 <- centerline_loc + loc_const * stat - centerline_loc
            zone_a_up_1 <- c(zone_a_up_1, centerline_loc + loc_const * stat)
            zone_ab_up_1 <- c(zone_ab_up_1, centerline_loc + (2 / 3) * ac1)
            zone_bc_up_1 <- c(zone_bc_up_1, centerline_loc + (1 / 3) * ac1)
            zone_a_low_1 <- c(zone_a_low_1, centerline_loc - loc_const * stat)
            zone_ab_low_1 <- c(zone_ab_low_1, centerline_loc - (2 / 3) * ac1)
            zone_bc_low_1 <- c(zone_bc_low_1, centerline_loc - (1 / 3) * ac1)
          }
        }

        plot_data <- cbind(set, sample, n_k, points_1, LCL1, centerline_1, UCL1, zone_a_up_1, zone_ab_up_1, zone_bc_up_1, zone_a_low_1, zone_ab_low_1, zone_bc_low_1)
        plot_data <- as.data.frame(plot_data[order(sample), ])
        control_vio_x <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = plot_data$points_1,
          center.line = plot_data$centerline_1,
          control.limits.ucl = plot_data$UCL1,
          zone.a.upper = plot_data$zone_a_up_1,
          zone.ab.upper = plot_data$zone_ab_up_1,
          zone.bc.upper = plot_data$zone_bc_up_1,
          control.limits.lcl = plot_data$LCL1,
          zone.a.lower = plot_data$zone_a_low_1,
          zone.ab.lower = plot_data$zone_ab_low_1,
          zone.bc.lower = plot_data$zone_bc_low_1
        )
      }

      if (as.numeric(x_lim_calc) == 8) { # std x loc
        for (j in set_id) {
          if (isTRUE(ind_chart)) {
            stat <- sd(points$Data[points$Set == j])
            Sample <- unique(points$Sample[points$Set == j])
          } else {
            stat <- sd(points$mean[points$Set == j])
            Sample <- unique(points$g[points$Set == j])
          }
          sd_set[j] <- sd(d$Data[d$Set == j])
          if (loc_center_type == 1) centerline_loc <- mean(d$Data[d$Set == j])
          if (loc_center_type == 2) centerline_loc <- median(d$Data[d$Set == j])

          for (i in Sample) {
            n <- points$n[i]
            loc_const <- 3

            if (isTRUE(ind_chart)) {
              points_1 <- c(points_1, points$Data[i])
            } else {
              points_1 <- c(points_1, points$mean[i])
            }

            sample <- c(sample, i)
            set <- c(set, j)
            n_k <- c(n_k, n)
            centerline_1 <- c(centerline_1, centerline_loc)
            UCL1 <- c(UCL1, centerline_loc + loc_const * stat)
            LCL1 <- c(LCL1, centerline_loc - loc_const * stat)
            ac1 <- centerline_loc + loc_const * stat - centerline_loc
            zone_a_up_1 <- c(zone_a_up_1, centerline_loc + loc_const * stat)
            zone_ab_up_1 <- c(zone_ab_up_1, centerline_loc + (2 / 3) * ac1)
            zone_bc_up_1 <- c(zone_bc_up_1, centerline_loc + (1 / 3) * ac1)
            zone_a_low_1 <- c(zone_a_low_1, centerline_loc - loc_const * stat)
            zone_ab_low_1 <- c(zone_ab_low_1, centerline_loc - (2 / 3) * ac1)
            zone_bc_low_1 <- c(zone_bc_low_1, centerline_loc - (1 / 3) * ac1)
          }
        }

        plot_data <- cbind(set, sample, n_k, points_1, LCL1, centerline_1, UCL1, zone_a_up_1, zone_ab_up_1, zone_bc_up_1, zone_a_low_1, zone_ab_low_1, zone_bc_low_1)
        plot_data <- as.data.frame(plot_data[order(sample), ])
        control_vio_x <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = plot_data$points_1,
          center.line = plot_data$centerline_1,
          control.limits.ucl = plot_data$UCL1,
          zone.a.upper = plot_data$zone_a_up_1,
          zone.ab.upper = plot_data$zone_ab_up_1,
          zone.bc.upper = plot_data$zone_bc_up_1,
          control.limits.lcl = plot_data$LCL1,
          zone.a.lower = plot_data$zone_a_low_1,
          zone.ab.lower = plot_data$zone_ab_low_1,
          zone.bc.lower = plot_data$zone_bc_low_1
        )
      }

      if (as.numeric(x_lim_calc) == 9) { # known sigma loc
        for (j in set_id) {
          if (isTRUE(ind_chart)) {
            Sample <- unique(points$Sample[points$Set == j])
          } else {
            Sample <- unique(points$g[points$Set == j])
          }

          stat <- known_sig_x
          sd_set[j] <- sd(d$Data[d$Set == j])
          if (loc_center_type == 1) centerline_loc <- mean(d$Data[d$Set == j])
          if (loc_center_type == 2) centerline_loc <- median(d$Data[d$Set == j])

          for (i in Sample) {
            n <- points$n[i]
            loc_const <- std_err / sqrt(n)

            if (isTRUE(ind_chart)) {
              points_1 <- c(points_1, points$Data[i])
            } else {
              points_1 <- c(points_1, points$mean[i])
            }

            sample <- c(sample, i)
            set <- c(set, j)
            n_k <- c(n_k, n)
            centerline_1 <- c(centerline_1, centerline_loc)
            UCL1 <- c(UCL1, centerline_loc + loc_const * stat)
            LCL1 <- c(LCL1, centerline_loc - loc_const * stat)
            ac1 <- centerline_loc + loc_const * stat - centerline_loc
            zone_a_up_1 <- c(zone_a_up_1, centerline_loc + loc_const * stat)
            zone_ab_up_1 <- c(zone_ab_up_1, centerline_loc + (2 / 3) * ac1)
            zone_bc_up_1 <- c(zone_bc_up_1, centerline_loc + (1 / 3) * ac1)
            zone_a_low_1 <- c(zone_a_low_1, centerline_loc - loc_const * stat)
            zone_ab_low_1 <- c(zone_ab_low_1, centerline_loc - (2 / 3) * ac1)
            zone_bc_low_1 <- c(zone_bc_low_1, centerline_loc - (1 / 3) * ac1)
          }
        }

        plot_data <- cbind(set, sample, n_k, points_1, LCL1, centerline_1, UCL1, zone_a_up_1, zone_ab_up_1, zone_bc_up_1, zone_a_low_1, zone_ab_low_1, zone_bc_low_1)
        plot_data <- as.data.frame(plot_data[order(sample), ])
        control_vio_x <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = plot_data$points_1,
          center.line = plot_data$centerline_1,
          control.limits.ucl = plot_data$UCL1,
          zone.a.upper = plot_data$zone_a_up_1,
          zone.ab.upper = plot_data$zone_ab_up_1,
          zone.bc.upper = plot_data$zone_bc_up_1,
          control.limits.lcl = plot_data$LCL1,
          zone.a.lower = plot_data$zone_a_low_1,
          zone.ab.lower = plot_data$zone_ab_low_1,
          zone.bc.lower = plot_data$zone_bc_low_1
        )
      }

      if (as.numeric(x_lim_calc) == 10 || as.numeric(x_lim_calc) == 12) { # custom loc
        for (j in set_id) {
          sd_set[j] <- sd(d$Data[d$Set == j])
          if (isTRUE(ind_chart)) {
            Sample <- unique(points$Sample[points$Set == j])
          } else {
            Sample <- unique(points$g[points$Set == j])
          }
          centerline_loc <- loc_center_custom

          for (i in Sample) {
            n <- points$n[i]
            if (isTRUE(ind_chart)) {
              points_1 <- c(points_1, points$Data[i])
            } else {
              points_1 <- c(points_1, points$mean[i])
            }

            sample <- c(sample, i)
            set <- c(set, j)
            n_k <- c(n_k, n)
            centerline_1 <- c(centerline_1, centerline_loc)
            UCL1 <- c(UCL1, loc_upper_custom)
            LCL1 <- c(LCL1, loc_lower_custom)
            zone_a_up_1 <- c(zone_a_up_1, loc_upper_custom)
            zone_ab_up_1 <- c(zone_ab_up_1, centerline_loc + (2 / 3) * (loc_upper_custom - centerline_loc))
            zone_bc_up_1 <- c(zone_bc_up_1, centerline_loc + (1 / 3) * (loc_upper_custom - centerline_loc))
            zone_a_low_1 <- c(zone_a_low_1, loc_lower_custom)
            zone_ab_low_1 <- c(zone_ab_low_1, centerline_loc - (2 / 3) * (centerline_loc - loc_lower_custom))
            zone_bc_low_1 <- c(zone_bc_low_1, centerline_loc - (1 / 3) * (centerline_loc - loc_lower_custom))
          }
        }

        plot_data <- cbind(set, sample, n_k, points_1, LCL1, centerline_1, UCL1, zone_a_up_1, zone_ab_up_1, zone_bc_up_1, zone_a_low_1, zone_ab_low_1, zone_bc_low_1)
        plot_data <- as.data.frame(plot_data[order(sample), ])
        control_vio_x <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = plot_data$points_1,
          center.line = plot_data$centerline_1,
          control.limits.ucl = plot_data$UCL1,
          zone.a.upper = plot_data$zone_a_up_1,
          zone.ab.upper = plot_data$zone_ab_up_1,
          zone.bc.upper = plot_data$zone_bc_up_1,
          control.limits.lcl = plot_data$LCL1,
          zone.a.lower = plot_data$zone_a_low_1,
          zone.ab.lower = plot_data$zone_ab_low_1,
          zone.bc.lower = plot_data$zone_bc_low_1
        )
      }

      # --- Dispersion limits (ported from monolithic) ---
      if (as.numeric(disp_lim_calc) == 1) { # avg R disp
        for (j in set_id) {
          stat <- mean(points$range[points$Set == j])
          disp_d <- spc.constant.calculation.d2(sample.size = mean(points$n[points$Set == j]))
          sig_est[j] <- stat / disp_d

          if (disp_type == 1) {
            if (disp_center_type == 1) centerline_disp <- mean(points$range[points$Set == j])
            if (disp_center_type == 2) centerline_disp <- median(points$range[points$Set == j])
          }
          if (disp_type == 2) {
            if (disp_center_type == 1) centerline_disp <- mean(points$sd[points$Set == j])
            if (disp_center_type == 2) centerline_disp <- median(points$sd[points$Set == j])
          }
          if (disp_type == 3) {
            if (disp_center_type == 1) centerline_disp <- mean(points$var[points$Set == j])
            if (disp_center_type == 2) centerline_disp <- median(points$var[points$Set == j])
          }

          Sample <- unique(points$g[points$Set == j])
          for (i in Sample) {
            n <- points$n[i]

            if (disp_type == 1) {
              disp_low <- spc.constant.calculation.D3(sample.size = n, n.sigma = std_err)
              disp_up <- spc.constant.calculation.D4(sample.size = n, n.sigma = std_err)
              points_2 <- c(points_2, points$range[i])
            }

            if (disp_type == 2) {
              mean_est <- sig_est[j] * spc.constant.calculation.c4(sample.size = n)
              disp_low <- spc.constant.calculation.B3(sample.size = n, n.sigma = std_err)
              disp_up <- spc.constant.calculation.B4(sample.size = n, n.sigma = std_err)
              points_2 <- c(points_2, points$sd[i])
              stat <- mean_est
            }

            if (disp_type == 3) {
              p_low <- pnorm(-std_err, 0, 1)
              p_high <- pnorm(std_err, 0, 1)
              mean_est <- sig_est[j]^2
              disp_low <- qchisq(p = p_low, df = n - 1) / (n - 1)
              disp_up <- qchisq(p = p_high, df = n - 1) / (n - 1)
              points_2 <- c(points_2, points$var[i])
              stat <- mean_est
            }

            sample2 <- c(sample2, i)
            centerline_2 <- c(centerline_2, centerline_disp)
            UCL2 <- c(UCL2, disp_up * stat)
            LCL2 <- c(LCL2, disp_low * stat)
            est_sig <- c(est_sig, sig_est[j])
            ind_sd <- c(ind_sd, st_dev_ind)
            set_sd <- c(set_sd, sd_set[j])
            ac2 <- disp_up * stat - centerline_disp
            zone_a_up_2 <- c(zone_a_up_2, disp_up * stat)
            zone_ab_up_2 <- c(zone_ab_up_2, centerline_disp + (2 / 3) * ac2)
            zone_bc_up_2 <- c(zone_bc_up_2, centerline_disp + (1 / 3) * ac2)
            ac3 <- centerline_disp - LCL2[i]
            if (is.na(ac3)) ac3 <- centerline_disp
            zone_a_low_2 <- c(zone_a_low_2, centerline_disp - ac3)
            zone_ab_low_2 <- c(zone_ab_low_2, centerline_disp - (2 / 3) * ac3)
            zone_bc_low_2 <- c(zone_bc_low_2, centerline_disp - (1 / 3) * ac3)
          }
        }

        plot_data_disp <- cbind(sample2, points_2, LCL2, centerline_2, UCL2, est_sig, ind_sd, set_sd, zone_a_up_2, zone_ab_up_2, zone_bc_up_2, zone_a_low_2, zone_ab_low_2, zone_bc_low_2)
        plot_data_disp <- as.data.frame(plot_data_disp[order(sample2), ])
        control_vio_disp <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = points_2,
          center.line = centerline_2,
          control.limits.ucl = UCL2,
          zone.a.upper = zone_a_up_2,
          zone.ab.upper = zone_ab_up_2,
          zone.bc.upper = zone_bc_up_2,
          control.limits.lcl = LCL2,
          zone.a.lower = zone_a_low_2,
          zone.ab.lower = zone_ab_low_2,
          zone.bc.lower = zone_bc_low_2
        )
      }

      if (as.numeric(disp_lim_calc) == 2) { # median R disp
        for (j in set_id) {
          stat <- median(points$range[points$Set == j])
          disp_d <- spc.constant.calculation.d4(sample.size = mean(points$n[points$Set == j]))
          sig_est[j] <- stat / disp_d

          if (disp_type == 1) {
            if (disp_center_type == 1) centerline_disp <- mean(points$range[points$Set == j])
            if (disp_center_type == 2) centerline_disp <- median(points$range[points$Set == j])
          }
          if (disp_type == 2) {
            if (disp_center_type == 1) centerline_disp <- mean(points$sd[points$Set == j])
            if (disp_center_type == 2) centerline_disp <- median(points$sd[points$Set == j])
          }
          if (disp_type == 3) {
            if (disp_center_type == 1) centerline_disp <- mean(points$var[points$Set == j])
            if (disp_center_type == 2) centerline_disp <- median(points$var[points$Set == j])
          }

          Sample <- unique(points$g[points$Set == j])
          for (i in Sample) {
            n <- points$n[i]

            if (disp_type == 1) {
              disp_low <- spc.constant.calculation.D5(sample.size = n, n.sigma = std_err)
              disp_up <- spc.constant.calculation.D6(sample.size = n, n.sigma = std_err)
              points_2 <- c(points_2, points$range[i])
            }

            if (disp_type == 2) {
              mean_est <- sig_est[j] * spc.constant.calculation.c4(sample.size = n)
              disp_low <- spc.constant.calculation.B3(sample.size = n, n.sigma = std_err)
              disp_up <- spc.constant.calculation.B4(sample.size = n, n.sigma = std_err)
              points_2 <- c(points_2, points$sd[i])
              stat <- mean_est
            }

            if (disp_type == 3) {
              p_low <- pnorm(-std_err, 0, 1)
              p_high <- pnorm(std_err, 0, 1)
              mean_est <- sig_est[j]^2
              disp_low <- qchisq(p = p_low, df = n - 1) / (n - 1)
              disp_up <- qchisq(p = p_high, df = n - 1) / (n - 1)
              points_2 <- c(points_2, points$var[i])
              stat <- mean_est
            }

            sample2 <- c(sample2, i)
            centerline_2 <- c(centerline_2, centerline_disp)
            UCL2 <- c(UCL2, disp_up * stat)
            LCL2 <- c(LCL2, disp_low * stat)
            est_sig <- c(est_sig, sig_est[j])
            ind_sd <- c(ind_sd, st_dev_ind)
            set_sd <- c(set_sd, sd_set[j])
            ac2 <- disp_up * stat - centerline_disp
            zone_a_up_2 <- c(zone_a_up_2, disp_up * stat)
            zone_ab_up_2 <- c(zone_ab_up_2, centerline_disp + (2 / 3) * ac2)
            zone_bc_up_2 <- c(zone_bc_up_2, centerline_disp + (1 / 3) * ac2)
            ac3 <- centerline_disp - LCL2[i]
            if (is.na(ac3)) ac3 <- centerline_disp
            zone_a_low_2 <- c(zone_a_low_2, centerline_disp - ac3)
            zone_ab_low_2 <- c(zone_ab_low_2, centerline_disp - (2 / 3) * ac3)
            zone_bc_low_2 <- c(zone_bc_low_2, centerline_disp - (1 / 3) * ac3)
          }
        }

        plot_data_disp <- cbind(sample2, points_2, LCL2, centerline_2, UCL2, est_sig, ind_sd, set_sd, zone_a_up_2, zone_ab_up_2, zone_bc_up_2, zone_a_low_2, zone_ab_low_2, zone_bc_low_2)
        plot_data_disp <- as.data.frame(plot_data_disp[order(sample2), ])
        control_vio_disp <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = points_2,
          center.line = centerline_2,
          control.limits.ucl = UCL2,
          zone.a.upper = zone_a_up_2,
          zone.ab.upper = zone_ab_up_2,
          zone.bc.upper = zone_bc_up_2,
          control.limits.lcl = LCL2,
          zone.a.lower = zone_a_low_2,
          zone.ab.lower = zone_ab_low_2,
          zone.bc.lower = zone_bc_low_2
        )
      }

      if (as.numeric(disp_lim_calc) == 3) { # ave s disp
        for (j in set_id) {
          stat <- mean(points$sd[points$Set == j])
          disp_d <- spc.constant.calculation.c4(sample.size = mean(points$n[points$Set == j]))
          sig_est[j] <- stat / disp_d

          if (disp_type == 1) {
            if (disp_center_type == 1) centerline_disp <- mean(points$range[points$Set == j])
            if (disp_center_type == 2) centerline_disp <- median(points$range[points$Set == j])
          }
          if (disp_type == 2) {
            if (disp_center_type == 1) centerline_disp <- mean(points$sd[points$Set == j])
            if (disp_center_type == 2) centerline_disp <- median(points$sd[points$Set == j])
          }
          if (disp_type == 3) {
            if (disp_center_type == 1) centerline_disp <- mean(points$var[points$Set == j])
            if (disp_center_type == 2) centerline_disp <- median(points$var[points$Set == j])
          }

          Sample <- unique(points$g[points$Set == j])
          for (i in Sample) {
            n <- points$n[i]

            if (disp_type == 1) {
              disp_low <- spc.constant.calculation.d2(sample.size = n) * spc.constant.calculation.D3(sample.size = n, n.sigma = std_err) / spc.constant.calculation.c4(sample.size = n)
              disp_up <- spc.constant.calculation.d2(sample.size = n) * spc.constant.calculation.D4(sample.size = n, n.sigma = std_err) / spc.constant.calculation.c4(sample.size = n)
              points_2 <- c(points_2, points$range[i])
            }

            if (disp_type == 2) {
              disp_low <- spc.constant.calculation.B3(sample.size = n, n.sigma = std_err)
              disp_up <- spc.constant.calculation.B4(sample.size = n, n.sigma = std_err)
              points_2 <- c(points_2, points$sd[i])
            }

            if (disp_type == 3) {
              p_low <- pnorm(-std_err, 0, 1)
              p_high <- pnorm(std_err, 0, 1)
              mean_est <- sig_est[j]^2
              disp_low <- qchisq(p = p_low, df = n - 1) / (n - 1)
              disp_up <- qchisq(p = p_high, df = n - 1) / (n - 1)
              points_2 <- c(points_2, points$var[i])
              stat <- mean_est
            }

            sample2 <- c(sample2, i)
            centerline_2 <- c(centerline_2, centerline_disp)
            UCL2 <- c(UCL2, disp_up * stat)
            LCL2 <- c(LCL2, disp_low * stat)
            est_sig <- c(est_sig, sig_est[j])
            ind_sd <- c(ind_sd, st_dev_ind)
            set_sd <- c(set_sd, sd_set[j])
            ac2 <- disp_up * stat - centerline_disp
            zone_a_up_2 <- c(zone_a_up_2, disp_up * stat)
            zone_ab_up_2 <- c(zone_ab_up_2, centerline_disp + (2 / 3) * ac2)
            zone_bc_up_2 <- c(zone_bc_up_2, centerline_disp + (1 / 3) * ac2)
            ac3 <- centerline_disp - LCL2[i]
            if (is.na(ac3)) ac3 <- centerline_disp
            zone_a_low_2 <- c(zone_a_low_2, centerline_disp - ac3)
            zone_ab_low_2 <- c(zone_ab_low_2, centerline_disp - (2 / 3) * ac3)
            zone_bc_low_2 <- c(zone_bc_low_2, centerline_disp - (1 / 3) * ac3)
          }
        }

        plot_data_disp <- cbind(sample2, points_2, LCL2, centerline_2, UCL2, est_sig, ind_sd, set_sd, zone_a_up_2, zone_ab_up_2, zone_bc_up_2, zone_a_low_2, zone_ab_low_2, zone_bc_low_2)
        plot_data_disp <- as.data.frame(plot_data_disp[order(sample2), ])
        control_vio_disp <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = points_2,
          center.line = centerline_2,
          control.limits.ucl = UCL2,
          zone.a.upper = zone_a_up_2,
          zone.ab.upper = zone_ab_up_2,
          zone.bc.upper = zone_bc_up_2,
          control.limits.lcl = LCL2,
          zone.a.lower = zone_a_low_2,
          zone.ab.lower = zone_ab_low_2,
          zone.bc.lower = zone_bc_low_2
        )
      }

      if (as.numeric(disp_lim_calc) == 4) { # median s disp
        for (j in set_id) {
          stat <- median(points$sd[points$Set == j])
          disp_d <- c6(mean(points$n[points$Set == j]))
          sig_est[j] <- stat / disp_d

          if (disp_type == 1) {
            if (disp_center_type == 1) centerline_disp <- mean(points$range[points$Set == j])
            if (disp_center_type == 2) centerline_disp <- median(points$range[points$Set == j])
          }
          if (disp_type == 2) {
            if (disp_center_type == 1) centerline_disp <- mean(points$sd[points$Set == j])
            if (disp_center_type == 2) centerline_disp <- median(points$sd[points$Set == j])
          }
          if (disp_type == 3) {
            if (disp_center_type == 1) centerline_disp <- mean(points$var[points$Set == j])
            if (disp_center_type == 2) centerline_disp <- median(points$var[points$Set == j])
          }

          Sample <- unique(points$g[points$Set == j])
          for (i in Sample) {
            n <- points$n[i]

            if (disp_type == 1) {
              disp_low <- spc.constant.calculation.d2(sample.size = n) * spc.constant.calculation.D3(sample.size = n, n.sigma = std_err) / c6(n)
              disp_up <- spc.constant.calculation.d2(sample.size = n) * spc.constant.calculation.D4(sample.size = n, n.sigma = std_err) / c6(n)
              points_2 <- c(points_2, points$range[i])
            }

            if (disp_type == 2) {
              mean_est <- sig_est[j] * spc.constant.calculation.c4(sample.size = n)
              disp_low <- spc.constant.calculation.B3(sample.size = n, n.sigma = std_err)
              disp_up <- spc.constant.calculation.B4(sample.size = n, n.sigma = std_err)
              points_2 <- c(points_2, points$sd[i])
              stat <- mean_est
            }

            if (disp_type == 3) {
              p_low <- pnorm(-std_err, 0, 1)
              p_high <- pnorm(std_err, 0, 1)
              mean_est <- sig_est[j]^2
              disp_low <- qchisq(p = p_low, df = n - 1) / (n - 1)
              disp_up <- qchisq(p = p_high, df = n - 1) / (n - 1)
              points_2 <- c(points_2, points$var[i])
              stat <- mean_est
            }

            sample2 <- c(sample2, i)
            centerline_2 <- c(centerline_2, centerline_disp)
            UCL2 <- c(UCL2, disp_up * stat)
            LCL2 <- c(LCL2, disp_low * stat)
            est_sig <- c(est_sig, sig_est[j])
            ind_sd <- c(ind_sd, st_dev_ind)
            set_sd <- c(set_sd, sd_set[j])
            ac2 <- disp_up * stat - centerline_disp
            zone_a_up_2 <- c(zone_a_up_2, disp_up * stat)
            zone_ab_up_2 <- c(zone_ab_up_2, centerline_disp + (2 / 3) * ac2)
            zone_bc_up_2 <- c(zone_bc_up_2, centerline_disp + (1 / 3) * ac2)
            ac3 <- centerline_disp - LCL2[i]
            if (is.na(ac3)) ac3 <- centerline_disp
            zone_a_low_2 <- c(zone_a_low_2, centerline_disp - ac3)
            zone_ab_low_2 <- c(zone_ab_low_2, centerline_disp - (2 / 3) * ac3)
            zone_bc_low_2 <- c(zone_bc_low_2, centerline_disp - (1 / 3) * ac3)
          }
        }

        plot_data_disp <- cbind(sample2, points_2, LCL2, centerline_2, UCL2, est_sig, ind_sd, set_sd, zone_a_up_2, zone_ab_up_2, zone_bc_up_2, zone_a_low_2, zone_ab_low_2, zone_bc_low_2)
        plot_data_disp <- as.data.frame(plot_data_disp[order(sample2), ])
        control_vio_disp <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = points_2,
          center.line = centerline_2,
          control.limits.ucl = UCL2,
          zone.a.upper = zone_a_up_2,
          zone.ab.upper = zone_ab_up_2,
          zone.bc.upper = zone_bc_up_2,
          control.limits.lcl = LCL2,
          zone.a.lower = zone_a_low_2,
          zone.ab.lower = zone_ab_low_2,
          zone.bc.lower = zone_bc_low_2
        )
      }

      if (as.numeric(disp_lim_calc) == 5) { # ave var disp
        for (j in set_id) {
          stat <- mean(points$var[points$Set == j])
          sig_est[j] <- sqrt(stat)

          if (disp_type == 1) {
            if (disp_center_type == 1) centerline_disp <- mean(points$range[points$Set == j])
            if (disp_center_type == 2) centerline_disp <- median(points$range[points$Set == j])
          }
          if (disp_type == 2) {
            if (disp_center_type == 1) centerline_disp <- mean(points$sd[points$Set == j])
            if (disp_center_type == 2) centerline_disp <- median(points$sd[points$Set == j])
          }
          if (disp_type == 3) {
            if (disp_center_type == 1) centerline_disp <- mean(points$var[points$Set == j])
            if (disp_center_type == 2) centerline_disp <- median(points$var[points$Set == j])
          }

          Sample <- unique(points$g[points$Set == j])
          for (i in Sample) {
            n <- points$n[i]

            if (disp_type == 1) {
              disp_low <- spc.constant.calculation.d2(sample.size = n) * spc.constant.calculation.D3(sample.size = n, n.sigma = std_err) / sqrt(stat)
              disp_up <- spc.constant.calculation.d2(sample.size = n) * spc.constant.calculation.D4(sample.size = n, n.sigma = std_err) / sqrt(stat)
              points_2 <- c(points_2, points$range[i])
            }

            if (disp_type == 2) {
              mean_est <- sig_est[j] * spc.constant.calculation.c4(sample.size = n)
              disp_low <- spc.constant.calculation.B3(sample.size = n, n.sigma = std_err)
              disp_up <- spc.constant.calculation.B4(sample.size = n, n.sigma = std_err)
              points_2 <- c(points_2, points$sd[i])
              stat <- mean_est
            }

            if (disp_type == 3) {
              p_low <- pnorm(-std_err, 0, 1)
              p_high <- pnorm(std_err, 0, 1)
              mean_est <- sig_est[j]^2
              disp_low <- qchisq(p = p_low, df = n - 1) / (n - 1)
              disp_up <- qchisq(p = p_high, df = n - 1) / (n - 1)
              points_2 <- c(points_2, points$var[i])
              stat <- mean_est
            }

            sample2 <- c(sample2, i)
            centerline_2 <- c(centerline_2, centerline_disp)
            UCL2 <- c(UCL2, disp_up * stat)
            LCL2 <- c(LCL2, disp_low * stat)
            est_sig <- c(est_sig, sig_est[j])
            ind_sd <- c(ind_sd, st_dev_ind)
            set_sd <- c(set_sd, sd_set[j])
            ac2 <- disp_up * stat - centerline_disp
            zone_a_up_2 <- c(zone_a_up_2, disp_up * stat)
            zone_ab_up_2 <- c(zone_ab_up_2, centerline_disp + (2 / 3) * ac2)
            zone_bc_up_2 <- c(zone_bc_up_2, centerline_disp + (1 / 3) * ac2)
            ac3 <- centerline_disp - LCL2[i]
            if (is.na(ac3)) ac3 <- centerline_disp
            zone_a_low_2 <- c(zone_a_low_2, centerline_disp - ac3)
            zone_ab_low_2 <- c(zone_ab_low_2, centerline_disp - (2 / 3) * ac3)
            zone_bc_low_2 <- c(zone_bc_low_2, centerline_disp - (1 / 3) * ac3)
          }
        }

        plot_data_disp <- cbind(sample2, points_2, LCL2, centerline_2, UCL2, est_sig, ind_sd, set_sd, zone_a_up_2, zone_ab_up_2, zone_bc_up_2, zone_a_low_2, zone_ab_low_2, zone_bc_low_2)
        plot_data_disp <- as.data.frame(plot_data_disp[order(sample2), ])
        control_vio_disp <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = points_2,
          center.line = centerline_2,
          control.limits.ucl = UCL2,
          zone.a.upper = zone_a_up_2,
          zone.ab.upper = zone_ab_up_2,
          zone.bc.upper = zone_bc_up_2,
          control.limits.lcl = LCL2,
          zone.a.lower = zone_a_low_2,
          zone.ab.lower = zone_ab_low_2,
          zone.bc.lower = zone_bc_low_2
        )
      }

      if (as.numeric(disp_lim_calc) == 6) { # ave mr of disp
        for (j in set_id) {
          n <- mean(points$n[points$Set == j])

          if (isTRUE(ind_chart)) {
            stat <- mean(na.omit(points$MR[points$Set == j]))
            Sample <- unique(points$Sample[points$Set == j])
            sig_est[j] <- stat / spc.constant.calculation.d2(sample.size = span)
            if (disp_center_type == 1) centerline_disp <- stat
            if (disp_center_type == 2) centerline_disp <- median(na.omit(points$MR[points$Set == j]))
          }

          if (!isTRUE(ind_chart)) {
            stat_r <- mean(abs(diff(points$range[points$Set == j])))
            stat_s <- mean(abs(diff(points$sd[points$Set == j])))
            stat_s2 <- mean(abs(diff(points$var[points$Set == j])))

            disp_d_r <- spc.constant.calculation.d2(sample.size = 2)
            if (x_lim_calc == 1) {
              stat_x <- mean(points$range[points$Set == j])
              disp_d_x <- spc.constant.calculation.d2(sample.size = mean(points$n[points$Set == j]))
              sig_est[j] <- stat_x / disp_d_x
            }
            if (x_lim_calc == 2) {
              stat_x <- median(points$range[points$Set == j])
              disp_d_x <- spc.constant.calculation.d4(sample.size = mean(points$n[points$Set == j]))
              sig_est[j] <- stat_x / disp_d_x
            }
            if (x_lim_calc == 3) {
              stat_x <- mean(points$sd[points$Set == j])
              disp_d_x <- spc.constant.calculation.c4(sample.size = mean(points$n[points$Set == j]))
              sig_est[j] <- stat_x / disp_d_x
            }
            if (x_lim_calc == 4) {
              stat_x <- median(points$sd[points$Set == j])
              disp_d_x <- c6(mean(points$n[points$Set == j]))
              sig_est[j] <- stat_x / disp_d_x
            }
            if (x_lim_calc == 5) {
              stat_x <- mean(points$var[points$Set == j])
              sig_est[j] <- sqrt(stat_x)
            }
            if (x_lim_calc == 6) {
              sig_est[j] <- sqrt(n) * mean(abs(diff(points$mean[points$Set == j]))) / spc.constant.calculation.d2(sample.size = 2)
            }
            if (x_lim_calc == 7) {
              sig_est[j] <- sqrt(n) * median(abs(diff(points$mean[points$Set == j]))) / spc.constant.calculation.d4(sample.size = 2)
            }
            if (x_lim_calc == 8) {
              sig_est[j] <- sqrt(n) * sd(points$mean[points$Set == j])
            }
            if (x_lim_calc == 9) {
              sig_est[j] <- known_sig_x
            }
            if (x_lim_calc == 10 || x_lim_calc == 11 || x_lim_calc == 12) {
              sig_est[j] <- NA
            }

            if (disp_type == 1) {
              if (disp_center_type == 1) centerline_disp <- mean(points$range[points$Set == j])
              if (disp_center_type == 2) centerline_disp <- median(points$range[points$Set == j])
            }
            if (disp_type == 2) {
              if (disp_center_type == 1) centerline_disp <- mean(points$sd[points$Set == j])
              if (disp_center_type == 2) centerline_disp <- median(points$sd[points$Set == j])
            }
            if (disp_type == 3) {
              if (disp_center_type == 1) centerline_disp <- mean(points$var[points$Set == j])
              if (disp_center_type == 2) centerline_disp <- median(points$var[points$Set == j])
            }
            Sample <- unique(points$g[points$Set == j])
          }

          for (i in Sample) {
            n <- points$n[i]

            if (disp_type == 4) {
              disp_low <- stat * spc.constant.calculation.D3(sample.size = span, n.sigma = std_err) + centerline_disp
              disp_up <- stat * spc.constant.calculation.D4(sample.size = span, n.sigma = std_err) - centerline_disp
              points_2 <- c(points_2, points$MR[i])
            }

            if (disp_type == 1) {
              disp_low <- (std_err * stat_r / disp_d_r)
              disp_up <- std_err * stat_r / disp_d_r
              if (centerline_disp - disp_low < 0) disp_low <- NA
              points_2 <- c(points_2, points$range[i])
            }

            if (disp_type == 2) {
              disp_low <- (std_err * stat_s / disp_d_r)
              disp_up <- std_err * stat_s / disp_d_r
              if (centerline_disp - disp_low < 0) disp_low <- NA
              points_2 <- c(points_2, points$sd[i])
            }

            if (disp_type == 3) {
              disp_low <- (std_err * stat_s2 / disp_d_r)
              disp_up <- std_err * stat_s2 / disp_d_r
              if (centerline_disp - disp_low < 0) disp_low <- NA
              points_2 <- c(points_2, points$var[i])
            }

            sample2 <- c(sample2, i)
            centerline_2 <- c(centerline_2, centerline_disp)
            UCL2 <- c(UCL2, centerline_disp + disp_up)
            LCL2 <- c(LCL2, centerline_disp - disp_low)
            est_sig <- c(est_sig, sig_est[j])
            ind_sd <- c(ind_sd, st_dev_ind)
            set_sd <- c(set_sd, sd_set[j])
            ac2 <- centerline_disp + disp_up - centerline_disp
            zone_a_up_2 <- c(zone_a_up_2, centerline_disp + disp_up)
            zone_ab_up_2 <- c(zone_ab_up_2, centerline_disp + (2 / 3) * ac2)
            zone_bc_up_2 <- c(zone_bc_up_2, centerline_disp + (1 / 3) * ac2)
            if (is.na(centerline_disp - disp_low)) {
              zone_a_low_2 <- c(zone_a_low_2, 0)
              ac2_low <- centerline_disp
            } else {
              zone_a_low_2 <- c(zone_a_low_2, centerline_disp - disp_low)
              ac2_low <- disp_low
            }
            zone_ab_low_2 <- c(zone_ab_low_2, centerline_disp - (2 / 3) * ac2_low)
            zone_bc_low_2 <- c(zone_bc_low_2, centerline_disp - (1 / 3) * ac2_low)
          }
        }

        plot_data_disp <- cbind(sample2, points_2, LCL2, centerline_2, UCL2, est_sig, ind_sd, set_sd, zone_a_up_2, zone_ab_up_2, zone_bc_up_2, zone_a_low_2, zone_ab_low_2, zone_bc_low_2)
        plot_data_disp <- as.data.frame(plot_data_disp[order(sample2), ])
        if (disp_type == 4) ooc_rules <- spc.rulesets.outside.limits()
        control_vio_disp <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = points_2,
          center.line = centerline_2,
          control.limits.ucl = UCL2,
          zone.a.upper = zone_a_up_2,
          zone.ab.upper = zone_ab_up_2,
          zone.bc.upper = zone_bc_up_2,
          control.limits.lcl = LCL2,
          zone.a.lower = zone_a_low_2,
          zone.ab.lower = zone_ab_low_2,
          zone.bc.lower = zone_bc_low_2
        )
      }

      if (as.numeric(disp_lim_calc) == 7) { # median mr of range disp
        for (j in set_id) {
          n <- mean(points$n[points$Set == j])

          if (isTRUE(ind_chart)) {
            stat <- median(na.omit(points$MR[points$Set == j]))
            Sample <- unique(points$Sample[points$Set == j])
            sig_est[j] <- stat / spc.constant.calculation.d4(sample.size = span)
            if (disp_center_type == 1) centerline_disp <- mean(na.omit(points$MR[points$Set == j]))
            if (disp_center_type == 2) centerline_disp <- stat
          }

          if (!isTRUE(ind_chart)) {
            stat_r <- median(abs(diff(points$range[points$Set == j])))
            stat_s <- median(abs(diff(points$sd[points$Set == j])))
            stat_s2 <- median(abs(diff(points$var[points$Set == j])))

            disp_d_r <- spc.constant.calculation.d4(sample.size = 2)
            if (x_lim_calc == 1) {
              stat_x <- mean(points$range[points$Set == j])
              disp_d_x <- spc.constant.calculation.d2(sample.size = mean(points$n[points$Set == j]))
              sig_est[j] <- stat_x / disp_d_x
            }
            if (x_lim_calc == 2) {
              stat_x <- median(points$range[points$Set == j])
              disp_d_x <- spc.constant.calculation.d4(sample.size = mean(points$n[points$Set == j]))
              sig_est[j] <- stat_x / disp_d_x
            }
            if (x_lim_calc == 3) {
              stat_x <- mean(points$sd[points$Set == j])
              disp_d_x <- spc.constant.calculation.c4(sample.size = mean(points$n[points$Set == j]))
              sig_est[j] <- stat_x / disp_d_x
            }
            if (x_lim_calc == 4) {
              stat_x <- median(points$sd[points$Set == j])
              disp_d_x <- c6(mean(points$n[points$Set == j]))
              sig_est[j] <- stat_x / disp_d_x
            }
            if (x_lim_calc == 5) {
              stat_x <- mean(points$var[points$Set == j])
              sig_est[j] <- sqrt(stat_x)
            }
            if (x_lim_calc == 6) {
              sig_est[j] <- sqrt(n) * mean(abs(diff(points$mean[points$Set == j]))) / spc.constant.calculation.d2(sample.size = 2)
            }
            if (x_lim_calc == 7) {
              sig_est[j] <- sqrt(n) * median(abs(diff(points$mean[points$Set == j]))) / spc.constant.calculation.d4(sample.size = 2)
            }
            if (x_lim_calc == 8) {
              sig_est[j] <- sqrt(n) * sd(points$mean[points$Set == j])
            }
            if (x_lim_calc == 9) {
              sig_est[j] <- known_sig_x
            }
            if (x_lim_calc == 10 || x_lim_calc == 11 || x_lim_calc == 12) {
              sig_est[j] <- NA
            }

            if (disp_type == 1) {
              if (disp_center_type == 1) centerline_disp <- mean(points$range[points$Set == j])
              if (disp_center_type == 2) centerline_disp <- median(points$range[points$Set == j])
            }
            if (disp_type == 2) {
              if (disp_center_type == 1) centerline_disp <- mean(points$sd[points$Set == j])
              if (disp_center_type == 2) centerline_disp <- median(points$sd[points$Set == j])
            }
            if (disp_type == 3) {
              if (disp_center_type == 1) centerline_disp <- mean(points$var[points$Set == j])
              if (disp_center_type == 2) centerline_disp <- median(points$var[points$Set == j])
            }
            Sample <- unique(points$g[points$Set == j])
          }

          for (i in Sample) {
            n <- points$n[i]

            if (disp_type == 4) {
              disp_low <- stat * spc.constant.calculation.D5(sample.size = span, n.sigma = std_err) + centerline_disp
              disp_up <- stat * spc.constant.calculation.D6(sample.size = span, n.sigma = std_err) - centerline_disp
              points_2 <- c(points_2, points$MR[i])
            }
            if (disp_type == 1) {
              disp_low <- (std_err * stat_r / disp_d_r)
              disp_up <- std_err * stat_r / disp_d_r
              if (centerline_disp - disp_low < 0) disp_low <- NA
              points_2 <- c(points_2, points$range[i])
            }
            if (disp_type == 2) {
              disp_low <- (std_err * stat_s / disp_d_r)
              disp_up <- std_err * stat_s / disp_d_r
              if (centerline_disp - disp_low < 0) disp_low <- NA
              points_2 <- c(points_2, points$sd[i])
            }
            if (disp_type == 3) {
              disp_low <- (std_err * stat_s2 / disp_d_r)
              disp_up <- std_err * stat_s2 / disp_d_r
              if (centerline_disp - disp_low < 0) disp_low <- NA
              points_2 <- c(points_2, points$var[i])
            }

            sample2 <- c(sample2, i)
            centerline_2 <- c(centerline_2, centerline_disp)
            UCL2 <- c(UCL2, centerline_disp + disp_up)
            LCL2 <- c(LCL2, centerline_disp - disp_low)
            est_sig <- c(est_sig, sig_est[j])
            ind_sd <- c(ind_sd, st_dev_ind)
            set_sd <- c(set_sd, sd_set[j])
            ac2 <- centerline_disp + disp_up - centerline_disp
            zone_a_up_2 <- c(zone_a_up_2, centerline_disp + disp_up)
            zone_ab_up_2 <- c(zone_ab_up_2, centerline_disp + (2 / 3) * ac2)
            zone_bc_up_2 <- c(zone_bc_up_2, centerline_disp + (1 / 3) * ac2)
            if (is.na(centerline_disp - disp_low)) {
              zone_a_low_2 <- c(zone_a_low_2, 0)
              ac2_low <- centerline_disp
            } else {
              zone_a_low_2 <- c(zone_a_low_2, centerline_disp - disp_low)
              ac2_low <- disp_low
            }
            zone_ab_low_2 <- c(zone_ab_low_2, centerline_disp - (2 / 3) * ac2_low)
            zone_bc_low_2 <- c(zone_bc_low_2, centerline_disp - (1 / 3) * ac2_low)
          }
        }

        plot_data_disp <- cbind(sample2, points_2, LCL2, centerline_2, UCL2, est_sig, ind_sd, set_sd, zone_a_up_2, zone_ab_up_2, zone_bc_up_2, zone_a_low_2, zone_ab_low_2, zone_bc_low_2)
        plot_data_disp <- as.data.frame(plot_data_disp[order(sample2), ])
        if (disp_type == 4) ooc_rules <- spc.rulesets.outside.limits()
        control_vio_disp <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = points_2,
          center.line = centerline_2,
          control.limits.ucl = UCL2,
          zone.a.upper = zone_a_up_2,
          zone.ab.upper = zone_ab_up_2,
          zone.bc.upper = zone_bc_up_2,
          control.limits.lcl = LCL2,
          zone.a.lower = zone_a_low_2,
          zone.ab.lower = zone_ab_low_2,
          zone.bc.lower = zone_bc_low_2
        )
      }

      if (as.numeric(disp_lim_calc) == 8) { # s of disp
        for (j in set_id) {
          n <- mean(points$n[points$Set == j])

          if (isTRUE(ind_chart)) {
            Sample <- unique(points$Sample[points$Set == j])
          }

          if (!isTRUE(ind_chart)) {
            stat_r <- sd(points$range[points$Set == j])
            stat_s <- sd(points$sd[points$Set == j])
            stat_s2 <- sd(points$var[points$Set == j])
            Sample <- unique(points$g[points$Set == j])
          }

          if (x_lim_calc == 1) {
            stat_x <- mean(points$range[points$Set == j])
            disp_d_x <- spc.constant.calculation.d2(sample.size = mean(points$n[points$Set == j]))
            sig_est[j] <- stat_x / disp_d_x
          }
          if (x_lim_calc == 2) {
            stat_x <- median(points$range[points$Set == j])
            disp_d_x <- spc.constant.calculation.d4(sample.size = mean(points$n[points$Set == j]))
            sig_est[j] <- stat_x / disp_d_x
          }
          if (x_lim_calc == 3) {
            stat_x <- mean(points$sd[points$Set == j])
            disp_d_x <- spc.constant.calculation.c4(sample.size = mean(points$n[points$Set == j]))
            sig_est[j] <- stat_x / disp_d_x
          }
          if (x_lim_calc == 4) {
            stat_x <- median(points$sd[points$Set == j])
            disp_d_x <- c6(mean(points$n[points$Set == j]))
            sig_est[j] <- stat_x / disp_d_x
          }
          if (x_lim_calc == 5) {
            stat_x <- mean(points$var[points$Set == j])
            sig_est[j] <- sqrt(stat_x)
          }
          if (isTRUE(ind_chart)) {
            sig_est[j] <- sd(points$Data[points$Set == j])
          }
          if (!isTRUE(ind_chart)) {
            if (x_lim_calc == 6) {
              sig_est[j] <- sqrt(n) * mean(abs(diff(points$mean[points$Set == j]))) / spc.constant.calculation.d2(sample.size = 2)
            }
            if (x_lim_calc == 7) {
              sig_est[j] <- sqrt(n) * median(abs(diff(points$mean[points$Set == j]))) / spc.constant.calculation.d4(sample.size = 2)
            }
            if (x_lim_calc == 8) {
              sig_est[j] <- sqrt(n) * sd(points$mean[points$Set == j])
            }
          }
          if (x_lim_calc == 9) {
            sig_est[j] <- known_sig_x
          }
          if (x_lim_calc == 10 || x_lim_calc == 11 || x_lim_calc == 12) {
            sig_est[j] <- NA
          }

          if (disp_type == 1) {
            if (disp_center_type == 1) centerline_disp <- mean(points$range[points$Set == j])
            if (disp_center_type == 2) centerline_disp <- median(points$range[points$Set == j])
          }
          if (disp_type == 2) {
            if (disp_center_type == 1) centerline_disp <- mean(points$sd[points$Set == j])
            if (disp_center_type == 2) centerline_disp <- median(points$sd[points$Set == j])
          }
          if (disp_type == 3) {
            if (disp_center_type == 1) centerline_disp <- mean(points$var[points$Set == j])
            if (disp_center_type == 2) centerline_disp <- median(points$var[points$Set == j])
          }
          if (disp_type == 4) {
            if (disp_center_type == 1) centerline_disp <- mean(na.omit(points$MR[points$Set == j]))
            if (disp_center_type == 2) centerline_disp <- median(na.omit(points$MR[points$Set == j]))
          }

          for (i in Sample) {
            n <- points$n[i]

            if (disp_type == 1) {
              disp_low <- std_err * stat_r
              disp_up <- std_err * stat_r
              if (centerline_disp - disp_low < 0) disp_low <- NA
              points_2 <- c(points_2, points$range[i])
            }
            if (disp_type == 2) {
              disp_low <- std_err * stat_s
              disp_up <- std_err * stat_s
              if (centerline_disp - disp_low < 0) disp_low <- NA
              points_2 <- c(points_2, points$sd[i])
            }
            if (disp_type == 3) {
              disp_low <- std_err * stat_s2
              disp_up <- std_err * stat_s2
              if (centerline_disp - disp_low < 0) disp_low <- NA
              points_2 <- c(points_2, points$var[i])
            }
            if (disp_type == 4) {
              mr_bar <- sig_est[j] * spc.constant.calculation.d2(sample.size = span)
              disp_low <- mr_bar * spc.constant.calculation.D3(sample.size = span, n.sigma = std_err) + centerline_disp
              disp_up <- mr_bar * spc.constant.calculation.D4(sample.size = span, n.sigma = std_err) - centerline_disp
              points_2 <- c(points_2, points$MR[i])
            }

            sample2 <- c(sample2, i)
            centerline_2 <- c(centerline_2, centerline_disp)
            UCL2 <- c(UCL2, centerline_disp + disp_up)
            LCL2 <- c(LCL2, centerline_disp - disp_low)
            est_sig <- c(est_sig, sig_est[j])
            ind_sd <- c(ind_sd, st_dev_ind)
            set_sd <- c(set_sd, sd_set[j])
            ac2 <- centerline_disp + disp_up - centerline_disp
            zone_a_up_2 <- c(zone_a_up_2, centerline_disp + disp_up)
            zone_ab_up_2 <- c(zone_ab_up_2, centerline_disp + (2 / 3) * ac2)
            zone_bc_up_2 <- c(zone_bc_up_2, centerline_disp + (1 / 3) * ac2)
            if (is.na(centerline_disp - disp_low)) {
              zone_a_low_2 <- c(zone_a_low_2, 0)
              ac2_low <- centerline_disp
            } else {
              zone_a_low_2 <- c(zone_a_low_2, centerline_disp - disp_low)
              ac2_low <- disp_low
            }
            zone_ab_low_2 <- c(zone_ab_low_2, centerline_disp - (2 / 3) * ac2_low)
            zone_bc_low_2 <- c(zone_bc_low_2, centerline_disp - (1 / 3) * ac2_low)
          }
        }

        plot_data_disp <- cbind(sample2, points_2, LCL2, centerline_2, UCL2, est_sig, ind_sd, set_sd, zone_a_up_2, zone_ab_up_2, zone_bc_up_2, zone_a_low_2, zone_ab_low_2, zone_bc_low_2)
        plot_data_disp <- as.data.frame(plot_data_disp[order(sample2), ])
        if (disp_type == 4) ooc_rules <- spc.rulesets.outside.limits()
        control_vio_disp <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = points_2,
          center.line = centerline_2,
          control.limits.ucl = UCL2,
          zone.a.upper = zone_a_up_2,
          zone.ab.upper = zone_ab_up_2,
          zone.bc.upper = zone_bc_up_2,
          control.limits.lcl = LCL2,
          zone.a.lower = zone_a_low_2,
          zone.ab.lower = zone_ab_low_2,
          zone.bc.lower = zone_bc_low_2
        )
      }

      if (as.numeric(disp_lim_calc) == 9) { # known sigma disp
        for (j in set_id) {
          n <- mean(points$n[points$Set == j])
          stat <- known_sig_x

          if (x_lim_calc == 1) {
            stat_x <- mean(points$range[points$Set == j])
            disp_d_x <- spc.constant.calculation.d2(sample.size = mean(points$n[points$Set == j]))
            sig_est[j] <- stat_x / disp_d_x
          }
          if (x_lim_calc == 2) {
            stat_x <- median(points$range[points$Set == j])
            disp_d_x <- spc.constant.calculation.d4(sample.size = mean(points$n[points$Set == j]))
            sig_est[j] <- stat_x / disp_d_x
          }
          if (x_lim_calc == 3) {
            stat_x <- mean(points$sd[points$Set == j])
            disp_d_x <- spc.constant.calculation.c4(sample.size = mean(points$n[points$Set == j]))
            sig_est[j] <- stat_x / disp_d_x
          }
          if (x_lim_calc == 4) {
            stat_x <- median(points$sd[points$Set == j])
            disp_d_x <- c6(mean(points$n[points$Set == j]))
            sig_est[j] <- stat_x / disp_d_x
          }
          if (x_lim_calc == 5) {
            stat_x <- mean(points$var[points$Set == j])
            sig_est[j] <- sqrt(stat_x)
          }

          if (!isTRUE(ind_chart)) {
            if (x_lim_calc == 6) {
              sig_est[j] <- sqrt(n) * mean(abs(diff(points$mean[points$Set == j]))) / spc.constant.calculation.d2(sample.size = 2)
            }
            if (x_lim_calc == 7) {
              sig_est[j] <- sqrt(n) * median(abs(diff(points$mean[points$Set == j]))) / spc.constant.calculation.d4(sample.size = 2)
            }
            if (x_lim_calc == 8) {
              sig_est[j] <- sqrt(n) * sd(points$mean[points$Set == j])
            }
          }

          if (isTRUE(ind_chart)) {
            if (x_lim_calc == 6) sig_est[j] <- stat
            if (x_lim_calc == 7) sig_est[j] <- stat
            if (x_lim_calc == 8) sig_est[j] <- stat
          }

          if (x_lim_calc == 9) sig_est[j] <- known_sig_x
          if (x_lim_calc == 10 || x_lim_calc == 11 || x_lim_calc == 12) sig_est[j] <- NA

          if (!isTRUE(ind_chart)) Sample <- unique(points$g[points$Set == j])
          if (isTRUE(ind_chart)) Sample <- unique(points$Sample[points$Set == j])

          for (i in Sample) {
            n <- points$n[i]

            if (disp_type == 1) {
              mean_est <- stat * spc.constant.calculation.d2(sample.size = n)
              disp_low <- spc.constant.calculation.D3(sample.size = n, n.sigma = std_err) * mean_est
              disp_up <- spc.constant.calculation.D4(sample.size = n, n.sigma = std_err) * mean_est
              centerline_disp <- mean_est
              points_2 <- c(points_2, points$range[i])
            }
            if (disp_type == 2) {
              mean_est <- stat * spc.constant.calculation.c4(sample.size = n)
              disp_low <- spc.constant.calculation.B3(sample.size = n, n.sigma = std_err) * mean_est
              disp_up <- spc.constant.calculation.B4(sample.size = n, n.sigma = std_err) * mean_est
              centerline_disp <- mean_est
              points_2 <- c(points_2, points$sd[i])
            }
            if (disp_type == 3) {
              p_low <- pnorm(-std_err, 0, 1)
              p_high <- pnorm(std_err, 0, 1)
              mean_est <- sig_est[j]^2
              disp_low <- mean_est * qchisq(p = p_low, df = n - 1) / (n - 1)
              disp_up <- mean_est * qchisq(p = p_high, df = n - 1) / (n - 1)
              points_2 <- c(points_2, points$var[i])
              centerline_disp <- (sig_est[j] * spc.constant.calculation.c4(sample.size = n))^2
            }
            if (disp_type == 4) {
              mr_bar <- stat * spc.constant.calculation.d2(sample.size = span)
              disp_low <- mr_bar * spc.constant.calculation.D3(sample.size = span, n.sigma = std_err)
              disp_up <- mr_bar * spc.constant.calculation.D4(sample.size = span, n.sigma = std_err)
              points_2 <- c(points_2, points$MR[i])
              centerline_disp <- mr_bar
            }

            sample2 <- c(sample2, i)
            centerline_2 <- c(centerline_2, centerline_disp)
            UCL2 <- c(UCL2, disp_up)
            LCL2 <- c(LCL2, disp_low)
            est_sig <- c(est_sig, sig_est[j])
            ind_sd <- c(ind_sd, st_dev_ind)
            set_sd <- c(set_sd, sd_set[j])
            ac2 <- disp_up - centerline_disp
            zone_a_up_2 <- c(zone_a_up_2, disp_up)
            zone_ab_up_2 <- c(zone_ab_up_2, centerline_disp + (2 / 3) * ac2)
            zone_bc_up_2 <- c(zone_bc_up_2, centerline_disp + (1 / 3) * ac2)
            if (is.na(disp_low)) {
              zone_a_low_2 <- c(zone_a_low_2, 0)
              ac2_low <- centerline_disp
            } else {
              zone_a_low_2 <- c(zone_a_low_2, disp_low)
              ac2_low <- disp_low
            }
            zone_ab_low_2 <- c(zone_ab_low_2, centerline_disp - (2 / 3) * ac2_low)
            zone_bc_low_2 <- c(zone_bc_low_2, centerline_disp - (1 / 3) * ac2_low)
          }
        }

        plot_data_disp <- cbind(sample2, points_2, LCL2, centerline_2, UCL2, est_sig, ind_sd, set_sd, zone_a_up_2, zone_ab_up_2, zone_bc_up_2, zone_a_low_2, zone_ab_low_2, zone_bc_low_2)
        plot_data_disp <- as.data.frame(plot_data_disp[order(sample2), ])
        if (disp_type == 4) ooc_rules <- spc.rulesets.outside.limits()
        control_vio_disp <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = points_2,
          center.line = centerline_2,
          control.limits.ucl = UCL2,
          zone.a.upper = zone_a_up_2,
          zone.ab.upper = zone_ab_up_2,
          zone.bc.upper = zone_bc_up_2,
          control.limits.lcl = LCL2,
          zone.a.lower = zone_a_low_2,
          zone.ab.lower = zone_ab_low_2,
          zone.bc.lower = zone_bc_low_2
        )
      }

      if (as.numeric(disp_lim_calc) == 10 || as.numeric(disp_lim_calc) == 12) { # custom disp
        for (j in set_id) {
          n <- mean(points$n[points$Set == j])

          if (x_lim_calc == 1) {
            stat_x <- mean(points$range[points$Set == j])
            disp_d_x <- spc.constant.calculation.d2(sample.size = mean(points$n[points$Set == j]))
            sig_est[j] <- stat_x / disp_d_x
          }
          if (x_lim_calc == 2) {
            stat_x <- median(points$range[points$Set == j])
            disp_d_x <- spc.constant.calculation.d4(sample.size = mean(points$n[points$Set == j]))
            sig_est[j] <- stat_x / disp_d_x
          }
          if (x_lim_calc == 3) {
            stat_x <- mean(points$sd[points$Set == j])
            disp_d_x <- spc.constant.calculation.c4(sample.size = mean(points$n[points$Set == j]))
            sig_est[j] <- stat_x / disp_d_x
          }
          if (x_lim_calc == 4) {
            stat_x <- median(points$sd[points$Set == j])
            disp_d_x <- c6(mean(points$n[points$Set == j]))
            sig_est[j] <- stat_x / disp_d_x
          }
          if (x_lim_calc == 5) {
            stat_x <- mean(points$var[points$Set == j])
            sig_est[j] <- sqrt(stat_x)
          }
          if (!isTRUE(ind_chart)) {
            if (x_lim_calc == 6) {
              sig_est[j] <- sqrt(n) * mean(abs(diff(points$mean[points$Set == j]))) / spc.constant.calculation.d2(sample.size = 2)
            }
            if (x_lim_calc == 7) {
              sig_est[j] <- sqrt(n) * median(abs(diff(points$mean[points$Set == j]))) / spc.constant.calculation.d4(sample.size = 2)
            }
            if (x_lim_calc == 8) {
              sig_est[j] <- sqrt(n) * sd(points$mean[points$Set == j])
            }
            if (x_lim_calc == 9) {
              sig_est[j] <- known_sig_x
            }
          }
          if (isTRUE(ind_chart)) {
            if (x_lim_calc == 6) {
              sig_est[j] <- sqrt(n) * mean(points$MR[points$Set == j][!is.na(points$MR)]) / spc.constant.calculation.d2(sample.size = span)
            }
            if (x_lim_calc == 7) {
              sig_est[j] <- sqrt(n) * median(points$MR[points$Set == j][!is.na(points$MR)]) / spc.constant.calculation.d2(sample.size = span)
            }
            if (x_lim_calc == 8) {
              sig_est[j] <- sqrt(n) * sd(points$Data[points$Set == j])
            }
            if (x_lim_calc == 9) {
              sig_est[j] <- known_sig_x
            }
          }
          if (x_lim_calc == 10 || x_lim_calc == 11 || x_lim_calc == 12) {
            sig_est[j] <- NA
          }

          if (!isTRUE(ind_chart)) Sample <- unique(points$g[points$Set == j])
          if (isTRUE(ind_chart)) Sample <- unique(points$Sample[points$Set == j])

          centerline_disp <- disp_center_custom

          for (i in Sample) {
            n <- points$n[i]

            if (disp_type == 1) points_2 <- c(points_2, points$range[i])
            if (disp_type == 2) points_2 <- c(points_2, points$sd[i])
            if (disp_type == 3) points_2 <- c(points_2, points$var[i])
            if (disp_type == 4) points_2 <- c(points_2, points$MR[i])

            sample2 <- c(sample2, i)
            centerline_2 <- c(centerline_2, centerline_disp)
            UCL2 <- c(UCL2, disp_upper_custom)
            LCL2 <- c(LCL2, disp_lower_custom)
            est_sig <- c(est_sig, sig_est[j])
            ind_sd <- c(ind_sd, st_dev_ind)
            set_sd <- c(set_sd, sd_set[j])
            zone_a_up_2 <- c(zone_a_up_2, disp_upper_custom)
            zone_ab_up_2 <- c(zone_ab_up_2, centerline_disp + (2 / 3) * (disp_upper_custom - centerline_disp))
            zone_bc_up_2 <- c(zone_bc_up_2, centerline_disp + (1 / 3) * (disp_upper_custom - centerline_disp))
            if (is.na(disp_lower_custom)) disp_lower_custom <- 0
            zone_a_low_2 <- c(zone_a_low_2, disp_lower_custom)
            zone_ab_low_2 <- c(zone_ab_low_2, centerline_disp - (2 / 3) * (centerline_disp - disp_lower_custom))
            zone_bc_low_2 <- c(zone_bc_low_2, centerline_disp - (1 / 3) * (centerline_disp - disp_lower_custom))
          }
        }

        plot_data_disp <- cbind(sample2, points_2, LCL2, centerline_2, UCL2, est_sig, ind_sd, set_sd, zone_a_up_2, zone_ab_up_2, zone_bc_up_2, zone_a_low_2, zone_ab_low_2, zone_bc_low_2)
        plot_data_disp <- as.data.frame(plot_data_disp[order(sample2), ])
        if (is.na(disp_lower_custom) || disp_lower_custom < 0) {
          cl_lcl <- rep(0, k_obs)
        } else {
          cl_lcl <- plot_data_disp$LCL2
        }
        if (disp_type == 4) ooc_rules <- spc.rulesets.outside.limits()
        control_vio_disp <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = plot_data_disp$points_2,
          center.line = plot_data_disp$centerline_2,
          control.limits.ucl = plot_data_disp$UCL2,
          zone.a.upper = plot_data_disp$zone_a_up_2,
          zone.ab.upper = plot_data_disp$zone_ab_up_2,
          zone.bc.upper = plot_data_disp$zone_bc_up_2,
          control.limits.lcl = cl_lcl,
          zone.a.lower = plot_data_disp$zone_a_low_2,
          zone.ab.lower = plot_data_disp$zone_ab_low_2,
          zone.bc.lower = plot_data_disp$zone_bc_low_2
        )
      }

      # Guard if unsupported limit options are selected
      if (is.null(plot_data) || is.null(plot_data_disp) || is.null(control_vio_x) || is.null(control_vio_disp)) {
        plot_data_r(NULL)
        return(
          ggplot() +
            theme_void() +
            annotate(
              "text",
              x = 0,
              y = 0,
              label = "Selected SPC limit method(s) are not available.\nPlease choose a supported limit calculation."
            )
        )
      }

      plot_data <- c(plot_data, plot_data_disp, x = control_vio_x, disp = control_vio_disp)

      # --- Final chart assembly (ported from monolithic) ---
      x_name <- names(choice_x_spc)[as.numeric(x_type)]
      disp_name <- names(choice_disp_spc)[as.numeric(disp_type)]

      x_label <- c(rep(seq_len(k_obs), 2))
      set <- c(rep(plot_data$set, 2))
      n_sample <- c(rep(n_k, 2))
      facet <- c(rep(x_name, k_obs), rep(disp_name, k_obs))
      UCL <- c(plot_data[["UCL1"]], plot_data[["UCL2"]])
      LCL <- c(plot_data[["LCL1"]], plot_data[["LCL2"]])
      measure <- c(plot_data[["points_1"]], plot_data[["points_2"]])
      centerline <- c(plot_data[["centerline_1"]], plot_data[["centerline_2"]])
      outside <- c(plot_data[["x.rule.results"]][["outside.limits"]], plot_data[["disp.rule.results"]][["outside.limits"]])
      runs <- c(plot_data[["x.rule.results"]][["runs"]], plot_data[["disp.rule.results"]][["runs"]])
      zone_a_up <- c(plot_data[["zone_a_up_1"]], plot_data[["zone_a_up_2"]])
      zone_a_low <- c(plot_data[["zone_a_low_1"]], plot_data[["zone_a_low_2"]])
      zone_bc_up <- c(plot_data[["zone_bc_up_1"]], plot_data[["zone_bc_up_2"]])
      zone_bc_low <- c(plot_data[["zone_bc_low_1"]], plot_data[["zone_bc_low_2"]])
      zone_ab_up <- c(plot_data[["zone_ab_up_1"]], plot_data[["zone_ab_up_2"]])
      zone_ab_low <- c(plot_data[["zone_ab_low_1"]], plot_data[["zone_ab_low_2"]])
      trends <- c(plot_data[["x.rule.results"]][["trends"]], plot_data[["disp.rule.results"]][["trends"]])
      alternating <- c(plot_data[["x.rule.results"]][["alternating"]], plot_data[["disp.rule.results"]][["alternating"]])
      zone_a <- c(plot_data[["x.rule.results"]][["zone.a"]], plot_data[["disp.rule.results"]][["zone.a"]])
      consec_c <- c(plot_data[["x.rule.results"]][["consecutive.zone.c"]], plot_data[["disp.rule.results"]][["consecutive.zone.c"]])
      consec_ab <- c(plot_data[["x.rule.results"]][["consecutive.zone.ab"]], plot_data[["disp.rule.results"]][["consecutive.zone.ab"]])
      zone_a_b <- c(plot_data[["x.rule.results"]][["consecutive.zone.ab"]], plot_data[["disp.rule.results"]][["consecutive.zone.ab"]])
      est_sig <- plot_data$est_sig
      ind_s <- plot_data$ind_s
      set_sd <- plot_data$set_sd

      plot_order <- factor(facet, levels = c(x_name, disp_name))

      plot_data_r(data.frame(
        x_label,
        facet,
        n_sample,
        set,
        UCL,
        LCL,
        measure,
        est_sig,
        ind_s,
        set_sd,
        centerline,
        outside,
        runs,
        zone_bc_up,
        zone_bc_low,
        zone_ab_up,
        zone_ab_low,
        zone_a_up,
        zone_a_low,
        trends,
        alternating,
        zone_a,
        consec_c,
        consec_ab,
        zone_a_b,
        plot_order
      )[order(-xtfrm(facet), x_label), ])
      # Avoid invalidating this reactive via self-dependency
      plot_data_change(isolate(plot_data_change()) + 1)

      plot_data_p <- plot_data_r()
      ooc_x <- input$ooc_rules_x
      ooc_disp <- input$ooc_rules_disp
      if (is.null(ooc_x)) ooc_x <- c(1, 2, 3, 4)
      if (is.null(ooc_disp)) ooc_disp <- c(1)

      tryCatch({
        for (i in seq_len(k_obs)) {
          if (plot_data_p$outside[i] == TRUE && is.element(1, ooc_x)) plot_data_p$outside[i] <- plot_data_p$measure[i] else plot_data_p$outside[i] <- NA
          if (plot_data_p$runs[i] == TRUE && is.element(2, ooc_x)) plot_data_p$runs[i] <- plot_data_p$measure[i] else plot_data_p$runs[i] <- NA
          if (plot_data_p$trends[i] == TRUE && is.element(3, ooc_x)) plot_data_p$trends[i] <- plot_data_p$measure[i] else plot_data_p$trends[i] <- NA
          if (plot_data_p$alternating[i] == TRUE && is.element(4, ooc_x)) plot_data_p$alternating[i] <- plot_data_p$measure[i] else plot_data_p$alternating[i] <- NA
          if (plot_data_p$zone_a[i] == TRUE && is.element(5, ooc_x)) plot_data_p$zone_a[i] <- plot_data_p$measure[i] else plot_data_p$zone_a[i] <- NA
          if (plot_data_p$consec_c[i] == TRUE && is.element(6, ooc_x)) plot_data_p$consec_c[i] <- plot_data_p$measure[i] else plot_data_p$consec_c[i] <- NA
          if (plot_data_p$consec_ab[i] == TRUE && is.element(7, ooc_x)) plot_data_p$consec_ab[i] <- plot_data_p$measure[i] else plot_data_p$consec_ab[i] <- NA
          if (plot_data_p$zone_a_b[i] == TRUE && is.element(8, ooc_x)) plot_data_p$zone_a_b[i] <- plot_data_p$measure[i] else plot_data_p$zone_a_b[i] <- NA
        }

        for (i in seq(from = k_obs + 1, to = k_obs * 2)) {
          if (plot_data_p$outside[i] == TRUE && is.element(1, ooc_disp)) plot_data_p$outside[i] <- plot_data_p$measure[i] else plot_data_p$outside[i] <- NA
          if (plot_data_p$runs[i] == TRUE && is.element(2, ooc_disp)) plot_data_p$runs[i] <- plot_data_p$measure[i] else plot_data_p$runs[i] <- NA
          if (plot_data_p$trends[i] == TRUE && is.element(3, ooc_disp)) plot_data_p$trends[i] <- plot_data_p$measure[i] else plot_data_p$trends[i] <- NA
          if (plot_data_p$alternating[i] == TRUE && is.element(4, ooc_disp)) plot_data_p$alternating[i] <- plot_data_p$measure[i] else plot_data_p$alternating[i] <- NA
          if (plot_data_p$zone_a[i] == TRUE && is.element(5, ooc_disp)) plot_data_p$zone_a[i] <- plot_data_p$measure[i] else plot_data_p$zone_a[i] <- NA
          if (plot_data_p$consec_c[i] == TRUE && is.element(6, ooc_disp)) plot_data_p$consec_c[i] <- plot_data_p$measure[i] else plot_data_p$consec_c[i] <- NA
          if (plot_data_p$consec_ab[i] == TRUE && is.element(7, ooc_disp)) plot_data_p$consec_ab[i] <- plot_data_p$measure[i] else plot_data_p$consec_ab[i] <- NA
          if (plot_data_p$zone_a_b[i] == TRUE && is.element(8, ooc_disp)) plot_data_p$zone_a_b[i] <- plot_data_p$measure[i] else plot_data_p$zone_a_b[i] <- NA
        }
      }, error = function(e) {
        stop(e)
      })

      plot_data_p$outside <- as.numeric(plot_data_p$outside)
      plot_data_p$runs <- as.numeric(plot_data_p$runs)
      plot_data_p$trends <- as.numeric(plot_data_p$trends)
      plot_data_p$alternating <- as.numeric(plot_data_p$alternating)
      plot_data_p$zone_a <- as.numeric(plot_data_p$zone_a)
      plot_data_p$consec_c <- as.numeric(plot_data_p$consec_c)
      plot_data_p$consec_ab <- as.numeric(plot_data_p$consec_ab)
      plot_data_p$zone_a_b <- as.numeric(plot_data_p$zone_a_b)

      plot_data_p <- plot_data_p[order(-xtfrm(facet), x_label), ]

      x_chart_options <- input$x_chart_options
      if (is.null(x_chart_options)) x_chart_options <- c(1, 2, 3, 4)

      if (is.null(plot_data_p) || nrow(plot_data_p) == 0 || all(is.na(plot_data_p$measure))) {
        plot_data_r(plot_data_p)
        return(
          ggplot() +
            theme_void() +
            annotate(
              "text",
              x = 0,
              y = 0,
              label = "No valid SPC chart data to plot.\nCheck that selected columns are numeric and contain values."
            )
        )
      }

      disp_labels <- c("R", "s", "s\u00b2", "MR")
      subtitle <- paste(
        "X-bar & ", disp_labels[as.numeric(disp_type)],
        ":: Location Limits from ", names(choice_x_bar_limits2[as.numeric(x_lim_calc)]),
        ", Dispersion Limits from", names(choice_r_limits2[as.numeric(disp_lim_calc)])
      )

      diff_t <- c(0, diff(plot_data_p$set))
      set_plot <- 1
      for (i in seq_len(k_obs)) {
        if (i == 1) next
        if (diff_t[i] != 0) {
          set_plot[i] <- set_plot[i - 1] + 1
        } else {
          set_plot[i] <- set_plot[i - 1]
        }
      }
      set_plot <- c(set_plot, set_plot)

      p <- ggplot(plot_data_p, aes(x = x_label, y = measure)) +
        facet_grid(plot_order ~ ., scales = "free_y") +
        labs(x = "Samples", y = y_lab, title = "Statistical Process Control Chart", subtitle = subtitle) +
        theme_gray(base_size = font_size)

      if (is.element(9, ooc_x) || is.element(6, x_chart_options)) {
        trans <- 90
        rgb_outer <- col2rgb(color[2])
        rgb_mid <- col2rgb(color[7])
        rgb_inner <- col2rgb(color[3])

        outer_col <- rgb(red = rgb_outer[1], green = rgb_outer[2], blue = rgb_outer[3], alpha = trans, maxColorValue = 255)
        mid_col <- rgb(red = rgb_mid[1], green = rgb_mid[2], blue = rgb_mid[3], alpha = trans, maxColorValue = 255)
        inner_col <- rgb(red = rgb_inner[1], green = rgb_inner[2], blue = rgb_inner[3], alpha = trans, maxColorValue = 255)

        p <- p +
          geom_ribbon(aes(ymin = zone_a_low, ymax = zone_a_up, group = set_plot), fill = outer_col, na.rm = TRUE) +
          geom_ribbon(aes(ymin = zone_ab_low, ymax = zone_ab_up, group = set_plot), fill = mid_col, na.rm = TRUE) +
          geom_ribbon(aes(ymin = zone_bc_low, ymax = zone_bc_up, group = set_plot), fill = inner_col, na.rm = TRUE)
      }

      p <- p + geom_point(color = color[4], na.rm = TRUE)
      if (is.element(1, x_chart_options)) {
        p <- p + geom_line(aes(group = set_plot), color = color[4], na.rm = TRUE)
      }
      if (is.element(3, x_chart_options)) {
        p <- p + geom_line(aes(y = centerline, group = set_plot), color = color[3], na.rm = TRUE)
      }
      if (is.element(2, x_chart_options)) {
        p <- p +
          geom_line(aes(y = UCL, group = set_plot), color = color[2], linetype = 5, na.rm = TRUE) +
          geom_line(aes(y = LCL, group = set_plot), color = color[2], linetype = 5, na.rm = TRUE)
      }

      # OOC points/labels (ported from monolithic)
      if (is.element(5, x_chart_options)) {
        p <- p + geom_text(aes(y = outside, label = "O"), nudge_x = 0.75, color = color[2], na.rm = TRUE)
      }
      if (is.element(4, x_chart_options)) {
        p <- p + geom_point(aes(y = outside), color = color[2], shape = 8, na.rm = TRUE)
      }

      if (is.element(5, x_chart_options)) {
        p <- p + geom_text(aes(y = runs, label = "R"), nudge_x = -0.75, color = color[2], na.rm = TRUE)
      }
      if (is.element(4, x_chart_options)) {
        p <- p + geom_point(aes(y = runs), color = color[2], shape = 8, na.rm = TRUE)
      }

      if (is.element(5, x_chart_options)) {
        p <- p + geom_text(aes(y = trends, label = "T"), nudge_y = 0.75, color = color[2], na.rm = TRUE)
      }
      if (is.element(4, x_chart_options)) {
        p <- p + geom_point(aes(y = trends), color = color[2], shape = 8, na.rm = TRUE)
      }

      if (is.element(5, x_chart_options)) {
        p <- p + geom_text(aes(y = alternating, label = "A"), nudge_y = -0.75, color = color[2], na.rm = TRUE)
      }
      if (is.element(4, x_chart_options)) {
        p <- p + geom_point(aes(y = alternating), color = color[2], shape = 8, na.rm = TRUE)
      }

      if (is.element(5, x_chart_options)) {
        p <- p + geom_text(aes(y = zone_a, label = "Z-a"), nudge_y = 0.75, nudge_x = 0.75, color = color[2], na.rm = TRUE)
      }
      if (is.element(4, x_chart_options)) {
        p <- p + geom_point(aes(y = zone_a), color = color[2], shape = 8, na.rm = TRUE)
      }

      if (is.element(5, x_chart_options)) {
        p <- p + geom_text(aes(y = consec_c, label = "C-c"), nudge_y = -0.75, nudge_x = 0.75, color = color[2], na.rm = TRUE)
      }
      if (is.element(4, x_chart_options)) {
        p <- p + geom_point(aes(y = consec_c), color = color[2], shape = 8, na.rm = TRUE)
      }

      if (is.element(5, x_chart_options)) {
        p <- p + geom_text(aes(y = consec_ab, label = "C-ab"), nudge_y = 0.75, nudge_x = -0.75, color = color[2], na.rm = TRUE)
      }
      if (is.element(4, x_chart_options)) {
        p <- p + geom_point(aes(y = consec_ab), color = color[2], shape = 8, na.rm = TRUE)
      }

      if (is.element(5, x_chart_options)) {
        p <- p + geom_text(aes(y = zone_a_b, label = "Z-ab"), nudge_y = -0.75, nudge_x = -0.75, color = color[2], na.rm = TRUE)
      }
      if (is.element(4, x_chart_options)) {
        p <- p + geom_point(aes(y = zone_a_b), color = color[2], shape = 8, na.rm = TRUE)
      }

      p
    })

    output$xchartout <- renderPlot({
      p <- x_chart_out()
      p
    })

    xchartout_width <- reactive(400 * 8)
    xchartout_height <- reactive(200 * 8)
    downloadServer("xchartout", x_chart_out, height = xchartout_height, width = xchartout_width)

    output$hover_info_x <- renderUI({
      hover <- input$plot_hover
      if (is.null(hover)) return(NULL)
      dat <- plot_data_r()
      if (is.null(dat) || !is.data.frame(dat)) return(NULL)

      point <- nearPoints(dat, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)

      R <- input$spc_x_anova_decimals
      left_px <- hover$coords_css$x
      top_px <- hover$coords_css$y
      style <- paste0(
        "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
        "left:", left_px + 2, "px; top:", top_px + 2, "px;"
      )

      wellPanel(
        style = style,
        p(HTML(paste0(
          "<span style='display:block; text-transform:capitalize; text-align:center'>", point$facet, "</span>",
          "<b> Point: </b>", ro(point$x_label, R), "<br/>",
          "<b> Measure: </b>", ro(point$measure, R), "<br/>",
          if (!is.null(dat$set) && length(unique(dat$set)) > 1) paste0("<b> Set: </b>", point$set, "<br/>") else "",
          "<b> UCL: </b>", ro(point$UCL, R), "<br/>",
          "<b> Centerline: </b>", ro(point$centerline, R), "<br/>",
          "<b> LCL: </b>", ro(point$LCL, R)
        )))
      )
    })

    output$xbar_table_out <- DT::renderDataTable({
      dat <- plot_data_r()
      if (is.null(dat)) {
        x_chart_out()
        dat <- plot_data_r()
      }
      req(dat)
      R <- input$spc_x_anova_decimals
      dat <- lolcat::round.object(dat, digits = R)
      DT::datatable(data = dat, options = list(dom = "t", paging = FALSE), rownames = FALSE)
    })
    outputOptions(output, "xbar_table_out", suspendWhenHidden = FALSE)

    output$limit_analysis_x <- renderUI({
      data <- filtered_data()
      req(data)
      data_type <- input$spc_var_data_type
      UI1 <- as.numeric(input$spc_var_UI1)
      UI2 <- as.numeric(input$spc_var_UI2)
      req(UI1)

      x_type <- input$spc_var_loc_type
      disp_type <- input$spc_var_disp_type
      span <- input$spc_mr_span
      loc_upper_custom <- input$custom.x.upper
      loc_center_custom <- input$custom.x.center
      loc_lower_custom <- input$custom.x.lower
      disp_upper_custom <- input$custom.disp.upper
      disp_center_custom <- input$custom.disp.center
      disp_lower_custom <- input$custom.disp.lower
      sets <- as.numeric(input$spc_var_set)
      known_sig <- input$known_sig_x
      ind_chart <- input$spc_var_ind_or_mean

      dat <- plot_data_r()
      req(dat)
      if (isTRUE(ind_chart)) {
        if (dat$facet[1] != "X") return(NULL)
      } else {
        if (dat$facet[1] != "X-bar") return(NULL)
      }

      R <- input$spc_x_anova_decimals
      loc_center_type <- as.numeric(input$spc_x_loc)
      disp_center_type <- as.numeric(input$spc_x_disp)
      x_lim_calc <- as.numeric(input$spc_var_loc_lim)
      disp_lim_calc <- as.numeric(input$spc_var_disp_lim)
      std_err <- as.numeric(input$std_err_x)

      choice_r_limits2_local <- choice_r_limits2
      choice_x_bar_limits2_local <- choice_x_bar_limits2

      if (as.numeric(disp_type) == 2) {
        choice_r_limits2_local <- c(1:12)
        names(choice_r_limits2_local) <- c(
          "Average Range",
          "Median Range",
          "Average Standard Deviation",
          "Median Standard Deviation",
          "Average Variance",
          "Average MR of s",
          "Median MR of s",
          "Standard Deviation of s",
          "Known \u03c3",
          "Centerline Only",
          "None",
          "Custom"
        )
      }

      if (as.numeric(disp_type) == 3) {
        choice_r_limits2_local <- c(1:12)
        names(choice_r_limits2_local) <- c(
          "Average Range",
          "Median Range",
          "Average Standard Deviation",
          "Median Standard Deviation",
          "Average Variance",
          "Average MR of s\u00b2",
          "Median MR of s\u00b2",
          "Standard Deviation of s\u00b2",
          "Known \u03c3",
          "Centerline Only",
          "None",
          "Custom"
        )
      }

      if (as.numeric(disp_type) == 4) {
        choice_r_limits2_local <- c(1:12)
        names(choice_r_limits2_local) <- c(
          "Average Range",
          "Median Range",
          "Average Standard Deviation",
          "Median Standard Deviation",
          "Average Variance",
          "Average MR",
          "Median MR",
          "Standard Deviation of X",
          "Known \u03c3",
          "Centerline Only",
          "None",
          "Custom"
        )
      }

      if (as.numeric(x_type) == 2) {
        choice_x_bar_limits2_local <- c(1:12)
        names(choice_x_bar_limits2_local) <- c(
          "Average Range",
          "Median Range",
          "Average Standard Deviation",
          "Median Standard Deviation",
          "Average Variance",
          "Average Moving Range of X",
          "Median Moving Range of X",
          "Standard Deviation of X",
          "Known \u03c3",
          "Centerline Only",
          "None",
          "Custom"
        )
      }

      if (as.numeric(disp_type) == 4) {
        sigma_from <- names(choice_r_limits2_local)[disp_lim_calc]
      } else if (as.numeric(disp_lim_calc) < 6) {
        sigma_from <- names(choice_r_limits2_local)[disp_lim_calc]
      } else {
        sigma_from <- names(choice_x_bar_limits2_local)[as.numeric(x_lim_calc)]
      }

      HTML(c(
        "<h4>Limit Calculation Summary</h4>",
        "<table>",
        "<tr><td><u>", names(choice_x_spc)[as.numeric(x_type)], " Chart</u></td>",
        "<td></td></tr>",
        "<tr><td>Limits from:</td><td style='text-align:left'>", names(choice_x_bar_limits2_local[x_lim_calc]), "</td></tr>",
        "<tr><td>Centerline:</td><td style='text-align:left'>",
        if (x_lim_calc == 11) {
          "None"
        } else if (x_lim_calc == 12) {
          "Custom"
        } else {
          names(choice_x_centerline)[loc_center_type]
        },
        "</td></tr>",
        "<td><u>", names(choice_disp_spc)[as.numeric(disp_type)], " Chart</u></td>",
        "<td></td></tr>",
        "<tr><td>Limits from:</td><td style='text-align:left'>", names(choice_r_limits2_local[disp_lim_calc]), "</td></tr>",
        "<tr><td>Centerline:</td><td style='text-align:left'>",
        if (disp_lim_calc == 11) {
          "None"
        } else if (disp_lim_calc == 12) {
          "Custom"
        } else {
          names(choice_x_centerline)[disp_center_type]
        },
        "</td></tr>",
        "</table>",
        "<br/>",
        "<table>",
        "<tr><td>\u03c3 estimated from: </td><td style='text-align:left'>", sigma_from, "</td></tr>",
        "<tr><td>Standard Errors Used: </td><td style='text-align:left'>", std_err, "</td></tr>",
        "</table>"
      ))
    })

    output$capability_analysis_x <- renderUI({
      data <- filtered_data()
      req(data)
      data_type <- input$spc_var_data_type
      UI1 <- as.numeric(input$spc_var_UI1)
      UI2 <- as.numeric(input$spc_var_UI2)
      req(UI1)
      x_type <- input$spc_var_loc_type
      disp_type <- input$spc_var_disp_type
      span <- input$spc_mr_span
      loc_upper_custom <- input$custom.x.upper
      loc_center_custom <- input$custom.x.center
      loc_lower_custom <- input$custom.x.lower
      disp_upper_custom <- input$custom.disp.upper
      disp_center_custom <- input$custom.disp.center
      disp_lower_custom <- input$custom.disp.lower
      sets <- as.numeric(input$spc_var_set)
      known_sig <- input$known_sig_x
      loc_center_type <- as.numeric(input$spc_x_loc)
      disp_center_type <- as.numeric(input$spc_x_disp)
      x_lim_calc <- as.numeric(input$spc_var_loc_lim)
      disp_lim_calc <- as.numeric(input$spc_var_disp_lim)
      std_err <- as.numeric(input$std_err_x)
      ind_chart <- input$spc_var_ind_or_mean

      dat <- plot_data_r()
      req(dat)
      if (isTRUE(ind_chart)) {
        if (dat$facet[1] != "X") return(NULL)
      } else {
        if (dat$facet[1] != "X-bar") return(NULL)
      }

      R <- input$spc_x_anova_decimals
      USL <- input$spc_x_USL
      Target <- input$spc_x_target
      LSL <- input$spc_x_LSL

      x_name <- names(choice_x_spc)[as.numeric(x_type)]
      disp_name <- names(choice_disp_spc)[as.numeric(disp_type)]

      output <- NULL
      set_num <- unique(dat$set)
      n_tot <- sum(dat$n_sample[dat$facet == x_name])

      header <- paste(
        "<h4>Chart Statistics</h4><u><b>All Data</b></u><br/>",
        withMathJax("$s_{individuals}=$"), ro(dat$ind_s[1], R),
        "<br/>Total n = ", n_tot, "<br/><br/>"
      )

      # Rebuild individual data for out-of-spec counts
      if (is.null(sets) || length(sets) == 0 || all(is.na(sets))) sets <- 0
      if (!is.null(sets) && !is.na(sets) && sets > ncol(data)) sets <- 0

      if (!isTRUE(ind_chart)) {
        if (identical(as.numeric(data_type), 1)) {
          req(length(UI1) > 1)
          k_obs <- nrow(data)
          dep <- lolcat::transform.dependent.format.to.independent.format(data = data[UI1])
          if (sets == 0) {
            d <- cbind("Data" = dep, "Sample" = seq_len(k_obs))
            names(d) <- c("Name", "Data", "Sample")
          } else {
            d <- cbind("Data" = dep, "Sample" = seq_len(k_obs), "Sets" = sets)
            names(d) <- c("Name", "Data", "Sample", "Sets")
          }
        } else {
          req(UI2)
          if (any(is.na(UI2)) || any(UI2 < 1) || any(UI2 > ncol(data))) return(NULL)
          if (sets == 0) {
            d <- cbind("Data" = data[UI2], "Sample" = data[UI1])
            names(d) <- c("Data", "Sample")
          } else {
            d <- cbind("Data" = data[UI2], "Sample" = data[UI1], "Sets" = data[sets])
            names(d) <- c("Data", "Sample", "Sets")
          }
        }
      } else {
        k_obs <- nrow(data)
        if (sets == 0) {
          d <- cbind("Data" = data[UI1], "Sample" = seq_len(k_obs))
          names(d) <- c("Data", "Sample")
        } else {
          d <- cbind("Data" = data[UI1], "Sample" = seq_len(k_obs), "Sets" = data[sets])
          names(d) <- c("Data", "Sample", "Sets")
        }
      }

      d <- d[order(d$Sample), ]
      if (is.data.frame(d$Data)) d$Data <- d$Data[[1]]
      if (is.factor(d$Data)) d$Data <- as.character(d$Data)
      d$Data <- suppressWarnings(as.numeric(d$Data))
      if (!is.null(d$Sets)) {
        if (is.data.frame(d$Sets)) d$Sets <- d$Sets[[1]]
        if (is.factor(d$Sets)) d$Sets <- as.character(d$Sets)
        d$Sets <- suppressWarnings(as.numeric(d$Sets))
      }

      if (sets == 0) {
        d$Sets <- 1
      }

      above_USL <- below_LSL <- numeric(0)
      if (!is.na(USL) || !is.na(Target) || !is.na(LSL)) {
        for (i in unique(d$Sets)) {
          above_USL[i] <- sum(d$Data[d$Sets == i] > USL)
          below_LSL[i] <- sum(d$Data[d$Sets == i] < LSL)
        }
      }

      for (i in set_num) {
        corr <- cor.test(
          x = dat$measure[dat$facet == x_name & dat$set == i],
          y = dat$measure[dat$facet == disp_name & dat$set == i],
          method = "pearson"
        )
        corr_r <- corr[["estimate"]][["cor"]]
        corr_p <- corr[["p.value"]]

        if (as.numeric(x_type) == 2) {
          acf <- stats::acf(dat$measure[dat$facet == x_name & dat$set == i], plot = FALSE)
          crit_acf <- qnorm((1 + 0.95) / 2) / sqrt(acf[["n.used"]])
          acf_up <- acf$acf[acf$acf > crit_acf][-1]
          acf_lag_up <- acf$lag[acf$acf > crit_acf][-1]
          acf_low <- acf$acf[acf$acf < -crit_acf]
          acf_lag_low <- acf$lag[acf$acf < -crit_acf]
          up_text <- ""
          down_text <- ""
        }

        output <- paste(
          output,
          "<table>",
          "<tr><th style='text-align:left'>Set ", i, "</th></tr>",
          "<tr><td>Centerline = ", ro(dat$centerline[which(dat$set == i & dat$facet == x_name)][1], R), "</td></tr>",
          "<tr><td>", withMathJax("$s_{set}=$"), ro(dat$set_sd[which(dat$set == i & dat$facet == x_name)][1], R), "</td></tr>",
          "<tr><td>", withMathJax("$\\hat{\\sigma}_{est}=$"), ro(dat$est_sig[which(dat$set == i & dat$facet == x_name)][1], R), "</td></tr></table>",
          "<br/>",
          "<u>", x_name, " and ", disp_name, " correlation</u>",
          "<table><tr><td>r = ", ro(corr_r, R), "</td><td>p = ", ro(corr_p, R), "</td></tr>",
          "</table><br/>",
          if (as.numeric(x_type) == 2) {
            if ((length(acf_up) + length(acf_low)) > 0) {
              paste(
                "<u>Autocorrelation</u>",
                "<table><tr><td>ACF greater than ", withMathJax("$ACF_{crit}=$"), ro(crit_acf, R), "</td><td>Confidence = 0.95</td></tr>",
                for (a in seq_len(length(acf_up))) {
                  up_text <- paste(up_text, "<tr><td>Lag = ", acf_lag_up[a], "</td><td>ACF =", ro(acf_up[a], R), "</td></tr>")
                },
                up_text,
                "</tr><tr><td>ACF less than ", withMathJax("$ACF_{crit}=$"), ro(-crit_acf, R), "</td><td>Confidence = 0.95</td></tr>",
                for (a in seq_len(length(acf_low))) {
                  down_text <- paste(down_text, "<tr><td>Lag = ", acf_lag_low[a], "</td><td>ACF =", ro(acf_low[a], R), "</td></tr>")
                },
                down_text,
                "</table><br/>"
              )
            } else {
              paste(
                "<u>Autocorrelation</u>",
                "<table><tr><td>ACF greater than ", withMathJax("$ACF_{crit}=$"), ro(crit_acf, R), "</td><td>Confidence = 0.95</td></tr>",
                "<tr><td>No significant autocorrelation found</td></tr></table><br/>"
              )
            }
          } else {
            paste("")
          }
        )

        if (!is.na(USL) || !is.na(Target) || !is.na(LSL)) {
          n_set <- sum(dat$n_sample[dat$facet == x_name & dat$set == i])
          cap <- spc.capability.summary.normal.simple(
            stat.lsl = LSL,
            stat.target = Target,
            stat.usl = USL,
            process.center = dat$centerline[dat$set == i][1],
            process.variability.estimate = dat$est_sig[dat$set == i][1]^2,
            process.variability.overall = dat$set_sd[dat$set == i][1]^2,
            process.n.upper = above_USL[i],
            process.n.lower = below_LSL[i],
            process.n = n_set
          )

          pct_off_target <- abs(100 * (dat$centerline[dat$facet == x_name & dat$set == i][1] - Target) / (USL - LSL))

          output <- paste(
            output,
            "<u>Capability Measures (Using ", withMathJax("$\\hat{\\sigma}_{est}$"), ")</u>",
            "<table>",
            "<tr><td>C<sub>p</sub> = </td><td style='text-align:left'>", ro(cap$value[1], R), "</td></tr>",
            "<tr><td>C<sub>pk</sub> = </td><td style='text-align:left'>", ro(cap$value[2], R), "</td></tr>",
            "<tr><td>C<sub>pm</sub> = </td><td style='text-align:left'>", ro(cap$value[3], R), "</td></tr>",
            "</table>",
            "<br/>",
            "<u>Potential Estimated Parts per Million (assuming normality of the individuals)</u>",
            "<table>",
            "<tr><td>Above Upper Spec = </td><td style='text-align:left'>", ro(cap$value[11], R), "</td></tr>",
            "<tr><td>Below Lower Spec </td><td style='text-align:left'>", ro(cap$value[10], R), "</td></tr>",
            "<tr><td>Total Out of Spec = </td><td style='text-align:left'>", ro(cap$value[12], R), "</td></tr>",
            "</table>",
            "<br/>",
            "<u>Performance Measures (Using observed s)</u>",
            "<table>",
            "<tr><td>P<sub>p</sub> = </td><td style='text-align:left'>", ro(cap$value[4], R), "</td></tr>",
            "<tr><td>P<sub>pk</sub> = </td><td style='text-align:left'>", ro(cap$value[5], R), "</td></tr>",
            "<tr><td>P<sub>pm</sub> = </td><td style='text-align:left'>", ro(cap$value[6], R), "</td></tr>",
            "<tr><td>% Off-Target = </td><td style='text-align:left'>", ro(pct_off_target, R), "% of spec width</td></tr>",
            "</table>",
            "<br/>",
            "<u>Actual Observed Parts per Million Nonconforming</u>",
            "<table>",
            "<tr><th></th><th>Count</th><th>ppm</th></tr>",
            "<tr><td>&gt;USL = </td><td>", cap$n[8], "</td><td>", ro(cap$value[8], R), "</td></tr>",
            "<tr><td>&lt;LSL = </td><td>", cap$n[7], "</td><td>", ro(cap$value[7], R), "</td></tr>",
            "<tr><td>Total Out of Spec = </td><td>", cap$n[9], "</td><td>", ro(cap$value[9], R), "</td></tr>",
            "</table><br/>"
          )
        }
      }

      HTML(c(header, output))
    })

    # -------------------------------------------------------------------------
    # Chart options: OOC rules UI (ported from monolithic)
    # -------------------------------------------------------------------------
    output$ooc_rules_x_ui <- renderUI({
      x_lim_calc <- as.numeric(input$spc_var_loc_lim)
      if (is.na(x_lim_calc)) return(NULL)

      if (x_lim_calc < 10 || x_lim_calc == 12) {
        return(pickerInput(
          inputId = ns("ooc_rules_x"),
          label = "Out-of-Control Rules - Location",
          selected = c(1, 2, 3, 4),
          multiple = TRUE,
          choices = list(
            Basic = c("Points Outside Limits" = 1, "Runs" = 2, "Trend of 6" = 3, "14 Alternating Values" = 4),
            Zones = c(
              "2 out of 3 in Outer Third" = 5,
              "15 in Inner Third" = 6,
              "4 out of 5 in Outer Two-thirds" = 7,
              "8 in Outer Two-thirds" = 8
            )
          )
        ))
      }
      if (x_lim_calc == 10) {
        return(pickerInput(
          inputId = ns("ooc_rules_x"),
          label = "Out-of-Control Rules - Location",
          selected = c(2),
          multiple = TRUE,
          choices = list(Basic = c("Runs" = 2))
        ))
      }
      NULL
    })

    output$ooc_rules_disp_ui <- renderUI({
      disp_lim_calc <- as.numeric(input$spc_var_disp_lim)
      x_type <- as.numeric(input$spc_var_loc_type)
      if (is.na(disp_lim_calc) || is.na(x_type)) return(NULL)

      if (x_type == 2 && (disp_lim_calc == 6 || disp_lim_calc == 7)) {
        return(pickerInput(
          inputId = ns("ooc_rules_disp"),
          label = "Out-of-Control Rules - Dispersion",
          selected = c(1),
          multiple = TRUE,
          choices = list(Basic = c("Points Outside Limits" = 1))
        ))
      }

      if (disp_lim_calc < 10 || disp_lim_calc == 12) {
        return(pickerInput(
          inputId = ns("ooc_rules_disp"),
          label = "Out-of-Control Rules - Dispersion",
          selected = c(1),
          multiple = TRUE,
          choices = list(
            Basic = c("Points Outside Limits" = 1, "Runs" = 2, "Trend of 6" = 3, "14 Alternating Values" = 4),
            Zones = c(
              "2 out of 3 in Outer Third" = 5,
              "15 in Inner Third" = 6,
              "4 out of 5 in Outer Two-thirds" = 7,
              "8 in Outer Two-thirds" = 8
            )
          )
        ))
      }

      if (disp_lim_calc == 10) {
        return(pickerInput(
          inputId = ns("ooc_rules_disp"),
          label = "Out-of-Control Rules - Dispersion",
          selected = c(2),
          multiple = TRUE,
          choices = list(Basic = c("Runs" = 2))
        ))
      }

      NULL
    })

    output$spc_analysis <- renderUI({
      if (isTRUE(input$spc_runchart)) return(NULL)
      prettySwitch(
        inputId = ns("x_spc_show_anova"),
        label = "Show Analysis",
        value = FALSE,
        status = "success",
        fill = TRUE
      )
    })

    output$spc_anova <- renderUI({
      data <- filtered_data()
      req(data)

      data_type <- input$spc_var_data_type
      UI1 <- as.numeric(input$spc_var_UI1)
      UI2 <- as.numeric(input$spc_var_UI2)
      sets <- as.numeric(input$spc_var_set)
      req(data_type, UI1)

      if (as.numeric(input$spc_var_loc_type) == 2) {
        return(p("Cannot perform ANOVA on individual data."))
      }

      # Change row data to column (match monolithic)
      if (as.numeric(data_type) == 1) {
        req(length(UI1) > 1)
        k_obs <- nrow(data)
        dep <- lolcat::transform.dependent.format.to.independent.format(data = data[UI1])
        if (sets == 0) {
          d <- cbind("Data" = dep, "Sample" = seq_len(k_obs))
          names(d) <- c("Name", "Data", "Sample")
        } else {
          d <- cbind("Data" = dep, "Sample" = seq_len(k_obs), "Sets" = sets)
          names(d) <- c("Name", "Data", "Sample", "Sets")
        }
      } else {
        req(UI2)
        if (sets == 0) {
          d <- cbind("Data" = data[UI2], "Sample" = data[UI1])
          names(d) <- c("Data", "Sample")
        } else {
          d <- cbind("Data" = data[UI2], "Sample" = data[UI1], "Sets" = data[sets])
          names(d) <- c("Data", "Sample", "Sets")
        }
      }

      conf <- input$spc_x_anova_conf
      R <- input$spc_x_anova_decimals
      req(conf, R)

      if (sets == 0) {
        oneway <- aov(formula = Data ~ as.factor(Sample), data = d)
        sum_aov <- summary(oneway)

        table_aov <- as.data.frame(table(d$Sample))
        table_aov <- cbind(table_aov, table_aov[2]^2)
        J <- nrow(table_aov)
        sum_n <- colSums(table_aov[2])
        sum_nsq <- colSums(table_aov[3])
        K_prime <- (1 / (J - 1)) * (sum_n - (sum_nsq / sum_n))

        mse <- sum_aov[[1]][["Mean Sq"]][1]
        msw <- sum_aov[[1]][["Mean Sq"]][2]
        bcv <- (mse - msw) / K_prime
        bcv <- max(0, bcv)
        ICC <- 100 * bcv / (bcv + msw)

        return(HTML(c(
          "<u>Model : ", names(oneway$model)[1], " by ", gsub(pattern = "as\\.factor\\(|\\)", replacement = "", names(oneway$model)[2]),
          "</u></br></br>",
          "<table><tr><th  style='padding: 2px 15px !important;'>Source</th><th  style='padding: 2px 15px !important;'>df</th><th  style='padding: 2px 15px !important;'>SS</th><th  style='padding: 2px 15px !important;'>MS</th><th  style='padding: 2px 15px !important;'>F</th><th  style='padding: 2px 15px !important;'>p</th></tr>",
          "<tr><td  style='padding: 2px 15px !important;'>", "Between Samples", "</td>",
          "<td  style='padding: 2px 15px !important;'>", sum_aov[[1]][["Df"]][1], "</td><td  style='padding: 2px 15px !important;'>", ro(sum_aov[[1]][["Sum Sq"]][1], R), "</td><td  style='padding: 2px 15px !important;'>", ro(sum_aov[[1]][["Mean Sq"]][1], R), "</td><td  style='padding: 2px 15px !important;'>", ro(sum_aov[[1]][["F value"]][1], R), "</td><td  style='padding: 2px 15px !important;'>", ro(sum_aov[[1]][["Pr(>F)"]][1], R), if (sum_aov[[1]][["Pr(>F)"]][1] < (1 - conf)) "*" else "", "</tr>",
          "<tr><td  style='padding: 2px 15px !important;'> Within Samples</td>", "<td  style='padding: 2px 15px !important;'>", sum_aov[[1]][["Df"]][2], "</td><td  style='padding: 2px 15px !important;'>", ro(sum_aov[[1]][["Sum Sq"]][2], R), "</td><td  style='padding: 2px 15px !important;'>", ro(sum_aov[[1]][["Mean Sq"]][2], R), "</td></tr>",
          "<tr><td  style='padding: 2px 15px !important;'> Total</td>", "<td  style='padding: 2px 15px !important;'>", sum_aov[[1]][["Df"]][1] + sum_aov[[1]][["Df"]][2], "</td><td  style='padding: 2px 15px !important;'>", ro(sum_aov[[1]][["Sum Sq"]][1] + sum_aov[[1]][["Sum Sq"]][2], R), "</td></tr>",
          "</table></br></br>",
          paste(
            "<table><tr><td>Random Effect Importance:</td></tr>",
            "<tr><td>Sample-to-Sample Variance =</td><td>", ro(bcv, R), "</td><td>", withMathJax("$\\hat{\\sigma}_{treat}=$"), ro(bcv^.5, R), "</tr>",
            "<tr><td>Within Sample Variance =</td><td>", ro(msw, R), "</td><td>", withMathJax("$\\hat{\\sigma}_{within}=$"), ro(msw^.5, R), "</tr>",
            "<tr><td>Total Variance =</td><td>", ro(bcv + msw, R), "</td><td>", withMathJax("$\\hat{\\sigma}_{total}=$"), ro((bcv + msw)^.5, R), "</tr>",
            "<tr><td><b>Intraclass Correlation =</b></td><td><b>", ro(ICC, R), "%</b></td>", "</tr>",
            "</table><br/>"
          )
        )))
      }

      # Sets case (ported at a high level): show first set only for now; full per-set output is implemented later
      HTML("ANOVA for Sets is pending full SPC implementation.")
    })

    # -------------------------------------------------------------------------
    # Attributes tab UI outputs
    # -------------------------------------------------------------------------
    output$spc_att_UI1 <- renderUI({
      req(filtered_data())
      choices <- seq(1:ncol(filtered_data()))
      names(choices) <- names(filtered_data())
      data_type <- input$spc_att_data_type

      if (data_type == 1) {
        return(pickerInput(
          inputId = ns("spc_att_UI1"),
          label = "Select Count Column",
          multiple = FALSE,
          options = list(`actions-box` = TRUE),
          choices = choices
        ))
      }
      if (data_type == 2) {
        return(pickerInput(
          inputId = ns("spc_att_UI1"),
          label = "Select Count Column",
          multiple = FALSE,
          options = list(`actions-box` = TRUE),
          choices = choices
        ))
      }
      NULL
    })

    output$spc_att_UI2 <- renderUI({
      req(filtered_data())
      choices <- seq(1:ncol(filtered_data()))
      names(choices) <- names(filtered_data())
      data_type <- input$spc_att_data_type
      UI1 <- input$spc_att_UI1
      req(UI1)
      const_n <- input$att_const_n

      if (isTRUE(const_n)) {
        return(numericInput(inputId = ns("spc_att_UI2"), label = "Sample Size", value = 1, min = 1, step = 1))
      }

      if (data_type == 1) {
        fact_selected <- as.numeric(unlist(strsplit(x = UI1, split = "\\s+")))
        temp <- seq(1:length(choices))
        temp <- temp[-fact_selected]
        choices <- choices[temp]
        return(pickerInput(
          inputId = ns("spc_att_UI2"),
          label = "Select Sample Size Column",
          multiple = FALSE,
          options = list(`actions-box` = TRUE),
          choices = choices
        ))
      }
      if (data_type == 2) {
        fact_selected <- as.numeric(unlist(strsplit(x = UI1, split = "\\s+")))
        temp <- seq(1:length(choices))
        temp <- temp[-fact_selected]
        choices <- choices[temp]
        return(pickerInput(
          inputId = ns("spc_att_UI2"),
          label = "Select Sample Column",
          multiple = FALSE,
          options = list(`actions-box` = TRUE),
          choices = choices
        ))
      }
      NULL
    })

    output$spc_att_set <- renderUI({
      req(filtered_data())
      choices <- seq(1:ncol(filtered_data()))
      names(choices) <- names(filtered_data())
      UI1 <- as.numeric(input$spc_att_UI1)
      UI2 <- as.numeric(input$spc_att_UI2)
      req(UI1)
      const_n <- input$att_const_n

      if (isTRUE(const_n)) {
        fact_selected <- c(UI1)
        temp <- seq(1:length(choices))
        temp <- temp[-fact_selected]
        choices <- c("None" = 0, choices[temp])
        return(pickerInput(
          inputId = ns("spc_att_set"),
          label = "Select Set Column",
          multiple = FALSE,
          options = list(`actions-box` = TRUE),
          choices = choices
        ))
      }

      if (is.null(input$spc_att_UI2) || is.na(input$spc_att_UI2)) return(NULL)
      fact_selected <- c(UI1, UI2)
      temp <- seq(1:length(choices))
      temp <- temp[-fact_selected]
      choices <- c("None" = 0, choices[temp])
      pickerInput(
        inputId = ns("spc_att_set"),
        label = "Select Set Column",
        multiple = FALSE,
        options = list(`actions-box` = TRUE),
        choices = choices
      )
    })

    output$spc_att_loc_type <- renderUI({
      selectInput(inputId = ns("spc_att_loc_type"), label = "Chart Type", choices = choice_att_charts)
    })

    output$spc_att_loc_lim <- renderUI({
      chart <- as.numeric(input$spc_att_loc_type)
      req(chart)

      if (chart == 1 || chart == 2) {
        return(selectInput(
          inputId = ns("spc_att_loc_lim"),
          label = "Limit Calculation",
          choices = choice_att_b_limits
        ))
      }
      if (chart == 3 || chart == 4) {
        return(selectInput(
          inputId = ns("spc_att_loc_lim"),
          label = "Limit Calculation",
          choices = choice_att_p_limits
        ))
      }
      NULL
    })

    output$ooc_rules_att_ui <- renderUI({
      att_lim_calc <- as.numeric(input$spc_att_loc_lim)
      req(att_lim_calc)

      if (att_lim_calc < 7) {
        return(pickerInput(
          inputId = ns("ooc_rules_att"),
          label = "Out-of-Control Rules",
          selected = c(1, 2, 3, 4),
          multiple = TRUE,
          choices = list(
            Basic = c("Points Outside Limits" = 1, "Runs" = 2, "Trend of 6" = 3, "14 Alternating Values" = 4),
            Zones = c(
              "2 out of 3 in Outer Third" = 5,
              "15 in Inner Third" = 6,
              "4 out of 5 in Outer Two-thirds" = 7,
              "8 in Outer Two-thirds" = 8
            )
          )
        ))
      }
      if (att_lim_calc == 7) {
        return(pickerInput(
          inputId = ns("ooc_rules_att"),
          label = "Out-of-Control Rules",
          selected = c(2),
          multiple = TRUE,
          choices = list(Basic = c("Runs" = 2))
        ))
      }
      NULL
    })

    output$known_param_att <- renderUI({
      chart <- as.numeric(input$spc_att_loc_type)
      req(chart)
      loc_type <- input$spc_att_loc
      req(loc_type)

      if (loc_type != 3) return(NULL)

      if (chart == 1) {
        return(numericInput(inputId = ns("known_param"), label = "Enter \u03c0", value = NA, min = 0, max = 1))
      }
      if (chart == 2) {
        return(numericInput(inputId = ns("known_param"), label = "Enter n\u03c0", value = NA, min = 0, max = 1))
      }
      if (chart == 3) {
        return(numericInput(inputId = ns("known_param"), label = "Enter \u03bb", value = NA, min = 0))
      }
      if (chart == 4) {
        return(numericInput(inputId = ns("known_param"), label = "Enter \u03bb/n", value = NA, min = 0))
      }
      NULL
    })

    output$spc_att_loc <- renderUI({
      chart_type <- input$spc_att_loc_type
      req(chart_type)

      if (chart_type == 1) {
        choice_att_centerline <- (1:3)
        names(choice_att_centerline) <- c("Mean", "Median", "Known \u03c0")
        return(selectInput(inputId = ns("spc_att_loc"), label = "Centerline", choices = choice_att_centerline))
      }
      if (chart_type == 2) {
        choice_att_centerline <- (1:3)
        names(choice_att_centerline) <- c("Mean", "Median", "Known n\u03c0")
        return(selectInput(inputId = ns("spc_att_loc"), label = "Centerline", choices = choice_att_centerline))
      }
      if (chart_type == 3) {
        choice_att_centerline <- (1:3)
        names(choice_att_centerline) <- c("Mean", "Median", "Known \u03bb")
        return(selectInput(inputId = ns("spc_att_loc"), label = "Centerline", choices = choice_att_centerline))
      }
      if (chart_type == 4) {
        choice_att_centerline <- (1:3)
        names(choice_att_centerline) <- c("Mean", "Median", "Known \u03bb/n")
        return(selectInput(inputId = ns("spc_att_loc"), label = "Centerline", choices = choice_att_centerline))
      }
      NULL
    })

    # -------------------------------------------------------------------------
    # Attributes chart: plot + plot data + hover
    # -------------------------------------------------------------------------
    att_plot_data_r <- reactiveVal(NULL)
    plot_data_change_att <- reactiveVal(0)

    output$attchartout <- renderPlot({
      att_chart_out()
    })

    attchartout_width <- reactive(400 * 8)
    attchartout_height <- reactive(400 * 4)
    downloadServer("attchartout", att_chart_out, width = attchartout_width, height = attchartout_height)

    att_chart_out <- reactive({ # start attribute plot
      data <- filtered_data()
      req(data)

      color <- reactive_color_palette()
      if (is.null(color) || length(color) < 4) color <- palette.colors(8)

      data_type <- input$spc_att_data_type
      UI1 <- as.numeric(input$spc_att_UI1) # column number of counts
      UI2 <- as.numeric(input$spc_att_UI2) # sample size column or constant size
      req(UI1)
      req(UI2)
      att_type <- input$spc_att_loc_type
      req(att_type)
      att_lim_calc <- input$spc_att_loc_lim
      req(att_lim_calc)
      loc_upper_custom <- input$custom.att.upper
      loc_center_custom <- input$custom.att.center
      loc_lower_custom <- input$custom.att.lower
      std_err <- as.numeric(input$std_err_att)
      req(std_err)
      known_param <- input$known_param
      loc_center_type <- input$spc_att_loc
      req(loc_center_type)
      const_n <- input$att_const_n
      font_size <- input$att_spc_font_size

      sets <- as.numeric(input$spc_att_set)
      req(sets)

      if (loc_center_type == 3) {
        req(known_param)
      }

      # form data
      if (data_type == 1) { # column of count, column of sample size OR constant sample size
        k_obs <- nrow(data[UI1])
        Sample <- seq(1, k_obs)
        if (sets == 0) {
          set <- rep(1, k_obs)
        } else {
          set <- data[sets]
        }
        if (isTRUE(const_n)) {
          plot_data <- cbind("Count" = data[UI1], "n" = rep(UI2, k_obs), "Sets" = set)
        } else {
          plot_data <- cbind("Count" = data[UI1], "n" = data[UI2], "Sets" = set)
        }
        if (att_type == 1) {
          plot_data <- cbind(Sample, plot_data, "p" = plot_data[1] / plot_data[2])
          names(plot_data) <- c("Sample", "Count", "n", "Sets", "p")
        }
        if (att_type == 2) {
          plot_data <- cbind(Sample, plot_data, "np" = plot_data[1])
          names(plot_data) <- c("Sample", "Count", "n", "Sets", "np")
        }
        if (att_type == 3) {
          plot_data <- cbind(Sample, plot_data, "c" = plot_data[1])
          names(plot_data) <- c("Sample", "Count", "n", "Sets", "c")
        }
        if (att_type == 4) {
          plot_data <- cbind(Sample, plot_data, "u" = plot_data[1] / plot_data[2])
          names(plot_data) <- c("Sample", "Count", "n", "Sets", "u")
        }
      } else {
        # column of count, column of observation, column of n OR constant n (not used in monolithic)
      }

      # data are now a column of Count, n, Sets, and p
      y_lab <- names(data[UI1])
      plot_data <- plot_data[order(plot_data$Sample), ] # reorder by sample if needed
      set_id <- unique(plot_data$Sets)

      # set up stats for alternate limits calculations
      set_sd_att <- NULL
      set_MR_att <- NULL

      for (i in set_id) {
        set_sd_att[i] <- sd(plot_data[[5]])
      }

      # calculate limits and check ooc
      run_length <- as.numeric(input$run_length_att)
      ooc_rules <- spc.rulesets.nelson.1984.test.1.2.3.4.5.6.7.8()
      ooc_rules$runs <- spc.controlviolation.nelson.1984.test2.runs.create(point.count = run_length)
      total_area <- pnorm(q = std_err, mean = 0, sd = 1) - (1 - pnorm(q = std_err, mean = 0, sd = 1))

      # set up vectors
      points <- NULL
      centerline <- NULL
      UCL <- NULL
      zone_a_up <- NULL
      zone_ab_up <- NULL
      zone_bc_up <- NULL
      LCL <- NULL
      zone_a_low <- NULL
      zone_ab_low <- NULL
      zone_bc_low <- NULL

      order <- NULL
      est_sig <- NULL

      if (att_lim_calc == 1) { # exact either binomial or Poisson
        for (j in set_id) {
          if (loc_center_type == 1) { # mean
            if (att_type == 1 || att_type == 4) { # p or u centerlines
              center <- sum(plot_data$Count[which(plot_data$Sets == j)]) / sum(plot_data$n[which(plot_data$Sets == j)])
            } else {
              n_bar_set <- mean(plot_data$n[which(plot_data$Sets == j)])
              center_set <- n_bar_set * sum(plot_data$Count[which(plot_data$Sets == j)]) / sum(plot_data$n[which(plot_data$Sets == j)])
            }
          } else if (loc_center_type == 2) { # median
            center <- median(plot_data[5][which(plot_data$Sets == j), ])
            center_set <- median(plot_data[5][which(plot_data$Sets == j), ])
          } else if (loc_center_type == 3) { # known parameter
            center <- known_param
            center_set <- known_param
            n_bar_set <- mean(plot_data$n[which(plot_data$Sets == j)])
          }
          set_obs <- unique(plot_data$Sample[plot_data$Sets == j])
          for (i in set_obs) {
            if (att_type == 2 || att_type == 3) {
              center <- center_set * plot_data$n[i] / n_bar_set
            }

            if (att_type == 1 || att_type == 2) { # p chart or np
              if (att_type == 1) {
                n_upper <- qbinom(p = (1 - total_area) / 2, size = plot_data$n[i], prob = center, lower.tail = FALSE) + 0.5
                n_lower <- qbinom(p = (1 - total_area) / 2, size = plot_data$n[i], prob = center, lower.tail = TRUE) - 0.5
                UCL_t <- n_upper / plot_data$n[i]
                LCL_t <- n_lower / plot_data$n[i]
              } else {
                n_upper <- qbinom(p = (1 - total_area) / 2, size = plot_data$n[i], prob = center_set / n_bar_set, lower.tail = FALSE) + 0.5
                n_lower <- qbinom(p = (1 - total_area) / 2, size = plot_data$n[i], prob = center_set / n_bar_set, lower.tail = TRUE) - 0.5
                UCL_t <- n_upper
                LCL_t <- n_lower
              }
              if (LCL_t < 0) LCL_t <- NA
            } else {
              if (att_type == 3 || att_type == 4) {
                if (att_type == 4) {
                  n_upper <- qpois(p = (1 - total_area) / 2, lambda = center * plot_data$n[i], lower.tail = FALSE) + 0.5
                  n_lower <- qpois(p = (1 - total_area) / 2, lambda = center * plot_data$n[i], lower.tail = TRUE) - 0.5
                  UCL_t <- n_upper / plot_data$n[i]
                  LCL_t <- n_lower / plot_data$n[i]
                } else {
                  n_upper <- qpois(p = (1 - total_area) / 2, lambda = center_set * (plot_data$n[i] / n_bar_set), lower.tail = FALSE) + 0.5
                  n_lower <- qpois(p = (1 - total_area) / 2, lambda = center_set * (plot_data$n[i] / n_bar_set), lower.tail = TRUE) - 0.5
                  UCL_t <- n_upper
                  LCL_t <- n_lower
                }
                if (LCL_t < 0) LCL_t <- NA
              }
            }
            order <- c(order, i)
            UCL <- c(UCL, UCL_t)
            LCL <- c(LCL, LCL_t)
            centerline <- c(centerline, center)
            zone_a_up <- c(zone_a_up, UCL_t)
            ac_up <- UCL_t - center
            zone_ab_up <- c(zone_ab_up, center + (2 / 3) * ac_up)
            zone_bc_up <- c(zone_bc_up, center + (1 / 3) * ac_up)
            if (is.na(LCL_t)) LCL_t <- 0
            ac_low <- center - LCL_t
            zone_a_low <- c(zone_a_low, LCL_t)
            zone_ab_low <- c(zone_ab_low, center - (2 / 3) * ac_low)
            zone_bc_low <- c(zone_bc_low, center - (1 / 3) * ac_low)
          }
        }

        lim_dat <- cbind(order, LCL, centerline, UCL, zone_a_up, zone_ab_up, zone_bc_up, zone_a_low, zone_ab_low, zone_bc_low)[order(order), ]
        plot_data <- cbind(plot_data, lim_dat)[-6]
        plot_data <- as.data.frame(plot_data[order(Sample), ])

        control_vio_att <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = plot_data[[5]],
          center.line = plot_data$centerline,
          control.limits.ucl = plot_data$UCL,
          zone.a.upper = plot_data$zone_a_up,
          zone.ab.upper = plot_data$zone_ab_up,
          zone.bc.upper = plot_data$zone_bc_up,
          control.limits.lcl = plot_data$LCL,
          zone.a.lower = plot_data$zone_a_low,
          zone.ab.lower = plot_data$zone_ab_low,
          zone.bc.lower = plot_data$zone_bc_low
        )
      }

      if (att_lim_calc == 2) { # normal approximation
        for (j in set_id) {
          if (loc_center_type == 1) {
            if (att_type == 1 || att_type == 4) {
              center <- sum(plot_data$Count[which(plot_data$Sets == j)]) / sum(plot_data$n[which(plot_data$Sets == j)])
            } else {
              n_bar_set <- mean(plot_data$n[which(plot_data$Sets == j)])
              center_set <- n_bar_set * sum(plot_data$Count[which(plot_data$Sets == j)]) / sum(plot_data$n[which(plot_data$Sets == j)])
            }
          } else if (loc_center_type == 2) {
            center <- median(plot_data[5][which(plot_data$Sets == j), ])
            center_set <- median(plot_data[5][which(plot_data$Sets == j), ])
          } else if (loc_center_type == 3) {
            center <- known_param
            center_set <- known_param
            n_bar_set <- mean(plot_data$n[which(plot_data$Sets == j)])
          }

          set_obs <- unique(plot_data$Sample[plot_data$Sets == j])
          for (i in set_obs) {
            if (att_type == 2 || att_type == 3) {
              center <- center_set * plot_data$n[i] / n_bar_set
            }

            if (att_type == 1 || att_type == 2) {
              if (att_type == 1) {
                n_upper <- center + 3 * sqrt(center * (1 - center) / plot_data$n[1])
                n_lower <- center - 3 * sqrt(center * (1 - center) / plot_data$n[1])
                UCL_t <- n_upper
                LCL_t <- n_lower
              } else {
                n_upper <- center + 3 * sqrt(center * (1 - (center_set / n_bar_set)))
                n_lower <- center - 3 * sqrt(center * (1 - (center_set / n_bar_set)))
                UCL_t <- n_upper
                LCL_t <- n_lower
              }
              if (LCL_t < 0) LCL_t <- NA
            } else {
              if (att_type == 3 || att_type == 4) {
                if (att_type == 4) {
                  n_upper <- center + 3 * sqrt(center / plot_data$n[i])
                  n_lower <- center - 3 * sqrt(center / plot_data$n[i])
                  UCL_t <- n_upper
                  LCL_t <- n_lower
                } else {
                  n_upper <- center + 3 * sqrt(center_set)
                  n_lower <- center - 3 * sqrt(center_set)
                  UCL_t <- n_upper
                  LCL_t <- n_lower
                }
                if (LCL_t < 0) LCL_t <- NA
              }
            }
            order <- c(order, i)
            UCL <- c(UCL, UCL_t)
            LCL <- c(LCL, LCL_t)
            centerline <- c(centerline, center)
            zone_a_up <- c(zone_a_up, UCL_t)
            ac_up <- UCL_t - center
            zone_ab_up <- c(zone_ab_up, center + (2 / 3) * ac_up)
            zone_bc_up <- c(zone_bc_up, center + (1 / 3) * ac_up)
            if (is.na(LCL_t)) LCL_t <- 0
            ac_low <- center - LCL_t
            zone_a_low <- c(zone_a_low, LCL_t)
            zone_ab_low <- c(zone_ab_low, center - (2 / 3) * ac_low)
            zone_bc_low <- c(zone_bc_low, center - (1 / 3) * ac_low)
          }
        }

        lim_dat <- cbind(order, LCL, centerline, UCL, zone_a_up, zone_ab_up, zone_bc_up, zone_a_low, zone_ab_low, zone_bc_low)[order(order), ]
        plot_data <- cbind(plot_data, lim_dat)[-6]
        plot_data <- as.data.frame(plot_data[order(Sample), ])

        control_vio_att <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = plot_data[[5]],
          center.line = plot_data$centerline,
          control.limits.ucl = plot_data$UCL,
          zone.a.upper = plot_data$zone_a_up,
          zone.ab.upper = plot_data$zone_ab_up,
          zone.bc.upper = plot_data$zone_bc_up,
          control.limits.lcl = plot_data$LCL,
          zone.a.lower = plot_data$zone_a_low,
          zone.ab.lower = plot_data$zone_ab_low,
          zone.bc.lower = plot_data$zone_bc_low
        )
      }

      if (att_lim_calc == 3) { # ave MR
        for (j in set_id) {
          if (loc_center_type == 1) {
            if (att_type == 1 || att_type == 4) {
              center <- sum(plot_data$Count[which(plot_data$Sets == j)]) / sum(plot_data$n[which(plot_data$Sets == j)])
            } else {
              n_bar_set <- mean(plot_data$n[which(plot_data$Sets == j)])
              center_set <- n_bar_set * sum(plot_data$Count[which(plot_data$Sets == j)]) / sum(plot_data$n[which(plot_data$Sets == j)])
            }
          } else if (loc_center_type == 2) {
            center <- median(plot_data[5][which(plot_data$Sets == j), ])
            center_set <- median(plot_data[5][which(plot_data$Sets == j), ])
          } else if (loc_center_type == 3) {
            center <- known_param
            center_set <- known_param
            n_bar_set <- mean(plot_data$n[which(plot_data$Sets == j)])
          }
          set_obs <- unique(plot_data$Sample[plot_data$Sets == j])
          MR_set <- mean(abs(diff(plot_data[, 5][plot_data$Sets == j])))
          est_sig_set <- MR_set / spc.constant.calculation.d2(sample.size = 2)
          for (i in set_obs) {
            if (att_type == 2 || att_type == 3) {
              center <- center_set * plot_data$n[i] / n_bar_set
            }

            n_upper <- center + 3 * est_sig_set
            n_lower <- center - 3 * est_sig_set
            UCL_t <- n_upper
            LCL_t <- n_lower

            if (LCL_t < 0) LCL_t <- NA

            order <- c(order, i)
            UCL <- c(UCL, UCL_t)
            LCL <- c(LCL, LCL_t)
            centerline <- c(centerline, center)
            zone_a_up <- c(zone_a_up, UCL_t)
            ac_up <- UCL_t - center
            zone_ab_up <- c(zone_ab_up, center + (2 / 3) * ac_up)
            zone_bc_up <- c(zone_bc_up, center + (1 / 3) * ac_up)
            if (is.na(LCL_t)) LCL_t <- 0
            ac_low <- center - LCL_t
            zone_a_low <- c(zone_a_low, LCL_t)
            zone_ab_low <- c(zone_ab_low, center - (2 / 3) * ac_low)
            zone_bc_low <- c(zone_bc_low, center - (1 / 3) * ac_low)
          }
        }

        lim_dat <- cbind(order, LCL, centerline, UCL, zone_a_up, zone_ab_up, zone_bc_up, zone_a_low, zone_ab_low, zone_bc_low)[order(order), ]
        plot_data <- cbind(plot_data, lim_dat)[-6]
        plot_data <- as.data.frame(plot_data[order(Sample), ])

        control_vio_att <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = plot_data[[5]],
          center.line = plot_data$centerline,
          control.limits.ucl = plot_data$UCL,
          zone.a.upper = plot_data$zone_a_up,
          zone.ab.upper = plot_data$zone_ab_up,
          zone.bc.upper = plot_data$zone_bc_up,
          control.limits.lcl = plot_data$LCL,
          zone.a.lower = plot_data$zone_a_low,
          zone.ab.lower = plot_data$zone_ab_low,
          zone.bc.lower = plot_data$zone_bc_low
        )
      }

      if (att_lim_calc == 4) { # median MR
        for (j in set_id) {
          if (loc_center_type == 1) {
            if (att_type == 1 || att_type == 4) {
              center <- sum(plot_data$Count[which(plot_data$Sets == j)]) / sum(plot_data$n[which(plot_data$Sets == j)])
            } else {
              n_bar_set <- mean(plot_data$n[which(plot_data$Sets == j)])
              center_set <- n_bar_set * sum(plot_data$Count[which(plot_data$Sets == j)]) / sum(plot_data$n[which(plot_data$Sets == j)])
            }
          } else if (loc_center_type == 2) {
            center <- median(plot_data[5][which(plot_data$Sets == j), ])
          } else if (loc_center_type == 3) {
            center <- known_param
            center_set <- known_param
            n_bar_set <- mean(plot_data$n[which(plot_data$Sets == j)])
          }
          set_obs <- unique(plot_data$Sample[plot_data$Sets == j])
          MR_set <- median(abs(diff(plot_data[, 5][plot_data$Sets == j])))
          est_sig_set <- MR_set / spc.constant.calculation.d4(sample.size = 2)
          for (i in set_obs) {
            if (att_type == 2 || att_type == 3) {
              center <- center_set * plot_data$n[i] / n_bar_set
            }

            n_upper <- center + 3 * est_sig_set
            n_lower <- center - 3 * est_sig_set
            UCL_t <- n_upper
            LCL_t <- n_lower

            if (LCL_t < 0) LCL_t <- NA

            order <- c(order, i)
            UCL <- c(UCL, UCL_t)
            LCL <- c(LCL, LCL_t)
            centerline <- c(centerline, center)
            zone_a_up <- c(zone_a_up, UCL_t)
            ac_up <- UCL_t - center
            zone_ab_up <- c(zone_ab_up, center + (2 / 3) * ac_up)
            zone_bc_up <- c(zone_bc_up, center + (1 / 3) * ac_up)
            if (is.na(LCL_t)) LCL_t <- 0
            ac_low <- center - LCL_t
            zone_a_low <- c(zone_a_low, LCL_t)
            zone_ab_low <- c(zone_ab_low, center - (2 / 3) * ac_low)
            zone_bc_low <- c(zone_bc_low, center - (1 / 3) * ac_low)
          }
        }

        lim_dat <- cbind(order, LCL, centerline, UCL, zone_a_up, zone_ab_up, zone_bc_up, zone_a_low, zone_ab_low, zone_bc_low)[order(order), ]
        plot_data <- cbind(plot_data, lim_dat)[-6]
        plot_data <- as.data.frame(plot_data[order(Sample), ])

        control_vio_att <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = plot_data[[5]],
          center.line = plot_data$centerline,
          control.limits.ucl = plot_data$UCL,
          zone.a.upper = plot_data$zone_a_up,
          zone.ab.upper = plot_data$zone_ab_up,
          zone.bc.upper = plot_data$zone_bc_up,
          control.limits.lcl = plot_data$LCL,
          zone.a.lower = plot_data$zone_a_low,
          zone.ab.lower = plot_data$zone_ab_low,
          zone.bc.lower = plot_data$zone_bc_low
        )
      }

      if (att_lim_calc == 5) { # std
        for (j in set_id) {
          if (loc_center_type == 1) {
            if (att_type == 1 || att_type == 4) {
              center <- sum(plot_data$Count[which(plot_data$Sets == j)]) / sum(plot_data$n[which(plot_data$Sets == j)])
            } else {
              n_bar_set <- mean(plot_data$n[which(plot_data$Sets == j)])
              center_set <- n_bar_set * sum(plot_data$Count[which(plot_data$Sets == j)]) / sum(plot_data$n[which(plot_data$Sets == j)])
            }
          } else if (loc_center_type == 2) {
            center <- median(plot_data[5][which(plot_data$Sets == j), ])
          } else if (loc_center_type == 3) {
            center <- known_param
            center_set <- known_param
            n_bar_set <- mean(plot_data$n[which(plot_data$Sets == j)])
          }
          set_obs <- unique(plot_data$Sample[plot_data$Sets == j])

          est_sig_set <- sd(plot_data[, 5][which(plot_data$Sets == j)])
          for (i in set_obs) {
            if (att_type == 2 || att_type == 3) {
              center <- center_set * plot_data$n[i] / n_bar_set
            }

            n_upper <- center + 3 * est_sig_set
            n_lower <- center - 3 * est_sig_set
            UCL_t <- n_upper
            LCL_t <- n_lower

            if (LCL_t < 0) LCL_t <- NA

            order <- c(order, i)
            UCL <- c(UCL, UCL_t)
            LCL <- c(LCL, LCL_t)
            centerline <- c(centerline, center)
            zone_a_up <- c(zone_a_up, UCL_t)
            ac_up <- UCL_t - center
            zone_ab_up <- c(zone_ab_up, center + (2 / 3) * ac_up)
            zone_bc_up <- c(zone_bc_up, center + (1 / 3) * ac_up)
            if (is.na(LCL_t)) LCL_t <- 0
            ac_low <- center - LCL_t
            zone_a_low <- c(zone_a_low, LCL_t)
            zone_ab_low <- c(zone_ab_low, center - (2 / 3) * ac_low)
            zone_bc_low <- c(zone_bc_low, center - (1 / 3) * ac_low)
          }
        }

        lim_dat <- cbind(order, LCL, centerline, UCL, zone_a_up, zone_ab_up, zone_bc_up, zone_a_low, zone_ab_low, zone_bc_low)[order(order), ]
        plot_data <- cbind(plot_data, lim_dat)[-6]
        plot_data <- as.data.frame(plot_data[order(Sample), ])

        control_vio_att <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = plot_data[[5]],
          center.line = plot_data$centerline,
          control.limits.ucl = plot_data$UCL,
          zone.a.upper = plot_data$zone_a_up,
          zone.ab.upper = plot_data$zone_ab_up,
          zone.bc.upper = plot_data$zone_bc_up,
          control.limits.lcl = plot_data$LCL,
          zone.a.lower = plot_data$zone_a_low,
          zone.ab.lower = plot_data$zone_ab_low,
          zone.bc.lower = plot_data$zone_bc_low
        )
      }

      if (att_lim_calc == 8) { # custom
        center <- loc_center_custom

        set_obs <- unique(plot_data$Sample)
        for (i in set_obs) {
          UCL_t <- loc_upper_custom
          LCL_t <- loc_lower_custom

          order <- c(order, i)
          UCL <- c(UCL, UCL_t)
          LCL <- c(LCL, LCL_t)
          centerline <- c(centerline, center)
          zone_a_up <- c(zone_a_up, UCL_t)
          ac_up <- UCL_t - center
          zone_ab_up <- c(zone_ab_up, center + (2 / 3) * ac_up)
          zone_bc_up <- c(zone_bc_up, center + (1 / 3) * ac_up)
          if (is.na(LCL_t)) LCL_t <- 0
          ac_low <- center - LCL_t
          zone_a_low <- c(zone_a_low, LCL_t)
          zone_ab_low <- c(zone_ab_low, center - (2 / 3) * ac_low)
          zone_bc_low <- c(zone_bc_low, center - (1 / 3) * ac_low)
        }

        lim_dat <- cbind(order, LCL, centerline, UCL, zone_a_up, zone_ab_up, zone_bc_up, zone_a_low, zone_ab_low, zone_bc_low)[order(order), ]
        plot_data <- cbind(plot_data, lim_dat)[-6]
        plot_data <- as.data.frame(plot_data[order(Sample), ])

        control_vio_att <- spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = plot_data[[5]],
          center.line = plot_data$centerline,
          control.limits.ucl = plot_data$UCL,
          zone.a.upper = plot_data$zone_a_up,
          zone.ab.upper = plot_data$zone_ab_up,
          zone.bc.upper = plot_data$zone_bc_up,
          control.limits.lcl = plot_data$LCL,
          zone.a.lower = plot_data$zone_a_low,
          zone.ab.lower = plot_data$zone_ab_low,
          zone.bc.lower = plot_data$zone_bc_low
        )
      }

      # plot att
      outside <- control_vio_att[["rule.results"]][["outside.limits"]]
      runs <- control_vio_att[["rule.results"]][["runs"]]
      trends <- control_vio_att[["rule.results"]][["trends"]]
      alternating <- control_vio_att[["rule.results"]][["alternating"]]
      zone_a <- control_vio_att[["rule.results"]][["zone.a"]]
      consec_c <- control_vio_att[["rule.results"]][["consecutive.zone.c"]]
      consec_ab <- control_vio_att[["rule.results"]][["consecutive.zone.ab"]]

      att_plot_data_r(data.frame(
        plot_data,
        outside,
        runs,
        trends,
        alternating,
        zone_a,
        consec_c,
        consec_ab
      ))
      plot_data_change_att(isolate(plot_data_change_att()) + 1)
      att_plot_data_p <- att_plot_data_r()

      ooc_att <- input$ooc_rules_att
      if (is.null(ooc_att)) ooc_att <- c(1, 2, 3, 4)

      for (i in 1:(k_obs)) {
        if (att_plot_data_p$outside[i] == TRUE && is.element(1, ooc_att)) {
          att_plot_data_p$outside[i] <- att_plot_data_p[i, 5]
        } else {
          att_plot_data_p$outside[i] <- NA
        }
        if (att_plot_data_p$runs[i] == TRUE && is.element(2, ooc_att)) {
          att_plot_data_p$runs[i] <- att_plot_data_p[i, 5]
        } else {
          att_plot_data_p$runs[i] <- NA
        }
        if (att_plot_data_p$trends[i] == TRUE && is.element(3, ooc_att)) {
          att_plot_data_p$trends[i] <- att_plot_data_p[i, 5]
        } else {
          att_plot_data_p$trends[i] <- NA
        }
        if (att_plot_data_p$alternating[i] == TRUE && is.element(4, ooc_att)) {
          att_plot_data_p$alternating[i] <- att_plot_data_p[i, 5]
        } else {
          att_plot_data_p$alternating[i] <- NA
        }
        if (att_plot_data_p$zone_a[i] == TRUE && is.element(5, ooc_att)) {
          att_plot_data_p$zone_a[i] <- att_plot_data_p[i, 5]
        } else {
          att_plot_data_p$zone_a[i] <- NA
        }
        if (att_plot_data_p$consec_c[i] == TRUE && is.element(6, ooc_att)) {
          att_plot_data_p$consec_c[i] <- att_plot_data_p[i, 5]
        } else {
          att_plot_data_p$consec_c[i] <- NA
        }
        if (att_plot_data_p$consec_ab[i] == TRUE && is.element(7, ooc_att)) {
          att_plot_data_p$consec_ab[i] <- att_plot_data_p[i, 5]
        } else {
          att_plot_data_p$consec_ab[i] <- NA
        }
        if (att_plot_data_p$zone_a_b[i] == TRUE && is.element(8, ooc_att)) {
          att_plot_data_p$zone_a_b[i] <- att_plot_data_p[i, 5]
        } else {
          att_plot_data_p$zone_a_b[i] <- NA
        }
      }

      # set columns with no OOC as numeric to avoid error
      att_plot_data_p$outside <- as.numeric(att_plot_data_p$outside)
      att_plot_data_p$runs <- as.numeric(att_plot_data_p$runs)
      att_plot_data_p$trends <- as.numeric(att_plot_data_p$trends)
      att_plot_data_p$alternating <- as.numeric(att_plot_data_p$alternating)
      att_plot_data_p$zone_a <- as.numeric(att_plot_data_p$zone_a)
      att_plot_data_p$consec_c <- as.numeric(att_plot_data_p$consec_c)
      att_plot_data_p$consec_ab <- as.numeric(att_plot_data_p$consec_ab)
      att_plot_data_p$zone_a_b <- as.numeric(att_plot_data_p$zone_a_b)

      att_chart_options <- input$att_chart_options

      if (att_type == 1 || att_type == 2) {
        subtitle <- paste(names(choice_att_charts)[as.numeric(att_type)], ":: Limits from", names(choice_att_b_limits)[as.numeric(att_lim_calc)])
      }

      if (att_type == 3 || att_type == 4) {
        subtitle <- paste(names(choice_att_charts)[as.numeric(att_type)], ":: Limits from", names(choice_att_p_limits)[as.numeric(att_lim_calc)])
      }

      diff_t <- c(0, diff(att_plot_data_p$Sets))
      set_plot <- 1
      for (i in seq(1, (k_obs))) {
        if (i == 1) {
          next
        }
        if (diff_t[i] != 0) {
          set_plot[i] <- set_plot[i - 1] + 1
        } else {
          set_plot[i] <- set_plot[i - 1]
        }
      }

      if (att_type == 1) {
        y <- "p"
      }
      if (att_type == 2) {
        y <- "np"
      }
      if (att_type == 3) {
        y <- "c"
      }
      if (att_type == 4) {
        y <- "u"
      }

      p <- ggplot(att_plot_data_p, aes(x = Sample, y = .data[[y]])) +
        theme_gray(base_size = font_size)
      if (loc_center_type != 3) {
        p <- p +
          labs(x = "Samples", y = y_lab, title = "Statistical Process Control Chart", subtitle = subtitle)
      } else {
        if (att_type == 1) {
          p <- p +
            labs(
              x = "Samples",
              y = y_lab,
              title = "Statistical Process Control Chart",
              subtitle = substitute(paste(subtitle, " :: ", pi, " known to be = ", known_param))
            )
        }
        if (att_type == 2) {
          p <- p +
            labs(
              x = "Samples",
              y = y_lab,
              title = "Statistical Process Control Chart",
              subtitle = substitute(paste(subtitle, " :: n", pi, " known to be = ", known_param))
            )
        }
        if (att_type == 3) {
          p <- p +
            labs(
              x = "Samples",
              y = y_lab,
              title = "Statistical Process Control Chart",
              subtitle = substitute(paste(subtitle, " :: ", lambda, " known to be = ", known_param))
            )
        }
        if (att_type == 4) {
          p <- p +
            labs(
              x = "Samples",
              y = y_lab,
              title = "Statistical Process Control Chart",
              subtitle = substitute(paste(subtitle, " :: ", lambda, "/n known to be = ", known_param))
            )
        }
      }

      if (is.element(9, ooc_att) || is.element(6, att_chart_options)) {
        trans <- 90
        rgb_outer <- col2rgb(color[2])
        rgb_mid <- col2rgb(color[7])
        rgb_inner <- col2rgb(color[3])

        outer_col <- rgb(red = rgb_outer[1], green = rgb_outer[2], blue = rgb_outer[3], alpha = trans, maxColorValue = 255)
        mid_col <- rgb(red = rgb_mid[1], green = rgb_mid[2], blue = rgb_mid[3], alpha = trans, maxColorValue = 255)
        inner_col <- rgb(red = rgb_inner[1], green = rgb_inner[2], blue = rgb_inner[3], alpha = trans, maxColorValue = 255)

        p <- p +
          geom_ribbon(aes(ymin = zone_a_low, ymax = zone_a_up, group = set_plot), fill = outer_col) +
          geom_ribbon(aes(ymin = zone_ab_low, ymax = zone_ab_up, group = set_plot), fill = mid_col) +
          geom_ribbon(aes(ymin = zone_bc_low, ymax = zone_bc_up, group = set_plot), fill = inner_col)
      }

      p <- p + geom_point(color = color[4])
      if (is.element(1, att_chart_options)) {
        p <- p + geom_line(aes(group = set_plot), color = color[4])
      }
      if (is.element(3, att_chart_options)) {
        p <- p + geom_line(aes(y = centerline, group = set_plot), color = color[3])
      }

      if (is.element(2, att_chart_options)) {
        p <- p +
          geom_line(aes(y = UCL, group = set_plot), color = color[2], linetype = 5) +
          geom_line(aes(y = LCL, group = set_plot), color = color[2], linetype = 5)
      }

      if (is.element(5, att_chart_options)) {
        p <- p + geom_text(aes(y = outside, label = "O"), nudge_x = 0.75, color = color[2])
      }
      if (is.element(4, att_chart_options)) {
        p <- p + geom_point(aes(y = outside), color = color[2], shape = 8)
      }
      if (is.element(5, att_chart_options)) {
        p <- p + geom_text(aes(y = runs, label = "R"), nudge_x = -0.75, color = color[2])
      }
      if (is.element(4, att_chart_options)) {
        p <- p + geom_point(aes(y = runs), color = color[2], shape = 8)
      }
      if (is.element(5, att_chart_options)) {
        p <- p + geom_text(aes(y = trends, label = "T"), nudge_y = 0.75, color = color[2])
      }
      if (is.element(4, att_chart_options)) {
        p <- p + geom_point(aes(y = trends), color = color[2], shape = 8)
      }
      if (is.element(5, att_chart_options)) {
        p <- p + geom_text(aes(y = alternating, label = "A"), nudge_y = -0.75, color = color[2])
      }
      if (is.element(4, att_chart_options)) {
        p <- p + geom_point(aes(y = alternating), color = color[2], shape = 8)
      }
      if (is.element(5, att_chart_options)) {
        p <- p + geom_text(aes(y = zone_a, label = "Z-a"), nudge_y = 0.75, nudge_x = 0.75, color = color[2])
      }
      if (is.element(4, att_chart_options)) {
        p <- p + geom_point(aes(y = zone_a), color = color[2], shape = 8)
      }
      if (is.element(5, att_chart_options)) {
        p <- p + geom_text(aes(y = consec_c, label = "C-c"), nudge_y = -0.75, nudge_x = 0.75, color = color[2])
      }
      if (is.element(4, att_chart_options)) {
        p <- p + geom_point(aes(y = consec_c), color = color[2], shape = 8)
      }
      if (is.element(5, att_chart_options)) {
        p <- p + geom_text(aes(y = consec_ab, label = "C-ab"), nudge_y = 0.75, nudge_x = -0.75, color = color[2])
      }
      if (is.element(4, att_chart_options)) {
        p <- p + geom_point(aes(y = consec_ab), color = color[2], shape = 8)
      }
      if (is.element(5, att_chart_options)) {
        p <- p + geom_text(aes(y = zone_a_b, label = "Z-ab"), nudge_y = -0.75, nudge_x = -0.75, color = color[2])
      }
      if (is.element(4, att_chart_options)) {
        p <- p + geom_point(aes(y = zone_a_b), color = color[2], shape = 8)
      }

      if (inherits(try(ggplot_build(p)), "try-error")) {
        plot <- ggplot()
        return()
      }

      p
    })

    output$att_table_out <- DT::renderDataTable({
      dat <- att_plot_data_r()
      if (is.null(dat)) {
        att_chart_out()
        dat <- att_plot_data_r()
      }
      req(dat)
      R <- input$spc_att_decimals
      dat <- lolcat::round.object(dat, digits = R)
      DT::datatable(data = dat, options = list(dom = "t", paging = FALSE), rownames = FALSE)
    })
    outputOptions(output, "att_table_out", suspendWhenHidden = FALSE)

    output$hover_info_att <- renderUI({
      req(input$spc_att_loc_type)
      R <- input$spc_att_decimals
      hover <- input$plot_hover_att
      if (is.null(hover)) return(NULL)
      data <- att_plot_data_r()
      req(data)
      point <- nearPoints(
        df = data,
        coordinfo = hover,
        xvar = "Sample",
        yvar = names(data)[5],
        threshold = 5,
        maxpoints = 1,
        addDist = TRUE
      )
      if (nrow(point) == 0) return(NULL)

      left_px <- hover$coords_css$x
      top_px <- hover$coords_css$y

      style <- paste0(
        "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
        "left:", left_px + 2, "px; top:", top_px + 2, "px;"
      )

      wellPanel(
        style = style,
        p(HTML(paste0(
          "<span style='display:block; text-transform:capitalize; text-align:center'>", point$facet, "</span>",
          "<b> Point: </b>", ro(point$Sample), "<br/>",
          "<b> Measure: </b>", ro(point[[5]], R), "<br/>",
          if (length(unique(att_plot_data_r()$Sets)) > 1) {
            paste0("<b> Set: </b>", point$Set, "<br/>")
          },
          "<b> UCL: </b>", ro(point$UCL, R), "<br/>",
          "<b> Centerline: </b>", ro(point$centerline, R), "<br/>",
          "<b> LCL: </b>", ro(point$LCL, R)
        )))
      )
    })

    output$limit_analysis_att <- renderUI({
      data <- filtered_data()
      req(data)
      data_type <- input$spc_att_data_type
      UI1 <- as.numeric(input$spc_att_UI1)
      UI2 <- as.numeric(input$spc_att_UI2)
      req(UI1)
      att_type <- input$spc_att_loc_type
      upper_custom <- input$custom.att.upper
      center_custom <- input$custom.att.center
      lower_custom <- input$custom.att.lower
      sets <- as.numeric(input$spc_att_set)
      known_sig <- input$known_sig_att

      dat <- att_plot_data_r()
      req(dat)

      R <- input$spc_att_decimals
      center_type <- as.numeric(input$spc_att_loc)
      known_param <- input$known_param
      att_lim_calc <- as.numeric(input$spc_att_loc_lim)
      std_err <- as.numeric(input$std_err_att)

      if (att_type == 1 || att_type == 2) {
        choice <- choice_att_b_limits
      } else {
        choice <- choice_att_p_limits
      }

      HTML(c(
        "<h4>Limit Calculation Summary</h4>",
        "<table>",
        "<tr><td><u>", names(choice_att_charts)[as.numeric(att_type)], " Chart</u></td>",
        "<td></td></tr>",
        "<tr><td>Limits from:</td><td style='text-align:left'>", names(choice[att_lim_calc]), "</td></tr>",
        "<tr><td>Centerline:</td><td style='text-align:left'>",
        if (att_lim_calc == 7) {
          "None"
        } else if (att_lim_calc == 8) {
          "Custom"
        } else if (center_type == 3) {
          paste("Known to be = ", known_param)
        } else {
          names(choice_x_centerline)[center_type]
        },
        "</td></tr>",
        "<tr><td>Standard Errors Used: </td><td style='text-align:left'>", std_err, "</td></tr>",
        "</table>",
        "<br/>"
      ))
    })

    # =========================================================================
    # LIMITS CALCULATIONS TAB
    # =========================================================================

    # -------------------------------------------------------------------------
    # Dynamic UI Rendering for Limits Calculations
    # -------------------------------------------------------------------------

    # X-bar dynamic stat input
    output$x_bar_lim_value <- renderUI({
      select <- input$x_bar_lim_stat
      req(select)

      labels <- list(
        "1" = withMathJax("$\\bar{R}:$"),
        "2" = withMathJax("$\\widetilde{R}:$"),
        "3" = withMathJax("$\\bar{s}:$"),
        "4" = withMathJax("$\\widetilde{s\\,}:$"),
        "5" = withMathJax("$\\overline{s^{2}}:$"),
        "6" = withMathJax("$\\overline{X}_{MR_\\widetilde{\\,X\\,}}:$"),
        "7" = withMathJax("$\\widetilde{X}_{MR_\\bar{X}}:$"),
        "8" = withMathJax("$s_{\\bar{X}}:$"),
        "9" = withMathJax("$\\sigma:$")
      )

      numericInput(
        inputId = ns("x_bar_lim_val"),
        label = labels[[as.character(select)]],
        value = 2,
        width = "75px"
      )
    })

    # X dynamic stat input
    output$x_lim_value <- renderUI({
      select <- input$x_lim_stat
      req(select)

      labels <- list(
        "1" = withMathJax("$\\overline{MR}:$"),
        "2" = withMathJax("$\\widetilde{MR}:$"),
        "3" = withMathJax("$s_{k}:$"),
        "4" = withMathJax("$\\sigma:$")
      )

      numericInput(
        inputId = ns("x_lim_val"),
        label = labels[[as.character(select)]],
        value = 5,
        width = "75px"
      )
    })

    # X conditional n input
    output$x_lim_n_UI <- renderUI({
      select <- input$x_lim_stat
      req(select)

      if (select == 1 || select == 2) {
        numericInput(
          inputId = ns("x_lim_n"),
          label = withMathJax("$n_{MR}:$"),
          value = 2,
          width = "75px"
        )
      } else if (select == 3) {
        numericInput(
          inputId = ns("x_lim_n"),
          label = withMathJax("$k_{s}:$"),
          value = 2,
          width = "75px"
        )
      } else {
        NULL
      }
    })

    # R dynamic stat input
    output$r_lim_value <- renderUI({
      select <- input$r_lim_stat
      req(select)

      labels <- list(
        "1" = withMathJax("$\\bar{R}:$"),
        "2" = withMathJax("$\\widetilde{R}:$"),
        "3" = withMathJax("$\\bar{s}:$"),
        "4" = withMathJax("$\\widetilde{s\\,}:$"),
        "5" = withMathJax("$\\bar{s^{2}}:$"),
        "6" = withMathJax("$\\sigma:$")
      )

      numericInput(
        inputId = ns("r_lim_val"),
        label = labels[[as.character(select)]],
        value = 5,
        width = "75px"
      )
    })

    # s dynamic stat input
    output$s_lim_value <- renderUI({
      select <- input$s_lim_stat
      req(select)

      labels <- list(
        "1" = withMathJax("$\\bar{R}:$"),
        "2" = withMathJax("$\\widetilde{R}:$"),
        "3" = withMathJax("$\\bar{s}:$"),
        "4" = withMathJax("$\\widetilde{s\\,}:$"),
        "5" = withMathJax("$\\bar{s^{2}}:$"),
        "6" = withMathJax("$\\sigma:$")
      )

      numericInput(
        inputId = ns("s_lim_val"),
        label = labels[[as.character(select)]],
        value = 5,
        width = "75px"
      )
    })

    # s² dynamic stat input
    output$s2_lim_value <- renderUI({
      select <- input$s2_lim_stat
      req(select)

      labels <- list(
        "1" = withMathJax("$\\bar{R}:$"),
        "2" = withMathJax("$\\widetilde{R}:$"),
        "3" = withMathJax("$\\bar{s}:$"),
        "4" = withMathJax("$\\widetilde{s\\,}:$"),
        "5" = withMathJax("$\\bar{s^{2}}:$"),
        "6" = withMathJax("$\\sigma:$")
      )

      numericInput(
        inputId = ns("s2_lim_val"),
        label = labels[[as.character(select)]],
        value = 5,
        width = "75px"
      )
    })

    # Kappa column selectors
    output$kappa_limits_kappa <- renderUI({
      data <- filtered_data()
      if (is.null(data) || ncol(data) == 0) {
        return(selectInput(
          inputId = ns("kappa_limits_k"),
          label = paste0("Select \u03BA"),
          choices = list("No data available" = "")
        ))
      }

      choices <- seq_len(ncol(data))
      names(choices) <- names(data)

      selectInput(
        inputId = ns("kappa_limits_k"),
        label = paste0("Select \u03BA"),
        choices = choices,
        selected = input$kappa_limits_k
      )
    })

    output$kappa_limits_var <- renderUI({
      data <- filtered_data()
      UI1 <- as.numeric(input$kappa_limits_k)
      
      if (is.null(data) || ncol(data) == 0) {
        return(selectInput(
          inputId = ns("kappa_limits_v"),
          label = paste0("Select V"),
          choices = list("No data available" = "")
        ))
      }

      if (is.na(UI1) || UI1 == 0) {
        return(selectInput(
          inputId = ns("kappa_limits_v"),
          label = paste0("Select V"),
          choices = list("Select \u03BA first" = "")
        ))
      }

      choices <- seq_len(ncol(data))
      names(choices) <- names(data)

      # Remove already selected column
      if (UI1 %in% choices) {
        temp <- seq_len(length(choices))
        temp <- temp[-UI1]
        choices <- choices[temp]
      }

      selectInput(
        inputId = ns("kappa_limits_v"),
        label = paste0("Select V"),
        choices = choices,
        selected = input$kappa_limits_v
      )
    })

    # -------------------------------------------------------------------------
    # HTML Output Rendering for Limits Calculations
    # -------------------------------------------------------------------------

    # X-bar output
    output$x_bar_lim_out <- renderUI({
      select <- as.numeric(input$x_bar_lim_stat)
      x_bar <- as.numeric(input$x_bar_lim_x_bar)
      stat <- as.numeric(input$x_bar_lim_val)
      n <- as.numeric(input$x_bar_lim_n)
      sterr <- as.numeric(input$x_bar_lim_sterr)
      R <- input$x_bar_lim_decimals

      req(select, x_bar, stat, n, sterr, R)

      results <- calculate_x_bar_limits(select, x_bar, stat, n, sterr, R)

      HTML(c(
        paste("<b>Control Limit Calculations: ", withMathJax("$\\bar{X}$"), "</b>"),
        "<br><br>",
        paste("Limits based on ", names(choice_x_bar_limits)[select], " = ", ro(stat, R), "<br>"),
        if (!is.null(results$note)) paste0(results$note, "<br>"),
        paste("Standard Errors = ", sterr),
        "<br><br>",
        "<table>",
        "<tr>",
        "<td>", paste(withMathJax("$UCL=$")), "</td>",
        "<td align='left'>", results$UCL, "</td>",
        "<td></td>",
        "<td>", paste(withMathJax("$\\hat{\\sigma}=$")), "</td>",
        "<td align='left'>", results$sig_est, "</td>",
        "</tr>",
        "<tr>",
        "<td>", paste(withMathJax("$\\overline{\\overline{X}}=$")), "</td>",
        "<td align='left'>", results$centerline, "</td>",
        "<td></td>",
        "<td>", paste(withMathJax("$n=$")), "</td>",
        "<td align='left'>", results$n, "</td>",
        "</tr>",
        "<tr>",
        "<td>", paste(withMathJax("$LCL =$")), "</td>",
        "<td>", results$LCL, "</td></tr>",
        "</table>"
      ))
    })

    # X output
    output$x_lim_out <- renderUI({
      select <- as.numeric(input$x_lim_stat)
      x_bar <- as.numeric(input$x_lim_x_bar)
      stat <- as.numeric(input$x_lim_val)
      n <- as.numeric(input$x_lim_n)
      sterr <- as.numeric(input$x_lim_sterr)
      R <- input$x_lim_decimals

      req(select, x_bar, stat, sterr, R)
      if (select < 4) req(n)

      results <- calculate_x_limits(select, x_bar, stat, n, sterr, R)

      HTML(c(
        paste("<b>Control Limit Calculations: ", withMathJax("$X$"), "</b>"),
        "<br><br>",
        paste("Limits based on ", names(choice_x_limits)[select], " = ", ro(stat, R), "<br>"),
        paste("Standard Errors = ", sterr),
        "<br><br>",
        "<table>",
        "<tr>",
        "<td>", paste(withMathJax("$UCL=$")), "</td>",
        "<td align='left'>", results$UCL, "</td>",
        "<td></td>",
        "<td>", paste(withMathJax(results$sig_label)), "</td>",
        "<td align='left'>", results$sig_est, "</td>",
        "</tr>",
        "<tr>",
        "<td>", paste(withMathJax("$\\bar{X}=$")), "</td>",
        "<td align='left'>", results$centerline, "</td>",
        "<td></td>",
        if (select < 4 && results$n_label != "") {
          paste(
            "<td>",
            paste(withMathJax(results$n_label)),
            "</td>",
            "<td align='left'>", results$n, "</td>"
          )
        } else {
          ""
        },
        "</tr>",
        "<tr>",
        "<td>", paste(withMathJax("$LCL =$")), "</td>",
        "<td>", results$LCL, "</td></tr>",
        "</table>"
      ))
    })

    # R output
    output$r_lim_out <- renderUI({
      select <- as.numeric(input$r_lim_stat)
      stat <- as.numeric(input$r_lim_val)
      n <- as.numeric(input$r_lim_n)
      sterr <- as.numeric(input$r_lim_sterr)
      R <- input$r_lim_decimals

      req(select, stat, n, sterr, R)

      results <- calculate_r_limits(select, stat, n, sterr, R)

      HTML(c(
        paste("<b>Control Limit Calculations: ", withMathJax("$R$"), "</b>"),
        "<br><br>",
        paste("Limits based on ", names(choice_r_limits)[select], " = ", ro(stat, R), "<br>"),
        paste("Standard Errors = ", sterr),
        "<br><br>",
        "<table>",
        "<tr>",
        "<td>", paste(withMathJax("$UCL=$")), "</td>",
        "<td align='left'>", results$UCL, "</td>",
        "<td></td>",
        "<td>", paste(withMathJax("$\\hat{\\sigma}=$")), "</td>",
        "<td align='left'>", results$sig_est, "</td>",
        "</tr>",
        "<tr>",
        "<td>", paste(results$stat_symb), "</td>",
        "<td align='left'>", results$centerline, "</td>",
        "<td></td>",
        "<td>", paste(withMathJax("$n=$")), "</td>",
        "<td align='left'>", results$n, "</td>",
        "</tr>",
        "<tr>",
        "<tr>",
        "<td>", paste(withMathJax("$LCL =$")), "</td>",
        "<td>", results$LCL, "</td></tr>",
        "</table>"
      ))
    })

    # s output
    output$s_lim_out <- renderUI({
      select <- as.numeric(input$s_lim_stat)
      stat <- as.numeric(input$s_lim_val)
      n <- as.numeric(input$s_lim_n)
      sterr <- as.numeric(input$s_lim_sterr)
      R <- input$s_lim_decimals

      req(select, stat, n, sterr, R)

      results <- calculate_s_limits(select, stat, n, sterr, R)

      HTML(c(
        paste("<b>Control Limit Calculations: ", withMathJax("$s$"), "</b>"),
        "<br><br>",
        paste("Limits based on ", names(choice_r_limits)[select], " = ", ro(stat, R), "<br>"),
        paste("Standard Errors = ", sterr),
        "<br><br>",
        "<table>",
        "<tr>",
        "<td>", paste(withMathJax("$UCL=$")), "</td>",
        "<td align='left'>", results$UCL, "</td>",
        "<td></td>",
        "<td>", paste(withMathJax("$\\hat{\\sigma}=$")), "</td>",
        "<td align='left'>", results$sig_est, "</td>",
        "</tr>",
        "<tr>",
        "<td>", paste(results$stat_symb), "</td>",
        "<td align='left'>", results$centerline, "</td>",
        "<td></td>",
        "<td>", paste(withMathJax("$n=$")), "</td>",
        "<td align='left'>", results$n, "</td>",
        "</tr>",
        "<tr>",
        "<tr>",
        "<td>", paste(withMathJax("$LCL =$")), "</td>",
        "<td>", results$LCL, "</td></tr>",
        "</table>"
      ))
    })

    # s² output
    output$s2_lim_out <- renderUI({
      select <- as.numeric(input$s2_lim_stat)
      stat <- as.numeric(input$s2_lim_val)
      n <- as.numeric(input$s2_lim_n)
      sterr <- as.numeric(input$s2_lim_sterr)
      R <- input$s2_lim_decimals

      req(select, stat, n, sterr, R)

      results <- calculate_s2_limits(select, stat, n, sterr, R)

      HTML(c(
        paste("<b>Control Limit Calculations: ", withMathJax("$s^2$"), "</b>"),
        "<br><br>",
        paste("Limits based on ", names(choice_r_limits)[select], " = ", ro(stat, R), "<br>"),
        paste("Standard Errors = ", sterr),
        "<br><br>",
        "<table>",
        "<tr>",
        "<td>", paste(withMathJax("$UCL=$")), "</td>",
        "<td align='left'>", results$UCL, "</td>",
        "<td></td>",
        "<td>", paste(withMathJax("$\\hat{\\sigma}=$")), "</td>",
        "<td align='left'>", results$sig_est, "</td>",
        "</tr>",
        "<tr>",
        "<td>", paste(results$stat_symb), "</td>",
        "<td align='left'>", results$centerline, "</td>",
        "<td></td>",
        "<td>", paste(withMathJax("$n=$")), "</td>",
        "<td align='left'>", results$n, "</td>",
        "</tr>",
        "<tr>",
        "<tr>",
        "<td>", paste(withMathJax("$LCL =$")), "</td>",
        "<td>", results$LCL, "</td></tr>",
        "</table>"
      ))
    })

    # p output
    output$p_lim_out <- renderUI({
      select <- as.numeric(input$p_lim_stat)
      stat <- as.numeric(input$p_lim_value)
      n <- as.numeric(input$p_lim_n)
      sterr <- as.numeric(input$p_lim_sterr)
      R <- input$p_lim_decimals

      req(select, stat, n, sterr, R)

      results <- calculate_p_limits(select, stat, n, sterr, R)

      HTML(c(
        paste("<b>Control Limit Calculations: ", withMathJax("$p$"), "</b>"),
        "<br><br>",
        paste("Limits are ", results$method_label, withMathJax("$\\bar{p}$"), " = ", ro(stat, R), "<br>"),
        paste("Standard Errors = ", sterr),
        "<br><br>",
        "<table>",
        "<tr>",
        "<td>", paste(withMathJax("$UCL=$")), "</td>",
        "<td align='left'>", results$UCL, "</td>",
        "<td></td>",
        "<td>", paste(withMathJax("$SE_{norm}=$")), "</td>",
        "<td align='left'>", results$sig_est, "</td>",
        "</tr>",
        "<tr>",
        "<td>", paste(results$stat_symb), "</td>",
        "<td align='left'>", results$centerline, "</td>",
        "<td></td>",
        "<td>", paste(withMathJax("$n=$")), "</td>",
        "<td align='left'>", results$n, "</td>",
        "</tr>",
        "<tr>",
        "<tr>",
        "<td>", paste(withMathJax("$LCL =$")), "</td>",
        "<td>", results$LCL, "</td></tr>",
        "</table>"
      ))
    })

    # np output
    output$np_lim_out <- renderUI({
      select <- as.numeric(input$np_lim_stat)
      stat <- as.numeric(input$np_lim_value)
      n <- as.numeric(input$np_lim_n)
      sterr <- as.numeric(input$np_lim_sterr)
      R <- input$np_lim_decimals

      req(select, stat, n, sterr, R)

      results <- calculate_np_limits(select, stat, n, sterr, R)

      HTML(c(
        paste("<b>Control Limit Calculations: ", withMathJax("$np$"), "</b>"),
        "<br><br>",
        paste("Limits are ", results$method_label, withMathJax("$\\overline{np}$"), " = ", ro(stat, R), "<br>"),
        paste("Standard Errors = ", sterr),
        "<br><br>",
        "<table>",
        "<tr>",
        "<td>", paste(withMathJax("$UCL=$")), "</td>",
        "<td align='left'>", results$UCL, "</td>",
        "<td></td>",
        "<td>", paste(withMathJax("$SE_{norm}=$")), "</td>",
        "<td align='left'>", results$sig_est, "</td>",
        "</tr>",
        "<tr>",
        "<td>", paste(results$stat_symb), "</td>",
        "<td align='left'>", results$centerline, "</td>",
        "<td></td>",
        "<td>", paste(withMathJax("$n=$")), "</td>",
        "<td align='left'>", results$n, "</td>",
        "</tr>",
        "<tr>",
        "<tr>",
        "<td>", paste(withMathJax("$LCL =$")), "</td>",
        "<td>", results$LCL, "</td></tr>",
        "</table>"
      ))
    })

    # c output
    output$c_lim_out <- renderUI({
      select <- as.numeric(input$c_lim_stat)
      stat <- as.numeric(input$c_lim_value)
      sterr <- as.numeric(input$c_lim_sterr)
      R <- input$c_lim_decimals

      req(select, stat, sterr, R)

      results <- calculate_c_limits(select, stat, sterr, R)

      HTML(c(
        paste("<b>Control Limit Calculations: ", withMathJax("$c$"), "</b>"),
        "<br><br>",
        paste("Limits are ", results$method_label, withMathJax("$\\bar{c}$"), " = ", ro(stat, R), "<br>"),
        paste("Standard Errors = ", sterr),
        "<br><br>",
        "<table>",
        "<tr>",
        "<td>", paste(withMathJax("$UCL=$")), "</td>",
        "<td align='left'>", results$UCL, "</td>",
        "<td></td>",
        "<td>", paste(withMathJax("$SE_{norm}=$")), "</td>",
        "<td align='left'>", results$sig_est, "</td>",
        "</tr>",
        "<tr>",
        "<td>", paste(results$stat_symb), "</td>",
        "<td align='left'>", results$centerline, "</td>",
        "</tr>",
        "<tr>",
        "<tr>",
        "<td>", paste(withMathJax("$LCL =$")), "</td>",
        "<td>", results$LCL, "</td></tr>",
        "</table>"
      ))
    })

    # u output
    output$u_lim_out <- renderUI({
      select <- as.numeric(input$u_lim_stat)
      stat <- as.numeric(input$u_lim_value)
      n <- as.numeric(input$u_lim_n)
      sterr <- as.numeric(input$u_lim_sterr)
      R <- input$u_lim_decimals

      req(select, stat, n, sterr, R)

      results <- calculate_u_limits(select, stat, n, sterr, R)

      HTML(c(
        paste("<b>Control Limit Calculations: ", withMathJax("$u$"), "</b>"),
        "<br><br>",
        paste("Limits are ", results$method_label, withMathJax("$\\bar{u}$"), " = ", ro(stat, R), "<br>"),
        paste("Standard Errors = ", sterr),
        "<br><br>",
        "<table>",
        "<tr>",
        "<td>", paste(withMathJax("$UCL=$")), "</td>",
        "<td align='left'>", results$UCL, "</td>",
        "<td></td>",
        "<td>", paste(withMathJax("$SE_{norm}=$")), "</td>",
        "<td align='left'>", results$sig_est, "</td>",
        "</tr>",
        "<tr>",
        "<td>", paste(results$stat_symb), "</td>",
        "<td align='left'>", results$centerline, "</td>",
        "<td></td>",
        "<td>", paste(withMathJax("$n=$")), "</td>",
        "<td align='left'>", results$n, "</td>",
        "</tr>",
        "<tr>",
        "<tr>",
        "<td>", paste(withMathJax("$LCL =$")), "</td>",
        "<td>", results$LCL, "</td></tr>",
        "</table>"
      ))
    })

    # kappa output
    output$kappa_lim_out <- renderUI({
      data <- filtered_data()
      UI1 <- as.numeric(input$kappa_limits_k)
      UI2 <- as.numeric(input$kappa_limits_v)
      std_err <- input$kappa_sterr
      R <- input$kappa_decimals

      req(data, UI1, UI2, std_err, R)

      results <- calculate_kappa_limits(data, UI1, UI2, std_err, R)

      if (!is.null(results$error)) {
        HTML(paste0("<p style='color:red'>", results$error, "</p>"))
      } else {
        HTML(c(
          paste("<b>Control Limit Calculations: ", withMathJax("$\\kappa$"), "</b>"),
          "<br><br>",
          paste("Limits are based on observed ", withMathJax("$\\kappa$"), "and", withMathJax("$V_{m}$"), "<br>"),
          paste("Standard Errors = ", std_err),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$UCL=$")), "</td>",
          "<td align='left'>", results$UCL, "</td>",
          "<td></td>",
          "<td>", paste(withMathJax("$SE_{\\kappa}=$")), "</td>",
          "<td align='left'>", results$sd_kappa, "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste0(withMathJax("$\\hat{\\kappa}^{'}$")), "</td>",
          "<td align='left'>", results$centerline, "</td>",
          "<td></td>",
          "<td>", paste(withMathJax("$k=$")), "</td>",
          "<td align='left'>", results$k, "</td>",
          "</tr>",
          "<tr>",
          "<tr>",
          "<td>", paste(withMathJax("$LCL =$")), "</td>",
          "<td>", results$LCL, "</td></tr>",
          "</table>"
        ))
      }
    })

    # kappa critical output
    output$kappa_crit_out <- renderUI({
      po <- input$kappa_cap_po
      cp <- input$kappa_cap_cp
      R <- input$kappa_decimals

      req(po, cp, R)

      results <- calculate_kappa_critical(po, cp, R)

      HTML(paste0(
        "<br><b>Discrete Gauge Capability</b><br>",
        withMathJax("$P_{chance}=$"), results$p_chance, "<br>",
        withMathJax("$\\kappa_{critical}=$"), results$kappa_crit,
        "<p>If the upper 90% confidence interval for \u03BA is below ",
        withMathJax("$\\kappa_{critical}$"), " the discrete gauge is not capable at ",
        withMathJax("$P_{0}=$"), results$po
      ))
    })

    # -------------------------------------------------------------------------
    # HTML Output Rendering for Capability Calculations
    # -------------------------------------------------------------------------

    output$capability_calc_out <- renderUI({
      distribution <- as.numeric(input$cap_dist)
      study <- as.numeric(input$cap_study)
      usl <- as.numeric(input$cap_usl)
      target <- as.numeric(input$cap_target)
      lsl <- as.numeric(input$cap_lsl)
      mean_val <- as.numeric(input$cap_mean)
      sd_val <- as.numeric(input$cap_sd)
      upl <- as.numeric(input$cap_upl)
      lpl <- as.numeric(input$cap_lpl)
      pct_usl <- as.numeric(input$cap_pct_usl)
      pct_lsl <- as.numeric(input$cap_pct_lsl)
      R <- input$cap_decimals

      req(distribution, study, mean_val, sd_val, R)

      results <- calculate_capability_measures(
        distribution = distribution,
        study = study,
        usl = usl,
        target = target,
        lsl = lsl,
        mean = mean_val,
        sd = sd_val,
        upl = upl,
        lpl = lpl,
        pct_usl = pct_usl,
        pct_lsl = pct_lsl,
        R = R
      )

      if (!is.null(results$error)) {
        return(HTML(paste0("<p>", results$error, "</p>")))
      }

      HTML(c(
        paste(
          "<b>Capability Calculations</b> (",
          results$distribution_label, ", ",
          results$study_label, ")"
        ),
        "<br><br>",
        "<u>Capability Measures</u>",
        "<table>",
        "<tr><td>C<sub>p</sub> = </td><td style='text-align:left'>", results$cp, "</td></tr>",
        "<tr><td>C<sub>pk</sub> = </td><td style='text-align:left'>", results$cpk, "</td></tr>",
        "<tr><td>C<sub>pm</sub> = </td><td style='text-align:left'>", results$cpm, "</td></tr>",
        "</table>"
      ))
    })
  })
}

