# Boxplots Server Component
# This graphics worker module follows the new architectural patterns and returns reactive functions only

# =============================================================================
# IMPORTS
# =============================================================================
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lolcat)

# =============================================================================
# GRAPHICS WORKER SERVER FUNCTION
# =============================================================================
create_boxplots_server <- function(id, data_source, original_data_source, data_type_reactive, input_values, reactive_color_palette) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================================
    # INPUT VALIDATION AND EXTRACTION
    # =========================================================================
    # Extract inputs with validation (following graphics template pattern)
    plot_width <- reactive({
      input_vals <- input_values()
      width_val <- input_vals$box_width
      if (is.null(width_val)) {
        return(400)  # Default size
      }
      width_val  # Use slider value directly
    })
    
    plot_height <- reactive({
      input_vals <- input_values()
      height_val <- input_vals$box_height
      if (is.null(height_val)) {
        return(400)  # Default size
      }
      height_val  # Use slider value directly
    })
    
    box_violin <- reactive({
      input_vals <- input_values()
      violin <- input_vals$box_violin
      if (is.null(violin)) return(FALSE)
      violin
    })
    
    notch_box <- reactive({
      input_vals <- input_values()
      notch <- input_vals$notch_box
      if (is.null(notch)) return(FALSE)
      notch
    })
    
    box_title <- reactive({
      input_vals <- input_values()
      title <- input_vals$box_title
      if (is.null(title) || title == "") return("")
      title
    })
    
    box_xlab <- reactive({
      input_vals <- input_values()
      xlab <- input_vals$box_xlab
      if (is.null(xlab) || xlab == "") return("")
      xlab
    })
    
    box_ylab <- reactive({
      input_vals <- input_values()
      ylab <- input_vals$box_ylab
      if (is.null(ylab) || ylab == "") return("Value")
      ylab
    })
    
    box_big <- reactive({
      input_vals <- input_values()
      big <- input_vals$box_big
      if (is.null(big)) return(FALSE)
      big
    })
    
    box_outliers <- reactive({
      input_vals <- input_values()
      outliers <- input_vals$box_outliers
      if (is.null(outliers)) return(FALSE)
      outliers
    })
    
    box_jitter <- reactive({
      input_vals <- input_values()
      jitter <- input_vals$box_jitter
      if (is.null(jitter)) return(FALSE)
      jitter
    })
    
    data_list_for_box <- reactive({
      input_vals <- input_values()
      data_col <- input_vals$data_list_for_box
      if (is.null(data_col)) {
        return(NULL)
      }
      data_col
    })
    
    # =========================================================================
    # DATA PROCESSING REACTIVE
    # =========================================================================
    # Data is already processed by coordinator, just return it directly
    processed_data <- reactive({
      data <- data_source()
      req(data)
      data
    })
    
    # =========================================================================
    # HELPER: Build standardized long-form plot data (ID, Data) with outlier
    # detection.  Returns list(plot_data, outliers, wild_outliers, stats).
    # =========================================================================
    build_boxplot_data <- function(data, data_type, selections, data_col) {
      if (data_type == 1) {
        # Column analysis
        if (ncol(data) == 1) {
          clean_vals <- eda_safe_numeric(data[, 1])
          clean_vals <- clean_vals[!is.na(clean_vals)]
          if (length(clean_vals) == 0) return(NULL)
          plot_data <- data.frame(ID = names(data)[1], Data = clean_vals,
                                 stringsAsFactors = FALSE)
        } else {
          num_data <- as.data.frame(lapply(data, eda_safe_numeric))
          data_long <- num_data %>%
            tidyr::pivot_longer(everything(), names_to = "variable", values_to = "value")
          data_long <- data_long[which(!is.na(data_long$value)), ]
          if (nrow(data_long) == 0) return(NULL)
          plot_data <- data.frame(ID = data_long$variable, Data = data_long$value,
                                 stringsAsFactors = FALSE)
        }
      } else if (data_type == 2) {
        if (is.null(data_col)) return(NULL)
        factors <- as.numeric(selections$eda_UI1)
        data_col_idx <- as.numeric(selections$eda_UI2)[as.numeric(data_col)]
        group_var <- interaction(data[, factors, drop = FALSE], sep = ", ")
        dep_var <- eda_safe_numeric(data[, data_col_idx])
        plot_data <- data.frame(Data = dep_var, ID = group_var, stringsAsFactors = FALSE)
      } else {
        return(NULL)
      }

      plot_data$ID <- factor(plot_data$ID)
      plot_data <- plot_data[which(!is.na(plot_data$Data)), ]
      if (nrow(plot_data) == 0) return(NULL)

      # Outlier detection (type = 6 to match original app)
      wild_outliers <- plot_data %>%
        dplyr::group_by(ID) %>%
        dplyr::filter(Data < quantile(Data, probs = .25, type = 6) - 3 * IQR(Data, type = 6) |
                      Data > quantile(Data, probs = .75, type = 6) + 3 * IQR(Data, type = 6))

      outliers <- plot_data %>%
        dplyr::group_by(ID) %>%
        dplyr::filter(Data < quantile(Data, probs = .25, type = 6) - 1.5 * IQR(Data, type = 6) |
                      Data > quantile(Data, probs = .75, type = 6) + 1.5 * IQR(Data, type = 6)) %>%
        dplyr::anti_join(wild_outliers, by = c("ID", "Data"))

      # Per-group summary stats for hover
      stats <- plot_data %>%
        dplyr::group_by(ID) %>%
        dplyr::summarise(
          Q1     = quantile(Data, probs = .25, type = 6),
          Median = median(Data),
          Q3     = quantile(Data, probs = .75, type = 6),
          IQR    = Q3 - Q1,
          Inner_Lower = Q1 - 1.5 * IQR,
          Inner_Upper = Q3 + 1.5 * IQR,
          Outer_Lower = Q1 - 3 * IQR,
          Outer_Upper = Q3 + 3 * IQR,
          .groups = "drop"
        )

      list(plot_data = plot_data, outliers = outliers,
           wild_outliers = wild_outliers, stats = stats)
    }

    # =========================================================================
    # PLOT REACTIVE (following graphics template pattern)
    # =========================================================================

    # Generate title following original app pattern
    generate_title <- function(data_type, data, UI1, UI2, data_col, x_lab, y_lab, title, violin = FALSE) {
      if (data_type == 1) {
        combo <- "ID"
        dataname <- "Data"
        if (isTruthy(x_lab)) combo <- x_lab
        if (isTruthy(y_lab)) dataname <- y_lab
        if (violin) {
          comboname <- paste0("Violin Plot of ", dataname, " by ", combo)
        } else {
          comboname <- paste0("Boxplot of ", dataname, " by ", combo)
        }
      } else {
        UI1_numeric <- as.numeric(UI1)
        combo <- paste0(names(data)[UI1_numeric], collapse = ", ")
        if (isTruthy(x_lab)) combo <- x_lab
        dataname <- names(data)[data_col]
        if (isTruthy(y_lab)) dataname <- y_lab
        if (violin) {
          comboname <- paste0("Violin Plot of ", dataname, " by ", combo)
        } else {
          comboname <- paste0("Boxplot of ", dataname, " by ", combo)
        }
      }
      if (isTruthy(title)) comboname <- title
      return(comboname)
    }

    box_plot <- reactive({
      data <- processed_data()
      data_type <- data_type_reactive()
      selections <- input_values()
      colors <- reactive_color_palette()
      data_col <- data_list_for_box()
      
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      color_palette <- unname(colors)
      
      tryCatch({
        bp <- build_boxplot_data(data, data_type, selections, data_col)
        if (is.null(bp)) return(NULL)

        plot_data     <- bp$plot_data
        outliers      <- bp$outliers
        wild_outliers <- bp$wild_outliers

        if (data_type == 1) {
          plot_title <- generate_title(data_type, data, selections$eda_UI1,
                                      selections$eda_UI2, 1,
                                      box_xlab(), box_ylab(), box_title(), box_violin())

          p <- ggplot(plot_data, aes(x = ID, y = Data))

          if (box_violin()) {
            p <- p +
              geom_violin(fill = color_palette[2], alpha = 0.5, bw = "sj") +
              stat_summary(fun = "mean", geom = "point", size = 4)
          } else {
            p <- p +
              stat_boxplot(geom = "errorbar", width = 0.5) +
              geom_boxplot(fill = color_palette[2], notch = notch_box(),
                           outlier.color = NA)
          }

          # Explicitly plot outliers so hover hit-testing works
          if (!box_violin() && nrow(outliers) > 0) {
            p <- p + geom_point(data = outliers, shape = 16, size = 2)
          }
          if (!box_violin() && nrow(wild_outliers) > 0) {
            p <- p + geom_point(data = wild_outliers, shape = "*", size = 8,
                                color = color_palette[1])
          }

          p <- p +
            theme_minimal() +
            labs(title = plot_title,
                 x = ifelse(box_xlab() == "", "ID", box_xlab()),
                 y = ifelse(box_ylab() == "", "Data", box_ylab()))

          if (ncol(data) > 1) {
            p <- p +
              scale_x_discrete() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          }

        } else if (data_type == 2) {
          data_col_idx <- as.numeric(selections$eda_UI2)[as.numeric(data_col)]
          plot_title <- generate_title(data_type, data, selections$eda_UI1,
                                      selections$eda_UI2, data_col_idx,
                                      box_xlab(), box_ylab(), box_title(), box_violin())

          p <- ggplot(plot_data, aes(x = ID, y = Data))

          if (box_violin()) {
            p <- p +
              geom_violin(fill = color_palette[2], alpha = 0.5, bw = "sj") +
              stat_summary(fun = "mean", geom = "point", size = 4)
          } else {
            p <- p +
              stat_boxplot(geom = "errorbar", width = 0.5) +
              geom_boxplot(notch = notch_box(), fill = color_palette[2],
                           outlier.color = NA)
          }

          p <- p +
            ggtitle(plot_title) +
            scale_x_discrete() +
            geom_point(data = outliers, shape = 16, size = 2) +
            geom_point(data = wild_outliers, shape = "*", size = 8,
                       color = color_palette[1]) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(x = ifelse(box_xlab() == "",
                            if (data_type == 2) paste0(names(data)[as.numeric(selections$eda_UI1)], collapse = ", ") else "ID",
                            box_xlab()),
                 y = ifelse(box_ylab() == "", "Data", box_ylab()))

          if (box_big()) {
            p <- p +
              theme(axis.title = element_text(size = rel(1.5)),
                    axis.text = element_text(size = rel(1.5)),
                    plot.title = element_text(size = rel(1.5)))
          }
        } else {
          return(NULL)
        }
        
        p
      }, error = function(e) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message), size = 5) +
          theme_void()
      })
    })

    # =========================================================================
    # HOVER DATA REACTIVE — provides fence_samples + outlier_points for
    # nearPoints() hit-testing in the coordinator tooltip.
    # =========================================================================
    box_plot_hover_data <- reactive({
      # No hover for violin mode
      if (box_violin()) return(NULL)

      data <- processed_data()
      data_type <- data_type_reactive()
      selections <- input_values()
      data_col <- data_list_for_box()

      if (is.null(data) || nrow(data) == 0) return(NULL)

      bp <- build_boxplot_data(data, data_type, selections, data_col)
      if (is.null(bp)) return(NULL)

      stats         <- bp$stats
      outliers      <- bp$outliers
      wild_outliers <- bp$wild_outliers
      plot_data     <- bp$plot_data

      # ----- fence_samples: synthetic points spanning the box/whisker region
      # so nearPoints can match anywhere within that vertical range.
      # We sample a handful of y-values between the inner whisker endpoints.
      fence_rows <- lapply(seq_len(nrow(stats)), function(i) {
        s <- stats[i, ]
        # Actual whisker endpoints: clamped to data range within inner fences
        grp_vals <- plot_data$Data[plot_data$ID == s$ID]
        whisker_lo <- min(grp_vals[grp_vals >= s$Inner_Lower], na.rm = TRUE)
        whisker_hi <- max(grp_vals[grp_vals <= s$Inner_Upper], na.rm = TRUE)
        y_pts <- unique(c(whisker_lo, s$Q1, s$Median, s$Q3, whisker_hi))
        data.frame(
          ID          = s$ID,
          Data        = y_pts,
          Q1          = s$Q1,
          Median      = s$Median,
          Q3          = s$Q3,
          IQR         = s$IQR,
          Inner_Lower = s$Inner_Lower,
          Inner_Upper = s$Inner_Upper,
          Outer_Lower = s$Outer_Lower,
          Outer_Upper = s$Outer_Upper,
          stringsAsFactors = FALSE
        )
      })
      fence_samples <- do.call(rbind, fence_rows)
      fence_samples$ID <- factor(fence_samples$ID, levels = levels(bp$plot_data$ID))

      # ----- outlier_points
      build_outlier_df <- function(df, kind) {
        if (is.null(df) || nrow(df) == 0) {
          return(data.frame(ID = factor(levels = levels(bp$plot_data$ID)),
                            Data = numeric(0), outlier_kind = character(0),
                            stringsAsFactors = FALSE))
        }
        data.frame(ID = df$ID, Data = df$Data, outlier_kind = kind,
                   stringsAsFactors = FALSE)
      }
      outlier_points <- rbind(
        build_outlier_df(as.data.frame(outliers), "inner"),
        build_outlier_df(as.data.frame(wild_outliers), "wild")
      )
      if (nrow(outlier_points) > 0) {
        outlier_points$ID <- factor(outlier_points$ID, levels = levels(bp$plot_data$ID))
      }

      list(fence_samples = fence_samples, outlier_points = outlier_points)
    })
    
    # =========================================================================
    # RETURN REACTIVE FUNCTIONS (coordinator will render these)
    # =========================================================================
    list(
      # Plot reactives
      box_plot = box_plot,
      box_width = plot_width,
      box_height = plot_height,
      
      # Hover data
      box_plot_hover_data = box_plot_hover_data,
      
      # Input reactives
      box_violin = box_violin,
      notch_box = notch_box,
      box_title = box_title,
      box_xlab = box_xlab,
      box_ylab = box_ylab
    )
  })
}

# =============================================================================
# ARCHITECTURAL PATTERNS USED
# =============================================================================
# 1. Graphics Worker Pattern: Contains only plotting logic, no UI rendering
# 2. Reactive Functions: Returns reactive functions for coordinator to render
# 3. Input Validation: Validates inputs with sensible defaults
# 4. Data Processing: Processes data based on coordinator inputs
# 5. Error Handling: Graceful handling of missing/invalid data
# 6. Template Compliance: Follows established patterns from graphics template