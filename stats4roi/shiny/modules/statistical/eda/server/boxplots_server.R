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
    # PLOT REACTIVE (following graphics template pattern)
    # =========================================================================
    box_plot <- reactive({
      data <- processed_data()
      data_type <- data_type_reactive()
      selections <- input_values()
      colors <- reactive_color_palette()
      data_col <- data_list_for_box()
      
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      # Get color palette
      color_palette <- unname(colors)
      
      # Generate title following original app pattern
      generate_title <- function(data_type, data, UI1, UI2, data_col, x_lab, y_lab, title, violin = FALSE) {
        if (data_type == 1) {
          # Column analysis - always use "ID" for combo (like original app)
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
          # Factor analysis
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
        
        # Override with custom title if provided
        if (isTruthy(title)) comboname <- title
        return(comboname)
      }
      
      tryCatch({
        if (data_type == 1) {
          # Column analysis - data contains only the selected columns
          if (ncol(data) == 1) {
            # Single column - simple boxplot
            plot_title <- generate_title(data_type, data, selections$eda_UI1, selections$eda_UI2, 
                                       1, box_xlab(), box_ylab(), box_title(), box_violin())
            
            # Filter out NAs (following original app pattern)
            clean_data <- data[which(!is.na(data[,1])), , drop = FALSE]
            
            # Create data frame with column name as x-axis label (like original app)
            plot_data <- data.frame(
              ID = names(data)[1],  # Use actual column name
              Data = clean_data[,1]
            )
            
            p <- ggplot(plot_data, aes(x = ID, y = Data))
            
            if (box_violin()) {
              # Violin plot (replaces boxplot, like original app)
              p <- p +
                geom_violin(fill = color_palette[2], alpha = 0.5, bw = "sj") +
                stat_summary(fun = "mean", geom = "point", size = 4)
            } else {
              # Boxplot
              p <- p +
                stat_boxplot(geom = "errorbar", width = 0.5) +  # Whisker caps
                geom_boxplot(
                  fill = color_palette[2], 
                  notch = notch_box(),
                  outlier.color = color_palette[1]
                )
            }
            
            p <- p +
              theme_minimal() +
              labs(
                title = plot_title,
                x = ifelse(box_xlab() == "", "ID", box_xlab()),
                y = ifelse(box_ylab() == "", "Data", box_ylab())
              )
          } else {
            # Multiple columns - grouped boxplot
            plot_title <- generate_title(data_type, data, selections$eda_UI1, selections$eda_UI2, 
                                       1, box_xlab(), box_ylab(), box_title(), box_violin())
            
            data_long <- data %>%
              tidyr::pivot_longer(everything(), names_to = "variable", values_to = "value")
            
            # Filter out NAs (following original app pattern)
            data_long <- data_long[which(!is.na(data_long$value)), ]
            
            p <- ggplot(data_long, aes(x = variable, y = value))
            
            if (box_violin()) {
              # Violin plot (replaces boxplot, like original app)
              p <- p +
                geom_violin(fill = color_palette[2], alpha = 0.5, bw = "sj") +
                stat_summary(fun = "mean", geom = "point", size = 4)
            } else {
              # Boxplot
              p <- p +
                stat_boxplot(geom = "errorbar", width = 0.5) +  # Whisker caps
                geom_boxplot(
                  fill = color_palette[2], 
                  notch = notch_box(),
                  outlier.color = color_palette[1]
                )
            }
            
            p <- p +
              theme_minimal() +
              labs(
                title = plot_title,
                x = ifelse(box_xlab() == "", "ID", box_xlab()),
                y = ifelse(box_ylab() == "", "Data", box_ylab())
              ) +
              scale_x_discrete() +  # This shows the original column names as tick marks
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          }
        } else if (data_type == 2) {
          # Factor analysis - following original app pattern exactly
          if (is.null(data_col)) {
            return(NULL)
          }
          
          # Get factor and data column indices (following original app)
          factors <- as.numeric(selections$eda_UI1)  # Factor columns
          data_col_idx <- as.numeric(selections$eda_UI2)[as.numeric(data_col)]  # Selected data column
          
          # Always use interaction (monolithic pattern) so numeric factors are discrete x levels
          group_var <- interaction(data[, factors, drop = FALSE], sep = ", ")
          
          # Get dependent variable (following original app)
          dep_var <- data[, data_col_idx]
          
          # Generate title for factor analysis
          plot_title <- generate_title(data_type, data, selections$eda_UI1, selections$eda_UI2, 
                                     data_col_idx, box_xlab(), box_ylab(), box_title(), box_violin())
          
          # Create plot data exactly like original app
          plot_data <- data.frame(
            Data = dep_var,
            ID = group_var,
            stringsAsFactors = FALSE
          )
          plot_data$ID <- factor(plot_data$ID)
          
          # Filter out NAs (following original app pattern)
          plot_data <- plot_data[which(!is.na(plot_data$Data)), ]
          
          if (nrow(plot_data) == 0) {
            return(NULL)
          }
          
          # Outlier detection (following original app pattern)
          wild_outliers <- plot_data %>%
            group_by(ID) %>%
            filter(Data < quantile(Data, probs = .25, type = 6) - 3 * IQR(Data, type = 6) |
                   Data > quantile(Data, probs = .75, type = 6) + 3 * IQR(Data, type = 6))
          
          outliers <- plot_data %>%
            group_by(ID) %>%
            filter(Data < quantile(Data, probs = .25, type = 6) - 1.5 * IQR(Data, type = 6) |
                   Data > quantile(Data, probs = .75, type = 6) + 1.5 * IQR(Data, type = 6)) %>%
            anti_join(wild_outliers, by = c("ID", "Data"))
          
          p <- ggplot(plot_data, aes(x = ID, y = Data))
          
          if (box_violin()) {
            # Violin plot (replaces boxplot, like original app)
            p <- p +
              geom_violin(fill = color_palette[2], alpha = 0.5, bw = "sj") +
              stat_summary(fun = "mean", geom = "point", size = 4)
          } else {
            # Boxplot (exactly like original app)
            p <- p +
              stat_boxplot(geom = "errorbar", width = 0.5) +  # Inner fences
              geom_boxplot(notch = notch_box(), fill = color_palette[2], outlier.color = NA)
          }
          
          # Add outliers and styling (exactly like original app)
          p <- p +
            ggtitle(plot_title) +
            scale_x_discrete() +
            geom_point(data = outliers, shape = 16, size = 2) +
            geom_point(data = wild_outliers, shape = "*", size = 8, color = color_palette[1]) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(x = ifelse(box_xlab() == "", 
                            if (data_type == 2) paste0(names(data)[as.numeric(selections$eda_UI1)], collapse = ", ") else "ID", 
                            box_xlab()),
                 y = ifelse(box_ylab() == "", "Data", box_ylab()))
          
          # Big font option (following original app)
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
        # Return error plot
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message), size = 5) +
          theme_void()
      })
    })
    
    # =========================================================================
    # RETURN REACTIVE FUNCTIONS (coordinator will render these)
    # =========================================================================
    list(
      # Plot reactives
      box_plot = box_plot,
      box_width = plot_width,
      box_height = plot_height,
      
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