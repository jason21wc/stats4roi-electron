# Histograms Server Component
# This graphics worker module follows the new architectural patterns and returns reactive functions only

# =============================================================================
# IMPORTS
# =============================================================================
library(shiny)
library(ggplot2)
library(dplyr)
library(lolcat)  # For transform.dependent.format.to.independent.format function

# =============================================================================
# GRAPHICS WORKER SERVER FUNCTION
# =============================================================================
create_histograms_server <- function(id, data_source, data_type_reactive, input_values, reactive_color_palette) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================================
    # INPUT VALIDATION AND EXTRACTION
    # =========================================================================
    plot_width <- reactive({
      input_vals <- input_values()
      width_val <- input_vals$hist_width
      if (is.null(width_val)) return(400)  # Default size
      width_val  # Use slider value directly
    })
    
    plot_height <- reactive({
      input_vals <- input_values()
      height_val <- input_vals$hist_height
      if (is.null(height_val)) return(400)  # Default size
      height_val  # Use slider value directly
    })
    
    hist_type <- reactive({
      input_vals <- input_values()
      type <- input_vals$hist_type
      if (is.null(type)) return(1)
      type
    })
    
    norm_curve <- reactive({
      input_vals <- input_values()
      curve <- input_vals$norm_curve
      if (is.null(curve)) return(FALSE)
      curve
    })
    
    mult_data_choice <- reactive({
      input_vals <- input_values()
      mult <- input_vals$mult_data_choice
      if (is.null(mult)) return(TRUE)
      mult
    })
    
    combine_data_choice <- reactive({
      input_vals <- input_values()
      combine <- input_vals$combine_data_choice
      if (is.null(combine)) return(FALSE)
      combine
    })
    
    hist_bins <- reactive({
      input_vals <- input_values()
      bins <- input_vals$hist_bins
      if (is.null(bins) || is.na(bins)) return(15)
      bins
    })
    
    hist_title <- reactive({
      input_vals <- input_values()
      title <- input_vals$hist_title
      if (is.null(title) || title == "") return(NA)
      title
    })
    
    hist_x_lab <- reactive({
      input_vals <- input_values()
      xlab <- input_vals$hist_x_lab
      if (is.null(xlab) || xlab == "") return(NA)
      xlab
    })
    
    hist_big <- reactive({
      input_vals <- input_values()
      big <- input_vals$hist_big
      if (is.null(big)) return(FALSE)
      big
    })
    
    # New advanced controls
    hist_specs <- reactive({
      input_vals <- input_values()
      specs <- input_vals$hist_specs
      if (is.null(specs)) return(FALSE)
      specs
    })
    
    hist_LSL <- reactive({
      input_vals <- input_values()
      lsl <- input_vals$hist_LSL
      if (is.null(lsl) || is.na(lsl)) return(NA)
      lsl
    })
    
    hist_target <- reactive({
      input_vals <- input_values()
      target <- input_vals$hist_target
      if (is.null(target) || is.na(target)) return(NA)
      target
    })
    
    hist_USL <- reactive({
      input_vals <- input_values()
      usl <- input_vals$hist_USL
      if (is.null(usl) || is.na(usl)) return(NA)
      usl
    })
    
    hist_bin_w <- reactive({
      input_vals <- input_values()
      bin_w <- input_vals$hist_bin_w
      if (is.null(bin_w) || is.na(bin_w)) return(NULL)
      bin_w
    })
    
    hist_center <- reactive({
      input_vals <- input_values()
      center <- input_vals$hist_center
      if (is.null(center) || is.na(center)) return(NULL)
      center
    })
    
    hist_extend_d <- reactive({
      input_vals <- input_values()
      extend <- input_vals$hist_extend_d
      if (is.null(extend)) return(FALSE)
      extend
    })
    
    hist_rug <- reactive({
      input_vals <- input_values()
      rug <- input_vals$hist_rug
      if (is.null(rug)) return(TRUE)
      rug
    })
    
    hist_freq_dist <- reactive({
      input_vals <- input_values()
      freq_dist <- input_vals$hist_freq_dist
      if (is.null(freq_dist)) return(FALSE)
      freq_dist
    })
    
    freq_dist_dec <- reactive({
      input_vals <- input_values()
      decimals <- input_vals$freq_dist_dec
      if (is.null(decimals)) return(5)
      decimals
    })
    
    hist_panel_filter <- reactive({
      input_vals <- input_values()
      panel_filter <- input_vals$hist_panel_filter
      if (is.null(panel_filter)) return(NULL)
      panel_filter
    })
    
    data_list_for_hist <- reactive({
      input_vals <- input_values()
      data_col <- input_vals$data_list_for_hist
      if (is.null(data_col)) return(NULL)
      data_col
    })
    
    indep_list_hist <- reactive({
      input_vals <- input_values()
      indep_list <- input_vals$indep_list_hist
      if (is.null(indep_list)) return(NULL)
      indep_list
    })
    
    
    # =========================================================================
    # DATA PROCESSING REACTIVE (not used for histograms - we work directly with original data)
    # =========================================================================
    
    # =========================================================================
    # FREQUENCY DISTRIBUTION TABLE REACTIVE
    # =========================================================================
    hist_freq_table <- reactive({
      if (!hist_freq_dist()) {
        return(NULL)
      }
      
      data <- data_source()
      req(data)
      
      data_type <- data_type_reactive()
      selections <- input_values()
      colors <- reactive_color_palette()
      
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      # Get input values
      type <- hist_type()
      if (type == 3) return(NULL)  # No frequency table for density plots
      
      mult_data_val <- mult_data_choice()
      combine_data_val <- combine_data_choice()
      bins <- hist_bins()
      if (is.na(bins)) bins <- 15
      if (is.null(mult_data_val)) mult_data_val <- TRUE
      if (is.null(combine_data_val)) combine_data_val <- FALSE
      bin_w <- hist_bin_w()
      if (!isTruthy(bin_w)) bin_w <- NULL
      center <- hist_center()
      if (!is.numeric(center)) center <- NULL
      data_col <- data_list_for_hist()
      indep_list <- indep_list_hist()
      
      tryCatch({
        if (data_type == 1) {
          # Column analysis
          UI1 <- selections$eda_UI1
          if (is.null(UI1) || length(UI1) == 0) {
            return(NULL)
          }
          
          selected_cols <- as.numeric(UI1)
          plot_data <- data.frame(transform.dependent.format.to.independent.format(data = data[selected_cols]))
          names(plot_data) <- c("ID", "Data")
          plot_data$Data <- suppressWarnings(as.numeric(plot_data$Data))
          
        } else {
          # Factor analysis
          if (is.null(data_col) || is.null(indep_list)) {
            return(NULL)
          }
          
          UI1 <- selections$eda_UI1
          UI2 <- selections$eda_UI2
          
          # Map data_col to original data column (same as descriptives server)
          selected_data_cols <- as.numeric(UI2)
          data_col_name <- colnames(data)[selected_data_cols[as.numeric(data_col)]]
          
          # Get factor column names
          factor_names <- colnames(data)[as.numeric(UI1)]
          
          # Always use interaction (monolithic/boxplots pattern) for discrete factor levels
          factor_cols <- data[, factor_names, drop = FALSE]
          group_var <- interaction(factor_cols, sep = ", ")
          
          # Create plot data directly (same as boxplots server)
          plot_data <- data.frame(
            Data = suppressWarnings(as.numeric(data[, data_col_name])),
            ID = group_var
          )
          
          # Filter to only include selected factor combinations
          selected_combos <- indep_list
          if (!is.null(selected_combos) && length(selected_combos) > 0) {
            plot_data <- plot_data[plot_data$ID %in% selected_combos, ]
          }
          
          # Generate title for factor analysis
          combo <- paste0(factor_names, collapse = ", ")
          dataname <- data_col_name
          comboname <- paste0(dataname, " by ", combo)
        }
        
        # Remove NA values
        plot_data <- plot_data[which(!is.na(plot_data$Data)), ]
        
        if (nrow(plot_data) == 0) {
          return(NULL)
        }
        
        
        # Create temporary plot to extract bin information
        p <- ggplot(plot_data, aes(x = Data))
        
        if (type == 1) {
          p <- p + geom_histogram(binwidth = bin_w, bins = bins, center = center)
          if (combine_data_val) {
            # Pooled histogram — no facets
          } else if (mult_data_val) {
            p <- p + facet_wrap(facets = vars(ID))
          } else {
            p <- p + facet_wrap(facets = vars(ID), scales = "free")
          }
        } else if (type == 2) {
          p <- p + geom_freqpoly(aes(color = ID), binwidth = bin_w, bins = bins, center = center)
          # Frequency polygons: overlaid on one chart (no facets)
        }
        
        # Extract bin information
        graph_info <- ggplot_build(p)
        
        if (length(graph_info$data) > 0 && nrow(graph_info$data[[1]]) > 0) {
          table <- data.frame(
            Panel = graph_info$data[[1]]$PANEL,
            xmin = graph_info$data[[1]]$xmin,
            Center = graph_info$data[[1]]$x,
            xmax = graph_info$data[[1]]$xmax,
            Count = graph_info$data[[1]]$y
          )
          
          # Calculate relative frequency
          temp <- table %>% group_by(Panel) %>% reframe("Rf" = Count/sum(Count))
          table <- cbind(table, temp[2])
          
          # Apply decimal precision
          R <- freq_dist_dec()
          if (!is.null(R) && is.numeric(R)) {
            table$xmin <- round(table$xmin, R)
            table$Center <- round(table$Center, R)
            table$xmax <- round(table$xmax, R)
            table$Rf <- round(table$Rf, R)
          }
          
          # Filter by selected panel if specified
          panel_filter <- hist_panel_filter()
          if (!is.null(panel_filter)) {
            table <- table[table$Panel == panel_filter, ]
          }
          
          return(table)
        }
        
        return(NULL)
      }, error = function(e) {
        return(NULL)
      })
    })
    
    # =========================================================================
    # PLOT REACTIVE (following original app logic exactly)
    # =========================================================================
    hist_plot <- reactive({
      data <- data_source()
      req(data)
      
      data_type <- data_type_reactive()
      selections <- input_values()
      colors <- reactive_color_palette()
      
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      
      # Get input values
      type <- hist_type()
      norm_curve_val <- norm_curve()
      hist_specs_val <- hist_specs()
      hist_LSL_val <- hist_LSL()
      hist_target_val <- hist_target()
      hist_USL_val <- hist_USL()
      mult_data_val <- mult_data_choice()
      combine_data_val <- combine_data_choice()
      bins <- hist_bins()
      if (is.na(bins)) bins <- 15
      if (is.null(mult_data_val)) mult_data_val <- TRUE
      if (is.null(combine_data_val)) combine_data_val <- FALSE
      extend <- hist_extend_d()
      bin_w <- hist_bin_w()
      if (!isTruthy(bin_w)) bin_w <- NULL
      center <- hist_center()
      if (!is.numeric(center)) center <- NULL
      title <- hist_title()
      x_lab <- hist_x_lab()
      big <- hist_big()
      data_col <- data_list_for_hist()
      rug <- hist_rug()
      
      # Get color palette
      color <- unname(colors())
      indep_list <- indep_list_hist()
      
      tryCatch({
        if (data_type == 1) {
          # Column analysis - plot selected columns
          UI1 <- selections$eda_UI1
          if (is.null(UI1) || length(UI1) == 0) {
            return(ggplot() + 
              annotate("text", x = 0.5, y = 0.5, label = "Please select columns for analysis", size = 5) +
              theme_void())
          }
          
          selected_cols <- as.numeric(UI1)
          plot_data <- data.frame(transform.dependent.format.to.independent.format(data = data[selected_cols]))
          names(plot_data) <- c("ID", "Data")
          plot_data$Data <- suppressWarnings(as.numeric(plot_data$Data))
          comboname <- c("Histogram", "Frequency Polygon", "Density")[as.numeric(type)]
          dataname <- "Data"
        } else {
          # Factor analysis
          if (is.null(data_col) || is.null(indep_list)) {
            return(ggplot() + 
              annotate("text", x = 0.5, y = 0.5, label = "Please select data and factors for analysis", size = 5) +
              theme_void())
          }
          
          UI1 <- selections$eda_UI1
          UI2 <- selections$eda_UI2
          
          # Map data_col to original data column (same as descriptives server)
          selected_data_cols <- as.numeric(UI2)
          data_col_name <- colnames(data)[selected_data_cols[as.numeric(data_col)]]
          
          # Get factor column names
          factor_names <- colnames(data)[as.numeric(UI1)]
          
          # Always use interaction (monolithic/boxplots pattern) for discrete factor levels
          factor_cols <- data[, factor_names, drop = FALSE]
          group_var <- interaction(factor_cols, sep = ", ")
          
          # Create plot data directly (same as boxplots server)
          plot_data <- data.frame(
            Data = suppressWarnings(as.numeric(data[, data_col_name])),
            ID = group_var
          )
          
          # Filter to only include selected factor combinations
          selected_combos <- indep_list
          if (!is.null(selected_combos) && length(selected_combos) > 0) {
            plot_data <- plot_data[plot_data$ID %in% selected_combos, ]
          }
          
          # Generate title for factor analysis
          combo <- paste0(factor_names, collapse = ", ")
          dataname <- data_col_name
          comboname <- paste0(dataname, " by ", combo)
        }
        
        # Remove NA values
        plot_data <- plot_data[which(!is.na(plot_data$Data)), ]
        
        # Ensure ID is a factor
        plot_data$ID <- as.factor(plot_data$ID)
        
        if (nrow(plot_data) == 0) {
          return(ggplot() + 
            annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 5) +
            theme_void())
        }
        
        # Apply title and x_lab overrides
        if (isTruthy(title)) comboname <- title
        if (isTruthy(x_lab)) dataname <- x_lab
        
        # Set up legend names
        leg_names <- c("Data" = colors[5])
        
        # Create base plot
        p <- ggplot(plot_data, aes(x = Data))
        
        # Add histogram based on type
        if (type == 1) {
          # Histogram
          if (norm_curve_val) {
            p <- p +
              geom_histogram(aes(fill = "Data", y = after_stat(density)), 
                           binwidth = bin_w, bins = bins, center = center, 
                           color = colors[1]) +
              scale_fill_manual(values = leg_names)
          } else {
            p <- p +
              geom_histogram(aes(fill = "Data"), 
                           binwidth = bin_w, bins = bins, center = center, 
                           color = colors[1]) +
              labs(y = "Count") +
              scale_fill_manual(values = leg_names)
          }
        } else if (type == 2) {
          # Frequency Polygon
          p <- p +
            scale_color_manual(values = rep_len(colors[-c(1,2,3)], 
                                             length.out = length(unique(plot_data$ID))))
          if (norm_curve_val) {
            # With normal curve, use density scale and color by ID
            p <- p +
              geom_freqpoly(aes(color = ID, y = after_stat(density)), 
                          binwidth = bin_w, bins = bins, center = center) +
              labs(y = "Density")
          } else {
            # Without normal curve, use count scale and color by ID
            p <- p +
              geom_freqpoly(aes(color = ID), 
                          binwidth = bin_w, bins = bins, center = center) +
              labs(y = "Count")
          }
        } else if (type == 3) {
          # Density
          if (extend) {
            if (combine_data_val) {
              dense_dat <- density(plot_data$Data, bw = "sj")
              newplot_dat <- data.frame(x = dense_dat$x, y = dense_dat$y)
            } else {
              # Generate base R density per ID
              dense_dat <- by(data = plot_data$Data, INDICES = plot_data$ID, 
                            FUN = density, bw = "sj")
              # Format for ggplot
              newplot_dat <- data.frame("ID" = NA, "x" = NA, "y" = NA)
              for (name in names(dense_dat)) {
                temp <- merge(name, dense_dat[[name]][["x"]])
                names(temp) <- c("ID", "x")
                temp <- cbind(temp, y = dense_dat[[name]][["y"]])
                newplot_dat <- rbind(newplot_dat, temp)
              }
              newplot_dat <- na.omit(newplot_dat)
            }
            
            p <- p +
              geom_area(data = newplot_dat, aes(x = x, y = y, fill = "Data"), 
                       color = colors[1]) +
              labs(y = "Density") +
              scale_fill_manual(values = leg_names)
            if (rug) {
              p <- p + geom_rug(color = colors[6], alpha = 0.5)
            }
          } else {
            p <- p +
              geom_density(aes(fill = "Data"), outline.type = "full", 
                         bw = "sj", trim = FALSE) +
              labs(y = "Density") +
              scale_fill_manual(values = leg_names)
            if (rug) {
              p <- p + geom_rug(color = colors[6], alpha = 0.35)
            }
          }
        }
        
        # Add normal curve if requested
        if (norm_curve_val) {
          if (type != 2) {
            # For histograms and density plots, use stat_theodensity (works correctly with facets)
            leg_names <- c("Normal" = "black")
            p <- p +
              scale_color_manual(values = leg_names) +
              stat_theodensity(aes(color = "Normal"), distri = "norm", size = 1) +
              labs(y = "Density")
          } else {
            # Frequency polygon with normal curve - use stat_theodensity for each ID
            p <- p +
              stat_theodensity(aes(fill = ID), geom = "area", distri = "norm", alpha = 0.2) +
              scale_fill_manual(values = rep_len(colors[-c(1,2,3)], 
                                               length.out = 1 + length(unique(plot_data$ID)))) +
              labs(y = "Density")
          }
        }
        
        # Add specifications if requested
        if (hist_specs_val) {
          if (!is.na(hist_USL_val)) {
            p <- p + geom_vline(xintercept = hist_USL_val, color = colors[2], linetype = 2)
          }
          if (!is.na(hist_LSL_val)) {
            p <- p + geom_vline(xintercept = hist_LSL_val, color = colors[2], linetype = 2)
          }
          if (!is.na(hist_target_val)) {
            p <- p + geom_vline(xintercept = hist_target_val, color = colors[3])
          }
        }
        
        # Facets: frequency polygon stays on one chart; hist/KDE facet or pool
        if (type == 2) {
          # Overlaid frequency polygons — no facets
        } else if (combine_data_val) {
          # Pooled histogram or KDE — no facets
        } else if (mult_data_val) {
          p <- p + facet_wrap(facets = vars(ID))
        } else {
          p <- p + facet_wrap(facets = vars(ID), scales = "free")
        }
        
        # Add sample size labels, title, and styling
        if (combine_data_val && type != 2) {
          temp_n <- data.frame(n = nrow(plot_data))
        } else {
          temp_n <- count(plot_data, ID)
        }
        p <- p +
          ggtitle(comboname) +
          geom_label(data = temp_n, aes(x = -Inf, y = Inf, label = paste0("n = ", n)), 
                    hjust = 0, vjust = "top") +
          theme(axis.text.x = element_text(angle = 45)) +
          labs(color = "", fill = "") +
          xlab(dataname)
        
        
        if (big) {
          p <- p + theme(
            axis.title = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.5)),
            plot.title = element_text(size = rel(1.5))
          )
        }
        
        p
      }, error = function(e) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message), size = 5) +
          theme_void()
      })
    })
    
    # =========================================================================
    # RETURN REACTIVE FUNCTIONS (coordinator will render these)
    # =========================================================================
    list(
      hist_plot = hist_plot,
      hist_freq_table = hist_freq_table,
      hist_width = plot_width,
      hist_height = plot_height,
      
      # Input reactives
      hist_type = hist_type,
      norm_curve = norm_curve,
      hist_specs = hist_specs,
      hist_LSL = hist_LSL,
      hist_target = hist_target,
      hist_USL = hist_USL,
      hist_bin_w = hist_bin_w,
      hist_bins = hist_bins,
      hist_center = hist_center,
      hist_extend_d = hist_extend_d,
      hist_rug = hist_rug,
      mult_data_choice = mult_data_choice,
      combine_data_choice = combine_data_choice,
      hist_freq_dist = hist_freq_dist,
      hist_title = hist_title,
      hist_x_lab = hist_x_lab,
      hist_big = hist_big
    )
  })
}