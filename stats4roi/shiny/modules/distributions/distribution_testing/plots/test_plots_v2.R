# Test Plots v2 - Simplified and Fixed
# This version handles Q-Q plots correctly and follows standardized patterns

# Required libraries
library(ggplot2)
library(dplyr)
library(ggh4x)  # for stat_theodensity

# Create test-specific plots
create_test_plot <- function(test_id, data, UI1, UI2, data_type, colors, plot_type = 2) {
  cat("create_test_plot called with:\n")
  cat("  test_id:", test_id, "\n")
  cat("  data rows:", nrow(data), "\n")
  cat("  UI1:", UI1, "\n")
  cat("  UI2:", UI2, "\n")
  cat("  data_type:", data_type, "\n")
  cat("  plot_type:", plot_type, "\n")
  cat("  colors:", paste(names(colors), "=", colors, collapse = ", "), "\n")
  
  # Basic validation
  if (is.null(data) || nrow(data) == 0) {
    cat("No data available\n")
    return(create_default_test_plot())
  }
  
  # Validate parameters
  if (is.null(UI1) || length(UI1) == 0) {
    cat("No UI1 selection\n")
    return(create_default_test_plot())
  }
  
  if (data_type == 2 && (is.null(UI2) || length(UI2) == 0)) {
    cat("Factor analysis requires UI2 selection\n")
    return(create_default_test_plot())
  }
  
  # Create plot data based on data type
  if (data_type == 1) {
    # Column analysis
    plot_data <- create_column_plot_data(data, UI1)
    comboname <- NULL
  } else {
    # Factor analysis
    plot_data <- create_factor_plot_data(data, UI1, UI2)
    comboname <- attr(plot_data, "comboname")
  }
  
  cat("Plot data created with", nrow(plot_data), "rows\n")
  
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    cat("No plot data created\n")
    return(create_default_test_plot())
  }
  
  # Create the appropriate plot
  if (test_id == 1) {
    cat("Creating Poisson plot\n")
    return(create_poisson_plot(plot_data, colors, plot_type, data_type, comboname))
  } else if (test_id %in% c(2, 3, 4, 5, 9)) {
    cat("Creating normality plot\n")
    return(create_normality_plot(plot_data, colors, plot_type, data_type, comboname))
  } else if (test_id %in% c(6, 7, 8)) {
    cat("Creating exponential plot\n")
    return(create_exponential_plot(plot_data, colors, plot_type, data_type, comboname))
  } else {
    cat("Unknown test_id:", test_id, "\n")
    return(create_default_test_plot())
  }
}

# Create plot data for column analysis
create_column_plot_data <- function(data, UI1) {
  cat("create_column_plot_data called with:\n")
  cat("  data columns:", ncol(data), "\n")
  cat("  UI1:", UI1, "\n")
  cat("  UI1 class:", class(UI1), "\n")
  
  # Transform data to long format
  plot_data <- data.frame()
  
  # Convert UI1 to numeric if it's character
  if (is.character(UI1)) {
    UI1 <- as.numeric(UI1)
  }
  
  cat("  UI1 after conversion:", UI1, "\n")
  cat("  data column names:", names(data), "\n")
  
  for (i in UI1) {
    cat("  Processing column", i, ":", names(data)[i], "\n")
    col_data <- na.omit(data[[i]])
    cat("    Column data length:", length(col_data), "\n")
    if (length(col_data) > 0) {
      plot_data <- rbind(plot_data, data.frame(
        ID = names(data)[i],
        Data = as.numeric(col_data)
      ))
      cat("    Added to plot_data, now has", nrow(plot_data), "rows\n")
    } else {
      cat("    No data after removing NAs\n")
    }
  }
  
  cat("Final plot_data has", nrow(plot_data), "rows\n")
  return(plot_data)
}

# Create plot data for factor analysis
create_factor_plot_data <- function(data, UI1, UI2) {
  cat("create_factor_plot_data called with:\n")
  cat("  UI1:", UI1, "\n")
  cat("  UI2:", UI2, "\n")
  
  # Create factor combinations
  comboname <- paste0(names(data)[UI1], collapse = ", ")
  cat("  comboname:", comboname, "\n")
  
  ID <- interaction(data[UI1], sep = ", ")
  cat("  ID levels:", length(unique(ID)), "\n")
  
  # Transform data to long format
  plot_data <- data.frame()
  for (i in UI2) {
    cat("  Processing data column", i, ":", names(data)[i], "\n")
    
    # Create a data frame with ID and data column
    temp_data <- data.frame(
      ID = as.character(ID),
      var = names(data)[i],
      Data = as.numeric(data[[i]])
    )
    
    # Remove rows with missing data (matching original app behavior)
    temp_data <- na.omit(temp_data)
    cat("    Data after removing NAs:", nrow(temp_data), "rows\n")
    
    if (nrow(temp_data) > 0) {
      plot_data <- rbind(plot_data, temp_data)
      cat("    Added to plot_data, now has", nrow(plot_data), "rows\n")
    } else {
      cat("    No data after removing NAs\n")
    }
  }
  
  cat("  Final plot_data has", nrow(plot_data), "rows\n")
  
  # Store comboname as attribute for later use
  attr(plot_data, "comboname") <- comboname
  
  return(plot_data)
}

# Create Poisson plots
create_poisson_plot <- function(plot_data, colors, plot_type, data_type = 1, comboname = NULL) {
  cat("create_poisson_plot called with:\n")
  cat("  plot_data rows:", nrow(plot_data), "\n")
  cat("  plot_type:", plot_type, "\n")
  cat("  data_type:", data_type, "\n")
  cat("  comboname:", comboname, "\n")
  
  # Safe color handling
  line_color <- if (!is.null(colors) && !is.null(colors$col_plot_line)) colors$col_plot_line else "#1f77b4"
  fill_color <- if (!is.null(colors) && !is.null(colors$col_fill_highlight)) colors$col_fill_highlight else "#ff7f0e"
  theoretical_color <- if (!is.null(colors) && !is.null(colors$col_line_control_chart)) colors$col_line_control_chart else "#2ca02c"
  
  if (plot_type == 1) {
    # Histogram - matching original implementation
    cat("Creating Poisson histogram\n")
    
    # Create legend names matching original
    leg_names <- c("Theoretical" = theoretical_color, "Empirical" = fill_color)
    
    # Create base plot
    p <- ggplot(plot_data, aes(x = Data)) +
      geom_histogram(aes(fill = "Empirical"), binwidth = 1, color = line_color, alpha = 0.7) +
      stat_theodensity(distri = "nbinom", geom = "point", aes(y = after_stat(count), xend = after_stat(x), yend = 0, color = "Theoretical")) +
      stat_theodensity(distri = "nbinom", geom = "segment", aes(y = after_stat(count), xend = after_stat(x), yend = 0), color = theoretical_color, linetype = "dashed") +
      ylab("Count") +
      ggtitle("Empirical and Theoretical Histograms") +
      scale_color_manual(values = leg_names) +
      scale_fill_manual(values = leg_names) +
      theme(legend.position = "bottom") +
      labs(color = "", fill = "")
    
    # Add faceting based on data type
    if (data_type == 1) {
      p <- p + facet_wrap2(~ID, scales = "free", shrink = FALSE)
    } else {
      p <- p + facet_wrap2(~ID + var, scales = "free", shrink = FALSE)
      if (!is.null(comboname)) {
        p <- p + labs(subtitle = comboname)
      }
    }
    
    return(p)
  } else {
    # CDF - matching original implementation
    cat("Creating Poisson CDF\n")
    
    # Create legend names matching original
    leg_names <- c("Theoretical" = line_color, "Empirical" = fill_color)
    
    # Create theoretical CDF data for each ID
    theoretical_data <- data.frame()
    for (id in unique(plot_data$ID)) {
      id_data <- plot_data[plot_data$ID == id, ]
      if (nrow(id_data) > 0) {
        xmin <- min(id_data$Data)
        xmax <- max(id_data$Data)
        xlambda <- mean(id_data$Data)
        
        # Create theoretical CDF data
        x_seq <- xmin:xmax
        y_seq <- cumsum(dpois(x = x_seq, lambda = xlambda))
        
        theoretical_data <- rbind(theoretical_data, data.frame(
          ID = id,
          x = x_seq,
          y = y_seq,
          color = "Theoretical"
        ))
      }
    }
    
    # Create base plot
    p <- ggplot() +
      geom_step(data = theoretical_data, aes(x = x, y = y, color = "Theoretical")) +
      stat_ecdf(data = plot_data, aes(x = Data, color = "Empirical"), geom = "point") +
      ylab("CDF") +
      xlab("Data") +
      ggtitle("Empirical and Theoretical Cumulative Density") +
      scale_color_manual(values = leg_names) +
      theme(legend.position = "bottom") +
      labs(color = "")
    
    # Add faceting based on data type
    if (data_type == 1) {
      p <- p + facet_wrap2(~ID, scales = "free", shrink = FALSE)
    } else {
      p <- p + facet_wrap2(~ID + var, scales = "free", shrink = FALSE)
      if (!is.null(comboname)) {
        p <- p + labs(subtitle = comboname)
      }
    }
    
    return(p)
  }
}

# Create normality plots
create_normality_plot <- function(plot_data, colors, plot_type, data_type = 1, comboname = NULL) {
  cat("create_normality_plot called with:\n")
  cat("  plot_data rows:", nrow(plot_data), "\n")
  cat("  plot_type:", plot_type, "\n")
  cat("  data_type:", data_type, "\n")
  cat("  comboname:", comboname, "\n")
  cat("  colors:", paste(names(colors), "=", colors, collapse = ", "), "\n")
  
  # Use safe color values
  line_color <- if (!is.null(colors) && !is.null(colors$col_plot_line)) colors$col_plot_line else "#1f77b4"
  fill_color <- if (!is.null(colors) && !is.null(colors$col_fill_highlight)) colors$col_fill_highlight else "#ff7f0e"
  rug_color <- if (!is.null(colors) && !is.null(colors$col_point_of_interest_line)) colors$col_point_of_interest_line else "#DF536B"
  
  cat("Using colors - line:", line_color, "fill:", fill_color, "rug:", rug_color, "\n")
  
  if (plot_type == 1) {
    # Density plot - matching original implementation and other distribution modules
    cat("Creating density plot\n")
    
    # Create legend names matching original
    leg_names <- c("Theoretical" = colors$col_plot_line, "Empirical" = fill_color)
    
    # Create base plot
    p <- ggplot(plot_data, aes(x = Data)) +
      geom_density(aes(fill = "Empirical"), color = line_color, bw = "sj", kernel = "gaussian", alpha = 0.7) +
      stat_theodensity(aes(color = "Theoretical"), distri = "norm") +
      geom_rug(aes(x = Data), color = rug_color, alpha = 0.35, inherit.aes = FALSE) +
      labs(
        x = "Data",
        y = "PDF"
      ) +
      ggtitle("Empirical and Theoretical Density Plots") +
      scale_color_manual(values = leg_names) +
      scale_fill_manual(values = leg_names) +
      theme(legend.position = "bottom") +
      labs(color = "", fill = "")
    
    # Add faceting based on data type
    if (data_type == 1) {
      p <- p + facet_wrap2(~ID, scales = "free", shrink = FALSE)
    } else {
      p <- p + facet_wrap2(~ID + var, scales = "free", shrink = FALSE)
      if (!is.null(comboname)) {
        p <- p + labs(subtitle = comboname)
      }
    }
    
    return(p)
  } else if (plot_type == 2) {
    # Q-Q plot - matching original implementation and other distribution modules
    cat("Creating Q-Q plot\n")
    p <- ggplot(plot_data, aes(sample = Data)) +
      stat_qq(color = fill_color) +
      stat_qq_line() +
      labs(
        x = "Theoretical Quantiles",
        y = "Empirical Quantiles"
      ) +
      ggtitle("Q-Q Plot") +
      theme(legend.position = "none")
    
    # Add faceting based on data type
    if (data_type == 1) {
      p <- p + facet_wrap2(~ID, scales = "free", shrink = FALSE)
    } else {
      p <- p + facet_wrap2(~ID + var, scales = "free", shrink = FALSE)
      if (!is.null(comboname)) {
        p <- p + labs(subtitle = comboname)
      }
    }
    
    return(p)
  } else {
    # P-P plot - matching original implementation and other distribution modules
    cat("Creating P-P plot\n")
    
    # Create P-P data matching original implementation
    pp_data <- plot_data %>%
      group_by(ID) %>%
      mutate(
        mean = mean(Data),
        sd = sd(Data),
        n = length(Data),
        p = pnorm(q = Data, mean = mean(Data), sd = sd(Data))
      ) %>%
      arrange(ID, p) %>%
      group_by(ID) %>%
      mutate(row = row_number()) %>%
      mutate(x = row/n - 0.5/n)
    
    p <- ggplot(pp_data, aes(x = x, y = p)) +
      geom_point(color = fill_color) +
      geom_abline(slope = 1, intercept = 0) +
      labs(
        x = "Theoretical Cumulative p",
        y = "Empirical Cumulative p"
      ) +
      ggtitle("P-P Plot - Normal") +
      xlim(c(0, 1)) +
      ylim(c(0, 1)) +
      theme(legend.position = "none")
    
    # Add faceting based on data type
    if (data_type == 1) {
      p <- p + facet_wrap2(~ID, scales = "free", shrink = FALSE)
    } else {
      p <- p + facet_wrap2(~ID + var, scales = "free", shrink = FALSE)
      if (!is.null(comboname)) {
        p <- p + labs(subtitle = comboname)
      }
    }
    
    return(p)
  }
}

# Create exponential plots
create_exponential_plot <- function(plot_data, colors, plot_type, data_type = 1, comboname = NULL) {
  cat("create_exponential_plot called with:\n")
  cat("  plot_data rows:", nrow(plot_data), "\n")
  cat("  plot_type:", plot_type, "\n")
  cat("  data_type:", data_type, "\n")
  cat("  comboname:", comboname, "\n")
  
  # Safe color handling
  line_color <- if (!is.null(colors) && !is.null(colors$col_plot_line)) colors$col_plot_line else "#1f77b4"
  fill_color <- if (!is.null(colors) && !is.null(colors$col_fill_highlight)) colors$col_fill_highlight else "#ff7f0e"
  theoretical_color <- if (!is.null(colors) && !is.null(colors$col_line_control_chart)) colors$col_line_control_chart else "#2ca02c"
  rug_color <- if (!is.null(colors) && !is.null(colors$col_point_of_interest_line)) colors$col_point_of_interest_line else "#DF536B"
  
  if (plot_type == 1) {
    # Density plot - matching original implementation
    cat("Creating exponential density plot\n")
    
    # Create legend names matching original
    leg_names <- c("Theoretical" = theoretical_color, "Empirical" = fill_color)
    
    # Create base plot
    p <- ggplot(plot_data, aes(x = Data)) +
      ylab("PDF") +
      geom_density(aes(fill = "Empirical"), color = line_color, alpha = 0.7) +
      ggtitle("Empirical and Theoretical Density Plots") +
      scale_color_manual(values = leg_names) +
      scale_fill_manual(values = leg_names) +
      theme(legend.position = "bottom") +
      labs(color = "", fill = "") +
      geom_rug(color = rug_color, alpha = 0.35)
    
    # Add theoretical density for each ID (matching original implementation)
    for (id in unique(plot_data$ID)) {
      id_data <- plot_data[plot_data$ID == id, ]
      if (nrow(id_data) > 0) {
        xmin <- min(id_data$Data)
        p <- p + stat_theodensity(
          data = ~ subset(.x, ID == id),
          aes(color = "Theoretical", x = stage(Data - xmin, after_stat = x + xmin)),
          distri = "exp",
          linewidth = 1
        )
      }
    }
    
    # Add faceting based on data type
    if (data_type == 1) {
      p <- p + facet_wrap2(~ID, scales = "free", shrink = FALSE)
    } else {
      p <- p + facet_wrap2(~ID + var, scales = "free", shrink = FALSE)
      if (!is.null(comboname)) {
        p <- p + labs(subtitle = comboname)
      }
    }
    
    return(p)
  } else if (plot_type == 2) {
    # Q-Q plot - matching original implementation
    cat("Creating exponential Q-Q plot\n")
    p <- ggplot(plot_data, aes(sample = Data)) +
      stat_qq(distribution = qexp, color = fill_color) +
      stat_qq_line(distribution = qexp) +
      ggtitle("Q-Q Plot - Exponential") +
      ylab("Empirical Quantiles") +
      xlab("Theoretical Quantiles")
    
    # Add faceting based on data type
    if (data_type == 1) {
      p <- p + facet_wrap2(~ID, scales = "free", shrink = FALSE)
    } else {
      p <- p + facet_wrap2(~ID + var, scales = "free", shrink = FALSE)
      if (!is.null(comboname)) {
        p <- p + labs(subtitle = comboname)
      }
    }
    
    return(p)
  } else {
    # P-P plot - matching original implementation
    cat("Creating exponential P-P plot\n")
    
    # Create P-P data matching original implementation
    pp_data <- plot_data %>%
      group_by(ID) %>%
      mutate(
        mean = mean(Data),
        min = min(Data),
        n = length(Data),
        p = pexp(q = Data - min, rate = 1 / (mean(Data) - min))
      ) %>%
      arrange(ID, p) %>%
      group_by(ID) %>%
      mutate(row = row_number()) %>%
      mutate(x = row / n - 0.5 / n) %>%
      ungroup()
    
    p <- ggplot(pp_data, aes(x = x, y = p)) +
      geom_point(color = fill_color) +
      geom_abline(slope = 1, intercept = 0) +
      xlab("Theoretical Cumulative p") +
      ylab("Empirical Cumulative p") +
      ggtitle("P-P Plot - Exponential with Xmin as Omicron") +
      xlim(c(0, 1)) +
      ylim(c(0, 1)) +
      theme(legend.position = "none")
    
    # Add faceting based on data type
    if (data_type == 1) {
      p <- p + facet_wrap2(~ID, scales = "free", shrink = FALSE)
    } else {
      p <- p + facet_wrap2(~ID + var, scales = "free", shrink = FALSE)
      if (!is.null(comboname)) {
        p <- p + labs(subtitle = comboname)
      }
    }
    
    return(p)
  }
}

# Default plot when no data
create_default_test_plot <- function() {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = "No data available for plotting", size = 6) +
    theme_void()
}

# Get test information
get_test_info <- function(test_id) {
  info_list <- list(
    "1" = "<h4>Poisson Dispersion Test</h4><p>Tests whether data follows a Poisson distribution by comparing the sample mean and variance.</p>",
    "2" = "<h4>Anderson-Darling Normality Test</h4><p>Tests whether data follows a normal distribution using the Anderson-Darling statistic.</p>",
    "3" = "<h4>Shapiro-Wilk Normality Test</h4><p>Tests whether data follows a normal distribution using the Shapiro-Wilk statistic.</p>",
    "4" = "<h4>Lin-Mudholkar Test</h4><p>Tests for normality using the Lin-Mudholkar statistic.</p>",
    "5" = "<h4>Skewness and Kurtosis Test</h4><p>Tests for normality using skewness and kurtosis measures.</p>",
    "6" = "<h4>Shapiro-Wilk Exponentiality Test</h4><p>Tests whether data follows an exponential distribution using Monte Carlo simulation.</p>",
    "7" = "<h4>MVP Exponentiality Test</h4><p>Tests whether data follows an exponential distribution using MVP method with Monte Carlo simulation.</p>",
    "8" = "<h4>Anderson-Darling Exponentiality Test</h4><p>Tests whether data follows an exponential distribution using the Anderson-Darling statistic.</p>",
    "9" = "<h4>D'Agostino's Omnibus Test</h4><p>Tests for normality using D'Agostino's omnibus test.</p>"
  )
  
  return(info_list[[as.character(test_id)]] %||% "<p>Test information not available.</p>")
}


