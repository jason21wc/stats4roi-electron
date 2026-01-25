# Weibull Distribution Module for stats4ROI
# This module exactly replicates the Weibull distribution functionality from the original app

# Source global configuration for ro() function and colors
source("modules/config/global_config.R")

# Weibull Distribution UI (replicating app.R lines 1022-1059)
create_weibull_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = ns("dist_weibull_shape"),
        label = "Shape Parameter (k)",
        value = 1.5,
        min = 0,
        step = 0.1,
        width = "100px"
      ),
      numericInput(
        inputId = ns("dist_weibull_scale"),
        label = "Scale Parameter (λ)",
        value = 1,
        min = 0,
        step = 0.1,
        width = "100px"
      ),
      radioButtons(
        inputId = ns("dist_weibull_tails"),
        label = "One or Two Tails?",
        choices = c("One-Tail" = 1, "Two-Tails" = 2)
      ),
      uiOutput(ns("lowertail_w")),
      uiOutput(ns("uppertail_w")),
      uiOutput(ns("inorout_w")),
      checkboxInput(
        inputId = ns("data_label_w"),
        label = "Label Graph?",
        value = TRUE
      ),
      numericInput(
        inputId = ns("decimal_w"),
        label = "Decimals",
        value = 4,
        min = 0,
        max = 9,
        step = 1,
        width = "75px"
      )
    ),
    mainPanel(
      plotOutput(outputId = ns("dist_plot"), height = "300px"),
      downloadUI(ns("weibull_plot")),
      fluidRow(
        column(
          width = 6,
          offset = 3,
          DT::dataTableOutput(ns("dist_table"))
        )
      )
    )
  )
}

# Weibull Distribution Server (replicating app.R lines 33407-33569)
create_weibull_server <- function(id, color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    # Get colors from palette
    colors <- reactive({
      get_distribution_colors(color_palette())
    })
    
    # UI outputs for conditional inputs
    output$lowertail_w <- renderUI({
      tails <- input$dist_weibull_tails
      req(tails)
      
      if (tails == 1) {
        numericInput(
          inputId = ns("tail1_w"),
          label = "Point of Interest",
          value = 9,
          width = "100px"
        )
      } else {
        numericInput(
          inputId = ns("tail1_w"),
          label = "Lower Tail",
          value = 9,
          width = "100px"
        )
      }
    })
    
    output$uppertail_w <- renderUI({
      tails <- input$dist_weibull_tails
      req(tails)
      
      if (tails == 1) {
        NULL
      } else {
        numericInput(
          inputId = ns("tail2_w"),
          label = "Upper Tail",
          value = 11,
          width = "100px"
        )
      }
    })
    
    output$inorout_w <- renderUI({
      tails <- input$dist_weibull_tails
      req(tails)
      
      if (tails == 1) {
        radioButtons(
          inputId = ns("in_or_out_w"),
          label = "Area above or below?",
          choices = c("Above" = 1, "Below" = 2)
        )
      } else {
        radioButtons(
          inputId = ns("in_or_out_w"),
          label = "Area between or outside of points?",
          choices = c("Inside" = 1, "Outside" = 2)
        )
      }
    })
    
    # Weibull plot function
    w_plot <- reactive({
      # Get parameters first
      scale <- input$dist_weibull_scale
      shape <- input$dist_weibull_shape
      tails <- input$dist_weibull_tails
      X1 <- as.numeric(input$tail1_w)
      X2 <- as.numeric(input$tail2_w)
      in_or_out <- input$in_or_out_w
      label <- input$data_label_w
      R <- input$decimal_w
      
      # Check if basic parameters are available
      if (is.null(scale) || is.null(shape) || is.null(tails) || is.null(X1) || is.null(R)) {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Loading inputs...", size = 6) +
               theme_void())
      }
      
      # Check for conditional inputs based on tail type
      if (tails == 2) {
        if (is.null(X2) || is.null(in_or_out)) {
          return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "Loading conditional inputs...", size = 6) +
                 theme_void())
        }
      } else {
        if (is.null(in_or_out)) {
          return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "Loading conditional inputs...", size = 6) +
                 theme_void())
        }
      }
      
      # Check if color palette is available
      if (is.null(colors())) {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Loading colors...", size = 6) +
               theme_void())
      }
      
      validate(need(shape, "Need shape parameter"),
               need(scale, "Need scale parameter"),
               need(X1, "Need X"),
               need(R, "Need the number of decimal places"))
      
      if (tails == 2) {
        validate(need(X2, "Need second tail"),
                 need(X1 < X2, "X1 needs to be less than X2"))
      }
      
      req(shape, scale, tails, X1, R)
      
      # Calculate xmin and xmax
      w_median <- scale * (log(2))^(1/shape)
      
      d <- 1
      xmax <- w_median
      while (d > 0.001) {
        xmax <- xmax + 0.01
        d <- dweibull(xmax, shape = shape, scale = scale)
      }
      
      d <- 1
      xmin <- w_median
      while (d > 0.001) {
        xmin <- xmin - 0.01
        d <- dweibull(xmin, shape = shape, scale = scale)
      }
      
      if (xmin > 0) xmin <- 0
      xmin <- min(X1, xmin)
      if (tails == 1) {
        xmax <- max(X1, xmax)
      } else {
        xmax <- max(X1, X2, xmax)
      }
      
      area <- 0
      
      # Create data for plotting
      x_seq <- seq(from = xmin, to = xmax, length.out = 1000)
      y_seq <- dweibull(x_seq, shape = shape, scale = scale)
      data <- data.frame(x = x_seq, y = y_seq)
      
      # Create base plot
      p <- ggplot(data = data, aes(x = x, y = y)) +
        geom_line(color = colors()$col_plot_line, linewidth = 1) +
        labs(
          x = "X",
          y = "Density"
        ) +
        theme(legend.position = "none")
      
      # Add point of interest lines and shading
      if (tails == 1) {
        # One tail
        p <- p + geom_vline(xintercept = X1, color = colors()$col_point_of_interest_line, 
                            linetype = "dashed", linewidth = 1)
        
        if (in_or_out == 1) {
          # Above X1
          area <- area + pweibull(q = X1, shape = shape, scale = scale, lower.tail = FALSE)
          shade_data <- data[data$x >= X1, ]
          p <- p + geom_area(data = shade_data, aes(x = x, y = y), 
                            fill = colors()$col_fill_highlight, alpha = 0.7)
        } else {
          # Below X1
          area <- area + pweibull(q = X1, shape = shape, scale = scale, lower.tail = TRUE)
          shade_data <- data[data$x <= X1, ]
          p <- p + geom_area(data = shade_data, aes(x = x, y = y), 
                            fill = colors()$col_fill_highlight, alpha = 0.7)
        }
      } else {
        # Two tails
        p <- p + geom_vline(xintercept = X1, color = colors()$col_point_of_interest_line, 
                            linetype = "dashed", linewidth = 1) +
          geom_vline(xintercept = X2, color = colors()$col_point_of_interest_line, 
                     linetype = "dashed", linewidth = 1)
        
        if (in_or_out == 1) {
          # Inside X1 and X2
          area <- area + pweibull(q = X1, shape = shape, scale = scale, lower.tail = FALSE) - 
                         pweibull(q = X2, shape = shape, scale = scale, lower.tail = FALSE)
          shade_data <- data[data$x >= X1 & data$x <= X2, ]
          p <- p + geom_area(data = shade_data, aes(x = x, y = y), 
                            fill = colors()$col_fill_highlight, alpha = 0.7)
        } else {
          # Outside X1 and X2
          area <- area + pweibull(q = X1, shape = shape, scale = scale, lower.tail = TRUE) + 
                         pweibull(X2, shape = shape, scale = scale, lower.tail = FALSE)
          shade_data1 <- data[data$x <= X1, ]
          shade_data2 <- data[data$x >= X2, ]
          p <- p + geom_area(data = shade_data1, aes(x = x, y = y), 
                            fill = colors()$col_fill_highlight, alpha = 0.7) +
            geom_area(data = shade_data2, aes(x = x, y = y), 
                     fill = colors()$col_fill_highlight, alpha = 0.7)
        }
      }
      
      # Add data labels if requested
      if (label) {
        p <- p + annotate("text", x = X1, y = max(y_seq) * 0.9, 
                         label = paste("X1 =", round(X1, R)), 
                         hjust = 0.5, vjust = 0)
        if (tails == 2) {
          p <- p + annotate("text", x = X2, y = max(y_seq) * 0.8, 
                           label = paste("X2 =", round(X2, R)), 
                           hjust = 0.5, vjust = 0)
        }
      }
      
      # Add title with subtitle showing shaded area
      p <- p + ggtitle(
        paste0("A Weibull distribution with shape (k) = ", shape, " and scale (λ) = ", scale),
        subtitle = paste0("Shaded area = ", ro(area, R))
      )
      
      p
    })
    
    # Weibull table function (following same pattern as other distributions)
    w_table <- reactive({
      # Get parameters first
      scale <- input$dist_weibull_scale
      shape <- input$dist_weibull_shape
      tails <- input$dist_weibull_tails
      X1 <- as.numeric(input$tail1_w)
      X2 <- as.numeric(input$tail2_w)
      in_or_out <- input$in_or_out_w
      R <- input$decimal_w
      
      # Check if basic parameters are available
      if (is.null(scale) || is.null(shape) || is.null(tails) || is.null(X1) || is.null(R)) {
        return(data.frame())
      }
      
      # Check for conditional inputs based on tail type
      if (tails == 2) {
        if (is.null(X2) || is.null(in_or_out)) {
          return(data.frame())
        }
      } else {
        if (is.null(in_or_out)) {
          return(data.frame())
        }
      }
      
      if (tails == 1) {
        # One tail
        x_or_lower <- pweibull(q = X1, shape = shape, scale = scale, lower.tail = TRUE)  # X or lower
        x_or_higher <- pweibull(q = X1, shape = shape, scale = scale, lower.tail = FALSE)  # X or higher
        
        # Create the table
        data_labels <- c("Shape (k) = ", "Scale (λ) = ", "X = ", "Area above X", "Area below X")
        results <- c(shape, scale, X1, x_or_higher, x_or_lower)
        w_table <- data.frame(data_labels, results)
        ro(w_table, R)
      } else {
        # Two tails
        if (in_or_out == 1) {
          # Inside X1 and X2
          between_x1_x2 <- pweibull(q = X1, shape = shape, scale = scale, lower.tail = FALSE) - 
                          pweibull(q = X2, shape = shape, scale = scale, lower.tail = FALSE)
          
          # Create the table
          data_labels <- c("Shape (k) = ", "Scale (λ) = ", "Lower X = ", "Upper X = ", "Area between tails =")
          results <- c(shape, scale, X1, X2, between_x1_x2)
          w_table <- data.frame(data_labels, results)
          ro(w_table, R)
        } else {
          # Outside X1 and X2
          outside_x1_x2 <- pweibull(q = X1, shape = shape, scale = scale, lower.tail = TRUE) + 
                           pweibull(X2, shape = shape, scale = scale, lower.tail = FALSE)
          
          # Create the table
          data_labels <- c("Shape (k) = ", "Scale (λ) = ", "Lower X = ", "Upper X = ", "Area of tails =")
          results <- c(shape, scale, X1, X2, outside_x1_x2)
          w_table <- data.frame(data_labels, results)
          ro(w_table, R)
        }
      }
    })
    
    # Plot output - directly in this module
    output$dist_plot <- renderPlot({
      w_plot()
    })
    
    # Table output
    output$dist_table <- DT::renderDataTable({
      DT::datatable(w_table(), 
                    options = list(dom = 't', pageLength = -1), 
                    rownames = FALSE, 
                    colnames = c("", "Result"))
    })
    
    # Download functionality
    plot_width <- reactive(400 * 4)
    plot_height <- reactive(300 * 4)
    downloadServer("weibull_plot", w_plot, height = plot_height, width = plot_width)
  })
}











