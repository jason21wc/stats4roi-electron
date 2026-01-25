# Normal Distribution Module for stats4ROI
# This module exactly replicates the Normal distribution functionality from the original app

# Source global configuration for ro() function and colors
source("modules/config/global_config.R")

# Normal Distribution UI (replicating app.R lines 869-912)
create_normal_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = ns("mu_norm"),
        label = "μ",
        value = 10,
        width = "100px"
      ),
      numericInput(
        inputId = ns("sigma_norm"),
        label = "σ",
        value = 1,
        width = "100px"
      ),
      radioButtons(
        inputId = ns("tails_norm"),
        label = "One or Two Tails?",
        choices = c("One-Tail" = 1, "Two-Tails" = 2)
      ),
      uiOutput(ns("lowertail_norm")),
      conditionalPanel(
        condition = paste0("input['", ns("tails_norm"), "'] == 2"),
        uiOutput(ns("uppertail_norm"))
      ),
      uiOutput(ns("inorout_norm")),
      numericInput(
        inputId = ns("decimal_norm"),
        label = "Decimals",
        value = 5,
        min = 0,
        max = 9,
        step = 1,
        width = "75px"
      ),
      prettySwitch(
        inputId = ns("data_label_norm"),
        label = "Show Data Labels?",
        value = FALSE,
        status = "success",
        fill = TRUE
      )
    ),
    mainPanel(
      plotOutput(outputId = ns("dist_plot"), height = "300px"),
      downloadUI(ns("normal_plot")),
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

# Normal Distribution Server (replicating app.R lines 4640-4780)
create_normal_server <- function(id, color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    # Get colors from palette
    colors <- reactive({
      get_distribution_colors(color_palette())
    })
    
    # UI outputs for conditional inputs
    output$lowertail_norm <- renderUI({
      numericInput(
        inputId = ns("tail1_norm"),
        label = "X",
        value = 12,
        width = "100px"
      )
    })
    
    output$uppertail_norm <- renderUI({
      if (input$tails_norm == 2) {
        numericInput(
          inputId = ns("tail2_norm"),
          label = "X2",
          value = 14,
          width = "100px"
        )
      }
    })
    
    output$inorout_norm <- renderUI({
      tails <- input$tails_norm
      req(tails)
      
      if (tails == 1) {
        radioButtons(
          inputId = ns("in_or_out_norm"),
          label = "Area above or below?",
          choices = c("Above" = 1, "Below" = 2)
        )
      } else {
        radioButtons(
          inputId = ns("in_or_out_norm"),
          label = "Area between or outside of points?",
          choices = c("Inside" = 1, "Outside" = 2)
        )
      }
    })
    
    # Normal plot function
    norm_plot <- reactive({
      # Get parameters first
      mu <- input$mu_norm
      sigma <- input$sigma_norm
      tails <- input$tails_norm
      R <- input$decimal_norm
      data_label <- input$data_label_norm
      X1 <- input$tail1_norm
      X2 <- input$tail2_norm
      in_or_out <- input$in_or_out_norm
      
      # Check if basic parameters are available (mu, sigma, tails, R, X1 needed for basic plot)
      if (is.null(mu) || is.null(sigma) || is.null(tails) || is.null(R) || is.null(X1)) {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Loading inputs...", size = 6) +
               theme_void())
      }
      
      # Check for conditional inputs based on tail type
      if (tails == 1) {
        # One tail: need in_or_out
        if (is.null(in_or_out)) {
          return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "Loading conditional inputs...", size = 6) +
                 theme_void())
        }
      } else if (tails == 2) {
        # Two tails: need X2 and in_or_out
        if (is.null(X2) || is.null(in_or_out)) {
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
      
      validate(need(mu, "Need a population mean"),
               need(sigma, "Need a population standard deviation"),
               need(X1, "Need X"),
               need(R, "Need the number of decimal places"))
      
      if (tails == 2) {
        validate(need(X2, "Need second tail"),
                 need(X1 <= X2, "X1 needs to be less than X2"),
                 need(in_or_out, "You shouldn't see this error"))
      }
      
      # Create data for plotting
      x_range <- c(mu - 4 * sigma, mu + 4 * sigma)
      x_seq <- seq(from = x_range[1], to = x_range[2], length.out = 1000)
      y_seq <- dnorm(x = x_seq, mean = mu, sd = sigma)
      data <- data.frame(x = x_seq, y = y_seq)
      
      # Initialize area calculation
      area <- 0
      
      # Create base plot
      pl <- ggplot(data = data, aes(x = x, y = y)) +
        geom_line(color = colors()$col_plot_line, linewidth = 1) +
        labs(
          x = "X",
          y = "Probability Density"
        ) +
        theme(legend.position = "none")
      
      # Add point of interest lines and shading
      if (tails == 1) {
        # One tail
        pl <- pl + geom_vline(xintercept = X1, color = colors()$col_point_of_interest_line, 
                              linetype = "dashed", linewidth = 1)
        
        if (in_or_out == 1) {
          # Above X1
          area <- area + pnorm(q = X1, mean = mu, sd = sigma, lower.tail = FALSE)
          shade_data <- data[data$x >= X1, ]
          pl <- pl + geom_area(data = shade_data, aes(x = x, y = y), 
                               fill = colors()$col_fill_highlight, alpha = 0.7)
        } else {
          # Below X1
          area <- area + pnorm(q = X1, mean = mu, sd = sigma, lower.tail = TRUE)
          shade_data <- data[data$x <= X1, ]
          pl <- pl + geom_area(data = shade_data, aes(x = x, y = y), 
                               fill = colors()$col_fill_highlight, alpha = 0.7)
        }
      } else {
        # Two tails
        pl <- pl + geom_vline(xintercept = X1, color = colors()$col_point_of_interest_line, 
                              linetype = "dashed", linewidth = 1) +
          geom_vline(xintercept = X2, color = colors()$col_point_of_interest_line, 
                     linetype = "dashed", linewidth = 1)
        
        if (in_or_out == 1) {
          # Inside X1 and X2
          area <- area + pnorm(q = X1, mean = mu, sd = sigma, lower.tail = FALSE) - 
                         pnorm(X2, mean = mu, sd = sigma, lower.tail = FALSE)
          shade_data <- data[data$x >= X1 & data$x <= X2, ]
          pl <- pl + geom_area(data = shade_data, aes(x = x, y = y), 
                               fill = colors()$col_fill_highlight, alpha = 0.7)
        } else {
          # Outside X1 and X2
          area <- area + pnorm(q = X1, mean = mu, sd = sigma, lower.tail = TRUE) + 
                         pnorm(X2, mean = mu, sd = sigma, lower.tail = FALSE)
          shade_data1 <- data[data$x <= X1, ]
          shade_data2 <- data[data$x >= X2, ]
          pl <- pl + geom_area(data = shade_data1, aes(x = x, y = y), 
                               fill = colors()$col_fill_highlight, alpha = 0.7) +
            geom_area(data = shade_data2, aes(x = x, y = y), 
                      fill = colors()$col_fill_highlight, alpha = 0.7)
        }
      }
      
      # Add data labels if requested
      if (data_label) {
        pl <- pl + annotate("text", x = X1, y = max(y_seq) * 0.9, 
                           label = paste("X =", round(X1, R)), 
                           hjust = 0.5, vjust = 0)
        if (tails == 2) {
          pl <- pl + annotate("text", x = X2, y = max(y_seq) * 0.8, 
                             label = paste("X2 =", round(X2, R)), 
                             hjust = 0.5, vjust = 0)
        }
      }
      
      # Add mean line on top of everything
      pl <- pl + geom_vline(xintercept = mu, color = colors()$col_mean_line, linewidth = 1.5)
      
      # Add title with subtitle showing shaded area
      pl <- pl + ggtitle(
        paste0("A normal distribution with μ = ", mu, " and σ = ", sigma),
        subtitle = paste0("Shaded area = ", ro(area, R))
      )
      
      pl
    })
    
    # Normal table function
    norm_table <- reactive({
      # Get parameters first
      mu <- input$mu_norm
      sigma <- input$sigma_norm
      tails <- input$tails_norm
      R <- input$decimal_norm
      X1 <- input$tail1_norm
      X2 <- input$tail2_norm
      in_or_out <- input$in_or_out_norm
      
      # Check if basic parameters are available
      if (is.null(mu) || is.null(sigma) || is.null(tails) || is.null(R) || is.null(X1) || is.null(in_or_out)) {
        return(data.frame())
      }
      
      # Check for conditional inputs based on tail type
      if (tails == 1) {
        # One tail: need in_or_out
        if (is.null(in_or_out)) {
          return(data.frame())
        }
      } else if (tails == 2) {
        # Two tails: need X2 and in_or_out
        if (is.null(X2) || is.null(in_or_out)) {
          return(data.frame())
        }
      }
      
      if (tails == 1) {
        # One tail
        x_or_lower <- pnorm(q = X1, mean = mu, sd = sigma, lower.tail = TRUE)  # X or lower
        x_or_higher <- pnorm(q = X1, mean = mu, sd = sigma, lower.tail = FALSE)  # X or higher
        
        # Create the table
        data_labels <- c("μ = ", "σ = ", "X = ", "Area above X", "Area below X")
        results <- c(mu, sigma, X1, x_or_higher, x_or_lower)
        norm_table <- data.frame(data_labels, results)
        ro(norm_table, R)
      } else {
        # Two tails
        if (in_or_out == 1) {
          # Inside X1 and X2
          between_x1_x2 <- pnorm(q = max(X1, X2), mean = mu, sd = sigma, lower.tail = TRUE) - 
                          pnorm(q = min(X1, X2), mean = mu, sd = sigma, lower.tail = TRUE)
          
          # Create the table
          data_labels <- c("μ = ", "σ = ", "Lower X = ", "Upper X = ", "Area between tails =")
          results <- c(mu, sigma, X1, X2, between_x1_x2)
          norm_table <- data.frame(data_labels, results)
          ro(norm_table, R)
        } else {
          # Outside X1 and X2
          outside_x1_x2 <- 1 - (pnorm(q = max(X1, X2), mean = mu, sd = sigma, lower.tail = TRUE) - 
                               pnorm(q = min(X1, X2), mean = mu, sd = sigma, lower.tail = TRUE))
          
          # Create the table
          data_labels <- c("μ = ", "σ = ", "Lower X = ", "Upper X = ", "Area of tails =")
          results <- c(mu, sigma, X1, X2, outside_x1_x2)
          norm_table <- data.frame(data_labels, results)
          ro(norm_table, R)
        }
      }
    })
    
    # Plot output - directly in this module
    output$dist_plot <- renderPlot({
      norm_plot()
    })
    
    # Table output
    output$dist_table <- DT::renderDataTable({
      DT::datatable(norm_table(), 
                    options = list(dom = 't', pageLength = -1), 
                    rownames = FALSE, 
                    colnames = c("", "Result"))
    })
    
    # Download functionality
    plot_width <- reactive(400 * 4)
    plot_height <- reactive(300 * 4)
    downloadServer("normal_plot", norm_plot, height = plot_height, width = plot_width)
  })
}
