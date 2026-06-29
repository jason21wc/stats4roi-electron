# Exponential Distribution Module for stats4ROI
# This module exactly replicates the Exponential distribution functionality from the original app

# Source global configuration for ro() function and colors
source("modules/config/global_config.R")

# Exponential Distribution UI (replicating app.R lines 916-956)
create_exponential_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = ns("mu_exp"),
        label = "μ",
        value = 100,
        width = "100px"
      ),
      numericInput(
        inputId = ns("X_min_exp"),
        label = "Xmin",
        value = 0,
        width = "100px"
      ),
      radioButtons(
        inputId = ns("tails_exp"),
        label = "One or Two Tails?",
        choices = c("One-Tail" = 1, "Two-Tails" = 2)
      ),
      uiOutput(ns("ui_exp1")),
      uiOutput(ns("ui_exp2")),
      uiOutput(ns("ui_exp3")),
      numericInput(
        inputId = ns("decimal_exp"),
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
      downloadUI(ns("exponential_plot")),
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

# Exponential Distribution Server (replicating app.R lines 4845-4944)
create_exponential_server <- function(id, color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    # Get colors from palette
    colors <- reactive({
      get_distribution_colors(color_palette())
    })
    
    # UI outputs for conditional inputs
    output$ui_exp1 <- renderUI({
      numericInput(
        inputId = ns("tail_exp"),
        label = "Point of Interest",
        value = 10,
        width = "100px"
      )
    })
    
    output$ui_exp2 <- renderUI({
      if (input$tails_exp == 2) {
        numericInput(
          inputId = ns("tailu_exp"),
          label = "Upper Tail",
          value = 20,
          width = "100px"
        )
      }
    })
    
    output$ui_exp3 <- renderUI({
      tails <- input$tails_exp
      req(tails)
      
      if (tails == 1) {
        radioButtons(
          inputId = ns("exp_interest"),
          label = "Area above or below?",
          choices = c("Above" = 1, "Below" = 2)
        )
      } else {
        radioButtons(
          inputId = ns("exp_interest"),
          label = "Area between or outside of points?",
          choices = c("Inside" = 1, "Outside" = 2)
        )
      }
    })
    
    # Exponential plot function
    exp_plot <- reactive({
      # Get parameters first
      X_min <- input$X_min_exp
      mu <- input$mu_exp
      X <- input$tail_exp
      upper_or_lower <- input$upper_or_lower_exp
      R <- input$decimal_exp
      tailu_exp <- input$tailu_exp
      interest <- input$exp_interest
      tails_exp <- input$tails_exp
      
      # Check if basic parameters are available
      if (is.null(X_min) || is.null(mu) || is.null(X) || is.null(tails_exp) || is.null(R) || is.null(interest)) {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Loading inputs...", size = 6) +
               theme_void())
      }
      
      # Check for conditional inputs based on tail type
      if (tails_exp == 2) {
        if (is.null(tailu_exp)) {
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
      
      req(X)
      
      max_exp <- 5 * mu
      
      data <- data.frame(x = X_min:max_exp, pdf = dexp(x = X_min:max_exp, rate = 1/mu))
      
      pl <- ggplot(data = data, aes(x = x, y = pdf)) +
        geom_line(color = colors()$col_plot_line, linewidth = 1)
      
      if (tails_exp == 1) {
        if (interest == 1) { # Above: area above point of interest
          data_u <- data.frame(x = X:max_exp, pdf = dexp(x = X:max_exp, rate = 1/mu))
          pl <- pl +
            geom_area(data = data_u, aes(x = x, y = pdf), fill = colors()$col_fill_highlight, color = colors()$col_plot_line, alpha = 0.7)
        }
        if (interest == 2) { # Below: area below point of interest
          data_l <- data.frame(x = X_min:X, pdf = dexp(x = X_min:X, rate = 1/mu))
          pl <- pl +
            geom_area(data = data_l, aes(x = x, y = pdf), fill = colors()$col_fill_highlight, color = colors()$col_plot_line, alpha = 0.7)
        }
      }
      
      if (tails_exp == 2) {
        req(tailu_exp)
        if (interest == 1) { # draw inner section
          data_in <- data.frame(x = X:tailu_exp, pdf = dexp(x = X:tailu_exp, rate = 1/mu))
          pl <- pl +
            geom_area(data = data_in, aes(x = x, y = pdf), fill = colors()$col_fill_highlight, color = colors()$col_plot_line, alpha = 0.7) +
            geom_vline(aes(xintercept = tailu_exp), color = colors()$col_plot_line, linewidth = 1, linetype = 2)
        }
        if (interest == 2) { # draw outer sections
          data_l <- data.frame(x = X_min:X, pdf = dexp(x = X_min:X, rate = 1/mu))
          data_u <- data.frame(x = tailu_exp:max_exp, pdf = dexp(x = tailu_exp:max_exp, rate = 1/mu))
          pl <- pl +
            geom_area(data = data_u, aes(x = x, y = pdf), fill = colors()$col_fill_highlight, color = colors()$col_plot_line, alpha = 0.7) +
            geom_area(data = data_l, aes(x = x, y = pdf), fill = colors()$col_fill_highlight, color = colors()$col_plot_line, alpha = 0.7) +
            geom_vline(aes(xintercept = tailu_exp), color = colors()$col_plot_line, linewidth = 1, linetype = 2)
        }
      }
      
      pl + theme(legend.position = "none") +
        ggtitle(paste("Exponential Distribution with μ = ", mu, "and Xmin = ", X_min)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_vline(aes(xintercept = mu), color = colors()$col_mean_line) +
        geom_vline(aes(xintercept = X), color = colors()$col_point_of_interest_line, linetype = 2) +
        xlab("X") +
        ylab("pdf(X)") +
        scale_x_continuous(guide = guide_axis(check.overlap = TRUE))
    })
    
    # Exponential table function
    exp_table <- reactive({
      # Get parameters first
      X_min <- input$X_min_exp
      mu <- input$mu_exp
      X <- input$tail_exp
      upper_or_lower <- input$upper_or_lower_exp
      R <- input$decimal_exp
      tailu_exp <- input$tailu_exp
      interest <- input$exp_interest
      tails_exp <- input$tails_exp
      
      # Check if basic parameters are available
      if (is.null(X_min) || is.null(mu) || is.null(X) || is.null(tails_exp) || is.null(R) || is.null(interest)) {
        return(data.frame())
      }
      
      # Check for conditional inputs based on tail type
      if (tails_exp == 2) {
        if (is.null(tailu_exp)) {
          return(data.frame())
        }
      }
      
      req(X)
      
      if (tails_exp == 1) {
        x_and_higher <- pexp.low(q = X, low = X_min, mean = mu, lower.tail = F) # Upper tail
        x_and_lower <- pexp.low(q = X, low = X_min, mean = mu, lower.tail = T) # Lower tail
        
        # build the table with areas above and below X.exp
        # create the table
        data_labels <- c("μ = ", "Xmin = ", "X = ", "Area above X", "Area below X")
        results <- c(mu, X_min, X, x_and_higher, x_and_lower)
        exp_table <- data.frame(data_labels, results)
        ro(exp_table, R)
      } else {
        req(tailu_exp)
        x_and_higher <- pexp.low(q = tailu_exp, low = X_min, mean = mu, lower.tail = F) # Upper tail
        x_and_lower <- pexp.low(q = X, low = X_min, mean = mu, lower.tail = T) # Lower tail
        
        # build the table with areas above and below X.exp
        # create the table
        data_labels <- c("μ = ", "Xmin = ", "X = ", "Area above Upper Tail", "Area below Lower Tail", "Area Inside Tails")
        results <- c(mu, X_min, X, x_and_higher, x_and_lower, 1 - (x_and_higher + x_and_lower))
        exp_table <- data.frame(data_labels, results)
        ro(exp_table, R)
      }
    })
    
    # Plot output - directly in this module
    output$dist_plot <- renderPlot({
      exp_plot()
    })
    
    # Table output
    output$dist_table <- DT::renderDataTable({
      DT::datatable(exp_table(), 
                    options = list(dom = 't', pageLength = -1), 
                    rownames = FALSE, 
                    colnames = c("", "Result"))
    })
    
    # Download functionality
    plot_width <- reactive(400 * 4)
    plot_height <- reactive(300 * 4)
    downloadServer("exponential_plot", exp_plot, height = plot_height, width = plot_width)
  })
}
