# Poisson Distribution Module for stats4ROI
# This module exactly replicates the Poisson distribution functionality from the original app

# Source global configuration for ro() function and colors
source("modules/config/global_config.R")

# Poisson Distribution UI (replicating app.R lines 808-866)
create_poisson_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = ns("lambda_po"),
        label = "λ",
        value = 5,
        min = 0,
        step = 1,
        width = "200px"
      ),
      prettySwitch(
        inputId = ns("r_po"),
        label = "Enter X of Interest?",
        value = FALSE,
        status = "success",
        fill = TRUE
      ),
      conditionalPanel(
        condition = paste0("input['", ns("r_po"), "'] == 1"),
        radioButtons(
          inputId = ns("one_or_two_po"),
          label = "One or Two Tails?",
          choices = c("One-Tail" = 1, "Two-Tails" = 2)
        ),
        fluidRow(
          column(6, uiOutput(ns("ui_po1"))),
          column(6, uiOutput(ns("ui_po3")))
        ),
        fluidRow(
          column(6, uiOutput(ns("ui_po2"))),
          column(6, uiOutput(ns("ui_po4")))
        )
      ),
      numericInput(
        inputId = ns("decimal_po"),
        label = "Decimals",
        value = 5,
        min = 0,
        max = 9,
        step = 1,
        width = "75px"
      ),
      prettySwitch(
        inputId = ns("r_po_table"),
        label = "Show Table?",
        value = FALSE,
        status = "success",
        fill = TRUE
      )
    ),
    mainPanel(
      plotOutput(outputId = ns("dist_plot"), height = "300px"),
      downloadUI(ns("poisson_plot")),
      fluidRow(
        column(
          width = 12,
          htmlOutput(ns("dist_results")),
          DT::dataTableOutput(ns("dist_table"))
        )
      )
    )
  )
}

# Poisson Distribution Server (replicating app.R lines 4533-4631)
create_poisson_server <- function(id, color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    # Get colors from palette
    colors <- reactive({
      get_distribution_colors(color_palette())
    })
    
    # UI outputs for conditional inputs
    output$ui_po1 <- renderUI({
      if (input$one_or_two_po == 1) {
        numericInput(
          inputId = ns("poUI1"),
          label = "X",
          value = 3,
          min = 0,
          step = 1,
          width = "100px"
        )
      } else {
        numericInput(
          inputId = ns("poUI1"),
          label = "X Lower",
          value = 2,
          min = 0,
          step = 1,
          width = "100px"
        )
      }
    })
    
    output$ui_po2 <- renderUI({
      if (input$one_or_two_po == 1) {
        radioButtons(
          inputId = ns("po_low_inc"),
          label = "Include X?",
          choices = c("Yes" = 1, "No" = 2),
          selected = 1
        )
      } else {
        numericInput(
          inputId = ns("poUI2"),
          label = "X Upper",
          value = 8,
          min = 0,
          step = 1,
          width = "100px"
        )
      }
    })
    
    output$ui_po3 <- renderUI({
      if (input$one_or_two_po == 2) {
        radioButtons(
          inputId = ns("po_low_inc"),
          label = "Include X Lower?",
          choices = c("Yes" = 1, "No" = 2),
          selected = 1
        )
      } else {
        NULL
      }
    })
    
    output$ui_po4 <- renderUI({
      if (input$one_or_two_po == 2) {
        radioButtons(
          inputId = ns("po_hi_inc"),
          label = "Include X Upper?",
          choices = c("Yes" = 1, "No" = 2),
          selected = 1
        )
      } else {
        NULL
      }
    })
    
    # Poisson plot function
    pois_plot <- reactive({
      # Get parameters first
      lambda <- input$lambda_po
      r_po <- input$r_po
      one_or_two <- input$one_or_two_po
      r_l <- as.numeric(input$poUI1)
      r_u <- as.numeric(input$poUI2)
      R <- input$decimal_po
      
      # Check if basic parameters are available (only lambda needed for basic plot)
      if (is.null(lambda)) {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Loading inputs...", size = 6) +
               theme_void())
      }
      
      # If r_po is TRUE, check for conditional inputs
      if (r_po) {
        if (is.null(one_or_two) || is.null(r_l) || is.null(input$po_low_inc)) {
          return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "Loading conditional inputs...", size = 6) +
                 theme_void())
        }
        
        # For two tails, also need r_u and hi_inc
        if (one_or_two == 2 && (is.null(r_u) || is.null(input$po_hi_inc))) {
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
      
      validate(need(lambda > 0, "λ must be positive"))
      
      min_po <- max(0, as.integer(lambda - 6 * (lambda)^0.5))
      max_po <- as.integer(lambda + 6 * (lambda)^0.5)
      
      # Create data for plotting
      R_seq <- min_po:max_po
      pdf <- dpois(x = R_seq, lambda = lambda)
      data <- data.frame(R = R_seq, pdf = pdf)
      
      # Create base plot
      pl <- ggplot(data = data, aes(x = factor(R), y = pdf)) +
        geom_col(fill = colors()$col_fill, color = colors()$col_plot_line) +
        labs(
          title = paste("Poisson Distribution with λ =", lambda),
          x = "X",
          y = "Probability at X"
        ) +
        theme(legend.position = "none")
      
      # Add highlighting for points of interest
      if (r_po) {
        low_inc <- input$po_low_inc
        hi_inc <- input$po_hi_inc
        
        if (one_or_two == 1) {
          # One tail
          if (low_inc == 2) {
            r_l <- r_l - 1
          }
          validate(need(r_l >= min_po, "X out of range of the graph!"),
                   need(r_l <= max_po, "X out of range of the graph!"))
          
          highlight_data <- data[data$R >= r_l, ]
          pl <- pl + geom_col(data = highlight_data, aes(x = factor(R), y = pdf), 
                              fill = colors()$col_fill_highlight, color = colors()$col_plot_line)
        } else {
          # Two tails
          if (low_inc == 2) {
            r_l <- r_l - 1
          }
          if (hi_inc == 2) {
            r_u <- r_u - 1
          }
          validate(need(r_l >= min_po, "X Lower out of range of the graph!"),
                   need(r_u <= max_po, "X Upper out of range of the graph!"),
                   need(r_l <= r_u, "X Lower must be less than or equal to X Upper"))
          
          highlight_data <- data[data$R >= r_l & data$R <= r_u, ]
          pl <- pl + geom_col(data = highlight_data, aes(x = factor(R), y = pdf), 
                              fill = colors()$col_fill_highlight, color = colors()$col_plot_line)
        }
      }
      
      pl
    })
    
    # Poisson results function
    pois_results <- reactive({
      # Get parameters first
      lambda <- input$lambda_po
      r_po <- input$r_po
      one_or_two <- input$one_or_two_po
      r_l <- as.numeric(input$poUI1)
      r_u <- as.numeric(input$poUI2)
      R <- input$decimal_po
      
      # Check if basic parameters are available
      if (is.null(lambda)) {
        return(HTML("Loading inputs..."))
      }
      
      # If r_po is TRUE, check for conditional inputs
      if (r_po) {
        if (is.null(one_or_two) || is.null(r_l) || is.null(input$po_low_inc)) {
          return(HTML("Loading conditional inputs..."))
        }
        
        # For two tails, also need r_u and hi_inc
        if (one_or_two == 2 && (is.null(r_u) || is.null(input$po_hi_inc))) {
          return(HTML("Loading conditional inputs..."))
        }
      }
      
      if (!r_po) return(HTML(""))
      
      # Calculate probabilities
      po_table <- table.dist.poisson(lambda = lambda)
      
      if (one_or_two == 1) {
        # One tail
        low_inc <- input$po_low_inc
        if (low_inc == 2) {
          r_l <- r_l - 1
        }
        
        p_at_r <- po_table$p.at.x[r_l + 1]
        p_and_above <- po_table$eq.and.above[r_l + 1]
        p_and_below <- po_table$eq.and.below[r_l + 1]
        
        output <- HTML(paste0(
          "<b>Poisson Distribution Results</b>",
          "<br><br>",
          "<table style='width: 100%; margin-bottom: 20px;'>",
          "<tr>",
          "<td style='padding: 5px;'>p(", r_l, ") = ", ro(p_at_r, R), "</td>",
          "<td style='padding: 5px;'>p(", r_l, " and below) = ", ro(p_and_below, R), "</td>",
          "<td style='padding: 5px;'>p(", r_l, " and above) = ", ro(p_and_above, R), "</td>",
          "</tr>",
          "</table>"
        ))
      } else {
        # Two tails
        low_inc <- input$po_low_inc
        hi_inc <- input$po_hi_inc
        
        if (low_inc == 2) {
          r_l <- r_l - 1
        }
        if (hi_inc == 2) {
          r_u <- r_u - 1
        }
        
        p_at_r_l <- po_table$p.at.x[r_l + 1]
        p_at_r_u <- po_table$p.at.x[r_u + 1]
        p_and_below_l <- po_table$eq.and.below[r_l + 1]
        p_and_above_l <- po_table$eq.and.above[r_l + 1]
        p_and_below_u <- po_table$eq.and.below[r_u + 1]
        p_and_above_u <- po_table$eq.and.above[r_u + 1]
        p_between <- 1 - p_and_below_l - p_and_above_u
        p_tails <- p_and_below_l + p_and_above_u
        
        output <- HTML(paste0(
          "<b>Poisson Distribution Results</b>",
          "<br><br>",
          "<table style='width: 100%; margin-bottom: 20px;'>",
          "<tr>",
          "<td style='padding: 5px;'>p(", r_l, ") = ", ro(p_at_r_l, R), "</td>",
          "<td style='padding: 5px;'>p(", r_l, " and below) = ", ro(p_and_below_l, R), "</td>",
          "<td style='padding: 5px;'>p(", r_l, " and above) = ", ro(p_and_above_l, R), "</td>",
          "</tr>",
          "<tr>",
          "<td style='padding: 5px;'>p(", r_u, ") = ", ro(p_at_r_u, R), "</td>",
          "<td style='padding: 5px;'>p(", r_u, " and below) = ", ro(p_and_below_u, R), "</td>",
          "<td style='padding: 5px;'>p(", r_u, " and above) = ", ro(p_and_above_u, R), "</td>",
          "</tr>",
          "<tr>",
          "<td style='padding: 5px; background-color:", colors()$col_fill, ";'></td>",
          "<td style='padding: 5px;'>p(between) = ", ro(p_between, R), "</td>",
          "<td style='padding: 5px;'>", r_l + 1, " ≤ X ≤ ", r_u - 1, "</td>",
          "</tr>",
          "<tr>",
          "<td style='padding: 5px; background-color:", colors()$col_fill_highlight, ";'></td>",
          "<td style='padding: 5px;'>p(tails) = ", ro(p_tails, R), "</td>",
          "<td style='padding: 5px;'>X ≤ ", r_l, " + X ≥ ", r_u, "</td>",
          "</tr>",
          "</table>"
        ))
      }
      
      output
    })
    
    # Poisson table function
    pois_table <- reactive({
      if (!input$r_po_table) return(NULL)
      
      lambda <- input$lambda_po
      R <- input$decimal_po
      
      # Get the Poisson table and apply rounding
      pois_table_raw <- table.dist.poisson(lambda = lambda)
      pois_table_raw <- as.data.frame(pois_table_raw)
      pois_table_rounded <- ro(pois_table_raw, R)
      
      # Filter out rows with very small probabilities (exact from original app)
      pois_table_rounded[pois_table_rounded$p.at.x > 0.000000001, ]
    })
    
    # Plot output - directly in this module
    output$dist_plot <- renderPlot({
      pois_plot()
    })
    
    # Results output
    output$dist_results <- renderUI({
      pois_results()
    })
    
    # Table output
    output$dist_table <- DT::renderDataTable({
      pois_table()
    }, options = list(dom = 't', pageLength = -1))
    
    # Download functionality
    plot_width <- reactive(400 * 4)
    plot_height <- reactive(300 * 4)
    downloadServer("poisson_plot", pois_plot, height = plot_height, width = plot_width)
  })
}
