# F-Distribution Module for stats4ROI
# This module exactly replicates the F-distribution functionality from the original app

# Source global configuration for ro() function and colors
source("modules/config/global_config.R")

# F-Distribution UI (replicating app.R lines 959-1017)
create_f_distribution_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = ns("df1_f"),
        label = "df1",
        value = 5,
        width = "100px"
      ),
      numericInput(
        inputId = ns("df2_f"),
        label = "df2",
        value = 10,
        width = "100px"
      ),
      radioButtons(
        inputId = ns("tails_f"),
        label = "One or Two Tails?",
        choices = c("One-Tail" = 1, "Two-Tails" = 2)
      ),
      uiOutput(ns("lowertail_f")),
      uiOutput(ns("uppertail_f")),
      uiOutput(ns("inorout_f")),
      checkboxInput(
        inputId = ns("data_label_f"),
        label = "Label Graph?",
        value = TRUE
      ),
      numericInput(
        inputId = ns("decimal_f"),
        label = "Decimals",
        value = 4,
        min = 0,
        max = 9,
        step = 1,
        width = "75px"
      )
    ),
    mainPanel(
      plotOutput(ns("dist_plot"), height = "300px"),
      downloadUI(ns("f_distribution_plot")),
      div(
        DT::dataTableOutput(ns("dist_table")),
        style = "margin:auto; width: 50%"
      )
    )
  )
}

# F-Distribution Server (replicating app.R lines 4892-5163)
create_f_distribution_server <- function(id, color_palette) {
  moduleServer(id, function(input, output, session) {
    
    # Get namespace function
    ns <- session$ns
    
    # Get colors from palette
    colors <- reactive({
      get_distribution_colors(color_palette())
    })
    
    # Dynamic UI outputs (replicating app.R lines 4895-4941)
    output$lowertail_f <- renderUI({
      tails <- input$tails_f
      req(tails)
      
      if (tails == 1) {
        numericInput(
          inputId = ns("tail1_f"),
          label = "Point of Interest",
          value = 0.5,
          width = "100px"
        )
      } else {
        numericInput(
          inputId = ns("tail1_f"),
          label = "Lower Tail",
          value = 0.5,
          width = "100px"
        )
      }
    })
    
    output$uppertail_f <- renderUI({
      tails <- input$tails_f
      req(tails)
      
      if (tails == 1) {
        NULL
      } else {
        numericInput(
          inputId = ns("tail2_f"),
          label = "Upper Tail",
          value = 3,
          width = "100px"
        )
      }
    })
    
    output$inorout_f <- renderUI({
      tails <- input$tails_f
      req(tails)
      
      if (tails == 1) {
        radioButtons(
          inputId = ns("in_or_out_f"),
          label = "Area above or below?",
          choices = c("Above" = 1, "Below" = 2)
        )
      } else {
        radioButtons(
          inputId = ns("in_or_out_f"),
          label = "Area between or outside of points?",
          choices = c("Inside" = 1, "Outside" = 2)
        )
      }
    })
    
    # F-distribution plot (replicating app.R lines 4951-5055)
    f_plot <- reactive({
      # Extract parameters
      df1 <- input$df1_f
      df2 <- input$df2_f
      tails <- input$tails_f
      R <- input$decimal_f
      data_label <- input$data_label_f
      X1 <- input$tail1_f
      X2 <- input$tail2_f
      in_or_out <- input$in_or_out_f
      
      # Basic parameter checks
      if (is.null(df1) || is.null(df2) || is.null(tails) || is.null(R)) {
        return(NULL)
      }
      
      # Validation checks
      validate(
        need(df1, "Need df1"),
        need(df2, "Need df2"),
        need(X1, "Need X"),
        need(R, "Need the number of decimal places")
      )
      
      if (tails == 2) {
        validate(
          need(X2, "Need second tail"),
          need(X1 <= X2, "X1 needs to be less than X2"),
          need(in_or_out, "You shouldn't see this error")
        )
      }
      
      # Calculate mode and range
      dmode <- ((df1 - 2) / df1) * (df2 / (df2 + 2))
      
      d <- 1
      xmax <- dmode
      while (d > 0.001) {
        xmax <- xmax + 0.01
        d <- df(xmax, df1 = df1, df2 = df2)
      }
      
      d <- 1
      xmin <- dmode
      while (d > 0.001) {
        xmin <- xmin - 0.01
        d <- df(xmin, df1 = df1, df2 = df2)
      }
      if (xmin < 0) xmin <- 0
      
      xmin <- min(X1, xmin)
      if (tails == 1) {
        xmax <- max(X1, xmax)
      } else {
        xmax <- max(X1, X2, xmax)
      }
      
      area <- 0
      
      # Create base plot
      p <- ggplot()
      
      # Add shading based on tails and area selection
      if (!is.null(X1)) {
        if (in_or_out == 1 && tails == 2) {
          # Inside two tails
          p <- p +
            stat_function(geom = "area", fun = df, args = list(df1 = df1, df2 = df2), 
                         xlim = c(X1, X2), fill = unname(colors()$col_fill_highlight), alpha = 0.7)
          area <- area + pf(q = X1, df1 = df1, df2 = df2, lower.tail = FALSE) - 
                         pf(X2, df1 = df1, df2 = df2, lower.tail = FALSE)
        } else if (in_or_out == 1 && tails == 1) {
          # Above one tail
          area <- area + pf(q = X1, df1 = df1, df2 = df2, lower.tail = FALSE)
          p <- p +
            stat_function(geom = "area", fun = df, args = list(df1 = df1, df2 = df2), 
                         xlim = c(X1, xmax), fill = unname(colors()$col_fill_highlight), alpha = 0.7)
        } else if (in_or_out == 2) {
          # Below one tail
          area <- area + pf(q = X1, df1 = df1, df2 = df2)
          p <- p +
            stat_function(geom = "area", fun = df, args = list(df1 = df1, df2 = df2), 
                         xlim = c(xmin, X1), fill = unname(colors()$col_fill_highlight), alpha = 0.7)
        }
        
        # Add vertical line at X1
        p <- p +
          geom_vline(xintercept = X1, color = unname(colors()$col_point_of_interest_line), 
                    linetype = 5, linewidth = 1)
      }
      
      if (!is.null(X2) && tails == 2) {
        if (in_or_out == 2) {
          # Outside two tails
          area <- area + pf(X2, df1 = df1, df2 = df2, lower.tail = FALSE)
          p <- p +
            stat_function(geom = "area", fun = df, args = list(df1 = df1, df2 = df2), 
                         xlim = c(X2, xmax), fill = unname(colors()$col_fill_highlight), alpha = 0.7)
        }
        
        # Add vertical line at X2
        p <- p +
          geom_vline(xintercept = X2, color = unname(colors()$col_point_of_interest_line), 
                    linetype = 5, linewidth = 1)
      }
      
      # Add labels if requested
      if (data_label) {
        if (!is.null(X1)) {
          p <- p +
            geom_label(aes(x = X1, y = Inf), label = paste0("X1 = ", X1), vjust = "inward")
        }
        if (tails == 2) {
          p <- p +
            geom_label(aes(x = X2, y = Inf), label = paste0("X2 = ", X2), vjust = "inward")
        }
      }
      
      # Final plot formatting
      p +
        ggtitle(paste0("An F-distribution with μ = ", df1, " and σ = ", df2, " degrees of freedom"),
                subtitle = paste0("Shaded area = ", ro(area, R))) +
        geom_function(fun = df, args = list(df1 = df1, df2 = df2), n = 512, 
                     color = unname(colors()$col_plot_line), linewidth = 1) +
        xlim(xmin, xmax) +
        ylab("Density") +
        xlab("F") +
        theme(legend.position = "none")
    })
    
    # F-distribution table (replicating app.R lines 5130-5163)
    f_table <- reactive({
      # Extract parameters
      df1 <- input$df1_f
      df2 <- input$df2_f
      tails <- input$tails_f
      R <- input$decimal_f
      X1 <- input$tail1_f
      X2 <- input$tail2_f
      in_or_out <- input$in_or_out_f
      
      # Basic parameter checks
      if (is.null(df1) || is.null(df2) || is.null(tails) || is.null(R) || is.null(X1)) {
        return(NULL)
      }
      
      if (tails == 1) {
        # One tail calculations
        x_or_lower <- pf(q = X1, df1 = df1, df2 = df2, lower.tail = TRUE)  # X or lower
        x_or_higher <- pf(q = X1, df1 = df1, df2 = df2, lower.tail = FALSE) # X or higher
        
        # Create the table
        data_labels <- c("df1 = ", "df2 = ", "X = ", "Area above X", "Area below X")
        results <- c(df1, df2, X1, x_or_higher, x_or_lower)
        f_table <- data.frame(data_labels, results)
        ro(f_table, R)
      } else {
        # Two tails calculations
        req(X2)
        outside_x1_x2 <- 1 - (pf(q = max(X1, X2), df1 = df1, df2 = df2, lower.tail = TRUE) - 
                              pf(q = min(X1, X2), df1 = df1, df2 = df2, lower.tail = TRUE)) # Outside of X1 and X2
        inside_x1_x2 <- 1 - outside_x1_x2
        
        data_labels <- c("df1 = ", "df2 = ", "Lower X = ", "Upper X = ", "Area outside = ", "Area inside = ")
        results <- c(df1, df2, X1, X2, outside_x1_x2, inside_x1_x2)
        f_table <- data.frame(data_labels, results)
        ro(f_table, R)
      }
    })
    
    # Render outputs
    output$dist_plot <- renderPlot({
      f_plot()
    })
    
    output$dist_table <- DT::renderDataTable({
      DT::datatable(f_table(), 
                   options = list(dom = "t", paging = FALSE, autoWidth = TRUE, 
                                columnDefs = list(list(width = '150px', targets = c(0, 1)))), 
                   rownames = FALSE, 
                   colnames = c("", "Result"))
    })
    
    # Download dimensions
    plot_width <- reactive(400 * 4)
    plot_height <- reactive(300 * 4)
    
    # Download server
    downloadServer("f_distribution_plot", f_plot, height = plot_height, width = plot_width)
  })
}
