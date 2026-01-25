# Geometric Distribution Module for stats4ROI
# This module exactly replicates the geometric distribution functionality from the original app

# Source global configuration for ro() function and colors
source("modules/config/global_config.R")

# Geometric Distribution UI (replicating app.R lines 741-804)
create_geometric_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = ns("pi_geom"),
        label = "π",
        value = 0.05,
        min = 0,
        max = 1,
        step = 0.1,
        width = "200px"
      ),
      numericInput(
        inputId = ns("xmax_geom"),
        label = "Max X to display",
        value = 20,
        min = 1,
        step = 1,
        width = "100px"
      ),
      uiOutput(ns("ui_geom3")),
      prettySwitch(
        inputId = ns("x_geom"),
        label = "Enter X of Interest?",
        value = FALSE,
        status = "success",
        fill = TRUE
      ),
      conditionalPanel(
        condition = paste0("input['", ns("x_geom"), "'] == 1"),
        fluidRow(
          column(12, uiOutput(ns("ui_geom1")))
        )
      ),
      numericInput(
        inputId = ns("decimal_geom"),
        label = "Decimals",
        value = 5,
        min = 0,
        max = 9,
        step = 1,
        width = "75px"
      ),
      prettySwitch(
        inputId = ns("geom_table_show"),
        label = "Show Table?",
        value = FALSE,
        status = "success",
        fill = TRUE
      )
    ),
    mainPanel(
      plotOutput(ns("dist_plot")),
      downloadUI(ns("geometric_plot")),
      htmlOutput(ns("dist_results")),
      DT::dataTableOutput(ns("dist_table"))
    )
  )
}

# Geometric Distribution Server (replicating app.R lines 4251-4455)
create_geometric_server <- function(id, color_palette) {
  moduleServer(id, function(input, output, session) {
    
    # Get namespace function
    ns <- session$ns
    
    # Get colors from palette
    colors <- reactive({
      get_distribution_colors(color_palette())
    })
    
    # Dynamic UI outputs (replicating app.R lines 4254-4291)
    output$ui_geom1 <- renderUI({
      low_inc <- input$geom_low_inc
      req(low_inc)
      
      if (low_inc == 1) {
        xmin <- 1
      } else {
        xmin <- 0
      }
      
      numericInput(
        inputId = ns("geomUI1"),
        label = "X",
        value = xmin,
        min = xmin,
        step = 1,
        width = "75px"
      )
    })
    
    output$ui_geom3 <- renderUI({
      radioButtons(
        inputId = ns("geom_low_inc"),
        label = "Select p()",
        choices = c("p(First Occurrence is X)" = 1, "p(X Non-Occurrences)" = 2)
      )
    })
    
    # Geometric plot (replicating app.R lines 4300-4382)
    geom_plot <- reactive({
      # Extract parameters
      xmax <- input$xmax_geom
      p <- input$pi_geom
      x_geom <- input$x_geom
      x_l <- as.numeric(input$geomUI1)
      low_inc <- input$geom_low_inc
      
      # Basic parameter checks
      if (is.null(xmax) || is.null(p) || is.null(low_inc)) {
        return(NULL)
      }
      
      # Validation checks
      req(low_inc)
      validate(
        need(xmax == round(xmax), "Enter integer for the max X"),
        need(xmax >= 0, "Enter a non-negative number for the max X"),
        need(p > 0 && p < 1, "π must be a proportion between 0 and 1")
      )
      
      # Determine x range based on form
      if (low_inc == 1) {
        xmin <- 1
      } else {
        xmin <- 0
      }
      
      x <- seq(xmin, xmax)
      
      # Calculate probabilities based on form
      if (low_inc == 1) {
        p_x <- dgeom(x = x - 1, prob = p)
      } else {
        p_x <- dgeom(x = x, prob = p)
      }
      
      plot_data <- data.frame(x = x, p = p_x)
      plot_data$x <- factor(plot_data$x)
      
      # Create legend names for colors
      leg_names <- c("Non-occurrence" = unname(colors()$col_fill_highlight), 
                     "Occurrence" = unname(colors()$col_mean_line), 
                     "Future" = unname(colors()$col_fill))
      
      # Create base plot
      pl <- ggplot(data = plot_data, aes(x = x, y = p)) +
        theme(legend.position = "none") +
        scale_fill_manual(values = leg_names) +
        geom_col(aes(fill = "Future"), color = unname(colors()$col_plot_line))
      
      # Add highlighting for X of interest
      if (x_geom) {
        if (!is.null(x_l)) {
          validate(
            need(x_l == round(x_l), "Enter integer for X of interest"),
            need(x_l <= xmax, "Enter X below the max X to display"),
            need(x_l >= 0, "Enter a non-negative X")
          )
          
          pl <- pl +
            theme(legend.position = "bottom") +
            labs(fill = "")
          
          if (low_inc == 1) {
            sel_x <- seq(xmin, x_l)
            sel_y <- dgeom(x = sel_x - 1, prob = p)
          } else {
            sel_x <- seq(xmin, x_l)
            sel_y <- dgeom(x = sel_x, prob = p)
          }
          
          selected <- data.frame(x = sel_x, p = sel_y)
          selected$x <- factor(selected$x)
          
          pl <- pl +
            geom_col(data = selected, aes(x = x, y = p, fill = "Non-occurrence"))
          
          if (low_inc == 1) {
            pl <- pl +
              geom_col(data = selected[x_l, ], aes(x = x, y = p, fill = "Occurrence"))
          } else {
            temp <- data.frame(x = x_l + 2, p = dgeom(x_l + 1, p))
            pl <- pl +
              geom_col(data = temp, aes(x = x, y = p, fill = "Occurrence"))
          }
        }
      }
      
      # Final plot formatting
      pl +
        ggtitle(paste("Geometric Distribution with π =", p)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("X") +
        ylab("p(X)") +
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
    })
    
    # Geometric results (replicating app.R lines 4384-4432)
    geom_results <- reactive({
      # Extract parameters
      p <- input$pi_geom
      xmax <- input$xmax_geom
      x_geom <- input$x_geom
      x_l <- as.numeric(input$geomUI1)
      low_inc <- input$geom_low_inc
      R <- input$decimal_geom
      
      # Basic parameter checks
      if (is.null(p) || is.null(xmax) || is.null(R)) {
        return(NULL)
      }
      
      if (x_geom == FALSE) {
        return(NULL)
      }
      
      if (is.null(x_l) || is.null(low_inc)) {
        return(NULL)
      }
      
      validate(need(x_l == round(x_l), "Enter integer for X of interest"))
      req(low_inc)
      
      # Build results table
      output <- paste("<table width=100%; style='border-spacing: 10px; border-collapse: separate;'>")
      
      if (low_inc == 1) {
        # Probability that the first occurrence is X
        output <- paste0(output,
          "<tr><td style='background-color:", colors()$col_fill_highlight, "; padding: 5px;'></td>",
          "<td style='padding: 5px;'>p(First Occurrence on or before ", x_l, ") = ", ro(pgeom(q = x_l - 1, prob = p, lower.tail = TRUE), R), "</td></tr>",
          "<tr><td style='background-color:", colors()$col_mean_line, "; padding: 5px;'></td>",
          "<td style='padding: 5px;'>p(First Occurrence is ", x_l, ") = ", ro(dgeom(x = x_l - 1, prob = p), R), "</td></tr>",
          "<tr><td style='background-color:", colors()$col_fill, "; padding: 5px;'></td>",
          "<td style='padding: 5px;'>p(First Occurrence on or after ", x_l, ") = ", ro(pgeom(q = x_l - 2, prob = p, lower.tail = FALSE), R), "</td></tr>"
        )
      } else {
        # Probability no occurrences until X
        output <- paste0(output,
          "<tr><td style='background-color:", colors()$col_fill_highlight, "; padding: 5px;'></td>",
          "<td style='padding: 5px;'>p(No Occurrence on or before ", x_l, ") = ", ro(pgeom(q = x_l, prob = p, lower.tail = TRUE), R), "</td></tr>",
          "<tr><td style='background-color:", colors()$col_mean_line, "; padding: 5px;'></td>",
          "<td style='padding: 5px;'>p(First Occurrence at ", x_l, ") = ", ro(dgeom(x = x_l, prob = p), R), "</td></tr>",
          "<tr><td style='background-color:", colors()$col_fill, "; padding: 5px;'></td>",
          "<td style='padding: 5px;'>p(No Occurrence on or after ", x_l, ") = ", ro(pgeom(q = x_l - 1, prob = p, lower.tail = FALSE), R), "</td></tr>"
        )
      }
      
      output <- paste(output, "</table>")
      HTML(output)
    })
    
    # Geometric table (replicating app.R lines 4435-4455)
    geom_table <- reactive({
      # Extract parameters
      xmax <- input$xmax_geom
      p <- input$pi_geom
      R <- input$decimal_geom
      geom_table_show <- input$geom_table_show
      low_inc <- input$geom_low_inc
      
      # Basic parameter checks
      if (is.null(xmax) || is.null(p) || is.null(R) || is.null(low_inc)) {
        return(NULL)
      }
      
      if (geom_table_show == FALSE) {
        return(NULL)
      }
      
      # Build the table
      output <- table.dist.geometric(p = p, max.x = xmax)
      output <- as.data.frame(output)
      if (low_inc == 1) {
        output$x <- output$x + 1
      }
      row.names(output) <- output$x
      output <- ro(output, R)
      output <- output[output$p.at.x > 0.000000001, ] # filter low p
      
      output
    })
    
    # Render outputs
    output$dist_plot <- renderPlot({
      geom_plot()
    })
    
    output$dist_results <- renderUI({
      geom_results()
    })
    
    output$dist_table <- DT::renderDataTable({
      geom_table()
    }, options = list(dom = 't', pageLength = -1))
    
    # Download dimensions
    plot_width <- reactive(400 * 4)
    plot_height <- reactive(300 * 4)
    
    # Download server
    downloadServer("geometric_plot", geom_plot, height = plot_height, width = plot_width)
  })
}
