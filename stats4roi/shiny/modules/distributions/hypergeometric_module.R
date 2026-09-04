# Hypergeometric Distribution Module for stats4ROI
# This module exactly replicates the hypergeometric distribution functionality from the original app

# Source global configuration for ro() function and colors
source("modules/config/global_config.R")
source("modules/distributions/discrete_x_of_interest.R")

# Hypergeometric Distribution UI (replicating app.R lines 664-738)
create_hypergeometric_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = ns("pi_hype"),
        label = "π",
        value = 0.05,
        min = 0,
        max = 1,
        step = 0.1,
        width = "200px"
      ),
      numericInput(
        inputId = ns("k_hype"),
        label = "Sample Size (k)",
        value = 30,
        min = 1,
        step = 1,
        width = "100px"
      ),
      numericInput(
        inputId = ns("N_hype"),
        label = "Total population (N)",
        value = 200,
        min = 1,
        step = 1,
        width = "100px"
      ),
      prettySwitch(
        inputId = ns("x_hype"),
        label = "Enter X of Interest?",
        value = FALSE,
        status = "success",
        fill = TRUE
      ),
      conditionalPanel(
        condition = paste0("input['", ns("x_hype"), "'] == 1"),
        radioButtons(
          inputId = ns("one_or_two_hype"),
          label = "One or Two Tails?",
          choices = c("One-Tail" = 1, "Two-Tails" = 2)
        ),
        fluidRow(
          column(6, uiOutput(ns("ui_hype1"))),
          column(6, uiOutput(ns("ui_hype3")))
        ),
        fluidRow(
          column(6, uiOutput(ns("ui_hype2"))),
          column(6, uiOutput(ns("ui_hype4")))
        )
      ),
      numericInput(
        inputId = ns("decimal_hype"),
        label = "Decimals",
        value = 5,
        min = 0,
        max = 9,
        step = 1,
        width = "75px"
      ),
      prettySwitch(
        inputId = ns("hype_table_show"),
        label = "Show Table?",
        value = FALSE,
        status = "success",
        fill = TRUE
      )
    ),
    mainPanel(
      plotOutput(ns("dist_plot")),
      downloadUI(ns("hypergeometric_plot")),
      htmlOutput(ns("dist_results")),
      DT::dataTableOutput(ns("dist_table"))
    )
  )
}

# Hypergeometric Distribution Server (replicating app.R lines 4051-4246)
create_hypergeometric_server <- function(id, color_palette) {
  moduleServer(id, function(input, output, session) {
    
    # Get namespace function
    ns <- session$ns
    
    # Get colors from palette
    colors <- reactive({
      get_distribution_colors(color_palette())
    })
    
    # Dynamic UI outputs (replicating app.R lines 4054-4100)
    output$ui_hype1 <- renderUI({
      one_or_two <- input$one_or_two_hype
      req(one_or_two)
      
      if (one_or_two == 1) {
        output <- numericInput(
          inputId = ns("hypeUI1"),
          label = "X",
          value = 0,
          min = 0,
          step = 1,
          width = "75px"
        )
      }
      if (one_or_two == 2) {
        output <- numericInput(
          inputId = ns("hypeUI1"),
          label = "Lower X",
          value = 0,
          min = 0,
          step = 1,
          width = "75px"
        )
      }
      output
    })
    
    output$ui_hype2 <- renderUI({
      one_or_two <- input$one_or_two_hype
      req(one_or_two)
      
      if (one_or_two == 1) {
        output <- NULL
      }
      if (one_or_two == 2) {
        output <- numericInput(
          inputId = ns("hypeUI2"),
          label = "Upper X",
          value = 4,
          min = 0,
          step = 1,
          width = "75px"
        )
      }
      output
    })
    
    output$ui_hype3 <- renderUI({
      enterX <- input$x_hype
      one_or_two <- input$one_or_two_hype
      if (!enterX) {
        output <- NULL
      } else {
        low_label <- if (!is.null(one_or_two) && one_or_two == 2) {
          "Include X in lower tail"
        } else {
          "Include X?"
        }
        output <- radioButtons(
          inputId = ns("hype_low_inc"),
          label = low_label,
          choices = c("≤" = 1, "<" = 2)
        )
      }
      output
    })
    
    output$ui_hype4 <- renderUI({
      one_or_two <- input$one_or_two_hype
      req(one_or_two)
      if (one_or_two == 1) {
        output <- NULL
      } else {
        output <- radioButtons(
          inputId = ns("hype_hi_inc"),
          label = "Include X in upper tail",
          choices = c("≥" = 1, ">" = 2)
        )
      }
      output
    })
    
    # Hypergeometric plot (replicating app.R lines 4109-4174)
    hype_plot <- reactive({
      # Extract parameters
      k <- input$k_hype
      N <- input$N_hype
      p <- input$pi_hype
      x_hype <- input$x_hype
      one_or_two <- input$one_or_two_hype
      x_l <- as.numeric(input$hypeUI1)
      x_u <- as.numeric(input$hypeUI2)
      low_inc <- input$hype_low_inc
      hi_inc <- input$hype_hi_inc
      
      # Basic parameter checks
      if (is.null(k) || is.null(N) || is.null(p)) {
        return(NULL)
      }
      
      # Validation checks
      validate(
        need(N == round(N), "Enter integer for the population size"),
        need(p > 0 && p < 1, "π must be a proportion between 0 and 1")
      )
      
      # Calculate m (success count in population)
      m <- ceiling(x = p * N)
      
      # Calculate x range
      xmin <- max(0, round((p * k) - 4 * ((k * p * (1 - p))^0.5)))
      xmax <- min(m, k + 1)
      
      # Adjust range based on X of interest
      if (x_hype && one_or_two == 1) {
        if (!is.null(x_l)) {
          validate(need(x_l == round(x_l), "Enter integer for the point of interest"))
          xmin <- min(xmin, x_l)
        }
      } else if (x_hype && one_or_two == 2) {
        if (!is.null(x_l) && !is.null(x_u)) {
          validate(
            need(x_l == round(x_l), "Enter integer for the lower point of interest"),
            need(x_u == round(x_u), "Enter integer for the upper point of interest"),
            need(x_u > x_l, "The upper point of interest needs to be greater than the lower point of interest")
          )
          xmin <- min(xmin, x_l)
          xmax <- max(xmax, x_u)
        }
      }
      
      # Generate data
      x <- seq(xmin, xmax)
      p_x <- dhyper(x = x, m = m, n = N - m, k = k)
      
      plot_data <- data.frame(x = x, p = p_x)
      plot_data$x <- factor(plot_data$x)
      
      # Create base plot
      pl <- ggplot(data = plot_data, aes(x = x, y = p)) +
        geom_col(fill = unname(colors()$col_fill), color = unname(colors()$col_plot_line))
      
      # Add highlighting for X of interest
      if (x_hype) {
        adj <- adjust_discrete_x_of_interest(
          x_l, low_inc, x_u, hi_inc, two_tails = (one_or_two == 2)
        )
        x_l <- adj$x_l
        x_u <- adj$x_u
        if (one_or_two == 1) {
          lower <- data.frame(x = x, p = p_x)
          lower[which(lower$x > x_l), 2] <- 0
          lower$x <- factor(lower$x)
          pl <- pl +
            geom_col(data = lower, aes(x = x, y = p), fill = unname(colors()$col_fill_highlight), color = unname(colors()$col_plot_line))
        } else {
          tails <- data.frame(x = x, p = p_x)
          tails[which(tails$x > x_l & tails$x < x_u), 2] <- 0
          tails$x <- factor(tails$x)
          pl <- pl +
            geom_col(data = tails, aes(x = x, y = p), fill = unname(colors()$col_fill_highlight), color = unname(colors()$col_plot_line))
        }
      }
      
      # Final plot formatting
      pl + theme(legend.position = "none") +
        ggtitle(paste("Hypergeometric Distribution with π =", p, ", m =", m, "\n k =", k, "and N =", N)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("X") +
        ylab("p(X)") +
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
    })
    
    # Hypergeometric results (replicating app.R lines 4176-4225)
    hype_results <- reactive({
      # Extract parameters
      k <- input$k_hype
      p <- input$pi_hype
      N <- input$N_hype
      x_hype <- input$x_hype
      one_or_two <- input$one_or_two_hype
      x_l <- as.numeric(input$hypeUI1)
      x_u <- as.numeric(input$hypeUI2)
      low_inc <- input$hype_low_inc
      hi_inc <- input$hype_hi_inc
      R <- input$decimal_hype
      
      # Basic parameter checks
      if (is.null(k) || is.null(p) || is.null(N) || is.null(R)) {
        return(NULL)
      }
      
      if (x_hype == FALSE) {
        return(NULL)
      }
      
      # Calculate m and get table
      m <- ceiling(x = p * N)
      hype_table <- table.dist.hypergeometric(pop.success.count = m, total.count = N, sample.size = k)
      
      req(x_l, low_inc)
      two_tails <- (one_or_two == 2)
      if (two_tails) req(x_u, hi_inc)
      probs <- discrete_x_of_interest_probs(
        hype_table, x_l, low_inc, x_u, hi_inc, two_tails = two_tails
      )
      x_l <- probs$lower$x
      
      # Build results table
      output <- paste(
        "<table width=100%; style='border-spacing: 10px; border-collapse: separate;'><tr>",
        "<td style='padding: 5px;'>p(", x_l, ") =", ro(probs$lower$p_at, R), "</td>",
        "<td style='padding: 5px;'>p(", x_l, " and below) =", ro(probs$lower$p_and_below, R), "</td>",
        "<td style='padding: 5px;'>p(", x_l, " and above) =", ro(probs$lower$p_and_above, R), "</td></tr>"
      )
      
      if (two_tails) {
        x_u <- probs$upper$x
        output <- paste(output,
          paste(
            "<tr><td style='padding: 5px;'>p(", x_u, ") =", ro(probs$upper$p_at, R), "</td>",
            "<td style='padding: 5px;'>p(", x_u, " and below) =", ro(probs$upper$p_and_below, R), "</td>",
            "<td style='padding: 5px;'>p(", x_u, " and above) =", ro(probs$upper$p_and_above, R), "</td></tr>",
            "<tr><td style='background-color:", colors()$col_fill, "; padding: 5px;'></td>",
            "<td style='padding: 5px;'>p(between) =", ro(probs$p_between, R), "</td><td style='padding: 5px;'>", paste0(x_l + 1, " ≤ X ≤ ", x_u - 1), "</td></tr>",
            "<tr><td style='background-color:", colors()$col_fill_highlight, "; padding: 5px;'></td>",
            "<td style='padding: 5px;'>p(tails) =", ro(probs$p_tails, R), "</td><td style='padding: 5px;'>", paste0("X ≤ ", x_l, " + X ≥ ", x_u), "</td></tr>"
          )
        )
      }
      
      output <- paste(output, "</table><br><br>")
      HTML(output)
    })
    
    # Hypergeometric table (replicating app.R lines 4227-4246)
    hype_table <- reactive({
      # Extract parameters
      k <- input$k_hype
      N <- input$N_hype
      p <- input$pi_hype
      R <- input$decimal_hype
      hype_table_show <- input$hype_table_show
      
      # Basic parameter checks
      if (is.null(k) || is.null(N) || is.null(p) || is.null(R)) {
        return(NULL)
      }
      
      if (hype_table_show == FALSE) {
        return(NULL)
      }
      
      # Build the table
      m <- ceiling(N * p)
      output <- table.dist.hypergeometric(pop.success.count = m, total.count = N, sample.size = k)
      output <- as.data.frame(output)
      output <- ro(output, R)
      output <- output[output$p.at.x > 0.000000001, ] # filter low p
      
      output
    })
    
    # Render outputs
    output$dist_plot <- renderPlot({
      hype_plot()
    })
    
    output$dist_results <- renderUI({
      hype_results()
    })
    
    output$dist_table <- DT::renderDataTable({
      hype_table()
    }, options = list(dom = 't', pageLength = -1))
    
    # Download dimensions
    plot_width <- reactive(400 * 4)
    plot_height <- reactive(300 * 4)
    
    # Download server
    downloadServer("hypergeometric_plot", hype_plot, height = plot_height, width = plot_width)
  })
}
