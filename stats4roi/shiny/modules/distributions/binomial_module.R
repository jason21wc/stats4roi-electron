# Binomial Distribution Module for stats4ROI
# This module exactly replicates the Binomial distribution functionality from the original app

# Source global configuration for ro() function and colors
source("modules/config/global_config.R")
source("modules/distributions/discrete_x_of_interest.R")

# Binomial Distribution UI (replicating app.R lines 593-661)
create_binomial_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = ns("pi_bi"),
        label = "π",
        value = 0.5,
        min = 0,
        max = 1,
        step = 0.1,
        width = "200px"
      ),
      numericInput(
        inputId = ns("n_bi"),
        label = "n",
        value = 10,
        min = 1,
        step = 1,
        width = "100px"
      ),
      prettySwitch(
        inputId = ns("r_bi"),
        label = "Enter R of Interest?",
        value = FALSE,
        status = "success",
        fill = TRUE
      ),
      conditionalPanel(
        condition = paste0("input['", ns("r_bi"), "'] == 1"),
        radioButtons(
          inputId = ns("one_or_two_bi"),
          label = "One or Two Tails?",
          choices = c("One-Tail" = 1, "Two-Tails" = 2)
        ),
        fluidRow(
          column(6, uiOutput(ns("ui_bi1"))),
          column(6, uiOutput(ns("ui_bi3")))
        ),
        fluidRow(
          column(6, uiOutput(ns("ui_bi2"))),
          column(6, uiOutput(ns("ui_bi4")))
        )
      ),
      numericInput(
        inputId = ns("decimal_bi"),
        label = "Decimals",
        value = 5,
        min = 0,
        max = 9,
        step = 1,
        width = "75px"
      ),
      prettySwitch(
        inputId = ns("r_bi_table"),
        label = "Show Table?",
        value = FALSE,
        status = "success",
        fill = TRUE
      )
    ),
    mainPanel(
      plotOutput(outputId = ns("dist_plot"), height = "300px"),
      downloadUI(ns("binomial_plot")),
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

# Binomial Distribution Server (replicating app.R lines 3978-4048)
create_binomial_server <- function(id, color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    # Get colors from palette
    colors <- reactive({
      get_distribution_colors(color_palette())
    })
    
    
    # UI outputs for conditional inputs
    output$ui_bi1 <- renderUI({
      if (input$one_or_two_bi == 1) {
        numericInput(
          inputId = ns("biUI1"),
          label = "R",
          value = 5,
          min = 0,
          step = 1,
          width = "100px"
        )
      } else {
        numericInput(
          inputId = ns("biUI1"),
          label = "R Lower",
          value = 3,
          min = 0,
          step = 1,
          width = "100px"
        )
      }
    })
    
    output$ui_bi2 <- renderUI({
      if (input$one_or_two_bi == 1) {
        radioButtons(
          inputId = ns("bi_low_inc"),
          label = "Include R?",
          choices = c("Yes" = 1, "No" = 2),
          selected = 1
        )
      } else {
        numericInput(
          inputId = ns("biUI2"),
          label = "R Upper",
          value = 7,
          min = 0,
          step = 1,
          width = "100px"
        )
      }
    })
    
    output$ui_bi3 <- renderUI({
      if (input$one_or_two_bi == 2) {
        radioButtons(
          inputId = ns("bi_low_inc"),
          label = "Include R Lower?",
          choices = c("Yes" = 1, "No" = 2),
          selected = 1
        )
      } else {
        NULL
      }
    })
    
    output$ui_bi4 <- renderUI({
      if (input$one_or_two_bi == 2) {
        radioButtons(
          inputId = ns("bi_hi_inc"),
          label = "Include R Upper?",
          choices = c("Yes" = 1, "No" = 2),
          selected = 1
        )
      } else {
        NULL
      }
    })
    
    # Binomial plot function
    bi_plot <- reactive({
      # Get parameters first
      n <- input$n_bi
      p <- input$pi_bi
      r_bi <- input$r_bi
      one_or_two <- input$one_or_two_bi
      r_l <- as.numeric(input$biUI1)
      r_u <- as.numeric(input$biUI2)
      low_inc <- input$bi_low_inc
      hi_inc <- input$bi_hi_inc
      
      # Check if basic parameters are available (only n and p needed for basic plot)
      if (is.null(n) || is.null(p)) {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Loading inputs...", size = 6) +
               theme_void())
      }
      
      # If r_bi is TRUE, check for conditional inputs
      if (r_bi) {
        if (is.null(one_or_two) || is.null(r_l) || is.null(low_inc)) {
          return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "Loading conditional inputs...", size = 6) +
                 theme_void())
        }
        
        # For two tails, also need r_u and hi_inc
        if (one_or_two == 2 && (is.null(r_u) || is.null(hi_inc))) {
          return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "Loading conditional inputs...", size = 6) +
                 theme_void())
        }
      }
      
      # Validation (exact from original)
      validate(need(n == round(n), "Enter integer for the sample size"),
               need(p > 0 && p < 1, "p must be a proportion between 0 and 1"))
      
      # Calculate range (exact from original)
      rmin <- max(0, round((p * n) - 4 * ((n * p * (1 - p))^0.5)))
      rmax <- round((p * n) + 4 * ((n * p * (1 - p))^0.5))
      
      if (r_bi && one_or_two == 1) {
        validate(need(r_l == round(r_l), "Enter integer for the point of interest"))
        rmin <- min(rmin, r_l)
      } else if (r_bi && one_or_two == 2) {
        validate(need(r_l == round(r_l), "Enter integer for the lower point of interest"),
                 need(r_u == round(r_u), "Enter integer for the upper point of interest"),
                 need(r_u > r_l, "The upper point of interest needs to be greater than the lower point of interest"))
        rmin <- min(rmin, r_l)
        rmax <- max(rmax, r_u)
      }
      
      # Create data for plotting (exact from original)
      r <- seq(rmin, rmax)
      p_r <- dbinom(r, n, p)
      
      plot_data <- data.frame(r = r, p = p_r)
      plot_data$r <- factor(plot_data$r)
      
      # Base plot (exact from original)
      pl <- ggplot(data = plot_data, aes(x = r, y = p)) +
        geom_col(fill = colors()$col_fill, color = colors()$col_plot_line)
      
      # Add highlighting for points of interest (same include/exclude as results)
      if (r_bi) {
        adj <- adjust_discrete_x_of_interest(
          r_l, low_inc, r_u, hi_inc, two_tails = (one_or_two == 2)
        )
        r_l <- adj$x_l
        r_u <- adj$x_u
        if (one_or_two == 1) {
          lower <- data.frame(r = r, p = p_r)
          lower[which(lower$r > r_l), 2] <- 0
          lower$r <- factor(lower$r)
          pl <- pl + geom_col(data = lower, aes(x = r, y = p), fill = colors()$col_fill_highlight, color = colors()$col_plot_line)
        } else {
          tails <- data.frame(r = r, p = p_r)
          tails[which(tails$r > r_l & tails$r < r_u), 2] <- 0
          tails$r <- factor(tails$r)
          pl <- pl + geom_col(data = tails, aes(x = r, y = p), fill = colors()$col_fill_highlight, color = colors()$col_plot_line)
        }
      }
      
      # Final formatting (exact from original)
      pl + theme(legend.position = "none") +
        ggtitle(paste("Binomial Distribution with π =", p, "and n =", n)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("R") +
        ylab("p(R)") +
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
    })
    
    # Binomial results function
    bi_results <- reactive({
      n <- input$n_bi
      p <- input$pi_bi
      r_bi <- input$r_bi
      one_or_two <- input$one_or_two_bi
      r_l <- as.numeric(input$biUI1)
      r_u <- as.numeric(input$biUI2)
      low_inc <- input$bi_low_inc
      hi_inc <- input$bi_hi_inc
      R <- input$decimal_bi
      
      if (!r_bi) return(HTML(""))
      
      # Check if required inputs are available
      if (is.null(one_or_two) || is.null(r_l) || is.null(low_inc)) {
        return(HTML("Please select tail options and enter R values"))
      }
      
      # Calculate probabilities (include/exclude R, same convention as Poisson)
      bi_table <- table.dist.binomial(n = n, p = p)
      two_tails <- (one_or_two == 2)
      probs <- discrete_x_of_interest_probs(
        bi_table, r_l, low_inc, r_u, hi_inc, two_tails = two_tails
      )
      r_l <- probs$lower$x

      if (one_or_two == 1) {
        output <- HTML(paste0(
          "<b>Binomial Distribution Results</b>",
          "<br><br>",
          "<table style='width: 100%; margin-bottom: 20px;'>",
          "<tr>",
          "<td style='padding: 5px;'>p(", r_l, ") = ", ro(probs$lower$p_at, R), "</td>",
          "<td style='padding: 5px;'>p(", r_l, " and below) = ", ro(probs$lower$p_and_below, R), "</td>",
          "<td style='padding: 5px;'>p(", r_l, " and above) = ", ro(probs$lower$p_and_above, R), "</td>",
          "</tr>",
          "</table>"
        ))
      } else {
        r_u <- probs$upper$x
        output <- HTML(paste0(
          "<b>Binomial Distribution Results</b>",
          "<br><br>",
          "<table style='width: 100%; margin-bottom: 20px;'>",
          "<tr>",
          "<td style='padding: 5px;'>p(", r_l, ") = ", ro(probs$lower$p_at, R), "</td>",
          "<td style='padding: 5px;'>p(", r_l, " and below) = ", ro(probs$lower$p_and_below, R), "</td>",
          "<td style='padding: 5px;'>p(", r_l, " and above) = ", ro(probs$lower$p_and_above, R), "</td>",
          "</tr>",
          "<tr>",
          "<td style='padding: 5px;'>p(", r_u, ") = ", ro(probs$upper$p_at, R), "</td>",
          "<td style='padding: 5px;'>p(", r_u, " and below) = ", ro(probs$upper$p_and_below, R), "</td>",
          "<td style='padding: 5px;'>p(", r_u, " and above) = ", ro(probs$upper$p_and_above, R), "</td>",
          "</tr>",
          "<tr>",
          "<td style='padding: 5px; background-color:", colors()$col_fill, ";'></td>",
          "<td style='padding: 5px;'>p(between) = ", ro(probs$p_between, R), "</td>",
          "<td style='padding: 5px;'>", r_l + 1, " ≤ R ≤ ", r_u - 1, "</td>",
          "</tr>",
          "<tr>",
          "<td style='padding: 5px; background-color:", colors()$col_fill_highlight, ";'></td>",
          "<td style='padding: 5px;'>p(tails) = ", ro(probs$p_tails, R), "</td>",
          "<td style='padding: 5px;'>R ≤ ", r_l, " + R ≥ ", r_u, "</td>",
          "</tr>",
          "</table>"
        ))
      }
      
      output
    })
    
    # Binomial table function
    bi_table <- reactive({
      if (!input$r_bi_table) return(NULL)
      
      n <- input$n_bi
      p <- input$pi_bi
      R <- input$decimal_bi
      
      # Get the binomial table and apply rounding
      bi_table_raw <- table.dist.binomial(n = n, p = p)
      bi_table_raw <- as.data.frame(bi_table_raw)
      bi_table_rounded <- ro(bi_table_raw, R)
      
      # Filter out rows with very small probabilities (exact from original app)
      bi_table_rounded[bi_table_rounded$p.at.x > 0.000000001, ]
    })
    
    
    # Plot output - directly in this module
    output$dist_plot <- renderPlot({
      bi_plot()
    })
    
    # Results output
    output$dist_results <- renderUI({
      bi_results()
    })
    
    # Table output
    output$dist_table <- DT::renderDataTable({
      bi_table()
    }, options = list(dom = 't', pageLength = -1))
    
    # Download functionality
    plot_width <- reactive(400 * 4)
    plot_height <- reactive(300 * 4)
    downloadServer("binomial_plot", bi_plot, height = plot_height, width = plot_width)
  })
}
