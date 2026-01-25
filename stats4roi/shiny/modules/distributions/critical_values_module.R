# Critical Values Module for stats4ROI
# This module exactly replicates the critical values functionality from the original app

# Critical Values UI (replicating app.R lines 1020-1054)
create_critical_values_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    uiOutput(ns("crit_label")),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          inputId = ns("crit_select"),
          label = "Select the statistic:",
          choices = c("z" = 1, "t" = 2, "χ²" = 3, "F" = 4)
        ),
        radioButtons(
          inputId = ns("stat_or_val"),
          label = "Want",
          choices = c("p-value" = 1, "Score" = 2)
        ),
        radioButtons(
          inputId = ns("tails_crit"),
          label = "Tails",
          choices = c("Two" = 1, "Lower" = 2, "Upper" = 3)
        ),
        numericInput(
          inputId = ns("crit_value"),
          label = "Enter Value",
          value = 2,
          width = "150px"
        ),
        conditionalPanel(
          condition = "input.crit_select > 1",
          ns = ns,
          numericInput(
            inputId = ns("df1_crit"),
            label = "df1",
            value = 5,
            width = "150px",
            min = 0
          )
        ),
        conditionalPanel(
          condition = "input.crit_select == 4",
          ns = ns,
          numericInput(
            inputId = ns("df2_crit"),
            label = "df2",
            value = 10,
            width = "150px",
            min = 0
          )
        ),
        numericInput(
          inputId = ns("decimals_crit"),
          label = "Decimals",
          value = 5,
          min = 0,
          step = 1,
          width = "75px"
        )
      ),
      mainPanel(
        plotOutput(ns("critplot"), height = "300px"),
        downloadUI(ns("critical_values_plot")),
        textOutput(ns("crit_text"))
      )
    )
  )
}

# Critical Values Server (replicating app.R lines 6302-7096)
create_critical_values_server <- function(id, color_palette) {
  moduleServer(id, function(input, output, session) {
    
    # Get namespace function
    ns <- session$ns
    
    # Get colors from palette
    colors <- reactive({
      get_distribution_colors(color_palette())
    })
    
    # Critical values plot (replicating app.R lines 6312-6558)
    crit_plot <- reactive({
      # Extract parameters
      crit_select <- input$crit_select  # 1=z, 2=t, 3=chi, 4=F
      stat_or_val <- input$stat_or_val  # 1=p, 2=score
      tails_crit <- input$tails_crit    # 1=both, 2=lower, 3=upper
      R <- input$decimals_crit
      crit_value <- input$crit_value    # entered value
      df1 <- input$df1_crit
      df2 <- input$df2_crit
      
      # Basic parameter checks
      if (is.null(crit_select) || is.null(stat_or_val) || is.null(tails_crit)) {
        return(NULL)
      }
      
      # Convert to numeric in case Shiny returns strings
      crit_select <- as.numeric(crit_select)
      stat_or_val <- as.numeric(stat_or_val)
      tails_crit <- as.numeric(tails_crit)
      
      # Check for valid numeric values
      if (is.na(crit_select) || is.na(stat_or_val) || is.na(tails_crit)) {
        return(NULL)
      }
      
      req(crit_select, stat_or_val, tails_crit)
      validate(
        need(R, "Need number of decimal places"),
        need(crit_value, "Need critical value")
      )
      
      p <- ggplot()
      
      # Z-distribution (Normal)
      if (crit_select == 1) {
        if (tails_crit == 1) {  # both tails
          if (stat_or_val == 2) {  # convert to stat
            validate(need(crit_value <= 1, "Enter a proportion"))
            crit_value <- qnorm(crit_value / 2)
          }
          crit_value <- abs(crit_value)
          xmin <- min(-crit_value, -4)
          xmax <- max(crit_value, 4)
          p <- p +
            stat_function(geom = "area", fun = dnorm, fill = unname(colors()$col_fill_highlight), 
                         xlim = c(xmin, -crit_value), alpha = 0.7) +  # lower
            stat_function(geom = "area", fun = dnorm, fill = unname(colors()$col_fill_highlight), 
                         xlim = c(crit_value, xmax), alpha = 0.7) +
            geom_vline(xintercept = crit_value, linetype = 5, color = unname(colors()$col_point_of_interest_line), linewidth = 1) +
            geom_vline(xintercept = -crit_value, linetype = 5, color = unname(colors()$col_point_of_interest_line), linewidth = 1)
          area <- 2 * pnorm(-crit_value, lower.tail = TRUE)
        } else {  # one tail
          if (stat_or_val == 2) {
            validate(need(crit_value <= 1, "Enter a proportion"))
            crit_value <- qnorm(crit_value)
            if (tails_crit == 3) {
              crit_value <- -(crit_value)
            }
          }
          xmin <- min(crit_value, -4)
          xmax <- max(crit_value, 4)
          if (tails_crit == 2) {
            p <- p +
              stat_function(geom = "area", fun = dnorm, fill = unname(colors()$col_fill_highlight), 
                           xlim = c(xmin, crit_value), alpha = 0.7) +
              geom_vline(xintercept = crit_value, linetype = 5, color = unname(colors()$col_point_of_interest_line), linewidth = 1)
            area <- pnorm(crit_value, lower.tail = TRUE)
          } else {
            p <- p +
              stat_function(geom = "area", fun = dnorm, fill = unname(colors()$col_fill_highlight), 
                           xlim = c(crit_value, xmax), alpha = 0.7) +
              geom_vline(xintercept = crit_value, linetype = 5, color = unname(colors()$col_point_of_interest_line), linewidth = 1)
            area <- pnorm(crit_value, lower.tail = FALSE)
          }
        }
        if (tails_crit == 1) {
          plusminus <- "±"
        } else {
          plusminus <- ""
        }
        p <- p +
          geom_function(fun = dnorm, color = unname(colors()$col_plot_line), xlim = c(-4, 4), linewidth = 1) +
          geom_vline(xintercept = 0, color = unname(colors()$col_mean_line), linewidth = 1) +
          ggtitle("Standard Normal Distribution", 
                 subtitle = paste0("Shaded Area = ", ro(area, R), ", z = ", plusminus, ro(crit_value, R))) +
          xlab("z") +
          ylab("Density") +
          theme(legend.position = "none")
      }
      
      # t-distribution
      if (crit_select == 2) {
        req(df1)
        if (tails_crit == 1) {  # both tails
          if (stat_or_val == 2) {  # convert to stat
            validate(need(crit_value <= 1, "Enter a probability"))
            crit_value <- qt(crit_value / 2, df = df1)
          }
          crit_value <- abs(crit_value)
          xmin <- min(-crit_value, -6)
          xmax <- max(crit_value, 6)
          p <- p +
            stat_function(geom = "area", fun = dt, args = list(df = df1), fill = unname(colors()$col_fill_highlight), 
                         xlim = c(xmin, -crit_value), alpha = 0.7) +  # lower
            stat_function(geom = "area", fun = dt, args = list(df = df1), fill = unname(colors()$col_fill_highlight), 
                         xlim = c(crit_value, xmax), alpha = 0.7) +
            geom_vline(xintercept = crit_value, linetype = 5, color = unname(colors()$col_point_of_interest_line), linewidth = 1) +
            geom_vline(xintercept = -crit_value, linetype = 5, color = unname(colors()$col_point_of_interest_line), linewidth = 1)
          area <- 2 * pt(-crit_value, df = df1, lower.tail = TRUE)
        } else {  # one tail
          if (stat_or_val == 2) {
            validate(need(crit_value <= 1, "Enter a probability"))
            crit_value <- qt(crit_value, df = df1)
            if (tails_crit == 3) {
              crit_value <- -(crit_value)
            }
          }
          xmin <- min(crit_value, -6)
          xmax <- max(crit_value, 6)
          if (tails_crit == 2) {
            p <- p +
              stat_function(geom = "area", fun = dt, args = list(df = df1), fill = unname(colors()$col_fill_highlight), 
                           xlim = c(xmin, crit_value), alpha = 0.7) +
              geom_vline(xintercept = crit_value, linetype = 5, color = unname(colors()$col_point_of_interest_line), linewidth = 1)
            area <- pt(crit_value, df = df1, lower.tail = TRUE)
          } else {
            p <- p +
              stat_function(geom = "area", fun = dt, args = list(df = df1), fill = unname(colors()$col_fill_highlight), 
                           xlim = c(crit_value, xmax), alpha = 0.7) +
              geom_vline(xintercept = crit_value, linetype = 5, color = unname(colors()$col_point_of_interest_line), linewidth = 1)
            area <- pt(crit_value, df = df1, lower.tail = FALSE)
          }
        }
        if (tails_crit == 1) {
          plusminus <- "±"
        } else {
          plusminus <- ""
        }
        p <- p +
          geom_function(fun = dt, args = list(df = df1), color = unname(colors()$col_plot_line), xlim = c(-6, 6), linewidth = 1) +
          geom_vline(xintercept = 0, color = unname(colors()$col_mean_line), linewidth = 1) +
          ggtitle(paste0("Student's t Distribution with ", df1, " df"), 
                 subtitle = paste0("Shaded Area = ", ro(area, R), ", t = ", plusminus, ro(crit_value, R))) +
          xlab("t") +
          ylab("Density") +
          theme(legend.position = "none")
      }
      
      # Chi-square distribution
      if (crit_select == 3) {
        validate(need(df1 > 0, "Need degrees of freedom"))
        
        xmin <- qchisq(p = 0.0001, df = df1)
        if (df1 == 1) {
          xmin <- 1e-2
        }
        xmax <- qchisq(p = 0.0001, df = df1, lower.tail = FALSE)
        
        if (tails_crit == 1) {  # two tail
          if (stat_or_val == 2) {  # convert to stat
            validate(need(crit_value <= 1, "Enter a probability"))
            crit_value_l <- qchisq(p = crit_value / 2, df = df1, lower.tail = TRUE)
            crit_value_u <- qchisq(p = crit_value / 2, df = df1, lower.tail = FALSE)
          } else {
            crit_value_l <- crit_value
            crit_value_u <- qchisq(p = pchisq(q = crit_value, df = df1), df = df1, lower.tail = FALSE)
          }
          # switch if crit_value_u is smaller
          if (crit_value_u < crit_value_l) {
            temp <- crit_value_u
            crit_value_u <- crit_value_l
            crit_value_l <- temp
          }
          area <- 2 * pchisq(q = crit_value_l, df = df1)
          tail_text <- paste0(ro(crit_value_l, R), " and ", ro(crit_value_u, R))
          
          p <- p +
            stat_function(geom = "area", fun = dchisq, args = list(df = df1), fill = unname(colors()$col_fill_highlight), 
                         xlim = c(xmin, crit_value_l), alpha = 0.7) +  # lower
            stat_function(geom = "area", fun = dchisq, args = list(df = df1), fill = unname(colors()$col_fill_highlight), 
                         xlim = c(crit_value_u, xmax), alpha = 0.7) +
            geom_vline(xintercept = crit_value_l, linetype = 5, color = unname(colors()$col_point_of_interest_line), linewidth = 1) +
            geom_vline(xintercept = crit_value_u, linetype = 5, color = unname(colors()$col_point_of_interest_line), linewidth = 1)
        } else {  # one tail
          if (stat_or_val == 2) {
            validate(need(crit_value <= 1, "Enter a probability"))
            if (tails_crit == 2) {
              crit_value <- qchisq(crit_value, df = df1)
            } else {
              crit_value <- qchisq(crit_value, df = df1, lower.tail = FALSE)
            }
          }
          
          tail_text <- ro(crit_value, R)
          if (tails_crit == 2) {
            p <- p +
              stat_function(geom = "area", fun = dchisq, args = list(df = df1), fill = unname(colors()$col_fill_highlight), 
                           xlim = c(xmin, crit_value), alpha = 0.7) +
              geom_vline(xintercept = crit_value, linetype = 5, color = unname(colors()$col_point_of_interest_line), linewidth = 1)
            area <- pchisq(q = crit_value, df = df1, lower.tail = TRUE)
          } else {
            p <- p +
              stat_function(geom = "area", fun = dchisq, args = list(df = df1), fill = unname(colors()$col_fill_highlight), 
                           xlim = c(crit_value, xmax), alpha = 0.7) +
              geom_vline(xintercept = crit_value, linetype = 5, color = unname(colors()$col_point_of_interest_line), linewidth = 1)
            area <- pchisq(q = crit_value, df = df1, lower.tail = FALSE)
          }
        }
        
        p <- p +
          geom_function(fun = dchisq, args = list(df = df1), color = unname(colors()$col_plot_line), n = 512, xlim = c(xmin, xmax), linewidth = 1) +
          xlab(expression(chi^2)) +
          ylab("Density") +
          ggtitle(label = paste0("χ² Distribution with ", df1, " df"), 
                 subtitle = paste0("Shaded Area = ", ro(area, R), ", χ² = ", tail_text)) +
          theme(legend.position = "none")
        
        if (df1 == 1) {
          p <- p +
            labs(caption = "At 1df, χ² rapidly drops from infinity at 0. This graph starts at 1e-2 instead of 0 so the graph can show the distribution.")
        }
      }
      
      # F-distribution
      if (crit_select == 4) {
        req(df1, df2)
        xmin <- qf(p = 0.0001, df1 = df1, df2 = df2)
        if (df1 == 1 || df2 == 1) {
          xmin <- 1e-2
        }
        xmax <- qf(p = 0.0001, df1 = df1, df2 = df2, lower.tail = FALSE)
        
        if (tails_crit == 1) {  # two tail
          if (stat_or_val == 2) {  # convert to stat
            validate(need(crit_value <= 1, "Enter a proportion"))
            crit_value_l <- qf(p = crit_value / 2, df1 = df1, df2 = df2, lower.tail = TRUE)
            crit_value_u <- qf(p = crit_value / 2, df1 = df1, df2 = df2, lower.tail = FALSE)
          } else {
            crit_value_u <- crit_value
            crit_value_l <- qf(p = pf(q = crit_value, df1 = df1, df2 = df2, lower.tail = FALSE), df = df1, df2 = df2, lower.tail = TRUE)
          }
          p <- p +
            stat_function(geom = "area", fun = df, args = list(df1 = df1, df2 = df2), fill = unname(colors()$col_fill_highlight), 
                         xlim = c(xmin, crit_value_l), alpha = 0.7) +  # lower
            stat_function(geom = "area", fun = df, args = list(df1 = df1, df2 = df2), fill = unname(colors()$col_fill_highlight), 
                         xlim = c(crit_value_u, xmax), alpha = 0.7) +
            geom_vline(xintercept = crit_value_l, linetype = 5, color = unname(colors()$col_point_of_interest_line), linewidth = 1) +
            geom_vline(xintercept = crit_value_u, linetype = 5, color = unname(colors()$col_point_of_interest_line), linewidth = 1)
          area <- pf(q = crit_value_u, df1 = df1, df2 = df2, lower.tail = FALSE) + pf(q = crit_value_l, df1 = df1, df2 = df2, lower.tail = TRUE)
          tail_text <- paste0(ro(crit_value_l, R), " and ", ro(crit_value_u, R))
        } else {  # one tail
          if (stat_or_val == 2) {
            validate(need(crit_value <= 1, "Enter a proportion"))
            if (tails_crit == 2) {
              crit_value <- qf(crit_value, df1 = df1, df2 = df2)
            } else {
              crit_value <- qf(crit_value, df1 = df1, df2 = df2, lower.tail = FALSE)
            }
          }
          
          tail_text <- ro(crit_value, R)
          if (tails_crit == 2) {
            p <- p +
              stat_function(geom = "area", fun = df, args = list(df1 = df1, df2 = df2), fill = unname(colors()$col_fill_highlight), 
                           xlim = c(xmin, crit_value), alpha = 0.7) +
              geom_vline(xintercept = crit_value, linetype = 5, color = unname(colors()$col_point_of_interest_line), linewidth = 1)
            area <- pf(q = crit_value, df1 = df1, df2 = df2, lower.tail = TRUE)
          } else {
            p <- p +
              stat_function(geom = "area", fun = df, args = list(df1 = df1, df2 = df2), fill = unname(colors()$col_fill_highlight), 
                           xlim = c(crit_value, xmax), alpha = 0.7) +
              geom_vline(xintercept = crit_value, linetype = 5, color = unname(colors()$col_point_of_interest_line), linewidth = 1)
            area <- pf(q = crit_value, df1 = df1, df2 = df2, lower.tail = FALSE)
          }
        }
        
        p <- p +
          geom_function(fun = df, args = list(df1 = df1, df2 = df2), color = unname(colors()$col_plot_line), n = 512, xlim = c(xmin, xmax), linewidth = 1) +
          xlab("F") +
          ylab("Density") +
          ggtitle(label = paste0("F Distribution with ", df1, ", ", df2, " df"), 
                 subtitle = paste0("Shaded Area = ", ro(area, R), ", F = ", tail_text)) +
          theme(legend.position = "none")
        
        if (df1 == 1 || df2 == 1) {
          p <- p +
            labs(caption = "At 1df, F rapidly drops from infinity at 0. This graph starts at 1e-2 instead of 0 so the graph can show the distribution.")
        }
      }
      
      p
    })
    
    # Critical values text output (replicating app.R lines 6563-7096)
    crit_text_out <- reactive({
      crit_select <- input$crit_select
      stat_or_val <- input$stat_or_val
      tails_crit <- input$tails_crit
      decimals_crit <- input$decimals_crit
      crit_value <- input$crit_value
      df1_crit <- input$df1_crit
      df2_crit <- input$df2_crit
      
      # Basic parameter checks
      if (is.null(crit_select) || is.null(stat_or_val) || is.null(tails_crit) || is.null(decimals_crit) || is.null(crit_value)) {
        return("")
      }
      
      # Convert to numeric in case Shiny returns strings
      crit_select <- as.numeric(crit_select)
      stat_or_val <- as.numeric(stat_or_val)
      tails_crit <- as.numeric(tails_crit)
      
      # Check for valid numeric values
      if (is.na(crit_select) || is.na(stat_or_val) || is.na(tails_crit)) {
        return("")
      }
      
      # Z critical values
      if (crit_select == 1) {
        if (stat_or_val == 1) {  # want p
          if (tails_crit == 1) {  # two tail
            p_crit <- 1 - (pnorm(q = abs(crit_value), mean = 0, sd = 1, lower.tail = TRUE) - 
                          pnorm(q = -abs(crit_value), mean = 0, sd = 1, lower.tail = TRUE))
            results <- c("Two-tailed p-value", ro(p_crit, decimals_crit))
          } else if (tails_crit == 2) {  # lower tail
            p_crit <- pnorm(q = crit_value, mean = 0, sd = 1, lower.tail = TRUE)
            results <- c("Lower-tail p-value", ro(p_crit, decimals_crit))
          } else if (tails_crit == 3) {  # upper tail
            p_crit <- pnorm(q = crit_value, mean = 0, sd = 1, lower.tail = FALSE)
            results <- c("Upper-tail p-value", ro(p_crit, decimals_crit))
          }
        } else {  # want value
          if (tails_crit == 1) {  # two tail
            p_crit <- abs(qnorm(p = crit_value / 2, mean = 0, sd = 1, lower.tail = TRUE))
            results <- c("Two-tailed value ±", ro(p_crit, decimals_crit))
          } else if (tails_crit == 2) {  # lower tail
            p_crit <- qnorm(p = crit_value, mean = 0, sd = 1, lower.tail = TRUE)
            results <- c("Lower-tail value", ro(p_crit, decimals_crit))
          } else if (tails_crit == 3) {  # upper tail
            p_crit <- qnorm(p = crit_value, mean = 0, sd = 1, lower.tail = FALSE)
            results <- c("Upper-tail value", ro(p_crit, decimals_crit))
          }
        }
      }
      
      # t critical values
      else if (crit_select == 2) {
        if (stat_or_val == 1) {  # want p
          if (tails_crit == 1) {  # two tail
            p_crit <- 1 - (pt(q = abs(crit_value), df = df1_crit, lower.tail = TRUE) - 
                          pt(q = -abs(crit_value), df = df1_crit, lower.tail = TRUE))
            results <- c("Two-tailed p-value", ro(p_crit, decimals_crit))
          } else if (tails_crit == 2) {  # lower tail
            p_crit <- pt(q = crit_value, df = df1_crit, lower.tail = TRUE)
            results <- c("Lower-tail p-value", ro(p_crit, decimals_crit))
          } else if (tails_crit == 3) {  # upper tail
            p_crit <- pt(q = crit_value, df = df1_crit, lower.tail = FALSE)
            results <- c("Upper-tail p-value", ro(p_crit, decimals_crit))
          }
        } else {  # want value
          if (tails_crit == 1) {  # two tail
            p_crit <- abs(qt(p = crit_value / 2, df = df1_crit, lower.tail = TRUE))
            results <- c("Two-tailed value ±", ro(p_crit, decimals_crit))
          } else if (tails_crit == 2) {  # lower tail
            p_crit <- qt(p = crit_value, df = df1_crit, lower.tail = TRUE)
            results <- c("Lower-tail value", ro(p_crit, decimals_crit))
          } else if (tails_crit == 3) {  # upper tail
            p_crit <- qt(p = crit_value, df = df1_crit, lower.tail = FALSE)
            results <- c("Upper-tail value", ro(p_crit, decimals_crit))
          }
        }
      }
      
      # Chi-square critical values
      else if (crit_select == 3) {
        if (stat_or_val == 1) {  # want p
          if (tails_crit == 1) {  # two tail
            chi_lower <- pchisq(q = crit_value, df = df1_crit, lower.tail = TRUE)
            chi_upper <- pchisq(q = crit_value, df = df1_crit, lower.tail = FALSE)
            if (chi_lower > chi_upper) {
              p_crit <- 2 * pchisq(q = crit_value, df = df1_crit, lower.tail = FALSE)
            } else {
              p_crit <- 2 * pchisq(q = crit_value, df = df1_crit, lower.tail = TRUE)
            }
            results <- c("Two-tailed p-value", ro(p_crit, decimals_crit))
          } else if (tails_crit == 2) {  # lower tail
            p_crit <- pchisq(q = crit_value, df = df1_crit, lower.tail = TRUE)
            results <- c("Lower-tail p-value", ro(p_crit, decimals_crit))
          } else if (tails_crit == 3) {  # upper tail
            p_crit <- pchisq(q = crit_value, df = df1_crit, lower.tail = FALSE)
            results <- c("Upper-tail p-value", ro(p_crit, decimals_crit))
          }
        } else {  # want value
          if (tails_crit == 1) {  # two tail
            p_crit <- qchisq(p = crit_value / 2, df = df1_crit, lower.tail = TRUE)  # lower
            p_crit2 <- qchisq(p = crit_value / 2, df = df1_crit, lower.tail = FALSE)  # upper
            results <- c("Two-tailed values ", ro(p_crit, decimals_crit), ", ", ro(p_crit2, decimals_crit))
          } else if (tails_crit == 2) {  # lower tail
            p_crit <- qchisq(p = crit_value, df = df1_crit, lower.tail = TRUE)
            results <- c("Lower-tail value", ro(p_crit, decimals_crit))
          } else if (tails_crit == 3) {  # upper tail
            p_crit <- qchisq(p = crit_value, df = df1_crit, lower.tail = FALSE)
            results <- c("Upper-tail value", ro(p_crit, decimals_crit))
          }
        }
      }
      
      # F critical values
      else if (crit_select == 4) {
        if (stat_or_val == 1) {  # want p
          if (tails_crit == 1) {  # two tail
            f_lower <- pf(q = crit_value, df1 = df1_crit, df2 = df2_crit, lower.tail = TRUE)
            f_upper <- pf(q = crit_value, df1 = df1_crit, df2 = df2_crit, lower.tail = FALSE)
            if (f_lower > f_upper) {
              p_crit <- 2 * pf(q = crit_value, df1 = df1_crit, df2 = df2_crit, lower.tail = FALSE)
            } else {
              p_crit <- 2 * pf(q = crit_value, df1 = df1_crit, df2 = df2_crit, lower.tail = TRUE)
            }
            results <- c("Two-tailed p-value", ro(p_crit, decimals_crit))
          } else if (tails_crit == 2) {  # lower tail
            p_crit <- pf(q = crit_value, df1 = df1_crit, df2 = df2_crit, lower.tail = TRUE)
            results <- c("Lower-tail p-value", ro(p_crit, decimals_crit))
          } else if (tails_crit == 3) {  # upper tail
            p_crit <- pf(q = crit_value, df1 = df1_crit, df2 = df2_crit, lower.tail = FALSE)
            results <- c("Upper-tail p-value", ro(p_crit, decimals_crit))
          }
        } else {  # want value
          if (tails_crit == 1) {  # two tail
            p_crit <- qf(p = crit_value / 2, df1 = df1_crit, df2 = df2_crit, lower.tail = TRUE)  # lower
            p_crit2 <- qf(p = crit_value / 2, df1 = df1_crit, df2 = df2_crit, lower.tail = FALSE)  # upper
            results <- c("Two-tailed values ", ro(p_crit, decimals_crit), ", ", ro(p_crit2, decimals_crit))
          } else if (tails_crit == 2) {  # lower tail
            p_crit <- qf(p = crit_value, df1 = df1_crit, df2 = df2_crit, lower.tail = TRUE)
            results <- c("Lower-tail value", ro(p_crit, decimals_crit))
          } else if (tails_crit == 3) {  # upper tail
            p_crit <- qf(p = crit_value, df1 = df1_crit, df2 = df2_crit, lower.tail = FALSE)
            results <- c("Upper-tail value", ro(p_crit, decimals_crit))
          }
        }
      }
      
      paste(results, collapse = " ")
    })
    
    # Dynamic label (replicating app.R lines 7099-7113)
    output$crit_label <- renderUI({
      crit_select <- input$crit_select
      stat_or_val <- input$stat_or_val
      
      if (is.null(crit_select) || is.null(stat_or_val)) {
        return(h3("Select a statistic to begin"))
      }
      
      # Convert to numeric in case Shiny returns strings
      crit_select <- as.numeric(crit_select)
      stat_or_val <- as.numeric(stat_or_val)
      
      # Check for valid numeric values
      if (is.na(crit_select) || is.na(stat_or_val)) {
        return(h3("Select a statistic to begin"))
      }
      
      # Ensure crit_select is within valid range (1-4)
      if (crit_select < 1 || crit_select > 4) {
        return(h3("Select a statistic to begin"))
      }
      
      stat_names <- c("z", "t", "χ²", "F")
      
      if (stat_or_val == 1) {
        h3(paste("Convert ", stat_names[crit_select], " into p-value"))
      } else {
        h3(paste("Convert p-value into ", stat_names[crit_select]))
      }
    })
    
    # Render outputs
    output$critplot <- renderPlot({
      crit_plot()
    })
    
    output$crit_text <- renderText({
      crit_text_out()
    })
    
    # Download dimensions
    plot_width <- reactive(400 * 4)
    plot_height <- reactive(300 * 4)
    
    # Download server
    downloadServer("critical_values_plot", crit_plot, height = plot_height, width = plot_width)
  })
}
