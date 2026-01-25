# Distribution Testing Module v2 - Rewritten with Standardized Architecture
# This module follows the same patterns as the EDA module for consistency

library(shiny)
library(DT)
library(ggplot2)
library(lolcat)
library(shinyWidgets)
library(digest)

# Source test modules
source("modules/distributions/distribution_testing/tests/poisson_tests.R")
source("modules/distributions/distribution_testing/tests/normality_tests.R")
source("modules/distributions/distribution_testing/tests/exponential_tests.R")
source("modules/distributions/distribution_testing/utils/test_helpers.R")
source("modules/distributions/distribution_testing/plots/test_plots_v2.R")
source("modules/distributions/distribution_testing/functions/exponential_test_functions.R")

# Distribution Testing UI (replicating app.R lines 1101-1151)
create_distribution_testing_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        radioButtons(inputId = ns("dist_test_type"), label = "Test Distribution", choices = c("Poisson"=1,"Normal"=2,"Exponential"=3)),
        uiOutput(outputId = ns("dist_test_subtypes")),
        conditionalPanel(condition = "input.dist_test_type_result==8",
          ns = ns,
          checkboxInput(inputId = ns("dist_test_zero"), label = "Set origin to Xmin?")
        ),
        radioButtons(inputId = ns("dist_test_data_type"), label = "Select Data Type", choices = c("Analyze Columns"=1,"Analyze by Factors"=2)),
        uiOutput(ns("dist_test_UI1")),
        uiOutput(ns("dist_test_UI2")),
        fluidRow(
          column(6,
            numericInput(inputId = ns("dist_test_conf"), label = "Confidence", value = .95, min = 0, max = 1, step = .05, width = "75px")
          ),
          column(6,
            numericInput(inputId = ns("dist_test_decimals"), label = "Decimals", value = 3, min = 1, max = 9, step = 1, width = "75px")
          )
        ),
        checkboxInput(inputId = ns("dist_test_info"), label = "Information on selected test?")
      ),#end sidebarpanel
      mainPanel(
        conditionalPanel(condition = "input.dist_test_type_result==6 || input.dist_test_type_result==7",
          ns = ns,
          fluidRow(
            column(3),
            column(6,
              actionBttn(inputId = ns("dist_test_mvp_go"), label = "Start Simulation", icon = icon("traffic-light"), style = "material-flat", color = "success")
            ),
            column(3,
              #actionBttn(inputId = "dist_test_mvp_stop",label = "Stop",icon = icon("traffic-light-stop"),style = "material-flat",color = "danger")
            )
          ),
          progressBar(id = ns("mvp_exp_prog"), value = 0, display_pct = TRUE, striped = TRUE, title = "Simulation Progress")
        ),
        uiOutput(ns("dist_test_plot_select_b")),
        plotOutput(ns("disttestplotout"), height='400px'),
        fluidRow(
          column(3,
            downloadButtonUI(ns('disttestplotout'))
          ),
          column(6,
            tags$div(id='inline1', class='inline', downloadSelectUI(ns('disttestplotout')))
          )
        ),
        uiOutput(ns("dist_test_out"))
      )#end main
    )#end sidebarlayout
  )#end page
}

# Distribution Testing Server
create_distribution_testing_server <- function(id, filtered_data, reactive_color_palette) {
  moduleServer(id, function(input, output, session) {
    
    # Get namespace function
    ns <- session$ns
    
    # Colors function using reactive color palette
    colors <- reactive({
      palette <- reactive_color_palette()
      if (is.null(palette) || length(palette) == 0) {
        # Default colors if palette not available
        list(
          col_plot_line = "#1f77b4",
          col_fill_highlight = "#ff7f0e",
          col_background = "#f0f0f0"
        )
      } else {
        list(
          col_plot_line = palette[1],
          col_fill_highlight = palette[2],
          col_background = "#f0f0f0"
        )
      }
    })
    
    # Data validation trigger - integrate with global data invalidation system
    data_trigger <- reactive({
      data <- filtered_data()
      if (!is.null(data)) {
        paste0("dist_test_data_", nrow(data), "_", ncol(data), "_", digest::digest(data))
      } else {
        "no_data"
      }
    })
    
    # Global data invalidation integration
    # This ensures the module works properly with the global data invalidation system
    observeEvent(data_trigger(), {
      # The global system will handle UI resets, we just need to ensure
      # our reactive values are properly updated
    })
    
    # Create shared reactive values for sub-modules (following EDA pattern)
    dist_test_data_type <- reactive({
      input$dist_test_data_type
    })
    
    # Reset UI selections when data type changes (following EDA pattern)
    observeEvent(dist_test_data_type(), {
      updatePickerInput(session, "dist_testUI1", selected = character(0))
      updatePickerInput(session, "dist_testUI2", selected = character(0))
    })
    
    # UI1 Selection (following EDA pattern)
    output$dist_test_UI1 <- renderUI({
      data <- filtered_data()
      req(data, ncol(data) > 0)
      
      choices <- seq_len(ncol(data))
      names(choices) <- names(data)
      data_type <- dist_test_data_type()
      
      # Debug output
      cat("Distribution Testing UI1 Debug:\n")
      cat("Data columns:", ncol(data), "\n")
      cat("Data type:", data_type, "\n")
      cat("Choices length:", length(choices), "\n")
      
      if (data_type == 1) {
        # Analyze columns - show column picker
        pickerInput(
          inputId = ns("dist_testUI1"),
          label = "Select Columns",
          multiple = TRUE,
          options = list(`actions-box` = TRUE),
          choices = choices
        )
      } else if (data_type == 2) {
        # Analyze by factors - show factor picker
        pickerInput(
          inputId = ns("dist_testUI1"),
          label = "Select Factor(s)",
          multiple = TRUE,
          options = list(`actions-box` = TRUE),
          choices = choices
        )
      }
    })
    
    # UI2 Selection (following EDA pattern)
    output$dist_test_UI2 <- renderUI({
      data <- filtered_data()
      req(data, ncol(data) > 0)
      
      choices <- seq_len(ncol(data))
      names(choices) <- names(data)
      data_type <- dist_test_data_type()
      UI1 <- input$dist_testUI1
      
      # Only show second UI for factor analysis
      if (data_type == 2 && !is.null(UI1) && length(UI1) > 0) {
        # Analyze by factors - show data column picker (excluding selected factors)
        fact_selected <- as.numeric(unlist(strsplit(x = UI1, split = "\\s+")))
        temp <- seq_along(choices)
        temp <- temp[-fact_selected]
        choices <- choices[temp]
        
        pickerInput(
          inputId = ns("dist_testUI2"),
          label = "Select Data",
          multiple = TRUE,
          options = list(`actions-box` = TRUE),
          choices = choices
        )
      }
    })
    
    # Subtype Selection (replicating app.R lines 28794-28808)
    output$dist_test_subtypes <- renderUI({
      type <- input$dist_test_type #distribution to be tested
      
      if (type == 1) { #Poisson
        output <- radioButtons(inputId = ns("dist_test_type_result"), label = "Select Test", choices = c("Poisson Dispersion Test"=1))
      }
      else if (type == 2) { #normal
        output <- radioButtons(inputId = ns("dist_test_type_result"), label = "Select Test", choices = c("Anderson-Darling"=2,"Shapiro-Wilk"=3,"Lin-Mudholkar"=4,"Skewness and Kurtosis"=5,"D'Agostino's Omnibus"=9))
      }
      else if (type == 3) { #expo
        output <- radioButtons(inputId = ns("dist_test_type_result"), label = "Select Test", choices = c("Shapiro-Wilk"=6,"MVP"=7,"Anderson-Darling"=8))
      }
      
      output
    })
    
    # Plot Type Selection (replicating app.R lines 29500-29507)
    output$dist_test_plot_select_b <- renderUI({
      if (input$dist_test_type != 1) {
        radioGroupButtons(inputId = ns("dist_test_plot_select"), label = "Choose plot type", choices = c("Density"=1,"Q-Q"=2,"P-P"=3), status="success")
      }
      else {
        radioGroupButtons(inputId = ns("dist_test_plot_select"), label = "Choose plot type", choices = c("Histogram"=1,"CDF"=2), status="success")
      }
    })
    
    # Monte Carlo simulation trigger system (Shiny best practices)
    simulation_triggered <- reactiveVal(FALSE)
    
    # Reset simulation trigger when test type changes
    observeEvent(input$dist_test_type_result, {
      simulation_triggered(FALSE)
      cat("Test type changed, simulation trigger reset to FALSE\n")
    })
    
    # Reset simulation trigger when data changes
    observeEvent(filtered_data(), {
      simulation_triggered(FALSE)
    })
    
    # Set simulation trigger when button is clicked
    observeEvent(input$dist_test_mvp_go, {
      simulation_triggered(TRUE)
      showNotification("Monte Carlo simulation started!", type = "message")
    })
    
    # Test Results Output (replicating app.R lines 28863+)
    output$dist_test_out <- renderUI({
      test <- input$dist_test_type_result
      conf <- input$dist_test_conf
      R <- input$dist_test_decimals
      data <- filtered_data()
      data_type <- input$dist_test_data_type
      UI1 <- as.numeric(input$dist_testUI1)
      UI2 <- as.numeric(input$dist_testUI2)
      req(test, data, UI1, R, data_type)
      if(data_type == 2) {
        req(UI2)
      }
      
      # Initialize output
      output <- NULL
      
      # Run the appropriate test and build HTML directly (replicating original app)
      if (test == 1) {
        output <- run_poisson_test(data, UI1, UI2, data_type, conf, R)
      } else if (test == 2) {
        output <- run_anderson_darling_test(data, UI1, UI2, data_type, conf, R)
      } else if (test == 3) {
        output <- run_shapiro_wilk_test(data, UI1, UI2, data_type, conf, R)
      } else if (test == 4) {
        output <- run_lin_mudholkar_test(data, UI1, UI2, data_type, conf, R)
      } else if (test == 5) {
        output <- run_skewness_kurtosis_test(data, UI1, UI2, data_type, conf, R)
      } else if (test == 6) {
        # Shapiro-Wilk Exponentiality Test - requires button click
        req(simulation_triggered())  # Only run if simulation button was clicked
        
        # Build HTML table exactly like original
        output <- paste0("<h3>Shapiro-Wilk Exponentiality Test</h3>",
                        "<table><tr><th>ID</th><th>n</th><th>W</th><th>p</th><th>Method</th></tr>")
        
        if (data_type == 1) { # columns
          for (i in UI1) {
            test_result <- shapiro.exp.test(x = data[[i]])
            n <- length(na.omit(data[[i]]))
            output <- paste0(output,
                            "<tr><td>", names(data[i]), "</td>",
                            "<td>", n, "</td>",
                            "<td>", round(test_result[["statistic"]][["W"]], R), "</td>",
                            "<td>", round(test_result[["p.value"]], R), if(test_result[["p.value"]] < (1-conf)){"*"}, "</td>",
                            "<td>", test_result[["method"]], "</td></tr>")
          }
        } else { # analysis using factors
          all_combos <- unique(data[UI1])
          num_combos <- nrow(all_combos)
          
          for (i in UI2) { # each column with data
            for (j in 1:num_combos) { # each combo
              this_combo <- all_combos[j,]
              sel <- paste0("data$", names(data)[UI1], "==", "'", this_combo, "'", collapse = " & ")
              sub_data <- data[which(eval(parse(text=sel))),]
              test_result <- shapiro.exp.test(x = sub_data[[i]])
              n <- length(na.omit(sub_data[[i]]))
              output <- paste0(output,
                              "<tr><td>", paste0(names(data)[i], ": ", paste(names(this_combo), "=", this_combo, collapse = ", ")), "</td>",
                              "<td>", n, "</td>",
                              "<td>", round(test_result[["statistic"]][["W"]], R), "</td>",
                              "<td>", round(test_result[["p.value"]], R), if(test_result[["p.value"]] < (1-conf)){"*"}, "</td>",
                              "<td>", test_result[["method"]], "</td></tr>")
            }
          }
        }
      } else if (test == 7) {
        # MVP Exponentiality Test - requires button click
        req(simulation_triggered())  # Only run if simulation button was clicked
        
        # Build HTML table exactly like original
        output <- paste0("<h3>MVP Exponentiality Test</h3>",
                        "Monte Carlo Estimation",
                        "<table><tr><th>ID</th><th>n</th><th>MVP(E)</th><th>p</th></tr>")
        
        if (data_type == 1) { # columns
          for (i in UI1) {
            test_result <- mvp_exp(x = data[[i]], max_sims = 100000)
            n <- length(na.omit(data[[i]]))
            output <- paste0(output,
                            "<tr><td>", names(data[i]), "</td>",
                            "<td>", n, "</td>",
                            "<td>", round(test_result[["MVP(E) = "]], R), "</td>",
                            "<td>", round(test_result[["p-value"]], R), if(test_result[["p-value"]] < (1-conf)){"*"}, "</td></tr>")
          }
        } else { # analysis using factors
          all_combos <- unique(data[UI1])
          num_combos <- nrow(all_combos)
          
          for (i in UI2) { # each column with data
            for (j in 1:num_combos) { # each combo
              this_combo <- all_combos[j,]
              sel <- paste0("data$", names(data)[UI1], "==", "'", this_combo, "'", collapse = " & ")
              sub_data <- data[which(eval(parse(text=sel))),]
              test_result <- mvp_exp(x = sub_data[[i]])
              n <- length(na.omit(sub_data[[i]]))
              output <- paste0(output,
                              "<tr><td>", paste0(names(data)[i], ": ", paste(names(this_combo), "=", this_combo, collapse = ", ")), "</td>",
                              "<td>", n, "</td>",
                              "<td>", round(test_result[["MVP(E) = "]], R), "</td>",
                              "<td>", round(test_result[["p-value"]], R), if(test_result[["p-value"]] < (1-conf)){"*"}, "</td></tr>")
            }
          }
        }
      } else if (test == 8) {
        use_min <- input$dist_test_zero
        output <- run_anderson_darling_exp_test(data, UI1, UI2, data_type, conf, R, use_min)
      } else if (test == 9) {
        output <- run_dagostino_test(data, UI1, UI2, data_type, conf, R)
      }
      
      # Make pretty output from result (replicating original app line 29373+)
      output <- HTML(paste0(output, "</table>"))
      
      output
    })
    
    # Plot Output - Enhanced to work with global data invalidation system
    output$disttestplotout <- renderPlot({
      # Wait for all required inputs to be available
      req(input$dist_test_type_result, input$dist_test_data_type, input$dist_testUI1, input$dist_test_plot_select)
      
      test <- input$dist_test_type_result
      data <- filtered_data()
      data_type <- input$dist_test_data_type
      UI1 <- input$dist_testUI1
      UI2 <- input$dist_testUI2
      plot_type <- input$dist_test_plot_select
      
      # Debug output
      cat("Distribution Testing Plot Debug:\n")
      cat("Test:", test, "\n")
      cat("Data available:", !is.null(data), "\n")
      cat("Data rows:", if(!is.null(data)) nrow(data) else "NULL", "\n")
      cat("UI1 (before conversion):", UI1, "\n")
      cat("UI2 (before conversion):", UI2, "\n")
      cat("Plot type:", plot_type, "\n")
      
      # Convert to numeric for plot functions (same as test functions)
      UI1 <- as.numeric(UI1)
      UI2 <- as.numeric(UI2)
      
      cat("UI1 (after conversion):", UI1, "\n")
      cat("UI2 (after conversion):", UI2, "\n")
      
      # Additional validation
      if (is.null(data) || nrow(data) == 0) {
        cat("No data available for plotting\n")
        return(create_default_test_plot())
      }
      
      if (is.null(UI1) || length(UI1) == 0 || any(is.na(UI1))) {
        cat("No valid columns selected for plotting\n")
        return(create_default_test_plot())
      }
      
      if (data_type == 2 && (is.null(UI2) || length(UI2) == 0 || any(is.na(UI2)))) {
        cat("Factor analysis requires UI2 selection\n")
        return(create_default_test_plot())
      }
      
      # Validate data columns
      if (any(UI1 > ncol(data) | UI1 < 1)) {
        cat("Invalid column selection\n")
        return(create_default_test_plot())
      }
      
      if (data_type == 2 && any(UI2 > ncol(data) | UI2 < 1)) {
        cat("Invalid UI2 column selection\n")
        return(create_default_test_plot())
      }
      
      # Create plot using the standardized approach
      tryCatch({
        # Get colors as a list, not reactive
        colors_list <- colors()
        cat("Colors for plot:", paste(names(colors_list), "=", colors_list, collapse = ", "), "\n")
        create_test_plot(test, data, UI1, UI2, data_type, colors_list, plot_type)
      }, error = function(e) {
        cat("Error creating plot:", e$message, "\n")
        create_default_test_plot()
      })
    })
    
    
    # Info button (replicating app.R lines 29509+)
    observeEvent(input$dist_test_info, {
      flag <- input$dist_test_info
      test <- input$dist_test_type_result
      if (!flag) { return() }
      req(test)
      
      if (test == 1) {
        title <- "Poisson Dispersion Test"
        desc <- paste0("Just because you have rate data doesn't mean you have a Poisson distribution. The <b>Poisson Dispersion Test </b> takes advantage of the fact that for the Poisson distribution, the variance is equal to the mean. The statistic is defined as:", withMathJax("$$D=\\sum_{i=1}^N \\frac{(X_{i}-\\bar X)^2}{\\bar X}$$"), "where D follows approximately a χ² distribution with N-1 degrees of freedom.")
      }
      else if (test == 2) {
        title <- "Anderson-Darling Normality Test"
        desc <- "The <b>Anderson-Darling Normality Test</b> is a EDF (empirical distribution function) test based on the idea that if we hypothesize the data follows a distribution, normal in this case, we can transform the data into a uniform distribution and then test for uniformity with a distance test. It also has a weighting function to penalize deviations in the tails of the distribution, which is often what we are interested in anyway. <br><br>The A-D test was found to be <a href='https://www.researchgate.net/publication/267205556_Power_Comparisons_of_Shapiro-Wilk_Kolmogorov-Smirnov_Lilliefors_and_Anderson-Darling_Tests'>nearly as powerful as the Shapiro-Wilk test</a> for detecting departures from normality when compared against other common tests. <br><br>The A-D test can be used with sample sizes <20 and remains useful into larger sample sizes, though it can reject normality too easily at very large N."
      }
      else if (test == 3) {
        title <- "Shapiro-Wilk Normality Test"
        desc <- "The <b>Shapiro-Wilk Normality Test</b> was found to be the <a href='https://www.researchgate.net/publication/267205556_Power_Comparisons_of_Shapiro-Wilk_Kolmogorov-Smirnov_Lilliefors_and_Anderson-Darling_Tests'>most powerful test</a> for detecting departures from normality when compared against other common tests. <br><br>The S-W test can be used with sample sizes <20 and remains useful into larger sample sizes, though it can reject normality too easily at very large N."
      }
      else if (test == 4) {
        title <- "Lin-Mudholkar Normality Test"
        desc <- "The <b>Lin-Mudholkar Normality Test</b> is based on the fact that the mean and variance of a random sample are independent if and only if the parent population is normally distributed. <br><br>This test only detects departures from normality for skewness, but it is quite good at that. However, be aware that it is unlikley to reject for kurtosis."
      }
      else if (test == 5) {
        title <- "Skewness and Kurtosis Normality Tests"
        desc <- "Skewness is how asymmetrical a distribution is. Kurtosis is how peaked the distribution is. A normal distribution has zero skewness and kurtosis. The <b>Skewness and Kurtosis Tests</b> test the null hypothesis that both are zero. <br><br>These tests are not particularly powerful for smaller sample sizes (say below 20) but can be quite useful at larger sample sizes where more powerful tests reject too easily."
      }
      else if (test == 6) {
        title <- "Shapiro-Wilk Exponential Test"
        desc <- paste0("The <b>Shapiro-Wilk Exponential Test</b> is based on the fact that the mean of an exponential distribution is equal to the standard deviation. The statistic is calculated", withMathJax("$$W(E)=\\frac{n(\\bar{X}-X_{min})^2}{[(n-1)s]^2}$$"), "If the sample size is ≤ 100, the software refers to a standard lookup table with some interpolation between points. If the sample size is > 100, the software instead will begin a Monte Carlo simulation. First 20,000 samples of the same size as the data are taken from a theoretical exponential distribution with the mean and minimum found in the data, and W statistic is calculated for those samples. If none or all are above the W calculated for the sample, the p-value is estimated to be zero and the simulation stops. If not, more W statistics are calculated and the proportion that fall above and below the test statistic are calculated and the smaller one doubled to generate the final p-value.<br><br>With larger sample sizes, the simulation can take some time.")
      }
      else if (test == 7) {
        title <- "MVP Exponential Test"
        desc <- paste0("The <b>MVP Exponential Test</b> is a transform of the Shapiro-Wilk exponential test, but the statistic is more straightforward. The statistic is calculated from the sample data:", withMathJax("$$MVP(E)=\\frac{(\\bar{X}-X_{min})^2}{s^2}$$"), "The expected value of the statistic is 1. <br><br>First 20,000 samples of the same size as the data are taken from a theoretical exponential distribution with the mean and minimum found in the data, and MVP(E) is calculated for those samples. If none or all are above the MVP(E) calculated for the sample, the p-value is estimated to be zero and the simulation stops. If not, more MVP(E) statistics are calculated and the proportion that fall above and below the test statistic are calculated and the smaller one doubled to generate the final p-value. <br><br>With larger sample sizes, the simulation can take some time. <br><br>This test was created by Michael V. Petrovich.")
      }
      else if (test == 8) {
        title <- "Anderson-Darling Exponential Test"
        desc <- paste0("The <b>Anderson-Darling Exponential Test</b> is a EDF (empirical distribution function) test based on the idea that if we hypothesize the data follows a distribution, exponential in this case, we can transform the data into a uniform distribution and then test for uniformity with a distance test. <br><br>It also has a weighting function to penalize deviations in the tails of the distribution, which is often what we are interested in anyway. It is a very good test but requires that the origin parameter is zero. It will falsely reject exponential distributions with higher minimums. <br><br>An option is available to shift the data to an origin of zero by subtracting the minimum observation from the data set.")
      }
      else if (test == 9) {
        title <- "D'Agostino's Omnibus Normality Test"
        desc <- paste0("The <b>D'Agostino's Omnibus Normality Test</b> is based on the idea that while the RSD of the sample skewness and kurtosis converge to normality, they do so very slowly. D'Agostino suggested a transform that would make the skewness as close to normal as possible and later authors proposed one for kurtosis. By squaring these transformed numbers and adding them together, you end up with an omnibus statistic that is distributed as a χ² with an expected value of around 2 with 2 degrees of freedom. The results include skewness and kurtosis, their confidence intervals and significance tests, and the omnibus χ² and it's significance test. It has the same requirement as the skewness and kurtosis tests on which it is based, so it should only be used for samples larger than 20.")
      }
      
      sendSweetAlert(title = title, text = HTML(desc), html = TRUE, showCloseButton = TRUE, btn_labels = "Close", type = "info")
      updateCheckboxInput(inputId = "dist_test_info", value = FALSE)
    })
    
    # Download functionality
    plot_width <- reactive(400 * 4)
    plot_height <- reactive(300 * 4)
    downloadServer("disttestplotout", reactive({
      # Wait for all required inputs to be available
      req(input$dist_test_type_result, input$dist_test_data_type, input$dist_testUI1, input$dist_test_plot_select)
      
      test <- input$dist_test_type_result
      data <- filtered_data()
      data_type <- input$dist_test_data_type
      UI1 <- input$dist_testUI1
      UI2 <- input$dist_testUI2
      plot_type <- input$dist_test_plot_select
      
      if (is.null(data)) {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available for plotting", size = 6) +
               theme_void())
      }
      
      # Convert to numeric for plot functions
      UI1 <- as.numeric(UI1)
      UI2 <- as.numeric(UI2)
      
      # Create the plot using the same logic as the renderPlot
      colors_list <- colors()
      create_test_plot(test, data, UI1, UI2, data_type, colors_list, plot_type)
    }), height = plot_height, width = plot_width)
    
    # Helper functions (replicating original app functions)
    shapiro.exp.test <- function(x, bail=20000, nrepl=100000) {
      # From original app lines 29432+
      x <- na.omit(x)
      DNAME <- deparse(substitute(x))
      l <- 0
      n <- length(x)
      x <- sort(x)
      y <- mean(x)
      w <- n*(y-x[1])^2/((n-1)*sum((x-y)^2))
      update_inc <- nrepl/100
      
      # Look up if n <= 100
      if(n <= 100) {
        temp <- lolcat::shapiro.wilk.exponentiality.test(x = x)
        prop.above <- NA
        prop.below <- NA
        p.value <- temp[["p.value"]]
        RVAL <- list(statistic=c(W=temp[["statistic"]][["W"]]), p.value=p.value, method="Lookup", data.name = DNAME)
        class(RVAL) <- "htest"
        return(RVAL)
      }
      
      for(i in 1:bail) {
        s <- rexp(n)
        s <- sort(s)
        y <- sum(s)/n
        W <- n*(y-s[1])*(y-s[1])/((n-1)*sum((s-y)*(s-y)))
        if (W<w) l=l+1
        if(i/update_inc==trunc(i/update_inc)) {
          updateProgressBar(id = "mvp_exp_prog", value = i, total = nrepl, session = session)
        }
      }
      
      if (l==0 || l==bail) {
        RVAL <- list(statistic=c(W=w), p.value=0, method="Monte Carlo", data.name = DNAME)
        class(RVAL) <- "htest"
        return(RVAL)
        updateProgressBar(id = "mvp_exp_prog", value = 100, session = session)
      }
      
      for(i in (bail+1):nrepl) {
        s <- rexp(n)
        s <- sort(s)
        y <- sum(s)/n
        W <- n*(y-s[1])*(y-s[1])/((n-1)*sum((s-y)*(s-y)))
        if (W<w) l=l+1
        if(i/update_inc==trunc(i/update_inc)) {
          updateProgressBar(id = "mvp_exp_prog", value = i, total = nrepl, session = session)
        }
      }
      prop.above <- l/nrepl
      prop.below <- (nrepl-l)/nrepl
      p.value <- min(prop.above, prop.below)*2
      RVAL <- list(statistic=c(W=w), p.value=p.value, method="Shapiro-Wilk test for exponentiality", data.name = DNAME)
      class(RVAL) <- "htest"
      return(RVAL)
    }
    
    mvp_exp <- function(x, bail=20000, max_sims=1000000) {
      # From original app lines 29383+
      x <- na.omit(x)
      samp_mean <- mean(x)
      samp_var <- var(x)
      samp_min <- min(x)
      mvp_e <- (samp_mean-samp_min)^2/samp_var
      n <- length(x)
      mvp_rsd <- rep(NA, max_sims)
      update_inc <- max_sims/100
      
      # Early bail if it is clearly not exponential
      for (i in 1:bail) {
        sim <- samp_min + rexp(n = n, rate = samp_mean^-1)
        sum_x <- sum(sim)
        sim_mvp <- ((sum_x/n)-min(sim))*((sum_x/n)-min(sim))/{{sum(sim*sim)-sum_x*sum_x/n}/(n-1)}
        mvp_rsd[i] <- sim_mvp
        if(i/update_inc==trunc(i/update_inc)) {
          updateProgressBar(id = "mvp_exp_prog", value = i, total = max_sims, session = session)
        }
      }
      if(length(mvp_rsd[na.omit(mvp_rsd)>mvp_e])==0 || length(mvp_rsd[na.omit(mvp_rsd)>mvp_e])==bail) {
        updateProgressBar(id = "mvp_exp_prog", value = 100, session=session)
        return(list("MVP(E) = "=mvp_e, "p-value"=0))
      }
      
      # Continue
      for (i in (bail+1):max_sims) {
        sim <- samp_min + rexp(n = n, rate = samp_mean^-1)
        sum_x <- sum(sim)
        sim_mvp <- ((sum_x/n)-min(sim))*((sum_x/n)-min(sim))/{{sum(sim*sim)-sum_x*sum_x/n}/(n-1)}
        mvp_rsd[i] <- sim_mvp
        if(i/update_inc==trunc(i/update_inc)) {
          updateProgressBar(id = "mvp_exp_prog", value = i, total = max_sims, session = session)
        }
      }
      
      prop_above <- length(mvp_rsd[mvp_rsd>mvp_e])/max_sims
      prop_below <- length(mvp_rsd[mvp_rsd<mvp_e])/max_sims
      prop <- min(prop_above, prop_below)
      output <- list("MVP(E) = "=mvp_e, "p-value"=prop*2)
      return(output)
    }
  })
}