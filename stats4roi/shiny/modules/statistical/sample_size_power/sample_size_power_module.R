# Sample Size and Power Analysis Module for stats4ROI
# This module exactly replicates the Sample Size/Power functionality from the original app
# Original implementation: app.R lines 1114-1148 (UI) and 5166-5320+ (server logic)

library(shiny)
library(lolcat)
library(dplyr)

# Helper function to create properly spaced table rows
create_table_row <- function(cell1, cell2 = "", cell3 = "") {
  if (cell3 != "") {
    # Three-column layout with spacing
    c(
      "<tr>",
      "<td>", cell1, "</td>",
      "<td>&nbsp;&nbsp;&nbsp;&nbsp;</td>",
      "<td>", cell2, "</td>",
      "<td>&nbsp;&nbsp;&nbsp;&nbsp;</td>",
      "<td>", cell3, "</td>",
      "</tr>"
    )
  } else if (cell2 != "") {
    # Two-column layout with spacing
    c(
      "<tr>",
      "<td>", cell1, "</td>",
      "<td>&nbsp;&nbsp;&nbsp;&nbsp;</td>",
      "<td>", cell2, "</td>",
      "</tr>"
    )
  } else {
    # Single-column layout
    c(
      "<tr>",
      "<td>", cell1, "</td>",
      "</tr>"
    )
  }
}

# Safe get function to handle atomic vectors, lists, and data frames
safe_get <- function(obj, name, default = NULL) {
  if (is.data.frame(obj) && name %in% names(obj)) {
    return(obj[[name]])
  } else if (is.list(obj) && name %in% names(obj)) {
    return(obj[[name]])
  } else {
    return(default)
  }
}

# Power function for Poisson one-sample exact test
power.count.poisson.onesample.exact <- function(lambda_0, lambda_1, n, alpha = 0.05, alternative = c("two.sided", "less", "greater")) {
  if (!is.finite(n)) { return() }
  if (lambda_0 == lambda_1) { return(data.frame(error_message = "Rates cannot be equal")) }
  
  if (alternative == "less") {
    if (lambda_1 >= lambda_0) {
      output <- data.frame(error_message = "Alternative must be less than null")
      return(output)
    }
    df <- table.dist.poisson(lambda_0 * n)
    df.with.index <- mutate(df, IDX = 1:n())
    result <- data.frame(filter(df.with.index, (eq.and.below <= alpha))$IDX)
    df2 <- table.dist.poisson(lambda_1 * n)
    power <- df2$eq.and.below[length(result$filter.df.with.index...eq.and.below....alpha...IDX) - 1]
    
    # Critical Xs
    crit_x_l <- qpois(p = alpha, lambda = n * lambda_0, lower.tail = T) - 1
    alpha_r <- ppois(q = crit_x_l, lambda = n * lambda_0, lower.tail = T)
    output <- data.frame(c(alpha = alpha_r, power = power, crti_x_l = crit_x_l))
    
    return(output)
  }
  
  if (alternative == "greater") {
    if (lambda_1 <= lambda_0) {
      output <- data.frame(error_message = "Alternative must be greater than null")
      return(output)
    }
    df <- table.dist.poisson(lambda_0 * n)
    df.with.index <- mutate(df, IDX = 1:n())
    result <- data.frame(filter(df.with.index, (eq.and.above <= alpha))$IDX)
    df2 <- table.dist.poisson(lambda_1 * n)
    power <- df2$eq.and.above[min(result$filter.df.with.index...eq.and.above....alpha...IDX) + 1]
    
    # Critical Xs
    crit_x_u <- qpois(p = 1 - alpha, lambda = n * lambda_0, lower.tail = T) + 1
    alpha_r <- ppois(q = (crit_x_u - 1), lambda = n * lambda_0, lower.tail = F)
    output <- data.frame(c(alpha = alpha_r, power = power, crit_x_u = crit_x_u))
    
    return(output)
  }
  
  if (alternative == "two.sided") {
    if (lambda_0 > lambda_1) {
      df <- table.dist.poisson(lambda_0 * n)
      df.with.index <- mutate(df, IDX = 1:n())
      alpha2 <- alpha / 2
      result <- data.frame(filter(df.with.index, (eq.and.below <= alpha2))$IDX)
      df2 <- table.dist.poisson(lambda_1 * n)
      power1 <- df2$eq.and.below[length(result$filter.df.with.index...eq.and.below....alpha2...IDX) - 1]
    } else {
      df <- table.dist.poisson(lambda_0 * n)
      df.with.index <- mutate(df, IDX = 1:n())
      alpha2 <- alpha / 2
      result <- data.frame(filter(df.with.index, (eq.and.above <= alpha2))$IDX)
      df2 <- table.dist.poisson(lambda_1 * n)
      power2 <- df2$eq.and.above[min(result$filter.df.with.index...eq.and.above....alpha2...IDX) + 1]
    }
    
    if (lambda_0 > lambda_1) {
      # Critical Xs
      crit_x_l <- qpois(p = alpha / 2, lambda = n * lambda_0, lower.tail = T) - 1
      crit_x_u <- qpois(p = 1 - (alpha / 2), lambda = n * lambda_0, lower.tail = T) + 1
      
      alpha_r <- ppois(q = crit_x_l, lambda = n * lambda_0, lower.tail = T)
      alpha_r <- alpha_r + ppois(q = crit_x_u - 1, lambda = n * lambda_0, lower.tail = F)
      
      output <- data.frame(c(alpha = alpha_r, power = power1, crit_x_l = crit_x_l, crit_x_u = crit_x_u))
      
      return(output)
    }
    
    if (lambda_0 < lambda_1) {
      # Critical Xs
      crit_x_l <- qpois(p = alpha / 2, lambda = n * lambda_0, lower.tail = T) - 1
      crit_x_u <- qpois(p = 1 - (alpha / 2), lambda = n * lambda_0, lower.tail = T) + 1
      
      alpha_r <- ppois(q = crit_x_l, lambda = n * lambda_0, lower.tail = T)
      alpha_r <- alpha_r + ppois(q = crit_x_u, lambda = n * lambda_0, lower.tail = F)
      
      output <- data.frame(c(alpha = alpha_r, power = power2, crit_x_l = crit_x_l, crit_x_u = crit_x_u))
      
      return(output)
    }
  }
}

# Sample size function for Poisson one-sample exact test
sample.size.count.poisson.onesample.exact <- function(lambda_0, lambda_1, alpha = 0.05, beta = 0.10, alternative = c("two.sided", "less", "greater")) {
  if (lambda_0 == lambda_1) { return(data.frame(error_message = "Rates cannot be equal")) }
  
  if (alternative == "less") {
    if (lambda_1 >= lambda_0) {
      output <- data.frame(error_message = "Alternative must be less than null")
      return(output)
    }
  }
  if (alternative == "greater") {
    if (lambda_1 <= lambda_0) {
      output <- data.frame(error_message = "Alternative must be greater than null")
      return(output)
    }
  }
  
  n <- sample.size.count.poisson.onesample.approximate(
    lambda.null.hypothesis = lambda_0,
    lambda.alternative.hypothesis = lambda_1,
    alpha = alpha,
    beta = beta,
    alternative = alternative,
    details = FALSE
  )
  
  pow <- power.count.poisson.onesample.exact(lambda_0 = lambda_0, lambda_1 = lambda_1, n = n, alpha = alpha, alternative = alternative)
  beta_this <- 1 - pow["power", ]
  
  while (beta_this > beta) {
    n <- n + 1
    pow <- power.count.poisson.onesample.exact(lambda_0 = lambda_0, lambda_1 = lambda_1, n = n, alpha = alpha, alternative = alternative)
    beta_this <- 1 - pow["power", ]
  }
  
  # Critical Xs
  crit_x_l <- NULL
  crit_x_u <- NULL
  
  if (alternative == "less") {
    crit_x_l <- qpois(p = alpha, lambda = n * lambda_0, lower.tail = T) - 1
    alpha_r <- ppois(q = crit_x_l, lambda = n * lambda_0, lower.tail = T)
  }
  if (alternative == "greater") {
    crit_x_u <- qpois(p = 1 - alpha, lambda = n * lambda_0, lower.tail = T) + 1
    alpha_r <- ppois(q = (crit_x_u - 1), lambda = n * lambda_0, lower.tail = F)
  }
  if (alternative == "two.sided") {
    crit_x_l <- qpois(p = alpha / 2, lambda = n * lambda_0, lower.tail = T) - 1
    crit_x_u <- qpois(p = 1 - (alpha / 2), lambda = n * lambda_0, lower.tail = T) + 1
    alpha_r <- ppois(q = (crit_x_u - 1), lambda = n * lambda_0, lower.tail = F)
    alpha_r <- alpha_r + ppois(q = crit_x_l, lambda = n * lambda_0, lower.tail = T)
  }
  
  output <- data.frame(alpha = alpha_r, power = 1 - beta_this, n = n, crit_x_l = crit_x_l, crit_x_u = crit_x_u)
  
  return(output)
}

# Power function for Poisson two-sample approximate test
power.count.poisson.twosample.approximate <- function(lambda_1, lambda_2, n1, n2, alpha = 0.05, alternative = c("two.sided", "less", "greater")) {
  
  if (alternative == "two.sided") {
    if (lambda_1 > lambda_2) {
      lambda_sm <- lambda_1
      lambda_bg <- lambda_2
    } else {
      lambda_sm <- lambda_2
      lambda_bg <- lambda_1
    }
    z_power <- (
      (sqrt(lambda_sm) - sqrt(lambda_bg)) /
        (.5 * sqrt(n1^-1 + n2^-1))
    ) -
      qnorm(p = (1 - alpha / 2), mean = 0, sd = 1, lower.tail = T)
    power_out <- pnorm(q = z_power, mean = 0, sd = 1, lower.tail = T)
  }
  
  if (alternative == "greater") {
    if (lambda_2 < lambda_1) {
      output <- data.frame(error_message = "Alternative must be greater than null")
      return(output)
    }
    z_power <- (
      (sqrt(lambda_2) - sqrt(lambda_1)) /
        (.5 * sqrt(n1^-1 + n2^-1))
    ) -
      qnorm(p = (1 - alpha), mean = 0, sd = 1, lower.tail = T)
    power_out <- pnorm(q = z_power, mean = 0, sd = 1, lower.tail = T)
  }
  
  if (alternative == "less") {
    if (lambda_2 > lambda_1) {
      output <- data.frame(error_message = "Alternative must be less than null")
      return(output)
    }
    z_power <- (
      (sqrt(lambda_1) - sqrt(lambda_2)) /
        (.5 * sqrt(n1^-1 + n2^-1))
    ) -
      qnorm(p = (1 - alpha), mean = 0, sd = 1, lower.tail = T)
    power_out <- pnorm(q = z_power, mean = 0, sd = 1, lower.tail = T)
  }
  
  return(power_out)
}

# Sample size function for Poisson two-sample approximate test
sample.size.count.poisson.twosample.approximate <- function(lambda_1, lambda_2, alpha = 0.05, beta = 0.10, alternative = c("two.sided", "less", "greater")) {
  if (lambda_1 == lambda_2) { return(data.frame(error_message = "Rates cannot be equal")) }
  if (alternative == "less") {
    if (lambda_2 >= lambda_1) {
      output <- data.frame(error_message = "Alternative must be less than null")
      return(output)
    }
  }
  if (alternative == "greater") {
    if (lambda_2 <= lambda_1) {
      output <- data.frame(error_message = "Alternative must be greater than null")
      return(output)
    }
  }
  
  n <- sample.size.count.poisson.onesample.approximate(
    lambda.null.hypothesis = lambda_1,
    lambda.alternative.hypothesis = lambda_2,
    alpha = alpha,
    beta = beta,
    alternative = alternative,
    details = FALSE
  )
  
  pow <- power.count.poisson.twosample.approximate(lambda_1 = lambda_1, lambda_2 = lambda_2, n1 = n, n2 = n, alpha = alpha, alternative = alternative)
  beta_this <- 1 - pow
  
  while (beta_this > beta) {
    n <- n + 1
    pow <- power.count.poisson.twosample.approximate(lambda_1 = lambda_1, lambda_2 = lambda_2, n1 = n, n2 = n, alpha = alpha, alternative = alternative)
    beta_this <- 1 - pow
  }
  
  output <- data.frame(power = 1 - beta_this, n = n)
  
  return(output)
}

# Sample Size and Power Analysis UI (replicating app.R lines 1114-1148)
create_sample_size_power_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = ns("sample_size_type"),
        label = "Calculate the sample size or power for:",
        choices = c("Means" = 1, "Variances" = 2, "Proportions (binomial)" = 3, "Rates (Poisson)" = 4, "ANOVA" = 5, "Correlations" = 6)
      ),
      uiOutput(ns("s_size_tests"))
    ), # end sidebarpanel
    mainPanel(
      fluidRow(
        conditionalPanel(
          condition = paste0("input['", ns("sample_size_type"), "'] != 5"),
          selectInput(
            inputId = ns("one_or_two_size"),
            label = "Alternative is:",
            choices = c("Equal to the null" = "two.sided", "Less Than the null" = "less", "Greater Than the null" = "greater"),
            width = "150px",
            selected = 2
          )
        ),
        checkboxInput(
          inputId = ns("power_s"),
          label = "Power",
          value = FALSE
        )
      ),
      tags$div(
        id = "inline1", 
        class = "inline",
        fluidRow(
          column(3, numericInput(
            inputId = ns("s_size_alpha"),
            label = withMathJax("$$\\alpha:{ }$$"),
            value = 0.05,
            min = 0,
            max = 1,
            step = 0.05,
            width = "150px"
          ))
        ),
        fluidRow(
          column(3,
            conditionalPanel(
              condition = paste0("input['", ns("power_s"), "'] == 0"),
              numericInput(
                inputId = ns("s_size_beta"),
                label = withMathJax("$$\\beta:{ }$$"),
                value = 0.1,
                min = 0,
                max = 1,
                step = 0.05,
                width = "150px"
              )
            ),
            conditionalPanel(
              condition = paste0("input['", ns("power_s"), "'] == 1"),
              numericInput(
                inputId = ns("s_size_n"),
                label = withMathJax("$$n:{ }$$"),
                value = 10,
                min = 0,
                step = 1,
                width = "150px"
              )
            )
          ),
          column(3, uiOutput(ns("s_sizeUI3")))
        ),
        fluidRow(
          column(3, uiOutput(ns("s_sizeUI1"))),
          column(6, uiOutput(ns("s_sizeUI4")))
        ),
        fluidRow(
          column(3, uiOutput(ns("s_sizeUI2")))
        )
      ),
      htmlOutput(ns("pretty_ssize"))
    )
  ) # end sidebarLayout
}

# Sample Size and Power Analysis Server (replicating app.R lines 5166-5320+)
create_sample_size_power_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    # Define choice_sample_size (replicating app.R lines 104-124)
    choice_sample_size <- c(seq(1, 18))
    names(choice_sample_size) <- c(
      "One-sample Mean z",
      "One-sample Mean z - alternate",
      "Two-sample Mean z Independent",
      "Two-sample Mean z Independent - alternate",
      "One-sample Mean t Independent",
      "Two-sample Mean t equal variance Independent",
      "Two-sample Mean t unequal variance Independent",
      "Two-sample Mean t Dependent",
      "One-sample Variance",
      "Two-sample Variance Independent",
      "One-sample Pearson r",
      "One-sample Proportion - Approximate",
      "One-sample Proportion - Exact",
      "Two-sample Proportion - Approximate",
      "ANOVA",
      "One-sample Poisson - Exact",
      "One-sample Poisson - Approximate",
      "Two-Sample Poisson - Approximate"
    )
    
    # Test selection UI (replicating app.R lines 7544-7569)
    output$s_size_tests <- renderUI({
      sample_size_type <- input$sample_size_type
      
      if (sample_size_type == 1) {
        s_size_test_out <- radioButtons(
          inputId = ns("sample_calc"),
          label = "Select the Test",
          choices = choice_sample_size[c(1, 3, 5:8)]
        )
      }
      if (sample_size_type == 2) {
        s_size_test_out <- radioButtons(
          inputId = ns("sample_calc"),
          label = "Select the Test",
          choices = choice_sample_size[c(9:10)]
        )
      }
      if (sample_size_type == 3) {
        s_size_test_out <- radioButtons(
          inputId = ns("sample_calc"),
          label = "Select the Test",
          choices = choice_sample_size[c(12:14)]
        )
      }
      if (sample_size_type == 4) {
        s_size_test_out <- radioButtons(
          inputId = ns("sample_calc"),
          label = "Select the Test",
          choices = choice_sample_size[c(16, 17, 18)]
        )
      }
      if (sample_size_type == 5) {
        s_size_test_out <- p("")
      }
      if (sample_size_type == 6) {
        s_size_test_out <- radioButtons(
          inputId = ns("sample_calc"),
          label = "Select the Test",
          choices = choice_sample_size[c(11)]
        )
      }
      
      s_size_test_out
    })
    
    # Dynamic UI generation for s_sizeUI1 (replicating app.R lines 7572-7637)
    output$s_sizeUI1 <- renderUI({
      sample_calc <- input$sample_calc
      sample_size_type <- input$sample_size_type
      
      req(sample_calc)
      
      if (sample_size_type == 5) {
        sample_calc <- 15
      }
      
      if (sample_calc == 1) {
        s_size1_out <- numericInput(
          inputId = ns("s_sizeUI1"),
          label = withMathJax("$$\\sigma:{ }$$"),
          value = 1,
          min = 0,
          max = 1
        )
      }
      if (sample_calc == 2) {
        s_size1_out <- numericInput(
          inputId = ns("s_sizeUI1"),
          label = withMathJax("$$\\sigma:{ }$$"),
          value = 1,
          min = 0,
          max = 1
        )
      }
      if (sample_calc == 3) {
        s_size1_out <- numericInput(
          inputId = ns("s_sizeUI1"),
          label = withMathJax("$$\\sigma:{ }$$"),
          value = 1,
          min = 0,
          max = 1
        )
      }
      if (sample_calc == 4) {
        s_size1_out <- numericInput(
          inputId = ns("s_sizeUI1"),
          label = withMathJax("$$\\sigma:{ }$$"),
          value = 1,
          min = 0,
          max = 1
        )
      }
      if (sample_calc == 5) {
        s_size1_out <- numericInput(
          inputId = ns("s_sizeUI1"),
          label = withMathJax("$$\\sigma:{ }$$"),
          value = 1,
          min = 0,
          max = 1
        )
      }
      if (sample_calc == 6) {
        s_size1_out <- numericInput(
          inputId = ns("s_sizeUI1"),
          label = withMathJax("$$\\sigma:{ }$$"),
          value = 1,
          min = 0,
          max = 1
        )
      }
      if (sample_calc == 7) {
        s_size1_out <- numericInput(
          inputId = ns("s_sizeUI1"),
          label = withMathJax("$$\\sigma:{ }$$"),
          value = 1,
          min = 0,
          max = 1
        )
      }
      if (sample_calc == 8) {
        s_size1_out <- numericInput(
          inputId = ns("s_sizeUI1"),
          label = withMathJax("$$\\sigma_{\\bar{D}}:{ }$$"),
          value = 1,
          min = 0,
          max = 1
        )
      }
      if (sample_calc == 9) {
        s_size1_out <- numericInput(
          inputId = ns("s_sizeUI1"),
          label = withMathJax("$$\\sigma_{0}:{ }$$"),
          value = 1,
          min = 0,
          max = 1,
          step = 0.05
        )
      }
      if (sample_calc == 10) {
        s_size1_out <- numericInput(
          inputId = ns("s_sizeUI1"),
          label = withMathJax("$$\\sigma_{1}:{ }$$"),
          value = 1,
          min = 0,
          max = 1,
          step = 0.05
        )
      }
      if (sample_calc == 11) {
        s_size1_out <- numericInput(
          inputId = ns("s_sizeUI1"),
          label = withMathJax("$$\\rho_{0}:{ }$$"),
          value = 0.0,
          min = 0,
          max = 1,
          step = 0.05
        )
      }
      if (sample_calc == 12) {
        s_size1_out <- numericInput(
          inputId = ns("s_sizeUI1"),
          label = withMathJax("$$\\pi_{0}:{ }$$"),
          value = 0.5,
          min = 0,
          max = 1,
          step = 0.05
        )
      }
      if (sample_calc == 13) {
        s_size1_out <- numericInput(
          inputId = ns("s_sizeUI1"),
          label = withMathJax("$$\\pi_{0}:{ }$$"),
          value = 0.5,
          min = 0,
          max = 1,
          step = 0.05
        )
      }
      if (sample_calc == 14) {
        s_size1_out <- numericInput(
          inputId = ns("s_sizeUI1"),
          label = withMathJax("$$\\pi_{1}:{ }$$"),
          value = 0.5,
          min = 0,
          max = 1,
          step = 0.05
        )
      }
      if (sample_calc == 15) {
        s_size1_out <- numericInput(
          inputId = ns("s_sizeUI1"),
          label = withMathJax("$$\\sigma_{w}:{ }$$"),
          value = 1,
          min = 0,
          max = 1,
          step = 0.05
        )
      }
      if (sample_calc == 16) {
        s_size1_out <- numericInput(
          inputId = ns("s_sizeUI1"),
          label = withMathJax("$$\\lambda_{0}:{ }$$"),
          value = 10,
          min = 0,
          max = 1,
          step = 0.05
        )
      }
      if (sample_calc == 17) {
        s_size1_out <- numericInput(
          inputId = ns("s_sizeUI1"),
          label = withMathJax("$$\\lambda_{0}:{ }$$"),
          value = 10,
          min = 0,
          max = 1,
          step = 0.05
        )
      }
      if (sample_calc == 18) {
        s_size1_out <- numericInput(
          inputId = ns("s_sizeUI1"),
          label = withMathJax("$$\\lambda_{1}:{ }$$"),
          value = 10,
          min = 0,
          max = 1,
          step = 0.05
        )
      }
      
      s_size1_out
    })
    
    # Dynamic UI generation for s_sizeUI2 (replicating app.R lines 7640-7703)
    output$s_sizeUI2 <- renderUI({
      sample_calc <- input$sample_calc
      sample_size_type <- input$sample_size_type
      
      req(sample_calc)
      
      if (sample_size_type == 5) {
        sample_calc <- 15
      }
      
      if (sample_calc == 1) {
        s_size2_out <- numericInput(
          inputId = ns("s_sizeUI2"),
          label = withMathJax("$$\\Delta:{ }$$"),
          value = 1,
          min = 0,
          max = 1
        )
      }
      if (sample_calc == 2) {
        s_size2_out <- numericInput(
          inputId = ns("s_sizeUI2"),
          label = withMathJax("$$\\Delta:{ }$$"),
          value = 1,
          min = 0,
          max = 1
        )
      }
      if (sample_calc == 3) {
        s_size2_out <- numericInput(
          inputId = ns("s_sizeUI2"),
          label = withMathJax("$$\\Delta:{ }$$"),
          value = 1,
          min = 0,
          max = 1
        )
      }
      if (sample_calc == 4) {
        s_size2_out <- numericInput(
          inputId = ns("s_sizeUI2"),
          label = withMathJax("$$\\Delta:{ }$$"),
          value = 1,
          min = 0,
          max = 1
        )
      }
      if (sample_calc == 5) {
        s_size2_out <- numericInput(
          inputId = ns("s_sizeUI2"),
          label = withMathJax("$$\\Delta:{ }$$"),
          value = 1,
          min = 0,
          max = 1
        )
      }
      if (sample_calc == 6) {
        s_size2_out <- numericInput(
          inputId = ns("s_sizeUI2"),
          label = withMathJax("$$\\Delta:{ }$$"),
          value = 1,
          min = 0,
          max = 1
        )
      }
      if (sample_calc == 7) {
        s_size2_out <- numericInput(
          inputId = ns("s_sizeUI2"),
          label = withMathJax("$$\\Delta:{ }$$"),
          value = 1,
          min = 0,
          max = 1
        )
      }
      if (sample_calc == 8) {
        s_size2_out <- numericInput(
          inputId = ns("s_sizeUI2"),
          label = withMathJax("$$\\Delta_{\\bar{D}}:{ }$$"),
          value = 1,
          min = 0,
          max = 1
        )
      }
      if (sample_calc == 9) {
        s_size2_out <- NULL
      }
      if (sample_calc == 10) {
        s_size2_out <- NULL
      }
      if (sample_calc == 11) {
        s_size2_out <- numericInput(
          inputId = ns("s_sizeUI2"),
          label = withMathJax("$$\\rho_{1}:{ }$$"),
          value = 0.5,
          min = 0,
          max = 1,
          step = 0.05
        )
      }
      if (sample_calc == 12) {
        s_size2_out <- numericInput(
          inputId = ns("s_sizeUI2"),
          label = withMathJax("$$\\pi_{1}:{ }$$"),
          value = 0.1,
          min = 0,
          max = 1,
          step = 0.05
        )
      }
      if (sample_calc == 13) {
        s_size2_out <- numericInput(
          inputId = ns("s_sizeUI2"),
          label = withMathJax("$$\\pi_{1}:{ }$$"),
          value = 0.1,
          min = 0,
          max = 1,
          step = 0.05
        )
      }
      if (sample_calc == 14) {
        s_size2_out <- numericInput(
          inputId = ns("s_sizeUI2"),
          label = withMathJax("$$\\pi_{2}:{ }$$"),
          value = 0.1,
          min = 0,
          max = 1,
          step = 0.05
        )
      }
      if (sample_calc == 15) {
        s_size2_out <- numericInput(
          inputId = ns("s_sizeUI2"),
          label = withMathJax("$$\\Delta_{b}:{ }$$"),
          value = 1,
          min = 2,
          step = 1
        )
      }
      if (sample_calc == 16) {
        s_size2_out <- numericInput(
          inputId = ns("s_sizeUI2"),
          label = withMathJax("$$\\lambda_{1}:{ }$$"),
          value = 20,
          min = 2,
          step = 1
        )
      }
      if (sample_calc == 17) {
        s_size2_out <- numericInput(
          inputId = ns("s_sizeUI2"),
          label = withMathJax("$$\\lambda_{1}:{ }$$"),
          value = 20,
          min = 2,
          step = 1
        )
      }
      if (sample_calc == 18) {
        s_size2_out <- numericInput(
          inputId = ns("s_sizeUI2"),
          label = withMathJax("$$\\lambda_{2}:{ }$$"),
          value = 20,
          min = 2,
          step = 1
        )
      }
      s_size2_out
    })
    
    # Dynamic UI generation for s_sizeUI3 (replicating app.R lines 7706-7780)
    output$s_sizeUI3 <- renderUI({
      sample_calc <- input$sample_calc
      power_s <- input$power_s
      sample_size_type <- input$sample_size_type
      
      req(sample_calc)
      
      if (sample_size_type == 5) {
        sample_calc <- 15
      }
      
      if (sample_calc == 1) {
        s_size3_out <- NULL
      }
      if (sample_calc == 2) {
        s_size3_out <- NULL
      }
      if (sample_calc == 3) {
        s_size3_out <- NULL
      }
      if (sample_calc == 4) {
        s_size3_out <- NULL
      }
      if (sample_calc == 5) {
        s_size3_out <- NULL
      }
      if (sample_calc == 6) {
        if (power_s == TRUE) {
          s_size3_out <- numericInput(
            inputId = ns("s_sizeUI3"),
            label = withMathJax("$$n_{2}:{ }$$"),
            value = 10,
            min = 1
          )
        } else {
          s_size3_out <- NULL
        }
      }
      if (sample_calc == 7) {
        if (power_s == TRUE) {
          s_size3_out <- numericInput(
            inputId = ns("s_sizeUI3"),
            label = withMathJax("$$n_{2}:{ }$$"),
            value = 10,
            min = 1
          )
        } else {
          s_size3_out <- NULL
        }
      }
      if (sample_calc == 8) {
        s_size3_out <- NULL
      }
      if (sample_calc == 9) {
        s_size3_out <- NULL
      }
      if (sample_calc == 10) {
        if (power_s == TRUE) {
          s_size3_out <- numericInput(
            inputId = ns("s_sizeUI3"),
            label = withMathJax("$$n_{2}:{ }$$"),
            value = 10,
            min = 1
          )
        } else {
          s_size3_out <- NULL
        }
      }
      if (sample_calc == 11) {
        s_size3_out <- NULL
      }
      if (sample_calc == 12) {
        s_size3_out <- NULL
      }
      if (sample_calc == 13) {
        s_size3_out <- NULL
      }
      if (sample_calc == 14) {
        s_size3_out <- NULL
      }
      if (sample_calc == 15) {
        s_size3_out <- NULL
      }
      if (sample_calc == 16) {
        s_size3_out <- NULL
      }
      if (sample_calc == 17) {
        s_size3_out <- NULL
      }
      if (sample_calc == 18) {
        if (power_s == TRUE) {
          s_size3_out <- numericInput(
            inputId = ns("s_sizeUI3"),
            label = withMathJax("$$n_{2}:{ }$$"),
            value = 10,
            min = 1
          )
        } else {
          s_size3_out <- NULL
        }
      }
      
      s_size3_out
    })
    
    # Dynamic UI generation for s_sizeUI4 (replicating app.R lines 7783-7848)
    output$s_sizeUI4 <- renderUI({
      sample_calc <- input$sample_calc
      power_s <- input$power_s
      sample_size_type <- input$sample_size_type
      
      req(sample_calc)
      
      if (sample_size_type == 5) {
        sample_calc <- 15
      }
      
      if (sample_calc == 1) {
        s_size4_out <- NULL
      }
      if (sample_calc == 2) {
        s_size4_out <- NULL
      }
      if (sample_calc == 3) {
        s_size4_out <- NULL
      }
      if (sample_calc == 4) {
        s_size4_out <- NULL
      }
      if (sample_calc == 5) {
        s_size4_out <- NULL
      }
      if (sample_calc == 6) {
        s_size4_out <- NULL
      }
      if (sample_calc == 7) {
        s_size4_out <- numericInput(
          inputId = ns("s_sizeUI4"),
          label = withMathJax("$$\\sigma_{2}:{ }$$"),
          value = 10,
          min = 0,
          max = 1
        )
      }
      if (sample_calc == 8) {
        s_size4_out <- NULL
      }
      if (sample_calc == 9) {
        s_size4_out <- numericInput(
          inputId = ns("s_sizeUI4"),
          label = withMathJax("$$\\sigma_{1}:{ }$$"),
          value = 2,
          min = 0,
          max = 1,
          step = 0.05
        )
      }
      if (sample_calc == 10) {
        s_size4_out <- numericInput(
          inputId = ns("s_sizeUI4"),
          label = withMathJax("$$\\sigma_{2}:{ }$$"),
          value = 2,
          min = 0,
          max = 1,
          step = 0.05
        )
      }
      if (sample_calc == 11) {
        s_size4_out <- NULL
      }
      if (sample_calc == 12) {
        s_size4_out <- NULL
      }
      if (sample_calc == 13) {
        s_size4_out <- NULL
      }
      if (sample_calc == 14) {
        s_size4_out <- NULL
      }
      if (sample_calc == 15) {
        s_size4_out <- numericInput(
          inputId = ns("s_sizeUI4"),
          label = withMathJax("$$\\text{Levels }:{ }$$"),
          value = 4,
          min = 2,
          step = 1,
          width = "300px"
        )
      }
      if (sample_calc == 16) {
        s_size4_out <- NULL
      }
      if (sample_calc == 17) {
        s_size4_out <- NULL
      }
      if (sample_calc == 18) {
        s_size4_out <- NULL
      }
      
      s_size4_out
    })
    
    # Sample Size Calculations (replicating app.R lines 5166-5330)
    s_size_results <- reactive({
      alt <- input$one_or_two_size
      sample_calc <- input$sample_calc
      power_s <- input$power_s
      s_size_alpha <- input$s_size_alpha
      s_size_beta <- input$s_size_beta
      s_size_n <- input$s_size_n
      s_sizeUI1 <- input$s_sizeUI1
      s_sizeUI2 <- input$s_sizeUI2
      s_sizeUI3 <- input$s_sizeUI3
      s_sizeUI4 <- input$s_sizeUI4
      sample_size_type <- input$sample_size_type
      
      req(sample_size_type, sample_calc, s_sizeUI1, s_sizeUI2, alt)
      
      if (s_size_alpha == 0 || s_size_beta == 0) {
        return(NULL)
      }
      
      if (sample_size_type == 5) {
        sample_calc <- 15
      }
      
      # Calculate sample size
      if (power_s == FALSE) {
        
        if (sample_calc == 1) {
          if (alt == "less") {
            alt <- "greater"
          }
          s_size_out <- sample.size.mean.z.onesample(
            effect.size = s_sizeUI2,
            variance = s_sizeUI1^2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = TRUE
          )
        }
        if (sample_calc == 3) {
          if (alt == "less") {
            alt <- "greater"
          }
          s_size_out <- sample.size.mean.z.twosample.independent(
            effect.size = s_sizeUI2,
            variance = s_sizeUI1^2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = TRUE
          )
        }
        if (sample_calc == 5) {
          if (alt == "less") {
            alt <- "greater"
          }
          s_size_out <- sample.size.mean.t.onesample(
            effect.size = s_sizeUI2,
            variance = s_sizeUI1^2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = TRUE
          )
        }
        if (sample_calc == 6) {
          if (alt == "less") {
            alt <- "greater"
          }
          s_size_out <- sample.size.mean.t.test.twosample.independent.equal.variance(
            mean.g1 = 0,
            mean.g2 = s_sizeUI2,
            variance.est.g1 = s_sizeUI1^2,
            variance.est.g2 = s_sizeUI1^2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            null.hypothesis.difference = 0,
            alternative = alt,
            details = TRUE,
            power.from.actual = TRUE
          )
        }
        if (sample_calc == 7) {
          req(s_sizeUI4)
          if (alt == "less") {
            alt <- "greater"
          }
          s_size_out <- sample.size.mean.t.test.twosample.independent.unequal.variance(
            mean.g1 = 0,
            mean.g2 = s_sizeUI2,
            variance.est.g1 = s_sizeUI1^2,
            variance.est.g2 = s_sizeUI4^2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            null.hypothesis.difference = 0,
            alternative = alt,
            details = TRUE,
            power.from.actual = TRUE
          )
        }
        if (sample_calc == 8) {
          if (alt == "less") {
            alt <- "greater"
          }
          s_size_out <- sample.size.mean.t.twosample.dependent.dbar(
            effect.size = s_sizeUI2,
            variance.diff = s_sizeUI1^2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = TRUE
          )
        }
        if (sample_calc == 9) {
          req(s_sizeUI4)
          s_size_out <- sample.size.variance.onesample(
            null.hypothesis.variance = s_sizeUI1^2,
            alternative.hypothesis.variance = s_sizeUI4^2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = TRUE
          )
        }
        if (sample_calc == 10) {
          req(s_sizeUI4)
          s_size_out <- sample.size.variance.twosample.independent(
            variance.estimate.g1 = s_sizeUI1^2,
            variance.estimate.g2 = s_sizeUI4^2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = TRUE
          )
        }
        if (sample_calc == 11) {
          s_size_out <- sample.size.cor.pearson.r.onesample(
            null.hypothesis.correlation = s_sizeUI1,
            alternative.hypothesis.correlation = s_sizeUI2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = TRUE
          )
        }
        if (sample_calc == 12) {
          s_size_out <- sample.size.proportion.test.onesample.approximate(
            null.hypothesis.proportion = s_sizeUI1,
            alternative.hypothesis.proportion = s_sizeUI2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = TRUE
          )
        }
        if (sample_calc == 13) {
          s_size_out <- sample.size.proportion.test.onesample.exact(
            null.hypothesis.proportion = s_sizeUI1,
            alternative.hypothesis.proportion = s_sizeUI2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = TRUE
          )
        }
        if (sample_calc == 14) {
          s_size_out <- sample.size.proportion.test.twosample.approximate(
            proportion.g1 = s_sizeUI1,
            proportion.g2 = s_sizeUI2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = TRUE
          )
        }
        if (sample_calc == 15) {
          req(s_sizeUI4)
          if (s_sizeUI4 < 2) {
            s_size_out <- data.frame(error_message = "Number of levels must be at least 2")
          } else {
            s_size_out <- power.anova.test(
              groups = s_sizeUI4,
              n = NULL,
              between.var = var(c(rep(0, s_sizeUI4 - 2), -0.5 * s_sizeUI2, 0.5 * s_sizeUI2)),
              within.var = s_sizeUI1^2,
              sig.level = s_size_alpha,
              power = 1 - s_size_beta
            )
          }
        }
        if (sample_calc == 16) { # Poisson rate one sample - exact
          req(s_sizeUI1, s_sizeUI2, s_size_alpha, s_size_beta, alt)
          s_size_out <- sample.size.count.poisson.onesample.exact(
            lambda_0 = s_sizeUI1,
            lambda_1 = s_sizeUI2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt
          )
        }
        if (sample_calc == 17) { # Poisson rate one sample - approximate
          s_size_out <- sample.size.count.poisson.onesample.approximate(
            lambda.null.hypothesis = s_sizeUI1,
            lambda.alternative.hypothesis = s_sizeUI2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = TRUE
          )
        }
        if (sample_calc == 18) { # Poisson rate two sample
          s_size_out <- sample.size.count.poisson.twosample.approximate(
            lambda_1 = s_sizeUI1,
            lambda_2 = s_sizeUI2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt
          )
        }
        
      } # end sample size calcs
      
      # Calculate power
      else if (power_s == TRUE) {
        req(s_size_n, alt)
        if (sample_calc == 1) {
          if (alt == "less") {
            s_sizeUI2 <- -s_sizeUI2
          }
          s_size_out <- power.mean.z.onesample(
            sample.size = s_size_n,
            effect.size = s_sizeUI2,
            variance = s_sizeUI1^2,
            alpha = s_size_alpha,
            alternative = alt,
            details = TRUE
          )
        }
        if (sample_calc == 3) { # no two sample z power function in lolcat?
          
          # two-tailed
          if (alt == "two.sided") {
            z_beta <- sqrt((s_size_n * s_sizeUI2^2) / (2 * s_sizeUI1^2)) - qnorm(1 - (s_size_alpha / 2))
            s_size_out <- pnorm(z_beta)
          }
          if (alt != "two.sided") {
            z_beta <- sqrt((s_size_n * s_sizeUI2^2) / (2 * s_sizeUI1^2)) - qnorm(1 - (s_size_alpha))
            s_size_out <- pnorm(z_beta)
          }
        }
        if (sample_calc == 5) {
          if (alt == "less") {
            s_sizeUI2 <- -s_sizeUI2
          }
          s_size_out <- power.mean.t.onesample(
            sample.size = s_size_n,
            effect.size = s_sizeUI2,
            variance.est = s_sizeUI1^2,
            alpha = s_size_alpha,
            alternative = alt,
            details = TRUE
          )
        }
        if (sample_calc == 6) {
          if (alt == "less") {
            s_sizeUI2 <- -s_sizeUI2
          }
          s_size_out <- power.mean.t.test.twosample.independent.equal.variance(
            mean.g1 = 0,
            mean.g2 = s_sizeUI2,
            variance.est.g1 = s_sizeUI1^2,
            variance.est.g2 = s_sizeUI1^2,
            sample.size.g1 = s_size_n,
            sample.size.g2 = s_sizeUI3,
            null.hypothesis.difference = 0,
            alpha = s_size_alpha,
            alternative = alt,
            details = TRUE
          )
        }
        if (sample_calc == 7) {
          if (alt == "less") {
            s_sizeUI2 <- -s_sizeUI2
          }
          s_size_out <- power.mean.t.test.twosample.independent.unequal.variance(
            mean.g1 = 0,
            mean.g2 = s_sizeUI2,
            variance.est.g1 = s_sizeUI1^2,
            variance.est.g2 = s_sizeUI4^2,
            sample.size.g1 = s_size_n,
            sample.size.g2 = s_sizeUI3,
            null.hypothesis.difference = 0,
            alpha = s_size_alpha,
            alternative = alt,
            details = TRUE
          )
        }
        if (sample_calc == 8) {
          if (alt == "less") {
            s_sizeUI2 <- -s_sizeUI2
          }
          s_size_out <- power.mean.t.onesample(
            sample.size = s_size_n,
            effect.size = s_sizeUI2,
            variance.est = s_sizeUI1^2,
            alpha = s_size_alpha,
            alternative = alt,
            details = TRUE
          )
        }
        if (sample_calc == 9) {
          s_size_out <- power.variance.onesample(
            sample.size = s_size_n,
            null.hypothesis.variance = s_sizeUI1^2,
            alternative.hypothesis.variance = s_sizeUI4^2,
            alpha = s_size_alpha,
            alternative = alt,
            details = TRUE
          )
        }
        if (sample_calc == 10) {
          s_size_out <- power.variance.twosample.independent(
            variance.estimate.g1 = s_sizeUI1^2,
            variance.estimate.g2 = s_sizeUI4^2,
            sample.size.g1 = s_size_n,
            sample.size.g2 = s_sizeUI3,
            alpha = s_size_alpha,
            alternative = alt,
            details = TRUE
          )
        }
        if (sample_calc == 11) {
          req(s_sizeUI1, s_sizeUI2, s_size_alpha, alt)
          s_size_out <- power.cor.pearson.r.onesample(
            sample.size = s_size_n,
            null.hypothesis.correlation = s_sizeUI1,
            alternative.hypothesis.correlation = s_sizeUI2,
            alpha = s_size_alpha,
            alternative = alt,
            details = TRUE
          )
        }
        if (sample_calc == 12) {
          s_size_out <- power.proportion.test.onesample.approximate(
            null.hypothesis.proportion = s_sizeUI1,
            alternative.hypothesis.proportion = s_sizeUI2,
            alpha = s_size_alpha,
            sample.size = s_size_n,
            alternative = alt,
            details = TRUE
          )
        }
        if (sample_calc == 13) {
          s_size_out <- power.proportion.test.onesample.exact(
            null.hypothesis.proportion = s_sizeUI1,
            alternative.hypothesis.proportion = s_sizeUI2,
            alpha = s_size_alpha,
            sample.size = s_size_n,
            alternative = alt,
            details = TRUE
          )
        }
        if (sample_calc == 14) {
          s_size_out <- power.proportion.test.twosample.approximate(
            proportion.g1 = s_sizeUI1,
            proportion.g2 = s_sizeUI2,
            alpha = s_size_alpha,
            sample.size = s_size_n,
            alternative = alt,
            details = TRUE
          )
        }
        if (sample_calc == 15) {
          if (s_sizeUI4 < 2) {
            s_size_out <- data.frame(error_message = "Number of levels must be at least 2")
          } else {
            s_size_out <- power.anova.test(
              groups = s_sizeUI4,
              n = s_size_n,
              between.var = var(c(rep(0, s_sizeUI4 - 2), -0.5 * s_sizeUI2, 0.5 * s_sizeUI2)),
              within.var = s_sizeUI1^2,
              sig.level = s_size_alpha,
              power = NULL
            )
          }
        }
        if (sample_calc == 16) {
          req(s_size_n, s_sizeUI1, s_sizeUI2, s_size_alpha, alt)
          s_size_out <- power.count.poisson.onesample.exact(
            n = s_size_n,
            lambda_0 = s_sizeUI1,
            lambda_1 = s_sizeUI2,
            alpha = s_size_alpha,
            alternative = alt
          )
        }
        if (sample_calc == 17) {
          s_size_out <- power.count.poisson.onesample.approximate(
            sample.size = s_size_n,
            lambda.null.hypothesis = s_sizeUI1,
            lambda.alternative.hypothesis = s_sizeUI2,
            alpha = s_size_alpha,
            alternative = alt,
            details = TRUE
          )
        }
        if (sample_calc == 18) {
          s_size_out <- power.count.poisson.twosample.approximate(
            n1 = s_size_n,
            n2 = s_sizeUI3,
            lambda_1 = s_sizeUI1,
            lambda_2 = s_sizeUI2,
            alpha = s_size_alpha,
            alternative = alt
          )
        }
        
      } # end power calcs
      
      # Round results to 4 decimal places (replicating app.R line 5329)
      round.object(s_size_out, 4)
    })
    
    # HTML output formatting (replicating app.R lines 11126+)
    output$pretty_ssize <- renderUI({
      alt <- input$one_or_two_size
      sample_calc <- input$sample_calc
      power_s <- input$power_s
      s_size_alpha <- input$s_size_alpha
      s_size_beta <- input$s_size_beta
      s_size_n <- input$s_size_n
      s_sizeUI1 <- input$s_sizeUI1
      s_sizeUI2 <- input$s_sizeUI2
      s_sizeUI3 <- input$s_sizeUI3
      s_sizeUI4 <- input$s_sizeUI4
      sample_size_type <- input$sample_size_type
      results <- s_size_results()
      
      req(sample_calc)
      req(sample_size_type)
      
      if (sample_size_type == 5) {
        sample_calc <- 15
      }
      
      
      # Helper function to create properly spaced table rows
      create_table_row <- function(cell1, cell2 = "", cell3 = "") {
        if (cell3 != "") {
          # Three-column layout with spacing
          c(
            "<tr>",
            "<td>", cell1, "</td>",
            "<td>&nbsp;&nbsp;&nbsp;&nbsp;</td>",
            "<td>", cell2, "</td>",
            "<td>&nbsp;&nbsp;&nbsp;&nbsp;</td>",
            "<td>", cell3, "</td>",
            "</tr>"
          )
        } else if (cell2 != "") {
          # Two-column layout with spacing
          c(
            "<tr>",
            "<td>", cell1, "</td>",
            "<td>&nbsp;&nbsp;&nbsp;&nbsp;</td>",
            "<td>", cell2, "</td>",
            "</tr>"
          )
        } else {
          # Single-column layout
          c(
            "<tr>",
            "<td>", cell1, "</td>",
            "</tr>"
          )
        }
      }
      
      if (power_s == FALSE) { # sample size
        if (sample_calc == 1) {
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - One-Sample Mean: ", safe_get(results, "test", "z"), " test, σ known", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            create_table_row(
              paste("$\\alpha = $", s_size_alpha),
              paste("$\\beta = $", s_size_beta)
            ),
            create_table_row(paste("$\\sigma = $", s_sizeUI1)),
            create_table_row(paste("$\\Delta = $", s_sizeUI2)),
            create_table_row(paste("$n_{calc} = $", safe_get(results, "actual", "N/A"))),
            create_table_row(
              paste("$n = $", safe_get(results, "sample.size", "N/A")),
              paste("Power = ", safe_get(results, "power", "N/A"))
            ),
            "</table>"
          )))
        } else if (sample_calc == 3) { # Two-sample Mean z Independent
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - Two-Sample Independent Means: ", safe_get(results, "test", "z"), " test, σ known", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            create_table_row(
              paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)),
              paste("$\\beta = $", safe_get(results, "beta", s_size_beta))
            ),
            create_table_row(paste("$\\sigma = $", s_sizeUI1)),
            create_table_row(paste("$\\Delta = $", safe_get(results, "effect.size", s_sizeUI2))),
            create_table_row(paste("$n_{calc} = $", safe_get(results, "actual", "N/A"))),
            create_table_row(
              paste("$n = $", safe_get(results, "sample.size", "N/A")),
              paste("Power = ", safe_get(results, "power", "N/A"))
            ),
            "</table>"
          )))
        } else if (sample_calc == 5) { # One-sample Mean t
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - One-Sample Mean: ", safe_get(results, "test", "t"), " test, σ unknown", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", safe_get(results, "beta", s_size_beta)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta = $", safe_get(results, "effect.size", s_sizeUI2)), "</td></tr>",
            "<tr>",
            "<td>", paste("$n_{calc} = $", safe_get(results, "actual", "N/A")), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", safe_get(results, "sample.size", "N/A")), "</td>",
            "<td></td><td>", paste("Power = ", safe_get(results, "power", "N/A")), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 6) { # Two-sample Mean t equal variance
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - Two-Sample Independent Means: ", safe_get(results, "test", "t"), " test, unknown but equal σ", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", safe_get(results, "beta", s_size_beta)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta = $", safe_get(results, "mean.diff", s_sizeUI2)), "</td></tr>",
            "<tr>",
            "<td>", paste("$n_1 = $", safe_get(results, "sample.size.g1", "N/A")), "</td>",
            "<td></td>",
            "<td>", paste("$n_2 = $", safe_get(results, "sample.size.g2", "N/A")), "</td></tr><tr>",
            "<td>", paste("Power = ", safe_get(results, "power", "N/A")), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 7) { # Two-sample Mean t unequal variance
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - Two-Sample Independent Means: ", safe_get(results, "test", "t"), " test, unknown and unequal σ", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", safe_get(results, "beta", s_size_beta)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma_1 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\sigma_2 = $", s_sizeUI4), "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta = $", safe_get(results, "mean.diff", s_sizeUI2)), "</td></tr>",
            "<tr>",
            "<td>", paste("$n_1 = $", safe_get(results, "sample.size.g1", "N/A")), "</td>",
            "<td></td>",
            "<td>", paste("$n_2 = $", safe_get(results, "sample.size.g2", "N/A")), "</td></tr><tr>",
            "<td>", paste("Power = ", safe_get(results, "power", "N/A")), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 8) { # Two-sample Mean t Dependent
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - Two-Sample Dependent Means: ", safe_get(results, "test", "t"), " test, σ unknown", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", safe_get(results, "beta", s_size_beta)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma_{\\bar{D}} = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta_{\\bar{D}} = $", safe_get(results, "effect.size", s_sizeUI2)), "</td></tr>",
            "<tr>",
            "<td>", paste("$n = $", safe_get(results, "sample.size", "N/A")), "</td>",
            "<td></td></tr><tr>",
            "<td>", paste("Power = ", safe_get(results, "power", "N/A")), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 9) { # One-sample Variance
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - One-Sample Variance: ", safe_get(results, "test", "χ²"), " test", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", safe_get(results, "beta", s_size_beta)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma_0 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\sigma_1 = $", s_sizeUI4), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", safe_get(results, "sample.size", "N/A")), "</td>",
            "<td></td><td>", paste("Power = ", safe_get(results, "power", "N/A")), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 10) { # Two-sample Variance
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - Two-Sample Independent Variances: ", safe_get(results, "test", "F"), " test", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", safe_get(results, "beta", s_size_beta)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma_1 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\sigma_2 = $", s_sizeUI4), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n_1 = $", safe_get(results, "sample.size.g1", "N/A")), "</td>",
            "<td></td>",
            "<td>", paste("$n_2 = $", safe_get(results, "sample.size.g2", "N/A")), "</td></tr><tr>",
            "<td>", paste("Power = ", safe_get(results, "power", "N/A")), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 11) { # One-sample Pearson r
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - Correlation: ", safe_get(results, "test", "t"), " test", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            create_table_row(
              paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)),
              paste("$\\beta = $", safe_get(results, "beta", s_size_beta))
            ),
            create_table_row(
              paste("$\\rho_0 = $", s_sizeUI1),
              ""
            ),
            create_table_row(paste("$\\rho_1 = $", s_sizeUI2)),
            create_table_row(paste("$n_{calc} = $", safe_get(results, "actual", "N/A"))),
            create_table_row(paste("$n = $", safe_get(results, "sample.size", "N/A"))),
            create_table_row(paste("Power = ", safe_get(results, "power", "N/A"))),
            "</table>"
          )))
        } else if (sample_calc == 12) { # One-sample Proportion Approximate
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - One-Sample Proportion: ", safe_get(results, "test", "z"), " test (approximate)", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", safe_get(results, "beta", s_size_beta)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\pi_0 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\pi_1 = $", s_sizeUI2), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", safe_get(results, "sample.size", "N/A")), "</td>",
            "<td></td><td>", paste("Power = ", safe_get(results, "power", "N/A")), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 13) { # One-sample Proportion Exact
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - One-Sample Proportion: ", safe_get(results, "test", "exact"), " test (exact)", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", safe_get(results, "beta", s_size_beta)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\pi_0 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\pi_1 = $", s_sizeUI2), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", safe_get(results, "sample.size", "N/A")), "</td>",
            "<td></td><td>", paste("Power = ", safe_get(results, "power", "N/A")), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 14) { # Two-sample Proportion Approximate
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - Two-Sample Proportions: ", safe_get(results, "test", "z"), " test (approximate)", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", safe_get(results, "beta", s_size_beta)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\pi_1 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\pi_2 = $", s_sizeUI2), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", safe_get(results, "sample.size", "N/A")), "</td>",
            "<td></td><td>", paste("Power = ", safe_get(results, "power", "N/A")), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 15) { # ANOVA
          # Check for error message first
          if (!is.null(safe_get(results, "error_message", NULL))) {
            output <- HTML(safe_get(results, "error_message", "Unknown error"))
          } else {
            output <- withMathJax(HTML(c(
              paste("Note that if you are interested in the power or sample size for an interaction, you can enter the effect's df + 1 in Levels above.",
                    "<br><br>",
                    "<b>", "Analysis of Variance Sample Size Calculation", "</b>"),
              "<br>",
              "Assumes that two level means depart the grand mean by ± 0.5Δ",
              "<br>", "<br>",
              "<table>",
              create_table_row(
                paste("$\\alpha = $", safe_get(results, "sig.level", s_size_alpha)),
                paste("$\\beta = $", 1 - safe_get(results, "power", 1 - s_size_beta))
              ),
              create_table_row(
                paste("$\\sigma_w = $", s_sizeUI1),
                paste("$\\sigma^2_w = $", safe_get(results, "within.var", "N/A"))
              ),
              create_table_row(
                paste("$\\Delta_b = $", s_sizeUI2),
                paste("$\\sigma^2_b = $", safe_get(results, "between.var", "N/A"))
              ),
              create_table_row(paste("$n_{calc} = $", safe_get(results, "n", "N/A"))),
              create_table_row(
                paste("$n = $", ceiling(safe_get(results, "n", 0)), " per level"),
                paste(safe_get(results, "groups", s_sizeUI4), " levels")
              ),
              "</table>"
            )))
          }
        } else if (sample_calc == 16) { # One-sample Poisson Exact
          # Check for error message first
          if (!is.null(safe_get(results, "error_message", NULL))) {
            output <- HTML(safe_get(results, "error_message", "Unknown error"))
          } else {
            output <- withMathJax(HTML(c(
              paste("<b>", "Sample Size Calculations - Exact One-Sample Poisson", "</b>"),
              "<br>",
              if (alt == "two.sided") {
                "<b>Two-Tail</b>"
              } else {
                "<b>One-Tail</b>"
              },
              "<br><br>",
              "<table>",
              create_table_row(
                paste("$\\alpha_{actual} = $", safe_get(results, "alpha", s_size_alpha)),
                paste("$\\beta_{actual} = $", 1 - safe_get(results, "power", 0))
              ),
              create_table_row(
                paste("$\\lambda_0 = $", s_sizeUI1),
                paste("$\\lambda_1 = $", s_sizeUI2)
              ),
              create_table_row(paste("$n = $", safe_get(results, "n", "N/A"))),
              create_table_row(
                paste("$X_{crit L} = $", safe_get(results, "crit_x_l", "N/A")),
                paste("$X_{crit U} = $", safe_get(results, "crit_x_u", "N/A"))
              ),
              create_table_row(paste("Power = ", safe_get(results, "power", "N/A"))),
              "</table>"
            )))
          }
        } else if (sample_calc == 17) { # One-sample Poisson Approximate
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - Approximate One-Sample Poisson: ", safe_get(results, "test", "z"), " test", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            create_table_row(
              paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)),
              paste("$\\beta = $", safe_get(results, "beta", s_size_beta))
            ),
            create_table_row(
              paste("$\\lambda_0 = $", s_sizeUI1),
              paste("$\\lambda_1 = $", s_sizeUI2)
            ),
            create_table_row(paste("$n_{calc} = $", safe_get(results, "actual", "N/A"))),
            create_table_row(
              paste("$n = $", safe_get(results, "sample.size", "N/A")),
              paste("Power = ", safe_get(results, "power", "N/A"))
            ),
            "</table>"
          )))
        } else if (sample_calc == 18) { # Two-sample Poisson Approximate
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - Approximate Two-Sample Poisson", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            create_table_row(
              paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)),
              paste("$\\beta = $", safe_get(results, "beta", s_size_beta))
            ),
            create_table_row(
              paste("$\\lambda_1 = $", s_sizeUI1),
              paste("$\\lambda_2 = $", s_sizeUI2)
            ),
            create_table_row(paste("$n = $", safe_get(results, "n", "N/A"))),
            create_table_row(paste("Power = ", safe_get(results, "power", "N/A"))),
            "</table>"
          )))
        } else {
          # Generic fallback for any other test types
          output <- withMathJax(HTML(paste("<b>Sample Size Calculation Results</b><br>", 
                              "Test: ", safe_get(results, "test", "Statistical Test"), "<br>",
                              "Sample Size: ", safe_get(results, "sample.size", "N/A"), "<br>",
                              "Power: ", safe_get(results, "power", "N/A"))))
        }
      } else { # power calculation
        # For power calculations, results might be a simple number
        if (is.numeric(results) && length(results) == 1) {
          power_value <- results
        } else {
          power_value <- safe_get(results, "power", "N/A")
        }
        
        # Calculate beta (Type II error rate) = 1 - power
        if (is.numeric(power_value) && !is.na(power_value)) {
          beta_value <- 1 - power_value
        } else {
          beta_value <- "N/A"
        }
        
        # Create detailed output based on test type
        if (sample_calc == 1) { # One-sample Mean z
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - One-Sample Mean: ", safe_get(results, "test", "z"), " test, σ known", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", s_size_n), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta = $", s_sizeUI2), "</td></tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 3) { # Two-sample Mean z Independent
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - Two-Sample Independent Means: ", safe_get(results, "test", "z"), " test, σ known", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", s_size_n), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta = $", s_sizeUI2), "</td></tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 5) { # One-sample Mean t
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - One-Sample Mean: ", safe_get(results, "test", "t"), " test, σ unknown", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", s_size_n), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta = $", s_sizeUI2), "</td></tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 6) { # Two-sample Mean t equal variance
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - Two-Sample Independent Means: ", safe_get(results, "test", "t"), " test, equal variance", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n_1 = $", s_size_n), "</td>",
            "<td>", "</td>",
            "<td>", paste("$n_2 = $", s_sizeUI3), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta = $", s_sizeUI2), "</td></tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 7) { # Two-sample Mean t unequal variance
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - Two-Sample Independent Means: ", safe_get(results, "test", "t"), " test, unequal variance", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n_1 = $", s_size_n), "</td>",
            "<td>", "</td>",
            "<td>", paste("$n_2 = $", s_sizeUI3), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma_1 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\sigma_2 = $", s_sizeUI4), "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta = $", s_sizeUI2), "</td></tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 8) { # Two-sample Mean t Dependent
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - Two-Sample Dependent Means: ", safe_get(results, "test", "t"), " test", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", s_size_n), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma_{\\bar{D}} = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta_{\\bar{D}} = $", s_sizeUI2), "</td></tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 9) { # One-sample Variance
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - One-Sample Variance: ", safe_get(results, "test", "χ²"), " test", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", s_size_n), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma_0 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\sigma_1 = $", s_sizeUI4), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 10) { # Two-sample Variance
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - Two-Sample Independent Variances: ", safe_get(results, "test", "F"), " test", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n_1 = $", s_size_n), "</td>",
            "<td>", "</td>",
            "<td>", paste("$n_2 = $", s_sizeUI3), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma_1 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\sigma_2 = $", s_sizeUI4), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 11) { # One-sample Pearson r
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - Correlation: ", safe_get(results, "test", "t"), " test", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            create_table_row(
              paste("$\\alpha = $", s_size_alpha),
              paste("$\\beta = $", beta_value)
            ),
            create_table_row(paste("$n = $", s_size_n)),
            create_table_row(
              paste("$\\rho_0 = $", s_sizeUI1),
              ""
            ),
            create_table_row(paste("$\\rho_1 = $", s_sizeUI2)),
            create_table_row(paste("Power = ", power_value)),
            "</table>"
          )))
        } else if (sample_calc == 12) { # One-sample Proportion Approximate
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - One-Sample Proportion: ", safe_get(results, "test", "z"), " test (approximate)", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", s_size_n), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\pi_0 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\pi_1 = $", s_sizeUI2), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 13) { # One-sample Proportion Exact
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - One-Sample Proportion: ", safe_get(results, "test", "exact"), " test (exact)", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", s_size_n), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\pi_0 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\pi_1 = $", s_sizeUI2), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 14) { # Two-sample Proportion Approximate
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - Two-Sample Proportions: ", safe_get(results, "test", "z"), " test (approximate)", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", s_size_n), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\pi_1 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\pi_2 = $", s_sizeUI2), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 15) { # ANOVA
          # Check for error message first
          if (!is.null(safe_get(results, "error_message", NULL))) {
            output <- HTML(safe_get(results, "error_message", "Unknown error"))
          } else {
            output <- withMathJax(HTML(c(
              paste("Note that if you are interested in the power or sample size for an interaction, you can enter the effect's df + 1 in Levels above.",
                    "<br><br>",
                    "<b>", "Analysis of Variance Power Calculation", "</b>"),
              "<br>",
              "Assumes that two level means depart the grand mean by ± 0.5Δ",
              "<br>", "<br>",
              "<table>",
              create_table_row(
                paste("$\\alpha = $", safe_get(results, "sig.level", s_size_alpha)),
                paste("$\\beta = $", beta_value)
              ),
              create_table_row(
                paste("$\\sigma_w = $", s_sizeUI1),
                paste("$\\sigma^2_w = $", safe_get(results, "within.var", "N/A"))
              ),
              create_table_row(
                paste("$\\Delta_b = $", s_sizeUI2),
                paste("$\\sigma^2_b = $", safe_get(results, "between.var", "N/A"))
              ),
              create_table_row(
                paste("$n = $", ceiling(s_size_n), " per level"),
                paste(safe_get(results, "groups", s_sizeUI4), " levels")
              ),
              create_table_row(paste("Power = ", power_value)),
              "</table>"
            )))
          }
        } else if (sample_calc == 16) { # One-sample Poisson Exact
          # Check for error message first
          if (!is.null(safe_get(results, "error_message", NULL))) {
            output <- HTML(safe_get(results, "error_message", "Unknown error"))
          } else {
            output <- withMathJax(HTML(c(
              paste("<b>", "Power Calculations - Exact One-Sample Poisson", "</b>"),
              "<br>",
              if (alt == "two.sided") {
                "<b>Two-Tail</b>"
              } else {
                "<b>One-Tail</b>"
              },
              "<br><br>",
              "<table>",
              "<tr>",
              "<td>", paste("$\\alpha_{actual} = $", results["alpha",]), "</td>",
              "<td>", "</td>",
              "<td>", paste("$\\beta_{actual} = $", 1 - results["power",]), "</td>",
              "</tr>",
              "<tr>",
              "<td>", paste("$\\lambda_0 = $", s_sizeUI1), "</td>",
              "<td>", "</td>",
              "<td>", paste("$\\lambda_1 = $", s_sizeUI2), "</td>",
              "</tr>",
              "<tr>",
              "<td>", paste("$n = $", s_size_n), "</td>",
              "</tr><tr>",
              "<td>", paste("$X_{crit L} = $", results["crit_x_l",]), "</td><td></td>",
              "<td>", paste("$X_{crit U} = $", results["crit_x_u",]), "</td>",
              "</tr><tr>",
              "<td>", paste("Power = ", results["power",]), "</td>",
              "</tr>",
              "</table>"
            )))
          }
        } else if (sample_calc == 17) { # One-sample Poisson Approximate
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - One-Sample Poisson: ", safe_get(results, "test", "z"), " test (approximate)", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", s_size_n), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\lambda_0 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\lambda_1 = $", s_sizeUI2), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 18) { # Two-sample Poisson Approximate
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - Approximate Two-Sample Poisson", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            create_table_row(
              paste("$\\alpha = $", s_size_alpha),
              paste("$\\beta = $", beta_value)
            ),
            create_table_row(
              paste("$\\lambda_1 = $", s_sizeUI1),
              paste("$\\lambda_2 = $", s_sizeUI2)
            ),
            create_table_row(
              paste("$n_1 = $", s_size_n),
              paste("$n_2 = $", s_sizeUI3)
            ),
            create_table_row(paste("Power = ", power_value)),
            "</table>"
          )))
        } else {
          # Generic fallback for any other test types
          output <- withMathJax(HTML(paste("<b>Power Calculation Results</b><br>", 
                              "Test: ", safe_get(results, "test", "Statistical Test"), "<br>",
                              "Power: ", power_value, "<br>",
                              "Beta: ", beta_value, "<br>",
                              "Sample Size: ", s_size_n)))
        }
      }
      
      output
    })
  })
}
