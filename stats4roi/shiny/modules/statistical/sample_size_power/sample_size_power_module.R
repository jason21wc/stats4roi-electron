# Sample Size and Power Analysis Module for stats4ROI
# This module exactly replicates the Sample Size/Power functionality from the original app
# Original implementation: app.R lines 1114-1148 (UI) and 5166-5320+ (server logic)

library(shiny)
library(lolcat)
library(dplyr)

# Source global config for rounding function
source("modules/config/global_config.R")

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

#####Estimation functions###################################################
# Function to calculate sample size for desired CI width (Mean, σ known)
# Replicating app.R lines 5432-5445
sample_size_for_mean_CI <- function(width, sd, conf.level = 0.95, sigfig = 2) {
  # Parameters:
  # width = width of confidence interval (e.g. 2*delta)
  # sd = known standard deviation
  # conf.level = confidence level of the ci
  # sigfig = number of significant digits in final width
  z <- qnorm(1 - (1 - conf.level)/2)
  n <- ceiling(signif((2 * z * sd/(width))^2, sigfig))
  act_width <- 2*z*sd/sqrt(n)
  return(list(
    n = n,
    act_width = act_width
  ))
}

# Function to calculate sample size for binomial proportion CI
# Replicating app.R lines 5448-5485
sample_size_for_binom_CI <- function(width, p_est = 0.5, conf.level = 0.95, sigfig = 2) {
  # Parameters:
  # width = width of confidence interval (e.g. 2*delta)
  # p_est = best guess as to proportion (.5 is worst-case)
  # conf.level = confidence level of the ci
  # sigfig = number of significant digits in final width
  
  alpha <- 1 - conf.level
  
  # Function to check if CI width is less than target width for a given n
  check_width <- function(n) {
    # Calculate confidence interval using exact method
    ci <- stats::binom.test(round(p_est * n), n, conf.level = conf.level)$conf.int
    actual_width <- diff(ci)
    return(actual_width)
  }
  # Binary search implementation
  n_min <- 2  # Minimum possible sample size
  n_max <- 1e6  # Maximum reasonable sample size
  
  while (n_max - n_min > 1) {
    n_mid <- floor((n_min + n_max) / 2)
    act_width <- check_width(n_mid)
    if (signif(act_width, sigfig) <= width) {
      n_max <- n_mid
    } else {
      n_min <- n_mid
    }
  }
  n <- n_max
  act_width <- check_width(n)
  
  if (n == 1e6) {
    warning("Maximum sample size reached. Consider wider interval width.")
  }
  
  return(list(n = n, act_width = act_width))
}

# Function to calculate sample size for SD CI (relative width)
# Replicating app.R lines 5488-5511
sample_size_for_sd_CI <- function(target_relative_width = 0.5, conf.level = 0.95, sigfig = 2, max_n = 100000) {
  # Parameters:
  # target_relative_width = the ratio of the width of the confidence interval to the calculated standard deviation
  # for example, the default of 0.5 will give the sample size needed to result in a confidence
  # interval that is half as wide as the standard deviation that is found
  # conf.level = confidence level of the ci
  # sigfig = number of significant digits in final width
  # max_n = the maximum iterations before stopping
  alpha <- 1 - conf.level
  for (n in 3:max_n) {  # minimum df = 2
    chi2_lower <- qchisq(alpha / 2, df = n - 1, lower.tail = FALSE)
    chi2_upper <- qchisq(1 - alpha / 2, df = n - 1, lower.tail = FALSE)
    
    relative_width <- sqrt(n - 1) * 
      (1 / sqrt(chi2_upper) - 1 / sqrt(chi2_lower))
    
    if (signif(relative_width, sigfig) <= target_relative_width) {
      return(list(n = n, act_width = relative_width))
    }
  }
  
  warning("Sample size exceeds maximum allowed (max_n).")
  return(NA)
}

# Function to calculate sample size for Poisson CI (relative width)
# Replicating app.R lines 5514-5543
sample_size_for_poisson_CI <- function(lambda_est, 
                                       target_relative_width = 0.2, 
                                       conf.level = 0.95, 
                                       sigfig = 2,
                                       max_T = 1e5) {
  # Parameters
  # lambda_est = expected average count
  # target_relative_width = the ratio of the width of the confidence interval to lambda_est
  # conf.level = confidence level of the ci
  # sigfig = number of significant digits in final width
  # max_T = the maximum iterations before stopping
  
  alpha <- 1 - conf.level
  for (T in 1:max_T) {
    expected_counts <- lambda_est * T
    
    # Lower and Upper CI bounds using chi-squared approximation
    upper <- qchisq(1 - alpha/2, 2 * (expected_counts+1)) / 2
    lower <- qchisq(alpha/2, 2 * expected_counts) / 2
    
    # Relative width calculation
    relative_width <- (upper - lower) / (lambda_est * T)
    if (signif(relative_width, sigfig) <= target_relative_width) {
      return(list(n = T, act_width = relative_width))
    }
  }
  
  warning("Required sample size exceeds maximum allowed (max_T).")
  return(NA)
}

# Function to calculate sample size for correlation CI
# Replicating app.R lines 5548-5588
sample_size_for_correlation_CI <- function(r_est, 
                                           width = 0.2, 
                                           conf.level = 0.95, 
                                           sigfig = 4,
                                           max_n = 1000) {
  
  # Uses Fisher's transformation to normalize
  # Parameters
  # r_est = estimated correlation coefficient
  # width = the ratio of the width of the confidence interval to rho
  # conf.level = confidence level of the ci
  # sigfig = number of significant digits in final width
  # max_n = the maximum iterations before stopping
  
  alpha <- 1 - conf.level
  # Fisher's z transform of the true correlation
  z <- 0.5 * log((1 + r_est) / (1 - r_est))
  
  # Critical value for two-sided CI
  z_crit <- qnorm(1 - alpha / 2)
  
  # Loop through sample sizes
  for (n in 4:max_n) {  # minimum n is 4
    SE_z <- 1 / sqrt(n - 3)
    z_lower <- z - z_crit * SE_z
    z_upper <- z + z_crit * SE_z
    
    # Back-transform to r
    r_lower <- (exp(2 * z_lower) - 1) / (exp(2 * z_lower) + 1)
    r_upper <- (exp(2 * z_upper) - 1) / (exp(2 * z_upper) + 1)
    
    act_width <- r_upper - r_lower
    
    if (signif(act_width, sigfig) <= width) {
      return(list(n = n, act_width = width))
    }
  }
  
  warning("Required sample size exceeds maximum allowed (max_n).")
  return(NA)
}

# Sample Size and Power Analysis UI (replicating app.R lines 1114-1148)
# REBUILT from scratch to match monolithic app structure exactly
create_sample_size_power_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = ns("sample_size_type"),
        label = "Calculate the sample size or power for:",
        choices = c("Means" = 1, "Standard Deviations" = 2, "Proportions (binomial)" = 3, "Rates (Poisson)" = 4, "ANOVA" = 5, "Correlations" = 6)
        # No selected parameter - matches monolithic app
      ),
      radioButtons(
        inputId = ns("sample_size_mode"),
        label = "Calculate for",
        choices = c("Hypothesis Test" = 1, "Estimation" = 2),
        selected = 1
      ),
      conditionalPanel(
        condition = "input.sample_size_mode == 1",
        ns = ns,
        # Use renderUI like monolithic app - this avoids issues with multiple radioButtons having same inputId
        uiOutput(ns("s_size_tests"))
      )
    ), # end sidebarpanel
    mainPanel(
      fluidRow(
        conditionalPanel(
          condition = "!(input.sample_size_type == 5 || input.sample_size_mode == 2)",
          ns = ns,
          selectInput(
            inputId = ns("one_or_two_size"),
            label = "Alternative is:",
            choices = c("Equal to the null" = "two.sided", "Less Than the null" = "less", "Greater Than the null" = "greater"),
            width = "150px",
            selected = 2
          )
        ),
        conditionalPanel(
          condition = "input.sample_size_mode == 1",
          ns = ns,
          checkboxInput(
            inputId = ns("power_s"),
            label = "Power",
            value = FALSE
          )
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
          )),
          column(9,
            conditionalPanel(
              condition = "input.sample_size_mode == 2",
              ns = ns,
              numericInput(
                inputId = ns("s_size_sigfig"),
                label = "CI Width SigFigs",
                value = 2,
                min = 1,
                max = 9,
                step = 1,
                width = "150px"
              )
            )
          )
        ),
        fluidRow(
          column(3,
            conditionalPanel(
              condition = "input.sample_size_mode == 1",
              ns = ns,
              conditionalPanel(
                condition = "input.power_s == 0",
                ns = ns,
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
                condition = "input.power_s == 1",
                ns = ns,
                numericInput(
                  inputId = ns("s_size_n"),
                  label = withMathJax("$$n:{ }$$"),
                  value = 10,
                  min = 0,
                  step = 1,
                  width = "150px"
                )
              )
            )
          ),
          column(3, 
            # s_sizeUI3 - moved from renderUI to UI function using conditionalPanel
            # Only for hypothesis test mode (sample_size_mode == 1)
            conditionalPanel(
              condition = "input.sample_size_mode == 1",
              ns = ns,
              # Handle sample_size_type == 5 (ANOVA) which sets sample_calc to 15 - returns NULL
              # sample_calc == 6 with power_s == TRUE
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 6 && input.power_s == 1",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI3"),
                  label = withMathJax("$$n_{2}:{ }$$"),
                  value = 10,
                  min = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 7 with power_s == TRUE
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 7 && input.power_s == 1",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI3"),
                  label = withMathJax("$$n_{2}:{ }$$"),
                  value = 10,
                  min = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 10 with power_s == TRUE
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 10 && input.power_s == true",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI3"),
                  label = withMathJax("$$n_{2}:{ }$$"),
                  value = 10,
                  min = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 18 with power_s == TRUE
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 18 && input.power_s == 1",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI3"),
                  label = withMathJax("$$n_{2}:{ }$$"),
                  value = 10,
                  min = 1,
                  width = "150px"
                )
              )
            )
          )
        ),
        fluidRow(
          column(3, 
            # s_sizeUI1 - moved from renderUI to UI function using conditionalPanel
            # Hypothesis test mode (sample_size_mode == 1)
            conditionalPanel(
              condition = "input.sample_size_mode == 1",
              ns = ns,
              # Handle sample_size_type == 5 (ANOVA) which sets sample_calc to 15
              conditionalPanel(
                condition = "input.sample_size_type == 5",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\sigma_{w}:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  step = 0.05,
                  width = "150px"
                )
              ),
              # sample_calc == 1
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 1",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\sigma:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 2
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 2",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\sigma:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 3
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 3",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\sigma:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 4
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 4",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\sigma:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 5
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 5",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\sigma:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 6
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 6",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\sigma:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 7
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 7",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\sigma:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 8
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 8",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\sigma_{\\bar{D}}:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 9
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 9",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\sigma_{0}:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  step = 0.05,
                  width = "150px"
                )
              ),
              # sample_calc == 10
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 10",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\sigma_{1}:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  step = 0.05,
                  width = "150px"
                )
              ),
              # sample_calc == 11
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 11",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\rho_{0}:{ }$$"),
                  value = 0.0,
                  min = 0,
                  max = 1,
                  step = 0.05,
                  width = "150px"
                )
              ),
              # sample_calc == 12
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 12",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\pi_{0}:{ }$$"),
                  value = 0.5,
                  min = 0,
                  max = 1,
                  step = 0.05,
                  width = "150px"
                )
              ),
              # sample_calc == 13
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 13",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\pi_{0}:{ }$$"),
                  value = 0.5,
                  min = 0,
                  max = 1,
                  step = 0.05,
                  width = "150px"
                )
              ),
              # sample_calc == 14
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 14",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\pi_{1}:{ }$$"),
                  value = 0.5,
                  min = 0,
                  max = 1,
                  step = 0.05,
                  width = "150px"
                )
              ),
              # sample_calc == 16
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 16",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\lambda_{0}:{ }$$"),
                  value = 10,
                  min = 0,
                  max = 1,
                  step = 0.05,
                  width = "150px"
                )
              ),
              # sample_calc == 17
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 17",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\lambda_{0}:{ }$$"),
                  value = 10,
                  min = 0,
                  max = 1,
                  step = 0.05,
                  width = "150px"
                )
              ),
              # sample_calc == 18
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 18",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\lambda_{1}:{ }$$"),
                  value = 10,
                  min = 0,
                  max = 1,
                  step = 0.05,
                  width = "150px"
                )
              )
            ),
            # Estimation mode (sample_size_mode == 2)
            conditionalPanel(
              condition = "input.sample_size_mode == 2",
              ns = ns,
              conditionalPanel(
                condition = "input.sample_size_type == 1",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\sigma_{est}:{ }$$"),
                  value = 1,
                  min = 0,
                  width = "150px"
                )
              ),
              conditionalPanel(
                condition = "input.sample_size_type == 3",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\pi_{est}:{ }$$"),
                  value = 0.5,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              conditionalPanel(
                condition = "input.sample_size_type == 4",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\lambda_{est}:{ }$$"),
                  value = 5,
                  min = 0,
                  width = "150px"
                )
              ),
              conditionalPanel(
                condition = "input.sample_size_type == 6",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI1"),
                  label = withMathJax("$$\\rho_{est}:{ }$$"),
                  value = 0.5,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              )
            )
          ),
          column(6, 
            # s_sizeUI4 - moved from renderUI to UI function using conditionalPanel
            # Only for hypothesis test mode (sample_size_mode == 1)
            conditionalPanel(
              condition = "input.sample_size_mode == 1",
              ns = ns,
              # Handle sample_size_type == 5 (ANOVA) which sets sample_calc to 15
              conditionalPanel(
                condition = "input.sample_size_type == 5",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI4"),
                  label = withMathJax("$$\\text{Levels }:{ }$$"),
                  value = 4,
                  min = 2,
                  step = 1,
                  width = "300px"
                )
              ),
              # sample_calc == 7
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 7",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI4"),
                  label = withMathJax("$$\\sigma_{2}:{ }$$"),
                  value = 10,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 9
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 9",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI4"),
                  label = withMathJax("$$\\sigma_{1}:{ }$$"),
                  value = 2,
                  min = 0,
                  max = 1,
                  step = 0.05,
                  width = "150px"
                )
              ),
              # sample_calc == 10
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 10",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI4"),
                  label = withMathJax("$$\\sigma_{2}:{ }$$"),
                  value = 2,
                  min = 0,
                  max = 1,
                  step = 0.05,
                  width = "150px"
                )
              )
            )
          )
        ),
        fluidRow(
          column(3, 
            # s_sizeUI2 - moved from renderUI to UI function using conditionalPanel
            # Hypothesis test mode (sample_size_mode == 1)
            conditionalPanel(
              condition = "input.sample_size_mode == 1",
              ns = ns,
              # Handle sample_size_type == 5 (ANOVA) which sets sample_calc to 15
              conditionalPanel(
                condition = "input.sample_size_type == 5",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$\\Delta_{b}:{ }$$"),
                  value = 1,
                  min = 2,
                  step = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 1
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 1",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$\\Delta:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 2
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 2",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$\\Delta:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 3
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 3",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$\\Delta:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 4
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 4",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$\\Delta:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 5
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 5",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$\\Delta:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 6
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 6",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$\\Delta:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 7
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 7",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$\\Delta:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 8
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 8",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$\\Delta_{\\bar{D}}:{ }$$"),
                  value = 1,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 11
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 11",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$\\rho_{1}:{ }$$"),
                  value = 0.5,
                  min = 0,
                  max = 1,
                  step = 0.05,
                  width = "150px"
                )
              ),
              # sample_calc == 12
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 12",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$\\pi_{1}:{ }$$"),
                  value = 0.1,
                  min = 0,
                  max = 1,
                  step = 0.05,
                  width = "150px"
                )
              ),
              # sample_calc == 13
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 13",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$\\pi_{1}:{ }$$"),
                  value = 0.1,
                  min = 0,
                  max = 1,
                  step = 0.05,
                  width = "150px"
                )
              ),
              # sample_calc == 14
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 14",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$\\pi_{2}:{ }$$"),
                  value = 0.1,
                  min = 0,
                  max = 1,
                  step = 0.05,
                  width = "150px"
                )
              ),
              # sample_calc == 16
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 16",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$\\lambda_{1}:{ }$$"),
                  value = 20,
                  min = 2,
                  step = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 17
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 17",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$\\lambda_{1}:{ }$$"),
                  value = 20,
                  min = 2,
                  step = 1,
                  width = "150px"
                )
              ),
              # sample_calc == 18
              conditionalPanel(
                condition = "input.sample_size_type != 5 && input.sample_calc == 18",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$\\lambda_{2}:{ }$$"),
                  value = 20,
                  min = 2,
                  step = 1,
                  width = "150px"
                )
              )
            ),
            # Estimation mode (sample_size_mode == 2)
            conditionalPanel(
              condition = "input.sample_size_mode == 2",
              ns = ns,
              conditionalPanel(
                condition = "input.sample_size_type == 1",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$CI_{Width}:{ }$$"),
                  value = 1,
                  min = 0,
                  width = "150px"
                )
              ),
              conditionalPanel(
                condition = "input.sample_size_type == 2",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$CI_{RelWidth}:{ }$$"),
                  value = 0.5,
                  min = 0,
                  width = "150px"
                )
              ),
              conditionalPanel(
                condition = "input.sample_size_type == 3",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$CI_{Width}:{ }$$"),
                  value = 0.1,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              ),
              conditionalPanel(
                condition = "input.sample_size_type == 4",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$CI_{RelWidth}:{ }$$"),
                  value = 1,
                  min = 0,
                  width = "150px"
                )
              ),
              conditionalPanel(
                condition = "input.sample_size_type == 6",
                ns = ns,
                numericInput(
                  inputId = ns("s_sizeUI2"),
                  label = withMathJax("$$CI_{Width}:{ }$$"),
                  value = 0.2,
                  min = 0,
                  max = 1,
                  width = "150px"
                )
              )
            )
          )
        )
      ),
      htmlOutput(ns("pretty_ssize"))
    )
  ) # end sidebarLayout
}

# Sample Size and Power Analysis Server (replicating app.R lines 5166-5320+)
create_sample_size_power_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # CRITICAL FIX: Create a reactive trigger to force immediate invalidation
    # When sample_calc is inside nested conditionalPanels, Shiny may not immediately
    # detect the change. This trigger forces s_size_results to invalidate immediately.
    sample_calc_trigger <- reactiveVal(0)
    
    observeEvent(input$sample_calc, {
      sample_calc_trigger(sample_calc_trigger() + 1)
    }, ignoreInit = FALSE, ignoreNULL = FALSE)
    
    # Define choice_sample_size in server (like monolithic app)
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
    
    # s_size_tests - renderUI like monolithic app (replicating app.R lines 7801-7826)
    # This avoids issues with multiple radioButtons having the same inputId in different conditionalPanels
    output$s_size_tests <- renderUI({
      sample_size_type <- input$sample_size_type
      
      req(sample_size_type)
      
      if (sample_size_type == 1) {
        s_size_test_out <- radioButtons(
          inputId = ns("sample_calc"),
          label = "Select the Test",
          choices = choice_sample_size[c(1, 3, 5:8)]
        )
      } else if (sample_size_type == 2) {
        s_size_test_out <- radioButtons(
          inputId = ns("sample_calc"),
          label = "Select the Test",
          choices = choice_sample_size[c(9:10)]
        )
      } else if (sample_size_type == 3) {
        s_size_test_out <- radioButtons(
          inputId = ns("sample_calc"),
          label = "Select the Test",
          choices = choice_sample_size[c(12:14)]
        )
      } else if (sample_size_type == 4) {
        s_size_test_out <- radioButtons(
          inputId = ns("sample_calc"),
          label = "Select the Test",
          choices = choice_sample_size[c(16, 17, 18)]
        )
      } else if (sample_size_type == 5) {
        s_size_test_out <- p("")
      } else if (sample_size_type == 6) {
        s_size_test_out <- radioButtons(
          inputId = ns("sample_calc"),
          label = "Select the Test",
          choices = choice_sample_size[c(11)]
        )
      } else {
        s_size_test_out <- NULL
      }
      
      s_size_test_out
    })
    
    # s_sizeUI1, s_sizeUI2, s_sizeUI3, s_sizeUI4 UIs remain as conditionalPanel (client-side)
    # These work fine and render instantly
    
    # Sample Size Calculations (replicating app.R lines 5166-5330)
    # CRITICAL FIX: Add sample_calc_trigger() as a dependency to force invalidation
    s_size_results <- reactive({
      # Access the trigger to force this reactive to invalidate when sample_calc changes
      # This ensures immediate invalidation even if input$sample_calc doesn't trigger it
      sample_calc_trigger()  # This dependency will force invalidation
      
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
      sample_size_mode <- if (is.null(input$sample_size_mode)) 1 else input$sample_size_mode # 1 = hyp test, 2 = estimation, default to 1
      sigfig <- input$s_size_sigfig
      
      # CRITICAL: Check if sample_size_type has a value - if it's NULL, we shouldn't proceed
      # This prevents the reactive from running with default/unselected values
      if (is.null(sample_size_type)) {
        return(NULL)
      }
      
      req(sample_size_type, s_size_alpha)
      
      # Handle ANOVA in estimation mode
      if (sample_size_type == 5 && sample_size_mode == 2) {
        return("estimation") # This will trigger message in pretty output
      }
      
      if (sample_size_mode == 1) { # Hypothesis test calculations
        # CRITICAL: Check if sample_calc has a value - if it's NULL, we shouldn't proceed
        # This prevents the reactive from running with default/unselected values
        if (is.null(sample_calc)) {
          return(NULL)
        }
        
        # CRITICAL: Validate that sample_calc matches sample_size_type
        # sample_calc == 11 should ONLY happen when sample_size_type == 6
        if (sample_calc == 11 && sample_size_type != 6) {
          return(NULL)
        }
        
        req(sample_calc, s_sizeUI1, s_sizeUI2, alt)
        
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
        if (sample_calc == 11) { # Correlation z-test - should only happen when sample_size_type == 6
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
      
      } else if (sample_size_mode == 2) { # Estimation mode
        if (sample_size_type == 1) {
          req(s_sizeUI1, s_sizeUI2, sigfig)
          s_size_out <- sample_size_for_mean_CI(
            width = s_sizeUI2, 
            sd = s_sizeUI1, 
            conf.level = 1 - s_size_alpha, 
            sigfig = sigfig
          )
        } else if (sample_size_type == 2) {
          req(s_sizeUI2, sigfig)
          s_size_out <- sample_size_for_sd_CI(
            target_relative_width = s_sizeUI2, 
            conf.level = 1 - s_size_alpha, 
            sigfig = sigfig
          )
        } else if (sample_size_type == 3) {
          req(s_sizeUI1, s_sizeUI2, sigfig)
          if (s_sizeUI2 >= 1 || s_sizeUI1 >= 1) {
            return(NULL)
          }
          s_size_out <- sample_size_for_binom_CI(
            width = s_sizeUI2, 
            p_est = s_sizeUI1, 
            conf.level = 1 - s_size_alpha, 
            sigfig = sigfig
          )
        } else if (sample_size_type == 4) {
          req(s_sizeUI1, s_sizeUI2, sigfig)
          s_size_out <- sample_size_for_poisson_CI(
            lambda_est = s_sizeUI1, 
            target_relative_width = s_sizeUI2, 
            conf.level = 1 - s_size_alpha, 
            sigfig = sigfig
          )
        } else if (sample_size_type == 5) {
          # Should already have been handled at beginning of function
          s_size_out <- "estimation"
        } else if (sample_size_type == 6) {
          req(s_sizeUI1, s_sizeUI2, sigfig)
          if (s_sizeUI1 >= 1 || s_sizeUI2 >= 1) {
            return(NULL)
          }
          s_size_out <- sample_size_for_correlation_CI(
            r_est = s_sizeUI1, 
            width = s_sizeUI2, 
            conf.level = 1 - s_size_alpha, 
            sigfig = sigfig
          )
        }
      } # end estimation section
      
      # Round results to 4 decimal places (replicating app.R line 5426)
      
      
      result <- ro(s_size_out, 4)
      
      
      
      
      
      result
    })
    
    # Track when pretty_ssize dependencies change (s_size_results reactive)
    observe({
      # This will fire whenever s_size_results() changes (is invalidated)
      s_size_results_val <- s_size_results()
      
      
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
      sample_size_mode <- if (is.null(input$sample_size_mode)) 1 else input$sample_size_mode # 1 = hyp test, 2 = estimation, default to 1
      sigfig <- input$s_size_sigfig
      
      
      
      
      
      results <- s_size_results()
      
      
      
      
      
      req(sample_size_type, results)
      
      
      
      
      
      if (sample_size_mode == 2) { # Estimation mode
        if (sample_size_type == 1) {
          output <- HTML(paste(
            "<b>Sample Size Calculations for Estimation - Mean σ known</b>",
            "<br><br>",
            "<table>",
            "<tr><td>", 100*(1-s_size_alpha), "% Confidence Interval", "</td></tr>",
            "<tr><td>", paste(withMathJax("$\\sigma_{est} =$"), s_sizeUI1), "</td></tr>",
            "<tr><td>", "Target CI Width = ", s_sizeUI2, "</td></tr>",
            "<tr><td>", " Significant Figures Used = ", sigfig, "</td></tr>",
            "<tr><td>", paste(withMathJax("$n =$"), results$n), "</td></tr>",
            "<tr><td>", "Actual CI Width = ", results$act_width, "</td></tr>",
            "</table>"
          ))
        } else if (sample_size_type == 2) {
          output <- HTML(paste(
            "<b>Sample Size Calculations for Estimation - Standard Deviation</b>",
            "<br>Relative Width = (UCI-LCI)/σ",
            "<br><br>",
            "<table>",
            "<tr><td>", 100*(1-s_size_alpha), "% Confidence Interval", "</td></tr>",
            "<tr><td>", "Target Relative CI Width = ", s_sizeUI2, "</td></tr>",
            "<tr><td>", " Significant Figures Used = ", sigfig, "</td></tr>",
            "<tr><td>", paste(withMathJax("$n =$"), results$n), "</td></tr>",
            "<tr><td>", "Actual Relative CI Width = ", results$act_width, "</td></tr>",
            "</table>"
          ))
        } else if (sample_size_type == 3) {
          output <- HTML(paste(
            "<b>Sample Size Calculations for Estimation - Binomial Proportion (Exact)</b>",
            "<br><br>",
            "<table>",
            "<tr><td>", 100*(1-s_size_alpha), "% Confidence Interval", "</td></tr>",
            "<tr><td>", paste(withMathJax("$\\pi_{est} =$"), s_sizeUI1), "</td></tr>",
            "<tr><td>", "Target CI Width = ", s_sizeUI2, "</td></tr>",
            "<tr><td>", " Significant Figures Used = ", sigfig, "</td></tr>",
            "<tr><td>", paste(withMathJax("$n =$"), results$n), "</td></tr>",
            "<tr><td>", "Actual CI Width = ", results$act_width, "</td></tr>",
            "</table>"
          ))
        } else if (sample_size_type == 4) {
          output <- HTML(paste(
            "<b>Sample Size Calculations for Estimation - Poisson Rates (Exact)</b>",
            "<br>Relative Width = (UCI-LCI)/λ",
            "<br><br>",
            "<table>",
            "<tr><td>", 100*(1-s_size_alpha), "% Confidence Interval", "</td></tr>",
            "<tr><td>", paste(withMathJax("$\\lambda_{est} =$"), s_sizeUI1), "</td></tr>",
            "<tr><td>", "Target Relative CI Width = ", s_sizeUI2, "</td></tr>",
            "<tr><td>", " Significant Figures Used = ", sigfig, "</td></tr>",
            "<tr><td>", paste(withMathJax("$n =$"), results$n), "</td></tr>",
            "<tr><td>", "Actual Relative CI Width = ", results$act_width, "</td></tr>",
            "</table>"
          ))
        } else if (sample_size_type == 5) {
          output <- HTML("For calculating a confidence interval sample size for ANOVA mean estimates, select Means. <br>For calculating a confidence interval sample size for ANOVA standard deviation estimates, select Standard Deviation.")
        } else if (sample_size_type == 6) {
          output <- HTML(paste(
            "<b>Sample Size Calculations for Estimation - Correlation: Pearson's R</b>",
            "<br><br>",
            "<table>",
            "<tr><td>", 100*(1-s_size_alpha), "% Confidence Interval", "</td></tr>",
            "<tr><td>", paste(withMathJax("$\\rho_{est} =$"), s_sizeUI1), "</td></tr>",
            "<tr><td>", "Target CI Width = ", s_sizeUI2, "</td></tr>",
            "<tr><td>", " Significant Figures Used = ", sigfig, "</td></tr>",
            "<tr><td>", paste(withMathJax("$n =$"), results$n), "</td></tr>",
            "<tr><td>", "Actual CI Width = ", results$act_width, "</td></tr>",
            "</table>"
          ))
        }
        return(output)
      }
      
      # Hypothesis test mode (existing code)
      req(sample_calc)
      
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
