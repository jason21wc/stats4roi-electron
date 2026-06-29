# How ANOVA Works Concept Simulator
# CC BY-NC-SA 4.0 Steven Ouellette

library(shiny)
library(ggplot2)

source("modules/config/global_config.R")

create_anova_simulator_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      p("Enter Parameters"),
      fluidRow(
        column(6, numericInput(inputId = ns("aov_mu1"), label = "$\\mu_1$", value = 100)),
        column(6, numericInput(inputId = ns("aov_sd1"), label = "$\\sigma_1$", value = 1))
      ),
      fluidRow(
        column(6, numericInput(inputId = ns("aov_mu2"), label = "$\\mu_2$", value = 100)),
        column(6, numericInput(inputId = ns("aov_sd2"), label = "$\\sigma_2$", value = 1))
      ),
      fluidRow(
        column(6, numericInput(inputId = ns("aov_mu3"), label = "$\\mu_3$", value = 100)),
        column(6, numericInput(inputId = ns("aov_sd3"), label = "$\\sigma_3$", value = 1))
      ),
      fluidRow(
        column(6, numericInput(inputId = ns("aov_mu4"), label = "$\\mu_4$", value = 100)),
        column(6, numericInput(inputId = ns("aov_sd4"), label = "$\\sigma_4$", value = 1)),
        column(6, numericInput(inputId = ns("aov_n"), label = "Sample Size", value = 5)),
        column(6, numericInput(inputId = ns("aov_dec"), label = "Decimals", value = 4))
      ),
      actionButton(ns("recalculate"), "Resample", class = "btn-primary"),
      htmlOutput(ns("stats_out"))
    ),
    mainPanel(
      h3("How Does ANOVA Work?"),
      plotOutput(ns("pop_plot")),
      downloadUI(ns("pop_plot_dl")),
      plotOutput(ns("samp_plot")),
      downloadUI(ns("samp_plot_dl"))
    )
  )
}

create_anova_simulator_server <- function(id, color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    color <- reactive({
      color_palette()
    })

    take_sample <- eventReactive(
      list(
        input$recalculate,
        input$aov_mu1, input$aov_sd1,
        input$aov_mu2, input$aov_sd2,
        input$aov_mu3, input$aov_sd3,
        input$aov_mu4, input$aov_sd4,
        input$aov_n
      ),
      {
        mu1 <- input$aov_mu1
        sd1 <- input$aov_sd1
        mu2 <- input$aov_mu2
        sd2 <- input$aov_sd2
        mu3 <- input$aov_mu3
        sd3 <- input$aov_sd3
        mu4 <- input$aov_mu4
        sd4 <- input$aov_sd4
        n <- input$aov_n
        req(mu1, sd1, mu2, sd2, mu3, sd3, mu4, sd4, n)

        samp1 <- data.frame(Sample = 1, Measure = rnorm(n = n, mean = mu1, sd = sd1))
        samp2 <- data.frame(Sample = 2, Measure = rnorm(n = n, mean = mu2, sd = sd2))
        samp3 <- data.frame(Sample = 3, Measure = rnorm(n = n, mean = mu3, sd = sd3))
        samp4 <- data.frame(Sample = 4, Measure = rnorm(n = n, mean = mu4, sd = sd4))

        data <- rbind(samp1, samp2, samp3, samp4)
        data$Sample <- factor(data$Sample)
        data
      },
      ignoreNULL = FALSE
    )

    pop_plot <- reactive({
      mu1 <- input$aov_mu1
      sd1 <- input$aov_sd1
      mu2 <- input$aov_mu2
      sd2 <- input$aov_sd2
      mu3 <- input$aov_mu3
      sd3 <- input$aov_sd3
      mu4 <- input$aov_mu4
      sd4 <- input$aov_sd4
      req(mu1, sd1, mu2, sd2, mu3, sd3, mu4, sd4)

      pal <- color()
      len <- 5000
      max_sd <- max(sd1, sd2, sd3, sd4)
      max_mu <- max(mu1, mu2, mu3, mu4)
      min_mu <- min(mu1, mu2, mu3, mu4)

      xmin <- min_mu - 6 * max_sd
      xmax <- max_mu + 6 * max_sd
      x <- seq(from = xmin, to = xmax, length.out = len)

      pop1 <- data.frame(Pop = 1, x = x, y = dnorm(x = x, mean = mu1, sd = sd1))
      pop2 <- data.frame(Pop = 2, x = x, y = dnorm(x = x, mean = mu2, sd = sd2))
      pop3 <- data.frame(Pop = 3, x = x, y = dnorm(x = x, mean = mu3, sd = sd3))
      pop4 <- data.frame(Pop = 4, x = x, y = dnorm(x = x, mean = mu4, sd = sd4))

      plot_data <- rbind(pop1, pop2, pop3, pop4)
      plot_data$Pop <- factor(plot_data$Pop)

      ggplot(data = plot_data, aes(x = x, y = y, fill = Pop)) +
        geom_line() +
        geom_area(position = "identity") +
        scale_fill_manual(values = alpha(pal[-1], 0.2)) +
        ylab("PDF(x)") +
        theme(legend.position = c(.1, .8))
    })

    samp_plot <- reactive({
      mu1 <- input$aov_mu1
      sd1 <- input$aov_sd1
      mu2 <- input$aov_mu2
      sd2 <- input$aov_sd2
      mu3 <- input$aov_mu3
      sd3 <- input$aov_sd3
      mu4 <- input$aov_mu4
      sd4 <- input$aov_sd4
      req(mu1, sd1, mu2, sd2, mu3, sd3, mu4, sd4)

      pal <- color()
      sample <- take_sample()

      x_bar1 <- mean(sample$Measure[sample$Sample == 1])
      s_1 <- sd(sample$Measure[sample$Sample == 1])
      x_bar2 <- mean(sample$Measure[sample$Sample == 2])
      s_2 <- sd(sample$Measure[sample$Sample == 2])
      x_bar3 <- mean(sample$Measure[sample$Sample == 3])
      s_3 <- sd(sample$Measure[sample$Sample == 3])
      x_bar4 <- mean(sample$Measure[sample$Sample == 4])
      s_4 <- sd(sample$Measure[sample$Sample == 4])

      len <- 5000
      max_sd <- max(sd1, sd2, sd3, sd4)
      max_mu <- max(mu1, mu2, mu3, mu4)
      min_mu <- min(mu1, mu2, mu3, mu4)

      xmin <- min_mu - 6 * max_sd
      xmax <- max_mu + 6 * max_sd

      ggplot(data = sample, aes(x = Measure)) +
        stat_function(fun = dnorm, args = list(mean = x_bar1, sd = s_1), n = len, color = pal[2]) +
        stat_function(fun = dnorm, args = list(mean = x_bar2, sd = s_2), n = len, color = pal[3]) +
        stat_function(fun = dnorm, args = list(mean = x_bar3, sd = s_3), n = len, color = pal[4]) +
        stat_function(fun = dnorm, args = list(mean = x_bar4, sd = s_4), n = len, color = pal[5]) +
        xlim(c(xmin, xmax)) +
        ylab("PDF(Measure)")
    })

    output$stats_out <- renderUI({
      R <- input$aov_dec
      sample <- take_sample()
      n <- input$aov_n
      req(R, sample, n)

      x_bar1 <- mean(sample$Measure[sample$Sample == 1])
      s_1 <- sd(sample$Measure[sample$Sample == 1])
      s2_1 <- var(sample$Measure[sample$Sample == 1])
      x_bar2 <- mean(sample$Measure[sample$Sample == 2])
      s_2 <- sd(sample$Measure[sample$Sample == 2])
      s2_2 <- var(sample$Measure[sample$Sample == 2])
      x_bar3 <- mean(sample$Measure[sample$Sample == 3])
      s_3 <- sd(sample$Measure[sample$Sample == 3])
      s2_3 <- var(sample$Measure[sample$Sample == 3])
      x_bar4 <- mean(sample$Measure[sample$Sample == 4])
      s_4 <- sd(sample$Measure[sample$Sample == 4])
      s2_4 <- var(sample$Measure[sample$Sample == 4])

      MSB <- n * var(c(x_bar1, x_bar2, x_bar3, x_bar4))
      MSW <- mean(c(s2_1, s2_2, s2_3, s2_4))
      F_rat <- MSB / MSW
      p_f <- pf(F_rat, 4 - 1, 4 * (n - 1), lower.tail = FALSE)

      HTML(paste0(
        "<table style='width:100%'><tr>",
        "<caption>Sample Statistics</caption>",
        "<td>", withMathJax("$\\bar{x}_{1}=$"), ro(x_bar1, R), "</td>",
        "<td>", withMathJax("$s_{1}=$"), ro(s_1, R), "</td>",
        "<td>", withMathJax("${s_{1}}^2=$"), ro(s2_1, R), "</td>",
        "</tr>",
        "<td>", withMathJax("$\\bar{x}_{2}=$"), ro(x_bar2, R), "</td>",
        "<td>", withMathJax("$s_{2}=$"), ro(s_2, R), "</td>",
        "<td>", withMathJax("${s_{2}}^2=$"), ro(s2_2, R), "</td>",
        "</tr>",
        "<td>", withMathJax("$\\bar{x}_{3}=$"), ro(x_bar3, R), "</td>",
        "<td>", withMathJax("$s_{3}=$"), ro(s_3, R), "</td>",
        "<td>", withMathJax("${s_{3}}^2=$"), ro(s2_3, R), "</td>",
        "</tr>",
        "<td>", withMathJax("$\\bar{x}_{4}=$"), ro(x_bar4, R), "</td>",
        "<td>", withMathJax("$s_{4}=$"), ro(s_4, R), "</td>",
        "<td>", withMathJax("${s_{4}}^2=$"), ro(s2_4, R), "</td>",
        "</tr>",
        "</table>",
        "<br>",
        withMathJax("$MSB=n\\times{s^2}_\\bar{X}=$"), ro(MSB, R), "<br>",
        withMathJax("$MSW=\\bar{s^2}=$"), ro(MSW, R), "<br>",
        withMathJax("$F=\\frac{MSB}{MSW}=$"), ro(F_rat, R), "<br>",
        withMathJax("$p(F)=$"), ro(p_f, R)
      ))
    })

    output$pop_plot <- renderPlot({
      pop_plot()
    })

    output$samp_plot <- renderPlot({
      samp_plot()
    })

    plot_height <- reactive(400 * 4)
    plot_width <- reactive(400 * 4)
    downloadServer("pop_plot_dl", pop_plot, height = plot_height, width = plot_width)
    downloadServer("samp_plot_dl", samp_plot, height = plot_height, width = plot_width)
  })
}
