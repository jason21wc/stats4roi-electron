# Central Limit Theorem Concept Simulator

library(shiny)
library(ggplot2)

source("modules/config/global_config.R")

create_clt_simulator_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = ns("dist"),
        label = "Select distribution",
        choices = c(
          "Normal" = 1,
          "Skewed (+)" = 2,
          "Skewed (-)" = 3,
          "Exponential" = 4,
          "Uniform" = 5
        )
      ),
      sliderInput(inputId = ns("samp_n"), label = "Sample Size (n)", value = 5, min = 1, max = 100),
      actionButton(ns("go_plot"), "Start Sampling"),
      textOutput(ns("sample_progress"))
    ),
    mainPanel(
      h3("Central Limit Theorem"),
      plotOutput(ns("clt_plot")),
      downloadUI(ns("clt_plot_dl")),
      htmlOutput(ns("text_out"))
    )
  )
}

create_clt_simulator_server <- function(id, color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    color <- reactive({
      color_palette()
    })

    sample_progress <- reactiveVal("Click Start Sampling to run the simulation.")
    trigger <- reactiveVal(0)
    last_plot <- reactiveVal(NULL)

    output$sample_progress <- renderText({
      sample_progress()
    })

    stats <- reactiveValues(
      ready = FALSE,
      mu = NA,
      sigma = NA,
      samp_doublebar = NA,
      samp_s = NA,
      theo_doublebar = NA,
      theo_s = NA,
      g3 = NA,
      g3_p = NA,
      g4 = NA,
      g4_p = NA
    )

    # Changing distribution or n clears results (matches original app behavior)
    observeEvent(
      list(input$dist, input$samp_n),
      {
        stats$ready <- FALSE
        last_plot(NULL)
      },
      ignoreInit = TRUE
    )

    run_clt_simulation <- function(dist, n, pal) {
      reps <- 10000
      length_out <- 500
      stats$ready <- FALSE
      sample_progress(paste0("Taking samples: 0 of ", reps))

      if (dist == 1) {
        xmin <- -6
        xmax <- 6
        x <- seq(from = xmin, to = xmax, length.out = 500)
        bhd <- data.frame(facet = 1, x = x, y = dnorm(x))
        pop <- rnorm(reps * n)
        theo <- data.frame(facet = 2, x = x, y = dnorm(x = x, mean = 0, sd = 1 / sqrt(n)))
        theo$facet <- factor(theo$facet, levels = c(2), labels = c("RSD of Means"))
        stats$mu <- 0
        stats$sigma <- 1
        stats$theo_doublebar <- 0
        stats$theo_s <- ro(1 / sqrt(n), 4)
      } else if (dist == 2) {
        xmin <- 0
        xmax <- 10
        x <- seq(from = xmin, to = xmax, length.out = 500)
        bhd <- data.frame(facet = 1, x = x, y = dlnorm(x = x))
        pop <- rlnorm(reps * n)
        stats$mu <- ro(exp(0 + 1 / (2 * 1^1)), 4)
        stats$sigma <- ro(sqrt(exp(2 * 0 + 1^2) * (exp(1^2) - 1)), 4)
        stats$theo_doublebar <- stats$mu
        stats$theo_s <- ro(stats$sigma / sqrt(n), 4)
        theo <- data.frame(
          facet = 2, x = x,
          y = dnorm(x = x, mean = stats$theo_doublebar, sd = stats$sigma / sqrt(n))
        )
        theo$facet <- factor(theo$facet, levels = c(2), labels = c("RSD of Means"))
      } else if (dist == 3) {
        xmin <- 0
        xmax <- 10
        x <- seq(from = xmin, to = xmax, length.out = 500)
        xprime <- sort(x, decreasing = TRUE)
        bhd <- data.frame(facet = 1, x = xprime, y = dlnorm(x))
        pop <- 10 - rlnorm(reps * n)
        stats$mu <- ro(10 - exp(0 + 1 / (2 * 1^1)), 4)
        stats$sigma <- ro(sqrt(exp(2 * 0 + 1^2) * (exp(1^2) - 1)), 4)
        stats$theo_doublebar <- stats$mu
        stats$theo_s <- ro(stats$sigma / sqrt(n), 4)
        theo <- data.frame(
          facet = 2, x = x,
          y = dnorm(x = x, mean = stats$theo_doublebar, sd = stats$sigma / sqrt(n))
        )
        theo$facet <- factor(theo$facet, levels = c(2), labels = c("RSD of Means"))
      } else if (dist == 4) {
        xmin <- 0
        xmax <- 10
        x <- seq(from = xmin, to = xmax, length.out = 500)
        bhd <- data.frame(facet = 1, x = x, y = dexp(x = x, rate = 1 / 1))
        pop <- rexp(n = reps * n, rate = 1 / 1)
        theo <- data.frame(facet = 2, x = x, y = dnorm(x = x, mean = 1, sd = 1 / sqrt(n)))
        theo$facet <- factor(theo$facet, levels = c(2), labels = c("RSD of Means"))
        stats$mu <- 1
        stats$sigma <- 1
        stats$theo_doublebar <- 1
        stats$theo_s <- ro(1 / sqrt(n), 4)
      } else if (dist == 5) {
        xmin <- 0
        xmax <- 4
        x <- seq(from = xmin, to = xmax, length.out = 500)
        bhd <- data.frame(facet = 1, x = x, y = dunif(x = x, min = 1, max = 3))
        pop <- runif(n = reps * n, min = 1, max = 3)
        stats$mu <- 2
        stats$sigma <- ro(sqrt((3 - 1)^2 / 12), 4)
        stats$theo_doublebar <- 2
        stats$theo_s <- ro(sqrt((3 - 1)^2 / 12) / sqrt(n), 4)
        theo <- data.frame(
          facet = 2, x = x,
          y = dnorm(x = x, mean = 2, sd = sqrt((3 - 1)^2 / 12) / sqrt(n))
        )
        theo$facet <- factor(theo$facet, levels = c(2), labels = c("RSD of Means"))
      }

      samp_mean <- rep(NA, reps)

      withProgress(message = "Taking Samples", value = 0, {
        for (i in seq_len(reps)) {
          low <- n * (i - 1) + 1
          high <- i * n
          samp <- pop[low:high]
          samp_mean[i] <- sum(samp) / n
          incProgress(1 / reps, detail = paste("Sample", i, "of", reps))
          if (i == 1 || i == reps || i %% 250 == 0) {
            sample_progress(paste0("Taking samples: ", i, " of ", reps))
          }
        }
      })
      sample_progress(paste0("Complete: ", reps, " samples taken."))

      test <- summary.continuous(samp_mean)
      stats$samp_doublebar <- ro(test$mean, 4)
      stats$samp_s <- ro(sqrt(test$var), 4)
      stats$g3 <- ro(test$g3.skewness, 4)
      stats$g3_p <- ro(test$g3test.p, 4)
      stats$g4 <- ro(test$g4.kurtosis, 4)
      stats$g4_p <- ro(test$g4test.p, 4)
      stats$ready <- TRUE

      dense <- density(samp_mean, bw = "SJ", n = length_out, from = xmin, to = xmax)
      mean_rsd <- data.frame(facet = 2, dense[1:2])

      plot_data <- data.frame(bhd)
      names(plot_data) <- c("facet", "x", "y")
      plot_data <- rbind(plot_data, mean_rsd)
      plot_data$facet <- factor(plot_data$facet, levels = c(1, 2),
                                labels = c("Population", "RSD of Means"))

      leg_names <- c(
        "Population" = alpha(pal[4], 0.5),
        "RSD of Means" = alpha(pal[5], 0.5),
        "Theoretical RSD" = pal[2]
      )

      ggplot(data = plot_data, aes(x = x, y = y, fill = facet)) +
        facet_grid(rows = vars(facet), scales = "free_y") +
        geom_line() +
        geom_area() +
        geom_line(data = theo, aes(x = x, y = y, color = "Theoretical RSD")) +
        scale_fill_manual(values = leg_names) +
        theme(legend.title = element_blank()) +
        labs(caption = paste0("Simulation size = ", reps)) +
        ylab("Density") +
        scale_x_continuous(n.breaks = 6)
    }

    output$clt_plot <- renderPlot({
      go <- input$go_plot
      req(go > 0)

      dist <- input$dist
      n <- input$samp_n
      req(dist, n)

      pal <- color()
      req(length(pal) >= 5)

      if (go == trigger()) {
        return(last_plot())
      }

      p <- run_clt_simulation(dist, n, pal)
      trigger(go)
      last_plot(p)
      p
    })
    outputOptions(output, "clt_plot", suspendWhenHidden = FALSE)

    output$text_out <- renderUI({
      go <- input$go_plot
      req(go > 0)
      req(go == trigger())
      req(stats$ready)

      HTML(paste0(
        "<table style='width:100%'>",
        "<th>Population</th><th>Simulation Statistics</th><th>Theoretical RSD</th>",
        "<tr><td>", withMathJax("$\\mu=$"), stats$mu, "</td>",
        "<td>", withMathJax("$\\overline{\\overline{X}}=$"), stats$samp_doublebar, "</td>",
        "<td>", withMathJax("$\\mu_{\\overline{X}}=$"), stats$theo_doublebar, "</td>",
        "</td></tr>",
        "<tr><td>", withMathJax("$\\sigma=$"), stats$sigma, "</td>",
        "<td>", withMathJax("$s_{\\overline{X}}=$"), stats$samp_s, "</td>",
        "<td>", withMathJax("$\\sigma_{\\overline{X}}=$"), stats$theo_s, "</td></tr></table>",
        "<br>Normality tests for a random sample of n=50 from the simulation means",
        "<br><table style='width:100%'><tr><th>Skewness ", withMathJax("$(g_3)$"), "</th><th>",
        withMathJax("$p(\\gamma_{3}=0)$"), "</th><th>Kurtosis ", withMathJax("$(g_4)$"), "</th><th>",
        withMathJax("$p(\\gamma_{4}=0)$"), "</th></tr>",
        "<tr><td>", stats$g3, "</td><td>", stats$g3_p, "</td><td>", stats$g4, "</td><td>",
        stats$g4_p, "</td></tr></table>"
      ))
    })

    clt_plot_dl <- reactive({
      req(last_plot())
      last_plot()
    })

    plot_height <- reactive(400 * 4)
    plot_width <- reactive(400 * 4)
    downloadServer("clt_plot_dl", clt_plot_dl, height = plot_height, width = plot_width)
  })
}
