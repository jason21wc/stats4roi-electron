# Power & Sample Size Concept Simulator
# CC BY-NC-SA 4.0 Steven Ouellette

library(shiny)
library(ggplot2)

source("modules/config/global_config.R")

hyp_choices <- c(
  "H\u2081: \u03bc \u2260 0" = 1,
  "H\u2081: \u03bc < 0" = 2,
  "H\u2081: \u03bc > 0" = 3
)

create_power_simulator_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = ns("alpha"),
        label = "Type I error \u03b1",
        choices = c(0.1, 0.05, 0.01)
      ),
      numericInput(inputId = ns("pop_sd"), label = "Population \u03c3", value = 4),
      numericInput(inputId = ns("delta"), label = "Effect Size (\u0394)", value = 4),
      numericInput(inputId = ns("sample_n"), label = "Sample Size (n)", value = 9),
      selectInput(inputId = ns("hyp"), label = "Alternative Hypothesis", choices = hyp_choices),
      htmlOutput(ns("stand_err"))
    ),
    mainPanel(
      h3("How \u03b1, \u03c3, \u0394, and Sample Size Affect \u03b2"),
      plotOutput(ns("power_plot"), width = "100%", height = "700px"),
      downloadUI(ns("power_plot_dl"))
    )
  )
}

create_power_simulator_server <- function(id, color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    color <- reactive({
      color_palette()
    })

    output$stand_err <- renderUI({
      sd <- input$pop_sd
      n <- input$sample_n
      alpha <- as.numeric(input$alpha)
      delta <- input$delta
      hyp <- input$hyp
      req(sd, n, alpha, delta, hyp)

      sd_error <- ro(sd / sqrt(n), 3)

      alpha_rsd <- if (hyp == 1) alpha / 2 else alpha
      crit_u <- qnorm(p = alpha_rsd, mean = 0, sd = sd_error, lower.tail = FALSE)

      beta <- pnorm(q = crit_u, mean = delta, sd = sd_error, lower.tail = TRUE)
      if (hyp == 1) {
        beta <- beta - pnorm(q = -crit_u, mean = delta, sd = sd_error, lower.tail = TRUE)
      }
      beta <- ro(beta, 3)

      div(
        HTML(paste0(
          withMathJax(sprintf("$\\sigma_{\\overline{X}}=\\frac{\\sigma_{X}}{\\sqrt{n}}$")),
          "<br>", withMathJax(sprintf("$\\sigma_{\\overline{X}}=$")), sd_error,
          "<br>",
          withMathJax(sprintf("$\\beta=$")), beta, "<br> Power = ", 1 - beta
        )),
        style = "font-size: 30px; text-align: center;"
      )
    })

    power_plot <- reactive({
      sd <- input$pop_sd
      n <- input$sample_n
      alpha <- as.numeric(input$alpha)
      delta <- input$delta
      hyp <- input$hyp
      req(sd, n, alpha, delta, hyp)

      pal <- color()
      sd_error <- sd / sqrt(n)

      alpha_rsd <- if (hyp == 1) alpha / 2 else alpha

      xmin <- min(0 - 4 * sd, 0 - delta - 4 * (sd / sqrt(n)))
      xmax <- max(0 + 4 * sd, 0 + delta + 4 * (sd / sqrt(n)))

      crit_u <- qnorm(p = alpha_rsd, mean = 0, sd = sd_error, lower.tail = FALSE)
      crit_l <- qnorm(p = alpha_rsd, mean = 0, sd = sd_error, lower.tail = TRUE)

      slices <- 500
      x <- seq(from = xmin, to = xmax, length.out = slices)

      bhd <- data.frame(facet = 1, plot = "bhd", x = x, y = dnorm(x = x, mean = 0, sd = sd))
      annotate <- data.frame(facet = c(1, 2, 3), x = 0, y = 0, label = c("\u03bc", "", ""), vjust = 0, hjust = .5)

      null_rsd <- data.frame(facet = 2, plot = "null_base", x = x, y = dnorm(x = x, mean = 0, sd = sd_error))
      plot_data <- rbind(bhd, null_rsd)
      annotate <- rbind(annotate, data.frame(
        facet = 2, x = 0, y = .5 * dnorm(x = 0, mean = 0, sd = sd_error),
        label = "Confidence", vjust = 0, hjust = .5
      ))

      if (hyp == 1 || hyp == 3) {
        null_upper <- data.frame(facet = 2, plot = "null_upper", x = x, y = dnorm(x = x, mean = 0, sd = sd_error))
        null_upper$y[null_upper$x < crit_u] <- 0
        plot_data <- rbind(plot_data, null_upper)
        alpha_label <- if (hyp == 1) "\u03b1/2" else "\u03b1"
        annotate <- rbind(annotate, data.frame(
          facet = 2, x = crit_u, y = dnorm(x = crit_u, mean = 0, sd = sd_error),
          label = alpha_label, vjust = 0, hjust = -1
        ))
      }

      if (hyp == 1 || hyp == 2) {
        null_lower <- data.frame(facet = 2, plot = "null_lower", x = x, y = dnorm(x = x, mean = 0, sd = sd_error))
        null_lower$y[null_lower$x > crit_l] <- 0
        plot_data <- rbind(plot_data, null_lower)
        alpha_label <- if (hyp == 1) "\u03b1/2" else "\u03b1"
        annotate <- rbind(annotate, data.frame(
          facet = 2, x = crit_l, y = dnorm(x = crit_u, mean = 0, sd = sd_error),
          label = alpha_label, vjust = 0, hjust = 2
        ))
      }

      if (hyp == 1 || hyp == 3) {
        alt_rsd2 <- data.frame(facet = 3, plot = "alt2", x = x, y = dnorm(x = x, mean = 0 + delta, sd = sd_error))
        alt_rsd2_tail <- data.frame(facet = 3, plot = "alt2_tail", x = x, y = dnorm(x = x, mean = 0 + delta, sd = sd_error))
        alt_rsd2$y[alt_rsd2$x < crit_u] <- 0
        alt_rsd2_tail$y[alt_rsd2_tail$x > crit_u] <- 0
        plot_data <- rbind(plot_data, alt_rsd2, alt_rsd2_tail)
        annotate <- rbind(annotate, data.frame(
          facet = 3, x = crit_u, y = .5 * dnorm(x = crit_u, mean = delta, sd = sd_error),
          label = "\u03b2", vjust = 0, hjust = 2
        ))
        annotate <- rbind(annotate, data.frame(
          facet = 3, x = crit_u, y = .5 * dnorm(x = crit_u, mean = 0, sd = sd_error),
          label = "Power", vjust = 0, hjust = -1
        ))
      }

      if (hyp == 1 || hyp == 2) {
        alt_rsd1 <- data.frame(facet = 3, plot = "alt1", x = x, y = dnorm(x = x, mean = 0 - delta, sd = sd_error))
        alt_rsd1_tail <- data.frame(facet = 3, plot = "alt1_tail", x = x, y = dnorm(x = x, mean = 0 - delta, sd = sd_error))
        alt_rsd1$y[alt_rsd1$x > crit_l] <- 0
        alt_rsd1_tail$y[alt_rsd1_tail$x < crit_l] <- 0
        plot_data <- rbind(plot_data, alt_rsd1, alt_rsd1_tail)
        annotate <- rbind(annotate, data.frame(
          facet = 3, x = crit_l, y = .5 * dnorm(x = crit_l, mean = -delta, sd = sd_error),
          label = "\u03b2", vjust = 0, hjust = -2
        ))
        annotate <- rbind(annotate, data.frame(
          facet = 3, x = crit_l, y = .5 * dnorm(x = crit_u, mean = 0, sd = sd_error),
          label = "Power", vjust = 0, hjust = 2
        ))
      }

      plot_data$facet <- factor(plot_data$facet, levels = c(1, 2, 3),
                                labels = c("Population", "Null RSD", "Alternative RSD"))
      annotate$facet <- factor(annotate$facet, levels = c(1, 2, 3),
                                 labels = c("Population", "Null RSD", "Alternative RSD"))

      leg_names <- c(
        "bhd" = alpha(pal[4], .5),
        "null_base" = alpha(pal[5], .5),
        "null_lower" = pal[2],
        "null_upper" = pal[2],
        "alt2_tail" = alpha(pal[7], .5),
        "alt1_tail" = alpha(pal[7], .5),
        "alt1" = alpha(pal[3], .5),
        "alt2" = alpha(pal[3], .5)
      )

      p <- ggplot(data = plot_data, aes(x = x, y = y, fill = plot)) +
        facet_grid(rows = vars(facet), scales = "free_y") +
        geom_line() +
        geom_area(position = "identity") +
        theme(
          axis.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.5)),
          plot.title = element_text(size = rel(1.5)),
          strip.text = element_text(size = rel(1.5)),
          plot.caption = element_text(size = rel(1)),
          legend.position = "none"
        ) +
        scale_fill_manual(values = leg_names) +
        ylab("Density") +
        geom_text(
          data = annotate,
          mapping = aes(x = x, y = y, label = label, vjust = vjust, hjust = hjust),
          inherit.aes = FALSE,
          size = rel(5)
        )

      if (hyp == 1) {
        p <- p + labs(caption = paste0(
          "If H\u2080 is not true by at least ", delta,
          " the RSD will look like one of these or more extreme"
        ))
      } else {
        p <- p + labs(caption = paste0(
          "If H\u2080 is not true in the selected direction by at least ", delta,
          " the RSD will look like this or more extreme"
        ))
      }

      p
    })

    output$power_plot <- renderPlot({
      power_plot()
    })

    plot_height <- reactive(700 * 4)
    plot_width <- reactive(400 * 4)
    downloadServer("power_plot_dl", power_plot, height = plot_height, width = plot_width)
  })
}
