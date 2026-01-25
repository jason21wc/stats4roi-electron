# MSA Linearity & Bias server (placeholder)

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(lolcat)

create_msa_linearity_bias_server <- function(id, filtered_data, reactive_color_palette, continuous_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    button_state_c <- continuous_state$button_state_c
    ui_render_flags <- continuous_state$ui_render_flags
    msa_data_r <- continuous_state$msa_data_r
    msa_init <- continuous_state$msa_init
    app_num <- continuous_state$app_num
    UI1_d <- continuous_state$UI1_d
    UI2_d <- continuous_state$UI2_d
    UI3_d <- continuous_state$UI3_d
    UI4_d <- continuous_state$UI4_d
    sig_e <- continuous_state$sig_e

    color_palette <- reactive({
      pal <- reactive_color_palette()
      if (is.null(pal) || length(pal) < 4) {
        pal <- palette.colors(8)
      }
      pal
    })

    msa_bias_correction <- reactiveValues()

    reset_bias_correction <- function() {
      existing <- names(reactiveValuesToList(msa_bias_correction))
      for (name in existing) {
        msa_bias_correction[[name]] <- NULL
      }
    }

    msa_linearity_data <- eventReactive(list(button_state_c(), filtered_data()), {
      level <- input$msa_level
      if (level == 2) {
        return()
      }
      UI1 <- as.numeric(UI1_d())
      UI2 <- as.numeric(UI2_d())
      UI3 <- as.numeric(UI3_d())
      UI4 <- as.numeric(UI4_d())
      if (length(UI4) == 0 || UI3 == UI4) {
        return(HTML("Waiting"))
      }
      data <- filtered_data()
      TV_col <- as.numeric(input$msa_UI5)
      R <- input$deci_msa
      conf <- input$conf_msa
      req(level, UI2, UI3, UI4, data, TV_col, R, conf, sig_e(), data, msa_data_r())

      TV <- na.omit(cbind(data[TV_col], data[UI2]))
      names(TV) <- c("TV", "Part")
      TV <- TV[order(TV$Part), ]
      if (length(unique(msa_data_r()$Part)) != nrow(TV)) {
        return(list(
          error = TRUE,
          message = "Standards need to match each and only one Part",
          test_data = NULL,
          model_labels = NULL,
          bias_labels = NULL,
          correction_equation = NULL,
          yscale_max = NULL,
          yscale_range = NULL,
          TV_width = NULL
        ))
      }

      msa_data <- msa_data_r()[order(msa_data_r()$Appraiser, msa_data_r()$Trial, msa_data_r()$Part), ]
      test_data <- merge(x = msa_data, y = TV, by = "Part")
      test_data <- test_data[order(test_data$Appraiser, test_data$Trial, test_data$Part), ]
      test_data$Bias <- test_data$Measures - test_data$TV
      all_combos <- expand.grid(TV$TV, TV$TV)
      all_combos$diff <- abs(all_combos[1] - all_combos[2])
      TV_width <- min(all_combos$diff[which(all_combos$diff > 0), ])

      test_data$lower <- NULL
      test_data$upper <- NULL
      model_text <- NULL
      bias_text <- NULL
      test_data$fit <- NA
      test_data$lwr <- NA
      test_data$upr <- NA
      test_data$se_fit_l <- NA
      test_data$se_fit_u <- NA
      corr_eq <- c(rep(NA, app_num()))

      isolate(reset_bias_correction())

      for (i in unique(as.numeric(test_data$Appraiser))) {
        subdat <- test_data[which(as.numeric(test_data$Appraiser) == i), ]
        model <- lm(data = subdat, formula = Bias ~ TV)
        test <- cor.pearson.r.onesample(x = subdat$TV, y = subdat$Bias, null.hypothesis.rho = 0, conf.level = conf)
        pred <- predict(model, interval = "prediction", level = conf, se.fit = TRUE)
        ave_bias <- t.test.onesample(x = subdat$Bias, null.hypothesis.mean = 0, conf.level = conf)
        model_text[i] <- paste0(
          "Model ", i, ": Bias = ", ro(model[["coefficients"]][["(Intercept)"]], R), " + ",
          ro(model[["coefficients"]][["TV"]], R), " * True Value ",
          ": r = ", ro(test[["estimate"]][["sample.r"]], R),
          " : r² = ", ro(test[["estimate"]][["r.squared"]], R),
          " : p(r=0) = ", ro(test[["p.value"]], R)
        )
        bias_text[i] <- paste0(
          "Average bias = ", ro(ave_bias[["estimate"]][["sample.mean"]], R),
          " : ", conf * 100, "%CI = ", ro(ave_bias[["conf.int"]][1], R), " to ", ro(ave_bias[["conf.int"]][2], R),
          " : p(Bias=0) = ", ro(ave_bias[["p.value"]], R)
        )
        if (ave_bias[["p.value"]] <= (1 - conf)) {
          bias_text[i] <- paste0(bias_text[i], "*")
          msa_bias_correction[[paste0(unique(test_data$Appraiser)[i])]] <- c(
            ave_bias[["estimate"]][["sample.mean"]],
            ave_bias[["conf.int"]]
          )
        }
        if (test[["p.value"]] <= (1 - conf)) {
          model_text[i] <- paste0(model_text[i], "*")
          model_corr <- lm(data = subdat, formula = TV ~ Measures)
          msa_bias_correction[[paste0(unique(test_data$Appraiser)[i])]] <- model_corr
          corr_eq[i] <- paste0(
            "Correction Model ", i, ": True Value = ", ro(model_corr[["coefficients"]][["(Intercept)"]], R),
            " + ", ro(model_corr[["coefficients"]][["Measures"]], R), " * Measured Value"
          )
        }

        if (ave_bias[["p.value"]] > (1 - conf) && test[["p.value"]] > (1 - conf)) {
          sd <- ave_bias[["estimate"]][["sd"]]
          error <- sd * qnorm((1 - conf) / 2)
          msa_bias_correction[[paste0(unique(test_data$Appraiser)[i])]] <- c(0, error, -error)
        }
        test_data$fit[which(as.numeric(test_data$Appraiser) == i)] <- pred$fit[, 1]
        test_data$lwr[which(as.numeric(test_data$Appraiser) == i)] <- pred$fit[, 2]
        test_data$upr[which(as.numeric(test_data$Appraiser) == i)] <- pred$fit[, 3]
        test_data$se_fit_l[which(as.numeric(test_data$Appraiser) == i)] <- pred$fit[, 1] - pred$se.fit
        test_data$se_fit_u[which(as.numeric(test_data$Appraiser) == i)] <- pred$fit[, 1] + pred$se.fit
      }
      model_labels <- data.frame(Appraiser = unique(test_data$Appraiser), label = model_text)
      bias_labels <- data.frame(Appraiser = unique(test_data$Appraiser), label = bias_text)
      correction_equation <- data.frame(Appraiser = unique(test_data$Appraiser), label = corr_eq)
      msa_bias_correction$present <- TRUE

      yscale_max <- max(test_data$Bias)
      yscale_range <- max(test_data$Bias) - min(test_data$Bias)

      list(
        test_data = test_data,
        model_labels = model_labels,
        bias_labels = bias_labels,
        correction_equation = correction_equation,
        yscale_max = yscale_max,
        yscale_range = yscale_range,
        TV_width = TV_width
      )
    })

    msa_linearity <- reactive({
      data <- msa_linearity_data()
      req(data)

      if (!is.null(data$error) && data$error) {
        return(ggplot() +
          annotate("text", x = 0, y = 0, label = data$message) +
          theme_void())
      }

      if (input$msa_jitter_line) {
        jitter_width <- 0.1 * data$TV_width
      } else {
        jitter_width <- 0
      }

      pal <- color_palette()
      col_fill <- pal[3]
      col_fill_highlight <- pal[2]

      p <- ggplot(data = data$test_data, aes(x = TV, y = Bias)) +
        facet_grid(rows = vars(Appraiser)) +
        geom_hline(yintercept = 0) +
        geom_ribbon(aes(x = TV, ymin = se_fit_l, ymax = se_fit_u), fill = "grey80")
      if (input$msa_violin_line) {
        p <- p + geom_violin(aes(group = cut_width(TV, data$TV_width)), fill = adjustcolor(col = col_fill, alpha.f = 0.5), bw = "sj")
      }
      p <- p +
        geom_line(aes(x = TV, y = fit), linetype = "solid", color = pal[1]) +
        geom_line(aes(x = TV, y = lwr), linetype = "dashed", color = col_fill_highlight) +
        geom_line(aes(x = TV, y = upr), linetype = "dashed", color = col_fill_highlight) +
        geom_jitter(width = jitter_width) +
        stat_summary(fun = "mean", geom = "point", size = 4, color = pal[2]) +
        labs(title = "Bias Analysis: Measures - True Value", x = "True Value") +
        geom_label(data = data$model_labels, mapping = aes(x = -Inf, y = Inf, label = label), hjust = 0, vjust = "top") +
        geom_label(data = data$bias_labels, mapping = aes(x = -Inf, y = data$yscale_max - (0.05 * data$yscale_range), label = label), hjust = 0, vjust = "top") +
        geom_label(data = data$correction_equation, mapping = aes(x = -Inf, y = data$yscale_max, label = label), hjust = 0, vjust = "top") +
        annotate(geom = "text", x = Inf, y = 0, label = "Bias = 0", hjust = "right", vjust = "top")
      p
    })

    output$msa_linear_est <- renderDT({
      req(filtered_data())
      data <- msa_linearity_data()
      req(data)

      if (!is.null(data$error) && data$error) {
        return(NULL)
      }

      UI1 <- as.numeric(UI1_d())
      UI2 <- as.numeric(UI2_d())
      UI3 <- as.numeric(UI3_d())
      UI4 <- as.numeric(UI4_d())
      TV_col <- as.numeric(input$msa_UI5)
      as_measured <- input$msa_as_measured

      req(UI1, UI2, UI3, UI4, TV_col, as_measured)
      if (is.null(msa_bias_correction$present)) {
        return()
      }
      models <- reactiveValuesToList(msa_bias_correction)
      apps <- setdiff(names(models), "present")
      R <- input$deci_msa
      data <- filtered_data()
      req(as_measured, models, apps, data, UI1, UI2, UI3, UI4, TV_col)
      estimates <- data.frame(Estimate = rep(NA, length(apps)), Lower = NA, Upper = NA)
      rownames(estimates) <- apps
      for (app in apps) {
        if (is.list(models[[app]])) {
          indep_var <- all.vars(formula(models[[app]]))[[2]]
          new_data <- data.frame(setNames(list(input$msa_as_measured), indep_var))
          estimates[app, ] <- predict(models[[app]], interval = "prediction", level = input$conf_msa, new_data)
        } else {
          estimates[app, ] <- c(
            input$msa_as_measured - models[[app]][1],
            input$msa_as_measured - models[[app]][3],
            input$msa_as_measured - models[[app]][2]
          )
        }
      }
      estimates <- ro(estimates, R)
      datatable(estimates, caption = paste0("Lower and Upper Estimates based on ", input$conf_msa * 100, "% Confidence"), options = list(dom = "t"))
    })

    output$msa_linearity <- renderPlot({
      req(filtered_data())
      msa_linearity()
    })

    output$msa_linearity_2 <- renderUI({
      req(filtered_data())
      level <- input$msa_level
      if (level == 2) {
        return()
      }
      UI1 <- as.numeric(input$msa_UI1)
      UI2 <- as.numeric(input$msa_UI2)
      UI3 <- as.numeric(input$msa_UI3)
      UI4 <- as.numeric(input$msa_UI4)
      if (length(UI4) == 0 || UI3 == UI4) {
        return(HTML("Waiting"))
      }
      data <- msa_data_r()
      req(msa_init(), UI1, UI2, UI3, UI4, data, app_num() > 0)

      ui_render_flags$msa_linearity_2 <- TRUE
      plotOutput(ns("msa_linearity"), height = app_num() * 400)
    })

    linearityplot_height <- reactive(app_num() * 400 * 4)
    linearityplot_width <- reactive(400 * 8)
    downloadServer("linearityPlot", msa_linearity, height = linearityplot_height, width = linearityplot_width)
  })
}
