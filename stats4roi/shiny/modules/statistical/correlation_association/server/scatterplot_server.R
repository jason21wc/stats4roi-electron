# Scatterplot Worker Module
# Contains business logic for scatterplot generation and statistics
# No UI rendering - that happens in the coordinator

library(ggplot2)
# Shiny-compatible fork required; CRAN propagate fails in Shiny (predictNLS). https://github.com/ProfessorPeregrine/propagate
library(propagate)  # For predictNLS function

create_scatterplot_worker <- function(id, filtered_data, input_values, choice_corr_alt_text) {
  moduleServer(id, function(input, output, session) {
    
    # Get namespace function
    ns <- session$ns
    
    # =========================================================================
    # SCATTERPLOT DATA REACTIVE
    # =========================================================================
    scat_dat <- reactive({
      inputs_vals <- input_values()
      data <- filtered_data()
      req(inputs_vals, data)
      
      x_col <- inputs_vals$scat_x_sel
      y_col <- inputs_vals$scat_y_sel
      
      req(x_col, y_col)
      
      # Create data frame with selected columns
      x_dat <- data[, as.numeric(x_col)]
      y_dat <- data[, as.numeric(y_col)]
      
      scat_data <- data.frame(x = x_dat, y = y_dat)
      names(scat_data) <- c(names(data)[as.numeric(x_col)], names(data)[as.numeric(y_col)])
      scat_data <- na.omit(scat_data)
      
      scat_data
    })
    
    # =========================================================================
    # SCATTERPLOT PLOT REACTIVE
    # =========================================================================
    scatter_plot <- reactive({
      inputs_vals <- input_values()
      data <- filtered_data()
      req(inputs_vals, data)
      
      x_col <- inputs_vals$scat_x_sel
      y_col <- inputs_vals$scat_y_sel
      curve_fit <- inputs_vals$curve_fit
      conf <- inputs_vals$conf_scatter
      ci <- inputs_vals$scat_ci
      p_ci <- inputs_vals$point_ci
      y_x <- inputs_vals$y_x_line
      R <- inputs_vals$decimal_scat
      font_size <- inputs_vals$scat_font_size
      
      validate(need(!is.null(x_col) && !is.null(y_col), "Need to select x and y data vectors"))
      validate(need(!is.na(as.numeric(x_col)) && !is.na(as.numeric(y_col)), "Need to enter x and y data vectors"))
      
      x <- as.numeric(x_col)
      y <- as.numeric(y_col)
      
      # Get color palette and rounding function from global config
      global_config <- get_global_config()
      ro <- global_config$ro  # Rounding function
      
      # Get color palette (vector of 8 colors)
      color_palette <- get_color_palette()
      color <- unname(color_palette)  # Vector for color[1], color[2], etc.
      
      # Get specific color variables
      col_line_control_chart <- color[4]  # color[4] from original app
      col_fill_highlight <- color[2]      # color[2] from original app
      
      # Make names valid for ggplot
      names(data) <- make.names(names(data))
      
      # Filter to complete paired cases for x and y
      complete_mask <- complete.cases(data[[x]], data[[y]])
      validate(need(sum(complete_mask) >= 2, "Need at least 2 complete paired data points"))
      data <- data[complete_mask, ]
      
      x_dat <- data[[x]]
      y_dat <- data[[y]]
      
      steps <- 50  # Number of data points for prediction
      
      # Base plot (theme will be applied at the end)
      p <- ggplot(data, aes(x = .data[[names(data)[x]]], y = .data[[names(data)[y]]])) +
        geom_point(data = data, color = col_line_control_chart, size = 3)
      
      output <- NULL
      
      # Curve fit 0: None
      if (curve_fit == 0 || curve_fit == "") {
        output <- p
      }
      
      # Curve fit 1: Linear: y = A + Bx
      else if (curve_fit == 1) {
        req(!is.null(p_ci), !is.null(ci))
        model <- lm(formula = y_dat ~ x_dat)
        model_text <- paste0("Model: y = ", ro(model[["coefficients"]][["(Intercept)"]], R), " + ", ro(model[["coefficients"]][["x_dat"]], R), "x")
        p <- p + labs(title = model_text)
        
        if (ci && !p_ci) {
          output <- p + stat_smooth(method = lm, formula = y ~ x, level = conf, color = color[1]) +
            geom_point(color = color[4], size = 3)
        }
        if (!ci && !p_ci) {
          output <- p + stat_smooth(method = lm, formula = y ~ x, level = 0, color = color[1]) +
            geom_point(color = color[4], size = 3)
        }
        if (ci && p_ci) {
          model <- lm(y_dat ~ x_dat)
          newdata <- data.frame(x_dat = seq(from = min(x_dat, na.rm = TRUE), to = max(x_dat, na.rm = TRUE), length.out = steps))
          pred <- data.frame(newdata, predict(model, newdata = newdata, interval = "prediction", level = conf))
          output <- p + stat_smooth(method = lm, formula = y ~ x, level = conf, color = color[1]) +
            geom_line(data = pred, aes(x = x_dat, y = lwr), linetype = "dashed", color = col_fill_highlight) +
            geom_line(data = pred, aes(x = x_dat, y = upr), linetype = "dashed", color = col_fill_highlight) +
            geom_point(color = color[4], size = 3)
        }
        if (!ci && p_ci) {
          model <- lm(y_dat ~ x_dat)
          newdata <- data.frame(x_dat = seq(from = min(x_dat, na.rm = TRUE), to = max(x_dat, na.rm = TRUE), length.out = steps))
          pred <- data.frame(newdata, predict(model, newdata = newdata, interval = "prediction", level = conf))
          output <- p + stat_smooth(method = lm, formula = y ~ x, level = 0, color = color[1]) +
            geom_line(data = pred, aes(x = x_dat, y = lwr), linetype = "dashed", color = col_fill_highlight) +
            geom_line(data = pred, aes(x = x_dat, y = upr), linetype = "dashed", color = col_fill_highlight) +
            geom_point(color = color[4], size = 3)
        }
      }
      
      # Curve fit 2: Exponential: y = Ae(Bx)
      else if (curve_fit == 2) {
        req(!is.null(p_ci), !is.null(ci))
        form_1 <- as.formula(paste("log(", names(data)[y], ")~log(a)+b*", names(data)[x]))
        form_2 <- as.formula(paste(names(data)[y], "~a*exp(b*", names(data)[x], ")"))
        new_dat <- data.frame(seq(from = min(data[, x], na.rm = TRUE), to = max(data[, x], na.rm = TRUE), length.out = steps))
        names(new_dat) <- names(data)[x]
        
        fm0 <- nls(formula = form_1, start = list(a = 1, b = 1), data = data)
        nls_m <- nls(formula = form_2, start = coef(fm0), data = data)
        nls_m$call$formula <- form_2  # Module scope: predictNLS evals in package; store value so eval() gets formula not symbol
        A_const <- ro(environment(nls_m[["m"]][["formula"]])[["internalPars"]][1], R)
        B_const <- ro(environment(nls_m[["m"]][["formula"]])[["internalPars"]][2], R)
        p <- p + labs(title = as.expression(bquote("Model: y = " ~ .(A_const) ~ "e"^{.(B_const) ~ "x"})))
        pred_conf <- predictNLS(model = nls_m, interval = "confidence", newdata = new_dat, alpha = (1 - conf), do.sim = F)
        pred_conf_data <- data.frame(new_dat, pred_conf$summary)
        names(pred_conf_data)[6] <- "lwr"
        names(pred_conf_data)[7] <- "upr"
        names(pred_conf_data)[1] <- "x"
        
        pred_pred <- predictNLS(model = nls_m, interval = "prediction", newdata = new_dat, alpha = (1 - conf), do.sim = F)
        pred_pred_data <- data.frame(new_dat, pred_pred$summary)
        names(pred_pred_data)[6] <- "lwr"
        names(pred_pred_data)[7] <- "upr"
        names(pred_pred_data)[1] <- "x"
        
        if (!ci && !p_ci) {
          output <- p + stat_smooth(method = nls, formula = y ~ a * exp(b * x), method.args = list(start = coef(fm0)), color = color[1], se = F) +
            geom_point(color = color[4], size = 3)
        }
        if (ci && p_ci) {
          output <- p +
            geom_ribbon(data = pred_conf_data, aes(x = x, ymin = lwr, ymax = upr), fill = "grey80", inherit.aes = FALSE) +
            geom_line(data = pred_pred_data, aes(x = x, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x, y = upr), linetype = "dashed", color = color[2]) +
            stat_smooth(data = data, method = nls, formula = y ~ a * exp(b * x), method.args = list(start = coef(fm0)), color = color[1], se = F) +
            geom_point(color = color[4], size = 3)
        }
        if (!ci && p_ci) {
          output <- p +
            geom_line(data = pred_pred_data, aes(x = x, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x, y = upr), linetype = "dashed", color = color[2]) +
            stat_smooth(method = nls, formula = y ~ a * exp(b * x), method.args = list(start = coef(fm0)), color = color[1], se = F) +
            geom_point(color = color[4], size = 3)
        }
        if (ci && !p_ci) {
          output <- p +
            geom_ribbon(data = pred_conf_data, aes(x = x, ymin = lwr, ymax = upr), fill = "grey80", inherit.aes = FALSE) +
            stat_smooth(method = nls, formula = y ~ a * exp(b * x), method.args = list(start = coef(fm0)), color = color[1], se = F) +
            geom_point(color = color[4], size = 3)
        }
      }
      
      # Curve fit 3: Logarithmic: A + B ln x
      else if (curve_fit == 3) {
        req(!is.null(p_ci), !is.null(ci))
        form_2 <- as.formula(paste(names(data)[y], "~a+b*log(", names(data)[x], ")"))
        new_dat <- data.frame(seq(from = min(data[, x], na.rm = TRUE), to = max(data[, x], na.rm = TRUE), length.out = steps))
        names(new_dat) <- names(data)[x]
        
        nls_m <- nls(formula = form_2, start = list(a = 1, b = 1), data = data)
        nls_m$call$formula <- form_2  # Module scope: predictNLS evals in package; store value so eval() gets formula not symbol
        model_text <- paste0("y = ", ro(environment(nls_m[["m"]][["formula"]])[["internalPars"]][1], R), " + ", ro(environment(nls_m[["m"]][["formula"]])[["internalPars"]][2], R), " ln x")
        p <- p + labs(title = model_text)
        
        pred_conf <- predictNLS(model = nls_m, interval = "confidence", newdata = new_dat, alpha = (1 - conf), do.sim = F)
        pred_conf_data <- data.frame(new_dat, pred_conf$summary)
        names(pred_conf_data)[6] <- "lwr"
        names(pred_conf_data)[7] <- "upr"
        names(pred_conf_data)[1] <- "x"
        
        pred_pred <- predictNLS(model = nls_m, interval = "prediction", newdata = new_dat, alpha = (1 - conf), do.sim = F)
        pred_pred_data <- data.frame(new_dat, pred_pred$summary)
        names(pred_pred_data)[6] <- "lwr"
        names(pred_pred_data)[7] <- "upr"
        names(pred_pred_data)[1] <- "x"
        
        if (ci && !p_ci) {
          output <- p +
            stat_smooth(method = lm, formula = y ~ log(x), level = conf, color = color[1]) +
            geom_point(color = color[4], size = 3) +
            geom_point(color = color[4], size = 3)
        }
        if (ci && p_ci) {
          output <- p +
            stat_smooth(method = lm, formula = y ~ log(x), level = conf, color = color[1]) +
            geom_line(data = pred_pred_data, aes(x = x, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x, y = upr), linetype = "dashed", color = color[2]) +
            geom_point(color = color[4], size = 3)
        }
        if (!ci && p_ci) {
          output <- p +
            stat_smooth(method = lm, formula = y ~ log(x), se = F, color = color[1]) +
            geom_line(data = pred_pred_data, aes(x = x, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x, y = upr), linetype = "dashed", color = color[2]) +
            geom_point(color = color[4], size = 3)
        }
        if (!ci && !p_ci) {
          output <- p +
            stat_smooth(method = lm, formula = y ~ log(x), se = F, color = color[1]) +
            geom_point(color = color[4], size = 3)
        }
      }
      
      # Curve fit 4: Proportional: y = Ax
      else if (curve_fit == 4) {
        req(!is.null(p_ci), !is.null(ci))
        form_2 <- as.formula(paste(names(data)[y], "~a*(", names(data)[x], ")+0"))
        new_dat <- data.frame(seq(from = min(data[, x], na.rm = TRUE), to = max(data[, x], na.rm = TRUE), length.out = steps))
        names(new_dat) <- names(data)[x]
        
        nls_m <- nls(formula = form_2, start = list(a = 1), data = data)
        nls_m$call$formula <- form_2  # Module scope: predictNLS evals in package; store value so eval() gets formula not symbol
        model_text <- paste0("Model: y = ", ro(environment(nls_m[["m"]][["formula"]])[["internalPars"]][1], R), " x")
        p <- p + labs(title = model_text)
        
        pred_conf <- predictNLS(model = nls_m, interval = "confidence", newdata = new_dat, alpha = (1 - conf), do.sim = F)
        pred_conf_data <- data.frame(new_dat, pred_conf$summary)
        names(pred_conf_data)[6] <- "lwr"
        names(pred_conf_data)[7] <- "upr"
        names(pred_conf_data)[1] <- "x"
        
        pred_pred <- predictNLS(model = nls_m, interval = "prediction", newdata = new_dat, alpha = (1 - conf), do.sim = F)
        pred_pred_data <- data.frame(new_dat, pred_pred$summary)
        names(pred_pred_data)[6] <- "lwr"
        names(pred_pred_data)[7] <- "upr"
        names(pred_pred_data)[1] <- "x"
        
        if (ci && !p_ci) {
          output <- p + stat_smooth(method = lm, formula = y ~ x + 0, level = conf, color = color[1])
        }
        if (ci && p_ci) {
          output <- p +
            geom_ribbon(data = pred_conf_data, aes(x = x, ymin = lwr, ymax = upr), fill = "grey80", inherit.aes = FALSE) +
            geom_line(data = pred_pred_data, aes(x = x, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x, y = upr), linetype = "dashed", color = color[2]) +
            stat_smooth(method = lm, formula = y ~ x + 0, level = conf, color = color[1]) +
            geom_point(data = data, color = col_line_control_chart, size = 3)
        }
        if (!ci && p_ci) {
          output <- p +
            geom_line(data = pred_pred_data, aes(x = x, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x, y = upr), linetype = "dashed", color = color[2]) +
            stat_smooth(method = lm, formula = y ~ x + 0, se = F, color = color[1]) +
            geom_point(data = data, color = col_line_control_chart, size = 3)
        }
        if (!ci && !p_ci) {
          output <- p + stat_smooth(method = lm, formula = y ~ x + 0, se = F, color = color[1])
        }
      }
      
      # Curve fit 6: Inverse: y = A + B/x
      else if (curve_fit == 6) {
        req(!is.null(p_ci), !is.null(ci))
        form_2 <- as.formula(paste(names(data)[y], "~a+b/(", names(data)[x], ")"))
        new_dat <- data.frame(seq(from = min(data[, x], na.rm = TRUE), to = max(data[, x], na.rm = TRUE), length.out = steps))
        names(new_dat) <- names(data)[x]
        
        nls_m <- nls(formula = form_2, start = list(a = 1, b = 1), data = data)
        nls_m$call$formula <- form_2  # Module scope: predictNLS evals in package; store value so eval() gets formula not symbol
        A_const <- ro(environment(nls_m[["m"]][["formula"]])[["internalPars"]][1], R)
        B_const <- ro(environment(nls_m[["m"]][["formula"]])[["internalPars"]][2], R)
        p <- p + labs(title = as.expression(bquote("Model: y = " ~ .(A_const) ~ "+" ~ .(B_const) ~ "/x")))
        
        pred_conf <- predictNLS(model = nls_m, interval = "confidence", newdata = new_dat, alpha = (1 - conf), do.sim = F)
        pred_conf_data <- data.frame(new_dat, pred_conf$summary)
        names(pred_conf_data)[6] <- "lwr"
        names(pred_conf_data)[7] <- "upr"
        names(pred_conf_data)[1] <- "x"
        
        pred_pred <- predictNLS(model = nls_m, interval = "prediction", newdata = new_dat, alpha = (1 - conf), do.sim = F)
        pred_pred_data <- data.frame(new_dat, pred_pred$summary)
        names(pred_pred_data)[6] <- "lwr"
        names(pred_pred_data)[7] <- "upr"
        names(pred_pred_data)[1] <- "x"
        
        if (!ci && !p_ci) {
          output <- p + stat_smooth(method = nls, formula = y ~ a + (b/x), method.args = list(start = list(a = 1, b = 1)), color = color[1], se = F)
        }
        if (ci && p_ci) {
          output <- p +
            geom_ribbon(data = pred_conf_data, aes(x = x, ymin = lwr, ymax = upr), fill = "grey80", inherit.aes = FALSE) +
            geom_line(data = pred_pred_data, aes(x = x, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x, y = upr), linetype = "dashed", color = color[2]) +
            stat_smooth(method = nls, formula = y ~ a + (b/x), method.args = list(start = list(a = 1, b = 1)), color = color[1], se = F) +
            geom_point(color = color[4], size = 3)
        }
        if (!ci && p_ci) {
          output <- p +
            geom_line(data = pred_pred_data, aes(x = x, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x, y = upr), linetype = "dashed", color = color[2]) +
            stat_smooth(method = nls, formula = y ~ a + (b/x), method.args = list(start = list(a = 1, b = 1)), color = color[1], se = F) +
            geom_point(color = color[4], size = 3)
        }
        if (ci && !p_ci) {
          output <- p +
            geom_ribbon(data = pred_conf_data, aes(x = x, ymin = lwr, ymax = upr), fill = "grey80", inherit.aes = FALSE) +
            stat_smooth(method = nls, formula = y ~ a + (b/x), method.args = list(start = list(a = 1, b = 1)), color = color[1], se = F) +
            geom_point(color = color[4], size = 3)
        }
      }
      
      # Curve fit 7: Compound: y = AB^x
      else if (curve_fit == 7) {
        req(!is.null(p_ci), !is.null(ci))
        form_2 <- as.formula(paste(names(data)[y], "~a*b^(", names(data)[x], ")"))
        new_dat <- data.frame(seq(from = min(data[, x], na.rm = TRUE), to = max(data[, x], na.rm = TRUE), length.out = steps))
        names(new_dat) <- names(data)[x]
        nls_m <- nls(formula = form_2, start = list(a = 1, b = 1), data = data)
        nls_m$call$formula <- form_2  # Module scope: predictNLS evals in package; store value so eval() gets formula not symbol
        A_const <- ro(environment(nls_m[["m"]][["formula"]])[["internalPars"]][1], R)
        B_const <- ro(environment(nls_m[["m"]][["formula"]])[["internalPars"]][2], R)
        p <- p + labs(title = as.expression(bquote("Model: y = " ~ .(A_const) ~ " * " ~ .(B_const)^"x")))
        
        pred_conf <- predictNLS(model = nls_m, interval = "confidence", newdata = new_dat, alpha = (1 - conf), do.sim = F)
        pred_conf_data <- data.frame(new_dat, pred_conf$summary)
        names(pred_conf_data)[6] <- "lwr"
        names(pred_conf_data)[7] <- "upr"
        names(pred_conf_data)[1] <- "x"
        
        pred_pred <- predictNLS(model = nls_m, interval = "prediction", newdata = new_dat, alpha = (1 - conf), do.sim = F)
        pred_pred_data <- data.frame(new_dat, pred_pred$summary)
        names(pred_pred_data)[6] <- "lwr"
        names(pred_pred_data)[7] <- "upr"
        names(pred_pred_data)[1] <- "x"
        
        if (!ci && !p_ci) {
          output <- p + stat_smooth(method = nls, formula = y ~ a * b^x, method.args = list(start = list(a = 1, b = 1)), color = color[1], se = F)
        }
        if (ci && p_ci) {
          output <- p +
            geom_ribbon(data = pred_conf_data, aes(x = x, ymin = lwr, ymax = upr), fill = "grey80", inherit.aes = FALSE) +
            geom_line(data = pred_pred_data, aes(x = x, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x, y = upr), linetype = "dashed", color = color[2]) +
            stat_smooth(method = nls, formula = y ~ a * b^x, method.args = list(start = list(a = 1, b = 1)), color = color[1], se = F) +
            geom_point(color = color[4], size = 3)
        }
        if (!ci && p_ci) {
          output <- p +
            geom_line(data = pred_pred_data, aes(x = x, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x, y = upr), linetype = "dashed", color = color[2]) +
            stat_smooth(method = nls, formula = y ~ a * b^x, method.args = list(start = list(a = 1, b = 1)), color = color[1], se = F) +
            geom_point(color = color[4], size = 3)
        }
        if (ci && !p_ci) {
          output <- p +
            geom_ribbon(data = pred_conf_data, aes(x = x, ymin = lwr, ymax = upr), fill = "grey80", inherit.aes = FALSE) +
            stat_smooth(method = nls, formula = y ~ a * b^x, method.args = list(start = list(a = 1, b = 1)), color = color[1], se = F) +
            geom_point(color = color[4], size = 3)
        }
      }
      
      # Curve fit 8: Growth: y = e(A + Bx)
      else if (curve_fit == 8) {
        req(!is.null(p_ci), !is.null(ci))
        fm0 <- nls(formula = log(y_dat) ~ a + b * x_dat, start = list(a = 1, b = 1), data = data)
        form_2 <- as.formula(paste(names(data)[y], "~exp(a+b*", names(data)[x], ")"))
        new_dat <- data.frame(seq(from = min(data[, x], na.rm = TRUE), to = max(data[, x], na.rm = TRUE), length.out = steps))
        names(new_dat) <- names(data)[x]
        nls_m <- nls(formula = form_2, start = coef(fm0), data = data)
        nls_m$call$formula <- form_2  # Module scope: predictNLS evals in package; store value so eval() gets formula not symbol
        A_const <- ro(environment(nls_m[["m"]][["formula"]])[["internalPars"]][1], R)
        B_const <- ro(environment(nls_m[["m"]][["formula"]])[["internalPars"]][2], R)
        p <- p + labs(title = as.expression(bquote("Model: y = e"^{.(A_const) ~ " + " ~ .(B_const) ~ "x"})))
        
        pred_conf <- predictNLS(model = nls_m, interval = "confidence", newdata = new_dat, alpha = (1 - conf), do.sim = F)
        pred_conf_data <- data.frame(new_dat, pred_conf$summary)
        names(pred_conf_data)[6] <- "lwr"
        names(pred_conf_data)[7] <- "upr"
        names(pred_conf_data)[1] <- "x"
        
        pred_pred <- predictNLS(model = nls_m, interval = "prediction", newdata = new_dat, alpha = (1 - conf), do.sim = F)
        pred_pred_data <- data.frame(new_dat, pred_pred$summary)
        names(pred_pred_data)[6] <- "lwr"
        names(pred_pred_data)[7] <- "upr"
        names(pred_pred_data)[1] <- "x"
        
        if (!ci && !p_ci) {
          output <- p + stat_smooth(method = nls, formula = y ~ exp(a + b * x), method.args = list(start = coef(fm0)), color = color[1], se = F)
        }
        if (ci && p_ci) {
          output <- p +
            geom_ribbon(data = pred_conf_data, aes(x = x, ymin = lwr, ymax = upr), fill = "grey80", inherit.aes = FALSE) +
            geom_line(data = pred_pred_data, aes(x = x, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x, y = upr), linetype = "dashed", color = color[2]) +
            stat_smooth(method = nls, formula = y ~ exp(a + b * x), method.args = list(start = coef(fm0)), color = color[1], se = F) +
            geom_point(color = color[4], size = 3)
        }
        if (!ci && p_ci) {
          output <- p +
            geom_line(data = pred_pred_data, aes(x = x, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x, y = upr), linetype = "dashed", color = color[2]) +
            stat_smooth(method = nls, formula = y ~ exp(a + b * x), method.args = list(start = coef(fm0)), color = color[1], se = F) +
            geom_point(color = color[4], size = 3)
        }
        if (ci && !p_ci) {
          output <- p +
            geom_ribbon(data = pred_conf_data, aes(x = x, ymin = lwr, ymax = upr), fill = "grey80", inherit.aes = FALSE) +
            stat_smooth(method = nls, formula = y ~ exp(a + b * x), method.args = list(start = coef(fm0)), color = color[1], se = F) +
            geom_point(color = color[4], size = 3)
        }
      }
      
      # Curve fit 9: Loess: Locally Weighted Regression
      else if (curve_fit == 9) {
        output <- p + stat_smooth(method = "loess", formula = y ~ x, level = conf, color = color[1])
      }
      
      # Curve fit 10: Quadratic: y = A + Bx + Cx²
      else if (curve_fit == 10) {
        req(!is.null(p_ci), !is.null(ci))
        model <- lm(y_dat ~ x_dat + I(x_dat^2))
        new_dat <- data.frame(x_dat = seq(from = min(x_dat, na.rm = TRUE), to = max(x_dat, na.rm = TRUE), length.out = steps))
        
        pred_pred <- predict(model, interval = "prediction", newdata = new_dat, level = conf)
        pred_pred_data <- data.frame(new_dat, pred_pred)
        
        A_const <- ro(model[["coefficients"]][["(Intercept)"]], R)
        B_const <- ro(model[["coefficients"]][["x_dat"]], R)
        C_const <- ro(model[["coefficients"]][["I(x_dat^2)"]], R)
        p <- p + labs(title = as.expression(bquote("Model: y = " ~ .(A_const) ~ " + " ~ .(B_const) ~ "x + " ~ .(C_const) ~ "x"^2)))
        
        if (ci && !p_ci) {
          output <- p + stat_smooth(method = "lm", formula = y ~ x + I(x^2), level = conf, color = color[1])
        }
        if (!ci && !p_ci) {
          output <- p + stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = F, color = color[1])
        }
        if (ci && p_ci) {
          output <- p +
            stat_smooth(method = lm, formula = y ~ x + I(x^2), level = conf, color = color[1]) +
            geom_line(data = pred_pred_data, aes(x = x_dat, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x_dat, y = upr), linetype = "dashed", color = color[2]) +
            geom_point(color = color[4], size = 3)
        }
        if (!ci && p_ci) {
          output <- p +
            stat_smooth(method = lm, formula = y ~ x + I(x^2), se = F, color = color[1]) +
            geom_line(data = pred_pred_data, aes(x = x_dat, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x_dat, y = upr), linetype = "dashed", color = color[2]) +
            geom_point(color = color[4], size = 3)
        }
      }
      
      # Curve fit 11: Cubic: y = A + Bx + Cx² + Dx³
      else if (curve_fit == 11) {
        req(!is.null(p_ci), !is.null(ci))
        model <- lm(y_dat ~ x_dat + I(x_dat^2) + I(x_dat^3))
        new_dat <- data.frame(x_dat = seq(from = min(x_dat, na.rm = TRUE), to = max(x_dat, na.rm = TRUE), length.out = steps))
        
        pred_pred <- predict(model, interval = "prediction", newdata = new_dat, level = conf)
        pred_pred_data <- data.frame(new_dat, pred_pred)
        
        A_const <- ro(model[["coefficients"]][["(Intercept)"]], R)
        B_const <- ro(model[["coefficients"]][["x_dat"]], R)
        C_const <- ro(model[["coefficients"]][["I(x_dat^2)"]], R)
        D_const <- ro(model[["coefficients"]][["I(x_dat^3)"]], R)
        p <- p + labs(title = as.expression(bquote("Model: y = " ~ .(A_const) ~ " + " ~ .(B_const) ~ "x + " ~ .(C_const) ~ "x"^2 ~ " + " ~ .(D_const) ~ "x"^3)))
        
        if (ci && !p_ci) {
          output <- p + stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), level = conf, color = color[1])
        }
        if (!ci && !p_ci) {
          output <- p + stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), se = F, color = color[1])
        }
        if (ci && p_ci) {
          output <- p +
            stat_smooth(method = lm, formula = y ~ x + I(x^2) + I(x^3), level = conf, color = color[1]) +
            geom_line(data = pred_pred_data, aes(x = x_dat, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x_dat, y = upr), linetype = "dashed", color = color[2]) +
            geom_point(color = color[4], size = 3)
        }
        if (!ci && p_ci) {
          output <- p +
            stat_smooth(method = lm, formula = y ~ x + I(x^2) + I(x^3), se = F, color = color[1]) +
            geom_line(data = pred_pred_data, aes(x = x_dat, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x_dat, y = upr), linetype = "dashed", color = color[2]) +
            geom_point(color = color[4], size = 3)
        }
      }
      
      # Curve fit 12: 4th Order: y = A + Bx + Cx² + Dx³ + Ex^4
      else if (curve_fit == 12) {
        req(!is.null(p_ci), !is.null(ci))
        model <- lm(y_dat ~ x_dat + I(x_dat^2) + I(x_dat^3) + I(x_dat^4))
        new_dat <- data.frame(x_dat = seq(from = min(x_dat, na.rm = TRUE), to = max(x_dat, na.rm = TRUE), length.out = steps))
        
        pred_pred <- predict(model, interval = "prediction", newdata = new_dat, level = conf)
        pred_pred_data <- data.frame(new_dat, pred_pred)
        
        A_const <- ro(model[["coefficients"]][["(Intercept)"]], R)
        B_const <- ro(model[["coefficients"]][["x_dat"]], R)
        C_const <- ro(model[["coefficients"]][["I(x_dat^2)"]], R)
        D_const <- ro(model[["coefficients"]][["I(x_dat^3)"]], R)
        E_const <- ro(model[["coefficients"]][["I(x_dat^4)"]], R)
        p <- p + labs(title = as.expression(bquote("Model: y = " ~ .(A_const) ~ " + " ~ .(B_const) ~ "x + " ~ .(C_const) ~ "x"^2 ~ " + " ~ .(D_const) ~ "x"^3 ~ " + " ~ .(E_const) ~ "x"^4)))
        
        if (ci && !p_ci) {
          output <- p + stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4), level = conf, color = color[1])
        }
        if (!ci && !p_ci) {
          output <- p + stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4), se = F, color = color[1])
        }
        if (ci && p_ci) {
          output <- p +
            stat_smooth(method = lm, formula = y ~ x + I(x^2) + I(x^3) + I(x^4), level = conf, color = color[1]) +
            geom_line(data = pred_pred_data, aes(x = x_dat, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x_dat, y = upr), linetype = "dashed", color = color[2]) +
            geom_point(color = color[4], size = 3)
        }
        if (!ci && p_ci) {
          output <- p +
            stat_smooth(method = lm, formula = y ~ x + I(x^2) + I(x^3) + I(x^4), se = F, color = color[1]) +
            geom_line(data = pred_pred_data, aes(x = x_dat, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x_dat, y = upr), linetype = "dashed", color = color[2]) +
            geom_point(color = color[4], size = 3)
        }
      }
      
      # Curve fit 13: 5th Order: y = A + Bx + Cx² + Dx³ + Ex^4 + Fx^5
      else if (curve_fit == 13) {
        req(!is.null(p_ci), !is.null(ci))
        model <- lm(y_dat ~ x_dat + I(x_dat^2) + I(x_dat^3) + I(x_dat^4) + I(x_dat^5))
        new_dat <- data.frame(x_dat = seq(from = min(x_dat, na.rm = TRUE), to = max(x_dat, na.rm = TRUE), length.out = steps))
        
        pred_pred <- predict(model, interval = "prediction", newdata = new_dat, level = conf)
        pred_pred_data <- data.frame(new_dat, pred_pred)
        
        A_const <- ro(model[["coefficients"]][["(Intercept)"]], R)
        B_const <- ro(model[["coefficients"]][["x_dat"]], R)
        C_const <- ro(model[["coefficients"]][["I(x_dat^2)"]], R)
        D_const <- ro(model[["coefficients"]][["I(x_dat^3)"]], R)
        E_const <- ro(model[["coefficients"]][["I(x_dat^4)"]], R)
        F_const <- ro(model[["coefficients"]][["I(x_dat^5)"]], R)
        p <- p + labs(title = as.expression(bquote("Model: y = " ~ .(A_const) ~ " + " ~ .(B_const) ~ "x + " ~ .(C_const) ~ "x"^2 ~ " + " ~ .(D_const) ~ "x"^3 ~ " + " ~ .(E_const) ~ "x"^4 ~ " + " ~ .(F_const) ~ "x"^5)))
        
        if (ci && !p_ci) {
          output <- p + stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5), level = conf, color = color[1])
        }
        if (!ci && !p_ci) {
          output <- p + stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5), se = F, color = color[1])
        }
        if (ci && p_ci) {
          output <- p +
            stat_smooth(method = lm, formula = y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5), level = conf, color = color[1]) +
            geom_line(data = pred_pred_data, aes(x = x_dat, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x_dat, y = upr), linetype = "dashed", color = color[2]) +
            geom_point(color = color[4], size = 3)
        }
        if (!ci && p_ci) {
          output <- p +
            stat_smooth(method = lm, formula = y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5), se = F, color = color[1]) +
            geom_line(data = pred_pred_data, aes(x = x_dat, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x_dat, y = upr), linetype = "dashed", color = color[2]) +
            geom_point(color = color[4], size = 3)
        }
      }
      
      # Curve fit 14: S: y=e(A + B/x)
      else if (curve_fit == 14) {
        req(!is.null(p_ci), !is.null(ci))
        form_1 <- as.formula(paste("log(", names(data)[y], ")~a+b/", names(data)[x]))
        form_2 <- as.formula(paste(names(data)[y], "~exp(a+b/", names(data)[x], ")"))
        new_dat <- data.frame(seq(from = min(data[, x], na.rm = TRUE), to = max(data[, x], na.rm = TRUE), length.out = steps))
        names(new_dat) <- names(data)[x]
        
        fm0 <- nls(formula = form_1, start = list(a = 1, b = 1), data = data)
        nls_m <- nls(formula = form_2, start = coef(fm0), data = data)
        nls_m$call$formula <- form_2  # Module scope: predictNLS evals in package; store value so eval() gets formula not symbol
        A_const <- ro(environment(nls_m[["m"]][["formula"]])[["internalPars"]][1], R)
        B_const <- ro(environment(nls_m[["m"]][["formula"]])[["internalPars"]][2], R)
        p <- p + labs(title = as.expression(bquote("Model: y = e"^{.(A_const) ~ " + " ~ .(B_const) ~ "/x"})))
        
        pred_conf <- predictNLS(model = nls_m, interval = "confidence", newdata = new_dat, alpha = (1 - conf), do.sim = F)
        pred_conf_data <- data.frame(new_dat, pred_conf$summary)
        names(pred_conf_data)[6] <- "lwr"
        names(pred_conf_data)[7] <- "upr"
        names(pred_conf_data)[1] <- "x"
        
        pred_pred <- predictNLS(model = nls_m, interval = "prediction", newdata = new_dat, alpha = (1 - conf), do.sim = F)
        pred_pred_data <- data.frame(new_dat, pred_pred$summary)
        names(pred_pred_data)[6] <- "lwr"
        names(pred_pred_data)[7] <- "upr"
        names(pred_pred_data)[1] <- "x"
        
        if (!ci && !p_ci) {
          output <- p + stat_smooth(method = nls, formula = y ~ exp(a + (b/x)), method.args = list(start = coef(fm0)), color = color[1], se = F)
        }
        if (ci && p_ci) {
          output <- p +
            geom_ribbon(data = pred_conf_data, aes(x = x, ymin = lwr, ymax = upr), fill = "grey80", inherit.aes = FALSE) +
            geom_line(data = pred_pred_data, aes(x = x, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x, y = upr), linetype = "dashed", color = color[2]) +
            stat_smooth(method = nls, formula = y ~ exp(a + (b/x)), method.args = list(start = coef(fm0)), color = color[1], se = F) +
            geom_point(color = color[4], size = 3)
        }
        if (!ci && p_ci) {
          output <- p +
            geom_line(data = pred_pred_data, aes(x = x, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x, y = upr), linetype = "dashed", color = color[2]) +
            stat_smooth(method = nls, formula = y ~ exp(a + (b/x)), method.args = list(start = coef(fm0)), color = color[1], se = F) +
            geom_point(color = color[4], size = 3)
        }
        if (ci && !p_ci) {
          output <- p +
            geom_ribbon(data = pred_conf_data, aes(x = x, ymin = lwr, ymax = upr), fill = "grey80", inherit.aes = FALSE) +
            stat_smooth(method = nls, formula = y ~ exp(a + (b/x)), method.args = list(start = coef(fm0)), color = color[1], se = F) +
            geom_point(color = color[4], size = 3)
        }
      }
      
      # Curve fit 5: Power: y = Ax^B
      else if (curve_fit == 5) {
        req(!is.null(p_ci), !is.null(ci))
        form_1 <- as.formula(paste("log(", names(data)[y], ")~log(", (names(data)[x]), ")"))
        form_2 <- as.formula(paste(names(data)[y], "~a*", names(data)[x], "^b"))
        new_dat <- data.frame(seq(from = min(data[, x], na.rm = TRUE), to = max(data[, x], na.rm = TRUE), length.out = steps))
        names(new_dat) <- names(data)[x]
        
        fm0 <- lm(formula = form_1, data = data)
        nls_m <- nls(formula = form_2, start = list(a = exp(coef(fm0)[1]), b = coef(fm0)[2]), data = data)
        nls_m$call$formula <- form_2  # Module scope: predictNLS evals in package; store value so eval() gets formula not symbol
        A_const <- ro(environment(nls_m[["m"]][["formula"]])[["internalPars"]][1], R)
        B_const <- ro(environment(nls_m[["m"]][["formula"]])[["internalPars"]][2], R)
        p <- p + labs(title = as.expression(bquote("Model: y = " ~ .(A_const) ~ "x"^.(B_const))))
        
        pred_conf <- predictNLS(model = nls_m, interval = "confidence", newdata = new_dat, alpha = (1 - conf), do.sim = F)
        pred_conf_data <- data.frame(new_dat, pred_conf$summary)
        names(pred_conf_data)[6] <- "lwr"
        names(pred_conf_data)[7] <- "upr"
        names(pred_conf_data)[1] <- "x"
        
        pred_pred <- predictNLS(model = nls_m, interval = "prediction", newdata = new_dat, alpha = (1 - conf), do.sim = F)
        pred_pred_data <- data.frame(new_dat, pred_pred$summary)
        names(pred_pred_data)[6] <- "lwr"
        names(pred_pred_data)[7] <- "upr"
        names(pred_pred_data)[1] <- "x"
        
        if (!ci && !p_ci) {
          output <- p + stat_smooth(method = nls, formula = y ~ a * x^b, method.args = list(start = list(a = exp(coef(fm0)[1]), b = coef(fm0)[2])), color = color[1], se = F)
        }
        if (ci && p_ci) {
          output <- p +
            geom_ribbon(data = pred_conf_data, aes(x = x, ymin = lwr, ymax = upr), fill = "grey80", inherit.aes = FALSE) +
            geom_line(data = pred_pred_data, aes(x = x, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x, y = upr), linetype = "dashed", color = color[2]) +
            stat_smooth(method = nls, formula = y ~ a * x^b, method.args = list(start = list(a = exp(coef(fm0)[1]), b = coef(fm0)[2])), color = color[1], se = F) +
            geom_point(color = color[4], size = 3)
        }
        if (!ci && p_ci) {
          output <- p +
            geom_line(data = pred_pred_data, aes(x = x, y = lwr), linetype = "dashed", color = color[2]) +
            geom_line(data = pred_pred_data, aes(x = x, y = upr), linetype = "dashed", color = color[2]) +
            stat_smooth(method = nls, formula = y ~ a * x^b, method.args = list(start = list(a = exp(coef(fm0)[1]), b = coef(fm0)[2])), color = color[1], se = F) +
            geom_point(color = color[4], size = 3)
        }
        if (ci && !p_ci) {
          output <- p +
            geom_ribbon(data = pred_conf_data, aes(x = x, ymin = lwr, ymax = upr), fill = "grey80", inherit.aes = FALSE) +
            stat_smooth(method = nls, formula = y ~ a * x^b, method.args = list(start = list(a = exp(coef(fm0)[1]), b = coef(fm0)[2])), color = color[1], se = F) +
            geom_point(color = col_line_control_chart, size = 3)
        }
      }
      
      # Add theme with font size
      if (!is.null(output)) {
        output <- output + theme_gray(base_size = font_size)
      } else {
        output <- p + theme_gray(base_size = font_size)
      }
      
      # Add y = x line if requested (using x_dat for consistency with original)
      # Handle NULL case - default to FALSE if not set
      if (!is.null(y_x) && isTRUE(y_x)) {
        # Use geom_line with x_dat for both x and y (matches original app implementation)
        # This ensures the line is visible within the data range, unlike geom_abline which may be clipped
        output <- output + geom_line(aes(x = x_dat, y = x_dat), color = color[6], inherit.aes = FALSE)
      }
      
      output
    })
    
    # =========================================================================
    # SCATTERPLOT STATISTICS REACTIVE
    # =========================================================================
    scatter_plot_stats <- reactive({
      inputs_vals <- input_values()
      data <- filtered_data()
      req(inputs_vals, data)
      
      y_col <- inputs_vals$scat_y_sel
      x_col <- inputs_vals$scat_x_sel
      curve_fit <- inputs_vals$curve_fit
      conf <- inputs_vals$conf_scatter
      R <- inputs_vals$decimal_scat
      
      validate(need(!is.null(x_col) && !is.null(y_col), "Need to select x and y data vectors"))
      validate(need(!is.na(as.numeric(x_col)) && !is.na(as.numeric(y_col)), "Need to enter x and y data vectors"))
      
      x <- as.numeric(x_col)
      y <- as.numeric(y_col)
      
      # Filter to complete paired cases for x and y
      complete_mask <- complete.cases(data[, x], data[, y])
      validate(need(sum(complete_mask) >= 2, "Need at least 2 complete paired data points for statistics"))
      data <- data[complete_mask, ]
      
      corr_tests <- 1  # Always use test 1 (Pearson r one-sample) for scatterplot stats
      beta_statement <- "Power to reject the null if the observed difference was real = "
      
      # Get choice_corr_alt_text from parent scope (passed as parameter)
      choice_corr_alt_text_local <- choice_corr_alt_text
      
      output <- NULL
      
      # Linear (curve_fit == 1)
      if (curve_fit == 1) {
        stats <- lm(data[, y] ~ data[, x])
        results <- cor.pearson.r.onesample(x = data[, x], y = data[, y], null.hypothesis.rho = 0, conf.level = conf)
        
        output <- HTML(c(
          paste("<b>", results$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r = $"), ro(results$estimate[1], R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{0}=$"), ro(results$parameter, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r^2 = $"), ro(results$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), ro(results$estimate[3], R)), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho :$")), "</td>",
          "<td>", ro(results$conf.int[1], R), "</td>",
          "<td>", " to ", "</td>",
          "<td>", ro(results$conf.int[2], R), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_local[3 * (as.numeric(corr_tests) - 1) + 1], ": "), "</td>",
          "<td>", paste(if (results$parameter == 0) {"t = "} else {"z = "}, ro(results$statistic, R)), if (results$parameter == 0) {paste("df = ", results$estimate[2])} else {""}, "</td>",
          "<td>", paste("p = ", ro(results$p.value, R), if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          
          paste(beta_statement, 100 * ro(results$estimate[8], R), "%")
        ))
      }
      
      # Exponential (curve_fit == 2) - transformed data
      else if (curve_fit == 2) {
        y_trans <- log(data[, y])
        x_trans <- data[, x]
        stats <- lm(y_trans ~ x_trans)
        results <- cor.pearson.r.onesample(x = x_trans, y = y_trans, null.hypothesis.rho = 0, conf.level = conf)
        
        output <- HTML(c(
          paste("<b>", results$method, "</b>"), "</br>Transformed Data",
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r = $"), ro(results$estimate[1], R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{0}=$"), ro(results$parameter, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r^2 = $"), ro(results$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), ro(results$estimate[3], R)), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho :$")), "</td>",
          "<td>", ro(results$conf.int[1], R), "</td>",
          "<td>", " to ", "</td>",
          "<td>", ro(results$conf.int[2], R), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_local[3 * (as.numeric(corr_tests) - 1) + 1], ": "), "</td>",
          "<td>", paste(if (results$parameter == 0) {"t = "} else {"z = "}, ro(results$statistic, R)), if (results$parameter == 0) {paste("df = ", results$estimate[2])} else {""}, "</td>",
          "<td>", paste("p = ", ro(results$p.value, R), if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          
          paste(beta_statement, 100 * ro(results$estimate[8], R), "%")
        ))
      }
      
      # Logarithmic (curve_fit == 3) - transformed data
      else if (curve_fit == 3) {
        y_trans <- data[, y]
        x_trans <- log(data[, x])
        stats <- lm(y_trans ~ x_trans)
        results <- cor.pearson.r.onesample(x = x_trans, y = y_trans, null.hypothesis.rho = 0, conf.level = conf)
        
        output <- HTML(c(
          paste("<b>", results$method, "</b>"), "</br>Transformed Data",
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r = $"), ro(results$estimate[1], R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{0}=$"), ro(results$parameter, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r^2 = $"), ro(results$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), ro(results$estimate[3], R)), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho :$")), "</td>",
          "<td>", ro(results$conf.int[1], R), "</td>",
          "<td>", " to ", "</td>",
          "<td>", ro(results$conf.int[2], R), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_local[3 * (as.numeric(corr_tests) - 1) + 1], ": "), "</td>",
          "<td>", paste(if (results$parameter == 0) {"t = "} else {"z = "}, ro(results$statistic, R)), if (results$parameter == 0) {paste("df = ", results$estimate[2])} else {""}, "</td>",
          "<td>", paste("p = ", ro(results$p.value, R), if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          
          paste(beta_statement, 100 * ro(results$estimate[8], R), "%")
        ))
      }
      
      # Proportional (curve_fit == 4)
      else if (curve_fit == 4) {
        y_trans <- data[, y]
        x_trans <- data[, x]
        stats <- lm(y_trans ~ x_trans + 0)
        results <- cor.pearson.r.onesample(x = x_trans, y = y_trans, null.hypothesis.rho = 0, conf.level = conf)
        
        output <- HTML(c(
          paste("<b>", results$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r = $"), ro(results$estimate[1], R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{0}=$"), ro(results$parameter, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r^2 = $"), ro(results$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), ro(results$estimate[3], R)), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho :$")), "</td>",
          "<td>", ro(results$conf.int[1], R), "</td>",
          "<td>", " to ", "</td>",
          "<td>", ro(results$conf.int[2], R), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_local[3 * (as.numeric(corr_tests) - 1) + 1], ": "), "</td>",
          "<td>", paste(if (results$parameter == 0) {"t = "} else {"z = "}, ro(results$statistic, R)), if (results$parameter == 0) {paste("df = ", results$estimate[2])} else {""}, "</td>",
          "<td>", paste("p = ", ro(results$p.value, R), if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          
          paste(beta_statement, 100 * ro(results$estimate[8], R), "%")
        ))
      }
      
      # Power (curve_fit == 5) - transformed data
      else if (curve_fit == 5) {
        y_trans <- log(data[, y])
        x_trans <- log(data[, x])
        stats <- lm(y_trans ~ x_trans)
        results <- cor.pearson.r.onesample(x = x_trans, y = y_trans, null.hypothesis.rho = 0, conf.level = conf)
        
        output <- HTML(c(
          paste("<b>", results$method, "</b>"), "</br>Transformed Data",
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r = $"), ro(results$estimate[1], R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{0}=$"), ro(results$parameter, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r^2 = $"), ro(results$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), ro(results$estimate[3], R)), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho :$")), "</td>",
          "<td>", ro(results$conf.int[1], R), "</td>",
          "<td>", " to ", "</td>",
          "<td>", ro(results$conf.int[2], R), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_local[3 * (as.numeric(corr_tests) - 1) + 1], ": "), "</td>",
          "<td>", paste(if (results$parameter == 0) {"t = "} else {"z = "}, ro(results$statistic, R)), if (results$parameter == 0) {paste("df = ", results$estimate[2])} else {""}, "</td>",
          "<td>", paste("p = ", ro(results$p.value, R), if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          
          paste(beta_statement, 100 * ro(results$estimate[8], R), "%")
        ))
      }
      
      # Inverse (curve_fit == 6) - transformed data
      else if (curve_fit == 6) {
        y_trans <- data[, y]
        x_trans <- 1 / data[, x]
        stats <- nls(formula = y_trans ~ a + (b / data[, x]), start = list(a = 1, b = 1))
        results <- cor.pearson.r.onesample(x = x_trans, y = y_trans, null.hypothesis.rho = 0, conf.level = conf)
        
        output <- HTML(c(
          paste("<b>", results$method, "</b>"), "</br>Transformed Data",
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r = $"), ro(results$estimate[1], R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{0}=$"), ro(results$parameter, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r^2 = $"), ro(results$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), ro(results$estimate[3], R)), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho :$")), "</td>",
          "<td>", ro(results$conf.int[1], R), "</td>",
          "<td>", " to ", "</td>",
          "<td>", ro(results$conf.int[2], R), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_local[3 * (as.numeric(corr_tests) - 1) + 1], ": "), "</td>",
          "<td>", paste(if (results$parameter == 0) {"t = "} else {"z = "}, ro(results$statistic, R)), if (results$parameter == 0) {paste("df = ", results$estimate[2])} else {""}, "</td>",
          "<td>", paste("p = ", ro(results$p.value, R), if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          
          paste(beta_statement, 100 * ro(results$estimate[8], R), "%")
        ))
      }
      
      # Compound (curve_fit == 7) - transformed data
      else if (curve_fit == 7) {
        y_trans <- log(data[, y])
        x_trans <- data[, x]
        stats <- lm(y_trans ~ x_trans)
        results <- cor.pearson.r.onesample(x = x_trans, y = log(y_trans), null.hypothesis.rho = 0, conf.level = conf)
        
        output <- HTML(c(
          paste("<b>", results$method, "</b>"), "</br>Transformed Data",
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r = $"), ro(results$estimate[1], R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{0}=$"), ro(results$parameter, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r^2 = $"), ro(results$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), ro(results$estimate[3], R)), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho :$")), "</td>",
          "<td>", ro(results$conf.int[1], R), "</td>",
          "<td>", " to ", "</td>",
          "<td>", ro(results$conf.int[2], R), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_local[3 * (as.numeric(corr_tests) - 1) + 1], ": "), "</td>",
          "<td>", paste(if (results$parameter == 0) {"t = "} else {"z = "}, ro(results$statistic, R)), if (results$parameter == 0) {paste("df = ", results$estimate[2])} else {""}, "</td>",
          "<td>", paste("p = ", ro(results$p.value, R), if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          
          paste(beta_statement, 100 * ro(results$estimate[8], R), "%")
        ))
      }
      
      # Growth (curve_fit == 8) - transformed data
      else if (curve_fit == 8) {
        y_trans <- log(data[, y])
        x_trans <- data[, x]
        stats <- lm(y_trans ~ x_trans)
        results <- cor.pearson.r.onesample(x = x_trans, y = log(y_trans), null.hypothesis.rho = 0, conf.level = conf)
        
        output <- HTML(c(
          paste("<b>", results$method, "</b>"), "</br>Transformed Data",
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r = $"), ro(results$estimate[1], R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{0}=$"), ro(results$parameter, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r^2 = $"), ro(results$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), ro(results$estimate[3], R)), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho :$")), "</td>",
          "<td>", ro(results$conf.int[1], R), "</td>",
          "<td>", " to ", "</td>",
          "<td>", ro(results$conf.int[2], R), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_local[3 * (as.numeric(corr_tests) - 1) + 1], ": "), "</td>",
          "<td>", paste(if (results$parameter == 0) {"t = "} else {"z = "}, ro(results$statistic, R)), if (results$parameter == 0) {paste("df = ", results$estimate[2])} else {""}, "</td>",
          "<td>", paste("p = ", ro(results$p.value, R), if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          
          paste(beta_statement, 100 * ro(results$estimate[8], R), "%")
        ))
      }
      
      # Loess (curve_fit == 9)
      else if (curve_fit == 9) {
        output <- HTML("Loess is only used for exploratory analysis to identify patterns. No r<sup>2</sup> should be calculated.")
      }
      
      # Quadratic (curve_fit == 10) - transformed data
      else if (curve_fit == 10) {
        y_trans <- data[, y]
        x_trans <- data[, x]
        stats <- lm(y_trans ~ x_trans + I(x_trans^2))
        r_sq <- as.numeric(summary(stats)[8])
        n_samp <- length(unlist(summary(stats)[3]))
        results <- cor.pearson.r.onesample.simple(sample.r = (r_sq)^.5, sample.size = n_samp, null.hypothesis.rho = 0, conf.level = conf)
        
        output <- HTML(c(
          paste("<b>", results$method, "</b>"), "</br>Transformed Data",
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r = $"), ro(results$estimate[1], R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{0}=$"), ro(results$parameter, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r^2 = $"), ro(results$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), ro(results$estimate[3], R)), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho :$")), "</td>",
          "<td>", ro(results$conf.int[1], R), "</td>",
          "<td>", " to ", "</td>",
          "<td>", ro(results$conf.int[2], R), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_local[3 * (as.numeric(corr_tests) - 1) + 1], ": "), "</td>",
          "<td>", paste(if (results$parameter == 0) {"t = "} else {"z = "}, ro(results$statistic, R)), if (results$parameter == 0) {paste("df = ", results$estimate[2])} else {""}, "</td>",
          "<td>", paste("p = ", ro(results$p.value, R), if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          
          paste(beta_statement, 100 * ro(results$estimate[8], R), "%")
        ))
      }
      
      # Cubic (curve_fit == 11) - transformed data
      else if (curve_fit == 11) {
        y_trans <- data[, y]
        x_trans <- data[, x]
        stats <- lm(y_trans ~ x_trans + I(x_trans^2) + I(x_trans^3))
        r_sq <- as.numeric(summary(stats)[8])
        n_samp <- length(unlist(summary(stats)[3]))
        results <- cor.pearson.r.onesample.simple(sample.r = (r_sq)^.5, sample.size = n_samp, null.hypothesis.rho = 0, conf.level = conf)
        
        output <- HTML(c(
          paste("<b>", results$method, "</b>"), "</br>Transformed Data",
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r = $"), ro(results$estimate[1], R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{0}=$"), ro(results$parameter, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r^2 = $"), ro(results$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), ro(results$estimate[3], R)), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho :$")), "</td>",
          "<td>", ro(results$conf.int[1], R), "</td>",
          "<td>", " to ", "</td>",
          "<td>", ro(results$conf.int[2], R), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_local[3 * (as.numeric(corr_tests) - 1) + 1], ": "), "</td>",
          "<td>", paste(if (results$parameter == 0) {"t = "} else {"z = "}, ro(results$statistic, R)), if (results$parameter == 0) {paste("df = ", results$estimate[2])} else {""}, "</td>",
          "<td>", paste("p = ", ro(results$p.value, R), if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          
          paste(beta_statement, 100 * ro(results$estimate[8], R), "%")
        ))
      }
      
      # 4th Order (curve_fit == 12) - transformed data
      else if (curve_fit == 12) {
        y_trans <- data[, y]
        x_trans <- data[, x]
        stats <- lm(y_trans ~ x_trans + I(x_trans^2) + I(x_trans^3) + I(x_trans^4))
        r_sq <- as.numeric(summary(stats)[8])
        n_samp <- length(unlist(summary(stats)[3]))
        results <- cor.pearson.r.onesample.simple(sample.r = (r_sq)^.5, sample.size = n_samp, null.hypothesis.rho = 0, conf.level = conf)
        
        output <- HTML(c(
          paste("<b>", results$method, "</b>"), "</br>Transformed Data",
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r = $"), ro(results$estimate[1], R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{0}=$"), ro(results$parameter, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r^2 = $"), ro(results$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), ro(results$estimate[3], R)), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho :$")), "</td>",
          "<td>", ro(results$conf.int[1], R), "</td>",
          "<td>", " to ", "</td>",
          "<td>", ro(results$conf.int[2], R), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_local[3 * (as.numeric(corr_tests) - 1) + 1], ": "), "</td>",
          "<td>", paste(if (results$parameter == 0) {"t = "} else {"z = "}, ro(results$statistic, R)), if (results$parameter == 0) {paste("df = ", results$estimate[2])} else {""}, "</td>",
          "<td>", paste("p = ", ro(results$p.value, R), if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          
          paste(beta_statement, 100 * ro(results$estimate[8], R), "%")
        ))
      }
      
      # 5th Order (curve_fit == 13) - transformed data
      else if (curve_fit == 13) {
        y_trans <- data[, y]
        x_trans <- data[, x]
        stats <- lm(y_trans ~ x_trans + I(x_trans^2) + I(x_trans^3) + I(x_trans^4) + I(x_trans^5))
        r_sq <- as.numeric(summary(stats)[8])
        n_samp <- length(unlist(summary(stats)[3]))
        results <- cor.pearson.r.onesample.simple(sample.r = (r_sq)^.5, sample.size = n_samp, null.hypothesis.rho = 0, conf.level = conf)
        
        output <- HTML(c(
          paste("<b>", results$method, "</b>"), "</br>Transformed Data",
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r = $"), ro(results$estimate[1], R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{0}=$"), ro(results$parameter, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r^2 = $"), ro(results$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), ro(results$estimate[3], R)), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho :$")), "</td>",
          "<td>", ro(results$conf.int[1], R), "</td>",
          "<td>", " to ", "</td>",
          "<td>", ro(results$conf.int[2], R), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_local[3 * (as.numeric(corr_tests) - 1) + 1], ": "), "</td>",
          "<td>", paste(if (results$parameter == 0) {"t = "} else {"z = "}, ro(results$statistic, R)), if (results$parameter == 0) {paste("df = ", results$estimate[2])} else {""}, "</td>",
          "<td>", paste("p = ", ro(results$p.value, R), if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          
          paste(beta_statement, 100 * ro(results$estimate[8], R), "%")
        ))
      }
      
      # S (curve_fit == 14) - transformed data
      else if (curve_fit == 14) {
        y_trans <- data[, y]
        x_trans <- data[, x]
        fm0 <- nls(formula = log(y_trans) ~ a + (b/x_trans), start = list(a = 1, b = 1))
        stats <- nls(formula = y_trans ~ exp(a + (b/x_trans)), start = coef(fm0))
        
        results <- cor.pearson.r.onesample(x = (1/x_trans), y = log(y_trans), null.hypothesis.rho = 0, conf.level = conf)
        
        output <- HTML(c(
          paste("<b>", results$method, "</b>"), "</br>Transformed Data",
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r = $"), ro(results$estimate[1], R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{0}=$"), ro(results$parameter, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r^2 = $"), ro(results$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), ro(results$estimate[3], R)), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho :$")), "</td>",
          "<td>", ro(results$conf.int[1], R), "</td>",
          "<td>", " to ", "</td>",
          "<td>", ro(results$conf.int[2], R), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_local[3 * (as.numeric(corr_tests) - 1) + 1], ": "), "</td>",
          "<td>", paste(if (results$parameter == 0) {"t = "} else {"z = "}, ro(results$statistic, R)), if (results$parameter == 0) {paste("df = ", results$estimate[2])} else {""}, "</td>",
          "<td>", paste("p = ", ro(results$p.value, R), if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          
          paste(beta_statement, 100 * ro(results$estimate[8], R), "%")
        ))
      }
      
      # No curve fit (curve_fit == 0) - still show correlation
      else if (curve_fit == 0 || curve_fit == "") {
        results <- cor.pearson.r.onesample(x = data[, x], y = data[, y], null.hypothesis.rho = 0, conf.level = conf)
        
        output <- HTML(c(
          paste("<b>", results$method, "</b>"),
          "<br><br>",
          "<table>",
          "<tr>",
          "<td>", paste(withMathJax("$r = $"), ro(results$estimate[1], R)), "</td>",
          "<td>", "</td>",
          "<td>", paste(withMathJax("$\\rho_{0}=$"), ro(results$parameter, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$r^2 = $"), ro(results$estimate[1]^2, R)), "</td>",
          "</tr>",
          "<tr>",
          "<td>", paste(withMathJax("$n = $"), ro(results$estimate[3], R)), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste(conf * 100, "% confidence interval for"), "</td>",
          "<td>", paste(withMathJax("$\\rho :$")), "</td>",
          "<td>", ro(results$conf.int[1], R), "</td>",
          "<td>", " to ", "</td>",
          "<td>", ro(results$conf.int[2], R), "</td>",
          "</tr>",
          "</table>",
          
          "<table>",
          "<tr>",
          "<td>", paste("Test for ", choice_corr_alt_text_local[3 * (as.numeric(corr_tests) - 1) + 1], ": "), "</td>",
          "<td>", paste(if (results$parameter == 0) {"t = "} else {"z = "}, ro(results$statistic, R)), if (results$parameter == 0) {paste("df = ", results$estimate[2])} else {""}, "</td>",
          "<td>", paste("p = ", ro(results$p.value, R), if (results$p.value < 1 - conf) {"*"}), "</td>",
          "</tr>",
          "</table>",
          
          paste(beta_statement, 100 * ro(results$estimate[8], R), "%")
        ))
      }
      
      output
    })
    
    # =========================================================================
    # RETURN REACTIVE FUNCTIONS
    # =========================================================================
    list(
      scat_dat = scat_dat,
      scatter_plot = scatter_plot,
      scatter_plot_stats = scatter_plot_stats
    )
  })
}
