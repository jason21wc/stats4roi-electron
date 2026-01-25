# Oneway Post-hoc Worker Module
# Contains business logic for oneway ANOVA post-hoc calculations and plots

library(shiny)
library(lolcat)
library(DT)
library(ggplot2)
library(dplyr)
library(stringr)

# Source global systems
source("modules/config/global_config.R")

create_oneway_posthoc_worker <- function(id, filtered_data, input_values, reactive_color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Get colors reactively - need full palette for random effects plot
    color_palette <- reactive({
      reactive_color_palette()
    })
    
    colors <- reactive({
      get_distribution_colors(color_palette())
    })
    
    # Extract inputs from coordinator
    inputs <- reactive({
      input_values()
    })
    
    # Note: UI rendering is done in coordinator, not worker
    # Workers should only return reactive functions with business logic
    
    # Post-hoc table output
    posthoc_table <- reactive({
      inputs_vals <- inputs()
      data <- filtered_data()
      req(data, inputs_vals)
      
      conf <- inputs_vals$conf_ow
      R <- inputs_vals$decimal_ow_ph
      type <- inputs_vals$type_ow
      ph_type <- as.numeric(inputs_vals$ow_ph_type)
      
      req(type, ph_type)
      
      # Make names valid
      names(data) <- make.names(names(data))
      
      data_col <- as.numeric(inputs_vals$ow_data)
      factor_ow <- as.numeric(inputs_vals$ow_factor)
      req(data_col, factor_ow)
      
      form <- as.formula(paste(names(data)[data_col], " ~ ", "as.factor(", names(data)[factor_ow], ")"))
      oneway <- aov(formula = form, data = data)
      sum_aov <- summary(oneway)
      
      sse <- sum_aov[[1]][["Sum Sq"]][1]
      ssw <- sum_aov[[1]][["Sum Sq"]][2]
      dfe <- sum_aov[[1]][["Df"]][1]
      dfw <- sum_aov[[1]][["Df"]][2]
      mse <- sum_aov[[1]][["Mean Sq"]][1]
      msw <- sum_aov[[1]][["Mean Sq"]][2]
      sst <- sse + ssw
      dft <- dfe + dfw
      
      sum_stats <- as.data.frame(as.matrix(aggregate(data[data_col], by = data[factor_ow], FUN = function(x) c(mean(x), n = length(x), var = var(x)))))
      cell_means <- as.numeric(as.vector(as.matrix(sum_stats[paste(names(data)[data_col], ".", sep = "")])))
      cell_n <- as.numeric(as.vector(as.matrix(sum_stats[paste(names(data)[data_col], ".n", sep = "")])))
      cell_var <- as.numeric(as.vector(as.matrix(sum_stats[paste(names(data)[data_col], ".var", sep = "")])))
      
      if (type == 1 || type == 4) {  # fixed
        req(ph_type)
        if (ph_type == 1) {
          ph_test_results <- contrasts.tukey.kgroups.simple(
            group.label = sum_stats[[1]],
            group.mean = cell_means,
            group.sample.size = cell_n,
            conf.level.familywise = conf,
            mean.squared.error = msw,
            df.mean.squared.error = dfw
          )
          output <- ph_test_results[["matrix.decision"]]
          # Round data before creating DT object
          output <- ro(output, R)
          output <- DT::datatable(output,
            caption = "Tukey's HSD",
            options = list(
              columnDefs = list(list(className = "dt-center", targets = "_all")),
              dom = "t",
              paging = FALSE
            ),
            class = "cell-border stripe"
          )
        }
        
        if (ph_type == 2) {
          ph_test_results <- contrasts.games.howell.kgroups.simple(
            group.label = sum_stats[[1]],
            group.mean = cell_means,
            group.variance = cell_var,
            group.sample.size = as.vector(as.matrix(cell_n)),
            conf.level.familywise = conf,
            mean.squared.error = NA
          )  # PHAST version
          output <- ph_test_results[["matrix.decision"]]
          # Round data before creating DT object
          output <- ro(output, R)
          output <- DT::datatable(output,
            caption = "Games & Howell",
            options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), dom = "t", paging = FALSE),
            class = "cell-border stripe"
          )
        }
      }  # end fixed
      
      if (type == 2) {  # random
        table_aov <- as.data.frame(table(data[factor_ow]))
        table_aov <- cbind(table_aov, table_aov[2]^2)
        J <- nrow(table_aov)
        sum_n <- colSums(table_aov[2])
        sum_nsq <- colSums(table_aov[3])
        K_prime <- (1 / (J - 1)) * (sum_n - (sum_nsq / sum_n))
        
        bcv <- (mse - msw) / K_prime
        bcv <- max(0, bcv)
        ICC <- 100 * bcv / (bcv + msw)
        
        Fu <- qf(p = (1 - conf) / 2, df1 = dfe, df2 = dfw, lower.tail = FALSE)
        Fl <- qf(p = (1 - conf) / 2, df1 = dfe, df2 = dfw, lower.tail = TRUE)
        thetal <- (1 / K_prime) * ((mse / (msw * Fu) - 1))
        thetau <- (1 / K_prime) * ((mse / (msw * Fl) - 1))
        
        lci <- 100 * (thetal / (1 + thetal))
        uci <- 100 * (thetau / (1 + thetau))
        
        title <- "Random Effect Importance"
        first_col <- c("Treatment Variance", "Within Variance", "Total Variance", "Interclass Correlation", paste("ICC ", 100 * conf, "% Confidence Interval"))
        second_col <- as.vector(c(ro(bcv, R), ro(msw, R), ro(bcv + msw, R), paste(ro(ICC, R), "%"), paste(ro(lci, R), "%")))
        third_col <- c("", "", "", "", paste(ro(uci, R), "%"))
        
        output <- cbind(" " = first_col, " " = second_col, " " = third_col)
        row.names(output) <- NULL
        # Round data before creating DT object (data is already rounded in second_col/third_col, but ensure output is data frame)
        output <- as.data.frame(output)
        output <- DT::datatable(output,
          caption = title,
          options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), dom = "t", paging = FALSE),
          class = "cell-border stripe"
        )
      }  # end random
      
      if (type == 3) {  # all pair-wise MWU
        combos <- factorial(dfe + 1) / (factorial(2) * factorial(dfe - 1))
        table_aov <- as.data.frame(table(data[factor_ow]))
        table_aov <- cbind(table_aov, table_aov[2]^2)
        J <- nrow(table_aov)
        form <- as.formula(paste(names(data)[data_col], " ~ ", names(data)[factor_ow]))
        mwu <- median.test.twosample.independent.mann.whitney.fx(fx = form, data = data)
        full_out <- NULL
        for (i in seq(1:combos)) {
          if (mwu[[i]][[5]] * combos > 1) {
            mwu[[i]][[5]] <- 1
          } else {
            mwu[[i]][[5]] <- mwu[[i]][[5]] * combos
          }
          full_out <- c(full_out, mwu[i])
        }
        output <- matrix("", J, J)
        rownames(output) <- unique(data[[factor_ow]])
        colnames(output) <- unique(data[[factor_ow]])
        loop <- 0
        for (i in seq(from = 1, to = (J - 1))) {
          for (j in seq(from = (i + 1), to = J)) {
            loop <- loop + 1
            output[i, j] <- ifelse(full_out[[loop]][[5]] < (1 - conf), "Reject", "")
            output[j, i] <- ifelse(full_out[[loop]][[5]] < (1 - conf), "Reject", "")
          }
        }
        title <- "Kruskal-Wallis post-hoc: Wilcoxon-Mann-Whitney U using Bonferroni-Dunn (p-value multiplied by # comparisons). Compare stated p-value to desired familywise alpha."
        # Convert to data frame and round before creating DT object
        output <- as.data.frame(output)
        output <- DT::datatable(output,
          caption = title,
          options = list(columnDefs = list(list(className = "dt-center", targets = "_all")), dom = "t", paging = FALSE),
          class = "cell-border stripe"
        )
      }  # end K-W
      
      # Return DT object (ro() was called on data frames before DT creation)
      output
    })
    
    # Post-hoc details output
    posthoc_details <- reactive({
      inputs_vals <- inputs()
      data <- filtered_data()
      req(data, inputs_vals)
      
      conf <- inputs_vals$conf_ow
      R <- inputs_vals$decimal_ow_ph
      type <- inputs_vals$type_ow
      ph_type <- as.numeric(inputs_vals$ow_ph_type)
      ph_details <- inputs_vals$ow_ph_details
      
      req(ph_details == TRUE)
      
      # Make names valid
      names(data) <- make.names(names(data))
      
      data_col <- as.numeric(inputs_vals$ow_data)
      factor_ow <- as.numeric(inputs_vals$ow_factor)
      req(data_col, factor_ow)
      
      # Common anova info
      form <- as.formula(paste(names(data)[data_col], " ~ ", "as.factor(", names(data)[factor_ow], ")"))
      oneway <- aov(formula = form, data = data)
      sum_aov <- summary(oneway)
      
      sse <- sum_aov[[1]][["Sum Sq"]][1]
      ssw <- sum_aov[[1]][["Sum Sq"]][2]
      dfe <- sum_aov[[1]][["Df"]][1]
      dfw <- sum_aov[[1]][["Df"]][2]
      mse <- sum_aov[[1]][["Mean Sq"]][1]
      msw <- sum_aov[[1]][["Mean Sq"]][2]
      sst <- sse + ssw
      dft <- dfe + dfw
      
      combos <- factorial(dfe + 1) / (factorial(2) * factorial(dfe - 1))
      
      sum_stats <- as.data.frame(as.matrix(aggregate(data[data_col], by = data[factor_ow], FUN = function(x) c(mean(x), n = length(x), var = var(x)))))
      cell_means <- as.numeric(as.vector(as.matrix(sum_stats[paste(names(data)[data_col], ".", sep = "")])))
      cell_n <- as.numeric(as.vector(as.matrix(sum_stats[paste(names(data)[data_col], ".n", sep = "")])))
      cell_var <- as.numeric(as.vector(as.matrix(sum_stats[paste(names(data)[data_col], ".var", sep = "")])))
      
      if (type == 1 || type == 4) {
        if (ph_type == 1) {
          ph_test_results <- contrasts.tukey.kgroups.simple(
            group.mean = cell_means,
            group.sample.size = cell_n,
            conf.level.familywise = conf,
            mean.squared.error = msw,
            df.mean.squared.error = dfw
          )
          output <- NULL
          for (n in seq(1:combos)) {
            output <- c(output, ph_test_results$list.tests[n])
          }
        }
        if (ph_type == 2) {
          ph_test_results <- contrasts.games.howell.kgroups.simple(
            group.mean = cell_means,
            group.variance = cell_var,
            group.sample.size = as.vector(as.matrix(cell_n)),
            conf.level.familywise = conf,
            mean.squared.error = NA
          )
          output <- NULL
          for (n in seq(1:combos)) {
            output <- c(output, ph_test_results$list.tests[n])
          }
        }
        output
      } else if (type == 2) {  # random
        output <- "p-values multiplied by the number of combinations in order to maintain the family-wise alpha at your selected value"
        output
      } else if (type == 3) {  # Kruskal-Wallis
        # Use formula without as.factor for K-W
        form <- as.formula(paste(names(data)[data_col], " ~ ", names(data)[factor_ow]))
        mwu <- median.test.twosample.independent.mann.whitney.fx(fx = form, data = data)
        output <- NULL
        for (n in seq(1:combos)) {
          if (mwu[[n]][[5]] * combos > 1) {
            mwu[[n]][[5]] <- 1
          } else {
            mwu[[n]][[5]] <- mwu[[n]][[5]] * combos
          }
          output <- c(output, mwu[n])
        }
        output
      } else {
        NULL
      }
    })
    
    # Post-hoc plot
    posthoc_plot <- reactive({
      inputs_vals <- inputs()
      data <- filtered_data()
      req(data, inputs_vals)
      
      conf <- inputs_vals$conf_ow
      R <- inputs_vals$decimal_ow_ph
      type <- inputs_vals$type_ow
      lines <- inputs_vals$lines_ow_ph
      plot_type <- as.numeric(inputs_vals$plot_type_ow_ph)
      font_size <- as.numeric(inputs_vals$ow_font_size)
      
      req(type)
      
      # Make names valid
      names(data) <- make.names(names(data))
      
      data_col <- as.numeric(inputs_vals$ow_data)
      factor_ow <- as.numeric(inputs_vals$ow_factor)
      req(data_col, factor_ow)
      
      cols <- colors()
      col_fill <- unname(cols$col_fill)
      # Get full color palette for random effects plot
      full_palette <- color_palette()
      
      if (type == 1 || type == 4) {  # fixed
        req(plot_type)
        p <- ggplot(data = data, aes(x = .data[[names(data)[factor_ow]]], y = .data[[names(data)[data_col]]], group = .data[[names(data)[factor_ow]]])) +
          stat_summary(fun = "mean", geom = "point", size = 4) +
          labs(caption = "Points are means")
        if (lines == TRUE) {  # add lines
          p <- p + stat_summary(fun = mean, geom = "line", aes(group = 1))
        }
        if (plot_type == 2) {  # add violin
          p <- p + geom_violin(fill = adjustcolor(col = col_fill, alpha.f = 0.5), bw = "sj")
        }
        if (plot_type == 3) {  # boxplot
          p <- p + geom_boxplot(fill = adjustcolor(col = col_fill, alpha.f = 0.5))
        }
      }  # end fixed
      
      if (type == 2) {  # random
        form <- as.formula(paste(names(data)[data_col], " ~ ", "as.factor(", names(data)[factor_ow], ")"))
        
        oneway <- aov(formula = form, data = data)
        sum_aov <- summary(oneway)
        
        sse <- sum_aov[[1]][["Sum Sq"]][1]
        ssw <- sum_aov[[1]][["Sum Sq"]][2]
        dfe <- sum_aov[[1]][["Df"]][1]
        dfw <- sum_aov[[1]][["Df"]][2]
        mse <- sum_aov[[1]][["Mean Sq"]][1]
        msw <- sum_aov[[1]][["Mean Sq"]][2]
        sst <- sse + ssw
        dft <- dfe + dfw
        
        table_aov <- as.data.frame(table(data[factor_ow]))
        table_aov <- cbind(table_aov, table_aov[2]^2)
        J <- nrow(table_aov)
        sum_n <- colSums(table_aov[2])
        sum_nsq <- colSums(table_aov[3])
        K_prime <- (1 / (J - 1)) * (sum_n - (sum_nsq / sum_n))
        
        bcv <- (mse - msw) / K_prime
        bcv <- max(0, bcv)
        ICC <- 100 * bcv / (bcv + msw)
        
        pop_mean <- mean(data[[data_col]])
        limits <- data.frame(x = c(pop_mean - 2 * bcv^.5 - 3 * msw^.5, pop_mean, pop_mean + 2 * bcv^.5 + 3 * msw^.5))
        colors_plot <- c("Population of Means" = as.character(full_palette[5]), "Unexplained Variability" = as.character(full_palette[3]), "Unexplained Variability" = as.character(full_palette[3]))
        effect_line <- c("95.45% Confidence Interval of Effect" = as.character(full_palette[8]))
        
        # Remove color from base aes() - it's not a column, set it in individual geoms instead
        p <- ggplot(data = limits, aes(x)) +
          ylab("PDF(x)") +
          geom_area(stat = "function", fun = dnorm, args = list(mean = pop_mean, sd = bcv^.5), aes(fill = "Population of Means"), color = full_palette[1]) +
          geom_area(stat = "function", fun = dnorm, args = list(mean = pop_mean - 2 * bcv^.5, sd = msw^.5), aes(fill = "Unexplained Variability"), color = full_palette[3], alpha = 0.5) +
          geom_area(stat = "function", fun = dnorm, args = list(mean = pop_mean + 2 * bcv^.5, sd = msw^.5), aes(fill = "Unexplained Variability"), color = full_palette[3], alpha = 0.5) +
          scale_fill_manual(values = colors_plot)
        
        ylim <- ggplot_build(p)[["layout"]][["panel_scales_y"]][[1]][["range"]][["range"]][2]
        
        # Add geom_segment on single-row data (avoid inheriting 3-row `limits`)
        # and do not inherit global aes mappings.
        p <- p +
          geom_segment(
            data = data.frame(
              x = pop_mean - 2 * bcv^.5,
              y = ylim / 2,
              xend = pop_mean + 2 * bcv^.5,
              yend = ylim / 2
            ),
            mapping = aes(
              x = x,
              y = y,
              xend = xend,
              yend = yend,
              color = "95.45% Confidence Interval of Effect"
            ),
            linewidth = 2,
            inherit.aes = FALSE
          ) +
          scale_color_manual(values = effect_line, labels = function(x) str_wrap(x, width = 10)) +
          labs(title = "Random Effects Post-Hoc", fill = " ", color = " ") +
          theme(legend.position = "bottom")
      }  # end random
      
      if (type == 3) {  # kruskal-wallis - use medians
        req(plot_type)
        p <- ggplot(data = data, aes(x = .data[[names(data)[factor_ow]]], y = .data[[names(data)[data_col]]], group = .data[[names(data)[factor_ow]]])) +
          stat_summary(fun = "median", geom = "point", size = 4) +
          labs(caption = "Points are medians")
        if (lines == TRUE) {  # add lines
          p <- p + stat_summary(fun = median, geom = "line", aes(group = 1))
        }
        if (plot_type == 2) {  # add CI
          p <- p + geom_violin(fill = adjustcolor(col = col_fill, alpha.f = 0.5), bw = "sj")
        }
        if (plot_type == 3) {  # boxplot
          p <- p + geom_boxplot(fill = adjustcolor(col = col_fill, alpha.f = 0.5))
        }
      }
      
      p <- p +
        theme_gray(base_size = font_size) +  # 11 is default, but may be too small for some exports
        theme(legend.position = "bottom") +
        guides(color = guide_legend(nrow = 2)) +
        guides(fill = guide_legend(nrow = 2))
      
      p
    })
    
    # Render outputs
    output$ow_ph_out_tab <- renderDataTable({
      posthoc_table()
    })
    
    output$ow_ph_details <- renderPrint({
      posthoc_details()
    })
    
    # Note: downloadServer is called in coordinator, not worker
    
    # Return reactive values for coordinator
    list(
      posthoc_table = posthoc_table,
      posthoc_plot = posthoc_plot,
      posthoc_details = posthoc_details
    )
  })
}
