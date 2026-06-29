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
    
    analysis_frame <- reactive({
      inputs_vals <- inputs()
      data <- filtered_data()
      req(data, inputs_vals)
      
      data_col <- as.numeric(inputs_vals$ow_data)
      factor_ow <- as.numeric(inputs_vals$ow_factor)
      type <- inputs_vals$type_ow
      req(data_col, factor_ow, type)
      
      build_oneway_analysis_frame(
        data = data,
        data_col = data_col,
        factor_col = factor_ow,
        analysis_disp = inputs_vals$ow_disp_analysis,
        disp_type_id = inputs_vals$ow_disp_type,
        type_ow = type
      )
    })

    pairwise_posthoc_result <- reactive({
      inputs_vals <- inputs()
      frame <- analysis_frame()
      req(inputs_vals, frame, isTRUE(frame$ok))

      type <- inputs_vals$type_ow
      ph_type <- as.numeric(inputs_vals$ow_ph_type)
      conf <- inputs_vals$conf_ow
      req(type, ph_type)

      if (!(type %in% c(1, 4)) || !(ph_type %in% c(1, 2))) {
        return(NULL)
      }

      work <- frame$data
      form <- stats::as.formula(".response ~ .factor")
      oneway <- aov(formula = form, data = work)
      sum_aov <- summary(oneway)
      dfw <- sum_aov[[1]][["Df"]][2]
      msw <- sum_aov[[1]][["Mean Sq"]][2]

      agg <- ow_oneway_aggregate_stats(work)
      cell_means <- agg$cell_means
      cell_n <- agg$cell_n
      cell_var <- agg$cell_var

      if (ph_type == 1) {
        ph_test_results <- contrasts.tukey.kgroups.simple(
          group.label = agg$group_labels,
          group.mean = cell_means,
          group.sample.size = cell_n,
          conf.level.familywise = conf,
          mean.squared.error = msw,
          df.mean.squared.error = dfw
        )
        procedure_label <- "Tukey HSD homogeneous subsets"
      } else {
        ph_test_results <- contrasts.games.howell.kgroups.simple(
          group.label = agg$group_labels,
          group.mean = cell_means,
          group.variance = cell_var,
          group.sample.size = as.vector(cell_n),
          conf.level.familywise = conf,
          mean.squared.error = NA
        )
        procedure_label <- "Games-Howell homogeneous subsets"
      }

      pmats <- oneway_pairwise_pmatrix_compute(
        group_labels = agg$group_labels,
        group_means = cell_means,
        group_n = cell_n,
        group_var = cell_var,
        ph_type = as.integer(ph_type),
        conf.level.familywise = conf,
        msw = msw,
        dfw = dfw
      )

      list(
        ph_test_results = ph_test_results,
        list.tests = ph_test_results$list.tests,
        matrix.decision = ph_test_results$matrix.decision,
        matrix.p.value = pmats$matrix.p.value,
        msw = msw,
        dfw = dfw,
        agg = agg,
        frame = frame,
        ph_type = ph_type,
        procedure_label = procedure_label
      )
    })
    
    # Post-hoc table output
    posthoc_table <- reactive({
      inputs_vals <- inputs()
      frame <- analysis_frame()
      req(inputs_vals, frame)
      
      if (!isTRUE(frame$ok)) {
        return(NULL)
      }
      
      conf <- inputs_vals$conf_ow
      R <- inputs_vals$decimal_ow_ph
      type <- inputs_vals$type_ow
      ph_type <- as.numeric(inputs_vals$ow_ph_type)
      
      req(type, ph_type)
      
      ph_suffix <- oneway_posthoc_table_suffix(frame$analysis, frame$disp_metric)
      pw <- pairwise_posthoc_result()

      if (type == 1 || type == 4) {  # fixed
        req(ph_type, pw)
        output <- pw$matrix.decision
        output <- ro(output, R)
        caption <- if (ph_type == 1) {
          paste0("Tukey's HSD", ph_suffix)
        } else {
          paste0("Games & Howell", ph_suffix)
        }
        output <- DT::datatable(
          output,
          caption = caption,
          options = list(
            columnDefs = list(list(className = "dt-center", targets = "_all")),
            dom = "t",
            paging = FALSE
          ),
          class = "cell-border stripe"
        )
      }  # end fixed
      
      if (type == 2) {  # random
        work <- frame$data
        form <- stats::as.formula(".response ~ .factor")
        oneway <- aov(formula = form, data = work)
        sum_aov <- summary(oneway)
        sse <- sum_aov[[1]][["Sum Sq"]][1]
        ssw <- sum_aov[[1]][["Sum Sq"]][2]
        dfe <- sum_aov[[1]][["Df"]][1]
        dfw <- sum_aov[[1]][["Df"]][2]
        mse <- sum_aov[[1]][["Mean Sq"]][1]
        msw <- sum_aov[[1]][["Mean Sq"]][2]
        table_aov <- as.data.frame(table(work$.factor))
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
        
        title <- paste0("Random Effect Importance", ph_suffix)
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
      
      if (type == 3) {  # all pair-wise MWU (means only)
        work <- frame$data
        form <- stats::as.formula(".response ~ .factor")
        oneway <- aov(formula = form, data = work)
        sum_aov <- summary(oneway)
        dfe <- sum_aov[[1]][["Df"]][1]
        combos <- factorial(dfe + 1) / (factorial(2) * factorial(dfe - 1))
        table_aov <- as.data.frame(table(work$.factor))
        table_aov <- cbind(table_aov, table_aov[2]^2)
        J <- nrow(table_aov)
        kw_df <- data.frame(
          work$.response,
          work$.factor,
          stringsAsFactors = FALSE
        )
        names(kw_df) <- c(frame$response_label, frame$factor_label)
        form_kw <- stats::as.formula(paste(frame$response_label, "~", frame$factor_label))
        mwu <- median.test.twosample.independent.mann.whitney.fx(fx = form_kw, data = kw_df)
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
        rownames(output) <- levels(work$.factor)
        colnames(output) <- levels(work$.factor)
        loop <- 0
        for (i in seq(from = 1, to = (J - 1))) {
          for (j in seq(from = (i + 1), to = J)) {
            loop <- loop + 1
            output[i, j] <- ifelse(full_out[[loop]][[5]] < (1 - conf), "Reject", "")
            output[j, i] <- ifelse(full_out[[loop]][[5]] < (1 - conf), "Reject", "")
          }
        }
        title <- paste0(
          "Kruskal-Wallis post-hoc: Wilcoxon-Mann-Whitney U using Bonferroni-Dunn (p-value multiplied by # comparisons). Compare stated p-value to desired familywise alpha.",
          ph_suffix
        )
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
      frame <- analysis_frame()
      req(inputs_vals, frame, isTRUE(frame$ok))
      
      R <- inputs_vals$decimal_ow_ph
      type <- inputs_vals$type_ow
      ph_type <- as.numeric(inputs_vals$ow_ph_type)
      ph_details <- inputs_vals$ow_ph_details
      
      req(ph_details == TRUE)
      
      if (type == 1 || type == 4) {
        pw <- pairwise_posthoc_result()
        req(pw)
        output <- NULL
        for (n in seq_along(pw$list.tests)) {
          output <- c(output, pw$list.tests[n])
        }
        output
      } else if (type == 2) {  # random
        output <- "p-values multiplied by the number of combinations in order to maintain the family-wise alpha at your selected value"
        output
      } else if (type == 3) {  # Kruskal-Wallis
        work <- frame$data
        form <- stats::as.formula(".response ~ .factor")
        oneway <- aov(formula = form, data = work)
        sum_aov <- summary(oneway)
        dfe <- sum_aov[[1]][["Df"]][1]
        combos <- factorial(dfe + 1) / (factorial(2) * factorial(dfe - 1))
        kw_df <- data.frame(
          work$.response,
          work$.factor,
          stringsAsFactors = FALSE
        )
        names(kw_df) <- c(frame$response_label, frame$factor_label)
        form_kw <- stats::as.formula(paste(frame$response_label, "~", frame$factor_label))
        mwu <- median.test.twosample.independent.mann.whitney.fx(fx = form_kw, data = kw_df)
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
      frame <- analysis_frame()
      req(inputs_vals, frame, isTRUE(frame$ok))
      
      conf <- inputs_vals$conf_ow
      R <- inputs_vals$decimal_ow_ph
      type <- inputs_vals$type_ow
      lines <- inputs_vals$lines_ow_ph
      plot_type <- as.numeric(inputs_vals$plot_type_ow_ph)
      font_size <- as.numeric(inputs_vals$ow_font_size)
      
      req(type)
      
      work <- frame$data
      plot_df <- data.frame(
        x = work$.factor,
        y = work$.response,
        stringsAsFactors = FALSE
      )
      ph_labels <- oneway_posthoc_plot_labels(frame$analysis, frame$disp_metric)
      
      cols <- colors()
      col_fill <- unname(cols$col_fill)
      # Get full color palette for random effects plot
      full_palette <- color_palette()
      
      if (type == 1 || type == 4) {  # fixed
        req(plot_type)
        p <- ggplot(data = plot_df, aes(x = .data$x, y = .data$y, group = .data$x)) +
          stat_summary(fun = "mean", geom = "point", size = 4) +
          labs(title = ph_labels$title, caption = ph_labels$caption_points)
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
        form <- stats::as.formula(".response ~ .factor")
        
        oneway <- aov(formula = form, data = work)
        sum_aov <- summary(oneway)
        
        sse <- sum_aov[[1]][["Sum Sq"]][1]
        ssw <- sum_aov[[1]][["Sum Sq"]][2]
        dfe <- sum_aov[[1]][["Df"]][1]
        dfw <- sum_aov[[1]][["Df"]][2]
        mse <- sum_aov[[1]][["Mean Sq"]][1]
        msw <- sum_aov[[1]][["Mean Sq"]][2]
        sst <- sse + ssw
        dft <- dfe + dfw
        
        table_aov <- as.data.frame(table(work$.factor))
        table_aov <- cbind(table_aov, table_aov[2]^2)
        J <- nrow(table_aov)
        sum_n <- colSums(table_aov[2])
        sum_nsq <- colSums(table_aov[3])
        K_prime <- (1 / (J - 1)) * (sum_n - (sum_nsq / sum_n))
        
        bcv <- (mse - msw) / K_prime
        bcv <- max(0, bcv)
        ICC <- 100 * bcv / (bcv + msw)
        
        pop_mean <- mean(work$.response)
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
          labs(title = ph_labels$title, fill = " ", color = " ") +
          theme(legend.position = "bottom")
      }  # end random
      
      if (type == 3) {  # kruskal-wallis - use medians (means analysis only)
        req(plot_type)
        p <- ggplot(data = plot_df, aes(x = .data$x, y = .data$y, group = .data$x)) +
          stat_summary(fun = "median", geom = "point", size = 4) +
          labs(title = ph_labels$title, caption = "Points are medians")
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

    posthoc_homogeneous_subsets <- reactive({
      inputs_vals <- inputs()
      req(isTRUE(inputs_vals$ow_ph_homogeneous))

      pw <- pairwise_posthoc_result()
      req(pw)

      R <- inputs_vals$decimal_ow_ph
      conf <- inputs_vals$conf_ow
      alpha <- 1 - conf
      ph_suffix <- oneway_posthoc_table_suffix(pw$frame$analysis, pw$frame$disp_metric)
      factor_label <- pw$frame$factor_label
      if (!nzchar(factor_label)) {
        factor_label <- "Group"
      }

      grid <- oneway_homogeneous_subsets_grid(
        means = pw$agg$cell_means,
        sample_sizes = pw$agg$cell_n,
        labels = pw$agg$group_labels,
        p_matrix = pw$matrix.p.value,
        alpha = alpha,
        mse = pw$msw,
        df_error = pw$dfw,
        factor_label = factor_label,
        digits = R
      )
      req(grid)

      foot <- paste0(
        "Means for groups in homogeneous subsets are displayed. ",
        "Based on observed means."
      )
      if (as.integer(pw$ph_type)[1L] == 1L) {
        foot <- paste0(
          foot,
          " Mean Square(Error) = ",
          ow_hsg_format_num(pw$msw, R),
          "."
        )
      } else {
        foot <- paste0(foot, ".")
      }
      caption <- paste0(pw$procedure_label, ph_suffix, ". ", foot)
      oneway_homogeneous_subsets_datatable(grid, caption, factor_label = factor_label)
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
      posthoc_details = posthoc_details,
      posthoc_homogeneous_subsets = posthoc_homogeneous_subsets
    )
  })
}
