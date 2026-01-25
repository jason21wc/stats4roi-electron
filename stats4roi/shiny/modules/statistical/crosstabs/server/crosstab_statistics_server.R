# Crosstab Statistics Worker Module
# Contains business logic for crosstab statistics calculations and table formatting
# No UI rendering - that happens in the coordinator

library(shiny)
library(lolcat)

# Source global systems
source("modules/config/global_config.R")

create_crosstab_statistics_worker <- function(id, table_data, input_values) {
  moduleServer(id, function(input, output, session) {
    
    # Get input values reactively
    inputs <- reactive({
      input_values()
    })
    
    # Get rounding function from global config
    global_config <- get_global_config()
    ro <- global_config$ro
    
    # =========================================================================
    # TABLE DISPLAY REACTIVE
    # =========================================================================
    table_display <- reactive({
      table <- table_data()
      inputs_vals <- inputs()
      
      req(table, inputs_vals)
      req(nrow(table) > 1, ncol(table) > 1)
      
      counts <- inputs_vals$counts
      ev <- inputs_vals$ev
      
      # Generate table display based on counts option
      if (counts == 1) {
        # Simple count
        tableout <- table
      } else if (counts == 2) {
        # Proportion
        sum <- sum(table)
        tableout <- table / sum
        tableout <- ro(tableout, 2)
      } else if (counts == 3) {
        # Percent
        sum <- sum(table)
        tableout <- table / sum
        tableout <- data.frame(lapply(tableout, function(x) sprintf("%.1f%%", x * 100)))
      }
      
      # Add expected values to table if requested
      if (ev) {
        if (counts == 1) {
          # Simple count
          expected <- chi.square.2d.expected.frequencies(table)
        } else if (counts == 2) {
          # Proportion
          expected <- chi.square.2d.expected.frequencies(table) / sum(table)
        } else if (counts == 3) {
          # Percentage
          expected <- chi.square.2d.expected.frequencies(table) / sum(table)
          expected <- data.frame(lapply(expected, function(x) sprintf("%.1f%%", x * 100)))
        }
        expected <- ro(expected, inputs_vals$R)
        
        # Merge back into tableout
        # First change to characters
        tableout <- sapply(X = tableout, FUN = function(X) as.character(X))
        expected <- sapply(X = expected, FUN = function(X) as.character(X))
        
        # Function to combine elements
        combine_elements <- function(x, y) {
          paste0(x, " (", y, ")")
        }
        
        tableout <- as.data.frame(matrix(
          mapply(combine_elements, tableout, expected),
          nrow = nrow(tableout)
        ))
        
        # Add row and column names back in
        rownames(tableout) <- rownames(table)
        colnames(tableout) <- colnames(table)
      }
      
      # Add row and column sums
      rowsums <- rowSums(table)
      colsums <- colSums(table)
      total <- sum(table)
      tableout$Total <- rowsums
      tableout[nrow(tableout) + 1, ] <- c(colsums, total)
      rownames(tableout)[nrow(tableout)] <- "Total"
      
      tableout
    })
    
    # =========================================================================
    # STATISTICS OUTPUT REACTIVE
    # =========================================================================
    statistics_output <- reactive({
      table <- table_data()
      inputs_vals <- inputs()
      
      req(table, inputs_vals)
      req(nrow(table) > 1, ncol(table) > 1)
      
      stats <- inputs_vals$stats
      R <- inputs_vals$R
      conf <- inputs_vals$conf
      row_label <- inputs_vals$row_label
      col_label <- inputs_vals$col_label
      dims <- inputs_vals$dims
      
      stats_out <- NULL
      
      if (!is.null(stats)) {
        stats_out <- paste0(stats_out, "<h3>Crosstabs: ", col_label, " by ", row_label, "</h3>")
        
        # Chi-Squared test
        if (1 %in% stats) {
          temp <- chi.square.independence.test.simple(table)
          stats_out <- paste0(stats_out, 
            "<table><tr><th colspan='3' style='text-align:left'>Chi-square</th></tr>",
            "<tr><td>", withMathJax("$\\chi^{2}=$"), ro(temp[["statistic"]][["chi-square statistic"]], R), "</td>",
            "<td>df = ", temp[["estimate"]][["df"]], "</td>",
            "<td>p-value = ", ro(temp[["p.value"]], R), if (!is.na(temp[["p.value"]]) && temp[["p.value"]] <= (1 - conf)) {"*"}, "</td></tr></table>"
          )
        }
        
        # Phi/Cramer's V
        if (2 %in% stats) {
          temp <- cor.cramer.v(table)
          stats_out <- paste0(stats_out,
            "<table><tr><th colspan='2' style='text-align:left'>Phi / Cramer's V</th></tr>",
            "<tr><td>", withMathJax("$\\phi=$"), ro(temp[["statistic"]][["V"]], R), "</td>",
            "<td>p-value = ", ro(temp[["estimate"]][["chi.square.p"]], R), if (!is.na(temp[["p.value"]]) && temp[["p.value"]] <= (1 - conf)) {"*"}, "</td></tr></table>"
          )
        }
        
        # Fisher's Exact Test (2x2 only)
        if (4 %in% stats) {
          if (dims$rows == 2 && dims$cols == 2) {
            temp <- cor.cramer.v(table)
            stats_out <- paste0(stats_out,
              "<table><tr><th style='text-align:left'>Fisher's Exact Test</th></tr>",
              "<td style='text-align:left'>p-value = ", ro(temp[["p.value"]], R), if (!is.na(temp[["p.value"]]) && temp[["p.value"]] <= (1 - conf)) {"*"}, "</td></tr></table>"
            )
          }
        }
        
        # McNemar's Test/Symmetry Test (k×k tables only)
        if (5 %in% stats) {
          if (dims$rows == dims$cols) {
            # Only calculate for k×k tables
            temp <- cor.bowker.mcnemar.symmetry.1948(observed.frequencies = table)
            stats_out <- paste0(stats_out,
              "<table><tr><th colspan='2' style='text-align:left'>", temp[["method"]], "</th></tr>",
              if (temp[["method"]] == "Bowker's Test of Symmetry (1948)") {
                paste0("<tr><td>", withMathJax("$\\chi^{2}=$"), ro(temp[["statistic"]][["chi-square statistic"]], R), "</td>",
                       "<td>p-value = ", ro(temp[["p.value"]], R), if (!is.na(temp[["p.value"]]) && temp[["p.value"]] <= (1 - conf)) {"*"}, "</td></table>"
                )
              } else {
                paste0("<tr><td style='text-align:left'>p-value = ", ro(temp[["p.value"]], R), if (!is.na(temp[["p.value"]]) && temp[["p.value"]] <= (1 - conf)) {"*"}, "</td></tr></table>")
              }
            )
          } else {
            stats_out <- paste0(stats_out, "<p><b>Symmetry can only be calculated for <i>k</i>×<i>k</i> tables.</b></p>")
          }
        }
        
        # Kappa (k×k tables only)
        if (6 %in% stats) {
          if (dims$rows == dims$cols) {
            # Only calculate for k×k tables
            temp <- cor.cohen.kappa.onesample(observed.frequencies = data.matrix(table), alternative = "greater", conf.level = conf)
            stats_out <- paste0(stats_out,
              "<table><tr><th colspan='3' style='text-align:left'>Kappa (Agreement)</th></tr>",
              "<tr><td>n=", temp[["estimate"]][["n"]], "</td></tr>",
              "<tr><td>Proportion Agreement = ", ro(temp[["estimate"]][["p_o"]], R), "</td></tr>",
              "<tr><td>Proportion Chance Agreement = ", ro(temp[["estimate"]][["p_c"]], R), "</td></tr>",
              "<tr><td>", withMathJax("$\\kappa_{max}=$"), ro(temp[["estimate"]][["kappa.max"]], R), "</td></tr>",
              "<tr><td>", withMathJax("$\\kappa=$"), ro(temp[["estimate"]][["kappa"]], R), "</td></tr>",
              "<tr><td>", withMathJax("$z_{\\kappa=0}=$"), ro(temp[["statistic"]][["z"]], R), "</td></tr>",
              "<tr><td>p-value = ", ro(temp[["p.value"]], R), if (!is.na(temp[["p.value"]]) && temp[["p.value"]] <= (1 - conf)) {"*"}, "</td></tr>",
              "<tr><td>", conf * 100, "% CI for ", withMathJax("$\\kappa:$"), "</td><td>", ro(temp[["conf.int"]][1], R), "</td><td>to</td><td>", ro(temp[["conf.int"]][2], R), "</td></tr></table>"
            )
          } else {
            stats_out <- paste0(stats_out, "<p><b>κ can only be calculated for <i>k</i>×<i>k</i> tables.</b></p>")
          }
        }
      }
      
      stats_out
    })
    
    # =========================================================================
    # RETURN REACTIVE FUNCTIONS
    # =========================================================================
    list(
      table_display = table_display,
      statistics_output = statistics_output
    )
  })
}
