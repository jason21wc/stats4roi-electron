# Exponential Distribution Tests
# This module contains functions for testing if data follows an exponential distribution

# Shapiro-Wilk Exponential Test (replicating app.R lines 28844-28890)
run_shapiro_wilk_exp_test <- function(data, UI1, UI2, data_type, conf, R, session = NULL) {
  results <- data.frame()
  
  if (data_type == 1) {  # columns
    for (i in UI1) {
      cat("Running Shapiro-Wilk test on column", i, ":", names(data[i]), "\n")
      tryCatch({
        result <- shapiro.exp.test(x = data[[i]], session = session)
        n <- length(na.omit(data[[i]]))
        cat("Test completed. W =", result[["statistic"]][["W"]], ", p =", result[["p.value"]], "\n")
        results <- rbind(results, data.frame(
          ID = names(data[i]),
          n = n,
          W = round(result[["statistic"]][["W"]], R),
          p = round(result[["p.value"]], R),
          significant = if (!is.na(result[["p.value"]]) && !is.null(result[["p.value"]]) && result[["p.value"]] < (1 - conf)) "*" else "",
          method = result[["method"]],
          stringsAsFactors = FALSE
        ))
      }, error = function(e) {
        cat("Error in Shapiro-Wilk test:", e$message, "\n")
        results <<- rbind(results, data.frame(
          ID = names(data[i]),
          n = length(na.omit(data[[i]])),
          W = "Error",
          p = "Error",
          significant = "",
          method = "Error",
          stringsAsFactors = FALSE
        ))
      })
    }
  } else {  # analysis using factors
    all_combos <- unique(data[UI1])
    num_combos <- nrow(all_combos)
    
    for (i in UI2) {  # each column with data
      for (j in 1:num_combos) {  # each combo
        this_combo <- all_combos[j, ]
        sel <- paste0("data$", names(data)[UI1], "==", "'", this_combo, "'", collapse = " & ")
        sub_data <- data[which(eval(parse(text = sel))), ]
        
        result <- shapiro.exp.test(x = sub_data[[i]], session = session)
        n <- length(na.omit(sub_data[[i]]))
        results <- rbind(results, data.frame(
          ID = paste0(names(data)[i], ": ", paste(names(all_combos[j, , drop = FALSE]), "=", this_combo, collapse = ", ")),
          n = n,
          W = round(result[["statistic"]][["W"]], R),
          p = round(result[["p.value"]], R),
          significant = if (!is.na(result[["p.value"]]) && !is.null(result[["p.value"]]) && result[["p.value"]] < (1 - conf)) "*" else "",
          method = result[["method"]],
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(results)
}

# MVP Exponential Test (replicating app.R lines 28891-28936)
run_mvp_exp_test <- function(data, UI1, UI2, data_type, conf, R, session = NULL) {
  results <- data.frame()
  
  if (data_type == 1) {  # columns
    for (i in UI1) {
      cat("Running MVP test on column", i, ":", names(data[i]), "\n")
      tryCatch({
        result <- mvp_exp(x = data[[i]], max_sims = 100000, session = session)
        n <- length(na.omit(data[[i]]))
        cat("MVP test completed. MVP(E) =", result[["MVP(E) = "]], ", p =", result[["p-value"]], "\n")
        results <- rbind(results, data.frame(
          ID = names(data[i]),
          n = n,
          MVP_E = round(result[["MVP(E) = "]], R),
          p = round(result[["p-value"]], R),
          significant = if (!is.na(result[["p-value"]]) && !is.null(result[["p-value"]]) && result[["p-value"]] < (1 - conf)) "*" else "",
          stringsAsFactors = FALSE
        ))
      }, error = function(e) {
        cat("Error in MVP test:", e$message, "\n")
        results <<- rbind(results, data.frame(
          ID = names(data[i]),
          n = length(na.omit(data[[i]])),
          MVP_E = "Error",
          p = "Error",
          significant = "",
          stringsAsFactors = FALSE
        ))
      })
    }
  } else {  # analysis using factors
    all_combos <- unique(data[UI1])
    num_combos <- nrow(all_combos)
    
    for (i in UI2) {  # each column with data
      for (j in 1:num_combos) {  # each combo
        this_combo <- all_combos[j, ]
        sel <- paste0("data$", names(data)[UI1], "==", "'", this_combo, "'", collapse = " & ")
        sub_data <- data[which(eval(parse(text = sel))), ]
        
        result <- mvp_exp(x = sub_data[[i]], max_sims = 100000, session = session)
        n <- length(na.omit(sub_data[[i]]))
        results <- rbind(results, data.frame(
          ID = paste0(names(data)[i], ": ", paste(names(all_combos[j, , drop = FALSE]), "=", this_combo, collapse = ", ")),
          n = n,
          MVP_E = round(result[["MVP(E) = "]], R),
          p = round(result[["p-value"]], R),
          significant = if (!is.na(result[["p-value"]]) && !is.null(result[["p-value"]]) && result[["p-value"]] < (1 - conf)) "*" else "",
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(results)
}

# Anderson-Darling Exponential Test (replicating app.R lines 28837-28936)
run_anderson_darling_exp_test <- function(data, UI1, UI2, data_type, conf, R, use_min = FALSE) {
  output <- paste0("<h3>Anderson-Darling Exponentiality Test</h3>",
                   if (use_min) {"Using data minimum as origin<br>"},
                   "Zeroes excluded if present",
                   "<table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>",
                   "<tr><th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>ID</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>n</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>W</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>p</th></tr>")
  
  if (data_type == 1) {  # columns
    for (i in UI1) {
      data_col <- data[[i]]
      if (use_min) {
        data_col <- data_col - min(na.omit(data_col))
      }
      result <- exp_test_ad(x = data_col[which(data_col > 0)])
      n <- length(na.omit(data[[i]]))
      output <- paste0(output,
                       "<tr><td style='padding: 8px 12px; border: 1px solid #ddd;'>", names(data[i]), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", n, "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["statistic"]][["W"]], R), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["p.value"]], R), 
                       if (!is.na(result[["p.value"]]) && !is.null(result[["p.value"]]) && result[["p.value"]] < (1 - conf)) {"*"}, "</td></tr>")
    }
  } else {  # analysis using factors
    all_combos <- unique(data[UI1])
    num_combos <- nrow(all_combos)
    
    for (i in UI2) {  # each column with data
      for (j in 1:num_combos) {  # each combo
        this_combo <- all_combos[j, ]
        sel <- paste0("data$", names(data)[UI1], "==", "'", this_combo, "'", collapse = " & ")
        sub_data <- data[which(eval(parse(text = sel))), ]
        
        data_col <- sub_data[[i]]
        if (use_min) {
          data_col <- data_col - min(na.omit(data_col))
        }
        result <- exp_test_ad(x = data_col[which(data_col > 0)])
        n <- length(na.omit(sub_data[[i]]))
        output <- paste0(output,
                         "<tr><td style='padding: 8px 12px; border: 1px solid #ddd;'>", 
                         paste0(names(data)[i], ": ", paste(names(all_combos[j, , drop = FALSE]), "=", this_combo, collapse = ", ")), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", n, "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["statistic"]][["W"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["p.value"]], R), 
                         if (!is.na(result[["p.value"]]) && !is.null(result[["p.value"]]) && result[["p.value"]] < (1 - conf)) {"*"}, "</td></tr>")
      }
    }
  }
  
  # Add important note about Anderson-Darling test
  output <- paste0(output, "</table>",
                   "<div style='background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 10px; border-radius: 4px; margin: 10px 0;'>",
                   "<strong>Note:</strong> A-D unfairly rejects if the origin is > 0. If your data have a minimum greater than zero, check the 'Set origin to Xmin' box.",
                   "</div>")
  return(output)
}
