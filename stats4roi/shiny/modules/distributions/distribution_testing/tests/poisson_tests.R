# Poisson Distribution Tests
# This module contains functions for testing if data follows a Poisson distribution

# Poisson Dispersion Test (replicating app.R lines 28496-28540)
run_poisson_test <- function(data, UI1, UI2, data_type, conf, R) {
  output <- paste0("<h3>Poisson Dispersion Test</h3>",
                   "<table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>",
                   "<tr><th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>ID</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>Sample Mean</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>Sample Variance</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>χ²</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>df</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>p</th></tr>")
  
  if (data_type == 1) {  # columns
    for (i in UI1) {
      result <- poisson.dist.test(data[[i]])
      output <- paste0(output,
                       "<tr><td style='padding: 8px 12px; border: 1px solid #ddd;'>", names(data[i]), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["sample mean"]], R), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["sample variance"]], R), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["statistic"]][["chi.square"]], R), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["parameter"]][["degrees of freedom"]], R), "</td>",
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
        
        result <- poisson.dist.test(sub_data[[i]])
        output <- paste0(output,
                         "<tr><td style='padding: 8px 12px; border: 1px solid #ddd;'>", paste0(names(data)[i], ": ", 
                                           paste(names(all_combos[j, , drop = FALSE]), "=", this_combo, collapse = ", ")), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["sample mean"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["sample variance"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["statistic"]][["chi.square"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["parameter"]][["degrees of freedom"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["p.value"]], R), 
                         if (!is.na(result[["p.value"]]) && !is.null(result[["p.value"]]) && result[["p.value"]] < (1 - conf)) {"*"}, "</td></tr>")
      }
    }
  }
  
  output <- paste0(output, "</table>")
  return(output)
}
