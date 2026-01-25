# Normality Distribution Tests
# This module contains functions for testing if data follows a normal distribution

# Anderson-Darling Normality Test (replicating app.R lines 28541-28585)
run_anderson_darling_test <- function(data, UI1, UI2, data_type, conf, R) {
  output <- paste0("<h3>Anderson-Darling Normality Test</h3>",
                   "<table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>",
                   "<tr><th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>ID</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>n</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>A²*</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>p</th></tr>")
  
  if (data_type == 1) {  # columns
    for (i in UI1) {
      result <- anderson.darling.normality.test(data[[i]])
      n <- length(na.omit(data[[i]]))
      output <- paste0(output,
                       "<tr><td style='padding: 8px 12px; border: 1px solid #ddd;'>", names(data[i]), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", n, "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["AA"]], R), "</td>",
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
        
        result <- anderson.darling.normality.test(sub_data[[i]])
        n <- length(na.omit(sub_data[[i]]))
        output <- paste0(output,
                         "<tr><td style='padding: 8px 12px; border: 1px solid #ddd;'>", paste0(names(data)[i], ": ", 
                                           paste(names(all_combos[j, , drop = FALSE]), "=", this_combo, collapse = ", ")), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", n, "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["AA"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["p.value"]], R), 
                         if (!is.na(result[["p.value"]]) && !is.null(result[["p.value"]]) && result[["p.value"]] < (1 - conf)) {"*"}, "</td></tr>")
      }
    }
  }
  
  output <- paste0(output, "</table>")
  return(output)
}

# Shapiro-Wilk Normality Test (replicating app.R lines 28586-28630)
run_shapiro_wilk_test <- function(data, UI1, UI2, data_type, conf, R) {
  output <- paste0("<h3>Shapiro-Wilk Normality Test</h3>",
                   "<table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>",
                   "<tr><th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>ID</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>n</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>W</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>p</th></tr>")
  
  if (data_type == 1) {  # columns
    for (i in UI1) {
      result <- shapiro.wilk.normality.test(data[[i]])
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
        
        result <- shapiro.wilk.normality.test(sub_data[[i]])
        n <- length(na.omit(sub_data[[i]]))
        output <- paste0(output,
                         "<tr><td style='padding: 8px 12px; border: 1px solid #ddd;'>", paste0(names(data)[i], ": ", 
                                           paste(names(all_combos[j, , drop = FALSE]), "=", this_combo, collapse = ", ")), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", n, "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["statistic"]][["W"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["p.value"]], R), 
                         if (!is.na(result[["p.value"]]) && !is.null(result[["p.value"]]) && result[["p.value"]] < (1 - conf)) {"*"}, "</td></tr>")
      }
    }
  }
  
  output <- paste0(output, "</table>")
  return(output)
}

# Lin-Mudholkar Test (replicating app.R lines 28631-28682)
run_lin_mudholkar_test <- function(data, UI1, UI2, data_type, conf, R) {
  output <- paste0("<h3>Lin-Mudholkar Normality Test (Only tests skewness)</h3>",
                   "<table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>",
                   "<tr><th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>ID</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>n</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>r</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>z</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>p</th></tr>")
  
  if (data_type == 1) {  # columns
    for (i in UI1) {
      result <- lin.mudholkar.normality.test(data[[i]])
      output <- paste0(output,
                       "<tr><td style='padding: 8px 12px; border: 1px solid #ddd;'>", names(data[i]), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["sample.size"]], R), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["r"]], R), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["statistic"]][["z statistic"]], R), "</td>",
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
        n <- length(na.omit(sub_data[[i]]))
        
        if (n < 10) {
          output <- paste0(output,
                           "<tr><td style='padding: 8px 12px; border: 1px solid #ddd;'>", 
                           paste0(names(data)[i], ": ", paste(names(all_combos[j, , drop = FALSE]), "=", this_combo, collapse = ", ")), "</td>",
                           "<td colspan='4' style='padding: 8px 12px; border: 1px solid #ddd; text-align: center;'>Ten points recommended to calculate Lin-Mudholkar: n = ", n, "</td></tr>")
          next
        }
        
        result <- lin.mudholkar.normality.test(sub_data[[i]])
        output <- paste0(output,
                         "<tr><td style='padding: 8px 12px; border: 1px solid #ddd;'>", 
                         paste0(names(data)[i], ": ", paste(names(all_combos[j, , drop = FALSE]), "=", this_combo, collapse = ", ")), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["sample.size"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["r"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["statistic"]][["z statistic"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["p.value"]], R), 
                         if (!is.na(result[["p.value"]]) && !is.null(result[["p.value"]]) && result[["p.value"]] < (1 - conf)) {"*"}, "</td></tr>")
      }
    }
  }
  
  output <- paste0(output, "</table>")
  return(output)
}

# Skewness and Kurtosis Test (replicating app.R lines 28683-28750)
run_skewness_kurtosis_test <- function(data, UI1, UI2, data_type, conf, R) {
  output <- paste0("<h3>Skewness and Kurtosis Normality Tests with ", conf * 100, "% Confidence Intervals</h3>",
                   "<table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>",
                   "<tr><th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>ID</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>n</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>g₃(low)</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>g₃</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>g₃(high)</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>p(Skew)</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>g₄(low)</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>g₄</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>g₄(high)</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>p(Kurt)</th></tr>")
  
  if (data_type == 1) {  # columns
    for (i in UI1) {
      result_skew <- skewness.test(x = data[[i]], conf.level = conf, alternative = "two.sided")
      n <- length(na.omit(data[[i]]))
      output <- paste0(output,
                       "<tr><td style='padding: 8px 12px; border: 1px solid #ddd;'>", names(data[i]), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", n, "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result_skew[["conf.int"]][1], R), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result_skew[["statistic"]][["skewness"]], R), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result_skew[["conf.int"]][2], R), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result_skew[["p.value"]], R), 
                       if (!is.na(result_skew[["p.value"]]) && !is.null(result_skew[["p.value"]]) && result_skew[["p.value"]] < (1 - conf)) {"*"}, "</td>")
      
      result_kurt <- kurtosis.test(x = data[[i]], conf.level = conf, alternative = "two.sided")
      output <- paste0(output,
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result_kurt[["conf.int"]][1], R), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result_kurt[["statistic"]][["kurtosis"]], R), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result_kurt[["conf.int"]][2], R), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result_kurt[["p.value"]], R), 
                       if (!is.na(result_kurt[["p.value"]]) && !is.null(result_kurt[["p.value"]]) && result_kurt[["p.value"]] < (1 - conf)) {"*"}, "</td></tr>")
    }
  } else {  # analysis using factors
    all_combos <- unique(data[UI1])
    num_combos <- nrow(all_combos)
    
    for (i in UI2) {  # each column with data
      for (j in 1:num_combos) {  # each combo
        this_combo <- all_combos[j, ]
        sel <- paste0("data$", names(data)[UI1], "==", "'", this_combo, "'", collapse = " & ")
        sub_data <- data[which(eval(parse(text = sel))), ]
        
        result_skew <- skewness.test(x = sub_data[[i]], conf.level = conf, alternative = "two.sided")
        n <- length(na.omit(sub_data[[i]]))
        
        if (is.nan(result_skew[["p.value"]])) {
          result_skew[["p.value"]] <- 999
        }
        
        output <- paste0(output,
                         "<tr><td style='padding: 8px 12px; border: 1px solid #ddd;'>", 
                         paste0(names(data)[i], ": ", paste(names(all_combos[j, , drop = FALSE]), "=", this_combo, collapse = ", ")), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", n, "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result_skew[["conf.int"]][1], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result_skew[["statistic"]][["skewness"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result_skew[["conf.int"]][2], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result_skew[["p.value"]], R), 
                         if (!is.na(result_skew[["p.value"]]) && !is.null(result_skew[["p.value"]]) && result_skew[["p.value"]] < (1 - conf)) {"*"}, "</td>")
        
        result_kurt <- kurtosis.test(x = sub_data[[i]], conf.level = conf, alternative = "two.sided")
        if (is.nan(result_kurt[["p.value"]])) {
          result_kurt[["p.value"]] <- 999
        }
        
        output <- paste0(output,
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result_kurt[["conf.int"]][1], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result_kurt[["statistic"]][["kurtosis"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result_kurt[["conf.int"]][2], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result_kurt[["p.value"]], R), 
                         if (!is.na(result_kurt[["p.value"]]) && !is.null(result_kurt[["p.value"]]) && result_kurt[["p.value"]] < (1 - conf)) {"*"}, "</td></tr>")
      }
    }
  }
  
  output <- paste0(output, "</table>")
  return(output)
}

# D'Agostino's Omnibus Test (replicating app.R lines 28752-28820)
run_dagostino_test <- function(data, UI1, UI2, data_type, conf, R) {
  output <- paste0("<h3>D'Agostino's Omnibus Test with ", conf * 100, "% Confidence Intervals</h3>",
                   "<table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>",
                   "<tr><th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>ID</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>n</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>g₃(low)</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>g₃</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>g₃(high)</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>p(Skew)</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>g₄(low)</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>g₄</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>g₄(high)</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>p(Kurt)</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>χ²</th>",
                   "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>p(χ²)</th></tr>")
  
  if (data_type == 1) {  # columns
    for (i in UI1) {
      result <- dagostino.normality.omnibus.test(x = data[[i]], conf.level = conf)
      n <- length(na.omit(data[[i]]))
      output <- paste0(output,
                       "<tr><td style='padding: 8px 12px; border: 1px solid #ddd;'>", names(data[i]), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", n, "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["g3.lowerci"]], R), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["g3.skewness"]], R), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["g3.upperci"]], R), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["g3.p.value"]], R), 
                       if (!is.na(result[["estimate"]][["g3.p.value"]]) && !is.null(result[["estimate"]][["g3.p.value"]]) && result[["estimate"]][["g3.p.value"]] < (1 - conf)) {"*"}, "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["g4.lowerci"]], R), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["g4.kurtosis"]], R), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["g4.upperci"]], R), "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["g4.p.value"]], R), 
                       if (!is.na(result[["estimate"]][["g4.p.value"]]) && !is.null(result[["estimate"]][["g4.p.value"]]) && result[["estimate"]][["g4.p.value"]] < (1 - conf)) {"*"}, "</td>",
                       "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["statistic"]][["chi.square"]], R), "</td>",
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
        
        result <- dagostino.normality.omnibus.test(x = sub_data[[i]], conf.level = conf)
        n <- length(na.omit(sub_data[[i]]))
        output <- paste0(output,
                         "<tr><td style='padding: 8px 12px; border: 1px solid #ddd;'>", 
                         paste0(names(data)[i], ": ", paste(names(all_combos[j, , drop = FALSE]), "=", this_combo, collapse = ", ")), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", n, "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["g3.lowerci"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["g3.skewness"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["g3.upperci"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["g3.p.value"]], R), 
                         if (!is.na(result[["estimate"]][["g3.p.value"]]) && !is.null(result[["estimate"]][["g3.p.value"]]) && result[["estimate"]][["g3.p.value"]] < (1 - conf)) {"*"}, "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["g4.lowerci"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["g4.kurtosis"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["g4.upperci"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["estimate"]][["g4.p.value"]], R), 
                         if (!is.na(result[["estimate"]][["g4.p.value"]]) && !is.null(result[["estimate"]][["g4.p.value"]]) && result[["estimate"]][["g4.p.value"]] < (1 - conf)) {"*"}, "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["statistic"]][["chi.square"]], R), "</td>",
                         "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", lolcat::round.object(result[["p.value"]], R), 
                         if (!is.na(result[["p.value"]]) && !is.null(result[["p.value"]]) && result[["p.value"]] < (1 - conf)) {"*"}, "</td></tr>")
      }
    }
  }
  
  output <- paste0(output, "</table>")
  return(output)
}
