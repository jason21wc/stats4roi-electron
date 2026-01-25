# Test Helper Functions
# This module contains common utilities for distribution testing

# Helper function to create factor combinations
create_factor_combinations <- function(data, factor_cols) {
  all_combos <- unique(data[factor_cols])
  return(all_combos)
}

# Helper function to filter data by factor combination
filter_data_by_factors <- function(data, factor_cols, combo_row) {
  this_combo <- combo_row
  sel <- paste0("data$", names(data)[factor_cols], "==", "'", this_combo, "'", collapse = " & ")
  sub_data <- data[which(eval(parse(text = sel))), ]
  return(sub_data)
}

# Helper function to format test results table with improved styling
format_test_results_table <- function(test_name, headers, results, conf, R) {
  output <- paste0("<h3>", test_name, "</h3>",
                   "<table style='border-collapse: collapse; width: 100%; margin: 10px 0;'><tr>")
  
  # Add headers with styling
  for (header in headers) {
    output <- paste0(output, "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>", header, "</th>")
  }
  output <- paste0(output, "</tr>")
  
  # Add results with styling
  for (result in results) {
    output <- paste0(output, "<tr>")
    for (i in seq_along(result)) {
      if (i == length(result)) {  # p-value column
        p_val <- result[[i]]
        significance <- if (p_val < (1 - conf)) {"*"} else {""}
        output <- paste0(output, "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", ro(p_val, R), significance, "</td>")
      } else if (i == 1) {  # ID column
        output <- paste0(output, "<td style='padding: 8px 12px; border: 1px solid #ddd;'>", ro(result[[i]], R), "</td>")
      } else {  # numeric columns
        output <- paste0(output, "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", ro(result[[i]], R), "</td>")
      }
    }
    output <- paste0(output, "</tr>")
  }
  
  output <- paste0(output, "</table>")
  return(output)
}

# Helper function to create styled table headers
create_table_headers <- function(headers) {
  header_html <- "<tr>"
  for (header in headers) {
    header_html <- paste0(header_html, "<th style='padding: 8px 12px; border: 1px solid #ddd; background-color: #f5f5f5;'>", header, "</th>")
  }
  header_html <- paste0(header_html, "</tr>")
  return(header_html)
}

# Helper function to create styled table row
create_table_row <- function(cells, is_p_value_col = NULL) {
  row_html <- "<tr>"
  for (i in seq_along(cells)) {
    if (!is.null(is_p_value_col) && i == is_p_value_col) {
      # P-value column - right aligned
      row_html <- paste0(row_html, "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", cells[i], "</td>")
    } else if (i == 1) {
      # ID column - left aligned
      row_html <- paste0(row_html, "<td style='padding: 8px 12px; border: 1px solid #ddd;'>", cells[i], "</td>")
    } else {
      # Numeric columns - right aligned
      row_html <- paste0(row_html, "<td style='padding: 8px 12px; border: 1px solid #ddd; text-align: right;'>", cells[i], "</td>")
    }
  }
  row_html <- paste0(row_html, "</tr>")
  return(row_html)
}

# Helper function to check if data is suitable for testing
validate_test_data <- function(data, col_indices) {
  if (is.null(data) || nrow(data) == 0) {
    return(FALSE)
  }
  
  for (i in col_indices) {
    if (i > ncol(data) || i < 1) {
      return(FALSE)
    }
    
    # Check if column has enough non-NA values
    col_data <- data[[i]]
    non_na_count <- length(na.omit(col_data))
    if (non_na_count < 3) {  # Minimum sample size for most tests
      return(FALSE)
    }
  }
  
  return(TRUE)
}

# Helper function to get test descriptions
get_test_descriptions <- function() {
  list(
    "1" = "Poisson Dispersion Test: Tests if data follows Poisson distribution by checking if variance equals mean.",
    "2" = "Anderson-Darling Normality Test: EDF-based test for normality with emphasis on tail deviations.",
    "3" = "Shapiro-Wilk Normality Test: Powerful test for normality, especially good for small samples.",
    "4" = "Lin-Mudholkar Test: Tests normality using skewness and kurtosis measures.",
    "5" = "Skewness and Kurtosis Test: Tests normality by examining third and fourth moments.",
    "6" = "Shapiro-Wilk Exponential Test: Tests if data follows exponential distribution.",
    "7" = "MVP Exponential Test: Monte Carlo-based test for exponential distribution.",
    "8" = "Anderson-Darling Exponential Test: EDF-based test for exponential distribution.",
    "9" = "D'Agostino's Omnibus Test: Combines skewness and kurtosis tests for normality."
  )
}
