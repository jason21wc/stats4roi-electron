# EDA Helper Functions
# Common utility functions for EDA module

# Safe numeric coercion for EDA measurement columns.
#
# When the import pipeline has converted a character column to a factor,
# calling as.numeric() directly returns internal level codes (1, 2, 3, …)
# rather than the labels' numeric values.  This helper coerces through
# as.character() first so that:
#   factor(c("10", "20", "bad"))  → c(10, 20, NA)   (labels parsed)
#   factor(c("Pass","Fail","Pending")) → c(NA, NA, NA)  (non-numeric → all NA)
#
# For grouping/factor columns (eda_UI1) use as.numeric() directly on the
# column-index integers, not this function.
eda_safe_numeric <- function(x) {
  if (is.factor(x)) {
    suppressWarnings(as.numeric(as.character(x)))
  } else {
    suppressWarnings(as.numeric(x))
  }
}

# Helper function to safely get values from reactive objects
safe_get <- function(obj, key, default = NULL) {
  if (is.null(obj) || is.null(key)) {
    return(default)
  }
  
  if (is.list(obj) && key %in% names(obj)) {
    return(obj[[key]])
  }
  
  if (is.data.frame(obj) && key %in% colnames(obj)) {
    return(obj[[key]])
  }
  
  return(default)
}

# Helper function to create table rows with consistent spacing
create_table_row <- function(...) {
  args <- list(...)
  if (length(args) == 0) return("")
  
  # Create table row with consistent spacing
  paste0(
    "<tr>",
    paste0("<td>", args, "</td>", collapse = ""),
    "</tr>"
  )
}

# Transpose a stats table so variables/groups are columns and statistics are rows.
# Uses id_cols (e.g. dv.name or factor columns) for column headers; R's t() drops
# those names when a character column is present.
format_eda_transposed_table <- function(output, id_cols = "dv.name", stat_label = "Statistic") {
  if (is.null(output) || nrow(output) == 0L) {
    return(output)
  }

  output <- as.data.frame(output, stringsAsFactors = FALSE)
  id_cols <- intersect(id_cols, names(output))

  if (length(id_cols) > 0L) {
    if (length(id_cols) == 1L) {
      row_ids <- output[[id_cols[1L]]]
    } else {
      row_ids <- apply(output[, id_cols, drop = FALSE], 1L, function(r) {
        paste(r, collapse = ", ")
      })
    }
    output <- output[, setdiff(names(output), id_cols), drop = FALSE]
  } else {
    row_ids <- rownames(output)
    if (is.null(row_ids) || identical(row_ids, as.character(seq_len(nrow(output))))) {
      row_ids <- seq_len(nrow(output))
    }
  }

  transposed <- as.data.frame(t(output), stringsAsFactors = FALSE)
  row_ids <- as.character(row_ids)
  row_ids[row_ids == ""] <- "Column"
  row_ids[is.na(row_ids)] <- "Column"
  colnames(transposed) <- make.unique(row_ids, sep = " ")
  result <- cbind(
    data.frame(Statistic = rownames(transposed), stringsAsFactors = FALSE),
    transposed,
    stringsAsFactors = FALSE
  )
  rownames(result) <- NULL
  names(result) <- make.unique(c(stat_label, colnames(transposed)), sep = " ")
  result
}

# Standard DT wrapper for EDA output tables (centered headers via eda-table CSS).
eda_datatable <- function(data, options = list(), rownames = FALSE, ...) {
  if (is.null(data) || !is.data.frame(data) || ncol(data) == 0L || nrow(data) == 0L) {
    data <- data.frame(Message = "No data available", stringsAsFactors = FALSE)
  } else {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    names(data) <- make.unique(as.character(names(data)), sep = " ")
  }
  default_options <- list(
    paging = FALSE,
    autoWidth = TRUE
  )
  DT::datatable(
    data,
    options = utils::modifyList(default_options, options),
    class = c("display", "compact", "eda-table"),
    rownames = rownames,
    ...
  )
}
