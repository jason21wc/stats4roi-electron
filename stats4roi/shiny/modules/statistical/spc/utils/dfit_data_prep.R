# Distribution Fitting data preparation

dfit_column_values <- function(data, column = NULL) {
  if (is.null(data) || !nrow(data)) {
    return(numeric(0))
  }
  col <- as.numeric(column)
  if (!length(col) || is.na(col) || col < 1L || col > ncol(data)) {
    return(numeric(0))
  }
  stats::na.omit(as.numeric(data[[col]]))
}
