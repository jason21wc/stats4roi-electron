# Attribute-chart control limit summary for Limit Summary UI.

#' Format one limit column within a set as a single value or min-max range.
spc_format_att_limit_cell <- function(x, digits = 4L) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0L) {
    return("Not plotted")
  }
  finite <- x[is.finite(x)]
  if (length(finite) == 0L) {
    return("Not plotted")
  }
  rounded <- unique(as.numeric(round(finite, digits = digits)))
  if (length(rounded) == 1L) {
    return(format(rounded, trim = TRUE, scientific = FALSE, digits = 15))
  }
  paste0(
    format(min(rounded), trim = TRUE, scientific = FALSE, digits = 15),
    " - ",
    format(max(rounded), trim = TRUE, scientific = FALSE, digits = 15)
  )
}

#' Per-set LCL, centerline, and UCL display rows from attribute plot data.
spc_summarize_att_limits_by_set <- function(dat, digits = 4L) {
  if (is.null(dat) || !is.data.frame(dat) || nrow(dat) == 0L) {
    return(data.frame(
      Set = character(0),
      LCL = character(0),
      Centerline = character(0),
      UCL = character(0),
      stringsAsFactors = FALSE
    ))
  }
  required <- c("Sets", "LCL", "centerline", "UCL")
  missing <- setdiff(required, names(dat))
  if (length(missing) > 0L) {
    stop("spc_summarize_att_limits_by_set requires columns: ", paste(missing, collapse = ", "))
  }

  set_ids <- unique(dat$Sets)
  out <- lapply(set_ids, function(set_val) {
    rows <- dat[dat$Sets == set_val, , drop = FALSE]
    data.frame(
      Set = as.character(set_val),
      LCL = spc_format_att_limit_cell(rows$LCL, digits),
      Centerline = spc_format_att_limit_cell(rows$centerline, digits),
      UCL = spc_format_att_limit_cell(rows$UCL, digits),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

#' HTML fragment: control limits by set for attribute Limit Summary.
spc_build_att_limits_summary_html <- function(dat, digits = 4L) {
  summary_df <- spc_summarize_att_limits_by_set(dat, digits)
  if (nrow(summary_df) == 0L) {
    return("")
  }

  multi_set <- nrow(summary_df) > 1L
  parts <- c("<h4>Control Limits</h4>")

  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, , drop = FALSE]
    if (multi_set) {
      parts <- c(
        parts,
        "<table>",
        "<tr><td><u>Set ", row$Set, "</u></td><td></td></tr>",
        "</table>"
      )
    }
    parts <- c(
      parts,
      "<table>",
      "<tr><td>LCL:</td><td style='text-align:left'>", row$LCL, "</td></tr>",
      "<tr><td>Centerline:</td><td style='text-align:left'>", row$Centerline, "</td></tr>",
      "<tr><td>UCL:</td><td style='text-align:left'>", row$UCL, "</td></tr>",
      "</table>",
      "<br/>"
    )
  }

  parts
}
