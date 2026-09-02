# Shared quantile type helpers for EDA (Hyndman-Fan types 1-9).
# lolcat summary.impl uses stats::quantile() default (type 7).

DEFAULT_QUANTILE_TYPE <- 6L

QUANTILE_TYPE_DOCS_URL <- "https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/quantile"

QUANTILE_TYPE_DESCRIPTIONS <- c(
  "1" = "Inverse of the empirical distribution function (discontinuous).",
  "2" = "Similar to type 1, with averaging at discontinuities (discontinuous).",
  "3" = "SAS definition: nearest even order statistic (discontinuous).",
  "4" = "Linear interpolation of the empirical cdf; p<sub>k</sub> = k/n (continuous).",
  "5" = "Piecewise linear with knots midway through the empirical cdf steps; popular amongst hydrologists (continuous).",
  "6" = "p<sub>k</sub> = k/(n+1), i.e. E[F(x<sub>k</sub>)]; used by Minitab and SPSS (continuous). Default for stats4ROI EDA.",
  "7" = "p<sub>k</sub> = (k-1)/(n-1), i.e. mode[F(x<sub>k</sub>)]; default in R stats::quantile() (continuous).",
  "8" = "Approximately median-unbiased regardless of the distribution of x (continuous).",
  "9" = "Approximately unbiased for expected order statistics when x is normally distributed (continuous)."
)

normalize_quantile_type <- function(type) {
  t <- suppressWarnings(as.integer(type)[1L])
  if (length(t) == 0L || is.na(t) || t < 1L || t > 9L) {
    return(DEFAULT_QUANTILE_TYPE)
  }
  t
}

quantile_type_picker_choices <- function() {
  list(
    `Discontinuous sample quantiles` = c(
      "Type 1 - Inverse of EDF" = 1,
      "Type 2 - Averaging at discontinuities" = 2,
      "Type 3 - SAS (nearest even order statistic)" = 3
    ),
    `Continuous sample quantiles` = c(
      "Type 4 - Linear interpolation of EDF" = 4,
      "Type 5 - Hydrologists" = 5,
      "Type 6 - Minitab / SPSS" = 6,
      "Type 7 - R default" = 7,
      "Type 8 - Median-unbiased" = 8,
      "Type 9 - Normal-unbiased" = 9
    )
  )
}

quantile_type_description_html <- function(type) {
  type <- normalize_quantile_type(type)
  desc <- QUANTILE_TYPE_DESCRIPTIONS[[as.character(type)]]
  paste0(
    "<br><br><strong>Type ", type, ":</strong> ", desc,
    " <a href='", QUANTILE_TYPE_DOCS_URL, "' target='_blank'>Learn more here.</a>"
  )
}

QUANTILE_STAT_PATTERN <- "^stat\\.(median=T|q1=T|q3=T|quantile=|iqr=T|sir=T|psd=T)"

strip_eda_quantile_stats <- function(selected_stats) {
  selected_stats[!grepl(QUANTILE_STAT_PATTERN, selected_stats)]
}

# Backward-compatible alias
strip_type6_quantile_stats <- strip_eda_quantile_stats

parse_eda_quantile_requests <- function(selected_stats) {
  prob_cols <- list()
  if (any(grepl("^stat\\.median=T$", selected_stats))) {
    prob_cols[["median"]] <- 0.5
  }
  if (any(grepl("^stat\\.q1=T$", selected_stats))) {
    prob_cols[["quartile.1"]] <- 0.25
  }
  if (any(grepl("^stat\\.q3=T$", selected_stats))) {
    prob_cols[["quartile.3"]] <- 0.75
  }
  for (spec in grep("^stat\\.quantile=", selected_stats, value = TRUE)) {
    p <- suppressWarnings(as.numeric(sub("^stat\\.quantile=", "", spec)))
    if (!is.na(p)) {
      prob_cols[[paste0("percentile.", p * 100)]] <- p
    }
  }
  derived <- character(0)
  if (any(grepl("^stat\\.iqr=T$", selected_stats))) {
    derived <- c(derived, "iqr")
  }
  if (any(grepl("^stat\\.sir=T$", selected_stats))) {
    derived <- c(derived, "sir")
  }
  if (any(grepl("^stat\\.psd=T$", selected_stats))) {
    derived <- c(derived, "psd")
  }
  list(prob_cols = prob_cols, derived = derived)
}

parse_type6_quantile_requests <- parse_eda_quantile_requests

has_eda_quantile_stats <- function(selected_stats) {
  req <- parse_eda_quantile_requests(selected_stats)
  length(req$prob_cols) > 0 || length(req$derived) > 0
}

has_type6_quantile_stats <- has_eda_quantile_stats

compute_eda_quantile_values <- function(x, requests, type = DEFAULT_QUANTILE_TYPE) {
  type <- normalize_quantile_type(type)
  x <- x[!is.na(x)]
  cols <- c(names(requests$prob_cols), requests$derived)
  if (length(x) == 0) {
    return(setNames(rep(NA_real_, length(cols)), cols))
  }
  out <- list()
  for (nm in names(requests$prob_cols)) {
    out[[nm]] <- unname(stats::quantile(
      x,
      requests$prob_cols[[nm]],
      type = type,
      na.rm = TRUE
    ))
  }
  if (length(requests$derived) > 0) {
    q <- stats::quantile(x, c(0.25, 0.75), type = type, na.rm = TRUE)
    iqr <- unname(q[2] - q[1])
    if ("iqr" %in% requests$derived) {
      out$iqr <- iqr
    }
    if ("sir" %in% requests$derived) {
      out$sir <- iqr / 2
    }
    if ("psd" %in% requests$derived) {
      out$psd <- iqr / 1.35
    }
  }
  out
}

compute_type6_values <- function(x, requests) {
  compute_eda_quantile_values(x, requests, type = DEFAULT_QUANTILE_TYPE)
}

get_data_vector_for_output_row <- function(output, data, row_i, dep_name = NULL, group_cols = NULL) {
  if (!is.null(group_cols) && length(group_cols) > 0) {
    sub <- data
    for (gc in group_cols) {
      sub <- sub[sub[[gc]] == output[[gc]][row_i], , drop = FALSE]
    }
    return(sub[[dep_name]])
  }
  data[[output$dv.name[row_i]]]
}

apply_eda_quantiles <- function(
  output,
  data,
  selected_stats,
  dep_name = NULL,
  group_cols = NULL,
  type = DEFAULT_QUANTILE_TYPE
) {
  if (is.null(output) || nrow(output) == 0L || !has_eda_quantile_stats(selected_stats)) {
    return(output)
  }
  type <- normalize_quantile_type(type)
  requests <- parse_eda_quantile_requests(selected_stats)
  cols_to_set <- c(names(requests$prob_cols), requests$derived)
  for (col in cols_to_set) {
    output[[col]] <- NA_real_
  }
  for (i in seq_len(nrow(output))) {
    x <- get_data_vector_for_output_row(output, data, i, dep_name, group_cols)
    vals <- compute_eda_quantile_values(x, requests, type = type)
    for (col in names(vals)) {
      output[i, col] <- vals[[col]]
    }
  }
  output
}

apply_type6_quantiles <- function(output, data, selected_stats, dep_name = NULL, group_cols = NULL) {
  apply_eda_quantiles(output, data, selected_stats, dep_name, group_cols, type = DEFAULT_QUANTILE_TYPE)
}

percentile_column_names <- function(probs) {
  paste0("percentile.", probs * 100)
}

append_percentiles <- function(base_row, x, probs, type = DEFAULT_QUANTILE_TYPE) {
  type <- normalize_quantile_type(type)
  x_num <- suppressWarnings(as.numeric(x))
  x_clean <- x_num[!is.na(x_num)]
  if (length(x_clean) == 0L) {
    pct <- as.data.frame(as.list(rep(NA_real_, length(probs))), stringsAsFactors = FALSE)
  } else {
    q <- stats::quantile(x_clean, probs = probs, type = type, na.rm = TRUE)
    pct <- as.data.frame(t(as.numeric(q)), stringsAsFactors = FALSE)
  }
  names(pct) <- percentile_column_names(probs)
  cbind(base_row, pct)
}

append_percentiles_type6 <- function(base_row, x, probs) {
  append_percentiles(base_row, x, probs, type = DEFAULT_QUANTILE_TYPE)
}

compute_quantiles_column_mode <- function(quant_dat, probs, type = DEFAULT_QUANTILE_TYPE) {
  rows <- lapply(names(quant_dat), function(nm) {
    x_raw <- quant_dat[[nm]]
    x_num <- suppressWarnings(as.numeric(x_raw))
    base <- data.frame(
      dv.name = nm,
      n = sum(!is.na(x_num)),
      missing = sum(is.na(x_num)),
      stringsAsFactors = FALSE
    )
    append_percentiles(base, x_num, probs, type = type)
  })
  do.call(rbind, rows)
}

compute_quantiles_factor_mode <- function(data, dep_name, group_cols, probs, type = DEFAULT_QUANTILE_TYPE) {
  group_key <- if (length(group_cols) == 1L) {
    data[[group_cols]]
  } else {
    interaction(data[group_cols], drop = TRUE)
  }
  groups <- split(data, group_key, drop = TRUE)
  rows <- lapply(groups, function(sub) {
    x_raw <- sub[[dep_name]]
    x_num <- suppressWarnings(as.numeric(x_raw))
    base <- sub[1, group_cols, drop = FALSE]
    base$n <- sum(!is.na(x_num))
    base$missing <- sum(is.na(x_num))
    append_percentiles(base, x_num, probs, type = type)
  })
  do.call(rbind, rows)
}

compute_quantiles_pooled_all_column <- function(quant_dat, probs, label = POOLED_ALL_LABEL, type = DEFAULT_QUANTILE_TYPE) {
  x_num <- suppressWarnings(as.numeric(unlist(quant_dat, use.names = FALSE)))
  base <- data.frame(
    dv.name = label,
    n = sum(!is.na(x_num)),
    missing = sum(is.na(x_num)),
    stringsAsFactors = FALSE
  )
  append_percentiles(base, x_num, probs, type = type)
}

compute_quantiles_pooled_all_factor <- function(
  data,
  dep_name,
  group_cols,
  probs,
  label = POOLED_ALL_LABEL,
  type = DEFAULT_QUANTILE_TYPE
) {
  x_raw <- data[[dep_name]]
  x_num <- suppressWarnings(as.numeric(x_raw))
  base <- data.frame(matrix(label, nrow = 1L, ncol = length(group_cols)), stringsAsFactors = FALSE)
  names(base) <- group_cols
  base$n <- sum(!is.na(x_num))
  base$missing <- sum(is.na(x_num))
  append_percentiles(base, x_num, probs, type = type)
}
