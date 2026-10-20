# Helpers for prepending a pooled "All" summary row in EDA tables (2+ groups only).

POOLED_ALL_LABEL <- "All"

needs_pooled_all_row <- function(n_groups) {
  if (is.null(n_groups)) {
    return(FALSE)
  }
  n <- suppressWarnings(as.integer(n_groups)[1L])
  !is.na(n) && n >= 2L
}

pool_data_frame_columns <- function(dat) {
  # Convert each column through eda_safe_numeric before unlisting so that
  # factor columns are coerced via their labels (not internal level codes).
  converted <- lapply(dat, eda_safe_numeric)
  vals <- unlist(converted, use.names = FALSE)
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0L) {
    return(data.frame(All = numeric(0), stringsAsFactors = FALSE))
  }
  data.frame(
    All = vals,
    stringsAsFactors = FALSE
  )
}

pool_numeric_vector <- function(...) {
  # Convert each element (column or vector) through eda_safe_numeric before
  # unlisting so that factor columns have their labels parsed rather than
  # internal level codes returned.
  args <- list(...)
  converted <- lapply(args, function(x) {
    if (is.data.frame(x)) {
      unlist(lapply(x, eda_safe_numeric), use.names = FALSE)
    } else {
      eda_safe_numeric(x)
    }
  })
  vals <- unlist(converted, use.names = FALSE)
  vals[!is.na(vals)]
}

# Resolve dependent column for factor-mode tabs (supports both UI conventions:
# index into eda_UI2 selection, or global column index from radio choices).
resolve_factor_dependent_column <- function(data, data_col, eda_ui2) {
  data_col_num <- suppressWarnings(as.integer(data_col)[1L])
  selected_data_cols <- suppressWarnings(as.integer(eda_ui2))
  if (length(selected_data_cols) == 0L || is.na(data_col_num)) {
    return(list(dep_name = NULL, dep_col_index = NA_integer_))
  }
  dep_col_index <- if (data_col_num %in% selected_data_cols) {
    data_col_num
  } else if (data_col_num >= 1L && data_col_num <= length(selected_data_cols)) {
    selected_data_cols[data_col_num]
  } else {
    data_col_num
  }
  dep_name <- make.names(colnames(data)[dep_col_index])
  list(dep_name = dep_name, dep_col_index = dep_col_index)
}

coerce_bind_row_types <- function(all_row, main) {
  shared <- intersect(names(all_row), names(main))
  for (col in shared) {
    all_vals <- all_row[[col]]
    main_vals <- main[[col]]
    if (is.character(all_vals) && !is.character(main_vals)) {
      main[[col]] <- as.character(main_vals)
    } else if (is.numeric(main_vals) && (is.character(all_vals) || is.factor(all_vals))) {
      main[[col]] <- as.character(main_vals)
    } else if (!identical(class(all_vals), class(main_vals))) {
      converted <- suppressWarnings(
        tryCatch(
          methods::as(all_vals, class(main_vals)[1]),
          error = function(e) NULL
        )
      )
      if (is.null(converted)) {
        main[[col]] <- as.character(main_vals)
        all_row[[col]] <- as.character(all_vals)
      } else {
        all_row[[col]] <- converted
      }
    }
  }
  list(all_row = all_row, main = main)
}

prepend_rows_top <- function(all_row, main) {
  if (is.null(all_row) || nrow(all_row) == 0L) {
    return(main)
  }
  if (is.null(main) || nrow(main) == 0L) {
    return(all_row)
  }
  aligned <- coerce_bind_row_types(all_row, main)
  dplyr::bind_rows(aligned$all_row, aligned$main)
}

label_factor_group_row <- function(row, group_cols, label = POOLED_ALL_LABEL) {
  for (gc in group_cols) {
    if (gc %in% names(row)) {
      row[[gc]] <- label
    }
  }
  row
}

# Build lolcat stat.* test argument string from manual normality test selections.
build_normality_test_selection <- function(test) {
  test_sel <- paste(test, collapse = ",")
  if (!grepl("stat.ad.test=2", test_sel)) {
    test_sel <- c(test_sel, ",stat.ad.test=0")
    test_sel <- paste(test_sel, collapse = "")
  }
  if (!grepl("stat.sw.test=2", test_sel)) {
    test_sel <- c(test_sel, ",stat.sw.test=0")
    test_sel <- paste(test_sel, collapse = "")
  }
  if (!grepl("stat.skew.test=2", test_sel)) {
    test_sel <- c(test_sel, ",stat.skew.test=0")
    test_sel <- paste(test_sel, collapse = "")
  }
  if (!grepl("stat.kurt.test=2", test_sel)) {
    test_sel <- c(test_sel, ",stat.kurt.test=0")
    test_sel <- paste(test_sel, collapse = "")
  }
  test_sel
}

# Auto-select normality tests by sample size.
# lolcat defaults (stat.*=1) still run skew/kurt for n<25; unused tests must be set to 0.
# Rule: n < 25 → Anderson-Darling + Shapiro-Wilk only; n >= 25 → skewness + kurtosis only.
normality_auto_summary_all_variables <- function(clean_df) {
  n_valid <- sum(!is.na(clean_df[[1]]))
  if (n_valid < 25) {
    summary.all.variables(
      data = clean_df,
      stat.sd = TRUE,
      stat.ad.test = 2,
      stat.sw.test = 2,
      stat.skew.test = 0,
      stat.kurt.test = 0
    )
  } else {
    summary.all.variables(
      data = clean_df,
      stat.sd = TRUE,
      stat.ad.test = 0,
      stat.sw.test = 0,
      stat.skew.test = 2,
      stat.kurt.test = 2
    )
  }
}

# Factor-mode auto: pick per-group tests by that group's n (lolcat cannot do this in one call).
normality_factor_auto_summary <- function(model_text, data) {
  out_small <- summary.continuous(
    fx = model_text,
    data = data,
    stat.sd = TRUE,
    stat.ad.test = 2,
    stat.sw.test = 2,
    stat.skew.test = 0,
    stat.kurt.test = 0
  )
  out_large <- summary.continuous(
    fx = model_text,
    data = data,
    stat.sd = TRUE,
    stat.ad.test = 0,
    stat.sw.test = 0,
    stat.skew.test = 2,
    stat.kurt.test = 2
  )
  
  if (is.null(out_small) || nrow(out_small) == 0) {
    return(out_small)
  }
  
  test_stat_cols <- c(
    "adtest.AA", "adtest.p", "swtest.W", "swtest.p",
    "g3.skewness", "g3test.p", "g4.kurtosis", "g4test.p"
  )
  base_cols <- setdiff(union(names(out_small), names(out_large)), test_stat_cols)
  
  # Ensure both frames have the full test column set (NA-fill missing).
  for (col in test_stat_cols) {
    if (!col %in% names(out_small)) out_small[[col]] <- NA
    if (!col %in% names(out_large)) out_large[[col]] <- NA
  }
  
  out <- out_small[, c(intersect(base_cols, names(out_small)), test_stat_cols), drop = FALSE]
  # Prefer out_large base stats where available (same groups); keep group keys from small.
  for (col in intersect(base_cols, names(out_large))) {
    if (col %in% names(out) && col %in% names(out_large)) {
      out[[col]] <- out_large[[col]]
    }
  }
  # n comes from either; they should match — use small's n for the threshold decision.
  n_vec <- if ("n" %in% names(out_small)) out_small$n else rep(0, nrow(out_small))
  for (i in seq_len(nrow(out))) {
    if (!is.na(n_vec[i]) && n_vec[i] >= 25) {
      for (col in test_stat_cols) {
        out[[col]][i] <- out_large[[col]][i]
      }
    } else {
      for (col in test_stat_cols) {
        out[[col]][i] <- out_small[[col]][i]
      }
    }
  }
  # Drop all-NA test columns (when every group used the other family).
  keep_test <- vapply(test_stat_cols, function(col) any(!is.na(out[[col]])), logical(1))
  out[, c(intersect(base_cols, names(out)), test_stat_cols[keep_test]), drop = FALSE]
}

normality_pooled_summary <- function(pooled_dat, auto, test, output) {
  if (auto) {
    normality_auto_summary_all_variables(pooled_dat)
  } else {
    test_sel <- build_normality_test_selection(test)
    eval(parse(text = paste(
      "summary.all.variables(data = pooled_dat, stat.sd = T,",
      test_sel,
      ")"
    )))
  }
}

# Column-mode normality: optional pooled "All" series first (2+ columns).
normality_column_specs_list <- function(norm_dat) {
  specs <- lapply(seq_len(ncol(norm_dat)), function(i) {
    list(
      name = names(norm_dat)[i],
      data = norm_dat[, i, drop = FALSE],
      x = norm_dat[[i]]
    )
  })
  if (needs_pooled_all_row(ncol(norm_dat))) {
    pooled <- pool_data_frame_columns(norm_dat)
    if (nrow(pooled) > 0) {
      specs <- c(
        list(list(name = POOLED_ALL_LABEL, data = pooled, x = pooled[[1]])),
        specs
      )
    }
  }
  specs
}

normality_standardize_column_results <- function(results_list, col_specs) {
  if (length(results_list) == 0) {
    return(data.frame(Message = "No data to process"))
  }
  
  used_test_columns <- character(0)
  used_statistic_columns <- character(0)
  
  for (i in seq_along(results_list)) {
    result <- results_list[[i]]
    if (!is.null(result) && nrow(result) > 0) {
      test_cols <- c("adtest.p", "swtest.p", "g3test.p", "g4test.p")
      present_test_cols <- test_cols[test_cols %in% names(result)]
      used_test_columns <- unique(c(used_test_columns, present_test_cols))
      
      stat_cols <- c("adtest.AA", "swtest.W", "g3.skewness", "g4.kurtosis")
      present_stat_cols <- stat_cols[stat_cols %in% names(result)]
      used_statistic_columns <- unique(c(used_statistic_columns, present_stat_cols))
    }
  }
  
  standardized_results <- list()
  for (i in seq_along(results_list)) {
    spec <- col_specs[[i]]
    col_name <- spec$name
    result <- results_list[[i]]
    x_vec <- spec$x
    
    std_row <- data.frame(
      Variable = col_name,
      n = if ("n" %in% names(result)) result$n[1] else sum(!is.na(x_vec)),
      missing = if ("missing" %in% names(result)) result$missing[1] else sum(is.na(x_vec)),
      mean = if ("mean" %in% names(result)) result$mean[1] else NA,
      sd = if ("sd" %in% names(result)) result$sd[1] else NA
    )
    
    if ("adtest.AA" %in% used_statistic_columns) {
      std_row[["adtest.AA"]] <- if ("adtest.AA" %in% names(result)) result[["adtest.AA"]][1] else NA
    }
    if ("adtest.p" %in% used_test_columns) {
      std_row[["adtest.p"]] <- if ("adtest.p" %in% names(result)) result[["adtest.p"]][1] else NA
    }
    if ("swtest.W" %in% used_statistic_columns) {
      std_row[["swtest.W"]] <- if ("swtest.W" %in% names(result)) result[["swtest.W"]][1] else NA
    }
    if ("swtest.p" %in% used_test_columns) {
      std_row[["swtest.p"]] <- if ("swtest.p" %in% names(result)) result[["swtest.p"]][1] else NA
    }
    if ("g3.skewness" %in% used_statistic_columns) {
      std_row[["g3.skewness"]] <- if ("g3.skewness" %in% names(result)) result[["g3.skewness"]][1] else NA
    }
    if ("g3test.p" %in% used_test_columns) {
      std_row[["g3test.p"]] <- if ("g3test.p" %in% names(result)) result[["g3test.p"]][1] else NA
    }
    if ("g4.kurtosis" %in% used_statistic_columns) {
      std_row[["g4.kurtosis"]] <- if ("g4.kurtosis" %in% names(result)) result[["g4.kurtosis"]][1] else NA
    }
    if ("g4test.p" %in% used_test_columns) {
      std_row[["g4test.p"]] <- if ("g4test.p" %in% names(result)) result[["g4test.p"]][1] else NA
    }
    
    standardized_results[[i]] <- std_row
  }
  
  do.call(rbind, standardized_results)
}

prepend_normality_factor_all_row <- function(output, data, dep_name, group_cols, auto, test) {
  if (!needs_pooled_all_row(nrow(output))) {
    return(output)
  }
  
  dep_name_m <- make.names(dep_name)
  pooled_x <- eda_safe_numeric(data[[dep_name_m]])
  pooled_x <- pooled_x[!is.na(pooled_x)]
  if (length(pooled_x) == 0L) {
    return(output)
  }
  pooled_dat <- data.frame(All = pooled_x, stringsAsFactors = FALSE)
  
  tryCatch({
    pooled_result <- normality_pooled_summary(pooled_dat, auto, test, output)
    
    pooled_row <- output[1, , drop = FALSE]
    pooled_row[1, ] <- NA
    pooled_row <- label_factor_group_row(pooled_row, group_cols)
    
    shared_cols <- intersect(names(output), names(pooled_result))
    for (col in shared_cols) {
      if (col %in% group_cols) next
      pooled_row[[col]] <- pooled_result[[col]][1]
    }
    
    prepend_rows_top(pooled_row, output)
  }, error = function(e) {
    output
  })
}
