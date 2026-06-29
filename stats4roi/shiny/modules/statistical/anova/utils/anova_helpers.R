# ANOVA Helper Functions
# Utility functions for ANOVA calculations

library(dplyr)
library(stringr)

# =============================================================================
# BALANCE TEST FUNCTION
# =============================================================================
# Check if a design is balanced (equal sample sizes across factor combinations)
# Extracted from app_monolithic.R lines 29863-29879
balance_test <- function(factors_names, data) {
  # Test to see if unbalanced from EMSaov
  if (is.null(factors_names) || is.null(data)) {
    return(FALSE)
  }

  factors_names <- factors_names[!is.na(factors_names) & nzchar(factors_names)]
  factors_names <- intersect(factors_names, names(data))
  if (length(factors_names) == 0L) {
    return(FALSE)
  }
  
  EMSflag <- FALSE
  n.table <- NULL
  
  for(i in 1:length(factors_names)) {
    temp <- table(data[, factors_names[i]])
    if(sum(temp != mean(temp)) != 0) {
      EMSflag <- TRUE
    }
    n.table <- c(n.table, length(temp))
  }
  
  return(EMSflag)
}

#' Build nested-within parent spec for EMSanova (one entry per selected factor).
#' @param factors_names factor column names in model order
#' @param inputs_vals worker/coordinator input list (nest_factor1, ...)
#' @param mixed_nest logical; when FALSE returns NULL
#' @keywords internal
build_mf_nested_chr <- function(factors_names, inputs_vals, mixed_nest) {
  if (!isTRUE(mixed_nest)) {
    return(NULL)
  }
  nested <- rep("", length(factors_names))
  for (i in seq_along(factors_names)) {
    nest <- inputs_vals[[paste0("nest_factor", i)]]
    if (is.null(nest) || length(nest) == 0L) {
      nested[i] <- ""
    } else {
      nest <- intersect(as.character(nest), factors_names)
      if (length(nest) == 0L) {
        nested[i] <- ""
      } else if (length(nest) > 1L) {
        nested[i] <- paste(nest, collapse = "*")
      } else {
        nested[i] <- nest
      }
    }
  }
  nested
}

# =============================================================================
# ONE-WAY COLUMN VALIDATION / COERCION
# =============================================================================

ow_column_is_numericish <- function(x) {
  if (is.numeric(x)) {
    return(any(is.finite(x)))
  }
  if (is.logical(x)) {
    return(any(!is.na(x)))
  }
  if (is.character(x) || is.factor(x)) {
    num <- suppressWarnings(as.numeric(as.character(x)))
    return(sum(is.finite(num), na.rm = TRUE) >= 2L)
  }
  FALSE
}

ow_numeric_column_indices <- function(data, exclude = integer(0)) {
  idx <- seq_len(ncol(data))
  idx <- setdiff(idx, as.integer(exclude))
  idx[vapply(data[idx], ow_column_is_numericish, logical(1L), USE.NAMES = FALSE)]
}

ow_oneway_error_html <- function(message) {
  paste0("<p style='color:red;'><b>Oneway ANOVA:</b> ", message, "</p>")
}

ow_coerce_response_numeric <- function(x) {
  if (is.numeric(x)) {
    if (!any(is.finite(x))) {
      return(list(ok = FALSE, x = x, note = NULL, message = "The selected response column has no finite numeric values."))
    }
    return(list(ok = TRUE, x = as.numeric(x), note = NULL, message = NULL))
  }
  raw <- if (is.factor(x)) as.character(x) else x
  num <- suppressWarnings(as.numeric(raw))
  if (!any(is.finite(num))) {
    return(list(
      ok = FALSE,
      x = num,
      note = NULL,
      message = "The selected response column must be numeric (or numeric text). Choose a different column for Select Data."
    ))
  }
  note <- NULL
  if (any(is.na(num) & !is.na(raw))) {
    note <- "Some response values were not numeric and were treated as missing."
  }
  list(ok = TRUE, x = num, note = note, message = NULL)
}

prepare_oneway_analysis_data <- function(data, data_col, factor_col) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L) {
    return(list(ok = FALSE, message = "No data available for analysis."))
  }
  data_col <- as.integer(data_col)
  factor_col <- as.integer(factor_col)
  if (!is.finite(data_col) || !is.finite(factor_col)) {
    return(list(ok = FALSE, message = "Select a factor and a numeric response column."))
  }
  names(data) <- make.names(names(data))
  if (data_col < 1L || data_col > ncol(data) || factor_col < 1L || factor_col > ncol(data)) {
    return(list(ok = FALSE, message = "Invalid column selection."))
  }
  if (data_col == factor_col) {
    return(list(ok = FALSE, message = "Response and factor must be different columns."))
  }

  response_name <- names(data)[data_col]
  factor_name <- names(data)[factor_col]
  coerced <- ow_coerce_response_numeric(data[[data_col]])
  if (!coerced$ok) {
    return(list(ok = FALSE, message = coerced$message))
  }

  analysis_df <- data.frame(
    .response = coerced$x,
    .factor = as.factor(data[[factor_col]]),
    stringsAsFactors = FALSE
  )
  analysis_df <- analysis_df[is.finite(analysis_df$.response) & !is.na(analysis_df$.factor), , drop = FALSE]
  if (nrow(analysis_df) < 3L) {
    return(list(ok = FALSE, message = "Too few complete cases after converting the response to numeric."))
  }
  if (nlevels(analysis_df$.factor) < 2L) {
    return(list(ok = FALSE, message = "The factor must define at least two groups."))
  }

  list(
    ok = TRUE,
    data = analysis_df,
    response_name = response_name,
    factor_name = factor_name,
    note = coerced$note
  )
}

oneway_apply_dispersion_response <- function(data, response_col, factor_col, disp_type = c("ADA", "ADM", "ADMn1")) {
  disp_type <- match.arg(disp_type)
  names(data) <- make.names(names(data))
  form_c <- stats::as.formula(paste(names(data)[response_col], "~", names(data)[factor_col]))
  vec <- switch(
    disp_type,
    ADA = compute.group.dispersion.ADA(fx = form_c, data = data),
    ADM = compute.group.dispersion.ADM(fx = form_c, data = data),
    ADMn1 = compute.group.dispersion.ADMn1(fx = form_c, data = data)
  )
  col_name <- paste0(names(data)[response_col], "_", disp_type)
  data[[col_name]] <- as.numeric(vec)
  if (disp_type == "ADMn1") {
    data <- dplyr::filter(data, !is.na(.data[[col_name]]))
  }
  list(data = data, response_col_name = col_name)
}

#' Attach ADA/ADM/ADMn1 column for multifactor ANOVA refits (pooled / dummy-column paths).
#' @param formula_str Full formula string, e.g. \code{"cm1 ~ a + c + a:d"}.
#' @return List with \code{data} and \code{response_name} (raw or transformed column).
#' @keywords internal
mf_attach_multifactor_dispersion_column <- function(data, response_col, disp, disp_type_id, formula_str) {
  response_col <- as.integer(response_col)[1L]
  resp_nm <- if (is.finite(response_col) && response_col >= 1L && response_col <= ncol(data)) {
    names(data)[response_col]
  } else {
    NA_character_
  }
  if (!isTRUE(disp) || is.na(resp_nm) || !nzchar(resp_nm)) {
    return(list(data = data, response_name = resp_nm))
  }
  disp_type_id <- as.integer(disp_type_id)[1L]
  if (!is.finite(disp_type_id) || disp_type_id < 1L || disp_type_id > 3L) {
    disp_type_id <- 1L
  }
  formula_str <- as.character(formula_str)[1L]
  if (disp_type_id == 1L) {
    data$ADA <- compute.group.dispersion.ADA(formula(formula_str), data = data)
    out_col <- paste0(resp_nm, "_ADA")
    colnames(data)[colnames(data) == "ADA"] <- out_col
  } else if (disp_type_id == 2L) {
    data$ADM <- compute.group.dispersion.ADM(formula(formula_str), data = data)
    out_col <- paste0(resp_nm, "_ADM")
    colnames(data)[colnames(data) == "ADM"] <- out_col
  } else {
    data$ADMn1 <- compute.group.dispersion.ADMn1(formula(formula_str), data = data)
    out_col <- paste0(resp_nm, "_ADMn1")
    colnames(data)[colnames(data) == "ADMn1"] <- out_col
  }
  list(data = data, response_name = out_col)
}

#' TRUE when a column name is a multifactor dispersion response (ADA/ADM/ADMn1).
#' @keywords internal
mf_is_dispersion_response_name <- function(nm) {
  nm <- as.character(nm)[1L]
  isTRUE(grepl("_(ADA|ADM|ADMn1)$", nm))
}

#' Drop stale dispersion columns for a raw response before recomputing ADM/ADA.
#' @keywords internal
mf_strip_dispersion_response_columns <- function(data, raw_response) {
  raw_response <- as.character(raw_response)[1L]
  if (is.na(raw_response) || !nzchar(raw_response)) {
    return(as.data.frame(data))
  }
  data <- as.data.frame(data)
  pat <- paste0("^", gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", raw_response), "_(ADA|ADM|ADMn1)$")
  keep <- !grepl(pat, names(data))
  if (all(keep)) data else data[, keep, drop = FALSE]
}

#' Attach ADA/ADM/ADMn1 for a specific model formula (monolithic ems_pooled reduced-model path).
#' @return list(data, formula, response)
#' @keywords internal
mf_dispersion_transform_formula <- function(data, response_id, disp_type_id, formula_chr, disp_active) {
  response_id <- as.integer(response_id)[1L]
  raw_nm <- if (is.finite(response_id) && response_id >= 1L && response_id <= ncol(data)) {
    names(data)[response_id]
  } else {
    NA_character_
  }
  formula_chr <- as.character(formula_chr)[1L]
  if (!isTRUE(disp_active) || is.na(raw_nm) || !nzchar(raw_nm)) {
    return(list(data = data, formula = formula_chr, response = raw_nm))
  }
  disp_type_id <- as.integer(disp_type_id)[1L]
  if (!is.finite(disp_type_id) || disp_type_id < 1L || disp_type_id > 3L) {
    disp_type_id <- 1L
  }
  data <- as.data.frame(data)
  if (disp_type_id == 1L) {
    data$ADA <- compute.group.dispersion.ADA(formula(formula_chr), data = data)
    out_nm <- paste0(raw_nm, "_ADA")
    colnames(data)[colnames(data) == "ADA"] <- out_nm
  } else if (disp_type_id == 2L) {
    data$ADM <- compute.group.dispersion.ADM(formula(formula_chr), data = data)
    out_nm <- paste0(raw_nm, "_ADM")
    colnames(data)[colnames(data) == "ADM"] <- out_nm
  } else {
    data$ADMn1 <- compute.group.dispersion.ADMn1(formula(formula_chr), data = data)
    out_nm <- paste0(raw_nm, "_ADMn1")
    colnames(data)[colnames(data) == "ADMn1"] <- out_nm
  }
  list(
    data = data,
    formula = sub(raw_nm, out_nm, formula_chr, fixed = TRUE),
    response = out_nm
  )
}

#' Strip stale dispersion columns, then attach ADA/ADM/ADMn1 for \code{formula_chr}.
#' @keywords internal
mf_dispersion_prepare_data_formula <- function(data, response_id, disp_type_id, formula_chr, disp_active) {
  response_id <- as.integer(response_id)[1L]
  raw_nm <- if (is.finite(response_id) && response_id >= 1L && response_id <= ncol(data)) {
    names(data)[response_id]
  } else {
    NA_character_
  }
  if (!isTRUE(disp_active) || is.na(raw_nm) || !nzchar(raw_nm)) {
    return(list(data = data, formula = formula_chr, response = raw_nm))
  }
  data <- mf_strip_dispersion_response_columns(data, raw_nm)
  response_id <- match(raw_nm, names(data))
  mf_dispersion_transform_formula(
    data = data,
    response_id = response_id,
    disp_type_id = disp_type_id,
    formula_chr = formula_chr,
    disp_active = TRUE
  )
}

#' ANOVA notes bundled on a pooled/source table (keeps bindCache table + notes aligned).
#' @keywords internal
mf_anova_notes_from_table <- function(aov_table, fallback_notes = NULL) {
  notes <- attr(aov_table, "anova_note", exact = FALSE)
  if (!is.null(notes) && nzchar(as.character(notes)[1L])) {
    return(as.character(notes)[1L])
  }
  if (!is.null(fallback_notes) && nzchar(as.character(fallback_notes)[1L])) {
    return(as.character(fallback_notes)[1L])
  }
  NULL
}

#' Build a consistent dispersion response + data frame for pooled ANOVA refits
#' (reduced-model fallback and dummy-column e1/e2 paths).
#' @keywords internal
mf_ems_pooled_dispersion_context <- function(
    data,
    data_id,
    factors_id,
    factors_names,
    disp_active,
    disp_type_id,
    pooled_effects,
    available_effects) {
  data_id <- as.integer(data_id)[1L]
  raw_resp <- if (is.finite(data_id) && data_id >= 1L && data_id <= ncol(data)) {
    names(data)[data_id]
  } else {
    NA_character_
  }
  if (!isTRUE(disp_active) || is.na(raw_resp) || !nzchar(raw_resp)) {
    return(list(data = data, response = raw_resp, active = FALSE))
  }
  data <- as.data.frame(data)
  if (length(factors_names) >= 1L) {
    data[, factors_names] <- lapply(data[, factors_id], factor)
  }
  pooled_effects <- as.character(pooled_effects)
  available_effects <- as.character(available_effects)
  disp_factors <- tryCatch(
    dispersion_cell_factors(
      factors_names,
      pooled_effects = pooled_effects,
      available_effects = available_effects
    ),
    error = function(e) factors_names
  )
  formula_disp <- paste0(raw_resp, " ~ ", paste(disp_factors, collapse = " * "))
  attached <- mf_attach_multifactor_dispersion_column(
    data = data,
    response_col = data_id,
    disp = TRUE,
    disp_type_id = disp_type_id,
    formula_str = formula_disp
  )
  list(data = attached$data, response = attached$response_name, active = TRUE)
}

#' Response column for dispersion pooled refits; prefer the unpooled ADM/ADA fit when present.
#' @keywords internal
mf_dispersion_pooled_refit_response <- function(disp_refit, unpooled_aov_table = NULL) {
  if (!isTRUE(disp_refit$active) || !nzchar(disp_refit$response)) {
    return(disp_refit$response)
  }
  mod_u <- attr(unpooled_aov_table, "aov_model", exact = FALSE)
  if (!is.null(mod_u)) {
    resp_u <- tryCatch(all.vars(stats::formula(mod_u))[1L], error = function(e) NA_character_)
    if (!is.na(resp_u) && grepl("_(ADA|ADM|ADMn1)$", resp_u) && resp_u %in% names(disp_refit$data)) {
      return(resp_u)
    }
  }
  disp_refit$response
}

build_oneway_analysis_frame <- function(data, data_col, factor_col, analysis_disp, disp_type_id, type_ow) {
  type_ow <- as.integer(type_ow)[1L]
  if (type_ow == 3L) {
    analysis_disp <- FALSE
  }
  analysis_disp <- isTRUE(analysis_disp)

  prep <- prepare_oneway_analysis_data(data, data_col, factor_col)
  if (!isTRUE(prep$ok)) {
    return(list(ok = FALSE, message = prep$message))
  }

  work <- prep$data
  response_label <- prep$response_name
  factor_label <- prep$factor_name
  analysis <- "means"
  disp_metric <- NA_character_
  type_labels <- c("ADA", "ADM", "ADM<sub>(n-1)</sub>")
  header_suffix <- "Means Analysis"

  if (analysis_disp) {
    disp_type_id <- as.integer(disp_type_id)[1L]
    if (!is.finite(disp_type_id) || disp_type_id < 1L || disp_type_id > 3L) {
      disp_type_id <- 1L
    }
    disp_types <- c("ADA", "ADM", "ADMn1")
    disp_metric <- disp_types[disp_type_id]

    min_n <- min(table(work$.factor))
    if (min_n < 3L) {
      return(list(ok = FALSE, message = "Not enough samples within cell to calculate dispersion."))
    }

    named_df <- prep$data
    names(named_df) <- c(prep$response_name, prep$factor_name)
    transformed <- oneway_apply_dispersion_response(named_df, 1L, 2L, disp_metric)
    work <- data.frame(
      .response = as.numeric(transformed$data[[transformed$response_col_name]]),
      .factor = as.factor(transformed$data[[prep$factor_name]]),
      stringsAsFactors = FALSE
    )
    if (disp_metric == "ADMn1") {
      work <- work[is.finite(work$.response), , drop = FALSE]
    }
    if (nrow(work) < 3L || nlevels(work$.factor) < 2L) {
      return(list(ok = FALSE, message = "Too few complete cases after dispersion transformation."))
    }

    response_label <- transformed$response_col_name
    analysis <- "dispersion"
    header_suffix <- paste0("Dispersion Analysis based on ", type_labels[disp_type_id])
  }

  list(
    ok = TRUE,
    data = work,
    response_label = response_label,
    factor_label = factor_label,
    analysis = analysis,
    disp_metric = disp_metric,
    header_suffix = header_suffix,
    note = prep$note
  )
}

ow_oneway_aggregate_stats <- function(work) {
  sum_stats <- as.data.frame(as.matrix(stats::aggregate(
    work$.response,
    by = list(grp = work$.factor),
    FUN = function(x) c(mean = mean(x), n = length(x), var = var(x))
  )))
  list(
    group_labels = as.character(sum_stats$grp),
    cell_means = as.numeric(as.vector(as.matrix(sum_stats$x.mean))),
    cell_n = as.numeric(as.vector(as.matrix(sum_stats$x.n))),
    cell_var = as.numeric(as.vector(as.matrix(sum_stats$x.var)))
  )
}

oneway_posthoc_plot_labels <- function(analysis, disp_metric = NA_character_) {
  if (identical(analysis, "dispersion")) {
    metric_label <- switch(
      disp_metric,
      ADA = "ADA",
      ADM = "ADM",
      ADMn1 = "ADM[n-1]",
      disp_metric
    )
    list(
      title = paste0("Post-hoc: Dispersion (", metric_label, ")"),
      caption_points = paste0("Points are cell means of dispersion (", metric_label, ")")
    )
  } else {
    list(
      title = "Post-hoc: Means",
      caption_points = "Points are means"
    )
  }
}

oneway_posthoc_table_suffix <- function(analysis, disp_metric = NA_character_) {
  labels <- oneway_posthoc_plot_labels(analysis, disp_metric)
  paste0(" — ", labels$title)
}

# =============================================================================
# MULTIFACTOR ANOVA TABLE METRICS (omega^2, ICC, %RFC row selection)
# =============================================================================

#' Effect rows above the residual/error term (may include intercept).
anova_table_effect_rows <- function(aov_out_l, residual_row) {
  setdiff(rownames(aov_out_l), c(residual_row, "Within Cells", "Residual"))
}

#' Rows eligible for omega^2, ICC, and %RFC (excludes intercept and error).
anova_table_metric_rows <- function(aov_out_l, residual_row) {
  setdiff(anova_table_effect_rows(aov_out_l, residual_row), "(Intercept)")
}

#' Remove the intercept row from an ANOVA source table for UI display.
#' R² and other totals should be computed from the table *before* calling this.
anova_table_strip_intercept_for_display <- function(aov_table) {
  if (is.null(aov_table) || !is.data.frame(aov_table) || nrow(aov_table) < 1L) {
    return(aov_table)
  }
  rn <- rownames(aov_table)
  if (is.null(rn) || !("(Intercept)" %in% rn)) {
    return(aov_table)
  }
  aov_table[!(rn %in% "(Intercept)"), , drop = FALSE]
}

#' Omega-squared (%) per ANOVA source row for fixed-effect importance display.
#' Uses row names for indexing; excludes (Intercept) SS from the total used in the denominator.
anova_table_omega_squared_values <- function(aov_table, residual_row) {
  if (is.null(aov_table) || !is.data.frame(aov_table) || nrow(aov_table) < 1L) {
    return(numeric(0))
  }
  rn <- rownames(aov_table)
  if (is.null(rn) || length(rn) < 1L) {
    return(numeric(0))
  }
  res_nm <- as.character(residual_row)[1L]
  if (!res_nm %in% rn) {
    res_nm <- intersect(c("Residuals", "Residual", "Within Cells"), rn)
    if (length(res_nm) != 1L) {
      return(setNames(rep(0, length(rn)), rn))
    }
    res_nm <- res_nm[[1L]]
  }
  ss <- suppressWarnings(as.numeric(aov_table$SS))
  df <- suppressWarnings(as.numeric(aov_table$Df))
  if (length(ss) != length(rn) || length(df) != length(rn)) {
    return(setNames(rep(0, length(rn)), rn))
  }
  msw <- suppressWarnings(as.numeric(aov_table[res_nm, "MS"])[1L])
  if (!is.finite(msw)) {
    return(setNames(rep(0, length(rn)), rn))
  }
  sst <- sum(ss[is.finite(ss) & ss >= 0], na.rm = TRUE)
  if ("(Intercept)" %in% rn) {
    ss_int <- ss[match("(Intercept)", rn)]
    if (is.finite(ss_int)) {
      sst <- sst - ss_int
    }
  }
  denom <- sst + msw
  if (!is.finite(denom) || denom <= 0) {
    return(setNames(rep(0, length(rn)), rn))
  }
  omega <- 100 * (ss - (df * msw)) / denom
  omega[!is.finite(omega)] <- 0
  omega[omega < 0] <- 0
  stats::setNames(as.numeric(omega), rn)
}

# =============================================================================
# MULTIFACTOR %RFC (Percent Relative Factor Contribution)
# =============================================================================

#' Parse factor names from an ANOVA source-table row name.
parse_anova_effect_factors <- function(effect_name) {
  combo <- unlist(str_split(effect_name, ":", simplify = FALSE))
  combo <- gsub("\\([^)]+\\)", "", combo)
  combo <- trimws(combo)
  combo[combo != ""]
}

#' Replication counts per factorial cell (same grouping used for min_count < 3 dispersion gate).
#' Factors are coerced to factor so level coding is consistent before counting.
#' @keywords internal
factorial_cell_replication <- function(data, factors_names) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) < 1L || length(factors_names) < 1L) {
    return(list(
      min_count = NA_integer_,
      max_count = NA_integer_,
      n_cells = 0L,
      n_rows = if (is.null(data) || !is.data.frame(data)) 0L else nrow(data),
      count_per_cell = NULL
    ))
  }
  miss <- setdiff(factors_names, names(data))
  if (length(miss) > 0L) {
    return(list(
      min_count = NA_integer_,
      max_count = NA_integer_,
      n_cells = 0L,
      n_rows = nrow(data),
      count_per_cell = NULL
    ))
  }
  d <- data[, factors_names, drop = FALSE]
  for (fn in factors_names) {
    d[[fn]] <- droplevels(factor(d[[fn]]))
  }
  count_per_cell <- d %>%
    group_by(across(all_of(factors_names))) %>%
    summarize(count = n(), .groups = "drop")
  if (nrow(count_per_cell) < 1L) {
    return(list(
      min_count = NA_integer_,
      max_count = NA_integer_,
      n_cells = 0L,
      n_rows = nrow(data),
      count_per_cell = count_per_cell
    ))
  }
  mc <- min(count_per_cell$count)
  xc <- max(count_per_cell$count)
  list(
    min_count = mc,
    max_count = xc,
    n_cells = nrow(count_per_cell),
    n_rows = nrow(data),
    count_per_cell = count_per_cell
  )
}

#' Factors used to define factorial cells for ADA/ADM within-cell dispersion.
#' Cell factors follow the currently active model effects (all_effects - pooled effects).
#' @keywords internal
dispersion_cell_factors <- function(factors_names, pooled_effects = character(0), available_effects = NULL) {
  if (length(factors_names) < 1L) return(character(0))
  if (is.null(available_effects) || length(available_effects) < 1L) {
    return(factors_names)
  }
  kept <- setdiff(as.character(available_effects), as.character(pooled_effects))
  kept <- kept[!is.na(kept) & nzchar(kept)]
  if (length(kept) < 1L) return(factors_names)
  out <- unique(unlist(lapply(kept, parse_anova_effect_factors)))
  out <- intersect(out, factors_names)
  if (length(out) < 1L) factors_names else out
}

#' Formula string for within-cell ADA/ADM/ADMn1 using the active model cell factors.
#' @keywords internal
mf_dispersion_model_grouping_formula <- function(raw_response, factors_names, pooled_effects, available_effects) {
  raw_response <- as.character(raw_response)[1L]
  disp_factors <- tryCatch(
    dispersion_cell_factors(
      factors_names,
      pooled_effects = as.character(pooled_effects),
      available_effects = as.character(available_effects)
    ),
    error = function(e) factors_names
  )
  list(
    disp_factors = disp_factors,
    formula_chr = paste0(raw_response, " ~ ", paste(disp_factors, collapse = " * "))
  )
}

#' TRUE when a dispersion response column has usable variation for ANOVA.
#' @keywords internal
mf_dispersion_response_usable <- function(y) {
  y <- suppressWarnings(as.numeric(y))
  y <- y[is.finite(y)]
  length(y) >= 3L && is.finite(stats::sd(y)) && stats::sd(y) > .Machine$double.eps
}

#' Build reduced-model \code{lm()} RHS parts from an ANOVA table (Graphs / registry parity).
#'
#' @param random_factor_names Optional random factor names to drop from significance (mixed models).
#' @param eff_tbl Optional effect-type table for \code{multifactor_filter_fixed_anova_effects}.
#' @return Character vector of formula terms, \code{character(0)} when none are significant,
#'   or \code{NULL} when the table cannot be read.
#' @keywords internal
multifactor_reduced_rhs_parts_from_anova <- function(
    aov_out_l,
    conf,
    random_factor_names = character(0),
    eff_tbl = NULL) {
  if (!is.data.frame(aov_out_l) || nrow(aov_out_l) < 1L) {
    return(NULL)
  }
  pc <- if ("Pvalue" %in% names(aov_out_l)) {
    "Pvalue"
  } else if ("Pr(>F)" %in% names(aov_out_l)) {
    "Pr(>F)"
  } else {
    nm <- names(aov_out_l)[grepl("^Pr\\(", names(aov_out_l))][1]
    if (length(nm) == 1L && !is.na(nm)) nm else NULL
  }
  if (is.null(pc)) {
    return(NULL)
  }

  aov2 <- aov_out_l
  rn <- rownames(aov2)
  if ("(Intercept)" %in% rn) {
    aov2 <- aov2[rn != "(Intercept)", , drop = FALSE]
    rn <- rownames(aov2)
  }
  keep <- !(rn %in% c("Residuals", "Residual", "Within Cells"))
  aov2 <- aov2[keep, , drop = FALSE]
  if (nrow(aov2) == 0L) {
    return(NULL)
  }

  pv <- suppressWarnings(as.numeric(as.character(aov2[[pc]])))
  sig_rn <- rownames(aov2)[!is.na(pv) & pv <= (1 - conf)]
  sig_rn <- sig_rn[sig_rn != "NA"]
  if (length(sig_rn) == 0L) {
    return(character(0))
  }

  if (length(random_factor_names) > 0L || (!is.null(eff_tbl) && is.data.frame(eff_tbl))) {
    sig_rn <- multifactor_filter_fixed_anova_effects(
      sig_rn,
      eff_tbl = eff_tbl,
      random_factor_names = random_factor_names
    )
  }
  if (length(sig_rn) == 0L) {
    return(character(0))
  }

  interaction_factors <- grep(":", sig_rn, value = TRUE)
  individual_factors <- unique(unlist(strsplit(interaction_factors, ":", fixed = TRUE)))
  filtered_factors <- individual_factors
  if (is.null(filtered_factors)) filtered_factors <- character(0)
  all_sig_interactions <- sig_rn[grepl(":", sig_rn)]
  sig_main <- setdiff(setdiff(sig_rn, interaction_factors), filtered_factors)

  rhs_parts <- character(0)
  if (length(filtered_factors) > 0L) rhs_parts <- c(rhs_parts, filtered_factors)
  if (length(all_sig_interactions) > 0L) {
    rhs_parts <- c(
      rhs_parts,
      vapply(all_sig_interactions, anova_effect_to_formula_term, character(1))
    )
  }
  if (length(sig_main) > 0L) {
    rhs_parts <- c(rhs_parts, vapply(sig_main, anova_effect_to_formula_term, character(1)))
  }
  unique(rhs_parts)
}

#' Means ANOVA table for reduced-model commit/registry (pooled when means pooling is active).
#'
#' @param worker Multifactor worker list with \code{aov_out_means} and \code{ems_pooled_means}.
#' @param pool_mean Applied means-pooling effect names for the active response.
#' @keywords internal
mf_means_anova_for_reduced_model <- function(worker, pool_mean = character(0)) {
  pool_mean <- as.character(pool_mean)
  pool_mean <- pool_mean[nzchar(pool_mean)]
  if (length(pool_mean) > 0L && !is.null(worker$ems_pooled_means)) {
    return(worker$ems_pooled_means())
  }
  worker$aov_out_means()
}

#' Dispersion ANOVA table for reduced-model commit/registry (pooled when dispersion pooling is active).
#'
#' @param worker Multifactor worker list with \code{aov_out_dispersion} and \code{ems_pooled_dispersion}.
#' @param pool_disp Applied dispersion-pooling effect names for the active response.
#' @keywords internal
mf_disp_anova_for_reduced_model <- function(worker, pool_disp = character(0)) {
  pool_disp <- as.character(pool_disp)
  pool_disp <- pool_disp[nzchar(pool_disp)]
  if (length(pool_disp) > 0L && !is.null(worker$ems_pooled_dispersion)) {
    return(worker$ems_pooled_dispersion())
  }
  worker$aov_out_dispersion()
}

#' Convert an ANOVA source-table effect name to an lm() formula term.
anova_effect_to_formula_term <- function(effect_name) {
  effect_name <- trimws(effect_name)
  if (is_nested_anova_effect(effect_name)) {
    parsed <- parse_nested_effect(effect_name)
    return(paste(c(parsed$parents, parsed$child), collapse = ":"))
  }
  if (grepl(":", effect_name, fixed = TRUE)) {
    return(gsub(":", "*", effect_name, fixed = TRUE))
  }
  effect_name
}

#' Build an emmeans formula RHS for the significant-effects plot.
anova_effect_to_emmeans_rhs <- function(effect_name) {
  effect_name <- trimws(effect_name)
  if (is_nested_anova_effect(effect_name)) {
    return(nested_effect_emmeans_rhs(effect_name))
  }
  if (grepl("^[^:]*:[^:]*$", effect_name)) {
    factors <- strsplit(effect_name, ":", fixed = TRUE)[[1]]
    return(paste(factors[1], "~", factors[2]))
  }
  paste0("~ ", effect_name)
}

#' Primary data-column name used to label a plotted ANOVA effect.
anova_effect_plot_factor <- function(effect_name) {
  factors <- parse_anova_effect_factors(effect_name)
  if (length(factors) == 0L) {
    return(effect_name)
  }
  factors[[1]]
}

#' TRUE when an ANOVA source-table row name denotes a nested effect (e.g. B(A), C(B*A)).
is_nested_anova_effect <- function(effect_name) {
  grepl("\\([^)]+\\)", effect_name, perl = TRUE)
}

#' Parse nested effect name into child factor and parent factor names.
parse_nested_effect <- function(effect_name) {
  effect_name <- trimws(effect_name)
  if (!is_nested_anova_effect(effect_name)) {
    return(NULL)
  }
  parents_str <- sub(".*\\(([^)]+)\\).*", "\\1", effect_name)
  child <- trimws(sub("\\(.*", "", effect_name))
  parents <- trimws(strsplit(parents_str, "*", fixed = TRUE)[[1]])
  list(child = child, parents = parents)
}

#' Human-readable nested effect title (e.g. "B within A" or "C within B, A").
nested_effect_title <- function(effect_name) {
  parsed <- parse_nested_effect(effect_name)
  if (is.null(parsed)) {
    return(effect_name)
  }
  parent_label <- paste(parsed$parents, collapse = ", ")
  paste0(parsed$child, " within ", parent_label)
}

#' emmeans RHS for nested effects, including multi-parent nesting.
nested_effect_emmeans_rhs <- function(effect_name) {
  parsed <- parse_nested_effect(effect_name)
  if (is.null(parsed)) {
    return(anova_effect_to_emmeans_rhs(effect_name))
  }
  if (length(parsed$parents) == 1L) {
    return(paste0("~ ", parsed$child, " | ", parsed$parents[[1]]))
  }
  parent_rhs <- paste(parsed$parents, collapse = " + ")
  paste0("~ ", parsed$child, " | ", parent_rhs)
}

#' All observed parent-level combinations for a nested effect.
nested_parent_combinations <- function(data, parent_factors) {
  if (length(parent_factors) == 0L) {
    return(data.frame())
  }
  combos <- unique(data[, parent_factors, drop = FALSE])
  rownames(combos) <- NULL
  combos
}

#' Label for one parent stratum row (e.g. "B (A = 1)" or "C (B = b1, A = a1)").
nested_stratum_label <- function(child, parent_row, parent_factors) {
  parent_parts <- vapply(parent_factors, function(p) {
    paste0(p, " = ", parent_row[[1, p]])
  }, character(1))
  paste0(child, " (", paste(parent_parts, collapse = ", "), ")")
}

anova_residual_row <- function(aov_out_l) {
  rows <- intersect(rownames(aov_out_l), c("Residuals", "Residual", "Within Cells"))
  if (length(rows) == 0L) {
    rownames(aov_out_l)[nrow(aov_out_l)]
  } else {
    rows[[1]]
  }
}

#' Within-stratum F tests for a nested fixed effect.
#' Numerator: one-way SS/MS for child within each parent stratum.
#' Denominator: residual MS/df from the full nested ANOVA table.
compute_nested_stratum_tests <- function(
  effect_name,
  data,
  response_col,
  aov_out_l,
  conf = 0.95,
  residual_rows = c("Residuals", "Residual", "Within Cells")
) {
  parsed <- parse_nested_effect(effect_name)
  if (is.null(parsed)) {
    return(NULL)
  }

  child <- parsed$child
  parents <- parsed$parents
  if (!(child %in% names(data)) || !all(parents %in% names(data))) {
    return(NULL)
  }
  if (!(effect_name %in% rownames(aov_out_l))) {
    return(NULL)
  }

  residual_row <- anova_residual_row(aov_out_l)
  ms_den <- as.numeric(aov_out_l[residual_row, "MS"])
  df_den <- as.numeric(aov_out_l[residual_row, "Df"])
  if (!is.finite(ms_den) || ms_den <= 0 || !is.finite(df_den) || df_den <= 0) {
    return(NULL)
  }

  overall <- data.frame(
    Source = effect_name,
    SS = as.numeric(aov_out_l[effect_name, "SS"]),
    Df = as.numeric(aov_out_l[effect_name, "Df"]),
    MS = as.numeric(aov_out_l[effect_name, "MS"]),
    Fvalue = as.numeric(aov_out_l[effect_name, "Fvalue"]),
    Pvalue = as.numeric(aov_out_l[effect_name, "Pvalue"]),
    Notes = "From EMS ANOVA",
    row_type = "overall",
    stringsAsFactors = FALSE
  )

  parent_combos <- nested_parent_combinations(data, parents)
  if (nrow(parent_combos) == 0L) {
    return(overall)
  }

  response_name <- names(data)[response_col]
  stratum_rows <- vector("list", nrow(parent_combos))

  for (i in seq_len(nrow(parent_combos))) {
    parent_vals <- parent_combos[i, , drop = FALSE]
    subset_data <- data
    for (p in parents) {
      subset_data <- subset_data[subset_data[[p]] == parent_vals[[1, p]], , drop = FALSE]
    }
    if (nrow(subset_data) < 2L || length(unique(subset_data[[child]])) < 2L) {
      next
    }

    fit <- stats::aov(
      stats::as.formula(paste(response_name, "~", child)),
      data = subset_data
    )
    sm <- summary(fit)[[1]]
    sm_rows <- trimws(rownames(sm))
    child_row <- which(sm_rows == child)
    if (length(child_row) != 1L) {
      next
    }

    ss_num <- as.numeric(sm["Sum Sq"][[1]][child_row])
    df_num <- as.numeric(sm["Df"][[1]][child_row])
    ms_num <- as.numeric(sm["Mean Sq"][[1]][child_row])
    f_val <- ms_num / ms_den
    p_val <- stats::pf(f_val, df_num, df_den, lower.tail = FALSE)

    stratum_rows[[i]] <- data.frame(
      Source = nested_stratum_label(child, parent_vals, parents),
      SS = ss_num,
      Df = df_num,
      MS = ms_num,
      Fvalue = f_val,
      Pvalue = p_val,
      Notes = "Within-stratum test",
      row_type = "stratum",
      stringsAsFactors = FALSE
    )
  }

  stratum_df <- do.call(rbind, c(list(overall), stratum_rows[!vapply(stratum_rows, is.null, logical(1))]))
  if (is.null(stratum_df) || nrow(stratum_df) == 0L) {
    return(overall)
  }

  alpha <- 1 - conf
  stratum_df$Sig <- ifelse(
    stratum_df$row_type == "overall" & !is.na(stratum_df$Pvalue) & stratum_df$Pvalue < alpha,
    "*",
    ifelse(
      stratum_df$row_type == "stratum" & !is.na(stratum_df$Pvalue) & stratum_df$Pvalue < alpha,
      "*",
      ""
    )
  )
  stratum_df$Denominator <- ifelse(
    stratum_df$row_type == "stratum",
    paste0("Residual MS = ", signif(ms_den, 4), " (df = ", df_den, ")"),
    ""
  )
  stratum_df
}

#' Count facet panels needed for a nested effect plot.
nested_effect_panel_count <- function(data, effect_name) {
  parsed <- parse_nested_effect(effect_name)
  if (is.null(parsed)) {
    return(0L)
  }
  nrow(nested_parent_combinations(data, parsed$parents))
}

anova_pvalue_numeric <- function(pvalue) {
  suppressWarnings(as.numeric(gsub("<", "", as.character(pvalue), fixed = TRUE)))
}

#' Compute %RFC per row: numeric values on fixed main-effect rows only (NA elsewhere).
compute_percent_rfc <- function(
  aov_out_l,
  effects_f_r,
  conf,
  residual_rows = c("Residuals", "Residual", "Within Cells")
) {
  row_names <- rownames(aov_out_l)
  rfc_values <- rep(NA_real_, length(row_names))
  names(rfc_values) <- row_names

  residual_row <- intersect(row_names, residual_rows)
  if (length(residual_row) == 0L) {
    residual_row <- row_names[length(row_names)]
  } else {
    residual_row <- residual_row[1L]
  }

  effect_names <- anova_table_metric_rows(aov_out_l, residual_row)
  if (length(effect_names) == 0L) {
    return(rfc_values)
  }

  omega <- anova_table_omega_squared_values(aov_out_l, residual_row)

  pvals <- anova_pvalue_numeric(aov_out_l$Pvalue)
  names(pvals) <- row_names
  sig <- !is.na(pvals) & pvals < (1 - conf)
  names(sig) <- row_names

  main_effects <- effect_names[!grepl(":", effect_names, fixed = TRUE)]
  if (length(main_effects) == 0L) {
    return(rfc_values)
  }

  type_codes <- effects_f_r[main_effects, "type_code", drop = TRUE]
  fixed_mains <- main_effects[type_codes == 1]

  for (fac in fixed_mains) {
    total <- 0
    for (eff in effect_names) {
      factors_in_eff <- parse_anova_effect_factors(eff)
      if (!(fac %in% factors_in_eff)) {
        next
      }
      if (identical(eff, fac)) {
        if (isTRUE(sig[[eff]])) {
          total <- total + omega[[eff]]
        }
      } else if (isTRUE(sig[[eff]])) {
        K <- length(factors_in_eff)
        total <- total + omega[[eff]] / K
      }
    }
    rfc_values[[fac]] <- total
  }

  rfc_values
}

#' Append optional %RFC column id after imp in ANOVA table column list.
ems_anova_append_rfc_column <- function(out_col, show_rfc) {
  if (!isTRUE(show_rfc)) {
    return(out_col)
  }
  append(out_col, "rfc", after = match("imp", out_col))
}

#' HTML header cell for the optional %RFC column.
ems_anova_rfc_header <- function() {
  "<th style='text-align:center'><b>%RFC</b></th>"
}

#' Format numeric %RFC vector into a character column aligned to aov_out_l rows.
format_anova_rfc_column <- function(rfc_numeric, R, ro_fun = ro) {
  rfc_col <- rep("", length(rfc_numeric))
  names(rfc_col) <- names(rfc_numeric)
  for (nm in names(rfc_numeric)) {
    val <- rfc_numeric[[nm]]
    if (!is.na(val)) {
      rfc_col[[nm]] <- paste0(as.character(ro_fun(val, R)), "%")
    }
  }
  rfc_col
}

#' R-squared statistics from a fitted lm/aov model (NULL if unavailable).
lm_r_squared <- function(model) {
  if (is.null(model)) {
    return(NULL)
  }
  sm <- tryCatch(stats::summary.lm(model), error = function(e) NULL)
  if (is.null(sm)) {
    return(NULL)
  }
  c(r.squared = sm$r.squared, adj.r.squared = sm$adj.r.squared)
}

#' Residual row name used in multifactor ANOVA source tables.
anova_table_residual_row <- function(aov_table) {
  residual_rows <- c("Residuals", "Residual", "Within Cells")
  hit <- intersect(residual_rows, rownames(aov_table))
  if (length(hit) == 0L) {
    return(NA_character_)
  }
  hit[[1L]]
}

#' R-squared from an ANOVA source table (matches the displayed SS partition).
anova_table_r_squared <- function(aov_table, n_obs = NULL) {
  if (is.null(aov_table) || !is.data.frame(aov_table) || nrow(aov_table) < 2L) {
    return(NULL)
  }
  if (!all(c("SS", "Df") %in% names(aov_table))) {
    return(NULL)
  }

  residual_row <- anova_table_residual_row(aov_table)
  if (is.na(residual_row)) {
    return(NULL)
  }

  ss <- suppressWarnings(as.numeric(aov_table$SS))
  df <- suppressWarnings(as.numeric(aov_table$Df))
  if (any(is.na(ss)) || any(is.na(df))) {
    return(NULL)
  }

  sst <- sum(ss, na.rm = TRUE)
  resid_idx <- match(residual_row, rownames(aov_table))
  ssr <- ss[[resid_idx]]
  dfr <- df[[resid_idx]]
  if (!is.finite(sst) || sst <= 0 || !is.finite(ssr) || ssr < 0) {
    return(NULL)
  }

  r2 <- 1 - ssr / sst
  adj_r2 <- NA_real_
  if (is.finite(dfr) && dfr > 0 && is.finite(n_obs) && n_obs > dfr) {
    adj_r2 <- 1 - (1 - r2) * (n_obs - 1) / dfr
  }

  c(r.squared = r2, adj.r.squared = adj_r2)
}

#' HTML footer with r-squared and adjusted r-squared (multifactor ANOVA tables).
ems_r_squared_table_html <- function(model = NULL, R, aov_table = NULL, n_obs = NULL) {
  rs <- if (!is.null(aov_table)) {
    anova_table_r_squared(aov_table, n_obs)
  } else {
    lm_r_squared(model)
  }
  if (is.null(rs)) {
    return("")
  }

  adj_label <- if (is.finite(rs[["adj.r.squared"]])) {
    ro(rs[["adj.r.squared"]], R)
  } else {
    ""
  }

  paste0(
    "<br><table><tr><td style='text-align:right'>",
    paste(withMathJax("$r^2=$"), ro(rs[["r.squared"]], R), "</td><td></td>",
          "<td>", withMathJax("$r_{adj}^2=$"), adj_label),
    "</td></tr></table>"
  )
}

# =============================================================================
# MULTIFACTOR FIXED / RANDOM (Graphs + target settings)
# =============================================================================

#' Split selected model factors into fixed vs random from Set Up inputs.
#' @keywords internal
multifactor_factor_types_from_inputs <- function(factor_names, inputs_vals, n_factors = length(factor_names)) {
  factor_names <- as.character(factor_names)
  if (length(factor_names) < 1L || !isTRUE(inputs_vals$ems_show_mixed_nest)) {
    return(list(fixed = factor_names, random = character(0)))
  }
  n_factors <- as.integer(n_factors)
  if (!is.finite(n_factors) || n_factors < 1L) {
    n_factors <- length(factor_names)
  }
  random <- character(0)
  for (i in seq_len(min(n_factors, length(factor_names)))) {
    fr <- inputs_vals[[paste0("f_r_factor", i)]]
    if (identical(as.character(fr)[1], "R")) {
      random <- c(random, factor_names[i])
    }
  }
  list(fixed = setdiff(factor_names, random), random = random)
}

#' Keep only ANOVA effects classified as fixed (marginal over random factors in lm EMMs).
#' @keywords internal
multifactor_filter_fixed_anova_effects <- function(effects, eff_tbl, random_factor_names = character(0)) {
  if (length(effects) < 1L) {
    return(effects)
  }
  effects[vapply(effects, function(eff) {
    if (!is.null(eff_tbl) && eff %in% rownames(eff_tbl)) {
      return(identical(eff_tbl[eff, "type"], "F"))
    }
    parts <- parse_anova_effect_factors(eff)
    !any(parts %in% random_factor_names)
  }, logical(1))]
}

# =============================================================================
# MULTIFACTOR TARGET SETTINGS (discrete + continuous factors)
# =============================================================================

#' Factor names appearing in a reduced multifactor lm model.
multifactor_model_factor_names <- function(model) {
  if (is.null(model)) {
    return(character(0))
  }
  labels <- attr(model$terms, "term.labels")
  if (is.null(labels) || length(labels) == 0L) {
    return(character(0))
  }
  unique(unlist(strsplit(labels, ":", fixed = TRUE)))
}

#' emmeans specs formula for the full factorial grid of model factors.
multifactor_emmeans_specs <- function(model) {
  fns <- multifactor_model_factor_names(model)
  if (length(fns) == 0L) {
    return(NULL)
  }
  stats::as.formula(paste("~", paste(fns, collapse = " * ")))
}

multifactor_emmeans_summary <- function(model) {
  specs <- multifactor_emmeans_specs(model)
  if (is.null(specs)) {
    return(data.frame())
  }
  df <- as.data.frame(summary(emmeans::emmeans(object = model, specs = specs)), stringsAsFactors = FALSE)
  df
}

multifactor_drop_emm_interval_cols <- function(df) {
  if (is.null(df) || nrow(df) == 0L) {
    return(df)
  }
  interval_cols <- c("SE", "df", "lower.CL", "upper.CL")
  drop_idx <- which(names(df) %in% interval_cols)
  if (length(drop_idx) == 0L) {
    return(df)
  }
  df[, -drop_idx, drop = FALSE]
}

multifactor_factor_numeric_levels <- function(data, factor_name) {
  x <- data[[factor_name]]
  if (is.null(x)) {
    return(numeric(0))
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  sort(unique(as.numeric(x)))
}

multifactor_emmean_lookup <- function(emm_df, setting) {
  if (nrow(emm_df) == 0L) {
    return(NA_real_)
  }
  idx <- rep(TRUE, nrow(emm_df))
  for (nm in names(setting)) {
    idx <- idx & as.character(emm_df[[nm]]) == as.character(setting[[nm]])
  }
  hits <- which(idx)
  if (length(hits) != 1L) {
    return(NA_real_)
  }
  as.numeric(emm_df$emmean[hits])
}

multifactor_multilinear_pred <- function(x, lo, hi, corner_emmeans) {
  k <- length(lo)
  n_vert <- 2L^k
  pred <- 0
  for (i in seq_len(n_vert)) {
    w <- 1
    for (j in seq_len(k)) {
      use_hi <- bitwAnd(as.integer(i - 1L), bitwShiftL(1L, j - 1L)) != 0L
      if (isTRUE(use_hi)) {
        w <- w * (x[j] - lo[j]) / (hi[j] - lo[j])
      } else {
        w <- w * (hi[j] - x[j]) / (hi[j] - lo[j])
      }
    }
    pred <- pred + w * corner_emmeans[i]
  }
  pred
}

multifactor_box_corner_emmeans <- function(emm_df, cont_factors, lo_vals, hi_vals, discrete_setting) {
  k <- length(cont_factors)
  n_vert <- 2L^k
  corners <- numeric(n_vert)
  for (i in seq_len(n_vert)) {
    setting <- discrete_setting
    for (j in seq_len(k)) {
      f <- cont_factors[j]
      use_hi <- bitwAnd(as.integer(i - 1L), bitwShiftL(1L, j - 1L)) != 0L
      setting[[f]] <- if (isTRUE(use_hi)) hi_vals[[f]] else lo_vals[[f]]
    }
    corners[i] <- multifactor_emmean_lookup(emm_df, setting)
  }
  corners
}

multifactor_find_box_candidates <- function(lo, hi, corners, target, tol) {
  k <- length(lo)
  if (any(is.na(corners))) {
    return(list())
  }
  corner_preds <- corners
  min_pred <- min(corner_preds)
  max_pred <- max(corner_preds)

  candidates <- list()
  add_candidate <- function(x, pred) {
    if (!is.finite(pred) || abs(pred - target) > tol) {
      return(invisible(NULL))
    }
    candidates[[length(candidates) + 1L]] <<- list(x = x, pred = pred)
    invisible(NULL)
  }

  if (k == 1L && min_pred <= target && target <= max_pred &&
      abs(max_pred - min_pred) > .Machine$double.eps) {
    x_star <- lo[1] + (target - min_pred) * (hi[1] - lo[1]) / (max_pred - min_pred)
    add_candidate(x_star, target)
  }

  start <- (lo + hi) / 2
  opt <- tryCatch(
    stats::optim(
      par = start,
      fn = function(x) (multifactor_multilinear_pred(x, lo, hi, corners) - target)^2,
      method = "L-BFGS-B",
      lower = lo,
      upper = hi
    ),
    error = function(e) NULL
  )
  if (!is.null(opt) && opt$convergence == 0L) {
    pred <- multifactor_multilinear_pred(opt$par, lo, hi, corners)
    add_candidate(opt$par, pred)
  }

  grid_pts <- as.matrix(expand.grid(rep(list(c(0, 1)), k)))
  for (r in seq_len(nrow(grid_pts))) {
    x <- lo
    corner_idx <- 0L
    for (j in seq_len(k)) {
      if (grid_pts[r, j] == 1L) {
        x[j] <- hi[j]
        corner_idx <- corner_idx + bitwShiftL(1L, j - 1L)
      }
    }
    add_candidate(x, corner_preds[corner_idx + 1L])
  }

  candidates
}

multifactor_settings_row_key <- function(row, factor_names, decimals) {
  paste(vapply(
    factor_names,
    function(f) {
      val <- row[[f]]
      if (is.numeric(val)) {
        format(round(val, decimals), trim = TRUE, scientific = FALSE)
      } else {
        as.character(val)
      }
    },
    character(1)
  ), collapse = "|")
}

#' Parse comma-separated actual values for factor levels (must match level count).
multifactor_parse_actual_values <- function(text, n_levels) {
  if (!is.character(text) || length(text) != 1L || !nzchar(trimws(text))) {
    return(NULL)
  }
  parts <- trimws(strsplit(text, ",", fixed = TRUE)[[1L]])
  parts <- parts[nzchar(parts)]
  if (length(parts) != n_levels) {
    return(NULL)
  }
  vals <- suppressWarnings(as.numeric(parts))
  if (any(is.na(vals))) {
    return(NULL)
  }
  vals
}

#' Map a coded factor setting to its actual process value via piecewise-linear interpolation.
multifactor_coded_to_actual <- function(coded, coded_levels, actual_levels) {
  if (length(coded_levels) != length(actual_levels) || length(coded_levels) < 1L) {
    return(NA_real_)
  }
  if (!is.finite(coded)) {
    return(NA_real_)
  }
  if (coded <= coded_levels[1L]) {
    return(actual_levels[1L])
  }
  n <- length(coded_levels)
  if (coded >= coded_levels[n]) {
    return(actual_levels[n])
  }
  for (i in seq_len(n - 1L)) {
    lo_c <- coded_levels[i]
    hi_c <- coded_levels[i + 1L]
    if (coded >= lo_c && coded <= hi_c) {
      if (abs(hi_c - lo_c) < .Machine$double.eps) {
        return(actual_levels[i])
      }
      frac <- (coded - lo_c) / (hi_c - lo_c)
      return(actual_levels[i] + frac * (actual_levels[i + 1L] - actual_levels[i]))
    }
  }
  NA_real_
}

multifactor_format_coded_with_actual <- function(coded, actual, decimals) {
  coded_s <- format(round(coded, decimals), trim = TRUE, scientific = FALSE)
  if (!is.finite(actual)) {
    return(coded_s)
  }
  actual_s <- format(round(actual, decimals), trim = TRUE, scientific = FALSE)
  paste0(coded_s, " (", actual_s, ")")
}

#' Apply optional coded-to-actual display for continuous factors in the result table.
multifactor_apply_actual_value_display <- function(
    result,
    factor_continuous,
    cont_levels,
    factor_actual_values,
    decimals) {
  if (is.null(result) || nrow(result) == 0L) {
    return(result)
  }
  if (is.null(factor_actual_values) || length(factor_actual_values) == 0L) {
    return(result)
  }

  cont_factors <- names(factor_continuous)[factor_continuous]
  cont_factors <- cont_factors[cont_factors %in% names(factor_actual_values)]
  if (length(cont_factors) == 0L) {
    return(result)
  }

  for (f in cont_factors) {
    if (!f %in% names(result)) {
      next
    }
    actual_levels <- factor_actual_values[[f]]
    coded_levels <- cont_levels[[f]]
    if (is.null(actual_levels) || is.null(coded_levels) ||
        length(actual_levels) != length(coded_levels)) {
      next
    }

    result[[f]] <- vapply(
      seq_len(nrow(result)),
      function(i) {
        coded <- suppressWarnings(as.numeric(result[[f]][i]))
        if (!is.finite(coded)) {
          return(as.character(result[[f]][i]))
        }
        actual <- multifactor_coded_to_actual(coded, coded_levels, actual_levels)
        multifactor_format_coded_with_actual(coded, actual, decimals)
      },
      character(1)
    )
  }

  result
}

#' Round numeric columns for closest-target table display.
multifactor_format_target_table_display <- function(result, factor_continuous, decimals) {
  if (is.null(result) || nrow(result) == 0L) {
    return(result)
  }

  format_cell <- function(x) {
    if (length(x) != 1L) {
      return(as.character(x))
    }
    x_chr <- as.character(x)
    if (grepl("\\(", x_chr, fixed = TRUE)) {
      return(x_chr)
    }
    num <- suppressWarnings(as.numeric(x))
    if (is.finite(num)) {
      return(format(round(num, decimals), trim = TRUE, scientific = FALSE))
    }
    x_chr
  }

  if ("emmean" %in% names(result)) {
    result$emmean <- vapply(result$emmean, format_cell, character(1))
  }

  cont_factors <- names(factor_continuous)[factor_continuous]
  for (f in cont_factors) {
    if (f %in% names(result)) {
      result[[f]] <- vapply(result[[f]], format_cell, character(1))
    }
  }

  result
}

#' Settings whose estimated marginal mean is within tol of target (discrete + optional continuous).
multifactor_closest_to_target <- function(model, data, target, tol, factor_continuous, decimals) {
  if (is.null(tol) || length(tol) != 1L || is.na(tol)) {
    tol <- 0
  }
  factor_names <- multifactor_model_factor_names(model)
  if (length(factor_names) == 0L || !is.finite(target)) {
    return(data.frame())
  }

  emm_df <- multifactor_emmeans_summary(model)
  if (nrow(emm_df) == 0L) {
    return(data.frame())
  }
  for (f in factor_names) {
    if (f %in% names(emm_df)) {
      emm_df[[f]] <- as.character(emm_df[[f]])
    }
  }

  if (is.null(factor_continuous) || length(factor_continuous) == 0L) {
    factor_continuous <- stats::setNames(rep(FALSE, length(factor_names)), factor_names)
  } else if (is.null(names(factor_continuous))) {
    names(factor_continuous) <- factor_names[seq_along(factor_continuous)]
  }
  factor_continuous <- factor_continuous[factor_names]
  factor_continuous[is.na(factor_continuous)] <- FALSE

  for (f in factor_names[factor_continuous]) {
    lv <- multifactor_factor_numeric_levels(data, f)
    if (length(lv) < 2L || !ow_column_is_numericish(data[[f]])) {
      factor_continuous[[f]] <- FALSE
    }
  }

  discrete_mask <- !factor_continuous
  discrete_factors <- factor_names[discrete_mask]
  cont_factors <- factor_names[!discrete_mask]

  keep_discrete <- abs(emm_df$emmean - target) <= tol
  result <- as.data.frame(emm_df[keep_discrete, , drop = FALSE], stringsAsFactors = FALSE)
  keep_cols <- c(factor_names, "emmean")
  if (nrow(result) > 0L) {
    result <- result[, intersect(keep_cols, names(result)), drop = FALSE]
  } else {
    result <- as.data.frame(
      setNames(
        replicate(length(keep_cols), numeric(0), simplify = FALSE),
        keep_cols
      ),
      stringsAsFactors = FALSE
    )
  }
  interp_rows <- list()

  if (length(cont_factors) == 0L) {
    return(multifactor_drop_emm_interval_cols(result))
  }

  cont_levels <- stats::setNames(
    lapply(cont_factors, function(f) multifactor_factor_numeric_levels(data, f)),
    cont_factors
  )
  if (any(vapply(cont_levels, length, integer(1)) < 2L)) {
    return(multifactor_drop_emm_interval_cols(result))
  }

  interval_lists <- lapply(cont_levels, function(lv) {
    seq_len(length(lv) - 1L)
  })
  interval_grid <- expand.grid(interval_lists, stringsAsFactors = FALSE)

  discrete_levels <- stats::setNames(
    lapply(discrete_factors, function(f) sort(unique(as.character(emm_df[[f]])))),
    discrete_factors
  )
  if (length(discrete_factors) == 0L) {
    discrete_grid <- data.frame(row.names = "1")
  } else {
    discrete_grid <- expand.grid(discrete_levels, stringsAsFactors = FALSE)
  }

  seen <- if (nrow(result) > 0L) {
    vapply(seq_len(nrow(result)), function(i) {
      multifactor_settings_row_key(result[i, , drop = FALSE], factor_names, decimals)
    }, character(1))
  } else {
    character(0)
  }

  for (d_row in seq_len(nrow(discrete_grid))) {
    discrete_setting <- if (length(discrete_factors) == 0L) {
      list()
    } else {
      as.list(discrete_grid[d_row, , drop = FALSE])
    }

    for (i_row in seq_len(nrow(interval_grid))) {
      lo <- hi <- numeric(length(cont_factors))
      names(lo) <- names(hi) <- cont_factors
      lo_vals <- hi_vals <- list()
      for (j in seq_along(cont_factors)) {
        f <- cont_factors[j]
        idx <- interval_grid[i_row, j]
        lv <- cont_levels[[f]]
        lo[j] <- lv[idx]
        hi[j] <- lv[idx + 1L]
        lo_vals[[f]] <- lv[idx]
        hi_vals[[f]] <- lv[idx + 1L]
      }

      corners <- multifactor_box_corner_emmeans(
        emm_df, cont_factors, lo_vals, hi_vals, discrete_setting
      )
      if (any(is.na(corners))) {
        next
      }

      candidates <- multifactor_find_box_candidates(lo, hi, corners, target, tol)
      for (cand in candidates) {
        row_data <- discrete_setting
        for (j in seq_along(cont_factors)) {
          f <- cont_factors[j]
          row_data[[f]] <- cand$x[j]
        }
        row_data[["emmean"]] <- cand$pred
        key <- multifactor_settings_row_key(row_data, factor_names, decimals)
        if (key %in% seen) {
          next
        }
        seen <- c(seen, key)
        row_values <- setNames(
          vector("list", length(keep_cols)),
          keep_cols
        )
        for (f in factor_names) {
          if (f %in% cont_factors) {
            row_values[[f]] <- as.numeric(row_data[[f]])
          } else {
            row_values[[f]] <- as.character(row_data[[f]])
          }
        }
        row_values[["emmean"]] <- cand$pred
        interp_rows[[length(interp_rows) + 1L]] <- as.data.frame(
          row_values,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(interp_rows) > 0L) {
    result <- rbind(result, do.call(rbind, interp_rows))
  }

  if (nrow(result) == 0L) {
    return(data.frame())
  }

  result <- result[order(abs(result$emmean - target)), , drop = FALSE]
  rownames(result) <- NULL
  keep_cols <- c(factor_names, "emmean")
  result <- result[, intersect(keep_cols, names(result)), drop = FALSE]
  multifactor_drop_emm_interval_cols(result)
}
