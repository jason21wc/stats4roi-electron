# SPC axis label helpers — collection-order grouping and text display labels.

spc_sample_group_key <- function(x) {
  if (length(x) != 1) {
    stop("spc_sample_group_key expects a scalar value.")
  }
  if (is.na(x)) {
    return(NA_character_)
  }
  if (inherits(x, "POSIXt") || inherits(x, "Date")) {
    return(as.character(x))
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.character(x)) {
    return(trimws(x))
  }
  as.character(x)
}

spc_assign_sample_groups <- function(sample_vec) {
  sample_vec <- as.vector(sample_vec)
  keys <- vapply(sample_vec, spc_sample_group_key, FUN.VALUE = character(1))
  group_levels <- unique(keys)
  row_group_id <- match(keys, group_levels)
  list(
    row_group_id = row_group_id,
    group_levels = group_levels,
    k_obs = length(group_levels)
  )
}

spc_format_axis_text <- function(x) {
  if (length(x) == 0) {
    return(character(0))
  }
  vapply(
    as.vector(x),
    FUN = function(val) {
      if (is.na(val)) {
        return("NA")
      }
      if (inherits(val, "POSIXt")) {
        if (all(format(val, "%H:%M:%S") == "00:00:00")) {
          return(format(val, "%Y-%m-%d"))
        }
        return(format(val, "%Y-%m-%d %H:%M:%S"))
      }
      if (inherits(val, "Date")) {
        return(format(val, "%Y-%m-%d"))
      }
      if (is.factor(val)) {
        val <- as.character(val)
      }
      if (is.character(val)) {
        return(trimws(val))
      }
      as.character(val)
    },
    FUN.VALUE = character(1)
  )
}

spc_first_label_per_group <- function(label_vec, row_group_id, n_groups) {
  labels <- rep(NA_character_, n_groups)
  for (g in seq_len(n_groups)) {
    idx <- which(row_group_id == g)[1]
    if (!is.na(idx)) {
      labels[g] <- spc_format_axis_text(label_vec[idx])
    }
  }
  labels
}

spc_axis_label_choices <- function(data) {
  cols <- seq_len(ncol(data))
  names(cols) <- names(data)
  c("Sample" = "Sample", cols)
}

spc_axis_title_from_input <- function(axis_label_input, data) {
  if (is.null(axis_label_input) || identical(axis_label_input, "Sample")) {
    return("Samples")
  }
  col_idx <- suppressWarnings(as.integer(axis_label_input))
  if (!is.na(col_idx) && col_idx >= 1 && col_idx <= ncol(data)) {
    return(names(data)[col_idx])
  }
  "Samples"
}

#' Row-index labels for attribute charts (Sample stays numeric for limit math).
spc_build_att_chart_point_labels <- function(data, axis_label_input, k_obs) {
  if (identical(axis_label_input, "Sample")) {
    return(as.character(seq_len(k_obs)))
  }
  label_col <- suppressWarnings(as.integer(axis_label_input))
  if (is.na(label_col) || label_col < 1L || label_col > ncol(data)) {
    return(as.character(seq_len(k_obs)))
  }
  spc_format_axis_text(data[[label_col]])
}

#' Floor negative attribute-chart LCL values to NA; leave existing NA unchanged.
spc_att_floor_lcl <- function(lcl) {
  if (is.finite(lcl) && lcl < 0) {
    return(NA_real_)
  }
  lcl
}

spc_resolve_axis_labels <- function(
    mode = c("index", "row", "subgroup_lookup"),
    k_obs = NULL,
    label_values = NULL,
    row_group_id = NULL,
    group_levels = NULL) {
  mode <- match.arg(mode)

  if (mode == "index") {
    return(as.character(seq_len(k_obs)))
  }

  if (mode == "row") {
    return(spc_format_axis_text(label_values))
  }

  if (mode == "subgroup_lookup") {
    n_groups <- length(group_levels)
    if (identical(label_values, "Sample")) {
      return(spc_format_axis_text(group_levels))
    }
    return(spc_first_label_per_group(label_values, row_group_id, n_groups))
  }

  stop("Unsupported axis label mode: ", mode)
}

spc_build_chart_point_labels <- function(
    data,
    axis_label_input,
    data_type,
    ind_chart,
    k_obs,
    sample_groups = NULL) {
  use_sample_default <- is.null(axis_label_input) || identical(axis_label_input, "Sample")
  data_type <- as.numeric(data_type)

  if (isTRUE(ind_chart)) {
    if (use_sample_default) {
      return(as.character(seq_len(k_obs)))
    }
    col_idx <- as.integer(axis_label_input)
    return(spc_resolve_axis_labels(
      mode = "row",
      label_values = data[[col_idx]]
    ))
  }

  if (identical(data_type, 1)) {
    if (use_sample_default) {
      return(as.character(seq_len(k_obs)))
    }
    col_idx <- as.integer(axis_label_input)
    return(spc_resolve_axis_labels(
      mode = "row",
      label_values = data[[col_idx]]
    ))
  }

  if (identical(data_type, 2)) {
    if (is.null(sample_groups)) {
      stop("sample_groups is required for column-defined subgroup charts.")
    }
    if (use_sample_default) {
      return(spc_resolve_axis_labels(
        mode = "subgroup_lookup",
        label_values = "Sample",
        row_group_id = sample_groups$row_group_id,
        group_levels = sample_groups$group_levels
      ))
    }
    col_idx <- as.integer(axis_label_input)
    return(spc_resolve_axis_labels(
      mode = "subgroup_lookup",
      label_values = data[[col_idx]],
      row_group_id = sample_groups$row_group_id,
      group_levels = sample_groups$group_levels
    ))
  }

  as.character(seq_len(k_obs))
}

spc_map_row_labels_to_subgroups <- function(row_labels, row_group_id, n_groups) {
  subgroup_labels <- spc_first_label_per_group(row_labels, row_group_id, n_groups)
  subgroup_labels[row_group_id]
}

spc_factor_in_order <- function(labels) {
  factor(labels, levels = unique(labels))
}

spc_axis_break_labels <- function(chart_point_labels, k_obs) {
  breaks <- seq_len(k_obs)
  if (length(chart_point_labels) < k_obs) {
    chart_point_labels <- c(chart_point_labels, rep(NA_character_, k_obs - length(chart_point_labels)))
  }
  labels <- as.character(chart_point_labels[breaks])
  labels[is.na(labels) | labels == ""] <- as.character(breaks[is.na(labels) | labels == ""])
  list(breaks = breaks, labels = labels)
}

spc_add_subgroup_axis_scale <- function(p, chart_point_labels, k_obs, angle = NULL) {
  ticks <- spc_axis_break_labels(chart_point_labels, k_obs)
  p <- p + ggplot2::scale_x_continuous(breaks = ticks$breaks, labels = ticks$labels)
  if (!is.null(angle) && angle != 0) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angle, hjust = 1))
  }
  p
}

spc_lookup_chart_labels <- function(chart_point_labels, sample_index) {
  if (length(sample_index) == 0) {
    return(character(0))
  }
  n_labels <- length(chart_point_labels)
  idx <- suppressWarnings(as.integer(sample_index))
  fallback <- vapply(
    sample_index,
    function(val) if (is.na(val)) "NA" else as.character(val),
    FUN.VALUE = character(1)
  )
  if (any(is.na(idx))) {
    idx[is.na(idx)] <- match(sample_index[is.na(idx)], chart_point_labels)
  }
  out <- ifelse(
    !is.na(idx) & idx >= 1L & idx <= n_labels,
    chart_point_labels[idx],
    fallback
  )
  out[is.na(out)] <- fallback[is.na(out)]
  out
}

spc_plot_sample_column <- function(plot_obj, col_name) {
  if (is.null(plot_obj)) {
    return(NULL)
  }
  if (!is.null(plot_obj[[col_name]])) {
    return(plot_obj[[col_name]])
  }
  NULL
}

#' Per-observation rows for overlay on location (X-bar) control charts.
spc_build_location_observations <- function(d, x_name, disp_name, k_obs = NULL) {
  if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) {
    return(NULL)
  }
  value_col <- if ("Data" %in% names(d)) {
    "Data"
  } else if ("measure" %in% names(d)) {
    "measure"
  } else {
    NULL
  }
  if (is.null(value_col)) {
    return(NULL)
  }
  x_pos <- if ("x_pos" %in% names(d)) d$x_pos else d$Sample
  out <- data.frame(
    x_pos = x_pos,
    measure = d[[value_col]],
    plot_order = factor(x_name, levels = c(x_name, disp_name)),
    stringsAsFactors = FALSE
  )
  if (!is.null(k_obs) && nrow(out) <= k_obs) {
    return(NULL)
  }
  out
}

#' Horizontal spec-limit reference lines for the location chart facet only.
spc_build_spec_limit_lines <- function(usl, lsl, x_name, disp_name) {
  ys <- c(
    if (!is.null(usl) && !is.na(usl)) usl else NULL,
    if (!is.null(lsl) && !is.na(lsl)) lsl else NULL
  )
  if (length(ys) == 0) {
    return(NULL)
  }
  data.frame(
    y = ys,
    plot_order = factor(x_name, levels = c(x_name, disp_name)),
    stringsAsFactors = FALSE
  )
}

spc_has_spec_limits <- function(usl, lsl) {
  (!is.null(usl) && !is.na(usl)) || (!is.null(lsl) && !is.na(lsl))
}

#' TRUE when an SPC rule-result flag should trigger an OOC marker.
spc_ooc_rule_triggered <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(FALSE)
  }
  isTRUE(x) || identical(x, 1L) || identical(x, 1)
}

#' Convert logical OOC rule flags into y-values (or NA) for star/label geoms.
spc_apply_ooc_plot_markers <- function(
    plot_df,
    loc_facet,
    disp_facet,
    ooc_loc_rules,
    ooc_disp_rules) {
  if (is.null(plot_df) || nrow(plot_df) == 0) {
    return(plot_df)
  }

  rule_specs <- list(
    list(col = "outside", rule = 1L),
    list(col = "runs", rule = 2L),
    list(col = "trends", rule = 3L),
    list(col = "alternating", rule = 4L),
    list(col = "zone_a", rule = 5L),
    list(col = "consec_c", rule = 6L),
    list(col = "consec_ab", rule = 7L),
    list(col = "zone_a_b", rule = 8L)
  )

  apply_for_rows <- function(row_idx, enabled_rules) {
    if (length(row_idx) == 0) {
      return(plot_df)
    }
    for (spec in rule_specs) {
      col <- spec$col
      if (!col %in% names(plot_df)) {
        next
      }
      vals <- plot_df[[col]]
      for (i in row_idx) {
        if (spc_ooc_rule_triggered(vals[i]) && is.element(spec$rule, enabled_rules)) {
          vals[i] <- plot_df$measure[i]
        } else {
          vals[i] <- NA
        }
      }
      plot_df[[col]] <- vals
    }
    plot_df
  }

  loc_idx <- which(plot_df$facet == loc_facet)
  disp_idx <- which(plot_df$facet == disp_facet)
  plot_df <- apply_for_rows(loc_idx, ooc_loc_rules)
  plot_df <- apply_for_rows(disp_idx, ooc_disp_rules)

  for (spec in rule_specs) {
    col <- spec$col
    if (col %in% names(plot_df)) {
      plot_df[[col]] <- as.numeric(plot_df[[col]])
    }
  }

  plot_df
}

spc_point_row_for_sample <- function(points, sample_id) {
  row_i <- match(sample_id, points[["Sample"]], nomatch = 0L)
  if (row_i <= 0L) {
    row_i <- sample_id
  }
  row_i
}

#' Ensure singular Set column exists for limit-calculation filters.
spc_ensure_set_column <- function(df) {
  if (is.null(df) || !is.data.frame(df)) {
    return(df)
  }
  if ("Sets" %in% names(df)) {
    df[["Set"]] <- df[["Sets"]]
  }
  df
}

spc_dup_both_facets <- function(x, n_loc) {
  if (is.null(x) || length(x) == 0) {
    return(rep(NA_real_, 2L * n_loc))
  }
  if (length(x) == n_loc) {
    return(c(x, x))
  }
  if (length(x) == 2L * n_loc) {
    return(x)
  }
  rep(x, length.out = 2L * n_loc)
}

spc_order_control_chart_rows <- function(plot_df) {
  if (is.null(plot_df) || nrow(plot_df) == 0) {
    return(plot_df)
  }
  plot_df[order(-xtfrm(plot_df$facet), plot_df$x_pos, seq_len(nrow(plot_df))), , drop = FALSE]
}

spc_show_order_note <- function(data, axis_label_input, data_type, ind_chart, UI1 = NULL) {
  if (!is.null(axis_label_input) && !identical(axis_label_input, "Sample")) {
    return(TRUE)
  }
  if (isTRUE(ind_chart)) {
    return(FALSE)
  }
  if (identical(as.numeric(data_type), 2) && !is.null(UI1)) {
    col_idx <- as.integer(UI1)
    if (!is.na(col_idx) && col_idx >= 1 && col_idx <= ncol(data)) {
      sample_col <- data[[col_idx]]
      return(is.character(sample_col) || is.factor(sample_col))
    }
  }
  FALSE
}
