# Group resolution and display labels for one- and two-sample tests (Use data mode).

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

#' Column choices for selectInput (named integer indices).
#' Build column index vector from Group 1 / optional Group 2 pickers (column mode).
ots_col_vector <- function(col_g1, col_g2 = NULL) {
  g1i <- as.integer(col_g1)[1L]
  if (is.na(g1i) || g1i < 1L) {
    return(integer(0))
  }
  if (is.null(col_g2) || length(col_g2) == 0L) {
    return(g1i)
  }
  g2i <- as.integer(col_g2)[1L]
  if (is.na(g2i) || g2i < 1L || g2i == g1i) {
    return(g1i)
  }
  c(g1i, g2i)
}

ots_column_choices <- function(data) {
  if (is.null(data) || !ncol(data)) {
    return(structure(integer(0), names = character(0)))
  }
  choices <- seq_len(ncol(data))
  names(choices) <- names(data)
  choices
}

#' Human-readable label for a reference factor level (e.g. "batch 1").
ots_ref_group_label <- function(ref_name, level) {
  trimws(paste(ref_name, as.character(level)))
}

#' Escape a name for MathJax \\text{...} subscripts.
ots_tex_name <- function(name) {
  if (length(name) == 0L || is.na(name[1L]) || !nzchar(as.character(name[1L]))) {
    return("G")
  }
  x <- as.character(name[1L])
  x <- gsub("\\\\", "\\\\textbackslash ", x)
  x <- gsub("_", "\\\\_", x)
  x <- gsub("%", "\\\\%", x)
  x <- gsub("#", "\\\\#", x)
  x <- gsub("&", "\\\\&", x)
  x <- gsub("\\$", "\\\\$", x)
  x <- gsub("\\{", "\\\\{", x)
  x <- gsub("\\}", "\\\\}", x)
  x
}

#' Build a MathJax label like $\\bar{X}_{\\text{old}}$.
ots_stat_label <- function(base_tex, group_name, with_mathjax = withMathJax) {
  key <- ots_tex_name(group_name)
  paste0("$", base_tex, "_{\\text{", key, "}}$")
}

#' HTML assignment summary for sidebar.
ots_group_assignment_html <- function(groups) {
  if (is.null(groups) || is.null(groups$g1)) {
    return(NULL)
  }
  g1 <- htmltools::htmlEscape(groups$g1$name)
  if (is.null(groups$g2)) {
    return(HTML(paste0(
      "<p class=\"ots-group-assignment text-muted\" style=\"margin:0.5em 0 0 0;font-size:0.9em;\">",
      "<strong>Sample:</strong> ", g1,
      "</p>"
    )))
  }
  g2 <- htmltools::htmlEscape(groups$g2$name)
  HTML(paste0(
    "<p class=\"ots-group-assignment text-muted\" style=\"margin:0.5em 0 0 0;font-size:0.9em;\">",
    "<strong>Group 1:</strong> ", g1, " &nbsp;&middot;&nbsp; ",
    "<strong>Group 2:</strong> ", g2,
    "</p>"
  ))
}

#' Note for dependent D-bar output: pairwise differences are Group 1 minus Group 2.
ots_dbar_pairing_note_html <- function(g1 = NULL, g2 = NULL) {
  n1 <- htmltools::htmlEscape(if (!is.null(g1)) g1$name else "Group 1")
  n2 <- htmltools::htmlEscape(if (!is.null(g2)) g2$name else "Group 2")
  paste0(
    "<p class=\"text-muted\" style=\"font-size:0.9em;margin:0 0 1em 0;\">",
    "<em>Note:</em> $\\bar{D}$ is the mean of (",
    "<strong>", n1, "</strong> &minus; <strong>", n2, "</strong>) ",
    "for each pair (Group 1 minus Group 2).",
    "</p>"
  )
}

.ots_make_group <- function(name, x, index = NULL) {
  list(
    index = index,
    name = name,
    label_key = name,
    x = x
  )
}

#' Resolve groups when data are in separate columns.
ots_resolve_column_mode <- function(data, col_g1, col_g2 = NULL) {
  if (is.null(data) || !ncol(data)) {
    return(NULL)
  }
  col_g1 <- as.integer(col_g1)[1L]
  if (is.na(col_g1) || col_g1 < 1L || col_g1 > ncol(data)) {
    return(NULL)
  }
  g1_name <- names(data)[col_g1]
  g1_x <- data[[col_g1]]

  # Blank selectInput ("") is length 1; treat as one-sample / not selected
  g2_raw <- if (is.null(col_g2) || length(col_g2) == 0L) NA_integer_ else as.integer(col_g2)[1L]
  if (is.na(g2_raw)) {
    return(list(
      mode = "column",
      g1 = .ots_make_group(g1_name, g1_x, col_g1),
      g2 = NULL
    ))
  }

  col_g2 <- g2_raw
  if (col_g2 < 1L || col_g2 > ncol(data) || col_g2 == col_g1) {
    return(NULL)
  }
  g2_name <- names(data)[col_g2]
  g2_x <- data[[col_g2]]

  list(
    mode = "column",
    g1 = .ots_make_group(g1_name, g1_x, col_g1),
    g2 = .ots_make_group(g2_name, g2_x, col_g2)
  )
}

#' Resolve groups when a reference column defines factor levels.
ots_resolve_reference_mode <- function(data, ref_col, data_col, level_g1, level_g2 = NULL) {
  if (is.null(data) || !ncol(data)) {
    return(NULL)
  }
  ref_col <- as.integer(ref_col)[1L]
  data_col <- as.integer(data_col)[1L]
  if (is.na(ref_col) || is.na(data_col) ||
      ref_col < 1L || ref_col > ncol(data) ||
      data_col < 1L || data_col > ncol(data) ||
      ref_col == data_col) {
    return(NULL)
  }

  ref_name <- names(data)[ref_col]
  g1_label <- ots_ref_group_label(ref_name, level_g1)
  g1_x <- data[[data_col]][data[[ref_col]] == level_g1]

  if (is.null(level_g2) || length(level_g2) == 0L) {
    return(list(
      mode = "reference",
      g1 = .ots_make_group(g1_label, g1_x, ref_col),
      g2 = NULL
    ))
  }

  g2_label <- ots_ref_group_label(ref_name, level_g2)
  g2_x <- data[[data_col]][data[[ref_col]] == level_g2]

  if (identical(level_g1, level_g2)) {
    return(NULL)
  }

  list(
    mode = "reference",
    g1 = .ots_make_group(g1_label, g1_x, ref_col),
    g2 = .ots_make_group(g2_label, g2_x, ref_col)
  )
}

#' Unified dispatcher for column vs reference mode.
#'
#' Column mode: pass \code{col_sample} (one-sample) or \code{col_g1} and \code{col_g2} (two-sample).
#' Reference mode: pass \code{ref_col}, \code{data_col}, \code{level_g1}, and optionally \code{level_g2}.
ots_groups_from_inputs <- function(data,
                                   mode,
                                   col_sample = NULL,
                                   col_g1 = NULL,
                                   col_g2 = NULL,
                                   ref_col = NULL,
                                   data_col = NULL,
                                   level_g1 = NULL,
                                   level_g2 = NULL,
                                   two_sample = NULL) {
  mode <- as.integer(mode)[1L]
  if (is.na(mode)) {
    return(NULL)
  }

  if (mode == 1L) {
    if (!is.null(two_sample) && isTRUE(two_sample)) {
      out <- ots_resolve_column_mode(data, col_g1, col_g2)
      if (is.null(out) || is.null(out$g2)) {
        return(NULL)
      }
      out
    } else if (!is.null(two_sample) && !isTRUE(two_sample)) {
      ots_resolve_column_mode(data, col_sample %||% col_g1, NULL)
    } else if (!is.null(col_g2) && !is.na(as.integer(col_g2)[1L]) &&
               nzchar(as.character(col_g2)[1L])) {
      ots_resolve_column_mode(data, col_g1, col_g2)
    } else {
      ots_resolve_column_mode(data, col_sample %||% col_g1, NULL)
    }
  } else if (mode == 2L) {
    if (!is.null(two_sample) && isTRUE(two_sample)) {
      ots_resolve_reference_mode(data, ref_col, data_col, level_g1, level_g2)
    } else if (!is.null(two_sample) && !isTRUE(two_sample)) {
      ots_resolve_reference_mode(data, ref_col, data_col, level_g1, NULL)
    } else if (!is.null(level_g2) && length(level_g2) > 0L) {
      ots_resolve_reference_mode(data, ref_col, data_col, level_g1, level_g2)
    } else {
      ots_resolve_reference_mode(data, ref_col, data_col, level_g1, NULL)
    }
  } else {
    NULL
  }
}

#' Whether column-mode inputs are ready for the given sample count.
ots_column_mode_ready <- function(col_sample = NULL, col_g1 = NULL, col_g2 = NULL, two_sample = FALSE) {
  if (isTRUE(two_sample)) {
    c1 <- as.integer(col_g1)[1L]
    c2 <- as.integer(col_g2)[1L]
    isTruthy(c1) && isTruthy(c2) && !is.na(c1) && !is.na(c2) && c1 != c2
  } else {
    c0 <- as.integer(col_sample %||% col_g1)[1L]
    isTruthy(c0) && !is.na(c0)
  }
}

#' Whether reference-mode inputs are ready.
#' Paired data frame from two group vectors (row-aligned, min length).
ots_paired_frame <- function(g1_x, g2_x) {
  n <- min(length(g1_x), length(g2_x))
  if (n < 1L) {
    return(NULL)
  }
  df <- na.omit(data.frame(g1 = g1_x[seq_len(n)], g2 = g2_x[seq_len(n)]))
  if (nrow(df) < 1L) {
    return(NULL)
  }
  df
}

#' Flatten arguments for ots_html_flatten (character-only table fragments).
.ots_flatten_html_args <- function(x) {
  if (length(x) == 1L) {
    inner <- x[[1L]]
    if (inherits(inner, "shiny.tag")) {
      return(as.character(inner))
    }
    if (is.atomic(inner) && !is.null(inner)) {
      return(as.character(inner))
    }
    if (is.list(inner) && !inherits(inner, "shiny.tag")) {
      return(.ots_flatten_html_args(inner))
    }
  }
  unlist(lapply(x, function(el) {
    if (is.null(el) || length(el) == 0L) {
      character(0)
    } else if (inherits(el, "shiny.tag")) {
      as.character(el)
    } else if (is.list(el) && !is.character(el)) {
      .ots_flatten_html_args(el)
    } else {
      as.character(el)
    }
  }), use.names = FALSE)
}

#' Plain TeX for results tables (no withMathJax script injection).
ots_mj_tex <- function(tex) {
  tex
}

#' Build one HTML() string for results tables (plain TeX $...$; typeset via ots_results_mathjax_wrap).
ots_html_flatten <- function(...) {
  flat <- .ots_flatten_html_args(list(...))
  if (!length(flat)) {
    if (exists("HTML", mode = "function", inherits = TRUE)) {
      return(HTML(""))
    }
    return(htmltools::HTML(""))
  }
  html_out <- paste(flat, collapse = "")
  if (exists("HTML", mode = "function", inherits = TRUE)) {
    HTML(html_out)
  } else {
    htmltools::HTML(html_out)
  }
}

#' Wrap results HTML: plain TeX in a container + one MathJax Typeset call (not per-cell scripts).
ots_results_mathjax_wrap <- function(html_fragment, container_id) {
  inner <- as.character(html_fragment)
  htmltools::tagList(
    htmltools::tags$div(
      id = container_id,
      class = "ots-math-results",
      htmltools::HTML(inner)
    ),
    htmltools::tags$script(htmltools::HTML(
      paste0(
        "if (window.MathJax && MathJax.Hub) {",
        "MathJax.Hub.Queue(['Typeset', MathJax.Hub, document.getElementById('",
        container_id, "')]);",
        "}"
      )
    ))
  )
}

#' MathJax $...$ string for stat symbol with \\text{group} subscript (no value).
.ots_mj_label_tex <- function(base_tex, label_key, label_suffix = "") {
  key <- ots_tex_name(label_key)
  paste0("$", base_tex, "_{\\text{", key, "}}", label_suffix, "$")
}

#' Plain TeX string for tables: MathJax label + normal-font value (no withMathJax script).
.ots_mj_tex_plain <- function(base_tex, label_key, value = NULL, label_suffix = "") {
  lab <- .ots_mj_label_tex(base_tex, label_key, label_suffix)
  if (is.null(value)) {
    return(lab)
  }
  paste0(lab, " = ", as.character(value))
}

#' MathJax output for a statistic with a named subscript and numeric value (summary UI).
ots_stat_value_html <- function(base_tex, group, value, with_mathjax = withMathJax) {
  htmltools::tagList(
    with_mathjax(.ots_mj_label_tex(base_tex, group$label_key)),
    " = ",
    as.character(value)
  )
}

#' MathJax output with modifier in subscript (e.g. n_{batch 1}^{+}) for summary UI.
ots_mj_mod_stat_html <- function(base_tex, group, modifier = "", value, with_mathjax = withMathJax) {
  key <- ots_tex_name(group$label_key)
  inner <- if (nzchar(modifier)) {
    paste0("\\text{", key, "}", modifier)
  } else {
    paste0("\\text{", key, "}")
  }
  tex <- paste0("$", base_tex, "_{", inner, "}$")
  htmltools::tagList(
    with_mathjax(tex),
    " = ",
    as.character(value)
  )
}

#' MathJax HTML for sigma/s subscript by group key (legacy numeric subscript names).
ots_sigma_sub_html <- function(which = 1L, value, group = NULL, with_mathjax = withMathJax) {
  if (!is.null(group)) {
    return(htmltools::tagList(
      with_mathjax(.ots_mj_label_tex("\\sigma", group$label_key)),
      " = ",
      as.character(value)
    ))
  }
  htmltools::tagList(
    with_mathjax(paste0("$\\sigma_{", which, "}$")),
    " = ",
    as.character(value)
  )
}

ots_s_sub_html <- function(which = 1L, value, group = NULL, with_mathjax = withMathJax) {
  if (!is.null(group)) {
    return(htmltools::tagList(
      with_mathjax(.ots_mj_label_tex("s", group$label_key)),
      " = ",
      as.character(value)
    ))
  }
  htmltools::tagList(
    with_mathjax(paste0("$s_{", which, "}$")),
    " = ",
    as.character(value)
  )
}

#' TeX fragment for results tables (typeset once via ots_results_mathjax_wrap).
ots_mj_paste_stat <- function(base_tex, group, value = NULL, with_mathjax = withMathJax) {
  if (is.null(value)) {
    return(.ots_mj_label_tex(base_tex, group$label_key, " :"))
  }
  .ots_mj_tex_plain(base_tex, group$label_key, value)
}

#' TeX fragment with modifier subscript (e.g. n_{size; above}) for results tables.
ots_mj_mod_stat <- function(base_tex, group, modifier = "", value = NULL, with_mathjax = withMathJax) {
  key <- ots_tex_name(group$label_key)
  inner <- if (nzchar(modifier)) {
    paste0("\\text{", key, "}", modifier)
  } else {
    paste0("\\text{", key, "}")
  }
  if (is.null(value)) {
    return(paste0("$", base_tex, "_{", inner, "} :$"))
  }
  paste0("$", base_tex, "_{", inner, "}$", " = ", as.character(value))
}

#' TeX label for a difference between two groups (e.g. mu_{a} - mu_{b}).
ots_mj_diff_label <- function(base_tex, g1, g2, suffix = ":", with_mathjax = withMathJax) {
  k1 <- ots_tex_name(g1$label_key)
  k2 <- ots_tex_name(g2$label_key)
  paste0("$", base_tex, "_{\\text{", k1, "}}-", base_tex, "_{\\text{", k2, "}}", suffix, "$")
}

#' TeX label for equal-variance test sigma^2_{g1} = sigma^2_{g2}.
ots_mj_var_ratio_label <- function(g1, g2, with_mathjax = withMathJax) {
  k1 <- ots_tex_name(g1$label_key)
  k2 <- ots_tex_name(g2$label_key)
  paste0("$\\sigma^2_{\\text{", k1, "}}=\\sigma^2_{\\text{", k2, "}}$")
}

ots_n_sub_html <- function(which = 1L, value, group = NULL, with_mathjax = withMathJax) {
  if (!is.null(group)) {
    return(htmltools::tagList(
      with_mathjax(.ots_mj_label_tex("n", group$label_key)),
      " = ",
      as.character(value)
    ))
  }
  htmltools::tagList(
    with_mathjax(paste0("$n_{", which, "}$")),
    " = ",
    as.character(value)
  )
}

ots_reference_mode_ready <- function(ref_col, data_col, level_g1, level_g2 = NULL, two_sample = FALSE) {
  if (!isTruthy(ref_col) || !isTruthy(data_col) || !isTruthy(level_g1)) {
    return(FALSE)
  }
  if (isTRUE(two_sample)) {
    isTruthy(level_g2) && !identical(level_g1, level_g2)
  } else {
    TRUE
  }
}
