# Goodness-of-fit test helpers for Distribution Fitting summary table

dfit_gof_row <- function(label, statistic, p_value, decimals = 4L) {
  stat_txt <- if (is.finite(statistic)) {
    format(round(statistic, decimals), nsmall = min(decimals, 6L), trim = TRUE)
  } else {
    ""
  }
  p_txt <- if (is.finite(p_value)) {
    format(round(p_value, decimals), nsmall = min(decimals, 6L), trim = TRUE)
  } else {
    ""
  }
  list(
    label = label,
    statistic = stat_txt,
    p_value = p_txt
  )
}

dfit_normality_gof_rows <- function(x, tests, decimals = 4L) {
  if (is.null(tests) || !length(tests)) {
    return(list())
  }
  x <- stats::na.omit(x)
  if (length(x) < 3L) {
    return(list())
  }
  rows <- list()
  if ("ad" %in% tests) {
    res <- tryCatch(
      lolcat::anderson.darling.normality.test(x),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      rows <- c(rows, list(dfit_gof_row(
        "Anderson-Darling (normality)",
        res[["estimate"]][["AA"]],
        res[["p.value"]],
        decimals
      )))
    }
  }
  if ("sw" %in% tests) {
    res <- tryCatch(
      lolcat::shapiro.wilk.normality.test(x),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      rows <- c(rows, list(dfit_gof_row(
        "Shapiro-Wilk (normality)",
        res[["statistic"]][["W"]],
        res[["p.value"]],
        decimals
      )))
    }
  }
  if ("lm" %in% tests) {
    res <- tryCatch(
      lolcat::lin.mudholkar.normality.test(x),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      rows <- c(rows, list(dfit_gof_row(
        "Lin-Mudholkar (normality)",
        res[["statistic"]][["z statistic"]],
        res[["p.value"]],
        decimals
      )))
    }
  }
  if ("skew" %in% tests) {
    res <- tryCatch(
      lolcat::skewness.test(x, conf.level = 0.95, alternative = "two.sided"),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      rows <- c(rows, list(dfit_gof_row(
        "Skewness (D'Agostino)",
        res[["statistic"]][["skewness"]],
        res[["p.value"]],
        decimals
      )))
    }
  }
  if ("kurt" %in% tests) {
    res <- tryCatch(
      lolcat::kurtosis.test(x, conf.level = 0.95, alternative = "two.sided"),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      rows <- c(rows, list(dfit_gof_row(
        "Kurtosis (D'Agostino)",
        res[["statistic"]][["kurtosis"]],
        res[["p.value"]],
        decimals
      )))
    }
  }
  rows
}

dfit_exp_test_x <- function(x, distribution_id) {
  x <- stats::na.omit(x)
  if (as.integer(distribution_id) == 3L && length(x)) {
    x <- x - min(x)
  }
  x
}

dfit_exp_needs_simulation <- function(tests, n) {
  if (is.null(tests) || !length(tests)) {
    return(FALSE)
  }
  "mvp_exp" %in% tests || ("sw_exp" %in% tests && n > 100L)
}

dfit_exp_instant_gof_rows <- function(x, distribution_id, tests, decimals = 4L) {
  if (is.null(tests) || !length(tests)) {
    return(list())
  }
  x <- dfit_exp_test_x(x, distribution_id)
  if (length(x) < 2L) {
    return(list())
  }
  rows <- list()
  n <- length(x)

  if ("sw_exp" %in% tests && n <= 100L) {
    res <- tryCatch(shapiro.exp.test(x), error = function(e) NULL)
    if (!is.null(res)) {
      rows <- c(rows, list(dfit_gof_row(
        "Shapiro-Wilk (exponential)",
        res[["statistic"]][["W"]],
        res[["p.value"]],
        decimals
      )))
    }
  }
  if ("ad_exp" %in% tests) {
    if (requireNamespace("agop", quietly = TRUE)) {
      pos <- x[x > 0]
      if (length(pos) >= 2L) {
        res <- tryCatch(exp_test_ad(pos), error = function(e) NULL)
        if (!is.null(res)) {
          stat <- res[["statistic"]]
          if (is.null(stat) && length(res) >= 1L) {
            stat <- res[[1]]
          }
          pval <- res[["p.value"]]
          if (is.null(pval) && length(res) >= 2L) {
            pval <- res[[2]]
          }
          rows <- c(rows, list(dfit_gof_row(
            "Anderson-Darling (exponential)",
            stat,
            pval,
            decimals
          )))
        }
      }
    }
  }
  rows
}

dfit_exp_simulation_gof_rows <- function(x, distribution_id, tests, decimals = 4L, session = NULL) {
  if (is.null(tests) || !length(tests)) {
    return(list())
  }
  x <- dfit_exp_test_x(x, distribution_id)
  if (length(x) < 2L) {
    return(list())
  }
  rows <- list()
  n <- length(x)

  if ("sw_exp" %in% tests && n > 100L) {
    res <- tryCatch(
      shapiro.exp.test(x, bail = 500L, nrepl = 2000L, session = session),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      rows <- c(rows, list(dfit_gof_row(
        "Shapiro-Wilk (exponential)",
        res[["statistic"]][["W"]],
        res[["p.value"]],
        decimals
      )))
    }
  }
  if ("mvp_exp" %in% tests) {
    res <- tryCatch(
      mvp_exp(x, bail = 500L, max_sims = 2000L, session = session),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      rows <- c(rows, list(dfit_gof_row(
        "MVP (exponential)",
        res[["MVP(E) = "]],
        res[["p-value"]],
        decimals
      )))
    }
  }
  rows
}

dfit_append_gof_rows_html <- function(html, rows) {
  if (is.null(rows) || !length(rows)) {
    return(html)
  }
  out <- paste0(html, "<tr><th colspan='2' style='text-align:left;padding:6px;'>Goodness-of-fit tests</th></tr>")
  for (row in rows) {
    out <- paste0(
      out,
      "<tr><td style='padding:4px 8px;border:1px solid #ddd;'>", row$label, " (stat)",
      "</td><td style='padding:4px 8px;border:1px solid #ddd;text-align:right;'>",
      row$statistic, "</td></tr>",
      "<tr><td style='padding:4px 8px;border:1px solid #ddd;'>", row$label, " (p)",
      "</td><td style='padding:4px 8px;border:1px solid #ddd;text-align:right;'>",
      row$p_value, "</td></tr>"
    )
  }
  out
}
