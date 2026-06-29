# Sample Size and Power Analysis Module for stats4ROI
# This module exactly replicates the Sample Size/Power functionality from the original app
# Original implementation: app.R lines 1114-1148 (UI) and 5166-5320+ (server logic)

library(shiny)
library(lolcat)
library(dplyr)

# Source global config for rounding function
source("modules/config/global_config.R")
source("modules/statistical/sample_size_power/utils/twosided_power.R")
source("modules/statistical/sample_size_power/utils/power.cor.pearson.r.twosample.R")
source("modules/statistical/sample_size_power/utils/sample.size.cor.pearson.r.twosample.R")

# Source UI layout (on-demand s_sizeUI1-4 via renderUI in this file)
source("modules/statistical/sample_size_power/ui/sample_size_power_ui.R")

# Helper function to create properly spaced table rows
create_table_row <- function(cell1, cell2 = "", cell3 = "") {
  if (cell3 != "") {
    # Three-column layout with spacing
    c(
      "<tr>",
      "<td>", cell1, "</td>",
      "<td>&nbsp;&nbsp;&nbsp;&nbsp;</td>",
      "<td>", cell2, "</td>",
      "<td>&nbsp;&nbsp;&nbsp;&nbsp;</td>",
      "<td>", cell3, "</td>",
      "</tr>"
    )
  } else if (cell2 != "") {
    # Two-column layout with spacing
    c(
      "<tr>",
      "<td>", cell1, "</td>",
      "<td>&nbsp;&nbsp;&nbsp;&nbsp;</td>",
      "<td>", cell2, "</td>",
      "</tr>"
    )
  } else {
    # Single-column layout
    c(
      "<tr>",
      "<td>", cell1, "</td>",
      "</tr>"
    )
  }
}

# Safe get function to handle atomic vectors, lists, and data frames
safe_get <- function(obj, name, default = NULL) {
  if (is.data.frame(obj) && name %in% names(obj)) {
    return(obj[[name]])
  } else if (is.list(obj) && name %in% names(obj)) {
    return(obj[[name]])
  } else {
    return(default)
  }
}

# --- Power curve helpers (symmetric sweep about center S) ---

# Some types have no test radio (or a stale sample_calc); match main results mapping
effective_sample_calc <- function(sample_size_type, sample_calc) {
  st <- as.integer(sample_size_type)
  if (!is.na(st) && st == 5L) {
    return(15L)
  }
  sc <- as.integer(sample_calc)
  if (is.na(sc)) {
    return(NA_integer_)
  }
  sc
}

# Allowed test id for sample_size_type; NA if sample_calc is missing or stale for this type.
sample_size_power_resolved_calc <- function(sample_size_type, sample_calc) {
  sc <- effective_sample_calc(sample_size_type, sample_calc)
  if (is.na(sc)) {
    return(NA_integer_)
  }
  allowed <- switch(
    as.character(as.integer(sample_size_type)),
    "1" = c(1L, 3L, 5L, 6L, 7L, 8L),
    "2" = c(9L, 10L),
    "3" = c(12L, 13L, 14L),
    "4" = c(16L, 17L, 18L),
    "5" = 15L,
    "6" = c(11L, 19L),
    integer(0)
  )
  if (sc %in% allowed) {
    sc
  } else {
    NA_integer_
  }
}

# req() guard: wait until tab inputs and dynamic UI numerics exist before calculating.
sample_size_power_req_inputs <- function(input) {
  req(input$sample_size_type, input$sample_size_mode, input$s_size_alpha)
  mode <- as.integer(input$sample_size_mode)
  type <- as.integer(input$sample_size_type)
  if (mode == 2L) {
    req(input$s_size_sigfig)
    if (type %in% c(1L, 3L, 4L, 6L)) {
      req(input$s_sizeUI1, input$s_sizeUI2)
    } else if (type == 2L) {
      req(input$s_sizeUI2)
    }
    return(invisible(NA_integer_))
  }
  sc <- sample_size_power_resolved_calc(type, input$sample_calc)
  req(!is.na(sc))
  req(input$s_sizeUI1, input$s_sizeUI2)
  if (type != 5L) {
    req(input$one_or_two_size)
  }
  if (isTRUE(input$power_s)) {
    req(input$s_size_n)
  } else {
    req(input$s_size_beta)
    alpha <- input$s_size_alpha
    beta <- input$s_size_beta
    req(
      is.numeric(alpha), length(alpha) == 1L, is.finite(alpha), alpha > 0,
      is.numeric(beta), length(beta) == 1L, is.finite(beta), beta > 0
    )
  }
  if (sc %in% c(7L, 9L, 10L, 15L)) {
    req(input$s_sizeUI4)
  }
  if (isTRUE(input$power_s) && sc %in% c(3L, 6L, 7L, 10L, 18L, 19L)) {
    req(input$s_sizeUI3)
  }
  invisible(sc)
}

# Initial center S for the power curve when the test type changes (not on every UI keystroke)
power_curve_default_start <- function(sc, s_sizeUI1, s_sizeUI2 = NULL) {
  sc <- as.integer(sc)
  if (sc %in% c(9L, 10L)) {
    if (!is.null(s_sizeUI1) && is.finite(s_sizeUI1) && s_sizeUI1 > 0) {
      return(s_sizeUI1)
    }
  } else if (sc %in% c(12L, 13L)) {
    if (!is.null(s_sizeUI1) && is.finite(s_sizeUI1) && s_sizeUI1 >= 0 && s_sizeUI1 <= 1) {
      return(s_sizeUI1)
    }
  } else if (sc %in% c(16L, 17L)) {
    if (!is.null(s_sizeUI1) && is.finite(s_sizeUI1) && s_sizeUI1 > 0) {
      return(s_sizeUI1)
    }
  } else if (sc == 11L) {
    if (!is.null(s_sizeUI1) && is.finite(s_sizeUI1) && s_sizeUI1 >= -1 && s_sizeUI1 <= 1) {
      return(s_sizeUI1)
    }
  } else if (sc == 19L) {
    if (!is.null(s_sizeUI2) && is.finite(s_sizeUI2) && s_sizeUI2 >= -1 && s_sizeUI2 <= 1) {
      return(s_sizeUI2)
    }
  } else if (sc == 15L) {
    return(0)
  }
  0
}

# Plain-text label for power_curve_start (avoid MathJax in renderUI; prevents RGui instability)
power_curve_start_ui_label <- function(sample_calc) {
  sc <- as.integer(sample_calc)
  labels <- c(
    "9" = "Null \u03c3\u2080",
    "10" = "Null \u03c3\u2081",
    "11" = "Null \u03c1\u2080",
    "19" = "Expected \u03c1\u2083\u2084",
    "12" = "Null \u03c0\u2080",
    "13" = "Null \u03c0\u2080",
    "15" = "Null mean",
    "16" = "Null \u03bb\u2080",
    "17" = "Null \u03bb\u2080"
  )
  lbl <- labels[as.character(sc)]
  if (is.na(lbl)) {
    power_curve_center_label(sc)
  } else {
    lbl
  }
}

# One sweep for table + plot (avoids duplicate vapply passes per Shiny flush)
power_curve_compute_sweep <- function(ctx, plot_n_arm = 25L) {
  power_at <- function(intervals) {
    vapply(
      intervals,
      power_curve_power_at_interval,
      FUN.VALUE = numeric(1),
      expected = ctx$S,
      sample_calc = ctx$sample_calc,
      alt = ctx$alt,
      s_size_n = ctx$s_size_n,
      s_sizeUI1 = ctx$s_sizeUI1,
      s_sizeUI2 = ctx$s_sizeUI2,
      s_sizeUI3 = ctx$s_sizeUI3,
      s_sizeUI4 = ctx$s_sizeUI4,
      s_size_alpha = ctx$s_size_alpha,
      curve_interval = ctx$I
    )
  }

  table_intervals <- power_curve_table_intervals(ctx$S, ctx$I)
  table_intervals <- power_curve_filter_correlation_intervals(ctx$sample_calc, table_intervals)
  table_powers <- power_at(table_intervals)
  table_df <- power_curve_sort_ascending(data.frame(
    parameter = table_intervals,
    power = table_powers,
    power_pct = 100 * table_powers,
    valid = !is.na(table_powers) | table_intervals == ctx$S,
    at_expected = table_intervals == ctx$S,
    stringsAsFactors = FALSE
  ))

  plot_intervals <- power_curve_plot_intervals(ctx$S, ctx$I, n_arm = plot_n_arm)
  plot_intervals <- power_curve_filter_correlation_intervals(ctx$sample_calc, plot_intervals)
  plot_powers <- power_at(plot_intervals)
  plot_df <- power_curve_sort_ascending(data.frame(
    parameter = plot_intervals,
    power_pct = 100 * plot_powers,
    stringsAsFactors = FALSE
  ))

  list(table = table_df, plot = plot_df)
}

# Table rows: low interval to high (50 ... 70 when S=60, I=1)
power_curve_table_intervals <- function(S, I) {
  k <- 1:10
  sort(unique(c(S - rev(k) * I, S, S + k * I)))
}

# Smooth plot: dense sampling near expected value S (both arms meet at S)
power_curve_plot_intervals <- function(S, I, n_arm = 75L) {
  below <- seq(S - 10 * I, S, length.out = n_arm)
  above <- seq(S, S + 10 * I, length.out = n_arm)[-1]
  sort(unique(c(below, above)))
}

power_curve_sort_ascending <- function(df) {
  df[order(df$parameter), , drop = FALSE]
}

# Pearson r is only defined on (-1, 1); exclude sweep points outside that range
power_curve_filter_correlation_intervals <- function(sample_calc, intervals) {
  if (!as.integer(sample_calc) %in% c(11L, 19L)) {
    return(intervals)
  }
  intervals[is.finite(intervals) & intervals > -1 & intervals < 1]
}

# Conventional curve point when swept sigma equals null (lolcat returns 0); use alpha at null
power_curve_variance_null_equal_power <- function(alt, s_size_alpha) {
  as.numeric(s_size_alpha)
}

power_curve_variance_at_null <- function(sample_calc, interval_val, s_sizeUI1) {
  sc <- as.integer(sample_calc)
  if (!sc %in% c(9L, 10L)) {
    return(FALSE)
  }
  is.finite(interval_val) &&
    is.finite(s_sizeUI1) &&
    isTRUE(all.equal(as.numeric(interval_val), as.numeric(s_sizeUI1), tolerance = 1e-12))
}

# Correlation power curve: sweep rho1 (alternative) about fixed rho0 (null).
# One-tailed: standard lolcat call (null = rho0, alt = swept rho1, UI alternative).
# Two-sided: lolcat is symmetric in rho1, so below rho0 use one-sided less with null
# shifted one interval left (reference / G*Power-style asymmetric curve).
power_correlation_curve_lolcat <- function(
    rho0,
    rho1_swept,
    user_alt,
    interval,
    sample_size,
    alpha) {
  if (!is.finite(rho0) || !is.finite(rho1_swept) ||
      !is.finite(interval) || interval <= 0) {
    return(NULL)
  }
  if (isTRUE(all.equal(rho1_swept, rho0, tolerance = 1e-12))) {
    return(list(at_null = TRUE))
  }
  if (user_alt %in% c("greater", "less")) {
    return(list(
      null = rho0,
      alt = rho1_swept,
      alternative = user_alt,
      at_null = FALSE
    ))
  }
  if (rho1_swept > rho0) {
    return(list(
      null = rho0,
      alt = rho1_swept,
      alternative = "two.sided",
      at_null = FALSE
    ))
  }
  null_shifted <- rho1_swept - interval
  if (null_shifted <= -1) {
    null_shifted <- rho1_swept
  }
  list(
    null = null_shifted,
    alt = rho0,
    alternative = "less",
    at_null = FALSE
  )
}

# Two-sample correlation power curve: sweep rho34 with fixed rho12.
# Use the two-sample power function directly (symmetric two-sided curve); do not
# apply the one-sample lolcat asymmetric below-null shift (test 11 only).
power_correlation_twosample_curve_lolcat <- function(
    r12,
    r34_swept,
    user_alt,
    interval,
    center) {
  if (!is.finite(r12) || !is.finite(r34_swept)) {
    return(NULL)
  }
  list(
    r_12 = r12,
    r_34 = r34_swept,
    alternative = user_alt,
    at_null = FALSE
  )
}

power_curve_power_at_interval <- function(
    interval_val,
    expected,
    sample_calc,
    alt,
    s_size_n,
    s_sizeUI1,
    s_sizeUI2,
    s_sizeUI3,
    s_sizeUI4,
    s_size_alpha,
    curve_interval = NULL) {
  if (!power_curve_point_valid(sample_calc, interval_val)) {
    return(NA_real_)
  }
  sc <- as.integer(sample_calc)
  if (sc == 11L &&
      is.finite(s_sizeUI1) &&
      isTRUE(all.equal(as.numeric(interval_val), as.numeric(s_sizeUI1), tolerance = 1e-12))) {
    return(as.numeric(s_size_alpha))
  }
  if (sc == 19L &&
      is.finite(s_sizeUI1) &&
      isTRUE(all.equal(as.numeric(interval_val), as.numeric(s_sizeUI1), tolerance = 1e-12))) {
    return(as.numeric(s_size_alpha))
  }
  out <- compute_power_for_test(
    sample_calc = sample_calc,
    alt = alt,
    s_size_n = s_size_n,
    s_sizeUI1 = s_sizeUI1,
    s_sizeUI2 = s_sizeUI2,
    s_sizeUI3 = s_sizeUI3,
    s_sizeUI4 = s_sizeUI4,
    s_size_alpha = s_size_alpha,
    parameter_override = interval_val,
    curve_expected = expected,
    curve_interval = curve_interval
  )
  if (power_result_has_error(out)) {
    return(NA_real_)
  }
  p <- extract_scalar_power(out)
  sc <- as.integer(sample_calc)
  if (power_curve_variance_at_null(sc, interval_val, s_sizeUI1)) {
    return(power_curve_variance_null_equal_power(alt, s_size_alpha))
  }
  p
}

power_curve_uses_ui4 <- function(sample_calc) {
  sample_calc %in% c(9L, 10L)
}

# lolcat mean power functions expect a negative effect.size for "less" alternatives
power_curve_lolcat_less_flip <- function(sample_calc) {
  as.integer(sample_calc) %in% c(1L, 5L, 6L, 7L, 8L)
}

# Means power curve: actual mean on x-axis, null mean S; effect size depends on alternative
power_curve_mean_effect <- function(null_mean, actual_mean, alternative = "two.sided") {
  diff <- actual_mean - null_mean
  if (alternative == "two.sided") {
    return(abs(diff))
  }
  if (alternative == "greater") {
    return(diff)
  }
  if (alternative == "less") {
    return(-diff)
  }
  abs(diff)
}

power_curve_delta_from_expected <- function(
    sample_calc,
    expected,
    interval_value,
    alternative = "two.sided") {
  if (as.integer(sample_calc) %in% c(1L, 3L, 5L, 6L, 7L, 8L)) {
    return(power_curve_mean_effect(expected, interval_value, alternative))
  }
  interval_value
}

# ANOVA: two level means at +/-0.5*delta_b from grand mean; scale contrast when sweeping a group mean
power_anova_between_var <- function(groups, delta_b, null_mean, swept_mean) {
  if (!is.finite(groups) || groups < 2 ||
      !is.finite(delta_b) || delta_b <= 0 ||
      !is.finite(null_mean) || !is.finite(swept_mean)) {
    return(NA_real_)
  }
  scale <- abs(null_mean - swept_mean) / delta_b
  stats::var(c(rep(0, groups - 2), -0.5 * delta_b * scale, 0.5 * delta_b * scale))
}

power_curve_point_valid <- function(sample_calc, parameter_k) {
  if (sample_calc %in% c(11L, 19L)) {
    return(
      !is.na(parameter_k) &&
        is.finite(parameter_k) &&
        parameter_k > -1 &&
        parameter_k < 1
    )
  }
  if (sample_calc %in% c(12L, 13L, 14L)) {
    return(!is.na(parameter_k) && parameter_k >= 0 && parameter_k <= 1)
  }
  if (sample_calc %in% c(16L, 17L, 18L)) {
    return(!is.na(parameter_k) && parameter_k > 0)
  }
  if (sample_calc %in% c(9L, 10L)) {
    return(!is.na(parameter_k) && parameter_k > 0)
  }
  TRUE
}

power_curve_center_label <- function(sample_calc) {
  if (sample_calc %in% c(1L, 3L, 5L, 6L, 7L, 8L)) {
    return("Null Mean")
  }
  if (sample_calc == 9L) {
    return("Null Sigma")
  }
  if (sample_calc == 10L) {
    return("Null Sigma")
  }
  if (sample_calc == 11L) {
    return("Null correlation")
  }
  if (sample_calc == 19L) {
    return("Expected correlation (sample 2)")
  }
  if (sample_calc %in% c(12L, 13L)) {
    return("Null Proportion")
  }
  if (sample_calc == 14L) {
    return("Expected proportion")
  }
  if (sample_calc == 15L) {
    return("Null mean")
  }
  if (sample_calc %in% c(16L, 17L, 18L)) {
    return("Null rate")
  }
  "Expected value"
}

power_curve_param_label_text <- function(sample_calc) {
  labels <- c(
    "1" = "Actual Mean", "3" = "Actual Mean", "5" = "Actual Mean", "6" = "Actual Mean", "7" = "Actual Mean", "8" = "Actual Mean",
    "9" = "sigma[1]", "10" = "sigma[2]", "11" = "rho[1]", "19" = "rho[34]",
    "12" = "pi[1]", "13" = "pi[1]", "14" = "pi[1]",
    "15" = "Group mean",
    "16" = "lambda[1]", "17" = "lambda[1]", "18" = "lambda[1]"
  )
  lbl <- labels[as.character(sample_calc)]
  if (is.na(lbl)) "Parameter" else lbl
}

# ggplot axis: plotmath expression for variance tests; plain text otherwise
power_curve_param_label_plot <- function(sample_calc) {
  sc <- as.integer(sample_calc)
  if (sc == 9L) {
    return(expression(sigma[1]))
  }
  if (sc == 10L) {
    return(expression(sigma[2]))
  }
  if (sc == 11L) {
    return(expression(rho[1]))
  }
  if (sc == 19L) {
    return(expression(rho[34]))
  }
  if (sc %in% c(12L, 13L)) {
    return(expression(pi[1]))
  }
  if (sc == 14L) {
    return(expression(pi[2]))
  }
  if (sc == 15L) {
    return("Group mean")
  }
  if (sc %in% c(16L, 17L, 18L)) {
    return(expression(lambda[1]))
  }
  power_curve_param_label_text(sample_calc)
}

# Table column header (renderTable does not render MathJax)
power_curve_param_label_table <- function(sample_calc) {
  sc <- as.integer(sample_calc)
  if (sc == 9L) {
    return("\u03c3\u2081")
  }
  if (sc == 10L) {
    return("\u03c3\u2082")
  }
  if (sc == 11L) {
    return("\u03c1\u2081")
  }
  if (sc == 19L) {
    return("\u03c1\u2083\u2084")
  }
  if (sc %in% c(12L, 13L)) {
    return("\u03c0\u2081")
  }
  if (sc == 14L) {
    return("\u03c0\u2082")
  }
  if (sc == 15L) {
    return("Group mean")
  }
  if (sc %in% c(16L, 17L, 18L)) {
    return("\u03bb\u2081")
  }
  power_curve_param_label_text(sample_calc)
}

extract_scalar_power <- function(s_size_out) {
  if (is.null(s_size_out)) {
    return(NA_real_)
  }
  if (is.numeric(s_size_out) && length(s_size_out) == 1) {
    return(as.numeric(s_size_out))
  }
  if (is.data.frame(s_size_out)) {
    if ("power" %in% names(s_size_out)) {
      p <- s_size_out[["power"]]
      if (is.numeric(p) && length(p) >= 1) {
        return(as.numeric(p[1]))
      }
    }
    if ("power" %in% rownames(s_size_out)) {
      return(as.numeric(s_size_out["power", 1]))
    }
  }
  p <- safe_get(s_size_out, "power", NA_real_)
  if (is.numeric(p) && length(p) >= 1) {
    return(as.numeric(p[1]))
  }
  NA_real_
}

power_result_has_error <- function(s_size_out) {
  if (is.null(s_size_out)) {
    return(TRUE)
  }
  if (!is.data.frame(s_size_out)) {
    return(FALSE)
  }
  "error_message" %in% names(s_size_out) || "error_message" %in% rownames(s_size_out)
}

#' Compute power for a hypothesis test (power_s mode).
#' @param parameter_override One swept interval point from the power curve table.
#' @param curve_expected Center S (expected value); for mean z/t tests, delta = S - parameter_override.
compute_power_for_test <- function(
    sample_calc,
    alt,
    s_size_n,
    s_sizeUI1,
    s_sizeUI2,
    s_sizeUI3,
    s_sizeUI4,
    s_size_alpha,
    parameter_override = NULL,
    curve_expected = NULL,
    curve_interval = NULL) {
  s_sizeUI2_eff <- s_sizeUI2
  s_sizeUI4_eff <- s_sizeUI4
  anova_null_mean <- NULL
  anova_swept_mean <- NULL
  from_curve <- !is.null(parameter_override) && !is.null(curve_expected)
  if (!is.null(parameter_override)) {
    if (from_curve && as.integer(sample_calc) == 15L) {
      anova_null_mean <- curve_expected
      anova_swept_mean <- parameter_override
    } else {
      eff <- parameter_override
      if (from_curve) {
        eff <- power_curve_delta_from_expected(
          sample_calc,
          curve_expected,
          parameter_override,
          alt
        )
      }
      if (power_curve_uses_ui4(sample_calc)) {
        s_sizeUI4_eff <- eff
      } else {
        s_sizeUI2_eff <- eff
      }
    }
  }

  s_size_out <- NULL

  if (sample_calc == 1) {
    ui2 <- s_sizeUI2_eff
    if (alt == "less" && power_curve_lolcat_less_flip(sample_calc)) ui2 <- -ui2
    s_size_out <- lolcat_power_with_twosided_fix(
      power.mean.z.onesample,
      list(
        sample.size = s_size_n,
        effect.size = ui2,
        variance = s_sizeUI1^2,
        alpha = s_size_alpha,
        alternative = alt
      ),
      fix_fn = function(a) {
        power_z_twosided(mean_z_ncp(a$effect.size, a$sample.size, a$variance), a$alpha)
      }
    )
  } else if (sample_calc == 3) {
    ui2 <- s_sizeUI2_eff
    alt_eff <- alt
    if (alt == "less") {
      alt_eff <- "greater"
    }
    s_size_out <- power.mean.z.twosample.independent(
      sample.size = s_size_n,
      sample.size.g2 = s_sizeUI3,
      effect.size = ui2,
      variance = s_sizeUI1^2,
      alpha = s_size_alpha,
      alternative = alt_eff,
      details = TRUE
    )
  } else if (sample_calc == 5) {
    ui2 <- s_sizeUI2_eff
    if (alt == "less" && power_curve_lolcat_less_flip(sample_calc)) ui2 <- -ui2
    s_size_out <- lolcat_power_with_twosided_fix(
      power.mean.t.onesample,
      list(
        sample.size = s_size_n,
        effect.size = ui2,
        variance.est = s_sizeUI1^2,
        alpha = s_size_alpha,
        alternative = alt
      ),
      fix_fn = function(a) {
        power_t_twosided(
          mean_t_ncp_onesample(a$effect.size, a$sample.size, a$variance.est),
          a$sample.size - 1,
          a$alpha
        )
      }
    )
  } else if (sample_calc == 6) {
    ui2 <- s_sizeUI2_eff
    if (alt == "less" && power_curve_lolcat_less_flip(sample_calc)) ui2 <- -ui2
    s_size_out <- lolcat_power_with_twosided_fix(
      power.mean.t.test.twosample.independent.equal.variance,
      list(
        mean.g1 = 0,
        mean.g2 = ui2,
        variance.est.g1 = s_sizeUI1^2,
        variance.est.g2 = s_sizeUI1^2,
        sample.size.g1 = s_size_n,
        sample.size.g2 = s_sizeUI3,
        null.hypothesis.difference = 0,
        alpha = s_size_alpha,
        alternative = alt
      ),
      fix_fn = function(a) {
        power_t_twosided(
          mean_t_ncp_twosample_equal(
            a$mean.g2 - a$mean.g1,
            a$variance.est.g1,
            a$sample.size.g1,
            a$sample.size.g2
          ),
          a$sample.size.g1 + a$sample.size.g2 - 2,
          a$alpha
        )
      }
    )
  } else if (sample_calc == 7) {
    ui2 <- s_sizeUI2_eff
    if (alt == "less" && power_curve_lolcat_less_flip(sample_calc)) ui2 <- -ui2
    s_size_out <- lolcat_power_with_twosided_fix(
      power.mean.t.test.twosample.independent.unequal.variance,
      list(
        mean.g1 = 0,
        mean.g2 = ui2,
        variance.est.g1 = s_sizeUI1^2,
        variance.est.g2 = s_sizeUI4^2,
        sample.size.g1 = s_size_n,
        sample.size.g2 = s_sizeUI3,
        null.hypothesis.difference = 0,
        alpha = s_size_alpha,
        alternative = alt
      )
    )
  } else if (sample_calc == 8) {
    ui2 <- s_sizeUI2_eff
    if (alt == "less" && power_curve_lolcat_less_flip(sample_calc)) ui2 <- -ui2
    s_size_out <- lolcat_power_with_twosided_fix(
      power.mean.t.onesample,
      list(
        sample.size = s_size_n,
        effect.size = ui2,
        variance.est = s_sizeUI1^2,
        alpha = s_size_alpha,
        alternative = alt
      ),
      fix_fn = function(a) {
        power_t_twosided(
          mean_t_ncp_onesample(a$effect.size, a$sample.size, a$variance.est),
          a$sample.size - 1,
          a$alpha
        )
      }
    )
  } else if (sample_calc == 9) {
    s_size_out <- lolcat_power_with_twosided_fix(
      power.variance.onesample,
      list(
        sample.size = s_size_n,
        null.hypothesis.variance = s_sizeUI1^2,
        alternative.hypothesis.variance = s_sizeUI4_eff^2,
        alpha = s_size_alpha,
        alternative = alt
      )
    )
  } else if (sample_calc == 10) {
    s_size_out <- lolcat_power_with_twosided_fix(
      power.variance.twosample.independent,
      list(
        variance.estimate.g1 = s_sizeUI1^2,
        variance.estimate.g2 = s_sizeUI4_eff^2,
        sample.size.g1 = s_size_n,
        sample.size.g2 = s_sizeUI3,
        alpha = s_size_alpha,
        alternative = alt
      )
    )
  } else if (sample_calc == 11) {
    corr_args <- list(
      null = s_sizeUI1,
      alt = s_sizeUI2_eff,
      alternative = alt
    )
    if (from_curve) {
      curve_cfg <- power_correlation_curve_lolcat(
        rho0 = s_sizeUI1,
        rho1_swept = s_sizeUI2_eff,
        user_alt = alt,
        interval = curve_interval,
        sample_size = s_size_n,
        alpha = s_size_alpha
      )
      if (is.null(curve_cfg)) {
        s_size_out <- data.frame(error_message = "Invalid correlation curve parameters")
      } else if (isTRUE(curve_cfg$at_null)) {
        s_size_out <- data.frame(error_message = "At null hypothesis")
      } else {
        corr_args <- list(
          null = curve_cfg$null,
          alt = curve_cfg$alt,
          alternative = curve_cfg$alternative
        )
      }
    }
    if (is.null(s_size_out)) {
      if (!is.finite(corr_args$null) || abs(corr_args$null) >= 1 ||
          !is.finite(corr_args$alt) || abs(corr_args$alt) >= 1) {
        s_size_out <- data.frame(error_message = "Correlation coefficients must be from -1 to 1")
      } else {
        s_size_out <- tryCatch(
          lolcat_power_with_twosided_fix(
            power.cor.pearson.r.onesample,
            list(
              sample.size = s_size_n,
              null.hypothesis.correlation = corr_args$null,
              alternative.hypothesis.correlation = corr_args$alt,
              alpha = s_size_alpha,
              alternative = corr_args$alternative
            ),
            fix_fn = function(a) {
              power_z_twosided(
                fisher_z_ncp_onesample(
                  a$null.hypothesis.correlation,
                  a$alternative.hypothesis.correlation,
                  a$sample.size
                ),
                a$alpha
              )
            }
          ),
          error = function(e) {
            data.frame(error_message = conditionMessage(e))
          }
        )
      }
    }
  } else if (sample_calc == 19) {
    r12 <- s_sizeUI1
    r34 <- s_sizeUI2_eff
    alt_eff <- alt
    if (from_curve) {
      curve_cfg <- power_correlation_twosample_curve_lolcat(
        r12 = r12,
        r34_swept = s_sizeUI2_eff,
        user_alt = alt,
        interval = curve_interval,
        center = curve_expected
      )
      if (is.null(curve_cfg)) {
        s_size_out <- data.frame(error_message = "Invalid correlation curve parameters")
      } else {
        r12 <- curve_cfg$r_12
        r34 <- curve_cfg$r_34
        alt_eff <- curve_cfg$alternative
      }
    }
    if (is.null(s_size_out)) {
      if (!is.finite(r12) || abs(r12) >= 1 ||
          !is.finite(r34) || abs(r34) >= 1 ||
          !is.finite(s_size_n) || s_size_n < 4 ||
          !is.finite(s_sizeUI3) || s_sizeUI3 < 4) {
        s_size_out <- data.frame(error_message = "Correlation coefficients must be from -1 to 1; sample sizes at least 4")
      } else {
        s_size_out <- tryCatch(
          power.cor.pearson.r.twosample(
            sample.size_12 = s_size_n,
            sample.size_34 = s_sizeUI3,
            r_12 = r12,
            r_34 = r34,
            alpha = s_size_alpha,
            alternative = alt_eff,
            details = TRUE
          ),
          error = function(e) {
            data.frame(error_message = conditionMessage(e))
          }
        )
      }
    }
  } else if (sample_calc == 12) {
    s_size_out <- lolcat_power_with_twosided_fix(
      power.proportion.test.onesample.approximate,
      list(
        null.hypothesis.proportion = s_sizeUI1,
        alternative.hypothesis.proportion = s_sizeUI2_eff,
        alpha = s_size_alpha,
        sample.size = s_size_n,
        alternative = alt
      ),
      fix_fn = function(a) {
        power_z_twosided(
          proportion_z_ncp(
            a$null.hypothesis.proportion,
            a$alternative.hypothesis.proportion,
            a$sample.size
          ),
          a$alpha
        )
      }
    )
  } else if (sample_calc == 13) {
    s_size_out <- lolcat_power_with_twosided_fix(
      power.proportion.test.onesample.exact,
      list(
        null.hypothesis.proportion = s_sizeUI1,
        alternative.hypothesis.proportion = s_sizeUI2_eff,
        alpha = s_size_alpha,
        sample.size = s_size_n,
        alternative = alt
      )
    )
  } else if (sample_calc == 14) {
    s_size_out <- lolcat_power_with_twosided_fix(
      power.proportion.test.twosample.approximate,
      list(
        proportion.g1 = s_sizeUI1,
        proportion.g2 = s_sizeUI2_eff,
        alpha = s_size_alpha,
        sample.size = s_size_n,
        alternative = alt
      )
    )
  } else if (sample_calc == 15) {
    if (!is.null(s_sizeUI4) && s_sizeUI4 >= 2) {
      if (from_curve && !is.null(anova_null_mean) && !is.null(anova_swept_mean)) {
        between_var <- power_anova_between_var(
          s_sizeUI4,
          s_sizeUI2,
          anova_null_mean,
          anova_swept_mean
        )
      } else {
        between_var <- stats::var(c(
          rep(0, s_sizeUI4 - 2),
          -0.5 * s_sizeUI2_eff,
          0.5 * s_sizeUI2_eff
        ))
      }
      if (!is.finite(between_var)) {
        s_size_out <- data.frame(error_message = "Invalid ANOVA parameters")
      } else {
        s_size_out <- power.anova.test(
          groups = s_sizeUI4,
          n = s_size_n,
          between.var = between_var,
          within.var = s_sizeUI1^2,
          sig.level = s_size_alpha,
          power = NULL
        )
      }
    } else {
      s_size_out <- data.frame(error_message = "Number of levels must be at least 2")
    }
  } else if (sample_calc == 16) {
    s_size_out <- power.count.poisson.onesample.exact(
      n = s_size_n,
      lambda_0 = s_sizeUI1,
      lambda_1 = s_sizeUI2_eff,
      alpha = s_size_alpha,
      alternative = alt
    )
  } else if (sample_calc == 17) {
    s_size_out <- power.count.poisson.onesample.approximate.app(
      sample.size = s_size_n,
      lambda.null.hypothesis = s_sizeUI1,
      lambda.alternative.hypothesis = s_sizeUI2_eff,
      alpha = s_size_alpha,
      alternative = alt,
      details = TRUE
    )
  } else if (sample_calc == 18) {
    s_size_out <- power.count.poisson.twosample.approximate(
      n1 = s_size_n,
      n2 = s_sizeUI3,
      lambda_1 = s_sizeUI1,
      lambda_2 = s_sizeUI2_eff,
      alpha = s_size_alpha,
      alternative = alt
    )
  }

  s_size_out
}

# lolcat <= 2.0.1 has swapped lower.tail flags in two branches of
# power.count.poisson.onesample.approximate(); see burrm/lolcat R/power.count.poisson.onesample.approximate.R
.poisson_approx_lolcat_tail_bug <- local({
  if (!requireNamespace("lolcat", quietly = TRUE)) {
    return(FALSE)
  }
  tryCatch(
    {
      r <- lolcat::power.count.poisson.onesample.approximate(
        sample.size = 100,
        lambda.null.hypothesis = 10,
        lambda.alternative.hypothesis = 10,
        alpha = 0.05,
        alternative = "less",
        details = TRUE
      )
      isTRUE(abs(r$power - 0.95) < 0.01)
    },
    error = function(e) FALSE
  )
})

power.count.poisson.onesample.approximate.fixed <- function(
    sample.size,
    lambda.null.hypothesis,
    lambda.alternative.hypothesis,
    alpha = 0.05,
    alternative = c("two.sided", "less", "greater"),
    details = TRUE) {
  validate.htest.alternative(alternative = alternative)

  z.upper <- qnorm(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), lower.tail = FALSE)
  z.lower <- qnorm(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), lower.tail = TRUE)

  z.beta <- 2 * sqrt(sample.size) * (
    sqrt(lambda.alternative.hypothesis) - sqrt(lambda.null.hypothesis)
  )

  if (alternative[1] == "two.sided") {
    pow <- power_z_twosided(z.beta, alpha)
    beta <- 1 - pow
  } else if (lambda.alternative.hypothesis < lambda.null.hypothesis) {
    if (alternative[1] == "greater") {
      beta <- pnorm(z.lower - z.beta, lower.tail = FALSE)
    } else {
      beta <- pnorm(z.lower - z.beta, lower.tail = FALSE)
    }
    pow <- 1 - beta
  } else if (lambda.alternative.hypothesis >= lambda.null.hypothesis) {
    if (alternative[1] == "greater") {
      beta <- pnorm(z.upper - z.beta, lower.tail = TRUE)
    } else {
      beta <- pnorm(z.upper - z.beta, lower.tail = TRUE)
    }
    pow <- 1 - beta
  }
  if (details) {
    return(data.frame(
      test = "poisson",
      type = "one.sample",
      alternative = alternative[1],
      sample.size = sample.size,
      actual = sample.size,
      lambda.null = lambda.null.hypothesis,
      lambda.alternative = lambda.alternative.hypothesis,
      alpha = alpha,
      conf.level = 1 - alpha,
      beta = beta,
      power = pow,
      stringsAsFactors = FALSE
    ))
  }
  pow
}

power.count.poisson.onesample.approximate.app <- function(
    sample.size,
    lambda.null.hypothesis,
    lambda.alternative.hypothesis,
    alpha = 0.05,
    alternative = c("two.sided", "less", "greater"),
    details = TRUE) {
  if (.poisson_approx_lolcat_tail_bug ||
      (alternative[1] == "two.sided" && isTRUE(.needs_lolcat_twosided_fix()))) {
    return(power.count.poisson.onesample.approximate.fixed(
      sample.size = sample.size,
      lambda.null.hypothesis = lambda.null.hypothesis,
      lambda.alternative.hypothesis = lambda.alternative.hypothesis,
      alpha = alpha,
      alternative = alternative,
      details = details
    ))
  }
  lolcat::power.count.poisson.onesample.approximate(
    sample.size = sample.size,
    lambda.null.hypothesis = lambda.null.hypothesis,
    lambda.alternative.hypothesis = lambda.alternative.hypothesis,
    alpha = alpha,
    alternative = alternative,
    details = details
  )
}

# Two-sample independent z power (lolcat has sample size only; no power function)
power.mean.z.twosample.independent <- function(
    sample.size,
    effect.size,
    variance,
    sample.size.g2 = NULL,
    alpha = 0.05,
    alternative = c("two.sided", "less", "greater"),
    details = FALSE) {
  alternative <- match.arg(alternative)
  n1 <- sample.size
  n2 <- if (is.null(sample.size.g2)) sample.size else sample.size.g2
  if (!is.finite(n1) || n1 <= 0 ||
      !is.finite(n2) || n2 <= 0 ||
      !is.finite(variance) || variance <= 0) {
    if (details) {
      return(data.frame(error_message = "Invalid sample size or variance"))
    }
    return(NA_real_)
  }
  se <- sqrt(variance * (1 / n1 + 1 / n2))
  ncp <- effect.size / se

  z.upper <- qnorm(ifelse(alternative == "two.sided", alpha / 2, alpha), lower.tail = FALSE)
  z.lower <- qnorm(ifelse(alternative == "two.sided", alpha / 2, alpha), lower.tail = TRUE)

  if (alternative == "two.sided") {
    pow <- power_z_twosided(ncp, alpha)
    beta <- 1 - pow
  } else if (alternative == "greater") {
    beta <- pnorm(z.upper, mean = ncp, sd = 1, lower.tail = TRUE)
    pow <- 1 - beta
  } else {
    beta <- pnorm(z.lower, mean = ncp, sd = 1, lower.tail = FALSE)
    pow <- 1 - beta
  }

  if (details) {
    return(data.frame(
      test = "z",
      type = "two.sample",
      alternative = alternative,
      sample.size = n1,
      sample.size.g2 = n2,
      effect.size = effect.size,
      variance = variance,
      alpha = alpha,
      power = pow,
      stringsAsFactors = FALSE
    ))
  }
  pow
}

# Power function for Poisson one-sample exact test
power.count.poisson.onesample.exact <- function(lambda_0, lambda_1, n, alpha = 0.05, alternative = c("two.sided", "less", "greater")) {
  if (!is.finite(n) || n <= 0 ||
      !is.finite(lambda_0) || !is.finite(lambda_1) ||
      lambda_0 <= 0 || lambda_1 <= 0) {
    return(data.frame(error_message = "Invalid rate or sample size"))
  }
  if (lambda_0 == lambda_1) {
    if (alternative == "two.sided") {
      return(data.frame(alpha = alpha, power = alpha))
    }
    return(data.frame(error_message = "Rates cannot be equal"))
  }
  
  if (alternative == "less") {
    if (lambda_1 >= lambda_0) {
      output <- data.frame(error_message = "Alternative must be less than null")
      return(output)
    }
    df <- table.dist.poisson(lambda_0 * n)
    df.with.index <- mutate(df, IDX = 1:n())
    result <- data.frame(filter(df.with.index, (eq.and.below <= alpha))$IDX)
    df2 <- table.dist.poisson(lambda_1 * n)
    power <- df2$eq.and.below[length(result$filter.df.with.index...eq.and.below....alpha...IDX) - 1]
    
    # Critical Xs
    crit_x_l <- qpois(p = alpha, lambda = n * lambda_0, lower.tail = T) - 1
    alpha_r <- ppois(q = crit_x_l, lambda = n * lambda_0, lower.tail = T)
    output <- data.frame(c(alpha = alpha_r, power = power, crti_x_l = crit_x_l))
    
    return(output)
  }
  
  if (alternative == "greater") {
    if (lambda_1 <= lambda_0) {
      output <- data.frame(error_message = "Alternative must be greater than null")
      return(output)
    }
    df <- table.dist.poisson(lambda_0 * n)
    df.with.index <- mutate(df, IDX = 1:n())
    result <- data.frame(filter(df.with.index, (eq.and.above <= alpha))$IDX)
    df2 <- table.dist.poisson(lambda_1 * n)
    power <- df2$eq.and.above[min(result$filter.df.with.index...eq.and.above....alpha...IDX) + 1]
    
    # Critical Xs
    crit_x_u <- qpois(p = 1 - alpha, lambda = n * lambda_0, lower.tail = T) + 1
    alpha_r <- ppois(q = (crit_x_u - 1), lambda = n * lambda_0, lower.tail = F)
    output <- data.frame(c(alpha = alpha_r, power = power, crit_x_u = crit_x_u))
    
    return(output)
  }
  
  if (alternative == "two.sided") {
    crit_x_l <- qpois(p = alpha / 2, lambda = n * lambda_0, lower.tail = TRUE) - 1
    crit_x_u <- qpois(p = 1 - (alpha / 2), lambda = n * lambda_0, lower.tail = TRUE) + 1
    alpha_r <- ppois(q = crit_x_l, lambda = n * lambda_0, lower.tail = TRUE) +
      ppois(q = crit_x_u - 1, lambda = n * lambda_0, lower.tail = FALSE)
    power <- ppois(q = crit_x_l, lambda = n * lambda_1, lower.tail = TRUE) +
      ppois(q = crit_x_u - 1, lambda = n * lambda_1, lower.tail = FALSE)
    output <- data.frame(c(
      alpha = alpha_r,
      power = power,
      crit_x_l = crit_x_l,
      crit_x_u = crit_x_u
    ))
    return(output)
  }
}

# Sample size function for Poisson one-sample exact test
sample.size.count.poisson.onesample.exact <- function(lambda_0, lambda_1, alpha = 0.05, beta = 0.10, alternative = c("two.sided", "less", "greater")) {
  if (lambda_0 == lambda_1) { return(data.frame(error_message = "Rates cannot be equal")) }
  
  if (alternative == "less") {
    if (lambda_1 >= lambda_0) {
      output <- data.frame(error_message = "Alternative must be less than null")
      return(output)
    }
  }
  if (alternative == "greater") {
    if (lambda_1 <= lambda_0) {
      output <- data.frame(error_message = "Alternative must be greater than null")
      return(output)
    }
  }
  
  n <- sample.size.count.poisson.onesample.approximate(
    lambda.null.hypothesis = lambda_0,
    lambda.alternative.hypothesis = lambda_1,
    alpha = alpha,
    beta = beta,
    alternative = alternative,
    details = FALSE
  )
  
  pow <- power.count.poisson.onesample.exact(lambda_0 = lambda_0, lambda_1 = lambda_1, n = n, alpha = alpha, alternative = alternative)
  beta_this <- 1 - pow["power", ]
  
  while (beta_this > beta) {
    n <- n + 1
    pow <- power.count.poisson.onesample.exact(lambda_0 = lambda_0, lambda_1 = lambda_1, n = n, alpha = alpha, alternative = alternative)
    beta_this <- 1 - pow["power", ]
  }
  
  # Critical Xs
  crit_x_l <- NULL
  crit_x_u <- NULL
  
  if (alternative == "less") {
    crit_x_l <- qpois(p = alpha, lambda = n * lambda_0, lower.tail = T) - 1
    alpha_r <- ppois(q = crit_x_l, lambda = n * lambda_0, lower.tail = T)
  }
  if (alternative == "greater") {
    crit_x_u <- qpois(p = 1 - alpha, lambda = n * lambda_0, lower.tail = T) + 1
    alpha_r <- ppois(q = (crit_x_u - 1), lambda = n * lambda_0, lower.tail = F)
  }
  if (alternative == "two.sided") {
    crit_x_l <- qpois(p = alpha / 2, lambda = n * lambda_0, lower.tail = T) - 1
    crit_x_u <- qpois(p = 1 - (alpha / 2), lambda = n * lambda_0, lower.tail = T) + 1
    alpha_r <- ppois(q = (crit_x_u - 1), lambda = n * lambda_0, lower.tail = F)
    alpha_r <- alpha_r + ppois(q = crit_x_l, lambda = n * lambda_0, lower.tail = T)
  }
  
  output <- data.frame(alpha = alpha_r, power = 1 - beta_this, n = n, crit_x_l = crit_x_l, crit_x_u = crit_x_u)
  
  return(output)
}

# Power function for Poisson two-sample approximate test
power.count.poisson.twosample.approximate <- function(lambda_1, lambda_2, n1, n2, alpha = 0.05, alternative = c("two.sided", "less", "greater")) {
  if (!is.finite(n1) || !is.finite(n2) || n1 <= 0 || n2 <= 0 ||
      !is.finite(lambda_1) || !is.finite(lambda_2) ||
      lambda_1 <= 0 || lambda_2 <= 0) {
    return(data.frame(error_message = "Invalid rate or sample size"))
  }
  
  if (alternative == "two.sided") {
    ncp <- poisson_z_ncp_twosample(lambda_1, lambda_2, n1, n2)
    power_out <- power_z_twosided(ncp, alpha)
  }
  
  if (alternative == "greater") {
    if (lambda_2 < lambda_1) {
      output <- data.frame(error_message = "Alternative must be greater than null")
      return(output)
    }
    z_power <- (
      (sqrt(lambda_2) - sqrt(lambda_1)) /
        (.5 * sqrt(n1^-1 + n2^-1))
    ) -
      qnorm(p = (1 - alpha), mean = 0, sd = 1, lower.tail = T)
    power_out <- pnorm(q = z_power, mean = 0, sd = 1, lower.tail = T)
  }
  
  if (alternative == "less") {
    if (lambda_2 > lambda_1) {
      output <- data.frame(error_message = "Alternative must be less than null")
      return(output)
    }
    z_power <- (
      (sqrt(lambda_1) - sqrt(lambda_2)) /
        (.5 * sqrt(n1^-1 + n2^-1))
    ) -
      qnorm(p = (1 - alpha), mean = 0, sd = 1, lower.tail = T)
    power_out <- pnorm(q = z_power, mean = 0, sd = 1, lower.tail = T)
  }
  
  return(power_out)
}

# Sample size function for Poisson two-sample approximate test
sample.size.count.poisson.twosample.approximate <- function(lambda_1, lambda_2, alpha = 0.05, beta = 0.10, alternative = c("two.sided", "less", "greater")) {
  if (lambda_1 == lambda_2) { return(data.frame(error_message = "Rates cannot be equal")) }
  if (alternative == "less") {
    if (lambda_2 >= lambda_1) {
      output <- data.frame(error_message = "Alternative must be less than null")
      return(output)
    }
  }
  if (alternative == "greater") {
    if (lambda_2 <= lambda_1) {
      output <- data.frame(error_message = "Alternative must be greater than null")
      return(output)
    }
  }
  
  n <- sample.size.count.poisson.onesample.approximate(
    lambda.null.hypothesis = lambda_1,
    lambda.alternative.hypothesis = lambda_2,
    alpha = alpha,
    beta = beta,
    alternative = alternative,
    details = FALSE
  )
  
  pow <- power.count.poisson.twosample.approximate(lambda_1 = lambda_1, lambda_2 = lambda_2, n1 = n, n2 = n, alpha = alpha, alternative = alternative)
  beta_this <- 1 - pow
  
  while (beta_this > beta) {
    n <- n + 1
    pow <- power.count.poisson.twosample.approximate(lambda_1 = lambda_1, lambda_2 = lambda_2, n1 = n, n2 = n, alpha = alpha, alternative = alternative)
    beta_this <- 1 - pow
  }
  
  output <- data.frame(power = 1 - beta_this, n = n)
  
  return(output)
}

#####Estimation functions###################################################
# Function to calculate sample size for desired CI width (Mean, σ known)
# Replicating app.R lines 5432-5445
sample_size_for_mean_CI <- function(width, sd, conf.level = 0.95, sigfig = 2) {
  # Parameters:
  # width = width of confidence interval (e.g. 2*delta)
  # sd = known standard deviation
  # conf.level = confidence level of the ci
  # sigfig = number of significant digits in final width
  z <- qnorm(1 - (1 - conf.level)/2)
  n <- ceiling(signif((2 * z * sd/(width))^2, sigfig))
  act_width <- 2*z*sd/sqrt(n)
  return(list(
    n = n,
    act_width = act_width
  ))
}

# Function to calculate sample size for binomial proportion CI
# Replicating app.R lines 5448-5485
sample_size_for_binom_CI <- function(width, p_est = 0.5, conf.level = 0.95, sigfig = 2) {
  # Parameters:
  # width = width of confidence interval (e.g. 2*delta)
  # p_est = best guess as to proportion (.5 is worst-case)
  # conf.level = confidence level of the ci
  # sigfig = number of significant digits in final width
  
  alpha <- 1 - conf.level
  
  # Function to check if CI width is less than target width for a given n
  check_width <- function(n) {
    # Calculate confidence interval using exact method
    ci <- stats::binom.test(round(p_est * n), n, conf.level = conf.level)$conf.int
    actual_width <- diff(ci)
    return(actual_width)
  }
  # Binary search implementation
  n_min <- 2  # Minimum possible sample size
  n_max <- 1e6  # Maximum reasonable sample size
  
  while (n_max - n_min > 1) {
    n_mid <- floor((n_min + n_max) / 2)
    act_width <- check_width(n_mid)
    if (signif(act_width, sigfig) <= width) {
      n_max <- n_mid
    } else {
      n_min <- n_mid
    }
  }
  n <- n_max
  act_width <- check_width(n)
  
  if (n == 1e6) {
    warning("Maximum sample size reached. Consider wider interval width.")
  }
  
  return(list(n = n, act_width = act_width))
}

# Function to calculate sample size for SD CI (relative width)
# Replicating app.R lines 5488-5511
sample_size_for_sd_CI <- function(target_relative_width = 0.5, conf.level = 0.95, sigfig = 2, max_n = 100000) {
  # Parameters:
  # target_relative_width = the ratio of the width of the confidence interval to the calculated standard deviation
  # for example, the default of 0.5 will give the sample size needed to result in a confidence
  # interval that is half as wide as the standard deviation that is found
  # conf.level = confidence level of the ci
  # sigfig = number of significant digits in final width
  # max_n = the maximum iterations before stopping
  alpha <- 1 - conf.level
  for (n in 3:max_n) {  # minimum df = 2
    chi2_lower <- qchisq(alpha / 2, df = n - 1, lower.tail = FALSE)
    chi2_upper <- qchisq(1 - alpha / 2, df = n - 1, lower.tail = FALSE)
    
    relative_width <- sqrt(n - 1) * 
      (1 / sqrt(chi2_upper) - 1 / sqrt(chi2_lower))
    
    if (signif(relative_width, sigfig) <= target_relative_width) {
      return(list(n = n, act_width = relative_width))
    }
  }
  
  warning("Sample size exceeds maximum allowed (max_n).")
  return(NA)
}

# Function to calculate sample size for Poisson CI
# Replacing app.R version with exact
sample_size_for_poisson_CI <- function(lambda_est, 
                                       target_width = 0.2, 
                                       conf.level = 0.95, 
                                       sigfig = 2,
                                       max_n = 1e5) {
  # Parameters
  # lambda_est = expected average count
  # width = the width of the confidence interval to lambda_est
  # conf.level = confidence level of the ci
  # sigfig = number of significant digits in final width
  # max_T = the maximum iterations before stopping
  
  for (n in 1:max_n) {
    
    test <- poisson.test(x = ceiling(lambda_est * n),T = n,conf.level = conf.level)
    # Lower and Upper CI bounds
    upper <- test[["conf.int"]][2]
    lower <- test[["conf.int"]][1]
    
    # Relative width calculation
    width_test <- (upper - lower)# / (lambda_est * T)
    if (signif(width_test, sigfig) <= target_width) {
      return(list(n = n, act_width = width_test))
    }
  }
  
  warning("Required sample size exceeds maximum allowed (max_T).")
  return(NA)
}

# Function to calculate sample size for correlation CI
# Replicating app.R lines 5548-5588
sample_size_for_correlation_CI <- function(r_est, 
                                           width = 0.2, 
                                           conf.level = 0.95, 
                                           sigfig = 4,
                                           max_n = 100000) {
  
  # Uses Fisher's transformation to normalize
  # Parameters
  # r_est = estimated correlation coefficient
  # width = the ratio of the width of the confidence interval to rho
  # conf.level = confidence level of the ci
  # sigfig = number of significant digits in final width
  # max_n = the maximum iterations before stopping
  
  alpha <- 1 - conf.level
  # Fisher's z transform of the true correlation
  z <- 0.5 * log((1 + r_est) / (1 - r_est))
  
  # Critical value for two-sided CI
  z_crit <- qnorm(1 - alpha / 2)
  
  # Loop through sample sizes
  for (n in 4:max_n) {  # minimum n is 4
    SE_z <- 1 / sqrt(n - 3)
    z_lower <- z - z_crit * SE_z
    z_upper <- z + z_crit * SE_z
    
    # Back-transform to r
    r_lower <- (exp(2 * z_lower) - 1) / (exp(2 * z_lower) + 1)
    r_upper <- (exp(2 * z_upper) - 1) / (exp(2 * z_upper) + 1)
    
    act_width <- r_upper - r_lower
    
    if (signif(act_width, sigfig) <= width) {
      return(list(n = n, act_width = width))
    }
  }
  
  warning("Required sample size exceeds maximum allowed (max_n).")
  return(NA)
}

# Sample Size and Power Analysis UI - delegates to UI file (on-demand s_sizeUI1-4)
create_sample_size_power_ui <- function(id) {
  ns <- NS(id)
  create_sample_size_power_ui_internal(ns)
}

# Sample Size and Power Analysis Server (replicating app.R lines 5166-5320+)
create_sample_size_power_server <- function(id, color_palette = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    power_curve_colors <- function() {
      pal <- if (is.function(color_palette)) {
        color_palette()
      } else {
        get_color_palette()
      }
      get_distribution_colors(unname(pal))
    }

    # Define choice_sample_size in server (like monolithic app)
    choice_sample_size <- c(seq(1, 19))
    names(choice_sample_size) <- c(
      "One-sample Mean z",
      "One-sample Mean z - alternate",
      "Two-sample Mean z Independent",
      "Two-sample Mean z Independent - alternate",
      "One-sample Mean t Independent",
      "Two-sample Mean t equal variance Independent",
      "Two-sample Mean t unequal variance Independent",
      "Two-sample Mean t Dependent",
      "One-sample Variance",
      "Two-sample Variance Independent",
      "One-sample Pearson r",
      "One-sample Proportion - Approximate",
      "One-sample Proportion - Exact",
      "Two-sample Proportion - Approximate",
      "ANOVA",
      "One-sample Poisson - Exact",
      "One-sample Poisson - Approximate",
      "Two-Sample Poisson - Approximate",
      "Two-sample independent Pearson r"
    )
    
    # s_size_tests - renderUI like monolithic app (replicating app.R lines 7801-7826)
    # This avoids issues with multiple radioButtons having the same inputId in different conditionalPanels
    output$s_size_tests <- renderUI({
      sample_size_type <- input$sample_size_type
      
      req(sample_size_type)
      
      if (sample_size_type == 1) {
        s_size_test_out <- radioButtons(
          inputId = ns("sample_calc"),
          label = "Select the Test",
          choices = choice_sample_size[c(1, 3, 5:8)]
        )
      } else if (sample_size_type == 2) {
        s_size_test_out <- radioButtons(
          inputId = ns("sample_calc"),
          label = "Select the Test",
          choices = choice_sample_size[c(9:10)]
        )
      } else if (sample_size_type == 3) {
        s_size_test_out <- radioButtons(
          inputId = ns("sample_calc"),
          label = "Select the Test",
          choices = choice_sample_size[c(12:14)]
        )
      } else if (sample_size_type == 4) {
        s_size_test_out <- radioButtons(
          inputId = ns("sample_calc"),
          label = "Select the Test",
          choices = choice_sample_size[c(16, 17, 18)]
        )
      } else if (sample_size_type == 5) {
        s_size_test_out <- p("")
      } else if (sample_size_type == 6) {
        s_size_test_out <- radioButtons(
          inputId = ns("sample_calc"),
          label = "Select the Test",
          choices = choice_sample_size[c(11, 19)]
        )
      } else {
        s_size_test_out <- NULL
      }
      
      s_size_test_out
    })

    # More information about selected calculation (estimation mode only)
    observeEvent(input$s_size_more_info, {
      if (!isTRUE(input$s_size_more_info)) return()
      sample_size_type <- input$sample_size_type
      req(sample_size_type)
      if (sample_size_type == 1) {
        title <- "Sample Size for Estimation - Means"
        text_out <- HTML("Sample size for estimating a population mean with a specified confidence interval width when the standard deviation is known (z-based). You provide the estimated standard deviation and the desired CI width; the calculation returns the sample size needed. For example, if your data is normally distributed (or if the sample size is enough for the central limit theorem to make the distribution of the means normal) with a standard deviation of 1 and you want a 95% confidence interval that is \u00B10.5 or a width of 1, you should take a sample of 16. If you do, the actual confidence interval width will be 0.98.")
      } else if (sample_size_type == 2) {
        title <- "Sample Size for Estimation - Standard Deviation"
        text_out <- HTML("Sample size for estimating a population standard deviation with a specified relative confidence interval width. The relative width is (UCL - LCL) / \u03C3. You specify the target relative width.<br>For example, if your data are normally distributed and you want the ratio of the width of the 95% confidence interval to the actual standard deviation to be no more than 0.5, you should take a sample of 36. If your sample standard deviation turns out to be 2, the confidence interval for the standard deviation will be 1.6222 to 2.6089. The width of 0.4935 divided by the sample standard deviation of 2 is 0.4934.")
      } else if (sample_size_type == 3) {
        title <- "Sample Size for Estimation - Binomial Proportion"
        text_out <- HTML("Sample size for estimating a binomial proportion (e.g., proportion of successes) with a specified confidence interval width. Uses exact (Clopper-Pearson) intervals. You provide a planning proportion and the desired CI width. If you don't have any historical data to estimate the proportion, the best practice is to use 0.5, since that will result in the worst-case confidence interval.<br>For example, if you want a 95% confidence interval of not more than a total width of 0.1, you should take a sample of 402. If the sample proportion is exactly 0.5, the confidence interval will be 0.45 to 0.55. If the sample proportion is more or less than 0.5, the confidence interval will be smaller.")
      } else if (sample_size_type == 4) {
        title <- "Sample Size for Estimation - Poisson Rate"
        text_out <- HTML("Sample size for estimating a Poisson rate with a specified confidence interval width, using iteration. You provide an estimated rate (\u03BB) and the target width of the CI.<br>For example, if you estimate that \u03BB is 5 and you want a 95% confidence interval width of 1, you should take a sample of 79. Since \u03BB is 5, you would expect an average count of occurrances to be 395.")
      } else if (sample_size_type == 5) {
        title <- "Sample Size for Estimation - ANOVA"
        text_out <- HTML("ANOVA is used for hypothesis testing rather than estimation. To estimate a sample size for estimating group means or within-group standard deviation confidence intervals, use the Means or Standard Deviations options.")
      } else if (sample_size_type == 6) {
        title <- "Sample Size for Estimation - Correlation (Pearson r)"
        text_out <- HTML(paste0("Sample size for estimating a Pearson correlation coefficient with a specified confidence interval width. Uses the Fisher z transformation to calculate the confidence interval. You provide a planning value of r and the desired CI width.<br>For example, if you estimate that \u03C1 is 0.5 and you want a 95% confidence interval to be \u00B10.05 or a width of 0.1, you should use 867 samples. If r in the sample is 0.5 then the 95% confidence interval would be 0.4483 to 0.5483. Note that the confidence interval will be wider if the sample r is less than your estimate of \u03C1 and narrower if it is more."))
      } else {
        title <- "Sample Size for Estimation"
        text_out <- HTML("Information about the selected calculation type.")
      }
      
      sendSweetAlert(
        title = title,
        text = HTML(text_out),
        html = TRUE,
        showCloseButton = TRUE,
        btn_labels = "Close",
        type = "info"
      )
      updateCheckboxInput(inputId = "s_size_more_info", value = FALSE)
      
      #updateCheckboxInput(session, inputId = ns("s_size_more_info"), value = FALSE)
    })

    # s_sizeUI1, s_sizeUI2, s_sizeUI3, s_sizeUI4 - on-demand renderUI (replicating app.R lines 7829-8164)
    output$s_sizeUI1 <- renderUI({
      sample_calc <- input$sample_calc
      sample_size_type <- input$sample_size_type
      sample_size_mode <- input$sample_size_mode
      req(sample_size_type, sample_size_mode)
      if (sample_size_mode == 1) {
        if (sample_size_type == 5L) {
          sample_calc <- 15L
        } else {
          sample_calc <- effective_sample_calc(sample_size_type, sample_calc)
        }
        req(!is.na(sample_calc))
        if (sample_calc == 1) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\sigma:{ }$$"), value = 1, min = 0, max = 1, width = "150px")
        else if (sample_calc == 2) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\sigma:{ }$$"), value = 1, min = 0, max = 1, width = "150px")
        else if (sample_calc == 3) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\sigma:{ }$$"), value = 1, min = 0, max = 1, width = "150px")
        else if (sample_calc == 4) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\sigma:{ }$$"), value = 1, min = 0, max = 1, width = "150px")
        else if (sample_calc == 5) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\sigma:{ }$$"), value = 1, min = 0, max = 1, width = "150px")
        else if (sample_calc == 6) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\sigma:{ }$$"), value = 1, min = 0, max = 1, width = "150px")
        else if (sample_calc == 7) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\sigma:{ }$$"), value = 1, min = 0, max = 1, width = "150px")
        else if (sample_calc == 8) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\sigma_{\\bar{D}}:{ }$$"), value = 1, min = 0, max = 1, width = "150px")
        else if (sample_calc == 9) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\sigma_{0}:{ }$$"), value = 1, min = 0, max = 1, step = 0.05, width = "150px")
        else if (sample_calc == 10) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\sigma_{1}:{ }$$"), value = 1, min = 0, max = 1, step = 0.05, width = "150px")
        else if (sample_calc == 11) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\rho_{0}:{ }$$"), value = 0.0, min = -1, max = 1, step = 0.05, width = "150px")
        else if (sample_calc == 19) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\rho_{12}:{ }$$"), value = 0.0, min = -1, max = 1, step = 0.05, width = "150px")
        else if (sample_calc == 12) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\pi_{0}:{ }$$"), value = 0.5, min = 0, max = 1, step = 0.05, width = "150px")
        else if (sample_calc == 13) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\pi_{0}:{ }$$"), value = 0.5, min = 0, max = 1, step = 0.05, width = "150px")
        else if (sample_calc == 14) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\pi_{1}:{ }$$"), value = 0.5, min = 0, max = 1, step = 0.05, width = "150px")
        else if (sample_calc == 15) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\sigma_{w}:{ }$$"), value = 1, min = 0, max = 1, step = 0.05, width = "150px")
        else if (sample_calc == 16) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\lambda_{0}:{ }$$"), value = 10, min = 0, step = 1, width = "150px")
        else if (sample_calc == 17) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\lambda_{0}:{ }$$"), value = 10, min = 0, step = 1, width = "150px")
        else if (sample_calc == 18) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\lambda_{1}:{ }$$"), value = 10, min = 0, max = 1, step = 0.05, width = "150px")
        else NULL
      } else if (sample_size_mode == 2) {
        if (sample_size_type == 1) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\sigma_{est}:{ }$$"), value = 1, min = 0, width = "150px")
        else if (sample_size_type == 2) NULL
        else if (sample_size_type == 3) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\pi_{est}:{ }$$"), value = 0.5, min = 0, max = 1, width = "150px")
        else if (sample_size_type == 4) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\lambda_{est}:{ }$$"), value = 5, min = 0, width = "150px")
        else if (sample_size_type == 5) NULL
        else if (sample_size_type == 6) numericInput(inputId = ns("s_sizeUI1"), label = withMathJax("$$\\rho_{est}:{ }$$"), value = 0.5, min = 0, max = 1, width = "150px")
        else NULL
      } else NULL
    })

    output$s_sizeUI2 <- renderUI({
      sample_calc <- input$sample_calc
      sample_size_type <- input$sample_size_type
      sample_size_mode <- input$sample_size_mode
      req(sample_size_type, sample_size_mode)
      if (sample_size_mode == 1) {
        if (sample_size_type == 5L) {
          sample_calc <- 15L
        } else {
          sample_calc <- effective_sample_calc(sample_size_type, sample_calc)
        }
        req(!is.na(sample_calc))
        s_size2_out <- NULL
        if (sample_calc == 1) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$\\Delta:{ }$$"), value = 1, min = 0, max = 1, width = "150px")
        if (sample_calc == 2) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$\\Delta:{ }$$"), value = 1, min = 0, max = 1, width = "150px")
        if (sample_calc == 3) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$\\Delta:{ }$$"), value = 1, min = 0, max = 1, width = "150px")
        if (sample_calc == 4) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$\\Delta:{ }$$"), value = 1, min = 0, max = 1, width = "150px")
        if (sample_calc == 5) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$\\Delta:{ }$$"), value = 1, min = 0, max = 1, width = "150px")
        if (sample_calc == 6) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$\\Delta:{ }$$"), value = 1, min = 0, max = 1, width = "150px")
        if (sample_calc == 7) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$\\Delta:{ }$$"), value = 1, min = 0, max = 1, width = "150px")
        if (sample_calc == 8) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$\\Delta_{\\bar{D}}:{ }$$"), value = 1, min = 0, max = 1, width = "150px")
        if (sample_calc == 11) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$\\rho_{1}:{ }$$"), value = 0.5, min = -1, max = 1, step = 0.05, width = "150px")
        if (sample_calc == 19) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$\\rho_{34}:{ }$$"), value = 0.5, min = -1, max = 1, step = 0.05, width = "150px")
        if (sample_calc == 12) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$\\pi_{1}:{ }$$"), value = 0.1, min = 0, max = 1, step = 0.05, width = "150px")
        if (sample_calc == 13) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$\\pi_{1}:{ }$$"), value = 0.1, min = 0, max = 1, step = 0.05, width = "150px")
        if (sample_calc == 14) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$\\pi_{2}:{ }$$"), value = 0.1, min = 0, max = 1, step = 0.05, width = "150px")
        if (sample_calc == 15) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$\\Delta_{b}:{ }$$"), value = 1, min = 2, step = 1, width = "150px")
        if (sample_calc == 16) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$\\lambda_{1}:{ }$$"), value = 20, min = 2, step = 1, width = "150px")
        if (sample_calc == 17) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$\\lambda_{1}:{ }$$"), value = 20, min = 2, step = 1, width = "150px")
        if (sample_calc == 18) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$\\lambda_{2}:{ }$$"), value = 20, min = 2, step = 1, width = "150px")
        s_size2_out
      } else if (sample_size_mode == 2) {
        s_size2_out <- NULL
        if (sample_size_type == 1) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$CI_{Width}:{ }$$"), value = 1, min = 0, width = "150px")
        if (sample_size_type == 2) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$CI_{RelWidth}:{ }$$"), value = 0.5, min = 0, width = "150px")
        if (sample_size_type == 3) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$CI_{Width}:{ }$$"), value = 0.1, min = 0, max = 1, width = "150px")
        if (sample_size_type == 4) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$CI_{Width}:{ }$$"), value = 1, min = 0, width = "150px")
        if (sample_size_type == 6) s_size2_out <- numericInput(inputId = ns("s_sizeUI2"), label = withMathJax("$$CI_{Width}:{ }$$"), value = 0.1, min = 0, max = 1, width = "150px")
        s_size2_out
      } else NULL
    })

    output$s_sizeUI3 <- renderUI({
      sample_calc <- input$sample_calc
      power_s <- input$power_s
      sample_size_type <- input$sample_size_type
      sample_size_mode <- input$sample_size_mode
      req(sample_size_type, sample_size_mode)
      if (sample_size_mode == 1) {
        if (sample_size_type == 5L) {
          sample_calc <- 15L
        } else {
          sample_calc <- effective_sample_calc(sample_size_type, sample_calc)
        }
        req(!is.na(sample_calc))
        s_size3_out <- NULL
        if (sample_calc == 3 && power_s) s_size3_out <- numericInput(inputId = ns("s_sizeUI3"), label = withMathJax("$$n_{2}:{ }$$"), value = 10, min = 1, width = "150px")
        if (sample_calc == 6 && power_s) s_size3_out <- numericInput(inputId = ns("s_sizeUI3"), label = withMathJax("$$n_{2}:{ }$$"), value = 10, min = 1, width = "150px")
        if (sample_calc == 7 && power_s) s_size3_out <- numericInput(inputId = ns("s_sizeUI3"), label = withMathJax("$$n_{2}:{ }$$"), value = 10, min = 1, width = "150px")
        if (sample_calc == 10 && power_s) s_size3_out <- numericInput(inputId = ns("s_sizeUI3"), label = withMathJax("$$n_{2}:{ }$$"), value = 10, min = 1, width = "150px")
        if (sample_calc == 18 && power_s) s_size3_out <- numericInput(inputId = ns("s_sizeUI3"), label = withMathJax("$$n_{2}:{ }$$"), value = 10, min = 1, width = "150px")
        if (sample_calc == 19 && power_s) s_size3_out <- numericInput(inputId = ns("s_sizeUI3"), label = withMathJax("$$n_{2}:{ }$$"), value = 10, min = 1, width = "150px")
        s_size3_out
      } else {
        NULL
      }
    })

    output$s_sizeUI4 <- renderUI({
      sample_calc <- input$sample_calc
      sample_size_type <- input$sample_size_type
      sample_size_mode <- input$sample_size_mode
      req(sample_size_type, sample_size_mode)
      if (sample_size_mode == 1) {
        if (sample_size_type == 5L) {
          sample_calc <- 15L
        } else {
          sample_calc <- effective_sample_calc(sample_size_type, sample_calc)
        }
        req(!is.na(sample_calc))
        s_size4_out <- NULL
        if (sample_calc == 7) s_size4_out <- numericInput(inputId = ns("s_sizeUI4"), label = withMathJax("$$\\sigma_{2}:{ }$$"), value = 10, min = 0, max = 1, width = "150px")
        if (sample_calc == 9) s_size4_out <- numericInput(inputId = ns("s_sizeUI4"), label = withMathJax("$$\\sigma_{1}:{ }$$"), value = 2, min = 0, max = 1, step = 0.05, width = "150px")
        if (sample_calc == 10) s_size4_out <- numericInput(inputId = ns("s_sizeUI4"), label = withMathJax("$$\\sigma_{2}:{ }$$"), value = 2, min = 0, max = 1, step = 0.05, width = "150px")
        if (sample_calc == 15) s_size4_out <- numericInput(inputId = ns("s_sizeUI4"), label = withMathJax("$$\\text{Levels }:{ }$$"), value = 4, min = 2, step = 1, width = "150px")
        s_size4_out
      } else {
        NULL
      }
    })

    # Sample Size Calculations (replicating app.R lines 5166-5330)
    s_size_results <- reactive({
      req(input$sample_size_type, input$sample_size_mode, input$s_size_alpha)
      sample_size_type <- input$sample_size_type
      sample_size_mode <- input$sample_size_mode
      s_size_alpha <- input$s_size_alpha
      s_size_beta <- input$s_size_beta
      sigfig <- input$s_size_sigfig
      s_size_out <- NULL

      # Handle ANOVA in estimation mode (no dynamic test radio yet)
      if (sample_size_type == 5 && sample_size_mode == 2) {
        return("estimation")
      }

      sample_calc <- sample_size_power_req_inputs(input)
      alt <- input$one_or_two_size
      if (is.null(alt) || length(alt) != 1L || !nzchar(as.character(alt))) {
        alt <- "two.sided"
      }
      power_s <- input$power_s
      s_size_n <- input$s_size_n
      s_sizeUI1 <- input$s_sizeUI1
      s_sizeUI2 <- input$s_sizeUI2
      s_sizeUI3 <- input$s_sizeUI3
      s_sizeUI4 <- input$s_sizeUI4

      if (sample_size_mode == 1) { # Hypothesis test calculations
        # Calculate sample size
        if (power_s == FALSE) {
        
        if (sample_calc == 1) {
          if (alt == "less") {
            alt <- "greater"
          }
          s_size_out <- sample.size.mean.z.onesample(
            effect.size = s_sizeUI2,
            variance = s_sizeUI1^2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = FALSE
          )
        }
        if (sample_calc == 3) {
          if (alt == "less") {
            alt <- "greater"
          }
          s_size_out <- sample.size.mean.z.twosample.independent(
            effect.size = s_sizeUI2,
            variance = s_sizeUI1^2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = FALSE
          )
        }
        if (sample_calc == 5) {
          if (alt == "less") {
            alt <- "greater"
          }
          s_size_out <- sample.size.mean.t.onesample(
            effect.size = s_sizeUI2,
            variance = s_sizeUI1^2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = FALSE
          )
        }
        if (sample_calc == 6) {
          if (alt == "less") {
            alt <- "greater"
          }
          s_size_out <- sample.size.mean.t.test.twosample.independent.equal.variance(
            mean.g1 = 0,
            mean.g2 = s_sizeUI2,
            variance.est.g1 = s_sizeUI1^2,
            variance.est.g2 = s_sizeUI1^2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            null.hypothesis.difference = 0,
            alternative = alt,
            details = TRUE,
            power.from.actual = FALSE
          )
        }
        if (sample_calc == 7) {
          req(s_sizeUI4)
          if (alt == "less") {
            alt <- "greater"
          }
          s_size_out <- sample.size.mean.t.test.twosample.independent.unequal.variance(
            mean.g1 = 0,
            mean.g2 = s_sizeUI2,
            variance.est.g1 = s_sizeUI1^2,
            variance.est.g2 = s_sizeUI4^2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            null.hypothesis.difference = 0,
            alternative = alt,
            details = TRUE,
            power.from.actual = FALSE
          )
        }
        if (sample_calc == 8) {
          if (alt == "less") {
            alt <- "greater"
          }
          s_size_out <- sample.size.mean.t.twosample.dependent.dbar(
            effect.size = s_sizeUI2,
            variance.diff = s_sizeUI1^2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = FALSE
          )
        }
        if (sample_calc == 9) {
          req(s_sizeUI4)
          s_size_out <- sample.size.variance.onesample(
            null.hypothesis.variance = s_sizeUI1^2,
            alternative.hypothesis.variance = s_sizeUI4^2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = FALSE
          )
        }
        if (sample_calc == 10) {
          req(s_sizeUI4)
          s_size_out <- sample.size.variance.twosample.independent(
            variance.estimate.g1 = s_sizeUI1^2,
            variance.estimate.g2 = s_sizeUI4^2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = FALSE
          )
        }
        if (sample_calc == 11) { # One-sample Pearson r
          s_size_out <- sample.size.cor.pearson.r.onesample(
            null.hypothesis.correlation = s_sizeUI1,
            alternative.hypothesis.correlation = s_sizeUI2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = FALSE
          )
        }
        if (sample_calc == 19) { # Two-sample independent Pearson r
          if (abs(s_sizeUI1) >= 1 || abs(s_sizeUI2) >= 1) {
            s_size_out <- data.frame(error_message = "Correlation coefficients must be from -1 to 1")
          } else {
            s_size_out <- tryCatch(
              sample.size.cor.pearson.r.twosample(
                r12 = s_sizeUI1,
                r34 = s_sizeUI2,
                alpha = s_size_alpha,
                beta = s_size_beta,
                alternative = alt,
                details = TRUE,
                power.from.actual = FALSE
              ),
              error = function(e) {
                data.frame(error_message = conditionMessage(e))
              }
            )
            if (is.data.frame(s_size_out) && is.na(s_size_out$sample.size)) {
              s_size_out <- data.frame(error_message = "Invalid parameter combination for one-sided test")
            }
          }
        }
        if (sample_calc == 12) {
          s_size_out <- sample.size.proportion.test.onesample.approximate(
            null.hypothesis.proportion = s_sizeUI1,
            alternative.hypothesis.proportion = s_sizeUI2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = FALSE
          )
        }
        if (sample_calc == 13) {
          s_size_out <- sample.size.proportion.test.onesample.exact(
            null.hypothesis.proportion = s_sizeUI1,
            alternative.hypothesis.proportion = s_sizeUI2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = FALSE
          )
        }
        if (sample_calc == 14) {
          s_size_out <- sample.size.proportion.test.twosample.approximate(
            proportion.g1 = s_sizeUI1,
            proportion.g2 = s_sizeUI2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = FALSE
          )
        }
        if (sample_calc == 15) {
          req(s_sizeUI4)
          if (s_sizeUI4 < 2) {
            s_size_out <- data.frame(error_message = "Number of levels must be at least 2")
          } else {
            s_size_out <- power.anova.test(
              groups = s_sizeUI4,
              n = NULL,
              between.var = var(c(rep(0, s_sizeUI4 - 2), -0.5 * s_sizeUI2, 0.5 * s_sizeUI2)),
              within.var = s_sizeUI1^2,
              sig.level = s_size_alpha,
              power = 1 - s_size_beta
            )
            #calculate actual power
            power_out<-power.anova.test(
              groups = s_sizeUI4,
              n = ceiling(s_size_out$n),
              between.var = var(c(rep(0, s_sizeUI4 - 2), -0.5 * s_sizeUI2, 0.5 * s_sizeUI2)),
              within.var = s_sizeUI1^2,
              sig.level = s_size_alpha,
              power =NULL
            )
            s_size_out$power<-power_out$power
          }
        }
        if (sample_calc == 16) { # Poisson rate one sample - exact
          req(s_sizeUI1, s_sizeUI2, s_size_alpha, s_size_beta, alt)
          s_size_out <- sample.size.count.poisson.onesample.exact(
            lambda_0 = s_sizeUI1,
            lambda_1 = s_sizeUI2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt
          )
        }
        if (sample_calc == 17) { # Poisson rate one sample - approximate
          s_size_out <- sample.size.count.poisson.onesample.approximate(
            lambda.null.hypothesis = s_sizeUI1,
            lambda.alternative.hypothesis = s_sizeUI2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt,
            details = TRUE,
            power.from.actual = FALSE
          )
        }
        if (sample_calc == 18) { # Poisson rate two sample
          s_size_out <- sample.size.count.poisson.twosample.approximate(
            lambda_1 = s_sizeUI1,
            lambda_2 = s_sizeUI2,
            alpha = s_size_alpha,
            beta = s_size_beta,
            alternative = alt
          )
        }
        
      } # end sample size calcs
      
      # Calculate power
      else if (power_s == TRUE) {
        req(s_size_n, alt)
        if (sample_calc == 7 || sample_calc == 9 || sample_calc == 10) {
          req(s_sizeUI4)
        }
        if (sample_calc == 3 || sample_calc == 6 || sample_calc == 7 || sample_calc == 10 || sample_calc == 18 || sample_calc == 19) {
          req(s_sizeUI3)
        }
        s_size_out <- compute_power_for_test(
          sample_calc = sample_calc,
          alt = alt,
          s_size_n = s_size_n,
          s_sizeUI1 = s_sizeUI1,
          s_sizeUI2 = s_sizeUI2,
          s_sizeUI3 = s_sizeUI3,
          s_sizeUI4 = s_sizeUI4,
          s_size_alpha = s_size_alpha,
          parameter_override = NULL
        )
      } # end power calcs
      
      } else if (sample_size_mode == 2) { # Estimation mode
        if (sample_size_type == 1) {
          req(s_sizeUI1, s_sizeUI2, sigfig)
          s_size_out <- sample_size_for_mean_CI(
            width = s_sizeUI2, 
            sd = s_sizeUI1, 
            conf.level = 1 - s_size_alpha, 
            sigfig = sigfig
          )
        } else if (sample_size_type == 2) {
          req(s_sizeUI2, sigfig)
          s_size_out <- sample_size_for_sd_CI(
            target_relative_width = s_sizeUI2, 
            conf.level = 1 - s_size_alpha, 
            sigfig = sigfig
          )
        } else if (sample_size_type == 3) {
          req(s_sizeUI1, s_sizeUI2, sigfig)
          if (s_sizeUI2 >= 1 || s_sizeUI1 >= 1) {
            return(NULL)
          }
          s_size_out <- sample_size_for_binom_CI(
            width = s_sizeUI2, 
            p_est = s_sizeUI1, 
            conf.level = 1 - s_size_alpha, 
            sigfig = sigfig
          )
        } else if (sample_size_type == 4) {
          req(s_sizeUI1, s_sizeUI2, sigfig)
          s_size_out <- sample_size_for_poisson_CI(
            lambda_est = s_sizeUI1, 
            target_width = s_sizeUI2, 
            conf.level = 1 - s_size_alpha, 
            sigfig = sigfig
          )
        } else if (sample_size_type == 5) {
          # Should already have been handled at beginning of function
          s_size_out <- "estimation"
        } else if (sample_size_type == 6) {
          req(s_sizeUI1, s_sizeUI2, sigfig)
          if (s_sizeUI1 >= 1 || s_sizeUI2 >= 1) {
            return(NULL)
          }
          s_size_out <- sample_size_for_correlation_CI(
            r_est = s_sizeUI1, 
            width = s_sizeUI2, 
            conf.level = 1 - s_size_alpha, 
            sigfig = sigfig
          )
        }
      } # end estimation section

      if (is.null(s_size_out)) {
        return(NULL)
      }

      ro(s_size_out, 4)
    })
    
    # HTML output formatting (replicating app.R lines 11126+)
    output$pretty_ssize <- renderUI({
      sample_size_type <- input$sample_size_type
      sample_size_mode <- input$sample_size_mode

      sample_calc <- sample_size_power_req_inputs(input)
      alt <- input$one_or_two_size
      if (is.null(alt) || length(alt) != 1L || !nzchar(as.character(alt))) {
        alt <- "two.sided"
      }
      power_s <- input$power_s
      s_size_alpha <- input$s_size_alpha
      s_size_beta <- input$s_size_beta
      s_size_n <- input$s_size_n
      s_sizeUI1 <- input$s_sizeUI1
      s_sizeUI2 <- input$s_sizeUI2
      s_sizeUI3 <- input$s_sizeUI3
      s_sizeUI4 <- input$s_sizeUI4
      sigfig <- input$s_size_sigfig

      results <- s_size_results()
      req(results)
      
      if (sample_size_mode == 2) { # Estimation mode
        if (sample_size_type == 1) {
          output <- HTML(paste(
            "<b>Sample Size Calculations for Estimation - Mean σ known</b>",
            "<br><br>",
            "<table>",
            "<tr><td>", 100*(1-s_size_alpha), "% Confidence Interval", "</td></tr>",
            "<tr><td>", paste(withMathJax("$\\sigma_{est} =$"), s_sizeUI1), "</td></tr>",
            "<tr><td>", "Target CI Width = ", s_sizeUI2, "</td></tr>",
            "<tr><td>", " Significant Figures Used = ", sigfig, "</td></tr>",
            "<tr><td>", paste(withMathJax("$n =$"), results$n), "</td></tr>",
            "<tr><td>", "Actual CI Width = ", results$act_width, "</td></tr>",
            "</table>"
          ))
        } else if (sample_size_type == 2) {
          output <- HTML(paste(
            "<b>Sample Size Calculations for Estimation - Standard Deviation</b>",
            "<br>Relative Width = (UCI-LCI)/σ",
            "<br><br>",
            "<table>",
            "<tr><td>", 100*(1-s_size_alpha), "% Confidence Interval", "</td></tr>",
            "<tr><td>", "Target Relative CI Width = ", s_sizeUI2, "</td></tr>",
            "<tr><td>", " Significant Figures Used = ", sigfig, "</td></tr>",
            "<tr><td>", paste(withMathJax("$n =$"), results$n), "</td></tr>",
            "<tr><td>", "Actual Relative CI Width = ", results$act_width, "</td></tr>",
            "</table>"
          ))
        } else if (sample_size_type == 3) {
          output <- HTML(paste(
            "<b>Sample Size Calculations for Estimation - Binomial Proportion (Exact)</b>",
            "<br><br>",
            "<table>",
            "<tr><td>", 100*(1-s_size_alpha), "% Confidence Interval", "</td></tr>",
            "<tr><td>", paste(withMathJax("$\\pi_{est} =$"), s_sizeUI1), "</td></tr>",
            "<tr><td>", "Target CI Width = ", s_sizeUI2, "</td></tr>",
            "<tr><td>", " Significant Figures Used = ", sigfig, "</td></tr>",
            "<tr><td>", paste(withMathJax("$n =$"), results$n), "</td></tr>",
            "<tr><td>", "Actual CI Width = ", results$act_width, "</td></tr>",
            "</table>"
          ))
        } else if (sample_size_type == 4) {
          output <- HTML(paste(
            "<b>Sample Size Calculations for Estimation - Poisson Rates (Exact)</b>",
            "<br><br>",
            "<table>",
            "<tr><td>", 100*(1-s_size_alpha), "% Confidence Interval", "</td></tr>",
            "<tr><td>", paste(withMathJax("$\\lambda_{est} =$"), s_sizeUI1), "</td></tr>",
            "<tr><td>", "Target CI Width = ", s_sizeUI2, "</td></tr>",
            "<tr><td>", " Significant Figures Used = ", sigfig, "</td></tr>",
            "<tr><td>", paste(withMathJax("$n =$"), results$n), "</td></tr>",
            "<tr><td>", paste(withMathJax("$\\bar{c} =$"),results$n*s_sizeUI1), "</td></tr>",
            "<tr><td>", "Actual CI Width = ", results$act_width, "</td></tr>",
            "</table>"
          ))
        } else if (sample_size_type == 5) {
          output <- HTML("For calculating a confidence interval sample size for ANOVA mean estimates, select Means. <br>For calculating a confidence interval sample size for ANOVA standard deviation estimates, select Standard Deviation.")
        } else if (sample_size_type == 6) {
          output <- HTML(paste(
            "<b>Sample Size Calculations for Estimation - Correlation: Pearson's r</b>",
            "<br>Using Fisher's z-transform",
            "<br><br>",
            "<table>",
            "<tr><td>", 100*(1-s_size_alpha), "% Confidence Interval", "</td></tr>",
            "<tr><td>", paste(withMathJax("$\\rho_{est} =$"), s_sizeUI1), "</td></tr>",
            "<tr><td>", "Target CI Width = ", s_sizeUI2, "</td></tr>",
            "<tr><td>", " Significant Figures Used = ", sigfig, "</td></tr>",
            "<tr><td>", paste(withMathJax("$n =$"), results$n), "</td></tr>",
            "<tr><td>", "Actual CI Width = ", results$act_width, "</td></tr>",
            "</table>"
          ))
        }
        return(output)
      }

      # Hypothesis test mode (existing code)
      # Helper function to create properly spaced table rows
      create_table_row <- function(cell1, cell2 = "", cell3 = "") {
        if (cell3 != "") {
          # Three-column layout with spacing
          c(
            "<tr>",
            "<td>", cell1, "</td>",
            "<td>&nbsp;&nbsp;&nbsp;&nbsp;</td>",
            "<td>", cell2, "</td>",
            "<td>&nbsp;&nbsp;&nbsp;&nbsp;</td>",
            "<td>", cell3, "</td>",
            "</tr>"
          )
        } else if (cell2 != "") {
          # Two-column layout with spacing
          c(
            "<tr>",
            "<td>", cell1, "</td>",
            "<td>&nbsp;&nbsp;&nbsp;&nbsp;</td>",
            "<td>", cell2, "</td>",
            "</tr>"
          )
        } else {
          # Single-column layout
          c(
            "<tr>",
            "<td>", cell1, "</td>",
            "</tr>"
          )
        }
      }
      
      if (power_s == FALSE) { # sample size
        
        
        if (sample_calc == 1) {
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - One-Sample Mean: ", safe_get(results, "test", "z"), " test, σ known", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            create_table_row(
              paste("$\\alpha = $", s_size_alpha),
              paste("$\\beta = $", s_size_beta)
            ),
            create_table_row(paste("$\\sigma = $", s_sizeUI1)),
            create_table_row(paste("$\\Delta = $", s_sizeUI2)),
            create_table_row(paste("$n_{calc} = $", safe_get(results, "actual", "N/A"))),
            create_table_row(
              paste("$n = $", safe_get(results, "sample.size", "N/A")),
              paste("Power = ", safe_get(results, "power", "N/A"))
            ),
            "</table>"
          )))
        } else if (sample_calc == 3) { # Two-sample Mean z Independent
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - Two-Sample Independent Means: ", safe_get(results, "test", "z"), " test, σ known", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            create_table_row(
              paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)),
              paste("$\\beta = $", safe_get(results, "beta", s_size_beta))
            ),
            create_table_row(paste("$\\sigma = $", s_sizeUI1)),
            create_table_row(paste("$\\Delta = $", safe_get(results, "effect.size", s_sizeUI2))),
            create_table_row(paste("$n_{calc} = $", safe_get(results, "actual", "N/A"))),
            create_table_row(
              paste("$n = $", safe_get(results, "sample.size", "N/A")),
              paste("Power = ", safe_get(results, "power", "N/A"))
            ),
            "</table>"
          )))
        } else if (sample_calc == 5) { # One-sample Mean t
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - One-Sample Mean: ", safe_get(results, "test", "t"), " test, σ unknown", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", safe_get(results, "beta", s_size_beta)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta = $", safe_get(results, "effect.size", s_sizeUI2)), "</td></tr>",
            "<tr>",
            "<td>", paste("$n_{calc} = $", safe_get(results, "actual", "N/A")), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", safe_get(results, "sample.size", "N/A")), "</td>",
            "<td></td><td>", paste("Power = ", safe_get(results, "power", "N/A")), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 6) { # Two-sample Mean t equal variance
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - Two-Sample Independent Means: ", safe_get(results, "test", "t"), " test, unknown but equal σ", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", safe_get(results, "beta", s_size_beta)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta = $", safe_get(results, "mean.diff", s_sizeUI2)), "</td></tr>",
            "<tr>",
            "<td>", paste("$n_1 = $", safe_get(results, "sample.size.g1", "N/A")), "</td>",
            "<td></td>",
            "<td>", paste("$n_2 = $", safe_get(results, "sample.size.g2", "N/A")), "</td></tr><tr>",
            "<td>", paste("Power = ", safe_get(results, "power", "N/A")), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 7) { # Two-sample Mean t unequal variance
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - Two-Sample Independent Means: ", safe_get(results, "test", "t"), " test, unknown and unequal σ", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", safe_get(results, "beta", s_size_beta)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma_1 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\sigma_2 = $", s_sizeUI4), "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta = $", safe_get(results, "mean.diff", s_sizeUI2)), "</td></tr>",
            "<tr>",
            "<td>", paste("$n_1 = $", safe_get(results, "sample.size.g1", "N/A")), "</td>",
            "<td></td>",
            "<td>", paste("$n_2 = $", safe_get(results, "sample.size.g2", "N/A")), "</td></tr><tr>",
            "<td>", paste("Power = ", safe_get(results, "power", "N/A")), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 8) { # Two-sample Mean t Dependent
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - Two-Sample Dependent Means: ", safe_get(results, "test", "t"), " test, σ unknown", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", safe_get(results, "beta", s_size_beta)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma_{\\bar{D}} = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta_{\\bar{D}} = $", safe_get(results, "effect.size", s_sizeUI2)), "</td></tr>",
            "<tr>",
            "<td>", paste("$n = $", safe_get(results, "sample.size", "N/A")), "</td>",
            "<td></td></tr><tr>",
            "<td>", paste("Power = ", safe_get(results, "power", "N/A")), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 9) { # One-sample Variance
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - One-Sample Variance: ", safe_get(results, "test", "χ²"), " test", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", safe_get(results, "beta", s_size_beta)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma_0 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\sigma_1 = $", s_sizeUI4), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", safe_get(results, "sample.size", "N/A")), "</td>",
            "<td></td><td>", paste("Power = ", safe_get(results, "power", "N/A")), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 10) { # Two-sample Variance
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - Two-Sample Independent Variances: ", safe_get(results, "test", "F"), " test", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", safe_get(results, "beta", s_size_beta)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma_1 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\sigma_2 = $", s_sizeUI4), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n_1 = $", safe_get(results, "sample.size.g1", "N/A")), "</td>",
            "<td></td>",
            "<td>", paste("$n_2 = $", safe_get(results, "sample.size.g2", "N/A")), "</td></tr><tr>",
            "<td>", paste("Power = ", safe_get(results, "power", "N/A")), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 11) { # One-sample Pearson r
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - Correlation: ", safe_get(results, "test", "t"), " test", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            create_table_row(
              paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)),
              paste("$\\beta = $", safe_get(results, "beta", s_size_beta))
            ),
            create_table_row(
              paste("$\\rho_0 = $", s_sizeUI1),
              ""
            ),
            create_table_row(paste("$\\rho_1 = $", s_sizeUI2)),
            create_table_row(paste("$n_{calc} = $", safe_get(results, "actual", "N/A"))),
            create_table_row(paste("$n = $", safe_get(results, "sample.size", "N/A"))),
            create_table_row(paste("Power = ", safe_get(results, "power", "N/A"))),
            "</table>"
          )))
        } else if (sample_calc == 19) { # Two-sample independent Pearson r
          err <- safe_get(results, "error_message", NULL)
          if (!is.null(err)) {
            output <- HTML(err)
          } else {
            output <- withMathJax(HTML(c(
              paste("<b>", "Sample Size Calculations - Two-sample independent Pearson r: ", safe_get(results, "test", "z"), " test", "</b>"),
              "<br>",
              if (alt == "two.sided") {
                "<b>Two-Tail</b>"
              } else {
                "<b>One-Tail</b>"
              },
              "<br><br>",
              "<table>",
              create_table_row(
                paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)),
                paste("$\\beta = $", safe_get(results, "beta", s_size_beta))
              ),
              create_table_row(
                paste("$\\rho_{12} = $", s_sizeUI1),
                paste("$\\rho_{34} = $", s_sizeUI2)
              ),
              create_table_row(paste("$n_{calc} = $", safe_get(results, "actual", "N/A"))),
              create_table_row(paste("$n = $", safe_get(results, "sample.size", "N/A"), " (per sample)")),
              create_table_row(paste("Power = ", safe_get(results, "power", "N/A"))),
              "</table>"
            )))
          }
        } else if (sample_calc == 12) { # One-sample Proportion Approximate
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - One-Sample Proportion: ", safe_get(results, "test", "z"), " test (approximate)", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", safe_get(results, "beta", s_size_beta)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\pi_0 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\pi_1 = $", s_sizeUI2), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", safe_get(results, "sample.size", "N/A")), "</td>",
            "<td></td><td>", paste("Power = ", safe_get(results, "power", "N/A")), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 13) { # One-sample Proportion Exact
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - One-Sample Proportion: ", safe_get(results, "test", "exact"), " test (exact)", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", safe_get(results, "beta", s_size_beta)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\pi_0 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\pi_1 = $", s_sizeUI2), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", safe_get(results, "sample.size", "N/A")), "</td>",
            "<td></td><td>", paste("Power = ", safe_get(results, "power", "N/A")), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 14) { # Two-sample Proportion Approximate
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - Two-Sample Proportions: ", safe_get(results, "test", "z"), " test (approximate)", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", safe_get(results, "beta", s_size_beta)), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\pi_1 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\pi_2 = $", s_sizeUI2), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", safe_get(results, "sample.size", "N/A")), "</td>",
            "<td></td><td>", paste("Power = ", safe_get(results, "power", "N/A")), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 15) { # ANOVA
          # Check for error message first
          if (!is.null(safe_get(results, "error_message", NULL))) {
            output <- HTML(safe_get(results, "error_message", "Unknown error"))
          } else {
            output <- withMathJax(HTML(c(
              paste("Note that if you are interested in the power or sample size for an interaction, you can enter the effect's df + 1 in Levels above.",
                    "<br><br>",
                    "<b>", "Analysis of Variance Sample Size Calculation", "</b>"),
              "<br>",
              "Assumes that two level means depart the grand mean by ± 0.5Δ",
              "<br>", "<br>",
              "<table>",
              create_table_row(
                paste("$\\alpha = $", safe_get(results, "sig.level", s_size_alpha)),
                paste("$\\beta = $", 1 - safe_get(results, "power", 1 - s_size_beta))
              ),
              create_table_row(
                paste("$\\sigma_w = $", s_sizeUI1),
                paste("$\\sigma^2_w = $", safe_get(results, "within.var", "N/A"))
              ),
              create_table_row(
                paste("$\\Delta_b = $", s_sizeUI2),
                paste("$\\sigma^2_b = $", safe_get(results, "between.var", "N/A"))
              ),
              create_table_row(paste("$n_{calc} = $", safe_get(results, "n", "N/A"))),
              create_table_row(
                paste("$n = $", ceiling(safe_get(results, "n", 0)), " per level"),
                paste(safe_get(results, "groups", s_sizeUI4), " levels")
              ),
              create_table_row(paste("Power = ", safe_get(results, "power", "N/A"))),
              "</table>"
            )))
          }
        } else if (sample_calc == 16) { # One-sample Poisson Exact
          # Check for error message first
          if (!is.null(safe_get(results, "error_message", NULL))) {
            output <- HTML(safe_get(results, "error_message", "Unknown error"))
          } else {
            output <- withMathJax(HTML(c(
              paste("<b>", "Sample Size Calculations - Exact One-Sample Poisson", "</b>"),
              "<br>",
              if (alt == "two.sided") {
                "<b>Two-Tail</b>"
              } else {
                "<b>One-Tail</b>"
              },
              "<br><br>",
              "<table>",
              create_table_row(
                paste("$\\alpha_{actual} = $", safe_get(results, "alpha", s_size_alpha)),
                paste("$\\beta_{actual} = $", 1 - safe_get(results, "power", 0))
              ),
              create_table_row(
                paste("$\\lambda_0 = $", s_sizeUI1),
                paste("$\\lambda_1 = $", s_sizeUI2)
              ),
              create_table_row(paste("$n = $", safe_get(results, "n", "N/A"))),
              create_table_row(
                paste("$X_{crit L} = $", safe_get(results, "crit_x_l", "N/A")),
                paste("$X_{crit U} = $", safe_get(results, "crit_x_u", "N/A"))
              ),
              create_table_row(paste("Power = ", safe_get(results, "power", "N/A"))),
              "</table>"
            )))
          }
        } else if (sample_calc == 17) { # One-sample Poisson Approximate
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - Approximate One-Sample Poisson: ", safe_get(results, "test", "z"), " test", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            create_table_row(
              paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)),
              paste("$\\beta = $", safe_get(results, "beta", s_size_beta))
            ),
            create_table_row(
              paste("$\\lambda_0 = $", s_sizeUI1),
              paste("$\\lambda_1 = $", s_sizeUI2)
            ),
            create_table_row(paste("$n_{calc} = $", safe_get(results, "actual", "N/A"))),
            create_table_row(
              paste("$n = $", safe_get(results, "sample.size", "N/A")),
              paste("Power = ", safe_get(results, "power", "N/A"))
            ),
            "</table>"
          )))
        } else if (sample_calc == 18) { # Two-sample Poisson Approximate
          output <- withMathJax(HTML(c(
            paste("<b>", "Sample Size Calculations - Approximate Two-Sample Poisson", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            create_table_row(
              paste("$\\alpha = $", safe_get(results, "alpha", s_size_alpha)),
              paste("$\\beta = $", safe_get(results, "beta", s_size_beta))
            ),
            create_table_row(
              paste("$\\lambda_1 = $", s_sizeUI1),
              paste("$\\lambda_2 = $", s_sizeUI2)
            ),
            create_table_row(paste("$n = $", safe_get(results, "n", "N/A"))),
            create_table_row(paste("Power = ", safe_get(results, "power", "N/A"))),
            "</table>"
          )))
        } else {
          # Generic fallback for any other test types
          output <- withMathJax(HTML(paste("<b>Sample Size Calculation Results</b><br>", 
                              "Test: ", safe_get(results, "test", "Statistical Test"), "<br>",
                              "Sample Size: ", safe_get(results, "sample.size", "N/A"), "<br>",
                              "Power: ", safe_get(results, "power", "N/A"))))
        }
      } else { # power calculation
        # For power calculations, results might be a simple number
        if (is.numeric(results) && length(results) == 1) {
          power_value <- results
        } else {
          power_value <- safe_get(results, "power", "N/A")
        }
        
        # Calculate beta (Type II error rate) = 1 - power
        if (is.numeric(power_value) && !is.na(power_value)) {
          beta_value <- 1 - power_value
        } else {
          beta_value <- "N/A"
        }
        
        # Create detailed output based on test type
        if (sample_calc == 1) { # One-sample Mean z
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - One-Sample Mean: ", safe_get(results, "test", "z"), " test, σ known", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", s_size_n), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta = $", s_sizeUI2), "</td></tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 3) { # Two-sample Mean z Independent
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - Two-Sample Independent Means: ", safe_get(results, "test", "z"), " test, σ known", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n_1 = $", s_size_n), "</td>",
            "<td>", "</td>",
            "<td>", paste("$n_2 = $", s_sizeUI3), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta = $", s_sizeUI2), "</td></tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 5) { # One-sample Mean t
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - One-Sample Mean: ", safe_get(results, "test", "t"), " test, σ unknown", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", s_size_n), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta = $", s_sizeUI2), "</td></tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 6) { # Two-sample Mean t equal variance
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - Two-Sample Independent Means: ", safe_get(results, "test", "t"), " test, equal variance", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n_1 = $", s_size_n), "</td>",
            "<td>", "</td>",
            "<td>", paste("$n_2 = $", s_sizeUI3), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta = $", s_sizeUI2), "</td></tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 7) { # Two-sample Mean t unequal variance
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - Two-Sample Independent Means: ", safe_get(results, "test", "t"), " test, unequal variance", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n_1 = $", s_size_n), "</td>",
            "<td>", "</td>",
            "<td>", paste("$n_2 = $", s_sizeUI3), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma_1 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\sigma_2 = $", s_sizeUI4), "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta = $", s_sizeUI2), "</td></tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 8) { # Two-sample Mean t Dependent
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - Two-Sample Dependent Means: ", safe_get(results, "test", "t"), " test", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", s_size_n), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma_{\\bar{D}} = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr><td>", paste("$\\Delta_{\\bar{D}} = $", s_sizeUI2), "</td></tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 9) { # One-sample Variance
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - One-Sample Variance: ", safe_get(results, "test", "χ²"), " test", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", s_size_n), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma_0 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\sigma_1 = $", s_sizeUI4), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 10) { # Two-sample Variance
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - Two-Sample Independent Variances: ", safe_get(results, "test", "F"), " test", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n_1 = $", s_size_n), "</td>",
            "<td>", "</td>",
            "<td>", paste("$n_2 = $", s_sizeUI3), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\sigma_1 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\sigma_2 = $", s_sizeUI4), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 11) { # One-sample Pearson r
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - Correlation: ", safe_get(results, "test", "t"), " test", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            create_table_row(
              paste("$\\alpha = $", s_size_alpha),
              paste("$\\beta = $", beta_value)
            ),
            create_table_row(paste("$n = $", s_size_n)),
            create_table_row(
              paste("$\\rho_0 = $", s_sizeUI1),
              ""
            ),
            create_table_row(paste("$\\rho_1 = $", s_sizeUI2)),
            create_table_row(paste("Power = ", power_value)),
            "</table>"
          )))
        } else if (sample_calc == 19) { # Two-sample independent Pearson r
          err <- safe_get(results, "error_message", NULL)
          if (!is.null(err)) {
            output <- HTML(err)
          } else {
            output <- withMathJax(HTML(c(
              paste("<b>", "Power Calculations - Two-sample independent Pearson r: ", safe_get(results, "test", "z"), " test", "</b>"),
              "<br>",
              if (alt == "two.sided") {
                "<b>Two-Tail</b>"
              } else {
                "<b>One-Tail</b>"
              },
              "<br><br>",
              "<table>",
              create_table_row(
                paste("$\\alpha = $", s_size_alpha),
                paste("$\\beta = $", beta_value)
              ),
              create_table_row(
                paste("$n_1 = $", s_size_n),
                paste("$n_2 = $", s_sizeUI3)
              ),
              create_table_row(
                paste("$\\rho_{12} = $", s_sizeUI1),
                paste("$\\rho_{34} = $", s_sizeUI2)
              ),
              create_table_row(paste("Power = ", power_value)),
              "</table>"
            )))
          }
        } else if (sample_calc == 12) { # One-sample Proportion Approximate
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - One-Sample Proportion: ", safe_get(results, "test", "z"), " test (approximate)", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", s_size_n), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\pi_0 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\pi_1 = $", s_sizeUI2), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 13) { # One-sample Proportion Exact
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - One-Sample Proportion: ", safe_get(results, "test", "exact"), " test (exact)", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", s_size_n), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\pi_0 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\pi_1 = $", s_sizeUI2), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 14) { # Two-sample Proportion Approximate
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - Two-Sample Proportions: ", safe_get(results, "test", "z"), " test (approximate)", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", s_size_n), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\pi_1 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\pi_2 = $", s_sizeUI2), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 15) { # ANOVA
          # Check for error message first
          if (!is.null(safe_get(results, "error_message", NULL))) {
            output <- HTML(safe_get(results, "error_message", "Unknown error"))
          } else {
            output <- withMathJax(HTML(c(
              paste("Note that if you are interested in the power or sample size for an interaction, you can enter the effect's df + 1 in Levels above.",
                    "<br><br>",
                    "<b>", "Analysis of Variance Power Calculation", "</b>"),
              "<br>",
              "Assumes that two level means depart the grand mean by ± 0.5Δ",
              "<br>", "<br>",
              "<table>",
              create_table_row(
                paste("$\\alpha = $", safe_get(results, "sig.level", s_size_alpha)),
                paste("$\\beta = $", beta_value)
              ),
              create_table_row(
                paste("$\\sigma_w = $", s_sizeUI1),
                paste("$\\sigma^2_w = $", safe_get(results, "within.var", "N/A"))
              ),
              create_table_row(
                paste("$\\Delta_b = $", s_sizeUI2),
                paste("$\\sigma^2_b = $", safe_get(results, "between.var", "N/A"))
              ),
              create_table_row(
                paste("$n = $", ceiling(s_size_n), " per level"),
                paste(safe_get(results, "groups", s_sizeUI4), " levels")
              ),
              create_table_row(paste("Power = ", power_value)),
              "</table>"
            )))
          }
        } else if (sample_calc == 16) { # One-sample Poisson Exact
          # Check for error message first
          if (!is.null(safe_get(results, "error_message", NULL))) {
            output <- HTML(safe_get(results, "error_message", "Unknown error"))
          } else {
            output <- withMathJax(HTML(c(
              paste("<b>", "Power Calculations - Exact One-Sample Poisson", "</b>"),
              "<br>",
              if (alt == "two.sided") {
                "<b>Two-Tail</b>"
              } else {
                "<b>One-Tail</b>"
              },
              "<br><br>",
              "<table>",
              "<tr>",
              "<td>", paste("$\\alpha_{actual} = $", results["alpha",]), "</td>",
              "<td>", "</td>",
              "<td>", paste("$\\beta_{actual} = $", 1 - results["power",]), "</td>",
              "</tr>",
              "<tr>",
              "<td>", paste("$\\lambda_0 = $", s_sizeUI1), "</td>",
              "<td>", "</td>",
              "<td>", paste("$\\lambda_1 = $", s_sizeUI2), "</td>",
              "</tr>",
              "<tr>",
              "<td>", paste("$n = $", s_size_n), "</td>",
              "</tr><tr>",
              "<td>", paste("$X_{crit L} = $", results["crit_x_l",]), "</td><td></td>",
              "<td>", paste("$X_{crit U} = $", results["crit_x_u",]), "</td>",
              "</tr><tr>",
              "<td>", paste("Power = ", results["power",]), "</td>",
              "</tr>",
              "</table>"
            )))
          }
        } else if (sample_calc == 17) { # One-sample Poisson Approximate
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - One-Sample Poisson: ", safe_get(results, "test", "z"), " test (approximate)", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            "<tr>",
            "<td>", paste("$\\alpha = $", s_size_alpha), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\beta = $", beta_value), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$n = $", s_size_n), "</td>",
            "<td>", "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("$\\lambda_0 = $", s_sizeUI1), "</td>",
            "<td>", "</td>",
            "<td>", paste("$\\lambda_1 = $", s_sizeUI2), "</td>",
            "</tr>",
            "<tr>",
            "<td>", paste("Power = ", power_value), "</td>",
            "</tr>",
            "</table>"
          )))
        } else if (sample_calc == 18) { # Two-sample Poisson Approximate
          output <- withMathJax(HTML(c(
            paste("<b>", "Power Calculations - Approximate Two-Sample Poisson", "</b>"),
            "<br>",
            if (alt == "two.sided") {
              "<b>Two-Tail</b>"
            } else {
              "<b>One-Tail</b>"
            },
            "<br><br>",
            "<table>",
            create_table_row(
              paste("$\\alpha = $", s_size_alpha),
              paste("$\\beta = $", beta_value)
            ),
            create_table_row(
              paste("$\\lambda_1 = $", s_sizeUI1),
              paste("$\\lambda_2 = $", s_sizeUI2)
            ),
            create_table_row(
              paste("$n_1 = $", s_size_n),
              paste("$n_2 = $", s_sizeUI3)
            ),
            create_table_row(paste("Power = ", power_value)),
            "</table>"
          )))
        } else {
          # Generic fallback for any other test types
          output <- withMathJax(HTML(paste("<b>Power Calculation Results</b><br>", 
                              "Test: ", safe_get(results, "test", "Statistical Test"), "<br>",
                              "Power: ", power_value, "<br>",
                              "Beta: ", beta_value, "<br>",
                              "Sample Size: ", s_size_n)))
        }
      }

      if (is.null(output)) {
        return(NULL)
      }
      output
    })

    # --- Power Curve (symmetric sweep, power mode only) ---

    observeEvent(
      input$sample_size_type,
      {
        if (isTRUE(input$power_curve)) {
          updateCheckboxInput(session, "power_curve", value = FALSE)
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$sample_calc,
      {
        if (isTRUE(input$power_curve)) {
          updateCheckboxInput(session, "power_curve", value = FALSE)
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      list(input$sample_calc, input$sample_size_type, input$power_curve),
      {
        if (!isTRUE(input$power_curve)) {
          return()
        }
        sc <- effective_sample_calc(input$sample_size_type, input$sample_calc)
        if (is.na(sc)) {
          return()
        }
        updateNumericInput(
          session,
          "power_curve_start",
          value = power_curve_default_start(sc, input$s_sizeUI1, input$s_sizeUI2)
        )
      },
      ignoreInit = FALSE
    )

    output$power_curve_start_input <- renderUI({
      req(input$sample_size_type)
      sc <- effective_sample_calc(input$sample_size_type, input$sample_calc)
      if (is.na(sc)) {
        return(NULL)
      }
      start_val <- power_curve_default_start(
        sc,
        isolate(input$s_sizeUI1),
        isolate(input$s_sizeUI2)
      )
      args <- list(
        inputId = ns("power_curve_start"),
        label = power_curve_start_ui_label(sc),
        value = start_val,
        width = "150px"
      )
      if (sc %in% c(9L, 10L, 16L, 17L, 18L)) {
        args$min <- 0
      }
      if (sc %in% c(12L, 13L, 14L)) {
        args$min <- 0
        args$max <- 1
      }
      if (sc == 11L) {
        args$min <- -1
        args$max <- 1
        args$step <- 0.05
      }
      if (sc == 19L) {
        args$min <- -1
        args$max <- 1
        args$step <- 0.05
      }
      if (sc == 15L) {
        args$step <- 1
      }
      if (sc %in% c(16L, 17L, 18L)) {
        args$step <- 1
      }
      do.call(numericInput, args)
    })

    power_curve_context_raw <- reactive({
      req(input$power_s, input$power_curve, input$sample_size_mode == 1, input$sample_size_type)
      sample_calc <- effective_sample_calc(input$sample_size_type, input$sample_calc)
      req(!is.na(sample_calc))
      S <- input$power_curve_start
      I <- input$power_curve_interval
      req(!is.null(S), !is.null(I), is.finite(S), is.finite(I), I > 0)

      alt <- input$one_or_two_size
      s_size_n <- input$s_size_n
      s_sizeUI1 <- input$s_sizeUI1
      s_sizeUI2 <- input$s_sizeUI2
      s_sizeUI3 <- input$s_sizeUI3
      s_sizeUI4 <- input$s_sizeUI4
      s_size_alpha <- input$s_size_alpha
      req(s_size_n, s_sizeUI1, s_size_alpha, alt)

      if (sample_calc == 7 || sample_calc == 9 || sample_calc == 10 || sample_calc == 15) {
        req(s_sizeUI4)
      }
      if (sample_calc == 3 || sample_calc == 6 || sample_calc == 7 || sample_calc == 10 || sample_calc == 18 || sample_calc == 19) {
        req(s_sizeUI3)
      }
      if (!power_curve_uses_ui4(sample_calc)) {
        req(s_sizeUI2)
      }

      list(
        sample_calc = sample_calc,
        S = S,
        I = I,
        alt = alt,
        s_size_n = s_size_n,
        s_sizeUI1 = s_sizeUI1,
        s_sizeUI2 = s_sizeUI2,
        s_sizeUI3 = s_sizeUI3,
        s_sizeUI4 = s_sizeUI4,
        s_size_alpha = s_size_alpha
      )
    })

    power_curve_context <- debounce(power_curve_context_raw, millis = 800)

    power_curve_sweep_data <- reactive({
      req(isTRUE(input$power_curve))
      ctx <- power_curve_context()
      req(!is.null(ctx))
      power_curve_compute_sweep(ctx)
    })

    output$power_curve_notice <- renderUI({
      req(input$power_curve)
      sweep <- power_curve_sweep_data()
      df <- sweep$table
      if (is.null(df)) {
        return(NULL)
      }
      if (!any(df$valid)) {
        return(p(
          style = "color: #a94442;",
          "No swept values are in the valid range. Adjust the expected value or interval."
        ))
      }
      if (any(!df$valid)) {
        return(p(
          style = "color: #8a6d3b;",
          "Power not calculated for rows outside the valid range (proportions 0\u20131; Poisson rates: \u2265 0; standard deviation > 0; correlations -1 to 1)."
        ))
      }
      NULL
    })

    output$power_curve_table <- renderTable({
      req(input$power_curve)
      df <- power_curve_sweep_data()$table
      if (is.null(df)) {
        return(NULL)
      }
      sample_calc <- effective_sample_calc(input$sample_size_type, input$sample_calc)
      if (sample_calc %in% c(11L, 19L)) {
        df <- df[df$parameter > -1 & df$parameter < 1, , drop = FALSE]
      }
      param_col <- power_curve_param_label_table(sample_calc)
      out <- data.frame(
        Parameter = df$parameter,
        `Power (%)` = ifelse(is.na(df$power_pct), "NA", round(df$power_pct, 4)),
        check.names = FALSE
      )
      names(out)[1] <- param_col
      out
    },
    rownames = FALSE,
    digits = 4)

    output$power_curve_plot <- renderPlot({
      req(input$power_curve)
      sweep <- power_curve_sweep_data()
      plot_df <- sweep$plot
      table_df <- sweep$table
      req(nrow(plot_df) > 0)
      sample_calc <- effective_sample_calc(input$sample_size_type, input$sample_calc)
      req(!is.na(sample_calc))
      plot_line <- plot_df[!is.na(plot_df$power_pct), , drop = FALSE]
      if (nrow(plot_line) < 2) {
        plot.new()
        text(0.5, 0.5, "No valid points to plot", cex = 1.2)
        return(invisible(NULL))
      }
      x_lab <- power_curve_param_label_text(sample_calc)
      table_pts <- table_df[table_df$valid & !is.na(table_df$power_pct), , drop = FALSE]
      cols <- power_curve_colors()
      xlim_use <- if (sample_calc %in% c(11L, 19L)) {
        c(-1, 1)
      } else {
        range(c(plot_line$parameter, table_pts$parameter), na.rm = TRUE)
      }
      graphics::plot(
        plot_line$parameter,
        plot_line$power_pct,
        type = "l",
        col = cols$col_plot_line,
        lwd = 2,
        xlab = x_lab,
        ylab = "Power (%)",
        ylim = c(0, 100),
        xlim = xlim_use
      )
      if (nrow(table_pts) > 0) {
        graphics::points(
          table_pts$parameter,
          table_pts$power_pct,
          pch = 16,
          col = cols$col_line_control_chart
        )
      }
      invisible(NULL)
    })
  })
}
