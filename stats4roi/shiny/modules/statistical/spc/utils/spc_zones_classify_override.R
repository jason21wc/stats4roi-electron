# Overrides for lolcat SPC control-violation helpers that error on NA inputs.
# Individual charts with MR-based limits can produce NA chart points or limits
# (e.g. first span-1 observations), which crashes the stock lolcat implementations.
#
# TO REMOVE THIS OVERRIDE WHEN lolcat IS UPDATED:
# 1. Remove the source() statement from stats4ROI_mod.R (and deployment copy)
# 2. Run tests/testthat/test-spc-utils.R
# 3. Optionally delete or archive this file

if (!exists(".spc_lolcat_patch_env", envir = .GlobalEnv, inherits = FALSE)) {
  assign(".spc_lolcat_patch_env", new.env(parent = emptyenv()), envir = .GlobalEnv)
}
.spc_lolcat_patch_env <- get(".spc_lolcat_patch_env", envir = .GlobalEnv)

if (!exists("zones_classify", envir = .spc_lolcat_patch_env, inherits = FALSE)) {
  .spc_lolcat_patch_env$zones_classify <- get(
    "spc.controlviolation.zones.classify",
    envir = asNamespace("lolcat")
  )
  .spc_lolcat_patch_env$evaluate_rules <- get(
    "spc.controlviolation.evaluate.rules",
    envir = asNamespace("lolcat")
  )
}

spc.controlviolation.zones.classify <- function(
    chart.series = NA,
    center.line = NA,
    control.limits.ucl = NA,
    zone.a.upper = NA,
    zone.ab.upper = NA,
    zone.bc.upper = NA,
    control.limits.lcl = NA,
    zone.a.lower = NA,
    zone.ab.lower = NA,
    zone.bc.lower = NA,
    ...) {
  vapply(
    seq_along(chart.series),
    FUN = function(i) {
      vals <- c(
        chart.series[i],
        center.line[i],
        zone.a.upper[i],
        zone.ab.upper[i],
        zone.bc.upper[i],
        zone.a.lower[i],
        zone.ab.lower[i],
        zone.bc.lower[i]
      )
      if (any(is.na(vals))) {
        return(NA_character_)
      }

      if (chart.series[i] > center.line[i]) {
        if (chart.series[i] > zone.a.upper[i]) {
          "UU"
        } else if (chart.series[i] <= zone.a.upper[i] && chart.series[i] > zone.ab.upper[i]) {
          "UA"
        } else if (chart.series[i] <= zone.ab.upper[i] && chart.series[i] > zone.bc.upper[i]) {
          "UB"
        } else if (chart.series[i] <= zone.bc.upper[i]) {
          "UC"
        } else {
          NA_character_
        }
      } else if (chart.series[i] < center.line[i]) {
        if (chart.series[i] < zone.a.lower[i]) {
          "LL"
        } else if (chart.series[i] >= zone.a.lower[i] && chart.series[i] < zone.ab.lower[i]) {
          "LA"
        } else if (chart.series[i] >= zone.ab.lower[i] && chart.series[i] < zone.bc.lower[i]) {
          "LB"
        } else if (chart.series[i] >= zone.bc.lower[i]) {
          "LC"
        } else {
          NA_character_
        }
      } else {
        "CC"
      }
    },
    FUN.VALUE = character(1)
  )
}

spc.controlviolation.evaluate.rules <- function(
    control.rules = NULL,
    chart.series = NA,
    center.line = NA,
    control.limits.ucl = NA,
    zone.a.upper = NA,
    zone.ab.upper = NA,
    zone.bc.upper = NA,
    control.limits.lcl = NA,
    zone.a.lower = NA,
    zone.ab.lower = NA,
    zone.bc.lower = NA,
    ...) {
  n <- length(chart.series)
  center.line <- rep_len(center.line, n)

  invalid <- is.na(chart.series) |
    is.na(center.line) |
    is.na(rep_len(control.limits.ucl, n)) |
    is.na(rep_len(control.limits.lcl, n))

  safe_series <- chart.series
  replace_na <- is.na(safe_series) & !is.na(center.line)
  safe_series[replace_na] <- center.line[replace_na]
  safe_series[is.na(safe_series)] <- 0

  res <- .spc_lolcat_patch_env$evaluate_rules(
    control.rules = control.rules,
    chart.series = safe_series,
    center.line = center.line,
    control.limits.ucl = control.limits.ucl,
    zone.a.upper = zone.a.upper,
    zone.ab.upper = zone.ab.upper,
    zone.bc.upper = zone.bc.upper,
    control.limits.lcl = control.limits.lcl,
    zone.a.lower = zone.a.lower,
    zone.ab.lower = zone.ab.lower,
    zone.bc.lower = zone.bc.lower,
    ...
  )

  if (any(invalid)) {
    res$overall.results[invalid] <- FALSE
    for (rule_name in names(res$rule.results)) {
      res$rule.results[[rule_name]][invalid] <- FALSE
    }
  }

  res
}

if (!exists("patched", envir = .spc_lolcat_patch_env, inherits = FALSE)) {
  assignInNamespace(
    x = "spc.controlviolation.zones.classify",
    value = spc.controlviolation.zones.classify,
    ns = "lolcat"
  )

  assignInNamespace(
    x = "spc.controlviolation.evaluate.rules",
    value = spc.controlviolation.evaluate.rules,
    ns = "lolcat"
  )

  .spc_lolcat_patch_env$patched <- TRUE
}
