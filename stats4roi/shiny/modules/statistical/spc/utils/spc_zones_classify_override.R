# Overrides for lolcat SPC control-violation helpers that error on NA inputs.
# Individual charts with MR-based limits can produce NA chart points or limits
# (e.g. first span-1 observations), which crashes the stock lolcat implementations.
#
# TO REMOVE THIS OVERRIDE WHEN lolcat IS UPDATED:
# 1. Remove the source() statement from stats4ROI_mod.R (and deployment copy)
# 2. Run tests/testthat/test-spc-utils.R
# 3. Optionally delete or archive this file
#
# IMPORTANT: originals live in options() (survives rm(list=ls())). Wrappers are
# factory closures over those originals. Never re-capture an already-patched
# namespace function as the "original" — that causes infinite recursion /
# "node stack overflow" in SPC charts after a messy RStudio re-run.

.stats4roi_lolcat_patch_flag <- "stats4roi_lolcat_spc_patch"
.stats4roi_lolcat_orig_option <- "stats4roi.lolcat_spc_originals"

.stats4roi_reload_lolcat_namespace <- function() {
  # Recover true package bindings when a prior session left a patched namespace
  # but lost the saved originals (e.g. rm(list=ls()) then Run App again).
  if ("package:lolcat" %in% search()) {
    try(detach("package:lolcat", unload = TRUE, character.only = TRUE), silent = TRUE)
  }
  if ("lolcat" %in% loadedNamespaces()) {
    try(unloadNamespace("lolcat"), silent = TRUE)
  }
  loadNamespace("lolcat")
  if (!"package:lolcat" %in% search()) {
    # Prefer library() so exports are on the search path like a normal app start
    suppressPackageStartupMessages(library(lolcat, quietly = TRUE))
  }
  invisible(TRUE)
}

.stats4roi_is_patched_evaluate_rules <- function(fn) {
  if (!is.function(fn)) {
    return(FALSE)
  }
  if (isTRUE(attr(fn, .stats4roi_lolcat_patch_flag))) {
    return(TRUE)
  }
  # Legacy override (pre-factory) looked up .spc_lolcat_patch_env at call time.
  body_txt <- paste(deparse(body(fn)), collapse = "\n")
  grepl("spc_lolcat_patch_env|stats4roi_lolcat", body_txt, fixed = FALSE)
}

.stats4roi_recover_evaluate_rules_original <- function(cur_eval) {
  if (!.stats4roi_is_patched_evaluate_rules(cur_eval)) {
    return(cur_eval)
  }

  # Factory wrapper: original is closed over as `original`.
  wrap_env <- environment(cur_eval)
  if (exists("original", envir = wrap_env, inherits = FALSE)) {
    recovered <- get("original", envir = wrap_env)
    if (is.function(recovered) && !.stats4roi_is_patched_evaluate_rules(recovered)) {
      return(recovered)
    }
  }

  # Legacy env may still hold the true original (unless it was overwritten with self).
  if (exists(".spc_lolcat_patch_env", envir = .GlobalEnv, inherits = FALSE)) {
    pe <- get(".spc_lolcat_patch_env", envir = .GlobalEnv)
    if (exists("evaluate_rules", envir = pe, inherits = FALSE)) {
      recovered <- get("evaluate_rules", envir = pe)
      if (is.function(recovered) &&
          !identical(recovered, cur_eval) &&
          !.stats4roi_is_patched_evaluate_rules(recovered)) {
        return(recovered)
      }
    }
  }

  NULL
}

.stats4roi_get_lolcat_originals <- function() {
  saved <- getOption(.stats4roi_lolcat_orig_option)
  if (is.list(saved) &&
      is.function(saved$evaluate_rules) &&
      is.function(saved$zones_classify) &&
      !.stats4roi_is_patched_evaluate_rules(saved$evaluate_rules)) {
    return(saved)
  }

  if (!requireNamespace("lolcat", quietly = TRUE)) {
    stop("Package 'lolcat' is required for SPC control-violation overrides.", call. = FALSE)
  }

  ns <- asNamespace("lolcat")
  cur_eval <- get("spc.controlviolation.evaluate.rules", envir = ns)
  cur_zones <- get("spc.controlviolation.zones.classify", envir = ns)

  recovered_eval <- .stats4roi_recover_evaluate_rules_original(cur_eval)
  if (is.function(recovered_eval)) {
    cur_eval <- recovered_eval
  }

  # If evaluate.rules is still a patch (legacy self-wrap or lost original), reload.
  if (.stats4roi_is_patched_evaluate_rules(cur_eval)) {
    .stats4roi_reload_lolcat_namespace()
    ns <- asNamespace("lolcat")
    cur_eval <- get("spc.controlviolation.evaluate.rules", envir = ns)
    cur_zones <- get("spc.controlviolation.zones.classify", envir = ns)
  }

  if (.stats4roi_is_patched_evaluate_rules(cur_eval)) {
    stop(
      "Unable to recover original lolcat::spc.controlviolation.evaluate.rules; ",
      "restart the R session.",
      call. = FALSE
    )
  }

  saved <- list(
    evaluate_rules = cur_eval,
    zones_classify = cur_zones
  )
  options(stats4roi.lolcat_spc_originals = saved)
  saved
}

.stats4roi_make_zones_classify <- function() {
  fn <- function(
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
  attr(fn, .stats4roi_lolcat_patch_flag) <- TRUE
  fn
}

.stats4roi_make_evaluate_rules <- function(original) {
  force(original)
  fn <- function(
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

    res <- original(
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
  attr(fn, .stats4roi_lolcat_patch_flag) <- TRUE
  fn
}

.stats4roi_lolcat_originals <- .stats4roi_get_lolcat_originals()

spc.controlviolation.zones.classify <- .stats4roi_make_zones_classify()
spc.controlviolation.evaluate.rules <- .stats4roi_make_evaluate_rules(
  .stats4roi_lolcat_originals$evaluate_rules
)

# Keep a small env for tests/debug that expect .spc_lolcat_patch_env
if (!exists(".spc_lolcat_patch_env", envir = .GlobalEnv, inherits = FALSE)) {
  assign(".spc_lolcat_patch_env", new.env(parent = emptyenv()), envir = .GlobalEnv)
}
.spc_lolcat_patch_env <- get(".spc_lolcat_patch_env", envir = .GlobalEnv)
.spc_lolcat_patch_env$evaluate_rules <- .stats4roi_lolcat_originals$evaluate_rules
.spc_lolcat_patch_env$zones_classify <- .stats4roi_lolcat_originals$zones_classify
.spc_lolcat_patch_env$patched <- TRUE

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
