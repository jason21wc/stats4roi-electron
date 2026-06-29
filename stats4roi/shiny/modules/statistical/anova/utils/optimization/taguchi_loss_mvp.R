# MVP: Taguchi-style loss grid from reduced mean lm + parallel dispersion lm + emmeans.
# See multifactor ems_sig_effects_plot for reduced-model semantics.

if (!exists("build_resolution_context", mode = "function")) {
  of <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(of) && nzchar(of)) {
    opt_dir_local <- dirname(normalizePath(of, winslash = "/", mustWork = FALSE))
    ah <- normalizePath(file.path(opt_dir_local, "..", "anova_helpers.R"), winslash = "/", mustWork = FALSE)
    drp <- file.path(opt_dir_local, "dispersion_resolution_prior.R")
    if (file.exists(ah) && !exists("factorial_cell_replication", mode = "function")) {
      source(ah, local = FALSE)
    }
    if (file.exists(drp)) source(drp, local = FALSE)
  }
}

#' Plain-text Taguchi loss disclaimer (Unicode symbols; for API / non-HTML consumers).
#' @param kind `"multifactor"` or `"oneway"`.
#' @keywords internal
taguchi_loss_disclaimer_plain <- function(kind = c("multifactor", "oneway")) {
  kind <- match.arg(kind)
  cl_cu <- "C\u2097/C\u1D64"
  if (identical(kind, "oneway")) {
    return(paste(
      sprintf("Normal surrogate; side-specific one-sided loss/ppm with independent %s.", cl_cu),
      "Single-factor reduced model with exact normal second-moment loss scaling."
    ))
  }
  paste(
    sprintf("Normal surrogate; side-specific one-sided loss/ppm with independent %s.", cl_cu),
    "Uses exact normal second moment ((\u03bc\u2212T)\u00b2 + \u03c3\u00b2) with side-specific scaling;",
    "total loss is the sum of active-side contributions; \u03c3 from predicted dispersion",
    "(\u221a(\u03c0/2) \u00d7 metric; refine in Specify if needed)."
  )
}

#' @keywords internal
.taguchi_standardize_interaction <- function(factor) {
  terms <- unlist(strsplit(factor, ":", fixed = TRUE))
  paste(sort(terms), collapse = ":")
}

#' @keywords internal
.taguchi_pvalue_column <- function(aov_df) {
  if ("Pvalue" %in% names(aov_df)) return("Pvalue")
  if ("Pr(>F)" %in% names(aov_df)) return("Pr(>F)")
  nm <- names(aov_df)[grepl("^Pr\\(", names(aov_df))][1]
  if (length(nm) == 1L && !is.na(nm)) return(nm)
  NULL
}

#' @keywords internal
.taguchi_sig_rhs_parts <- function(aov_out_l, conf, random_factors = character(0)) {
  if (exists("multifactor_reduced_rhs_parts_from_anova", mode = "function")) {
    parts <- multifactor_reduced_rhs_parts_from_anova(
      aov_out_l,
      conf,
      random_factor_names = random_factors
    )
    if (is.null(parts)) {
      return(NULL)
    }
    return(parts)
  }
  if (!is.data.frame(aov_out_l) || nrow(aov_out_l) < 1L) {
    return(NULL)
  }
  pc <- .taguchi_pvalue_column(aov_out_l)
  if (is.null(pc)) return(NULL)

  aov2 <- aov_out_l
  rn <- rownames(aov2)
  if ("(Intercept)" %in% rn) {
    aov2 <- aov2[rn != "(Intercept)", , drop = FALSE]
    rn <- rownames(aov2)
  }
  keep <- !(rn %in% c("Residuals", "Residual", "Within Cells"))
  aov2 <- aov2[keep, , drop = FALSE]
  rn <- rownames(aov2)
  if (nrow(aov2) == 0L) return(NULL)

  pv <- suppressWarnings(as.numeric(as.character(aov2[[pc]])))
  sig_rn <- rn[!is.na(pv) & pv <= (1 - conf)]
  sig_rn <- sig_rn[sig_rn != "NA"]
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
  if (length(filtered_factors) > 0) rhs_parts <- c(rhs_parts, filtered_factors)
  if (length(all_sig_interactions) > 0) {
    if (exists("anova_effect_to_formula_term", mode = "function")) {
      rhs_parts <- c(
        rhs_parts,
        vapply(all_sig_interactions, anova_effect_to_formula_term, character(1))
      )
    } else {
      rhs_parts <- c(rhs_parts, all_sig_interactions)
    }
  }
  if (length(sig_main) > 0) {
    if (exists("anova_effect_to_formula_term", mode = "function")) {
      rhs_parts <- c(rhs_parts, vapply(sig_main, anova_effect_to_formula_term, character(1)))
    } else {
      rhs_parts <- c(rhs_parts, sig_main)
    }
  }
  if (length(rhs_parts) == 0L) return(NULL)
  unique(rhs_parts)
}

#' Drop model terms that include random/block/excluded factors.
#' @keywords internal
.taguchi_filter_random_terms <- function(rhs_parts, random_factors) {
  if (length(rhs_parts) < 1L || length(random_factors) < 1L) {
    return(rhs_parts)
  }
  keep <- vapply(rhs_parts, function(term) {
    parts <- unlist(strsplit(term, ":", fixed = TRUE))
    !any(parts %in% random_factors)
  }, logical(1))
  rhs_parts[keep]
}

#' Split an lm RHS string into individual term labels.
#' @keywords internal
.taguchi_rhs_to_terms <- function(rhs) {
  rhs <- trimws(as.character(rhs))
  if (length(rhs) < 1L || !nzchar(rhs) || identical(rhs, "1")) return(character(0))
  trimws(unlist(strsplit(rhs, "+", fixed = TRUE)))
}

#' TRUE when RHS is a legacy compact full-factorial star (e.g. \code{block*a*b}), not \code{a+b+a*b}.
#' @keywords internal
.mf_is_compact_factorial_star_rhs <- function(rhs) {
  rhs <- trimws(as.character(rhs)[1])
  if (!nzchar(rhs) || identical(rhs, "1") || grepl("+", rhs, fixed = TRUE)) {
    return(FALSE)
  }
  grepl("*", rhs, fixed = TRUE)
}

#' Expand a compact star RHS (e.g. \code{a*b*c}) into factor names.
#' @keywords internal
.taguchi_star_rhs_factors <- function(rhs) {
  rhs <- trimws(as.character(rhs)[1])
  if (!.mf_is_compact_factorial_star_rhs(rhs)) {
    return(character(0))
  }
  parts <- trimws(unlist(strsplit(rhs, "*", fixed = TRUE)))
  parts[nzchar(parts)]
}

#' RHS after removing terms that involve excluded factors; fallback to intercept-only.
#' @keywords internal
.taguchi_rhs_after_excluding <- function(rhs, all_factors, excluded_factors) {
  rhs <- trimws(as.character(rhs)[1])
  if (identical(rhs, "1")) return("1")

  star_factors <- .taguchi_star_rhs_factors(rhs)
  if (length(star_factors) > 0L && !grepl("+", rhs, fixed = TRUE)) {
    remaining_star <- setdiff(star_factors, as.character(excluded_factors))
    if (length(remaining_star) < 1L) return("1")
    # Legacy full-factorial star storage: never expand to a residual saturated product.
    return("1")
  }

  terms <- .taguchi_rhs_to_terms(rhs)
  filtered <- .taguchi_filter_random_terms(terms, excluded_factors)
  if (length(filtered) == 0L) return("1")
  paste(filtered, collapse = "+")
}

#' Factors for within-cell ADA/ADM grouping (matches Results / Graphs tab).
#' @keywords internal
.taguchi_disp_cell_factors <- function(
    factors_names,
    pool_disp = character(0),
    available_effects = NULL) {
  factors_names <- as.character(factors_names)
  pool_disp <- as.character(pool_disp)
  pool_disp <- pool_disp[nzchar(pool_disp)]
  if (exists("dispersion_cell_factors", mode = "function")) {
    return(dispersion_cell_factors(
      factors_names,
      pooled_effects = pool_disp,
      available_effects = if (is.null(available_effects) || length(available_effects) < 1L) {
        factors_names
      } else {
        available_effects
      }
    ))
  }
  out <- setdiff(factors_names, pool_disp)
  if (length(out) < 1L) factors_names else out
}

#' Resolve within-cell dispersion factors for a committed registry fit (Results parity).
#' @keywords internal
.mf_fit_disp_cell_factors <- function(fit, factors_names, blocked = character(0)) {
  factors_names <- as.character(factors_names)
  factors_names <- factors_names[nzchar(factors_names)]
  pool_disp_fit <- if (!is.null(fit$pool_disp)) as.character(fit$pool_disp) else character(0)
  pool_disp_fit <- pool_disp_fit[nzchar(pool_disp_fit)]
  disp_cell_factors <- character(0)
  if (!is.null(fit$disp_cell_factors) && length(fit$disp_cell_factors) > 0L) {
    disp_cell_factors <- as.character(fit$disp_cell_factors)
    disp_cell_factors <- disp_cell_factors[disp_cell_factors %in% factors_names]
  }
  if (length(disp_cell_factors) < 1L) {
    ae <- if (!is.null(fit$available_effects)) as.character(fit$available_effects) else NULL
    disp_cell_factors <- .taguchi_disp_cell_factors(
      factors_names,
      pool_disp = pool_disp_fit,
      available_effects = ae
    )
  }
  blocked <- as.character(blocked)
  blocked <- blocked[nzchar(blocked)]
  if (length(blocked) > 0L) {
    disp_cell_factors <- setdiff(disp_cell_factors, blocked)
  }
  if (length(disp_cell_factors) < 1L) {
    disp_cell_factors <- setdiff(factors_names, blocked)
  }
  disp_cell_factors
}

#' Refresh dispersion column/model for loss grid when registry data is stale.
#' @keywords internal
.mf_loss_grid_prepare_dispersion <- function(fit, factors_names, blocked = character(0)) {
  if (is.null(fit) || !isTRUE(fit$ok)) {
    return(list(ok = FALSE, message = "Committed fit is not ready for loss grid dispersion."))
  }
  dat <- fit$dat
  if (is.null(dat) || !is.data.frame(dat) || nrow(dat) < 1L) {
    return(list(ok = FALSE, message = "Committed fit data is missing for loss grid."))
  }
  disp_cell_factors <- .mf_fit_disp_cell_factors(fit, factors_names, blocked = blocked)
  disp_type <- if (!is.null(fit$disp_type)) as.character(fit$disp_type)[1] else "ADM"
  dat <- .taguchi_compute_disp_column(
    dat,
    fit$response_name,
    disp_cell_factors,
    disp_type = disp_type
  )
  disp_chk <- suppressWarnings(as.numeric(dat$.taguchi_disp))
  if (!".taguchi_disp" %in% names(dat) ||
      !any(is.finite(disp_chk) & disp_chk > 0, na.rm = TRUE)) {
    return(list(
      ok = FALSE,
      message = paste0(
        "Could not compute within-cell ",
        disp_type,
        " values for the loss grid; check dispersion pooling and factorial cell replication on the Results tab."
      )
    ))
  }
  disp_mod <- .taguchi_refit_disp_mod(
    dat,
    fit$rhs_lm_disp,
    factors_names,
    disp_intercept_only = isTRUE(fit$disp_intercept_only)
  )
  if (is.null(disp_mod)) {
    return(list(
      ok = FALSE,
      message = "Could not refit the committed dispersion model after refreshing cell dispersion values."
    ))
  }
  list(
    ok = TRUE,
    dat = dat,
    disp_mod = disp_mod,
    disp_cell_factors = disp_cell_factors
  )
}

#' Refresh within-cell dispersion and dispersion lm for optimizer (parity with loss grid).
#' @keywords internal
mf_refresh_fit_dispersion_for_optimizer <- function(fit, blocked = character(0)) {
  if (is.null(fit) || !isTRUE(fit$ok)) {
    return(fit)
  }
  factors_names <- as.character(fit$factors_names)
  factors_names <- factors_names[nzchar(factors_names)]
  if (length(factors_names) < 1L) {
    return(fit)
  }
  prep <- .mf_loss_grid_prepare_dispersion(fit, factors_names, blocked = blocked)
  if (!isTRUE(prep$ok)) {
    return(fit)
  }
  fit$dat <- prep$dat
  fit$disp_mod <- prep$disp_mod
  fit$disp_cell_factors <- prep$disp_cell_factors
  fit
}

#' Refresh dispersion model and resolution context on an optimizer bundle.
#' @keywords internal
mf_refresh_bundle_dispersion_for_optimizer <- function(bundle, blocked = character(0)) {
  if (is.null(bundle) || !is.list(bundle) || is.null(bundle$disp_model) || is.null(bundle$dat)) {
    return(bundle)
  }
  mean_mod <- bundle$mean_model
  if (is.null(mean_mod)) {
    return(bundle)
  }
  factors_names <- names(mean_mod$xlevels)
  blocked <- as.character(blocked)
  blocked <- blocked[nzchar(blocked)]
  fit_like <- list(
    ok = TRUE,
    dat = bundle$dat,
    response_name = bundle$response_name,
    rhs_lm_disp = bundle$rhs_lm_disp,
    disp_intercept_only = bundle$disp_intercept_only,
    disp_type = bundle$disp_type,
    pool_disp = bundle$pool_disp,
    disp_cell_factors = bundle$disp_cell_factors,
    available_effects = bundle$available_effects,
    factors_names = factors_names
  )
  prep <- .mf_loss_grid_prepare_dispersion(fit_like, factors_names, blocked = blocked)
  if (!isTRUE(prep$ok)) {
    return(bundle)
  }
  bundle$dat <- prep$dat
  bundle$disp_model <- prep$disp_mod
  bundle$disp_cell_factors <- prep$disp_cell_factors
  conf <- 0.95
  delta_user <- NULL
  if (!is.null(bundle$resolution_ctx) && is.list(bundle$resolution_ctx)) {
    conf_i <- suppressWarnings(as.numeric(bundle$resolution_ctx$confidence))[1]
    if (is.finite(conf_i) && conf_i > 0 && conf_i < 1) conf <- conf_i
    delta_user <- bundle$resolution_ctx$delta_user
  }
  disp_type <- if (!is.null(bundle$disp_type)) as.character(bundle$disp_type)[1] else "ADM"
  bundle$resolution_ctx <- build_resolution_context(
    dat = prep$dat,
    response_name = bundle$response_name,
    cell_factors = prep$disp_cell_factors,
    disp_type = disp_type,
    confidence = conf,
    delta_user = delta_user
  )
  bundle
}

#' Compute \code{.taguchi_disp} using active-model cell factors (not raw factor list).
#' @keywords internal
.taguchi_compute_disp_column <- function(
    dat,
    response_name,
    disp_cell_factors,
    disp_type = "ADM") {
  if (!is.data.frame(dat) || length(disp_cell_factors) < 1L) return(dat)
  response_name <- as.character(response_name)[1]
  disp_cell_factors <- as.character(disp_cell_factors)
  disp_cell_factors <- disp_cell_factors[disp_cell_factors %in% names(dat)]
  if (length(disp_cell_factors) < 1L || !response_name %in% names(dat)) return(dat)
  disp_type <- as.character(disp_type)[1]
  if (is.na(disp_type) || !nzchar(disp_type)) disp_type <- "ADM"
  full_fac_form <- stats::as.formula(paste(response_name, "~", paste(disp_cell_factors, collapse = "*")))
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  disp_vals <- tryCatch(
    if (disp_type == "ADA") {
      lolcat::compute.group.dispersion.ADA(full_fac_form, data = dat)
    } else if (disp_type == "ADM") {
      lolcat::compute.group.dispersion.ADM(full_fac_form, data = dat)
    } else {
      lolcat::compute.group.dispersion.ADMn1(full_fac_form, data = dat)
    },
    error = function(e) NULL
  )
  if (!is.null(disp_vals) && length(disp_vals) == nrow(dat)) {
    dat$.taguchi_disp <- disp_vals
  }
  dat
}

#' Ensure \code{.taguchi_disp} exists on fit data (registry means-only commits may omit it).
#' @keywords internal
.taguchi_ensure_disp_column <- function(
    dat,
    response_name,
    factors_names,
    disp_type = "ADM",
    pool_disp = character(0),
    available_effects = NULL,
    force = FALSE) {
  if (!is.data.frame(dat)) return(dat)
  if (!force && ".taguchi_disp" %in% names(dat)) return(dat)
  disp_cell_factors <- .taguchi_disp_cell_factors(
    factors_names,
    pool_disp = pool_disp,
    available_effects = available_effects
  )
  .taguchi_compute_disp_column(dat, response_name, disp_cell_factors, disp_type = disp_type)
}

#' Whether the stored dispersion RHS is intercept-only (ignore stale boolean flags).
#' @keywords internal
.mf_disp_rhs_is_intercept_only <- function(rhs_lm_disp) {
  rhs <- trimws(as.character(rhs_lm_disp)[1])
  !nzchar(rhs) || identical(rhs, "1")
}

#' Refit dispersion lm from stored RHS after refreshing \code{.taguchi_disp}.
#' @keywords internal
.taguchi_refit_disp_mod <- function(dat, rhs_lm_disp, factors_names, disp_intercept_only = FALSE) {
  if (is.null(dat) || !is.data.frame(dat) || !".taguchi_disp" %in% names(dat)) return(NULL)
  rhs_lm_disp <- trimws(as.character(rhs_lm_disp)[1])
  if (!nzchar(rhs_lm_disp)) return(NULL)
  if (.mf_disp_rhs_is_intercept_only(rhs_lm_disp)) {
    rhs_lm_disp <- "1"
  }
  tryCatch(
    .taguchi_fit_lm_contr_sum(
      paste(".taguchi_disp ~", rhs_lm_disp),
      dat,
      .taguchi_factors_from_rhs(rhs_lm_disp, factors_names)
    ),
    error = function(e) NULL
  )
}

#' Refit mean/dispersion models for optimization after dropping blocked-factor terms.
#'
#' ANOVA finalization models are unchanged; this is applied only on the optimization path.
#' Blocked factors are removed from prediction formulas and from the optimizer search space.
#' @keywords internal
multifactor_taguchi_mvp_refit_excluding_factors <- function(fit, excluded_factors) {
  if (!isTRUE(fit$ok)) return(fit)
  excluded <- unique(as.character(excluded_factors))
  excluded <- excluded[nzchar(excluded)]
  if (length(excluded) < 1L) return(fit)
  fnames <- as.character(fit$factors_names)
  excluded <- intersect(excluded, fnames)
  if (length(excluded) < 1L) return(fit)
  remaining <- setdiff(fnames, excluded)
  if (length(remaining) < 1L) {
    return(list(ok = FALSE, message = "Cannot optimize: every factor is marked blocked."))
  }

  dat <- fit$dat
  response_name <- fit$response_name
  disp_type <- if (!is.null(fit$disp_type)) fit$disp_type else "ADM"
  pool_disp_fit <- if (!is.null(fit$pool_disp)) as.character(fit$pool_disp) else character(0)
  dat <- .taguchi_ensure_disp_column(
    dat,
    response_name,
    fnames,
    disp_type = disp_type,
    pool_disp = pool_disp_fit,
    available_effects = fit$available_effects,
    force = !".taguchi_disp" %in% names(dat)
  )
  fit$dat <- dat
  rhs_lm_mean <- .taguchi_rhs_after_excluding(fit$rhs_lm_mean, fnames, excluded)
  if (identical(trimws(rhs_lm_mean), "1") && .mf_is_compact_factorial_star_rhs(fit$rhs_lm_mean)) {
    return(list(
      ok = FALSE,
      message = paste(
        "Blocked-factor optimization requires a committed reduced means model, not a legacy full-factorial fit.",
        "Re-commit the means model on the Results tab, then run the optimizer again."
      )
    ))
  }
  rhs_lm_disp <- if (.mf_disp_rhs_is_intercept_only(fit$rhs_lm_disp)) {
    "1"
  } else if (!is.null(fit$rhs_lm_disp)) {
    .taguchi_rhs_after_excluding(fit$rhs_lm_disp, fnames, excluded)
  } else {
    NULL
  }

  if (!is.null(rhs_lm_disp) && !".taguchi_disp" %in% names(dat)) {
    return(list(ok = FALSE, message = "Could not refit models after applying blocked factors."))
  }

  mean_mod <- tryCatch(
    .taguchi_fit_lm_contr_sum(
      paste(response_name, "~", rhs_lm_mean),
      dat,
      .taguchi_factors_from_rhs(rhs_lm_mean, remaining)
    ),
    error = function(e) NULL
  )
  disp_mod <- if (!is.null(rhs_lm_disp)) {
    tryCatch(
      .taguchi_fit_lm_contr_sum(
        paste(".taguchi_disp ~", rhs_lm_disp),
        dat,
        .taguchi_factors_from_rhs(rhs_lm_disp, remaining)
      ),
      error = function(e) NULL
    )
  } else {
    fit$disp_mod
  }

  if (is.null(mean_mod) || is.null(disp_mod)) {
    return(list(ok = FALSE, message = "Could not refit models after applying blocked factors."))
  }

  note <- paste0(
    "Blocked factors removed from optimization predictions (terms dropped; settings averaged over blocked levels): ",
    paste(excluded, collapse = ", "),
    "."
  )
  fit_notes <- c(if (!is.null(fit$message)) as.character(fit$message) else character(0), note)

  fit$mean_mod <- mean_mod
  fit$disp_mod <- disp_mod
  fit$rhs_lm_mean <- rhs_lm_mean
  fit$rhs_lm_disp <- rhs_lm_disp
  fit$factors_names <- remaining
  fit$optimizer_factor_names <- .taguchi_factors_from_rhs(rhs_lm_mean, remaining)
  fit$blocked_factors <- excluded
  fit$message <- paste(trimws(fit_notes[nzchar(fit_notes)]), collapse = " ")
  fit
}

#' Refit one optimizer bundle after dropping blocked-factor terms.
#' @keywords internal
mf_refit_bundle_excluding_factors <- function(bundle, excluded_factors) {
  if (is.null(bundle) || !is.list(bundle) || length(excluded_factors) < 1L) return(bundle)
  mean_mod <- bundle$mean_model
  disp_mod <- bundle$disp_model
  dat <- bundle$dat
  if (is.null(mean_mod) || is.null(disp_mod) || is.null(dat)) return(bundle)

  xvars <- names(mean_mod$xlevels)
  excluded <- intersect(as.character(excluded_factors), xvars)
  if (length(excluded) < 1L) return(bundle)
  remaining <- setdiff(xvars, excluded)

  resp <- as.character(all.vars(stats::formula(mean_mod)))[1L]
  disp_intercept_only <- isTRUE(bundle$disp_intercept_only)
  disp_type <- if (!is.null(bundle$disp_type)) bundle$disp_type else "ADM"
  dat <- .taguchi_ensure_disp_column(dat, bundle$response_name, xvars, disp_type = disp_type)
  bundle$dat <- dat
  disp_resp <- as.character(all.vars(stats::formula(disp_mod)))[1L]
  mean_terms <- attr(stats::terms(mean_mod), "term.labels")
  disp_terms <- attr(stats::terms(disp_mod), "term.labels")

  rhs_mean_src <- if (!is.null(bundle$rhs_lm_mean) && nzchar(trimws(as.character(bundle$rhs_lm_mean)[1]))) {
    trimws(as.character(bundle$rhs_lm_mean)[1])
  } else if (length(mean_terms) > 0L) {
    paste(mean_terms, collapse = "+")
  } else {
    "1"
  }
  rhs_disp_src <- if (.mf_disp_rhs_is_intercept_only(bundle$rhs_lm_disp)) {
    "1"
  } else if (!is.null(bundle$rhs_lm_disp) && nzchar(trimws(as.character(bundle$rhs_lm_disp)[1]))) {
    trimws(as.character(bundle$rhs_lm_disp)[1])
  } else if (length(disp_terms) > 0L) {
    paste(disp_terms, collapse = "+")
  } else {
    "1"
  }

  rhs_mean <- .taguchi_rhs_after_excluding(rhs_mean_src, xvars, excluded)
  rhs_disp <- if (.mf_disp_rhs_is_intercept_only(rhs_disp_src)) {
    "1"
  } else {
    .taguchi_rhs_after_excluding(rhs_disp_src, xvars, excluded)
  }

  if (!".taguchi_disp" %in% names(dat)) {
    return(bundle)
  }

  mean_new <- tryCatch(
    .taguchi_fit_lm_contr_sum(
      paste(resp, "~", rhs_mean),
      dat,
      .taguchi_factors_from_rhs(rhs_mean, remaining)
    ),
    error = function(e) NULL
  )
  disp_new <- tryCatch(
    .taguchi_fit_lm_contr_sum(
      paste(disp_resp, "~", rhs_disp),
      dat,
      .taguchi_factors_from_rhs(rhs_disp, remaining)
    ),
    error = function(e) NULL
  )
  if (is.null(mean_new) || is.null(disp_new)) return(bundle)

  bundle$mean_model <- mean_new
  bundle$disp_model <- disp_new
  bundle$rhs_lm_mean <- rhs_mean
  bundle$rhs_lm_disp <- rhs_disp
  bundle$blocked_factors <- excluded
  bundle$optimizer_factor_names <- remaining
  bundle$interp_ctx <- NULL
  bundle$optimization_mode <- NULL
  if (!is.null(bundle$resolution_ctx) && length(remaining) >= 1L) {
    conf <- suppressWarnings(as.numeric(bundle$resolution_ctx$confidence))[1]
    if (!is.finite(conf) || conf <= 0 || conf >= 1) conf <- 0.95
    delta_user <- bundle$resolution_ctx$delta_user
    disp_type <- if (!is.null(bundle$disp_type)) as.character(bundle$disp_type)[1] else "ADM"
    bundle$resolution_ctx <- build_resolution_context(
      dat = dat,
      response_name = bundle$response_name,
      cell_factors = remaining,
      disp_type = disp_type,
      confidence = conf,
      delta_user = delta_user
    )
  }
  bundle
}

#' Apply blocked-factor exclusion to fit object and all response bundles.
#' @keywords internal
mf_apply_optimizer_blocked_factors <- function(fit_active, model_bundles, blocked_factors) {
  blocked <- unique(as.character(blocked_factors))
  blocked <- blocked[nzchar(blocked)]
  if (length(blocked) < 1L) {
    return(list(
      ok = TRUE,
      message = "",
      fit_active = fit_active,
      model_bundles = model_bundles,
      blocked_factors = character(0)
    ))
  }
  blocked <- intersect(blocked, fit_active$factors_names)
  if (length(blocked) < 1L) {
    return(list(
      ok = TRUE,
      message = "",
      fit_active = fit_active,
      model_bundles = model_bundles,
      blocked_factors = character(0)
    ))
  }
  fit_adj <- multifactor_taguchi_mvp_refit_excluding_factors(fit_active, blocked)
  if (!isTRUE(fit_adj$ok)) {
    return(list(
      ok = FALSE,
      message = fit_adj$message,
      fit_active = fit_active,
      model_bundles = model_bundles,
      blocked_factors = blocked
    ))
  }
  bundles_adj <- lapply(model_bundles, mf_refit_bundle_excluding_factors, excluded_factors = blocked)
  list(
    ok = TRUE,
    message = fit_adj$message,
    fit_active = fit_adj,
    model_bundles = bundles_adj,
    blocked_factors = blocked
  )
}

#' Factor names referenced in an lm RHS string.
#' @keywords internal
.taguchi_factors_from_rhs <- function(rhs, all_factors) {
  terms <- .taguchi_rhs_to_terms(rhs)
  if (length(terms) < 1L) return(character(0))
  unique(intersect(unlist(strsplit(terms, ":", fixed = TRUE)), as.character(all_factors)))
}

#' Apply sum-to-zero contrasts on model factors (matches Graphs / coefficient table).
#' @keywords internal
.taguchi_apply_contr_sum <- function(dat, model_factors) {
  model_factors <- unique(as.character(model_factors))
  model_factors <- model_factors[model_factors %in% names(dat)]
  if (length(model_factors) < 1L) return(dat)
  for (v in model_factors) {
    dat[[v]] <- factor(as.character(dat[[v]]))
    stats::contrasts(dat[[v]]) <- stats::contr.sum
  }
  dat
}

#' Fit lm with contr.sum on listed factors (parity with Results Graphs reduced model).
#' @keywords internal
.taguchi_fit_lm_contr_sum <- function(formula_str, dat, model_factors) {
  backup_opts <- options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(backup_opts), add = TRUE)
  dat <- .taguchi_apply_contr_sum(dat, model_factors)
  stats::lm(stats::as.formula(formula_str), data = dat, na.action = stats::na.omit)
}

#' EMM predictions on an observed factor grid (matches emmeans / Graphs tab).
#' @keywords internal
.taguchi_emmeans_on_grid <- function(mod, grid, factor_names = NULL) {
  if (is.null(mod) || is.null(grid) || nrow(grid) < 1L) return(numeric(0))
  if (is.null(factor_names)) {
    factor_names <- names(mod$xlevels)
  } else {
    factor_names <- intersect(as.character(factor_names), names(mod$xlevels))
  }
  if (length(factor_names) < 1L) {
    return(suppressWarnings(as.numeric(stats::predict(mod, newdata = grid))))
  }
  nd <- as.data.frame(grid[, factor_names, drop = FALSE], stringsAsFactors = FALSE)
  for (v in factor_names) {
    xl <- mod$xlevels[[v]]
    lv <- unique(c(xl, as.character(nd[[v]])))
    nd[[v]] <- factor(as.character(nd[[v]]), levels = lv)
    stats::contrasts(nd[[v]]) <- stats::contr.sum
  }
  # Optimizer evaluates one setting at a time; use predict only (no full-factorial emmeans).
  if (nrow(grid) == 1L) {
    pred <- suppressWarnings(as.numeric(stats::predict(mod, newdata = nd)))
    if (length(pred) >= 1L && is.finite(pred[[1L]])) {
      return(pred)
    }
    return(numeric(0))
  }
  if (requireNamespace("emmeans", quietly = TRUE)) {
    dfr <- tryCatch(suppressWarnings(as.numeric(mod$df.residual)), error = function(e) NA_real_)
    if (is.finite(dfr) && dfr >= 1) {
      specs <- stats::as.formula(paste("~", paste(factor_names, collapse = " * ")))
      emm <- tryCatch(
        suppressWarnings(emmeans::emmeans(mod, specs = specs)),
        error = function(e) NULL
      )
      if (!is.null(emm)) {
        emm_df <- as.data.frame(summary(emm))
        emm_col <- if ("emmean" %in% names(emm_df)) {
          "emmean"
        } else if ("prediction" %in% names(emm_df)) {
          "prediction"
        } else {
          NULL
        }
        if (!is.null(emm_col)) {
          for (v in factor_names) {
            nd[[v]] <- as.character(nd[[v]])
            emm_df[[v]] <- as.character(emm_df[[v]])
          }
          keys_grid <- apply(nd[, factor_names, drop = FALSE], 1, paste, collapse = "\t")
          keys_emm <- apply(emm_df[, factor_names, drop = FALSE], 1, paste, collapse = "\t")
          matched <- as.numeric(emm_df[[emm_col]][match(keys_grid, keys_emm)])
          if (all(is.finite(matched))) {
            return(matched)
          }
        }
      }
    }
  }
  suppressWarnings(as.numeric(stats::predict(mod, newdata = nd)))
}

#' One-row lm / EMM prediction.
#'
#' Optimizer inner loops call this thousands of times; prefer fast \code{predict()}
#' and reserve full \code{emmeans} for the multi-row loss grid path.
#' @keywords internal
.taguchi_predict_point <- function(mod, newdata) {
  if (is.null(mod) || is.null(newdata) || nrow(newdata) < 1L) return(NA_real_)
  nd <- newdata[1, , drop = FALSE]
  facs <- names(mod$xlevels)
  if (length(facs) >= 1L) {
    for (v in intersect(facs, names(nd))) {
      xl <- mod$xlevels[[v]]
      nd[[v]] <- factor(as.character(nd[[v]]), levels = xl)
      stats::contrasts(nd[[v]]) <- stats::contr.sum
    }
  }
  pred <- suppressWarnings(as.numeric(stats::predict(mod, newdata = nd)))
  if (length(pred) >= 1L && is.finite(pred[[1L]])) {
    return(pred[[1L]])
  }
  vals <- .taguchi_emmeans_on_grid(mod, nd)
  if (length(vals) < 1L) {
    return(NA_real_)
  }
  vals[[1L]]
}

#' @keywords internal
.taguchi_normalize_emm_summary_df <- function(df) {
  df <- as.data.frame(df)
  ip <- match("prediction", names(df), nomatch = 0L)
  if (ip > 0L && !("emmean" %in% names(df))) {
    names(df)[ip] <- "emmean"
  }
  df
}

#' Predict one model across an observed factor grid (EMM when possible).
#' @keywords internal
.taguchi_predict_on_grid <- function(mod, grid, factor_names) {
  if (is.null(mod) || is.null(grid) || nrow(grid) < 1L) {
    return(NULL)
  }
  facs <- intersect(as.character(factor_names), names(mod$xlevels))
  vals <- if (length(facs) < 1L) {
    suppressWarnings(as.numeric(stats::predict(mod, newdata = grid)))
  } else {
    tryCatch(.taguchi_emmeans_on_grid(mod, grid, facs), error = function(e) NULL)
  }
  need_rowwise <- is.null(vals) || length(vals) != nrow(grid) || sum(is.finite(vals)) < nrow(grid)
  if (need_rowwise && nrow(grid) > 1L) {
    vals <- vapply(seq_len(nrow(grid)), function(i) {
      .taguchi_predict_point(mod, grid[i, , drop = FALSE])
    }, numeric(1))
  }
  if (is.null(vals) || length(vals) < 1L) {
    return(NULL)
  }
  if (length(vals) == 1L && nrow(grid) > 1L) {
    vals <- vapply(seq_len(nrow(grid)), function(i) {
      .taguchi_predict_point(mod, grid[i, , drop = FALSE])
    }, numeric(1))
  }
  suppressWarnings(as.numeric(vals))
}

#' Build one observed factor grid and predict both models on it.
#' This is more robust than joining separate emmeans tables when reduced models differ.
#' @keywords internal
.taguchi_predict_join_on_observed <- function(mean_mod, disp_mod, dat, factor_names) {
  if (length(factor_names) < 1L || is.null(dat) || nrow(dat) < 1L) {
    return(list(table = NULL, n_grid = 0L, n_kept = 0L))
  }
  g <- unique(dat[, factor_names, drop = FALSE])
  g <- as.data.frame(g, stringsAsFactors = FALSE)
  if (nrow(g) < 1L) {
    return(list(table = NULL, n_grid = 0L, n_kept = 0L))
  }
  n_counts <- as.data.frame(table(dat[, factor_names, drop = FALSE]), stringsAsFactors = FALSE)
  names(n_counts)[names(n_counts) == "Freq"] <- "n_obs"

  for (v in factor_names) {
    lv_m <- mean_mod$xlevels[[v]]
    lv_d <- disp_mod$xlevels[[v]]
    lv <- unique(c(lv_m, lv_d, as.character(g[[v]])))
    if (length(lv) > 0L) {
      g[[v]] <- factor(as.character(g[[v]]), levels = lv)
    }
  }

  p_mean <- .taguchi_predict_on_grid(mean_mod, g, factor_names)
  p_disp <- .taguchi_predict_on_grid(disp_mod, g, factor_names)
  if (is.null(p_mean) || is.null(p_disp)) {
    return(list(table = NULL, n_grid = nrow(g), n_kept = 0L))
  }

  out <- g
  out$mu_pred <- suppressWarnings(as.numeric(p_mean))
  out$se_mu <- NA_real_
  out$disp_pred <- suppressWarnings(as.numeric(p_disp))
  ok <- is.finite(out$mu_pred) & is.finite(out$disp_pred)
  out <- out[ok, , drop = FALSE]
  for (v in factor_names) out[[v]] <- as.character(out[[v]])
  for (v in factor_names) n_counts[[v]] <- as.character(n_counts[[v]])
  out <- merge(out, n_counts, by = factor_names, all.x = TRUE, sort = FALSE)
  if (!"n_obs" %in% names(out)) out$n_obs <- NA_integer_
  list(table = out, n_grid = nrow(g), n_kept = nrow(out))
}

#' Fit reduced mean and dispersion models for multifactor Taguchi loss / continuous optimizer.
#'
#' Uses the same significance-based RHS as \code{multifactor_taguchi_loss_mvp}. Intended for reuse
#' so the continuous optimizer matches the Loss grid (EMM / \code{predict} semantics).
#'
#' @return \code{list(ok, message, mean_mod, disp_mod, dat, factors_names, response_name, disp_type)}
#' @keywords internal
multifactor_taguchi_mvp_fit_models <- function(
    dat,
    factors_id,
    data_id,
    conf,
    aov_out_l,
    aov_out_mean,
    ems_disp_type = 1L,
    ems_show_mixed_nest = FALSE,
    f_r_types = NULL,
    mean_model_override = NULL,
    fit_scope = c("both", "means_only", "dispersion_only"),
    pool_disp = character(0),
    available_effects = NULL) {
  fit_scope <- match.arg(fit_scope)
  if (is.null(dat) || nrow(dat) < 2L) {
    return(list(ok = FALSE, message = "Insufficient data."))
  }
  dat <- as.data.frame(dat)
  if (length(factors_id) < 2L) {
    return(list(ok = FALSE, message = "Select at least two factors for Multi-Factor loss."))
  }
  mean_model_ok <- is.data.frame(aov_out_mean) && nrow(aov_out_mean) >= 1L && !is.character(aov_out_mean)
  disp_tab_ok <- !is.character(aov_out_l) && is.data.frame(aov_out_l) && nrow(aov_out_l) >= 1L
  if (fit_scope == "means_only") {
    if (!mean_model_ok) {
      return(list(ok = FALSE, message = "Means ANOVA table is not ready yet (run Results ANOVA or check factor selections)."))
    }
  } else if (fit_scope == "dispersion_only") {
    if (!disp_tab_ok) {
      return(list(
        ok = FALSE,
        message = paste0(
          "Dispersion ANOVA is not available yet. ",
          "ADA, ADM, and ADM(n\u22121) require at least three observations per factorial cell."
        )
      ))
    }
  } else if (!mean_model_ok || !disp_tab_ok) {
    msg <- if (!mean_model_ok && !disp_tab_ok) {
      "Means and dispersion ANOVA tables are not ready yet (check design and factor selections)."
    } else if (!mean_model_ok) {
      "Means ANOVA table is not ready yet (run Results ANOVA or check factor selections)."
    } else {
      paste0(
        "Dispersion ANOVA is not available yet. ",
        "ADA, ADM, and ADM(n\u22121) require at least three observations per factorial cell."
      )
    }
    return(list(ok = FALSE, message = msg))
  }

  col_order <- c(data_id, factors_id[factors_id != data_id])
  dat <- dat[, col_order, drop = FALSE]
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  names(dat) <- make.names(names(dat))
  response_name <- names(dat)[1L]
  factors_names <- names(dat)[-1L]
  random_factors <- character(0)
  if (isTRUE(ems_show_mixed_nest) && length(f_r_types) == length(factors_id)) {
    random_factors <- factors_names[which(f_r_types == "R")]
    random_factors <- random_factors[!is.na(random_factors)]
  }

  dat[[response_name]] <- suppressWarnings(as.numeric(dat[[response_name]]))
  for (fn in factors_names) {
    dat[[fn]] <- factor(as.character(dat[[fn]]))
  }

  disp_type <- c("ADA", "ADM", "ADMn1")[as.integer(ems_disp_type)]
  if (is.na(disp_type)) disp_type <- "ADA"

  rhs_lm_mean <- NULL
  rhs_lm_disp <- NULL
  mean_mod <- NULL
  disp_mod <- NULL
  disp_intercept_only <- FALSE
  disp_cell_factors <- NULL
  fit_notes <- character(0)

  if (fit_scope != "dispersion_only") {
    rhs_lm_mean <- paste(factors_names, collapse = "*")
  }

  if (fit_scope != "dispersion_only") {
    rhs_parts_mean <- NULL
    if (mean_model_ok) {
      rhs_parts_mean <- .taguchi_sig_rhs_parts(aov_out_mean, conf, random_factors = random_factors)
      if (!is.null(rhs_parts_mean)) {
        rhs_parts_mean <- rhs_parts_mean[rhs_parts_mean != response_name]
      }
    }
    if (is.null(rhs_parts_mean) || length(rhs_parts_mean) == 0L) {
      rhs_lm_mean <- "1"
    } else {
      rhs_lm_mean <- paste(rhs_parts_mean, collapse = "+")
    }
  }

  disp_cell_factors <- character(0)
  if (fit_scope != "means_only") {
    rhs_parts_disp <- .taguchi_sig_rhs_parts(aov_out_l, conf, random_factors = random_factors)
    if (is.null(rhs_parts_disp)) {
      return(list(ok = FALSE, message = "Could not read ANOVA effects for dispersion reduced model."))
    }
    rhs_parts_disp <- rhs_parts_disp[rhs_parts_disp != response_name]
    if (length(rhs_parts_disp) == 0L) {
      rhs_lm_disp <- "1"
      disp_intercept_only <- TRUE
      fit_notes <- c(
        fit_notes,
        "No significant fixed dispersion effects at the current confidence; using intercept-only dispersion prediction."
      )
    } else {
      rhs_lm_disp <- paste(rhs_parts_disp, collapse = "+")
    }
    if (exists("mf_dispersion_model_grouping_formula", mode = "function")) {
      dg <- mf_dispersion_model_grouping_formula(
        response_name,
        factors_names,
        pool_disp,
        available_effects
      )
      disp_cell_factors <- as.character(dg$disp_factors)
    } else {
      disp_cell_factors <- .taguchi_disp_cell_factors(
        factors_names,
        pool_disp = pool_disp,
        available_effects = available_effects
      )
    }
    dat <- .taguchi_compute_disp_column(dat, response_name, disp_cell_factors, disp_type = disp_type)
  }

  dat <- .taguchi_apply_contr_sum(dat, unique(c(
    .taguchi_factors_from_rhs(rhs_lm_mean, factors_names),
    if (!is.null(rhs_lm_disp)) .taguchi_factors_from_rhs(rhs_lm_disp, factors_names) else character(0)
  )))

  if (fit_scope != "dispersion_only") {
    form_str <- paste(response_name, "~", rhs_lm_mean)
    mean_mod <- if (!is.null(mean_model_override)) {
      mean_model_override
    } else {
      tryCatch(
        .taguchi_fit_lm_contr_sum(form_str, dat, .taguchi_factors_from_rhs(rhs_lm_mean, factors_names)),
        error = function(e) NULL
      )
    }
  }

  if (fit_scope != "means_only") {
    disp_form_str <- paste(".taguchi_disp ~", rhs_lm_disp)
    disp_mod <- tryCatch(
      .taguchi_fit_lm_contr_sum(disp_form_str, dat, .taguchi_factors_from_rhs(rhs_lm_disp, factors_names)),
      error = function(e) NULL
    )
  }

  if (fit_scope == "means_only") {
    if (is.null(mean_mod)) {
      return(list(ok = FALSE, message = "Could not fit mean prediction model."))
    }
  } else if (fit_scope == "dispersion_only") {
    if (is.null(disp_mod)) {
      return(list(ok = FALSE, message = "Could not fit dispersion prediction model."))
    }
  } else if (is.null(mean_mod) || is.null(disp_mod)) {
    return(list(ok = FALSE, message = "Could not fit mean or dispersion prediction model."))
  }

  list(
    ok = TRUE,
    message = {
      if (length(random_factors) > 0L) {
        fit_notes <- c(
          fit_notes,
          paste0(
            "Random/block factors pooled out of prediction equations: ",
            paste(random_factors, collapse = ", "),
            "."
          )
        )
      }
      paste(trimws(fit_notes), collapse = " ")
    },
    mean_mod = mean_mod,
    disp_mod = disp_mod,
    dat = dat,
    factors_names = factors_names,
    response_name = response_name,
    disp_type = disp_type,
    rhs_lm_mean = rhs_lm_mean,
    rhs_lm_disp = rhs_lm_disp,
    disp_intercept_only = disp_intercept_only,
    disp_cell_factors = if (fit_scope != "means_only") disp_cell_factors else NULL,
    pool_disp = as.character(pool_disp),
    optimizer_factor_names = if (fit_scope != "dispersion_only") {
      .taguchi_factors_from_rhs(rhs_lm_mean, factors_names)
    } else {
      NULL
    },
    available_effects = if (fit_scope != "means_only" && !is.null(available_effects)) {
      as.character(available_effects)
    } else {
      NULL
    }
  )
}

#' Multifactor MVP loss grid (continuous response, normal surrogate).
#'
#' @param dat Full filtered data frame.
#' @param factors_id Integer column indices for factors.
#' @param data_id Integer column index for response.
#' @param conf Confidence level for significance.
#' @param aov_out_l ANOVA table on the dispersion scale (ADA/ADM/ADMn1); pooled or unpooled as supplied.
#' @param aov_out_mean ANOVA table on the original response (means). Required for multifactor Loss;
#'   the reduced mean model follows mean-significance from this table.
#' @param target Loss target used to scale side-specific coefficients from spec-to-target distance.
#' @param C_l,C_u Side-specific loss-at-spec costs (lower/upper).
#' @param lsl,usl Spec limits (at least one required).
#' @param ems_disp Deprecated; ignored. Loss runs whenever both \code{aov_out_mean} (or fallback) and
#'   \code{aov_out_l} dispersion tables are available.
#' @param ems_disp_type 1 = ADA, 2 = ADM, 3 = ADMn1.
#' @param ems_show_mixed_nest Mixed-model UI flag.
#' @param f_r_types Optional \code{"F"}/\code{"R"} per factor.
#' @param mean_model_override Optional mean model from Graphs path (preferred for parity).
#' @param fit_prebuilt Optional finalized registry fit (\code{list(ok, mean_mod, disp_mod, ...)}); when valid, skips ANOVA-table refit.
#' @return \code{list(ok, message, table, disclaimer)}
multifactor_taguchi_loss_mvp <- function(
    dat,
    factors_id,
    data_id,
    conf,
    aov_out_l,
    aov_out_mean = NULL,
    target,
    C_l = 1,
    C_u = 1,
    lsl = NA_real_,
    usl = NA_real_,
    ems_disp = TRUE,
    ems_disp_type = 1L,
    ems_show_mixed_nest = FALSE,
    f_r_types = NULL,
    mean_model_override = NULL,
    resolution_delta_user = NULL,
    blocked_factors = NULL,
    fit_prebuilt = NULL) {
  disclaimer <- taguchi_loss_disclaimer_plain("multifactor")

  if (is.null(dat) || nrow(dat) < 2L) {
    return(list(ok = FALSE, message = "Insufficient data.", table = NULL, disclaimer = disclaimer))
  }
  dat <- as.data.frame(dat)
  if (length(factors_id) < 2L) {
    return(list(ok = FALSE, message = "Select at least two factors for Multi-Factor loss.", table = NULL, disclaimer = disclaimer))
  }
  use_prebuilt <- !is.null(fit_prebuilt) && isTRUE(fit_prebuilt$ok) &&
    !is.null(fit_prebuilt$mean_mod) && !is.null(fit_prebuilt$disp_mod)
  fit <- if (isTRUE(use_prebuilt)) {
    fit_prebuilt
  } else {
    multifactor_taguchi_mvp_fit_models(
      dat = dat,
      factors_id = factors_id,
      data_id = data_id,
      conf = conf,
      aov_out_l = aov_out_l,
      aov_out_mean = aov_out_mean,
      ems_disp_type = ems_disp_type,
      ems_show_mixed_nest = ems_show_mixed_nest,
      f_r_types = f_r_types,
      mean_model_override = mean_model_override,
      fit_scope = "both"
    )
  }
  if (!isTRUE(fit$ok)) {
    return(list(ok = FALSE, message = fit$message, table = NULL, disclaimer = disclaimer))
  }
  blocked <- unique(as.character(blocked_factors))
  blocked <- blocked[nzchar(blocked)]
  if (length(blocked) > 0L) {
    fit <- multifactor_taguchi_mvp_refit_excluding_factors(fit, blocked)
    if (!isTRUE(fit$ok)) {
      return(list(ok = FALSE, message = fit$message, table = NULL, disclaimer = disclaimer))
    }
  }
  mean_mod <- fit$mean_mod
  disp_mod <- fit$disp_mod
  dat <- fit$dat
  factors_names <- fit$factors_names
  response_name <- fit$response_name
  disp_type <- fit$disp_type
  pool_disp_fit <- if (!is.null(fit$pool_disp)) as.character(fit$pool_disp) else character(0)
  disp_prep <- .mf_loss_grid_prepare_dispersion(fit, factors_names, blocked = blocked)
  if (!isTRUE(disp_prep$ok)) {
    return(list(ok = FALSE, message = disp_prep$message, table = NULL, disclaimer = disclaimer))
  }
  dat <- disp_prep$dat
  disp_mod <- disp_prep$disp_mod
  disp_cell_factors <- disp_prep$disp_cell_factors
  fit$dat <- dat
  fit$disp_mod <- disp_mod
  if (!(is.finite(lsl) || is.finite(usl))) {
    return(list(ok = FALSE, message = "Enter at least one spec limit (LSL and/or USL).", table = NULL, disclaimer = disclaimer))
  }
  if (is.finite(lsl) && is.finite(usl) && lsl >= usl) {
    return(list(ok = FALSE, message = "LSL must be less than USL when both are provided.", table = NULL, disclaimer = disclaimer))
  }
  if (!is.finite(target)) {
    return(list(ok = FALSE, message = "Enter a numeric Target (T).", table = NULL, disclaimer = disclaimer))
  }
  if (is.finite(lsl) && target <= lsl) {
    return(list(ok = FALSE, message = "Target (T) must be greater than LSL for lower-side loss scaling.", table = NULL, disclaimer = disclaimer))
  }
  if (is.finite(usl) && target >= usl) {
    return(list(ok = FALSE, message = "Target (T) must be less than USL for upper-side loss scaling.", table = NULL, disclaimer = disclaimer))
  }

  join_factors <- sort(unique(as.character(factors_names)))
  if (length(join_factors) < 1L) {
    join_factors <- character(0)
  }

  joined <- tryCatch(
    .taguchi_predict_join_on_observed(mean_mod, disp_mod, dat, join_factors),
    error = function(e) list(table = NULL, n_grid = 0L, n_kept = 0L)
  )
  j <- joined$table
  if (is.null(j) || nrow(j) == 0L) {
    return(list(ok = FALSE, message = "Could not align estimated means and dispersion predictions.", table = NULL, disclaimer = disclaimer))
  }
  fallback_note <- ""
  mu_sd <- suppressWarnings(stats::sd(j$mu_pred, na.rm = TRUE))
  mu_uniq_6 <- length(unique(round(as.numeric(j$mu_pred), 6)))
  if (nrow(j) > 1L && ((is.finite(mu_sd) && mu_sd < 1e-06) || mu_uniq_6 <= 1L)) {
    form_full <- stats::as.formula(paste(response_name, "~", paste(factors_names, collapse = "*")))
    mean_mod_full <- tryCatch(stats::lm(form_full, data = dat), error = function(e) NULL)
    if (!is.null(mean_mod_full)) {
      joined_full <- tryCatch(
        .taguchi_predict_join_on_observed(mean_mod_full, disp_mod, dat, factors_names),
        error = function(e) list(table = NULL, n_grid = 0L, n_kept = 0L)
      )
      if (!is.null(joined_full$table) && nrow(joined_full$table) > 0L) {
        j <- joined_full$table
        joined <- joined_full
        fallback_note <- "Reduced mean model was flat; used full factorial mean model for prediction."
      }
    }
  }

  resolution_ctx <- build_resolution_context(
    dat = dat,
    response_name = response_name,
    cell_factors = disp_cell_factors,
    disp_type = disp_type,
    confidence = conf,
    delta_user = resolution_delta_user
  )
  j <- apply_dispersion_resolution_to_grid(j, resolution_ctx)
  if (!"disp_effective" %in% names(j) || length(j$disp_effective) != nrow(j)) {
    j$disp_effective <- j$disp_pred
    if (!"disp_emm" %in% names(j)) j$disp_emm <- j$disp_pred
    if (!"disp_tier" %in% names(j)) j$disp_tier <- "EMM"
  }
  sigma <- vapply(j$disp_effective, dispersion_metric_to_sigma, numeric(1), type = disp_type)
  side_metrics <- taguchi_side_specific_metrics_normal(
    mu = j$mu_pred,
    sigma = sigma,
    target = target,
    lsl = lsl,
    usl = usl,
    C_l = C_l,
    C_u = C_u
  )
  out <- cbind(j, sigma = sigma, side_metrics)
  msg <- if (!is.null(fit$message)) as.character(fit$message) else ""
  if (is.finite(joined$n_grid) && is.finite(joined$n_kept) && joined$n_grid > joined$n_kept) {
    msg <- paste(
      trimws(msg),
      paste0("Computed on ", joined$n_kept, " of ", joined$n_grid, " observed setting(s); dropped non-estimable rows.")
    )
  }
  if (nzchar(fallback_note)) {
    msg <- paste(trimws(msg), fallback_note)
  }
  tier3_n <- sum(out$disp_tier == "Resolution delta", na.rm = TRUE)
  if (tier3_n > 0L) {
    msg <- paste(
      trimws(msg),
      paste0(tier3_n, " setting(s) use Policy 3 (gauge resolution prior); see dispersion policy note on the Loss tab.")
    )
  }
  list(ok = TRUE, message = msg, table = out, disclaimer = disclaimer, resolution_ctx = resolution_ctx)
}

#' Oneway MVP loss grid.
#'
#' @param ow_loss_disp_type 1 = ADA, 2 = ADM, 3 = ADMn1 (Loss tab).
#' @inheritParams multifactor_taguchi_loss_mvp
oneway_taguchi_loss_mvp <- function(
    dat,
    factor_ow,
    data_col,
    type_ow,
    disp_ow,
    ow_loss_disp_type,
    target,
    C_l = 1,
    C_u = 1,
    lsl = NA_real_,
    usl = NA_real_,
    ow_conf = 0.95,
    resolution_delta_user = NULL) {
  disclaimer <- taguchi_loss_disclaimer_plain("oneway")

  if (is.null(dat) || nrow(dat) < 2L) {
    return(list(ok = FALSE, message = "Insufficient data.", table = NULL, disclaimer = disclaimer))
  }
  dat <- as.data.frame(dat)
  if (isTRUE(type_ow == 3L)) {
    return(list(ok = FALSE, message = "Loss / optimization is not available for Kruskal-Wallis.", table = NULL, disclaimer = disclaimer))
  }
  if (isTRUE(type_ow == 2L)) {
    return(list(ok = FALSE, message = "Loss / optimization is not available for random-effects Oneway ANOVA.", table = NULL, disclaimer = disclaimer))
  }
  if (!isTRUE(disp_ow)) {
    return(list(
      ok = FALSE,
      message = "Turn on Show Dispersion Tests on the Oneway ANOVA tab.",
      table = NULL,
      disclaimer = disclaimer
    ))
  }
  if (!(is.finite(lsl) || is.finite(usl))) {
    return(list(ok = FALSE, message = "Enter at least one spec limit (LSL and/or USL).", table = NULL, disclaimer = disclaimer))
  }
  if (is.finite(lsl) && is.finite(usl) && lsl >= usl) {
    return(list(ok = FALSE, message = "LSL must be less than USL when both are provided.", table = NULL, disclaimer = disclaimer))
  }
  if (!is.finite(target)) {
    return(list(ok = FALSE, message = "Enter a numeric Target (T).", table = NULL, disclaimer = disclaimer))
  }
  if (is.finite(lsl) && target <= lsl) {
    return(list(ok = FALSE, message = "Target (T) must be greater than LSL for lower-side loss scaling.", table = NULL, disclaimer = disclaimer))
  }
  if (is.finite(usl) && target >= usl) {
    return(list(ok = FALSE, message = "Target (T) must be less than USL for upper-side loss scaling.", table = NULL, disclaimer = disclaimer))
  }

  dat <- dat[, c(data_col, factor_ow), drop = FALSE]
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  names(dat) <- make.names(names(dat))
  yname <- names(dat)[1L]
  fname <- names(dat)[2L]

  dat[[yname]] <- suppressWarnings(as.numeric(dat[[yname]]))
  dat[[fname]] <- factor(as.character(dat[[fname]]))

  form_c <- stats::as.formula(paste(yname, "~", fname))
  form_mean <- stats::as.formula(paste(yname, "~", fname))

  disp_type <- c("ADA", "ADM", "ADMn1")[as.integer(ow_loss_disp_type)]
  if (is.na(disp_type)) disp_type <- "ADA"

  if (disp_type == "ADA") {
    dat$.taguchi_disp <- lolcat::compute.group.dispersion.ADA(fx = form_c, data = dat)
  } else if (disp_type == "ADM") {
    dat$.taguchi_disp <- lolcat::compute.group.dispersion.ADM(fx = form_c, data = dat)
  } else {
    dat$.taguchi_disp <- lolcat::compute.group.dispersion.ADMn1(fx = form_c, data = dat)
  }

  mean_mod <- tryCatch(
    .taguchi_fit_lm_contr_sum(paste(yname, "~", fname), dat, fname),
    error = function(e) NULL
  )
  disp_mod <- tryCatch(
    .taguchi_fit_lm_contr_sum(paste(".taguchi_disp ~", fname), dat, fname),
    error = function(e) NULL
  )
  if (is.null(mean_mod) || is.null(disp_mod)) {
    return(list(ok = FALSE, message = "Could not fit mean or dispersion model.", table = NULL, disclaimer = disclaimer))
  }

  joined <- tryCatch(
    .taguchi_predict_join_on_observed(mean_mod, disp_mod, dat, fname),
    error = function(e) list(table = NULL, n_grid = 0L, n_kept = 0L)
  )
  j <- joined$table
  if (is.null(j) || nrow(j) == 0L) {
    return(list(ok = FALSE, message = "Could not build prediction grid.", table = NULL, disclaimer = disclaimer))
  }

  resolution_ctx <- build_resolution_context(
    dat = dat,
    response_name = yname,
    cell_factors = fname,
    disp_type = disp_type,
    confidence = ow_conf,
    delta_user = resolution_delta_user
  )
  j <- apply_dispersion_resolution_to_grid(j, resolution_ctx)
  sigma <- vapply(j$disp_effective, dispersion_metric_to_sigma, numeric(1), type = disp_type)
  side_metrics <- taguchi_side_specific_metrics_normal(
    mu = j$mu_pred,
    sigma = sigma,
    target = target,
    lsl = lsl,
    usl = usl,
    C_l = C_l,
    C_u = C_u
  )
  out <- cbind(j, sigma = sigma, side_metrics)
  msg <- ""
  if (is.finite(joined$n_grid) && is.finite(joined$n_kept) && joined$n_grid > joined$n_kept) {
    msg <- paste0("Computed on ", joined$n_kept, " of ", joined$n_grid, " observed setting(s); dropped non-estimable rows.")
  }
  tier3_n <- sum(out$disp_tier == "Resolution delta", na.rm = TRUE)
  if (tier3_n > 0L) {
    msg <- paste(
      trimws(msg),
      paste0(tier3_n, " setting(s) use Policy 3 (gauge resolution prior); see dispersion policy note on the Loss tab.")
    )
  }
  list(ok = TRUE, message = msg, table = out, disclaimer = disclaimer, resolution_ctx = resolution_ctx)
}
