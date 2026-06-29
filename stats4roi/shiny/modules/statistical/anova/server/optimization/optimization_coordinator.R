# Domain coordinator for multifactor Taguchi optimization (pure R).

#' Resolve dispersion ANOVA source for bundle fitting.
#' @keywords internal
mf_resolve_aov_disp <- function(has_disp_pool, disp_mode, pooled_aov, unpooled_aov) {
  if (isTRUE(has_disp_pool) && isTRUE(disp_mode)) pooled_aov else unpooled_aov
}

#' Resolve dispersion ANOVA when applied dispersion pooling exists (Slice 3 path).
#' @keywords internal
mf_resolve_aov_disp_for_optimizer <- function(has_disp_pool, disp_mode, pooled_aov, unpooled_aov) {
  if (isTRUE(has_disp_pool)) pooled_aov else unpooled_aov
}

#' @keywords internal
mf_aov_disp_table_ok <- function(x) {
  !is.character(x) && is.data.frame(x) && nrow(x) >= 1L
}

#' Pick the first usable dispersion ANOVA table (primary path, then pooled, then unpooled).
#' @keywords internal
mf_pick_valid_aov_disp <- function(has_disp_pool, disp_mode, pooled_aov, unpooled_aov) {
  primary <- mf_resolve_aov_disp_for_optimizer(has_disp_pool, disp_mode, pooled_aov, unpooled_aov)
  if (mf_aov_disp_table_ok(primary)) {
    return(primary)
  }
  if (!identical(primary, pooled_aov) && mf_aov_disp_table_ok(pooled_aov)) {
    return(pooled_aov)
  }
  if (!identical(primary, unpooled_aov) && mf_aov_disp_table_ok(unpooled_aov)) {
    return(unpooled_aov)
  }
  primary
}

#' Return committed registry fit for the Loss grid (no worker ANOVA refit on the Loss tab).
#' @keywords internal
mf_get_finalized_registry_fit <- function(
    did_i,
    d,
    fid,
    f_r_types,
    ems_show_mixed_nest,
    get_state_fn,
    registry_by_response,
    finalized_by_response) {
  key <- as.character(as.integer(did_i))
  st <- get_state_fn(did_i)
  conf_i <- suppressWarnings(as.numeric(if (!is.null(st$ems_conf)) st$ems_conf else 0.95))
  if (!is.finite(conf_i)) conf_i <- 0.95
  ems_disp_type_i <- suppressWarnings(as.integer(st$ems_disp_type)[1])
  if (length(ems_disp_type_i) < 1L || is.na(ems_disp_type_i)) ems_disp_type_i <- 1L
  pool_mean <- if (is.null(st$ems_pool_means)) character(0) else as.character(st$ems_pool_means)
  pool_disp <- if (is.null(st$ems_pool_disp)) character(0) else as.character(st$ems_pool_disp)
  nr <- if (is.data.frame(d)) nrow(d) else NA_integer_
  nc <- if (is.data.frame(d)) ncol(d) else NA_integer_
  sig_mean <- mf_fit_signature(
    did = did_i, fid = fid, conf = conf_i, ems_disp_type = ems_disp_type_i,
    ems_show_mixed_nest = isTRUE(ems_show_mixed_nest), f_r_types = f_r_types,
    pool = pool_mean, ems_disp = FALSE, nrow_d = nr, ncol_d = nc
  )
  sig_disp <- mf_fit_signature(
    did = did_i, fid = fid, conf = conf_i, ems_disp_type = ems_disp_type_i,
    ems_show_mixed_nest = isTRUE(ems_show_mixed_nest), f_r_types = f_r_types,
    pool = pool_disp, ems_disp = TRUE, nrow_d = nr, ncol_d = nc
  )
  cached <- registry_by_response[[key]]
  finalized <- finalized_by_response[[key]]
  if (is.null(finalized)) finalized <- list()
  if (is.null(cached) || is.null(cached$fit)) {
    return(list(
      ok = FALSE,
      message = "Committed model is missing from the registry; re-commit means and dispersion on the Results tab."
    ))
  }
  mean_ok <- !is.null(finalized$means_signature) && !is.null(cached$signature_mean) &&
    identical(finalized$means_signature, cached$signature_mean)
  disp_ok <- !is.null(finalized$dispersion_signature) && !is.null(cached$signature_disp) &&
    identical(finalized$dispersion_signature, cached$signature_disp)
  if (!isTRUE(mean_ok) || !isTRUE(disp_ok)) {
    miss <- character(0)
    if (!isTRUE(mean_ok)) miss <- c(miss, "means model not finalized")
    if (!isTRUE(disp_ok)) miss <- c(miss, "dispersion model not finalized")
    return(list(
      ok = FALSE,
      message = paste0(
        "Means and dispersion models must be finalized on the Results tab before computing loss (",
        paste(miss, collapse = "; "),
        ")."
      )
    ))
  }
  fit <- cached$fit
  if (is.null(fit$mean_mod) || is.null(fit$disp_mod)) {
    return(list(
      ok = FALSE,
      message = "Committed model is incomplete; re-commit means and dispersion on the Results tab."
    ))
  }
  if (!isTRUE(fit$ok)) {
    fit$ok <- TRUE
  }
  fit
}

#' Cell factors for dispersion resolution (matches loss-grid path).
#' @keywords internal
.mf_resolution_cell_factors <- function(fit) {
  if (is.null(fit) || !is.list(fit)) return(character(0))
  fnames <- as.character(fit$factors_names)
  fnames <- fnames[nzchar(fnames)]
  if (!is.null(fit$disp_cell_factors) && length(fit$disp_cell_factors) > 0L) {
    cf <- as.character(fit$disp_cell_factors)
    cf <- cf[cf %in% fnames]
    if (length(cf) > 0L) return(cf)
  }
  fnames
}

#' Build model bundles for all selected responses.
#'
#' @param economics_by_response Named list of economics per response name or id key.
#' @keywords internal
mf_build_model_bundles <- function(
    d,
    fid,
    did_all,
    active_did,
    f_r_types,
    ems_show_mixed_nest,
    readiness,
    get_state_fn,
    get_economics_fn,
    registry_by_response,
    finalized_by_response,
    build_fit_fn,
    aov_mean_fn,
    ems_pooled_means_fn = NULL,
    aov_disp_active_fn,
    aov_disp_cached_fn,
    get_resolution_delta_fn = NULL) {
  not_ready <- list()
  model_bundles <- list()
  fit_active <- NULL

  for (did_i in did_all) {
    key_i <- as.character(as.integer(did_i))
    rr <- readiness[[key_i]]
    if (is.null(rr) || !isTRUE(rr$mean_ready) || !isTRUE(rr$disp_ready)) {
      label <- if (!is.null(rr$response)) rr$response else if (did_i >= 1L && did_i <= ncol(d)) names(d)[did_i] else as.character(did_i)
      miss <- if (!is.null(rr$missing) && length(rr$missing) > 0) paste(rr$missing, collapse = ", ") else "model not ready"
      not_ready[[length(not_ready) + 1L]] <- paste0(label, " (", miss, ")")
    }
  }
  if (length(not_ready) > 0L) {
    return(list(
      ok = FALSE,
      message = paste0(
        "Optimization blocked. Complete model finalization for: ",
        paste(not_ready, collapse = "; "),
        "."
      ),
      preflight = readiness,
      model_bundles = list(),
      fit_active = NULL,
      not_ready = not_ready
    ))
  }

  for (did_i in did_all) {
    st_i <- get_state_fn(did_i)
    conf_i <- if (!is.null(st_i$ems_conf)) st_i$ems_conf else 0.95
    did_name <- if (did_i >= 1L && did_i <= ncol(d)) names(d)[did_i] else as.character(did_i)
    economics_i <- get_economics_fn(did_i, did_name)
    fit_i <- mf_get_finalized_registry_fit(
      did_i = did_i,
      d = d,
      fid = fid,
      f_r_types = f_r_types,
      ems_show_mixed_nest = ems_show_mixed_nest,
      get_state_fn = get_state_fn,
      registry_by_response = registry_by_response,
      finalized_by_response = finalized_by_response
    )
    if (!isTRUE(fit_i$ok)) {
      not_ready[[length(not_ready) + 1L]] <- paste0(did_name, " (", fit_i$message, ")")
      next
    }
    fit_i <- mf_refresh_fit_dispersion_for_optimizer(fit_i, blocked = character(0))
    delta_user_i <- if (is.function(get_resolution_delta_fn)) get_resolution_delta_fn(did_i, did_name) else NULL
    cell_facs <- .mf_resolution_cell_factors(fit_i)
    ctx_i <- build_resolution_context(
      dat = fit_i$dat,
      response_name = fit_i$response_name,
      cell_factors = cell_facs,
      disp_type = fit_i$disp_type,
      confidence = conf_i,
      delta_user = delta_user_i
    )
    model_bundles[[length(model_bundles) + 1L]] <- opt_build_model_bundle(fit_i, did_name, economics_i, resolution_ctx = ctx_i)
    if (as.integer(did_i) == as.integer(active_did)) {
      fit_active <- fit_i
    }
  }

  if (length(not_ready) > 0L) {
    return(list(
      ok = FALSE,
      message = paste0(
        "Optimization blocked. Required model(s) not ready for: ",
        paste(not_ready, collapse = "; "),
        ". Build/adjust ANOVA models for each selected response first."
      ),
      model_bundles = list(),
      fit_active = NULL,
      not_ready = not_ready
    ))
  }

  if (is.null(fit_active) || !isTRUE(fit_active$ok)) {
    return(list(
      ok = FALSE,
      message = "Active response model is not ready for optimization.",
      model_bundles = list(),
      fit_active = NULL,
      not_ready = not_ready
    ))
  }

  list(
    ok = TRUE,
    message = "",
    model_bundles = model_bundles,
    fit_active = fit_active,
    not_ready = not_ready
  )
}

#' Evaluate scalar objective at one setting row.
#' @keywords internal
mf_eval_objective_at <- function(
    nd,
    model_bundles,
    mean_mod,
    disp_mod,
    disp_type,
    economics_single,
    optimize_target,
    volume,
    unit_setting_cost_fn) {
  if (length(model_bundles) > 1L) {
    agg <- aggregate_multiresponse_loss(model_bundles = model_bundles, newdata = nd)
    usc <- suppressWarnings(as.numeric(unit_setting_cost_fn(nd)))
    if (!is.finite(usc)) usc <- 0
    if (identical(optimize_target, "total_cost")) {
      volume * (agg$value + usc)
    } else {
      agg$value
    }
  } else if (length(model_bundles) == 1L && !is.null(model_bundles[[1]]$interp_ctx) &&
      length(model_bundles[[1]]$interp_ctx$cont_vars) > 0L) {
    pred <- .bundle_predict_metrics(model_bundles[[1]], newdata = nd)
    m <- pred$metrics
    usc <- suppressWarnings(as.numeric(unit_setting_cost_fn(nd)))
    if (!is.finite(usc)) usc <- 0
    if (identical(optimize_target, "total_cost")) volume * (m$expected_loss[1] + usc) else m$expected_loss[1]
  } else {
    nd_eval <- nd
    if (length(model_bundles) == 1L) {
      if (exists("mf_complete_bundle_newdata", mode = "function")) {
        nd_eval <- mf_complete_bundle_newdata(model_bundles[[1]], nd_eval)
      }
      nd_eval <- mf_prepare_optimizer_newdata(model_bundles[[1]], nd_eval)
    }
    mu <- .taguchi_predict_point(mean_mod, nd_eval)
    dp <- .taguchi_predict_point(disp_mod, nd_eval)
    ctx <- if (length(model_bundles) == 1L) model_bundles[[1]]$resolution_ctx else NULL
    if (!is.null(ctx)) {
      sg <- resolve_dispersion_for_newdata(dp, nd_eval, ctx)$sigma[1]
    } else {
      sg <- dispersion_metric_to_sigma(dp, type = disp_type)[1]
    }
    m <- taguchi_side_specific_metrics_normal(
      mu = mu, sigma = sg,
      target = economics_single$target,
      lsl = economics_single$lsl, usl = economics_single$usl,
      C_l = economics_single$C_l, C_u = economics_single$C_u
    )
    usc <- suppressWarnings(as.numeric(unit_setting_cost_fn(nd)))
    if (!is.finite(usc)) usc <- 0
    if (identical(optimize_target, "total_cost")) volume * (m$expected_loss[1] + usc) else m$expected_loss[1]
  }
}

#' Build four-point confirmation package (domain).
#' @keywords internal
build_confirmation_package <- function(
    res_best,
    worst_res,
    observed_rows,
    eval_obj_at_fn,
    xnames) {
  if (!isTRUE(res_best$ok)) return(NULL)
  observed_cost <- rep(NA_real_, nrow(observed_rows))
  for (i in seq_len(nrow(observed_rows))) {
    observed_cost[i] <- eval_obj_at_fn(observed_rows[i, , drop = FALSE])
  }
  obs_best_idx <- if (length(observed_cost) > 0) which.min(observed_cost) else integer(0)
  obs_worst_idx <- if (length(observed_cost) > 0) which.max(observed_cost) else integer(0)

  make_row <- function(label, setting_row, cost_val) {
    if (is.null(setting_row) || nrow(setting_row) < 1L) return(NULL)
    vals <- as.list(setting_row[1, , drop = FALSE])
    vals$Scenario <- label
    vals$EstimatedCost <- suppressWarnings(as.numeric(cost_val))
    as.data.frame(vals, stringsAsFactors = FALSE)
  }
  rows <- list()
  if (length(obs_best_idx) > 0) {
    rows[[length(rows) + 1L]] <- make_row(
      "Observed Best",
      observed_rows[obs_best_idx[1], , drop = FALSE],
      observed_cost[obs_best_idx[1]]
    )
  }
  if (length(obs_worst_idx) > 0) {
    rows[[length(rows) + 1L]] <- make_row(
      "Observed Worst",
      observed_rows[obs_worst_idx[1], , drop = FALSE],
      observed_cost[obs_worst_idx[1]]
    )
  }
  if (!is.null(res_best$par_snapped) && nrow(res_best$par_snapped) == 1L) {
    rows[[length(rows) + 1L]] <- make_row(
      "Theoretical Best",
      res_best$par_snapped[, xnames, drop = FALSE],
      res_best$value
    )
  } else {
    best_row <- mf_setting_row_from_opt_result(res_best, xnames)
    if (!is.null(best_row)) {
      rows[[length(rows) + 1L]] <- make_row("Theoretical Best", best_row, res_best$value)
    }
  }
  if (isTRUE(worst_res$ok) && !is.null(worst_res$par_snapped) && nrow(worst_res$par_snapped) == 1L) {
    rows[[length(rows) + 1L]] <- make_row(
      "Theoretical Worst",
      worst_res$par_snapped[, xnames, drop = FALSE],
      worst_res$value
    )
  } else {
    worst_row <- mf_setting_row_from_opt_result(worst_res, xnames)
    if (!is.null(worst_row)) {
      rows[[length(rows) + 1L]] <- make_row("Theoretical Worst", worst_row, worst_res$value)
    }
  }
  if (length(rows) < 1L) return(NULL)
  conf_tbl <- do.call(rbind, rows)
  conf_tbl[, c("Scenario", xnames, "EstimatedCost"), drop = FALSE]
}

#' Add per-response Taguchi loss columns to confirmation rows (multi-response).
#' @keywords internal
enrich_confirmation_multiresponse <- function(conf_tbl, model_bundles, xnames) {
  if (is.null(conf_tbl) || nrow(conf_tbl) < 1L || length(model_bundles) < 2L) {
    return(conf_tbl)
  }
  loss_cols <- vapply(model_bundles, function(b) {
    rn <- if (!is.null(b$response_name)) as.character(b$response_name) else "response"
    paste0("Loss_", make.names(rn))
  }, character(1))
  for (col in loss_cols) conf_tbl[[col]] <- NA_real_
  for (i in seq_len(nrow(conf_tbl))) {
    nd <- conf_tbl[i, xnames, drop = FALSE]
    agg <- aggregate_multiresponse_loss(model_bundles = model_bundles, newdata = nd)
    if (!is.null(agg$details)) {
      for (j in seq_along(agg$details)) {
        di <- agg$details[[j]]
        rn <- if (!is.null(di$response)) as.character(di$response) else paste0("response_", j)
        col <- paste0("Loss_", make.names(rn))
        if (col %in% names(conf_tbl)) {
          conf_tbl[[col]][i] <- suppressWarnings(as.numeric(di$expected_loss))
        }
      }
    }
  }
  conf_tbl
}

#' Run multifactor optimizer (min + max for confirmation).
#' @keywords internal
mf_run_optimization <- function(
    model_bundles,
    fit_active,
    economics_single,
    cont_vars,
    optimize_target,
    volume,
    factor_cost_txt,
    use_factor_costs,
    xnames,
    blocked_factors = character(0)) {
  blocked_factors <- unique(as.character(blocked_factors))
  blocked_factors <- blocked_factors[nzchar(blocked_factors)]
  blocked_apply <- mf_apply_optimizer_blocked_factors(fit_active, model_bundles, blocked_factors)
  if (!isTRUE(blocked_apply$ok)) {
    return(list(ok = FALSE, message = blocked_apply$message))
  }
  fit_active <- blocked_apply$fit_active
  model_bundles <- blocked_apply$model_bundles
  blocked_factors <- blocked_apply$blocked_factors
  fit_active <- mf_refresh_fit_dispersion_for_optimizer(fit_active, blocked = blocked_factors)
  model_bundles <- lapply(
    model_bundles,
    mf_refresh_bundle_dispersion_for_optimizer,
    blocked = blocked_factors
  )
  xnames <- mf_optimizer_search_factors_union(fit_active, model_bundles)
  if (length(xnames) < 1L) {
    return(list(
      ok = FALSE,
      message = paste(
        "Cannot optimize factor settings: the committed mean model has no adjustable factors",
        "(intercept-only or outdated full-factorial commit).",
        "Re-commit the means model on the Results tab after applying pooling, then run the optimizer again."
      )
    ))
  }

  dat <- fit_active$dat
  mean_mod <- fit_active$mean_mod
  disp_mod <- fit_active$disp_mod
  disp_type <- fit_active$disp_type

  bounds <- mf_factor_bounds_from_dat(dat, xnames)
  cont_vars <- unique(as.character(cont_vars))
  cont_vars <- cont_vars[nzchar(cont_vars)]
  if (length(blocked_factors) > 0L) {
    cont_vars <- setdiff(cont_vars, blocked_factors)
  }
  if (length(cont_vars) > 0L) {
    fid_idx <- match(xnames, names(dat))
    if (any(is.na(fid_idx))) fid_idx <- match(xnames, make.names(names(dat)))
    cont_vars <- mf_align_continuous_vars(cont_vars, xnames, dat, fid_idx)
  }
  disc_vars <- setdiff(xnames, cont_vars)
  if (length(cont_vars) > 0L) {
    unknown <- setdiff(cont_vars, xnames)
    if (length(unknown) > 0L) {
      return(list(
        ok = FALSE,
        message = paste0(
          "Unknown continuous factor(s): ", paste(unknown, collapse = ", "),
          ". Select factors from the dropdown list."
        )
      ))
    }
  }
  factor_kinds <- mf_factor_kinds_from_cont_vars(xnames, cont_vars)
  optimization_mode <- if (length(cont_vars) > 0L) "interp" else "discrete"
  interp_ctx <- NULL

  if (length(cont_vars) > 0L) {
    bridged <- lapply(model_bundles, function(bi) coerce_bundle_continuous_models(bi, cont_vars))
    bridge_ok <- all(vapply(bridged, function(x) isTRUE(x$ok), logical(1)))
    if (bridge_ok) {
      model_bundles <- lapply(bridged, function(x) {
        b <- x$bundle
        b$optimization_mode <- "ancova"
        b$interp_ctx <- NULL
        b$cont_vars <- cont_vars
        b
      })
      mean_mod <- model_bundles[[1]]$mean_model
      disp_mod <- model_bundles[[1]]$disp_model
      dat <- model_bundles[[1]]$dat
      optimization_mode <- "ancova"
    } else {
      interp_ctx <- build_factor_model_interp_ctx(mean_mod, disp_mod, dat, cont_vars, disc_vars)
      if (!is.null(interp_ctx) && length(model_bundles) >= 1L) {
        interp_ctx$resolution_ctx <- model_bundles[[1]]$resolution_ctx
      }
      if (is.null(interp_ctx)) {
        msgs <- vapply(which(!vapply(bridged, function(x) isTRUE(x$ok), logical(1))), function(i) {
          rn <- if (!is.null(model_bundles[[i]]$response_name)) model_bundles[[i]]$response_name else paste0("response_", i)
          paste0(rn, " (", bridged[[i]]$message, ")")
        }, character(1))
        return(list(
          ok = FALSE,
          message = paste0(
            "Could not optimize continuous factors (ANCOVA bridge or interpolation failed): ",
            paste(msgs, collapse = "; ")
          )
        ))
      }
      model_bundles <- lapply(model_bundles, function(bi) {
        bi$interp_ctx <- build_factor_model_interp_ctx(
          bi$mean_model, bi$disp_model, bi$dat, cont_vars, disc_vars
        )
        if (!is.null(bi$interp_ctx)) {
          bi$interp_ctx$resolution_ctx <- bi$resolution_ctx
        }
        bi$optimization_mode <- "interp"
        bi$cont_vars <- cont_vars
        bi
      })
      optimization_mode <- "interp"
    }
  }

  if (!is.finite(volume) || volume <= 0) {
    return(list(ok = FALSE, message = "Production volume must be a positive number."))
  }

  factor_cost_tbl <- if (isTRUE(use_factor_costs)) {
    parse_factor_level_costs(factor_cost_txt, valid_factors = xnames)
  } else {
    data.frame(factor = character(0), level = numeric(0), cost = numeric(0), stringsAsFactors = FALSE)
  }
  unit_setting_cost_fn <- make_unit_setting_cost_fn(factor_cost_tbl, factor_kinds = factor_kinds)

  res <- tryCatch({
    if (length(model_bundles) > 1L) {
      optimize_multiresponse_continuous(
        model_bundles = model_bundles,
        bounds = bounds,
        factor_kinds = factor_kinds,
        optimize_target = optimize_target,
        volume = volume,
        unit_setting_cost_fn = unit_setting_cost_fn
      )
    } else {
      optimize_loss_continuous(
        mean_model = mean_mod,
        disp_model = disp_mod,
        bounds = bounds,
        target = economics_single$target,
        lsl = economics_single$lsl,
        usl = economics_single$usl,
        C_l = economics_single$C_l,
        C_u = economics_single$C_u,
        disp_type = disp_type,
        factor_kinds = factor_kinds,
        optimize_target = optimize_target,
        volume = volume,
        unit_setting_cost_fn = unit_setting_cost_fn,
        interp_ctx = if (identical(optimization_mode, "ancova")) NULL else interp_ctx,
        resolution_ctx = if (length(model_bundles) == 1L) model_bundles[[1]]$resolution_ctx else NULL,
        dat = dat
      )
    }
  }, error = function(e) {
    list(ok = FALSE, message = conditionMessage(e))
  })

  res$optimization_mode <- optimization_mode

  res$bounds_used <- data.frame(
    Variable = names(bounds),
    Lower = vapply(bounds, function(b) as.numeric(b$lower), numeric(1)),
    Upper = vapply(bounds, function(b) as.numeric(b$upper), numeric(1))
  )

  if (!isTRUE(res$ok)) return(res)

  xnames_dat <- intersect(as.character(xnames), names(dat))
  if (length(xnames_dat) < 1L) {
    return(list(
      ok = FALSE,
      message = "Optimizer factor names are not present in the training data."
    ))
  }
  observed_rows <- dat[, xnames_dat, drop = FALSE]
  observed_rows <- observed_rows[stats::complete.cases(observed_rows), , drop = FALSE]
  if (nrow(observed_rows) > 0) observed_rows <- unique(observed_rows)

  eval_obj_at <- function(nd) {
    mf_eval_objective_at(
      nd = nd,
      model_bundles = model_bundles,
      mean_mod = mean_mod,
      disp_mod = disp_mod,
      disp_type = disp_type,
      economics_single = economics_single,
      optimize_target = optimize_target,
      volume = volume,
      unit_setting_cost_fn = unit_setting_cost_fn
    )
  }

  worst_res <- tryCatch({
    if (length(model_bundles) > 1L) {
      optimize_multiresponse_continuous(
        model_bundles = model_bundles,
        bounds = bounds,
        factor_kinds = factor_kinds,
        optimize_target = optimize_target,
        objective_direction = "max",
        volume = volume,
        unit_setting_cost_fn = unit_setting_cost_fn
      )
    } else {
      optimize_loss_continuous(
        mean_model = mean_mod, disp_model = disp_mod, bounds = bounds,
        target = economics_single$target,
        lsl = economics_single$lsl, usl = economics_single$usl,
        C_l = economics_single$C_l, C_u = economics_single$C_u,
        disp_type = disp_type,
        factor_kinds = factor_kinds,
        optimize_target = optimize_target,
        objective_direction = "max",
        volume = volume,
        unit_setting_cost_fn = unit_setting_cost_fn,
        interp_ctx = if (identical(optimization_mode, "ancova")) NULL else interp_ctx,
        resolution_ctx = if (length(model_bundles) == 1L) model_bundles[[1]]$resolution_ctx else NULL,
        dat = dat
      )
    }
  }, error = function(e) list(ok = FALSE))

  disc_levels <- mf_disc_levels_from_bounds(mean_mod, disc_vars, bounds)
  if (!isTRUE(worst_res$ok) || mf_opt_same_settings(res, worst_res, xnames)) {
    worst_res <- mf_extreme_boundary_search(
      eval_fn = eval_obj_at,
      bounds = bounds,
      factor_kinds = factor_kinds,
      disc_levels = disc_levels,
      direction = "max"
    )
  }

  conf_tbl <- build_confirmation_package(
    res_best = res,
    worst_res = worst_res,
    observed_rows = observed_rows,
    eval_obj_at_fn = eval_obj_at,
    xnames = xnames_dat
  )
  conf_tbl <- enrich_confirmation_multiresponse(conf_tbl, model_bundles, xnames_dat)

  list(
    ok = TRUE,
    result = res,
    confirmation = conf_tbl,
    model_bundles = model_bundles,
    unit_setting_cost_fn = unit_setting_cost_fn,
    blocked_factors = blocked_factors
  )
}

#' Unified optimization readiness (model + economics + pending pooling).
#' @keywords internal
optimization_readiness <- function(
    d,
    fid,
    dids,
    registry_by_response,
    finalized_by_response,
    ems_show_mixed_nest,
    f_r_types,
    get_state_fn,
    session = NULL,
    pooling_pending_fn = NULL,
    require_economics = FALSE) {
  model_ready <- mf_model_readiness_compute(
    d = d,
    fid = fid,
    dids = dids,
    registry_by_response = registry_by_response,
    finalized_by_response = finalized_by_response,
    ems_show_mixed_nest = ems_show_mixed_nest,
    f_r_types = f_r_types,
    get_state_fn = get_state_fn
  )
  misses <- character(0)
  for (k in names(model_ready)) {
    rr <- model_ready[[k]]
    did_i <- suppressWarnings(as.integer(k))
    resp_name <- if (!is.null(rr$response)) rr$response else k
    if (!isTRUE(rr$mean_ready) || !isTRUE(rr$disp_ready)) {
      misses <- c(misses, paste0(resp_name, ": ", paste(rr$missing, collapse = ", ")))
    }
    if (!is.null(pooling_pending_fn)) {
      if (isTRUE(pooling_pending_fn(did_i, FALSE))) {
        misses <- c(misses, paste0(resp_name, ": Means pooling has pending changes (Apply or Reset before optimization)"))
      }
      if (isTRUE(pooling_pending_fn(did_i, TRUE))) {
        misses <- c(misses, paste0(resp_name, ": Dispersion pooling has pending changes (Apply or Reset before optimization)"))
      }
    }
    econ <- opt_session_get_economics(session, k)
    if (isTRUE(require_economics) && !isTRUE(opt_economics_complete(econ))) {
      misses <- c(misses, paste0(resp_name, ": Taguchi economics incomplete (enter target, spec limit, and C_l/C_u on Loss tab)"))
    }
  }
  list(
    per_response = model_ready,
    all_ready = length(misses) < 1L,
    blockers = unique(misses)
  )
}

#' HTML checklist for Results / Loss preflight.
#' @keywords internal
optimization_checklist_ui <- function(readiness_result) {
  if (isTRUE(readiness_result$all_ready)) {
    return(shiny::tags$p(class = "text-success", "Checklist: all selected responses are ready for optimization."))
  }
  shiny::tags$div(
    class = "text-warning",
    shiny::tags$strong("Optimization checklist — complete these steps:"),
    shiny::tags$ul(lapply(readiness_result$blockers, shiny::tags$li))
  )
}

#' Build loss grid for one response using finalized registry fits only.
#' @keywords internal
mf_build_loss_grid_for_response <- function(
    d,
    fid,
    did_i,
    resp_name,
    economics,
    registry_by_response,
    finalized_by_response,
    get_state_fn,
    f_r_types,
    ems_show_mixed_nest,
    ems_disp_type_default,
    conf_default,
    resolution_delta_user = NULL,
    blocked_factors = character(0)) {
  st <- get_state_fn(did_i)
  conf_i <- if (!is.null(st$ems_conf)) st$ems_conf else conf_default
  ems_disp_type_i <- if (!is.null(st$ems_disp_type)) as.integer(st$ems_disp_type)[1] else ems_disp_type_default
  fit_i <- mf_get_finalized_registry_fit(
    did_i = did_i,
    d = d,
    fid = fid,
    f_r_types = f_r_types,
    ems_show_mixed_nest = ems_show_mixed_nest,
    get_state_fn = get_state_fn,
    registry_by_response = registry_by_response,
    finalized_by_response = finalized_by_response
  )
  fit_msg_prefix <- if (nzchar(resp_name)) paste0(resp_name, ": ") else ""
  tryCatch(
    {
      out <- multifactor_taguchi_loss_mvp(
        dat = d,
        factors_id = fid,
        data_id = did_i,
        conf = conf_i,
        aov_out_l = NULL,
        aov_out_mean = NULL,
        target = economics$target,
        C_l = economics$C_l,
        C_u = economics$C_u,
        lsl = economics$lsl,
        usl = economics$usl,
        ems_disp = TRUE,
        ems_disp_type = ems_disp_type_i,
        ems_show_mixed_nest = ems_show_mixed_nest,
        f_r_types = f_r_types,
        mean_model_override = NULL,
        resolution_delta_user = resolution_delta_user,
        blocked_factors = blocked_factors,
        fit_prebuilt = if (isTRUE(fit_i$ok)) fit_i else NULL
      )
      if (!isTRUE(out$ok) && nzchar(fit_msg_prefix) && !grepl(paste0("^", resp_name, ":"), as.character(out$message)[1])) {
        out$message <- paste0(fit_msg_prefix, as.character(out$message)[1])
      }
      if (!isTRUE(out$ok) && !isTRUE(fit_i$ok) && nzchar(as.character(fit_i$message)[1])) {
        out$message <- paste0(fit_msg_prefix, as.character(fit_i$message)[1])
      }
      out
    },
    error = function(e) {
      list(ok = FALSE, message = paste0(fit_msg_prefix, conditionMessage(e)), table = NULL, disclaimer = "")
    }
  )
}

#' Build loss grids for all selected responses (registry-only path).
#' @keywords internal
mf_build_loss_grid_all_responses <- function(
    d,
    fid,
    dids,
    session,
    registry_by_response,
    finalized_by_response,
    get_state_fn,
    f_r_types,
    ems_show_mixed_nest,
    ems_disp_type_default,
    conf_default,
    get_resolution_delta_fn,
    blocked_factors = character(0)) {
  resp_names <- vapply(dids, function(di) {
    if (di >= 1L && di <= ncol(d)) names(d)[di] else as.character(di)
  }, character(1))
  out <- stats::setNames(vector("list", length(dids)), resp_names)
  for (i in seq_along(dids)) {
    did_i <- dids[[i]]
    rn <- resp_names[[i]]
    econ <- opt_session_get_economics(session, as.character(did_i), global_fallback = opt_economics_defaults())
    delta_user <- if (!is.null(get_resolution_delta_fn)) {
      get_resolution_delta_fn(did_i, rn)
    } else {
      NULL
    }
    out[[rn]] <- mf_build_loss_grid_for_response(
      d = d,
      fid = fid,
      did_i = did_i,
      resp_name = rn,
      economics = econ,
      registry_by_response = registry_by_response,
      finalized_by_response = finalized_by_response,
      get_state_fn = get_state_fn,
      f_r_types = f_r_types,
      ems_show_mixed_nest = ems_show_mixed_nest,
      ems_disp_type_default = ems_disp_type_default,
      conf_default = conf_default,
      resolution_delta_user = delta_user,
      blocked_factors = blocked_factors
    )
  }
  out
}
