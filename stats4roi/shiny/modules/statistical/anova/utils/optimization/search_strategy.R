# Stage 2: mixed-space optimizer over coded factors.
#
# Optimizes expected Taguchi loss (or weighted loss+PPM) while supporting a mix of
# discrete and continuous factors:
# - discrete factors are searched on observed coded levels from the fitted model;

.taguchi_opt_load_deps <- function() {
  if (exists(".taguchi_predict_point", mode = "function")) return(invisible(NULL))
  dirs <- character(0)
  of <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(of) && nzchar(of)) {
    dirs <- c(dirs, dirname(normalizePath(of, winslash = "/", mustWork = FALSE)))
  }
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  repeat {
    cand <- file.path(d, "modules", "statistical", "anova", "utils", "optimization")
    if (dir.exists(cand)) {
      dirs <- c(dirs, cand)
      break
    }
    nd <- dirname(d)
    if (identical(nd, d)) break
    d <- nd
  }
  for (opt_dir_local in unique(dirs)) {
    ah <- normalizePath(file.path(opt_dir_local, "..", "anova_helpers.R"), winslash = "/", mustWork = FALSE)
    if (file.exists(ah) && !exists("factorial_cell_replication", mode = "function")) {
      source(ah, local = FALSE)
    }
    for (fn in c("dispersion_metric_sigma.R", "dispersion_resolution_prior.R", "taguchi_loss_mvp.R")) {
      dep <- file.path(opt_dir_local, fn)
      if (file.exists(dep)) source(dep, local = FALSE)
    }
    oh <- normalizePath(file.path(opt_dir_local, "..", "..", "server", "optimization", "optimization_helpers.R"), winslash = "/", mustWork = FALSE)
    if (file.exists(oh) && !exists("mf_discrete_coded_levels", mode = "function")) {
      source(oh, local = FALSE)
    }
    if (exists(".taguchi_predict_point", mode = "function")) break
  }
  invisible(NULL)
}
.taguchi_opt_load_deps()

# - continuous factors use L-BFGS-B within bounds (or interpolation when configured).

#' @keywords internal
.numeric_levels_from_xlevels <- function(model, vn, lo = -Inf, hi = Inf, dat = NULL) {
  if (exists("mf_discrete_coded_levels", mode = "function")) {
    return(mf_discrete_coded_levels(model, vn, dat = dat, lo = lo, hi = hi))
  }
  xl <- model$xlevels[[vn]]
  if (is.null(xl) || length(xl) < 1L) return(NULL)
  lv <- suppressWarnings(as.numeric(as.character(xl)))
  if (any(!is.finite(lv))) return(NULL)
  in_rng <- sort(unique(lv[lv >= lo & lv <= hi]))
  if (length(in_rng) >= 1L) return(in_rng)
  mid <- (lo + hi) / 2
  d <- abs(lv - mid)
  lv <- sort(unique(lv[d == min(d, na.rm = TRUE)]))
  if (length(lv) < 1L) return(NULL)
  lv
}

#' Piecewise-linear interpolation between adjacent observed coded design levels.
#' @keywords internal
.interp_factor_level_metric <- function(interp, x) {
  stats::approx(
    x = interp$codes,
    y = interp$values,
    xout = suppressWarnings(as.numeric(x)),
    method = "linear",
    rule = 2,
    ties = "ordered"
  )$y
}

#' Scalar objective wrapper for stats::optim (always minimizes).
#' @keywords internal
.optim_minimand <- function(objective_fun, objective_direction = c("min", "max")) {
  objective_direction <- match.arg(objective_direction)
  if (identical(objective_direction, "max")) {
    function(x) {
      val <- suppressWarnings(as.numeric(objective_fun(x)))[1L]
      if (!is.finite(val)) return(Inf)
      -val
    }
  } else {
    function(x) {
      val <- suppressWarnings(as.numeric(objective_fun(x)))[1L]
      if (!is.finite(val)) return(Inf)
      val
    }
  }
}

#' Optimize a single continuous coded factor on each adjacent design-level segment.
#' @keywords internal
.optimize_cont_var_on_segments <- function(
    objective_fun,
    codes,
    start_scalar,
    control,
    objective_direction = c("min", "max"),
    lower = -Inf,
    upper = Inf) {
  objective_direction <- match.arg(objective_direction)
  codes <- sort(unique(suppressWarnings(as.numeric(codes))))
  codes <- codes[is.finite(codes)]
  lower <- suppressWarnings(as.numeric(lower))[1L]
  upper <- suppressWarnings(as.numeric(upper))[1L]
  if (!is.finite(lower)) lower <- -Inf
  if (!is.finite(upper)) upper <- Inf
  if (length(codes) < 1L) {
    return(list(par = NA_real_, value = NA_real_, conv = 1L, msg = "No design levels for segment optimization."))
  }

  best_val <- if (identical(objective_direction, "min")) Inf else -Inf
  best_par <- NA_real_
  best_conv <- 0L
  best_msg <- ""
  is_better <- function(candidate, current) {
    if (!is.finite(candidate)) return(FALSE)
    if (identical(objective_direction, "max")) candidate > current else candidate < current
  }
  consider <- function(par) {
    par <- suppressWarnings(as.numeric(par))[1L]
    if (!is.finite(par) || par < lower || par > upper) return(invisible(NULL))
    val <- objective_fun(par)
    if (is_better(val, best_val)) {
      best_val <<- val
      best_par <<- par
      best_conv <<- 0L
      best_msg <<- ""
    }
    invisible(NULL)
  }

  for (code in codes) consider(code)

  if (length(codes) >= 2L) {
    for (i in seq_len(length(codes) - 1L)) {
      seg_lo <- max(codes[[i]], lower)
      seg_hi <- min(codes[[i + 1L]], upper)
      if (!is.finite(seg_lo) || !is.finite(seg_hi) || seg_lo >= seg_hi) next
      seg_start <- suppressWarnings(as.numeric(start_scalar))[1L]
      if (!is.finite(seg_start) || seg_start < seg_lo || seg_start > seg_hi) {
        seg_start <- (seg_lo + seg_hi) / 2
      }
      opt <- stats::optim(
        par = seg_start,
        fn = .optim_minimand(objective_fun, objective_direction),
        method = "L-BFGS-B",
        lower = seg_lo,
        upper = seg_hi,
        control = control
      )
      consider(as.numeric(opt$par))
    }
  }

  list(par = best_par, value = best_val, conv = best_conv, msg = best_msg)
}

#' @keywords internal
.predict_interp_metrics <- function(nd, cont_vars, factor_interp) {
  if (length(cont_vars) != 1L || is.null(factor_interp) || !cont_vars[[1L]] %in% names(factor_interp)) {
    return(NULL)
  }
  vn <- cont_vars[[1L]]
  ip <- factor_interp[[vn]]
  if (is.null(ip$codes) || length(ip$codes) < 2L) return(NULL)
  x <- suppressWarnings(as.numeric(nd[[vn]]))[1L]
  if (!is.finite(x)) return(NULL)
  mu <- .interp_factor_level_metric(list(codes = ip$codes, values = ip$mu), x)
  disp_pred <- .interp_factor_level_metric(list(codes = ip$codes, values = ip$disp), x)
  if (!is.finite(mu) || !is.finite(disp_pred)) return(NULL)
  list(mu = mu, disp_pred = disp_pred)
}

#' @keywords internal
.build_newdata_mixed <- function(par_cont, cont_vars, disc_assignment, disc_vars, ref_model) {
  nd <- as.data.frame(as.list(setNames(rep(NA_real_, length(cont_vars) + length(disc_vars)), c(cont_vars, disc_vars))), stringsAsFactors = FALSE)
  if (length(cont_vars) > 0L) {
    for (vn in cont_vars) nd[[vn]] <- suppressWarnings(as.numeric(par_cont[[vn]]))
  }
  if (length(disc_vars) > 0L) {
    for (vn in disc_vars) {
      xl <- ref_model$xlevels[[vn]]
      pick <- as.character(disc_assignment[[vn]])
      nd[[vn]] <- factor(pick, levels = xl)
    }
  }
  nd[, c(cont_vars, disc_vars), drop = FALSE]
}

optimize_loss_continuous <- function(
    mean_model,
    disp_model,
    bounds,
    target,
    lsl = NA_real_,
    usl = NA_real_,
    C_l = 0,
    C_u = 0,
    disp_type = "ADA",
    optimize_target = c("taguchi_loss", "total_cost"),
    objective_direction = c("min", "max"),
    volume = 1,
    unit_setting_cost_fn = NULL,
    start = NULL,
    control = list(maxit = 200),
    factor_kinds = NULL,
    factor_interp = NULL,
    interp_ctx = NULL,
    resolution_ctx = NULL,
    dat = NULL) {
  optimize_target <- match.arg(optimize_target)
  objective_direction <- match.arg(objective_direction)

  if (is.null(mean_model) || is.null(disp_model)) stop("mean_model and disp_model are required.", call. = FALSE)
  if (!is.list(bounds) || length(bounds) < 1L) stop("bounds must be a named list with lower/upper for each predictor.", call. = FALSE)
  if (!is.finite(target)) stop("target must be finite.", call. = FALSE)
  if (!is.finite(volume) || volume <= 0) stop("volume must be a positive finite number.", call. = FALSE)
  if (is.finite(lsl) && is.finite(usl) && lsl >= usl) stop("LSL must be less than USL when both are provided.", call. = FALSE)
  if (is.finite(lsl) && target <= lsl) stop("target must be greater than LSL for lower-side scaling.", call. = FALSE)
  if (is.finite(usl) && target >= usl) stop("target must be less than USL for upper-side scaling.", call. = FALSE)

  vars <- names(bounds)
  if (is.null(vars) || any(!nzchar(vars))) stop("bounds must be named by predictor names.", call. = FALSE)

  lower <- vapply(bounds, function(b) as.numeric(b[["lower"]]), numeric(1))
  upper <- vapply(bounds, function(b) as.numeric(b[["upper"]]), numeric(1))
  if (any(!is.finite(lower)) || any(!is.finite(upper)) || any(lower >= upper)) stop("each bound must provide finite lower < upper.", call. = FALSE)

  if (is.null(start)) {
    start <- (lower + upper) / 2
  } else {
    start <- as.numeric(start)
    if (length(start) != length(vars)) stop("start length must match number of bounded predictors.", call. = FALSE)
  }
  start <- pmin(pmax(start, lower), upper)
  names(start) <- vars

  kinds <- setNames(rep(NA_character_, length(vars)), vars)
  if (!is.null(factor_kinds)) {
    fk <- tolower(as.character(factor_kinds))
    names(fk) <- names(factor_kinds)
    shared <- intersect(names(fk), vars)
    kinds[shared] <- fk[shared]
  }
  for (vn in vars) {
    if (!kinds[[vn]] %in% c("continuous", "discrete")) {
      kinds[[vn]] <- if (!is.null(mean_model$xlevels[[vn]]) && length(mean_model$xlevels[[vn]]) > 0L) "discrete" else "continuous"
    }
  }
  cont_vars <- vars[kinds[vars] == "continuous"]
  disc_vars <- vars[kinds[vars] == "discrete"]

  use_oneway_factor_interp <- !is.null(factor_interp) && length(factor_interp) > 0L &&
    length(disc_vars) == 0L && length(cont_vars) == 1L && cont_vars[[1L]] %in% names(factor_interp)
  use_factor_model_interp <- !is.null(interp_ctx) && length(interp_ctx$cont_vars) > 0L
  use_interp <- isTRUE(use_oneway_factor_interp) || isTRUE(use_factor_model_interp)

  if (length(cont_vars) > 0L && !use_interp) {
    bad_cont <- cont_vars[vapply(cont_vars, function(vn) !is.null(mean_model$xlevels[[vn]]) && length(mean_model$xlevels[[vn]]) > 0L, logical(1))]
    if (length(bad_cont) > 0L) {
      stop(paste0("Continuous factors were modeled as discrete levels: ", paste(bad_cont, collapse = ", "), ". Mark them as discrete or recode in data/model."), call. = FALSE)
    }
  }

  disc_levels <- list()
  if (length(disc_vars) > 0L) {
    for (vn in disc_vars) {
      lv <- .numeric_levels_from_xlevels(mean_model, vn, lower[[vn]], upper[[vn]], dat = dat)
      if (is.null(lv)) {
        stop(paste0("Could not determine discrete coded levels for factor '", vn, "'."), call. = FALSE)
      }
      disc_levels[[vn]] <- lv
    }
  }

  eval_metrics <- function(nd, disc_assignment = list()) {
    interp_pred <- if (use_oneway_factor_interp) {
      .predict_interp_metrics(nd, cont_vars, factor_interp)
    } else if (use_factor_model_interp) {
      x_cont <- stats::setNames(
        vapply(interp_ctx$cont_vars, function(vn) suppressWarnings(as.numeric(nd[[vn]])), numeric(1)),
        interp_ctx$cont_vars
      )
      disc_asgn <- disc_assignment
      if (length(disc_asgn) < 1L) {
        disc_asgn <- .extract_disc_assignment(nd, interp_ctx$disc_vars)
      }
      predict_factor_model_interp(
        mean_mod = interp_ctx$mean_mod,
        disp_mod = interp_ctx$disp_mod,
        x_cont = x_cont,
        cont_vars = interp_ctx$cont_vars,
        cont_codes = interp_ctx$cont_codes,
        disc_assignment = disc_asgn,
        resolution_ctx = if (!is.null(interp_ctx$resolution_ctx)) interp_ctx$resolution_ctx else resolution_ctx
      )
    } else {
      NULL
    }
    if (!is.null(interp_pred)) {
      mu <- interp_pred$mu
      disp_pred <- interp_pred$disp_pred
      disp_res <- if (!is.null(resolution_ctx) || (!is.null(interp_ctx) && !is.null(interp_ctx$resolution_ctx))) {
        ctx_use <- if (!is.null(interp_ctx) && !is.null(interp_ctx$resolution_ctx)) interp_ctx$resolution_ctx else resolution_ctx
        resolve_dispersion_for_newdata(disp_pred, nd, ctx_use)
      } else {
        list(effective = disp_pred, emm_raw = disp_pred, sigma = dispersion_metric_to_sigma(disp_pred, type = disp_type))
      }
    } else {
      mu <- .taguchi_predict_point(mean_model, nd)
      disp_pred <- .taguchi_predict_point(disp_model, nd)
      disp_res <- if (!is.null(resolution_ctx)) {
        resolve_dispersion_for_newdata(disp_pred, nd, resolution_ctx)
      } else {
        list(effective = disp_pred, emm_raw = disp_pred, sigma = dispersion_metric_to_sigma(disp_pred, type = disp_type))
      }
    }
    sigma <- disp_res$sigma[1]
    disp_effective <- disp_res$effective[1]
    m <- taguchi_side_specific_metrics_normal(mu = mu, sigma = sigma, target = target, lsl = lsl, usl = usl, C_l = C_l, C_u = C_u)
    loss <- m$expected_loss[1]
    ppm <- m$ppm[1]
    obj_loss <- loss
    ppm_penalty_term <- 0
    unit_setting_cost <- if (is.null(unit_setting_cost_fn)) {
      0
    } else {
      suppressWarnings(as.numeric(unit_setting_cost_fn(nd)))
    }
    if (!is.finite(unit_setting_cost)) unit_setting_cost <- 0
    total_obj <- if (identical(optimize_target, "total_cost")) {
      volume * (obj_loss + unit_setting_cost)
    } else {
      obj_loss
    }
    list(
      mu = mu, disp_pred = disp_pred, disp_effective = disp_effective, sigma = sigma, metrics = m,
      dispersion_resolution = disp_res,
      value = total_obj, objective_loss = obj_loss,
      ppm_penalty_term = ppm_penalty_term,
      unit_setting_cost = unit_setting_cost,
      total_setting_cost = volume * unit_setting_cost
    )
  }

  disc_candidates <- if (length(disc_vars) == 0L) {
    data.frame(.dummy = 1L)[, 0, drop = FALSE]
  } else {
    n_grid <- prod(vapply(disc_levels, length, numeric(1)))
    if (!is.finite(n_grid) || n_grid < 1L || n_grid > 1e5) stop("Discrete search grid is too large; tighten bounds or reduce discrete factors.", call. = FALSE)
    expand.grid(disc_levels, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  best <- list(value = if (identical(objective_direction, "min")) Inf else -Inf, nd = NULL, par = NULL, conv = 0L, msg = "")
  is_better <- function(candidate, current) {
    if (!is.finite(candidate)) return(FALSE)
    if (identical(objective_direction, "max")) {
      return(candidate > current)
    }
    candidate < current
  }

  for (i in seq_len(max(1L, nrow(disc_candidates)))) {
    disc_assignment <- if (length(disc_vars) == 0L) list() else as.list(disc_candidates[i, , drop = FALSE])

    if (length(cont_vars) == 0L) {
      nd <- .build_newdata_mixed(par_cont = numeric(0), cont_vars = character(0), disc_assignment = disc_assignment, disc_vars = disc_vars, ref_model = mean_model)
      ev <- eval_metrics(nd, disc_assignment = disc_assignment)
      if (is_better(ev$value, best$value)) {
        par <- numeric(length(vars)); names(par) <- vars
        for (vn in disc_vars) par[[vn]] <- as.numeric(disc_assignment[[vn]])
        best <- list(value = ev$value, nd = nd, par = par, conv = 0L, msg = "")
      }
      next
    }

    cont_lower <- lower[cont_vars]
    cont_upper <- upper[cont_vars]
    cont_start <- start[cont_vars]

    objective_fun <- function(x) {
      x <- as.numeric(x)
      if (length(x) == 1L) x <- setNames(x, cont_vars)
      names(x) <- cont_vars
      nd <- .build_newdata_mixed(par_cont = x, cont_vars = cont_vars, disc_assignment = disc_assignment, disc_vars = disc_vars, ref_model = mean_model)
      ev <- eval_metrics(nd, disc_assignment = disc_assignment)
      if (!is.finite(ev$value)) return(Inf)
      ev$value
    }

    if (use_oneway_factor_interp) {
      vn <- cont_vars[[1L]]
      codes <- factor_interp[[vn]]$codes
      seg_res <- .optimize_cont_var_on_segments(
        objective_fun = function(val) objective_fun(setNames(val, vn)),
        codes = codes,
        start_scalar = cont_start[[vn]],
        control = control,
        objective_direction = objective_direction,
        lower = cont_lower[[vn]],
        upper = cont_upper[[vn]]
      )
      par_cont <- setNames(seg_res$par, vn)
      opt_conv <- seg_res$conv
      opt_msg <- seg_res$msg
    } else if (use_factor_model_interp) {
      objective_fun_vec <- function(x) {
        x <- as.numeric(x)
        names(x) <- cont_vars
        objective_fun(x)
      }
      par_cont <- cont_start
      best_val <- objective_fun_vec(par_cont)
      if (!is.finite(best_val)) {
        best_val <- if (identical(objective_direction, "min")) Inf else -Inf
      }
      for (cycle in seq_len(12L)) {
        improved <- FALSE
        for (vn in cont_vars) {
          seg_res <- .optimize_cont_var_on_segments(
            objective_fun = function(val) {
              trial <- par_cont
              trial[[vn]] <- val
              objective_fun_vec(trial)
            },
            codes = interp_ctx$cont_codes[[vn]],
            start_scalar = par_cont[[vn]],
            control = control,
            objective_direction = objective_direction,
            lower = cont_lower[[vn]],
            upper = cont_upper[[vn]]
          )
          if (is_better(seg_res$value, best_val)) {
            par_cont[[vn]] <- seg_res$par
            best_val <- seg_res$value
            improved <- TRUE
          }
        }
        if (!isTRUE(improved)) break
      }
      opt_conv <- 0L
      opt_msg <- ""
    } else {
      objective_fun_vec <- function(x) {
        x <- as.numeric(x)
        names(x) <- cont_vars
        objective_fun(x)
      }
      opt <- stats::optim(
        par = cont_start,
        fn = .optim_minimand(objective_fun_vec, objective_direction),
        method = "L-BFGS-B",
        lower = cont_lower,
        upper = cont_upper,
        control = control
      )
      par_cont <- as.numeric(opt$par)
      names(par_cont) <- cont_vars
      opt_conv <- opt$convergence
      opt_msg <- if (opt$convergence == 0) "" else opt$message
    }

    nd <- .build_newdata_mixed(par_cont = par_cont, cont_vars = cont_vars, disc_assignment = disc_assignment, disc_vars = disc_vars, ref_model = mean_model)
    ev <- eval_metrics(nd, disc_assignment = disc_assignment)

    if (is_better(ev$value, best$value)) {
      par <- numeric(length(vars))
      names(par) <- vars
      for (vn in cont_vars) par[[vn]] <- par_cont[[vn]]
      for (vn in disc_vars) par[[vn]] <- as.numeric(disc_assignment[[vn]])
      best <- list(value = ev$value, nd = nd, par = par, conv = opt_conv, msg = opt_msg)
    }
  }

  if (!is.finite(best$value) || is.null(best$nd) || is.null(best$par)) {
    return(list(ok = FALSE, message = "Optimization failed to find a finite objective.", convergence = 1))
  }

  ev_best <- eval_metrics(
    best$nd,
    disc_assignment = .extract_disc_assignment(best$nd, disc_vars)
  )
  cap <- optimizer_capability_measures(
    mu = ev_best$mu,
    sigma = ev_best$sigma,
    target = target,
    lsl = lsl,
    usl = usl
  )
  list(
    ok = is.finite(ev_best$value),
    message = best$msg,
    convergence = best$conv,
    optimize_target = optimize_target,
    volume = volume,
    value = ev_best$value,
    par = best$par,
    par_snapped = as.data.frame(best$nd, stringsAsFactors = FALSE),
    mu = ev_best$mu,
    sigma = ev_best$sigma,
    disp_pred = ev_best$disp_pred,
    disp_effective = ev_best$disp_effective,
    dispersion_resolution = ev_best$dispersion_resolution,
    metrics = ev_best$metrics,
    factor_kinds = kinds,
    objective_loss = ev_best$objective_loss,
    ppm_penalty_term = ev_best$ppm_penalty_term,
    unit_setting_cost = ev_best$unit_setting_cost,
    total_setting_cost = ev_best$total_setting_cost,
    capability = cap,
    objective_breakdown = list(
      weighted_base_loss_sum = ev_best$metrics$expected_loss[1],
      weighted_loss_sum = ev_best$metrics$expected_loss[1],
      weighted_ppm_sum = ev_best$metrics$ppm[1],
      ppm_penalty_term = ev_best$ppm_penalty_term,
      objective_loss = ev_best$objective_loss,
      unit_setting_cost = ev_best$unit_setting_cost,
      total_setting_cost = ev_best$total_setting_cost,
      final_objective = ev_best$value
    )
  )
}
