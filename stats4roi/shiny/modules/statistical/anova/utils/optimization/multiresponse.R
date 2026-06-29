# Stage 3: multi-output aggregation and optimization.

if (!exists(".taguchi_emmeans_on_grid", mode = "function") && exists(".taguchi_opt_load_deps", mode = "function")) {
  .taguchi_opt_load_deps()
} else if (!exists(".taguchi_emmeans_on_grid", mode = "function")) {
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  repeat {
    cand <- file.path(d, "modules", "statistical", "anova", "utils", "optimization", "taguchi_loss_mvp.R")
    if (file.exists(cand)) {
      source(cand, local = FALSE)
      break
    }
    nd <- dirname(d)
    if (identical(nd, d)) break
    d <- nd
  }
}

#' @keywords internal
.coerce_bundle_response_type <- function(x) {
  t <- tolower(trimws(as.character(if (is.null(x)) "normal" else x)))
  if (!t %in% c("normal", "poisson", "binomial", "proportion")) "normal" else t
}

#' @keywords internal
.bundle_predict_metrics <- function(b, newdata) {
  if (!is.null(b$interp_ctx) && length(b$interp_ctx$cont_vars) > 0L) {
    ctx <- b$interp_ctx
    x_cont <- stats::setNames(
      vapply(ctx$cont_vars, function(vn) suppressWarnings(as.numeric(newdata[[vn]])), numeric(1)),
      ctx$cont_vars
    )
    disc_asgn <- .extract_disc_assignment(newdata, ctx$disc_vars)
    ip <- predict_factor_model_interp(
      mean_mod = ctx$mean_mod,
      disp_mod = ctx$disp_mod,
      x_cont = x_cont,
      cont_vars = ctx$cont_vars,
      cont_codes = ctx$cont_codes,
      disc_assignment = disc_asgn,
      resolution_ctx = if (!is.null(b$resolution_ctx)) b$resolution_ctx else ctx$resolution_ctx
    )
    if (!is.null(ip) && is.finite(ip$mu) && is.finite(ip$disp_pred)) {
      mu <- ip$mu
      disp_pred <- ip$disp_pred
      disp_res <- if (!is.null(b$resolution_ctx)) {
        resolve_dispersion_for_newdata(disp_pred, newdata, b$resolution_ctx)
      } else {
        list(effective = disp_pred, emm_raw = disp_pred, sigma = dispersion_metric_to_sigma(disp_pred, type = b$disp_type))
      }
      sigma <- disp_res$sigma[1]
      m <- taguchi_side_specific_metrics_normal(
        mu = mu,
        sigma = sigma,
        target = b$target,
        lsl = b$lsl,
        usl = b$usl,
        C_l = b$C_l,
        C_u = b$C_u
      )
      return(list(
        mu = mu,
        sigma = sigma,
        disp_pred = disp_pred,
        disp_effective = disp_res$effective[1],
        dispersion_resolution = disp_res,
        metrics = m,
        response_type = .coerce_bundle_response_type(b$response_type)
      ))
    }
  }

  if (exists("mf_complete_bundle_newdata", mode = "function")) {
    newdata <- mf_complete_bundle_newdata(b, newdata)
  }
  newdata <- mf_prepare_optimizer_newdata(b, newdata)
  if (nrow(newdata) == 1L) {
    mu <- .taguchi_predict_point(b$mean_model, newdata)
    mu <- c(mu)
  } else {
    mu <- .taguchi_emmeans_on_grid(b$mean_model, newdata)
  }
  response_type <- .coerce_bundle_response_type(b$response_type)
  sigma <- rep(NA_real_, length(mu))
  disp_pred <- rep(NA_real_, length(mu))
  disp_effective <- rep(NA_real_, length(mu))

  if (identical(response_type, "normal")) {
    if (nrow(newdata) == 1L) {
      disp_pred <- c(.taguchi_predict_point(b$disp_model, newdata))
    } else {
      disp_pred <- .taguchi_emmeans_on_grid(b$disp_model, newdata)
    }
    if (!is.null(b$resolution_ctx)) {
      disp_res <- resolve_dispersion_for_newdata(disp_pred[1], newdata, b$resolution_ctx)
      disp_effective[1] <- disp_res$effective[1]
      sigma[1] <- disp_res$sigma[1]
    } else {
      sigma <- vapply(disp_pred, dispersion_metric_to_sigma, numeric(1), type = b$disp_type)
      disp_effective <- disp_pred
    }
  } else if (identical(response_type, "poisson")) {
    mu_pos <- pmax(mu, 0)
    sigma <- sqrt(mu_pos)
  } else {
    # For binomial/proportion, default to 1 trial if bundle metadata is missing.
    n_trials <- suppressWarnings(as.numeric(if (!is.null(b$n_trials)) b$n_trials else if (!is.null(b$trials)) b$trials else if (!is.null(b$size)) b$size else 1))
    if (!is.finite(n_trials) || n_trials <= 0) n_trials <- 1
    p <- pmin(pmax(mu, 0), 1)
    sigma <- sqrt(n_trials * p * (1 - p))
  }

  m <- taguchi_side_specific_metrics_normal(
    mu = mu,
    sigma = sigma,
    target = b$target,
    lsl = b$lsl,
    usl = b$usl,
    C_l = b$C_l,
    C_u = b$C_u
  )

  # Override ppm for non-normal response families with exact family tails.
  if (!identical(response_type, "normal")) {
    has_lsl <- is.finite(b$lsl)
    has_usl <- is.finite(b$usl)
    ppm_l <- rep(NA_real_, length(mu))
    ppm_u <- rep(NA_real_, length(mu))
    if (identical(response_type, "poisson")) {
      mu_pos <- pmax(mu, 0)
      if (has_lsl) ppm_l <- stats::ppois(q = floor(b$lsl), lambda = mu_pos) * 1e6
      if (has_usl) ppm_u <- stats::ppois(q = ceiling(b$usl) - 1, lambda = mu_pos, lower.tail = FALSE) * 1e6
    } else {
      n_trials <- suppressWarnings(as.numeric(if (!is.null(b$n_trials)) b$n_trials else if (!is.null(b$trials)) b$trials else if (!is.null(b$size)) b$size else 1))
      if (!is.finite(n_trials) || n_trials <= 0) n_trials <- 1
      p <- pmin(pmax(mu, 0), 1)
      if (has_lsl) ppm_l <- stats::pbinom(q = floor(b$lsl), size = n_trials, prob = p) * 1e6
      if (has_usl) ppm_u <- stats::pbinom(q = ceiling(b$usl) - 1, size = n_trials, prob = p, lower.tail = FALSE) * 1e6
    }
    m$ppm_lower <- ppm_l
    m$ppm_upper <- ppm_u
    m$ppm <- rowSums(cbind(ppm_l, ppm_u), na.rm = TRUE)
    if (!has_lsl && !has_usl) m$ppm[] <- NA_real_
  }

  disp_res <- NULL
  if (identical(response_type, "normal") && !is.null(b$resolution_ctx) && length(disp_effective) >= 1L) {
    disp_res <- resolve_dispersion_for_newdata(disp_pred[1], newdata, b$resolution_ctx)
  }

  list(
    mu = mu,
    sigma = sigma,
    disp_pred = disp_pred,
    disp_effective = disp_effective,
    dispersion_resolution = disp_res,
    metrics = m,
    response_type = response_type
  )
}

#' Aggregate expected loss/ppm across response model bundles at a single setting.
#'
#' Per-response Taguchi expected loss and PPM are summed (no separate aggregation multipliers;
#' tune economics via each bundle's C_l/C_u and specs).
#' @keywords internal
aggregate_multiresponse_loss <- function(
    model_bundles,
    newdata) {
  if (!is.list(model_bundles) || length(model_bundles) < 1L) {
    stop("model_bundles must contain at least one response bundle.", call. = FALSE)
  }

  total_loss <- 0
  total_ppm <- 0
  total_base_loss_weighted <- 0
  details <- list()
  for (i in seq_along(model_bundles)) {
    b <- model_bundles[[i]]
    pred <- .bundle_predict_metrics(b, newdata = newdata)
    mu <- pred$mu
    sigma <- pred$sigma
    disp_pred <- pred$disp_pred
    m <- pred$metrics
    loss_i <- m$expected_loss[1]
    ppm_i <- m$ppm[1]
    loss_scaled <- if (is.finite(loss_i)) loss_i else Inf
    ppm_scaled <- if (is.finite(ppm_i)) ppm_i else 0
    total_loss <- total_loss + loss_scaled
    total_ppm <- total_ppm + ppm_scaled
    total_base_loss_weighted <- total_base_loss_weighted + loss_scaled
    details[[i]] <- list(
      response = if (!is.null(b$response_name)) b$response_name else paste0("response_", i),
      aggregation_weight = 1,
      response_type = pred$response_type,
      target = if (!is.null(b$target)) suppressWarnings(as.numeric(b$target)) else NA_real_,
      lsl = if (!is.null(b$lsl)) suppressWarnings(as.numeric(b$lsl)) else NA_real_,
      usl = if (!is.null(b$usl)) suppressWarnings(as.numeric(b$usl)) else NA_real_,
      C_l = if (!is.null(b$C_l)) suppressWarnings(as.numeric(b$C_l)) else NA_real_,
      C_u = if (!is.null(b$C_u)) suppressWarnings(as.numeric(b$C_u)) else NA_real_,
      mu = mu,
      sigma = sigma,
      disp_pred = disp_pred,
      disp_effective = if (!is.null(pred$disp_effective)) pred$disp_effective else disp_pred,
      dispersion_resolution = pred$dispersion_resolution,
      expected_loss = loss_i,
      weighted_expected_loss = loss_scaled,
      ppm = ppm_i,
      weighted_ppm = ppm_scaled,
      metrics = m
    )
  }

  value <- total_loss

  list(
    value = value,
    total_expected_loss = total_loss,
    total_ppm = total_ppm,
    total_weighted_base_loss = total_base_loss_weighted,
    ppm_penalty_term = 0,
    details = details
  )
}

#' @keywords internal
.mr_numeric_levels_from_xlevels <- function(model, vn, lo = -Inf, hi = Inf, dat = NULL) {
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

#' @keywords internal
.build_newdata_mr <- function(par_cont, cont_vars, disc_assignment, disc_vars, ref_model) {
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

optimize_multiresponse_continuous <- function(
    model_bundles,
    bounds,
    optimize_target = c("taguchi_loss", "total_cost"),
    objective_direction = c("min", "max"),
    volume = 1,
    unit_setting_cost_fn = NULL,
    start = NULL,
    control = list(maxit = 200),
    factor_kinds = NULL) {
  optimize_target <- match.arg(optimize_target)
  objective_direction <- match.arg(objective_direction)
  if (!is.list(model_bundles) || length(model_bundles) < 1L) {
    stop("model_bundles must contain at least one response bundle.", call. = FALSE)
  }
  if (!is.list(bounds) || length(bounds) < 1L) {
    stop("bounds must be a named list with lower/upper for each predictor.", call. = FALSE)
  }
  if (!is.finite(volume) || volume <= 0) {
    stop("volume must be a positive finite number.", call. = FALSE)
  }

  vars <- names(bounds)
  if (is.null(vars) || any(!nzchar(vars))) {
    stop("bounds must be named by predictor names.", call. = FALSE)
  }
  lower <- vapply(bounds, function(b) as.numeric(b[["lower"]]), numeric(1))
  upper <- vapply(bounds, function(b) as.numeric(b[["upper"]]), numeric(1))
  if (any(!is.finite(lower)) || any(!is.finite(upper)) || any(lower >= upper)) {
    stop("each bound must provide finite lower < upper.", call. = FALSE)
  }

  if (is.null(start)) {
    start <- (lower + upper) / 2
  } else {
    start <- as.numeric(start)
    if (length(start) != length(vars)) stop("start length must match number of bounded predictors.", call. = FALSE)
  }
  start <- pmin(pmax(start, lower), upper)
  names(start) <- vars

  ref_model <- model_bundles[[1]]$mean_model
  ref_dat <- model_bundles[[1]]$dat

  kinds <- setNames(rep(NA_character_, length(vars)), vars)
  if (!is.null(factor_kinds)) {
    fk <- tolower(as.character(factor_kinds)); names(fk) <- names(factor_kinds)
    shared <- intersect(names(fk), vars)
    kinds[shared] <- fk[shared]
  }
  for (vn in vars) {
    if (!kinds[[vn]] %in% c("continuous", "discrete")) {
      kinds[[vn]] <- if (!is.null(ref_model$xlevels[[vn]]) && length(ref_model$xlevels[[vn]]) > 0L) "discrete" else "continuous"
    }
  }
  cont_vars <- vars[kinds[vars] == "continuous"]
  disc_vars <- vars[kinds[vars] == "discrete"]

  use_factor_model_interp <- length(cont_vars) > 0L && all(vapply(
    model_bundles,
    function(b) !is.null(b$interp_ctx) && length(b$interp_ctx$cont_vars) > 0L,
    logical(1)
  ))

  if (length(cont_vars) > 0L && !isTRUE(use_factor_model_interp)) {
    bad_cont <- cont_vars[vapply(cont_vars, function(vn) !is.null(ref_model$xlevels[[vn]]) && length(ref_model$xlevels[[vn]]) > 0L, logical(1))]
    if (length(bad_cont) > 0L) {
      stop(paste0("Continuous factors were modeled as discrete levels: ", paste(bad_cont, collapse = ", "), "."), call. = FALSE)
    }
  }

  disc_levels <- list()
  if (length(disc_vars) > 0L) {
    for (vn in disc_vars) {
      lv <- .mr_numeric_levels_from_xlevels(ref_model, vn, lower[[vn]], upper[[vn]], dat = ref_dat)
      if (is.null(lv)) stop(paste0("Could not determine discrete coded levels for factor '", vn, "'."), call. = FALSE)
      disc_levels[[vn]] <- lv
    }
  }

  eval_obj <- function(nd) {
    agg <- aggregate_multiresponse_loss(
      model_bundles = model_bundles,
      newdata = nd
    )
    unit_setting_cost <- if (is.null(unit_setting_cost_fn)) {
      0
    } else {
      suppressWarnings(as.numeric(unit_setting_cost_fn(nd)))
    }
    if (!is.finite(unit_setting_cost)) unit_setting_cost <- 0
    agg$objective_loss <- agg$value
    agg$unit_setting_cost <- unit_setting_cost
    agg$total_setting_cost <- volume * unit_setting_cost
    agg$total_cost_term <- if (identical(optimize_target, "total_cost")) volume * (agg$objective_loss + unit_setting_cost) else NA_real_
    agg$value <- if (identical(optimize_target, "total_cost")) {
      volume * (agg$objective_loss + unit_setting_cost)
    } else {
      agg$objective_loss
    }
    agg
  }

  disc_candidates <- if (length(disc_vars) == 0L) {
    data.frame(.dummy = 1L)[, 0, drop = FALSE]
  } else {
    n_grid <- prod(vapply(disc_levels, length, numeric(1)))
    if (!is.finite(n_grid) || n_grid < 1L || n_grid > 1e5) {
      stop("Discrete search grid is too large; tighten bounds or reduce discrete factors.", call. = FALSE)
    }
    expand.grid(disc_levels, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  best <- list(
    value = if (identical(objective_direction, "min")) Inf else -Inf,
    nd = NULL, par = NULL, conv = 0L, msg = "", agg = NULL
  )
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
      nd <- .build_newdata_mr(par_cont = numeric(0), cont_vars = character(0), disc_assignment = disc_assignment, disc_vars = disc_vars, ref_model = ref_model)
      ev <- eval_obj(nd)
      if (is_better(ev$value, best$value)) {
        par <- numeric(length(vars)); names(par) <- vars
        for (vn in disc_vars) par[[vn]] <- as.numeric(disc_assignment[[vn]])
        best <- list(value = ev$value, nd = nd, par = par, conv = 0L, msg = "", agg = ev)
      }
      next
    }

    cont_lower <- lower[cont_vars]
    cont_upper <- upper[cont_vars]
    cont_start <- start[cont_vars]

    objective_fun <- function(x) {
      x <- as.numeric(x); names(x) <- cont_vars
      nd <- .build_newdata_mr(par_cont = x, cont_vars = cont_vars, disc_assignment = disc_assignment, disc_vars = disc_vars, ref_model = ref_model)
      ev <- eval_obj(nd)
      if (!is.finite(ev$value)) return(Inf)
      ev$value
    }

    opt <- stats::optim(
      par = cont_start,
      fn = .optim_minimand(objective_fun, objective_direction),
      method = "L-BFGS-B",
      lower = cont_lower,
      upper = cont_upper,
      control = control
    )
    par_cont <- as.numeric(opt$par); names(par_cont) <- cont_vars
    nd <- .build_newdata_mr(par_cont = par_cont, cont_vars = cont_vars, disc_assignment = disc_assignment, disc_vars = disc_vars, ref_model = ref_model)
    ev <- eval_obj(nd)
    if (is_better(ev$value, best$value)) {
      par <- numeric(length(vars)); names(par) <- vars
      for (vn in cont_vars) par[[vn]] <- par_cont[[vn]]
      for (vn in disc_vars) par[[vn]] <- as.numeric(disc_assignment[[vn]])
      best <- list(value = ev$value, nd = nd, par = par, conv = opt$convergence, msg = if (opt$convergence == 0) "" else opt$message, agg = ev)
    }
  }

  if (!is.finite(best$value) || is.null(best$nd) || is.null(best$par) || is.null(best$agg)) {
    return(list(ok = FALSE, message = "Optimization failed to find a finite multi-response objective.", convergence = 1))
  }

  agg <- best$agg
  first_detail <- agg$details[[1]]

  list(
    ok = is.finite(agg$value),
    message = best$msg,
    convergence = best$conv,
    optimize_target = optimize_target,
    volume = volume,
    value = agg$value,
    par = best$par,
    par_snapped = as.data.frame(best$nd, stringsAsFactors = FALSE),
    mu = first_detail$mu,
    sigma = first_detail$sigma,
    disp_pred = first_detail$disp_pred,
    metrics = first_detail$metrics,
    factor_kinds = kinds,
    objective_loss = agg$objective_loss,
    unit_setting_cost = agg$unit_setting_cost,
    total_setting_cost = agg$total_setting_cost,
    objective_breakdown = list(
      weighted_base_loss_sum = agg$total_weighted_base_loss,
      weighted_loss_sum = agg$total_expected_loss,
      weighted_ppm_sum = agg$total_ppm,
      ppm_penalty_term = agg$ppm_penalty_term,
      objective_loss = agg$objective_loss,
      unit_setting_cost = agg$unit_setting_cost,
      total_setting_cost = agg$total_setting_cost,
      final_objective = agg$value
    ),
    aggregate = list(
      response_count = length(model_bundles),
      total_expected_loss = agg$total_expected_loss,
      total_ppm = agg$total_ppm,
      details = agg$details
    )
  )
}
