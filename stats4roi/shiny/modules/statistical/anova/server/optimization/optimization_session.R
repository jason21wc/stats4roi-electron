# OptimizationSession state helpers (pure R, no Shiny).

#' Default Taguchi economics for one response.
#' @keywords internal
opt_economics_defaults <- function() {
  list(target = 0, lsl = NA_real_, usl = NA_real_, C_l = 0, C_u = 0)
}

#' Initialize an OptimizationSession list.
#' @keywords internal
opt_session_init <- function() {
  list(by_response = list())
}

#' Prune session entries when selected responses change.
#' @keywords internal
opt_session_prune <- function(session, keep_keys) {
  if (is.null(session) || !is.list(session)) session <- opt_session_init()
  br <- session$by_response
  if (is.null(br) || length(br) < 1L) {
    session$by_response <- list()
    return(session)
  }
  keep_keys <- as.character(keep_keys)
  session$by_response <- br[names(br) %in% keep_keys]
  session
}

#' Read economics for a response; optional global fallback for single-response mode.
#' @keywords internal
opt_session_get_economics <- function(session, response_key, global_fallback = NULL) {
  defaults <- opt_economics_defaults()
  key <- as.character(response_key)
  br <- if (!is.null(session) && is.list(session$by_response)) session$by_response else list()
  stored <- br[[key]]
  if (!is.null(stored) && is.list(stored$economics)) {
    econ <- stored$economics
    return(list(
      target = if (!is.null(econ$target)) suppressWarnings(as.numeric(econ$target)) else defaults$target,
      lsl = if (!is.null(econ$lsl)) suppressWarnings(as.numeric(econ$lsl)) else defaults$lsl,
      usl = if (!is.null(econ$usl)) suppressWarnings(as.numeric(econ$usl)) else defaults$usl,
      C_l = if (!is.null(econ$C_l)) suppressWarnings(as.numeric(econ$C_l)) else defaults$C_l,
      C_u = if (!is.null(econ$C_u)) suppressWarnings(as.numeric(econ$C_u)) else defaults$C_u
    ))
  }
  if (!is.null(global_fallback) && is.list(global_fallback)) {
    return(list(
      target = if (!is.null(global_fallback$target)) suppressWarnings(as.numeric(global_fallback$target)) else defaults$target,
      lsl = if (!is.null(global_fallback$lsl)) suppressWarnings(as.numeric(global_fallback$lsl)) else defaults$lsl,
      usl = if (!is.null(global_fallback$usl)) suppressWarnings(as.numeric(global_fallback$usl)) else defaults$usl,
      C_l = if (!is.null(global_fallback$C_l)) suppressWarnings(as.numeric(global_fallback$C_l)) else defaults$C_l,
      C_u = if (!is.null(global_fallback$C_u)) suppressWarnings(as.numeric(global_fallback$C_u)) else defaults$C_u
    ))
  }
  defaults
}

#' Write economics for one response into session (returns updated session).
#' @keywords internal
opt_session_set_economics <- function(session, response_key, economics) {
  if (is.null(session) || !is.list(session)) session <- opt_session_init()
  key <- as.character(response_key)
  if (is.null(session$by_response)) session$by_response <- list()
  entry <- session$by_response[[key]]
  if (is.null(entry)) entry <- list()
  entry$economics <- economics
  session$by_response[[key]] <- entry
  session
}

#' Build economics list from scalar inputs.
#' @keywords internal
opt_economics_from_scalars <- function(target, lsl, usl, C_l, C_u) {
  list(
    target = suppressWarnings(as.numeric(target)),
    lsl = suppressWarnings(as.numeric(lsl)),
    usl = suppressWarnings(as.numeric(usl)),
    C_l = suppressWarnings(as.numeric(C_l)),
    C_u = suppressWarnings(as.numeric(C_u))
  )
}

#' Whether per-response economics are complete enough to optimize (finite target).
#' @keywords internal
opt_economics_ready <- function(economics) {
  if (is.null(economics) || !is.list(economics)) return(FALSE)
  tgt <- suppressWarnings(as.numeric(economics$target))
  isTRUE(is.finite(tgt))
}

#' Whether Taguchi economics are complete for loss-grid / optimization (target, spec, costs).
#' @keywords internal
opt_economics_complete <- function(economics) {
  if (is.null(economics) || !is.list(economics)) return(FALSE)
  tgt <- suppressWarnings(as.numeric(economics$target))
  lsl <- suppressWarnings(as.numeric(economics$lsl))
  usl <- suppressWarnings(as.numeric(economics$usl))
  c_l <- suppressWarnings(as.numeric(economics$C_l))
  c_u <- suppressWarnings(as.numeric(economics$C_u))
  isTRUE(is.finite(tgt)) &&
    (is.finite(lsl) || is.finite(usl)) &&
    is.finite(c_l) && is.finite(c_u)
}

#' Whether Taguchi economics are valid for loss computation (target inside spec window).
#' @keywords internal
opt_economics_valid_for_loss <- function(economics) {
  if (!isTRUE(opt_economics_complete(economics))) return(FALSE)
  tgt <- suppressWarnings(as.numeric(economics$target))
  lsl <- suppressWarnings(as.numeric(economics$lsl))
  usl <- suppressWarnings(as.numeric(economics$usl))
  if (is.finite(lsl) && tgt <= lsl) return(FALSE)
  if (is.finite(usl) && tgt >= usl) return(FALSE)
  if (is.finite(lsl) && is.finite(usl) && lsl >= usl) return(FALSE)
  TRUE
}

#' Build a model bundle from a fit object and economics.
#' @keywords internal
opt_build_model_bundle <- function(fit, response_name, economics, overrides = NULL, resolution_ctx = NULL) {
  econ <- if (is.null(economics)) opt_economics_defaults() else economics
  b <- list(
    response_name = response_name,
    mean_model = fit$mean_mod,
    disp_model = fit$disp_mod,
    dat = fit$dat,
    disp_type = fit$disp_type,
    disp_intercept_only = isTRUE(fit$disp_intercept_only),
    rhs_lm_mean = fit$rhs_lm_mean,
    rhs_lm_disp = fit$rhs_lm_disp,
    optimizer_factor_names = fit$optimizer_factor_names,
    pool_disp = fit$pool_disp,
    disp_cell_factors = fit$disp_cell_factors,
    available_effects = fit$available_effects,
    target = econ$target,
    lsl = econ$lsl,
    usl = econ$usl,
    C_l = econ$C_l,
    C_u = econ$C_u
  )
  if (!is.null(resolution_ctx)) {
    b$resolution_ctx <- resolution_ctx
  }
  if (!is.null(overrides) && length(overrides) > 0L) {
    b <- apply_response_overrides(b, overrides)
  }
  b
}

#' Whether draft pooling differs from applied (pure helper for tests and readiness).
#' @keywords internal
mf_pool_store_pending_differs <- function(applied, draft) {
  ap <- unique(as.character(if (is.null(applied)) character(0) else applied))
  dr <- unique(as.character(if (is.null(draft)) character(0) else draft))
  !identical(sort(ap), sort(dr))
}

#' Sync economics from Shiny inputs into OptimizationSession.
#' @keywords internal
opt_session_sync_from_inputs <- function(session, dids, response_names, global_economics, per_response_economics = NULL) {
  if (is.null(session) || !is.list(session)) session <- opt_session_init()
  dids <- suppressWarnings(as.integer(dids))
  dids <- dids[is.finite(dids)]
  if (length(dids) < 1L) return(session)
  if (length(dids) == 1L) {
    return(opt_session_set_economics(session, as.character(dids[1]), global_economics))
  }
  if (is.null(per_response_economics) || length(per_response_economics) < 1L) {
    return(session)
  }
  for (i in seq_along(dids)) {
    rn <- if (i <= length(response_names)) response_names[[i]] else as.character(dids[[i]])
    if (!is.null(per_response_economics[[rn]])) {
      session <- opt_session_set_economics(session, as.character(dids[[i]]), per_response_economics[[rn]])
    }
  }
  session
}

#' Read user resolution delta stored in session for a response id.
#' @keywords internal
opt_session_get_resolution_delta <- function(session, response_key) {
  key <- as.character(response_key)
  br <- if (!is.null(session) && is.list(session$by_response)) session$by_response else list()
  stored <- br[[key]]
  if (!is.null(stored) && !is.null(stored$resolution_delta)) {
    val <- suppressWarnings(as.numeric(stored$resolution_delta))
    if (length(val) == 1L && is.finite(val)) return(val)
  }
  NULL
}

#' Write resolution delta for one response (returns updated session).
#' @keywords internal
opt_session_set_resolution_delta <- function(session, response_key, delta) {
  if (is.null(session) || !is.list(session)) session <- opt_session_init()
  key <- as.character(response_key)
  if (is.null(session$by_response)) session$by_response <- list()
  entry <- session$by_response[[key]]
  if (is.null(entry)) entry <- list()
  entry$resolution_delta <- suppressWarnings(as.numeric(delta))
  session$by_response[[key]] <- entry
  session
}

#' Economics from static Loss tab inputs.
#' @keywords internal
mf_economics_from_loss_inputs <- function(input) {
  opt_economics_from_scalars(
    target = input$loss_mf_target,
    lsl = input$loss_mf_lsl,
    usl = input$loss_mf_usl,
    C_l = input$loss_mf_C_l,
    C_u = input$loss_mf_C_u
  )
}

#' Persist active-response economics from static inputs into session.
#' @keywords internal
mf_sync_active_economics_to_session <- function(session, active_did, input) {
  if (is.null(active_did) || length(active_did) < 1L || !is.finite(suppressWarnings(as.numeric(active_did)[1L]))) {
    return(session)
  }
  did <- as.integer(active_did)[1L]
  opt_session_set_economics(session, as.character(did), mf_economics_from_loss_inputs(input))
}

#' Validate economics for all selected responses using session only.
#' @keywords internal
mf_validate_session_economics_all <- function(session, dids, resp_names) {
  if (is.null(dids) || length(dids) < 1L) return(character(0))
  misses <- character(0)
  for (i in seq_along(dids)) {
    econ <- opt_session_get_economics(session, as.character(dids[[i]]), global_fallback = NULL)
    rn <- if (i <= length(resp_names)) resp_names[[i]] else as.character(dids[[i]])
    if (!isTRUE(opt_economics_complete(econ))) {
      misses <- c(misses, paste0(rn, ": enter target, at least one spec limit, and C_l/C_u."))
      next
    }
    if (!isTRUE(opt_economics_valid_for_loss(econ))) {
      tgt <- suppressWarnings(as.numeric(econ$target))
      lsl <- suppressWarnings(as.numeric(econ$lsl))
      usl <- suppressWarnings(as.numeric(econ$usl))
      if (is.finite(lsl) && is.finite(tgt) && tgt <= lsl) {
        misses <- c(misses, paste0(rn, ": target (", signif(tgt, 5), ") must be greater than LSL (", signif(lsl, 5), ")."))
      } else if (is.finite(usl) && is.finite(tgt) && tgt >= usl) {
        misses <- c(misses, paste0(rn, ": target (", signif(tgt, 5), ") must be less than USL (", signif(usl, 5), ")."))
      } else {
        misses <- c(misses, paste0(rn, ": check target and spec limits (LSL must be less than USL)."))
      }
    }
  }
  unique(misses)
}

#' Cache key for registry fit lookup.
#' @keywords internal
mf_registry_cache_hit <- function(cached, sig_mean, sig_disp, fit_scope) {
  fit_scope <- match.arg(fit_scope, c("both", "means_only", "dispersion_only"))
  if (is.null(cached) || !isTRUE(cached$fit$ok)) {
    return(FALSE)
  }
  if (fit_scope == "means_only") {
    return(
      !is.null(cached$signature_mean) &&
        identical(cached$signature_mean, sig_mean) &&
        !is.null(cached$fit$mean_mod)
    )
  }
  if (fit_scope == "dispersion_only") {
    return(
      !is.null(cached$signature_disp) &&
        identical(cached$signature_disp, sig_disp) &&
        !is.null(cached$fit$disp_mod)
    )
  }
  !is.null(cached$signature_mean) && identical(cached$signature_mean, sig_mean) &&
    !is.null(cached$signature_disp) && identical(cached$signature_disp, sig_disp) &&
    !is.null(cached$fit$mean_mod) && !is.null(cached$fit$disp_mod)
}

#' Update registry entry after fit.
#' @keywords internal
mf_registry_store_fit <- function(cached, sig_mean, sig_disp, fit_scope, fit) {
  sm <- if (!is.null(cached)) cached$signature_mean else NULL
  sd <- if (!is.null(cached)) cached$signature_disp else NULL
  fit_scope <- match.arg(fit_scope, c("both", "means_only", "dispersion_only"))
  if (fit_scope == "means_only") {
    sm <- sig_mean
  } else if (fit_scope == "dispersion_only") {
    sd <- sig_disp
  } else {
    sm <- sig_mean
    sd <- sig_disp
  }
  merged <- fit
  if (!is.null(cached) && !is.null(cached$fit) && isTRUE(cached$fit$ok)) {
    prev <- cached$fit
    if (fit_scope == "means_only") {
      if (!is.null(fit$mean_mod)) merged$mean_mod <- fit$mean_mod
      if (!is.null(fit$rhs_lm_mean)) merged$rhs_lm_mean <- fit$rhs_lm_mean
      if (!is.null(fit$optimizer_factor_names)) {
        merged$optimizer_factor_names <- fit$optimizer_factor_names
      }
      if (is.null(merged$disp_mod) && !is.null(prev$disp_mod)) {
        merged$disp_mod <- prev$disp_mod
      }
      if (is.null(merged$rhs_lm_disp) && !is.null(prev$rhs_lm_disp)) {
        merged$rhs_lm_disp <- prev$rhs_lm_disp
      }
      if (is.null(merged$response_name) && !is.null(prev$response_name)) {
        merged$response_name <- prev$response_name
      }
      if (is.null(merged$factors_names) && !is.null(prev$factors_names)) {
        merged$factors_names <- prev$factors_names
      }
      if (is.null(merged$disp_type) && !is.null(prev$disp_type)) {
        merged$disp_type <- prev$disp_type
      }
      if (is.null(merged$disp_intercept_only) && !is.null(prev$disp_intercept_only)) {
        merged$disp_intercept_only <- prev$disp_intercept_only
      }
      if (!is.null(prev$disp_cell_factors)) {
        merged$disp_cell_factors <- prev$disp_cell_factors
      }
      if (!is.null(prev$available_effects)) {
        merged$available_effects <- prev$available_effects
      }
      if (!is.null(prev$pool_disp) && (is.null(merged$pool_disp) || !nzchar(paste(merged$pool_disp, collapse = "")))) {
        merged$pool_disp <- prev$pool_disp
      }
      if (!is.null(merged$disp_mod) && !is.null(merged$disp_cell_factors) &&
          length(merged$disp_cell_factors) > 0L && exists(".taguchi_ensure_disp_column", mode = "function")) {
        merged$dat <- .taguchi_ensure_disp_column(
          merged$dat,
          merged$response_name,
          merged$factors_names,
          disp_type = if (!is.null(merged$disp_type)) merged$disp_type else "ADM",
          pool_disp = if (!is.null(merged$pool_disp)) merged$pool_disp else character(0),
          available_effects = merged$available_effects,
          force = TRUE
        )
        if (exists(".taguchi_refit_disp_mod", mode = "function") && !is.null(merged$rhs_lm_disp)) {
          disp_ref <- .taguchi_refit_disp_mod(
            merged$dat,
            merged$rhs_lm_disp,
            merged$factors_names,
            disp_intercept_only = isTRUE(merged$disp_intercept_only)
          )
          if (!is.null(disp_ref)) merged$disp_mod <- disp_ref
        }
      }
    } else if (fit_scope == "dispersion_only") {
      if (!is.null(fit$disp_mod)) merged$disp_mod <- fit$disp_mod
      if (!is.null(fit$rhs_lm_disp)) merged$rhs_lm_disp <- fit$rhs_lm_disp
      if (!is.null(fit$disp_intercept_only)) {
        merged$disp_intercept_only <- fit$disp_intercept_only
      }
      if (!is.null(fit$disp_cell_factors)) {
        merged$disp_cell_factors <- fit$disp_cell_factors
      }
      if (!is.null(fit$available_effects)) {
        merged$available_effects <- fit$available_effects
      }
      if (!is.null(fit$pool_disp)) {
        merged$pool_disp <- fit$pool_disp
      }
      if (!is.null(prev$mean_mod)) merged$mean_mod <- prev$mean_mod
      if (!is.null(prev$rhs_lm_mean)) merged$rhs_lm_mean <- prev$rhs_lm_mean
      if (!is.null(prev$optimizer_factor_names)) {
        merged$optimizer_factor_names <- prev$optimizer_factor_names
      }
    } else {
      if (is.null(merged$mean_mod) && !is.null(prev$mean_mod)) {
        merged$mean_mod <- prev$mean_mod
      }
      if (is.null(merged$disp_mod) && !is.null(prev$disp_mod)) {
        merged$disp_mod <- prev$disp_mod
      }
      if (is.null(merged$rhs_lm_mean) && !is.null(prev$rhs_lm_mean)) {
        merged$rhs_lm_mean <- prev$rhs_lm_mean
      }
      if (is.null(merged$rhs_lm_disp) && !is.null(prev$rhs_lm_disp)) {
        merged$rhs_lm_disp <- prev$rhs_lm_disp
      }
      if (is.null(merged$optimizer_factor_names) && !is.null(prev$optimizer_factor_names)) {
        merged$optimizer_factor_names <- prev$optimizer_factor_names
      }
    }
    if (is.null(merged$pool_disp) && !is.null(prev$pool_disp)) {
      merged$pool_disp <- prev$pool_disp
    }
    if (is.null(merged$available_effects) && !is.null(prev$available_effects)) {
      merged$available_effects <- prev$available_effects
    }
  }
  list(signature_mean = sm, signature_disp = sd, fit = merged)
}
