# Multifactor optimization readiness helpers (pure functions + shared signature).
# Used by anova_module.R for Loss/optimization gating and model cache keys.

#' @keywords internal
mf_fit_signature <- function(
    did,
    fid,
    conf,
    ems_disp_type,
    ems_show_mixed_nest,
    f_r_types,
    pool,
    ems_disp,
    nrow_d = NA_integer_,
    ncol_d = NA_integer_) {
  paste(
    paste0("did=", as.integer(did)),
    paste0("fid=", paste(as.integer(fid), collapse = ",")),
    paste0("conf=", signif(as.numeric(conf), 8)),
    paste0("disp_type=", as.integer(ems_disp_type)),
    paste0("mixed=", as.integer(isTRUE(ems_show_mixed_nest))),
    paste0("fr=", paste(as.character(f_r_types), collapse = ",")),
    paste0("pool=", paste(sort(as.character(pool)), collapse = ",")),
    paste0("disp=", as.integer(isTRUE(ems_disp))),
    paste0("nr=", as.integer(nrow_d)),
    paste0("nc=", as.integer(ncol_d)),
  # Bump when reduced-model / registry commit semantics change (invalidates stale caches).
    paste0("rhs_rev=", 2L),
    sep = "|"
  )
}

#' Extract confidence from a serialized \code{mf_fit_signature} string.
#' @keywords internal
mf_conf_from_fit_signature <- function(sig) {
  if (is.null(sig) || length(sig) < 1L) {
    return(NA_real_)
  }
  sig <- as.character(sig)[1]
  if (!nzchar(sig)) {
    return(NA_real_)
  }
  m <- regmatches(sig, regexpr("conf=[^|]+", sig, perl = TRUE))
  if (length(m) < 1L || !nzchar(m[[1]])) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(sub("^conf=", "", m[[1]], perl = TRUE)))
}

#' Clear finalized means/dispersion signatures when confidence no longer matches commit.
#'
#' @return List with \code{changed} (logical) and \code{entry} (updated finalized list).
#' @keywords internal
mf_invalidate_finalized_on_conf_change <- function(finalized_entry, new_conf) {
  if (is.null(finalized_entry) || !is.list(finalized_entry)) {
    return(list(changed = FALSE, entry = finalized_entry))
  }
  new_conf <- suppressWarnings(as.numeric(new_conf))
  if (!is.finite(new_conf)) {
    return(list(changed = FALSE, entry = finalized_entry))
  }
  sig_refs <- c(finalized_entry$means_signature, finalized_entry$dispersion_signature)
  sig_refs <- sig_refs[!is.na(sig_refs) & nzchar(as.character(sig_refs))]
  if (length(sig_refs) < 1L) {
    return(list(changed = FALSE, entry = finalized_entry))
  }
  old_confs <- vapply(sig_refs, mf_conf_from_fit_signature, numeric(1))
  old_confs <- old_confs[is.finite(old_confs)]
  if (length(old_confs) < 1L) {
    return(list(changed = FALSE, entry = finalized_entry))
  }
  if (all(abs(old_confs - new_conf) < 1e-8)) {
    return(list(changed = FALSE, entry = finalized_entry))
  }
  finalized_entry$means_signature <- NULL
  finalized_entry$dispersion_signature <- NULL
  finalized_entry$means_finalized_at <- NULL
  finalized_entry$dispersion_finalized_at <- NULL
  list(changed = TRUE, entry = finalized_entry)
}

#' Per-response readiness for multifactor optimization (no Shiny dependencies).
#'
#' @param d Data frame (working data).
#' @param fid Integer vector of factor column indices.
#' @param dids Integer vector of response column indices to evaluate.
#' @param registry_by_response Named list: `key -> list(signature_mean=, signature_disp=, fit=)`.
#'   Mean and dispersion builds use different signature strings (`disp=` bit); both slots must
#'   match the current mean/disp pooling context for that response to be fit-ready on both tracks.
#' @param finalized_by_response Named list: `key -> list(means_signature=, dispersion_signature=)`.
#' @param ems_show_mixed_nest Logical.
#' @param f_r_types Character vector of F/R codes, length = length(fid).
#' @param get_state_fn Function `(did) -> list(ems_conf, ems_disp_type, ems_pool_means, ems_pool_disp)`.
#' @keywords internal
mf_model_readiness_compute <- function(
    d,
    fid,
    dids,
    registry_by_response,
    finalized_by_response,
    ems_show_mixed_nest,
    f_r_types,
    get_state_fn) {
  out <- vector("list", length(dids))
  names(out) <- as.character(dids)
  nr <- if (is.data.frame(d)) nrow(d) else NA_integer_
  nc <- if (is.data.frame(d)) ncol(d) else NA_integer_
  for (did_i in dids) {
    st_i <- get_state_fn(did_i)
    conf_i <- suppressWarnings(as.numeric(if (!is.null(st_i$ems_conf)) st_i$ems_conf else 0.95))
    if (!is.finite(conf_i)) conf_i <- 0.95
    ems_disp_type_i <- suppressWarnings(as.integer(st_i$ems_disp_type)[1])
    if (length(ems_disp_type_i) < 1L || is.na(ems_disp_type_i)) ems_disp_type_i <- 1L
    pool_mean <- st_i$ems_pool_means
    pool_disp <- st_i$ems_pool_disp
    key_i <- as.character(as.integer(did_i))
    cached_i <- registry_by_response[[key_i]]
    finalized_i <- finalized_by_response[[key_i]]
    if (is.null(finalized_i)) finalized_i <- list()

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

    mean_fit_ready <- !is.null(cached_i) && !is.null(cached_i$signature_mean) &&
      isTRUE(cached_i$fit$ok) && !is.null(cached_i$fit$mean_mod)
    disp_fit_ready <- !is.null(cached_i) && !is.null(cached_i$signature_disp) &&
      isTRUE(cached_i$fit$ok) && !is.null(cached_i$fit$disp_mod)
    mean_finalized <- !is.null(finalized_i$means_signature) && !is.null(cached_i$signature_mean) &&
      identical(finalized_i$means_signature, cached_i$signature_mean)
    disp_finalized <- !is.null(finalized_i$dispersion_signature) && !is.null(cached_i$signature_disp) &&
      identical(finalized_i$dispersion_signature, cached_i$signature_disp)
    mean_live_current <- !is.null(cached_i) && !is.null(cached_i$signature_mean) &&
      identical(cached_i$signature_mean, sig_mean)
    disp_live_current <- !is.null(cached_i) && !is.null(cached_i$signature_disp) &&
      identical(cached_i$signature_disp, sig_disp)

    mean_ready <- isTRUE(mean_fit_ready) && isTRUE(mean_finalized)
    disp_ready <- isTRUE(disp_fit_ready) && isTRUE(disp_finalized)
    missing <- character(0)
    if (!isTRUE(mean_ready)) missing <- c(missing, "Means model not finalized/current")
    if (!isTRUE(disp_ready)) missing <- c(missing, "Dispersion model not finalized/current")
    out[[key_i]] <- list(
      response = if (did_i >= 1L && did_i <= ncol(d)) names(d)[did_i] else as.character(did_i),
      mean_ready = mean_ready,
      disp_ready = disp_ready,
      mean_fit_ready = mean_fit_ready,
      disp_fit_ready = disp_fit_ready,
      mean_finalized = mean_finalized,
      disp_finalized = disp_finalized,
      mean_live_current = mean_live_current,
      disp_live_current = disp_live_current,
      missing = missing
    )
  }
  out
}
