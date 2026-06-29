# Three-tier dispersion resolution: EMM -> observed cell metric -> resolution delta (delta).

.dispersion_resolution_cache <- new.env(parent = emptyenv())

#' Minimum positive difference among unique finite response values (gauge resolution).
#' @keywords internal
infer_response_delta_min <- function(y) {
  y <- suppressWarnings(as.numeric(y))
  y <- y[is.finite(y)]
  y <- sort(unique(y))
  if (length(y) < 2L) return(NA_real_)
  d <- diff(y)
  d <- d[d > 0]
  if (length(d) < 1L) return(NA_real_)
  min(d)
}

#' Quantile of range (max - min) for n i.i.d. standard normals at probability prob.
#' @keywords internal
.standard_normal_range_quantile <- function(n, prob, nsim = 80000L) {
  n <- as.integer(n)[1L]
  if (!is.finite(n) || n < 2L) return(NA_real_)
  if (!is.finite(prob) || prob <= 0 || prob >= 1) return(NA_real_)
  key <- paste(n, prob, nsim, sep = "|")
  if (!is.null(.dispersion_resolution_cache[[key]])) {
    return(.dispersion_resolution_cache[[key]])
  }
  set.seed(20260315L + n)
  mat <- matrix(stats::rnorm(n * nsim), nrow = n, ncol = nsim)
  ranges <- apply(mat, 2, function(x) diff(range(x)))
  q <- as.numeric(stats::quantile(ranges, probs = prob, type = 7, names = FALSE))
  .dispersion_resolution_cache[[key]] <- q
  q
}

#' Recommend resolution delta from Normal range bound P(max - min < delta_min) = confidence.
#' @keywords internal
recommend_resolution_delta <- function(delta_min, n, confidence = 0.95, nsim = 80000L) {
  delta_min <- suppressWarnings(as.numeric(delta_min))[1]
  n <- as.integer(suppressWarnings(as.numeric(n))[1])
  confidence <- suppressWarnings(as.numeric(confidence))[1]
  if (!is.finite(delta_min) || delta_min <= 0) {
    return(list(
      ok = FALSE,
      message = "Delta_min is not available (need at least two distinct response values).",
      sigma_bound = NA_real_,
      delta_metric = NA_real_,
      delta_min = delta_min,
      n = n,
      confidence = confidence
    ))
  }
  if (!is.finite(n) || n < 2L) n <- 2L
  q_std <- .standard_normal_range_quantile(n, confidence, nsim = nsim)
  if (!is.finite(q_std) || q_std <= 0) {
    return(list(
      ok = FALSE,
      message = "Could not compute standard-normal range quantile.",
      sigma_bound = NA_real_,
      delta_metric = NA_real_,
      delta_min = delta_min,
      n = n,
      confidence = confidence
    ))
  }
  sigma_bound <- delta_min / q_std
  delta_metric <- sigma_bound * sqrt(2 / pi)
  list(
    ok = TRUE,
    message = "",
    sigma_bound = sigma_bound,
    delta_metric = delta_metric,
    delta_min = delta_min,
    n = n,
    confidence = confidence
  )
}

#' Stable key for a factorial cell from factor names and level values.
#' @keywords internal
dispersion_cell_key <- function(cell_factors, factor_values) {
  cell_factors <- as.character(cell_factors)
  vals <- vapply(cell_factors, function(fn) {
    if (is.list(factor_values) && !is.null(factor_values[[fn]])) {
      as.character(factor_values[[fn]])[1L]
    } else if (fn %in% names(factor_values)) {
      as.character(factor_values[[fn]])[1L]
    } else {
      ""
    }
  }, character(1))
  paste(paste(cell_factors, vals, sep = "="), collapse = "|")
}

#' Observed within-cell dispersion metrics keyed by factorial cell.
#' @keywords internal
build_cell_dispersion_lookup <- function(dat, cell_factors) {
  if (is.null(dat) || !is.data.frame(dat) || nrow(dat) < 1L || length(cell_factors) < 1L) {
    return(list())
  }
  cell_factors <- as.character(cell_factors)
  if (!".taguchi_disp" %in% names(dat)) return(list())
  miss <- setdiff(cell_factors, names(dat))
  if (length(miss) > 0L) return(list())

  d <- dat[, c(cell_factors, ".taguchi_disp"), drop = FALSE]
  for (fn in cell_factors) {
    d[[fn]] <- as.character(d[[fn]])
  }
  ok_rows <- stats::complete.cases(d[, cell_factors, drop = FALSE])
  d <- d[ok_rows, , drop = FALSE]
  if (nrow(d) < 1L) return(list())

  u <- unique(d[, cell_factors, drop = FALSE])
  out <- list()
  for (i in seq_len(nrow(u))) {
    row <- u[i, , drop = FALSE]
    mask <- rep(TRUE, nrow(d))
    for (fn in cell_factors) {
      mask <- mask & (d[[fn]] == as.character(row[[fn]]))
    }
    sub <- d[mask, , drop = FALSE]
    key <- dispersion_cell_key(cell_factors, as.list(row[1, , drop = FALSE]))
    out[[key]] <- list(
      disp_metric = suppressWarnings(as.numeric(sub$.taguchi_disp[1L])),
      n_obs = nrow(sub)
    )
  }
  out
}

#' Resolve effective dispersion metric via three-tier stack.
#' @keywords internal
resolve_dispersion_metric <- function(emm, factor_values, ctx) {
  emm_raw <- suppressWarnings(as.numeric(emm))[1]
  if (is.null(ctx) || length(ctx$cell_factors) < 1L) {
    eff <- if (is.finite(emm_raw) && emm_raw > 0) emm_raw else NA_real_
    return(list(
      effective = eff,
      tier = if (is.finite(eff)) 1L else NA_integer_,
      emm_raw = emm_raw,
      cell_raw = NA_real_,
      delta_used = NA_real_,
      audit_label = if (is.finite(eff)) "EMM" else NA_character_
    ))
  }

  cell_key <- dispersion_cell_key(ctx$cell_factors, factor_values)
  cell_entry <- ctx$cell_lookup[[cell_key]]
  cell_run <- !is.null(cell_entry)
  cell_raw <- if (cell_run) suppressWarnings(as.numeric(cell_entry$disp_metric)) else NA_real_
  n_cell <- if (cell_run && is.finite(cell_entry$n_obs)) as.integer(cell_entry$n_obs) else ctx$n_mean

  if (is.finite(emm_raw) && emm_raw > 0) {
    return(list(
      effective = emm_raw,
      tier = 1L,
      emm_raw = emm_raw,
      cell_raw = if (cell_run) cell_raw else NA_real_,
      delta_used = NA_real_,
      audit_label = "EMM"
    ))
  }

  if (cell_run && is.finite(cell_raw) && cell_raw > 0) {
    return(list(
      effective = cell_raw,
      tier = 2L,
      emm_raw = emm_raw,
      cell_raw = cell_raw,
      delta_used = NA_real_,
      audit_label = "Cell observed"
    ))
  }

  delta_used <- suppressWarnings(as.numeric(ctx$delta_user))[1]
  if (!is.finite(delta_used) || delta_used <= 0) {
    dr <- recommend_resolution_delta(ctx$delta_min, n = n_cell, confidence = ctx$confidence)
    delta_used <- dr$delta_metric
  }

  list(
    effective = if (is.finite(delta_used) && delta_used > 0) delta_used else NA_real_,
    tier = 3L,
    emm_raw = emm_raw,
    cell_raw = if (cell_run) cell_raw else NA_real_,
    delta_used = delta_used,
    audit_label = "Resolution delta"
  )
}

#' Bundle resolution inputs for one response.
#' @keywords internal
build_resolution_context <- function(
    dat,
    response_name,
    cell_factors,
    disp_type = c("ADA", "ADM", "ADMn1"),
    confidence = 0.95,
    delta_user = NULL) {
  disp_type <- match.arg(disp_type)
  cell_factors <- as.character(cell_factors)
  y <- if (!is.null(response_name) && response_name %in% names(dat)) dat[[response_name]] else NA
  delta_min <- infer_response_delta_min(y)

  n_mean <- NA_real_
  if (length(cell_factors) >= 1L && all(cell_factors %in% names(dat))) {
    rep_info <- factorial_cell_replication(dat, cell_factors)
    if (!is.null(rep_info$count_per_cell) && nrow(rep_info$count_per_cell) > 0L) {
      n_mean <- mean(rep_info$count_per_cell$count)
    }
  }
  if (!is.finite(n_mean) || n_mean < 2L) n_mean <- 2L

  dr <- recommend_resolution_delta(delta_min, n = n_mean, confidence = confidence)
  delta_rec <- dr$delta_metric
  delta_eff <- suppressWarnings(as.numeric(delta_user))[1]
  if (!is.finite(delta_eff) || delta_eff <= 0) {
    delta_eff <- delta_rec
  }

  list(
    delta_min = delta_min,
    n_mean = n_mean,
    confidence = confidence,
    delta_recommended = delta_rec,
    sigma_recommended = dr$sigma_bound,
    delta_user = delta_eff,
    cell_factors = cell_factors,
    cell_lookup = build_cell_dispersion_lookup(dat, cell_factors),
    disp_type = disp_type,
    recommend_ok = isTRUE(dr$ok)
  )
}

#' Resolve dispersion at a newdata row and convert to sigma.
#' @keywords internal
resolve_dispersion_for_newdata <- function(emm, newdata, ctx) {
  if (is.null(ctx)) {
    eff <- suppressWarnings(as.numeric(emm))[1]
    sg <- dispersion_metric_to_sigma(eff, type = "ADA")
    return(list(
      effective = eff,
      emm_raw = eff,
      cell_raw = NA_real_,
      delta_used = NA_real_,
      tier = NA_integer_,
      audit_label = "EMM",
      sigma = sg
    ))
  }
  cell_facs <- as.character(ctx$cell_factors)
  if (length(cell_facs) < 1L || !all(cell_facs %in% names(newdata))) {
    eff <- suppressWarnings(as.numeric(emm))[1]
    sg <- dispersion_metric_to_sigma(eff, type = if (!is.null(ctx$disp_type)) ctx$disp_type else "ADA")
    return(list(
      effective = eff,
      emm_raw = eff,
      cell_raw = NA_real_,
      delta_used = NA_real_,
      tier = NA_integer_,
      audit_label = "EMM",
      sigma = sg
    ))
  }
  fv <- as.list(newdata[1, cell_facs, drop = FALSE])
  for (fn in cell_facs) {
    if (fn %in% names(fv)) fv[[fn]] <- as.character(fv[[fn]])
  }
  res <- resolve_dispersion_metric(emm, fv, ctx)
  res$sigma <- dispersion_metric_to_sigma(res$effective, type = ctx$disp_type)
  res
}

#' Apply three-tier resolution to a loss-grid table (columns disp_pred = EMM retained).
#' @keywords internal
apply_dispersion_resolution_to_grid <- function(grid_table, ctx) {
  if (is.null(grid_table) || nrow(grid_table) < 1L || is.null(ctx)) {
    return(grid_table)
  }
  cell_factors <- ctx$cell_factors
  if (length(cell_factors) < 1L || !all(cell_factors %in% names(grid_table))) {
    return(grid_table)
  }
  n <- nrow(grid_table)
  disp_emm <- disp_cell <- disp_effective <- delta_used <- rep(NA_real_, n)
  disp_tier <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    row_list <- as.list(grid_table[i, cell_factors, drop = FALSE])
    for (fn in cell_factors) row_list[[fn]] <- as.character(row_list[[fn]])
    res <- resolve_dispersion_metric(grid_table$disp_pred[i], row_list, ctx)
    disp_emm[i] <- res$emm_raw
    disp_cell[i] <- res$cell_raw
    disp_effective[i] <- res$effective
    delta_used[i] <- res$delta_used
    disp_tier[i] <- res$audit_label
  }
  grid_table$disp_emm <- disp_emm
  grid_table$disp_cell <- disp_cell
  grid_table$disp_effective <- disp_effective
  grid_table$disp_tier <- disp_tier
  grid_table$delta_used <- delta_used
  grid_table
}

#' Whether tier 1/2 fail so gauge-resolution delta (policy 3) is required.
#' @keywords internal
would_use_resolution_delta <- function(emm, factor_values, ctx) {
  if (is.null(ctx) || length(ctx$cell_factors) < 1L) {
    return(FALSE)
  }
  emm_raw <- suppressWarnings(as.numeric(emm))[1]
  if (is.finite(emm_raw) && emm_raw > 0) {
    return(FALSE)
  }
  cell_key <- dispersion_cell_key(ctx$cell_factors, factor_values)
  cell_entry <- ctx$cell_lookup[[cell_key]]
  cell_raw <- if (!is.null(cell_entry)) suppressWarnings(as.numeric(cell_entry$disp_metric)) else NA_real_
  if (!is.null(cell_entry) && is.finite(cell_raw) && cell_raw > 0) {
    return(FALSE)
  }
  TRUE
}

#' Summarize dispersion policy tiers used on a loss grid.
#' @keywords internal
summarize_dispersion_policy <- function(grid_table) {
  if (is.null(grid_table) || !is.data.frame(grid_table) || nrow(grid_table) < 1L ||
      !"disp_tier" %in% names(grid_table)) {
    return(list(
      ok = FALSE,
      n_settings = 0L,
      tier_counts = list(),
      uses_tier3 = FALSE,
      policy_code = NA_integer_,
      policy_label = NA_character_,
      message = "Dispersion policy not available until the loss grid is computed."
    ))
  }
  tiers <- as.character(grid_table$disp_tier)
  tiers <- tiers[!is.na(tiers) & nzchar(tiers)]
  if (length(tiers) < 1L) {
    return(list(
      ok = FALSE,
      n_settings = nrow(grid_table),
      tier_counts = list(),
      uses_tier3 = FALSE,
      policy_code = NA_integer_,
      policy_label = NA_character_,
      message = "No dispersion tier information on the loss grid."
    ))
  }
  counts <- table(tiers)
  tier_counts <- as.list(as.integer(counts))
  names(tier_counts) <- names(counts)
  uses_tier3 <- "Resolution delta" %in% names(tier_counts)
  tier3_count <- if (uses_tier3) as.integer(tier_counts[["Resolution delta"]]) else 0L
  unique_tiers <- names(tier_counts)
  if (length(unique_tiers) == 1L) {
    policy_label <- unique_tiers[[1L]]
    policy_code <- switch(
      policy_label,
      "EMM" = 1L,
      "Cell observed" = 2L,
      "Resolution delta" = 3L,
      NA_integer_
    )
    message <- dispersion_policy_description(policy_code)
  } else {
    policy_label <- "Mixed"
    policy_code <- NA_integer_
    message <- dispersion_policy_mixed_description(tier_counts, nrow(grid_table))
  }
  list(
    ok = TRUE,
    n_settings = nrow(grid_table),
    tier_counts = tier_counts,
    uses_tier3 = uses_tier3,
    tier3_count = tier3_count,
    policy_code = policy_code,
    policy_label = policy_label,
    message = message
  )
}

#' Short description for a single dispersion policy code (1–3).
#' @keywords internal
dispersion_policy_description <- function(policy_code) {
  pc <- as.integer(policy_code)[1L]
  if (pc == 1L) {
    return("Policy 1 — Dispersion EMM: sigma from the reduced-model dispersion prediction.")
  }
  if (pc == 2L) {
    return("Policy 2 — Observed cell: sigma from within-cell ADA/ADM/ADMn1 for run factorial cells.")
  }
  if (pc == 3L) {
    return("Policy 3 — Gauge resolution prior: sigma from estimated sub-resolution delta.")
  }
  "Dispersion policy could not be determined."
}

#' Description when multiple dispersion policies appear on the loss grid.
#' @keywords internal
dispersion_policy_mixed_description <- function(tier_counts, n_settings) {
  parts <- character(0)
  if (!is.null(tier_counts[["EMM"]])) {
    parts <- c(parts, paste0("Policy 1 (EMM): ", tier_counts[["EMM"]], " setting(s)"))
  }
  if (!is.null(tier_counts[["Cell observed"]])) {
    parts <- c(parts, paste0("Policy 2 (cell): ", tier_counts[["Cell observed"]], " setting(s)"))
  }
  if (!is.null(tier_counts[["Resolution delta"]])) {
    parts <- c(parts, paste0("Policy 3 (resolution delta): ", tier_counts[["Resolution delta"]], " setting(s)"))
  }
  paste0(
    "Mixed dispersion policies across ", n_settings, " observed setting(s) — ",
    paste(parts, collapse = "; "),
    "."
  )
}

#' One-line policy message for a response on the Loss tab.
#' @keywords internal
format_response_dispersion_policy_line <- function(response_name, summary, metric_label = NULL) {
  rn <- as.character(response_name)[1L]
  metric_part <- if (!is.null(metric_label) && nzchar(as.character(metric_label)[1L])) {
    paste0(" [", metric_label, "]")
  } else {
    ""
  }
  if (is.null(summary) || !isTRUE(summary$ok)) {
    msg <- if (!is.null(summary$message)) as.character(summary$message) else "Dispersion policy unavailable."
    return(paste0(rn, metric_part, ": ", msg))
  }
  paste0(rn, metric_part, ": ", summary$message)
}

#' Read user resolution delta from Shiny inputs (per response).
#' @keywords internal
mf_get_resolution_delta_user <- function(input, response_name, input_prefix = "loss_mf_disp_delta__") {
  if (is.null(input) || is.null(response_name)) return(NULL)
  safe <- gsub("[^A-Za-z0-9_]", "_", as.character(response_name))
  key <- paste0(input_prefix, safe)
  if (is.null(input[[key]])) return(NULL)
  suppressWarnings(as.numeric(input[[key]]))
}
