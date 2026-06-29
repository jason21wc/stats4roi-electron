# Pure helpers for multifactor Taguchi loss / optimization (no Shiny dependencies).

.taguchi_opt_load_deps <- function() {
  if (exists(".taguchi_predict_point", mode = "function")) return(invisible(NULL))
  dirs <- character(0)
  of <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(of) && nzchar(of)) {
    dirs <- c(dirs, normalizePath(
      file.path(dirname(normalizePath(of, winslash = "/", mustWork = FALSE)), "..", "..", "utils", "optimization"),
      winslash = "/",
      mustWork = FALSE
    ))
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
  for (opt_dir_local in unique(dirs[nzchar(dirs)])) {
    ah <- normalizePath(file.path(opt_dir_local, "..", "anova_helpers.R"), winslash = "/", mustWork = FALSE)
    if (file.exists(ah) && !exists("factorial_cell_replication", mode = "function")) {
      source(ah, local = FALSE)
    }
    for (fn in c("dispersion_metric_sigma.R", "dispersion_resolution_prior.R", "taguchi_loss_mvp.R")) {
      dep <- file.path(opt_dir_local, fn)
      if (file.exists(dep)) source(dep, local = FALSE)
    }
    if (exists(".taguchi_predict_point", mode = "function")) break
  }
  invisible(NULL)
}
.taguchi_opt_load_deps()

#' @keywords internal
parse_factor_level_costs <- function(txt, valid_factors = character(0)) {
  rows <- list()
  valid_factors <- unique(as.character(valid_factors))
  valid_factors <- valid_factors[!is.na(valid_factors) & nzchar(valid_factors)]
  if (is.null(txt) || length(txt) < 1L) {
    return(data.frame(factor = character(0), level = numeric(0), cost = numeric(0), stringsAsFactors = FALSE))
  }
  txt <- as.character(txt)[1]
  if (!nzchar(trimws(txt))) {
    return(data.frame(factor = character(0), level = numeric(0), cost = numeric(0), stringsAsFactors = FALSE))
  }
  entries <- unlist(strsplit(as.character(txt), ";", fixed = TRUE))
  entries <- trimws(entries)
  entries <- entries[nzchar(entries)]
  for (e in entries) {
    parts <- unlist(strsplit(e, "=", fixed = TRUE))
    if (length(parts) != 2L) next
    key <- trimws(parts[[1]])
    val <- suppressWarnings(as.numeric(trimws(parts[[2]])))
    if (!is.finite(val)) next
    kv <- unlist(strsplit(key, ":", fixed = TRUE))
    if (length(kv) != 2L) next
    fac <- trimws(kv[[1]])
    lev <- trimws(kv[[2]])
    if (nzchar(fac) && nzchar(lev)) {
      if (length(valid_factors) > 0L && !fac %in% valid_factors) next
      lev_num <- suppressWarnings(as.numeric(lev))
      if (!is.finite(lev_num)) next
      rows[[length(rows) + 1L]] <- data.frame(
        factor = fac,
        level = lev_num,
        cost = val,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) < 1L) {
    return(data.frame(factor = character(0), level = numeric(0), cost = numeric(0), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  out <- out[order(out$factor, out$level), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' @keywords internal
make_unit_setting_cost_fn <- function(cost_table, factor_kinds = NULL) {
  ct <- cost_table
  if (!is.null(ct) && nrow(ct) > 0L) {
    ct$factor <- as.character(ct$factor)
  }
  by_factor <- if (!is.null(ct) && nrow(ct) > 0L) split(ct, ct$factor) else list()
  fk <- if (is.null(factor_kinds)) setNames(character(0), character(0)) else factor_kinds
  function(nd) {
    if (is.null(nd) || nrow(nd) < 1L || length(by_factor) < 1L) return(0)
    row <- nd[1, , drop = FALSE]
    rownms <- names(row)
    total <- 0
    .resolve_row_col <- function(bnm) {
      if (length(bnm) != 1L || !nzchar(as.character(bnm)[1])) return(NA_character_)
      bnm <- as.character(bnm)[1]
      if (bnm %in% rownms) return(bnm)
      hit <- rownms[tolower(rownms) == tolower(bnm)]
      if (length(hit) == 1L) return(hit[1L])
      NA_character_
    }
    for (bnm in names(by_factor)) {
      spec <- by_factor[[bnm]]
      if (is.null(spec) || nrow(spec) < 1L) next
      nm <- .resolve_row_col(bnm)
      if (is.na(nm)) next
      x <- suppressWarnings(as.numeric(as.character(row[[nm]][1])))
      if (!is.finite(x)) next
      mode <- if (!is.null(fk[[nm]]) && identical(as.character(fk[[nm]]), "continuous")) {
        "continuous"
      } else if (!is.null(fk[[bnm]]) && identical(as.character(fk[[bnm]]), "continuous")) {
        "continuous"
      } else {
        "discrete"
      }
      lv <- suppressWarnings(as.numeric(spec$level))
      cv <- suppressWarnings(as.numeric(spec$cost))
      ok <- is.finite(lv) & is.finite(cv)
      lv <- lv[ok]
      cv <- cv[ok]
      if (length(lv) < 1L) next
      if (identical(mode, "continuous")) {
        ord <- order(lv)
        lv <- lv[ord]
        cv <- cv[ord]
        if (length(unique(lv)) == 1L) {
          total <- total + cv[1]
        } else {
          total <- total + as.numeric(stats::approx(x = lv, y = cv, xout = x, method = "linear", ties = "ordered", rule = 2)$y)
        }
      } else {
        hit <- which(abs(lv - x) < 1e-9)
        if (length(hit) > 0L) {
          total <- total + cv[hit[1]]
        }
      }
    }
    if (!is.finite(total)) total <- 0
    total
  }
}

#' @keywords internal
parse_response_overrides <- function(txt, valid_responses = character(0)) {
  out <- list()
  issues <- character(0)
  if (is.null(txt) || !nzchar(trimws(txt))) return(list(overrides = out, issues = issues))
  valid_lower <- tolower(valid_responses)
  dup_lower <- unique(valid_lower[duplicated(valid_lower)])
  if (length(dup_lower) > 0L) {
    issues <- c(
      issues,
      paste0(
        "Ambiguous selected response names under case-insensitive matching: ",
        paste(dup_lower, collapse = ", "),
        ". Use exact response names."
      )
    )
  }
  valid_lookup <- setNames(valid_responses, valid_lower)
  allowed_keys <- c(
    "target", "lsl", "usl", "c_l", "cl", "c_u", "cu",
    "response_type", "n_trials", "trials", "size"
  )
  allowed_types <- c("normal", "poisson", "binomial", "proportion")
  entries <- unlist(strsplit(as.character(txt), ";", fixed = TRUE))
  entries <- trimws(entries)
  entries <- entries[nzchar(entries)]
  for (e in entries) {
    parts <- unlist(strsplit(e, ":", fixed = TRUE))
    if (length(parts) < 2L) {
      issues <- c(issues, paste0("Malformed override entry: '", e, "'"))
      next
    }
    resp <- trimws(parts[[1]])
    if (!nzchar(resp)) {
      issues <- c(issues, paste0("Missing response name in override entry: '", e, "'"))
      next
    }
    resp_key <- tolower(resp)
    if (length(valid_responses) > 0L) {
      if (!(resp_key %in% names(valid_lookup))) {
        issues <- c(issues, paste0("Unknown response in overrides: '", resp, "'"))
        next
      }
      resp <- unname(valid_lookup[[resp_key]])
    }
    spec <- paste(parts[-1], collapse = ":")
    kvs <- unlist(strsplit(spec, ",", fixed = TRUE))
    kvs <- trimws(kvs)
    kvs <- kvs[nzchar(kvs)]
    ov <- list()
    for (kv in kvs) {
      p <- unlist(strsplit(kv, "=", fixed = TRUE))
      if (length(p) != 2L) {
        issues <- c(issues, paste0("Malformed key/value override '", kv, "' for response '", resp, "'"))
        next
      }
      k <- trimws(tolower(p[[1]]))
      v <- trimws(p[[2]])
      if (!nzchar(k)) next
      if (!(k %in% allowed_keys)) {
        issues <- c(issues, paste0("Unknown override key '", k, "' for response '", resp, "'"))
        next
      }
      if (k %in% c("response_type")) {
        typ <- tolower(v)
        if (!(typ %in% allowed_types)) {
          issues <- c(issues, paste0("Invalid response_type '", v, "' for response '", resp, "'"))
          next
        }
        ov[[k]] <- typ
      } else {
        num <- suppressWarnings(as.numeric(v))
        if (!is.finite(num)) {
          issues <- c(issues, paste0("Non-numeric value '", v, "' for key '", k, "' in response '", resp, "'"))
          next
        }
        ov[[k]] <- num
      }
    }
    if (nzchar(resp) && length(ov) > 0L) out[[resp]] <- ov
  }
  list(overrides = out, issues = unique(issues))
}

#' @keywords internal
apply_response_overrides <- function(bundle, overrides_for_response) {
  if (length(overrides_for_response) < 1L) return(bundle)
  key_map <- c(
    "target" = "target",
    "lsl" = "lsl",
    "usl" = "usl",
    "c_l" = "C_l",
    "cl" = "C_l",
    "c_u" = "C_u",
    "cu" = "C_u",
    "response_type" = "response_type",
    "n_trials" = "n_trials",
    "trials" = "n_trials",
    "size" = "n_trials"
  )
  for (k in names(overrides_for_response)) {
    dest <- key_map[[k]]
    if (!is.null(dest) && nzchar(dest)) bundle[[dest]] <- overrides_for_response[[k]]
  }
  bundle
}

#' @keywords internal
refit_model_with_data <- function(mod, dat_local) {
  if (inherits(mod, "glm")) {
    fam <- tryCatch(mod$family, error = function(e) NULL)
    if (is.null(fam)) stop("Missing GLM family for model refit.", call. = FALSE)
    return(stats::glm(stats::formula(mod), data = dat_local, family = fam))
  }
  stats::lm(stats::formula(mod), data = dat_local)
}

#' Piecewise-linear interpolation between adjacent observed coded design levels.
#' @keywords internal
.multilinear_corner_interp <- function(x, lo, hi, corner_values) {
  k <- length(lo)
  n_vert <- 2L^k
  if (length(x) != k || length(hi) != k || length(corner_values) != n_vert) {
    return(NA_real_)
  }
  pred <- 0
  for (i in seq_len(n_vert)) {
    w <- 1
    for (j in seq_len(k)) {
      use_hi <- bitwAnd(as.integer(i - 1L), bitwShiftL(1L, j - 1L)) != 0L
      denom <- hi[j] - lo[j]
      if (!is.finite(denom) || abs(denom) < .Machine$double.eps) {
        w <- if (isTRUE(use_hi) && abs(x[j] - hi[j]) < .Machine$double.eps) 1 else 0
        break
      }
      if (isTRUE(use_hi)) {
        w <- w * (x[j] - lo[j]) / denom
      } else {
        w <- w * (hi[j] - x[j]) / denom
      }
    }
    pred <- pred + w * corner_values[i]
  }
  pred
}

#' @keywords internal
.interp_linear_codes <- function(codes, values, x) {
  stats::approx(
    x = codes,
    y = values,
    xout = suppressWarnings(as.numeric(x)),
    method = "linear",
    rule = 2,
    ties = "ordered"
  )$y
}

#' Observed coded design levels for a factor in a fitted factorial model.
#' @keywords internal
factor_coded_design_levels <- function(mean_mod, dat, vn) {
  codes <- numeric(0)
  if (!is.null(dat) && vn %in% names(dat)) {
    codes <- sort(unique(suppressWarnings(as.numeric(as.character(dat[[vn]])))))
    codes <- codes[is.finite(codes)]
  }
  if (length(codes) < 1L) {
    xl <- mean_mod$xlevels[[vn]]
    if (!is.null(xl)) {
      codes <- sort(unique(suppressWarnings(as.numeric(as.character(xl)))))
      codes <- codes[is.finite(codes)]
    }
  }
  codes
}

#' @keywords internal
.build_newdata_factor_setting <- function(setting, mean_mod) {
  vars <- names(setting)
  nd <- as.data.frame(as.list(setNames(rep(NA, length(vars)), vars)), stringsAsFactors = FALSE)
  for (vn in vars) {
    xl <- mean_mod$xlevels[[vn]]
    val <- as.character(setting[[vn]])
    if (!is.null(xl) && length(xl) > 0L) {
      nd[[vn]] <- factor(val, levels = xl)
    } else {
      nd[[vn]] <- suppressWarnings(as.numeric(val))
    }
  }
  nd
}

#' @keywords internal
.extract_disc_assignment <- function(nd, disc_vars) {
  disc_vars <- unique(as.character(disc_vars))
  disc_vars <- disc_vars[nzchar(disc_vars)]
  if (length(disc_vars) < 1L) return(list())
  out <- list()
  for (dv in disc_vars) {
    if (dv %in% names(nd)) {
      out[[dv]] <- as.character(nd[[dv]])[1L]
    }
  }
  out
}

#' Model predictions at corners of a coded hyper-rectangle (factor-model path).
#' Only dimensions where lo != hi contribute corners (2^k active, not full factorial).
#' @keywords internal
factor_model_corner_metrics <- function(mean_mod, disp_mod, cont_vars, lo_vals, hi_vals, disc_assignment, resolution_ctx = NULL) {
  cont_vars <- as.character(cont_vars)
  active <- cont_vars[vapply(cont_vars, function(vn) {
    !identical(as.character(lo_vals[[vn]]), as.character(hi_vals[[vn]]))
  }, logical(1))]
  setting <- as.list(disc_assignment)
  for (vn in cont_vars) {
    setting[[vn]] <- as.character(lo_vals[[vn]])
  }
  if (length(active) < 1L) {
    nd <- .build_newdata_factor_setting(setting, mean_mod)
    mu <- .taguchi_predict_point(mean_mod, nd)
    disp <- .taguchi_predict_point(disp_mod, nd)
    if (!is.null(resolution_ctx)) {
      disp <- resolve_dispersion_for_newdata(disp, nd, resolution_ctx)$effective
    }
    return(list(mu = mu, disp = disp))
  }
  k <- length(active)
  n_vert <- 2L^k
  mu_corners <- disp_corners <- rep(NA_real_, n_vert)
  for (i in seq_len(n_vert)) {
    corner_setting <- setting
    for (j in seq_len(k)) {
      vn <- active[j]
      use_hi <- bitwAnd(as.integer(i - 1L), bitwShiftL(1L, j - 1L)) != 0L
      corner_setting[[vn]] <- as.character(if (isTRUE(use_hi)) hi_vals[[vn]] else lo_vals[[vn]])
    }
    nd <- .build_newdata_factor_setting(corner_setting, mean_mod)
    mu_corners[i] <- .taguchi_predict_point(mean_mod, nd)
    disp_emm <- .taguchi_predict_point(disp_mod, nd)
    disp_corners[i] <- if (!is.null(resolution_ctx)) {
      resolve_dispersion_for_newdata(disp_emm, nd, resolution_ctx)$effective
    } else {
      disp_emm
    }
  }
  list(mu = mu_corners, disp = disp_corners, active = active)
}

#' Interpolate mean and dispersion metric between coded design levels (multifactor factorial models).
#' @keywords internal
predict_factor_model_interp <- function(mean_mod, disp_mod, x_cont, cont_vars, cont_codes, disc_assignment, resolution_ctx = NULL) {
  cont_vars <- as.character(cont_vars)
  if (length(cont_vars) < 1L) return(NULL)
  lo <- hi <- numeric(length(cont_vars))
  names(lo) <- names(hi) <- cont_vars
  lo_vals <- hi_vals <- list()
  for (vn in cont_vars) {
    codes <- cont_codes[[vn]]
    x <- suppressWarnings(as.numeric(x_cont[[vn]]))[1L]
    if (!is.finite(x) || length(codes) < 1L) return(NULL)
    if (x <= codes[1L]) {
      lo_vals[[vn]] <- hi_vals[[vn]] <- as.character(codes[1L])
      lo[vn] <- hi[vn] <- codes[1L]
      next
    }
    if (x >= codes[length(codes)]) {
      lo_vals[[vn]] <- hi_vals[[vn]] <- as.character(codes[length(codes)])
      lo[vn] <- hi[vn] <- codes[length(codes)]
      next
    }
    seg <- findInterval(x, codes, all.inside = TRUE)
    if (seg < 1L) seg <- 1L
    if (seg >= length(codes)) {
      lo_vals[[vn]] <- hi_vals[[vn]] <- as.character(codes[length(codes)])
      lo[vn] <- hi[vn] <- codes[length(codes)]
    } else {
      lo_vals[[vn]] <- as.character(codes[seg])
      hi_vals[[vn]] <- as.character(codes[seg + 1L])
      lo[vn] <- codes[seg]
      hi[vn] <- codes[seg + 1L]
    }
  }
  corners <- factor_model_corner_metrics(
    mean_mod = mean_mod,
    disp_mod = disp_mod,
    cont_vars = cont_vars,
    lo_vals = lo_vals,
    hi_vals = hi_vals,
    disc_assignment = disc_assignment,
    resolution_ctx = resolution_ctx
  )
  if (any(!is.finite(corners$mu)) || any(!is.finite(corners$disp))) return(NULL)
  active <- if (!is.null(corners$active)) corners$active else cont_vars
  if (length(active) < 1L) {
    return(list(mu = corners$mu, disp_pred = corners$disp))
  }
  lo_a <- lo[active]
  hi_a <- hi[active]
  x_vec <- vapply(active, function(vn) suppressWarnings(as.numeric(x_cont[[vn]])), numeric(1))
  mu <- if (length(active) == 1L) {
    .interp_linear_codes(c(lo_a[[1L]], hi_a[[1L]]), corners$mu, x_vec[[1L]])
  } else {
    .multilinear_corner_interp(x_vec, lo_a, hi_a, corners$mu)
  }
  disp <- if (length(active) == 1L) {
    .interp_linear_codes(c(lo_a[[1L]], hi_a[[1L]]), corners$disp, x_vec[[1L]])
  } else {
    .multilinear_corner_interp(x_vec, lo_a, hi_a, corners$disp)
  }
  if (!is.finite(mu) || !is.finite(disp)) return(NULL)
  list(mu = mu, disp_pred = disp)
}

#' Context for interpolating multifactor factorial mean/dispersion models during search.
#' @keywords internal
build_factor_model_interp_ctx <- function(mean_mod, disp_mod, dat, cont_vars, disc_vars) {
  cont_vars <- unique(as.character(cont_vars))
  cont_vars <- cont_vars[nzchar(cont_vars)]
  if (length(cont_vars) < 1L) return(NULL)
  cont_codes <- stats::setNames(lapply(cont_vars, function(vn) {
    factor_coded_design_levels(mean_mod, dat, vn)
  }), cont_vars)
  if (any(vapply(cont_codes, length, integer(1)) < 1L)) return(NULL)
  list(
    mean_mod = mean_mod,
    disp_mod = disp_mod,
    cont_vars = cont_vars,
    disc_vars = unique(as.character(disc_vars)),
    cont_codes = cont_codes
  )
}

#' Cell predictions at observed coded design levels; mu and dispersion metric are
#' piecewise-linearly interpolated between each adjacent level pair during search.
#' @keywords internal
build_factor_level_interp <- function(mean_mod, disp_mod, dat, cont_vars, fixed_setting = NULL, resolution_ctx = NULL) {
  if (length(cont_vars) < 1L || is.null(dat) || !is.data.frame(dat)) return(NULL)
  cont_vars <- as.character(cont_vars)
  fixed_setting <- if (is.null(fixed_setting)) list() else as.list(fixed_setting)
  out <- list()
  for (vn in cont_vars) {
    if (!vn %in% names(dat)) next
    codes <- factor_coded_design_levels(mean_mod, dat, vn)
    if (length(codes) < 2L) next
    lv <- mean_mod$xlevels[[vn]]
    if (is.null(lv) || length(lv) < 1L) {
      lv <- sort(unique(as.character(dat[[vn]])))
    }
    mu <- disp <- rep(NA_real_, length(codes))
    for (i in seq_along(codes)) {
      setting <- fixed_setting
      setting[[vn]] <- as.character(codes[[i]])
      nd <- .build_newdata_factor_setting(setting, mean_mod)
      mu[[i]] <- .taguchi_predict_point(mean_mod, nd)[1L]
      disp_emm <- .taguchi_predict_point(disp_mod, nd)[1L]
      disp[[i]] <- if (!is.null(resolution_ctx)) {
        resolve_dispersion_for_newdata(disp_emm, nd, resolution_ctx)$effective
      } else {
        disp_emm
      }
    }
    if (length(mu) != length(codes) || length(disp) != length(codes)) next
    if (any(!is.finite(mu)) || any(!is.finite(disp))) next
    out[[vn]] <- list(codes = codes, mu = mu, disp = disp)
  }
  if (length(out) < 1L) NULL else out
}

#' @keywords internal
coerce_bundle_continuous_models <- function(bundle, cont_vars) {
  if (length(cont_vars) < 1L) return(list(ok = TRUE, bundle = bundle, message = ""))
  dat_local <- bundle$dat
  if (is.null(dat_local) || !is.data.frame(dat_local)) {
    return(list(ok = FALSE, bundle = bundle, message = "Missing model training data for continuous-model bridge refit."))
  }
  touched <- character(0)
  for (vn in cont_vars) {
    if (!vn %in% names(dat_local)) next
    if (is.factor(dat_local[[vn]]) || is.character(dat_local[[vn]])) {
      num <- suppressWarnings(as.numeric(as.character(dat_local[[vn]])))
      if (length(num) != nrow(dat_local) || any(!is.finite(num))) {
        return(list(
          ok = FALSE,
          bundle = bundle,
          message = paste0(
            "Selected continuous factor '", vn,
            "' has non-numeric coded levels; cannot bridge to continuous optimization."
          )
        ))
      }
      dat_local[[vn]] <- num
      touched <- c(touched, vn)
    }
  }
  if (length(touched) < 1L) {
    return(list(ok = TRUE, bundle = bundle, message = ""))
  }
  mm <- tryCatch(refit_model_with_data(bundle$mean_model, dat_local), error = function(e) e)
  if (inherits(mm, "error")) {
    return(list(ok = FALSE, bundle = bundle, message = paste0("Failed to refit mean model for continuous bridge: ", conditionMessage(mm))))
  }
  dm <- tryCatch(refit_model_with_data(bundle$disp_model, dat_local), error = function(e) e)
  if (inherits(dm, "error")) {
    return(list(ok = FALSE, bundle = bundle, message = paste0("Failed to refit dispersion model for continuous bridge: ", conditionMessage(dm))))
  }
  bundle$dat <- dat_local
  bundle$mean_model <- mm
  bundle$disp_model <- dm
  bundle$optimization_mode <- "ancova"
  bundle$interp_ctx <- NULL
  bundle$cont_vars <- unique(c(as.character(cont_vars), bundle$cont_vars))
  bundle$cont_vars <- bundle$cont_vars[!is.na(bundle$cont_vars) & nzchar(bundle$cont_vars)]
  list(ok = TRUE, bundle = bundle, message = if (length(touched) > 0L) paste(unique(touched), collapse = ", ") else "")
}

#' @keywords internal
optimizer_response_detail_rows <- function(r) {
  if (is.null(r$aggregate) || !is.list(r$aggregate) || is.null(r$aggregate$details)) {
    return(list(metrics = character(0), values = character(0)))
  }
  det <- r$aggregate$details
  if (length(det) < 1L) return(list(metrics = character(0), values = character(0)))
  ms <- character(0)
  vs <- character(0)
  for (i in seq_along(det)) {
    di <- det[[i]]
    rn <- if (!is.null(di$response)) as.character(di$response) else paste0("response_", i)
    ms <- c(ms, paste0("Response economics [", rn, "]"))
    txt <- paste(
      c(
        paste0("type=", ifelse(is.null(di$response_type), "normal", as.character(di$response_type))),
        paste0("target=", signif(as.numeric(ifelse(is.null(di$target), NA_real_, di$target)), 6)),
        paste0("lsl=", signif(as.numeric(ifelse(is.null(di$lsl), NA_real_, di$lsl)), 6)),
        paste0("usl=", signif(as.numeric(ifelse(is.null(di$usl), NA_real_, di$usl)), 6)),
        paste0("C_l=", signif(as.numeric(ifelse(is.null(di$C_l), NA_real_, di$C_l)), 6)),
        paste0("C_u=", signif(as.numeric(ifelse(is.null(di$C_u), NA_real_, di$C_u)), 6))
      ),
      collapse = "; "
    )
    vs <- c(vs, txt)
    mu_v <- if (!is.null(di$mu)) suppressWarnings(as.numeric(di$mu)[1]) else NA_real_
    sig_v <- if (!is.null(di$sigma)) suppressWarnings(as.numeric(di$sigma)[1]) else NA_real_
    disp_v <- if (!is.null(di$disp_pred)) suppressWarnings(as.numeric(di$disp_pred)[1]) else NA_real_
    loss_v <- if (!is.null(di$expected_loss)) suppressWarnings(as.numeric(di$expected_loss)[1]) else NA_real_
    ppm_v <- if (!is.null(di$ppm)) suppressWarnings(as.numeric(di$ppm)[1]) else NA_real_
    ms <- c(
      ms,
      paste0("Predicted mean [", rn, "]"),
      paste0("Predicted sigma [", rn, "]"),
      paste0("Dispersion EMM [", rn, "]"),
      paste0("Expected loss [", rn, "]"),
      paste0("PPM [", rn, "]")
    )
    vs <- c(vs, mu_v, sig_v, disp_v, loss_v, ppm_v)
    eff_v <- if (!is.null(di$disp_effective)) suppressWarnings(as.numeric(di$disp_effective)[1]) else disp_v
    if (is.finite(eff_v) && (is.na(disp_v) || !is.finite(disp_v) || abs(eff_v - disp_v) > 1e-12)) {
      ms <- c(ms, paste0("Effective dispersion [", rn, "]"))
      vs <- c(vs, eff_v)
    }
    dr <- di$dispersion_resolution
    if (!is.null(dr) && !is.null(dr$audit_label) && nzchar(as.character(dr$audit_label)[1])) {
      ms <- c(ms, paste0("Dispersion tier [", rn, "]"))
      vs <- c(vs, as.character(dr$audit_label)[1])
    }
    if (!is.null(di$metrics) && is.list(di$metrics)) {
      ms <- c(ms, paste0("Loss lower [", rn, "]"), paste0("Loss upper [", rn, "]"))
      vs <- c(
        vs,
        suppressWarnings(as.numeric(di$metrics$loss_lower)[1]),
        suppressWarnings(as.numeric(di$metrics$loss_upper)[1])
      )
    }
  }
  list(metrics = ms, values = vs)
}

#' @keywords internal
mf_factor_bounds_from_dat <- function(dat, xnames) {
  setNames(lapply(xnames, function(xn) {
    v <- suppressWarnings(as.numeric(as.character(dat[[xn]])))
    list(lower = min(v, na.rm = TRUE), upper = max(v, na.rm = TRUE))
  }), xnames)
}

#' Discrete coded levels for one factor (model \code{xlevels}, else training data).
#' @keywords internal
mf_discrete_coded_levels <- function(model, vn, dat = NULL, lo = -Inf, hi = Inf) {
  xl <- if (!is.null(model)) model$xlevels[[vn]] else NULL
  if (is.null(xl) || length(xl) < 1L) {
    if (is.null(dat) || !vn %in% names(dat)) {
      return(NULL)
    }
    xl <- sort(unique(as.character(dat[[vn]])))
  }
  if (length(xl) < 1L) {
    return(NULL)
  }
  lv <- suppressWarnings(as.numeric(xl))
  if (any(!is.finite(lv))) {
    return(NULL)
  }
  in_rng <- sort(unique(lv[lv >= lo & lv <= hi]))
  if (length(in_rng) >= 1L) {
    return(in_rng)
  }
  mid <- (lo + hi) / 2
  d <- abs(lv - mid)
  lv <- sort(unique(lv[d == min(d, na.rm = TRUE)]))
  if (length(lv) < 1L) {
    return(NULL)
  }
  lv
}

#' Factors the optimizer may vary: design factors that appear in the reduced mean model.
#' @keywords internal
mf_optimizer_search_factors <- function(fit) {
  if (is.null(fit) || !is.list(fit)) {
    return(character(0))
  }
  fnames <- as.character(fit$factors_names)
  fnames <- fnames[nzchar(fnames)]
  if (length(fnames) < 1L) {
    return(character(0))
  }
  stored <- as.character(fit$optimizer_factor_names)
  stored <- stored[nzchar(stored)]
  if (length(stored) > 0L) {
    return(intersect(fnames, unique(stored)))
  }
  rhs <- trimws(as.character(fit$rhs_lm_mean)[1])
  if (!nzchar(rhs) || identical(rhs, "1")) {
    return(character(0))
  }
  model_facs <- if (!is.null(fit$mean_mod)) names(fit$mean_mod$xlevels) else character(0)
  if (length(model_facs) < 1L && exists(".taguchi_factors_from_rhs", mode = "function")) {
    model_facs <- .taguchi_factors_from_rhs(rhs, fnames)
  }
  intersect(fnames, unique(model_facs))
}

#' Union of adjustable factors across multiresponse bundles (search space for joint optimization).
#' @keywords internal
mf_optimizer_search_factors_union <- function(fit_active, model_bundles) {
  xnames <- mf_optimizer_search_factors(fit_active)
  if (length(model_bundles) < 2L) {
    return(sort(unique(xnames)))
  }
  for (bi in model_bundles) {
    bx <- mf_optimizer_search_factors(list(
      factors_names = fit_active$factors_names,
      mean_mod = bi$mean_model,
      rhs_lm_mean = bi$rhs_lm_mean,
      optimizer_factor_names = bi$optimizer_factor_names
    ))
    xnames <- union(xnames, bx)
  }
  sort(unique(xnames[nzchar(xnames)]))
}

#' @keywords internal
mf_factor_kinds_from_cont_vars <- function(xnames, cont_vars) {
  factor_kinds <- setNames(rep("discrete", length(xnames)), xnames)
  cont_vars <- unique(as.character(cont_vars))
  cont_vars <- cont_vars[nzchar(cont_vars)]
  if (length(cont_vars) > 0L) factor_kinds[cont_vars] <- "continuous"
  factor_kinds
}

#' Map UI continuous-factor names to model column names (make.names parity).
#' @keywords internal
mf_align_continuous_vars <- function(cont_vars, xnames, dat, fid) {
  cont_vars <- unique(as.character(cont_vars))
  cont_vars <- cont_vars[nzchar(cont_vars)]
  if (length(cont_vars) < 1L) return(character(0))
  raw <- names(dat)[as.integer(fid)]
  mn <- make.names(raw)
  map <- c(setNames(mn, raw), setNames(mn, mn))
  aligned <- vapply(cont_vars, function(cv) {
    if (cv %in% xnames) return(cv)
    hit <- map[[cv]]
    if (!is.null(hit) && hit %in% xnames) return(hit)
    mn_cv <- make.names(cv)
    if (mn_cv %in% xnames) return(mn_cv)
    NA_character_
  }, character(1))
  aligned <- aligned[!is.na(aligned) & nzchar(aligned)]
  unique(aligned)
}

#' Ensure newdata includes every factor column required by a response bundle's models.
#' @keywords internal
mf_complete_bundle_newdata <- function(b, nd) {
  if (is.null(b) || is.null(nd) || nrow(nd) < 1L) return(nd)
  nd <- as.data.frame(nd, stringsAsFactors = FALSE)
  model_vars <- character(0)
  if (!is.null(b$mean_model)) {
    model_vars <- unique(c(model_vars, names(b$mean_model$xlevels)))
  }
  if (!is.null(b$disp_model)) {
    model_vars <- unique(c(model_vars, names(b$disp_model$xlevels)))
  }
  model_vars <- model_vars[nzchar(model_vars)]
  if (length(model_vars) < 1L) return(nd)
  dat_ref <- if (!is.null(b$dat)) b$dat else NULL
  added <- character(0)
  for (vn in model_vars) {
    if (vn %in% names(nd)) next
    xl <- if (!is.null(b$mean_model) && !is.null(b$mean_model$xlevels[[vn]])) {
      b$mean_model$xlevels[[vn]]
    } else if (!is.null(b$disp_model)) {
      b$disp_model$xlevels[[vn]]
    } else {
      NULL
    }
    if (is.null(xl) || length(xl) < 1L) {
      if (!is.null(dat_ref) && vn %in% names(dat_ref)) {
        xl <- sort(unique(as.character(dat_ref[[vn]])))
      }
    }
    if (length(xl) < 1L) next
    nd[[vn]] <- factor(rep(xl[1], nrow(nd)), levels = xl)
    stats::contrasts(nd[[vn]]) <- stats::contr.sum
    added <- c(added, vn)
  }
  nd
}

#' Coerce a prediction row to the model type used during optimization (ANCOVA vs factor).
#' @keywords internal
mf_prepare_optimizer_newdata <- function(b, nd) {
  if (is.null(nd) || nrow(nd) < 1L) return(nd)
  nd <- as.data.frame(nd, stringsAsFactors = FALSE)
  if (!identical(b$optimization_mode, "ancova")) return(nd)
  cv <- if (!is.null(b$cont_vars)) as.character(b$cont_vars) else character(0)
  cv <- cv[cv %in% names(nd)]
  if (length(cv) < 1L) return(nd)
  for (vn in cv) {
    if (is.factor(nd[[vn]]) || is.character(nd[[vn]])) {
      nd[[vn]] <- suppressWarnings(as.numeric(as.character(nd[[vn]])))
    }
  }
  nd
}

#' Build a one-row settings data frame from an optimizer result.
#' @keywords internal
mf_setting_row_from_opt_result <- function(res, xnames) {
  if (is.null(res) || !isTRUE(res$ok)) return(NULL)
  if (!is.null(res$par_snapped) && is.data.frame(res$par_snapped) && nrow(res$par_snapped) >= 1L) {
    row <- res$par_snapped[1, xnames, drop = FALSE]
    return(as.data.frame(row, stringsAsFactors = FALSE))
  }
  if (!is.null(res$par)) {
    vals <- res$par[xnames]
    return(as.data.frame(as.list(vals), stringsAsFactors = FALSE))
  }
  NULL
}

#' Corner search on factor bounds (fast worst/best fallback for confirmation).
#' @keywords internal
mf_extreme_boundary_search <- function(
    eval_fn,
    bounds,
    factor_kinds,
    disc_levels = list(),
    direction = c("min", "max")) {
  direction <- match.arg(direction)
  vars <- names(bounds)
  if (length(vars) < 1L) return(list(ok = FALSE, message = "No factors to search."))
  kinds <- factor_kinds[vars]
  kinds[is.na(kinds)] <- "discrete"
  cont <- vars[kinds == "continuous"]
  disc <- vars[kinds == "discrete"]

  disc_grid <- if (length(disc) < 1L) {
    data.frame(row.names = "1")
  } else {
    dl <- disc_levels[disc]
    if (length(dl) < 1L) {
      data.frame(row.names = "1")
    } else {
      expand.grid(dl, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    }
  }

  cont_corners <- if (length(cont) < 1L) {
    list(setNames(list(), character(0)))
  } else {
    corner_list <- stats::setNames(lapply(cont, function(vn) {
      c(as.numeric(bounds[[vn]]$lower), as.numeric(bounds[[vn]]$upper))
    }), cont)
    grid <- expand.grid(corner_list, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    lapply(seq_len(nrow(grid)), function(i) as.list(grid[i, , drop = FALSE]))
  }

  best_val <- if (identical(direction, "min")) Inf else -Inf
  best_nd <- NULL
  is_better <- function(candidate, current) {
    if (!is.finite(candidate)) return(FALSE)
    if (identical(direction, "max")) candidate > current else candidate < current
  }

  for (i in seq_len(nrow(disc_grid))) {
    disc_asgn <- if (ncol(disc_grid) > 0L) as.list(disc_grid[i, , drop = FALSE]) else list()
    for (cv in cont_corners) {
      par <- c(disc_asgn, cv)
      nd <- as.data.frame(par, stringsAsFactors = FALSE)
      val <- suppressWarnings(as.numeric(eval_fn(nd)))
      if (length(val) < 1L || !is.finite(val[[1L]])) next
      if (is_better(val[[1L]], best_val)) {
        best_val <- val[[1L]]
        best_nd <- nd
      }
    }
  }

  if (is.null(best_nd) || !is.finite(best_val)) {
    return(list(ok = FALSE, message = "Boundary search did not find a finite objective."))
  }
  par <- vapply(vars, function(vn) suppressWarnings(as.numeric(best_nd[[vn]][1L])), numeric(1))
  names(par) <- vars
  list(ok = TRUE, value = best_val, par = par, par_snapped = best_nd, convergence = 0L, msg = "")
}

#' Discrete coded levels from a fitted model and bounds.
#' @keywords internal
mf_disc_levels_from_bounds <- function(mean_mod, disc_vars, bounds) {
  disc_levels <- list()
  if (length(disc_vars) < 1L) return(disc_levels)
  for (vn in disc_vars) {
    lo <- suppressWarnings(as.numeric(bounds[[vn]]$lower))
    hi <- suppressWarnings(as.numeric(bounds[[vn]]$upper))
    xl <- mean_mod$xlevels[[vn]]
    if (is.null(xl) || length(xl) < 1L) next
    lv <- suppressWarnings(as.numeric(as.character(xl)))
    if (any(!is.finite(lv))) next
    in_rng <- sort(unique(lv[lv >= lo & lv <= hi]))
    if (length(in_rng) >= 1L) {
      disc_levels[[vn]] <- in_rng
    } else {
      mid <- (lo + hi) / 2
      d <- abs(lv - mid)
      disc_levels[[vn]] <- sort(unique(lv[d == min(d, na.rm = TRUE)]))
    }
  }
  disc_levels
}

#' Parse Loss-tab actual-value inputs for continuous factors.
#' @keywords internal
mf_loss_factor_actual_map <- function(d, cont_vars, input) {
  cont_vars <- unique(as.character(cont_vars))
  cont_vars <- cont_vars[nzchar(cont_vars)]
  if (length(cont_vars) < 1L) return(list())
  map <- list()
  for (fnm in cont_vars) {
    safe <- make.names(fnm)
    key <- paste0("loss_mf_opt_actual__", safe)
    txt <- if (!is.null(input)) input[[key]] else NULL
    if (is.null(txt) || !nzchar(trimws(as.character(txt)))) next
    lv <- multifactor_factor_numeric_levels(d, fnm)
    parsed <- multifactor_parse_actual_values(txt, length(lv))
    if (!is.null(parsed)) map[[fnm]] <- parsed
  }
  map
}

#' Coded design levels per continuous factor for Loss-tab display.
#' @keywords internal
mf_loss_cont_levels_map <- function(d, cont_vars) {
  cont_vars <- unique(as.character(cont_vars))
  cont_vars <- cont_vars[nzchar(cont_vars)]
  if (length(cont_vars) < 1L) return(list())
  stats::setNames(lapply(cont_vars, function(fnm) multifactor_factor_numeric_levels(d, fnm)), cont_vars)
}

#' TRUE when two optimizer results share the same settings and objective.
#' @keywords internal
mf_opt_same_settings <- function(best_res, worst_res, xnames, tol = 1e-5) {
  if (!isTRUE(best_res$ok) || !isTRUE(worst_res$ok)) return(FALSE)
  if (is.null(best_res$par) || is.null(worst_res$par)) return(FALSE)
  pa <- suppressWarnings(as.numeric(best_res$par[xnames]))
  pb <- suppressWarnings(as.numeric(worst_res$par[xnames]))
  if (length(pa) != length(pb) || any(!is.finite(pa)) || any(!is.finite(pb))) return(FALSE)
  same_par <- max(abs(pa - pb)) <= tol
  same_val <- abs(best_res$value - worst_res$value) <= tol * max(1, abs(best_res$value))
  isTRUE(same_par && same_val)
}

#' Format optimizer factor settings with optional coded-to-actual display.
#' @keywords internal
format_optimizer_factor_display <- function(
    par,
    cont_vars,
    cont_levels,
    factor_actual_values,
    decimals = 4L) {
  cont_vars <- as.character(cont_vars)
  cont_vars <- cont_vars[nzchar(cont_vars)]
  out <- vapply(seq_along(par), function(i) {
    nm <- names(par)[[i]]
    coded <- suppressWarnings(as.numeric(par[[i]]))
    if (!is.finite(coded)) return(as.character(par[[i]]))
    if (nm %in% cont_vars && !is.null(factor_actual_values[[nm]])) {
      lv <- cont_levels[[nm]]
      actual <- multifactor_coded_to_actual(coded, lv, factor_actual_values[[nm]])
      return(multifactor_format_coded_with_actual(coded, actual, decimals))
    }
    format(round(coded, decimals), trim = TRUE, scientific = FALSE)
  }, character(1))
  names(out) <- names(par)
  out
}
