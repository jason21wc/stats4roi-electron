# Multi-Factor ANOVA Worker Module
# Business logic for Multi-Factor (EMS) ANOVA, pooling, and downstream modeling.
#
# Follows coordinator/worker separation:
# - Worker returns reactive functions / values
# - Coordinator is responsible for renderUI/renderPlot/renderDT

library(shiny)
library(lolcat)
library(ggplot2)
library(dplyr)
library(stringr)
library(emmeans)
library(nlme)
library(tidyr)

# Avoid R CMD check notes for ggplot aesthetics
utils::globalVariables(c("FirstFactor", "SecondFactor", "section", "y"))

# Helper function to standardize interaction term names (for matching)
standardize_interaction <- function(factor) {
  # Split by colon (:) if it's an interaction term
  terms <- unlist(strsplit(factor, ":"))
  # Sort the terms alphabetically and join them with colon
  standardized <- paste(sort(terms), collapse = ":")
  return(standardized)
}

#' Map pooled-effect selections from Set Up / Results onto the spelling used by
#' all_effects(), ANOVA rownames, and lm formulae (\code{make.names} column names
#' plus standardized ":" interaction order).
#' @keywords internal
normalize_ems_pool_input <- function(pool_vars, all_effects_vec, data, factors_id) {
  if (is.null(pool_vars) || length(pool_vars) < 1L) return(character(0))
  pool_vars <- unique(as.character(pool_vars))
  fid <- as.integer(factors_id)
  if (length(fid) < 2L || is.null(all_effects_vec) || length(all_effects_vec) < 1L) {
    return(pool_vars)
  }
  if (is.null(data) || !is.data.frame(data)) return(pool_vars)
  raw_fac <- names(data)[fid]
  model_fac <- make.names(raw_fac)

  canon_tokens <- function(parts) {
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    mp <- character(length(parts))
    for (j in seq_along(parts)) {
      t <- parts[j]
      w <- match(t, raw_fac, nomatch = 0L)
      if (w >= 1L) {
        mp[j] <- model_fac[w]
        next
      }
      w <- match(t, model_fac, nomatch = 0L)
      if (w >= 1L) {
        mp[j] <- model_fac[w]
        next
      }
      wm <- match(make.names(t), model_fac, nomatch = 0L)
      if (wm >= 1L) {
        mp[j] <- model_fac[wm]
        next
      }
      return(NULL)
    }
    mp
  }

  resolve_one <- function(pv_one, ae_map, std_keys, all_effects_vec) {
    parts <- unlist(strsplit(as.character(pv_one), ":", fixed = TRUE))
    mp <- canon_tokens(parts)
    if (is.null(mp)) return(NULL)
    key <- standardize_interaction(paste(mp, collapse = ":"))
    hit <- ae_map[[key]]
    if (!is.null(hit)) return(unname(hit[[1L]]))
    # Fallback: linear scan only when hash lookup misses (legacy spellings)
    hits <- all_effects_vec[vapply(all_effects_vec, function(ae) identical(standardize_interaction(ae), key), logical(1))]
    if (length(hits) >= 1L) return(hits[[1L]])
    paste(sort(mp), collapse = ":")
  }

  std_keys <- vapply(all_effects_vec, standardize_interaction, character(1))
  keep <- !duplicated(std_keys)
  ae_map <- split(all_effects_vec[keep], std_keys[keep])

  out <- character(0)
  for (pv in pool_vars) {
    r <- resolve_one(pv, ae_map, std_keys, all_effects_vec)
    if (!is.null(r) && nzchar(r)) out <- c(out, r)
  }
  unique(out)
}

# lm() fails with "contrasts can be applied only to factors with 2 or more levels" when a
# factor predictor has a single level; filter dummy-column predictors before fitting.
predictor_ok_for_lm <- function(dat, nm) {
  if (!nm %in% names(dat)) {
    return(FALSE)
  }
  x <- dat[[nm]]
  if (inherits(x, "factor") || is.factor(x)) {
    return(nlevels(droplevels(x)) >= 2L)
  }
  if (is.numeric(x) || is.integer(x)) {
    return(TRUE)
  }
  if (is.logical(x)) {
    return(TRUE)
  }
  if (is.character(x)) {
    return(nlevels(droplevels(factor(x))) >= 2L)
  }
  # Dates / other types: let lm() decide
  TRUE
}

# Every variable in formula (except response) must allow contrasts or be numeric-like.
validate_lm_predictors <- function(dat, formula_str) {
  f <- tryCatch(stats::as.formula(formula_str), error = function(e) NULL)
  if (is.null(f)) {
    return(FALSE)
  }
  resp <- all.vars(f[[2]])
  preds <- setdiff(all.vars(f), resp)
  if (length(preds) == 0L) {
    return(TRUE)
  }
  ok <- vapply(preds, function(nm) predictor_ok_for_lm(dat, nm), NA)
  !anyNA(ok) && all(ok)
}

# Catch contrasts / rank-deficiency edge cases that still reach lm() (e.g. interaction aliasing).
safe_lm <- function(formula_str, dat, context = "lm") {
  f <- tryCatch(stats::as.formula(formula_str), error = function(e) NULL)
  if (is.null(f)) {
    warning(context, ": invalid formula", call. = FALSE)
    return(NULL)
  }
  tryCatch(
    stats::lm(f, data = dat),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("contrasts can be applied only to factors", msg, fixed = TRUE) ||
        grepl("2 or more levels", msg, fixed = TRUE)) {
        warning(context, ": ", msg, call. = FALSE)
        return(NULL)
      }
      stop(e)
    }
  )
}

# Source global systems and helper/override functions
source("modules/config/global_config.R")
source("modules/statistical/anova/utils/anova_helpers.R")
source("modules/statistical/anova/utils/emsanova_roi_overrides.R")

create_multifactor_anova_worker <- function(id, filtered_data, core_input_values, posthoc_input_values, reactive_color_palette, data_invalidation_trigger = NULL) {
  moduleServer(id, function(input, output, session) {
    # Inputs from coordinator (NOT namespaced to this module; passed in explicitly)
    core_inputs <- reactive({
      core_input_values()
    })
    posthoc_inputs <- reactive({
      posthoc_input_values()
    })

    .ems_pooled_force_disp <- reactiveVal(FALSE)
    .ems_pooled_force_means <- reactiveVal(FALSE)

    # Session memo for ems_pooled() and the expensive full-factorial e2 residual fit.
    .ems_pooled_mem_env <- new.env(parent = emptyenv())
    .ems_pooled_mem_env$key <- NULL
    .ems_pooled_mem_env$result <- NULL
    .ems_pooled_mem_env$note <- NULL

    clear_ems_pooled_mem <- function() {
      .ems_pooled_mem_env$key <- NULL
      .ems_pooled_mem_env$result <- NULL
      .ems_pooled_mem_env$note <- NULL
      e2_keys <- grep("^e2\\|", ls(.ems_pooled_mem_env, all.names = TRUE), value = TRUE)
      if (length(e2_keys) > 0L) rm(list = e2_keys, envir = .ems_pooled_mem_env)
    }

    # Bump when pooled dispersion refit / note bundling logic changes (invalidates bindCache).
    ANOVA_POOLED_LOGIC_REV <- 10L

    build_ems_pooled_memo_key <- function(inputs_vals, info, pool_vars, force_disp, force_means = FALSE) {
      fd <- tryCatch(filtered_data(), error = function(e) NULL)
      list(
        logic_rev = ANOVA_POOLED_LOGIC_REV,
        factors = sort(as.integer(info$factors_id)),
        data_col = as.integer(info$data_id)[1L],
        n = nrow(info$data),
        pool = sort(as.character(pool_vars)),
        setup_pool = sort(as.character(inputs_vals$ems_pool_setup %||% character(0))),
        applied_pool = sort(as.character(inputs_vals$ems_pool %||% character(0))),
        primary = sort(as.character(inputs_vals$ems_primary_col %||% character(0))),
        conf = inputs_vals$ems_conf,
        ems = inputs_vals$ems_ems,
        disp = if (isTRUE(force_means)) {
          FALSE
        } else {
          isTRUE(inputs_vals$ems_disp) || isTRUE(force_disp)
        },
        disp_type = inputs_vals$ems_disp_type,
        mixed = isTRUE(inputs_vals$ems_show_mixed_nest),
        n_filtered = if (is.data.frame(fd)) nrow(fd) else NA_integer_
      )
    }

    finish_ems_pooled <- function(aov_out, memo_key) {
      if (is.data.frame(aov_out)) {
        note_txt <- report_commentary(isolate(anova_note()))
        attr(aov_out, "anova_note") <- note_txt
        .ems_pooled_mem_env$key <- memo_key
        .ems_pooled_mem_env$result <- aov_out
        .ems_pooled_mem_env$note <- isolate(anova_note())
      }
      aov_out
    }

    odd_level_e2_residual <- function(formula2_chr, data_no_factor) {
      e2_key <- paste0(
        "e2|", formula2_chr, "|", nrow(data_no_factor), "|",
        paste(names(data_no_factor), collapse = ",")
      )
      cached <- .ems_pooled_mem_env[[e2_key]]
      if (!is.null(cached)) return(cached)
      unique.out <- suppressWarnings(
        stats::lm(formula = stats::as.formula(formula2_chr), data = data_no_factor, singular.ok = TRUE)
      )
      e2 <- stats::anova(unique.out)["Residuals", , drop = FALSE]
      e2$`Mean Sq` <- e2$`Sum Sq` / e2$Df
      e2 <- e2[c(2, 1, 4, 5, 3)]
      .ems_pooled_mem_env[[e2_key]] <- e2
      e2
    }

    # -------------------------------------------------------------------------
    # ANOVA Notes (ported concept from monolithic)
    # -------------------------------------------------------------------------
    Commentary <- function() {
      list(text = character())
    }

    add_comment <- function(commentary, new_text) {
      if (is.null(commentary)) commentary <- Commentary()
      commentary$text <- c(commentary$text, new_text)
      commentary
    }

    report_commentary <- function(commentary) {
      if (is.null(commentary)) return("")
      paste0(commentary$text, collapse = "\n")
    }

    anova_note <- reactiveVal(Commentary())

    reset_anova_note <- function() {
      anova_note(Commentary())
    }

    # Models cached for downstream steps (graphs/post-hocs)
    aov_model <- reactiveVal(NULL)
    emm_model <- reactiveVal(NULL)
    model_mean_est <- reactiveVal(NULL)

    #' Grand-mean lm when the ANOVA has no significant fixed effects (coefficient table / export).
    #' @keywords internal
    fit_intercept_only_mean_model <- function(data, data_col) {
      resp <- names(data)[as.integer(data_col)[1L]]
      if (length(resp) < 1L || !nzchar(resp) || !resp %in% names(data)) {
        return(NULL)
      }
      stats::lm(stats::as.formula(paste(resp, "~ 1")), data = data)
    }

    reset_multifactor_worker_state <- function() {
      aov_model(NULL)
      emm_model(NULL)
      model_mean_est(NULL)
      reset_anova_note()
      clear_ems_pooled_mem()
    }

    if (!is.null(data_invalidation_trigger)) {
      observeEvent(data_invalidation_trigger(), {
        reset_multifactor_worker_state()
      }, ignoreInit = TRUE)
    }

    # Data prepared for graphing (factors as factor, dispersion column substituted if requested)
    analysis_data <- reactive({
      inputs_vals <- core_inputs()
      info <- factors_info()
      req(inputs_vals, info)

      data <- info$data
      data_col <- info$data_id
      factors_id <- info$factors_id
      factors_names <- info$factors_names

      disp <- isTRUE(inputs_vals$ems_disp)
      disp_type <- as.numeric(inputs_vals$ems_disp_type)
      disp_factors <- tryCatch(
        dispersion_cell_factors(factors_names, pooled_effects = pool_for_core(inputs_vals), available_effects = all_effects()),
        error = function(e) factors_names
      )

      # Make factors factors
      data[, factors_id] <- lapply(data[, factors_id], factor)

      # Optional dispersion transformation (monolithic semantics)
      if (disp) {
        formula_str <- paste(names(data)[data_col], "~", paste(factors_names, collapse = "*"))
        formula_str_disp <- paste(names(data)[data_col], "~", paste(disp_factors, collapse = "*"))
        if (disp_type == 1) {
          data$ADA <- compute.group.dispersion.ADA(formula(formula_str_disp), data = data)
          colnames(data)[colnames(data) == "ADA"] <- paste0(names(data)[data_col], "_ADA")
          data_col <- ncol(data)
        } else if (disp_type == 2) {
          data$ADM <- compute.group.dispersion.ADM(formula(formula_str_disp), data = data)
          colnames(data)[colnames(data) == "ADM"] <- paste0(names(data)[data_col], "_ADM")
          data_col <- ncol(data)
        } else if (disp_type == 3) {
          data$ADMn1 <- compute.group.dispersion.ADMn1(formula(formula_str_disp), data = data)
          colnames(data)[colnames(data) == "ADMn1"] <- paste0(names(data)[data_col], "_ADMn1")
          data_col <- ncol(data)
        }
      }

      list(
        data = data,
        response_col = data_col,
        response_name = names(data)[data_col],
        factors_names = factors_names
      )
    })

    # Convenience: extract selected factors/response
    factors_info <- reactive({
      inputs_vals <- core_inputs()
      data <- filtered_data()
      req(data, inputs_vals)

      names(data) <- make.names(names(data))

      factors_id <- as.numeric(inputs_vals$factors_ems)
      data_id <- as.numeric(inputs_vals$data_ems)
      req(length(factors_id) >= 1L, length(data_id) == 1L)
      req(all(factors_id >= 1L & factors_id <= ncol(data)))
      req(data_id >= 1L && data_id <= ncol(data))

      factors_names <- names(data)[factors_id]
      response_name <- names(data)[data_id]
      list(
        data = data,
        factors_id = factors_id,
        factors_names = factors_names,
        data_id = data_id,
        response_name = response_name
      )
    })

    # -------------------------------------------------------------------------
    # all_effects(): list of factorial effects (used for pooling selection)
    # Ported from monolithic all_effects()
    # -------------------------------------------------------------------------
    all_effects <- reactive({
      info <- factors_info()
      inputs_vals <- core_inputs()
      req(info, inputs_vals)

      factors_id <- info$factors_id
      factors_names <- info$factors_names
      mixed_nest <- isTRUE(inputs_vals$ems_show_mixed_nest)

      if (length(factors_id) < 2) return(NULL)

      nested_chr <- build_mf_nested_chr(factors_names, inputs_vals, mixed_nest)
      ems_design_effect_ids(factors_names, nested_chr)
    })

    #' Pooled / excluded effects in the same naming space as all_effects() and rownames(aov_out).
    pool_for_core <- function(inputs_vals) {
      pv <- inputs_vals$ems_pool
      if (is.null(pv) || length(pv) < 1L) return(character(0))
      inf <- factors_info()
      req(inf, inputs_vals)
      dat <- filtered_data()
      req(dat)
      ae <- all_effects()
      req(ae)
      normalize_ems_pool_input(pv, ae, dat, inf$factors_id)
    }

    #' Pooled / excluded effects: Set Up exclusions plus Results-tab applied pooling (\code{ems_pool}).
    pool_for_effective <- function(inputs_vals) {
      pool_for_core(inputs_vals)
    }

    #' Set Up exclusions (monolithic \code{input$ems_pool}) — live picker, not Results draft/applied store.
    pool_for_setup <- function(inputs_vals) {
      pv <- inputs_vals$ems_pool_setup
      if (is.null(pv) || length(pv) < 1L) return(character(0))
      inf <- factors_info()
      req(inf, inputs_vals)
      dat <- filtered_data()
      req(dat)
      ae <- all_effects()
      req(ae)
      normalize_ems_pool_input(pv, ae, dat, inf$factors_id)
    }

    # -------------------------------------------------------------------------
    # Unpooled ANOVA table (balanced or unbalanced). When assign_aov_model is FALSE,
    # skip caching lm() and skip anova_note updates (used for means-only table while
    # Results tab is in dispersion mode — Loss tab needs mean-significance separately).
    # -------------------------------------------------------------------------
    multifactor_unpooled_aov_core <- function(inputs_vals, info, assign_aov_model = TRUE) {
      data <- info$data
      factors_id <- info$factors_id
      factors_names <- info$factors_names
      data_id <- info$data_id

      conf <- inputs_vals$ems_conf
      R <- inputs_vals$ems_dec
      disp <- isTRUE(inputs_vals$ems_disp) # monolithic semantics: TRUE => dispersion analysis
      disp_type <- as.numeric(inputs_vals$ems_disp_type)
      mixed_nest <- isTRUE(inputs_vals$ems_show_mixed_nest)

      req(data, factors_id, data_id, conf, R)
      req(nrow(data) > 0L)

      pool_for_disp <- normalize_ems_pool_input(
        if (is.null(inputs_vals$ems_pool_setup)) character(0) else inputs_vals$ems_pool_setup,
        all_effects(),
        filtered_data(),
        factors_id
      )

      backup_opts <- options()
      options(contrasts = c("contr.sum", "contr.poly"))
      on.exit(options(backup_opts), add = TRUE)

      # Factor types (fixed/random) and nesting vector
      type <- matrix("F", nrow = length(factors_id))
      rownames(type) <- factors_names
      nested <- build_mf_nested_chr(factors_names, inputs_vals, mixed_nest)
      if (mixed_nest) {
        for (i in seq_along(factors_id)) {
          fr <- inputs_vals[[paste0("f_r_factor", i)]]
          if (is.null(fr) || !nzchar(as.character(fr)[1])) fr <- "F"
          type[i] <- as.character(fr)[1]
        }
      }

      # Base factorial model (means ANOVA uses all selected factors on RHS).
      formula_str <- paste(names(data)[data_id], "~", paste(factors_names, collapse = "*"))

      disp_factors <- factors_names
      if (disp) {
        disp_factors <- tryCatch(
          dispersion_cell_factors(
            factors_names,
            pooled_effects = pool_for_effective(inputs_vals),
            available_effects = all_effects()
          ),
          error = function(e) factors_names
        )
      }
      # Monolithic unpooled path: attach dispersion using active-model cell factors.
      formula_str_disp <- paste(names(data)[data_id], "~", paste(disp_factors, collapse = "*"))

      if (disp) {
        cell_rep <- factorial_cell_replication(data, disp_factors)
        min_count <- cell_rep$min_count
        if (is.na(min_count) || min_count < 3) return(NULL) # monolithic returns NULL; coordinator shows message later
        if (disp_type == 1) {
          data$ADA <- compute.group.dispersion.ADA(formula(formula_str_disp), data = data)
          colnames(data)[colnames(data) == "ADA"] <- paste0(names(data)[data_id], "_ADA")
          formula_str <- sub(names(data)[data_id], paste0(names(data)[data_id], "_ADA"), formula_str)
        } else if (disp_type == 2) {
          data$ADM <- compute.group.dispersion.ADM(formula(formula_str_disp), data = data)
          colnames(data)[colnames(data) == "ADM"] <- paste0(names(data)[data_id], "_ADM")
          formula_str <- sub(names(data)[data_id], paste0(names(data)[data_id], "_ADM"), formula_str)
        } else if (disp_type == 3) {
          data$ADMn1 <- compute.group.dispersion.ADMn1(formula(formula_str_disp), data = data)
          colnames(data)[colnames(data) == "ADMn1"] <- paste0(names(data)[data_id], "_ADMn1")
          formula_str <- sub(names(data)[data_id], paste0(names(data)[data_id], "_ADMn1"), formula_str)
        }
      }

      # Convert factor columns to factor for modeling
      data[, factors_names] <- lapply(data[, factors_id], factor)

      # Balanced/unbalanced check
      EMSflag <- balance_test(factors_names = factors_names, data = data) # nolint

      if (!EMSflag) {
        # Balanced: EMSanova_roi with approximate F (ANOVA notes added in ems_pooled when pooled path runs)
        # Pass nested as-is (vector of strings, empty strings are handled by EMSanova_roi)
        result <- EMSanova_roi( # nolint
          formula = formula(formula_str),
          data = data,
          type = type,
          nested = nested,
          level = NULL,
          approximate = TRUE
        )

        # Cache fitted model for downstream use
        if (assign_aov_model) {
          aov_model(attr(result, "aov_model"))
        }

        # Normalize/sort like monolithic
        result$Df <- as.numeric(result$Df)
        result$SS <- as.numeric(result$SS)
        result$MS <- as.numeric(result$MS)
        result$Fvalue <- as.numeric(result$Fvalue)
        result <- result[order(rownames(result)), ]
        result <- result[order(str_count(string = row.names(result), ":"), str_count(string = row.names(result), ":")), ]
        result <- result[order(str_count(string = row.names(result), pattern = "Residuals")), ]
        return(result)
      }

      # Unbalanced: restrictions and selected analysis approach
      if (assign_aov_model) {
        anova_note(isolate(add_comment(anova_note(), "Unbalanced design")))
      }
      if ("R" %in% type) {
        return("stats4ROI can't calculate unbalanced mixed or random effects models")
      }

      unbal <- inputs_vals$ems_ems
      req(unbal)
      if (!is.null(nested) && length(unique(nested)) > 1) {
        return("stats4ROI can't calculate unbalanced nested models")
      }

      if (as.numeric(unbal) == 1) {
        if (assign_aov_model) {
          anova_note(isolate(add_comment(anova_note(), ", Unweighted analysis")))
        }
        # Unweighted approach (ported)
        agg <- do.call(
          data.frame,
          aggregate(formula(formula_str), data = data, FUN = function(x) c(n = length(x), sum = sum(x), mean = mean(x)))
        )
        if (disp) {
          d_type <- c("ADA", "ADM", "ADMn1")
          names(agg)[(ncol(agg) - 2):ncol(agg)] <- c("n", "sum", paste0(names(data)[data_id], "_", d_type[as.numeric(disp_type)]))
        } else {
          names(agg)[(ncol(agg) - 2):ncol(agg)] <- c("n", "sum", names(data)[data_id])
        }

        ssw <- sum(data[data_id]^2) - sum(agg$sum^2 / agg$n)
        dfw <- sum(agg$n) - nrow(agg)
        msw <- ssw / dfw
        n_harm <- nrow(agg) / sum(1 / agg$n)
        aet <- msw / n_harm

        # Fit on aggregated means, but keep full-data lm for downstream model use
        model_agg <- lm(formula = formula(formula_str), data = agg)
        if (assign_aov_model) {
          aov_model(lm(formula = formula(formula_str), data = data))
        }

        temp <- suppressWarnings(anova(model_agg))
        temp$`F value` <- temp$`Mean Sq` / aet
        temp$`Pr(>F)` <- pf(q = temp$`F value`, df1 = temp$Df, df2 = dfw, lower.tail = FALSE)
        temp["Residuals", ] <- c(dfw, ssw, msw, NA, NA)
        temp <- data.frame(temp)
        names(temp) <- c("Df", "SS", "MS", "Fvalue", "Pvalue")

        temp <- temp[order(rownames(temp)), ]
        temp <- temp[order(str_count(string = row.names(temp), ":"), str_count(string = row.names(temp), ":")), ]
        temp <- temp[order(str_count(string = row.names(temp), pattern = "Residuals")), ]
        return(temp)
      }

      if (as.numeric(unbal) == 2) {
        if (assign_aov_model) {
          anova_note(isolate(add_comment(anova_note(), ", Orthogonal design odd levels (reduced model)")))
        }
        # Fractional/odd-level factorial: pass reduced formula to ems_pooled() (monolithic contract)
        effects <- setdiff(all_effects(), pool_for_disp)
        effects <- sub(":", "*", effects)
        effects <- paste(effects, collapse = "+")
        new_formula <- paste(names(data)[data_id], "~", effects)
        return(new_formula)
      }

      # Weighted analysis (uses car::Anova in monolithic)
      if (assign_aov_model) {
        anova_note(isolate(add_comment(anova_note(), ", Weighted analysis")))
      }
      aov_mod <- lm(formula = formula(formula_str), data = data)
      if (assign_aov_model) {
        aov_model(aov_mod)
      }
      # car is not imported globally; caller must ensure dependency installed (handled later)
      aov_tab <- as.data.frame(car::Anova(aov_mod, type = 3))
      aov_tab <- aov_tab[order(rownames(aov_tab)), ]
      aov_tab <- aov_tab[order(str_count(string = row.names(aov_tab), ":"), str_count(string = row.names(aov_tab), ":")), ]
      aov_tab <- aov_tab[order(str_count(string = row.names(aov_tab), pattern = "Residuals")), ]
      aov_tab <- aov_tab[!rownames(aov_tab) %in% "(Intercept)", ]
      aov_tab$MS <- aov_tab$`Sum Sq` / aov_tab$Df
      names(aov_tab) <- c("SS", "Df", "Fvalue", "Pvalue", "MS")
      aov_tab
    }

    # -------------------------------------------------------------------------
    # aov_out(): compute ANOVA table (balanced EMS path or unbalanced alternatives)
    # Ported from monolithic aov_out reactive.
    # -------------------------------------------------------------------------
    aov_out <- reactive({
      inputs_vals <- core_inputs()
      info <- factors_info()
      req(inputs_vals, info)
      multifactor_unpooled_aov_core(inputs_vals, info, assign_aov_model = TRUE)
    })

    # Unpooled means ANOVA (original response), used by Loss / readiness UI.
    #
    # When Results is on Means, `aov_out()` already computed this table with
    # assign_aov_model = TRUE — repeating multifactor_unpooled_aov_core here doubled
    # lm/car::Anova cost (visible as ~2× ANOVA delay). Delegate in that case.
    # When Results is on Dispersion, still run means separately with
    # assign_aov_model = FALSE so we do not clobber the active dispersion fit
    # in aov_model() / ANOVA notes while building loss/significance for the raw response.
    aov_out_means <- reactive({
      inputs_vals <- core_inputs()
      info <- factors_info()
      req(inputs_vals, info)
      if (!isTRUE(inputs_vals$ems_disp)) {
        return(aov_out())
      }
      iv <- inputs_vals
      iv$ems_disp <- FALSE
      multifactor_unpooled_aov_core(iv, info, assign_aov_model = FALSE)
    })

    # Unpooled dispersion ANOVA (ADA/ADM/ADMn1 response), independent of Results Means/Dispersion toggle.
    # Symmetric delegation when Results is already on Dispersion — avoids duplicating `aov_out()`.
    aov_out_dispersion <- reactive({
      inputs_vals <- core_inputs()
      info <- factors_info()
      req(inputs_vals, info)
      if (isTRUE(inputs_vals$ems_disp)) {
        return(aov_out())
      }
      iv <- inputs_vals
      iv$ems_disp <- TRUE
      multifactor_unpooled_aov_core(iv, info, assign_aov_model = FALSE)
    })

    # -------------------------------------------------------------------------
    # ems_pooled(): parity with app_monolithic.R ems_pooled (~L30379-30793)
    # -------------------------------------------------------------------------
    ems_pooled <- reactive({
      inputs_vals <- core_inputs()
      req(inputs_vals)

      info <- factors_info()
      req(info)
      pool_vars <- pool_for_effective(inputs_vals)
      # Applied model pool (Set Up + Results); do not reuse after PooledANOVA row filtering.
      model_pool_vars <- pool_vars

      data <- info$data
      factors_id <- info$factors_id
      factors_names <- info$factors_names
      data_id <- info$data_id
      conf <- inputs_vals$ems_conf
      force_disp <- isTRUE(.ems_pooled_force_disp())
      force_means <- isTRUE(.ems_pooled_force_means())
      disp <- if (force_means) FALSE else (isTRUE(inputs_vals$ems_disp) || force_disp)
      disp_type <- as.numeric(inputs_vals$ems_disp_type)
      primary_dummy <- inputs_vals$ems_primary_col
      disp_pool_effects <- pool_for_effective(inputs_vals)
      ae_for_disp <- tryCatch(all_effects(), error = function(e) character(0))

      memo_key <- build_ems_pooled_memo_key(inputs_vals, info, pool_vars, force_disp, force_means)
      if (identical(memo_key, .ems_pooled_mem_env$key) && is.data.frame(.ems_pooled_mem_env$result)) {
        if (!is.null(.ems_pooled_mem_env$note)) anova_note(.ems_pooled_mem_env$note)
        out <- .ems_pooled_mem_env$result
        if (is.null(attr(out, "anova_note", exact = FALSE))) {
          attr(out, "anova_note") <- report_commentary(.ems_pooled_mem_env$note)
        }
        return(out)
      }

      reset_anova_note()

      backup_opts <- options()
      options(contrasts = c("contr.sum", "contr.poly"))
      on.exit(options(backup_opts), add = TRUE)

      disp_factors <- tryCatch(
        dispersion_cell_factors(factors_names, pooled_effects = model_pool_vars, available_effects = all_effects()),
        error = function(e) factors_names
      )
      cell_rep <- factorial_cell_replication(data, disp_factors)
      min_count <- cell_rep$min_count

      EMSflag <- balance_test(factors_names = factors_names, data = data) # nolint

      # Reduced model formula string (effects not excluded by pool UI), matching monolithic odd-level path
      reduced_formula_chr <- function() {
        ae <- all_effects()
        req(ae)
        excl <- setdiff(ae, pool_vars)
        excl <- sub(":", "*", excl)
        paste(names(data)[data_id], "~", paste(excl, collapse = "+"))
      }

      if (EMSflag) {
        unbal <- inputs_vals$ems_ems
        req(unbal)
        if (is.logical(unbal)) {
          return(NULL)
        }
        unbal <- as.numeric(unbal)

        data[, factors_names] <- lapply(data[, factors_id], factor)

        fo <- aov_out()
        if (is.character(fo) && length(fo) == 1 && grepl("can't calculate", fo, fixed = TRUE)) {
          return(fo)
        }

        formula <- if (unbal == 2 && is.character(fo) && length(fo) == 1) {
          fo
        } else {
          reduced_formula_chr()
        }

        formula2 <- paste(names(data)[data_id], "~", paste(factors_names, collapse = "*"))

        if (disp) {
          if (is.na(min_count) || min_count < 3) {
            return(NULL)
          }
          raw_resp <- names(data)[data_id]
          disp_form <- mf_dispersion_model_grouping_formula(
            raw_resp, factors_names, model_pool_vars, all_effects()
          )
          if (disp_type == 1L) {
            data$ADA <- compute.group.dispersion.ADA(formula(disp_form$formula_chr), data = data)
            colnames(data)[colnames(data) == "ADA"] <- paste0(raw_resp, "_ADA")
            formula <- sub(raw_resp, paste0(raw_resp, "_ADA"), formula, fixed = TRUE)
            formula2 <- sub(raw_resp, paste0(raw_resp, "_ADA"), formula2, fixed = TRUE)
          } else if (disp_type == 2L) {
            data$ADM <- compute.group.dispersion.ADM(formula(disp_form$formula_chr), data = data)
            colnames(data)[colnames(data) == "ADM"] <- paste0(raw_resp, "_ADM")
            formula <- sub(raw_resp, paste0(raw_resp, "_ADM"), formula, fixed = TRUE)
            formula2 <- sub(raw_resp, paste0(raw_resp, "_ADM"), formula2, fixed = TRUE)
          } else if (disp_type == 3L) {
            data$ADMn1 <- compute.group.dispersion.ADMn1(formula(disp_form$formula_chr), data = data)
            colnames(data)[colnames(data) == "ADMn1"] <- paste0(raw_resp, "_ADMn1")
            formula <- sub(raw_resp, paste0(raw_resp, "_ADMn1"), formula, fixed = TRUE)
            formula2 <- sub(raw_resp, paste0(raw_resp, "_ADMn1"), formula2, fixed = TRUE)
          }
        }

        if (unbal == 1L) {
          agg <- do.call(
            data.frame,
            aggregate(formula(formula), data = data, FUN = function(x) c(n = length(x), sum = sum(x), mean = mean(x)))
          )
          if (disp) {
            d_type <- c("ADA", "ADM", "ADMn1")
            names(agg)[(ncol(agg) - 2):ncol(agg)] <- c("n", "sum", paste0(names(data)[data_id], "_", d_type[as.numeric(disp_type)]))
          } else {
            names(agg)[(ncol(agg) - 2):ncol(agg)] <- c("n", "sum", names(data)[data_id])
          }
          ssw <- sum(data[[data_id]]^2) - sum(agg$sum^2 / agg$n)
          dfw <- sum(agg$n) - nrow(agg)
          msw <- ssw / dfw
          n_harm <- nrow(agg) / sum(1 / agg$n)
          aet <- msw / n_harm
          temp <- lm(formula = formula(formula), data = agg)
          aov_model(temp)
          temp <- suppressWarnings(stats::anova(temp))
          temp$`F value` <- temp$`Mean Sq` / aet
          temp$`Pr(>F)` <- pf(q = temp$`F value`, df1 = temp$Df, df2 = dfw, lower.tail = FALSE)
          temp["Residuals", ] <- c(dfw, ssw, msw, NA, NA)
          temp <- data.frame(temp)
          names(temp) <- c("Df", "SS", "MS", "Fvalue", "Pvalue")
          aov_out <- temp
          aov_out <- aov_out[order(rownames(aov_out)), ]
          aov_out <- aov_out[order(str_count(string = row.names(aov_out), ":"), str_count(string = row.names(aov_out), ":")), ]
          aov_out <- aov_out[order(str_count(string = row.names(aov_out), pattern = "Residuals")), ]
          return(finish_ems_pooled(aov_out, memo_key))
        }

        if (unbal == 2L) {
          anova_note(isolate(add_comment(anova_note(), "Unbalanced design due to dummy level(s)")))

          if (!validate_lm_predictors(data, formula)) {
            return(paste0(
              "Unable to fit odd-level ANOVA: one or more model terms correspond to factors with ",
              "fewer than two levels in the current data (after filtering)."
            ))
          }
          lm_effects <- tryCatch(
            stats::lm(formula = stats::as.formula(formula), data = data),
            error = function(e) {
              return(paste0(
                "Unable to fit odd-level ANOVA: linear model could not be constructed (",
                conditionMessage(e), ")."
              ))
            }
          )
          if (is.character(lm_effects) && length(lm_effects) == 1L) {
            return(lm_effects)
          }
          aov_lm_effects <- suppressWarnings(car::Anova(lm_effects, type = 3, singular.ok = TRUE))
          aov_model(lm_effects)
          aov_lm_effects$`Mean Sq` <- aov_lm_effects$`Sum Sq` / aov_lm_effects$Df
          e_tot <- aov_lm_effects["Residuals", , drop = FALSE]

          dummy_col_names <- primary_dummy
          dummy_col <- isTruthy(dummy_col_names) && length(dummy_col_names) > 0
          if (dummy_col) {
            dummy_col_names <- dummy_col_names[vapply(dummy_col_names, function(nm) predictor_ok_for_lm(data, nm), logical(1))]
            dummy_col <- length(dummy_col_names) > 0
          }
          aov_lm_w_dummy_col <- NULL
          if (dummy_col) {
            formula_w_dummy_col <- paste(formula, "+", paste(dummy_col_names, collapse = "+"))
            lm_w_dummy_col <- tryCatch(
              stats::lm(formula = stats::as.formula(formula_w_dummy_col), data = data),
              error = function(e) NULL
            )
            if (is.null(lm_w_dummy_col)) {
              dummy_col <- FALSE
            } else {
              aov_lm_w_dummy_col <- suppressWarnings(car::Anova(lm_w_dummy_col, type = 3, singular.ok = TRUE))
              aov_lm_w_dummy_col$`Mean Sq` <- aov_lm_w_dummy_col$`Sum Sq` / aov_lm_w_dummy_col$Df
            }
          }

          agg <- do.call(
            data.frame,
            aggregate(formula(formula), data = data, FUN = function(x) c(n = length(x), sum = sum(x), mean = mean(x)))
          )
          names(agg)[seq(from = ncol(agg) - 2, to = ncol(agg))] <- c("n", "sum", "mean")

          if (mean(agg$n) == 1) {
            if (dummy_col) {
              e1 <- aov_lm_w_dummy_col["Residuals", , drop = FALSE]
              dum_SS <- sum(aov_lm_w_dummy_col[dummy_col_names, "Sum Sq", drop = TRUE])
              dum_df <- sum(aov_lm_w_dummy_col[dummy_col_names, "Df", drop = TRUE])
              e1$`Sum Sq` <- e1$`Sum Sq` + dum_SS
              e1$Df <- e1$Df + dum_df
              e1$`Mean Sq` <- e1$`Sum Sq` / e1$Df
              aov_lm_effects["Residuals", ] <- e1
              aov_lm_effects$`F value` <- aov_lm_effects$`Mean Sq` / aov_lm_effects["Residuals", ]$`Mean Sq`
              aov_lm_effects$`Pr(>F)` <- pf(
                q = aov_lm_effects$`F value`, df1 = aov_lm_effects$Df,
                df2 = aov_lm_effects["Residuals", ]$Df, lower.tail = FALSE
              )
              aov_lm_effects <- aov_lm_effects[c(1, 2, 5, 3, 4)]
              anova_note(isolate(add_comment(anova_note(), ", unreplicated, using dummy column(s) and dummy level(s) as error term")))
            } else {
              anova_note(isolate(add_comment(anova_note(), ", unreplicated, dummy level(s) added to error term")))
              aov_lm_effects <- aov_lm_effects[c(1, 2, 5, 3, 4)]
            }
          } else {
            # Monolithic: unique SS (e2) from full factorial on raw filtered data (not factor-coerced).
            data_no_factor <- filtered_data()
            if (disp) {
              data_no_factor <- cbind(data_no_factor, data[, ncol(data), drop = FALSE])
              names(data_no_factor)[ncol(data_no_factor)] <- names(data)[ncol(data)]
            }
            e2 <- odd_level_e2_residual(formula2, data_no_factor)

            if (dummy_col) {
              e1 <- aov_lm_w_dummy_col["Residuals", , drop = FALSE] - e2
              dum_SS <- sum(aov_lm_w_dummy_col[dummy_col_names, "Sum Sq", drop = TRUE])
              dum_df <- sum(aov_lm_w_dummy_col[dummy_col_names, "Df", drop = TRUE])
              e1$`Sum Sq` <- e1$`Sum Sq` + dum_SS
              e1$Df <- e1$Df + dum_df
            } else {
              e1 <- e_tot - e2
            }
            e1$`Mean Sq` <- e1$`Sum Sq` / e1$Df

            F_prime_sec <- e1$`Mean Sq` / e2$`Mean Sq`
            p_F_prime_sec <- pf(q = F_prime_sec, df1 = e1$Df, df2 = e2$Df, lower.tail = FALSE)
            if (p_F_prime_sec <= (1 - conf)) {
              aov_lm_effects["Residuals", ] <- e1
              aov_lm_effects$`F value` <- aov_lm_effects$`Mean Sq` / e1$`Mean Sq`
              aov_lm_effects$`Pr(>F)` <- pf(
                q = aov_lm_effects$`F value`, df1 = aov_lm_effects$Df, df2 = e1$Df, lower.tail = FALSE
              )
              anova_note(isolate(add_comment(
                anova_note(),
                ", used e<sub>1</sub> for error term since MSe<sub>1</sub>/MSe<sub>2</sub> was significant, examine alias table for potential confounded factors in unassigned columns"
              )))
              aov_lm_effects <- aov_lm_effects[c(1, 2, 5, 3, 4)]
              if (e1$Df == 1) {
                anova_note(isolate(add_comment(
                  anova_note(),
                  ", e<sub>1</sub> had 1df so you are allowed to pool insignificant main or interactive effects"
                )))
              }
            } else {
              anova_note(isolate(add_comment(
                anova_note(),
                ", e<sub>1</sub> and e<sub>2</sub> not significantly different, pooled e<sub>1</sub> and e<sub>2</sub>"
              )))
              if (dummy_col) {
                aov_lm_effects <- aov_lm_w_dummy_col[-match(dummy_col_names, rownames(aov_lm_w_dummy_col), nomatch = 0L), ]
              }
              aov_lm_effects <- aov_lm_effects[-which(rownames(aov_lm_effects) == "Residuals"), ]
              aov_lm_effects["Residuals", ] <- c(
                e1$`Sum Sq` + e2$`Sum Sq`, e1$Df + e2$Df, NA, NA,
                (e1$`Sum Sq` + e2$`Sum Sq`) / (e1$Df + e2$Df)
              )
              aov_lm_effects["Residuals", ]$`Mean Sq` <- aov_lm_effects["Residuals", ]$`Sum Sq` / aov_lm_effects["Residuals", ]$Df
              aov_lm_effects$`F value` <- aov_lm_effects$`Mean Sq` / aov_lm_effects["Residuals", ]$`Mean Sq`
              aov_lm_effects$`Pr(>F)` <- pf(
                q = aov_lm_effects$`F value`, df1 = aov_lm_effects$Df,
                df2 = aov_lm_effects["Residuals", ]$Df, lower.tail = FALSE
              )
              aov_lm_effects <- aov_lm_effects[c(1, 2, 5, 3, 4)]
            }
          }

          aov_out <- as.data.frame(aov_lm_effects)
          aov_out <- aov_out[order(rownames(aov_out)), ]
          aov_out <- aov_out[order(str_count(string = row.names(aov_out), ":"), str_count(string = row.names(aov_out), ":")), ]
          aov_out <- aov_out[order(str_count(string = row.names(aov_out), pattern = "Within Cells")), ]
          aov_out <- aov_out[order(str_count(string = row.names(aov_out), pattern = "Residuals")), ]
          names(aov_out) <- c("SS", "Df", "MS", "Fvalue", "Pvalue")
          return(finish_ems_pooled(aov_out, memo_key))
        }

        if (unbal == 3L) {
          aov_mod <- lm(formula = formula(formula), data = data, singular.ok = TRUE)
          aov_model(aov_mod)
          aov_out <- as.data.frame(car::Anova(aov_mod, type = 3))
          aov_out <- aov_out[order(rownames(aov_out)), ]
          aov_out <- aov_out[order(str_count(string = row.names(aov_out), ":"), str_count(string = row.names(aov_out), ":")), ]
          aov_out <- aov_out[order(str_count(string = row.names(aov_out), pattern = "Residuals")), ]
          aov_out <- aov_out[!rownames(aov_out) %in% "(Intercept)", ]
          aov_out$MS <- aov_out$`Sum Sq` / aov_out$Df
          names(aov_out) <- c("SS", "Df", "Fvalue", "Pvalue", "MS")
          return(finish_ems_pooled(aov_out, memo_key))
        }

        return(NULL)
      }

      # Balanced design: PooledANOVA path (monolithic L30609+)
      anova_note(isolate(add_comment(anova_note(), "Balanced design")))
      if (disp && (is.na(min_count) || min_count < 3)) {
        return(NULL)
      }

      aov_out_l <- if (isTRUE(disp)) {
        iv <- inputs_vals
        iv$ems_disp <- TRUE
        multifactor_unpooled_aov_core(iv, info, assign_aov_model = TRUE)
      } else {
        aov_out()
      }
      if (is.null(aov_out_l)) {
        return(NULL)
      }
      if (is.character(aov_out_l) && length(aov_out_l) == 1) {
        return(aov_out_l)
      }
      if (!is.data.frame(aov_out_l)) {
        return(aov_out_l)
      }

      # No applied exclusions left to pool — return the working (unpooled) table.
      if (length(pool_vars) < 1L) {
        return(finish_ems_pooled(aov_out_l, memo_key))
      }

      aov_out_original <- aov_out_l
      data[, factors_names] <- lapply(data[, factors_id], factor)

      # Monolithic L30616-30630: drop pool selections not present in the unpooled ANOVA
      rn_aov <- row.names(aov_out_l)
      rn_effects <- rn_aov[rn_aov != "Residuals" & rn_aov != "(Intercept)"]
      if (length(pool_vars) > 0 && any(!(pool_vars %in% rn_effects))) {
        anova_note(isolate(add_comment(anova_note(), ", the following effects selected to pool were not in the initial ANOVA and will be removed from the pooling list:<br>")))
        anova_note(isolate(add_comment(anova_note(), pool_vars[!(pool_vars %in% rn_effects)])))
        anova_note(isolate(add_comment(anova_note(), "<br>")))
        pool_vars <- pool_vars[pool_vars %in% rn_aov]
        if (length(pool_vars) == 0) {
          anova_note(isolate(add_comment(anova_note(), "<br>All pooling variables removed")))
        }
      }

      pool_vars <- pool_vars[pool_vars %in% row.names(aov_out_l)]

      if (length(pool_vars) == 0) {
        test_pool <- aov_out_l
      } else {
        test_pool <- PooledANOVA_roi(SS.table = aov_out_l, del.ID = c(pool_vars, "Residuals")) # nolint
        test_pool$Df <- as.numeric(test_pool$Df)
        test_pool$SS <- as.numeric(test_pool$SS)
        test_pool$MS <- as.numeric(test_pool$MS)
        test_pool$Fvalue <- as.numeric(test_pool$Fvalue)
        test_pool$Pvalue <- as.numeric(test_pool$Pvalue)
      }

      use_reduced_refit <- any(is.na(test_pool[-nrow(test_pool), "SS", drop = FALSE])) ||
        (isTRUE(disp) && length(model_pool_vars) > 0L)

      if (use_reduced_refit) {
        # Pooled has returned NAs somewhere not in residuals, or dispersion pooling
        # requires a reduced-model refit on the active ADM/ADA response (monolithic path).
        anova_note(isolate(add_comment(anova_note(), ", ANOVA based on reduced model")))

        # Get data and factor information
        info <- factors_info()
        req(info)
        data <- info$data
        factors_id <- info$factors_id
        factors_names <- info$factors_names
        data_id <- info$data_id

        disp_type <- as.numeric(inputs_vals$ems_disp_type)

        # Convert factors to factor class
        if (length(factors_names) >= 1L) {
          data[, factors_names] <- lapply(data[, factors_id], factor)
        }

        # Build reduced model formula by removing pooled variables
        # Line 30651-30656 in monolithic
        temp <- unlist(strsplit(row.names(aov_out_original), " "))
        temp <- temp[!temp %in% pool_vars]
        temp <- temp[!temp %in% "Residuals"]
        temp <- temp[temp != ""]
        rhs_reduced <- paste(temp, collapse = " + ")
        rhs_reduced <- gsub(":", "*", rhs_reduced, fixed = TRUE)
        raw_resp <- names(data)[data_id]
        formula_str <- paste0(raw_resp, " ~ ", rhs_reduced)

        # Recompute within-cell dispersion for the active (pooled) model, then refit ANOVA.
        if (isTRUE(disp)) {
          if (is.na(min_count) || min_count < 3L) {
            return(NULL)
          }
          disp_form <- mf_dispersion_model_grouping_formula(
            raw_resp, factors_names, model_pool_vars, ae_for_disp
          )
          prep <- mf_dispersion_prepare_data_formula(
            data = data,
            response_id = data_id,
            disp_type_id = disp_type,
            formula_chr = disp_form$formula_chr,
            disp_active = TRUE
          )
          data <- prep$data
          if (!mf_dispersion_response_usable(data[[prep$response]])) {
            return(NULL)
          }
          formula_str <- paste0(prep$response, " ~ ", rhs_reduced)
        }

        # Fit reduced model with lm()
        options(contrasts = c("contr.sum","contr.poly"))
        aov_mod <- lm(formula = formula(formula_str), data = data)
        if (isTRUE(disp)) {
          fit_resp <- all.vars(stats::formula(aov_mod))[1L]
          if (!mf_is_dispersion_response_name(fit_resp)) {
            disp_form <- mf_dispersion_model_grouping_formula(
              raw_resp, factors_names, model_pool_vars, ae_for_disp
            )
            prep <- mf_dispersion_prepare_data_formula(
              data = info$data,
              response_id = data_id,
              disp_type_id = disp_type,
              formula_chr = disp_form$formula_chr,
              disp_active = TRUE
            )
            data <- prep$data
            if (!mf_dispersion_response_usable(data[[prep$response]])) {
              return(NULL)
            }
            if (length(factors_names) >= 1L) {
              data[, factors_names] <- lapply(data[, factors_id], factor)
            }
            formula_str <- paste0(prep$response, " ~ ", rhs_reduced)
            aov_mod <- lm(formula = formula(formula_str), data = data)
          }
        }
        aov_model(aov_mod) # Cache for downstream use

        # Get count_per_cell for unreplicated check
        cell_factors <- if (disp) {
          tryCatch(
            dispersion_cell_factors(factors_names, pooled_effects = disp_pool_effects, available_effects = ae_for_disp),
            error = function(e) factors_names
          )
        } else {
          factors_names
        }
        count_per_cell <- data %>%
          group_by(across(all_of(cell_factors))) %>%
          summarize(count = n(), .groups = "drop")

        # Divert from Anova if unreplicated (line 30679-30686 in monolithic)
        if (all(count_per_cell$count == 1)) {
          # If unreplicated, use anova()
          # anova() output: Df, Sum Sq, Mean Sq, F value, Pr(>F)
          aov_out_reduced <- suppressWarnings(anova(aov_mod))
          aov_out_reduced <- as.data.frame(aov_out_reduced)
          # Remove column 3 (Mean Sq) - line 30682
          aov_out_reduced <- aov_out_reduced[-3]
          # Reorder columns: [2,1,3,4] = Sum Sq, Df, F value, Pr(>F) - line 30683
          aov_out_reduced <- aov_out_reduced[c(2, 1, 3, 4)]
        } else {
          # If at least a few replicates, use Anova(type=3)
          if (isTRUE(disp)) {
            resp_nm <- all.vars(stats::formula(aov_mod))[1L]
            y <- aov_mod$model[[resp_nm]]
            if (!mf_dispersion_response_usable(y)) {
              return(NULL)
            }
          }
          aov_out_reduced <- suppressWarnings(car::Anova(aov_mod, type = 3, singular.ok = TRUE))
          aov_out_reduced <- as.data.frame(aov_out_reduced)
          # Remove (Intercept) if present (matching monolithic line 30603)
          if ("(Intercept)" %in% rownames(aov_out_reduced)) {
            aov_out_reduced <- aov_out_reduced[!rownames(aov_out_reduced) %in% "(Intercept)", ]
          }
        }

        # Merge with test_pool to get EMS values (lines 30688-30715 in monolithic)
        aov_out_reduced$Factor <- rownames(aov_out_reduced) # Create a factor column
        test_pool$Factor <- rownames(test_pool) # Create a factor column
        rownames(aov_out_reduced) <- NULL
        rownames(test_pool) <- NULL

        # Ensure 'Factor' is the same format in both data frames
        aov_out_reduced$Factor <- gsub("\\s+", "", aov_out_reduced$Factor)
        # Apply standardization to interaction terms in both data frames
        aov_out_reduced$Factor <- sapply(aov_out_reduced$Factor, standardize_interaction)
        test_pool$Factor <- sapply(test_pool$Factor, standardize_interaction)

        # Handle missing interaction terms by adding placeholders
        missing_interactions <- setdiff(aov_out_reduced$Factor, test_pool$Factor)
        for (interaction in missing_interactions) {
          test_pool <- rbind(
            test_pool,
            data.frame(
              Df = NA, SS = NA, MS = NA, Fvalue = NA, Pvalue = NA,
              Sig = NA, EMS = NA, Factor = interaction
            )
          )
        }

        # Merge aov_out_reduced with test_pool based on matching Factor
        test_pool_ems <- data.frame(EMS = test_pool$EMS, Factor = test_pool$Factor)
        aov_out_reduced <- aov_out_reduced %>%
          left_join(test_pool_ems, by = c("Factor" = "Factor"))

        # Restore rownames and clean up (lines 30708-30709 in monolithic)
        rownames(aov_out_reduced) <- aov_out_reduced$Factor
        aov_out_reduced[, "Factor"] <- NULL

        # Calculate MS and rename columns (lines 30711-30712 in monolithic)
        # After merge, columns are: Sum Sq, Df, F value (or Fvalue), Pr(>F) (or Pvalue), EMS
        # Calculate MS = Sum Sq / Df, then rename all columns
        # Note: left_join preserves column order from aov_out_reduced, then adds EMS
        # So order should be: Sum Sq, Df, F value/Fvalue, Pr(>F)/Pvalue, EMS
        aov_out_reduced$MS <- aov_out_reduced$`Sum Sq` / aov_out_reduced$Df
        # Rename columns to match expected format: SS, Df, Fvalue, Pvalue, EMS, MS
        # Match monolithic line 30712 exactly
        names(aov_out_reduced) <- c("SS", "Df", "Fvalue", "Pvalue", "EMS", "MS")

        # Sort like monolithic (lines 30713-30715)
        aov_out_reduced <- aov_out_reduced[order(rownames(aov_out_reduced)), ]
        aov_out_reduced <- aov_out_reduced[order(str_count(string = row.names(aov_out_reduced), ":"), str_count(string = row.names(aov_out_reduced), ":")), ]
        aov_out_reduced <- aov_out_reduced[order(str_count(string = row.names(aov_out_reduced), pattern = "Residuals")), ]

        aov_out <- aov_out_reduced
      } else {
        # EMSanova has not returned NAs, use PooledANOVA result directly (monolithic L30719-30730)
        if (length(pool_vars) == 0) {
          aov_out <- aov_out_l
        } else {
          aov_out <- test_pool
          aov_out <- aov_out[order(rownames(aov_out)), ]
        }
        aov_out <- aov_out[order(str_count(string = row.names(aov_out), ":"), str_count(string = row.names(aov_out), ":")), ]
        aov_out <- aov_out[order(str_count(string = row.names(aov_out), pattern = "Residuals")), ]
      }

      # Balanced dummy-column e1/e2 (monolithic L30733-30786)
      if (isTruthy(primary_dummy)) {
        info <- factors_info()
        req(info)
        data <- info$data
        factors_id <- info$factors_id
        factors_names <- info$factors_names
        data_id <- info$data_id
        if (length(factors_names) >= 1L) {
          data[, factors_names] <- lapply(data[, factors_id], factor)
        }
        rhs_terms <- row.names(aov_out)[!row.names(aov_out) %in% c("Residuals", "(Intercept)")]
        rhs_str <- paste(rhs_terms, collapse = "+")
        formula_bal <- paste0(names(data)[data_id], " ~ ", rhs_str)
        if (isTRUE(disp)) {
          disp_form <- mf_dispersion_model_grouping_formula(
            names(data)[data_id], factors_names, model_pool_vars, all_effects()
          )
          prep <- mf_dispersion_prepare_data_formula(
            data = data,
            response_id = data_id,
            disp_type_id = disp_type,
            formula_chr = disp_form$formula_chr,
            disp_active = TRUE
          )
          data <- prep$data
          formula_bal <- paste0(prep$response, " ~ ", rhs_str)
        }
        agg <- do.call(
          data.frame,
          aggregate(stats::as.formula(formula_bal), data = data, FUN = function(x) c(n = length(x), sum = sum(x), mean = mean(x)))
        )
        names(agg)[seq(from = ncol(agg) - 2, to = ncol(agg))] <- c("n", "sum", "mean")
        if (mean(agg$n) == 1) {
          if (isTruthy(aov_out[nrow(aov_out), ]$SS)) {
            anova_note(isolate(add_comment(anova_note(), ", unreplicated, dummy column(s) added to error term")))
          }
        } else {
          dcn <- primary_dummy[vapply(primary_dummy, function(nm) predictor_ok_for_lm(data, nm), logical(1))]
          if (length(dcn) == 0) {
            # Selected dummy columns are constant / single-level; skip lm with extra terms
          } else {
          dummy_col_names <- dcn
          anova_note(isolate(add_comment(anova_note(), "replicated, with dummy columns")))
          formula_w_dummy_col <- paste(formula_bal, "+", paste(dummy_col_names, collapse = "+"))
          if (!validate_lm_predictors(data, formula_w_dummy_col)) {
            anova_note(isolate(add_comment(
              anova_note(),
              ", balanced dummy-column model skipped: insufficient factor level variation"
            )))
          } else {
          lm_w_dummy_col <- safe_lm(formula_w_dummy_col, data, "balanced dummy-column ANOVA")
          if (is.null(lm_w_dummy_col)) {
            anova_note(isolate(add_comment(
              anova_note(),
              ", balanced dummy-column model could not be fitted"
            )))
          } else {
          aov_lm_w_dummy_col <- suppressWarnings(car::Anova(lm_w_dummy_col, type = 3, singular.ok = TRUE))
          aov_lm_w_dummy_col$`Mean Sq` <- aov_lm_w_dummy_col$`Sum Sq` / aov_lm_w_dummy_col$Df
          e2 <- aov_lm_w_dummy_col["Residuals", , drop = FALSE]
          dum_SS <- sum(aov_lm_w_dummy_col[dummy_col_names, "Sum Sq", drop = TRUE])
          dum_df <- sum(aov_lm_w_dummy_col[dummy_col_names, "Df", drop = TRUE])
          e1 <- data.frame(`Sum Sq` = dum_SS, Df = dum_df, check.names = FALSE)
          e1$`Mean Sq` <- e1$`Sum Sq` / e1$Df

          F_prime_sec <- e1$`Mean Sq` / e2$`Mean Sq`
          p_F_prime_sec <- pf(q = F_prime_sec, df1 = e1$Df, df2 = e2$Df, lower.tail = FALSE)
          if (p_F_prime_sec <= (1 - conf)) {
            aov_out["Residuals", ]$SS <- e1$`Sum Sq`
            aov_out["Residuals", ]$Df <- e1$Df
            aov_out["Residuals", ]$MS <- e1$`Mean Sq`
            aov_out$Fvalue <- aov_out$MS / aov_out["Residuals", ]$MS
            aov_out$Pvalue <- pf(q = aov_out$Fvalue, df1 = aov_out$Df, df2 = aov_out["Residuals", ]$Df, lower.tail = FALSE)
            anova_note(isolate(add_comment(anova_note(), ", used e<sub>1</sub> for error term since MSe<sub>1</sub>/MSe<sub>2</sub> was significant, examine alias table for potential confounded factors in unassigned columns")))
            if (e1$Df == 1) {
              anova_note(isolate(add_comment(anova_note(), ", e<sub>1</sub> had 1df so you are allowed to pool insignificant main or interactive effects")))
            }
          } else {
            anova_note(isolate(add_comment(anova_note(), ", e<sub>1</sub> and e<sub>2</sub> not significantly different, pooled e<sub>1</sub> and e<sub>2</sub>")))
            aov_out <- aov_lm_w_dummy_col[-which(rownames(aov_lm_w_dummy_col) == "Residuals"), , drop = FALSE]
            aov_out["Residuals", ] <- c(
              e1$`Sum Sq` + e2$`Sum Sq`,
              e1$Df + e2$Df,
              NA_real_,
              NA_real_,
              (e1$`Sum Sq` + e2$`Sum Sq`) / (e1$Df + e2$Df)
            )
            aov_out$`F value` <- aov_out$`Mean Sq` / aov_out["Residuals", ]$`Mean Sq`
            aov_out$`Pr(>F)` <- pf(
              q = aov_out$`F value`,
              df1 = aov_out$Df,
              df2 = aov_out["Residuals", ]$Df,
              lower.tail = FALSE
            )
            aov_out <- aov_out[c(1, 2, 5, 3, 4)]
            names(aov_out) <- c("SS", "Df", "MS", "Fvalue", "Pvalue")
          }
          }
          }
        }
        }
      }

      finish_ems_pooled(aov_out, memo_key)
    })

    ems_pooled_dispersion <- reactive({
      .ems_pooled_force_disp(TRUE)
      on.exit(.ems_pooled_force_disp(FALSE), add = TRUE)
      ems_pooled()
    })

    ems_pooled_means <- reactive({
      .ems_pooled_force_means(TRUE)
      on.exit(.ems_pooled_force_means(FALSE), add = TRUE)
      ems_pooled()
    })

    # -------------------------------------------------------------------------
    # eff_types(): determine fixed/random status for each effect (for ICC vs ω²)
    # Ported from monolithic eff_types().
    # -------------------------------------------------------------------------
    eff_types <- reactive({
      inputs_vals <- core_inputs()
      data <- filtered_data()
      req(inputs_vals, data)

      pool <- pool_for_setup(inputs_vals)
      ae <- all_effects()
      req(ae)

      # All-fixed designs: effect types come from Set Up exclusions only (no ANOVA refit).
      if (!isTRUE(inputs_vals$ems_show_mixed_nest)) {
        active <- setdiff(ae, pool)
        if (length(active) < 1L) return(NULL)
        effects_f_r <- data.frame(
          effect = active,
          type = "F",
          type_code = 1L,
          stringsAsFactors = FALSE,
          row.names = active
        )
        return(effects_f_r)
      }

      ao <- aov_out()
      use_pooled <- (!is.null(pool) && length(pool) > 0) ||
        (is.character(ao) && length(ao) == 1L && !grepl("can't calculate", ao, fixed = TRUE) && grepl("~", ao, fixed = TRUE))
      if (use_pooled) {
        aov_l <- ems_pooled()
      } else {
        aov_l <- ao
      }
      factors_sel <- inputs_vals$factors_ems
      n_factors <- length(factors_sel)
      req(aov_l, n_factors)

      if (is.character(aov_l)) return(NULL)
      if (!is.data.frame(aov_l)) return(NULL)

      if ("(Intercept)" %in% rownames(aov_l)) {
        aov_l <- aov_l[-which(rownames(aov_l) == "(Intercept)"), ]
      }

      effects_f_r <- data.frame(effect = row.names(head(aov_l, -1)))
      row.names(effects_f_r) <- effects_f_r$effect
      effects_f_r$type <- rep("F", nrow(effects_f_r))
      effects_f_r$type_code <- 1

      fac_names <- make.names(names(data)[as.numeric(factors_sel)])
      if (isTRUE(inputs_vals$ems_show_mixed_nest)) {
        for (i in seq_len(n_factors)) {
          fr <- inputs_vals[[paste0("f_r_factor", i)]]
          if (is.null(fr) || !nzchar(as.character(fr)[1])) fr <- "F"
          fr <- as.character(fr)[1]
          idx_main <- which(gsub("\\(.*?\\)", "", effects_f_r$effect) == fac_names[i])
          if (length(idx_main) >= 1L) {
            effects_f_r$type[idx_main] <- fr
            if (fr == "R") effects_f_r$type_code[idx_main] <- 0
          }
        }
        if (n_factors == nrow(effects_f_r)) return(effects_f_r)

        for (i in (n_factors + 1):nrow(effects_f_r)) {
          m_effects <- str_split(string = effects_f_r[i, 1], pattern = ":", simplify = TRUE)
          sub_effects <- effects_f_r[m_effects, ]
          sub_effects$type_code[sub_effects$type == "R"] <- 0
          effects_f_r$type_code[i] <- prod(sub_effects$type_code)
          if (effects_f_r$type_code[i] == 0) {
            effects_f_r$type[i] <- "R"
          }
        }
      }

      effects_f_r
    })

    factor_types <- reactive({
      info <- factors_info()
      inputs_vals <- core_inputs()
      req(info, inputs_vals)
      multifactor_factor_types_from_inputs(
        factor_names = info$factors_names,
        inputs_vals = inputs_vals,
        n_factors = length(info$factors_id)
      )
    })

    has_fixed_factors <- reactive({
      length(factor_types()$fixed) > 0L
    })

    # -------------------------------------------------------------------------
    # Significant effects plot (Graphs tab) + reduced model for downstream tables
    # Ported from monolithic ems_sig_effects()
    # -------------------------------------------------------------------------
    ems_sig_effects_plot <- reactive({
      inputs_vals <- core_inputs()
      info <- factors_info()
      req(inputs_vals, info)

      message_plot <- function(msg) {
        ggplot() +
          theme_void() +
          annotate("text", x = 0, y = 0, label = msg, size = 5) +
          xlim(-1, 1) +
          ylim(-1, 1)
      }

      prepared <- analysis_data()
      data <- prepared$data
      data_col <- prepared$response_col
      factor_col <- info$factors_id
      conf <- inputs_vals$ems_conf

      req(data, data_col, factor_col, conf)
      data[, factor_col] <- lapply(data[, factor_col], factor)

      eff_tbl <- eff_types()
      factor_types <- multifactor_factor_types_from_inputs(
        factor_names = names(data)[factor_col],
        inputs_vals = inputs_vals,
        n_factors = length(factor_col)
      )
      mixed_random_present <- length(factor_types$random) > 0L

      # Pooled ANOVA when any means pooling is applied (Set Up + Results), matching commit/registry.
      pool <- pool_for_effective(inputs_vals)
      aov_out_l <- if (!is.null(pool) && length(pool) > 0) ems_pooled() else aov_out()
      req(aov_out_l)
      if (!is.data.frame(aov_out_l)) {
        return(message_plot("No ANOVA table available to plot."))
      }
      if ("(Intercept)" %in% row.names(aov_out_l)) {
        aov_out_l <- aov_out_l[!(row.names(aov_out_l) %in% "(Intercept)"), ]
      }

      # Significant effects (fixed only in mixed models; lm EMMs marginalize over omitted random factors)
      sig_effects <- rownames(aov_out_l[aov_out_l$Pvalue <= (1 - conf), , drop = FALSE])
      sig_effects <- sig_effects[sig_effects != "NA"]
      sig_effects <- multifactor_filter_fixed_anova_effects(
        sig_effects,
        eff_tbl = eff_tbl,
        random_factor_names = factor_types$random
      )
      if (length(sig_effects) == 0) {
        set_intercept_only <- function(msg) {
          m0 <- fit_intercept_only_mean_model(data, data_col)
          if (!is.null(m0)) model_mean_est(m0)
          message_plot(msg)
        }
        if (mixed_random_present && length(factor_types$fixed) == 0L) {
          return(set_intercept_only(
            "No fixed effects to plot.\nAll model factors are random.\nGrand-mean (intercept-only) coefficients are shown when enabled."
          ))
        }
        if (mixed_random_present) {
          return(set_intercept_only(
            paste0(
              "No significant fixed effects to plot.\nSee random interaction plots below.",
              "\nGrand-mean (intercept-only) coefficients are shown when enabled."
            )
          ))
        }
        return(set_intercept_only(
          "No significant effects to plot.\nGrand-mean (intercept-only) coefficients are shown when enabled."
        ))
      }

      # Build reduced model using only significant fixed effects (+ required main effects)
      interaction_factors <- grep(":", sig_effects, value = TRUE)
      individual_factors <- unique(unlist(strsplit(interaction_factors, ":")))
      filtered_factors <- individual_factors
      if (is.null(filtered_factors)) filtered_factors <- character(0)
      all_sig_interactions <- sig_effects[grepl(":", sig_effects)]
      sig_main <- setdiff(setdiff(sig_effects, interaction_factors), filtered_factors)

      rhs_parts <- character(0)
      if (length(filtered_factors) > 0) rhs_parts <- c(rhs_parts, filtered_factors)
      if (length(all_sig_interactions) > 0) {
        rhs_parts <- c(rhs_parts, vapply(all_sig_interactions, anova_effect_to_formula_term, character(1)))
      }
      if (length(sig_main) > 0) {
        rhs_parts <- c(rhs_parts, vapply(sig_main, anova_effect_to_formula_term, character(1)))
      }
      rhs_parts <- unique(rhs_parts)
      if (length(rhs_parts) == 0) {
        m0 <- fit_intercept_only_mean_model(data, data_col)
        if (!is.null(m0)) model_mean_est(m0)
        return(message_plot(
          "No valid effects for reduced model.\nGrand-mean (intercept-only) coefficients are shown when enabled."
        ))
      }
      form_str <- paste(names(data)[data_col], "~", paste(rhs_parts, collapse = "+"))
      formula_vars <- all.vars(stats::as.formula(form_str))
      if (!all(formula_vars %in% names(data))) {
        return(message_plot("Significant effects reference columns not in the current data.\nReselect factors on the Set Up tab."))
      }

      # Use sum-to-zero contrasts for coefficient estimates
      # MUST set options BEFORE converting factors to factors, so they use the correct contrasts
      backup_opts <- options()
      options(contrasts = c("contr.sum", "contr.poly"))
      on.exit(options(backup_opts), add = TRUE)

      # Convert factors to character first (to clear any existing contrasts), then back to factor
      # Then EXPLICITLY set contrast attributes to ensure sum-to-zero contrasts are used
      factor_cols_to_convert <- unique(unlist(lapply(
        c(filtered_factors, sig_main),
        parse_anova_effect_factors
      )))
      factor_cols_to_convert <- intersect(factor_cols_to_convert, names(data))
      if (length(factor_cols_to_convert) > 0) {
        data[factor_cols_to_convert] <- lapply(data[factor_cols_to_convert], as.character)
        data[factor_cols_to_convert] <- lapply(data[factor_cols_to_convert], factor)

        # EXPLICITLY set contrast attributes to sum-to-zero
        for (f in factor_cols_to_convert) {
          if (is.factor(data[[f]])) {
            contrasts(data[[f]]) <- "contr.sum"
          }
        }
      }

      model <- lm(formula = formula(form_str), data = data)
      model_mean_est(model)

      # Split significant effects into main effects and two-way interactions for plotting
      main_effects <- sig_effects[!grepl(":", sig_effects)]
      main_effects <- main_effects[!vapply(main_effects, is_nested_anova_effect, logical(1))]
      two_way_interactions <- sig_effects[grepl("^[^:]*:[^:]*$", sig_effects)]
      higher <- sig_effects[str_detect(sig_effects, ".*:.*:.*")]
      higher_present <- length(higher) > 0

      # Drop main effects that are part of two-way interactions
      if (length(main_effects) > 0 && length(two_way_interactions) > 0) {
        to_keep <- vapply(main_effects, function(x) !any(str_detect(two_way_interactions, fixed(x))), logical(1))
        main_effects <- main_effects[to_keep]
      }

      if (length(main_effects) == 0 && length(two_way_interactions) == 0) {
        if (any(vapply(sig_effects, is_nested_anova_effect, logical(1)))) {
          return(message_plot("Significant nested effects are shown in the Nested Effects section below."))
        }
        return(message_plot("This graph cannot plot three-way or higher interactions.\nCheck interaction plots below."))
      }

      PANEL <- 1
      plot_data <- NULL

      if (length(main_effects) > 0) {
        for (i in main_effects) {
          plot_factor <- anova_effect_plot_factor(i)
          plot_temp <- emmeans::emmip(model, formula(anova_effect_to_emmeans_rhs(i)), engine = "ggplot")
          main_data <- ggplot_build(plot_temp)$data[[1]]
          main_data$x <- paste(i, "=", levels(plot_temp[["data"]][[plot_factor]])[main_data$x])
          main_data$group <- NA
          main_data$PANEL <- PANEL
          PANEL <- PANEL + 1
          plot_data <- if (is.null(plot_data)) main_data else rbind(plot_data, main_data)
        }
      }

      if (length(two_way_interactions) > 0) {
        # If plot_data already exists (from main effects), need to add columns for interaction output
        # to match the structure of int_data (lines 31095-31102 in monolithic)
        if (!is.null(plot_data)) {
          plot_data$shape <- NA
          plot_data$size <- NA
          plot_data$stroke <- NA
          plot_data$fill <- NA
          # Remove columns that might not be in int_data
          if ("flipped_aes" %in% colnames(plot_data)) {
            plot_data <- plot_data[-which(colnames(plot_data) == "flipped_aes")]
          }
          if ("linewidth" %in% colnames(plot_data)) {
            plot_data <- plot_data[-which(colnames(plot_data) == "linewidth")]
          }
        }
        for (i in two_way_interactions) {
          factors <- strsplit(i, ":")[[1]]
          plot_temp <- emmeans::emmip(model, formula(paste(factors[1], "~", factors[2])), engine = "ggplot")
          int_data <- ggplot_build(plot_temp)$data[[1]]
          int_data$group <- paste(factors[1], "=", levels(plot_temp[["data"]][[factors[1]]])[int_data$group])
          int_data$x <- paste(factors[2], "=", levels(plot_temp[["data"]][[factors[2]]])[int_data$x])
          int_data$PANEL <- PANEL
          PANEL <- PANEL + 1

          # Swap x/group columns to match monolithic labeling
          names(int_data)[names(int_data) == "x"] <- "group1"
          names(int_data)[names(int_data) == "group"] <- "x"
          names(int_data)[names(int_data) == "group1"] <- "group"

          plot_data <- if (is.null(plot_data)) int_data else rbind(plot_data, int_data)
        }
      }

      plot_data <- plot_data[c("group", "x", "y", "PANEL")]
      names(plot_data) <- c("SecondFactor", "FirstFactor", "y", "section")
      plot_data$FirstFactor <- factor(plot_data$FirstFactor)
      plot_data$SecondFactor <- factor(plot_data$SecondFactor)

      title <- "Significant Effects - Estimated Marginal Means\nMain effects not included if part of an interaction"
      if (higher_present) {
        title <- paste0(title, " - Significant three-way or higher interactions not displayed")
      }

      # Palette: prefer coordinator-provided palette, fall back to base R palette
      pal <- reactive_color_palette()
      if (is.null(pal) || length(pal) < 8) pal <- palette.colors(8)
      level_colors <- rep(pal[-1], length.out = ceiling(length(unique(plot_data$SecondFactor)) / 7) * 7)

      p <- ggplot(plot_data, aes(.data$FirstFactor, .data$y, group = .data$SecondFactor, label = .data$SecondFactor, color = .data$SecondFactor)) +
        scale_color_manual(values = level_colors) +
        geom_line(linewidth = 1.5) +
        geom_point(size = 4) +
        geom_text(
          data = plot_data %>% group_by(.data$section) %>% filter(.data$FirstFactor == dplyr::last(.data$FirstFactor)),
          color = pal[1]
        ) +
        facet_wrap(~section, nrow = 1, scales = "free_x") + # nolint
        xlab("Factor") +
        ylab(names(data)[data_col]) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        ggtitle(title) +
        labs(subtitle = if (mixed_random_present) {
          "Reduced model: significant fixed effects only (averaged over random factors)"
        } else {
          "Based on a reduced model using only significant effects"
        })

      if (isTruthy(inputs_vals$ems_target)) {
        p <- p + geom_hline(yintercept = as.numeric(inputs_vals$ems_target), color = pal[2], linetype = 2)
      }
      p
    })

    # -------------------------------------------------------------------------
    # Significant nested fixed effects (Graphs tab)
    # -------------------------------------------------------------------------
    significant_nested_effects <- reactive({
      inputs_vals <- core_inputs()
      req(inputs_vals)
      if (!isTRUE(inputs_vals$ems_show_mixed_nest)) {
        return(character(0))
      }

      conf <- inputs_vals$ems_conf
      pool <- inputs_vals$ems_pool
      aov_out_l <- if (!is.null(pool) && length(pool) > 0) ems_pooled() else aov_out()
      req(aov_out_l)
      if (!is.data.frame(aov_out_l)) {
        return(character(0))
      }

      sig_effects <- rownames(aov_out_l[aov_out_l$Pvalue <= (1 - conf), , drop = FALSE])
      sig_effects <- sig_effects[sig_effects != "NA"]
      sig_effects <- sig_effects[!sig_effects %in% c("Residuals", "Residual", "(Intercept)", "Within Cells")]
      nested_effects <- sig_effects[vapply(sig_effects, is_nested_anova_effect, logical(1))]
      if (length(nested_effects) == 0) {
        return(character(0))
      }

      eff_tbl <- eff_types()
      if (is.null(eff_tbl)) {
        return(nested_effects)
      }

      nested_effects[vapply(nested_effects, function(eff) {
        if (!(eff %in% rownames(eff_tbl))) {
          return(TRUE)
        }
        identical(eff_tbl[eff, "type"], "F")
      }, logical(1))]
    })

    significant_nested_random_effects <- reactive({
      inputs_vals <- core_inputs()
      req(inputs_vals)
      if (!isTRUE(inputs_vals$ems_show_mixed_nest)) {
        return(character(0))
      }

      conf <- inputs_vals$ems_conf
      pool <- inputs_vals$ems_pool
      aov_out_l <- if (!is.null(pool) && length(pool) > 0) ems_pooled() else aov_out()
      req(aov_out_l, is.data.frame(aov_out_l))

      sig_effects <- rownames(aov_out_l[aov_out_l$Pvalue <= (1 - conf), , drop = FALSE])
      sig_effects <- sig_effects[sig_effects != "NA"]
      sig_effects <- sig_effects[!sig_effects %in% c("Residuals", "Residual", "(Intercept)", "Within Cells")]
      nested_effects <- sig_effects[vapply(sig_effects, is_nested_anova_effect, logical(1))]
      if (length(nested_effects) == 0) {
        return(character(0))
      }

      eff_tbl <- eff_types()
      req(eff_tbl)

      nested_effects[vapply(nested_effects, function(eff) {
        (eff %in% rownames(eff_tbl)) && identical(eff_tbl[eff, "type"], "R")
      }, logical(1))]
    })

    nested_stratum_table_data <- reactive({
      inputs_vals <- core_inputs()
      prepared <- analysis_data()
      effects <- significant_nested_effects()
      req(inputs_vals, prepared)
      if (length(effects) == 0) {
        return(NULL)
      }

      pool <- inputs_vals$ems_pool
      aov_out_l <- if (!is.null(pool) && length(pool) > 0) ems_pooled() else aov_out()
      req(aov_out_l, is.data.frame(aov_out_l))

      data <- prepared$data
      data_col <- prepared$response_col
      conf <- inputs_vals$ems_conf
      R <- inputs_vals$ems_dec
      residual_row <- anova_residual_row(aov_out_l)
      ms_den <- as.numeric(aov_out_l[residual_row, "MS"])
      df_den <- as.numeric(aov_out_l[residual_row, "Df"])

      table_parts <- vector("list", length(effects))
      footnotes <- character(0)

      for (i in seq_along(effects)) {
        eff <- effects[[i]]
        tbl <- compute_nested_stratum_tests(
          effect_name = eff,
          data = data,
          response_col = data_col,
          aov_out_l = aov_out_l,
          conf = conf
        )
        if (!is.null(tbl)) {
          table_parts[[i]] <- tbl
          footnotes <- c(
            footnotes,
            paste0(
              "Within-stratum tests for ", eff,
              " use one-way SS/MS holding parents constant, with denominator MS = ",
              lolcat::round.object(ms_den, R),
              " (df = ", df_den, ") from the full nested ANOVA."
            )
          )
        }
      }

      table_parts <- table_parts[!vapply(table_parts, is.null, logical(1))]
      if (length(table_parts) == 0) {
        return(NULL)
      }

      out <- do.call(rbind, table_parts)
      rownames(out) <- NULL
      attr(out, "footnotes") <- unique(footnotes)
      out
    })

    nested_effect_plot <- function(effect_name) {
      inputs_vals <- core_inputs()
      prepared <- analysis_data()
      req(inputs_vals, prepared, effect_name)

      message_plot <- function(msg) {
        ggplot() +
          theme_void() +
          annotate("text", x = 0, y = 0, label = msg, size = 5) +
          xlim(-1, 1) +
          ylim(-1, 1)
      }

      parsed <- parse_nested_effect(effect_name)
      if (is.null(parsed)) {
        return(message_plot("Invalid nested effect."))
      }

      eff_tbl <- eff_types()
      if (!is.null(eff_tbl) && effect_name %in% rownames(eff_tbl) && identical(eff_tbl[effect_name, "type"], "R")) {
        return(message_plot(paste0(
          "Random nested effect ", effect_name, " is not shown here.\n",
          "See the ANOVA table (ICC) and interaction plots below."
        )))
      }

      data <- prepared$data
      data_col <- prepared$response_col
      response_name <- prepared$response_name
      conf <- inputs_vals$ems_conf
      target <- inputs_vals$ems_target
      R <- inputs_vals$ems_dec

      pool <- inputs_vals$ems_pool
      aov_out_l <- if (!is.null(pool) && length(pool) > 0) ems_pooled() else aov_out()
      req(aov_out_l, is.data.frame(aov_out_l), effect_name %in% rownames(aov_out_l))

      panel_count <- nested_effect_panel_count(data, effect_name)
      if (panel_count > 48) {
        return(message_plot("Too many parent panels to display graphically.\nSee the nested stratum table below."))
      }

      model <- aov_model()
      req(model)

      pal <- reactive_color_palette()
      if (is.null(pal) || length(pal) < 4) pal <- palette.colors(8)

      overall_f <- lolcat::round.object(as.numeric(aov_out_l[effect_name, "Fvalue"]), R)
      overall_p <- lolcat::round.object(as.numeric(aov_out_l[effect_name, "Pvalue"]), R)
      title_text <- paste0("Nested effect: ", nested_effect_title(effect_name), " — Estimated marginal means")
      subtitle_text <- paste0(
        "Overall ANOVA: ", effect_name, " — F = ", overall_f,
        ", p = ", overall_p, " (EMS table at ", conf * 100, "% confidence)"
      )
      caption_text <- paste0(
        "Each panel shows ", parsed$child, " within one parent combination. ",
        "Overall ", effect_name, " tests variation of ", parsed$child, " across all parent strata."
      )

      if (panel_count > 24) {
        child <- parsed$child
        parents <- parsed$parents
        combos <- nested_parent_combinations(data, parents)
        combos$stratum <- apply(combos, 1, function(row_idx) {
          nested_stratum_label(child, combos[row_idx, , drop = FALSE], parents)
        })

        df <- data
        df$stratum <- apply(seq_len(nrow(df)), 1, function(row_idx) {
          parent_row <- df[row_idx, parents, drop = FALSE]
          nested_stratum_label(child, parent_row, parents)
        })
        df$x_num <- as.numeric(factor(df[[child]]))

        p <- ggplot(
          df,
          aes(
            x = .data$x_num,
            y = .data[[response_name]],
            group = .data$stratum,
            color = .data$stratum
          )
        ) +
          scale_color_manual(values = pal[-1]) +
          scale_x_continuous(
            breaks = seq_along(levels(factor(df[[child]]))),
            labels = levels(factor(df[[child]]))
          ) +
          stat_summary(fun = mean, geom = "line", linewidth = 1.5) +
          stat_summary(fun = mean, geom = "point", size = 4) +
          xlab(paste0(parsed$child, " level")) +
          ylab(response_name) +
          ggtitle(title_text) +
          labs(subtitle = subtitle_text, caption = paste(caption_text, "Profile plot shown because parent panel count exceeds 24.")) +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.title = element_blank()
          )
      } else {
        emm_rhs <- nested_effect_emmeans_rhs(effect_name)
        plot_temp <- emmeans::emmip(model, formula(emm_rhs), engine = "ggplot")
        p <- plot_temp +
          ggtitle(title_text) +
          labs(
            subtitle = subtitle_text,
            caption = caption_text,
            x = paste0(parsed$child, " level"),
            y = response_name
          ) +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text = element_text(size = rel(1.1))
          )
      }

      if (isTruthy(target)) {
        p <- p + geom_hline(yintercept = as.numeric(target), color = pal[2], linetype = 2)
      }
      p
    }

    nested_effect_plot_height_one <- function(effect_name) {
      prepared <- analysis_data()
      req(prepared, effect_name)
      panels <- nested_effect_panel_count(prepared$data, effect_name)
      if (panels > 24) {
        400L
      } else {
        as.integer(400 * ceiling(max(panels, 1L) / 3))
      }
    }

    nested_effects_plot_height <- reactive({
      effects <- significant_nested_effects()
      prepared <- analysis_data()
      req(prepared)
      if (length(effects) == 0) {
        return(400)
      }
      heights <- vapply(effects, nested_effect_plot_height_one, integer(1))
      sum(heights)
    })

    # Main effects boxplots (Graphs tab)
    ems_main_effects_plot <- reactive({
      inputs_vals <- core_inputs()
      prepared <- analysis_data()
      req(inputs_vals, prepared)

      data <- prepared$data
      data_col_name <- prepared$response_name
      factors <- prepared$factors_names
      factor_types <- multifactor_factor_types_from_inputs(
        factor_names = factors,
        inputs_vals = inputs_vals,
        n_factors = length(factors)
      )
      if (length(factor_types$fixed) > 0L) {
        factors <- intersect(factors, factor_types$fixed)
      }
      target <- inputs_vals$ems_target
      disp <- isTRUE(inputs_vals$ems_disp)
      if (length(factors) < 1L) {
        return(
          ggplot2::ggplot() +
            ggplot2::theme_void() +
            ggplot2::ggtitle("No fixed factors for main-effects boxplots.")
        )
      }
      req(data, data_col_name, factors)

      me_title <- if (disp) "Main Effects - Dispersion" else "Main Effects - Means"

      pal <- reactive_color_palette()
      if (is.null(pal) || length(pal) < 4) pal <- palette.colors(8)

      # Match monolithic formatting:
      # - pivot_longer over factor columns
      # - level labels "Factor = Level"
      # - stat_boxplot errorbars + filled boxplot + mean point
      # - facet_wrap by factor name, scales free
      p <- data %>%
        tidyr::pivot_longer(cols = all_of(factors)) %>%
        mutate(level = paste(.data$name, "=", .data$value)) %>%
        ggplot(aes(y = !!sym(data_col_name))) +
        stat_boxplot(aes(x = level), geom = "errorbar", width = 0.5) +
        geom_boxplot(aes(x = level), fill = pal[3]) +
        stat_summary(aes(x = level), fun = "mean", geom = "point", size = 5) +
        facet_wrap(vars(.data$name), scales = "free") +
        ggtitle(me_title) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.5)),
          plot.title = element_text(size = rel(1.5)),
          strip.text = element_text(size = rel(1.5))
        )

      if (isTruthy(target)) {
        p <- p + geom_hline(yintercept = as.numeric(target), color = pal[2], linetype = 2)
      }

      p
    })

    # Interaction plot helper (Graphs tab) - returns a ggplot for a given effect string "A:B" or "A:B:C"
    interaction_plot <- function(effect) {
      prepared <- analysis_data()
      inputs_vals <- core_inputs()
      req(prepared, inputs_vals)
      pooled_active <- length(pool_for_setup(inputs_vals)) > 0L

      data <- prepared$data
      ycol <- prepared$response_name
      target <- inputs_vals$ems_target
      R <- as.numeric(inputs_vals$ems_dec)
      req(data, ycol, effect)

      factors <- strsplit(effect, ":", fixed = TRUE)[[1]]
      factors <- factors[factors != ""]
      if (length(factors) < 2) return(NULL)

      pal <- reactive_color_palette()
      if (is.null(pal) || length(pal) < 4) pal <- palette.colors(8)

      # Determine if this effect is random vs fixed (match monolithic behavior)
      eff_tbl <- eff_types()
      is_random <- FALSE
      if (!is.null(eff_tbl) && effect %in% rownames(eff_tbl)) {
        is_random <- identical(eff_tbl[effect, "type"], "R")
      }

      if (length(factors) == 2) {
        a <- factors[1]
        b <- factors[2]

        if (is_random) {
          # Match monolithic random-effects interaction display (density + ICC annotation)
          aov_l <- if (pooled_active) ems_pooled() else aov_out()
          req(aov_l)
          if (!is.data.frame(aov_l)) return(NULL)

          res_row <- if ("Residual" %in% rownames(aov_l)) "Residual" else "Residuals"
          msb <- aov_l[paste0(a, ":", b), "MS"]
          msw <- aov_l[res_row, "MS"]
          J <- nrow(unique(data[c(a, b)]))
          counts <- data %>% count(.data[[a]], .data[[b]])
          sum_n <- sum(counts$n)
          sum_nsq <- sum(counts$n^2)
          K_prime <- (1 / (J - 1)) * (sum_n - (sum_nsq / sum_n))
          bcv <- (msb - msw) / K_prime
          bcv <- max(0, bcv)
          ICC <- 100 * bcv / (bcv + msw)

          pop_mean <- mean(data[[ycol]])
          limits <- data.frame(x = c(pop_mean - 2 * bcv^0.5 - 3 * msw^0.5, pop_mean, pop_mean + 2 * bcv^0.5 + 3 * msw^0.5))
          fills <- c("Population of Means" = as.character(pal[5]), "Unexplained Variability" = as.character(pal[3]))

          p <- ggplot(data = limits, aes(.data$x)) +
            ylab("PDF(x)") +
            geom_area(stat = "function", fun = dnorm, args = list(mean = pop_mean, sd = bcv^0.5), aes(fill = "Population of Means"), color = pal[1]) +
            geom_area(stat = "function", fun = dnorm, args = list(mean = pop_mean - 2 * bcv^0.5, sd = msw^0.5), aes(fill = "Unexplained Variability"), color = pal[3], alpha = 0.5) +
            geom_area(stat = "function", fun = dnorm, args = list(mean = pop_mean + 2 * bcv^0.5, sd = msw^0.5), aes(fill = "Unexplained Variability"), color = pal[3], alpha = 0.5) +
            labs(title = paste0("Random Effects Post-Hoc for ", a, ":", b), fill = " ") +
            scale_fill_manual(values = fills)

          ylim <- ggplot_build(p)[["layout"]][["panel_scales_y"]][[1]][["range"]][["range"]][2]
          effect_line <- c("95.45% Confidence Interval of Effect" = as.character(pal[1]))
          p <- p +
            geom_segment(
              data = data.frame(),
              aes(x = pop_mean - 2 * bcv^0.5, y = ylim / 2, xend = pop_mean + 2 * bcv^0.5, yend = ylim / 2, color = "95.45% Confidence Interval of Effect"),
              linewidth = 2,
              inherit.aes = FALSE
            ) +
            scale_color_manual(values = effect_line) +
            labs(color = " ") +
            theme(legend.position = "bottom") +
            annotate("text", x = -Inf, y = Inf, hjust = "left", vjust = "top", label = paste0("ICC = ", lolcat::round.object(ICC, R), "%"))
        } else {
          # Fixed effects formatting matches monolithic
          # Use numeric x positions (1..k) so we can add "in-between" minor marks (1.5, 2.5, ...)
          df <- data
          df[[a]] <- factor(df[[a]])
          df$x_num <- as.numeric(df[[a]])
          x_labels <- levels(df[[a]])
          n_levels <- length(x_labels)

          p <- ggplot(
            df,
            aes(
              x = .data$x_num,
              y = .data[[ycol]],
              group = factor(.data[[b]]),
              color = factor(.data[[b]]),
              linetype = factor(.data[[b]]),
              shape = factor(.data[[b]])
            )
          ) +
            scale_color_manual(values = pal[-1]) +
            # Match monolithic feel: small padding at ends + midpoint marks between integer levels
            scale_x_continuous(
              breaks = seq_len(n_levels),
              labels = x_labels,
              minor_breaks = if (n_levels >= 2) seq(1.5, n_levels - 0.5, by = 1) else NULL,
              expand = ggplot2::expansion(mult = c(0.03, 0.03))
            ) +
            stat_summary(fun = mean, geom = "line", linewidth = 1.5) +
            stat_summary(fun = mean, geom = "point", size = 4) +
            ggtitle(paste0("Two-way Interaction ", a, ":", b)) +
            theme(
              panel.grid.major.x = element_line(color = "grey75"),
              panel.grid.minor.x = element_line(color = "grey85"),
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title = element_text(size = rel(1.5)),
              axis.text = element_text(size = rel(1.5)),
              plot.title = element_text(size = rel(1.5)),
              strip.text = element_text(size = rel(1.5)),
              legend.text = element_text(size = rel(1.5)),
              aspect.ratio = 1
            )
        }
      } else {
        a <- factors[1]
        b <- factors[2]
        c <- factors[3]

        if (is_random) {
          aov_l <- if (pooled_active) ems_pooled() else aov_out()
          req(aov_l)
          if (!is.data.frame(aov_l)) return(NULL)

          res_row <- if ("Residual" %in% rownames(aov_l)) "Residual" else "Residuals"
          msb <- aov_l[paste0(a, ":", b, ":", c), "MS"]
          msw <- aov_l[res_row, "MS"]
          J <- nrow(unique(data[c(a, b, c)]))
          counts <- data %>% count(.data[[a]], .data[[b]], .data[[c]])
          sum_n <- sum(counts$n)
          sum_nsq <- sum(counts$n^2)
          K_prime <- (1 / (J - 1)) * (sum_n - (sum_nsq / sum_n))
          bcv <- (msb - msw) / K_prime
          bcv <- max(0, bcv)
          ICC <- 100 * bcv / (bcv + msw)

          pop_mean <- mean(data[[ycol]])
          limits <- data.frame(x = c(pop_mean - 2 * bcv^0.5 - 3 * msw^0.5, pop_mean, pop_mean + 2 * bcv^0.5 + 3 * msw^0.5))
          fills <- c("Population of Means" = as.character(pal[5]), "Unexplained Variability" = as.character(pal[3]))

          p <- ggplot(data = limits, aes(.data$x)) +
            ylab("PDF(x)") +
            geom_area(stat = "function", fun = dnorm, args = list(mean = pop_mean, sd = bcv^0.5), aes(fill = "Population of Means"), color = pal[1]) +
            geom_area(stat = "function", fun = dnorm, args = list(mean = pop_mean - 2 * bcv^0.5, sd = msw^0.5), aes(fill = "Unexplained Variability"), color = pal[3], alpha = 0.5) +
            geom_area(stat = "function", fun = dnorm, args = list(mean = pop_mean + 2 * bcv^0.5, sd = msw^0.5), aes(fill = "Unexplained Variability"), color = pal[3], alpha = 0.5) +
            labs(title = paste0("Random Effects Post-Hoc for ", a, ":", b, ":", c), fill = " ") +
            scale_fill_manual(values = fills)

          ylim <- ggplot_build(p)[["layout"]][["panel_scales_y"]][[1]][["range"]][["range"]][2]
          effect_line <- c("95.45% Confidence Interval of Effect" = as.character(pal[1]))
          p <- p +
            geom_segment(
              data = data.frame(),
              aes(x = pop_mean - 2 * bcv^0.5, y = ylim / 2, xend = pop_mean + 2 * bcv^0.5, yend = ylim / 2, color = "95.45% Confidence Interval of Effect"),
              linewidth = 2,
              inherit.aes = FALSE
            ) +
            scale_color_manual(values = effect_line) +
            labs(color = " ") +
            theme(legend.position = "bottom") +
            annotate("text", x = -Inf, y = Inf, hjust = "left", vjust = "top", label = paste0("ICC = ", lolcat::round.object(ICC, R), "%"))
        } else {
          # Use numeric x positions (1..k) so we can add "in-between" minor marks (1.5, 2.5, ...)
          df <- data
          df[[a]] <- factor(df[[a]])
          df$x_num <- as.numeric(df[[a]])
          x_labels <- levels(df[[a]])
          n_levels <- length(x_labels)

          p <- ggplot(
            df,
            aes(
              x = .data$x_num,
              y = .data[[ycol]],
              group = factor(.data[[b]]),
              color = factor(.data[[b]]),
              linetype = factor(.data[[b]]),
              shape = factor(.data[[b]])
            )
          ) +
            scale_color_manual(values = pal[-1]) +
            # Match monolithic feel: small padding at ends + midpoint marks between integer levels
            scale_x_continuous(
              breaks = seq_len(n_levels),
              labels = x_labels,
              minor_breaks = if (n_levels >= 2) seq(1.5, n_levels - 0.5, by = 1) else NULL,
              expand = ggplot2::expansion(mult = c(0.03, 0.03))
            ) +
            facet_grid(vars(.data[[c]]), labeller = "label_both") +
            stat_summary(fun = mean, geom = "line", linewidth = 1.5) +
            stat_summary(fun = mean, geom = "point", size = 4) +
            ggtitle(paste0("Three-way Interaction ", a, ":", b, ":", c)) +
            labs(color = b) +
            theme(
              panel.grid.major.x = element_line(color = "grey75"),
              panel.grid.minor.x = element_line(color = "grey85"),
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title = element_text(size = rel(1.5)),
              axis.text = element_text(size = rel(1.5)),
              plot.title = element_text(size = rel(1.5)),
              strip.text = element_text(size = rel(1.5)),
              legend.text = element_text(size = rel(1.5)),
              aspect.ratio = 1
            ) +
            guides(shape = "none", linetype = "none")
        }
      }

      if (isTruthy(target) && !is_random) {
        p <- p + geom_hline(yintercept = as.numeric(target), color = pal[2], linetype = 2)
      }
      p
    }

    # -------------------------------------------------------------------------
    # Post-hocs (Post-hocs tab)
    # - posthoc_plot(): ggplot contrast plot via emmeans::plot()
    # - posthoc_out_html(): HTML decision matrix (Reject/blank)
    # -------------------------------------------------------------------------
    posthoc_plot <- reactive({
      core_vals <- core_inputs()
      ph_vals <- posthoc_inputs()
      prepared <- analysis_data()
      req(core_vals, ph_vals, prepared)

      message_plot <- function(msg) {
        ggplot() +
          theme_void() +
          annotate("text", x = 0, y = 0, label = msg, size = 5) +
          xlim(-1, 1) +
          ylim(-1, 1)
      }

      data <- prepared$data
      factors_col <- factors_info()$factors_id
      response_name <- prepared$response_name

      conf <- core_vals$ems_conf
      ph_test <- as.numeric(ph_vals$ems_ph_select)
      ph_effects <- ph_vals$ems_ph_effects
      ft_ph <- factor_types()
      ph_effects <- multifactor_filter_fixed_anova_effects(
        as.character(ph_effects),
        eff_tbl = eff_types(),
        random_factor_names = if (!is.null(ft_ph)) ft_ph$random else character(0)
      )
      plot_options <- ph_vals$ems_ph_plot_options
      font_size <- as.numeric(ph_vals$ph_font_size)
      target <- core_vals$ems_target

      # Show a clear message instead of returning nothing
      if (!isTruthy(ph_test) || is.na(ph_test)) {
        return(message_plot("Select a post-hoc test to generate the contrast plot."))
      }
      if (!isTruthy(ph_effects) || length(ph_effects) < 1) {
        return(message_plot("Select at least one fixed effect for post-hoc to generate the contrast plot."))
      }
      req(conf, font_size)
      ph_effect <- ph_effects[[1]]

      # Handle options
      CIs <- isTruthy(plot_options) && ("CIs" %in% plot_options)
      PIs <- isTruthy(plot_options) && ("PIs" %in% plot_options)
      hor <- isTruthy(plot_options) && ("hor" %in% plot_options)

      test_name <- c(
        "Tukey", "Bonferroni Procedure", "Holm's Method",
        "Games-Howell", "Bonferroni Procedure - unequal variances", "Holm's Method - unequal variances"
      )[ph_test]
      adjust_method <- c("tukey", "bonferroni", "holm", "tukey", "bonferroni", "holm")[ph_test]

      pool_ph <- pool_for_core(core_vals)

      # Build model formula (respect pooling by excluding pooled effects)
      if (length(pool_ph) > 0L) {
        effects <- setdiff(all_effects(), pool_ph)
        gls_formula <- formula(paste(
          response_name,
          "~",
          gsub(pattern = ":", x = paste(effects, collapse = "+"), replacement = "*")
        ))
      } else {
        base_model <- aov_model()
        req(base_model)
        gls_formula <- formula(base_model)
      }

      # Unequal variance post-hoc needs varIdent weights
      if (ph_test > 3) {
        data$group <- interaction(data[factors_col])
        sd_test <- aggregate(x = formula(paste(response_name, "~", "group")), data = data, FUN = sd)
        if (min(sd_test[[2]], na.rm = TRUE) == 0) {
          emm_model("zerovar")
          return(message_plot("At least one combination has zero variance.\nAn unequal variance post-hoc cannot be used."))
        }
        model <- do.call(nlme::gls, list(gls_formula, data = data, weights = nlme::varIdent(form = ~1 | group)))
      } else {
        model <- if (length(pool_ph) > 0L) {
          lm(gls_formula, data = data, singular.ok = TRUE)
        } else {
          aov_model()
        }
      }

      emm <- emmeans::emmeans(
        model,
        formula(paste("pairwise ~", gsub(pattern = ":", replacement = "*", x = ph_effect))),
        adjust = adjust_method,
        type = "response",
        data = data,
        level = conf
      )
      emm_model(emm)

      pal <- reactive_color_palette()
      if (is.null(pal) || length(pal) < 2) pal <- palette.colors(8)

      # emmeans provides an S3 plot method for emmGrid; use the generic plot()
      # (not emmeans::plot, which is not exported).
      p <- plot(
        emm,
        horizontal = hor,
        type = "response",
        PIs = PIs,
        CIs = CIs,
        comparisons = TRUE,
        adjust = adjust_method,
        alpha = (1 - conf),
        colors = pal,
        xlab = paste0("Estimated Marginal Means: ", response_name)
      ) +
        ggtitle(paste0("Contrast Plot using ", test_name, " adjustment at ", conf * 100, "% Confidence Interval")) +
        theme_bw(base_size = font_size) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

      if (isTruthy(target)) {
        p <- p + geom_vline(xintercept = as.numeric(target), color = pal[2], linetype = 2)
      }
      p
    })

    posthoc_out_dt <- reactive({
      core_vals <- core_inputs()
      ph_vals <- posthoc_inputs()
      prepared <- analysis_data()
      req(core_vals, ph_vals, prepared)

      data <- prepared$data
      response_name <- prepared$response_name
      factors_col <- factors_info()$factors_id

      conf <- core_vals$ems_conf
      effects_4_ph <- ph_vals$ems_ph_effects
      ft_ph <- factor_types()
      effects_4_ph <- multifactor_filter_fixed_anova_effects(
        as.character(effects_4_ph),
        eff_tbl = eff_types(),
        random_factor_names = if (!is.null(ft_ph)) ft_ph$random else character(0)
      )
      ph_test <- as.numeric(ph_vals$ems_ph_select)
      req(data, conf, effects_4_ph, ph_test)
      if (length(effects_4_ph) < 1L) {
        return(DT::datatable(
          matrix("No fixed effects selected for post-hoc.", nrow = 1),
          options = list(dom = "t", paging = FALSE),
          rownames = FALSE
        ))
      }

      adjust_method <- c("tukey", "bonferroni", "holm", "tukey", "bonferroni", "holm")[ph_test]

      pool_ph_dt <- pool_for_core(core_vals)

      # Build model formula (respect pooling)
      if (length(pool_ph_dt) > 0L) {
        effects <- setdiff(all_effects(), pool_ph_dt)
        gls_formula <- formula(paste(
          response_name,
          "~",
          gsub(pattern = ":", x = paste(effects, collapse = "+"), replacement = "*")
        ))
      } else {
        base_model <- aov_model()
        req(base_model)
        gls_formula <- formula(base_model)
      }

      # Fit model (unequal variance handled like in posthoc_plot)
      if (ph_test > 3) {
        data$group <- interaction(data[factors_col])
        sd_test <- aggregate(x = formula(paste(response_name, "~", "group")), data = data, FUN = sd)
        if (min(sd_test[[2]], na.rm = TRUE) == 0) {
          emm_model("zerovar")
          # Match monolithic behavior: return an informative message
          msg <- "At least one combination has zero variance. An unequal variance post-hoc cannot be used."
          return(DT::datatable(matrix(msg, nrow = 1), options = list(dom = "t", paging = FALSE), rownames = FALSE))
        }
        model <- do.call(nlme::gls, list(gls_formula, data = data, weights = nlme::varIdent(form = ~1 | group)))
      } else {
        model <- if (length(pool_ph_dt) > 0L) {
          lm(gls_formula, data = data, singular.ok = TRUE)
        } else {
          aov_model()
        }
      }

      # Monolithic app effectively renders one decision matrix at a time
      effect <- effects_4_ph[[1]]

      emm <- emmeans::emmeans(
        model,
        formula(paste("pairwise ~", gsub(pattern = ":", replacement = "*", x = effect))),
        adjust = adjust_method,
        type = "response",
        data = data,
        level = conf
      )
      emm_model(emm)

      results <- as.data.frame(emm$contrasts)
      results$Decision <- ifelse(results$p.value <= (1 - conf), "Reject", "")
      results <- tidyr::separate(results, col = contrast, into = c("Row", "Column"), sep = " - ")

      # Replicate monolithic matrix.decision construction (produces a working DT table)
      matrix <- tidyr::pivot_wider(
        data = results[c("Row", "Column", "Decision")],
        names_from = "Column",
        values_from = "Decision"
      )
      matrix <- rbind(matrix, NA)
      matrix[nrow(matrix), 1] <- names(matrix)[ncol(matrix)]
      matrix <- as.matrix(matrix)
      row.names(matrix) <- matrix[, 1]
      matrix <- cbind(NA, matrix)
      matrix <- matrix[, -2, drop = FALSE]
      colnames(matrix)[1] <- rownames(matrix)[1]
      for (i in seq_len(nrow(matrix))) {
        for (j in seq_len(i - 1)) {
          matrix[i, j] <- matrix[j, i]
        }
      }
      matrix[is.na(matrix)] <- ""
      results <- as.list(results)
      results$matrix.decision <- matrix
      output <- results[["matrix.decision"]]

      test_name <- c(
        "Tukey", "Bonferroni Procedure", "Holm's Method",
        "Games-Howell", "Bonferroni Procedure - unequal variances", "Holm's Method - unequal variances"
      )[ph_test]

      DT::datatable(
        output,
        caption = test_name,
        options = list(
          columnDefs = list(list(className = "dt-center", targets = "_all")),
          dom = "t",
          paging = FALSE
        ),
        class = "cell-border stripe"
      )
    })

    list(
      all_effects = all_effects,
      aov_out = aov_out,
      aov_out_means = aov_out_means,
      aov_out_dispersion = aov_out_dispersion,
      ems_pooled = ems_pooled,
      ems_pooled_means = ems_pooled_means,
      ems_pooled_dispersion = ems_pooled_dispersion,
      eff_types = eff_types,
      factor_types = factor_types,
      has_fixed_factors = has_fixed_factors,
      anova_notes = reactive(report_commentary(anova_note())),
      ems_sig_effects_plot = ems_sig_effects_plot,
      significant_nested_effects = significant_nested_effects,
      significant_nested_random_effects = significant_nested_random_effects,
      nested_stratum_table_data = nested_stratum_table_data,
      nested_effect_plot = nested_effect_plot,
      nested_effect_plot_height_one = nested_effect_plot_height_one,
      nested_effects_plot_height = nested_effects_plot_height,
      ems_main_effects_plot = ems_main_effects_plot,
      interaction_plot = interaction_plot,
      posthoc_plot = posthoc_plot,
      posthoc_out_dt = posthoc_out_dt,
      aov_model = reactive(aov_model()),
      emm_model = reactive(emm_model()),
      model_mean_est = reactive(model_mean_est()),
      factorial_cell_summary = reactive({
        info <- factors_info()
        inputs_vals <- core_inputs()
        req(info, inputs_vals)
        disp_factors <- tryCatch(
          dispersion_cell_factors(info$factors_names, pooled_effects = pool_for_core(inputs_vals), available_effects = all_effects()),
          error = function(e) info$factors_names
        )
        factorial_cell_replication(info$data, disp_factors)
      })
    )
  })
}

