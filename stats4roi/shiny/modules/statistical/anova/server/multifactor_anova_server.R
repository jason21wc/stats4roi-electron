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

create_multifactor_anova_worker <- function(id, filtered_data, core_input_values, posthoc_input_values, reactive_color_palette) {
  moduleServer(id, function(input, output, session) {
    # Inputs from coordinator (NOT namespaced to this module; passed in explicitly)
    core_inputs <- reactive({
      core_input_values()
    })
    posthoc_inputs <- reactive({
      posthoc_input_values()
    })

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

      # Make factors factors
      data[, factors_id] <- lapply(data[, factors_id], factor)

      # Optional dispersion transformation (monolithic semantics)
      if (disp) {
        formula_str <- paste(names(data)[data_col], "~", paste(factors_names, collapse = "*"))
        if (disp_type == 1) {
          data$ADA <- compute.group.dispersion.ADA(formula(formula_str), data = data)
          colnames(data)[colnames(data) == "ADA"] <- paste0(names(data)[data_col], "_ADA")
          data_col <- ncol(data)
        } else if (disp_type == 2) {
          data$ADM <- compute.group.dispersion.ADM(formula(formula_str), data = data)
          colnames(data)[colnames(data) == "ADM"] <- paste0(names(data)[data_col], "_ADM")
          data_col <- ncol(data)
        } else if (disp_type == 3) {
          data$ADMn1 <- compute.group.dispersion.ADMn1(formula(formula_str), data = data)
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
      req(factors_id, data_id)

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

      data <- info$data
      factors_id <- info$factors_id
      factors_names <- info$factors_names
      data_id <- info$data_id
      mixed_nest <- isTRUE(inputs_vals$ems_show_mixed_nest)

      if (length(factors_id) < 2) return(NULL)

      # Build full factorial formula string (used to derive model.id list)
      form_str <- paste(names(data)[data_id], "~", paste(factors_names, collapse = "*"))

      type <- matrix("F", nrow = length(factors_id))
      rownames(type) <- factors_names

      nested <- NULL
      level <- NULL
      if (mixed_nest) {
        nested <- rep("", length(factors_id))  # Initialize as vector of empty strings only when mixed_nest is TRUE
        for (i in seq_along(factors_id)) {
          # Requires coordinator to generate f_r_factor{i} and nest_factor{i}
          req(inputs_vals[[paste0("f_r_factor", i)]])
          type[i] <- inputs_vals[[paste0("f_r_factor", i)]]
          nest <- inputs_vals[[paste0("nest_factor", i)]]
          if (is.null(nest)) {
            nested[i] <- ""
          } else {
            # checkboxGroupInput returns a character vector
            # When assigning vector to single element, R pastes with spaces
            # But nested processing expects "*" separator, so we need to paste with "*"
            if (length(nest) > 1) {
              nested[i] <- paste(nest, collapse = "*")
            } else {
              nested[i] <- nest
            }
          }
        }
      }

      # Derive model.id using the same approach as monolithic EMSaov code-path
      formula.t <- as.character(formula(form_str))
      Y.name <- formula.t[2]
      data.n <- strsplit(formula.t[3], " \\+ ")[[1]]
      if (data.n[1] == ".") {
        var.list <- colnames(data)[colnames(data) != Y.name]
      } else {
        temp1 <- unlist(sapply(data.n, strsplit, " "))
        var.list <- unique(temp1[temp1 != " " & temp1 != "*" & temp1 != ""])
      }
      if (!is.null(level)) {
        sort.id <- sort.list(level)
        nested <- nested[sort.id]
        level <- level[sort.id]
        var.list <- var.list[sort.id]
      }
      if (!is.null(nested) && ifelse(length(nested) != 0, sum(!is.na(nested)), 0) != 0) {
        nested <- lapply(nested, function(x) {
          xx <- strsplit(x, split = "\\*")[[1]]
          temp <- NULL
          for (i in seq_along(xx)) temp <- c(temp, which(var.list == xx[i]))
          if (length(temp) == 0) NA else temp
        })
      } else {
        nested <- as.list(rep(NA, length(var.list)))
      }

      # Ensure factors are factors for consistent design construction
      data2 <- data[, c(var.list, Y.name)]
      for (i in var.list) data2[, i] <- factor(data2[, i])

      n <- length(var.list)
      design.M1 <- NULL
      for (i in seq_len(n)) {
        design.M1 <- rbind(design.M1, design.M1)
        temp1 <- rep(c("", var.list[i]), each = 2^(i - 1))
        design.M1 <- cbind(design.M1, temp1)
      }
      design.M1 <- design.M1[-1, , drop = FALSE]

      model.id <- c(apply(design.M1, 1, function(x) paste(paste(x[x != ""], collapse = ":"))))
      model.id <- str_sort(model.id)
      model.id <- model.id[order(str_count(model.id, ":"))]
      model.id
    })

    # -------------------------------------------------------------------------
    # aov_out(): compute ANOVA table (balanced EMS path or unbalanced alternatives)
    # Ported from monolithic aov_out reactive.
    # -------------------------------------------------------------------------
    aov_out <- reactive({
      inputs_vals <- core_inputs()
      info <- factors_info()
      req(inputs_vals, info)

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

      backup_opts <- options()
      options(contrasts = c("contr.sum", "contr.poly"))
      on.exit(options(backup_opts), add = TRUE)

      # Factor types (fixed/random) and nesting vector
      type <- matrix("F", nrow = length(factors_id))
      rownames(type) <- factors_names
      nested <- NULL
      if (mixed_nest) {
        nested <- rep("", length(factors_id))  # Initialize as vector of empty strings only when mixed_nest is TRUE
        for (i in seq_along(factors_id)) {
          req(inputs_vals[[paste0("f_r_factor", i)]])
          type[i] <- inputs_vals[[paste0("f_r_factor", i)]]
          nest <- inputs_vals[[paste0("nest_factor", i)]]
          if (is.null(nest)) {
            nested[i] <- ""
          } else {
            # checkboxGroupInput returns a character vector
            # When assigning vector to single element, R pastes with spaces
            # But nested processing expects "*" separator, so we need to paste with "*"
            if (length(nest) > 1) {
              nested[i] <- paste(nest, collapse = "*")
            } else {
              nested[i] <- nest
            }
          }
        }
      }

      # Base factorial model
      formula_str <- paste(names(data)[data_id], "~", paste(factors_names, collapse = "*"))

      # Check minimum per-cell sample size (needed for dispersion computations)
      count_per_cell <- data %>%
        group_by(across(all_of(factors_names))) %>%
        summarize(count = n(), .groups = "drop")
      min_count <- min(count_per_cell$count)

      if (disp) {
        if (min_count < 3) return(NULL) # monolithic returns NULL; coordinator shows message later
        if (disp_type == 1) {
          data$ADA <- compute.group.dispersion.ADA(formula(formula_str), data = data)
          colnames(data)[colnames(data) == "ADA"] <- paste0(names(data)[data_id], "_ADA")
          formula_str <- sub(names(data)[data_id], paste0(names(data)[data_id], "_ADA"), formula_str)
        } else if (disp_type == 2) {
          data$ADM <- compute.group.dispersion.ADM(formula(formula_str), data = data)
          colnames(data)[colnames(data) == "ADM"] <- paste0(names(data)[data_id], "_ADM")
          formula_str <- sub(names(data)[data_id], paste0(names(data)[data_id], "_ADM"), formula_str)
        } else if (disp_type == 3) {
          data$ADMn1 <- compute.group.dispersion.ADMn1(formula(formula_str), data = data)
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
        aov_model(attr(result, "aov_model"))

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
      anova_note(isolate(add_comment(anova_note(), "Unbalanced design")))
      if ("R" %in% type) {
        return("stats4ROI can't calculate unbalanced mixed or random effects models")
      }

      unbal <- inputs_vals$ems_ems
      req(unbal)
      if (!is.null(nested) && length(unique(nested)) > 1) {
        return("stats4ROI can't calculate unbalanced nested models")
      }

      if (as.numeric(unbal) == 1) {
        anova_note(isolate(add_comment(anova_note(), ", Unweighted analysis")))
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
        aov_model(lm(formula = formula(formula_str), data = data))

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
        anova_note(isolate(add_comment(anova_note(), ", Orthogonal design odd levels (reduced model)")))
        # Fractional/odd-level factorial: pass reduced formula to ems_pooled() (monolithic contract)
        effects <- setdiff(all_effects(), inputs_vals$ems_pool)
        effects <- sub(":", "*", effects)
        effects <- paste(effects, collapse = "+")
        new_formula <- paste(names(data)[data_id], "~", effects)
        return(new_formula)
      }

      # Weighted analysis (uses car::Anova in monolithic)
      anova_note(isolate(add_comment(anova_note(), ", Weighted analysis")))
      aov_mod <- lm(formula = formula(formula_str), data = data)
      aov_model(aov_mod)
      # car is not imported globally; caller must ensure dependency installed (handled later)
      aov_tab <- as.data.frame(car::Anova(aov_mod, type = 3))
      aov_tab <- aov_tab[order(rownames(aov_tab)), ]
      aov_tab <- aov_tab[order(str_count(string = row.names(aov_tab), ":"), str_count(string = row.names(aov_tab), ":")), ]
      aov_tab <- aov_tab[order(str_count(string = row.names(aov_tab), pattern = "Residuals")), ]
      aov_tab <- aov_tab[!rownames(aov_tab) %in% "(Intercept)", ]
      aov_tab$MS <- aov_tab$`Sum Sq` / aov_tab$Df
      names(aov_tab) <- c("SS", "Df", "Fvalue", "Pvalue", "MS")
      aov_tab
    })

    # -------------------------------------------------------------------------
    # ems_pooled(): parity with app_monolithic.R ems_pooled (~L30379-30793)
    # -------------------------------------------------------------------------
    ems_pooled <- reactive({
      inputs_vals <- core_inputs()
      req(inputs_vals)

      pool_vars <- inputs_vals$ems_pool
      if (is.null(pool_vars)) pool_vars <- character(0)

      info <- factors_info()
      req(info)
      data <- info$data
      factors_id <- info$factors_id
      factors_names <- info$factors_names
      data_id <- info$data_id
      conf <- inputs_vals$ems_conf
      disp <- isTRUE(inputs_vals$ems_disp)
      disp_type <- as.numeric(inputs_vals$ems_disp_type)
      primary_dummy <- inputs_vals$ems_primary_col

      reset_anova_note()

      backup_opts <- options()
      options(contrasts = c("contr.sum", "contr.poly"))
      on.exit(options(backup_opts), add = TRUE)

      count_per_cell <- data %>%
        group_by(across(all_of(factors_names))) %>%
        summarize(count = n(), .groups = "drop")
      min_count <- min(count_per_cell$count)

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
          if (min_count < 3) {
            return(NULL)
          }
          if (disp_type == 1L) {
            data$ADA <- compute.group.dispersion.ADA(formula(formula), data = data)
            colnames(data)[colnames(data) == "ADA"] <- paste0(names(data)[data_id], "_ADA")
            formula <- sub(names(data)[data_id], paste0(names(data)[data_id], "_ADA"), formula, fixed = TRUE)
            formula2 <- sub(names(data)[data_id], paste0(names(data)[data_id], "_ADA"), formula2, fixed = TRUE)
          } else if (disp_type == 2L) {
            data$ADM <- compute.group.dispersion.ADM(formula(formula), data = data)
            colnames(data)[colnames(data) == "ADM"] <- paste0(names(data)[data_id], "_ADM")
            formula <- sub(names(data)[data_id], paste0(names(data)[data_id], "_ADM"), formula, fixed = TRUE)
            formula2 <- sub(names(data)[data_id], paste0(names(data)[data_id], "_ADM"), formula2, fixed = TRUE)
          } else if (disp_type == 3L) {
            data$ADMn1 <- compute.group.dispersion.ADMn1(formula(formula), data = data)
            colnames(data)[colnames(data) == "ADMn1"] <- paste0(names(data)[data_id], "_ADMn1")
            formula <- sub(names(data)[data_id], paste0(names(data)[data_id], "_ADMn1"), formula, fixed = TRUE)
            formula2 <- sub(names(data)[data_id], paste0(names(data)[data_id], "_ADMn1"), formula2, fixed = TRUE)
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
          return(aov_out)
        }

        if (unbal == 2L) {
          anova_note(isolate(add_comment(anova_note(), "Unbalanced design due to dummy level(s)")))

          if (!validate_lm_predictors(data, formula)) {
            return(paste0(
              "Unable to fit odd-level ANOVA: one or more model terms correspond to factors with ",
              "fewer than two levels in the current data (after filtering)."
            ))
          }
          lm_effects <- safe_lm(formula, data, "odd-level ANOVA (reduced model)")
          if (is.null(lm_effects)) {
            return("Unable to fit odd-level ANOVA: linear model could not be constructed (check factor levels).")
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
            if (!validate_lm_predictors(data, formula_w_dummy_col)) {
              anova_note(isolate(add_comment(
                anova_note(),
                ", dummy-column terms omitted: one or more predictors lack two or more factor levels"
              )))
              dummy_col <- FALSE
            } else {
              lm_w_dummy_col <- safe_lm(formula_w_dummy_col, data, "dummy-column odd-level ANOVA")
              if (is.null(lm_w_dummy_col)) {
                dummy_col <- FALSE
              } else {
                aov_lm_w_dummy_col <- suppressWarnings(car::Anova(lm_w_dummy_col, type = 3, singular.ok = TRUE))
                aov_lm_w_dummy_col$`Mean Sq` <- aov_lm_w_dummy_col$`Sum Sq` / aov_lm_w_dummy_col$Df
              }
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
            data_no_factor <- filtered_data()
            names(data_no_factor) <- make.names(names(data_no_factor))
            data_id_nm <- make.names(names(data)[data_id])
            fac_nm <- make.names(factors_names)
            if (disp) {
              data_no_factor <- cbind(data_no_factor, data[, ncol(data), drop = FALSE])
              names(data_no_factor)[ncol(data_no_factor)] <- names(data)[ncol(data)]
            }
            formula2_unique <- paste(data_id_nm, "~", paste(fac_nm, collapse = "*"))
            if (!validate_lm_predictors(data_no_factor, formula2_unique)) {
              return(paste0(
                "Unable to compute unique-SS error term: one or more factors have only one level ",
                "in the data used for replication (e<sub>2</sub>)."
              ))
            }
            unique.out <- safe_lm(formula2_unique, data_no_factor, "unique-SS (e2) lm")
            if (is.null(unique.out)) {
              return("Unable to compute unique-SS error term (linear model failed).")
            }
            e2 <- stats::anova(unique.out)["Residuals", , drop = FALSE]
            e2$`Mean Sq` <- e2$`Sum Sq` / e2$Df
            e2 <- e2[c(2, 1, 4, 5, 3)]

            if (dummy_col) {
              e1 <- aov_lm_w_dummy_col["Residuals", , drop = FALSE]
              e1$`Sum Sq` <- e1$`Sum Sq` - e2$`Sum Sq`
              e1$Df <- e1$Df - e2$Df
              dum_SS <- sum(aov_lm_w_dummy_col[dummy_col_names, "Sum Sq", drop = TRUE])
              dum_df <- sum(aov_lm_w_dummy_col[dummy_col_names, "Df", drop = TRUE])
              e1$`Sum Sq` <- e1$`Sum Sq` + dum_SS
              e1$Df <- e1$Df + dum_df
            } else {
              e1 <- e_tot
              e1$`Sum Sq` <- e_tot$`Sum Sq` - e2$`Sum Sq`
              e1$Df <- e_tot$Df - e2$Df
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
          return(aov_out)
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
          return(aov_out)
        }

        return(NULL)
      }

      # Balanced design: PooledANOVA path (monolithic L30609+)
      anova_note(isolate(add_comment(anova_note(), "Balanced design")))
      if (disp && min_count < 2) {
        return(NULL)
      }

      aov_out_l <- aov_out()
      req(aov_out_l)
      if (is.character(aov_out_l) && length(aov_out_l) == 1) {
        return(aov_out_l)
      }
      if (!is.data.frame(aov_out_l)) {
        return(aov_out_l)
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

      if (any(is.na(test_pool[-nrow(test_pool), ]$SS))) {
        # Pooled has returned NAs somewhere not in residuals, use lm to get reduced model
        anova_note(isolate(add_comment(anova_note(), ", ANOVA based on reduced model")))

        # Get data and factor information
        info <- factors_info()
        req(info)
        data <- info$data
        factors_id <- info$factors_id
        factors_names <- info$factors_names
        data_id <- info$data_id

        disp <- isTRUE(inputs_vals$ems_disp)
        disp_type <- as.numeric(inputs_vals$ems_disp_type)

        # Convert factors to factor class
        data[, factors_names] <- lapply(data[, factors_id], factor)

        # Build reduced model formula by removing pooled variables
        # Line 30651-30656 in monolithic
        # Note: row.names(aov_out_original) may contain interaction terms with ":" that need to be split
        # The monolithic app uses strsplit with " " which works because rownames are space-separated
        temp <- unlist(strsplit(row.names(aov_out_original), " "))
        temp <- temp[!temp %in% pool_vars]
        temp <- temp[!temp %in% "Residuals"]
        # Remove empty strings that might result from splitting
        temp <- temp[temp != ""]
        formula_str <- paste(temp, collapse = " + ")
        formula_str <- gsub(pattern = ":", replacement = "*", x = formula_str)
        formula_str <- paste0(names(data)[data_id], " ~ ", formula_str)

        # Handle dispersion transformation if needed
        if (disp) {
          if (disp_type == 1) {
            data$ADA <- compute.group.dispersion.ADA(formula(formula_str), data = data)
            colnames(data)[colnames(data) == "ADA"] <- paste0(names(data)[data_id], "_ADA")
            formula_str <- sub(names(data)[data_id], paste0(names(data)[data_id], "_ADA"), formula_str)
          } else if (disp_type == 2) {
            data$ADM <- compute.group.dispersion.ADM(formula(formula_str), data = data)
            colnames(data)[colnames(data) == "ADM"] <- paste0(names(data)[data_id], "_ADM")
            formula_str <- sub(names(data)[data_id], paste0(names(data)[data_id], "_ADM"), formula_str)
          } else if (disp_type == 3) {
            data$ADMn1 <- compute.group.dispersion.ADMn1(formula(formula_str), data = data)
            colnames(data)[colnames(data) == "ADMn1"] <- paste0(names(data)[data_id], "_ADMn1")
            formula_str <- sub(names(data)[data_id], paste0(names(data)[data_id], "_ADMn1"), formula_str)
          }
        }

        # Fit reduced model with lm()
        options(contrasts = c("contr.sum","contr.poly"))
        aov_mod <- lm(formula = formula(formula_str), data = data)
        aov_model(aov_mod) # Cache for downstream use

        # Get count_per_cell for unreplicated check
        count_per_cell <- data %>%
          group_by(across(all_of(factors_names))) %>%
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
        formula_bal <- paste(
          names(data)[data_id], "~",
          paste(
            row.names(aov_out)[!row.names(aov_out) %in% c("Residuals", "(Intercept)")],
            collapse = "+"
          )
        )
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

      aov_out
    })

    # -------------------------------------------------------------------------
    # eff_types(): determine fixed/random status for each effect (for ICC vs ω²)
    # Ported from monolithic eff_types().
    # -------------------------------------------------------------------------
    eff_types <- reactive({
      inputs_vals <- core_inputs()
      data <- filtered_data()
      req(inputs_vals, data)

      pool <- inputs_vals$ems_pool
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

      fac_names <- names(data)[as.numeric(factors_sel)]
      if (isTRUE(inputs_vals$ems_show_mixed_nest)) {
        for (i in seq_len(n_factors)) {
          effects_f_r[which(gsub("\\(.*?\\)", "", effects_f_r$effect) == fac_names[i]), ]$type <- inputs_vals[[paste0("f_r_factor", i)]]
          if (effects_f_r$type[i] == "R") effects_f_r$type_code[i] <- 0
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

      # If any random effects are selected, this plot is not shown in monolithic
      if (isTRUE(inputs_vals$ems_show_mixed_nest)) {
        for (i in seq_along(factor_col)) {
          if (identical(inputs_vals[[paste0("f_r_factor", i)]], "R")) {
            return(message_plot("This plot can't display random effects.\nExamine the interaction plots below"))
          }
        }
      }

      # Determine which ANOVA table to use
      pool <- inputs_vals$ems_pool
      aov_out_l <- if (!is.null(pool) && length(pool) > 0) ems_pooled() else aov_out()
      req(aov_out_l)
      if (!is.data.frame(aov_out_l)) {
        return(message_plot("No ANOVA table available to plot."))
      }
      if ("(Intercept)" %in% row.names(aov_out_l)) {
        aov_out_l <- aov_out_l[!(row.names(aov_out_l) %in% "(Intercept)"), ]
      }

      # Significant effects
      sig_effects <- rownames(aov_out_l[aov_out_l$Pvalue <= (1 - conf), , drop = FALSE])
      sig_effects <- sig_effects[sig_effects != "NA"]
      if (length(sig_effects) == 0) {
        return(message_plot("No significant effects to plot."))
      }

      # Build reduced model using only significant effects (+ required main effects)
      interaction_factors <- grep(":", sig_effects, value = TRUE)
      individual_factors <- unique(unlist(strsplit(interaction_factors, ":")))
      filtered_factors <- individual_factors
      if (is.null(filtered_factors)) filtered_factors <- character(0)
      all_sig_interactions <- sig_effects[grepl(":", sig_effects)]
      sig_main <- setdiff(setdiff(sig_effects, interaction_factors), filtered_factors)

      rhs_parts <- character(0)
      if (length(filtered_factors) > 0) rhs_parts <- c(rhs_parts, filtered_factors)
      if (length(all_sig_interactions) > 0) {
        rhs_parts <- c(rhs_parts, gsub(pattern = ":", replacement = "*", x = all_sig_interactions))
      }
      if (length(sig_main) > 0) rhs_parts <- c(rhs_parts, sig_main)
      if (length(rhs_parts) == 0) {
        return(message_plot("No valid effects for reduced model."))
      }
      form_str <- paste(names(data)[data_col], "~", paste(rhs_parts, collapse = "+"))

      # Use sum-to-zero contrasts for coefficient estimates
      # MUST set options BEFORE converting factors to factors, so they use the correct contrasts
      backup_opts <- options()
      options(contrasts = c("contr.sum", "contr.poly"))
      on.exit(options(backup_opts), add = TRUE)

      # Convert factors to character first (to clear any existing contrasts), then back to factor
      # Then EXPLICITLY set contrast attributes to ensure sum-to-zero contrasts are used
      if (length(sig_main) > 0) {
        factors_to_convert <- c(filtered_factors, sig_main)
        data[factors_to_convert] <- lapply(data[factors_to_convert], as.character)
        data[factors_to_convert] <- lapply(data[factors_to_convert], factor)
        
        # EXPLICITLY set contrast attributes to sum-to-zero
        for (f in factors_to_convert) {
          if (is.factor(data[[f]])) {
            contrasts(data[[f]]) <- "contr.sum"
          }
        }
      } else if (length(filtered_factors) > 0) {
        data[filtered_factors] <- lapply(data[filtered_factors], as.character)
        data[filtered_factors] <- lapply(data[filtered_factors], factor)
        
        # EXPLICITLY set contrast attributes to sum-to-zero
        for (f in filtered_factors) {
          if (is.factor(data[[f]])) {
            contrasts(data[[f]]) <- "contr.sum"
          }
        }
      }

      model <- lm(formula = formula(form_str), data = data)
      model_mean_est(model)

      # Split significant effects into main effects and two-way interactions for plotting
      main_effects <- sig_effects[!grepl(":", sig_effects)]
      two_way_interactions <- sig_effects[grepl("^[^:]*:[^:]*$", sig_effects)]
      higher <- sig_effects[str_detect(sig_effects, ".*:.*:.*")]
      higher_present <- length(higher) > 0

      # Drop main effects that are part of two-way interactions
      if (length(main_effects) > 0 && length(two_way_interactions) > 0) {
        to_keep <- vapply(main_effects, function(x) !any(str_detect(two_way_interactions, fixed(x))), logical(1))
        main_effects <- main_effects[to_keep]
      }

      if (length(main_effects) == 0 && length(two_way_interactions) == 0) {
        return(message_plot("This graph cannot plot three-way or higher interactions.\nCheck interaction plots below."))
      }

      PANEL <- 1
      plot_data <- NULL

      if (length(main_effects) > 0) {
        for (i in main_effects) {
          plot_temp <- emmeans::emmip(model, formula(paste("~", i)), engine = "ggplot")
          main_data <- ggplot_build(plot_temp)$data[[1]]
          main_data$x <- paste(i, "=", levels(plot_temp[["data"]][[i]])[main_data$x])
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
        labs(subtitle = "Based on a reduced model using only significant effects")

      if (isTruthy(inputs_vals$ems_target)) {
        p <- p + geom_hline(yintercept = as.numeric(inputs_vals$ems_target), color = pal[2], linetype = 2)
      }
      p
    })

    # Main effects boxplots (Graphs tab)
    ems_main_effects_plot <- reactive({
      inputs_vals <- core_inputs()
      prepared <- analysis_data()
      req(inputs_vals, prepared)

      data <- prepared$data
      data_col_name <- prepared$response_name
      factors <- prepared$factors_names
      target <- inputs_vals$ems_target
      disp <- isTRUE(inputs_vals$ems_disp)
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
          aov_l <- if (!is.null(inputs_vals$ems_pool) && length(inputs_vals$ems_pool) > 0) ems_pooled() else aov_out()
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
          aov_l <- if (!is.null(inputs_vals$ems_pool) && length(inputs_vals$ems_pool) > 0) ems_pooled() else aov_out()
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

      if (isTruthy(target)) {
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
      plot_options <- ph_vals$ems_ph_plot_options
      font_size <- as.numeric(ph_vals$ph_font_size)
      target <- core_vals$ems_target

      # Show a clear message instead of returning nothing
      if (!isTruthy(ph_test) || is.na(ph_test)) {
        return(message_plot("Select a post-hoc test to generate the contrast plot."))
      }
      if (!isTruthy(ph_effects) || length(ph_effects) < 1) {
        return(message_plot("Select at least one effect for post-hoc to generate the contrast plot."))
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

      # Build model formula (respect pooling by excluding pooled effects)
      if (isTruthy(core_vals$ems_pool)) {
        effects <- setdiff(all_effects(), core_vals$ems_pool)
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
        model <- if (isTruthy(core_vals$ems_pool)) {
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
      ph_test <- as.numeric(ph_vals$ems_ph_select)
      req(data, conf, effects_4_ph, ph_test)

      adjust_method <- c("tukey", "bonferroni", "holm", "tukey", "bonferroni", "holm")[ph_test]

      # Build model formula (respect pooling)
      if (isTruthy(core_vals$ems_pool)) {
        effects <- setdiff(all_effects(), core_vals$ems_pool)
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
        model <- if (isTruthy(core_vals$ems_pool)) {
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
      ems_pooled = ems_pooled,
      eff_types = eff_types,
      anova_notes = reactive(report_commentary(anova_note())),
      ems_sig_effects_plot = ems_sig_effects_plot,
      ems_main_effects_plot = ems_main_effects_plot,
      interaction_plot = interaction_plot,
      posthoc_plot = posthoc_plot,
      posthoc_out_dt = posthoc_out_dt,
      aov_model = reactive(aov_model()),
      emm_model = reactive(emm_model()),
      model_mean_est = reactive(model_mean_est())
    )
  })
}

