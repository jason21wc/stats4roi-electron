# EMS ANOVA ROI Overrides
# Ported from app_monolithic.R (EMSanova_roi / ApproxF_roi / PooledANOVA_roi)
#
# Purpose:
# - Preserve the (bug-fixed) behavior used by the original monolithic app
# - Avoid package-side bugs (notably around nested random effects and ApproxF calls)
# - Keep these functions PURE (no reliance on monolithic global reactives)
#
# Notes vs monolithic:
# - The monolithic versions performed side-effects like `aov_model(aov_mod)`.
#   In the modular app, callers should capture the fitted model explicitly.
#   We attach the fitted model as an attribute: attr(result, "aov_model").
# - PooledANOVA_roi in monolithic called `ApproxF()`; here we call `ApproxF_roi()`.

library(stringr)

#' Calculate ANOVA table with EMS (ROI override)
#' Modified from the monolithic app to avoid package bugs and to keep results unrounded.
#'
#' @param formula model formula
#' @param data data frame for ANOVA
#' @param type vector/matrix of "F" or "R" for each factor
#' @param nested vector/list describing nesting (same semantics as monolithic)
#' @param level model level vector (optional)
#' @param approximate logical; whether to compute approximate F
#' @return data.frame (ANOVA table) with EMS column; fitted model attached as attr(., "aov_model")
EMSanova_roi <- function(formula, data, type = NULL, nested = NULL, level = NULL, approximate = FALSE) {
  Call <- match.call()
  indx <- match(c("formula", "data"), names(Call), nomatch = 0L)
  if (indx[1] == 0L) {
    stop("a 'formula' argument is required")
  }
  temp <- Call[c(1L, indx)]
  temp[[1L]] <- quote(stats::model.frame)
  eval.parent(temp) # keep monolithic side-effects/validation; result unused here
  # Terms <- attr(m, "terms") # kept in monolithic; unused here

  formula.t <- as.character(formula)
  Y.name <- formula.t[2]
  data.n <- strsplit(formula.t[3], " \\+ ")[[1]]
  if (data.n[1] == ".") {
    var.list <- colnames(data)[colnames(data) != Y.name]
  } else {
    temp1 <- unlist(sapply(data.n, strsplit, " "))
    var.list <- unique(temp1[temp1 != " " & temp1 != "*" & temp1 != ""])
  }

  # Adjust order of X variables for multi-level model
  if (!is.null(level)) {
    sort.id <- sort.list(level)
    nested <- nested[sort.id]
    level <- level[sort.id]
    var.list <- var.list[sort.id]
    type <- type[sort.id]
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

  EMSflag <- FALSE
  n.table <- NULL
  for (i in seq_along(var.list)) {
    temp <- table(data[, var.list[i]])
    if (sum(temp != mean(temp)) != 0) EMSflag <- TRUE
    n.table <- c(n.table, length(temp))
  }
  n.table <- c(
    n.table,
    mean(table(apply(data[, var.list, drop = FALSE], 1, function(x) paste(x, collapse = ""))))
  )
  if (EMSflag) {
    stop("EMSanova cannot handle the unbalanced design.")
  }

  # Change all X variables to factors
  data <- data[, c(var.list, Y.name)]
  for (i in var.list) {
    data[, i] <- factor(data[, i])
  }

  # Build design.M1
  n <- length(var.list)
  design.M1 <- NULL
  for (i in seq_len(n)) {
    design.M1 <- rbind(design.M1, design.M1)
    temp1 <- rep(c("", var.list[i]), each = 2^(i - 1))
    design.M1 <- cbind(design.M1, temp1)
  }
  design.M1 <- design.M1[-1, , drop = FALSE]

  # Full model ANOVA
  model.F <- paste(Y.name, "~", paste(apply(design.M1, 1, function(x) paste(paste(x[x != ""], collapse = "*"))), collapse = "+"))
  model.id <- c(apply(design.M1, 1, function(x) paste(paste(x[x != ""], collapse = ":"))), "Residuals")
  options(warn = -1)
  SS.table <- stats::anova(stats::lm(eval(model.F), data = data))[model.id, 1:2]
  aov_mod <- stats::lm(formula, data = data)
  # Attach the fitted model for downstream use in modular worker
  attr(SS.table, "aov_model") <- aov_mod
  # modelSS <- stats::anova(aov_mod)[, 1:2] # kept in monolithic; unused here
  options(warn = 0)

  # Treat nested
  colnames(design.M1) <- var.list
  nest.id <- which(!is.na(nested))
  if (length(nest.id) > 0) {
    for (i in seq_along(nest.id)) {
      for (j in seq_along(nested[[nest.id[i]]])) {
        temp.list <- apply(design.M1[, 1:n], 1, function(x) ifelse(sum(x == var.list[nest.id[i]]) == 0, NA, var.list[nested[[nest.id[i]]][j]]))
        del.list <- which(apply(design.M1[, 1:n], 1, function(x) sum(x == var.list[nest.id[i]]) * sum(x == var.list[nested[[nest.id[i]]][j]])) == 1)
        for (k in seq_along(del.list)) {
          comb.id <- del.list[k]
          temp.k <- design.M1[comb.id, ]
          temp.k <- temp.k[temp.k != "" & temp.k != var.list[nested[[nest.id[i]]][j]]]
          temp.k <- paste(temp.k, collapse = "")
          comb.id <- c(comb.id, which(apply(design.M1, 1, function(x) paste(x, sep = "", collapse = "")) == temp.k))
          SS.temp <- apply(SS.table[comb.id, ], 2, sum)
          SS.table[comb.id[length(comb.id)], ] <- SS.temp
        }
        design.M1 <- cbind(design.M1, temp.list)
        colnames(design.M1) <- c(colnames(design.M1)[-ncol(design.M1)], "nested")
        design.M1 <- design.M1[-del.list, ]
        SS.table <- SS.table[-del.list, ]

        # nested-nested-...
        flag <- TRUE
        id.t <- nest.id[i]
        while (flag) {
          n.id.t <- nested[[id.t]]
          temp.c <- unlist(nested[n.id.t])
          temp.c <- temp.c[!is.na(temp.c)]

          if (length(temp.c) == 0) {
            flag <- FALSE
          } else {
            for (l in seq_along(temp.c)) {
              temp.j <- ncol(design.M1)
              del.list <- which(apply(design.M1, 1, function(x) sum(x[1:n] == var.list[temp.c[l]]) * !is.na(x[temp.j])) == 1)
              design.M1 <- design.M1[-del.list, ]
              SS.temp <- apply(SS.table[del.list, ], 2, sum)
              design.M1[which(design.M1[, temp.j] == var.list[nested[[nest.id[i]]][j]]), n + temp.c[l]] <- var.list[temp.c[l]]
              sel.id <- which(design.M1[, temp.j] == var.list[nested[[nest.id[i]]][j]])
              SS.table[sel.id, ] <- SS.table[sel.id, ] + SS.temp
              SS.table <- SS.table[-del.list, ]
            }
          }
          id.t <- nested[[id.t]]
        }
      }
    }
  }

  # EMS.table
  design.M1[is.na(design.M1)] <- ""
  out <- apply(design.M1, 1, function(x) {
    ifelse(
      paste(x[-(1:n)], collapse = "") != "",
      paste(paste(x[1:n][x[1:n] != ""], collapse = ":"), "(", paste(x[-(1:n)][x[-(1:n)] != ""], collapse = "*"), ")", sep = ""),
      paste(x[1:n][x[1:n] != ""], collapse = ":")
    )
  })
  rownames(SS.table)[-nrow(SS.table)] <- out
  EMS.table <- matrix(0, ncol = length(var.list) + 1, nrow = length(out) + 1)
  colnames(EMS.table) <- c(var.list, "Error")
  rownames(EMS.table) <- c(out, "Error")
  n.EMS <- nrow(EMS.table)
  p.EMS <- ncol(EMS.table)
  EMS.table[, p.EMS] <- n.table[p.EMS]
  EMS.table[n.EMS, ] <- 1
  temp <- design.M1[, seq_along(var.list), drop = FALSE]
  temp.nest <- design.M1[, -seq_along(var.list), drop = FALSE]
  temp[temp == ""] <- NA
  for (i in seq_len(ncol(temp))) {
    if (sum(temp[, i] == var.list[i], na.rm = TRUE) != 0) {
      id.t <- which(is.na(temp[, i]))
      EMS.table[id.t, i] <- n.table[i]
      if (!is.null(type) && type[i] == "R") EMS.table[-c(id.t, n.EMS), i] <- 1
    } else {
      sel.id <- which(!is.na(temp[, i]))
      EMS.table[sel.id, which(var.list == temp[sel.id, i][1])] <- 1
    }
    if (length(nest.id) > 0) {
      for (k in seq_len(ncol(temp.nest))) EMS.table[which(temp.nest[, k] == var.list[i]), i] <- 1
    }
  }

  # EMS strings
  temp.t <- design.M1[, seq_along(var.list), drop = FALSE]
  EMS <- NULL
  n.E <- nrow(EMS.table)
  hid.flag <- NULL
  for (i in n.E:1) {
    if (i != n.E) {
      sel.id <- temp.t[i, , drop = FALSE]
      if (length(nest.id) > 0) {
        tt <- temp.nest[i, ]
        id.keep <- NULL
        for (l in seq_along(tt)) id.keep <- c(id.keep, which(names(hid.flag) == tt[l]))
      }
      hid.flag <- rep(TRUE, ncol(EMS.table))
      names(hid.flag) <- colnames(EMS.table)
      for (j in seq_along(var.list)) hid.flag[which(names(hid.flag) == sel.id[j])] <- FALSE
      pick.id <- design.M1[i, ]
      pick.id <- pick.id[pick.id != ""]
      temp.keep <- apply(design.M1, 1, function(x) {
        keep.t <- TRUE
        for (ii in seq_along(pick.id)) keep.t <- keep.t * (sum(x == pick.id[ii]) != 0)
        keep.t
      })
      if (length(nest.id) > 0) hid.flag[id.keep] <- TRUE
      temp.T <- apply(EMS.table[, hid.flag, drop = FALSE], 1, prod)
      temp.T.1 <- temp.T[temp.T != 0]
      temp.T.1[length(temp.T.1)] <- ""
      name.temp.T <- names(temp.T.1)
      temp2 <- c(temp.keep, 1)[temp.T != 0]
      nn <- length(temp.T.1)
      temp.T.1 <- temp.T.1[nn:1]
      name.temp.T <- name.temp.T[nn:1]
      temp2 <- temp2[nn:1]
      temp.EMS <- paste(temp.T.1[temp2 == 1], name.temp.T[temp2 == 1], sep = "", collapse = "+")
    } else {
      temp.EMS <- "Error"
    }
    EMS <- cbind(temp.EMS, EMS)
  }

  # ---------------------------------------------------------------------------
  # Model level + MS/F/P calculation (ported from monolithic EMSanova_roi)
  # ---------------------------------------------------------------------------
  if (!is.null(level)) {
    level.list <- sort(unique(level))
    n.L <- length(level.list)
    Model.level <- rep(level.list[n.L], nrow(SS.table) - 1)
    temp.flag <- rep(TRUE, length(Model.level))
    for (i in n.L:1) {
      i.id <- which(level == i)
      for (k in i.id) {
        Model.level[which((design.M1[, k] != "") * temp.flag == 1)] <- i
        temp.flag[design.M1[, k] != ""] <- FALSE
      }
    }
    Model.level <- c(Model.level, max(Model.level))
  } else {
    Model.level <- NULL
  }

  n.t <- nrow(SS.table)
  # If residual SS is zero, the monolithic app collapses the last term
  if (SS.table[n.t, 2] == 0) {
    SS.table[n.t, 1:2] <- SS.table[n.t - 1, 1:2]
    temp.name <- rownames(SS.table)[n.t]
    SS.table <- SS.table[-n.t, ]
    rownames(SS.table)[n.t - 1] <- temp.name
    t.EMS <- lapply(EMS, function(x) strsplit(x, "[+]")[[1]])
    del.list <- t.EMS[[n.t - 1]][-1]
    for (i in seq_along(t.EMS)) {
      keep.id <- NULL
      for (j in seq_along(del.list)) keep.id <- c(keep.id, which(t.EMS[[i]] == del.list[j]))
      if (length(keep.id) != 0) t.EMS[[i]] <- t.EMS[[i]][-keep.id]
    }
    EMS <- unlist(lapply(t.EMS, function(x) paste(x, sep = "", collapse = "+")))
    EMS[n.t - 1] <- EMS[n.t]
    EMS <- EMS[1:(n.t - 1)]
    Model.level <- Model.level[1:(n.t - 1)]
  }

  # Calculate MS, F (approx.F), P-value
  SS.table[, 3] <- SS.table[, 2] / SS.table[, 1]
  split.EMS <- lapply(EMS, function(x) strsplit(x, "[+]")[[1]])
  F.value <- NULL
  P.value <- NULL
  Signif <- NULL
  for (i in seq_len(nrow(SS.table))) {
    n.SE <- length(split.EMS[[i]])
    SS.temp <- paste(split.EMS[[i]][-n.SE], collapse = "+")
    if (sum(EMS == SS.temp) != 0) {
      F.temp <- SS.table[i, 3] / SS.table[which(EMS == SS.temp), 3]
      pValue.temp <- 1 - stats::pf(F.temp, SS.table[i, 1], SS.table[which(EMS == SS.temp), 1])
    } else if (i != nrow(SS.table) && approximate) {
      Appr.result <- ApproxF_roi(
        SS.table = data.frame(SS.table, EMS = c(EMS)),
        approx.name = rownames(SS.table)[i]
      )
      F.temp <- Appr.result$Appr.F
      pValue.temp <- Appr.result$Appr.Pvalue
    } else {
      F.temp <- NA
      pValue.temp <- NA
    }
    F.value <- c(F.value, F.temp)
    P.value <- c(P.value, pValue.temp)
    Signif <- c(Signif, "")
  }

  SS.table.t <- cbind(SS.table[, 1], SS.table[, 2], SS.table[, 3])
  colnames(SS.table.t) <- c("Df", "SS", "MS")
  if (!is.null(Model.level)) {
    tot.result <- data.frame(SS.table.t, Fvalue = F.value, Pvalue = P.value, Sig = Signif, Model.Level = Model.level, EMS = matrix(EMS))
  } else {
    tot.result <- data.frame(SS.table.t, Fvalue = F.value, Pvalue = P.value, Sig = Signif, EMS = matrix(EMS))
  }
  rownames(tot.result) <- rownames(SS.table)

  # Preserve fitted model for downstream modular worker usage
  attr(tot.result, "aov_model") <- aov_mod

  return(tot.result)
}

#' Calculate ANOVA with approximate F value (ROI override)
#' Modified to fix bug with nested random effects.
ApproxF_roi <- function(SS.table, approx.name) {
  approx.id <- NULL
  for (i in approx.name) approx.id <- c(approx.id, which(rownames(SS.table) == i))

  EMS <- as.character(SS.table$EMS)
  split.EMS <- lapply(EMS, function(x) strsplit(x, "[+]")[[1]])

  # Determine last element per row robustly for nested cases (monolithic fix)
  split.EMS.last <- list()
  rownum <- 1
  for (effect in row.names(SS.table)) {
    flat <- unlist(split.EMS[rownum])
    if (length(flat) == 1) {
      if (flat == "Error") effect_element <- "Error"
    } else {
      effect_element <- grep(
        pattern = paste0("[0-9]+\\Q", effect, "\\E(?!\\W)"),
        x = flat,
        value = TRUE,
        perl = TRUE
      )
    }
    split.EMS.last <- c(split.EMS.last, effect_element)
    rownum <- rownum + 1
  }
  split.EMS.last <- as.list(split.EMS.last)

  test.EMS <- split.EMS[[approx.id]]
  TEMP.EMS <- test.EMS[-grep(pattern = paste0("[0-9]+", approx.name), x = test.EMS)]
  keep.id <- NULL
  keep.var <- NULL
  for (kk in 2:length(TEMP.EMS)) {
    keep.id <- c(keep.id, which(split.EMS.last == TEMP.EMS[kk]))
    keep.var <- c(keep.var, TEMP.EMS[kk])
  }

  TEMP.EMS <- unlist(split.EMS[keep.id])
  TEMP.EMS <- TEMP.EMS[TEMP.EMS != "Error"]
  den.id <- names(table(TEMP.EMS))[table(TEMP.EMS) == 1]

  ms.num <- SS.table[approx.id, 3]
  ms.den <- 0
  df.num <- SS.table[approx.id, 3]^2 / SS.table[approx.id, 1]
  df.den <- 0

  for (kk in seq_along(keep.var)) {
    if (sum(keep.var[kk] == den.id) == 1) {
      id.i <- which(split.EMS.last == keep.var[kk])
      ms.den <- ms.den + SS.table[id.i, 3]
      df.den <- df.den + SS.table[id.i, 3]^2 / SS.table[id.i, 1]
    } else {
      id.i <- which(split.EMS.last == keep.var[kk])
      ms.num <- ms.num + SS.table[id.i, 3]
      df.num <- df.num + SS.table[id.i, 3]^2 / SS.table[id.i, 1]
    }
  }

  Appr.F <- ms.num / ms.den
  Appr.F.df1 <- ms.num^2 / df.num
  Appr.F.df2 <- ms.den^2 / df.den
  Appr.Pvalue <- 1 - stats::pf(Appr.F, Appr.F.df1, Appr.F.df2)
  list(Appr.F = Appr.F, df1 = Appr.F.df1, df2 = Appr.F.df2, Appr.Pvalue = Appr.Pvalue)
}

#' Pool nonsignificant interactions to Residuals (ROI override)
PooledANOVA_roi <- function(SS.table, del.ID) {
  temp.SS <- SS.table[, c("Df", "SS")]
  temp.EMS <- as.character(SS.table$EMS)
  Model.level <- SS.table$Model.Level
  temp.ID <- del.ID[del.ID != "Residuals"]
  temp.ID <- unlist(lapply(temp.ID, function(x) which(rownames(temp.SS) == x)))
  temp.EMS <- as.character(temp.EMS)

  temp.SS[nrow(temp.SS), ] <- apply(temp.SS[del.ID, ], 2, function(x) sum(x, na.rm = TRUE))
  temp.SS <- temp.SS[-temp.ID, ]
  Model.level <- Model.level[-temp.ID]

  temp.SS[, 3] <- temp.SS[, 2] / temp.SS[, 1]
  temp.split.EMS <- lapply(temp.EMS, function(x) {
    temp1 <- strsplit(x, "[+]")[[1]]
    for (i in seq_along(temp.ID)) {
      t.id <- grep(del.ID[i], temp1)
      if (length(t.id) != 0) temp1 <- temp1[-t.id]
    }
    temp1
  })

  temp.split.EMS <- temp.split.EMS[-temp.ID]
  EMS.t <- lapply(temp.split.EMS, function(x) paste(x, sep = "", collapse = "+"))

  F.value <- NULL
  P.value <- NULL
  Signif <- NULL
  for (i in seq_len(nrow(temp.SS))) {
    n.SE <- length(temp.split.EMS[[i]])
    SS.temp <- paste(temp.split.EMS[[i]][-n.SE], collapse = "+")
    test.EMS <- temp.split.EMS[[i]]
    if (sum(temp.EMS == SS.temp) != 0) {
      F.temp <- temp.SS[i, 3] / temp.SS[which(EMS.t == SS.temp), 3]
      pValue.temp <- 1 - stats::pf(F.temp, temp.SS[i, 1], temp.SS[which(EMS.t == SS.temp), 1])
    } else if (i != nrow(temp.SS) && length(test.EMS) != 1) {
      Appr.result <- ApproxF_roi(data.frame(temp.SS, EMS = unlist(EMS.t)), rownames(temp.SS)[i])
      F.temp <- Appr.result$Appr.F
      pValue.temp <- Appr.result$Appr.Pvalue
    } else {
      F.temp <- NA
      pValue.temp <- NA
    }

    if (!is.na(pValue.temp)) {
      if (pValue.temp <= 0.001) {
        Signif.temp <- "***"
      } else if (pValue.temp <= 0.01) {
        Signif.temp <- "**"
      } else if (pValue.temp <= 0.05) {
        Signif.temp <- "*"
      } else if (pValue.temp <= 0.1) {
        Signif.temp <- "."
      } else {
        Signif.temp <- ""
      }
    } else {
      Signif.temp <- ""
      # Keep columns numeric; HTML renderers can display blanks for NA
      pValue.temp <- NA_real_
      F.temp <- NA_real_
    }
    F.value <- c(F.value, F.temp)
    P.value <- c(P.value, pValue.temp)
    Signif <- c(Signif, Signif.temp)
  }

  SS.table.t <- cbind(
    temp.SS[, 1],
    round(temp.SS[, 2], 4),
    round(temp.SS[, 3], 4)
  )
  colnames(SS.table.t) <- c("Df", "SS", "MS")
  EMS.t <- as.character(EMS.t)
  if (!is.null(Model.level)) {
    tot.result <- data.frame(SS.table.t, Fvalue = F.value, Pvalue = P.value, Sig = Signif, Model.Level = Model.level, EMS = matrix(EMS.t))
  } else {
    tot.result <- data.frame(SS.table.t, Fvalue = F.value, Pvalue = P.value, Sig = Signif, EMS = matrix(EMS.t))
  }
  rownames(tot.result) <- rownames(temp.SS)
  tot.result
}

