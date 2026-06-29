# SPSS-style homogeneous subsets grid for oneway Tukey / Games-Howell post-hoc.

#' Parse "A vs. B" labels from lolcat list.tests names.
oneway_parse_pairwise_label <- function(label) {
  parts <- strsplit(as.character(label), "\\s+vs\\.\\s+", perl = TRUE)[[1L]]
  if (length(parts) != 2L) {
    return(NULL)
  }
  trimws(parts)
}

#' Build symmetric p-value and decision matrices from contrasts list.tests.
oneway_pairwise_pmatrix_from_list_tests <- function(list.tests, labels) {
  n <- length(labels)
  if (n < 2L) {
    return(NULL)
  }
  pmat <- matrix(1, nrow = n, ncol = n, dimnames = list(labels, labels))
  dmat <- matrix("", nrow = n, ncol = n, dimnames = list(labels, labels))
  diag(pmat) <- 0
  if (is.null(list.tests) || !length(list.tests)) {
    return(list(matrix.p.value = pmat, matrix.decision = dmat))
  }
  for (nm in names(list.tests)) {
    pair <- oneway_parse_pairwise_label(nm)
    if (is.null(pair)) next
    i <- match(pair[1L], labels, nomatch = 0L)
    j <- match(pair[2L], labels, nomatch = 0L)
    if (i < 1L || j < 1L) next
    pv <- as.numeric(list.tests[[nm]]$p.value)
    if (!is.finite(pv)) next
    pmat[i, j] <- pv
    pmat[j, i] <- pv
    dmat[i, j] <- if (pv < 1) "Reject" else ""
    dmat[j, i] <- dmat[i, j]
  }
  list(matrix.p.value = pmat, matrix.decision = dmat)
}

#' All pairwise p-values >= alpha within mean-sorted block [start, end] (by label name).
oneway_block_all_non_sig <- function(pmat, sorted_labels, start, end, alpha) {
  if (start >= end) {
    return(TRUE)
  }
  block <- sorted_labels[start:end]
  if (length(block) < 2L) {
    return(TRUE)
  }
  for (a in seq_len(length(block) - 1L)) {
    for (b in seq.int(a + 1L, length(block))) {
      pv <- pmat[block[a], block[b]]
      if (!is.finite(pv) || pv < alpha) {
        return(FALSE)
      }
    }
  }
  TRUE
}

#' SPSS staircase homogeneous subset column ranges on mean-sorted groups.
oneway_homogeneous_subset_ranges <- function(pmat, labels, means, alpha) {
  n <- length(labels)
  if (n < 2L) {
    return(NULL)
  }
  labels <- as.character(labels)
  ord <- order(means, labels, method = "radix")
  sorted_labels <- labels[ord]
  ranges <- vector("list", n)
  end_prev <- 0L
  j <- 0L
  for (j in seq_len(n)) {
    start_j <- if (j == 1L) {
      1L
    } else if (end_prev < n &&
        oneway_block_all_non_sig(pmat, sorted_labels, end_prev, end_prev + 1L, alpha)) {
      end_prev
    } else {
      end_prev + 1L
    }
    if (start_j > n) {
      break
    }
    end_j <- start_j
    if (start_j < n) {
      for (i in seq.int(start_j + 1L, n)) {
        if (oneway_block_all_non_sig(pmat, sorted_labels, start_j, i, alpha)) {
          end_j <- i
        } else {
          break
        }
      }
    }
    ranges[[j]] <- c(start = start_j, end = end_j)
    end_prev <- end_j
    if (end_prev >= n) {
      break
    }
  }
  ranges[seq_len(j)]
}

ow_hsg_format_num <- function(x, digits) {
  if (exists("ro", mode = "function", inherits = TRUE)) {
    return(as.character(ro(x, digits)))
  }
  if (is.numeric(x)) {
    return(as.character(round(x, digits)))
  }
  as.character(x)
}

#' Sig. row p-value for a subset column (SPSS homogeneous subsets).
#' Multi-group columns: minimum pairwise p within the block.
#' Single-group columns: 1.000 (SPSS reports no within-subset difference).
oneway_subset_sig_pvalue <- function(pmat, sorted_labels, start, end) {
  if (start == end) {
    return(1)
  }
  if (start > end) {
    return(NA_real_)
  }
  block <- sorted_labels[start:end]
  if (length(block) < 2L) {
    return(1)
  }
  min_p <- Inf
  for (a in seq_len(length(block) - 1L)) {
    for (b in seq.int(a + 1L, length(block))) {
      pv <- pmat[block[a], block[b]]
      if (is.finite(pv) && pv < min_p) {
        min_p <- pv
      }
    }
  }
  if (!is.finite(min_p)) {
    return(NA_real_)
  }
  min_p
}

#' Pairwise p-matrix via lolcat two-group contrasts (kgroups list.tests p.value is often NaN).
oneway_pairwise_pmatrix_compute <- function(
    group_labels,
    group_means,
    group_n,
    group_var,
    ph_type,
    conf.level.familywise,
    msw,
    dfw) {
  n <- length(group_labels)
  if (n < 2L) {
    return(NULL)
  }
  labels <- as.character(group_labels)
  means <- as.numeric(group_means)
  ns <- as.numeric(group_n)
  vars <- as.numeric(group_var)
  pmat <- matrix(1, nrow = n, ncol = n, dimnames = list(labels, labels))
  diag(pmat) <- 0
  for (i in seq_len(n - 1L)) {
    for (j in seq.int(i + 1L, n)) {
      if (means[i] <= means[j]) {
        gm <- c(means[i], means[j])
        gn <- c(ns[i], ns[j])
        gv <- c(vars[i], vars[j])
      } else {
        gm <- c(means[j], means[i])
        gn <- c(ns[j], ns[i])
        gv <- c(vars[j], vars[i])
      }
      tr <- if (ph_type == 1L) {
        lolcat::contrasts.tukey.twogroups.simple(
          weight = c(-1, 1),
          group.mean = gm,
          group.sample.size = gn,
          conf.level.familywise = conf.level.familywise,
          n.means = n,
          mean.squared.error = msw,
          df.mean.squared.error = dfw
        )
      } else {
        lolcat::contrasts.games.howell.twogroups.simple(
          weight = c(-1, 1),
          group.mean = gm,
          group.variance = gv,
          group.sample.size = gn,
          conf.level.familywise = conf.level.familywise,
          n.means = n,
          mean.squared.error = NA
        )
      }
      pv <- as.numeric(tr$p.value)
      if (is.finite(pv)) {
        pmat[labels[i], labels[j]] <- pv
        pmat[labels[j], labels[i]] <- pv
      }
    }
  }
  list(matrix.p.value = pmat)
}

#' Build display data.frame for homogeneous subsets grid (includes Sig. row).
oneway_homogeneous_subsets_grid <- function(
    means,
    sample_sizes,
    labels,
    p_matrix,
    alpha,
    mse,
    df_error,
    factor_label = "Group",
    digits = 4) {
  n <- length(labels)
  if (n < 2L) {
    return(NULL)
  }
  labels <- as.character(labels)
  ord <- order(means, labels, method = "radix")
  sorted_labels <- labels[ord]
  sorted_means <- means[ord]
  sorted_n <- sample_sizes[ord]
  ranges <- oneway_homogeneous_subset_ranges(p_matrix, labels, means, alpha)
  if (is.null(ranges) || !length(ranges)) {
    return(NULL)
  }
  n_subset_cols <- length(ranges)

  empty_chr <- ""
  subset_cols <- vector("list", n_subset_cols)
  sig_vals <- rep(empty_chr, n_subset_cols)
  for (j in seq_len(n_subset_cols)) {
    col <- rep(empty_chr, n)
    rg <- ranges[[j]]
    if (!is.null(rg)) {
      rows <- rg["start"]:rg["end"]
      for (r in rows) {
        col[r] <- ow_hsg_format_num(sorted_means[r], digits)
      }
      pv <- oneway_subset_sig_pvalue(p_matrix, sorted_labels, rg["start"], rg["end"])
      sig_vals[j] <- if (is.finite(pv)) ow_hsg_format_num(pv, digits) else empty_chr
    }
    subset_cols[[j]] <- col
  }

  out <- data.frame(
    row_label = c(sorted_labels, "Sig."),
    N = c(as.character(sorted_n), empty_chr),
    stringsAsFactors = FALSE
  )
  names(out)[1L] <- factor_label
  for (j in seq_len(n_subset_cols)) {
    out[[paste0("Subset_", j)]] <- c(subset_cols[[j]], sig_vals[j])
  }
  out
}

#' DT wrapper for homogeneous subsets table.
oneway_homogeneous_subsets_datatable <- function(
    grid_df,
    caption,
    factor_label = "Group") {
  if (is.null(grid_df) || !nrow(grid_df)) {
    return(NULL)
  }
  num_cols <- setdiff(names(grid_df), c(factor_label, "N"))
  DT::datatable(
    grid_df,
    caption = caption,
    options = list(
      dom = "t",
      paging = FALSE,
      columnDefs = list(
        list(className = "dt-left", targets = 0L),
        list(className = "dt-center", targets = seq_along(num_cols) + 1L)
      )
    ),
    rownames = FALSE,
    class = "cell-border stripe"
  )
}
