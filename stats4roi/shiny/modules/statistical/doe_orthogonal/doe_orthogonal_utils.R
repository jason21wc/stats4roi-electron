# DOE Orthogonal Array Utilities (Tsui 1988)
# Feasibility, column assignment, run sheet, alias table, recoding.
# CSP-style constraints: triangle/confound lookups, tier rules, Das-style necessary failures.

source("modules/statistical/doe_orthogonal/doe_orthogonal_data.R")

# L81: required 2fis need Taguchi confound table in doe_orthogonal_data.R (interaction_table_complete).
.doe_l81_interactions_ready <- function() {
  inf <- DOE_OA_3LEVEL[["L81"]]
  if (is.null(inf)) return(FALSE)
  if (isTRUE(inf$interaction_table_complete)) return(TRUE)
  cf <- inf$confound
  any(vapply(cf, function(z) any(grepl("x", z, fixed = TRUE)), logical(1L)))
}

# Das et al. (2006): on OA(8), two two-factor interactions with no common factor cannot be optimally accommodated.
.doe_das_l8_two_disjoint_2fi_impossible <- function(oa_label, interaction_pairs) {
  if (!identical(oa_label, "L8") || length(interaction_pairs) < 2L) return(FALSE)
  for (i in seq_along(interaction_pairs))
    for (j in seq_along(interaction_pairs)) {
      if (i >= j) next
      p1 <- interaction_pairs[[i]]
      p2 <- interaction_pairs[[j]]
      if (length(unique(c(p1[1L], p1[2L], p2[1L], p2[2L]))) == 4L) return(TRUE)
    }
  FALSE
}

# ---- Optional profiling (set options(doe.assign.profile = TRUE)) ----
.doe_assign_profile_enabled <- function() {
  isTRUE(getOption("doe.assign.profile", FALSE))
}

# Adjacency: factors that share a required 2fi with f
.doe_2fi_neighbors <- function(n_factors, interaction_pairs) {
  nb <- vector("list", n_factors)
  for (p in interaction_pairs) {
    a <- p[1L]; b <- p[2L]
    nb[[a]] <- c(nb[[a]], b)
    nb[[b]] <- c(nb[[b]], a)
  }
  nb
}

# Logical mask: columns that are 2fi of two assigned factors (not valid as new main)
.doe_reserved_main_mask <- function(factor_col, tri, n_cols, n_factors) {
  irr <- logical(n_cols)
  for (i in seq_len(n_factors - 1L)) {
    ci <- factor_col[i]
    if (ci == 0L) next
    for (j in (i + 1L):n_factors) {
      cj <- factor_col[j]
      if (cj == 0L) next
      cc <- tri[min(ci, cj), max(ci, cj)]
      if (!is.na(cc) && cc >= 1L && cc <= n_cols) irr[cc] <- TRUE
    }
  }
  irr
}

# For Resolution V/IV we forbid parking a new main on tri(i,j) of two assigned mains — each
# such column "belongs" to that 2fi in a clear layout. For Resolution III, that rule is too
# strong: a main may occupy a column that is algebraically another pair's 2fi (intentional
# aliasing), which humans and tools like Taguchi DOS often use. Required 2fis are still
# enforced via used[] and col_2fi_owner in check_2fi / commit.
.doe_reserved_main_slots <- function(tier, factor_col, tri, n_cols, n_factors) {
  if (identical(tier, "III")) return(logical(n_cols))
  .doe_reserved_main_mask(factor_col, tri, n_cols, n_factors)
}

# Fast tier skip (necessary conditions only)
.doe_2level_tier_impossible <- function(tier, n_factors, n_cols, n_pairs, res_iv, res_v_2fi) {
  if (n_factors + n_pairs > n_cols) return(TRUE)
  len_iv <- length(res_iv)
  if (tier %in% c("V", "IV") && n_factors > len_iv) return(TRUE)
  if (identical(tier, "V")) {
    if (length(res_v_2fi) == 0L) return(TRUE)
    if (n_pairs > length(res_v_2fi)) return(TRUE)
  }
  FALSE
}

# ---- Column count for a factor level (2-level OA) ----
# 2->1, 3->3, 4->3, 5/6/7/8->7
columns_for_level_2level <- function(levels) {
  if (levels <= 2L) return(1L)
  if (levels <= 4L) return(3L)
  return(7L)
}

# ---- Column count for a factor level (3-level OA) ----
# 3->1, 9->2
columns_for_level_3level <- function(levels) {
  if (levels <= 3L) return(1L)
  return(2L)
}

# ---- Feasibility: which arrays can accommodate the requirement set ----
# n_factors: integer
# levels_per_factor: integer vector (2-8 for 2-level OA, 3 or 9 for 3-level)
# required_interactions: list of length-2 integer vectors (factor indices, 1-based), or character "AxB" form
# Returns: list(possible_2level, possible_3level, possible_mixed)
# possible_mixed: L18 when 1 two-level + up to 7 three-level factors, main effects only.
doe_feasible_arrays <- function(n_factors, levels_per_factor, required_interactions) {
  use_2level <- all(levels_per_factor >= 2L & levels_per_factor <= 8L)
  use_3level <- any(levels_per_factor == 3L | levels_per_factor == 9L)

  possible_2level <- character(0)
  possible_3level <- character(0)
  possible_mixed <- character(0)

  if (use_2level) {
    main_cols <- sum(vapply(levels_per_factor, columns_for_level_2level, integer(1)))
    # Use interaction degrees-of-freedom as a lower bound for required interaction capacity.
    # This prevents under-counting (e.g., 3x3 interactions) from admitting infeasible 2-level OAs.
    n_int_cols <- 0L
    for (p in required_interactions) {
      if (length(p) != 2L) next
      i <- as.integer(p[1L]); j <- as.integer(p[2L])
      if (is.na(i) || is.na(j) || i < 1L || j < 1L || i > length(levels_per_factor) || j > length(levels_per_factor) || i == j) next
      n_int_cols <- n_int_cols + (as.integer(levels_per_factor[i]) - 1L) * (as.integer(levels_per_factor[j]) - 1L)
    }
    total_needed <- main_cols + n_int_cols
    # Possible = array has enough columns (necessary condition). Assignment is run when user clicks "Design the Experiment".
    for (label in c("L4", "L8", "L16", "L32", "L64")) {
      info <- DOE_OA_2LEVEL[[label]]
      if (!is.null(info) && info$n_cols >= total_needed) possible_2level <- c(possible_2level, label)
    }
    # L12: main effects only (no 2fi); feasible when total_needed <= 11 and no interactions
    if (length(required_interactions) == 0L) {
      info <- DOE_OA_2LEVEL[["L12"]]
      if (!is.null(info) && main_cols <= 11L) possible_2level <- c(possible_2level, "L12")
    }
  }

  if (use_3level) {
    main_cols <- sum(vapply(levels_per_factor, columns_for_level_3level, integer(1)))
    n_int_cols <- length(required_interactions) * 2L
    total_needed <- main_cols + n_int_cols
    # Possible = array has enough columns (necessary condition). Assignment is run when user clicks "Design the Experiment".
    for (label in c("L9", "L27", "L81")) {
      info <- DOE_OA_3LEVEL[[label]]
      if (is.null(info) || info$n_cols < total_needed) next
      if (identical(label, "L81") && length(required_interactions) > 0L && !.doe_l81_interactions_ready()) next
      possible_3level <- c(possible_3level, label)
    }
  }

  # L18: 1 two-level column + 7 three-level columns; main effects only
  if (length(required_interactions) == 0L && n_factors >= 1L && n_factors <= 8L) {
    n_2 <- sum(levels_per_factor == 2L)
    n_3 <- sum(levels_per_factor == 3L)
    if (n_2 <= 1L && n_3 <= 7L && (n_2 + n_3) == n_factors) {
      if (!is.null(DOE_OA_MIXED[["L18"]])) possible_mixed <- "L18"
    }
  }

  list(possible_2level = possible_2level, possible_3level = possible_3level, possible_mixed = possible_mixed)
}

# ---- Find column(s) that represent interaction col_a x col_b ----
# 2-level: use triangle table (TRIANGLE_L64 in data; leading submatrix for L4/L8/L16/L32). User's table: e.g. 1x15 -> 14.
# 3-level: use confounding table (each 2fi in two columns).
get_interaction_columns <- function(oa_type, col_a, col_b) {
  a <- min(col_a, col_b)
  b <- max(col_a, col_b)
  if (oa_type %in% c("L4", "L8", "L12", "L16", "L32", "L64")) {
    info <- DOE_OA_2LEVEL[[oa_type]]
    if (is.null(info)) return(integer(0))
    n_cols <- info$n_cols
    if (a < 1L || b < 1L || a > n_cols || b > n_cols || a == b) return(integer(0))
    if (oa_type == "L12" && (a > 11L || b > 11L)) return(integer(0))
    c <- TRIANGLE_L64[a, b]
    if (is.na(c) || c < 1L || c > n_cols) return(integer(0))
    return(as.integer(c))
  }
  if (oa_type %in% c("L9", "L27", "L81")) {
    info <- DOE_OA_3LEVEL[[oa_type]]
    if (is.null(info)) return(integer(0))
    pair <- paste0(a, "x", b)
    out <- integer(0)
    for (c in seq_along(info$confound)) {
      if (pair %in% info$confound[[c]]) out <- c(out, c)
    }
    return(out)
  }
  # L18: mixed array, no 2fi column assignment
  integer(0)
}

# Expand independent generators to all OA columns for that factor (generators + internal folds).
# Matches inner columns_for_main_set in .doe_assign_2level_backtrack (2-level OA only).
doe_columns_for_main_set_2level <- function(main_set, tri, n_cols) {
  main_set <- as.integer(main_set)
  if (length(main_set) <= 1L) return(main_set)
  tri <- tri[seq_len(n_cols), seq_len(n_cols), drop = FALSE]
  if (length(main_set) == 2L) {
    c12 <- tri[min(main_set[1L], main_set[2L]), max(main_set[1L], main_set[2L])]
    return(sort(unique(c(main_set[1L], main_set[2L], c12))))
  }
  if (length(main_set) == 3L) {
    c12 <- tri[min(main_set[1L], main_set[2L]), max(main_set[1L], main_set[2L])]
    c13 <- tri[min(main_set[1L], main_set[3L]), max(main_set[1L], main_set[3L])]
    c23 <- tri[min(main_set[2L], main_set[3L]), max(main_set[2L], main_set[3L])]
    # Three-way interaction column (g1*g2*g3) for embedded 2^3 factorial
    c123 <- tri[min(c12, main_set[3L]), max(c12, main_set[3L])]
    return(sort(unique(c(main_set[1L], main_set[2L], main_set[3L], c12, c13, c23, c123))))
  }
  main_set
}

# Next power of two >= levels (embedding size for 2^k OA columns). Levels are capped at 8 in the UI.
doe_embedded_level_count_2level <- function(levels) {
  lev <- as.integer(levels)[1L]
  if (is.na(lev) || lev <= 1L) return(1L)
  lev <- min(lev, 8L)
  if (lev <= 2L) return(2L)
  2L^ceiling(log2(lev))
}

# Number of id columns to drop: embed in 2^k, then drop (2^k - levels) columns from the highest OA indices.
doe_n_id_columns_2level <- function(levels) {
  lev <- as.integer(levels)[1L]
  if (is.na(lev) || lev < 2L) return(0L)
  lev <- min(lev, 8L)
  emb <- doe_embedded_level_count_2level(lev)
  as.integer(max(0L, emb - lev))
}

# Columns of the other factor used when reserving cross-factor 2fi: full 2^k expansion minus id
# columns. Id columns are the n_id highest OA column numbers among this factor's columns (set
# aside for optional follow-up tests). levels capped at 8.
doe_structural_columns_for_cross_2level <- function(main_set, levels, tri, n_cols) {
  main_set <- as.integer(main_set)
  if (length(main_set) <= 1L) return(main_set)
  lev <- if (is.null(levels)) NA_integer_ else min(as.integer(levels)[1L], 8L)
  full <- doe_columns_for_main_set_2level(main_set, tri, n_cols)
  if (is.na(lev) || lev < 2L) return(sort(unique(full)))
  n_drop <- doe_n_id_columns_2level(lev)
  if (n_drop <= 0L) return(sort(unique(full)))
  fu <- sort(unique(full), decreasing = TRUE)
  n_take <- min(n_drop, length(fu))
  id <- fu[seq_len(n_take)]
  structural <- full[!(full %in% id)]
  structural <- sort(unique(structural))
  if (length(structural) == 0L) return(main_set)
  structural
}

# Required 2fi between factors on 2-level OAs: reserve Taguchi column for each pair
# (generators of one) x (structural columns of the other per doe_structural_columns_for_cross_2level), both ways.
# Example L16: A at 3 levels on gens 4,8 — id = highest OA column in {4,8,12} (12); B at 4 levels (full 1,2,3).
# -> 4,8 x {1,2,3} and 1,2 x {4,8} -> columns 5,6,7,9,10,11 only.
doe_interaction_columns_cross_generators_2level <- function(main_set_i, main_set_j, tri, n_cols,
                                                           levels_i = NULL, levels_j = NULL) {
  tri <- tri[seq_len(n_cols), seq_len(n_cols), drop = FALSE]
  gen_i <- as.integer(main_set_i)
  gen_j <- as.integer(main_set_j)
  full_i <- doe_structural_columns_for_cross_2level(gen_i, levels_i, tri, n_cols)
  full_j <- doe_structural_columns_for_cross_2level(gen_j, levels_j, tri, n_cols)
  out <- integer(0)
  for (a in gen_i) {
    for (b in full_j) {
      if (a == b) next
      cc <- tri[min(a, b), max(a, b)]
      if (!is.na(cc) && cc >= 1L && cc <= n_cols) out <- c(out, cc)
    }
  }
  for (a in gen_j) {
    for (b in full_i) {
      if (a == b) next
      cc <- tri[min(a, b), max(a, b)]
      if (!is.na(cc) && cc >= 1L && cc <= n_cols) out <- c(out, cc)
    }
  }
  sort(unique(out))
}

# ---- Catalog: precomputed factor-column assignments for common (oa, n_factors, 2fi) ----
# Key: list(oa_label, n_factors, pairs_str). Value: integer vector factor_col (primary column per factor).
# pairs_str = paste(sort(paste0(min(p),"x",max(p)) for p in interaction_pairs), collapse = ",")
.doe_oa_catalog_key <- function(oa_label, n_factors, interaction_pairs) {
  if (length(interaction_pairs) == 0L) return(NULL)
  pairs_str <- paste(sort(vapply(interaction_pairs, function(p) paste0(min(p), "x", max(p)), character(1))), collapse = ",")
  list(oa = oa_label, n = n_factors, pairs = pairs_str)
}

# Precomputed solutions (Taguchi column indices). Add more as needed.
DOE_OA_ASSIGN_CATALOG <- list(
  # L8, 4 factors, AB AC AD (1x2, 1x3, 1x4): A=1, B=2, C=4, D=7 -> 2fi in 3,5,6
  list(key = list(oa = "L8", n = 4L, pairs = "1x2,1x3,1x4"), factor_col = c(1L, 2L, 4L, 7L)),
  # L8, 3 factors, AB (1x2)
  list(key = list(oa = "L8", n = 3L, pairs = "1x2"), factor_col = c(1L, 2L, 3L)),
  # L8, 5 factors, AB AC (1x2, 1x3): A=1,B=2,C=3; D,E to 5,6 (exclude 4=12, 7=13)
  list(key = list(oa = "L8", n = 5L, pairs = "1x2,1x3"), factor_col = c(1L, 2L, 3L, 5L, 6L)),
  # L8, 4 factors, AB AC BC (1x2, 1x3, 2x3): A=1,B=2,C=3,D=7
  list(key = list(oa = "L8", n = 4L, pairs = "1x2,1x3,2x3"), factor_col = c(1L, 2L, 3L, 7L))
)

.doe_lookup_catalog <- function(oa_label, n_factors, interaction_pairs) {
  if (length(interaction_pairs) == 0L) return(NULL)
  k <- .doe_oa_catalog_key(oa_label, n_factors, interaction_pairs)
  for (e in DOE_OA_ASSIGN_CATALOG) {
    if (identical(e$key$oa, k$oa) && identical(e$key$n, k$n) && identical(e$key$pairs, k$pairs))
      return(e$factor_col)
  }
  NULL
}

# ---- Phase-1 assignment: primary columns only (2-level factors). Fast backtrack over single column per factor. ----
# Returns list(assignment_list, factor_col, design_resolution, profile = optional). Used when all main_cols_needed==1.
# Optimizations: adjacency lists, reserved-main mask once per node, MRV factor choice, bounded fail memo, tier pre-checks.
.doe_assign_2level_phase1 <- function(oa_label, n_factors, interaction_pairs, n_cols, res_iv, res_v_2fi) {
  if (!exists("TRIANGLE_L64", inherits = TRUE)) return(NULL)
  tri <- get("TRIANGLE_L64", inherits = TRUE)
  tri <- tri[seq_len(n_cols), seq_len(n_cols), drop = FALSE]
  degree <- integer(n_factors)
  for (p in interaction_pairs) {
    degree[p[1L]] <- degree[p[1L]] + 1L
    degree[p[2L]] <- degree[p[2L]] + 1L
  }
  neighbors <- .doe_2fi_neighbors(n_factors, interaction_pairs)
  n_pairs <- length(interaction_pairs)
  factor_col <- integer(n_factors)
  used <- integer(n_cols)
  col_2fi_owner <- character(n_cols)
  profile_on <- .doe_assign_profile_enabled()
  nodes <- 0L
  MAX_PHASE1_NODES <- if (n_factors >= 8L) 50e6L else 10e6L
  t_start <- if (profile_on) proc.time() else NULL

  # Bounded failure memo (partial states that cannot extend to solution); env reset per tier below
  MAX_MEMO_FAIL <- 8000L
  memo_fail_n <- 0L
  .memo_key <- function(tier) {
    paste0(tier, "|", paste(factor_col, collapse = "."), "|", paste(used, collapse = ""))
  }
  .memo_fail_remember <- function(tier) {
    if (memo_fail_n >= MAX_MEMO_FAIL) return()
    k <- .memo_key(tier)
    if (exists(k, envir = memo_fail, inherits = FALSE)) return()
    assign(k, TRUE, envir = memo_fail)
    memo_fail_n <<- memo_fail_n + 1L
  }

  # Tsui: res_iv first, then other free columns ascending (tier III).
  candidate_cols <- function(tier, reserved_main) {
    free <- (used == 0L) & !reserved_main
    if (tier == "V" || tier == "IV") {
      cand <- res_iv[free[res_iv]]
      cand <- cand[!is.na(cand)]
      as.integer(cand)
    } else {
      iv <- res_iv[free[res_iv]]
      iv <- iv[!is.na(iv)]
      oth <- which(free & !(seq_len(n_cols) %in% res_iv))
      c(as.integer(iv), as.integer(oth))
    }
  }

  check_2fi <- function(f, c, tier, reserved_main) {
    if (c <= n_cols && reserved_main[c]) return(FALSE)
    cc_list <- integer(0)
    for (other in neighbors[[f]]) {
      if (factor_col[other] == 0L) next
      co <- factor_col[other]
      ccs <- doe_interaction_columns_cross_generators_2level(c(c), c(co), tri, n_cols, 2L, 2L)
      if (length(ccs) == 0L) return(FALSE)
      key <- if (f < other) paste0(f, "x", other) else paste0(other, "x", f)
      for (cc in ccs) {
        if (is.na(cc) || cc < 1L || cc > n_cols) return(FALSE)
        if (tier == "V" && !(cc %in% res_v_2fi)) return(FALSE)
        if (used[cc] != 0L && col_2fi_owner[cc] != key) return(FALSE)
        if (nzchar(col_2fi_owner[cc]) && col_2fi_owner[cc] != key) return(FALSE)
        cc_list <- c(cc_list, cc)
      }
    }
    if (length(cc_list) != length(unique(cc_list))) return(FALSE)
    TRUE
  }

  commit <- function(f, c) {
    factor_col[f] <<- c
    used[c] <<- 1L
    set_2fi <- list()
    for (other in neighbors[[f]]) {
      if (factor_col[other] == 0L) next
      co <- factor_col[other]
      ccs <- unique(doe_interaction_columns_cross_generators_2level(c(c), c(co), tri, n_cols, 2L, 2L))
      key <- if (f < other) paste0(f, "x", other) else paste0(other, "x", f)
      for (cc in ccs) {
        if (is.na(cc) || cc < 1L || cc > n_cols) next
        col_2fi_owner[cc] <<- key
        used[cc] <<- 1L
        set_2fi[[length(set_2fi) + 1L]] <- list(col = cc, key = key)
      }
    }
    set_2fi
  }

  undo_commit <- function(f, c, set_2fi) {
    factor_col[f] <<- 0L
    used[c] <<- 0L
    for (s in set_2fi) {
      col_2fi_owner[s$col] <<- ""
      used[s$col] <<- 0L
    }
  }

  # Pinned columns: sole valid main column for another unassigned factor's required 2fi with assigned partner
  pinned_for_target <- function(f_target) {
    pinned <- integer(0)
    unass <- which(factor_col == 0L)
    for (g in unass) {
      if (g == f_target) next
      for (other in neighbors[[g]]) {
        if (factor_col[other] == 0L) next
        col_d <- factor_col[other]
        opts <- integer(0)
        for (c in seq_len(n_cols)) {
          if (used[c] != 0L) next
          cc <- tri[min(c, col_d), max(c, col_d)]
          if (!is.na(cc) && cc >= 1L && cc <= n_cols && used[cc] == 0L) opts <- c(opts, c)
        }
        if (length(opts) == 1L) pinned <- c(pinned, opts[1L])
      }
    }
    unique(pinned)
  }

  # MRV: next unassigned factor with fewest valid columns; tie-break higher degree
  pick_next_factor <- function(tier, use_pinning = TRUE) {
    unass <- which(factor_col == 0L)
    if (length(unass) == 0L) return(NA_integer_)
    reserved_main <- .doe_reserved_main_slots(tier, factor_col, tri, n_cols, n_factors)
    base_cand <- candidate_cols(tier, reserved_main)
    best_f <- unass[1L]
    best_n <- .Machine$integer.max
    for (g in unass) {
      cand <- base_cand
      pin <- if (isTRUE(use_pinning)) pinned_for_target(g) else integer(0)
      if (length(pin) > 0L) cand <- cand[!(cand %in% pin)]
      n_ok <- 0L
      for (cc in cand) {
        if (check_2fi(g, cc, tier, reserved_main)) n_ok <- n_ok + 1L
      }
      if (n_ok < best_n || (n_ok == best_n && degree[g] > degree[best_f])) {
        best_n <- n_ok
        best_f <- g
      }
    }
    if (best_n == 0L) NA_integer_ else best_f
  }

  build_assignment <- function(tier) {
    assig <- list()
    for (f in seq_len(n_factors)) {
      c <- factor_col[f]
      assig[[length(assig) + 1L]] <- list(factor = f, columns = c, type = "main")
    }
    for (c in seq_len(n_cols)) {
      key <- col_2fi_owner[c]
      if (!nzchar(key)) next
      assig[[length(assig) + 1L]] <- list(factor = key, columns = as.integer(c), type = "interaction")
    }
    list(assignment_list = assig, factor_col = factor_col, design_resolution = tier)
  }

  recurse <- function(tier, use_pinning = TRUE) {
    nodes <<- nodes + 1L
    if (nodes > MAX_PHASE1_NODES) return(NULL)
    if (exists(.memo_key(tier), envir = memo_fail, inherits = FALSE)) return(NULL)
    if (all(factor_col > 0L)) return(build_assignment(tier))
    f <- pick_next_factor(tier, use_pinning = use_pinning)
    if (is.na(f)) {
      .memo_fail_remember(tier)
      return(NULL)
    }
    reserved_main <- .doe_reserved_main_slots(tier, factor_col, tri, n_cols, n_factors)
    cand <- candidate_cols(tier, reserved_main)
    pin <- if (isTRUE(use_pinning)) pinned_for_target(f) else integer(0)
    if (length(pin) > 0L) cand <- cand[!(cand %in% pin)]
    ok <- logical(length(cand))
    for (i in seq_along(cand)) ok[i] <- check_2fi(f, cand[i], tier, reserved_main)
    cand <- cand[ok]
    if (length(cand) == 0L) {
      .memo_fail_remember(tier)
      return(NULL)
    }
    for (c in cand) {
      set_2fi <- commit(f, c)
      result <- recurse(tier, use_pinning = use_pinning)
      undo_commit(f, c, set_2fi)
      if (!is.null(result)) return(result)
    }
    .memo_fail_remember(tier)
    NULL
  }

  n_res_iv <- length(res_iv)
  tiers_to_try <- if (n_factors > n_res_iv) "III" else c("V", "IV", "III")
  out <- NULL
  for (use_pinning in c(TRUE, FALSE)) {
    for (tier in tiers_to_try) {
      if (identical(tier, "V") && (is.null(res_v_2fi) || length(res_v_2fi) == 0L)) next
      if (.doe_2level_tier_impossible(tier, n_factors, n_cols, n_pairs, res_iv, res_v_2fi)) next
      factor_col[] <- 0L
      used[] <- 0L
      col_2fi_owner[] <- ""
      memo_fail <- new.env(parent = emptyenv())
      memo_fail_n <- 0L
      nodes <- 0L
      result <- recurse(tier, use_pinning = use_pinning)
      if (!is.null(result)) {
        out <- result
        break
      }
    }
    if (!is.null(out)) break
  }
  if (is.null(out)) return(NULL)
  if (profile_on) {
    out$profile <- list(nodes = nodes, elapsed_sec = (proc.time() - t_start)[3L], oa_label = oa_label)
  }
  out
}

# ---- Backtracking assignment for 2-level OAs (distinct 2fi columns, resolution-ordered) ----
# Used when required_interactions is non-empty. Tries Resolution V, then IV, then III.
# Returns list(assignment_list, factor_col, design_resolution) or NULL if no valid assignment.
.doe_assign_2level_backtrack <- function(oa_label, n_factors, levels_per_factor, interaction_pairs,
                                         main_cols_needed, n_cols, res_iv, res_v_2fi) {
  tri <- TRIANGLE_L64[seq_len(n_cols), seq_len(n_cols), drop = FALSE]
  degree <- integer(n_factors)
  for (p in interaction_pairs) {
    degree[p[1L]] <- degree[p[1L]] + 1L
    degree[p[2L]] <- degree[p[2L]] + 1L
  }
  factor_order <- order(-degree, seq_len(n_factors))
  neighbors <- .doe_2fi_neighbors(n_factors, interaction_pairs)
  n_pairs <- length(interaction_pairs)

  backtrack_nodes <- 0L
  MAX_BACKTRACK_NODES <- if (n_factors >= 8L) 50e6L else 10e6L
  profile_on <- .doe_assign_profile_enabled()
  t_start <- if (profile_on) proc.time() else NULL
  tier <- "III" # updated each outer iteration; read by forward_check_ok / recurse

  forward_check_ok <- function() {
    rsv <- .doe_reserved_main_slots(tier, factor_col, tri, n_cols, n_factors)
    cand <- candidate_main_cols(tier, rsv)
    for (g in seq_len(n_factors)) {
      if (factor_col[g] != 0L) next
      need <- main_cols_needed[g]
      if (need == 1L) {
        found <- FALSE
        for (c in cand) {
          if (check_2fi_ok(g, c, c, tier, rsv)) { found <- TRUE; break }
        }
        if (!found) return(FALSE)
      } else if (need == 3L && length(cand) < 2L) return(FALSE)
      else if (need == 7L && length(cand) < 3L) return(FALSE)
    }
    TRUE
  }

  factor_col <- integer(n_factors)
  main_set_per_factor <- vector("list", n_factors)
  used <- integer(n_cols)
  col_2fi_owner <- character(n_cols)

  candidate_main_cols <- function(tier, reserved_main = NULL) {
    if (is.null(reserved_main))
      reserved_main <- .doe_reserved_main_slots(tier, factor_col, tri, n_cols, n_factors)
    free <- (used == 0L) & !reserved_main
    if (tier == "V" || tier == "IV") {
      cand <- res_iv[free[res_iv]]
      cand <- cand[!is.na(cand)]
      as.integer(cand)
    } else {
      iv <- res_iv[free[res_iv]]
      iv <- iv[!is.na(iv)]
      oth <- which(free & !(seq_len(n_cols) %in% res_iv))
      c(as.integer(iv), as.integer(oth))
    }
  }

  columns_for_main_set <- function(main_set) {
    doe_columns_for_main_set_2level(as.integer(main_set), tri, n_cols)
  }

  check_2fi_ok <- function(f, primary, main_set, tier, reserved_main = NULL) {
    if (is.null(reserved_main))
      reserved_main <- .doe_reserved_main_slots(tier, factor_col, tri, n_cols, n_factors)
    ms <- as.integer(main_set)
    if (any(ms >= 1L & ms <= n_cols & reserved_main[ms])) return(FALSE)
    cc_list <- integer(0)
    for (other in neighbors[[f]]) {
      if (factor_col[other] == 0L) next
      if (is.na(other) || other < 1L || other > length(main_set_per_factor)) {
        return(FALSE)
      }
      ms_other <- main_set_per_factor[[other]]
      if (is.null(ms_other)) next
      ccs <- doe_interaction_columns_cross_generators_2level(ms, ms_other, tri, n_cols,
                                                             levels_per_factor[f], levels_per_factor[other])
      if (length(ccs) == 0L) return(FALSE)
      key <- if (f < other) paste0(f, "x", other) else paste0(other, "x", f)
      for (cc in ccs) {
        if (is.na(cc) || cc < 1L || cc > n_cols) return(FALSE)
        if (tier == "V" && !(cc %in% res_v_2fi)) return(FALSE)
        if (used[cc] != 0L && col_2fi_owner[cc] != key) return(FALSE)
        if (nzchar(col_2fi_owner[cc]) && col_2fi_owner[cc] != key) return(FALSE)
        cc_list <- c(cc_list, cc)
      }
    }
    if (length(cc_list) != length(unique(cc_list))) return(FALSE)
    TRUE
  }

  # Enumerate candidate main sets for factor f: list of main_set (vector of 1, 2, or 3 columns).
  # Lazy 2fi: restrict to primary columns that pass 2fi check before building pairs/triples.
  enum_main_sets <- function(f, tier) {
    need <- main_cols_needed[f]
    rsv <- .doe_reserved_main_slots(tier, factor_col, tri, n_cols, n_factors)
    cand <- candidate_main_cols(tier, rsv)
    cand <- cand[vapply(cand, function(c) check_2fi_ok(f, c, c, tier, rsv), logical(1))]
    n_slots <- if (need == 1L) 1L else if (need == 3L) 2L else 3L
    if (length(cand) < n_slots) return(list())
    out <- list()
    if (n_slots == 1L) {
      for (c in cand) out[[length(out) + 1L]] <- c
    } else if (n_slots == 2L) {
      for (ii in seq_len(length(cand) - 1L))
        for (jj in (ii + 1L):length(cand)) {
          ms <- c(cand[ii], cand[jj])
          cols_all <- columns_for_main_set(ms)
          if (any(used[cols_all] != 0L)) next
          if (tier == "V") {
            c12 <- tri[min(ms[1L], ms[2L]), max(ms[1L], ms[2L])]
            if (!(c12 %in% res_v_2fi)) next
          }
          out[[length(out) + 1L]] <- ms
        }
    } else {
      for (ii in seq_len(length(cand) - 2L))
        for (jj in (ii + 1L):(length(cand) - 1L))
          for (kk in (jj + 1L):length(cand)) {
            ms <- c(cand[ii], cand[jj], cand[kk])
            cols_all <- columns_for_main_set(ms)
            if (any(used[cols_all] != 0L)) next
            out[[length(out) + 1L]] <- ms
          }
    }
    out
  }

  commit_factor <- function(f, main_set) {
    primary <- main_set[1L]
    factor_col[f] <<- primary
    main_set_per_factor[[f]] <<- main_set
    cols_all <- columns_for_main_set(main_set)
    for (c in cols_all) used[c] <<- 1L
    set_2fi <- list()
    for (other in neighbors[[f]]) {
      if (factor_col[other] == 0L) next
      if (is.na(other) || other < 1L || other > length(main_set_per_factor)) {
        next
      }
      ms_other <- main_set_per_factor[[other]]
      if (is.null(ms_other)) next
      ccs <- unique(doe_interaction_columns_cross_generators_2level(main_set, ms_other, tri, n_cols,
                                                                    levels_per_factor[f], levels_per_factor[other]))
      key <- if (f < other) paste0(f, "x", other) else paste0(other, "x", f)
      for (cc in ccs) {
        if (is.na(cc) || cc < 1L || cc > n_cols) next
        col_2fi_owner[cc] <<- key
        used[cc] <<- 1L
        set_2fi[[length(set_2fi) + 1L]] <- list(col = cc, key = key)
      }
    }
    set_2fi
  }

  undo_commit <- function(f, main_set, set_2fi) {
    factor_col[f] <<- 0L
    main_set_per_factor[f] <<- list(NULL)
    cols_all <- columns_for_main_set(main_set)
    for (c in cols_all) used[c] <<- 0L
    for (s in set_2fi) {
      col_2fi_owner[s$col] <<- ""
      used[s$col] <<- 0L
    }
  }

  # Recursive backtrack: assign factor at position k in factor_order (1-based index into factor_order)
  recurse <- function(k, tier) {
    backtrack_nodes <<- backtrack_nodes + 1L
    if (backtrack_nodes > MAX_BACKTRACK_NODES) return(list(assignment_list = NULL, factor_col = NULL, design_resolution = NULL, error = "Assignment search limit reached; try a larger array or fewer required interactions."))
    if (k > n_factors) {
      complete_main <- vapply(seq_len(n_factors), function(i) !is.null(main_set_per_factor[[i]]), logical(1))
      if (!all(complete_main)) return(NULL)
      # Build assignment_list from main_set_per_factor and col_2fi_owner (not triangle: each column has one 2fi owner)
      assig <- list()
      for (f in seq_len(n_factors)) {
        main_set <- main_set_per_factor[[f]]
        if (is.null(main_set)) next
        cols <- columns_for_main_set(main_set)
        assig[[length(assig) + 1L]] <- list(factor = f, columns = cols, type = "main")
      }
      for (c in seq_len(n_cols)) {
        key <- col_2fi_owner[c]
        if (!nzchar(key)) next
        assig[[length(assig) + 1L]] <- list(factor = key, columns = as.integer(c), type = "interaction")
      }
      return(list(assignment_list = assig, factor_col = factor_col, design_resolution = tier))
    }
    f <- factor_order[k]
    main_sets <- enum_main_sets(f, tier)
    for (ms in main_sets) {
      primary <- ms[1L]
      if (!check_2fi_ok(f, primary, ms, tier, NULL)) next
      set_2fi <- commit_factor(f, ms)
      if (!forward_check_ok()) {
        undo_commit(f, ms, set_2fi)
        next
      }
      result <- recurse(k + 1L, tier)
      undo_commit(f, ms, set_2fi)
      if (!is.null(result)) return(result)
    }
    NULL
  }

  for (tier_try in c("V", "IV", "III")) {
    if (tier_try == "V" && (is.null(res_v_2fi) || length(res_v_2fi) == 0L)) next
    if (.doe_2level_tier_impossible(tier_try, n_factors, n_cols, n_pairs, res_iv, res_v_2fi)) next
    tier <- tier_try
    backtrack_nodes <- 0L
    factor_col[] <- 0L
    main_set_per_factor <- vector("list", n_factors)
    used[] <- 0L
    col_2fi_owner[] <- ""
    result <- recurse(1L, tier)
    if (!is.null(result)) {
      if (profile_on)
        result$profile <- list(nodes = backtrack_nodes, elapsed_sec = (proc.time() - t_start)[3L],
                               oa_label = oa_label, tier = tier_try)
      return(result)
    }
  }
  NULL
}

# ---- Column assignment algorithm (2-level OAs): seek highest resolution ----
# Resolution V: main effects only in main columns (1,2,4,8,...); 2fi only in 2fi columns; all distinct.
# Resolution IV: main in res_iv; 2fi distinct (any column). Resolution III: main in any column not 2fi of two assigned.
# When required_interactions is non-empty we use backtracking so no two required 2fi share a column.
doe_assign_2level <- function(oa_label, n_factors, levels_per_factor, required_interactions,
                              factor_names = NULL, level_names_per_factor = NULL,
                              odd_level_merge = NULL) {
  # odd_level_merge: for each factor index with odd levels (3,5,6,7), which level gets doubled? e.g. list(2 = 3) for factor 2 merge 4->3
  info <- DOE_OA_2LEVEL[[oa_label]]
  if (is.null(info)) return(list(assignment = NULL, run_sheet = NULL, error = "Unknown OA"))
  n_cols <- info$n_cols
  oa <- info$oa
  res_iv <- info$res_iv
  res_v_2fi <- info$res_v_2fi

  levels_per_factor <- as.integer(levels_per_factor)
  if (length(levels_per_factor) != n_factors)
    return(list(assignment = NULL, run_sheet = NULL, error = "levels_per_factor length must match n_factors"))
  levels_per_factor <- pmin(pmax(levels_per_factor, 2L), 8L)

  # Normalize required_interactions to list of (i,j) factor index pairs
  if (length(required_interactions) == 0L) required_interactions <- list()
  interaction_pairs <- required_interactions
  if (length(interaction_pairs) > 0L && .doe_das_l8_two_disjoint_2fi_impossible(oa_label, interaction_pairs)) {
    return(list(
      assignment = NULL, run_sheet = NULL,
      error = "This requirement set cannot be met on an 8-run array (two disjoint two-factor interactions). Use a larger array."
    ))
  }
  main_cols_needed <- vapply(levels_per_factor, columns_for_level_2level, integer(1))
  assign_profile <- NULL
  resolution_from_catalog <- FALSE

  # When we have required 2fi, try catalog, then phase-1 (2-level only), then full backtrack.
  if (length(interaction_pairs) > 0L && oa_label %in% c("L4", "L8", "L16", "L32", "L64")) {
    catalog_col <- .doe_lookup_catalog(oa_label, n_factors, interaction_pairs)
    if (!is.null(catalog_col)) {
      resolution_from_catalog <- TRUE
      assignment <- list()
      for (f in seq_len(n_factors))
        assignment[[length(assignment) + 1L]] <- list(factor = f, columns = catalog_col[f], type = "main")
      for (p in interaction_pairs) {
        ic <- get_interaction_columns(oa_label, catalog_col[p[1L]], catalog_col[p[2L]])
        for (cc in ic)
          assignment[[length(assignment) + 1L]] <- list(factor = paste0(p[1L], "x", p[2L]), columns = cc, type = "interaction")
      }
      factor_col <- catalog_col
      design_resolution <- if (all(catalog_col %in% res_iv)) "V" else "IV"
    } else if (all(main_cols_needed == 1L)) {
      bt <- .doe_assign_2level_phase1(oa_label, n_factors, interaction_pairs, n_cols, res_iv, res_v_2fi)
      if (!is.null(bt)) {
        assignment <- bt$assignment_list
        design_resolution <- bt$design_resolution
        factor_col <- bt$factor_col
        if (!is.null(bt$profile)) assign_profile <- bt$profile
      } else {
        bt <- .doe_assign_2level_backtrack(oa_label, n_factors, levels_per_factor, interaction_pairs,
                                           main_cols_needed, n_cols, res_iv, res_v_2fi)
        if (is.null(bt)) {
          return(list(assignment = NULL, run_sheet = NULL, error = "No valid assignment; try a larger array or fewer required interactions."))
        }
        if (!is.null(bt$error))
          return(list(assignment = NULL, run_sheet = NULL, error = bt$error))
        assignment <- bt$assignment_list
        design_resolution <- bt$design_resolution
        factor_col <- bt$factor_col
        if (!is.null(bt$profile)) assign_profile <- bt$profile
      }
    } else {
      bt <- .doe_assign_2level_backtrack(oa_label, n_factors, levels_per_factor, interaction_pairs,
                                         main_cols_needed, n_cols, res_iv, res_v_2fi)
      if (is.null(bt)) {
        return(list(assignment = NULL, run_sheet = NULL, error = "No valid assignment; try a larger array or fewer required interactions."))
      }
      if (!is.null(bt$error))
        return(list(assignment = NULL, run_sheet = NULL, error = bt$error))
      assignment <- bt$assignment_list
      design_resolution <- bt$design_resolution
      factor_col <- bt$factor_col
      if (!is.null(bt$profile)) assign_profile <- bt$profile
    }
  } else {
    # Main effects only (no required 2fi) or L12: use greedy assignment
    used <- integer(n_cols)
    assignment <- list()
    factor_col <- integer(n_factors)
    free_cols <- function() {
      cand <- which(used == 0L)
      iv <- cand[cand %in% res_iv]
      if (length(iv) > 0L) return(iv)
      return(cand)
    }
    free_main_cols <- function() res_iv[used[res_iv] == 0L]
    columns_for_main_set <- function(main_set) {
      doe_columns_for_main_set_2level(as.integer(main_set), TRIANGLE_L64, n_cols)
    }
    for (f in seq_len(n_factors)) {
      need <- main_cols_needed[f]
      n_main_slots <- if (need == 1L) 1L else if (need == 3L) 2L else 3L
      fm <- free_main_cols()
      if (length(fm) < n_main_slots) {
        fc <- free_cols()
        if (need != 1L || length(fc) < 1L) return(list(assignment = NULL, run_sheet = NULL, error = "Not enough columns; try a larger array."))
        main_set <- fc[1L]
      } else main_set <- fm[seq_len(n_main_slots)]
      cols <- columns_for_main_set(main_set)
      for (c in cols) used[c] <- 1L
      assignment[[length(assignment) + 1L]] <- list(factor = f, columns = cols, type = "main")
      factor_col[f] <- main_set[1L]
    }
    design_resolution <- "IV"
    if (oa_label %in% c("L4", "L8", "L16", "L32", "L64")) {
      main_ok <- all(vapply(assignment, function(a) all(a$columns %in% res_iv), logical(1)))
      design_resolution <- if (main_ok) "V" else "IV"
    }
  }

  # Ensure each main assignment carries the factor level count for downstream labeling (id columns, tables).
  assignment <- lapply(assignment, function(a) {
    if (!is.null(a$type) && a$type == "main" && (is.null(a$levels) || length(a$levels) == 0L)) {
      f <- as.integer(a$factor)[1L]
      if (!is.na(f) && f >= 1L && f <= length(levels_per_factor)) a$levels <- as.integer(levels_per_factor[f])
    }
    a
  })

  # Build run sheet: one column per factor (after merging multi-column factors)
  run_sheet <- matrix(1L, nrow = info$n_runs, ncol = n_factors)
  for (a in assignment) {
    if (a$type != "main" || length(a$factor) > 1L) next
    f <- a$factor
    if (!is.integer(f) || f < 1L || f > n_factors) next
    cols <- a$columns
    L <- levels_per_factor[f]
    if (length(cols) == 1L) {
      run_sheet[, f] <- oa[, cols[1]]
      if (L == 3L && !is.null(odd_level_merge) && !is.null(odd_level_merge[[as.character(f)]])) {
        merge_into <- odd_level_merge[[as.character(f)]]
        run_sheet[, f] <- ifelse(run_sheet[, f] == 4L, merge_into, run_sheet[, f])
      }
    } else if (length(cols) == 3L) {
      # 4-level or 3-level (collapsed): (1,1)->1, (1,2)->2, (2,1)->3, (2,2)->4
      v1 <- oa[, cols[1]]; v2 <- oa[, cols[2]]
      run_sheet[, f] <- (v1 - 1L) * 2L + (v2 - 1L) + 1L  # 1..4
      if (L == 3L && !is.null(odd_level_merge) && !is.null(odd_level_merge[[as.character(f)]])) {
        merge_into <- odd_level_merge[[as.character(f)]]
        run_sheet[, f] <- ifelse(run_sheet[, f] == 4L, merge_into, run_sheet[, f])
      }
    } else if (length(cols) == 7L) {
      # 8-level or 5/6/7 (collapsed)
      v1 <- oa[, cols[1]]; v2 <- oa[, cols[2]]; v3 <- oa[, cols[3]]
      run_sheet[, f] <- (v1 - 1L) * 4L + (v2 - 1L) * 2L + (v3 - 1L) + 1L  # 1..8
      if (L >= 5L && L <= 7L && !is.null(odd_level_merge) && !is.null(odd_level_merge[[as.character(f)]])) {
        merge_into <- odd_level_merge[[as.character(f)]]
        for (lev in (L + 1L):8L) run_sheet[, f] <- ifelse(run_sheet[, f] == lev, merge_into, run_sheet[, f])
      }
    }
  }

  # Apply level names if provided (run_sheet stays numeric 1,2,...; we'll map in UI or export)
  assign_df <- do.call(rbind, lapply(assignment, function(a) {
    data.frame(factor = if (a$type == "interaction") as.character(a$factor) else as.character(a$factor),
               columns = paste(a$columns, collapse = ","), type = a$type, stringsAsFactors = FALSE)
  }))

  # Defining-relation resolution (shortest word length), including catalog assignments; overrides Tsui column heuristics when they disagree.
  if (oa_label %in% c("L4", "L8", "L16", "L32", "L64")) {
    res_word <- doe_design_resolution(oa_label, assignment)
    if (!is.null(res_word)) {
      skip_word <- identical(res_word, "II") && identical(design_resolution, "III")
      if (!skip_word) design_resolution <- res_word
    }
  }

  out <- list(assignment = assign_df, run_sheet = run_sheet, assignment_list = assignment,
              design_resolution = design_resolution, error = NULL)
  if (!is.null(assign_profile)) out$assign_profile <- assign_profile
  out
}

# ---- Backtracking for 3-level OAs: no two required 2fi share the same column(s) ----
.doe_assign_3level_backtrack <- function(oa_label, n_factors, levels_per_factor, interaction_pairs,
                                         main_cols_needed, n_cols, res_iv) {
  degree <- integer(n_factors)
  for (p in interaction_pairs) {
    degree[p[1L]] <- degree[p[1L]] + 1L
    degree[p[2L]] <- degree[p[2L]] + 1L
  }
  factor_order <- order(-degree, seq_len(n_factors))

  factor_col <- integer(n_factors)
  main_set_per_factor <- vector("list", n_factors)
  used <- integer(n_cols)
  col_2fi_owner <- character(n_cols)

  free_cols <- function() {
    cand <- which(used == 0L)
    iv <- cand[cand %in% res_iv]
    if (length(iv) > 0L) return(iv)
    return(cand)
  }

  # Check: assigning factor f to primary (and cols) causes no 2fi conflict
  check_2fi_ok <- function(f, primary) {
    for (p in interaction_pairs) {
      if (p[1L] != f && p[2L] != f) next
      other <- if (p[1L] == f) p[2L] else p[1L]
      if (factor_col[other] == 0L) next
      ic <- get_interaction_columns(oa_label, primary, factor_col[other])
      key <- paste0(min(p[1L], p[2L]), "x", max(p[1L], p[2L]))
      for (cc in ic) {
        if (nzchar(col_2fi_owner[cc]) && col_2fi_owner[cc] != key) return(FALSE)
      }
    }
    TRUE
  }

  enum_main_sets <- function(f) {
    need <- main_cols_needed[f]
    cand <- free_cols()
    if (need == 1L) return(lapply(cand, function(c) c))
    if (need == 2L) {
      out <- list()
      for (ii in seq_len(length(cand) - 1L))
        for (jj in (ii + 1L):length(cand))
          out[[length(out) + 1L]] <- c(cand[ii], cand[jj])
      return(out)
    }
    list()
  }

  commit_factor <- function(f, main_set) {
    primary <- main_set[1L]
    factor_col[f] <<- primary
    main_set_per_factor[[f]] <<- main_set
    for (c in main_set) used[c] <<- 1L
    set_2fi <- list()
    for (p in interaction_pairs) {
      if (p[1L] != f && p[2L] != f) next
      other <- if (p[1L] == f) p[2L] else p[1L]
      if (factor_col[other] == 0L) next
      ic <- get_interaction_columns(oa_label, primary, factor_col[other])
      key <- paste0(min(p[1L], p[2L]), "x", max(p[1L], p[2L]))
      for (cc in ic) {
        col_2fi_owner[cc] <<- key
        set_2fi[[length(set_2fi) + 1L]] <- list(col = cc, key = key)
      }
    }
    set_2fi
  }

  undo_commit <- function(f, main_set, set_2fi) {
    factor_col[f] <<- 0L
    main_set_per_factor[[f]] <<- NULL
    for (c in main_set) used[c] <<- 0L
    for (s in set_2fi) col_2fi_owner[s$col] <<- ""
  }

  recurse <- function(k) {
    if (k > n_factors) {
      assig <- list()
      for (f in seq_len(n_factors)) {
        main_set <- main_set_per_factor[[f]]
        if (is.null(main_set)) next
        assig[[length(assig) + 1L]] <- list(factor = f, columns = main_set, type = "main")
      }
      for (p in interaction_pairs) {
        i <- p[1L]; j <- p[2L]
        if (factor_col[i] == 0L || factor_col[j] == 0L) next
        ic <- get_interaction_columns(oa_label, factor_col[i], factor_col[j])
        for (cc in ic)
          assig[[length(assig) + 1L]] <- list(factor = paste0(i, "x", j), columns = cc, type = "interaction")
      }
      return(list(assignment_list = assig, factor_col = factor_col))
    }
    f <- factor_order[k]
    main_sets <- enum_main_sets(f)
    for (ms in main_sets) {
      if (any(used[ms] != 0L)) next
      primary <- ms[1L]
      if (!check_2fi_ok(f, primary)) next
      set_2fi <- commit_factor(f, ms)
      result <- recurse(k + 1L)
      undo_commit(f, ms, set_2fi)
      if (!is.null(result)) return(result)
    }
    NULL
  }

  recurse(1L)
}

# ---- Column assignment for 3-level OAs (L9, L27) ----
doe_assign_3level <- function(oa_label, n_factors, levels_per_factor, required_interactions,
                              factor_names = NULL, level_names_per_factor = NULL,
                              odd_level_merge = NULL) {
  info <- DOE_OA_3LEVEL[[oa_label]]
  if (is.null(info)) return(list(assignment = NULL, run_sheet = NULL, error = "Unknown OA"))
  n_cols <- info$n_cols
  oa <- info$oa
  res_iv <- info$res_iv
  main_cols_needed <- vapply(levels_per_factor, columns_for_level_3level, integer(1))

  if (length(required_interactions) == 0L) required_interactions <- list()
  interaction_pairs <- required_interactions

  if (identical(oa_label, "L81") && length(interaction_pairs) > 0L && !.doe_l81_interactions_ready()) {
    return(list(
      assignment = NULL, run_sheet = NULL,
      error = "L81 two-factor interaction columns are not loaded yet. Remove required interactions or add CONFOUND_L81 (and set interaction_table_complete = TRUE) in doe_orthogonal_data.R."
    ))
  }

  if (length(interaction_pairs) > 0L) {
    bt <- .doe_assign_3level_backtrack(oa_label, n_factors, levels_per_factor, interaction_pairs,
                                      main_cols_needed, n_cols, res_iv)
    if (is.null(bt))
      return(list(assignment = NULL, run_sheet = NULL, error = "No valid assignment; try a larger array or fewer required interactions."))
    assignment <- bt$assignment_list
  } else {
    used <- integer(n_cols)
    assignment <- list()
    free_cols <- function() {
      cand <- which(used == 0L)
      iv <- cand[cand %in% res_iv]
      if (length(iv) > 0L) return(iv)
      return(cand)
    }
    for (f in seq_len(n_factors)) {
      need <- main_cols_needed[f]
      fc <- free_cols()
      if (length(fc) < need) return(list(assignment = NULL, run_sheet = NULL, error = "Not enough columns"))
      cols <- fc[seq_len(need)]
      for (c in cols) used[c] <- 1L
      assignment[[length(assignment) + 1L]] <- list(factor = f, columns = cols, type = "main")
    }
  }

  run_sheet <- matrix(1L, nrow = info$n_runs, ncol = n_factors)
  for (a in assignment) {
    if (a$type != "main" || length(a$factor) > 1L) next
    f <- a$factor
    if (!is.integer(f) || f < 1L || f > n_factors) next
    cols <- a$columns
    if (length(cols) == 1L) {
      run_sheet[, f] <- oa[, cols[1]]
    } else {
      run_sheet[, f] <- (oa[, cols[1]] - 1L) * 3L + (oa[, cols[2]] - 1L) + 1L
    }
  }

  assign_df <- do.call(rbind, lapply(assignment, function(a) {
    data.frame(factor = if (a$type == "interaction") as.character(a$factor) else as.character(a$factor),
               columns = paste(a$columns, collapse = ","), type = a$type, stringsAsFactors = FALSE)
  }))
  assignment <- lapply(assignment, function(a) {
    if (!is.null(a$type) && a$type == "main" && (is.null(a$levels) || length(a$levels) == 0L)) {
      f <- as.integer(a$factor)[1L]
      if (!is.na(f) && f >= 1L && f <= length(levels_per_factor)) a$levels <- as.integer(levels_per_factor[f])
    }
    a
  })
  design_resolution <- doe_design_resolution(oa_label, assignment)
  list(assignment = assign_df, run_sheet = run_sheet, assignment_list = assignment, design_resolution = design_resolution, error = NULL)
}

# ---- Column assignment for mixed OAs (L18: 1 two-level + 7 three-level columns, main only) ----
doe_assign_mixed <- function(oa_label, n_factors, levels_per_factor, required_interactions,
                             factor_names = NULL, level_names_per_factor = NULL,
                             odd_level_merge = NULL) {
  if (length(required_interactions) > 0L)
    return(list(assignment = NULL, run_sheet = NULL, error = "L18 does not support interaction columns"))
  info <- DOE_OA_MIXED[[oa_label]]
  if (is.null(info)) return(list(assignment = NULL, run_sheet = NULL, error = "Unknown mixed OA"))
  n_cols <- info$n_cols
  oa <- info$oa
  if (n_factors > n_cols) return(list(assignment = NULL, run_sheet = NULL, error = "Too many factors for L18"))
  # L18: column 1 = two-level; columns 2..8 = three-level. Order factors: 2-level first, then 3-level.
  idx_2 <- which(levels_per_factor == 2L)
  idx_3 <- which(levels_per_factor == 3L)
  if (length(idx_2) > 1L) return(list(assignment = NULL, run_sheet = NULL, error = "L18 has only one two-level column"))
  if (length(idx_3) > 7L) return(list(assignment = NULL, run_sheet = NULL, error = "L18 has only seven three-level columns"))
  order_f <- c(idx_2, idx_3)[seq_len(n_factors)]
  assignment <- list()
  for (k in seq_along(order_f)) {
    f <- order_f[k]
    assignment[[length(assignment) + 1L]] <- list(factor = f, columns = k, type = "main", levels = as.integer(levels_per_factor[f]))
  }
  run_sheet <- matrix(1L, nrow = info$n_runs, ncol = n_factors)
  for (k in seq_along(order_f)) {
    f <- order_f[k]
    run_sheet[, f] <- oa[, k]
  }
  assign_df <- do.call(rbind, lapply(assignment, function(a) {
    data.frame(factor = as.character(a$factor), columns = paste(a$columns, collapse = ","), type = a$type, stringsAsFactors = FALSE)
  }))
  list(assignment = assign_df, run_sheet = run_sheet, assignment_list = assignment, design_resolution = "III", error = NULL)
}

# ---- Factor-column assignment as run table: Run | Col1 (effect) | Col2 (effect) | ... with OA levels ----
# Returns data frame: first column = Run number, then one column per OA column with header "c (Effect)" and values from OA.
doe_assignment_run_table <- function(oa_label, assignment_list, factor_names = NULL) {
  is_2level <- oa_label %in% c("L4", "L8", "L12", "L16", "L32", "L64")
  is_mixed <- oa_label %in% names(DOE_OA_MIXED)
  info <- if (is_2level) DOE_OA_2LEVEL[[oa_label]] else if (is_mixed) DOE_OA_MIXED[[oa_label]] else DOE_OA_3LEVEL[[oa_label]]
  if (is.null(info)) return(NULL)
  oa <- info$oa
  n_runs <- nrow(oa)
  n_cols <- ncol(oa)
  fnames <- factor_names
  if (is.null(fnames)) fnames <- LETTERS[seq_len(26)]
  id_labels <- .doe_id_column_labels(assignment_list, fnames)
  # Map column index -> effect label (for header)
  col_label <- character(n_cols)
  for (c in seq_len(n_cols)) {
    lab <- ""
    id_lab <- unname(id_labels[as.character(c)])
    if (length(id_lab) > 0L && !is.na(id_lab[1L]) && nzchar(id_lab[1L])) {
      lab <- id_lab[1L]
    } else {
    for (a in assignment_list) {
      if (!(c %in% a$columns)) next
      if (a$type == "main") {
        i <- as.integer(a$factor)[1L]
        lab <- if (!is.na(i) && i >= 1L && i <= length(fnames)) fnames[i] else as.character(a$factor)
      } else {
        p <- strsplit(as.character(a$factor), "x", fixed = TRUE)[[1]]
        i <- as.integer(p[1]); j <- as.integer(p[2])
        lab <- if (!is.na(i) && !is.na(j) && i <= length(fnames) && j <= length(fnames))
          paste0(fnames[i], "\u00D7", fnames[j]) else paste0(p[1], "x", p[2])
      }
      break
    }
    }
    col_label[c] <- if (nzchar(lab)) paste0(c, " (", lab, ")") else as.character(c)
  }
  out <- data.frame(Run = seq_len(n_runs), stringsAsFactors = FALSE)
  for (c in seq_len(n_cols)) out[[col_label[c]]] <- oa[, c]
  out
}

# Run sheet with factor labels plus primary-error columns (id...) when present.
doe_run_sheet_table <- function(oa_label, run_sheet, assignment_list, factor_names,
                                levels_vec, level_names_per_factor = NULL) {
  if (is.null(run_sheet)) return(NULL)
  rs <- as.data.frame(run_sheet, stringsAsFactors = FALSE)
  colnames(rs) <- factor_names
  lnames <- level_names_per_factor %||% lapply(levels_vec, function(m) as.character(seq_len(m)))
  for (j in seq_len(ncol(rs))) {
    vec <- lnames[[j]]
    nlev <- levels_vec[j]
    if (length(vec) < nlev) vec <- c(vec, as.character(seq(length(vec) + 1L, nlev)))
    rs[, j] <- vec[as.integer(rs[, j])]
  }

  is_2level <- oa_label %in% c("L4", "L8", "L12", "L16", "L32", "L64")
  is_mixed <- oa_label %in% names(DOE_OA_MIXED)
  info <- if (is_2level) DOE_OA_2LEVEL[[oa_label]] else if (is_mixed) DOE_OA_MIXED[[oa_label]] else DOE_OA_3LEVEL[[oa_label]]
  if (is.null(info) || is.null(info$oa)) return(rs)
  id_labels <- .doe_id_column_labels(assignment_list, factor_names)
  if (length(id_labels) == 0L) return(rs)
  id_cols <- sort(unique(as.integer(names(id_labels))))
  id_cols <- id_cols[id_cols >= 1L & id_cols <= ncol(info$oa)]
  for (c in id_cols) {
    nm <- unname(id_labels[as.character(c)])
    if (length(nm) == 0L || is.na(nm[1L]) || !nzchar(nm[1L])) next
    rs[[nm[1L]]] <- info$oa[, c]
  }
  rs
}

# ---- Alias table: human-readable effects; filter by order; omit when factor appears in both ----
# Build mapping: for each OA column index, the set of factor indices (main = 1, 2fi = 2)
.col_to_effect <- function(assignment_list) {
  out <- list()
  for (a in assignment_list) {
    factors_in_effect <- if (a$type == "main") {
      as.integer(a$factor)[1L]
    } else {
      p <- strsplit(as.character(a$factor), "x", fixed = TRUE)[[1L]]
      c(as.integer(p[1L]), as.integer(p[2L]))
    }
    for (col in a$columns) out[[col]] <- factors_in_effect
  }
  out
}

# Effect label from factor indices (e.g. c(2,3) -> "B\u00D7C" or "BC")
.effect_label <- function(factor_indices, factor_names, use_times = TRUE) {
  if (is.null(factor_indices) || length(factor_indices) == 0L) return("")
  if (length(factor_names) == 0L) return("")
  idx <- as.integer(factor_indices)
  idx <- idx[idx >= 1L & idx <= length(factor_names)]
  if (length(idx) == 0L) return("")
  nms <- factor_names[idx]
  nms <- nms[!is.na(nms) & nms != ""]
  if (length(nms) == 0L) return(paste(idx, collapse = "\u00D7"))
  if (use_times && length(nms) == 2L) return(paste0(nms[1L], "\u00D7", nms[2L]))
  paste(sort(nms), collapse = "")
}

# Main-effect display name for factor index.
.doe_main_factor_name <- function(factor_idx, factor_names) {
  i <- as.integer(factor_idx)[1L]
  if (is.na(i) || i < 1L) return(as.character(factor_idx))
  if (i <= length(factor_names) && nzchar(factor_names[i])) return(factor_names[i])
  as.character(i)
}

# Named character vector: OA column index -> id label (e.g., "12" -> "idA").
.doe_id_column_labels <- function(assignment_list, factor_names) {
  id_labels <- character(0)
  for (a in assignment_list) {
    if (is.null(a$type) || a$type != "main") next
    lev <- as.integer(a$levels)[1L]
    if (is.na(lev) || lev < 2L) next
    cols <- sort(unique(as.integer(a$columns)))
    if (length(cols) <= 1L) next
    n_id <- doe_n_id_columns_2level(lev)
    if (n_id <= 0L) next
    id_cols <- sort(cols, decreasing = TRUE)[seq_len(min(n_id, length(cols)))]
    base <- paste0("id", .doe_main_factor_name(a$factor, factor_names))
    if (length(id_cols) == 1L) {
      id_labels[as.character(id_cols[1L])] <- base
    } else {
      ord <- order(id_cols)
      for (k in seq_along(id_cols)) {
        id_labels[as.character(id_cols[ord[k]])] <- paste0(base, "_", k)
      }
    }
  }
  id_labels
}

# 2-level: column product in GF(2) uses bitwise XOR on Taguchi column indices.
# Identity is 0 (not an OA column), and a column multiplied by itself cancels to 0.
.column_product_2level <- function(cols, tri, n_cols) {
  if (length(cols) == 0L) return(0L)
  cols <- as.integer(cols)
  if (any(is.na(cols)) || any(cols < 1L) || any(cols > n_cols)) return(NA_integer_)
  x <- 0L
  for (c in cols) x <- bitwXor(x, c)
  as.integer(x)
}

# Symmetric difference of factor sets (for 2-level alias: effect product = XOR of sets).
.symdiff_sets <- function(list_of_sets) {
  out <- integer(0)
  for (s in list_of_sets) {
    s <- unique(as.integer(s))
    for (x in s) {
      if (x %in% out) out <- out[out != x] else out <- c(out, x)
    }
  }
  sort(unique(out))
}

# For an unassigned column c (2-level), find factor indices of the effect in that column by
# expressing c as product of a subset of used_cols; returns NULL if not expressible.
.find_effect_unassigned_2level <- function(c, used_cols, col_to_effect, tri, n_cols) {
  if (length(used_cols) < 2L) return(NULL)
  for (k in 2L:min(length(used_cols), 10L)) {
    combs <- utils::combn(used_cols, k, simplify = FALSE)
    for (S in combs) {
      if (.column_product_2level(S, tri, n_cols) == c) {
        sets <- lapply(S, function(col) {
          e <- col_to_effect[[col]]
          if (is.null(e)) integer(0) else unique(as.integer(e))
        })
        return(.symdiff_sets(sets))
      }
    }
  }
  NULL
}

# Factor indices for the alias table "Effect" column: mains always; 2fi only if user required that pair.
.user_declared_effect_indices_for_column <- function(col, assignment_list, required_effect_labels, factor_names) {
  for (a in assignment_list) {
    cols <- as.integer(a$columns)
    if (!col %in% cols) next
    if (!is.null(a$type) && a$type == "main") {
      f <- as.integer(a$factor)
      if (length(f) < 1L || is.na(f[1L]) || f[1L] < 1L) return(integer(0))
      return(unique(f[f >= 1L & f <= length(factor_names)]))
    }
    if (!is.null(a$type) && a$type == "interaction") {
      p <- strsplit(as.character(a$factor), "x", fixed = TRUE)[[1L]]
      if (length(p) < 2L) return(integer(0))
      ij <- c(as.integer(p[1L]), as.integer(p[2L]))
      ij <- ij[!is.na(ij)]
      if (length(ij) < 2L) return(integer(0))
      lab <- .effect_label(ij, factor_names, use_times = FALSE)
      if (nzchar(lab) && lab %in% required_effect_labels) return(sort(unique(ij)))
      return(integer(0))
    }
  }
  integer(0)
}

# If the OA column structurally carries an interaction the user did not require, list it under Confounded.
.prepend_structural_unrequested_to_confounded <- function(translated, structural_factor_idx,
                                                          factor_names, required_effect_labels, alias_display) {
  if (is.null(structural_factor_idx) || length(structural_factor_idx) == 0L) return(translated)
  idx <- unique(as.integer(structural_factor_idx))
  idx <- idx[idx >= 1L & idx <= length(factor_names)]
  if (length(idx) < 2L) return(translated)
  order_effect <- length(idx)
  if (alias_display == "2fi" && order_effect != 2L) return(translated)
  if (alias_display == "2fi_3fi" && (order_effect < 2L || order_effect > 3L)) return(translated)
  lab <- .effect_label(idx, factor_names, use_times = FALSE)
  if (!nzchar(lab) || lab %in% required_effect_labels) return(translated)
  if (lab %in% translated) return(translated)
  c(lab, translated)
}

# Escape label text for HTML in alias table Confounded_interactions cells.
.doe_alias_escape_html <- function(x) {
  x <- as.character(x)[1L]
  if (length(x) < 1L || is.na(x) || !nzchar(x)) return("")
  if (requireNamespace("htmltools", quietly = TRUE))
    return(as.character(htmltools::htmlEscape(x, attribute = FALSE)))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# First pass (Taguchi pairs) → bold; second pass (multi-column XOR) and structural prepend → plain.
.doe_alias_format_confounded_cell <- function(labels_after_prepend, pass1_labels) {
  if (length(labels_after_prepend) == 0L) return("\u2014")
  p1 <- unique(pass1_labels)
  parts <- vapply(seq_along(labels_after_prepend), function(i) {
    lab <- labels_after_prepend[i]
    esc <- .doe_alias_escape_html(lab)
    if (lab %in% p1) paste0("<strong>", esc, "</strong>") else esc
  }, character(1L))
  paste(parts, collapse = ", ")
}

doe_alias_table <- function(oa_label, assignment_list, required_interactions,
                            factor_names = NULL, alias_display = "all") {
  is_2level <- oa_label %in% c("L4", "L8", "L12", "L16", "L32", "L64")
  is_mixed <- oa_label %in% names(DOE_OA_MIXED)
  info <- if (is_2level) DOE_OA_2LEVEL[[oa_label]] else if (is_mixed) DOE_OA_MIXED[[oa_label]] else DOE_OA_3LEVEL[[oa_label]]
  if (is.null(info)) return(NULL)
  confound <- info$confound
  if (is.null(confound) || length(confound) == 0L) return(NULL)
  fnames <- factor_names
  if (is.null(fnames)) fnames <- LETTERS[seq_len(26L)]
  id_labels <- .doe_id_column_labels(assignment_list, fnames)
  col_to_effect <- .col_to_effect(assignment_list)
  ncol_effect <- length(col_to_effect)
  required_pairs <- character(0)
  for (p in required_interactions) required_pairs <- c(required_pairs, paste0(min(p), "x", max(p)))
  # Effect labels for required interactions (factor index pairs); exclude these from confounded list
  required_effect_labels <- character(0)
  for (p in required_interactions) {
    if (length(p) != 2L) next
    lab <- .effect_label(as.integer(p), fnames, use_times = FALSE)
    if (nzchar(lab)) required_effect_labels <- c(required_effect_labels, lab)
  }
  required_effect_labels <- unique(required_effect_labels)
  used_cols <- unique(unlist(lapply(assignment_list, function(a) a$columns)))
  used_cols <- as.integer(used_cols)
  used_cols <- used_cols[used_cols >= 1L & used_cols <= length(confound)]
  # For multi-column aliasing (e.g. A ~ BCD when 2*4*7=1), use only main-effect columns.
  main_cols <- integer(0)
  for (a in assignment_list) {
    if (!is.null(a$type) && a$type == "main") main_cols <- c(main_cols, a$columns)
  }
  main_cols <- unique(as.integer(main_cols))
  main_cols <- main_cols[main_cols >= 1L & main_cols <= length(confound)]
  alias_display <- match.arg(alias_display, c("2fi", "2fi_3fi", "all"))
  n_cols <- info$n_cols
  tri_2level <- if (is_2level && exists("TRIANGLE_L64", inherits = TRUE)) get("TRIANGLE_L64", inherits = TRUE) else NULL

  alias_rows <- list()
  for (c in used_cols) {
    raw <- confound[[c]]
    if (is.null(raw)) raw <- character(0)
    unselected <- setdiff(raw, required_pairs)
    translated_pass1 <- character(0)
    translated_pass2 <- character(0)
    tmp_c <- col_to_effect[[c]]
    effect_in_c <- if (is.null(tmp_c)) integer(0) else unique(as.integer(tmp_c))
    for (pair in unselected) {
      parts <- strsplit(pair, "x", fixed = TRUE)[[1L]]
      if (length(parts) != 2L) next
      i <- as.integer(parts[1L])
      j <- as.integer(parts[2L])
      if (is.na(i) || is.na(j)) next
      if (i < 1L || i > ncol_effect || j < 1L || j > ncol_effect) next
      effect_i <- col_to_effect[[i]]
      effect_j <- col_to_effect[[j]]
      if (is.null(effect_i) || is.null(effect_j)) next
      set_i <- unique(as.integer(effect_i))
      set_j <- unique(as.integer(effect_j))
      if (length(intersect(set_i, set_j)) > 0L) next
      combined <- sort(unique(c(set_i, set_j)))
      # Only list effects that do not contain any factor from this column's effect (triangle-based rule)
      if (length(intersect(combined, effect_in_c)) > 0L) next
      order_effect <- length(combined)
      if (alias_display == "2fi" && order_effect != 2L) next
      if (alias_display == "2fi_3fi" && (order_effect < 2L || order_effect > 3L)) next
      nms <- if (max(combined) <= length(fnames)) fnames[combined] else LETTERS[combined]
      nms[is.na(nms)] <- as.character(combined[is.na(nms)])
      label <- paste(nms, collapse = "")
      if (nzchar(label) && !(label %in% required_effect_labels))
        translated_pass1 <- c(translated_pass1, label)
    }
    # 2-level: add aliases from multi-column products of main columns (from triangle table)
    if (is_2level && !is.null(tri_2level) && n_cols >= 2L && length(main_cols) >= 2L) {
      for (k in 2L:min(length(main_cols), 10L)) {
        combs <- utils::combn(main_cols, k, simplify = FALSE)
        for (S in combs) {
          if (.column_product_2level(S, tri_2level, n_cols) != c) next
          sets <- lapply(S, function(col) { e <- col_to_effect[[col]]; if (is.null(e)) integer(0) else unique(as.integer(e)) })
          if (any(vapply(sets, function(s) length(s) == 0L, FUN.VALUE = logical(1L)))) next
          effect_sym <- .symdiff_sets(sets)
          if (identical(sort(effect_sym), sort(effect_in_c))) next
          if (length(effect_sym) == 0L) next
          # Only list effects that do not contain any factor from this column's effect (triangle-based rule)
          if (length(intersect(effect_sym, effect_in_c)) > 0L) next
          order_effect <- length(effect_sym)
          if (alias_display == "2fi" && order_effect != 2L) next
          if (alias_display == "2fi_3fi" && (order_effect < 2L || order_effect > 3L)) next
          label <- .effect_label(effect_sym, fnames, use_times = FALSE)
          if (nzchar(label) && !(label %in% required_effect_labels))
            translated_pass2 <- c(translated_pass2, label)
        }
      }
    }
    p1u <- unique(translated_pass1)
    p2u <- setdiff(unique(translated_pass2), p1u)
    merged <- c(p1u, p2u)
    translated <- .prepend_structural_unrequested_to_confounded(
      merged, effect_in_c, fnames, required_effect_labels, alias_display
    )
    user_eff <- .user_declared_effect_indices_for_column(c, assignment_list, required_effect_labels, fnames)
    id_lab <- unname(id_labels[as.character(c)])
    effect_in_col <- if (length(id_lab) > 0L && !is.na(id_lab[1L]) && nzchar(id_lab[1L])) {
      as.character(id_lab[1L])
    } else if (length(user_eff) > 0L) {
      .effect_label(user_eff, fnames, use_times = TRUE)
    } else {
      "\u2014"
    }
    confound_str <- .doe_alias_format_confounded_cell(translated, p1u)
    alias_rows[[length(alias_rows) + 1L]] <- data.frame(
      Column = c,
      Effect = effect_in_col,
      Confounded_interactions = confound_str,
      stringsAsFactors = FALSE
    )
  }

  # Unassigned columns: show what effect each represents (e.g. column 7 in L8 = ABC)
  has_triangle <- is_2level && oa_label %in% c("L4", "L8", "L16", "L32", "L64") && !is.null(tri_2level)
  unassigned_cols <- setdiff(seq_len(n_cols), used_cols)
  for (c in unassigned_cols) {
    if (c > length(confound) || is.null(confound[[c]])) next
    raw <- confound[[c]]
    if (is.null(raw)) raw <- character(0)
    effect_in_c <- if (has_triangle)
      .find_effect_unassigned_2level(c, main_cols, col_to_effect, tri_2level, n_cols) else NULL
    struct_vec <- if (!is.null(effect_in_c) && length(effect_in_c) > 0L) {
      unique(as.integer(effect_in_c))
    } else {
      integer(0)
    }
    # No user assignment to an unused OA column → Effect stays empty; structural interaction → Confounded.
    id_lab <- unname(id_labels[as.character(c)])
    effect_in_col <- if (length(id_lab) > 0L && !is.na(id_lab[1L]) && nzchar(id_lab[1L])) as.character(id_lab[1L]) else "\u2014"
    translated_pass1 <- character(0)
    translated_pass2 <- character(0)
    for (pair in raw) {
      parts <- strsplit(pair, "x", fixed = TRUE)[[1L]]
      if (length(parts) != 2L) next
      i <- as.integer(parts[1L])
      j <- as.integer(parts[2L])
      if (is.na(i) || is.na(j)) next
      if (i < 1L || i > ncol_effect || j < 1L || j > ncol_effect) next
      effect_i <- col_to_effect[[i]]
      effect_j <- col_to_effect[[j]]
      if (is.null(effect_i) || is.null(effect_j)) next
      set_i <- unique(as.integer(effect_i))
      set_j <- unique(as.integer(effect_j))
      if (length(intersect(set_i, set_j)) > 0L) next
      combined <- sort(unique(c(set_i, set_j)))
      effect_in_c_vec <- if (is.null(effect_in_c)) integer(0) else effect_in_c
      if (length(intersect(combined, effect_in_c_vec)) > 0L) next
      order_effect <- length(combined)
      if (alias_display == "2fi" && order_effect != 2L) next
      if (alias_display == "2fi_3fi" && (order_effect < 2L || order_effect > 3L)) next
      nms <- if (max(combined) <= length(fnames)) fnames[combined] else LETTERS[combined]
      nms[is.na(nms)] <- as.character(combined[is.na(nms)])
      label <- paste(nms, collapse = "")
      if (nzchar(label) && !(label %in% required_effect_labels))
        translated_pass1 <- c(translated_pass1, label)
    }
    if (has_triangle && n_cols >= 2L && length(main_cols) >= 2L) {
      for (k in 2L:min(length(main_cols), 10L)) {
        combs <- utils::combn(main_cols, k, simplify = FALSE)
        for (S in combs) {
          if (.column_product_2level(S, tri_2level, n_cols) != c) next
          sets <- lapply(S, function(col) {
            e <- col_to_effect[[col]]
            if (is.null(e)) integer(0) else unique(as.integer(e))
          })
          if (any(vapply(sets, function(s) length(s) == 0L, FUN.VALUE = logical(1L)))) next
          effect_sym <- .symdiff_sets(sets)
          if (!is.null(effect_in_c) && identical(sort(effect_sym), sort(effect_in_c))) next
          if (length(effect_sym) == 0L) next
          effect_in_c_vec <- if (is.null(effect_in_c)) integer(0) else effect_in_c
          if (length(intersect(effect_sym, effect_in_c_vec)) > 0L) next
          order_effect <- length(effect_sym)
          if (alias_display == "2fi" && order_effect != 2L) next
          if (alias_display == "2fi_3fi" && (order_effect < 2L || order_effect > 3L)) next
          label <- .effect_label(effect_sym, fnames, use_times = FALSE)
          if (nzchar(label) && !(label %in% required_effect_labels))
            translated_pass2 <- c(translated_pass2, label)
        }
      }
    }
    p1u <- unique(translated_pass1)
    p2u <- setdiff(unique(translated_pass2), p1u)
    merged <- c(p1u, p2u)
    translated <- .prepend_structural_unrequested_to_confounded(
      merged, struct_vec, fnames, required_effect_labels, alias_display
    )
    confound_str <- .doe_alias_format_confounded_cell(translated, p1u)
    alias_rows[[length(alias_rows) + 1L]] <- data.frame(
      Column = c,
      Effect = effect_in_col,
      Confounded_interactions = confound_str,
      stringsAsFactors = FALSE
    )
  }

  if (length(alias_rows) == 0L) return(NULL)
  do.call(rbind, alias_rows)
}

# ---- Design resolution from defining relation (shortest word length) ----
# NIST: resolution R = length of shortest word in defining relation (excluding I).
# Words in the defining subgroup are products of main-effect columns; identity is 0 in XOR coding.
# GF(2): a column multiplied by itself cancels to identity (I), handled in .column_product_2level.
# Mapping: shortest word length 5 -> V, 4 -> IV, 3 -> III (Roman numerals for common R).
.doe_resolution_2level_word <- function(oa_label, assignment_list, info, tri, n_cols) {
  n_factors <- 0L
  for (a in assignment_list) {
    if (!identical(a$type, "main") || length(a$factor) != 1L) next
    f <- as.integer(a$factor)[1L]
    if (is.na(f) || f < 1L) next
    if (f > n_factors) n_factors <- f
  }
  if (n_factors < 1L) return(NULL)
  main_cols <- integer(n_factors)
  for (a in assignment_list) {
    if (!identical(a$type, "main") || length(a$factor) != 1L) next
    f <- as.integer(a$factor)[1L]
    if (is.na(f) || f < 1L || f > n_factors) next
    cols <- as.integer(a$columns)
    if (length(cols) < 1L) next
    main_cols[f] <- cols[1L]
  }
  if (any(main_cols < 1L)) return(NULL)
  shortest <- Inf
  for (k in 2L:min(n_factors, 20L)) {
    combs <- utils::combn(n_factors, k, simplify = FALSE)
    for (S in combs) {
      cols <- main_cols[as.integer(S)]
      prod_c <- .column_product_2level(cols, tri, n_cols)
      if (!is.na(prod_c) && prod_c == 0L) {
        shortest <- min(shortest, k)
        break
      }
    }
    if (is.finite(shortest)) break
  }
  if (!is.finite(shortest)) return(NULL)
  if (shortest >= 5L) return("V")
  if (shortest >= 4L) return("IV")
  if (shortest >= 3L) return("III")
  "II"
}

# Number of distinct two-level factors (main effects only) in assignment_list.
.doe_count_main_factors_2level <- function(assignment_list) {
  idx <- integer(0)
  for (a in assignment_list) {
    if (!identical(a$type, "main")) next
    f <- a$factor
    if (length(f) == 1L && is.numeric(f) && !is.na(f[1L])) idx <- c(idx, as.integer(f[1L]))
  }
  length(unique(idx))
}

# Principal half-fraction 2^(k-1) of 2^k: n_runs = 2^(k-1), k >= 3 → resolution = k (III, IV, V, …).
# Matches the standard defining relation I = (all k letters); see NIST / Montgomery.
.doe_roman_resolution_from_k <- function(k) {
  if (k < 3L) return(NULL)
  rom <- c("III", "IV", "V", "VI", "VII", "VIII", "IX", "X")
  if (k <= 10L) return(rom[k - 2L])
  sprintf("R=%d", k)
}

doe_design_resolution <- function(oa_label, assignment_list) {
  if (oa_label %in% names(DOE_OA_MIXED)) return("III")
  if (oa_label %in% c("L4", "L8", "L12", "L16", "L32", "L64")) {
    info <- DOE_OA_2LEVEL[[oa_label]]
    if (is.null(info)) return("IV")
    # Principal half-fraction: run count 2^(k-1) with k two-level factors → resolution k (overrides Taguchi-column heuristics).
    if (oa_label %in% c("L4", "L8", "L16", "L32", "L64")) {
      k_main <- .doe_count_main_factors_2level(assignment_list)
      nr <- info$n_runs
      if (k_main >= 3L && nr == as.integer(2^(k_main - 1L))) {
        return(.doe_roman_resolution_from_k(k_main))
      }
    }
    n_cols <- info$n_cols
    tri <- if (oa_label %in% c("L4", "L8", "L16", "L32", "L64") && exists("TRIANGLE_L64", inherits = TRUE))
      get("TRIANGLE_L64", inherits = TRUE) else NULL
    if (!is.null(tri)) {
      res <- .doe_resolution_2level_word(oa_label, assignment_list, info, tri, n_cols)
      if (!is.null(res)) return(res)
    }
    res_v_2fi <- info$res_v_2fi
    main_cols_ok <- TRUE
    int_cols_ok <- TRUE
    for (a in assignment_list) {
      cols <- a$columns
      if (a$type == "main") {
        for (c in cols) if (!(c %in% info$res_iv)) main_cols_ok <- FALSE
      } else if (a$type == "interaction") {
        for (c in cols) if (!(c %in% res_v_2fi)) int_cols_ok <- FALSE
      }
    }
    if (main_cols_ok && int_cols_ok) return("V")
    if (!main_cols_ok) return("III")
    return("IV")
  }
  if (oa_label %in% c("L9", "L27", "L81")) {
    info <- DOE_OA_3LEVEL[[oa_label]]
    if (is.null(info)) return("III")
    return("IV")
  }
  "IV"
}

# ---- Smallest feasible array (for default selection) ----
doe_smallest_array <- function(possible_2level, possible_3level, prefer_2level = TRUE) {
  if (prefer_2level && length(possible_2level) > 0L) return(possible_2level[1])
  if (length(possible_3level) > 0L) return(possible_3level[1])
  if (length(possible_2level) > 0L) return(possible_2level[1])
  return(NULL)
}
