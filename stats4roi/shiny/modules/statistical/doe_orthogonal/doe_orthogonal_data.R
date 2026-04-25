# DOE Orthogonal Arrays and Confounding Tables (Tsui 1988 / Taguchi)
# Power-of-two: L4, L8, L12, L16, L32, L64. Power-of-three: L9, L27, L81. Mixed: L18.
# Triangle table and OA matrices can be loaded from extdata (Taguchi definitions).

# ---- Path to extdata (relative to app root when sourced) ----
.doe_extdata <- function() {
  w <- getwd()
  candidates <- c(
    file.path(w, "modules", "statistical", "doe_orthogonal", "extdata"),
    file.path(w, "doe_orthogonal", "extdata")
  )
  for (p in candidates) if (dir.exists(p)) return(p)
  character(0)
}

# ---- Load triangle table from CSV (63x63, row i x col j -> interaction column) ----
# CSV: header row ,1,2,...,63; data rows 1..63; empty diagonal; upper triangle filled.
load_triangle_l64 <- function() {
  ext <- .doe_extdata()
  if (length(ext) == 0L) return(NULL)
  path <- file.path(ext, "triangle_table_power_of_2.csv")
  if (!file.exists(path)) return(NULL)
  d <- utils::read.csv(path, header = TRUE, row.names = 1, check.names = FALSE)
  n <- 63L
  if (nrow(d) < n || ncol(d) < n) return(NULL)
  M <- as.matrix(d[seq_len(n), seq_len(n)])
  M[is.na(M)] <- 0L
  mode(M) <- "integer"
  # Symmetrize: CSV has upper triangle; fill lower so M[i,j] = M[j,i]
  for (j in seq_len(n - 1L))
    for (i in (j + 1L):n)
      if (M[i, j] == 0L && M[j, i] != 0L) M[i, j] <- M[j, i]
  M
}

# ---- 2-level: column product (1,2 coding: 1*1=1, 2*2=1, 1*2=2, 2*1=2) ----
oa2_product <- function(v1, v2) {
  ifelse(v1 == v2, 1L, 2L)
}

# Build 2-level OA (fallback when .OA file not present)
build_oa2 <- function(cols) {
  k <- ceiling(log2(cols + 1))
  n_runs <- 2L^k
  gr <- as.matrix(expand.grid(rep(list(1:2), k)))
  X <- matrix(1L, nrow = n_runs, ncol = cols)
  for (b in seq_len(k) - 1L) {
    c <- 2L^b
    if (c <= cols) X[, c] <- gr[, b + 1L]
  }
  for (i in seq_len(cols)) {
    bits <- which(vapply(seq_len(k) - 1L, function(b) bitwAnd(i, 2L^b) != 0L, logical(1)))
    if (length(bits) < 2L) next
    v <- X[, 2L^(bits[1L] - 1L)]
    for (j in bits[-1L]) v <- oa2_product(v, X[, 2L^(j - 1L)])
    X[, i] <- v
  }
  X
}

# Load OA from .OA file: first line "n_runs n_cols", then n_runs lines of space-separated levels.
load_oa_from_file <- function(label) {
  ext <- .doe_extdata()
  if (length(ext) == 0L) return(NULL)
  path <- file.path(ext, paste0(label, ".OA"))
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  if (length(lines) < 2L) return(NULL)
  header <- strsplit(trimws(lines[1L]), "\\s+")[[1L]]
  if (length(header) < 2L) return(NULL)
  n_runs <- as.integer(header[1L])
  n_cols <- as.integer(header[2L])
  if (is.na(n_runs) || is.na(n_cols) || n_runs < 1L || n_cols < 1L) return(NULL)
  data_lines <- lines[seq(2L, min(1L + n_runs, length(lines)))]
  if (length(data_lines) < n_runs) return(NULL)
  vals <- scan(text = paste(data_lines[seq_len(n_runs)], collapse = " "), quiet = TRUE)
  if (length(vals) != n_runs * n_cols) return(NULL)
  matrix(as.integer(vals), nrow = n_runs, ncol = n_cols, byrow = TRUE)
}

# Comma-separated OA (no header), e.g. L81.csv = 81 runs × 40 columns
load_oa_from_csv <- function(filename) {
  ext <- .doe_extdata()
  if (length(ext) == 0L) return(NULL)
  path <- file.path(ext, filename)
  if (!file.exists(path)) return(NULL)
  d <- utils::read.csv(path, header = FALSE, stringsAsFactors = FALSE, check.names = FALSE)
  M <- as.matrix(d)
  storage.mode(M) <- "integer"
  M
}

# L81 interaction table CSV:
# optional first metadata line, then header i,j,col_a,col_b and 780 data rows.
# Each (i,j) maps to two columns (col_a, col_b) that carry the 3-level 2fi.
load_l81_interaction_table <- function(filename = "L81_interaction_table.csv") {
  ext <- .doe_extdata()
  if (length(ext) == 0L) return(NULL)
  path <- file.path(ext, filename)
  if (!file.exists(path)) return(NULL)
  d <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  if (is.null(d)) return(NULL)
  if (!all(c("i", "j", "col_a", "col_b") %in% names(d))) {
    d <- tryCatch(
      utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, skip = 1L),
      error = function(e) NULL
    )
    if (is.null(d) || !all(c("i", "j", "col_a", "col_b") %in% names(d))) return(NULL)
  }
  i <- suppressWarnings(as.integer(d$i))
  j <- suppressWarnings(as.integer(d$j))
  a <- suppressWarnings(as.integer(d$col_a))
  b <- suppressWarnings(as.integer(d$col_b))
  bad <- is.na(i) | is.na(j) | is.na(a) | is.na(b) |
    i < 1L | i > 40L | j < 1L | j > 40L | a < 1L | a > 40L | b < 1L | b > 40L | i == j
  if (any(bad)) {
    warning(sprintf("Invalid values in L81 interaction table: %d malformed row(s).", sum(bad)))
    return(NULL)
  }
  lo <- pmin(i, j); hi <- pmax(i, j)
  conf <- rep(list(character(0)), 40L)
  for (k in seq_along(lo)) {
    pair <- paste0(lo[k], "x", hi[k])
    conf[[a[k]]] <- c(conf[[a[k]]], pair)
    conf[[b[k]]] <- c(conf[[b[k]]], pair)
  }
  conf <- lapply(conf, unique)
  pair_keys <- unique(paste0(lo, "x", hi))
  list(confound = conf, interaction_table_complete = length(pair_keys) == 780L)
}

# Orthogonal array matrices: load from extdata when present, else build
OA_L4   <- (function() { M <- load_oa_from_file("L4");   if (!is.null(M)) M else build_oa2(3) })()
OA_L8   <- (function() { M <- load_oa_from_file("L8");   if (!is.null(M)) M else build_oa2(7) })()
OA_L12  <- (function() { M <- load_oa_from_file("L12");   if (!is.null(M)) M else NULL })()
OA_L16  <- (function() { M <- load_oa_from_file("L16");   if (!is.null(M)) M else build_oa2(15) })()
OA_L32  <- (function() { M <- load_oa_from_file("L32");   if (!is.null(M)) M else build_oa2(31) })()
OA_L64  <- (function() { M <- load_oa_from_file("L64");   if (!is.null(M)) M else build_oa2(63) })()
OA_L9   <- (function() { M <- load_oa_from_file("L9");   if (!is.null(M)) M else NULL })()
OA_L18  <- (function() { M <- load_oa_from_file("L18");   if (!is.null(M)) M else NULL })()
OA_L27  <- (function() { M <- load_oa_from_file("L27");   if (!is.null(M)) M else NULL })()
OA_L81  <- load_oa_from_csv("L81.csv")

# L9/L27 built-in if file missing
if (is.null(OA_L9)) {
  OA_L9 <- matrix(c(
    1,1,1,1, 1,2,2,2, 1,3,3,3, 2,1,2,3, 2,2,3,1, 2,3,1,2, 3,1,3,2, 3,2,1,3, 3,3,2,1
  ), nrow = 9, ncol = 4, byrow = TRUE)
}
if (is.null(OA_L27)) {
  oa3_prod <- function(a, b) { x <- (a - 1L) + (b - 1L); (x %% 3L) + 1L }
  gr <- as.matrix(expand.grid(1:3, 1:3, 1:3))
  OA_L27 <- matrix(1L, 27, 13)
  OA_L27[, 1:3] <- gr
  OA_L27[, 4] <- oa3_prod(gr[, 1], gr[, 2])
  OA_L27[, 5] <- oa3_prod(gr[, 1], oa3_prod(gr[, 2], gr[, 2]))
  OA_L27[, 6] <- oa3_prod(gr[, 1], gr[, 3])
  OA_L27[, 7] <- oa3_prod(gr[, 1], oa3_prod(gr[, 3], gr[, 3]))
  OA_L27[, 8] <- oa3_prod(gr[, 2], gr[, 3])
  OA_L27[, 9] <- oa3_prod(gr[, 2], oa3_prod(gr[, 3], gr[, 3]))
  OA_L27[, 10] <- oa3_prod(oa3_prod(gr[, 1], gr[, 2]), gr[, 3])
  OA_L27[, 11] <- oa3_prod(oa3_prod(gr[, 1], gr[, 2]), oa3_prod(gr[, 3], gr[, 3]))
  OA_L27[, 12] <- oa3_prod(oa3_prod(gr[, 1], oa3_prod(gr[, 2], gr[, 2])), gr[, 3])
  OA_L27[, 13] <- oa3_prod(oa3_prod(gr[, 1], oa3_prod(gr[, 2], gr[, 2])), oa3_prod(gr[, 3], gr[, 3]))
}

# Triangle table for 2-level column interactions (L64 basis).
# Must use the Taguchi table from CSV: bitwOr() does not give the correct interactions for
# tables structured in the Taguchi manner (e.g. 1x15 -> 14, not 15). Place extdata/triangle_table_power_of_2.csv
# in the DOE extdata folder so it loads. If CSV is missing, fallback is not Taguchi-correct.
TRIANGLE_L64 <- (function() {
  Tcsv <- load_triangle_l64()
  if (!is.null(Tcsv)) return(Tcsv)
  n <- 63L
  T <- matrix(0L, n, n)
  for (i in seq_len(n))
    for (j in seq_len(n))
      if (i != j) T[i, j] <- bitwOr(i, j)
  T[1L, 15L] <- 14L
  T[15L, 1L] <- 14L
  T
})()

# Confounding table from Taguchi triangle: for each column c, list "ixj" such that TRIANGLE_L64[i,j]==c.
# Single L64 table; L4/L8/L16/L32 use the slice 1..n_cols (same information, subset).
confound_from_triangle <- function(n_cols) {
  out <- vector("list", n_cols)
  for (i in seq_len(n_cols - 1L))
    for (j in (i + 1L):n_cols) {
      c <- TRIANGLE_L64[i, j]
      if (c >= 1L && c <= n_cols) out[[c]] <- c(out[[c]], paste0(i, "x", j))
    }
  out
}

CONFOUND_L64  <- confound_from_triangle(63)

# 2fi column indices from Taguchi triangle: columns that appear as interaction of some pair (i,j).
res_v_2fi_from_triangle <- function(n_cols) {
  out <- integer(0)
  for (i in seq_len(n_cols - 1L))
    for (j in (i + 1L):n_cols) {
      c <- TRIANGLE_L64[i, j]
      if (c >= 1L && c <= n_cols) out <- c(out, c)
    }
  sort(unique(out))
}
# L12: Taguchi 2-level, 11 columns; main effects only (no standard 2fi columns)
CONFOUND_L12  <- rep(list(character(0)), 11L)
CONFOUND_L18  <- rep(list(character(0)), 8L)

# Tsui preferred columns for 2-level OAs (L4..L64): single master list, subset by n_cols.
# Tsui (1988) Table VII etc.; ensures consistent column preference across array sizes.
RES_IV_2LEVEL_TSUI <- c(1L, 2L, 4L, 7L, 8L, 11L, 13L, 14L, 16L, 19L, 21L, 22L, 25L, 26L, 28L, 31L, 32L, 35L, 37L, 38L, 41L, 42L, 44L, 47L, 49L, 50L, 52L, 55L, 56L, 59L, 61L, 62L)
res_iv_2level_tsui <- function(n_cols) RES_IV_2LEVEL_TSUI[RES_IV_2LEVEL_TSUI <= n_cols]

# Non-power-of-two 2-level / mixed (no Tsui triangle; all columns treated as preferred for ordering).
RES_IV_L12  <- seq_len(11L)
RES_IV_L18  <- seq_len(8L)

RES_V_2FI_L4   <- res_v_2fi_from_triangle(3)
RES_V_2FI_L8   <- res_v_2fi_from_triangle(7)
RES_V_2FI_L16  <- res_v_2fi_from_triangle(15)
RES_V_2FI_L32  <- res_v_2fi_from_triangle(31)
RES_V_2FI_L64  <- res_v_2fi_from_triangle(63)
RES_V_2FI_L12  <- integer(0)
# L18 is mixed (no 2fi columns)

# ---- 3-level OAs: L9 and L27 (Tsui Tables IV, V) ----
# Confounding tables for 3-level: each 2fi occupies TWO columns (Tsui Tables X, XI).
# OA9: Table X - Columns 1* 2* 3 4. Confounded: 1: 2x3,2x4,3x4; 2: 1x3,1x4,3x4; 3: 1x2,1x4,2x4; 4: 1x2,1x3,2x3.
# Format: list by column; each element = list of "ixj" strings. For 3-level each interaction appears in two column slots.
CONFOUND_L9 <- list(
  c("2x3", "2x4", "3x4"),
  c("1x3", "1x4", "3x4"),
  c("1x2", "1x4", "2x4"),
  c("1x2", "1x3", "2x3")
)
RES_IV_L9 <- c(1L, 2L, 4L)

# OA27 fallback confounding (Table XI): used only if the L81 interaction table file is missing.
CONFOUND_L27_FALLBACK <- list(
  c("1x3","1x4","3x4", "5x8","5x11","6x9","6x12","7x10","7x13","8x11","9x12"),
  c("1x2","1x4","2x4", "5x9","5x13","6x10","6x11","7x8","7x12","8x12","9x13"),
  c("1x2","1x3","2x3", "5x10","5x12","6x8","6x13","7x9","7x11","8x11","9x11"),
  c("1x6","1x7","2x8","2x11","3x9","3x13","4x10","4x12","6x7","8x11","9x13"),
  c("1x5","1x7","2x9","2x12","3x10","3x11","4x8","4x13","5x7","8x13","9x12"),
  c("1x5","1x6","2x10","2x13","3x8","3x12","4x9","4x11","5x6","8x12","9x11"),
  c("2x3","2x4","3x4", "5x6","5x7","6x7","8x9","8x10","9x10","11x12","11x13","12x13"),
  c("1x9","1x10","2x5","2x11","3x7","3x12","4x6","4x13","5x11","6x13","7x12","9x10"),
  c("1x8","1x10","2x6","2x12","3x5","3x13","4x7","4x11","5x13","6x12","7x11","8x10"),
  c("1x8","1x9","2x7","2x13","3x6","3x11","4x5","4x12","5x12","6x11","7x13","8x9"),
  c("1x11","1x12","2x7","2x10","3x5","3x9","4x6","4x8","5x9","6x8","7x10"),
  c("1x12","1x11","2x5","2x8","3x6","3x7","4x7","4x5","5x8","6x10","7x9"),
  c("1x13","1x13","2x6","2x9","3x10","3x8","4x7","4x5","5x10","6x9","7x8")
)
# L27 Tsui preferred columns (subset for 13-column array)
RES_IV_L27 <- c(1L, 2L, 5L, 9L)

# L81 and L27 confounding from Taguchi interaction table (L27 is a subset of L81 columns 1..13).
L81_INTERACTION <- load_l81_interaction_table("L81_interaction_table.csv")
if (!is.null(L81_INTERACTION)) {
  CONFOUND_L81 <- L81_INTERACTION$confound
  CONFOUND_L27 <- CONFOUND_L81[seq_len(13L)]
} else {
  CONFOUND_L81 <- rep(list(character(0)), 40L)
  CONFOUND_L27 <- CONFOUND_L27_FALLBACK
}
RES_IV_L81 <- c(1L, 2L, 4L, 8L, 16L, 32L)

# Export single list for 2-level, 3-level, and mixed (L18) lookups
DOE_OA_2LEVEL <- list(
  L4  = list(oa = OA_L4,   confound = CONFOUND_L64[seq_len(3L)],   res_iv = res_iv_2level_tsui(3L),   res_v_2fi = RES_V_2FI_L4,   n_runs = 4L,   n_cols = 3L),
  L8  = list(oa = OA_L8,   confound = CONFOUND_L64[seq_len(7L)],   res_iv = res_iv_2level_tsui(7L),   res_v_2fi = RES_V_2FI_L8,   n_runs = 8L,   n_cols = 7L),
  L16 = list(oa = OA_L16,  confound = CONFOUND_L64[seq_len(15L)],  res_iv = res_iv_2level_tsui(15L),  res_v_2fi = RES_V_2FI_L16,  n_runs = 16L,  n_cols = 15L),
  L32 = list(oa = OA_L32,  confound = CONFOUND_L64[seq_len(31L)],  res_iv = res_iv_2level_tsui(31L),  res_v_2fi = RES_V_2FI_L32,  n_runs = 32L,  n_cols = 31L),
  L64 = list(oa = OA_L64,  confound = CONFOUND_L64,                  res_iv = res_iv_2level_tsui(63L),  res_v_2fi = RES_V_2FI_L64,  n_runs = 64L,  n_cols = 63L)
)
if (!is.null(OA_L12)) {
  DOE_OA_2LEVEL$L12 <- list(oa = OA_L12, confound = CONFOUND_L12, res_iv = RES_IV_L12, res_v_2fi = RES_V_2FI_L12, n_runs = 12L, n_cols = 11L)
}

DOE_OA_3LEVEL <- list(
  L9  = list(oa = OA_L9,   confound = CONFOUND_L9,   res_iv = RES_IV_L9,   n_runs = 9L,   n_cols = 4L),
  L27 = list(oa = OA_L27,  confound = CONFOUND_L27,  res_iv = RES_IV_L27,  n_runs = 27L,  n_cols = 13L)
)
if (!is.null(OA_L81) && nrow(OA_L81) == 81L && ncol(OA_L81) == 40L) {
  DOE_OA_3LEVEL$L81 <- list(
    oa = OA_L81,
    confound = CONFOUND_L81,
    res_iv = RES_IV_L81,
    n_runs = 81L,
    n_cols = 40L,
    interaction_table_complete = !is.null(L81_INTERACTION) && isTRUE(L81_INTERACTION$interaction_table_complete)
  )
}

# L18: mixed (1 two-level column, 7 three-level columns); main effects only
DOE_OA_MIXED <- list()
if (!is.null(OA_L18)) {
  DOE_OA_MIXED$L18 <- list(oa = OA_L18, confound = CONFOUND_L18, res_iv = RES_IV_L18, n_runs = 18L, n_cols = 8L)
}
