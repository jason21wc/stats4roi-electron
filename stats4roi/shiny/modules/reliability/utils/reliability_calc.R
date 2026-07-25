# Pure reliability block-diagram (series-of-parallels) calculations.
# Parity with docs/reliability/ReliabilityCalculator2.xls

RELIABILITY_STAGE_MIN <- 1L
RELIABILITY_STAGE_MAX <- 15L
RELIABILITY_PARALLEL_MIN <- 1L
RELIABILITY_PARALLEL_MAX <- 8L
RELIABILITY_DEFAULT_STAGES <- 5L
RELIABILITY_DEFAULT_PARALLELS <- 3L

#' Validate a single reliability value in [0, 1].
#' Blank / NA / empty string are allowed (treated as unused slot).
#' @return list(ok = logical, value = numeric or NA_real_, message = character)
validate_reliability_cell <- function(x) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) {
    return(list(ok = TRUE, value = NA_real_, message = ""))
  }
  if (is.character(x)) {
    x <- trimws(x)
    if (!nzchar(x)) {
      return(list(ok = TRUE, value = NA_real_, message = ""))
    }
  }
  num <- suppressWarnings(as.numeric(x))
  if (length(num) != 1L || is.na(num)) {
    return(list(ok = FALSE, value = NA_real_, message = "Reliability must be numeric."))
  }
  if (num < 0 || num > 1) {
    return(list(
      ok = FALSE,
      value = NA_real_,
      message = "Reliability must be between 0 and 1 (inclusive)."
    ))
  }
  list(ok = TRUE, value = num, message = "")
}

#' Parallel reliability for one stage (row).
#' All-blank / all-NA row returns 1 (identity for series product).
#' Blank slots are omitted (Excel treats blank as 0 in (1-R), which is equivalent).
row_reliability <- function(values) {
  vals <- as.numeric(values)
  if (length(vals) == 0L || all(is.na(vals))) {
    return(1)
  }
  # Blank/NA slots contribute (1 - 0) = 1 to the product, matching Excel.
  filled <- vals
  filled[is.na(filled)] <- 0
  1 - prod(1 - filled)
}

#' Series (product) of stage reliabilities.
system_reliability <- function(row_reliabilities) {
  rr <- as.numeric(row_reliabilities)
  if (length(rr) == 0L) {
    return(1)
  }
  rr[is.na(rr)] <- 1
  prod(rr)
}

#' Build an empty component matrix (stages x parallels) filled with NA.
empty_reliability_grid <- function(n_stages = RELIABILITY_DEFAULT_STAGES,
                                   n_parallels = RELIABILITY_DEFAULT_PARALLELS) {
  n_stages <- as.integer(n_stages)
  n_parallels <- as.integer(n_parallels)
  stopifnot(
    n_stages >= RELIABILITY_STAGE_MIN,
    n_stages <= RELIABILITY_STAGE_MAX,
    n_parallels >= RELIABILITY_PARALLEL_MIN,
    n_parallels <= RELIABILITY_PARALLEL_MAX
  )
  mat <- matrix(
    NA_real_,
    nrow = n_stages,
    ncol = n_parallels,
    dimnames = list(
      paste("Stage", seq_len(n_stages)),
      paste0("P", seq_len(n_parallels))
    )
  )
  mat
}

#' Compute row reliabilities for each stage of a component matrix.
compute_row_reliabilities <- function(grid) {
  if (is.null(grid) || nrow(grid) == 0L) {
    return(numeric(0))
  }
  unname(apply(grid, 1L, row_reliability))
}

#' Attach a Row reliability column to the component grid for display.
reliability_display_table <- function(grid) {
  rows <- compute_row_reliabilities(grid)
  out <- as.data.frame(grid, stringsAsFactors = FALSE)
  out[["Row reliability"]] <- rows
  rownames(out) <- rownames(grid)
  out
}

#' Validate all editable cells in a grid; return first error message or "".
validate_reliability_grid <- function(grid) {
  if (is.null(grid) || length(grid) == 0L) {
    return("")
  }
  for (i in seq_len(nrow(grid))) {
    for (j in seq_len(ncol(grid))) {
      check <- validate_reliability_cell(grid[i, j])
      if (!isTRUE(check$ok)) {
        return(sprintf(
          "Stage %d, P%d: %s",
          i, j, check$message
        ))
      }
    }
  }
  ""
}

#' Coerce a table/matrix of cell values to a numeric reliability grid.
#' Invalid cells become NA and are reported via validate_reliability_grid.
coerce_reliability_grid <- function(df, n_parallels = NULL) {
  if (is.null(df)) {
    return(empty_reliability_grid())
  }
  mat <- as.matrix(df)
  # Drop computed column if present
  if ("Row reliability" %in% colnames(mat)) {
    mat <- mat[, colnames(mat) != "Row reliability", drop = FALSE]
  }
  if (!is.null(n_parallels)) {
    n_parallels <- as.integer(n_parallels)
    if (ncol(mat) > n_parallels) {
      mat <- mat[, seq_len(n_parallels), drop = FALSE]
    } else if (ncol(mat) < n_parallels) {
      pad <- matrix(NA_real_, nrow = nrow(mat), ncol = n_parallels - ncol(mat))
      mat <- cbind(mat, pad)
    }
  }
  storage.mode(mat) <- "character"
  out <- matrix(
    NA_real_,
    nrow = nrow(mat),
    ncol = ncol(mat)
  )
  for (i in seq_len(nrow(mat))) {
    for (j in seq_len(ncol(mat))) {
      check <- validate_reliability_cell(mat[i, j])
      out[i, j] <- check$value
    }
  }
  rownames(out) <- paste("Stage", seq_len(nrow(out)))
  colnames(out) <- paste0("P", seq_len(ncol(out)))
  out
}

#' Resize grid, preserving existing values.
#' When shrinking, returns list(ok, grid, message). Blocks shrink if trailing
#' cells have values (prefer preserve-and-warn per plan).
resize_reliability_grid <- function(grid, n_stages, n_parallels) {
  n_stages <- as.integer(n_stages)
  n_parallels <- as.integer(n_parallels)
  if (n_stages < RELIABILITY_STAGE_MIN || n_stages > RELIABILITY_STAGE_MAX) {
    return(list(
      ok = FALSE,
      grid = grid,
      message = sprintf(
        "Stages must be between %d and %d.",
        RELIABILITY_STAGE_MIN, RELIABILITY_STAGE_MAX
      )
    ))
  }
  if (n_parallels < RELIABILITY_PARALLEL_MIN || n_parallels > RELIABILITY_PARALLEL_MAX) {
    return(list(
      ok = FALSE,
      grid = grid,
      message = sprintf(
        "Parallel components must be between %d and %d.",
        RELIABILITY_PARALLEL_MIN, RELIABILITY_PARALLEL_MAX
      )
    ))
  }

  old_r <- nrow(grid)
  old_c <- ncol(grid)

  if (n_stages < old_r) {
    trailing <- grid[(n_stages + 1L):old_r, , drop = FALSE]
    if (any(!is.na(trailing))) {
      return(list(
        ok = FALSE,
        grid = grid,
        message = "Clear values in stages you want to remove before shrinking."
      ))
    }
  }
  if (n_parallels < old_c) {
    trailing <- grid[, (n_parallels + 1L):old_c, drop = FALSE]
    if (any(!is.na(trailing))) {
      return(list(
        ok = FALSE,
        grid = grid,
        message = "Clear values in parallel columns you want to remove before shrinking."
      ))
    }
  }

  new_grid <- empty_reliability_grid(n_stages, n_parallels)
  copy_r <- min(old_r, n_stages)
  copy_c <- min(old_c, n_parallels)
  if (copy_r > 0L && copy_c > 0L) {
    new_grid[seq_len(copy_r), seq_len(copy_c)] <- grid[seq_len(copy_r), seq_len(copy_c)]
  }
  list(ok = TRUE, grid = new_grid, message = "")
}

#' Seed example: two series stages (0.9 then 0.8) for demos.
example_reliability_grid <- function() {
  grid <- empty_reliability_grid(5L, 3L)
  grid[1, 1] <- 0.9
  grid[2, 1] <- 0.8
  grid
}
