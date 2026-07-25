# Process Performance Analysis — data preparation

PPA_SAMPLE_TIME_ORDER_ID <- "__PPA_TIME_ORDER__"

#' Sentinel sample/time id: observations in file order (top = oldest).
ppa_sample_time_order_id <- function() {
  PPA_SAMPLE_TIME_ORDER_ID
}

#' Default limit configuration for golden tests (Individuals + median MR).
ppa_golden_limit_profile <- function() {
  if (!exists("ppa_default_limit_config", mode = "function")) {
    if (file.exists("modules/statistical/spc/utils/spc_limit_choice_helpers.R")) {
      source("modules/statistical/spc/utils/spc_limit_choice_helpers.R", local = FALSE)
    }
  }
  modifyList(ppa_default_limit_config("single"), list(
    custom_disp = NULL
  ))
}

#' Read a PPA case text file; skip embedded spec label rows.
ppa_read_case_file <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (length(lines) == 0) {
    return(data.frame())
  }
  header <- lines[[1]]
  body <- lines[-1]
  body <- body[!grepl("^(USL|LSL|Target):", body)]
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(header, body), tmp)
  read.delim(tmp, stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL)
}

#' Build a composite stream key from ordered factor columns.
ppa_stream_key <- function(data, stream_factors) {
  if (length(stream_factors) == 0) {
    return(rep("1", nrow(data)))
  }
  if (length(stream_factors) == 1) {
    return(as.character(data[[stream_factors[[1]]]]))
  }
  interaction(data[stream_factors], drop = TRUE, sep = " | ")
}

#' Order stream / factor levels numerically when possible, else alphabetically.
ppa_order_stream_levels <- function(levels) {
  levels <- unique(as.character(levels))
  if (length(levels) == 0) {
    return(levels)
  }
  if (all(grepl(" \\| ", levels, perl = TRUE))) {
    parts <- strsplit(levels, " \\| ", perl = TRUE)
    n_parts <- length(parts[[1]])
    mat <- matrix(NA_real_, nrow = length(levels), ncol = n_parts)
    for (i in seq_along(levels)) {
      nums <- suppressWarnings(as.numeric(parts[[i]]))
      if (length(nums) == n_parts && all(!is.na(nums))) {
        mat[i, ] <- nums
      }
    }
    if (all(!is.na(mat))) {
      ord <- do.call(order, as.data.frame(mat))
      return(levels[ord])
    }
  }
  keys <- ppa_sample_sort_key(levels)
  levels[order(keys, levels, na.last = TRUE)]
}

#' Sort key for sample identifiers (numeric when possible, else character).
ppa_sample_sort_key <- function(x) {
  x_chr <- as.character(x)
  num <- suppressWarnings(as.numeric(x_chr))
  if (length(x) > 0 && all(!is.na(num))) {
    return(num)
  }
  x_chr
}

#' Row order indices for a vector of sample ids.
ppa_order_by_sample <- function(x) {
  order(ppa_sample_sort_key(x), na.last = TRUE)
}

ppa_resolve_measure_shape <- function(response_cols, multiple_measures = NULL) {
  response_cols <- as.character(response_cols)
  response_cols <- response_cols[nzchar(response_cols)]
  if (length(response_cols) <= 1) {
    return(list(
      data_shape = "single",
      measure_cols = NULL,
      response = response_cols[1]
    ))
  }
  mm <- multiple_measures %||% "sample_items"
  if (identical(mm, "repeated_measures")) {
    list(
      data_shape = "replicate",
      measure_cols = response_cols,
      response = NULL
    )
  } else {
    list(
      data_shape = "subgroup",
      measure_cols = response_cols,
      response = NULL
    )
  }
}

ppa_resolve_sample_column <- function(data, sample_id) {
  if (identical(sample_id, ppa_sample_time_order_id()) ||
      identical(sample_id, ".ppa_row_id")) {
    if (!".ppa_row_id" %in% names(data)) {
      data$.ppa_row_id <- seq_len(nrow(data))
    }
    return(list(data = data, sample_col = ".ppa_row_id"))
  }
  list(data = data, sample_col = sample_id)
}

ppa_prepared_has_sample_items <- function(prepared) {
  !is.null(prepared) &&
    "subgroup_id" %in% names(prepared) &&
    any(!is.na(prepared$subgroup_id))
}

ppa_prepared_has_repeated_measures <- function(prepared) {
  !is.null(prepared) && !is.null(attr(prepared, "replicate_matrix"))
}

ppa_prepared_data_shape <- function(prepared, mappings = NULL) {
  if (!is.null(mappings) && !is.null(mappings$data_shape)) {
    return(mappings$data_shape)
  }
  if (is.null(prepared)) {
    return("single")
  }
  if (ppa_prepared_has_repeated_measures(prepared)) {
    return("replicate")
  }
  if (ppa_prepared_has_sample_items(prepared)) {
    return("subgroup")
  }
  "single"
}

ppa_all_response_values <- function(prepared) {
  vals <- attr(prepared, "all_response_values")
  if (!is.null(vals)) {
    return(as.numeric(vals))
  }
  as.numeric(prepared$response)
}

#' Stream labels aligned with \code{ppa_all_response_values()}.
ppa_response_streams <- function(prepared) {
  reps <- attr(prepared, "replicate_matrix")
  if (!is.null(reps)) {
    return(rep(prepared$stream, each = ncol(reps)))
  }
  prepared$stream
}

#' Reshape wide subgroup columns to long observations.
ppa_reshape_subgroups <- function(data, subgroup_cols, sample_id, stream_factors) {
  id_cols <- unique(c(sample_id, stream_factors))
  id_cols <- id_cols[id_cols %in% names(data)]
  long <- reshape(
    data,
    direction = "long",
    varying = subgroup_cols,
    v.names = "value",
    timevar = ".subgroup",
    times = seq_along(subgroup_cols),
    idvar = id_cols,
    ids = seq_len(nrow(data))
  )
  rownames(long) <- NULL
  long$.row_id <- long$.id
  long$.id <- NULL
  long
}

#' Prepare analysis-ready long data frame.
ppa_prepare_data <- function(
  data,
  response,
  sample_id,
  stream_factors,
  data_shape = c("single", "subgroup", "replicate"),
  measure_cols = NULL,
  subgroup_cols = NULL,
  replicate_cols = NULL,
  use_replicate_mean = TRUE
) {
  data_shape <- match.arg(data_shape)
  stream_factors <- as.character(stream_factors)
  resolved <- ppa_resolve_sample_column(data, sample_id)
  data <- resolved$data
  sample_col <- resolved$sample_col

  if (!is.null(measure_cols) && length(measure_cols) > 0) {
    if (data_shape == "subgroup") {
      subgroup_cols <- measure_cols
    } else if (data_shape == "replicate") {
      replicate_cols <- measure_cols
    }
  }

  if (data_shape == "subgroup") {
    if (is.null(subgroup_cols) || length(subgroup_cols) == 0) {
      stop("measure columns required for sample items", call. = FALSE)
    }
    long <- ppa_reshape_subgroups(data, subgroup_cols, sample_col, stream_factors)
    long$response <- suppressWarnings(as.numeric(long$value))
    long$sample_id <- long[[sample_col]]
    long$stream <- ppa_stream_key(long, stream_factors)
    long$subgroup_id <- long$.subgroup
    long$row_id <- long$.row_id
  } else if (data_shape == "replicate") {
    if (is.null(replicate_cols) || length(replicate_cols) == 0) {
      stop("measure columns required for repeated measures", call. = FALSE)
    }
    reps <- as.data.frame(lapply(data[replicate_cols], function(x) suppressWarnings(as.numeric(x))))
    data$response <- rowMeans(reps, na.rm = TRUE)
    long <- data.frame(
      response = data$response,
      sample_id = data[[sample_col]],
      stream = ppa_stream_key(data, stream_factors),
      stringsAsFactors = FALSE
    )
    long$subgroup_id <- NA_integer_
    long$row_id <- seq_len(nrow(long))
    attr(long, "replicate_matrix") <- as.matrix(reps)
  } else {
    if (is.null(response) || !nzchar(response)) {
      stop("response column required", call. = FALSE)
    }
    long <- data.frame(
      response = suppressWarnings(as.numeric(data[[response]])),
      sample_id = data[[sample_col]],
      stream = ppa_stream_key(data, stream_factors),
      stringsAsFactors = FALSE
    )
    long$subgroup_id <- NA_integer_
    long$row_id <- seq_len(nrow(long))
  }

  long <- long[!is.na(long$response), , drop = FALSE]
  long$stream <- factor(long$stream)
  for (sf in stream_factors) {
    if (sf %in% names(long)) next
    if (!sf %in% names(data)) next
    if (data_shape == "subgroup") {
      long[[sf]] <- data[[sf]][match(long$row_id, seq_len(nrow(data)))]
    } else {
      long[[sf]] <- data[[sf]][long$row_id]
    }
  }
  long <- long[order(
    long$stream,
    ppa_sample_sort_key(long$sample_id),
    long$subgroup_id,
    na.last = TRUE
  ), , drop = FALSE]
  rownames(long) <- NULL
  reps_mat <- attr(long, "replicate_matrix")
  if (!is.null(reps_mat)) {
    reps_mat <- reps_mat[long$row_id, , drop = FALSE]
    attr(long, "replicate_matrix") <- reps_mat
    attr(long, "all_response_values") <- as.vector(t(reps_mat))
  } else {
    attr(long, "all_response_values") <- long$response
  }
  long
}

#' One row per sample (stream x sample) with subgroup summary statistics.
ppa_build_subgroup_points <- function(prepared) {
  df <- prepared[, c("response", "sample_id", "stream"), drop = FALSE]
  df$.key <- paste(as.character(df$stream), as.character(df$sample_id), sep = "\x01")
  parts <- split(df, df$.key)
  pts <- do.call(rbind, lapply(parts, function(part) {
    n <- nrow(part)
    data.frame(
      mean = mean(part$response),
      range = if (n > 1) max(part$response) - min(part$response) else NA_real_,
      sd = if (n > 1) stats::sd(part$response) else NA_real_,
      var = if (n > 1) stats::var(part$response) else NA_real_,
      n = n,
      stream = part$stream[1],
      sample_id = part$sample_id[1],
      stringsAsFactors = FALSE
    )
  }))
  rownames(pts) <- NULL
  if (nrow(pts) > 0) {
    pts <- pts[ppa_order_by_sample(pts$sample_id), , drop = FALSE]
    rownames(pts) <- NULL
  }
  pts
}
