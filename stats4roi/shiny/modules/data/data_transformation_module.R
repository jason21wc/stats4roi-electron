# Data Transformation Module for stats4ROI
# Allows mathematical transforms, random distribution columns, and run numbers on working data.
# Original data is preserved; new columns are appended.

# Helper: ensure new column name is unique given existing names
make_unique_name <- function(base_name, existing_names) {
  name <- base_name
  i <- 1L
  while (name %in% existing_names) {
    i <- i + 1L
    name <- paste0(base_name, "_", i)
  }
  name
}

# Apply a single transform formula to a numeric vector. Returns the new vector or NULL on error.
# params: c(A=, B=, C=); C used only for power1, power2, inverse.
compute_transform <- function(type, x, A, B, C = 1) {
  if (!is.numeric(x)) return(NULL)
  A <- as.numeric(A)
  B <- as.numeric(B)
  C <- as.numeric(C)
  tryCatch({
    switch(type,
      linear = A * x + B,
      power1 = {
        base <- A * x + B
        if (any(is.na(base)) || (C != round(C) && any(base < 0))) return(NULL)
        base^C
      },
      power2 = {
        base <- A * x + B
        if (any(is.na(base)) || (C <= 0) || (any(base < 0))) return(NULL)
        base^(1 / C)
      },
      inverse = {
        ax <- A * x
        if (any(ax == 0, na.rm = TRUE)) return(NULL)
        B / (ax^C)
      },
      log = {
        arg <- A * x + B
        if (any(arg <= 0, na.rm = TRUE)) return(NULL)
        log(arg)
      },
      exp = exp(A * x + B),
      arcsine = asin(pmax(-1, pmin(1, A * x + B))),
      sine = sin(A * x + B),
      poisson_sqrt = {
        arg <- A * x + B
        if (any(arg < 0, na.rm = TRUE)) return(NULL)
        sqrt(arg) + sqrt(arg + 1)
      },
      NULL
    )
  }, error = function(e) NULL)
}

# Rank values across selected columns together (ties.method = "average").
# Returns named list of rank vectors per column, or NULL on error.
compute_global_rank <- function(data, columns) {
  if (is.null(data) || !is.data.frame(data)) return(NULL)
  columns <- as.character(columns)
  if (length(columns) == 0L || !all(columns %in% names(data))) return(NULL)
  if (!all(vapply(data[columns], is.numeric, logical(1)))) return(NULL)
  n <- nrow(data)
  if (n == 0L) {
    return(stats::setNames(vector("list", length(columns)), columns))
  }
  n_cols <- length(columns)
  values <- numeric(n * n_cols)
  col_indices <- vector("list", n_cols)
  names(col_indices) <- columns
  idx <- 1L
  for (col in columns) {
    col_indices[[col]] <- idx:(idx + n - 1L)
    values[idx:(idx + n - 1L)] <- data[[col]]
    idx <- idx + n
  }
  ranks <- rep(NA_real_, length(values))
  ok <- !is.na(values)
  ranks[ok] <- rank(values[ok], ties.method = "average")
  lapply(col_indices, function(ii) ranks[ii])
}

# Collect output column names from transform specs.
spec_output_names <- function(spec_list) {
  if (length(spec_list) == 0L) return(character(0))
  unlist(lapply(spec_list, function(x) {
    if (identical(x$type, "global_rank")) x$newnames else x$newname
  }), use.names = FALSE)
}

# Check data frame for Inf/NaN in numeric columns. Returns list(has_issue, columns, n_inf, n_nan).
check_nonfinite <- function(data) {
  if (is.null(data) || !is.data.frame(data)) return(list(has_issue = FALSE, columns = character(0), n_inf = 0L, n_nan = 0L))
  cols_with_inf <- character(0)
  cols_with_nan <- character(0)
  for (j in names(data)) {
    if (!is.numeric(data[[j]])) next
    x <- data[[j]]
    if (any(is.infinite(x), na.rm = TRUE)) cols_with_inf <- c(cols_with_inf, j)
    if (any(is.nan(x), na.rm = TRUE)) cols_with_nan <- c(cols_with_nan, j)
  }
  all_cols <- unique(c(cols_with_inf, cols_with_nan))
  list(
    has_issue = length(all_cols) > 0L,
    columns = all_cols,
    has_inf = length(cols_with_inf) > 0L,
    has_nan = length(cols_with_nan) > 0L,
    inf_columns = cols_with_inf,
    nan_columns = cols_with_nan
  )
}

# Parse optional RNG seed from a text input. Blank/whitespace => seed NULL (unseeded).
# Valid signed integer string => seed as integer. Otherwise ok = FALSE.
parse_optional_seed <- function(x) {
  if (is.null(x) || length(x) == 0L) return(list(ok = TRUE, seed = NULL))
  s <- trimws(as.character(x[[1]]))
  if (identical(s, "") || is.na(s)) return(list(ok = TRUE, seed = NULL))
  if (!grepl("^-?[0-9]+$", s)) return(list(ok = FALSE, seed = NULL))
  seed <- suppressWarnings(as.integer(s))
  if (is.na(seed)) return(list(ok = FALSE, seed = NULL))
  list(ok = TRUE, seed = seed)
}

# Generate a random column of length n. params: A, B, C per distribution.
# Optional seed: when not NULL, set.seed inside a save/restore of .Random.seed.
# Returns numeric vector or NULL on error.
generate_random_column <- function(type, n, A, B, C = 0, seed = NULL) {
  n <- as.integer(n)
  if (is.na(n) || n < 1) return(NULL)
  A <- as.numeric(A)
  B <- as.numeric(B)
  C <- as.numeric(C)
  draw <- function() {
    tryCatch({
      switch(type,
        uniform = runif(n, min = A, max = B),
        normal = rnorm(n, mean = A, sd = B),
        exponential = A + rexp(n, rate = 1 / B),
        chisq = C + B * rchisq(n, df = A),
        NULL
      )
    }, error = function(e) NULL)
  }
  if (is.null(seed)) return(draw())
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  draw()
}

# Pad or trim a vector to length n. Shorter vectors get NA of the same type.
pad_vector_to_n <- function(vec, n) {
  n <- as.integer(n)
  if (is.na(n) || n < 0L) n <- 0L
  len <- length(vec)
  if (len == n) return(vec)
  if (len > n) return(vec[seq_len(n)])
  if (len == 0L) {
    if (is.integer(vec)) return(rep(NA_integer_, n))
    if (is.logical(vec)) return(rep(NA, n))
    if (is.character(vec)) return(rep(NA_character_, n))
    return(rep(NA_real_, n))
  }
  fill <- if (is.integer(vec)) {
    NA_integer_
  } else if (is.logical(vec)) {
    NA
  } else if (is.character(vec)) {
    NA_character_
  } else {
    NA_real_
  }
  c(vec, rep(fill, n - len))
}

# Pad every column of a data frame to nrow = n.
pad_dataframe_to_n <- function(data, n) {
  n <- as.integer(n)
  if (is.null(data) || !is.data.frame(data) || ncol(data) == 0L) {
    return(data.frame(row.names = if (n > 0L) seq_len(n) else NULL))
  }
  if (nrow(data) == n) return(data)
  out <- as.data.frame(lapply(data, pad_vector_to_n, n = n), stringsAsFactors = FALSE)
  names(out) <- names(data)
  out
}

is_random_spec <- function(x) {
  !is.null(x$values) && is.null(x$column) &&
    !identical(x$type, "run_number") && !identical(x$type, "global_rank")
}

# Build working data + transform/random columns, aligned to the longest column.
# When wd is NULL/empty, random-only specs still produce a data frame.
# Returns NULL when there is no base and no usable random specs.
assemble_transformed_data <- function(wd, specs) {
  has_base <- is.data.frame(wd) && ncol(wd) > 0L
  specs <- if (is.null(specs)) list() else specs
  random_lens <- integer(0)
  if (length(specs) > 0L) {
    for (x in specs) {
      if (is_random_spec(x)) random_lens <- c(random_lens, length(x$values))
    }
  }
  max_random <- if (length(random_lens) == 0L) 0L else max(random_lens)

  if (!has_base) {
    if (max_random < 1L) return(NULL)
    n_out <- max_random
    out <- data.frame(row.names = seq_len(n_out))
  } else {
    n_base <- nrow(wd)
    n_out <- max(n_base, max_random)
    if (n_out < 1L && length(specs) == 0L) return(wd)
    out <- pad_dataframe_to_n(wd, n_out)
  }

  if (length(specs) == 0L) return(out)

  for (x in specs) {
    if (identical(x$type, "run_number")) {
      if (!has_base) next
      newcol <- generate_run_numbers(wd, x$factor_columns)
      if (!is.null(newcol)) {
        out[[x$newname]] <- pad_vector_to_n(newcol, n_out)
      }
    } else if (identical(x$type, "global_rank")) {
      if (!has_base) next
      ranked <- compute_global_rank(wd, x$columns)
      if (!is.null(ranked)) {
        for (i in seq_along(x$columns)) {
          out[[x$newnames[i]]] <- pad_vector_to_n(ranked[[x$columns[i]]], n_out)
        }
      }
    } else if (!is.null(x$column)) {
      if (!has_base || is.null(wd[[x$column]])) next
      p <- x$params
      newcol <- compute_transform(x$type, wd[[x$column]], p["A"], p["B"], p["C"])
      if (!is.null(newcol)) {
        out[[x$newname]] <- pad_vector_to_n(newcol, n_out)
      }
    } else if (is_random_spec(x)) {
      out[[x$newname]] <- pad_vector_to_n(x$values, n_out)
    }
  }
  out
}

# Assign integer run numbers to each unique factor combination (sorted order).
# Returns integer vector or NULL on invalid input.
generate_run_numbers <- function(data, factor_cols) {
  if (is.null(data) || !is.data.frame(data)) return(NULL)
  if (is.null(factor_cols) || length(factor_cols) == 0) return(NULL)
  factor_cols <- as.character(factor_cols)
  if (!all(factor_cols %in% names(data))) return(NULL)
  factor_data <- data[, factor_cols, drop = FALSE]
  if (nrow(factor_data) == 0) return(integer(0))
  factor_chr <- lapply(factor_data, as.character)
  factor_df <- as.data.frame(factor_chr, stringsAsFactors = FALSE)
  interaction_key <- interaction(factor_df, drop = TRUE, lex.order = TRUE, sep = "\x01")
  as.integer(factor(interaction_key))
}

# Data Transformation UI
create_data_transformation_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12,
      h3("Transform Data"),
      p("Apply mathematical transforms to selected columns. ",
        "New columns are appended; original data is preserved. ",
        "Transformed data flows to Dynamic Filter and analysis modules."),
      uiOutput(ns("nonfinite_warning"))
    ),
    column(
      width = 6,
      h4("Apply transform to column(s)"),
      shinyWidgets::pickerInput(
        inputId = ns("columns"),
        label = "Select numeric column(s)",
        choices = character(0),
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      selectInput(
        inputId = ns("transform_type"),
        label = "Transform",
        choices = c(
          "Linear (y = A*X + B)" = "linear",
          "Power1 (y = (A*X+B)^C)" = "power1",
          "Power2 (y = (A*X+B)^(1/C))" = "power2",
          "Inverse (y = B/(A*X)^C)" = "inverse",
          "Log (y = ln(A*X+B))" = "log",
          "EXP (y = exp(A*X+B))" = "exp",
          "Arcsine (y = asin(A*X+B))" = "arcsine",
          "Sine (y = sin(A*X+B), radians)" = "sine",
          "Poisson (y = sqrt(A*X+B) + sqrt(A*X+B+1))" = "poisson_sqrt",
          "Global rank (ties = average)" = "global_rank"
        )
      ),
      fluidRow(
        column(4, numericInput(ns("param_a"), "A", value = 1, width = "100%")),
        column(4, numericInput(ns("param_b"), "B", value = 0, width = "100%")),
        column(4, uiOutput(ns("param_c_ui")))
      ),
      actionButton(ns("apply_transform"), "Apply transform", class = "btn-primary")
    ),
    column(
      width = 6,
      h4("Applied transforms"),
      verbatimTextOutput(ns("specs_summary"))
    ),
    column(
      width = 12,
      hr(),
      h4("Add random distribution column")
    ),
    column(
      width = 6,
      selectInput(
        inputId = ns("rand_dist"),
        label = "Distribution",
        choices = c(
          "Uniform (A=min, B=max)" = "uniform",
          "Normal (A=mean, B=sd)" = "normal",
          "Exponential (A=min, B=mean)" = "exponential",
          "Chi-square (A=df, B=scale, C=min)" = "chisq"
        )
      ),
      numericInput(ns("rand_n"), "Length (n)", value = 100, min = 1, width = "100%"),
      fluidRow(
        column(4, numericInput(ns("rand_a"), "A", value = 0, width = "100%")),
        column(4, numericInput(ns("rand_b"), "B", value = 1, width = "100%")),
        column(4, numericInput(ns("rand_c"), "C", value = 0, width = "100%"))
      ),
      textInput(ns("rand_colname"), "Column name (optional)", value = "", placeholder = "e.g. my_random"),
      textInput(ns("rand_seed"), "Seed (optional)", value = "", placeholder = "Leave blank for random"),
      actionButton(ns("add_random"), "Add column", class = "btn-primary")
    ),
    column(
      width = 12,
      hr(),
      h4("Generate Run Number from factors")
    ),
    column(
      width = 6,
      shinyWidgets::pickerInput(
        inputId = ns("run_factors"),
        label = "Select factor column(s)",
        choices = character(0),
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      actionButton(ns("add_run_number"), "Generate Run Number", class = "btn-primary")
    ),
    column(width = 12, tags$div(style = "display: none;", numericInput(ns("clear_trigger"), "", value = 0)))
  )
}

# Data Transformation Server
create_data_transformation_server <- function(id, working_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # State: list of transform specs (type, column, params, newname)
    specs <- reactiveVal(list())

    # When global data invalidation fires, clear_trigger is updated; clear transform state
    observeEvent(input$clear_trigger, {
      specs(list())
    }, ignoreInit = TRUE)

    # Register for global data invalidation (clear transforms when new data is loaded)
    config <- get_global_config()
    if (!is.null(config$register_module)) {
      config$register_module("data_transformation_module",
        ui_reset = function() {
          updateNumericInput(session, ns("clear_trigger"), value = runif(1))
        },
        validation = NULL
      )
    }

    # Show C parameter only for power1, power2, inverse
    output$param_c_ui <- renderUI({
      need_c <- input$transform_type %in% c("power1", "power2", "inverse")
      if (!need_c) return(NULL)
      numericInput(ns("param_c"), "C", value = 1, width = "100%")
    })

    # Update column choices and default random n when working data changes
    observe({
      wd <- working_data()
      if (is.null(wd) || !is.data.frame(wd) || ncol(wd) == 0) {
        shinyWidgets::updatePickerInput(session, "columns", choices = character(0))
        shinyWidgets::updatePickerInput(session, "run_factors", choices = character(0))
        return(invisible(NULL))
      }
      num_cols <- names(wd)[vapply(wd, is.numeric, logical(1))]
      if (length(num_cols) == 0) {
        shinyWidgets::updatePickerInput(session, "columns", choices = character(0))
      } else {
        choices <- stats::setNames(num_cols, num_cols)
        shinyWidgets::updatePickerInput(session, "columns", choices = choices)
      }
      all_choices <- stats::setNames(names(wd), names(wd))
      shinyWidgets::updatePickerInput(session, "run_factors", choices = all_choices)
      n <- nrow(wd)
      if (n > 0) updateNumericInput(session, "rand_n", value = n)
    })

    # Apply transform: add one spec per selected column
    observeEvent(input$apply_transform, {
      wd <- working_data()
      if (is.null(wd) || nrow(wd) == 0) return(invisible(NULL))
      cols <- input$columns
      if (is.null(cols) || length(cols) == 0) {
        shiny::showNotification("Please select at least one column.", type = "warning")
        return(invisible(NULL))
      }
      type <- input$transform_type
      if (is.null(type) || type == "") return(invisible(NULL))
      A <- if (is.null(input$param_a) || is.na(input$param_a)) 1 else input$param_a
      B <- if (is.null(input$param_b) || is.na(input$param_b)) 0 else input$param_b
      C <- if (is.null(input$param_c) || is.na(input$param_c)) 1 else input$param_c
      if (!type %in% c("power1", "power2", "inverse")) C <- 1
      existing <- c(names(wd), spec_output_names(specs()))
      new_specs <- specs()
      if (identical(type, "global_rank")) {
        valid_cols <- cols[cols %in% names(wd) & vapply(wd[cols], is.numeric, logical(1))]
        if (length(valid_cols) == 0) {
          shiny::showNotification("Please select at least one numeric column.", type = "warning")
          return(invisible(NULL))
        }
        ranked <- compute_global_rank(wd, valid_cols)
        if (is.null(ranked)) {
          shiny::showNotification("Global rank transform produced invalid values or error.", type = "warning")
          return(invisible(NULL))
        }
        newnames <- character(length(valid_cols))
        for (i in seq_along(valid_cols)) {
          base_name <- paste0(valid_cols[i], "_global_rank")
          newnames[i] <- make_unique_name(base_name, existing)
          existing <- c(existing, newnames[i])
        }
        specs(c(new_specs, list(list(
          type = "global_rank",
          columns = valid_cols,
          newnames = newnames
        ))))
        return(invisible(NULL))
      }
      msg_invalid <- switch(type,
        log = "Log requires A*X+B > 0 for all values.",
        inverse = "Inverse requires A*X ≠ 0 for all values.",
        power1 = , power2 = "Power requires A*X+B >= 0 (and C > 0 for Power2).",
        poisson_sqrt = "Poisson transform requires A*X+B >= 0 for all values.",
        "Transform produced invalid values or error."
      )
      for (col in cols) {
        if (!col %in% names(wd) || !is.numeric(wd[[col]])) next
        x <- wd[[col]]
        newcol <- compute_transform(type, x, A, B, C)
        if (is.null(newcol)) {
          shiny::showNotification(paste0(col, ": ", msg_invalid), type = "warning")
          next
        }
        base_name <- paste0(col, "_", type)
        newname <- make_unique_name(base_name, existing)
        existing <- c(existing, newname)
        new_specs <- c(new_specs, list(list(
          type = type,
          column = col,
          params = c(A = A, B = B, C = C),
          newname = newname
        )))
      }
      specs(new_specs)
    })

    # Add random distribution column
    observeEvent(input$add_random, {
      wd <- working_data()
      n <- if (is.null(input$rand_n) || is.na(input$rand_n)) 100L else as.integer(input$rand_n)
      if (is.null(wd) || nrow(wd) == 0) n <- max(1L, n)
      dist_type <- input$rand_dist
      if (is.null(dist_type) || dist_type == "") return(invisible(NULL))
      seed_parsed <- parse_optional_seed(input$rand_seed)
      if (!isTRUE(seed_parsed$ok)) {
        shiny::showNotification("Seed must be an integer (or leave blank for a random sample).", type = "warning")
        return(invisible(NULL))
      }
      A <- if (is.null(input$rand_a) || is.na(input$rand_a)) 0 else input$rand_a
      B <- if (is.null(input$rand_b) || is.na(input$rand_b)) 1 else input$rand_b
      C <- if (is.null(input$rand_c) || is.na(input$rand_c)) 0 else input$rand_c
      if (dist_type == "uniform") { lo <- min(A, B); hi <- max(A, B); A <- lo; B <- hi }
      if (dist_type == "normal" && B < 0) B <- 0
      if (dist_type == "exponential" && B <= 0) B <- 1
      if (dist_type == "chisq" && A <= 0) A <- 1
      vec <- generate_random_column(dist_type, n, A, B, C, seed = seed_parsed$seed)
      if (is.null(vec)) {
        shiny::showNotification("Invalid parameters for random distribution.", type = "error")
        return(invisible(NULL))
      }
      existing <- c(if (is.null(wd)) character(0) else names(wd),
                   spec_output_names(specs()))
      base_name <- trimws(input$rand_colname)
      if (base_name == "") base_name <- paste0(dist_type, "_", A, "_", B, if (dist_type == "chisq") paste0("_", C) else "")
      newname <- make_unique_name(base_name, existing)
      new_specs <- c(specs(), list(list(
        type = dist_type,
        column = NULL,
        params = c(A = A, B = B, C = C),
        newname = newname,
        values = vec,
        n = n,
        seed = seed_parsed$seed
      )))
      specs(new_specs)
    })

    # Generate run number column from selected factor combinations
    observeEvent(input$add_run_number, {
      wd <- working_data()
      if (is.null(wd) || nrow(wd) == 0) return(invisible(NULL))
      factor_cols <- input$run_factors
      if (is.null(factor_cols) || length(factor_cols) == 0) {
        shiny::showNotification("Please select at least one factor column.", type = "warning")
        return(invisible(NULL))
      }
      if (!all(factor_cols %in% names(wd))) {
        shiny::showNotification("Selected factor columns are not in the current data.", type = "warning")
        return(invisible(NULL))
      }
      newcol <- generate_run_numbers(wd, factor_cols)
      if (is.null(newcol)) {
        shiny::showNotification("Could not generate run numbers from selected factors.", type = "error")
        return(invisible(NULL))
      }
      existing <- c(names(wd), spec_output_names(specs()))
      newname <- make_unique_name("Run Number", existing)
      specs(c(specs(), list(list(
        type = "run_number",
        factor_columns = factor_cols,
        newname = newname
      ))))
    })

    # Warn when transformed data contains Inf or NaN (e.g. log(0), 1/0)
    output$nonfinite_warning <- renderUI({
      d <- data()
      chk <- check_nonfinite(d)
      if (!chk$has_issue) return(NULL)
      parts <- character(0)
      if (length(chk$inf_columns) > 0L) {
        parts <- c(parts, paste0("Inf in: ", paste(chk$inf_columns, collapse = ", ")))
      }
      if (length(chk$nan_columns) > 0L) {
        parts <- c(parts, paste0("NaN in: ", paste(chk$nan_columns, collapse = ", ")))
      }
      msg <- paste(parts, collapse = ". ")
      div(
        class = "alert alert-warning",
        role = "alert",
        strong("Non-finite values in transformed data: "),
        msg,
        ". These are treated as missing (NA) in Dynamic Filter and in exports."
      )
    })

    # Summary of applied transforms
    output$specs_summary <- renderPrint({
      s <- specs()
      if (length(s) == 0) {
        cat("No transforms applied.\n")
        return(invisible(NULL))
      }
      for (i in seq_along(s)) {
        x <- s[[i]]
        if (identical(x$type, "run_number")) {
          cat(sprintf("%s: [run number] %s -> %s\n",
                      i, paste(x$factor_columns, collapse = ", "), x$newname))
          next
        }
        if (identical(x$type, "global_rank")) {
          cat(sprintf("%s: [global rank] %s -> %s (ties = average)\n",
                      i, paste(x$columns, collapse = ", "), paste(x$newnames, collapse = ", ")))
          next
        }
        p <- x$params
        if (!is.null(x$column)) {
          if (x$type %in% c("power1", "power2", "inverse")) {
            cat(sprintf("%s: %s -> %s (A=%.4g, B=%.4g, C=%.4g)\n",
                        i, x$column, x$newname, p["A"], p["B"], p["C"]))
          } else {
            cat(sprintf("%s: %s -> %s (A=%.4g, B=%.4g)\n",
                        i, x$column, x$newname, p["A"], p["B"]))
          }
        } else {
          seed_part <- if (!is.null(x$seed)) sprintf(", seed=%s", x$seed) else ""
          cat(sprintf("%s: [random %s] -> %s (A=%.4g, B=%.4g, C=%.4g, n=%s%s)\n",
                      i, x$type, x$newname, p["A"], p["B"], p["C"], x$n, seed_part))
        }
      }
      invisible(NULL)
    })

    # Data reactive: working data + all transformed and random columns
    data <- reactive({
      assemble_transformed_data(working_data(), specs())
    })

    return(list(data = data))
  })
}
