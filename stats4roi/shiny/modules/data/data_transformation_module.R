# Data Transformation Module for stats4ROI
# Allows mathematical transforms and random distribution columns on working data.
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
      NULL
    )
  }, error = function(e) NULL)
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

# Generate a random column of length n. params: A, B, C per distribution.
# Returns numeric vector or NULL on error.
generate_random_column <- function(type, n, A, B, C = 0) {
  n <- as.integer(n)
  if (is.na(n) || n < 1) return(NULL)
  A <- as.numeric(A)
  B <- as.numeric(B)
  C <- as.numeric(C)
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
          "Sine (y = sin(A*X+B), radians)" = "sine"
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
      actionButton(ns("add_random"), "Add column", class = "btn-primary")
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
        ui_reset = function(session) {
          updateNumericInput(session, "transform-clear_trigger", value = runif(1))
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
        return(invisible(NULL))
      }
      num_cols <- names(wd)[vapply(wd, is.numeric, logical(1))]
      if (length(num_cols) == 0) {
        shinyWidgets::updatePickerInput(session, "columns", choices = character(0))
      } else {
        choices <- stats::setNames(num_cols, num_cols)
        shinyWidgets::updatePickerInput(session, "columns", choices = choices)
      }
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
      existing <- c(names(wd), vapply(specs(), `[[`, character(1), "newname"))
      new_specs <- specs()
      msg_invalid <- switch(type,
        log = "Log requires A*X+B > 0 for all values.",
        inverse = "Inverse requires A*X ≠ 0 for all values.",
        power1 = , power2 = "Power requires A*X+B >= 0 (and C > 0 for Power2).",
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
      A <- if (is.null(input$rand_a) || is.na(input$rand_a)) 0 else input$rand_a
      B <- if (is.null(input$rand_b) || is.na(input$rand_b)) 1 else input$rand_b
      C <- if (is.null(input$rand_c) || is.na(input$rand_c)) 0 else input$rand_c
      if (dist_type == "uniform") { lo <- min(A, B); hi <- max(A, B); A <- lo; B <- hi }
      if (dist_type == "normal" && B < 0) B <- 0
      if (dist_type == "exponential" && B <= 0) B <- 1
      if (dist_type == "chisq" && A <= 0) A <- 1
      vec <- generate_random_column(dist_type, n, A, B, C)
      if (is.null(vec)) {
        shiny::showNotification("Invalid parameters for random distribution.", type = "error")
        return(invisible(NULL))
      }
      existing <- c(if (is.null(wd)) character(0) else names(wd),
                   vapply(specs(), `[[`, character(1), "newname"))
      base_name <- trimws(input$rand_colname)
      if (base_name == "") base_name <- paste0(dist_type, "_", A, "_", B, if (dist_type == "chisq") paste0("_", C) else "")
      newname <- make_unique_name(base_name, existing)
      new_specs <- c(specs(), list(list(
        type = dist_type,
        column = NULL,
        params = c(A = A, B = B, C = C),
        newname = newname,
        values = vec,
        n = n
      )))
      specs(new_specs)
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
          cat(sprintf("%s: [random %s] -> %s (A=%.4g, B=%.4g, C=%.4g, n=%s)\n",
                      i, x$type, x$newname, p["A"], p["B"], p["C"], x$n))
        }
      }
      invisible(NULL)
    })

    # Data reactive: working data + all transformed and random columns
    data <- reactive({
      wd <- working_data()
      if (is.null(wd) || !is.data.frame(wd)) return(wd)
      s <- specs()
      if (length(s) == 0) return(wd)
      out <- wd
      n_out <- nrow(wd)
      for (x in s) {
        if (!is.null(x$column)) {
          type <- x$type
          col_vec <- wd[[x$column]]
          p <- x$params
          newcol <- compute_transform(type, col_vec, p["A"], p["B"], p["C"])
          if (!is.null(newcol)) {
            out <- dplyr::bind_cols(out, tibble::tibble(!!x$newname := newcol))
          }
        } else {
          # Random column: use stored values, truncate or pad to match current nrow
          vec <- x$values
          if (length(vec) >= n_out) {
            newcol <- vec[seq_len(n_out)]
          } else {
            newcol <- c(vec, rep(NA_real_, n_out - length(vec)))
          }
          out <- dplyr::bind_cols(out, tibble::tibble(!!x$newname := newcol))
        }
      }
      out
    })

    return(list(data = data))
  })
}
