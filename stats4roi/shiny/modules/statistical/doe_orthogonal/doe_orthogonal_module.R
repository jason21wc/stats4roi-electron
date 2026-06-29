# DOE Screening Design Module (Tsui 1988)
# Orthogonal arrays and confounding tables for experiment planning.

library(shiny)
library(DT)

source("modules/config/global_config.R")
source("modules/config/global_data_invalidation.R")
source("modules/statistical/doe_orthogonal/doe_orthogonal_data.R")
source("modules/statistical/doe_orthogonal/doe_orthogonal_utils.R")

# Sentinel value for initial checkboxGroupInput (Shiny binds poorly when choices = character(0)).
# Filter out everywhere we read input$interactions for logic.
DOE_INTERACTION_PLACEHOLDER_VALUE <- "__doe_inter_placeholder__"

# =============================================================================
# UI
# =============================================================================
create_doe_orthogonal_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "DOE Screening Design",
    sidebarLayout(
      sidebarPanel(
        h4("Design setup"),
        numericInput(ns("n_factors"), "Number of factors", value = 3, min = 2, max = 20, step = 1),
        textInput(ns("factor_names_custom"), "Factor names (optional; comma-separated, default A, B, C, ...)", placeholder = "e.g. Temp, Pressure, Speed"),
        uiOutput(ns("levels_ui")),
        uiOutput(ns("odd_level_merge_ui")),
        # Button label is rendered in server (updateActionButton did not change the visible label in this app).
        uiOutput(ns("interactions_select_all_btn")),
        # Built in server (renderUI); updateCheckboxGroupInput was unreliable for this control.
        uiOutput(ns("interactions_ui")),
        uiOutput(ns("array_choice_ui")),
        actionButton(ns("design_btn"), "Design the Experiment", class = "btn-primary"),
        hr(),
        p("Based on Tsui (1988), ", tags$a("Strategies for Planning Experiments Using Orthogonal Arrays and Confounding Tables", href = "https://doi.org/10.1002/qre.4680040207", target = "_blank"), ", Quality and Reliability Engineering International, 4, 113-122.")
      ),
      mainPanel(
        # Status / errors in uiOutput; assignment + run sheet DT outputs are static so
        # DataTables initializes reliably (DT inside renderUI often shows headers-only).
        uiOutput(ns("design_status_ui")),
        uiOutput(ns("design_results_top_ui")),
        DT::dataTableOutput(ns("assign_table")),
        uiOutput(ns("design_results_run_header_ui")),
        DT::dataTableOutput(ns("run_sheet_table")),
        checkboxInput(ns("show_alias"), "Show alias table", value = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("show_alias"), "'] == true"),
          radioButtons(
            ns("alias_display"),
            "Show confounding effects",
            choices = c("2-way only" = "2fi", "2-way and 3-way" = "2fi_3fi", "All" = "all"),
            selected = "2fi_3fi",
            inline = TRUE
          ),
          p(
            class = "text-muted",
            style = "font-size: 0.9em; margin-top: 0.5rem;",
            strong("Bold"),
            ": confounding from Taguchi column pairs (first pass). ",
            "Plain text: confounding from multi-column products and structural terms (second pass)."
          ),
          DT::dataTableOutput(ns("alias_table"))
        ),
        uiOutput(ns("design_results_bottom_ui"))
      )
    )
  )
}

# =============================================================================
# Server
# =============================================================================
create_doe_orthogonal_server <- function(id, color_palette = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Lazy alias tables per design (avoid recomputing when only alias_display changes)
    alias_cache_env <- new.env(parent = emptyenv())
    alias_cache_env$key <- NULL

    clear_doe_alias_cache <- function() {
      alias_cache_env$key <- NULL
      for (nm in c("t2fi", "t23", "tall")) {
        if (exists(nm, envir = alias_cache_env, inherits = FALSE)) {
          rm(list = nm, envir = alias_cache_env)
        }
      }
    }

    .doe_safe_n_factors <- function(raw_n, default_n = 3L, min_n = 2L, max_n = 20L) {
      n <- suppressWarnings(as.integer(raw_n))
      if (length(n) == 0L || is.na(n[1L])) n <- as.integer(default_n)
      n <- n[1L]
      n <- min(max_n, max(min_n, n))
      as.integer(n)
    }

    # Global invalidation pattern: setup fingerprint vs snapshot taken at last successful "Design"
    doe_setup_trigger <- create_doe_orthogonal_setup_trigger(input)
    committed_design_setup <- reactiveVal(NULL)
    design_result <- reactiveVal(NULL)

    observeEvent(doe_setup_trigger(), {
      comm <- committed_design_setup()
      if (is.null(comm)) return(invisible(NULL))
      cur <- doe_setup_trigger()
      if (!identical(cur, comm)) {
        design_result(NULL)
        committed_design_setup(NULL)
        clear_doe_alias_cache()
      }
    }, ignoreInit = TRUE)

    # Factor names: default A, B, C, ... or user override (comma-separated)
    factor_names <- reactive({
      n <- .doe_safe_n_factors(input$n_factors %||% 3)
      custom <- trimws(strsplit(input$factor_names_custom %||% "", ",")[[1]])
      if (length(custom) >= n) return(custom[seq_len(n)])
      default <- paste0(LETTERS[seq_len(min(n, 26))], if (n > 26) rep("", n - 26) else NULL)[seq_len(n)]
      if (length(custom) > 0) default[seq_along(custom)] <- custom
      default
    })

    # Level names: default "1", "2", "3", ... per factor; optional override (one line per factor, comma-separated)
    level_names <- reactive({
      n <- .doe_safe_n_factors(input$n_factors %||% 3)
      levels_vec <- vapply(seq_len(n), function(i) as.integer(input[[paste0("level_", i)]] %||% 2), integer(1))
      raw <- input$level_names_custom %||% ""
      lines <- trimws(strsplit(raw, "\n")[[1]])
      out <- list()
      for (i in seq_len(n)) {
        nlev <- levels_vec[i]
        default <- as.character(seq_len(nlev))
        if (length(lines) >= i && nzchar(lines[i])) {
          custom <- trimws(strsplit(lines[i], ",")[[1]])
          if (length(custom) >= nlev) out[[i]] <- custom[seq_len(nlev)] else out[[i]] <- default
        } else {
          out[[i]] <- default
        }
      }
      out
    })

    # Levels per factor: dynamic UI (per-factor selection)
    output$levels_ui <- renderUI({
      n <- .doe_safe_n_factors(input$n_factors %||% 3)
      lev_choices_2 <- c("2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7, "8" = 8)
      lev_choices_3 <- c("3" = 3, "9" = 9)
      tagList(
        lapply(seq_len(n), function(i) {
          selectInput(
            ns(paste0("level_", i)),
            label = paste0("Factor ", factor_names()[i], " levels"),
            choices = c(lev_choices_2, lev_choices_3),
            selected = 2
          )
        }),
        textAreaInput(ns("level_names_custom"), "Level names (optional; one line per factor, comma-separated)", value = "", rows = 3, placeholder = "e.g. Low, High\n1, 2, 3\nCold, Warm, Hot")
      )
    })

    # Which level gets doubled (for odd-level factors)
    output$odd_level_merge_ui <- renderUI({
      n <- .doe_safe_n_factors(input$n_factors %||% 3)
      levels_vec <- vapply(seq_len(n), function(i) as.integer(input[[paste0("level_", i)]] %||% 2), integer(1))
      odd_idx <- which(levels_vec == 3L | (levels_vec >= 5L & levels_vec <= 7L))
      oa_choice <- input$array_choice %||% ""
      is_power3_oa <- oa_choice %in% c("L9", "L27", "L81")
      if (is_power3_oa) return(NULL)
      if (length(odd_idx) == 0) return(NULL)
      tagList(lapply(odd_idx, function(i) {
        lev <- levels_vec[i]
        if (lev == 3) {
          selectInput(ns(paste0("merge_", i)), paste0("Level 4 merged into (Factor ", factor_names()[i], ")"), choices = c("2" = 2, "3" = 3), selected = 2)
        } else {
          selectInput(ns(paste0("merge_", i)), paste0("Levels 6-8 merged into (Factor ", factor_names()[i], ")"), choices = as.character(1:5), selected = "2")
        }
      }))
    })

    # All two-way interaction labels (Unicode × between factor names)
    interaction_pair_choices <- reactive({
      n <- .doe_safe_n_factors(input$n_factors %||% 3L)
      fnames <- factor_names()
      pairs <- character(0)
      for (i in seq_len(n - 1)) for (j in (i + 1):n) pairs <- c(pairs, paste0(fnames[i], "\u00D7", fnames[j]))
      pairs
    })

    # Select-all / deselect-all: bump state and set selected inside renderUI (updateCheckboxGroupInput does not update this control).
    interactions_pairs_digest <- reactive({
      digest::digest(sort(interaction_pair_choices()))
    })
    interactions_select_all_nonce <- reactiveVal(0L)
    interactions_select_all_tgt_digest <- reactiveVal(NULL)
    interactions_deselect_all_nonce <- reactiveVal(0L)
    interactions_deselect_all_tgt_digest <- reactiveVal(NULL)
    observeEvent(interactions_pairs_digest(), {
      interactions_select_all_nonce(0L)
      interactions_select_all_tgt_digest(NULL)
      interactions_deselect_all_nonce(0L)
      interactions_deselect_all_tgt_digest(NULL)
    }, ignoreInit = TRUE)

    # Recreate when factor count/names change; preserve selection via isolate(input$interactions).
    output$interactions_ui <- renderUI({
      pairs <- interaction_pair_choices()
      nonce_s <- interactions_select_all_nonce()
      tgt_s <- interactions_select_all_tgt_digest()
      nonce_d <- interactions_deselect_all_nonce()
      tgt_d <- interactions_deselect_all_tgt_digest()
      d <- digest::digest(sort(pairs))
      cur <- tryCatch(isolate(input$interactions %||% character(0)), error = function(e) character(0))
      cur <- setdiff(cur, DOE_INTERACTION_PLACEHOLDER_VALUE)
      use_deselect_all <- isTRUE(nonce_d > 0L) && !is.null(tgt_d) && identical(d, tgt_d) && length(pairs) > 0L
      use_select_all <- isTRUE(nonce_s > 0L) && !is.null(tgt_s) && identical(d, tgt_s) && length(pairs) > 0L
      sel <- if (isTRUE(use_deselect_all)) {
        character(0)
      } else if (isTRUE(use_select_all)) {
        pairs
      } else {
        intersect(cur, pairs)
      }
      if (length(pairs) == 0L) {
        ch <- c("\u2026" = DOE_INTERACTION_PLACEHOLDER_VALUE)
        sel <- character(0)
      } else {
        ch <- pairs
      }
      checkboxGroupInput(
        ns("interactions"),
        "Required two-way interactions",
        choices = ch,
        selected = sel
      )
    })

    observeEvent(input$interactions_select_all, {
      req(input$interactions_select_all > 0L)
      pairs <- interaction_pair_choices()
      req(length(pairs) > 0L)
      cur <- setdiff(input$interactions %||% character(0), DOE_INTERACTION_PLACEHOLDER_VALUE)
      all_selected <- length(intersect(cur, pairs)) == length(pairs)
      d <- digest::digest(sort(pairs))
      if (isTRUE(all_selected)) {
        # Clear stale select flags so deselect is not competing with select (both true → deselect wins first, button stuck).
        interactions_select_all_nonce(0L)
        interactions_select_all_tgt_digest(NULL)
        interactions_deselect_all_tgt_digest(d)
        interactions_deselect_all_nonce(interactions_deselect_all_nonce() + 1L)
      } else {
        interactions_deselect_all_nonce(0L)
        interactions_deselect_all_tgt_digest(NULL)
        interactions_select_all_tgt_digest(d)
        interactions_select_all_nonce(interactions_select_all_nonce() + 1L)
      }
    })

    # Dynamic label on the action button (updateActionButton did not update the DOM; same pattern as interactions_ui).
    output$interactions_select_all_btn <- renderUI({
      pairs <- interaction_pair_choices()
      cur <- setdiff(input$interactions %||% character(0), DOE_INTERACTION_PLACEHOLDER_VALUE)
      all_on <- length(pairs) > 0L && length(intersect(cur, pairs)) == length(pairs)
      lab <- if (isTRUE(all_on)) {
        "Deselect all two-way interactions"
      } else {
        "Select all two-way interactions"
      }
      actionButton(ns("interactions_select_all"), lab, class = "btn-secondary btn-sm")
    })

    # Possible arrays and selection (pre-select smallest)
    feasible <- reactive({
      n <- .doe_safe_n_factors(input$n_factors %||% 3)
      levels_vec <- vapply(seq_len(n), function(i) as.integer(input[[paste0("level_", i)]] %||% 2), integer(1))
      int_raw <- setdiff(input$interactions %||% character(0), DOE_INTERACTION_PLACEHOLDER_VALUE)
      int_list <- list()
      fnames <- factor_names()
      for (s in int_raw) {
        s <- gsub("\u00D7", "x", s)
        for (i in seq_len(n - 1)) for (j in (i + 1):n) {
          if (s == paste0(fnames[i], "x", fnames[j])) int_list[[length(int_list) + 1]] <- c(i, j)
        }
      }
      doe_feasible_arrays(n, levels_vec, int_list)
    })

    output$array_choice_ui <- renderUI({
      p2 <- feasible()$possible_2level
      p3 <- feasible()$possible_3level
      pm <- feasible()$possible_mixed %||% character(0)
      choices <- c(p2, p3, pm)
      if (length(choices) > 1L) {
        runs <- suppressWarnings(as.integer(sub("^L", "", choices)))
        ord <- order(ifelse(is.na(runs), .Machine$integer.max, runs), choices)
        choices <- choices[ord]
      }
      if (length(choices) == 0) {
        return(p("No array can accommodate the current design. Add fewer factors or interactions, or use more levels."))
      }
      default <- choices[1L]
      selectInput(ns("array_choice"), "Orthogonal array", choices = choices, selected = default)
    })

    observeEvent(input$design_btn, {
      req(input$design_btn > 0L)
      n <- .doe_safe_n_factors(input$n_factors %||% 3)
      levels_vec <- vapply(seq_len(n), function(i) as.integer(input[[paste0("level_", i)]] %||% 2), integer(1))
      int_raw <- setdiff(input$interactions %||% character(0), DOE_INTERACTION_PLACEHOLDER_VALUE)
      int_list <- list()
      fnames <- factor_names()
      for (s in int_raw) {
        s <- gsub("\u00D7", "x", s)
        for (i in seq_len(n - 1)) for (j in (i + 1):n) {
          if (s == paste0(fnames[i], "x", fnames[j])) int_list[[length(int_list) + 1]] <- c(i, j)
        }
      }
      oa_choice <- input$array_choice %||% feasible()$possible_2level[1] %||% feasible()$possible_3level[1] %||% (feasible()$possible_mixed %||% character(0))[1]
      if (is.null(oa_choice)) {
        design_result(list(error = "Select an array"))
        committed_design_setup(isolate(doe_setup_trigger()))
        return(invisible(NULL))
      }

      odd_merge <- list()
      for (i in seq_len(n)) {
        lev <- levels_vec[i]
        if (lev == 3) {
          m <- as.integer(input[[paste0("merge_", i)]] %||% 2)
          if (i %in% which(levels_vec == 3)) odd_merge[[as.character(i)]] <- m
        } else if (lev >= 5 && lev <= 7) {
          m <- as.integer(input[[paste0("merge_", i)]] %||% 2)
          odd_merge[[as.character(i)]] <- m
        }
      }

      if (oa_choice %in% c("L4", "L8", "L12", "L16", "L32", "L64")) {
        res <- doe_assign_2level(oa_choice, n, levels_vec, int_list, factor_names(), NULL, odd_merge)
      } else if (oa_choice %in% names(DOE_OA_MIXED)) {
        res <- doe_assign_mixed(oa_choice, n, levels_vec, int_list, factor_names(), NULL, odd_merge)
      } else {
        res <- doe_assign_3level(oa_choice, n, levels_vec, int_list, factor_names(), NULL, odd_merge)
      }
      if (is.null(res$error)) {
        res$assignment_run_table <- doe_assignment_run_table(oa_choice, res$assignment_list, factor_names())
        res$factor_names <- factor_names()
        res$level_names <- level_names()
        res$levels_vec <- levels_vec
        res$oa_label <- oa_choice
        res$required_interactions <- int_list
      }
      design_result(res)
      clear_doe_alias_cache()
      committed_design_setup(isolate(doe_setup_trigger()))
    })

    # Alias table: compute each display mode at most once per design; toggle is cheap
    alias_table <- reactive({
      res <- design_result()
      if (is.null(res) || !is.null(res$error)) return(NULL)
      req(!is.null(res$assignment_list))
      # Include factor names in cache key so clearing/renaming factors after a redesign
      # invalidates cached alias tables (assignment geometry alone is not enough).
      k <- paste0(
        res$oa_label, "|",
        paste(vapply(res$assignment_list, function(a) {
          paste0(a$type, ":", paste(a$columns, collapse = ","), ":", as.character(a$factor)[1L])
        }, character(1)), collapse = ";"),
        "|fn:", paste(res$factor_names %||% character(0), collapse = ",")
      )
      if (!identical(alias_cache_env$key, k)) {
        alias_cache_env$key <- k
        # Must rm() bindings: assigning NULL leaves exists(name)==TRUE in R, so the cache
        # would skip recomputation and get(name)$df would become NULL (empty table).
        for (nm in c("t2fi", "t23", "tall")) {
          if (exists(nm, envir = alias_cache_env, inherits = FALSE)) {
            rm(list = nm, envir = alias_cache_env)
          }
        }
      }
      mode <- input$alias_display %||% "2fi_3fi"
      slot <- switch(mode, "2fi" = "t2fi", "2fi_3fi" = "t23", "all" = "tall", "t23")
      if (!exists(slot, envir = alias_cache_env, inherits = FALSE)) {
        df <- doe_alias_table(
          res$oa_label,
          res$assignment_list,
          res$required_interactions %||% list(),
          factor_names = res$factor_names,
          alias_display = mode
        )
        assign(slot, list(df = df), envir = alias_cache_env)
      }
      get(slot, envir = alias_cache_env)$df
    })

    output$design_status_ui <- renderUI({
      res <- design_result()
      if (!is.null(res) && is.null(res$error)) {
        return(NULL)
      }
      if (is.null(res)) {
        return(fluidRow(column(12, p("Click 'Design the Experiment' to generate a design."))))
      }
      fluidRow(column(12, p(style = "color: red;", res$error)))
    })

    output$design_results_top_ui <- renderUI({
      res <- design_result()
      if (is.null(res) || !is.null(res$error)) {
        return(NULL)
      }
      {
        res_tip <- paste0(
          "III: main effects may be confounded with two-factor interactions. ",
          "IV: main effects are not confounded with two-factor interactions, but two-factor interactions may be confounded with each other. ",
          "V: main effects and two-factor interactions are not confounded with other main effects or two-factor interactions (they may be confounded with three-factor interactions). ",
          "Resolution uses the shortest word length in the defining relation. ",
          "Reference: https://www.itl.nist.gov/div898/handbook/pri/section3/pri3344.htm"
        )
        tagList(
          p(
            strong("Design resolution:"),
            tags$span(
              class = "text-muted",
              style = "cursor: help; margin-left: 0.2rem;",
              title = res_tip,
              HTML("&#9432;")
            ),
            " ",
            res$design_resolution %||% "—"
          ),
          h4("Factor–column assignment")
        )
      }
    })

    output$design_results_run_header_ui <- renderUI({
      res <- design_result()
      if (is.null(res) || !is.null(res$error)) {
        return(NULL)
      }
      h4("Run sheet")
    })

    output$design_results_bottom_ui <- renderUI({
      res <- design_result()
      if (is.null(res) || !is.null(res$error)) {
        return(NULL)
      }
      ns <- session$ns
      tagList(
        h4("Export"),
        checkboxGroupInput(ns("export_which"), "Export", choices = c("Run sheet" = "run", "Column assignment" = "assign", "Alias table" = "alias"), selected = "run"),
        downloadButton(ns("download_btn"), "Download selected (CSV)")
      )
    })

    output$assign_table <- DT::renderDataTable({
      res <- design_result()
      if (is.null(res) || !is.null(res$error)) return(NULL)
      # Factor-column assignment: Run in first column, then each OA column with assigned effect in header, OA levels in rows
      doe_assignment_run_table(res$oa_label, res$assignment_list, res$factor_names)
    }, options = list(paging = FALSE, searching = FALSE, autoWidth = TRUE, columnDefs = list(list(className = "dt-center", targets = "_all"))))

    output$run_sheet_table <- DT::renderDataTable({
      res <- design_result()
      if (is.null(res) || !is.null(res$error)) return(NULL)
      doe_run_sheet_table(
        oa_label = res$oa_label,
        run_sheet = res$run_sheet,
        assignment_list = res$assignment_list,
        factor_names = res$factor_names,
        levels_vec = res$levels_vec,
        level_names_per_factor = res$level_names
      )
    }, options = list(paging = FALSE, searching = FALSE, autoWidth = TRUE, columnDefs = list(list(className = "dt-center", targets = "_all"))))

    output$alias_table <- DT::renderDataTable({
      req(isTRUE(input$show_alias))
      res <- design_result()
      if (is.null(res) || !is.null(res$error)) return(NULL)
      at <- alias_table()
      if (is.null(at)) return(NULL)
      at
    }, escape = c(TRUE, TRUE, FALSE), options = list(paging = FALSE, searching = FALSE, autoWidth = TRUE, columnDefs = list(list(className = "dt-center", targets = "_all"))))
    shiny::outputOptions(output, "alias_table", suspendWhenHidden = TRUE)

    # Sanitize data for CSV: replace Unicode × (U+00D7) with ASCII "x" so files open correctly;
    # strip HTML from alias Confounded_interactions (bold vs plain in the UI).
    sanitize_for_csv <- function(df) {
      if (is.null(df) || !is.data.frame(df)) return(df)
      colnames(df) <- gsub("\u00D7", "x", colnames(df), fixed = TRUE)
      for (j in seq_len(ncol(df))) {
        if (is.character(df[[j]]) || is.factor(df[[j]])) {
          col <- gsub("\u00D7", "x", as.character(df[[j]]), fixed = TRUE)
          if (identical(names(df)[j], "Confounded_interactions"))
            col <- gsub("<[^>]*>", "", col, perl = TRUE)
          df[[j]] <- col
        }
      }
      df
    }

    output$download_btn <- downloadHandler(
      filename = function() {
        which <- input$export_which %||% "run"
        if (length(which) > 1) paste0("doe_design_", format(Sys.time(), "%Y%m%d_%H%M"), ".zip") else paste0("doe_design_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
      },
      content = function(file) {
        res <- design_result()
        if (is.null(res) || !is.null(res$error)) return()
        which <- input$export_which %||% "run"
        if (length(which) == 0) which <- "run"
        out <- list()
        if ("run" %in% which) {
          rs <- doe_run_sheet_table(
            oa_label = res$oa_label,
            run_sheet = res$run_sheet,
            assignment_list = res$assignment_list,
            factor_names = res$factor_names,
            levels_vec = res$levels_vec,
            level_names_per_factor = res$level_names
          )
          out$run_sheet <- sanitize_for_csv(rs)
        }
        if ("assign" %in% which) out$column_assignment <- sanitize_for_csv(res$assignment_run_table %||% res$assignment)
        if ("alias" %in% which) {
          at <- alias_table()
          if (!is.null(at)) out$alias <- sanitize_for_csv(at)
        }
        if (length(out) == 1) {
          write.csv(out[[1]], file, row.names = FALSE)
        } else {
          td <- tempdir()
          fnames <- character(0)
          for (i in seq_along(out)) {
            fn <- file.path(td, paste0("doe_", names(out)[i], ".csv"))
            write.csv(out[[i]], fn, row.names = FALSE)
            fnames <- c(fnames, fn)
          }
          if (requireNamespace("zip", quietly = TRUE)) {
            zip::zip(file, fnames, mode = "cherry-pick")
          } else {
            write.csv(out[[1]], file, row.names = FALSE)
          }
        }
      }
    )
  })
}
