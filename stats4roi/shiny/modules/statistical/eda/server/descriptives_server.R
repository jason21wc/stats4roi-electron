# Descriptives Server Component
# This worker module follows the new architectural patterns and returns reactive functions only

# =============================================================================
# IMPORTS
# =============================================================================
library(shiny)
library(DT)
library(dplyr)
library(lolcat)

# Source global systems
source("modules/config/global_config.R")

# =============================================================================
# WORKER SERVER FUNCTION
# =============================================================================
create_descriptives_server <- function(id, data_source, data_type_reactive, input_values) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================================
    # INPUT VALIDATION AND EXTRACTION
    # =========================================================================
    # Extract inputs with validation (following worker template pattern)
    decimals <- reactive({
      input_vals <- input_values()
      dec <- input_vals$decimals_desc2
      if (is.null(dec) || is.na(dec)) return(5)
      dec
    })
    
    desc_stats <- reactive({
      input_vals <- input_values()
      stats <- input_vals$desc_stats
      if (is.null(stats) || length(stats) == 0) return(character(0))
      stats
    })
    
    data_list_for_desc <- reactive({
      input_vals <- input_values()
      data_col <- input_vals$data_list_for_desc
      if (is.null(data_col)) return(NULL)
      data_col
    })

    quantile_type <- reactive({
      normalize_quantile_type(input_values()$desc_quantile_type)
    })
    
    # =========================================================================
    # DATA PROCESSING REACTIVE
    # =========================================================================
    processed_data <- reactive({
      # Get data from coordinator
      data <- data_source()
      req(data)
      
      # Get data type and selections from coordinator
      data_type <- data_type_reactive()
      selections <- input_values()
      
      # Validate data
      if (is.null(data) || nrow(data) == 0) {
        return(data.frame())
      }
      
      # Process data based on type
      if (data_type == 1) {
        # Column analysis
        if (is.null(selections$eda_UI1) || length(selections$eda_UI1) == 0) {
          return(data.frame())
        }
        selected_cols <- as.numeric(selections$eda_UI1)
        result <- data[, selected_cols, drop = FALSE]
      } else if (data_type == 2) {
        # Factor analysis - return full data for formula-based analysis
        if (is.null(selections$eda_UI1) || is.null(selections$eda_UI2)) {
          return(data.frame())
        }
        # For factor analysis, we need the full data because summary.continuous uses formula
        result <- data
      } else {
        return(data.frame())
      }
      
      as.data.frame(result)
    })
    
    # =========================================================================
    # HELPER FUNCTIONS
    # =========================================================================
    wants_sample_mode <- function(desc_stats) {
      any(grepl("^stat\\.sample\\.mode=T", desc_stats))
    }
    
    format_sample_mode_value <- function(x) {
      if (length(x) == 0L || all(is.na(x))) {
        return(NA_character_)
      }
      if (length(x) == 1L) {
        return(as.character(x)[1])
      }
      paste(as.character(x), collapse = ", ")
    }
    
    insert_sample_mode_column <- function(output, values) {
      output$sample.mode <- values
      nms <- names(output)
      nms <- nms[nms != "sample.mode"]
      if ("true.mode" %in% nms) {
        ti <- match("true.mode", nms)
        nms <- c(nms[seq_len(ti - 1)], "sample.mode", nms[seq(ti, length(nms))])
      } else if ("median" %in% nms) {
        mi <- match("median", nms)
        nms <- c(nms[seq_len(mi)], "sample.mode", nms[seq(mi + 1, length(nms))])
      } else {
        nms <- c(nms, "sample.mode")
      }
      output[, nms, drop = FALSE]
    }
    
    append_sample_mode <- function(output, data, selected_stats, dep_name = NULL, group_cols = NULL) {
      if (is.null(output) || nrow(output) == 0L || !wants_sample_mode(selected_stats)) {
        return(output)
      }
      
      values <- if (is.null(group_cols) || length(group_cols) == 0L) {
        if (!"dv.name" %in% names(output)) {
          return(output)
        }
        vapply(
          output$dv.name,
          function(nm) format_sample_mode_value(sample.mode(data[[nm]])),
          FUN.VALUE = character(1)
        )
      } else {
        vapply(
          seq_len(nrow(output)),
          function(i) {
            sub <- data
            for (gc in group_cols) {
              sub <- sub[sub[[gc]] == output[[gc]][i], , drop = FALSE]
            }
            format_sample_mode_value(sample.mode(sub[[dep_name]]))
          },
          FUN.VALUE = character(1)
        )
      }
      
      insert_sample_mode_column(output, values)
    }
    
    build_stats_selection <- function(desc_stats) {
      # sample.mode is not a summary.impl flag; computed separately via lolcat::sample.mode()
      desc_stats <- desc_stats[!grepl("^stat\\.sample\\.mode=", desc_stats)]
      # quantiles use selected Hyndman-Fan type via quantile_types.R, not lolcat's default type 7
      desc_stats <- strip_eda_quantile_stats(desc_stats)
      
      # Build statistics selection string (following original app logic)
      stats_sel <- paste(desc_stats, collapse = ",")
      if (grepl("stat.mean=T", stats_sel) != T) {
        stats_sel <- c(stats_sel, ",stat.mean=F")
        stats_sel <- paste(stats_sel, collapse = "")
      }
      if (grepl("stat.var=T", stats_sel) != T) {
        stats_sel <- c(stats_sel, ",stat.var=F")
        stats_sel <- paste(stats_sel, collapse = "")
      }
      if (grepl("stat.miss=T", stats_sel) != T) {
        stats_sel <- c(stats_sel, ",stat.miss=F")
        stats_sel <- paste(stats_sel, collapse = "")
      }
      if (grepl("stat.sd=T", stats_sel) != T) {
        stats_sel <- c(stats_sel, ",stat.sd=F")
        stats_sel <- paste(stats_sel, collapse = "")
      }
      if (grepl("stat.skew.test=T", stats_sel) != T) {
        stats_sel <- c(stats_sel, ",stat.skew.test=0")
        stats_sel <- paste(stats_sel, collapse = "")
      }
      if (grepl("stat.kurt.test=T", stats_sel) != T) {
        stats_sel <- c(stats_sel, ",stat.kurt.test=0")
        stats_sel <- paste(stats_sel, collapse = "")
      }
      
      stats_sel <- c(stats_sel, ",stat.ad.test = 0,stat.sw.test = 0,stat.dago.test = 0")
      stats_sel <- paste(stats_sel, collapse = "")
      
      return(stats_sel)
    }
    
    # =========================================================================
    # BUSINESS LOGIC REACTIVE
    # =========================================================================
    descriptives_data <- reactive({
      data <- processed_data()  # Use processed data from coordinator
      data_type <- data_type_reactive()
      selections <- input_values()
      R <- decimals()
      desc_stats <- desc_stats()
      q_type <- quantile_type()
      
      if (is.null(data) || nrow(data) == 0) {
        return(data.frame())
      }
      
      # If no statistics selected, return empty table
      if (is.null(desc_stats) || length(desc_stats) == 0) {
        return(data.frame(Message = "Please select statistics to display"))
      }
      
      # Names already processed by coordinator
      
      tryCatch({
        if (data_type == 1) {
          # Column analysis - column-isolated evaluation for damage tolerance
          # Get original data and column indices (following original app pattern)
          original_data <- data_source()
          names(original_data) <- make.names(names(original_data))
          
          # Convert UI1 to column indices (like original app)
          selected_cols <- as.numeric(selections$eda_UI1)
          desc_dat <- original_data[, selected_cols, drop = FALSE]
          
          # Build statistics selection string using helper function
          stats_sel <- build_stats_selection(desc_stats)
          
          # Execute statistics per column so empty/non-numeric columns don't fail the entire analysis
          results_list <- list()
          for (j in seq_along(selected_cols)) {
            col_name <- names(desc_dat)[j]
            x_raw <- desc_dat[[j]]
            x_num <- eda_safe_numeric(x_raw)
            n_valid <- sum(!is.na(x_num))
            
            if (n_valid > 0) {
              col_res <- tryCatch({
                col_df <- data.frame(setNames(list(x_num), col_name), stringsAsFactors = FALSE)
                res <- eval(parse(text = paste("summary.all.variables(data = col_df,", stats_sel, ")")))
                res <- append_sample_mode(res, col_df, desc_stats)
                res <- apply_eda_quantiles(res, col_df, desc_stats, type = q_type)
                res
              }, error = function(e) NULL)
              
              if (!is.null(col_res) && nrow(col_res) > 0) {
                results_list[[j]] <- col_res
              } else {
                results_list[[j]] <- data.frame(
                  dv.name = col_name,
                  n = 0L,
                  n.missing = length(x_raw),
                  stringsAsFactors = FALSE
                )
              }
            } else {
              results_list[[j]] <- data.frame(
                dv.name = col_name,
                n = 0L,
                n.missing = length(x_raw),
                stringsAsFactors = FALSE
              )
            }
          }
          
          output <- dplyr::bind_rows(results_list)
          
          if (needs_pooled_all_row(ncol(desc_dat))) {
            pooled_dat <- pool_data_frame_columns(desc_dat)
            if (nrow(pooled_dat) > 0) {
              pooled_out <- tryCatch({
                res <- eval(parse(text = paste("summary.all.variables(data = pooled_dat,", stats_sel, ")")))
                if ("dv.name" %in% names(res)) {
                  res$dv.name <- POOLED_ALL_LABEL
                }
                res <- append_sample_mode(res, pooled_dat, desc_stats)
                res <- apply_eda_quantiles(res, pooled_dat, desc_stats, type = q_type)
                res
              }, error = function(e) NULL)
              
              if (!is.null(pooled_out) && nrow(pooled_out) > 0) {
                output <- prepend_rows_top(pooled_out, output)
              }
            }
          }
        } else if (data_type == 2) {
          # Factor analysis - use summary.continuous with formula (following original app logic)
          data_col <- data_list_for_desc()
          if (is.null(data_col)) {
            return(data.frame(Message = "Please select a data column"))
          }
          
          names(data) <- make.names(names(data))
          
          # Get column names for formula
          # data_col is the index within the selected data columns (UI2), need to map to original data
          selected_data_cols <- as.numeric(selections$eda_UI2)
          dep_name <- colnames(data)[selected_data_cols[as.numeric(data_col)]]
          
          dep_name_m <- make.names(dep_name)
          dep_x <- eda_safe_numeric(data[[dep_name_m]])
          if (sum(!is.na(dep_x)) == 0) {
            return(data.frame(Message = paste0("Selected dependent column '", dep_name, "' contains no valid numeric observations.")))
          }
          
          indep <- colnames(data)[as.numeric(selections$eda_UI1)]
          indep_names <- paste(indep, collapse = "+")
          model_text <- formula(paste(dep_name, " ~ ", indep_names))
          
          # Build statistics selection string using helper function
          stats_sel <- build_stats_selection(desc_stats)
          
          # Execute statistics using the selection string (respecting user choices)
          output <- eval(parse(text = paste("summary.continuous(fx=model_text, data = data,", stats_sel, ")")))
          group_cols <- make.names(indep)
          output <- append_sample_mode(
            output,
            data,
            desc_stats,
            dep_name = make.names(dep_name),
            group_cols = group_cols
          )
          output <- apply_eda_quantiles(
            output,
            data,
            desc_stats,
            dep_name = make.names(dep_name),
            group_cols = group_cols,
            type = q_type
          )
          
          if (needs_pooled_all_row(nrow(output))) {
            pooled_x <- eda_safe_numeric(data[[dep_name_m]])
            pooled_x <- pooled_x[!is.na(pooled_x)]
            if (length(pooled_x) > 0) {
              pooled_dat <- data.frame(All = pooled_x, stringsAsFactors = FALSE)
              pooled_out <- tryCatch({
                eval(parse(text = paste("summary.all.variables(data = pooled_dat,", stats_sel, ")")))
              }, error = function(e) NULL)
              
              if (!is.null(pooled_out) && nrow(pooled_out) > 0) {
                pooled_row <- pooled_out[1, , drop = FALSE]
                if ("dv.name" %in% names(pooled_row)) {
                  pooled_row$dv.name <- NULL
                }
                if (wants_sample_mode(desc_stats)) {
                  pooled_row <- insert_sample_mode_column(
                    pooled_row,
                    format_sample_mode_value(sample.mode(pooled_x))
                  )
                }
                if (has_eda_quantile_stats(desc_stats)) {
                  requests <- parse_eda_quantile_requests(desc_stats)
                  vals <- compute_eda_quantile_values(pooled_x, requests, type = q_type)
                  for (col in names(vals)) {
                    pooled_row[[col]] <- vals[[col]]
                  }
                }
                pooled_row <- pooled_row[, intersect(names(output), names(pooled_row)), drop = FALSE]
                missing_cols <- setdiff(names(output), names(pooled_row))
                for (col in missing_cols) {
                  if (col %in% group_cols) {
                    pooled_row[[col]] <- POOLED_ALL_LABEL
                  } else {
                    pooled_row[[col]] <- NA
                  }
                }
                pooled_row <- pooled_row[, names(output), drop = FALSE]
                pooled_row <- label_factor_group_row(pooled_row, group_cols)
                output <- prepend_rows_top(pooled_row, output)
              }
            }
          }
        } else {
          output <- data.frame()
        }
      }, error = function(e) {
        # Handle known issues with summary.all.variables()
        error_msg <- e$message
        if (grepl("names do not match previous names", error_msg)) {
          error_msg <- "This error can occur with certain data combinations (e.g., columns with different data types or missing values). This is a known limitation of the statistical function."
        }
        output <<- data.frame(Error = paste("Calculation error:", error_msg))
      })
      
      # Apply rounding using global function
      output <- ro(output, R)
      output
    })
    
    # =========================================================================
    # RETURN REACTIVE FUNCTIONS (coordinator will render these)
    # =========================================================================
    list(
      # Data reactives
      descriptives_data = descriptives_data,
      processed_data = processed_data,
      
      # Input reactives
      decimals = decimals,
      desc_stats = desc_stats
    )
  })
}

# =============================================================================
# ARCHITECTURAL PATTERNS USED
# =============================================================================
# 1. Worker Pattern: Contains only business logic, no UI rendering
# 2. Reactive Functions: Returns reactive functions for coordinator to render
# 3. Input Validation: Validates inputs with sensible defaults
# 4. Data Processing: Processes data based on coordinator inputs
# 5. Error Handling: Graceful handling of missing/invalid data
# 6. Template Compliance: Follows established patterns from working modules