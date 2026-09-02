# =========================================================================
# NATURAL TOLERANCE SERVER MODULE
# =========================================================================
# Server logic for natural tolerance analysis tab
# Part of EDA module - handles natural tolerance calculations

# Import required functions from lolcat package
ro <- round.object

create_natural_tolerance_server <- function(id, data_source, data_type_reactive, input_values) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================================
    # INPUT VALIDATION AND EXTRACTION
    # =========================================================================
    # Extract inputs with validation (following worker template pattern)
    decimals_nt_data <- reactive({
      input_vals <- input_values()
      dec <- input_vals$decimals_nt_data
      if (is.null(dec) || is.na(dec)) return(4)
      dec
    })
    
    
    dist_nt_data <- reactive({
      input_vals <- input_values()
      dist <- input_vals$dist_nt_data
      if (is.null(dist) || is.na(dist)) return(1)
      as.numeric(dist)
    })
    
    
    data_list_for_nt <- reactive({
      input_vals <- input_values()
      data_col <- input_vals$data_list_for_nt
      if (!isTruthy(data_col)) return(NULL)
      data_col
    })
    
    
    # =========================================================================
    # NATURAL TOLERANCE CALCULATIONS
    # =========================================================================
    
    natural_tolerance_data <- reactive({
      data <- data_source()
      req(data)
      
      # Get data type and selections from coordinator
      data_type <- data_type_reactive()
      selections <- input_values()
      
      # Validate data
      if (is.null(data) || nrow(data) == 0) {
        return(data.frame())
      }
      
      names(data) <- make.names(names(data))
      
      # Get input values
      R <- decimals_nt_data()
      data_col <- data_list_for_nt()
      distr <- dist_nt_data()
      
      # Extract UI1 and UI2 (matching original app pattern)
      UI1 <- selections$eda_UI1
      UI2 <- selections$eda_UI2
      
      # Validate UI1 exists (matching original app)
      if (is.null(UI1)) {
        return(data.frame())
      }
      
      UI1 <- as.numeric(UI1)
      
      # Process data based on type (matching original app exactly)
      if (data_type == 1) {
        # Column analysis - column-isolated evaluation for damage tolerance
        len <- length(UI1)
        calc_single_nt <- function(x_col) {
          x_num <- suppressWarnings(as.numeric(x_col))
          x_clean <- x_num[!is.na(x_num)]
          if (length(x_clean) < 2) {
            if (distr == 1) {
              return(data.frame(n = length(x_clean), mean = if (length(x_clean) == 1) x_clean[1] else NA_real_, variance = NA_real_, lower = NA_real_, upper = NA_real_))
            } else if (distr == 2) {
              return(data.frame(n = length(x_clean), min = if (length(x_clean) == 1) x_clean[1] else NA_real_, rate = NA_real_, lower = NA_real_, upper = NA_real_))
            } else {
              return(data.frame(n = length(x_clean), rate = NA_real_, lower = NA_real_, upper = NA_real_))
            }
          }
          tryCatch({
            if (distr == 1) {
              natural.tolerance.normal(x = x_clean)
            } else if (distr == 2) {
              natural.tolerance.exp.low(x = x_clean)
            } else if (distr == 3) {
              natural.tolerance.exp(x = x_clean)
            }
          }, error = function(e) {
            data.frame(n = length(x_clean), lower = NA_real_, upper = NA_real_)
          })
        }
        
        rows <- lapply(seq_along(UI1), function(idx) {
          col_idx <- UI1[idx]
          col_name <- names(data)[col_idx]
          res <- calc_single_nt(data[[col_idx]])
          cbind(Data = col_name, res)
        })
        output <- dplyr::bind_rows(rows)
        
        if (needs_pooled_all_row(len)) {
          pooled_x <- pool_numeric_vector(data[UI1])
          if (length(pooled_x) >= 2) {
            all_nt <- tryCatch({
              if (distr == 1) {
                natural.tolerance.normal(x = pooled_x)
              } else if (distr == 2) {
                natural.tolerance.exp.low(x = pooled_x)
              } else {
                natural.tolerance.exp(x = pooled_x)
              }
            }, error = function(e) NULL)
            if (!is.null(all_nt)) {
              all_row <- cbind(Data = POOLED_ALL_LABEL, all_nt)
              output <- prepend_rows_top(all_row, output)
            }
          }
        }
        
      } else if (data_type == 2) {
        # Factor analysis - matching original app lines 18465-18491
        req(data_col)
        UI2 <- as.numeric(UI2)
        dep_info <- resolve_factor_dependent_column(data, data_col, UI2)
        dep_name <- dep_info$dep_name
        if (is.null(dep_name)) {
          return(data.frame())
        }
        dep_name_m <- make.names(dep_name)
        dep_x <- suppressWarnings(as.numeric(data[[dep_name_m]]))
        if (sum(!is.na(dep_x)) < 2) {
          return(data.frame(Message = paste0("Selected dependent column '", dep_name, "' contains insufficient numeric observations.")))
        }
        
        indep <- colnames(data)[as.numeric(unlist(strsplit(x = as.character(UI1), split = "\\s+")))]
        group_cols <- make.names(indep)
        indep_names <- paste(indep, collapse = "+")
        req(dep_name)
        req(indep_names)
        model_text <- formula(paste(dep_name, " ~ ", indep_names))
        
        tryCatch({
          if (distr == 1) {
            sum_out <- summary.continuous(fx = model_text, data = na.omit(data[c(UI1, UI2)]))
            output <- natural.tolerance.normal.simple(mean = sum_out$mean, variance = sum_out$var)
            output <- cbind(sum_out[c(1, 2, 3)], output)
          } else if (distr == 2) {
            sum_out <- summary.continuous(fx = model_text, data = na.omit(data[c(UI1, UI2)]), stat.min = T)
            output <- natural.tolerance.exp.low.simple(rate = 1/(sum_out$mean - sum_out$min), low = sum_out$min)
            output <- cbind(sum_out[c(1, 2, 3)], output)
          } else if (distr == 3) {
            sum_out <- summary.continuous(fx = model_text, data = na.omit(data[c(UI1, UI2)]))
            output <- natural.tolerance.exp.simple(rate = 1/(sum_out$mean), low = 0)
            output <- cbind(sum_out[c(1, 2, 3)], output)
          }
          
          if (needs_pooled_all_row(nrow(output))) {
            pooled_x <- na.omit(suppressWarnings(as.numeric(data[[dep_name]])))
            if (length(pooled_x) >= 2) {
              if (distr == 1) {
                all_nt <- natural.tolerance.normal(x = pooled_x)
              } else if (distr == 2) {
                all_nt <- natural.tolerance.exp.low(x = pooled_x)
              } else {
                all_nt <- natural.tolerance.exp(x = pooled_x)
              }
              all_row <- output[1, , drop = FALSE]
              all_row[1, ] <- NA
              all_row <- label_factor_group_row(all_row, group_cols)
              all_row$n <- length(pooled_x)
              for (col in names(all_nt)) {
                if (col %in% names(all_row)) {
                  all_row[[col]] <- all_nt[[col]]
                }
              }
              output <- prepend_rows_top(all_row, output)
            }
          }
        }, error = function(e) {
          output <<- data.frame(Error = paste("Calculation error:", e$message))
        })
      } else {
        return(data.frame())
      }
      
      # Round output (matching original app line 18493)
      output <- ro(output, R)
      output
    })
    
    # =========================================================================
    # ENTER STATISTICS CALCULATIONS
    # =========================================================================
    
    # Input reactives for Enter Statistics tab
    decimals_nt <- reactive({
      input_vals <- input_values()
      dec <- input_vals$decimals_nt
      if (is.null(dec) || is.na(dec)) return(4)
      dec
    })
    
    dist_nt <- reactive({
      input_vals <- input_values()
      dist <- input_vals$dist_nt
      if (is.null(dist) || is.na(dist)) return(1)
      as.numeric(dist)
    })
    
    UI1_nt <- reactive({
      input_vals <- input_values()
      ui1 <- input_vals$UI1_nt
      if (is.null(ui1) || is.na(ui1)) return(0)
      as.numeric(ui1)
    })
    
    UI2_nt <- reactive({
      input_vals <- input_values()
      ui2 <- input_vals$UI2_nt
      if (is.null(ui2) || is.na(ui2)) return(1)
      as.numeric(ui2)
    })
    
    # Natural tolerance simple calculations (Enter Statistics tab)
    natural_tolerance_simple <- reactive({
      R <- decimals_nt()
      distr <- dist_nt()
      UI1_val <- UI1_nt()
      UI2_val <- UI2_nt()

      req(R)
      validate(need(isTruthy(UI1_val) && isTruthy(distr), "Select a distribution and enter the parameters"))
      
      tryCatch({
        if (distr == 1) {
          validate(need(isTruthy(UI2_val), "Enter a value"))
          output <- natural.tolerance.normal.simple(mean = UI1_val, variance = UI2_val^2)
        } else if (distr == 2) {
          validate(need(isTruthy(UI2_val), "Enter a value"))
          output <- natural.tolerance.exp.low.simple(rate = 1/(UI1_val - UI2_val), low = UI2_val)
        } else if (distr == 3) {
          validate(need(isTruthy(UI2_val), "Enter a value"))
          output <- natural.tolerance.binom.simple(size = UI1_val, prob = UI2_val)
        } else if (distr == 4) {
          output <- natural.tolerance.poisson.simple(lambda = UI1_val)
        } else if (distr == 5) {
          validate(need(isTruthy(UI2_val), "Enter a value"))
          output <- natural.tolerance.chisquare.simple(df = UI1_val, ncp = UI2_val)
        }
        
        # Round output like original app
        output <- ro(output, R)
        return(output)
      }, error = function(e) {
        warning("Error in natural tolerance simple calculation: ", e$message)
        return(data.frame())
      })
    })
    
    # =========================================================================
    # RETURN REACTIVE FUNCTIONS
    # =========================================================================
    return(list(
      natural_tolerance_data = natural_tolerance_data,
      natural_tolerance_simple = natural_tolerance_simple
    ))
  })
}