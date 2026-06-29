# =========================================================================
# QUANTILES SERVER MODULE
# =========================================================================
# Server logic for quantiles analysis tab
# Part of EDA module - handles quantile calculations and data processing

# Import required functions from lolcat package
ro <- round.object

create_quantiles_server <- function(id, data_source, data_type_reactive, input_values) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================================
    # INPUT VALIDATION AND EXTRACTION
    # =========================================================================
    # Extract inputs with validation (following worker template pattern)
    decimals_quant <- reactive({
      input_vals <- input_values()
      dec <- input_vals$decimals_quant
      if (is.null(dec) || is.na(dec)) return(5)
      dec
    })
    
    quant_sel <- reactive({
      input_vals <- input_values()
      sel <- input_vals$quant_sel
      if (is.null(sel) || is.na(sel)) return(4)
      as.numeric(sel)
    })
    
    quant_cust <- reactive({
      input_vals <- input_values()
      cust <- input_vals$quant_cust
      if (is.null(cust) || is.na(cust)) return(4)
      as.numeric(cust)
    })
    
    data_list_for_quant <- reactive({
      input_vals <- input_values()
      data_col <- input_vals$data_list_for_quant
      if (is.null(data_col) || is.na(data_col)) return(NULL)
      data_col
    })

    quantile_type <- reactive({
      normalize_quantile_type(input_values()$quantile_type)
    })
    
    # =========================================================================
    # QUANTILES CALCULATION
    # =========================================================================
    
    quantiles_data <- reactive({
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
      R <- decimals_quant()
      data_col <- data_list_for_quant()
      quant <- quant_sel()
      custom <- quant_cust()
      q_type <- quantile_type()
      
      # Calculate quantile sequence
      if (quant == 1) {
        quant <- custom
      }
      quantiles <- c(seq(from = 0, to = 1, by = (1/quant)))
      
      # Process data based on type
      if (data_type == 1) {
        # Column analysis
        if (is.null(selections$eda_UI1) || length(selections$eda_UI1) == 0) {
          return(data.frame())
        }
        selected_cols <- as.numeric(selections$eda_UI1)
        quant_dat <- data[, selected_cols, drop = FALSE]
        output <- compute_quantiles_column_mode(quant_dat, quantiles, type = q_type)
        if (needs_pooled_all_row(ncol(quant_dat))) {
          pooled_row <- compute_quantiles_pooled_all_column(quant_dat, quantiles, type = q_type)
          output <- prepend_rows_top(pooled_row, output)
        }
      } else if (data_type == 2) {
        # Factor analysis
        if (is.null(selections$eda_UI1) || is.null(selections$eda_UI2) || is.null(data_col)) {
          return(data.frame())
        }
        dep_info <- resolve_factor_dependent_column(data, data_col, selections$eda_UI2)
        dep_name <- dep_info$dep_name
        if (is.null(dep_name)) {
          return(data.frame())
        }
        group_cols <- make.names(colnames(data)[as.numeric(selections$eda_UI1)])
        
        output <- compute_quantiles_factor_mode(
          data = data,
          dep_name = dep_name,
          group_cols = group_cols,
          probs = quantiles,
          type = q_type
        )
        if (needs_pooled_all_row(nrow(output))) {
          pooled_row <- compute_quantiles_pooled_all_factor(
            data = data,
            dep_name = dep_name,
            group_cols = group_cols,
            probs = quantiles,
            type = q_type
          )
          output <- prepend_rows_top(pooled_row, output)
        }
      } else {
        return(data.frame())
      }
      
      # Round output and transpose (preserve variable/group names as column headers)
      output <- ro(output, R)
      id_cols <- if (data_type == 1L) {
        "dv.name"
      } else {
        group_cols
      }
      format_eda_transposed_table(output, id_cols = id_cols)
    })
    
    # =========================================================================
    # RETURN REACTIVE FUNCTIONS
    # =========================================================================
    
    return(list(
      quantiles_data = quantiles_data
    ))
  })
}