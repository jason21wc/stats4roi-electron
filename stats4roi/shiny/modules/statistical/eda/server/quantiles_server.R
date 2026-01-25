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
        output <- summary.all.variables(
          data = quant_dat,
          stat.mean = F,
          stat.var = F,
          stat.ad.test = 0,
          stat.sw.test = 0,
          stat.skew.test = 0,
          stat.kurt.test = 0,
          stat.dago.test = 0,
          stat.quantiles = quantiles
        )
      } else if (data_type == 2) {
        # Factor analysis
        if (is.null(selections$eda_UI1) || is.null(selections$eda_UI2) || is.null(data_col)) {
          return(data.frame())
        }
        dep_name <- colnames(data)[as.numeric(data_col)]
        indep <- colnames(data)[as.numeric(selections$eda_UI1)]
        indep_names <- paste(indep, collapse = "+")
        model_text <- formula(paste(dep_name, " ~ ", indep_names))
        
        output <- summary.continuous(
          fx = model_text,
          data = data,
          stat.mean = F,
          stat.var = F,
          stat.ad.test = 0,
          stat.sw.test = 0,
          stat.skew.test = 0,
          stat.kurt.test = 0,
          stat.dago.test = 0,
          stat.quantiles = quantiles
        )
      } else {
        return(data.frame())
      }
      
      # Round output and transpose
      output <- t(ro(output, R))
      output
    })
    
    # =========================================================================
    # RETURN REACTIVE FUNCTIONS
    # =========================================================================
    
    return(list(
      quantiles_data = quantiles_data
    ))
  })
}