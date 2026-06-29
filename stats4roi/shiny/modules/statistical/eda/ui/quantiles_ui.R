# =========================================================================
# QUANTILES UI MODULE
# =========================================================================
# UI for quantiles analysis tab
# Part of EDA module - handles quantile calculations and display

create_quantiles_ui <- function(ns) {
  
  tagList(
    h4("Quantiles"),
    sidebarLayout(
      sidebarPanel(
        numericInput(
          inputId = ns("decimals_quant"),
          label = "Decimals",
          value = 5,
          min = 0,
          step = 1,
          width = "75px"
        ),
        quantile_type_picker_input(ns, "quantile_type"),
        uiOutput(ns("quant_data_list"))
      ), # end sidebarpanel
      mainPanel(
        radioGroupButtons(
          inputId = ns("quant_sel"),
          label = "Select quantile",
          choices = c(
            "4 - Quartiles" = 4,
            "10 - Deciles" = 10,
            "20 - Ventiles" = 20,
            "100 - Percentiles" = 100,
            "Custom" = 1
          )
        ),
        conditionalPanel(
          condition = "input.quant_sel==1",
          numericInput(
            inputId = ns("quant_cust"),
            label = "Enter the number of divisions",
            value = 4,
            min = 0,
            max = 1,
            step = 1
          )
        ),
        DTOutput(ns("quant_out")),
        quantile_type_help_output(ns, "quantile_type_help")
      ) # end main panel
    ) # end sidebarlayout
  )
}