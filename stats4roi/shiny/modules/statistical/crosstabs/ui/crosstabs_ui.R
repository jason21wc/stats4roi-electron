# =========================================================================
# CROSSTABS UI MODULE
# =========================================================================
# UI for Crosstabs tab - handles "Manual" and "Use Data" modes

create_crosstabs_ui_internal <- function(ns) {
  # Create namespace functions for worker modules (nested under coordinator)
  # The coordinator's ns is passed in, so we create nested namespaces
  ns_manual <- function(id) ns(paste0("crosstab_manual-", id))
  ns_data <- function(id) ns(paste0("crosstab_data-", id))
  
  tabsetPanel(
    id = ns("crosstabs_panel"),
    # Manual Tab
    tabPanel(
      title = "Manual",
      value = "crosstabs_manual",
      tags$style(HTML("
    .handsontable .ht_master th.ht_clone_left,
    .ht_master th,
    .ht_master td {
        display: table-cell;
        height: 28px;
        line-height: normal;
        padding: 0px;
        vertical-align: middle;
    }
    th.rowHeader {
        display: table-cell;
        height: 28px;
        visibility: visible;
        border-color: #ddd;
    }
    th {
    padding: 0px !important;
    }
")),
      h3("Manual Crosstabs"),
      
      # Manual crosstab UI controls (in worker namespace)
      fluidRow(
        column(4, numericInput(ns_manual("rows"), "Number of Rows", value = 4, min = 1, width = "150px")),
        column(4, textInput(ns_manual("row_label"), "Row Label", value = "Row"))
      ),
      fluidRow(
        column(4, numericInput(ns_manual("cols"), "Number of Columns", value = 4, min = 1, width = "150px")),
        column(4, textInput(ns_manual("col_label"), "Column Label", value = "Column")),
        column(2, numericInput(ns_manual("decimals"), label = "Decimals", value = 3, min = 0, max = 9, step = 1, width = "75px")),
        column(2, numericInput(ns_manual("conf"), label = "Confidence", value = 0.95, min = 0, max = 1, step = 0.05, width = "75px"))
      ),
      h4("Crosstab Data Entry"),
      rHandsontableOutput(ns_manual("crosstab")),
      hr(),
      fluidRow(
        column(4, actionButton(ns_manual("merge"), "Merge Selected", class = "btn-success", 
                               title = "Select adjacent rows or columns by clicking and dragging across headers, or non-adjacent rows or columns by clicking and holding CTRL")),
        column(4, actionButton(ns_manual("delete_btn"), "Delete Selected", class = "btn-danger")),
        column(4, actionButton(ns_manual("reset"), "Reset Table", class = "btn-warning"))
      ),
      hr(),
      h4("Table and Statistical Output"),
      fluidRow(
        column(4,
               radioButtons(inputId = ns("xtab_man_counts"), label = "Display counts as:", 
                           choices = c("Counts" = 1, "Proportion" = 2, "Percent" = 3)),
               checkboxInput(inputId = ns("xtab_man_ev"), label = "Expected Values?", value = FALSE),
               checkboxGroupInput(inputId = ns("xtab_man_stats"), label = "Statistics",
                                 choices = c("Chi-Squared" = 1,
                                            "Phi/Cramer's V" = 2,
                                            "Fisher's Exact Test (2x2)" = 4,
                                            "McNemar's Test/Symmetry Test" = 5,
                                            "Kappa" = 6)
               )
        ),
        column(8,
               uiOutput(ns("xtab_manual_out"))
        )
      )
    ),
    
    # Use Data Tab
    tabPanel(
      title = "Use Data",
      value = "crosstabs_use_data",
      h3("Crosstabs from Data"),
      
      # Column selectors
      uiOutput(ns("xtab_ui_data")),
      
      # Crosstab table UI (dynamically rendered)
      uiOutput(ns("xtab_ui_data_2")),
      
      hr(),
      h4("Table and Statistical Output"),
      fluidRow(
        column(4,
               radioButtons(inputId = ns("xtab_dat_counts"), label = "Display counts as:", 
                           choices = c("Counts" = 1, "Proportion" = 2, "Percent" = 3)),
               checkboxInput(inputId = ns("xtab_dat_ev"), label = "Expected Values?", value = FALSE),
               checkboxGroupInput(inputId = ns("xtab_dat_stats"), label = "Statistics",
                                 choices = c("Chi-Squared" = 1,
                                            "Phi/Cramer's V" = 2,
                                            "Fisher's Exact Test (2x2)" = 4,
                                            "McNemar's Test/Symmetry Test" = 5,
                                            "Kappa" = 6)
               )
        ),
        column(8,
               uiOutput(ns("xtab_filedat_out"))
        )
      )
    )
  )
}
