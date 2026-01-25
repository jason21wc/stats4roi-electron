# Base Distribution Module for stats4ROI
# This module provides common functionality for all distribution plotting modules
# Replicates the unified download system and common utilities from the original app

# Global download functions (replicating app.R lines 323-341)
downloadButtonUI <- function(id) {
  downloadButton(NS(id, "dl_plot"))
}

downloadSelectUI <- function(id) {
  pickerInput(NS(id, "format"), 
              label = "Format: ", 
              choices = c("eps","ps","tex","pdf","jpeg","tiff","png","bmp","svg","wmf"),
              selected = "svg",
              width = "75px")
}

# Combined download UI with button and format selector on same line
downloadUI <- function(id) {
  fluidRow(
    column(3, downloadButton(NS(id, "dl_plot"))),
    column(6, 
           tags$div(style = "display: flex; align-items: center;",
                    tags$span(style = "margin-right: 10px; white-space: nowrap;", "Format:"),
                    pickerInput(NS(id, "format"), 
                               label = NULL, 
                               choices = c("eps","ps","tex","pdf","jpeg","tiff","png","bmp","svg","wmf"),
                               selected = "svg",
                               width = "75px",
                               options = list(style = "btn-sm"))))
  )
}

downloadServer <- function(id, plot, height = NULL, width = NULL) {
  moduleServer(id, function(input, output, session) {
    output$dl_plot <- downloadHandler(
      filename = function() {
        file_format <- tolower(input$format)
        paste0(id, ".", file_format)
      },
      content = function(file) {
        ggsave(file, plot = plot(), height = height(), width = width(), 
               units = "px", device = tolower(input$format))
      }
    )
  })
}

# Note: UI components are now handled directly in each distribution module

# Common distribution server functions
# Note: Individual distribution modules now handle their own renderPlot, 
# renderUI, and renderDataTable outputs directly. This base module 
# provides only utility functions and common UI components.

# Common validation functions
validate_positive_integer <- function(value, name) {
  validate(need(value == round(value), paste("Enter integer for", name)),
           need(value > 0, paste(name, "must be positive")))
}

validate_probability <- function(value, name) {
  validate(need(value > 0 && value < 1, paste(name, "must be between 0 and 1")))
}

validate_positive_value <- function(value, name) {
  validate(need(value > 0, paste(name, "must be positive")))
}

# Common probability calculation functions
calculate_tail_probabilities <- function(distribution, params, x_lower, x_upper, R = 4) {
  # This function will be implemented based on specific distribution needs
  # For now, return a placeholder structure
  list(
    p_at_lower = 0,
    p_at_upper = 0,
    p_between = 0,
    p_tails = 0
  )
}

# Common HTML output formatting
format_probability_results <- function(results, R = 4) {
  # Format probability results as HTML table
  # This will be customized for each distribution
  HTML("Probability results will be formatted here")
}

# Common color management (exact replication from app.R lines 46-53)
get_distribution_colors <- function(color_palette) {
  # color_palette is already the result of palette.colors(n = 8, palette = pal_col)
  # Access colors directly like the original app: color[1], color[5], etc.
  list(
    col_fill = unname(color_palette[5]),           # color[5] - main fill color
    col_fill_highlight = unname(color_palette[2]), # color[2] - highlight color for tails
    col_mean_line = unname(color_palette[3]),      # color[3] - mean line color
    col_point_of_interest_line = unname(color_palette[6]), # color[6] - point of interest line
    col_plot_line = unname(color_palette[1]),      # color[1] - plot line/border color
    col_line_control_chart = unname(color_palette[4]) # color[4] - control chart line color
  )
}
