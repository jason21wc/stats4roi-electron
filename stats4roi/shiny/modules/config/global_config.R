# Global Configuration Module for stats4ROI
# This module contains all global functions, variables, and settings
# that need to be available throughout the modular application

# =============================================================================
# GLOBAL SETTINGS
# =============================================================================

# R Options (from app.R lines 34-40)
options(dplyr.summarise.inform = FALSE)
options(sci.pen = 99, digits = 10)    # Increase digits for precision
options(tibble.print_max = Inf)      # Display more of a data frame
options(show.signif.stars = T)       # Flag for * to indicate significance

# Shiny Options
options(shiny.sanitize.errors = FALSE)
options(shiny.reactlog = FALSE)

# =============================================================================
# ROUNDING SYSTEM
# =============================================================================

# Global rounding function and digits
ro <- round.object  # Round object function
R <- 4             # Default rounding digits

# =============================================================================
# COLOR SYSTEM
# =============================================================================

# Default color palette
pal_col <- "R4"  # Default to a colorblind safe, equal perceptual weight, qualitative palette

# Get color palette function
get_color_palette <- function(palette_name = pal_col) {
  palette.colors(n = 8, palette = palette_name)
}

# Standardized color system for all modules
get_distribution_colors <- function(color_palette) {
  list(
    col_plot_line = color_palette[1],
    col_fill_highlight = color_palette[2], 
    col_point_of_interest_line = color_palette[3],
    col_mean_line = color_palette[4],
    col_fill = color_palette[5],
    col_line_control_chart = color_palette[6]
  )
}

# Legacy color function for backward compatibility
colors <- function() {
  color_palette <- get_color_palette()
  get_distribution_colors(color_palette)
}

# Global Rounding Function (for modules to access ro function)
ro <- function(x, digits = R) {
  round.object(x, digits = digits)
}

# Global Rounding Digits (for modules to access R)
get_rounding_digits <- function() {
  R
}

# Global Choice Vectors (from app.R lines 55-100)
# These are used throughout the app for dropdown choices

# Graph Options
curve_fit_choice <- c(
  "None"=0,
  "Linear: y = A + Bx"=1,
  "Exponential: y = Ae(Bx)"=2,
  "Logarithmic: A + B ln x"=3,
  "Proportional: y = Ax"=4,
  "Power: y = Ax^B"=5,
  "Inverse: y = A + B/x"=6,
  "S: y=e(A + B/x)"=14,
  "Compound: y = AB^x"=7,
  "Growth: y = e(A + Bx)"=8,
  "Loess: Locally Weighted Regression"=9,
  "Quadratic: y = A + Bx + Cx²"=10,
  "Cubic: y = A + Bx + Cx² + Dx³"=11,
  "4th Order: y = A + Bx + Cx² + Dx³ + Ex^4"=12,
  "5th Order: y = A + Bx + Cx² + Dx³ + Ex^4 + Fx^5"=13
)

# Statistical Test Choices
choice_sd_alt_1 <- c("two.sided","less","greater")
names(choice_sd_alt_1) <- c("σ of sample is not equal to σ₀","σ of sample is less than σ₀","σ of sample is greater than σ₀")

choice_mean_alt_1 <- c("two.sided","less","greater")
names(choice_mean_alt_1) <- c("μ of sample is not equal to μ₀","μ of sample is less than μ₀","μ of sample is greater than μ₀")

choice_mean_alt_2 <- c("two.sided","less","greater")
names(choice_mean_alt_2) <- c("μ₁ is not equal to μ₂","μ₁ is less than μ₂","μ₁ is greater than μ₂")

choice_mean_alt_3 <- c("two.sided","less","greater")
names(choice_mean_alt_3) <- c("Δ of sample is not equal to Δ","Δ of sample is less than Δ","Δ of sample is greater than Δ")

choice_prop_alt_1 <- c("two.sided","less","greater")
names(choice_prop_alt_1) <- c("π of sample is not equal to π₀","π of sample is less than π₀","π of sample is greater than π₀")

choice_prop_alt_2 <- c("two.sided","less","greater")
names(choice_prop_alt_2) <- c("π₁ is not equal to π₂","π₁ is less than π₂","π₁ is greater than π₂")

choice_poi_alt_1 <- c("two.sided","less","greater")
names(choice_poi_alt_1) <- c("λ of sample is not equal to λ₀","λ of sample is less than λ₀","λ of sample is greater than λ₀")

choice_poi_alt_2 <- c("two.sided","less","greater")
names(choice_poi_alt_2) <- c("λ₁ is not equal to λ₂","λ₁ is less than λ₂","λ₁ is greater than λ₂")

# Import Choices
choice_import <- c("file", "copypaste", "googlesheets", "url", "dropbox", "onedrive")

# =============================================================================
# ERROR HANDLING SYSTEM
# =============================================================================

# Standard error table creator
create_global_error_table <- function(message) {
  DT::datatable(
    data.frame(Message = message), 
    options = list(paging = FALSE, searching = FALSE, info = FALSE)
  )
}

# Input validation helpers
validate_numeric_input <- function(input_value, default_value, min_val = NULL, max_val = NULL) {
  if (is.null(input_value) || is.na(input_value)) {
    return(default_value)
  }
  
  if (!is.null(min_val) && input_value < min_val) {
    return(min_val)
  }
  
  if (!is.null(max_val) && input_value > max_val) {
    return(max_val)
  }
  
  input_value
}

validate_logical_input <- function(input_value, default_value = FALSE) {
  if (is.null(input_value)) {
    return(default_value)
  }
  input_value
}

validate_character_input <- function(input_value, default_value = "") {
  if (is.null(input_value) || input_value == "") {
    return(default_value)
  }
  input_value
}

# =============================================================================
# DOWNLOAD FUNCTIONALITY
# =============================================================================

# Standard download UI components
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

# Standard download server
downloadServer <- function(id, plot, height = NULL, width = NULL) {
  moduleServer(id, function(input, output, session) {
    output$dl_plot <- downloadHandler(
      filename = function() {
        file_format <- tolower(input$format)
        paste0(id, ".", file_format)
      },
      content = function(file) {
        tryCatch({
          ggsave(file, plot = plot(), height = height(), width = width(), 
                 units = "px", device = tolower(input$format))
        }, error = function(e) {
          cat("Download error:", e$message, "\n")
        })
      }
    )
  })
}

# =============================================================================
# MODULE REGISTRATION SYSTEM
# =============================================================================

# Module registry for data invalidation
module_registry <- list()

# Register module for global systems
register_module <- function(module_name, ui_reset_function, validation_function) {
  module_registry[[module_name]] <<- list(
    ui_reset = ui_reset_function,
    validation = validation_function
  )
}

# Get registered modules
get_registered_modules <- function() {
  names(module_registry)
}

# =============================================================================
# CONFIGURATION EXPORT
# =============================================================================

# Export function for modules to access all global variables
get_global_config <- function() {
  list(
    # Core functions
    ro = ro,
    R = R,
    colors = colors(),
    
    # Color system
    get_color_palette = get_color_palette,
    get_distribution_colors = get_distribution_colors,
    
    # Error handling
    create_global_error_table = create_global_error_table,
    validate_numeric_input = validate_numeric_input,
    validate_logical_input = validate_logical_input,
    validate_character_input = validate_character_input,
    
    # Download functionality
    downloadButtonUI = downloadButtonUI,
    downloadSelectUI = downloadSelectUI,
    downloadServer = downloadServer,
    
    # Module registry
    register_module = register_module,
    get_registered_modules = get_registered_modules,
    
    # Choice vectors
    curve_fit_choice = curve_fit_choice,
    choice_sd_alt_1 = choice_sd_alt_1,
    choice_mean_alt_1 = choice_mean_alt_1,
    choice_mean_alt_2 = choice_mean_alt_2,
    choice_mean_alt_3 = choice_mean_alt_3,
    choice_prop_alt_1 = choice_prop_alt_1,
    choice_prop_alt_2 = choice_prop_alt_2,
    choice_poi_alt_1 = choice_poi_alt_1,
    choice_poi_alt_2 = choice_poi_alt_2,
    choice_import = choice_import
  )
}











