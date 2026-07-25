# SPC constants and choice vectors
# Ported from app_monolithic.R (around lines 200–316).

# Chart type choices
choice_x_spc <- c("X-bar" = 1, "X" = 2)
choice_disp_spc <- c(
  "Range" = 1,
  "Standard Deviation" = 2,
  "Variance" = 3,
  "Moving Range" = 4
)

# Variables: X-bar limit calculation choices (for limit calculations)
choice_x_bar_limits <- (1:9)
names(choice_x_bar_limits) <- c(
  "Average Range",
  "Median Range",
  "Average Standard Deviation",
  "Median Standard Deviation",
  "Average Variance",
  "Average Moving Range of X-bars",
  "Median Moving Range of X-Bars",
  "Standard Deviation of X-bars",
  "Known \u03c3"
)

# Variables: X-bar limit choices (for generating charts)
choice_x_bar_limits2 <- c(seq(1, 9), 12)
names(choice_x_bar_limits2) <- c(
  "Average Range",
  "Median Range",
  "Average Standard Deviation",
  "Median Standard Deviation",
  "Average Variance",
  "Average Moving Range of X-bars",
  "Median Moving Range of X-Bars",
  "Standard Deviation of X-bars",
  "Known \u03c3",
  "Custom"
)

# Variables: X-tilde limit choices (included for parity; used in monolithic limit calcs)
choice_x_tilde_limits <- (1:9)
names(choice_x_tilde_limits) <- c(
  "Average Range",
  "Median Range",
  "Average Standard Deviation",
  "Median Standard Deviation",
  "Average Variance",
  "Average Moving Range of Medians",
  "Median Moving Range of Medians",
  "Standard Deviation of Medians",
  "Known \u03c3"
)

# Variables: Individuals X limit choices
choice_x_limits <- (1:4)
names(choice_x_limits) <- c(
  "Average Moving Range",
  "Median Moving Range",
  "Standard Deviation from k samples",
  "Known \u03c3"
)

# Variables: Dispersion limit choices (for limit calculations)
choice_r_limits <- (1:6)
names(choice_r_limits) <- c(
  "Average Range",
  "Median Range",
  "Average Standard Deviation",
  "Median Standard Deviation",
  "Average Variance",
  "Known \u03c3"
)

# Variables: Dispersion limit choices (for generating charts)
choice_r_limits2 <- c(seq(1, 9), 12)
names(choice_r_limits2) <- c(
  "Average Range",
  "Median Range",
  "Average Standard Deviation",
  "Median Standard Deviation",
  "Average Variance",
  "Average MR of Range",
  "Median MR of Range",
  "Standard Deviation of Range",
  "Known \u03c3",
  "Custom"
)

# PPA Individuals X location limit choices (no Custom — process σ must be estimable)
ppa_individuals_loc_lim_choices <- 6:9
names(ppa_individuals_loc_lim_choices) <- c(
  "Average Moving Range of X",
  "Median Moving Range of X",
  "Standard Deviation of X",
  "Known \u03c3"
)

ppa_mr_disp_lim_choices <- c(6:9, 12)
names(ppa_mr_disp_lim_choices) <- c(
  "Average MR",
  "Median MR",
  "Standard Deviation of X",
  "Known \u03c3",
  "Custom"
)

# Centerline options
choice_x_centerline <- (1:2)
names(choice_x_centerline) <- c(
  "Mean",
  "Median"
)

# Default location and dispersion limit calculations for each dispersion chart type.
spc_default_limits_for_disp_type <- function(disp_type) {
  disp_type <- as.numeric(disp_type)
  switch(
    as.character(disp_type),
    "1" = list(loc_lim = 1L, disp_lim = 1L), # Range: Average Range
    "2" = list(loc_lim = 3L, disp_lim = 3L), # Standard Deviation: Average Standard Deviation
    "3" = list(loc_lim = 5L, disp_lim = 5L), # Variance: Average Variance
    "4" = list(loc_lim = 6L, disp_lim = 6L), # Moving Range: Average MR
    list(loc_lim = 1L, disp_lim = 1L)
  )
}

#' Coerce shiny materialSwitch values (0/1 or logical) to a scalar TRUE/FALSE.
spc_is_switch_on <- function(x) {
  if (length(x) == 0L || is.null(x)) {
    return(FALSE)
  }
  val <- x[[1L]]
  isTRUE(val) || identical(val, 1L) || identical(val, 1)
}

#' TRUE when a chart-options checkbox value is selected (numeric or character).
spc_chart_option_on <- function(options, id) {
  if (is.null(options) || length(options) == 0L) {
    return(FALSE)
  }
  id <- as.character(id)
  any(as.character(options) == id)
}

#' MR span must be >= 2; d2(1) is 0 and breaks limit calculations.
spc_normalize_mr_span <- function(span) {
  span <- suppressWarnings(as.integer(unlist(span, use.names = FALSE)))
  if (length(span) == 0L) {
    return(2L)
  }
  span <- span[[1L]]
  if (is.na(span) || span < 2L) {
    return(2L)
  }
  span
}

spc_resolve_limit_selection <- function(current, default, available) {
  available <- as.numeric(unlist(available, use.names = FALSE))
  available <- available[!is.na(available)]
  if (length(available) == 0) {
    return(NA_real_)
  }

  scalar_num <- function(x) {
    x <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
    if (length(x) == 0L) {
      return(NA_real_)
    }
    x <- x[[1]]
    if (length(x) != 1L || is.na(x)) {
      return(NA_real_)
    }
    x
  }

  current <- scalar_num(current)
  default <- scalar_num(default)
  if (!is.na(current) && isTRUE(current %in% available)) {
    return(current)
  }
  if (!is.na(default) && isTRUE(default %in% available)) {
    return(default)
  }
  available[[1]]
}

# Attributes: chart type choices
choice_att_charts <- (1:4)
names(choice_att_charts) <- c(
  "p-chart",
  "np-chart",
  "c-chart",
  "u-chart"
)

# Attributes: p/u limit calculation choices
choice_att_p_limits <- c(seq(1, 5), 8)
names(choice_att_p_limits) <- c(
  "Exact Poisson",
  "Normal Approximation",
  "Average MR",
  "Median MR",
  "Standard Deviation",
  "Custom"
)

# Attributes: np/c limit calculation choices
choice_att_b_limits <- c(seq(1, 5), 8)
names(choice_att_b_limits) <- c(
  "Exact Binomial",
  "Normal Approximation",
  "Average MR",
  "Median MR",
  "Standard Deviation",
  "Custom"
)

# Capability Calculations: distribution and study choices
choice_cap_distribution <- c("Normal" = 1, "Nonnormal" = 2)
choice_cap_study <- c("Short-term" = 1, "Long-term" = 2)

