# SPC limit-calculation helpers ported from app_monolithic.R

# Lookup table function for c6 used for median standard deviations
c6 <- function(sample.size = 5) {
  c6_tab <- c(
    .674489524, .832554611, .888064165, .9160641325, .932894441, .944115161,
    .952126282, .958131091, .962798738, .966530795, .969582762, .972124923,
    .974275110, .976117460, .977713643, .979109892, .980341548, .981436090,
    .982415200, .983296223, .984093190, .984817583, .985478882, .986084984,
    .988487644, .990182489, .991442675, .992416146, .993190756, .993821792,
    .994345788, .994787849, .995165799, .995492642, .995778088, .996029532,
    .996252710, .996631404
  )
  c6_tab <- cbind(value = c6_tab, n = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 100))
  idx <- which(abs(c6_tab[, 2] - sample.size) == min(abs(c6_tab[, 2] - sample.size)))
  c6_tab[idx]
}

# Calculate the moving range across a designated span
#' Evaluate dispersion-chart OOC rules on the sorted plot_data_disp rows.
#' Return a dispersion-chart LCL, or NA when no lower limit exists (<= 0 or missing).
spc_dispersion_lcl_value <- function(lcl) {
  lcl <- suppressWarnings(as.numeric(lcl))
  if (length(lcl) != 1L || is.na(lcl) || lcl <= 0) {
    return(NA_real_)
  }
  lcl
}

spc_mr_lcl_value <- function(centerline_disp, disp_low) {
  if (is.na(disp_low)) {
    return(NA_real_)
  }
  spc_dispersion_lcl_value(centerline_disp - disp_low)
}

#' LCL passed to OOC rule evaluation when the plotted LCL is omitted (NA).
spc_dispersion_lcl_for_rules <- function(lcl, disp_type = NULL) {
  lcl <- as.numeric(lcl)
  if (length(lcl) == 0L) {
    return(lcl)
  }
  if (!is.null(disp_type) && as.numeric(disp_type) %in% c(1L, 4L)) {
    missing_lcl <- is.na(lcl) | !is.finite(lcl) | lcl <= 0
    lcl[missing_lcl] <- 0
  }
  lcl
}

#' Mean or median centerline for location limits; NA observations are omitted.
spc_centerline_value <- function(values, center_type = 1) {
  values <- as.numeric(values)
  center_type <- as.numeric(center_type)
  if (length(values) == 0L || all(is.na(values))) {
    return(NA_real_)
  }
  if (identical(center_type, 2L)) {
    return(stats::median(values, na.rm = TRUE))
  }
  mean(values, na.rm = TRUE)
}

spc_evaluate_dispersion_violations <- function(
    plot_data_disp,
    ooc_rules,
    disp_type = NULL,
    disp_lower_custom = NA) {
  rules <- ooc_rules
  if (!is.null(disp_type) && identical(as.numeric(disp_type), 4)) {
    rules <- spc.rulesets.outside.limits()
  }
  cl_lcl_plot <- plot_data_disp$LCL2
  cl_lcl <- spc_dispersion_lcl_for_rules(cl_lcl_plot, disp_type)
  spc.controlviolation.evaluate.rules(
    control.rules = rules,
    chart.series = plot_data_disp$points_2,
    center.line = plot_data_disp$centerline_2,
    control.limits.ucl = plot_data_disp$UCL2,
    zone.a.upper = plot_data_disp$zone_a_up_2,
    zone.ab.upper = plot_data_disp$zone_ab_up_2,
    zone.bc.upper = plot_data_disp$zone_bc_up_2,
    control.limits.lcl = cl_lcl,
    zone.a.lower = plot_data_disp$zone_a_low_2,
    zone.ab.lower = plot_data_disp$zone_ab_low_2,
    zone.bc.lower = plot_data_disp$zone_bc_low_2
  )
}

MR_span <- function(data = NULL, span = 2) {
  n <- length(data)
  mr <- NULL
  loops <- seq(n - span + 1)
  for (i in loops) {
    low <- i
    high <- span + i - 1
    mr[i] <- max(data[low:high]) - min(data[low:high])
  }
  return(c(rep(NA, span - 1), mr))
}

