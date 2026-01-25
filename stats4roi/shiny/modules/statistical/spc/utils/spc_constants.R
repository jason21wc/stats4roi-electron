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

# Centerline options
choice_x_centerline <- (1:2)
names(choice_x_centerline) <- c(
  "Mean",
  "Median"
)

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

