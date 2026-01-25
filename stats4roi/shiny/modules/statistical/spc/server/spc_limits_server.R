# SPC Limits Calculations Worker Module
# Calculation functions for control chart limit calculations.
# These functions perform the calculations and return structured data that the coordinator formats into HTML.

# Note: These functions use the ro() rounding function from global_config
# and SPC constants from spc_constants.R

# Calculate X-bar limits
calculate_x_bar_limits <- function(select, x_bar, stat, n, sterr, R) {
  UCL <- NULL
  LCL <- NULL
  sig_est <- NULL
  note <- NULL
  
  if (select == 1) {
    A2 <- spc.constant.calculation.A2(sample.size = n, n.sigma = sterr)
    d2 <- spc.constant.calculation.d2(sample.size = n)
    UCL <- ro(x_bar + A2 * stat, R)
    LCL <- ro(x_bar - A2 * stat, R)
    sig_est <- ro(stat / d2, R)
  }
  if (select == 2) {
    A2 <- spc.constant.calculation.A4(sample.size = n, n.sigma = sterr)
    d2 <- spc.constant.calculation.d4(sample.size = n)
    UCL <- ro(x_bar + A2 * stat, R)
    LCL <- ro(x_bar - A2 * stat, R)
    sig_est <- ro(stat / d2, R)
  }
  if (select == 3) {
    A3 <- spc.constant.calculation.A3(sample.size = n, n.sigma = sterr)
    c4 <- spc.constant.calculation.c4(sample.size = n)
    UCL <- ro(x_bar + A3 * stat, R)
    LCL <- ro(x_bar - A3 * stat, R)
    sig_est <- ro(stat / c4, R)
  }
  if (select == 4) { # median stdev
    A10 <- sterr / (c6(n) * sqrt(n))
    UCL <- ro(x_bar + A10 * stat, R)
    LCL <- ro(x_bar - A10 * stat, R)
    sig_est <- ro(stat / c6(n), R)
  }
  if (select == 5) {
    UCL <- ro(x_bar + 3 * sqrt(stat) / sqrt(n), R)
    LCL <- ro(x_bar - 3 * sqrt(stat) / sqrt(n), R)
    sig_est <- ro(stat^0.5, R)
  }
  if (select == 6) {
    d2 <- spc.constant.calculation.d2(sample.size = 2)
    sig_est_b <- ro(stat / d2, R)
    UCL <- ro(x_bar + sterr * stat / d2, R)
    LCL <- ro(x_bar - sterr * stat / d2, R)
    sig_est <- ro(stat / d2 * sqrt(n), R)
    note <- "Assuming Moving Ranges are calculated from two means"
  }
  if (select == 7) {
    d2 <- spc.constant.calculation.d4(sample.size = 2)
    sig_est_b <- ro(stat / d2, R)
    UCL <- ro(x_bar + sterr * stat / d2, R)
    LCL <- ro(x_bar - sterr * stat / d2, R)
    sig_est <- ro(stat / d2 * sqrt(n), R)
    note <- "Assuming Moving Ranges are calculated from two means"
  }
  if (select == 8) {
    UCL <- ro(x_bar + sterr * stat, R)
    LCL <- ro(x_bar - sterr * stat, R)
    sig_est <- ro(stat * sqrt(n), R)
  }
  if (select == 9) {
    sig_est <- ro(stat, R)
    UCL <- ro(x_bar + sterr * stat / sqrt(n), R)
    LCL <- ro(x_bar - sterr * stat / sqrt(n), R)
  }
  
  list(
    UCL = UCL,
    LCL = LCL,
    sig_est = sig_est,
    centerline = ro(x_bar, R),
    n = n,
    note = note
  )
}

# Calculate X (individuals) limits
calculate_x_limits <- function(select, x_bar, stat, n, sterr, R) {
  UCL <- NULL
  LCL <- NULL
  sig_est <- NULL
  sig_label <- if (select < 4) "$\\hat{\\sigma}=$" else "$\\sigma=$"
  n_label <- if (select == 1 || select == 2) "$n_{MR}=$" else if (select == 3) "$s_{k}=$" else ""
  
  if (select == 1) {
    d2 <- spc.constant.calculation.d2(sample.size = n)
    sig_est_b <- ro(stat / d2, R)
    UCL <- ro(x_bar + sterr * stat / d2, R)
    LCL <- ro(x_bar - sterr * stat / d2, R)
    sig_est <- ro(stat / d2, R)
  }
  if (select == 2) {
    d4 <- spc.constant.calculation.d4(sample.size = n)
    sig_est <- ro(stat / d4, R)
    UCL <- ro(x_bar + sterr * stat / d4, R)
    LCL <- ro(x_bar - sterr * stat / d4, R)
  }
  if (select == 3) {
    c4 <- spc.constant.calculation.c4(sample.size = n)
    sig_est <- ro(stat / c4, R)
    UCL <- ro(x_bar + sterr * stat / c4, R)
    LCL <- ro(x_bar - sterr * stat / c4, R)
  }
  if (select == 4) {
    sig_est <- ro(stat, R)
    UCL <- ro(x_bar + sterr * stat, R)
    LCL <- ro(x_bar - sterr * stat, R)
  }
  
  list(
    UCL = UCL,
    LCL = LCL,
    sig_est = sig_est,
    centerline = ro(x_bar, R),
    n = n,
    sig_label = sig_label,
    n_label = n_label
  )
}

# Calculate R limits
calculate_r_limits <- function(select, stat, n, sterr, R) {
  UCL <- NULL
  LCL <- NULL
  sig_est <- NULL
  r_center <- NULL
  stat_symb <- NULL
  
  if (select == 1) {
    D3 <- spc.constant.calculation.D3(sample.size = n, n.sigma = sterr)
    D4 <- spc.constant.calculation.D4(sample.size = n, n.sigma = sterr)
    d2 <- spc.constant.calculation.d2(sample.size = n)
    UCL <- ro(D4 * stat, R)
    LCL <- ro(D3 * stat, R)
    sig_est <- ro(stat / d2, R)
    r_center <- stat
    stat_symb <- withMathJax("$\\bar{R}=$")
  }
  if (select == 2) {
    D5 <- spc.constant.calculation.D5(sample.size = n, n.sigma = sterr)
    D6 <- spc.constant.calculation.D6(sample.size = n, n.sigma = sterr)
    d4 <- spc.constant.calculation.d4(sample.size = n)
    UCL <- ro(D6 * stat, R)
    LCL <- ro(D5 * stat, R)
    sig_est <- ro(stat / d4, R)
    r_center <- ro(stat * spc.constant.calculation.d2(sample.size = n) / d4, R)
    stat_symb <- withMathJax("$\\bar{R}_{est}=$")
  }
  if (select == 3) {
    c4 <- spc.constant.calculation.c4(sample.size = n)
    d2 <- spc.constant.calculation.d2(sample.size = n)
    D4 <- spc.constant.calculation.D4(sample.size = n, n.sigma = sterr)
    D3 <- spc.constant.calculation.D3(sample.size = n, n.sigma = sterr)
    sig_est <- stat / c4
    r_center <- sig_est * d2
    UCL <- ro(r_center * D4, R)
    LCL <- ro(r_center * D3, R)
    sig_est <- ro(sig_est, R)
    stat_symb <- withMathJax("$\\bar{R}_{est}=$")
  }
  if (select == 4) {
    c6_val <- c6(n)
    D4 <- spc.constant.calculation.D4(sample.size = n, n.sigma = sterr)
    D3 <- spc.constant.calculation.D3(sample.size = n, n.sigma = sterr)
    d2 <- spc.constant.calculation.d2(sample.size = n)
    sig_est <- stat / c6_val
    r_center <- stat * d2 / c6_val
    UCL <- ro(r_center * D4, R)
    LCL <- ro(r_center * D3, R)
    sig_est <- ro(sig_est, R)
    stat_symb <- withMathJax("$\\bar{R}_{est}=$")
  }
  if (select == 5) {
    D4 <- spc.constant.calculation.D4(sample.size = n, n.sigma = sterr)
    D3 <- spc.constant.calculation.D3(sample.size = n, n.sigma = sterr)
    d2 <- spc.constant.calculation.d2(sample.size = n)
    sig_est <- sqrt(stat)
    r_center <- sig_est * d2
    UCL <- ro(r_center * D4, R)
    LCL <- ro(r_center * D3, R)
    sig_est <- ro(sig_est, R)
    stat_symb <- withMathJax("$\\bar{R}_{est}=$")
  }
  if (select == 6) {
    D4 <- spc.constant.calculation.D4(sample.size = n, n.sigma = sterr)
    D3 <- spc.constant.calculation.D3(sample.size = n, n.sigma = sterr)
    d2 <- spc.constant.calculation.d2(sample.size = n)
    sig_est <- stat
    r_center <- sig_est * d2
    UCL <- ro(r_center * D4, R)
    LCL <- ro(r_center * D3, R)
    sig_est <- ro(sig_est, R)
    stat_symb <- withMathJax("$\\bar{R}_{est}=$")
  }
  
  list(
    UCL = UCL,
    LCL = LCL,
    sig_est = sig_est,
    centerline = ro(r_center, R),
    n = n,
    stat_symb = stat_symb
  )
}

# Calculate s limits
calculate_s_limits <- function(select, stat, n, sterr, R) {
  UCL <- NULL
  LCL <- NULL
  sig_est <- NULL
  s_center <- NULL
  stat_symb <- NULL
  
  if (select == 1) {
    B3 <- spc.constant.calculation.B3(sample.size = n, n.sigma = sterr)
    B4 <- spc.constant.calculation.B4(sample.size = n, n.sigma = sterr)
    d2 <- spc.constant.calculation.d2(sample.size = n)
    c4 <- spc.constant.calculation.c4(sample.size = n)
    sig_est <- stat / d2
    s_center <- sig_est * c4
    UCL <- ro(B4 * s_center, R)
    LCL <- ro(B3 * s_center, R)
    s_center <- ro(s_center, R)
    sig_est <- ro(sig_est, R)
    stat_symb <- withMathJax("$\\bar{s}_{est}=$")
  }
  if (select == 2) {
    B3 <- spc.constant.calculation.B3(sample.size = n, n.sigma = sterr)
    B4 <- spc.constant.calculation.B4(sample.size = n, n.sigma = sterr)
    d4 <- spc.constant.calculation.d4(sample.size = n)
    c4 <- spc.constant.calculation.c4(sample.size = n)
    sig_est <- stat / d4
    s_center <- sig_est * c4
    UCL <- ro(B4 * s_center, R)
    LCL <- ro(B3 * s_center, R)
    s_center <- ro(s_center, R)
    sig_est <- ro(sig_est, R)
    stat_symb <- withMathJax("$\\bar{s}_{est}=$")
  }
  if (select == 3) {
    c4 <- spc.constant.calculation.c4(sample.size = n)
    B3 <- spc.constant.calculation.B3(sample.size = n, n.sigma = sterr)
    B4 <- spc.constant.calculation.B4(sample.size = n, n.sigma = sterr)
    sig_est <- stat / c4
    s_center <- stat
    UCL <- ro(B4 * s_center, R)
    LCL <- ro(B3 * s_center, R)
    s_center <- ro(s_center, R)
    sig_est <- ro(sig_est, R)
    stat_symb <- withMathJax("$\\bar{s}=$")
  }
  if (select == 4) {
    c6_val <- c6(n)
    B3 <- spc.constant.calculation.B3(sample.size = n, n.sigma = sterr)
    B4 <- spc.constant.calculation.B4(sample.size = n, n.sigma = sterr)
    sig_est <- stat / c6_val
    s_center <- sig_est * spc.constant.calculation.c4(sample.size = n)
    UCL <- ro(B4 * s_center, R)
    LCL <- ro(B3 * s_center, R)
    s_center <- ro(s_center, R)
    sig_est <- ro(sig_est, R)
    stat_symb <- withMathJax("$\\bar{s}_{est}=$")
  }
  if (select == 5) {
    sig_est <- sqrt(stat)
    s_center <- sig_est * spc.constant.calculation.c4(sample.size = n)
    B3 <- spc.constant.calculation.B3(sample.size = n, n.sigma = sterr)
    B4 <- spc.constant.calculation.B4(sample.size = n, n.sigma = sterr)
    UCL <- ro(B4 * s_center, R)
    LCL <- ro(B3 * s_center, R)
    s_center <- ro(s_center, R)
    sig_est <- ro(sig_est, R)
    stat_symb <- withMathJax("$\\bar{s}_{est}=$")
  }
  if (select == 6) {
    sig_est <- stat
    s_center <- sig_est * spc.constant.calculation.c4(sample.size = n)
    B3 <- spc.constant.calculation.B3(sample.size = n, n.sigma = sterr)
    B4 <- spc.constant.calculation.B4(sample.size = n, n.sigma = sterr)
    UCL <- ro(B4 * s_center, R)
    LCL <- ro(B3 * s_center, R)
    s_center <- ro(s_center, R)
    sig_est <- ro(sig_est, R)
    stat_symb <- withMathJax("$\\bar{s}_{est}=$")
  }
  
  list(
    UCL = UCL,
    LCL = LCL,
    sig_est = sig_est,
    centerline = s_center,
    n = n,
    stat_symb = stat_symb
  )
}

# Calculate s² limits
calculate_s2_limits <- function(select, stat, n, sterr, R) {
  UCL <- NULL
  LCL <- NULL
  sig_est <- NULL
  s_center <- NULL
  stat_symb <- NULL
  
  p_low <- pnorm(-sterr, 0, 1)
  p_high <- pnorm(sterr, 0, 1)
  
  if (select == 1) {
    d2 <- spc.constant.calculation.d2(sample.size = n)
    sig_est <- stat / d2
    s_center <- sig_est^2
    chi_low <- qchisq(p = p_low, df = n - 1)
    chi_high <- qchisq(p = p_high, df = n - 1)
    UCL <- ro(s_center * chi_high / (n - 1), R)
    LCL <- ro(s_center * chi_low / (n - 1), R)
    s_center <- ro((sig_est * spc.constant.calculation.c4(sample.size = n))^2, R)
    sig_est <- ro(sig_est, R)
    stat_symb <- withMathJax("$\\bar{s^{2}}_{est}=$")
  }
  if (select == 2) {
    d4 <- spc.constant.calculation.d4(sample.size = n)
    sig_est <- stat / d4
    s_center <- sig_est^2
    chi_low <- qchisq(p = p_low, df = n - 1)
    chi_high <- qchisq(p = p_high, df = n - 1)
    UCL <- ro(s_center * chi_high / (n - 1), R)
    LCL <- ro(s_center * chi_low / (n - 1), R)
    s_center <- ro((sig_est * spc.constant.calculation.c4(sample.size = n))^2, R)
    sig_est <- ro(sig_est, R)
    stat_symb <- withMathJax("$\\bar{s^{2}}_{est}=$")
  }
  if (select == 3) {
    c4 <- spc.constant.calculation.c4(sample.size = n)
    sig_est <- stat / c4
    s_center <- sig_est^2
    chi_low <- qchisq(p = p_low, df = n - 1)
    chi_high <- qchisq(p = p_high, df = n - 1)
    UCL <- ro(s_center * chi_high / (n - 1), R)
    LCL <- ro(s_center * chi_low / (n - 1), R)
    s_center <- ro(sig_est^2, R)
    sig_est <- ro(sig_est, R)
    stat_symb <- withMathJax("$\\bar{s^{2}}_{est}=$")
  }
  if (select == 4) {
    c6_val <- c6(n)
    sig_est <- stat / c6_val
    s_center <- sig_est^2
    chi_low <- qchisq(p = p_low, df = n - 1)
    chi_high <- qchisq(p = p_high, df = n - 1)
    UCL <- ro(s_center * chi_high / (n - 1), R)
    LCL <- ro(s_center * chi_low / (n - 1), R)
    s_center <- ro((sig_est * spc.constant.calculation.c4(sample.size = n))^2, R)
    sig_est <- ro(sig_est, R)
    stat_symb <- withMathJax("$\\bar{s^{2}}_{est}=$")
  }
  if (select == 5) {
    sig_est <- sqrt(stat)
    s_center <- sig_est^2
    chi_low <- qchisq(p = p_low, df = n - 1)
    chi_high <- qchisq(p = p_high, df = n - 1)
    UCL <- ro(s_center * chi_high / (n - 1), R)
    LCL <- ro(s_center * chi_low / (n - 1), R)
    s_center <- ro((sig_est * spc.constant.calculation.c4(sample.size = n))^2, R)
    sig_est <- ro(sig_est, R)
    stat_symb <- withMathJax("$\\bar{s^{2}}=$")
  }
  if (select == 6) {
    sig_est <- stat
    s_center <- sig_est^2
    chi_low <- qchisq(p = p_low, df = n - 1)
    chi_high <- qchisq(p = p_high, df = n - 1)
    UCL <- ro(s_center * chi_high / (n - 1), R)
    LCL <- ro(s_center * chi_low / (n - 1), R)
    s_center <- ro((sig_est * spc.constant.calculation.c4(sample.size = n))^2, R)
    sig_est <- ro(sig_est, R)
    stat_symb <- withMathJax("$\\bar{s^{2}}_{est}=$")
  }
  
  list(
    UCL = UCL,
    LCL = LCL,
    sig_est = sig_est,
    centerline = s_center,
    n = n,
    stat_symb = stat_symb
  )
}

# Calculate p limits
calculate_p_limits <- function(select, stat, n, sterr, R) {
  UCL <- NULL
  LCL <- NULL
  sig_est <- NULL
  p_center <- stat
  stat_symb <- withMathJax("$\\bar{p}=$")
  
  p_low <- pnorm(-sterr, 0, 1)
  p_high <- pnorm(sterr, 0, 1)
  
  if (select == 1) {
    LCL <- ro((qbinom(p = p_low, size = n, prob = stat, lower.tail = T) - 0.5) / n, R)
    if (LCL < 0) LCL <- NA
    UCL <- ro((qbinom(p = p_high, size = n, prob = stat, lower.tail = T) + 0.5) / n, R)
    sig_est <- ro(sqrt(stat * (1 - stat) / n), R)
  }
  if (select == 2) {
    sig_est <- ro(sqrt(stat * (1 - stat) / n), R)
    LCL <- ro(p_center - 3 * sig_est, R)
    if (LCL < 0) LCL <- NA
    UCL <- ro(p_center + 3 * sig_est, R)
  }
  
  list(
    UCL = UCL,
    LCL = LCL,
    sig_est = sig_est,
    centerline = ro(p_center, R),
    n = n,
    stat_symb = stat_symb,
    method_label = c("Exact Bimonial, ", "Normal Approximate, ")[select]
  )
}

# Calculate np limits
calculate_np_limits <- function(select, stat, n, sterr, R) {
  UCL <- NULL
  LCL <- NULL
  sig_est <- NULL
  p_center <- stat
  stat_symb <- withMathJax("$\\overline{np}=$")
  
  p_low <- pnorm(-sterr, 0, 1)
  p_high <- pnorm(sterr, 0, 1)
  
  if (select == 1) {
    LCL <- ro((qbinom(p = p_low, size = n, prob = stat / n, lower.tail = T) - 0.5), R)
    if (LCL < 0) LCL <- NA
    UCL <- ro((qbinom(p = p_high, size = n, prob = stat / n, lower.tail = T) + 0.5), R)
    sig_est <- ro(sqrt(stat * (1 - (stat / n))), R)
  }
  if (select == 2) {
    sig_est <- ro(sqrt(stat * (1 - (stat / n))), R)
    LCL <- ro(p_center - 3 * sig_est, R)
    if (LCL < 0) LCL <- NA
    UCL <- ro(p_center + 3 * sig_est, R)
  }
  
  list(
    UCL = UCL,
    LCL = LCL,
    sig_est = sig_est,
    centerline = ro(p_center, R),
    n = n,
    stat_symb = stat_symb,
    method_label = c("Exact Binomial, ", "Normal Approximate, ")[select]
  )
}

# Calculate c limits
calculate_c_limits <- function(select, stat, sterr, R) {
  UCL <- NULL
  LCL <- NULL
  sig_est <- NULL
  c_center <- stat
  stat_symb <- withMathJax("$\\bar{c}=$")
  
  p_low <- pnorm(-sterr, 0, 1)
  p_high <- pnorm(sterr, 0, 1)
  
  if (select == 1) {
    LCL <- ro((qpois(p = p_low, lambda = stat, lower.tail = T) - 0.5), R)
    if (LCL < 0) LCL <- NA
    UCL <- ro((qpois(p = p_high, lambda = stat, lower.tail = T) + 0.5), R)
    sig_est <- ro(sqrt(stat), R)
  }
  if (select == 2) {
    sig_est <- sqrt(stat)
    LCL <- ro(c_center - 3 * sig_est, R)
    if (LCL < 0) LCL <- NA
    UCL <- ro(c_center + 3 * sig_est, R)
    sig_est <- ro(sig_est, R)
  }
  
  list(
    UCL = UCL,
    LCL = LCL,
    sig_est = sig_est,
    centerline = ro(c_center, R),
    stat_symb = stat_symb,
    method_label = c("Exact Poisson, ", "Approximate Normal, ")[select]
  )
}

# Calculate u limits
calculate_u_limits <- function(select, stat, n, sterr, R) {
  UCL <- NULL
  LCL <- NULL
  sig_est <- NULL
  u_center <- stat
  stat_symb <- withMathJax("$\\bar{u}=$")
  
  p_low <- pnorm(-sterr, 0, 1)
  p_high <- pnorm(sterr, 0, 1)
  
  if (select == 1) {
    LCL <- ro((qpois(p = p_low, lambda = stat * n, lower.tail = T) - 0.5) / n, R)
    if (LCL < 0) LCL <- NA
    UCL <- ro((qpois(p = p_high, lambda = stat * n, lower.tail = T) + 0.5) / n, R)
    sig_est <- ro(sqrt(stat * n) / n, R)
  }
  if (select == 2) {
    sig_est <- sqrt(stat / n)
    LCL <- ro(u_center - 3 * sig_est, R)
    if (LCL < 0) LCL <- NA
    UCL <- ro(u_center + 3 * sig_est, R)
    sig_est <- ro(sig_est, R)
  }
  
  list(
    UCL = UCL,
    LCL = LCL,
    sig_est = sig_est,
    centerline = ro(u_center, R),
    n = n,
    stat_symb = stat_symb,
    method_label = c("Exact Poisson, ", "Approximate Normal, ")[select]
  )
}

# Calculate kappa limits
calculate_kappa_limits <- function(data, UI1, UI2, std_err, R) {
  if (nrow(data[UI1]) != nrow(data[UI2])) {
    return(list(error = "Length of κ and V must be equal."))
  }
  
  kappa_dat <- data[c(UI1, UI2)]
  kappa_dat$KV <- kappa_dat[, 1] / kappa_dat[, 2]
  kappa_dat$inv_V <- 1 / kappa_dat[, 2]
  centerline <- sum(kappa_dat$KV) / sum(kappa_dat$inv_V)
  sd_kappa <- sqrt(1 / sum(kappa_dat$inv_V))
  UCL <- centerline + std_err * sd_kappa
  LCL <- centerline - std_err * sd_kappa
  
  list(
    UCL = ro(UCL, R),
    LCL = ro(LCL, R),
    centerline = ro(centerline, R),
    sd_kappa = ro(sd_kappa, R),
    k = nrow(kappa_dat)
  )
}

# Calculate kappa critical for capability
calculate_kappa_critical <- function(po, cp, R) {
  if (cp <= 0.5) {
    p_chance <- (0.5 + (1 - po) / 2)^2 + (1 - 0.5 - (1 - po) / 2)^2
  } else {
    p_chance <- (cp + (1 - po) / 2)^2 + (1 - cp - (1 - po) / 2)^2
  }
  kappa_crit <- (po - p_chance) / (1 - p_chance)
  
  list(
    p_chance = p_chance,
    kappa_crit = ro(kappa_crit, R),
    po = po
  )
}
