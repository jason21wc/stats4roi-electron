# Two-sided power helpers for app-owned tests (lolcat lacks some power.*).
# beta = P(critical_low <= Z <= critical_high | ncp)
# power = 1 - beta

power_z_twosided <- function(ncp, alpha = 0.05) {
  zl <- stats::qnorm(alpha / 2)
  zu <- stats::qnorm(1 - alpha / 2)
  beta <- stats::pnorm(zu, mean = ncp, sd = 1, lower.tail = TRUE) -
    stats::pnorm(zl, mean = ncp, sd = 1, lower.tail = TRUE)
  1 - beta
}

power_t_twosided <- function(ncp, df, alpha = 0.05) {
  tl <- stats::qt(alpha / 2, df)
  tu <- stats::qt(1 - alpha / 2, df)
  beta <- stats::pt(tu, df = df, ncp = ncp, lower.tail = TRUE) -
    stats::pt(tl, df = df, ncp = ncp, lower.tail = TRUE)
  1 - beta
}

poisson_z_ncp_twosample <- function(lambda_1, lambda_2, n1, n2) {
  (sqrt(lambda_2) - sqrt(lambda_1)) / (0.5 * sqrt(1 / n1 + 1 / n2))
}
