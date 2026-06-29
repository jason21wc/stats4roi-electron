power.cor.pearson.r.twosample <- function(
    sample.size_12,
    sample.size_34,
    r_12,
    r_34,
    alpha = 0.05,
    alternative = c("two.sided", "less", "greater"),
    details = TRUE) {
  validate.htest.alternative(alternative = alternative)
  z_r12 <- 0.5 * log((1 + r_12) / (1 - r_12))
  z_r34 <- 0.5 * log((1 + r_34) / (1 - r_34))
  se <- sqrt(1 / (sample.size_12 - 3) + 1 / (sample.size_34 - 3))

  ncp <- (z_r34 - z_r12) / se

  z.upper <- qnorm(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), lower.tail = FALSE)
  z.lower <- qnorm(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), lower.tail = TRUE)

  if (alternative[1] == "two.sided") {
    pow <- power_z_twosided(ncp, alpha)
    beta <- 1 - pow
  } else if (alternative[1] == "greater") {
    beta <- pnorm(z.upper, mean = ncp, sd = 1, lower.tail = TRUE)
    pow <- 1 - beta
  } else {
    beta <- pnorm(z.lower, mean = ncp, sd = 1, lower.tail = FALSE)
    pow <- 1 - beta
  }

  if (details) {
    as.data.frame(list(
      test = "z",
      type = "cor.pearson.r.twosample",
      alternative = alternative[1],
      sample.size_12 = sample.size_12,
      sample.size_34 = sample.size_34,
      effect.size = r_34 - r_12,
      alpha = alpha,
      conf.level = 1 - alpha,
      beta = beta,
      power = pow
    ))
  } else {
    pow
  }
}
