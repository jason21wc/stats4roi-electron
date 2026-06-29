# Sample size for two independent correlation coefficients

sample.size.cor.pearson.r.twosample <- function(
    r12,
    r34,
    alpha = 0.05,
    beta = 0.1,
    alternative = c("two.sided", "less", "greater"),
    details = TRUE,
    power.from.actual = FALSE) {
  validate.htest.alternative(alternative = alternative)
  z_r12 <- 0.5 * log((1 + r12) / (1 - r12))
  z_r34 <- 0.5 * log((1 + r34) / (1 - r34))

  z_alpha <- qnorm(ifelse(alternative[1] == "two.sided", alpha / 2, alpha), lower.tail = FALSE)
  z_beta <- qnorm(beta, lower.tail = FALSE)

  n <- 2 * ((z_alpha + z_beta) / (z_r34 - z_r12))^2 + 3

  if (alternative[1] == "greater" && z_r34 < z_r12) {
    n <- NA_real_
  }

  if (alternative[1] == "less" && z_r34 > z_r12) {
    n <- NA_real_
  }

  n.rounded <- ceiling(n)

  if (power.from.actual) {
    power <- 1 - beta
  } else {
    power <- power.cor.pearson.r.twosample(
      sample.size_12 = n.rounded,
      sample.size_34 = n.rounded,
      r_12 = r12,
      r_34 = r34,
      alpha = alpha,
      alternative = alternative,
      details = FALSE
    )
    beta <- 1 - power
  }

  if (details) {
    as.data.frame(list(
      test = "z",
      type = "cor.pearson.r.twosample",
      alternative = alternative[1],
      sample.size = n.rounded,
      actual = n,
      effect.size = r34 - r12,
      alpha = alpha,
      conf.level = 1 - alpha,
      beta = beta,
      power = power
    ))
  } else {
    n.rounded
  }
}
