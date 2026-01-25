# MSA helper utilities

# Probability of misclassifying a true in-spec part
misclass_in_spec_prob <- function(x, LSL, USL, sigma_m) {
  p_lower <- pnorm(LSL, mean = x, sd = sigma_m)
  p_upper <- 1 - pnorm(USL, mean = x, sd = sigma_m)
  p_lower + p_upper
}

# Probability of misclassifying a true out-of-spec part as in-spec
misclass_out_of_spec_prob <- function(x, LSL, USL, sigma_m) {
  p_in_spec <- pnorm(USL, mean = x, sd = sigma_m) - pnorm(LSL, mean = x, sd = sigma_m)
  p_in_spec
}

# Integrand for in-spec misclassification
total_misclass_in_spec_prob <- function(x, mu_p, sigma_p, LSL, USL, sigma_m) {
  dnorm(x, mu_p, sigma_p) * misclass_in_spec_prob(x, LSL, USL, sigma_m)
}

# Integrand for out-of-spec misclassification
total_misclass_out_of_spec_prob <- function(x, mu_p, sigma_p, LSL, USL, sigma_m) {
  dnorm(x, mu_p, sigma_p) * misclass_out_of_spec_prob(x, LSL, USL, sigma_m)
}
