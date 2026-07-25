# Distribution Fitting constants and parameter metadata

choice_dfit_distribution <- c(
  "None" = 0L,
  "Normal" = 1L,
  "Exponential (0)" = 2L,
  "Exponential (Low)" = 3L,
  "Weibull (2 Param)" = 4L,
  "Uniform" = 5L,
  "Johnson" = 6L,
  "Gamma (0)" = 7L,
  "Gamma (Low)" = 8L,
  "Beta" = 9L,
  "Rayleigh (0)" = 10L,
  "Rayleigh (Low)" = 11L
)

dfit_distribution_label <- function(id) {
  id <- as.integer(id)
  names(choice_dfit_distribution)[match(id, choice_dfit_distribution)]
}

choice_dfit_johnson_family <- c(
  "Unbounded (Su)" = "Su",
  "Bounded (Sb)" = "Sb",
  "Lognormal (Sl)" = "Sl"
)

# Parameter override field definitions: name -> label
dfit_param_fields <- function(distribution_id) {
  id <- as.integer(distribution_id)
  switch(
    id,
    `1` = list(mean = "Mean", sd = "Std Dev"),
    `2` = list(rate = "Rate", min = "Min"),
    `3` = list(rate = "Rate", min = "Min"),
    `4` = list(shape = "Beta", scale = "Eta"),
    `5` = list(min = "Min", max = "Max"),
    `6` = list(
      family = "Family", gamma = "Gamma", eta = "Nu",
      lambda = "Lambda", epsilon = "Epsilon"
    ),
    `7` = list(shape = "Nu", rate = "Lambda", min = "Min"),
    `8` = list(shape = "Nu", rate = "Lambda", min = "Min"),
    `9` = list(shape1 = "Gamma", shape2 = "Nu", min = "Min", max = "Max"),
    `10` = list(scale = "Beta", min = "Min"),
    `11` = list(scale = "Beta", min = "Min"),
    list()
  )
}
