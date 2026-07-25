# Distribution Fitting orchestrator

source("modules/statistical/spc/utils/dfit_data_prep.R")
source("modules/statistical/spc/utils/dfit_descriptives.R")
source("modules/statistical/spc/utils/dfit_conformance.R")
source("modules/statistical/spc/utils/dfit_distribution_fit.R")
source("modules/statistical/spc/utils/dfit_capability.R")

dfit_analyze <- function(
  x,
  spec = list(lsl = NA_real_, target = NA_real_, usl = NA_real_),
  distribution_id = 0L,
  overrides = NULL,
  mr_span = 2L
) {
  x <- stats::na.omit(as.numeric(x))
  sd_pot <- dfit_sd_potential(x, mr_span = mr_span)
  descriptives <- dfit_descriptives(x, sd_potential = sd_pot, mr_span = mr_span)
  conformance <- dfit_conformance(x, spec = spec)
  fit <- dfit_fit_distribution(x, distribution_id, overrides = overrides)
  capability <- dfit_capability(x, spec, fit, sd_potential = sd_pot)

  list(
    x = x,
    descriptives = descriptives,
    conformance = conformance,
    fit = fit,
    capability = capability,
    spec = spec,
    distribution_id = as.integer(distribution_id)
  )
}
