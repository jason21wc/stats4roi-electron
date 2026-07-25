# Pure helpers for SPC / PPA limit picker choice vectors (Variables parity).

if (!exists("choice_r_limits2", mode = "object")) {
  if (file.exists("modules/statistical/spc/utils/spc_constants.R")) {
    source("modules/statistical/spc/utils/spc_constants.R", local = FALSE)
  }
}

#' Individuals (X) location limit choices — IDs 6-9 (PPA; no Custom).
spc_individuals_loc_lim_choices <- function() {
  ch <- 6:9
  names(ch) <- c(
    "Average Moving Range of X",
    "Median Moving Range of X",
    "Standard Deviation of X",
    "Known \u03c3"
  )
  ch
}

#' Individuals (X) dispersion limit choices when dispersion is MR — IDs 6-10 subset.
spc_individuals_mr_disp_lim_choices <- function() {
  ch <- c(6:9, 12)
  names(ch) <- c(
    "Average MR",
    "Median MR",
    "Standard Deviation of X",
    "Known \u03c3",
    "Custom"
  )
  ch
}

#' X-bar location limit choices for PPA (no Custom; σ must remain estimable).
spc_xbar_loc_lim_choices <- function() {
  ch <- choice_x_bar_limits2
  ch[as.integer(ch) != 12L]
}

#' Case 3 (repeated measures) within-row dispersion limit choices — IDs 1-5, 9.
spc_replicate_disp_lim_choices <- function() {
  ch <- c(1:5, 9)
  names(ch) <- c(
    "Average Range",
    "Median Range",
    "Average Standard Deviation",
    "Median Standard Deviation",
    "Average Variance",
    "Known \u03c3"
  )
  ch
}

#' Dispersion limit choices for X-bar charts by dispersion type.
spc_disp_lim_choices_for_type <- function(disp_type, replicate_means = FALSE) {
  if (isTRUE(replicate_means)) {
    return(spc_replicate_disp_lim_choices())
  }
  disp_type <- as.integer(disp_type)
  if (disp_type == 2L) {
    ch <- c(1:9, 12)
    names(ch) <- c(
      "Average Range",
      "Median Range",
      "Average Standard Deviation",
      "Median Standard Deviation",
      "Average Variance",
      if (replicate_means) "Average MR of means" else "Average MR of s",
      if (replicate_means) "Median MR of means" else "Median MR of s",
      if (replicate_means) "Standard Deviation of means" else "Standard Deviation of s",
      "Known \u03c3",
      "Custom"
    )
    return(ch)
  }
  if (disp_type == 3L) {
    ch <- c(1:9, 12)
    names(ch) <- c(
      "Average Range",
      "Median Range",
      "Average Standard Deviation",
      "Median Standard Deviation",
      "Average Variance",
      if (replicate_means) "Average MR of means\u00b2" else "Average MR of s\u00b2",
      if (replicate_means) "Median MR of means\u00b2" else "Median MR of s\u00b2",
      if (replicate_means) "Standard Deviation of means\u00b2" else "Standard Deviation of s\u00b2",
      "Known \u03c3",
      "Custom"
    )
    return(ch)
  }
  if (disp_type == 4L) {
    ch <- c(1:9, 12)
    names(ch) <- c(
      "Average Range",
      "Median Range",
      "Average Standard Deviation",
      "Median Standard Deviation",
      "Average Variance",
      if (replicate_means) "Average MR" else "Average MR",
      if (replicate_means) "Median MR" else "Median MR",
      if (replicate_means) "Standard Deviation of means" else "Standard Deviation of X",
      "Known \u03c3",
      "Custom"
    )
    return(ch)
  }
  # Range (default for X-bar / replicate)
  choice_r_limits2[c(1:4, 12)]
}

#' Default limit_config list for a PPA data shape.
ppa_default_limit_config <- function(data_shape = c("single", "subgroup", "replicate")) {
  data_shape <- match.arg(data_shape)
  base <- list(
    mr_span = 2L,
    known_sigma = NULL,
    custom_disp = NULL,
    std_err = 3L
  )
  switch(
    data_shape,
    single = modifyList(base, list(
      data_shape = "single",
      ind_or_mean = FALSE,
      loc_type = 2L,
      disp_type = 4L,
      loc_lim = 7L,
      disp_lim = 7L,
      loc_center = 2L,
      disp_center = 2L
    )),
    subgroup = modifyList(base, list(
      data_shape = "subgroup",
      ind_or_mean = TRUE,
      loc_type = 1L,
      disp_type = 1L,
      loc_lim = 2L,
      disp_lim = 2L,
      loc_center = 2L,
      disp_center = 2L
    )),
    replicate = modifyList(base, list(
      data_shape = "replicate",
      ind_or_mean = FALSE,
      loc_type = 2L,
      disp_type = 1L,
      loc_lim = 7L,
      disp_lim = 1L,
      loc_center = 1L,
      disp_center = 1L
    ))
  )
}

#' Which limit statistic drives sigma_potential (SPC Variables parity).
#'
#' Repeated measures always use the location (means-as-individuals) limit because
#' within-row R/s/s² dispersion tracks measurement error, not product variation.
spc_sigma_source_rule <- function(limit_config) {
  shape <- limit_config$data_shape %||% NULL
  if (identical(shape, "replicate")) {
    return("location")
  }
  disp_type <- as.integer(limit_config$disp_type %||% 4L)
  disp_lim <- as.integer(limit_config$disp_lim %||% 7L)
  if (disp_type == 4L) {
    return("dispersion")
  }
  if (disp_lim < 6L) {
    return("dispersion")
  }
  "location"
}
