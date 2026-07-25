# Shared SPC chart / limit picker UI (Variables parity)

ppa_custom_disp_limits_ui_content <- function(ns, id_prefix) {
  tagList(
    hr(style = "border-top: 1px solid #000000;"),
    h4("Custom dispersion limits"),
    numericInput(ns(paste0(id_prefix, "_custom_disp_upper")), "UCL", value = 0),
    numericInput(ns(paste0(id_prefix, "_custom_disp_center")), "Centerline", value = 0),
    numericInput(ns(paste0(id_prefix, "_custom_disp_lower")), "LCL", value = 0)
  )
}

ppa_known_sigma_ui_content <- function(ns, id_prefix) {
  numericInput(
    inputId = ns(paste0(id_prefix, "_known_sigma")),
    label = "Known \u03c3",
    value = 1,
    min = 0
  )
}

ppa_limit_custom_panels <- function(ns, id_prefix) {
  # Location Custom is not offered in PPA (process σ must remain estimable).
  uiOutput(ns(paste0(id_prefix, "_custom_disp_limits_ui")))
}

ppa_limit_common_footer <- function(ns, id_prefix) {
  tagList(
    selectInput(
      inputId = ns(paste0(id_prefix, "_loc_center")),
      label = "Location centerline",
      choices = choice_x_centerline,
      selected = 1
    ),
    selectInput(
      inputId = ns(paste0(id_prefix, "_disp_center")),
      label = "Dispersion centerline",
      choices = choice_x_centerline,
      selected = 2
    ),
    selectInput(
      inputId = ns(paste0(id_prefix, "_std_err")),
      label = "Standard errors",
      choices = c(2, 3),
      selected = 3
    ),
    uiOutput(ns(paste0(id_prefix, "_known_sigma_ui")))
  )
}

#' Case 1: one measure per row — Individuals (X) + Moving Range only.
create_spc_individuals_chart_limits_ui <- function(ns, id_prefix = "ppa") {
  defs <- ppa_default_limit_config("single")
  loc_ch <- spc_individuals_loc_lim_choices()
  disp_ch <- spc_individuals_mr_disp_lim_choices()
  tagList(
    helpText("One value per sample: Individuals chart with Moving Range dispersion."),
    selectInput(
      inputId = ns(paste0(id_prefix, "_loc_type")),
      label = "Location chart",
      choices = c("X" = 2),
      selected = 2
    ),
    selectInput(
      inputId = ns(paste0(id_prefix, "_disp_type")),
      label = "Dispersion chart",
      choices = choice_disp_spc[4],
      selected = 4
    ),
    numericInput(
      inputId = ns(paste0(id_prefix, "_mr_span")),
      label = "Span of MR",
      value = defs$mr_span,
      min = 2,
      step = 1
    ),
    selectInput(
      inputId = ns(paste0(id_prefix, "_loc_lim")),
      label = "Location limit calculation",
      choices = loc_ch,
      selected = defs$loc_lim
    ),
    selectInput(
      inputId = ns(paste0(id_prefix, "_disp_lim")),
      label = "Dispersion limit calculation",
      choices = disp_ch,
      selected = defs$disp_lim
    ),
    ppa_limit_custom_panels(ns, id_prefix),
    ppa_limit_common_footer(ns, id_prefix)
  )
}

#' Case 2: sample items — X-bar + Range / s / s\u00b2.
create_spc_xbar_chart_limits_ui <- function(ns, id_prefix = "ppa") {
  defs <- ppa_default_limit_config("subgroup")
  tagList(
    helpText(
      "Multiple measures per sample: location chart uses subgroup means; ",
      "dispersion chart estimates within-sample variation."
    ),
    selectInput(
      inputId = ns(paste0(id_prefix, "_loc_type")),
      label = "Location chart",
      choices = c("X-bar" = 1),
      selected = 1
    ),
    selectInput(
      inputId = ns(paste0(id_prefix, "_disp_type")),
      label = "Dispersion chart",
      choices = choice_disp_spc[1:3],
      selected = defs$disp_type
    ),
    selectInput(
      inputId = ns(paste0(id_prefix, "_loc_lim")),
      label = "Location limit calculation",
      choices = spc_xbar_loc_lim_choices(),
      selected = defs$loc_lim
    ),
    uiOutput(ns(paste0(id_prefix, "_disp_lim_ui"))),
    ppa_limit_custom_panels(ns, id_prefix),
    ppa_limit_common_footer(ns, id_prefix)
  )
}

#' Case 3: repeated measures — Individuals on row means + within-row R/s/s\u00b2.
create_spc_replicate_chart_limits_ui <- function(ns, id_prefix = "ppa") {
  defs <- ppa_default_limit_config("replicate")
  loc_ch <- spc_individuals_loc_lim_choices()
  tagList(
    helpText(
      "Repeated measures: location chart uses the row average (product variation); ",
      "dispersion chart uses within-row R / s / s\u00b2 (measurement error). ",
      "Moving-range OOC of means is flagged on the location chart without a separate MR chart."
    ),
    selectInput(
      inputId = ns(paste0(id_prefix, "_loc_type")),
      label = "Location chart",
      choices = c("X" = 2),
      selected = 2
    ),
    selectInput(
      inputId = ns(paste0(id_prefix, "_disp_type")),
      label = "Dispersion chart",
      choices = choice_disp_spc[1:3],
      selected = defs$disp_type
    ),
    selectInput(
      inputId = ns(paste0(id_prefix, "_loc_lim")),
      label = "Location limit calculation",
      choices = loc_ch,
      selected = defs$loc_lim
    ),
    uiOutput(ns(paste0(id_prefix, "_disp_lim_ui"))),
    ppa_limit_custom_panels(ns, id_prefix),
    ppa_limit_common_footer(ns, id_prefix)
  )
}

#' @rdname create_spc_individuals_chart_limits_ui
create_spc_chart_limits_ui <- create_spc_individuals_chart_limits_ui
