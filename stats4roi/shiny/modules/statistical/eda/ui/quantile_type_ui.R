# Shared quantile algorithm picker and help text for EDA tabs.

quantile_type_picker_input <- function(
  ns,
  input_id,
  selected = DEFAULT_QUANTILE_TYPE
) {
  pickerInput(
    inputId = ns(input_id),
    label = "Quantile algorithm",
    choices = quantile_type_picker_choices(),
    selected = selected,
    multiple = FALSE
  )
}

quantile_type_help_output <- function(ns, output_id) {
  uiOutput(ns(output_id))
}

render_quantile_type_help <- function(type_reactive) {
  renderUI({
    type <- type_reactive()
    HTML(quantile_type_description_html(type))
  })
}
