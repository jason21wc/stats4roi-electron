# ANOVA Data Invalidation Helper Functions
# Reuses shared EDA trigger primitives; ANOVA-specific UI reset lives here.

if (!exists("create_data_invalidation_trigger", mode = "function")) {
  source("modules/statistical/eda/utils/data_invalidation_helpers.R", local = FALSE)
}

safe_anova_update <- safe_eda_update

# Reset data-driven ANOVA UI when global invalidation fires (new working data).
# Optional on_after_reset supports coordinator-only navigation (e.g. Multi-Factor Set Up tab).
reset_anova_data_driven_ui <- function(session, ns, on_after_reset = NULL) {
  safe_anova_update(updatePickerInput(session, ns("ow_factor"), selected = character(0)))
  safe_anova_update(updatePickerInput(session, ns("ow_data"), selected = character(0)))
  safe_anova_update(updateRadioGroupButtons(session, ns("type_ow"), selected = 1))
  safe_anova_update(updateNumericInput(session, ns("conf_ow"), value = 0.95))
  safe_anova_update(updateNumericInput(session, ns("decimal_ow"), value = 4))
  safe_anova_update(updateNumericInput(session, ns("decimal_ow_ph"), value = 4))
  safe_anova_update(updateNumericInput(session, ns("ow_font_size"), value = 11))
  safe_anova_update(updatePrettySwitch(session, ns("disp_ow"), value = TRUE))
  safe_anova_update(updateMaterialSwitch(session, ns("ow_disp_analysis"), value = FALSE))
  safe_anova_update(updateRadioGroupButtons(session, ns("ow_disp_type"), selected = 1))
  safe_anova_update(updatePrettySwitch(session, ns("lines_ow_ph"), value = TRUE))
  safe_anova_update(updateCheckboxInput(session, ns("ow_ph_details"), value = FALSE))
  safe_anova_update(updateCheckboxInput(session, ns("ow_ph_homogeneous"), value = FALSE))
  safe_anova_update(updateRadioButtons(session, ns("ow_ph_type"), selected = character(0)))

  safe_anova_update(updatePickerInput(session, ns("factors_ems"), selected = character(0)))
  safe_anova_update(updatePickerInput(session, ns("data_ems"), selected = character(0)))
  safe_anova_update(updatePickerInput(session, ns("data_ems_active"), selected = character(0)))

  safe_anova_update(updateCheckboxInput(session, ns("ems_show_mixed_nest"), value = FALSE))
  safe_anova_update(updateCheckboxInput(session, ns("ems_show_pool"), value = FALSE))
  safe_anova_update(updateCheckboxInput(session, ns("ems_show_rfc"), value = FALSE))
  safe_anova_update(updateCheckboxInput(session, ns("ems_disp"), value = FALSE))
  safe_anova_update(updateCheckboxInput(session, ns("ems_show_coeffs"), value = FALSE))
  safe_anova_update(updateCheckboxInput(session, ns("ems_show_optimum"), value = FALSE))
  safe_anova_update(updateCheckboxInput(session, ns("ems_show_box"), value = FALSE))

  safe_anova_update(updateNumericInput(session, ns("ems_conf"), value = 0.95))
  safe_anova_update(updateNumericInput(session, ns("ems_dec"), value = 4))
  safe_anova_update(updateNumericInput(session, ns("ems_target"), value = NA))
  safe_anova_update(updateNumericInput(session, ns("multi_response_tol"), value = 0))
  safe_anova_update(updateNumericInput(session, ns("ph_font_size"), value = 11))

  safe_anova_update(updatePickerInput(session, ns("ems_pool"), selected = character(0)))
  safe_anova_update(updatePickerInput(session, ns("ems_primary_col"), selected = character(0)))
  safe_anova_update(updatePickerInput(session, ns("ems_ph_select"), selected = character(0)))
  safe_anova_update(updatePickerInput(session, ns("ems_ph_effects"), selected = character(0)))
  safe_anova_update(updatePickerInput(session, ns("ems_int_selected"), selected = character(0)))
  safe_anova_update(updatePrettyCheckboxGroup(session, ns("ems_ph_plot_options"), selected = c("CIs", "PIs")))

  safe_anova_update(updateRadioGroupButtons(session, ns("ems_disp_type"), selected = 1))
  safe_anova_update(updateCheckboxInput(session, ns("ems_ems"), value = FALSE))
  safe_anova_update(updateRadioGroupButtons(session, ns("ems_ems"), selected = 1))

  for (i in seq_len(20L)) {
    safe_anova_update(updateRadioButtons(session, ns(paste0("f_r_factor", i)), selected = "F"))
    safe_anova_update(updateCheckboxGroupInput(session, ns(paste0("nest_factor", i)), selected = character(0)))
    safe_anova_update(updateMaterialSwitch(session, ns(paste0("ems_factor_continuous", i)), value = FALSE))
    safe_anova_update(updateTextInput(session, ns(paste0("ems_factor_actual", i)), value = ""))
  }

  safe_anova_update(updateTabsetPanel(session, ns("mw_anova"), selected = ns("mw_su")))
  safe_anova_update(updateTabsetPanel(session, ns("mw_results_nav"), selected = "ANOVA"))

  safe_anova_update(updateNumericInput(session, ns("loss_mf_target"), value = 0))
  safe_anova_update(updateNumericInput(session, ns("loss_mf_dec"), value = 4))
  safe_anova_update(updateNumericInput(session, ns("loss_mf_C_l"), value = 1))
  safe_anova_update(updateNumericInput(session, ns("loss_mf_C_u"), value = 1))
  safe_anova_update(updateNumericInput(session, ns("loss_mf_lsl"), value = NA))
  safe_anova_update(updateNumericInput(session, ns("loss_mf_usl"), value = NA))
  safe_anova_update(updateSelectInput(session, ns("loss_mf_opt_target"), selected = "taguchi_loss"))
  safe_anova_update(updateNumericInput(session, ns("loss_mf_opt_volume"), value = 1))
  safe_anova_update(updateCheckboxInput(session, ns("loss_mf_opt_use_factor_costs"), value = FALSE))
  safe_anova_update(updatePickerInput(session, "loss_mf_opt_continuous", choices = character(0), selected = character(0)))
  safe_anova_update(updateNumericInput(session, ns("loss_ow_target"), value = 0))
  safe_anova_update(updateNumericInput(session, ns("loss_ow_dec"), value = 4))
  safe_anova_update(updateNumericInput(session, ns("loss_ow_C_l"), value = 1))
  safe_anova_update(updateNumericInput(session, ns("loss_ow_C_u"), value = 1))
  safe_anova_update(updateNumericInput(session, ns("loss_ow_lsl"), value = NA))
  safe_anova_update(updateNumericInput(session, ns("loss_ow_usl"), value = NA))
  safe_anova_update(updateSelectInput(session, ns("loss_ow_opt_target"), selected = "taguchi_loss"))
  safe_anova_update(updateNumericInput(session, ns("loss_ow_opt_volume"), value = 1))
  safe_anova_update(updateCheckboxInput(session, ns("loss_ow_opt_use_factor_costs"), value = FALSE))
  safe_anova_update(updatePickerInput(session, "loss_ow_opt_continuous", choices = character(0), selected = character(0)))

  if (is.function(on_after_reset)) {
    on_after_reset()
  }
}
