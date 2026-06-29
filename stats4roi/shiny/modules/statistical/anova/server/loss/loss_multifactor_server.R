# Multifactor Loss / optimization tab server (static economics + explicit compute).

#' @param loss_mf_results_val Shared reactiveVal for computed loss grids.
#' @param mf_loss_tier3_snapshot Shared reactiveVal for Policy 3 UI state.
#' @param loss_mf_calculating Shared reactiveVal for calculate-in-progress flag.
#' @param loss_mf_opt_running Shared reactiveVal for optimizer running flag.
#' @keywords internal
loss_multifactor_server <- function(
    input,
    output,
    session,
    ns,
    filtered_data,
    multifactor_worker,
    active_data_ems,
    mf_resolved_data_ems,
    mf_resolved_data_ems_r,
    mf_resolved_factors_ems,
    mf_resolved_active_did,
    mf_model_registry,
    mf_model_finalized,
    mf_optimization_session,
    mf_results_state,
    loss_mf_opt_result_val,
    loss_mf_results_val,
    mf_loss_tier3_snapshot,
    loss_mf_calculating,
    loss_mf_opt_running,
    get_or_build_mf_fit,
    bump_mf_optimization_model_rev,
    mf_optimization_model_rev,
    mf_applied_pool_rev,
    mf_pooling_pending_differs,
    current_effect_choices,
    applied_effect_pool_for_did_mode,
    get_saved_or_current_response_state,
    mf_model_readiness,
    mf_optimization_readiness_full,
    mf_resolved_f_r_types,
    mf_resolved_ems_show_mixed_nest,
    mf_loss_tab_rev,
    mf_loss_setup_rev,
    mf_design_setup,
    mf_loss_setup_cache,
    loss_mf_saved_factor_costs) {

    # -------------------------------------------------------------------------
    # Loss / optimization (Taguchi MVP â€” multifactor)
    # -------------------------------------------------------------------------
    mf_loss_dispersion_type_id <- function() {
      if (isTRUE(input$ems_disp)) {
        dt <- suppressWarnings(as.integer(input$ems_disp_type)[1])
        if (is.na(dt) || dt < 1L || dt > 3L) 1L else dt
      } else {
        1L
      }
    }

    mf_loss_dispersion_type_label <- function() {
      lbl <- c("ADA", "ADM", "ADM(n-1)")
      lbl[mf_loss_dispersion_type_id()]
    }

    mf_response_dispersion_metric_label <- function(did) {
      st <- get_saved_or_current_response_state(did)
      dt <- if (!is.null(st$ems_disp_type)) as.integer(st$ems_disp_type)[1] else mf_loss_dispersion_type_id()
      lbl <- c("ADA", "ADM", "ADM(n-1)")[dt]
      if (is.na(lbl)) "ADA" else lbl
    }

    output$loss_mf_disp_metric_note <- renderText({
      req(mf_loss_tab_active())
      d <- filtered_data()
      dids <- mf_resolved_data_ems()
      req(d, !is.null(dids), length(dids) >= 1L)
      labels <- unique(vapply(dids, mf_response_dispersion_metric_label, character(1)))
      if (length(labels) == 1L) {
        paste0(
          " ",
          labels[[1L]],
          " (per response, from committed Results dispersion models)"
        )
      } else {
        paste0(
          " Mixed per response (",
          paste(labels, collapse = ", "),
          "); from committed Results dispersion models"
        )
      }
    })

    loss_mf_confirmation_val <- reactiveVal(NULL)

    mf_loss_tab_active <- reactive({
      cur <- input$mw_anova
      if (is.null(cur)) {
          return(FALSE)
      }
      cur <- as.character(cur)
      active <- identical(cur, ns("mw_loss")) || grepl("Loss", cur, fixed = TRUE)
      active
    })

    mf_loss_econ_last_active <- reactiveVal(NULL)
    mf_loss_econ_active_label <- reactiveVal("")
    mf_loss_econ_programmatic <- reactiveVal(FALSE)
    mf_loss_econ_refresh_pending <- reactiveVal(FALSE)

    mf_loss_resolved_dids <- function() {
      dids <- isolate(mf_resolved_data_ems())
      if (!is.null(dids) && length(dids) >= 1L) {
        dids <- suppressWarnings(as.integer(as.numeric(dids)))
        dids <- dids[is.finite(dids)]
        if (length(dids) >= 1L) return(dids)
      }
      cached <- isolate(mf_loss_setup_cache$data_ems)
      if (!is.null(cached) && length(cached) >= 1L) {
        cached <- suppressWarnings(as.integer(as.numeric(cached)))
        cached <- cached[is.finite(cached)]
        if (length(cached) >= 1L) return(cached)
      }
      NULL
    }

    schedule_loss_econ_picker_refresh <- function() {
      if (!isTRUE(isolate(mf_loss_tab_active()))) return(invisible(NULL))
      if (isTRUE(isolate(mf_loss_econ_refresh_pending()))) return(invisible(NULL))
      mf_loss_econ_refresh_pending(TRUE)
      session$onFlushed(function() {
        mf_loss_econ_refresh_pending(FALSE)
        mf_refresh_loss_econ_picker()
      }, once = TRUE)
      invisible(NULL)
    }

    mf_safe_update_numeric <- function(input_id, value) {
      val <- suppressWarnings(as.numeric(value))
      if (length(val) != 1L || !is.finite(val)) {
        val <- NA_real_
      }
      shiny::updateNumericInput(session, input_id, value = val)
    }

    output$loss_mf_econ_active_label <- shiny::renderText({
      lbl <- mf_loss_econ_active_label()
      if (!nzchar(lbl)) "" else lbl
    })

    mf_load_economics_inputs <- function(session, did, d) {
      if (is.null(did) || !is.finite(suppressWarnings(as.numeric(did)[1L]))) {
        return(invisible(NULL))
      }
      did <- as.integer(did)[1L]
      econ <- opt_session_get_economics(session, as.character(did), global_fallback = opt_economics_defaults())
      mf_safe_update_numeric("loss_mf_target", econ$target)
      mf_safe_update_numeric("loss_mf_lsl", econ$lsl)
      mf_safe_update_numeric("loss_mf_usl", econ$usl)
      mf_safe_update_numeric("loss_mf_C_l", econ$C_l)
      mf_safe_update_numeric("loss_mf_C_u", econ$C_u)
      delta <- opt_session_get_resolution_delta(session, as.character(did))
      if (!is.null(delta) && is.finite(delta)) {
        mf_safe_update_numeric("loss_mf_disp_delta", delta)
      }
      rn <- if (did >= 1L && did <= ncol(d)) names(d)[did] else as.character(did)
      mf_loss_econ_active_label(paste0("Editing economics for: ", rn))
      invisible(NULL)
    }

    mf_refresh_loss_econ_picker <- function() {
      if (!isTRUE(isolate(mf_loss_tab_active()))) return(invisible(NULL))
      d <- isolate(filtered_data())
      dids <- mf_loss_resolved_dids()
      if (is.null(d) || is.null(dids) || length(dids) < 1L) {
        shiny::updateSelectInput(
          session,
          "loss_mf_econ_active",
          choices = c("Select responses on Set Up" = ""),
          selected = ""
        )
        return(invisible(NULL))
      }
      choices <- loss_economics_picker_choices(d, dids)
      if (length(choices) < 1L) {
        shiny::updateSelectInput(
          session,
          "loss_mf_econ_active",
          choices = c("Select responses on Set Up" = ""),
          selected = ""
        )
        return(invisible(NULL))
      }
      selected <- loss_economics_picker_selected(choices, isolate(input$loss_mf_econ_active))
      mf_loss_econ_programmatic(TRUE)
      on.exit(mf_loss_econ_programmatic(FALSE), add = TRUE)
      mf_loss_econ_last_active(as.integer(selected))
      shiny::updateSelectInput(session, "loss_mf_econ_active", choices = choices, selected = selected)
      mf_load_economics_inputs(isolate(mf_optimization_session()), as.integer(selected), d)
      invisible(NULL)
    }

    observeEvent(input$mw_anova, {
      if (isTRUE(isolate(mf_loss_tab_active()))) {
        schedule_loss_econ_picker_refresh()
      }
    }, ignoreInit = TRUE)

    observeEvent(mf_loss_tab_rev(), {
      schedule_loss_econ_picker_refresh()
    }, ignoreInit = TRUE)

    observeEvent(mf_loss_setup_rev(), {
      if (isTRUE(isolate(mf_loss_tab_active()))) {
        schedule_loss_econ_picker_refresh()
      }
    }, ignoreInit = TRUE)

    observeEvent(input$loss_mf_econ_active, {
      if (isTRUE(isolate(mf_loss_econ_programmatic()))) return(invisible(NULL))
      if (!isTRUE(isolate(mf_loss_tab_active()))) return(invisible(NULL))
      new_did <- suppressWarnings(as.integer(input$loss_mf_econ_active))
      if (length(new_did) < 1L || !is.finite(new_did[[1L]])) return(invisible(NULL))
      new_did <- new_did[[1L]]
      old_did <- isolate(mf_loss_econ_last_active())
      if (!is.null(old_did) && is.finite(old_did) && identical(as.integer(old_did), as.integer(new_did))) {
        return(invisible(NULL))
      }
      sess <- isolate(mf_optimization_session())
      if (!is.null(old_did) && is.finite(old_did)) {
        sess <- mf_sync_active_economics_to_session(sess, old_did, input)
        sess <- mf_sync_active_delta_to_session(sess, old_did)
      }
      mf_optimization_session(sess)
      mf_loss_econ_last_active(new_did)
      d <- filtered_data()
      if (!is.null(d)) {
        mf_load_economics_inputs(isolate(mf_optimization_session()), new_did, d)
      }
    }, ignoreInit = TRUE)

    output$loss_mf_disp_delta_context <- shiny::renderText({
      snap <- isolate(mf_loss_tier3_snapshot())
      req(snap, snap$rbr)
      active <- suppressWarnings(as.integer(isolate(input$loss_mf_econ_active)))
      if (!is.finite(active[[1L]])) return("")
      d <- filtered_data()
      req(d)
      rn <- if (active[[1L]] >= 1L && active[[1L]] <= ncol(d)) names(d)[active[[1L]]] else as.character(active[[1L]])
      if (!rn %in% names(snap$rbr)) return("")
      ctx <- snap$rbr[[rn]]$resolution_ctx
      if (is.null(ctx)) return("")
      conf <- if (!is.null(isolate(input$ems_conf))) isolate(input$ems_conf) else 0.95
      paste0(
        "Response: ", rn,
        " | Delta_min: ", if (is.finite(ctx$delta_min)) signif(ctx$delta_min, 4) else "n/a",
        " | n_bar: ", if (is.finite(ctx$n_mean)) signif(ctx$n_mean, 3) else "n/a",
        " | confidence: ", conf
      )
    })

    output$loss_mf_disp_delta_recommended <- shiny::renderText({
      snap <- isolate(mf_loss_tier3_snapshot())
      if (is.null(snap) || is.null(snap$rbr)) return("")
      active <- suppressWarnings(as.integer(isolate(input$loss_mf_econ_active)))
      if (!is.finite(active[[1L]])) return("")
      d <- filtered_data()
      req(d)
      rn <- if (active[[1L]] >= 1L && active[[1L]] <= ncol(d)) names(d)[active[[1L]]] else as.character(active[[1L]])
      r <- snap$rbr[[rn]]
      if (is.null(r) || is.null(r$resolution_ctx)) return("")
      rec <- r$resolution_ctx$delta_recommended
      if (is.finite(rec)) paste0("Recommended delta: ", signif(rec, 4)) else "Set resolution delta manually."
    })

    observe({
      snap <- isolate(mf_loss_tier3_snapshot())
      active <- suppressWarnings(as.integer(isolate(input$loss_mf_econ_active)))
      flag <- 0
      if (!is.null(snap) && !is.null(snap$summaries) && length(active) >= 1L && is.finite(active[[1L]])) {
        d <- filtered_data()
        if (!is.null(d)) {
          rn <- if (active[[1L]] >= 1L && active[[1L]] <= ncol(d)) names(d)[active[[1L]]] else as.character(active[[1L]])
          s <- snap$summaries[[rn]]
          if (!is.null(s) && isTRUE(s$uses_tier3)) flag <- 1
        }
      }
      shiny::updateNumericInput(session, "loss_mf_tier3_flag", value = flag)
    })

    mf_global_economics <- function() {
      opt_economics_from_scalars(
        target = input$loss_mf_target,
        lsl = input$loss_mf_lsl,
        usl = input$loss_mf_usl,
        C_l = input$loss_mf_C_l,
        C_u = input$loss_mf_C_u
      )
    }

    mf_get_economics_for_response <- function(did, response_name, d, did_all) {
      session <- isolate(mf_optimization_session())
      opt_session_get_economics(session, as.character(as.integer(did)), global_fallback = opt_economics_defaults())
    }

    mf_active_aov_disp <- function(has_disp_pool, disp_mode) {
      mf_pick_valid_aov_disp(
        has_disp_pool = has_disp_pool,
        disp_mode = disp_mode,
        pooled_aov = multifactor_worker$ems_pooled_dispersion(),
        unpooled_aov = multifactor_worker$aov_out_dispersion()
      )
    }

    observeEvent(input$data_ems, {
      sel <- suppressWarnings(as.numeric(input$data_ems))
      sel <- sel[is.finite(sel)]
      keep <- as.character(as.integer(sel))
      old <- isolate(mf_model_registry$by_response)
      if (length(old) > 0) {
        mf_model_registry$by_response <- old[names(old) %in% keep]
      }
      old_finalized <- isolate(mf_model_finalized$by_response)
      if (length(old_finalized) > 0) {
        mf_model_finalized$by_response <- old_finalized[names(old_finalized) %in% keep]
      }
      mf_optimization_session(opt_session_prune(isolate(mf_optimization_session()), keep))
      bump_mf_optimization_model_rev()
    }, ignoreInit = TRUE)

    mf_sync_active_delta_to_session <- function(session, active_did) {
      if (is.null(active_did) || !is.finite(suppressWarnings(as.numeric(active_did)[1L]))) {
        return(session)
      }
      if (!is.null(input$loss_mf_disp_delta)) {
        session <- opt_session_set_resolution_delta(
          session,
          as.character(as.integer(active_did)[1L]),
          input$loss_mf_disp_delta
        )
      }
      session
    }

    mf_sync_loss_economics_from_inputs <- function(active_did) {
      sess <- isolate(mf_optimization_session())
      sess <- mf_sync_active_economics_to_session(sess, active_did, input)
      mf_sync_active_delta_to_session(sess, active_did)
    }

    mf_validate_loss_economics <- function(dids, resp_names, session) {
      mf_validate_session_economics_all(session, dids, resp_names)
    }

    mf_run_loss_grid_compute <- function(session) {
      d <- filtered_data()
      shiny::req(d)
      fid <- mf_resolved_factors_ems()
      shiny::req(fid, length(fid) >= 2L)
      dids <- isolate(mf_resolved_data_ems())
      shiny::req(!is.null(dids), length(dids) >= 1L)
      conf_default <- if (!is.null(isolate(input$ems_conf))) isolate(input$ems_conf) else 0.95
      ems_disp_type_default <- if (!is.null(isolate(input$ems_disp_type))) {
        as.integer(isolate(input$ems_disp_type))[1]
      } else {
        1L
      }
      f_r_types <- mf_resolved_f_r_types(fid)
      fnames <- make.names(names(d)[fid])
      blocked_vars <- isolate({
        if (!is.null(input$loss_mf_opt_blocked)) unique(as.character(input$loss_mf_opt_blocked)) else character(0)
      })
      blocked_vars <- mf_align_continuous_vars(blocked_vars, fnames, d, fid)
      mf_build_loss_grid_all_responses(
        d = d,
        fid = fid,
        dids = dids,
        session = session,
        registry_by_response = isolate(mf_model_registry$by_response),
        finalized_by_response = isolate(mf_model_finalized$by_response),
        get_state_fn = get_saved_or_current_response_state,
        f_r_types = f_r_types,
        ems_show_mixed_nest = mf_resolved_ems_show_mixed_nest(),
        ems_disp_type_default = ems_disp_type_default,
        conf_default = conf_default,
        get_resolution_delta_fn = function(did_i, rn) {
          mf_get_resolution_delta_for_response(session, input, as.character(did_i), rn)
        },
        blocked_factors = blocked_vars
      )
    }

    loss_mf_results_by_response <- reactive({
      req(mf_loss_tab_active())
      loss_mf_results_val()
    })

    observeEvent(input$loss_mf_calc_run, {
      req(mf_loss_tab_active())
      loss_mf_calculating(TRUE)
      on.exit(loss_mf_calculating(FALSE), add = TRUE)
      d <- filtered_data()
      if (is.null(d)) {
        shiny::showNotification("No data available.", type = "error", duration = 5)
        return(invisible(NULL))
      }
      dids <- mf_resolved_data_ems()
      if (is.null(dids) || length(dids) < 1L) {
        shiny::showNotification("Select responses on Set Up first.", type = "warning", duration = 6)
        return(invisible(NULL))
      }
      resp_names <- vapply(dids, function(di) {
        if (di >= 1L && di <= ncol(d)) names(d)[di] else as.character(di)
      }, character(1))
      active_did <- suppressWarnings(as.integer(input$loss_mf_econ_active))
      if (!is.finite(active_did[[1L]])) {
        active_did <- dids[[1L]]
      } else {
        active_did <- active_did[[1L]]
      }
      new_sess <- mf_sync_loss_economics_from_inputs(active_did)
      mf_optimization_session(new_sess)
      misses <- mf_validate_loss_economics(dids, resp_names, isolate(mf_optimization_session()))
      if (length(misses) >= 1L) {
        shiny::showNotification(paste(misses, collapse = " "), type = "warning", duration = 10)
        return(invisible(NULL))
      }
      fid <- mf_resolved_factors_ems()
      full_ready <- mf_optimization_readiness_full(d, fid, dids)
      if (!isTRUE(full_ready$all_ready)) {
        shiny::showNotification(
          paste0("Loss grid blocked: ", paste(full_ready$blockers, collapse = "; ")),
          type = "warning",
          duration = 12
        )
        return(invisible(NULL))
      }
      out <- tryCatch(mf_run_loss_grid_compute(isolate(mf_optimization_session())), error = function(e) {
        list(`__error__` = list(ok = FALSE, message = conditionMessage(e), table = NULL, disclaimer = ""))
      })
      if (!is.null(out$`__error__`)) {
        loss_mf_results_val(out)
        shiny::showNotification(out$`__error__`$message, type = "error", duration = 8)
        return(invisible(NULL))
      }
      loss_mf_results_val(out)
      summaries <- lapply(names(out), function(rn) {
        r <- out[[rn]]
        if (isTRUE(r$ok)) summarize_dispersion_policy(r$table) else list(ok = FALSE, message = r$message, uses_tier3 = FALSE)
      })
      names(summaries) <- names(out)
      mf_loss_tier3_snapshot(list(summaries = summaries, rbr = out))
      failed_notes <- loss_multiresponse_assembly_notes(out)
      if (length(dids) >= 2L && length(failed_notes) >= 1L) {
        ok_n <- sum(vapply(out, function(r) isTRUE(r$ok) && is.data.frame(r$table) && nrow(r$table) >= 1L, logical(1)))
        if (ok_n >= 1L) {
          shiny::showNotification(
            paste0(
              "Loss grid partial: ",
              paste(failed_notes, collapse = " "),
              " Fix those responses and recalculate."
            ),
            type = "warning",
            duration = 12
          )
        } else {
          shiny::showNotification(paste(failed_notes, collapse = " "), type = "error", duration = 12)
        }
      } else {
        shiny::showNotification("Loss grid calculated.", type = "message", duration = 4)
      }
    }, ignoreInit = TRUE)

    mf_dispersion_policy_summaries <- reactive({
      req(mf_loss_tab_active())
      snap <- mf_loss_tier3_snapshot()
      if (is.null(snap) || is.null(snap$summaries)) {
        return(list())
      }
      snap$summaries
    })

    loss_mf_result <- reactive({
      req(mf_loss_tab_active())
      rbr <- loss_mf_results_val()
      if (is.null(rbr)) {
        return(list(ok = FALSE, message = "Click Calculate loss grid after entering Taguchi economics.", table = NULL, disclaimer = ""))
      }
      d <- filtered_data()
      shiny::req(d)
      dids <- mf_resolved_data_ems()
      shiny::req(!is.null(dids), length(dids) >= 1L)
      if (length(dids) >= 2L) {
        ok_any <- any(vapply(rbr, function(r) isTRUE(r$ok), logical(1)))
        if (!isTRUE(ok_any)) {
          msgs <- vapply(rbr, function(r) as.character(r$message)[1], character(1))
          return(list(ok = FALSE, message = paste(unique(msgs[nzchar(msgs)]), collapse = "; "), table = NULL, disclaimer = ""))
        }
        first_ok <- names(rbr)[which(vapply(rbr, function(r) isTRUE(r$ok), logical(1)))][1L]
        return(rbr[[first_ok]])
      }
      did <- as.numeric(active_data_ems())
      shiny::req(did)
      rn <- if (did >= 1L && did <= ncol(d)) names(d)[did] else as.character(did)
      if (!is.null(rbr[[rn]])) rbr[[rn]] else list(ok = FALSE, message = "Response not available for loss grid.", table = NULL, disclaimer = "")
    })

    observeEvent(mf_optimization_model_rev(), {
      loss_mf_results_val(NULL)
      mf_loss_tier3_snapshot(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$loss_mf_opt_run, {
      loss_mf_opt_running(TRUE)
      on.exit(loss_mf_opt_running(FALSE), add = TRUE)
      loss_mf_confirmation_val(NULL)
      loss_mf_opt_result_val(list(ok = FALSE, message = "Running optimizer...", running = TRUE))

      tryCatch({
        d <- filtered_data()
        if (is.null(d) || !is.data.frame(d) || nrow(d) < 2L) {
          loss_mf_opt_result_val(list(ok = FALSE, message = "No data available for optimization."))
          return(invisible(NULL))
        }
        fid <- mf_resolved_factors_ems()
        did <- mf_resolved_active_did()
        if (is.null(fid) || length(fid) < 2L) {
          loss_mf_opt_result_val(list(
            ok = FALSE,
            message = "Select at least two factors on the Set Up tab before running the optimizer."
          ))
          shiny::showNotification("Select factors on Set Up first.", type = "warning", duration = 6)
          return(invisible(NULL))
        }
        if (is.null(did) || !is.finite(did)) {
          loss_mf_opt_result_val(list(
            ok = FALSE,
            message = "Select at least one response on the Set Up tab before running the optimizer."
          ))
          shiny::showNotification("Select a response on Set Up first.", type = "warning", duration = 6)
          return(invisible(NULL))
        }
        did_all <- mf_resolved_data_ems()
        if (is.null(did_all) || length(did_all) < 1L) did_all <- did

        if (is.null(isolate(loss_mf_results_val()))) {
          loss_mf_opt_result_val(list(
            ok = FALSE,
            message = "Calculate the loss grid (with complete Taguchi economics) before running the optimizer."
          ))
          shiny::showNotification("Calculate loss grid first.", type = "warning", duration = 6)
          return(invisible(NULL))
        }
        resp_names <- vapply(did_all, function(di) {
          if (di >= 1L && di <= ncol(d)) names(d)[di] else as.character(di)
        }, character(1))
        econ_misses <- mf_validate_loss_economics(did_all, resp_names, isolate(mf_optimization_session()))
        if (length(econ_misses) >= 1L) {
          loss_mf_opt_result_val(list(
            ok = FALSE,
            message = paste("Taguchi economics incomplete:", paste(econ_misses, collapse = " "))
          ))
          return(invisible(NULL))
        }
        active_did <- suppressWarnings(as.integer(input$loss_mf_econ_active))
        if (!is.finite(active_did[[1L]])) active_did <- did_all[[1L]] else active_did <- active_did[[1L]]
        mf_optimization_session(mf_sync_loss_economics_from_inputs(active_did))

        full_ready <- mf_optimization_readiness_full(d, fid, did_all)
        if (!isTRUE(full_ready$all_ready)) {
          loss_mf_opt_result_val(list(
            ok = FALSE,
            message = paste0("Optimization blocked. ", paste(full_ready$blockers, collapse = "; ")),
            preflight = full_ready$per_response
          ))
          return(invisible(NULL))
        }

        f_r_types <- mf_resolved_f_r_types(fid)

        readiness <- mf_model_readiness(d = d, fid = fid, dids = did_all, active_did = did)

        bundle_out <- NULL
        opt_out <- NULL
        shiny::withProgress(
          message = "Running Taguchi optimizer",
          detail = "Searching factor settings...",
          value = 0.1,
          {
            bundle_out <- mf_build_model_bundles(
              d = d,
              fid = fid,
              did_all = did_all,
              active_did = did,
              f_r_types = f_r_types,
              ems_show_mixed_nest = mf_resolved_ems_show_mixed_nest(),
              readiness = readiness,
              get_state_fn = get_saved_or_current_response_state,
              get_economics_fn = function(did_i, did_name) mf_get_economics_for_response(did_i, did_name, d, did_all),
              registry_by_response = isolate(mf_model_registry$by_response),
              finalized_by_response = isolate(mf_model_finalized$by_response),
              build_fit_fn = function(...) {
                do.call(get_or_build_mf_fit, c(list(...), list(
                  ems_show_mixed_nest = mf_resolved_ems_show_mixed_nest(),
                  f_r_types = f_r_types
                )))
              },
              aov_mean_fn = function() multifactor_worker$aov_out_means(),
              ems_pooled_means_fn = function() multifactor_worker$ems_pooled_means(),
              aov_disp_active_fn = function(has_disp_pool_i, disp_mode_i) {
                mf_active_aov_disp(has_disp_pool_i, disp_mode_i)
              },
              aov_disp_cached_fn = function(has_disp_pool_i) {
                mf_active_aov_disp(has_disp_pool_i, FALSE)
              },
              get_resolution_delta_fn = function(did_i, did_name) {
                mf_get_resolution_delta_for_response(
                  isolate(mf_optimization_session()),
                  input,
                  as.character(did_i),
                  did_name
                )
              }
            )

            if (!isTRUE(bundle_out$ok)) {
              loss_mf_opt_result_val(list(
                ok = FALSE,
                message = bundle_out$message,
                preflight = if (!is.null(bundle_out$preflight)) bundle_out$preflight else readiness
              ))
              return(invisible(NULL))
            }

            fit_active <- bundle_out$fit_active
            model_bundles <- bundle_out$model_bundles
            economics_single <- mf_get_economics_for_response(
              did,
              if (did >= 1L && did <= ncol(d)) names(d)[did] else as.character(did),
              d,
              did_all
            )

            optimize_target <- if (!is.null(input$loss_mf_opt_target)) as.character(input$loss_mf_opt_target) else "taguchi_loss"
            volume <- suppressWarnings(as.numeric(input$loss_mf_opt_volume))
            cont_vars <- if (!is.null(input$loss_mf_opt_continuous)) unique(as.character(input$loss_mf_opt_continuous)) else character(0)
            cont_vars <- mf_align_continuous_vars(cont_vars, fit_active$factors_names, d, fid)
            blocked_vars <- if (!is.null(input$loss_mf_opt_blocked)) unique(as.character(input$loss_mf_opt_blocked)) else character(0)
            blocked_vars <- mf_align_continuous_vars(blocked_vars, fit_active$factors_names, d, fid)
            blocked_vars <- setdiff(blocked_vars, cont_vars)

            shiny::incProgress(0.2, detail = "Optimizing...")
            opt_out <- mf_run_optimization(
              model_bundles = model_bundles,
              fit_active = fit_active,
              economics_single = economics_single,
              cont_vars = cont_vars,
              optimize_target = optimize_target,
              volume = volume,
              factor_cost_txt = isolate(loss_mf_saved_factor_costs()),
              use_factor_costs = isTRUE(input$loss_mf_opt_use_factor_costs),
              xnames = fit_active$factors_names,
              blocked_factors = blocked_vars
            )
          }
        )

        if (is.null(opt_out)) return(invisible(NULL))

        if (!isTRUE(opt_out$ok)) {
          loss_mf_opt_result_val(list(ok = FALSE, message = opt_out$message))
          shiny::showNotification(opt_out$message, type = "error", duration = 8)
          return(invisible(NULL))
        }

        loss_mf_confirmation_val(opt_out$confirmation)
        opt_res <- opt_out$result
        if (!is.null(opt_out$blocked_factors) && length(opt_out$blocked_factors) > 0L) {
          opt_res$blocked_factors <- opt_out$blocked_factors
        }
        loss_mf_opt_result_val(opt_res)
        shiny::showNotification("Optimizer finished.", type = "message", duration = 4)
      }, error = function(e) {
        msg <- conditionMessage(e)
        loss_mf_opt_result_val(list(ok = FALSE, message = paste0("Optimization error: ", msg)))
        shiny::showNotification(msg, type = "error", duration = 10)
      })
    }, ignoreInit = TRUE)

    observeEvent(
      list(
        input$loss_mf_target,
        input$loss_mf_dec,
        input$loss_mf_C_l,
        input$loss_mf_C_u,
        input$loss_mf_lsl,
        input$loss_mf_usl,
        input$loss_mf_opt_target,
        input$loss_mf_opt_volume,
        input$loss_mf_opt_blocked,
        input$loss_mf_opt_continuous,
        input$loss_mf_opt_use_factor_costs,
        loss_mf_saved_factor_costs()
      ),
      {
        if (!isTRUE(isolate(loss_mf_opt_running()))) {
          loss_mf_opt_result_val(NULL)
          loss_mf_confirmation_val(NULL)
        }
      },
      ignoreInit = TRUE
    )

    output$loss_mf_dispersion_policy <- renderUI({
      req(mf_loss_tab_active())
      d <- filtered_data()
      req(d)
      dids <- mf_resolved_data_ems()
      req(!is.null(dids), length(dids) >= 1L)
      summaries <- mf_dispersion_policy_summaries()
      resp_names <- names(summaries)
      if (length(resp_names) < 1L) {
        return(NULL)
      }
      lines <- vapply(seq_along(resp_names), function(i) {
        rn <- resp_names[[i]]
        did_i <- dids[[i]]
        metric_lbl <- mf_response_dispersion_metric_label(did_i)
        format_response_dispersion_policy_line(rn, summaries[[rn]], metric_lbl)
      }, character(1))
      build_dispersion_policy_messages_ui(lines)
    })

    output$loss_mf_optimization_checklist <- renderUI({
      d <- filtered_data()
      fid <- mf_resolved_factors_ems()
      req(d, fid, length(fid) >= 2L)
      dids <- mf_resolved_data_ems()
      req(!is.null(dids), length(dids) >= 1L)
      full_ready <- mf_optimization_readiness_full(d, fid, dids)
      optimization_checklist_ui(full_ready)
    })

    output$loss_mf_msg <- renderUI({
      r <- tryCatch(loss_mf_result(), error = function(e) {
        list(ok = FALSE, message = conditionMessage(e), disclaimer = "")
      })
      if (!isTRUE(r$ok)) {
        return(shiny::tags$p(class = "text-danger", r$message))
      }
      shiny::tagList(
        if (nzchar(as.character(r$message))) shiny::tags$p(class = "text-warning", r$message),
        taguchi_loss_disclaimer_ui("multifactor")
      )
    })

    output$loss_mf_opt_msg <- renderUI({
      loss_mf_opt_running()
      r <- loss_mf_opt_result_val()
      if (is.null(r)) {
        return(shiny::tags$p(class = "text-muted", "Click Run Optimizer to search for best/worst theoretical settings."))
      }
      if (isTRUE(r$running)) {
        return(shiny::tags$p(class = "text-info", r$message))
      }
      if (!isTRUE(r$ok)) {
        return(shiny::tags$p(class = "text-danger", ifelse(is.null(r$message), "Optimization failed.", r$message)))
      }
      shiny::tags$p(
        class = "text-muted",
        paste0(
          "Optimizer converged (code ", r$convergence, "). Objective value: ", signif(r$value, 6), ".",
          if (!is.null(r$optimize_target) && identical(r$optimize_target, "total_cost")) {
            paste0(" Optimized on total cost with volume=", signif(as.numeric(r$volume), 6), ".")
          } else {
            ""
          },
          if (!is.null(r$aggregate) && is.list(r$aggregate)) {
            paste0(" Aggregated across ", as.integer(r$aggregate$response_count), " responses.")
          } else {
            ""
          },
          if (!is.null(r$optimization_mode) && identical(r$optimization_mode, "ancova")) {
            " Continuous factors optimized via ANCOVA (fast linear model)."
          } else if (!is.null(r$optimization_mode) && identical(r$optimization_mode, "interp")) {
            " Continuous factors optimized via design-level interpolation."
          } else {
            ""
          },
          if (!is.null(r$blocked_factors) && length(r$blocked_factors) > 0L) {
            paste0(" Blocked factors (marginalized): ", paste(r$blocked_factors, collapse = ", "), ".")
          } else {
            ""
          }
        )
      )
    })

    output$loss_mf_preflight <- renderUI({
      d <- filtered_data()
      fid <- mf_resolved_factors_ems()
      req(d, fid, length(fid) >= 2L)
      dids <- mf_resolved_data_ems()
      req(!is.null(dids), length(dids) >= 1L)
      full_ready <- mf_optimization_readiness_full(d, fid, dids)
      optimization_checklist_ui(full_ready)
    })

    output$loss_mf_opt_actual_values_panel <- renderUI({
      cont <- input$loss_mf_opt_continuous
      if (is.null(cont) || length(cont) < 1L) return(NULL)
      d <- filtered_data()
      req(d)
      model <- tryCatch(multifactor_worker$model_mean_est(), error = function(e) NULL)
      factor_names <- if (!is.null(model)) multifactor_model_factor_names(model) else character(0)
      build_loss_opt_actual_values_ui(ns, cont, d, factor_names, input = input)
    })

    mf_loss_display_context <- function() {
      d <- filtered_data()
      cont <- if (!is.null(input$loss_mf_opt_continuous)) unique(as.character(input$loss_mf_opt_continuous)) else character(0)
      cont <- cont[nzchar(cont)]
      actual_map <- mf_loss_factor_actual_map(d, cont, input)
      cont_levels <- mf_loss_cont_levels_map(d, cont)
      factor_continuous <- stats::setNames(cont %in% names(actual_map), cont)
      list(
        cont_vars = cont,
        actual_map = actual_map,
        cont_levels = cont_levels,
        factor_continuous = factor_continuous
      )
    }

    output$loss_mf_confirmation_tab <- renderDT({
      conf_tbl <- loss_mf_confirmation_val()
      req(conf_tbl)
      R <- if (!is.null(input$loss_mf_dec)) input$loss_mf_dec else 4
      disp <- mf_loss_display_context()
      if (length(disp$actual_map) > 0L) {
        conf_tbl <- multifactor_apply_actual_value_display(
          result = conf_tbl,
          factor_continuous = disp$factor_continuous,
          cont_levels = disp$cont_levels,
          factor_actual_values = disp$actual_map,
          decimals = as.integer(R)
        )
      }
      if ("EstimatedCost" %in% names(conf_tbl)) {
        conf_tbl$EstimatedCost <- round(as.numeric(conf_tbl$EstimatedCost), digits = as.integer(R))
      }
      loss_cols <- grep("^Loss_", names(conf_tbl), value = TRUE)
      for (col in loss_cols) {
        conf_tbl[[col]] <- round(as.numeric(conf_tbl[[col]]), digits = as.integer(R))
      }
      DT::datatable(conf_tbl, options = optimizer_dt_options(), rownames = FALSE)
    })

    mf_optimizer_export_metadata <- function() {
      d <- filtered_data()
      dids <- mf_resolved_data_ems()
      resp_names <- character(0)
      if (!is.null(dids) && !is.null(d) && is.data.frame(d)) {
        resp_names <- vapply(dids, function(di) {
          di <- suppressWarnings(as.integer(di))
          if (is.finite(di) && di >= 1L && di <= ncol(d)) names(d)[di] else as.character(di)
        }, character(1))
      }
      list(
        response_names = resp_names,
        decimals = if (!is.null(input$loss_mf_dec)) input$loss_mf_dec else 4L,
        optimization_target = input$loss_mf_opt_target,
        volume = input$loss_mf_opt_volume
      )
    }

    output$loss_mf_confirmation_export <- downloadHandler(
      filename = function() paste0("loss_mf_confirmation_", Sys.Date(), ".csv"),
      content = function(file) {
        conf_tbl <- shiny::isolate(loss_mf_confirmation_val())
        if (is.null(conf_tbl) || !is.data.frame(conf_tbl) || nrow(conf_tbl) < 1L) {
          utils::write.csv(
            data.frame(Note = "No confirmation settings to export. Run the optimizer first."),
            file,
            row.names = FALSE
          )
          return(invisible(NULL))
        }
        utils::write.csv(conf_tbl, file, row.names = FALSE)
      }
    )
    shiny::outputOptions(output, "loss_mf_confirmation_export", suspendWhenHidden = FALSE)

    output$loss_mf_opt_tab <- renderDT({
      if (!isTRUE(input$loss_mf_opt_show_details)) {
        return(DT::datatable(data.frame(), options = list(dom = "t", paging = FALSE), rownames = FALSE))
      }
      r <- loss_mf_opt_result_val()
      req(r)
      if (!isTRUE(r$ok)) {
        return(DT::datatable(data.frame(Note = ifelse(is.null(r$message), "Optimization failed.", r$message)), options = optimizer_dt_options(), rownames = FALSE))
      }
      R <- if (!is.null(input$loss_mf_dec)) input$loss_mf_dec else 4
      detail_rows <- build_optimizer_detail_rows(r)
      tbl <- data.frame(Metric = detail_rows$metrics, Value = detail_rows$values)
      num_idx <- !(grepl("Factor levels", tbl$Metric, fixed = TRUE) |
        grepl("^Optimization target$", tbl$Metric) |
        grepl("^Continuous optimization mode$", tbl$Metric) |
        grepl("^Response economics \\[", tbl$Metric) |
        grepl("^Dispersion tier \\[", tbl$Metric))
      tbl$Value[num_idx] <- round(as.numeric(tbl$Value[num_idx]), digits = as.integer(R))
      DT::datatable(tbl, options = optimizer_dt_options(), rownames = FALSE)
    })

    output$loss_mf_opt_dist_cards <- renderUI({
      r <- loss_mf_opt_result_val()
      req(r)
      if (!isTRUE(r$ok)) return(NULL)
      R <- if (!is.null(input$loss_mf_dec)) input$loss_mf_dec else 4

      d <- filtered_data()
      dc <- suppressWarnings(as.numeric(active_data_ems()))
      mf_single_response_name <- if (!is.null(d) && is.data.frame(d) && length(dc) >= 1L && is.finite(dc[[1L]]) && dc[[1L]] >= 1L && dc[[1L]] <= ncol(d)) {
        as.character(names(d)[dc[[1L]]])
      } else {
        "response"
      }

      did_all <- mf_resolved_data_ems()
      if (is.null(did_all) || length(did_all) < 1L) did_all <- dc
      econ_disp <- if (length(dc) >= 1L && is.finite(dc[[1L]])) {
        mf_get_economics_for_response(dc[[1L]], mf_single_response_name, d, did_all)
      } else {
        opt_economics_defaults()
      }

      details <- if (!is.null(r$aggregate) && is.list(r$aggregate) && !is.null(r$aggregate$details)) {
        r$aggregate$details
      } else {
        list(list(
          response = mf_single_response_name,
          mu = r$mu,
          sigma = r$sigma,
          target = econ_disp$target,
          lsl = econ_disp$lsl,
          usl = econ_disp$usl,
          expected_loss = r$metrics$expected_loss[1],
          ppm = r$metrics$ppm[1]
        ))
      }
      if (length(details) < 1L) return(NULL)

      disp <- mf_loss_display_context()
      settings_tbl <- build_optimizer_settings_table_ui(
        r$par,
        digits = R,
        display_values = if (length(disp$actual_map) > 0L) {
          format_optimizer_factor_display(
            par = r$par,
            cont_vars = disp$cont_vars,
            cont_levels = disp$cont_levels,
            factor_actual_values = disp$actual_map,
            decimals = as.integer(R)
          )
        } else {
          NULL
        }
      )

      cards <- lapply(seq_along(details), function(i) {
        di <- details[[i]]
        plot_id <- paste0("loss_mf_opt_dist_plot_", i)
        summary_id <- paste0("loss_mf_opt_dist_summary_", i)
        legend_id <- paste0("loss_mf_opt_dist_legend_", i)
        resp_name <- if (!is.null(di$response) && nzchar(as.character(di$response))) as.character(di$response) else paste0("Output ", i)
        ttl <- paste0("Estimated Distribution at Optimum for ", resp_name)
        output[[plot_id]] <- renderPlot({
          build_optimizer_normal_plot(
            mu = di$mu,
            sigma = di$sigma,
            target = di$target,
            lsl = di$lsl,
            usl = di$usl,
            title = ttl
          )
        })
        output[[legend_id]] <- renderUI({
          shiny::tags$p(
            class = "text-muted",
            style = "margin-top: 0.25em;",
            shiny::tags$span(style = "color:#2ca25f;font-weight:600;", "Dashed green"),
            " = Target | ",
            shiny::tags$span(style = "color:#de2d26;font-weight:600;", "Dotted red"),
            " = LSL/USL"
          )
        })
        output[[summary_id]] <- renderUI({
          build_optimizer_summary_table_ui(
            mu = di$mu,
            sigma = di$sigma,
            expected_loss = di$expected_loss,
            ppm = di$ppm,
            volume = if (!is.null(r$volume)) r$volume else 1,
            unit_setting_cost = if (!is.null(r$unit_setting_cost)) r$unit_setting_cost else 0,
            target = di$target,
            lsl = di$lsl,
            usl = di$usl,
            digits = R
          )
        })
        shiny::tagList(
          if (i > 1L) shiny::tags$hr(),
          shiny::plotOutput(outputId = ns(plot_id), height = 260),
          shiny::uiOutput(ns(legend_id)),
          shiny::uiOutput(ns(summary_id))
        )
      })

      shiny::tagList(
        shiny::tags$hr(),
        settings_tbl,
        do.call(shiny::tagList, cards)
      )
    })

    output$loss_mf_opt_bounds_tab <- renderDT({
      if (!isTRUE(input$loss_mf_opt_show_details)) {
        return(DT::datatable(data.frame(), options = list(dom = "t", paging = FALSE), rownames = FALSE))
      }
      r <- loss_mf_opt_result_val()
      req(r)
      if (is.null(r$bounds_used)) return(NULL)
      DT::datatable(r$bounds_used, options = optimizer_dt_options(), rownames = FALSE)
    })

    output$loss_mf_opt_export <- downloadHandler(
      filename = function() paste0("loss_mf_optimizer_report_", Sys.Date(), ".zip"),
      content = function(file) {
        tryCatch({
          r <- shiny::isolate(loss_mf_opt_result_val())
          conf <- shiny::isolate(loss_mf_confirmation_val())
          grids <- shiny::isolate(loss_mf_results_val())
          snapshot <- build_optimizer_report_snapshot(
            opt_result = r,
            confirmation = conf,
            loss_grids = grids,
            metadata = shiny::isolate(mf_optimizer_export_metadata())
          )
          write_optimizer_report_zip(file, snapshot)
        }, error = function(e) {
          write_optimizer_report_error_zip(file, conditionMessage(e))
        })
      }
    )
    shiny::outputOptions(output, "loss_mf_opt_export", suspendWhenHidden = FALSE)

    output$loss_mf_tab <- renderDT({
      loss_mf_calculating()
      rbr <- loss_mf_results_val()
      R <- if (!is.null(input$loss_mf_dec)) input$loss_mf_dec else 4
      if (is.null(rbr)) {
        return(DT::datatable(
          data.frame(Note = "Enter Taguchi economics for all responses, then click Calculate loss grid."),
          options = optimizer_dt_options(),
          rownames = FALSE
        ))
      }
      dids <- mf_resolved_data_ems()
      caption_txt <- NULL
      if (!is.null(dids) && length(dids) >= 2L) {
        tbl <- assemble_multiresponse_loss_top_table(rbr, n_top = 5L)
        failed_notes <- loss_multiresponse_assembly_notes(rbr)
        if (is.null(tbl) || nrow(tbl) < 1L) {
          msgs <- vapply(rbr, function(x) if (!isTRUE(x$ok)) as.character(x$message)[1] else "", character(1))
          msgs <- unique(c(msgs[nzchar(msgs)], failed_notes))
          return(DT::datatable(
            data.frame(Note = if (length(msgs) >= 1L) paste(msgs, collapse = "; ") else "Loss grid could not be assembled."),
            options = optimizer_dt_options(),
            rownames = FALSE
          ))
        }
        if (length(failed_notes) >= 1L) {
          caption_txt <- paste0(
            "Excluded from table: ",
            paste(failed_notes, collapse = " "),
            " Re-commit on the Results tab for each response, then recalculate."
          )
        }
      } else {
        r <- tryCatch(loss_mf_result(), error = function(e) {
          list(ok = FALSE, message = conditionMessage(e), table = NULL)
        })
        if (!isTRUE(r$ok)) {
          return(DT::datatable(
            data.frame(Note = r$message),
            options = optimizer_dt_options(),
            rownames = FALSE
          ))
        }
        tbl <- r$table
        if ("se_mu" %in% names(tbl)) tbl$se_mu <- NULL
        if ("disp_emm" %in% names(tbl) && "disp_pred" %in% names(tbl)) tbl$disp_pred <- NULL
        if ("expected_loss" %in% names(tbl)) {
          tbl$expected_loss <- suppressWarnings(as.numeric(tbl$expected_loss))
          tbl <- tbl[order(tbl$expected_loss, na.last = TRUE), , drop = FALSE]
        }
        if (nrow(tbl) > 5L) tbl <- utils::head(tbl, 5L)
        pretty_names <- c(
          method = "Method",
          mu_pred = "Predicted mean",
          disp_pred = "Dispersion EMM",
          disp_emm = "Dispersion EMM",
          disp_cell = "Cell dispersion",
          disp_effective = "Effective dispersion",
          disp_tier = "Dispersion tier",
          delta_used = "Resolution delta used",
          sigma = "Predicted sigma",
          loss_lower = "Lower-side Taguchi loss",
          loss_upper = "Upper-side Taguchi loss",
          expected_loss = "Total Taguchi loss per unit",
          ppm_lower = "PPM below LSL",
          ppm_upper = "PPM above USL",
          ppm = "Total PPM outside specs"
        )
        for (nm in names(pretty_names)) {
          if (nm %in% names(tbl)) names(tbl)[names(tbl) == nm] <- pretty_names[[nm]]
        }
      }
      num_cols <- names(tbl)[vapply(tbl, is.numeric, logical(1))]
      for (nm in num_cols) {
        tbl[[nm]] <- suppressWarnings(as.numeric(tbl[[nm]]))
        tbl[[nm]] <- round(tbl[[nm]], digits = as.integer(R))
      }
      DT::datatable(
        tbl,
        options = optimizer_dt_options(list(scrollX = TRUE, autoWidth = TRUE)),
        rownames = FALSE,
        class = "cell-border stripe compact",
        caption = caption_txt
      )
    })

    invisible(NULL)
}
