# Discrete (Attribute) MSA server

library(shiny)
library(dplyr)
library(DT)
library(lolcat)
library(shinyWidgets)

create_attribute_msa_server <- function(id, filtered_data, reactive_color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$msa_d_1 <- renderUI({
      data <- filtered_data()
      req(data)
      choices <- seq(1:ncol(data))
      names(choices) <- names(data)
      pickerInput(
        inputId = ns("msa_d_UI1"),
        label = "Select Appraisers",
        multiple = TRUE,
        options = list(`actions-box` = TRUE),
        choices = choices
      )
    })

    output$msa_d_standard_ID <- renderUI({
      data <- filtered_data()
      UI1 <- as.numeric(input$msa_d_UI1)
      req(data, UI1, ncol(data) > 0, nrow(data) > 0)
      need_stand <- input$msa_d_standard
      req(need_stand == TRUE)

      choices <- seq(1:ncol(filtered_data()))
      names(choices) <- names(filtered_data())
      fact_selected <- UI1
      temp <- seq(1:length(choices))
      temp <- temp[-fact_selected]
      choices <- choices[temp]

      pickerInput(
        inputId = ns("msa_d_stand_id"),
        label = "Select Standard",
        choices = choices,
        multiple = FALSE,
        options = list(`actions-box` = TRUE, style = "btn-success")
      )
    })

    previous_data <- reactiveValues(data = NULL)

    observeEvent(filtered_data(), {
      data <- filtered_data()
      if (!identical(data, previous_data$data)) {
        updateCheckboxInput(inputId = "msa_d_internal", value = FALSE)
        previous_data$data <- data

        ids_to_remove <- grep("^d_assessor_", names(input), value = TRUE)
        lapply(ids_to_remove, function(id) {
          removeUI(selector = paste0("#", ns(paste0("container_", id))), immediate = TRUE, session = session)
        })
      }
    })

    observeEvent(c(input$msa_d_UI1, input$msa_d_internal), {
      UI1 <- as.numeric(input$msa_d_UI1)
      data <- filtered_data()
      do_internal <- input$msa_d_internal
      req(UI1, data, ncol(data) > 0, nrow(data) > 0)

      if (!isTruthy(do_internal)) {
        ids_to_remove <- grep("^d_assessor_", names(input), value = TRUE)
        lapply(ids_to_remove, function(id) {
          removeUI(selector = paste0("#", ns(paste0("container_", id))), immediate = TRUE, session = session)
        })
        return()
      }

      choices <- seq_len(ncol(data))
      names(choices) <- names(data)
      choices <- choices[-UI1]

      output$d_repeat_sel <- renderUI({
        dropdown_list <- lapply(UI1, function(i) {
          div(
            id = ns(paste0("container_d_assessor_", i)),
            pickerInput(
              inputId = ns(paste0("d_assessor_", i)),
              label = paste0("Select Repeated Measures for ", names(data)[i]),
              choices = choices,
              multiple = TRUE,
              options = list(`actions-box` = TRUE)
            )
          )
        })
        do.call(tagList, dropdown_list)
      })
    })

    d_reps_values <- reactiveValues(d_reps = NULL)

    observe({
      UI1 <- as.numeric(input$msa_d_UI1)
      num_apps <- length(UI1)
      data <- filtered_data()
      req(data, UI1, num_apps, ncol(data) > 0, nrow(data) > 0)
      app_names <- names(data)[UI1]
      do_internal <- input$msa_d_internal
      req(app_names, UI1, data)
      if (!do_internal) {
        d_reps_values$d_reps <- NULL
        return()
      }
      values <- sapply(UI1, function(i) {
        req(input[[paste0("d_assessor_", i)]])
        as.numeric(input[[paste0("d_assessor_", i)]])
      })

      d_reps_list <- vector(mode = "list", length = num_apps)
      for (i in UI1) {
        if (is.null(input[[paste0("d_assessor_", i)]])) {
          return()
        }
        d_reps_list[[i]] <- input[[paste0("d_assessor_", i)]]
      }
      d_reps_values$d_reps <- d_reps_list
    })

    button_state <- reactiveVal(FALSE)
    observeEvent(input$msa_d_go, {
      button_state(TRUE)
    })

    observeEvent(input$msa_d_go, {
      output$msa_out_d <- renderUI({
        UI1 <- as.numeric(input$msa_d_UI1)
        data <- filtered_data()
        req(UI1, data)
        do_internal <- input$msa_d_internal
        stand <- input$msa_d_standard
        stand_ID <- input$msa_d_stand_id
        type <- input$msa_d_type
        num_apps <- length(UI1)
        reps <- d_reps_values$d_reps

        if (!isTruthy(UI1)) {
          return()
        }
        one_OK <- FALSE
        if (num_apps == 1) {
          if (!((isTruthy(stand_ID) && stand) || isTruthy(input[[paste0("d_assessor_", UI1)]]))) {
            return()
          }
          one_OK <- TRUE
        }
        if (do_internal && !one_OK) {
          reps_per_app <- data.frame(app = UI1, reps_per_app = rep(NA, num_apps))
          for (operator in UI1) {
            reps_per_app$reps_per_app[reps_per_app$app == operator] <- 1 + length(input[[paste0("d_assessor_", operator)]])
          }
          if (is.null(reps) || (is.na(min(reps_per_app$reps_per_app)) || min(reps_per_app$reps_per_app) < 3)) {
            return(HTML("<p style='color:red'>If you are doing repeated measures, you need at least three for each appraiser in order to calculate a mode for the analyisis.</p>"))
          }
        }
        if (stand && !isTruthy(stand_ID)) {
          return(HTML("<p style='color:red'>Please select the standard.</p>"))
        }
        button_test <- isolate(button_state())
        if (!button_test) {
          return(NULL)
        }

        R <- input$deci_msa_d
        conf <- input$conf_msa_d
        data_temp <- data[UI1]
        categories <- sort(unique(unlist(data[UI1])))
        num_cat <- length(categories)
        app_names <- data.frame(app = names(data[UI1]), col = UI1)
        reps <- d_reps_values$d_reps
        if (!is.null(reps)) {
          names(reps)[app_names$col] <- app_names$app
        }

        if (do_internal && !is.null(reps)) {
          internal_test <- 1
          for (i in UI1) {
            if (length(input[[paste0("d_assessor_", i)]]) == 0) {
              internal_test <- internal_test * 0
            }
          }
          internal_test <- as.logical(internal_test)

          if (internal_test) {
            int_tables <- vector(mode = "list", length = length(app_names))
            int_kappa <- vector(mode = "list", length = length(app_names))
            int_mcnemar <- vector(mode = "list", length = length(app_names))
            int_cat_analysis <- list()

            for (app in app_names$app) {
              rep_columns <- as.numeric(c(app_names$col[app_names$app == app], reps[[app]]))
              int_combos <- combn(rep_columns, 2)
              num_compare <- ncol(int_combos)
              all_measures_data <- data[rep_columns]
              int_kappa[[app]] <- do.call(msa.nominal.internalconsistency, as.list(all_measures_data, conf.int = conf))
              if (num_cat > 2) {
                for (j in 1:num_compare) {
                  internal_comparison <- paste0(names(data)[int_combos[1, j]], " v ", names(data)[int_combos[2, j]])
                  data_this <- data.frame(data[int_combos[1, j]], data[int_combos[2, j]])
                  int_tables[[app]][[internal_comparison]] <- table(data[, int_combos[1, j]], data[, int_combos[2, j]])
                  rownames(int_tables[[app]][[internal_comparison]]) <- paste0(names(data)[int_combos[1, j]], " = ", rownames(int_tables[[app]][[j]]))
                  colnames(int_tables[[app]][[internal_comparison]]) <- paste0(names(data)[int_combos[2, j]], " = ", colnames(int_tables[[app]][[j]]))

                  for (k in 1:length(categories)) {
                    merge1 <- data_this[[1]]
                    merge2 <- data_this[[2]]
                    versusothers <- data.frame(merge1, merge2)
                    cat_value <- categories[k]
                    versusothers <- apply(X = versusothers, MARGIN = c(1, 2), FUN = function(x) {
                      if (is.na(x)) {
                        NA
                      } else {
                        if (x == cat_value) {
                          1
                        } else {
                          2
                        }
                      }
                    })
                    int_cat_analysis[[app]][[internal_comparison]][[paste0(cat_value, " v Not")]][["kappa"]] <- msa.nominal.internalconsistency(versusothers[, 1], versusothers[, 2], conf.level = conf)
                  }
                }
              }

              if (num_cat > 2) {
                all_measures_data <- data[rep_columns]
                if (!(num_apps == 1 && length(reps[[app]]) == 1)) {
                  for (k in 1:length(categories)) {
                    cat_value <- categories[k]
                    versusothers <- as.data.frame(apply(X = all_measures_data, MARGIN = c(1, 2), FUN = function(x) {
                      if (is.na(x)) {
                        NA
                      } else {
                        if (x == cat_value) {
                          1
                        } else {
                          2
                        }
                      }
                    }))
                    versusothers$ID <- seq(1:nrow(versusothers))
                    versusothers <- na.omit(versusothers)
                    ind_discrete <- transform.dependent.format.to.independent.format(versusothers[-ncol(versusothers)])
                    ind_discrete$ID <- versusothers$ID
                    cat_label <- paste0(cat_value, " v Not")
                    int_cat_analysis[[app]][["All Measures"]][[cat_label]][["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]] <- cor.cohen.kappa.onesample.1979.fleiss(subject = ind_discrete$ID, rating = ind_discrete$measure, alternative = "greater", conf.level = conf)
                    int_cat_analysis[[app]][["All Measures"]][[cat_label]][["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["n.agree"]] <- sum(na.omit(apply(versusothers[-ncol(versusothers)], 1, function(row) all(row == row[1]))))
                    int_cat_analysis[[app]][["All Measures"]][[cat_label]][["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["n.disagree"]] <- sum(na.omit(apply(versusothers[-ncol(versusothers)], 1, function(row) !all(row == row[1]))))
                    int_cat_analysis[[app]][["All Measures"]][[cat_label]][["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["n"]] <- int_cat_analysis[[app]][["All Measures"]][[cat_label]][["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["n.agree"]] + int_cat_analysis[[app]][["All Measures"]][[cat_label]][["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["n.disagree"]]
                    int_cat_analysis[[app]][["All Measures"]][[cat_label]][["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["p_o"]] <- int_cat_analysis[[app]][["All Measures"]][[cat_label]][["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["n.agree"]] / int_cat_analysis[[app]][["All Measures"]][[cat_label]][["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["n"]]
                    int_cat_analysis[[app]][["All Measures"]][[cat_label]][["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["p_c"]] <- NA
                    int_cat_analysis[[app]][["All Measures"]][[cat_label]][["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["kappa.max"]] <- NA
                    int_cat_analysis[[app]][["All Measures"]][[cat_label]][["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["symmetry"]][["p.value"]] <- NA
                  }
                }

                combo_array <- array(unlist(int_tables), dim = c(nrow(int_tables[[app]][[1]]), ncol(int_tables[[app]][[1]]), length(int_tables[[app]])))
                ave_array <- apply(combo_array, c(1, 2), mean)
                int_kappa[[app]][["Average"]] <- cor.cohen.kappa.onesample.1969.fleiss(ave_array, alternative = "greater", conf.level = conf)

                all_measures_data$ID <- seq(1:nrow(all_measures_data))
                all_measures_data <- na.omit(all_measures_data)
                ind_discrete <- transform.dependent.format.to.independent.format(all_measures_data[-ncol(all_measures_data)])
                ind_discrete$ID <- all_measures_data$ID
                int_kappa[[app]][["Overall"]] <- cor.cohen.kappa.onesample.1979.fleiss(subject = ind_discrete$ID, rating = ind_discrete$measure, alternative = "greater", conf.level = conf)

                n <- nrow(all_measures_data)
                n.agree <- sum(na.omit(apply(all_measures_data[-ncol(all_measures_data)], 1, function(row) all(row == row[1]))))
                n.disagree <- n - n.agree
                p_o <- n.agree / n

                int_kappa[[app]][["Overall"]][["estimate"]][["n.agree"]] <- n.agree
                int_kappa[[app]][["Overall"]][["estimate"]][["n.disagree"]] <- n.disagree
                int_kappa[[app]][["Overall"]][["estimate"]][["n"]] <- n
                int_kappa[[app]][["Overall"]][["estimate"]][["p_o"]] <- p_o
                int_kappa[[app]][["Overall"]][["estimate"]][["p_c"]] <- NA
                int_kappa[[app]][["Overall"]][["estimate"]][["kappa.max"]] <- NA
                int_kappa[[app]][["Overall"]][["symmetry"]][["p.value"]] <- NA
              }
            }
          }
        }

        sufficient_reps <- TRUE
        if (!exists("internal_test")) {
          internal_test <- FALSE
        }
        if (internal_test) {
          reps_per_app <- data.frame(app = UI1, reps_per_app = rep(NA, num_apps))
          for (operator in UI1) {
            reps_per_app$reps_per_app[reps_per_app$app == operator] <- 1 + length(input[[paste0("d_assessor_", operator)]])
          }
          max_reps_done <- max(reps_per_app$reps_per_app)
          if (min(reps_per_app$reps_per_app) < 3) {
            reps_per_app$enough <- FALSE
          } else {
            reps_per_app$enough <- TRUE
          }
          if (all(reps_per_app$enough)) {
            for (app in app_names$app) {
              data_temp[[app]] <- int_kappa[[app]][["mode"]]
            }
            indep_data <- transform.dependent.format.to.independent.format(data_temp)
            indep_data <- cbind(ID = rep(1:nrow(data), num_apps), indep_data)
          } else {
            sufficient_reps <- FALSE
          }
        }

        if (num_apps > 1 && sufficient_reps) {
          if (stand) {
            stand_dat <- data.frame(data[, as.numeric(stand_ID)])
            args <- c(as.list(data_temp), conf.level = conf, standard = stand_dat)
            names(args)[length(names(args))] <- "standard"
            kappa_out <- do.call(msa.nominal.concordance, args)
            
            # Category validity analysis (monolithic lines 28313-28322)
            if (num_cat > 2) {
              for (app in app_names$app) {
                this_app_data <- as.data.frame(c(data_temp[app], data[as.numeric(stand_ID)]))
                for (j in 1:num_cat) {
                  cat_value <- categories[j]
                  versusothers <- apply(X = this_app_data, MARGIN = c(1, 2), FUN = function(x) {
                    if (is.na(x)) {
                      NA
                    } else {
                      if (x == cat_value) {
                        1
                      } else {
                        2
                      }
                    }
                  })
                  kappa_out[["Category Validity Analysis"]][[app]][[paste0(cat_value, " v Not")]] <- msa.nominal.concordance(versusothers[, 1], standard = versusothers[, 2], conf.level = conf)
                }
              }
            }
          } else {
            kappa_out <- do.call(msa.nominal.concordance, c(as.list(data_temp), conf.level = conf))
          }

          app_combos <- combn(app_names$app, 2)
          num_compare <- length(app_combos) / 2
          app_tables <- list()
          cat_analysis <- list()
          for (i in seq_len(num_compare)) {
            app_tables[[paste0(app_combos[1, i], " v ", app_combos[2, i])]] <- table(data_temp[, app_combos[1, i]], data_temp[, app_combos[2, i]])
            
            # Category analysis between appraisers (monolithic lines 28335-28349)
            if (num_cat > 2) {
              for (k in 1:length(categories)) {
                cat_value <- categories[k]
                merge1 <- apply(X = app_tables[[paste0(app_combos[1, i], " v ", app_combos[2, i])]][, -k], 1, sum)
                inter1 <- cbind(app_tables[[paste0(app_combos[1, i], " v ", app_combos[2, i])]][, k], merge1)
                merge2 <- apply(X = inter1[-k, ], 2, sum)
                versusothers <- rbind(inter1[k, ], merge2)
                cat_label <- paste0(cat_value, " v Not")
                cat_analysis[[paste0(app_combos[1, i], " v ", app_combos[2, i])]][["kappa"]][[cat_label]] <- cor.cohen.kappa.onesample.1969.fleiss(observed.frequencies = versusothers)
                test_err2 <- try(proportion.test.mcnemar.simple(b = versusothers[1, 2], c = versusothers[2, 1]), silent = TRUE)
                if (inherits(test_err2, "try-error")) {
                  cat_analysis[[paste0(app_combos[1, i], " v ", app_combos[2, i])]][["sym"]][[cat_label]][["p.value"]] <- 1
                } else {
                  cat_analysis[[paste0(app_combos[1, i], " v ", app_combos[2, i])]][["sym"]][[cat_label]] <- test_err2
                }
              }
            }
          }
          
          # Category analysis for "All Appraisers" (monolithic lines 28353-28376)
          if (num_apps > 2) {
            for (cat_vs in categories) {
              versusothers <- as.data.frame(apply(X = data_temp, MARGIN = c(1, 2), FUN = function(x) {
                if (is.na(x)) {
                  NA
                } else {
                  if (x == cat_vs) {
                    1
                  } else {
                    2
                  }
                }
              }))
              versusothers$ID <- seq(1:nrow(versusothers))
              versusothers <- na.omit(versusothers)
              ind_discrete <- transform.dependent.format.to.independent.format(versusothers[-ncol(versusothers)])
              ind_discrete$ID <- versusothers$ID
              cat_analysis[["All Appraisers"]][["kappa"]][[paste0(cat_vs, " v Not")]] <- cor.cohen.kappa.onesample.1979.fleiss(subject = ind_discrete$ID, rating = ind_discrete$measure, alternative = "greater", conf.level = conf)
              
              n <- nrow(versusothers)
              all_agree <- sum(na.omit(apply(versusothers[-ncol(versusothers)], 1, function(row) all(row == row[1]))))
              all_disagree <- n - all_agree
              p_o <- all_agree / n
              
              cat_analysis[["All Appraisers"]][["kappa"]][[paste0(cat_vs, " v Not")]][["estimate"]][["n"]] <- n
              cat_analysis[["All Appraisers"]][["kappa"]][[paste0(cat_vs, " v Not")]][["estimate"]][["n.agree"]] <- all_agree
              cat_analysis[["All Appraisers"]][["kappa"]][[paste0(cat_vs, " v Not")]][["estimate"]][["n.disagree"]] <- all_disagree
              cat_analysis[["All Appraisers"]][["kappa"]][[paste0(cat_vs, " v Not")]][["estimate"]][["p_o"]] <- p_o
              cat_analysis[["All Appraisers"]][["kappa"]][[paste0(cat_vs, " v Not")]][["estimate"]][["p_c"]] <- NA
              cat_analysis[["All Appraisers"]][["kappa"]][[paste0(cat_vs, " v Not")]][["estimate"]][["kappa.max"]] <- NA
              cat_analysis[["All Appraisers"]][["sym"]][[paste0(cat_vs, " v Not")]][["p.value"]] <- NA
            }
          }
        }
        
        # Calculate Light's G (monolithic lines 28382-28408)
        if (stand) {
          req(stand_ID)
          if (!is.null(stand_ID)) {
            val_data <- data.frame(data_temp, data[[as.numeric(stand_ID)]])
            val_data <- na.omit(val_data)
            stand_name <- names(data)[as.numeric(stand_ID)]
            names(val_data)[ncol(val_data)] <- stand_name
            light_data <- transform.dependent.format.to.independent.format(data = val_data)
            light_data <- cbind(ID = seq(1, nrow(val_data)), light_data)
            
            light_out <- cor.light.g.onesample(subject = light_data$ID, rater = light_data$cell, rating = light_data$measure, rater.standard = stand_name)
            
            last_col <- val_data[[ncol(val_data)]]
            t_m <- sum(apply(as.data.frame(val_data[, -ncol(val_data)]), 2, function(col) col == last_col))
            valid_obs <- sum(rowSums(!is.na(val_data[-ncol(val_data)])))
            p_agree_G <- t_m / valid_obs
            light_out[["estimate"]][["n"]] <- length(last_col)
            light_out[["estimate"]][["valid.obs"]] <- valid_obs
            light_out[["estimate"]][["n.agree"]] <- t_m
            light_out[["estimate"]][["n.disagree"]] <- valid_obs - t_m
            light_out[["estimate"]][["p.agree"]] <- p_agree_G
            light_out[["estimate"]][["p.disagree"]] <- 1 - p_agree_G
          }
        }

        # Form into output (monolithic lines 28411-28420)
        if (type == 1) { # Light's fixed
          output <- ""
          output <- paste0(
            output,
            "<h2>Measurement System Analysis Nominal Data<h2>",
            "<p>Appraisers: ", paste0(app_names$app, collapse = ", "), "</p>",
            "<p>Confidence Level = ", conf * 100, "%<p>",
            "<h3>Appraisers<h3>",
            "<p>", paste0(names(data)[UI1], collapse = ", "), "</p><br/>"
          )
        } else {
          output <- ""
          output <- paste0(
            output,
            "<h2>Measurement System Analysis Nominal Data<h2>",
            "<p>Appraisers: ", paste0(app_names$app, collapse = ", "), "</p>",
            "<p>Confidence Level = ", conf * 100, "%<p>"
          )
        }

        # Initialize output sections
        int_appraisers <- NULL
        conc_appraisers <- NULL
        valid_appraisers <- NULL

        # Internal consistency output (if applicable)
        if (exists("internal_test") && internal_test) {
          int_appraisers <- "<h3>Internal Consistency (Within Appraiser)</h3>"
          for (app in app_names$app) {
            int_appraisers <- paste0(int_appraisers, "<h4>", paste0(app, " - Internal Consistency Analysis</h4>"),
              "<table style='margin-left:auto;margin-right:auto' class='msa-table' style='border-collapse: collapse;'><tr><th>Group</th><th>n</th><th>n(Agree)</th><th>n(Disagree)</th><th>p(Agree)</th><th>p(Disagree)</th><th>p(Chance)</th><th>", withMathJax("$\\kappa_{max}$"), "</th><th>CI(low)</th><th>", withMathJax("$\\kappa$"), "</th><th>CI(high)</th><th>", withMathJax("$p(\\kappa \\leq  0)$"), "</th><th>", withMathJax("$p(Sym)$"), "</th></tr>"
            )
            count <- 1
            for (element in int_kappa[[app]][["comparisons"]]) {
              if (element[["agreement"]][["estimate"]][["kappa"]] < 0) {
                element[["agreement"]][["p.value"]] <- 1 - (element[["agreement"]][["p.value"]] / 2)
              } else {
                element[["agreement"]][["p.value"]] <- element[["agreement"]][["p.value"]] / 2
              }
              int_appraisers <- paste0(int_appraisers,
                "<tr><td>", names(int_kappa[[app]][["comparisons"]])[count], "</td>",
                "<td>", element[["agreement"]][["estimate"]][["n"]], "</td>",
                "<td>", ro(element[["agreement"]][["estimate"]][["n.agree"]], R), "</td>",
                "<td>", ro(element[["agreement"]][["estimate"]][["n.disagree"]], R), "</td>",
                "<td>", ro(element[["agreement"]][["estimate"]][["p_o"]], R), "</td>",
                "<td>", ro(1 - element[["agreement"]][["estimate"]][["p_o"]], R), "</td>",
                "<td>", ro(element[["agreement"]][["estimate"]][["p_c"]], R), "</td>",
                "<td>", ro(element[["agreement"]][["estimate"]][["kappa.max"]], R), "</td>",
                "<td>", ro(element[["agreement"]][["conf.int"]][1], R), "</td>",
                "<td>", ro(element[["agreement"]][["estimate"]][["kappa"]], R), "</td>",
                "<td>", ro(element[["agreement"]][["conf.int"]][2], R), "</td>",
                "<td>", ro(element[["agreement"]][["p.value"]], R), if (element[["agreement"]][["p.value"]] < (1 - conf)) {
                  "*"
                } else {
                  ""
                }, "</td>",
                "<td>", ro(element[["symmetry"]][["p.value"]], R), if (element[["symmetry"]][["p.value"]] < (1 - conf)) {
                  "*"
                } else {
                  ""
                }, "</td></tr>"
              )
              count <- count + 1
            }
            # Add Average and Overall rows if applicable (monolithic lines 28454-28493)
            int_cat <- NULL
            if (exists("max_reps_done") && !(num_apps == 1 && max_reps_done == 2)) {
              int_cat <- paste0("<h4>", app, " Internal Category Analysis</h4>")
              int_appraisers <- paste0(int_appraisers,
                "<tr><td>Average</td>",
                "<td>", int_kappa[[app]][["Average"]][["estimate"]][["n"]], "</td>",
                "<td>", ro(int_kappa[[app]][["Average"]][["estimate"]][["n.agree"]], R), "</td>",
                "<td>", ro(int_kappa[[app]][["Average"]][["estimate"]][["n.disagree"]], R), "</td>",
                "<td>", ro(int_kappa[[app]][["Average"]][["estimate"]][["p_o"]], R), "</td>",
                "<td>", ro(1 - int_kappa[[app]][["Average"]][["estimate"]][["p_o"]], R), "</td>",
                "<td>", ro(int_kappa[[app]][["Average"]][["estimate"]][["p_c"]], R), "</td>",
                "<td>", ro(int_kappa[[app]][["Average"]][["estimate"]][["kappa.max"]], R), "</td>",
                "<td>---</td>",
                "<td>", ro(int_kappa[[app]][["Average"]][["estimate"]][["kappa"]], R), "</td>",
                "<td>---</td>",
                "<td>---</td>",
                "<td>---</td></tr>"
              )
              if (int_kappa[[app]][["Average"]][["estimate"]][["kappa"]] < 0) {
                int_kappa[[app]][["Overall"]][["p.value"]] <- 1 - (int_kappa[[app]][["Overall"]][["p.value"]] / 2)
              } else {
                int_kappa[[app]][["Overall"]][["p.value"]] <- int_kappa[[app]][["Overall"]][["p.value"]] / 2
              }
              int_appraisers <- paste0(int_appraisers,
                "<tr><td>Overall</td>",
                "<td>", int_kappa[[app]][["Overall"]][["estimate"]][["n"]], "</td>",
                "<td>", ro(int_kappa[[app]][["Overall"]][["estimate"]][["n.agree"]], R), "</td>",
                "<td>", ro(int_kappa[[app]][["Overall"]][["estimate"]][["n.disagree"]], R), "</td>",
                "<td>", ro(int_kappa[[app]][["Overall"]][["estimate"]][["p_o"]], R), "</td>",
                "<td>", ro(1 - int_kappa[[app]][["Overall"]][["estimate"]][["p_o"]], R), "</td>",
                "<td>", ro(int_kappa[[app]][["Overall"]][["estimate"]][["p_c"]], R), "</td>",
                "<td>", ro(int_kappa[[app]][["Overall"]][["estimate"]][["kappa.max"]], R), "</td>",
                "<td>", ro(int_kappa[[app]][["Overall"]][["conf.int"]][1], R), "</td>",
                "<td>", ro(int_kappa[[app]][["Average"]][["estimate"]][["kappa"]], R), "</td>",
                "<td>", ro(int_kappa[[app]][["Overall"]][["conf.int"]][2], R), "</td>",
                "<td>", ro(int_kappa[[app]][["Overall"]][["p.value"]], R), if (int_kappa[[app]][["Overall"]][["p.value"]] < (1 - conf)) {
                  "*"
                } else {
                  ""
                }, "</td>",
                "<td>---</td></tr>"
              )
            }
            
            # Internal category analysis table (monolithic lines 28497-28554)
            if (num_cat > 2) {
              if (is.null(int_cat)) {
                int_cat <- paste0("<h4>", app, " Internal Category Analysis</h4>")
              }
              int_cat <- paste0(int_cat, "<table style='margin-left:auto;margin-right:auto' class='msa-table' style='border-collapse: collapse;'><tr><th>Group</th><th>n</th><th>n(Agree)</th><th>n(Disagree)</th><th>p(Agree)</th><th>p(Disagree)</th><th>p(Chance)</th><th>", withMathJax("$\\kappa_{max}$"), "</th><th>CI(low)</th><th>", withMathJax("$\\kappa$"), "</th><th>CI(high)</th><th>", withMathJax("$p(\\kappa \\leq  0)$"), "</th><th>", withMathJax("$p(Sym)$"), "</th></tr>")
              measure <- 1
              for (measure_compare in int_cat_analysis[[app]]) {
                int_cat <- paste0(int_cat, "<tr><th style='text-align: left;  font-size: small;' colspan='13'>Internal Category Analysis:", names(int_cat_analysis[[app]])[measure], "</th></tr>")
                category <- 1
                
                for (element in measure_compare) {
                  if (names(int_cat_analysis[[app]])[measure] != "All Measures") {
                    if (element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["kappa"]] < 0) {
                      element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["p.value"]] <- 1 - (element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["p.value"]] / 2)
                    } else {
                      element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["p.value"]] <- element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["p.value"]] / 2
                    }
                    int_cat <- paste0(int_cat,
                      "<tr><td>", names(int_cat_analysis[[app]][[measure]])[category], "</td>",
                      "<td>", element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["n"]], "</td>",
                      "<td>", element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["n.agree"]], "</td>",
                      "<td>", element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["n.disagree"]], "</td>",
                      "<td>", ro(element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["p_o"]], R), "</td>",
                      "<td>", ro(1 - element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["p_o"]], R), "</td>",
                      "<td>", ro(element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["p_c"]], R), "</td>",
                      "<td>", ro(element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["kappa.max"]], R), "</td>",
                      "<td>", ro(element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["conf.int"]][1], R), "</td>",
                      "<td>", ro(element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["kappa"]], R), "</td>",
                      "<td>", ro(element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["conf.int"]][2], R), "</td>",
                      "<td>", ro(element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["p.value"]], R), if (element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["p.value"]] < (1 - conf)) {
                        "*"
                      } else {
                        ""
                      }, "</td>",
                      "<td>", ro(element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["symmetry"]][["p.value"]], R), if (element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["symmetry"]][["p.value"]] < (1 - conf)) {
                        "*"
                      } else {
                        ""
                      }, "</td></tr>"
                    )
                  } else {
                    if (element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["kappa"]] < 0) {
                      element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["p.value"]] <- 1 - (element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["p.value"]] / 2)
                    } else {
                      element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["p.value"]] <- element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["p.value"]] / 2
                    }
                    int_cat <- paste0(int_cat,
                      "<tr><td>", names(int_cat_analysis[[app]][[measure]])[category], "</td>",
                      "<td>", element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["n"]], "</td>",
                      "<td>", element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["n.agree"]], "</td>",
                      "<td>", element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["n.disagree"]], "</td>",
                      "<td>", ro(element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["p_o"]], R), "</td>",
                      "<td>", ro(1 - element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["p_o"]], R), "</td>",
                      "<td>", ro(element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["p_c"]], R), "</td>",
                      "<td>", ro(element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["kappa.max"]], R), "</td>",
                      "<td>", ro(element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["conf.int"]][1], R), "</td>",
                      "<td>", ro(element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["estimate"]][["kappa"]], R), "</td>",
                      "<td>", ro(element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["conf.int"]][2], R), "</td>",
                      "<td>", ro(element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["p.value"]], R), if (element[["kappa"]][["comparisons"]][["versusothers[, 1] v versusothers[, 2]"]][["agreement"]][["p.value"]] < (1 - conf)) {
                        "*"
                      } else {
                        ""
                      }, "</td>",
                      "<td>---</td></tr>"
                    )
                  }
                  
                  category <- category + 1
                }
                
                measure <- measure + 1
              }
              int_cat <- paste0(int_cat, "</table>")
            }
            
            int_appraisers <- paste0(int_appraisers, "</table>")
            int_appraisers <- paste0(int_appraisers, int_cat)
          }
        }

        if (exists("kappa_out") && num_apps > 1 && sufficient_reps) {
          conc_appraisers <- paste0(
            "<h3>Concordance Analysis Between Appraisers</h3>",
            "<table style='margin-left:auto;margin-right:auto' class='msa-table' style='border-collapse: collapse;'><tr><th>Group</th><th>n</th><th>n(Agree)</th><th>n(Disagree)</th><th>p(Agree)</th><th>p(Disagree)</th><th>p(Chance)</th><th>", withMathJax("$\\kappa_{max}$"), "</th><th>CI(low)</th><th>", withMathJax("$\\kappa$"), "</th><th>CI(high)</th><th>", withMathJax("$p(\\kappa \\leq  0)$"), "</th><th>", withMathJax("$p(Sym)$"), "</th></tr>"
          )
          count <- 1
          for (element in kappa_out[["comp.operators"]]) {
            if (element[["agreement"]][["estimate"]][["kappa"]] < 0) {
              element[["agreement"]][["p.value"]] <- 1 - (element[["agreement"]][["p.value"]] / 2)
            } else {
              element[["agreement"]][["p.value"]] <- element[["agreement"]][["p.value"]] / 2
            }
            conc_appraisers <- paste0(conc_appraisers,
              "<tr><td>", names(kappa_out[["comp.operators"]])[count], "</td>",
              "<td>", element[["agreement"]][["estimate"]][["n"]], "</td>",
              "<td>", element[["agreement"]][["estimate"]][["n.agree"]], "</td>",
              "<td>", element[["agreement"]][["estimate"]][["n.disagree"]], "</td>",
              "<td>", ro(element[["agreement"]][["estimate"]][["p_o"]], R), "</td>",
              "<td>", ro(1 - element[["agreement"]][["estimate"]][["p_o"]], R), "</td>",
              "<td>", ro(element[["agreement"]][["estimate"]][["p_c"]], R), "</td>",
              "<td>", ro(element[["agreement"]][["estimate"]][["kappa.max"]], R), "</td>",
              "<td>", ro(element[["agreement"]][["conf.int"]][1], R), "</td>",
              "<td>", ro(element[["agreement"]][["estimate"]][["kappa"]], R), "</td>",
              "<td>", ro(element[["agreement"]][["conf.int"]][2], R), "</td>",
              "<td>", ro(element[["agreement"]][["p.value"]], R), if (element[["agreement"]][["p.value"]] < (1 - conf)) {
                "*"
              } else {
                ""
              }, "</td>",
              "<td>", ro(element[["symmetry"]][["p.value"]], R), if (element[["symmetry"]][["p.value"]] < (1 - conf)) {
                "*"
              } else {
                ""
              }, "</td></tr>"
            )
            count <- count + 1
          }
          # Add Average and Overall rows when num_apps > 2 (monolithic lines 28598-28639)
          if (num_apps > 2) {
            combo_array <- array(unlist(app_tables), dim = c(nrow(app_tables[[1]]), ncol(app_tables[[1]]), length(app_tables)))
            ave_array <- apply(combo_array, c(1, 2), mean)
            ave_kappa <- cor.cohen.kappa.onesample.1969.fleiss(ave_array, alternative = "greater", conf.level = conf)
            overall_kappa <- kappam.light(data_temp)
            all_agree <- sum(na.omit(apply(data_temp, 1, function(row) all(row == row[1]))))
            all_disagree <- overall_kappa[["subjects"]] - all_agree
            # Do Fleiss 1979 to get overall SE
            ind_discrete <- transform.dependent.format.to.independent.format(data_temp)
            ind_discrete$ID <- seq(1:nrow(data_temp))
            fliess <- cor.cohen.kappa.onesample.1979.fleiss(subject = ind_discrete$ID, rating = ind_discrete$measure, alternative = "greater", conf.level = conf)
            se_overall_kappa <- fliess[["estimate"]][["se.kappa"]]
            overall_p <- pnorm(q = overall_kappa[["value"]] / se_overall_kappa, lower.tail = FALSE)
            conc_appraisers <- paste0(conc_appraisers,
              "<tr><td>Average</td><td>", ave_kappa[["estimate"]][["n"]], "</td>",
              "<td>", ro(ave_kappa[["estimate"]][["n.agree"]], R), "</td>",
              "<td>", ro(ave_kappa[["estimate"]][["n.disagree"]], R), "</td>",
              "<td>", ro(ave_kappa[["estimate"]][["p_o"]], R), "</td>",
              "<td>", ro(1 - ave_kappa[["estimate"]][["p_o"]], R), "</td>",
              "<td>", ro(ave_kappa[["estimate"]][["p_c"]], R), "</td>",
              "<td>", ro(ave_kappa[["estimate"]][["kappa.max"]], R), "</td>",
              "<td>---</td>",
              "<td>", ro(ave_kappa[["estimate"]][["kappa"]], R), "</td>",
              "<td>---</td>",
              "<td>---</td>",
              "<td></td></tr>",
              "<tr><td>Overall</td><td>", overall_kappa[["subjects"]], "</td>",
              "<td>", all_agree, "</td>",
              "<td>", all_disagree, "</td>",
              "<td>", ro(all_agree / overall_kappa[["subjects"]], R), "</td>",
              "<td>", ro(all_disagree / overall_kappa[["subjects"]], R), "</td>",
              "<td>", ro(overall_kappa[["chanceP"]], R), "</td>",
              "<td>---</td>",
              "<td>", ro(overall_kappa[["value"]] + qnorm((1 - conf) / 2, lower.tail = TRUE) * se_overall_kappa, R), "</td>",
              "<td>", ro(overall_kappa[["value"]], R), "</td>",
              "<td>", ro(overall_kappa[["value"]] + qnorm((1 - conf) / 2, lower.tail = FALSE) * se_overall_kappa, R), "</td>",
              "<td>", ro(overall_p, R), if (overall_p < (1 - conf)) {
                "*"
              } else {
                ""
              }, "</td>",
              "<td>---</td></tr>"
            )
          }
          
          conc_appraisers <- paste0(conc_appraisers, "</table>")
          
          # Category Analysis Between Appraisers (monolithic lines 28708-28746)
          if (num_cat > 2) {
            conc_appraisers <- paste0(conc_appraisers,
              "<h4>Category Analysis Between Appraisers</h4>",
              "<table style='margin-left:auto;margin-right:auto' class='msa-table' style='border-collapse: collapse;'><tr><th>Comparison</th><th>n</th><th>n(Agree)</th><th>n(Disagree)</th><th>p(Agree)</th><th>p(Disagree)</th><th>p(Chance)</th><th>", withMathJax("$\\kappa_{max}$"), "</th><th>CI(low)</th><th>", withMathJax("$\\kappa$"), "</th><th>CI(high)</th><th>", withMathJax("$p(\\kappa \\leq  0)$"), "</th><th>", withMathJax("$p(Sym)$"), "</th></tr>"
            )
            count <- 1
            for (app_comp in cat_analysis) {
              if (names(cat_analysis)[count] != "All Appraisers") {
                conc_appraisers <- paste0(conc_appraisers,
                  "<tr><th style='text-align: left; font-size: small;' colspan='13'>Category Analysis: ", names(cat_analysis)[count], "</th></tr>"
                )
                count2 <- 1
                for (element in app_comp[["kappa"]]) {
                  if (element[["estimate"]][["kappa"]] < 0) {
                    element[["p.value"]] <- 1 - (element[["p.value"]] / 2)
                  } else {
                    element[["p.value"]] <- element[["p.value"]] / 2
                  }
                  conc_appraisers <- paste0(conc_appraisers,
                    "<tr><td>", names(app_comp[["kappa"]])[count2], "</td>",
                    "<td>", element[["estimate"]][["n"]], "</td>",
                    "<td>", element[["estimate"]][["n.agree"]], "</td>",
                    "<td>", element[["estimate"]][["n.disagree"]], "</td>",
                    "<td>", ro(element[["estimate"]][["p_o"]], R), "</td>",
                    "<td>", ro(1 - element[["estimate"]][["p_o"]], R), "</td>",
                    "<td>", ro(element[["estimate"]][["p_c"]], R), "</td>",
                    "<td>", ro(element[["estimate"]][["kappa.max"]], R), "</td>",
                    "<td>", ro(element[["conf.int"]][1], R), "</td>",
                    "<td>", ro(element[["estimate"]][["kappa"]], R), "</td>",
                    "<td>", ro(element[["conf.int"]][2], R), "</td>",
                    "<td>", ro(element[["p.value"]], R), if (element[["p.value"]] < (1 - conf)) {
                      "*"
                    }, "</td>",
                    "<td>", ro(app_comp[["sym"]][[names(app_comp[["kappa"]])[count2]]][["p.value"]], R), if (is.na(app_comp[["sym"]][[names(app_comp[["kappa"]])[count2]]][["p.value"]])) {
                        "---"
                      } else {
                        if (app_comp[["sym"]][[names(app_comp[["kappa"]])[count2]]][["p.value"]] < (1 - conf)) {
                          "*"
                        } else {
                          ""
                        }
                      }, "</td></tr>"
                  )
                  count2 <- count2 + 1
                }
              }
              count <- count + 1
            }
            conc_appraisers <- paste0(conc_appraisers, "</table>")
          }
        } else {
          conc_appraisers <- NULL
        }
        
        # Validity Analysis output (monolithic lines 28643-28704)
        if (stand && exists("kappa_out") && num_apps > 1 && sufficient_reps) {
          valid_appraisers <- paste0(
            "<h3>Validity Analysis (Concordance with Standard)</h3>"
          )
          for (app in app_names$app) {
            valid_appraisers <- paste0(valid_appraisers, "<h4>", app, " - Validity Analysis</h4>",
              "<table style='margin-left:auto;margin-right:auto' class='msa-table' style='border-collapse: collapse;'><tr><th>Group</th><th>n</th><th>n(Agree)</th><th>n(Disagree)</th><th>p(Agree)</th><th>p(Disagree)</th><th>p(Chance)</th><th>", withMathJax("$\\kappa_{max}$"), "</th><th>CI(low)</th><th>", withMathJax("$\\kappa$"), "</th><th>CI(high)</th><th>", withMathJax("$p(\\kappa \\leq  0)$"), "</th><th>", withMathJax("$p(Sym)$"), "</th></tr>"
            )
            this_app <- paste0(app, " v Standard")
            if (kappa_out[["comp.standard"]][[this_app]][["agreement"]][["estimate"]][["kappa"]] < 0) {
              kappa_out[["comp.standard"]][[this_app]][["agreement"]][["p.value"]] <- 1 - (kappa_out[["comp.standard"]][[this_app]][["agreement"]][["p.value"]] / 2)
            } else {
              kappa_out[["comp.standard"]][[this_app]][["agreement"]][["p.value"]] <- kappa_out[["comp.standard"]][[this_app]][["agreement"]][["p.value"]] / 2
            }
            valid_appraisers <- paste0(valid_appraisers, "<tr><td>", this_app, "</td>",
              "<td>", kappa_out[["comp.standard"]][[this_app]][["agreement"]][["estimate"]][["n"]], "</td>",
              "<td>", kappa_out[["comp.standard"]][[this_app]][["agreement"]][["estimate"]][["n.agree"]], "</td>",
              "<td>", kappa_out[["comp.standard"]][[this_app]][["agreement"]][["estimate"]][["n.disagree"]], "</td>",
              "<td>", ro(kappa_out[["comp.standard"]][[this_app]][["agreement"]][["estimate"]][["p_o"]], R), "</td>",
              "<td>", ro(1 - kappa_out[["comp.standard"]][[this_app]][["agreement"]][["estimate"]][["p_o"]], R), "</td>",
              "<td>", ro(kappa_out[["comp.standard"]][[this_app]][["agreement"]][["estimate"]][["p_c"]], R), "</td>",
              "<td>", ro(kappa_out[["comp.standard"]][[this_app]][["agreement"]][["estimate"]][["kappa.max"]], R), "</td>",
              "<td>", ro(kappa_out[["comp.standard"]][[this_app]][["agreement"]][["conf.int"]][1], R), "</td>",
              "<td>", ro(kappa_out[["comp.standard"]][[this_app]][["agreement"]][["estimate"]][["kappa"]], R), "</td>",
              "<td>", ro(kappa_out[["comp.standard"]][[this_app]][["agreement"]][["conf.int"]][2], R), "</td>",
              "<td>", ro(kappa_out[["comp.standard"]][[this_app]][["agreement"]][["p.value"]], R), if (kappa_out[["comp.standard"]][[this_app]][["agreement"]][["p.value"]] < (1 - conf)) {
                "*"
              } else {
                ""
              }, "</td>",
              "<td>", ro(kappa_out[["comp.standard"]][[this_app]][["symmetry"]][["p.value"]], R), if (kappa_out[["comp.standard"]][[this_app]][["symmetry"]][["p.value"]] < (1 - conf)) {
                "*"
              } else {
                ""
              }, "</td></tr>"
            )
            
            if (num_cat > 2) {
              valid_appraisers <- paste0(valid_appraisers,
                "<tr><th style='text-align: left; font-size: small;' colspan='13'>Category Analysis: ", this_app, "</th></tr>"
              )
              count <- 1
              for (element in kappa_out[["Category Validity Analysis"]][[app]]) {
                if (element[["comp.standard"]][["versusothers[, 1] v Standard"]][["agreement"]][["estimate"]][["kappa"]] < 1) {
                  element[["comp.standard"]][["versusothers[, 1] v Standard"]][["agreement"]][["p.value"]] <- 1 - (element[["comp.standard"]][["versusothers[, 1] v Standard"]][["agreement"]][["p.value"]] / 2)
                } else {
                  element[["comp.standard"]][["versusothers[, 1] v Standard"]][["agreement"]][["p.value"]] <- element[["comp.standard"]][["versusothers[, 1] v Standard"]][["agreement"]][["p.value"]] / 2
                }
                valid_appraisers <- paste0(valid_appraisers, "<tr><td>", names(kappa_out[["Category Validity Analysis"]][[app]])[count], "</td>",
                  "<td>", element[["comp.standard"]][["versusothers[, 1] v Standard"]][["agreement"]][["estimate"]][["n"]], "</td>",
                  "<td>", element[["comp.standard"]][["versusothers[, 1] v Standard"]][["agreement"]][["estimate"]][["n.agree"]], "</td>",
                  "<td>", element[["comp.standard"]][["versusothers[, 1] v Standard"]][["agreement"]][["estimate"]][["n.disagree"]], "</td>",
                  "<td>", ro(element[["comp.standard"]][["versusothers[, 1] v Standard"]][["agreement"]][["estimate"]][["p_o"]], R), "</td>",
                  "<td>", ro(1 - element[["comp.standard"]][["versusothers[, 1] v Standard"]][["agreement"]][["estimate"]][["p_o"]], R), "</td>",
                  "<td>", ro(element[["comp.standard"]][["versusothers[, 1] v Standard"]][["agreement"]][["estimate"]][["p_c"]], R), "</td>",
                  "<td>", ro(element[["comp.standard"]][["versusothers[, 1] v Standard"]][["agreement"]][["estimate"]][["kappa.max"]], R), "</td>",
                  "<td>", ro(element[["comp.standard"]][["versusothers[, 1] v Standard"]][["agreement"]][["conf.int"]][1], R), "</td>",
                  "<td>", ro(element[["comp.standard"]][["versusothers[, 1] v Standard"]][["agreement"]][["estimate"]][["kappa"]], R), "</td>",
                  "<td>", ro(element[["comp.standard"]][["versusothers[, 1] v Standard"]][["agreement"]][["conf.int"]][2], R), "</td>",
                  "<td>", ro(element[["comp.standard"]][["versusothers[, 1] v Standard"]][["agreement"]][["p.value"]] / 2, R), if (element[["comp.standard"]][["versusothers[, 1] v Standard"]][["agreement"]][["p.value"]] / 2 < (1 - conf)) {
                    "*"
                  } else {
                    ""
                  }, "</td>",
                  "<td>", ro(element[["comp.standard"]][["versusothers[, 1] v Standard"]][["symmetry"]][["p.value"]], R), if (element[["comp.standard"]][["versusothers[, 1] v Standard"]][["symmetry"]][["p.value"]] < (1 - conf)) {
                    "*"
                  } else {
                    ""
                  }, "</td></tr>"
                )
                count <- count + 1
              }
            }
            valid_appraisers <- paste0(valid_appraisers, "</table>")
          }
          
          # Overall System Validity table (monolithic lines 28751-28765)
          if (stand && exists("light_out") && exists("p_agree_G")) {
            valid_appraisers <- paste0(valid_appraisers,
              "<br/><h3>Overall System Validity</h3>",
              "<table style='margin-left:auto;margin-right:auto' class='msa-table' style='border-collapse: collapse;'><tr><th>n</th><th>p(Agree)</th><th>p(Disagree)</th><th>G</th><th>p(G)</th></tr>",
              "<tr><td>", nrow(data), "</td>",
              "<td>", ro(p_agree_G, R), "</td>",
              "<td>", ro(1 - p_agree_G, R), "</td>",
              "<td>", ro(light_out[["estimate"]][["G"]], R), "</td>",
              "<td>", ro(light_out[["p.value"]], R), if (light_out[["p.value"]] < conf) {
                "*"
              }, "</td>",
              "</tr></table>"
            )
          }
        } else {
          valid_appraisers <- NULL
        }

        # Add CSS styling for tables (monolithic lines 28776-28781)
        tab_style <- HTML("<style>
          .msa-table tr:nth-child(even) { background-color: #f2f2f2; }
          .msa-table td:nth-child(even) { background-color: rgba(173, 216, 230, 0.5); }
          .msa-table td { text-align: center; }
          .msa-table th { text-align: center; }
        </style>")
        
        # Combine output sections (monolithic lines 28783-28786)
        if (exists("internal_test") && internal_test) {
          output <- int_appraisers
        } else {
          int_appraisers <- NULL
        }
        # When internal_test is false, output contains the header but isn't used in combination
        # Prepend header to first available section to ensure it's included
        if (!exists("internal_test") || !internal_test) {
          if (!is.null(conc_appraisers)) {
            conc_appraisers <- paste0(output, conc_appraisers)
          } else if (!is.null(valid_appraisers)) {
            valid_appraisers <- paste0(output, valid_appraisers)
          } else {
            # If neither exists, use output (which has header) as int_appraisers
            int_appraisers <- output
          }
        }
        output <- HTML(c(tab_style, int_appraisers, conc_appraisers, valid_appraisers))
        button_state(FALSE)
        output
      })
    })
  })
}
