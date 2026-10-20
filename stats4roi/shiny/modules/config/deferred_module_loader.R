# Deferred module loading for stats4ROI
# Sources feature modules on first tab visit (or via idle prefetch) and registers
# module servers once. Literal source("...") paths below keep identify_deployment_files.R working.

.deferred_state <- new.env(parent = emptyenv())

reset_deferred_session_state <- function() {
  # Per-session flags (must reset on each new Shiny session).
  # sourced_paths stay cached across sessions in the same R process.
  .deferred_state$servers_started <- character()
  .deferred_state$servers_starting <- character()
  .deferred_state$ui_ready <- character()
  .deferred_state$activating <- character()
  .deferred_state$prefetch_index <- 1L
  .deferred_state$prefetch_started <- FALSE
  invisible(TRUE)
}

# Initialize session flags; source cache starts empty for the R process.
if (is.null(.deferred_state$sourced_paths)) {
  .deferred_state$sourced_paths <- character()
}
reset_deferred_session_state()

deferred_loading_ui <- function(label) {
  tags$div(
    class = "stats4roi-deferred-loading",
    style = "padding: 2.5rem 1rem; text-align: center;",
    tags$div(
      style = paste(
        "width: 2.5rem; height: 2.5rem; margin: 0 auto 1rem;",
        "border: 0.3rem solid rgba(13, 110, 253, 0.2);",
        "border-top-color: #0d6efd; border-radius: 50%;",
        "animation: stats4roi-spin 0.85s linear infinite;"
      )
    ),
    tags$p(
      style = "margin: 0; font-size: 1.05rem; font-weight: 600; color: #212529;",
      paste0("Loading ", label, "...")
    )
  )
}

unwrap_tab_panel_ui <- function(ui) {
  if (inherits(ui, "shiny.tag")) {
    cls <- ui$attribs$class
    if (!is.null(cls) && grepl("tab-pane", paste(cls, collapse = " "), fixed = FALSE)) {
      return(ui$children)
    }
  }
  ui
}

ensure_sourced_path <- function(path) {
  if (path %in% .deferred_state$sourced_paths) {
    return(invisible(FALSE))
  }
  if (!file.exists(path)) {
    stop("Deferred module file not found: ", path, call. = FALSE)
  }
  source(path, local = FALSE)
  .deferred_state$sourced_paths <- c(.deferred_state$sourced_paths, path)
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Explicit source() list for deployment file discovery (identify_deployment_files.R).
# These calls are never executed; the live loader uses ensure_sourced_path().
# ---------------------------------------------------------------------------
deferred_deployment_source_manifest <- function() {
  if (isTRUE(getOption("stats4roi.run_deferred_manifest", FALSE))) {
    source("modules/distributions/base_distribution_module.R")
    source("modules/distributions/binomial_module.R")
    source("modules/distributions/normal_module.R")
    source("modules/distributions/poisson_module.R")
    source("modules/distributions/hypergeometric_module.R")
    source("modules/distributions/geometric_module.R")
    source("modules/distributions/exponential_module.R")
    source("modules/distributions/weibull_module.R")
    source("modules/distributions/f_distribution_module.R")
    source("modules/distributions/critical_values_module.R")
    source("modules/distributions/distribution_testing/distribution_testing_module.R")
    source("modules/statistical/sample_size_power/sample_size_power_module.R")
    source("modules/statistical/eda/eda_module.R")
    source("modules/statistical/one_two_sample_tests/one_two_sample_tests_module.R")
    source("modules/statistical/correlation_association/correlation_association_module.R")
    source("modules/statistical/autocorrelation/autocorrelation_module.R")
    source("modules/statistical/spc/spc_module.R")
    source("modules/statistical/msa/msa_module.R")
    source("modules/statistical/crosstabs/crosstabs_module.R")
    source("modules/statistical/anova/anova_module.R")
    source("modules/statistical/doe_orthogonal/doe_orthogonal_module.R")
    source("modules/reliability/reliability_calculator_module.R")
    source("modules/reliability/growth_analysis_module.R")
    source("modules/reliability/weibull_analysis_module.R")
    source("modules/learning/simulators/clt_simulator_module.R")
    source("modules/learning/simulators/power_simulator_module.R")
    source("modules/learning/simulators/anova_simulator_module.R")
  }
  invisible(NULL)
}

get_deferred_registry <- function() {
  list(
    clt_sim = list(
      label = "Central Limit Theorem",
      tab_values = "clt_sim",
      sources = "modules/learning/simulators/clt_simulator_module.R",
      output_ids = c(clt_sim = "deferred_ui_clt_sim"),
      ui_builders = list(clt_sim = function() create_clt_simulator_ui("clt_sim")),
      server_key = "clt_sim",
      start_server = function(deps) {
        create_clt_simulator_server("clt_sim", deps$color_palette)
      }
    ),
    power_sim = list(
      label = "Power & Sample Size simulator",
      tab_values = "power_sim",
      sources = "modules/learning/simulators/power_simulator_module.R",
      output_ids = c(power_sim = "deferred_ui_power_sim"),
      ui_builders = list(power_sim = function() create_power_simulator_ui("power_sim")),
      server_key = "power_sim",
      start_server = function(deps) {
        create_power_simulator_server("power_sim", deps$color_palette)
      }
    ),
    anova_sim = list(
      label = "How ANOVA Works",
      tab_values = "anova_sim",
      sources = "modules/learning/simulators/anova_simulator_module.R",
      output_ids = c(anova_sim = "deferred_ui_anova_sim"),
      ui_builders = list(anova_sim = function() create_anova_simulator_ui("anova_sim")),
      server_key = "anova_sim",
      start_server = function(deps) {
        create_anova_simulator_server("anova_sim", deps$color_palette)
      }
    ),
    binomial = list(
      label = "Binomial",
      tab_values = "binomial",
      sources = c(
        "modules/distributions/base_distribution_module.R",
        "modules/distributions/binomial_module.R"
      ),
      output_ids = c(binomial = "deferred_ui_binomial"),
      ui_builders = list(binomial = function() create_binomial_ui("binomial")),
      server_key = "binomial",
      start_server = function(deps) {
        create_binomial_server("binomial", deps$color_palette)
      }
    ),
    normal = list(
      label = "Normal",
      tab_values = "normal",
      sources = c(
        "modules/distributions/base_distribution_module.R",
        "modules/distributions/normal_module.R"
      ),
      output_ids = c(normal = "deferred_ui_normal"),
      ui_builders = list(normal = function() create_normal_ui("normal")),
      server_key = "normal",
      start_server = function(deps) {
        create_normal_server("normal", deps$color_palette)
      }
    ),
    poisson = list(
      label = "Poisson",
      tab_values = "poisson",
      sources = c(
        "modules/distributions/base_distribution_module.R",
        "modules/distributions/poisson_module.R"
      ),
      output_ids = c(poisson = "deferred_ui_poisson"),
      ui_builders = list(poisson = function() create_poisson_ui("poisson")),
      server_key = "poisson",
      start_server = function(deps) {
        create_poisson_server("poisson", deps$color_palette)
      }
    ),
    hypergeometric = list(
      label = "Hypergeometric",
      tab_values = "hypergeometric",
      sources = c(
        "modules/distributions/base_distribution_module.R",
        "modules/distributions/hypergeometric_module.R"
      ),
      output_ids = c(hypergeometric = "deferred_ui_hypergeometric"),
      ui_builders = list(hypergeometric = function() create_hypergeometric_ui("hypergeometric")),
      server_key = "hypergeometric",
      start_server = function(deps) {
        create_hypergeometric_server("hypergeometric", deps$color_palette)
      }
    ),
    geometric = list(
      label = "Geometric",
      tab_values = "geometric",
      sources = c(
        "modules/distributions/base_distribution_module.R",
        "modules/distributions/geometric_module.R"
      ),
      output_ids = c(geometric = "deferred_ui_geometric"),
      ui_builders = list(geometric = function() create_geometric_ui("geometric")),
      server_key = "geometric",
      start_server = function(deps) {
        create_geometric_server("geometric", deps$color_palette)
      }
    ),
    exponential = list(
      label = "Exponential",
      tab_values = "exponential",
      sources = c(
        "modules/distributions/base_distribution_module.R",
        "modules/distributions/exponential_module.R"
      ),
      output_ids = c(exponential = "deferred_ui_exponential"),
      ui_builders = list(exponential = function() create_exponential_ui("exponential")),
      server_key = "exponential",
      start_server = function(deps) {
        create_exponential_server("exponential", deps$color_palette)
      }
    ),
    weibull = list(
      label = "Weibull",
      tab_values = "weibull",
      sources = c(
        "modules/distributions/base_distribution_module.R",
        "modules/distributions/weibull_module.R"
      ),
      output_ids = c(weibull = "deferred_ui_weibull"),
      ui_builders = list(weibull = function() create_weibull_ui("weibull")),
      server_key = "weibull",
      start_server = function(deps) {
        create_weibull_server("weibull", deps$color_palette)
      }
    ),
    f_distribution = list(
      label = "F Distribution",
      tab_values = "f_distribution",
      sources = c(
        "modules/distributions/base_distribution_module.R",
        "modules/distributions/f_distribution_module.R"
      ),
      output_ids = c(f_distribution = "deferred_ui_f_distribution"),
      ui_builders = list(f_distribution = function() create_f_distribution_ui("f_distribution")),
      server_key = "f_distribution",
      start_server = function(deps) {
        create_f_distribution_server("f_distribution", deps$color_palette)
      }
    ),
    critical_values = list(
      label = "Critical Values",
      tab_values = "critical_values",
      sources = c(
        "modules/distributions/base_distribution_module.R",
        "modules/distributions/critical_values_module.R"
      ),
      output_ids = c(critical_values = "deferred_ui_critical_values"),
      ui_builders = list(critical_values = function() create_critical_values_ui("critical_values")),
      server_key = "critical_values",
      start_server = function(deps) {
        create_critical_values_server("critical_values", deps$color_palette)
      }
    ),
    distribution_testing = list(
      label = "Distribution Testing",
      tab_values = "distribution_testing",
      sources = "modules/distributions/distribution_testing/distribution_testing_module.R",
      output_ids = c(distribution_testing = "deferred_ui_distribution_testing"),
      ui_builders = list(
        distribution_testing = function() create_distribution_testing_ui("distribution_testing")
      ),
      server_key = "distribution_testing",
      start_server = function(deps) {
        create_distribution_testing_server(
          "distribution_testing",
          deps$filtered_data,
          deps$color_palette
        )
      }
    ),
    reliability_calc = list(
      label = "Reliability Calculator",
      tab_values = "reliability_calc",
      sources = "modules/reliability/reliability_calculator_module.R",
      output_ids = c(reliability_calc = "deferred_ui_reliability_calc"),
      ui_builders = list(
        reliability_calc = function() create_reliability_calculator_ui("reliability_calc")
      ),
      server_key = "reliability_calc",
      start_server = function(deps) {
        create_reliability_calculator_server("reliability_calc", deps$color_palette)
      }
    ),
    growth_analysis = list(
      label = "Growth Analysis",
      tab_values = "growth_analysis",
      sources = "modules/reliability/growth_analysis_module.R",
      output_ids = c(growth_analysis = "deferred_ui_growth_analysis"),
      ui_builders = list(
        growth_analysis = function() create_growth_analysis_ui("growth_analysis")
      ),
      server_key = "growth_analysis",
      start_server = function(deps) {
        create_growth_analysis_server("growth_analysis", deps$filtered_data, deps$color_palette)
      }
    ),
    weibull_analysis = list(
      label = "Weibull Analysis",
      tab_values = "weibull_analysis",
      sources = "modules/reliability/weibull_analysis_module.R",
      output_ids = c(weibull_analysis = "deferred_ui_weibull_analysis"),
      ui_builders = list(
        weibull_analysis = function() create_weibull_analysis_ui("weibull_analysis")
      ),
      server_key = "weibull_analysis",
      start_server = function(deps) {
        create_weibull_analysis_server("weibull_analysis", deps$filtered_data, deps$color_palette)
      }
    ),
    sample_size_power = list(
      label = "Sample Size/Power",
      tab_values = "sample_size_power",
      sources = "modules/statistical/sample_size_power/sample_size_power_module.R",
      output_ids = c(sample_size_power = "deferred_ui_sample_size_power"),
      ui_builders = list(
        sample_size_power = function() create_sample_size_power_ui("sample_size_power")
      ),
      server_key = "sample_size_power",
      start_server = function(deps) {
        create_sample_size_power_server("sample_size_power", deps$color_palette)
      }
    ),
    eda = list(
      label = "EDA",
      tab_values = "eda",
      sources = "modules/statistical/eda/eda_module.R",
      output_ids = c(eda = "deferred_ui_eda"),
      ui_builders = list(
        eda = function() unwrap_tab_panel_ui(create_eda_ui("eda"))
      ),
      server_key = "eda",
      start_server = function(deps) {
        create_eda_server("eda", deps$filtered_data, deps$color_palette)
      }
    ),
    ots = list(
      label = "One- and Two-Sample Tests",
      tab_values = c(
        "ots_test_reference",
        "ots_means_dispersion",
        "ots_proportion",
        "ots_poisson",
        "ots_nonparametric"
      ),
      sources = "modules/statistical/one_two_sample_tests/one_two_sample_tests_module.R",
      output_ids = c(
        ots_test_reference = "deferred_ui_ots_test_reference",
        ots_means_dispersion = "deferred_ui_ots_means_dispersion",
        ots_proportion = "deferred_ui_ots_proportion",
        ots_poisson = "deferred_ui_ots_poisson",
        ots_nonparametric = "deferred_ui_ots_nonparametric"
      ),
      ui_builders = list(
        ots_test_reference = function() {
          create_test_reference_ui(NS("one_two_sample_tests"))
        },
        ots_means_dispersion = function() {
          create_means_dispersion_ui(NS("one_two_sample_tests"))
        },
        ots_proportion = function() {
          create_proportions_ui(NS("one_two_sample_tests"))
        },
        ots_poisson = function() {
          create_poisson_one_two_sample_ui(NS("one_two_sample_tests"))
        },
        ots_nonparametric = function() {
          create_nonparametric_one_two_sample_ui(NS("one_two_sample_tests"))
        }
      ),
      server_key = "one_two_sample_tests",
      start_server = function(deps) {
        create_one_two_sample_tests_server(
          "one_two_sample_tests",
          deps$filtered_data,
          deps$color_palette
        )
      }
    ),
    correlation_association = list(
      label = "Correlation and Association",
      tab_values = "correlation_association",
      sources = "modules/statistical/correlation_association/correlation_association_module.R",
      output_ids = c(correlation_association = "deferred_ui_correlation_association"),
      ui_builders = list(
        correlation_association = function() {
          unwrap_tab_panel_ui(create_correlation_association_ui("correlation_association"))
        }
      ),
      server_key = "correlation_association",
      start_server = function(deps) {
        create_correlation_association_server(
          "correlation_association",
          deps$filtered_data,
          deps$color_palette
        )
      }
    ),
    autocorrelation = list(
      label = "Autocorrelation",
      tab_values = "autocorrelation",
      sources = "modules/statistical/autocorrelation/autocorrelation_module.R",
      output_ids = c(autocorrelation = "deferred_ui_autocorrelation"),
      ui_builders = list(
        autocorrelation = function() {
          unwrap_tab_panel_ui(create_autocorrelation_ui("autocorrelation"))
        }
      ),
      server_key = "autocorrelation",
      start_server = function(deps) {
        create_autocorrelation_server("autocorrelation", deps$filtered_data, deps$color_palette)
      }
    ),
    spc = list(
      label = "SPC/Capability",
      tab_values = "spc",
      sources = "modules/statistical/spc/spc_module.R",
      output_ids = c(spc = "deferred_ui_spc"),
      ui_builders = list(
        spc = function() unwrap_tab_panel_ui(create_spc_ui("spc"))
      ),
      server_key = "spc",
      start_server = function(deps) {
        create_spc_server("spc", deps$filtered_data, deps$color_palette)
      }
    ),
    msa = list(
      label = "MSA",
      tab_values = c("msa_continuous", "msa_discrete"),
      sources = "modules/statistical/msa/msa_module.R",
      output_ids = c(
        msa_continuous = "deferred_ui_msa_continuous",
        msa_discrete = "deferred_ui_msa_discrete"
      ),
      ui_builders = list(
        msa_continuous = function() create_continuous_msa_ui_internal(NS("msa")),
        msa_discrete = function() create_attribute_msa_ui_internal(NS("msa"))
      ),
      server_key = "msa",
      start_server = function(deps) {
        create_msa_server("msa", deps$filtered_data, deps$color_palette)
      }
    ),
    crosstabs = list(
      label = "Crosstabs",
      tab_values = "crosstabs",
      sources = "modules/statistical/crosstabs/crosstabs_module.R",
      output_ids = c(crosstabs = "deferred_ui_crosstabs"),
      ui_builders = list(
        crosstabs = function() unwrap_tab_panel_ui(create_crosstabs_ui("crosstabs"))
      ),
      server_key = "crosstabs",
      start_server = function(deps) {
        create_crosstabs_server("crosstabs", deps$filtered_data, deps$color_palette)
      }
    ),
    anova = list(
      label = "ANOVA",
      tab_values = c("anova_oneway", "anova_multifactor"),
      sources = "modules/statistical/anova/anova_module.R",
      output_ids = c(
        anova_oneway = "deferred_ui_anova_oneway",
        anova_multifactor = "deferred_ui_anova_multifactor"
      ),
      ui_builders = list(
        anova_oneway = function() create_oneway_anova_ui_internal(NS("anova")),
        anova_multifactor = function() create_multifactor_anova_ui_internal(NS("anova"))
      ),
      server_key = "anova",
      start_server = function(deps) {
        create_anova_server("anova", deps$filtered_data, deps$color_palette)
      }
    ),
    doe_orthogonal = list(
      label = "DOE Screening Design",
      tab_values = "doe_orthogonal",
      sources = "modules/statistical/doe_orthogonal/doe_orthogonal_module.R",
      output_ids = c(doe_orthogonal = "deferred_ui_doe_orthogonal"),
      ui_builders = list(
        doe_orthogonal = function() unwrap_tab_panel_ui(create_doe_orthogonal_ui("doe_orthogonal"))
      ),
      server_key = "doe_orthogonal",
      start_server = function(deps) {
        create_doe_orthogonal_server("doe_orthogonal", deps$color_palette)
      }
    )
  )
}

tab_value_to_module_key <- function(tab_value, registry = get_deferred_registry()) {
  if (is.null(tab_value) || !nzchar(tab_value)) {
    return(NULL)
  }
  for (key in names(registry)) {
    if (tab_value %in% registry[[key]]$tab_values) {
      return(key)
    }
  }
  NULL
}

ensure_module_sourced <- function(module_key, registry = get_deferred_registry()) {
  mod <- registry[[module_key]]
  if (is.null(mod)) {
    stop("Unknown deferred module: ", module_key, call. = FALSE)
  }
  for (path in mod$sources) {
    ensure_sourced_path(path)
  }
  invisible(TRUE)
}

ensure_module_server <- function(module_key, deps, registry = get_deferred_registry()) {
  mod <- registry[[module_key]]
  if (is.null(mod)) {
    return(invisible(FALSE))
  }
  sk <- mod$server_key
  if (sk %in% .deferred_state$servers_started || sk %in% .deferred_state$servers_starting) {
    return(invisible(FALSE))
  }
  # Mark BEFORE start_server: registering outputs (esp. suspendWhenHidden=FALSE)
  # can flush reactives and re-enter this function; a second moduleServer(id)
  # nests render functions and causes "node stack overflow".
  .deferred_state$servers_starting <- unique(c(.deferred_state$servers_starting, sk))
  ensure_module_sourced(module_key, registry)
  tryCatch({
    mod$start_server(deps)
    .deferred_state$servers_started <- unique(c(.deferred_state$servers_started, sk))
  }, error = function(e) {
    .deferred_state$servers_starting <- setdiff(.deferred_state$servers_starting, sk)
    stop(e)
  })
  .deferred_state$servers_starting <- setdiff(.deferred_state$servers_starting, sk)
  invisible(TRUE)
}

activate_deferred_tab <- function(tab_value, output, deps, session,
                                   registry = get_deferred_registry()) {
  module_key <- tab_value_to_module_key(tab_value, registry)
  if (is.null(module_key)) {
    return(invisible(FALSE))
  }
  mod <- registry[[module_key]]
  out_id <- unname(mod$output_ids[[tab_value]])
  if (is.null(out_id) || !nzchar(as.character(out_id))) {
    out_id <- unname(mod$output_ids[[1]])
  }
  if (out_id %in% .deferred_state$ui_ready) {
    # Server may still need starting if UI was prefetched via sibling activation
    ensure_module_server(module_key, deps, registry)
    return(invisible(FALSE))
  }
  # Prevent double-scheduling if navbar observer re-fires during activation
  if (out_id %in% .deferred_state$activating) {
    return(invisible(FALSE))
  }
  .deferred_state$activating <- unique(c(.deferred_state$activating, out_id))

  builder <- mod$ui_builders[[tab_value]]
  if (is.null(builder)) {
    builder <- mod$ui_builders[[1]]
  }

  # Show spinner first; finish load after this flush so the browser can paint it.
  # Heavy work must run under withReactiveDomain(session) — bare later()/onFlushed
  # lost the Shiny session and broke moduleServer/callModule.
  output[[out_id]] <- renderUI({
    deferred_loading_ui(mod$label)
  })

  session$onFlushed(function() {
    closed <- tryCatch(isTRUE(session$isClosed()), error = function(e) TRUE)
    if (closed) {
      .deferred_state$activating <- setdiff(.deferred_state$activating, out_id)
      return()
    }

    shiny::withReactiveDomain(session, {
      tryCatch({
        ensure_module_sourced(module_key, registry)

        output[[out_id]] <- renderUI({
          builder()
        })
        .deferred_state$ui_ready <- unique(c(.deferred_state$ui_ready, out_id))

        # Warm sibling tabs for multi-panel modules (ANOVA, MSA, OTS)
        for (tv in names(mod$output_ids)) {
          sibling_id <- unname(mod$output_ids[[tv]])
          if (identical(sibling_id, out_id)) next
          if (sibling_id %in% .deferred_state$ui_ready) next
          sibling_builder <- mod$ui_builders[[tv]]
          if (is.null(sibling_builder)) next
          local({
            sid <- sibling_id
            sb <- sibling_builder
            output[[sid]] <- renderUI({ sb() })
            .deferred_state$ui_ready <- unique(c(.deferred_state$ui_ready, sid))
          })
        }

        # Start servers on the *next* flush so nested uiOutput targets exist in
        # the client. Registering moduleServer in the same flush as renderUI
        # leaves nested pickers suspended until an unrelated input changes
        # (e.g. EDA "Analyze by factors").
        session$onFlushed(function() {
          closed2 <- tryCatch(isTRUE(session$isClosed()), error = function(e) TRUE)
          if (closed2) {
            .deferred_state$activating <- setdiff(.deferred_state$activating, out_id)
            return()
          }
          shiny::withReactiveDomain(session, {
            tryCatch({
              ensure_module_server(module_key, deps, registry)
            }, error = function(e) {
              output[[out_id]] <- renderUI({
                tags$div(
                  class = "alert alert-danger",
                  role = "alert",
                  style = "margin: 1.5rem;",
                  tags$strong(paste0("Failed to start ", mod$label, ": ")),
                  conditionMessage(e)
                )
              })
            }, finally = {
              .deferred_state$activating <- setdiff(.deferred_state$activating, out_id)
            })
          })
        }, once = TRUE)
      }, error = function(e) {
        output[[out_id]] <- renderUI({
          tags$div(
            class = "alert alert-danger",
            role = "alert",
            style = "margin: 1.5rem;",
            tags$strong(paste0("Failed to load ", mod$label, ": ")),
            conditionMessage(e)
          )
        })
        .deferred_state$activating <- setdiff(.deferred_state$activating, out_id)
      })
    })
  }, once = TRUE)

  invisible(TRUE)
}

prefetch_deferred_modules <- function(session, registry = get_deferred_registry()) {
  if (isTRUE(.deferred_state$prefetch_started)) {
    return(invisible(FALSE))
  }
  .deferred_state$prefetch_started <- TRUE
  keys <- names(registry)
  .deferred_state$prefetch_index <- 1L

  schedule_next <- function() {
    closed <- tryCatch(isTRUE(session$isClosed()), error = function(e) FALSE)
    if (closed) {
      return()
    }
    idx <- .deferred_state$prefetch_index
    if (idx > length(keys)) {
      return()
    }
    key <- keys[[idx]]
    .deferred_state$prefetch_index <- idx + 1L
    tryCatch(ensure_module_sourced(key, registry), error = function(e) {
      warning("Deferred prefetch failed for ", key, ": ", conditionMessage(e), call. = FALSE)
    })
    if (requireNamespace("later", quietly = TRUE)) {
      later::later(schedule_next, delay = 0.35)
    }
  }

  if (requireNamespace("later", quietly = TRUE)) {
    later::later(schedule_next, delay = 0.5)
  } else {
    schedule_next()
  }
  invisible(TRUE)
}

# Placeholder tab helper
deferred_tab_panel <- function(title, value, output_id) {
  tabPanel(
    title = title,
    value = value,
    uiOutput(output_id)
  )
}
