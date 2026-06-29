# Oneway ANOVA Worker Module
# Contains business logic for oneway ANOVA calculations

library(shiny)
library(lolcat)
library(DT)
library(dplyr)

# Source global systems
source("modules/config/global_config.R")

# Min within-cell variance for dispersion (n<2 -> NA excluded); avoids min(NA,...) and if(NA) on tibble==0.
ow_dispersion_min_cell_var <- function(disp_df, var_name) {
  v <- disp_df %>%
    dplyr::group_by(grp = as.factor(.data$grp)) %>%
    dplyr::filter(!is.na(.data[[var_name]])) %>%
    dplyr::summarize(
      cell_var = if (dplyr::n() > 1L) stats::var(.data[[var_name]]) else NA_real_,
      .groups = "drop"
    ) %>%
    dplyr::pull(cell_var)
  if (!length(v)) return(Inf)
  mv <- suppressWarnings(min(v, na.rm = TRUE))
  if (!is.finite(mv)) Inf else mv
}

# p can be NA from degenerate aov/oneway.test; never use if(p <= ...) inside paste()
ow_dispersion_sig_star <- function(p, conf) {
  if (length(conf) < 1L) return("")
  thr <- 1 - conf[[1L]]
  if (is.na(thr) || length(p) < 1L) return("")
  pv <- p[[1L]]
  if (is.na(pv)) return("")
  if (isTRUE(pv <= thr)) "*" else ""
}

create_oneway_anova_worker <- function(id, filtered_data, input_values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Extract inputs from coordinator (for conf, decimals, type, disp_ow)
    # Note: ow_factor and ow_data are in this worker's namespace
    inputs <- reactive({
      input_values()
    })
    
    # Note: UI rendering is done in coordinator, not worker
    # Workers should only return reactive functions with business logic
    
    # Main ANOVA table output
    anova_table <- reactive({
      inputs_vals <- inputs()
      data <- filtered_data()
      req(data, inputs_vals)
      
      data_col <- as.numeric(inputs_vals$ow_data)
      factor_ow <- as.numeric(inputs_vals$ow_factor)
      conf <- inputs_vals$conf_ow
      R <- inputs_vals$decimal_ow
      type <- inputs_vals$type_ow  # 1=Fisher, 2=Random, 3=K-W, 4=Welch
      
      req(data_col, factor_ow, conf, R, type)
      
      frame <- build_oneway_analysis_frame(
        data = data,
        data_col = data_col,
        factor_col = factor_ow,
        analysis_disp = inputs_vals$ow_disp_analysis,
        disp_type_id = inputs_vals$ow_disp_type,
        type_ow = type
      )
      if (!isTRUE(frame$ok)) {
        return(HTML(ow_oneway_error_html(frame$message)))
      }
      
      work <- frame$data
      form <- stats::as.formula(".response ~ .factor")
      resp_label <- frame$response_label
      factor_label <- frame$factor_label
      
      header_block <- paste0(
        "Dependent Variable: ", resp_label, "<br>",
        frame$header_suffix
      )
      if (!is.null(frame$note) && nzchar(frame$note)) {
        header_block <- paste0(header_block, "<br><i>", frame$note, "</i>")
      }
      header_block <- paste0(header_block, "</br></br>")
      
      if (type == 1 || type == 2) {  # Fixed (Fisher) or Random
        oneway <- aov(formula = form, data = work)
        sum_aov <- ro(summary(oneway), R)
        
        temp <- summary(lm(formula = form, data = work))
        r_sq <- temp[["r.squared"]]
        r_sqr_adj <- temp[["adj.r.squared"]]
        
        sse <- sum_aov[[1]][["Sum Sq"]][1]
        ssw <- sum_aov[[1]][["Sum Sq"]][2]
        dfe <- sum_aov[[1]][["Df"]][1]
        dfw <- sum_aov[[1]][["Df"]][2]
        mse <- sum_aov[[1]][["Mean Sq"]][1]
        msw <- sum_aov[[1]][["Mean Sq"]][2]
        sst <- sse + ssw
        dft <- dfe + dfw
        
        omega_sq <- 100 * (sse - (dfe * msw)) / (sst + msw)  # fixed
        
        table_aov <- as.data.frame(table(work$.factor))
        table_aov <- cbind(table_aov, table_aov[2]^2)
        J <- nrow(table_aov)
        sum_n <- colSums(table_aov[2])
        sum_nsq <- colSums(table_aov[3])
        K_prime <- (1 / (J - 1)) * (sum_n - (sum_nsq / sum_n))
        
        bcv <- (mse - msw) / K_prime
        bcv <- max(0, bcv)
        ICC <- 100 * bcv / (bcv + msw)
        
        output <- HTML(c(
          header_block,
          "Fisher's One-way analysis of variance (assumes equal variances, robust if equal n per group)", "</br></br>",
          "Model : ", resp_label, " by ", factor_label,
          "</br></br>",
          "<table><tr><th  style='padding: 2px 15px !important;'>Source</th><th  style='padding: 2px 15px !important;'>df</th><th  style='padding: 2px 15px !important;'>SS</th><th  style='padding: 2px 15px !important;'>MS</th><th  style='padding: 2px 15px !important;'>F</th><th  style='padding: 2px 15px !important;'>p</th></tr>",
          "<tr><td  style='padding: 2px 15px !important;'>", factor_label, "</td>",
          "<td  style='padding: 2px 15px !important;'>", sum_aov[[1]][["Df"]][1], "</td><td  style='padding: 2px 15px !important;'>", sum_aov[[1]][["Sum Sq"]][1], "</td><td  style='padding: 2px 15px !important;'>", sum_aov[[1]][["Mean Sq"]][1], "</td><td  style='padding: 2px 15px !important;'>", sum_aov[[1]][["F value"]][1], "</td><td  style='padding: 2px 15px !important;'>", sum_aov[[1]][["Pr(>F)"]][1], if(sum_aov[[1]][["Pr(>F)"]][1] <= (1 - conf)){"*"}, "</tr>",
          "<tr><td  style='padding: 2px 15px !important;'> Within</td>", "<td  style='padding: 2px 15px !important;'>", sum_aov[[1]][["Df"]][2], "</td><td  style='padding: 2px 15px !important;'>", sum_aov[[1]][["Sum Sq"]][2], "</td><td  style='padding: 2px 15px !important;'>", sum_aov[[1]][["Mean Sq"]][2], "</td></tr>",
          "<tr><td  style='padding: 2px 15px !important;'> Total</td>", "<td  style='padding: 2px 15px !important;'>", sum_aov[[1]][["Df"]][1] + sum_aov[[1]][["Df"]][2], "</td><td  style='padding: 2px 15px !important;'>", sum_aov[[1]][["Sum Sq"]][1] + sum_aov[[1]][["Sum Sq"]][2], "</td></tr>",
          "</table></br>",
          "<table><tr><td style='text-align:right'>", paste(withMathJax("$r^2=$"), ro(r_sq, R), "</td><td></td>",
                                                             "<td>", withMathJax("$r_{adj}^2=$"), ro(r_sqr_adj, R)), "</td></tr></table><br>",
          if(type == 1) {
            paste(
              "<table><tr><td>Fixed Effect Importance:</td></tr>",
              "<tr><td><b>", paste(withMathJax("$\\omega^2=$"), ro(omega_sq, R), "%"), "</b></td></tr></table>"
            )
          },
          if(type == 2) {
            paste(
              "<table><tr><td>Random Effect Importance:</td></tr>",
              "<tr><td>Treatment Variance =</td><td>", ro(bcv, R), "</td><td>", withMathJax("$\\hat{\\sigma}_{treat}=$"), ro(bcv^.5, R), "</tr>",
              "<tr><td>Within Variance =</td><td>", ro(msw, R), "</td><td>", withMathJax("$\\hat{\\sigma}_{within}=$"), ro(msw^.5, R), "</tr>",
              "<tr><td>Total Variance =</td><td>", ro(bcv + msw, R), "</td><td>", withMathJax("$\\hat{\\sigma}_{total}=$"), ro((bcv + msw)^.5, R), "</tr>",
              "<tr><td><b>Intraclass Correlation =</b></td><td><b>", ro(ICC, R), "%</b></td>", "</tr>",
              "</table>"
            )
          }
        ))
      }  # end fixed and random
      
      if (type == 3) {  # Kruskal-Wallis (means only)
        KW <- anova.independent.kruskal.wallis(fx = form, data = work, conf.level = conf)
        
        output <- HTML(c(
          header_block,
          "Method : ", KW$method, "</br>",
          "Model : ", resp_label, " by ", factor_label,
          "</br></br>",
          "<table><tr>",
          "<td>H = ", ro(KW$statistic, R), "</td><td>p =", ro(KW$p.value, R), if(KW$p.value <= 1 - conf){"*"}, "</td></tr>",
          "</table></br></br>"
        ))
      }  # end K-W
      
      if (type == 4) {  # Fixed Welch
        oneway_w <- oneway.test(formula = form, data = work, var.equal = FALSE)
        nTot <- nrow(work)
        dfEff <- oneway_w[["parameter"]][["num df"]]
        wF <- oneway_w[["statistic"]][["F"]]
        imp_w <- (dfEff * (wF - 1)) / (dfEff * (wF - 1) + nTot) * 100
        if(is.nan(wF)) {
          return(HTML(paste0(header_block, "NaN returned. This is possibly due to having a 0 variance for one or more cells. Try the Fisher ANOVA.")))
        }
        output <- HTML(c(
          header_block,
          "Welch's ", oneway_w[["method"]],
          "</br></br>",
          "Model : ", resp_label, " by ", factor_label,
          "</br></br>",
          "<table><tr><th  style='padding: 2px 15px !important;'>Source</th><th  style='padding: 2px 15px !important;'>df</th><th  style='padding: 2px 15px !important;'>F</th><th  style='padding: 2px 15px !important;'>p</th></tr>",
          "<tr><td  style='padding: 2px 15px !important;'>", factor_label, "</td>",
          "<td  style='padding: 2px 15px !important;'>", dfEff, ",", ro(oneway_w[["parameter"]][["denom df"]], R), "</td><td  style='padding: 2px 15px !important;'>", ro(wF, R), "</td><td  style='padding: 2px 15px !important;'>", ro(oneway_w[["p.value"]], R), if(oneway_w[["p.value"]] <= (1 - conf)){"*"}, "</tr>",
          "</table></br></br>",
          "<table><tr><td>Fixed Effect Importance:</td></tr>",
          "<tr><td><b>", paste(withMathJax("$\\omega^2=$"), ro(imp_w, R), "%"), "</b></td></tr></table>"
        ))
      }  # end welch
      
      output
    })
    
    # Kruskal-Wallis mean rank table
    kw_table <- reactive({
      inputs_vals <- inputs()
      data <- filtered_data()
      req(data, inputs_vals)
      
      data_col <- as.numeric(inputs_vals$ow_data)
      factor_ow <- as.numeric(inputs_vals$ow_factor)
      R <- inputs_vals$decimal_ow
      
      req(data_col, factor_ow)
      
      frame <- build_oneway_analysis_frame(
        data = data,
        data_col = data_col,
        factor_col = factor_ow,
        analysis_disp = FALSE,
        disp_type_id = 1L,
        type_ow = 3L
      )
      req(isTRUE(frame$ok))
      work <- frame$data
      
      mean_rank_tab <- data.frame(
        factor = work$.factor,
        response = work$.response,
        rank = rank(work$.response),
        stringsAsFactors = FALSE
      )
      
      KW_tab <- mean_rank_tab %>%
        dplyr::group_by(.data$factor) %>%
        dplyr::summarize(
          `Mean Rank` = mean(.data$rank),
          Count = dplyr::n(),
          .groups = "drop"
        )
      KW_tab <- ro(as.data.frame(KW_tab), R)
      names(KW_tab)[1L] <- frame$factor_label
      
      KW_tab
    })
    
    # Dispersion tests output
    dispersion_output <- reactive({
      inputs_vals <- inputs()
      data <- filtered_data()
      req(data, inputs_vals)
      
      data_col <- as.numeric(inputs_vals$ow_data)
      factor_ow <- as.numeric(inputs_vals$ow_factor)
      conf <- inputs_vals$conf_ow
      R <- inputs_vals$decimal_ow
      type <- inputs_vals$type_ow
      disp_ow <- inputs_vals$disp_ow
      ow_disp_analysis <- isTRUE(inputs_vals$ow_disp_analysis)
      
      req(data_col, factor_ow, conf, R, type)
      
      if (!disp_ow || type == 3 || ow_disp_analysis) {
        return(NULL)
      }
      
      # Make names valid
      names(data) <- make.names(names(data))
      
      form_c <- as.formula(paste(names(data)[data_col], " ~ ", names(data)[factor_ow]))
      form <- as.formula(paste(names(data)[data_col], " ~ ", "as.factor(", names(data)[factor_ow], ")"))
      
      if (type == 1 || type == 2 || type == 4) {  # fixed, random, Welch need dispersion tests
        # Calculate ada, adm, admn-1
        ada_dat <- compute.group.dispersion.ADA(fx = form_c, data = data)
        adm_dat <- compute.group.dispersion.ADM(fx = form_c, data = data)
        admn1_dat <- compute.group.dispersion.ADMn1(fx = form_c, data = data)
        
        # data.frame + as.numeric: cbind() with a character factor column coerces ada/adm/admn1 to character → invalid aov()
        disp_data <- data.frame(
          grp = data[[factor_ow]],
          ada = as.numeric(ada_dat),
          adm = as.numeric(adm_dat),
          admn1 = as.numeric(admn1_dat),
          stringsAsFactors = FALSE
        )
        # ADMn-1 sets one obs per cell to NA (lolcat::dispersion.ADMn1); match two-sample path na.omit(ADMn1) in monolithic app
        disp_data_admn1 <- dplyr::filter(disp_data, !is.na(.data$admn1))
        
        if (length(unique((data %>% count(!!as.name(names(data)[factor_ow])))$n)) != 1) {  # branch to use Welch anova for dispersion if sample sizes are unequal
          if (ow_dispersion_min_cell_var(disp_data, "ada") == 0) {  # must use Fisher
            ada_type <- "Fisher (unequal n, cell var = 0)"
            ada_aov <- summary(aov(formula = ada ~ as.factor(grp), data = disp_data))
            lev_df <- paste("F(", ada_aov[[1]]$Df[1], ",", ada_aov[[1]]$Df[2], ")")
            lev_f <- ada_aov[[1]]$`F value`[1]
            lev_p <- ada_aov[[1]]$`Pr(>F)`[1]
          } else {  # use Welch
            ada_type <- "Welch (unequal n)"
            ada_aov <- oneway.test(formula = ada ~ as.factor(grp), data = disp_data, var.equal = FALSE)
            lev_df <- paste("F(", ada_aov[["parameter"]][1], ",", ro(ada_aov[["parameter"]][2], R), ")")
            lev_f <- ada_aov[["statistic"]][["F"]]
            lev_p <- ada_aov[["p.value"]]
          }
          
          if (ow_dispersion_min_cell_var(disp_data, "adm") == 0) {  # must use Fisher
            adm_type <- "Fisher (unequal n, cell var = 0)"
            adm_aov <- summary(aov(formula = adm ~ as.factor(grp), data = disp_data))
            adm_df <- paste("F(", adm_aov[[1]]$Df[1], ",", ro(adm_aov[[1]]$Df[2], R), ")")
            adm_f <- adm_aov[[1]]$`F value`[1]
            adm_p <- adm_aov[[1]]$`Pr(>F)`[1]
          } else {  # use Welch
            adm_type <- "Welch (unequal n)"
            adm_aov <- oneway.test(formula = adm ~ as.factor(grp), data = disp_data, var.equal = FALSE)
            adm_df <- paste("F(", adm_aov[["parameter"]][1], ",", ro(adm_aov[["parameter"]][2], R), ")")
            adm_f <- adm_aov[["statistic"]][["F"]]
            adm_p <- adm_aov[["p.value"]]
          }
          
          if (ow_dispersion_min_cell_var(disp_data, "admn1") == 0) {  # must use Fisher
            admn1_type <- "Fisher (unequal n, cell var = 0)"
            admn1_aov <- summary(aov(formula = admn1 ~ as.factor(grp), data = disp_data_admn1))
            admn1_df <- paste("F(", admn1_aov[[1]]$Df[1], ",", admn1_aov[[1]]$Df[2], ")")
            admn1_f <- admn1_aov[[1]]$`F value`[1]
            admn1_p <- admn1_aov[[1]]$`Pr(>F)`[1]
          } else {  # use Welch
            admn1_type <- "Welch (unequal n)"
            admn1_aov <- oneway.test(formula = admn1 ~ as.factor(grp), data = disp_data_admn1, var.equal = FALSE)
            admn1_df <- paste("F(", admn1_aov[["parameter"]][1], ",", admn1_aov[["parameter"]][2], ")")
            admn1_f <- admn1_aov[["statistic"]][["F"]]
            admn1_p <- admn1_aov[["p.value"]]
          }
        } else {  # Use Fisher
          ada_type <- "Fisher (equal n)"
          adm_type <- "Fisher (equal n)"
          admn1_type <- "Fisher (equal n)"
          ada_aov <- summary(aov(formula = ada ~ as.factor(grp), data = disp_data))
          lev_df <- paste("F(", ada_aov[[1]]$Df[1], ",", ada_aov[[1]]$Df[2], ")")
          lev_f <- ada_aov[[1]]$`F value`[1]
          lev_p <- ada_aov[[1]]$`Pr(>F)`[1]
          
          adm_aov <- summary(aov(formula = adm ~ as.factor(grp), data = disp_data))
          adm_df <- paste("F(", adm_aov[[1]]$Df[1], ",", adm_aov[[1]]$Df[2], ")")
          adm_f <- adm_aov[[1]]$`F value`[1]
          adm_p <- adm_aov[[1]]$`Pr(>F)`[1]
          
          admn1_aov <- summary(aov(formula = admn1 ~ as.factor(grp), data = disp_data_admn1))
          admn1_df <- paste("F(", admn1_aov[[1]]$Df[1], ",", admn1_aov[[1]]$Df[2], ")")
          admn1_f <- admn1_aov[[1]]$`F value`[1]
          admn1_p <- admn1_aov[[1]]$`Pr(>F)`[1]
        }
        
        # Present results as HTML table
        output <- HTML(paste(
          "</br></br><b><u>Dispersion Analysis</u></b></br>",
          "<table>",
          "<tr><td colspan='4' style='text-align:left;background-color:#DCDCDC'><i>If normally distributed within cells</i></td><td style='text-align:left;background-color:#DCDCDC'><b>Calculation</b></td></tr>",
          "<tr><td style='text-align:left;'>", withMathJax("$\\text{Levene}$"), "</td><td style='text-align:left;'>", lev_df, " = ", ro(lev_f, R), "</td><td></td><td style='text-align:left;'>p = ", ro(lev_p, R), ow_dispersion_sig_star(lev_p, conf), "</td><td style='text-align:left;'>", ada_type, "</td></tr>",
          "<tr><td colspan='4' style='text-align:left;background-color:#DCDCDC'><i>If not normally distributed within cells</i></td><td style='text-align:left;background-color:#DCDCDC'><b>Calculation</b></td></tr>",
          "<tr><td style='text-align:left;'>If n ≤ 10", withMathJax("$ADM$"), "</td><td style='text-align:left;'>", adm_df, " = ", ro(adm_f, R), "</td><td></td><td style='text-align:left;'>p = ", ro(adm_p, R), ow_dispersion_sig_star(adm_p, conf), "</td><td style='text-align:left;'>", adm_type, "</td></tr>",
          "<tr><td style='text-align:left;'>If n > 10", withMathJax("$ADM_{n-1}$"), "</td><td style='text-align:left;'>", admn1_df, " = ", ro(admn1_f, R), "</td><td></td><td style='text-align:left;'>p = ", ro(admn1_p, R), ow_dispersion_sig_star(admn1_p, conf), "</td><td style='text-align:left;'>", admn1_type, "</td></tr>",
          "</table>"
        ))
      }
      
      output
    })
    
    # Note: UI rendering is done in coordinator, not worker
    # Workers should only return reactive functions with business logic
    
    # Return reactive values for coordinator
    list(
      anova_table = anova_table,
      kw_table = kw_table,
      dispersion_output = dispersion_output
    )
  })
}
