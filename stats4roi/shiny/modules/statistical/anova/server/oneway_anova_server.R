# Oneway ANOVA Worker Module
# Contains business logic for oneway ANOVA calculations

library(shiny)
library(lolcat)
library(DT)
library(dplyr)

# Source global systems
source("modules/config/global_config.R")

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
      
      # Extract values - all from coordinator via input_values
      data_col <- as.numeric(inputs_vals$ow_data)
      factor_ow <- as.numeric(inputs_vals$ow_factor)
      conf <- inputs_vals$conf_ow
      R <- inputs_vals$decimal_ow
      type <- inputs_vals$type_ow  # 1=Fisher, 2=Random, 3=K-W, 4=Welch
      
      req(data_col, factor_ow, conf, R, type)
      
      # Make names valid
      names(data) <- make.names(names(data))
      
      form <- as.formula(paste(names(data)[data_col], " ~ ", "as.factor(", names(data)[factor_ow], ")"))
      
      if (type == 1 || type == 2) {  # Fixed (Fisher) or Random
        oneway <- aov(formula = form, data = data)
        sum_aov <- ro(summary(oneway), R)
        
        temp <- summary(lm(formula = form, data = data))
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
        
        table_aov <- as.data.frame(table(data[factor_ow]))
        table_aov <- cbind(table_aov, table_aov[2]^2)
        J <- nrow(table_aov)
        sum_n <- colSums(table_aov[2])
        sum_nsq <- colSums(table_aov[3])
        K_prime <- (1 / (J - 1)) * (sum_n - (sum_nsq / sum_n))
        
        bcv <- (mse - msw) / K_prime
        bcv <- max(0, bcv)
        ICC <- 100 * bcv / (bcv + msw)
        
        output <- HTML(c(
          "Fisher's One-way analysis of variance (assumes equal variances, robust if equal n per group)", "</br></br>",
          "Model : ", names(oneway$model)[1], " by ", gsub(pattern = "as\\.factor\\(|\\)", replacement = "", names(oneway$model)[2]),
          "</br></br>",
          "<table><tr><th  style='padding: 2px 15px !important;'>Source</th><th  style='padding: 2px 15px !important;'>df</th><th  style='padding: 2px 15px !important;'>SS</th><th  style='padding: 2px 15px !important;'>MS</th><th  style='padding: 2px 15px !important;'>F</th><th  style='padding: 2px 15px !important;'>p</th></tr>",
          "<tr><td  style='padding: 2px 15px !important;'>", gsub(pattern = "as\\.factor\\(|\\)", replacement = "", names(oneway$model)[2]), "</td>",
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
      
      if (type == 3) {  # Kruskal-Wallis
        form <- as.formula(paste(names(data)[data_col], " ~ ", names(data)[factor_ow]))
        KW <- anova.independent.kruskal.wallis(fx = form, data = data, conf.level = conf)
        
        output <- HTML(c(
          "Method : ", KW$method, "</br>",
          "Model : ", names(data)[data_col], " by ", names(data)[factor_ow],
          "</br></br>",
          "<table><tr>",
          "<td>H = ", ro(KW$statistic, R), "</td><td>p =", ro(KW$p.value, R), if(KW$p.value <= 1 - conf){"*"}, "</td></tr>",
          "</table></br></br>"
        ))
      }  # end K-W
      
      if (type == 4) {  # Fixed Welch
        oneway_w <- oneway.test(formula = form, data = data, var.equal = FALSE)
        oneway <- aov(formula = form, data = data)  # just for labels
        nTot <- nrow(data)
        dfEff <- oneway_w[["parameter"]][["num df"]]
        wF <- oneway_w[["statistic"]][["F"]]
        imp_w <- (dfEff * (wF - 1)) / (dfEff * (wF - 1) + nTot) * 100
        if(is.nan(wF)) {
          return(HTML("NaN returned. This is possibly due to having a 0 variance for one or more cells. Try the Fisher ANOVA."))
        }
        output <- HTML(c(
          "Welch's ", oneway_w[["method"]],
          "</br></br>",
          "Model : ", names(oneway$model)[1], " by ", gsub(pattern = "as\\.factor\\(|\\)", replacement = "", names(oneway$model)[2]),
          "</br></br>",
          "<table><tr><th  style='padding: 2px 15px !important;'>Source</th><th  style='padding: 2px 15px !important;'>df</th><th  style='padding: 2px 15px !important;'>F</th><th  style='padding: 2px 15px !important;'>p</th></tr>",
          "<tr><td  style='padding: 2px 15px !important;'>", gsub(pattern = "as\\.factor\\(|\\)", replacement = "", names(oneway$model)[2]), "</td>",
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
      
      # Make names valid (consistent with other reactives)
      names(data) <- make.names(names(data))
      
      mean_rank_tab <- cbind(factor = data[factor_ow], data = data[data_col], rank = rank(data[data_col]))
      
      KW_tab <- ro(aggregate(x = mean_rank_tab, by = mean_rank_tab[1], FUN = function(x) c(Mean.Rank = mean(x), Count = length(x)))[c(-2, -3)], R)
      
      names(KW_tab) <- c(names(data)[factor_ow], "Mean Rank", "Count")
      
      # Ensure proper data frame structure - flatten any matrix columns from aggregate
      KW_tab <- as.data.frame(KW_tab)
      
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
      
      req(data_col, factor_ow, conf, R, type)
      
      if (!disp_ow || type == 3) {
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
        
        disp_data <- as.data.frame(cbind(factor = data[[factor_ow]], ada = ada_dat, adm = adm_dat, admn1 = admn1_dat))
        
        if (length(unique((data %>% count(!!as.name(names(data)[factor_ow])))$n)) != 1) {  # branch to use Welch anova for dispersion if sample sizes are unequal
          if (disp_data %>% group_by(as.factor(factor)) %>% filter(!is.na(ada)) %>% summarize(var = var(ada)) %>% summarize(value = min(var)) == 0) {  # must use Fisher
            ada_type <- "Fisher (unequal n, cell var = 0)"
            ada_aov <- summary(aov(formula = ada ~ as.factor(factor), data = disp_data))
            lev_df <- paste("F(", ada_aov[[1]]$Df[1], ",", ada_aov[[1]]$Df[2], ")")
            lev_f <- ada_aov[[1]]$`F value`[1]
            lev_p <- ada_aov[[1]]$`Pr(>F)`[1]
          } else {  # use Welch
            ada_type <- "Welch (unequal n)"
            ada_aov <- oneway.test(formula = ada ~ as.factor(factor), data = disp_data, var.equal = FALSE)
            lev_df <- paste("F(", ada_aov[["parameter"]][1], ",", ro(ada_aov[["parameter"]][2], R), ")")
            lev_f <- ada_aov[["statistic"]][["F"]]
            lev_p <- ada_aov[["p.value"]]
          }
          
          if (disp_data %>% group_by(as.factor(factor)) %>% filter(!is.na(adm)) %>% summarize(var = var(adm)) %>% summarize(value = min(var)) == 0) {  # must use Fisher
            adm_type <- "Fisher (unequal n, cell var = 0)"
            adm_aov <- summary(aov(formula = adm ~ as.factor(factor), data = disp_data))
            adm_df <- paste("F(", adm_aov[[1]]$Df[1], ",", ro(adm_aov[[1]]$Df[2], R), ")")
            adm_f <- adm_aov[[1]]$`F value`[1]
            adm_p <- adm_aov[[1]]$`Pr(>F)`[1]
          } else {  # use Welch
            adm_type <- "Welch (unequal n)"
            adm_aov <- oneway.test(formula = adm ~ as.factor(factor), data = disp_data, var.equal = FALSE)
            adm_df <- paste("F(", adm_aov[["parameter"]][1], ",", ro(adm_aov[["parameter"]][2], R), ")")
            adm_f <- adm_aov[["statistic"]][["F"]]
            adm_p <- adm_aov[["p.value"]]
          }
          
          if (disp_data %>% group_by(as.factor(factor)) %>% filter(!is.na(admn1)) %>% summarize(var = var(admn1)) %>% summarize(value = min(var)) == 0) {  # must use Fisher
            admn1_type <- "Fisher (unequal n, cell var = 0)"
            admn1_aov <- summary(aov(formula = admn1 ~ as.factor(factor), data = disp_data))
            admn1_df <- paste("F(", admn1_aov[[1]]$Df[1], ",", admn1_aov[[1]]$Df[2], ")")
            admn1_f <- admn1_aov[[1]]$`F value`[1]
            admn1_p <- admn1_aov[[1]]$`Pr(>F)`[1]
          } else {  # use Welch
            admn1_type <- "Welch (unequal n)"
            admn1_aov <- oneway.test(formula = admn1 ~ as.factor(factor), data = disp_data, var.equal = FALSE)
            admn1_df <- paste("F(", admn1_aov[["parameter"]][1], ",", admn1_aov[["parameter"]][2], ")")
            admn1_f <- admn1_aov[["statistic"]][["F"]]
            admn1_p <- admn1_aov[["p.value"]]
          }
        } else {  # Use Fisher
          ada_type <- "Fisher (equal n)"
          adm_type <- "Fisher (equal n)"
          admn1_type <- "Fisher (equal n)"
          ada_aov <- summary(aov(formula = ada ~ as.factor(factor), data = disp_data))
          lev_df <- paste("F(", ada_aov[[1]]$Df[1], ",", ada_aov[[1]]$Df[2], ")")
          lev_f <- ada_aov[[1]]$`F value`[1]
          lev_p <- ada_aov[[1]]$`Pr(>F)`[1]
          
          adm_aov <- summary(aov(formula = adm ~ as.factor(factor), data = disp_data))
          adm_df <- paste("F(", adm_aov[[1]]$Df[1], ",", adm_aov[[1]]$Df[2], ")")
          adm_f <- adm_aov[[1]]$`F value`[1]
          adm_p <- adm_aov[[1]]$`Pr(>F)`[1]
          
          admn1_aov <- summary(aov(formula = admn1 ~ as.factor(factor), data = disp_data))
          admn1_df <- paste("F(", admn1_aov[[1]]$Df[1], ",", admn1_aov[[1]]$Df[2], ")")
          admn1_f <- admn1_aov[[1]]$`F value`[1]
          admn1_p <- admn1_aov[[1]]$`Pr(>F)`[1]
        }
        
        # Present results as HTML table
        output <- HTML(paste(
          "</br></br><b><u>Dispersion Analysis</u></b></br>",
          "<table>",
          "<tr><td colspan='4' style='text-align:left;background-color:#DCDCDC'><i>If normally distributed within cells</i></td><td style='text-align:left;background-color:#DCDCDC'><b>Calculation</b></td></tr>",
          "<tr><td style='text-align:left;'>", withMathJax("$\\text{Levene}$"), "</td><td style='text-align:left;'>", lev_df, " = ", ro(lev_f, R), "</td><td></td><td style='text-align:left;'>p = ", ro(lev_p, R), if(lev_p <= 1 - conf){"*"}else{""}, "</td><td style='text-align:left;'>", ada_type, "</td></tr>",
          "<tr><td colspan='4' style='text-align:left;background-color:#DCDCDC'><i>If not normally distributed within cells</i></td><td style='text-align:left;background-color:#DCDCDC'><b>Calculation</b></td></tr>",
          "<tr><td style='text-align:left;'>If n ≤ 10", withMathJax("$ADM$"), "</td><td style='text-align:left;'>", adm_df, " = ", ro(adm_f, R), "</td><td></td><td style='text-align:left;'>p = ", ro(adm_p, R), if(adm_p <= 1 - conf){"*"}else{""}, "</td><td style='text-align:left;'>", adm_type, "</td></tr>",
          "<tr><td style='text-align:left;'>If n > 10", withMathJax("$ADM_{n-1}$"), "</td><td style='text-align:left;'>", admn1_df, " = ", ro(admn1_f, R), "</td><td></td><td style='text-align:left;'>p = ", ro(admn1_p, R), if(admn1_p <= 1 - conf){"*"}else{""}, "</td><td style='text-align:left;'>", admn1_type, "</td></tr>",
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
