# Continuous MSA server (Interval/Ratio)

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(lolcat)
library(shinyWidgets)

create_continuous_msa_server <- function(id, filtered_data, reactive_color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    colors <- reactive({
      pal <- reactive_color_palette()
      if (is.null(pal) || length(pal) < 6) {
        pal <- palette.colors(8)
      }
      pal
    })

    # Core reactive values
    sig_e <- reactiveVal(0)
    sig_rpt <- reactiveVal(0)
    sig_rpd <- reactiveVal(0)
    sig_part_app <- reactiveVal(0)
    sig_app <- reactiveVal(0)
    msa_init <- reactiveVal(0)
    app_num <- reactiveVal(0)

    # Track button press and output rendering for "Perform Analysis"
    button_state_c <- reactiveVal(FALSE)
    lt_ready <- reactiveVal(FALSE)
    ui_render_flags <- reactiveValues(
      msa_out = FALSE,
      lt_plot = FALSE,
      lt_plot_2 = FALSE,
      msa_cchart1 = FALSE,
      msa_linearity_2 = FALSE,
      msascatter = FALSE,
      msa_cchart0 = FALSE,
      msa_box = FALSE,
      msavarcompgraph = FALSE,
      msadangerzone = FALSE
    )

    observeEvent(input$msa_c_go, {
      button_state_c(TRUE)
      for (name in names(ui_render_flags)) {
        ui_render_flags[[name]] <- FALSE
      }
    })

    observeEvent(input$msa_type, {
      lt_ready(FALSE)
    })

    observe({
      type <- input$msa_type
      all_rendered <- ui_render_flags$msa_out

      if (!isTRUE(input$msa_graphs)) {
        isolate({
          button_state_c(FALSE)
        })
        return()
      }

      if (type == 1 || type == 2) {
        all_rendered <- all(
          ui_render_flags$msa_cchart1,
          ui_render_flags$msascatter,
          ui_render_flags$msa_cchart0,
          ui_render_flags$msa_box,
          ui_render_flags$msavarcompgraph,
          ui_render_flags$msadangerzone
        )
      } else if (type == 3) {
        all_rendered <- all(
          ui_render_flags$msa_cchart1,
          ui_render_flags$msascatter,
          ui_render_flags$msa_cchart0,
          ui_render_flags$msa_box,
          ui_render_flags$msavarcompgraph,
          ui_render_flags$msadangerzone,
          ui_render_flags$lt_plot,
          ui_render_flags$lt_plot_2
        )
      }

      if (input$msa_stand) {
        all_rendered <- all(all_rendered, ui_render_flags$msa_linearity_2)
      }

      if (all_rendered) {
        isolate({
          button_state_c(FALSE)
        })
      }
    })

    output$msa_UI1 <- renderUI({
      data_type <- input$msa_data_type
      req(filtered_data())
      choices <- seq(1:ncol(filtered_data()))
      names(choices) <- names(filtered_data())

      if (data_type == 2) {
        pickerInput(
          inputId = ns("msa_UI1"),
          label = "Select Appraiser Column",
          multiple = FALSE,
          options = list(`actions-box` = TRUE),
          choices = choices
        )
      }
    })

    output$msa_UI2 <- renderUI({
      data_type <- input$msa_data_type
      req(filtered_data())
      choices <- seq(1:ncol(filtered_data()))
      names(choices) <- names(filtered_data())
      UI1 <- input$msa_UI1
      req(UI1)

      if (data_type == 2) {
        fact_selected <- as.numeric(unlist(strsplit(x = UI1, split = "\\s+")))
        temp <- seq(1:length(choices))
        temp <- temp[-fact_selected]
        choices <- choices[temp]

        pickerInput(
          inputId = ns("msa_UI2"),
          label = "Select Part Column",
          multiple = FALSE,
          options = list(`actions-box` = TRUE),
          choices = choices
        )
      }
    })

    output$msa_UI3 <- renderUI({
      data_type <- input$msa_data_type
      req(filtered_data())
      choices <- seq(1:ncol(filtered_data()))
      names(choices) <- names(filtered_data())
      UI1 <- as.numeric(input$msa_UI1)
      UI2 <- as.numeric(input$msa_UI2)
      req(UI2)

      if (data_type == 2) {
        fact_selected <- c(UI1, UI2)
        temp <- seq(1:length(choices))
        temp <- temp[-fact_selected]
        choices <- choices[temp]

        pickerInput(
          inputId = ns("msa_UI3"),
          label = "Select Trial Column",
          multiple = FALSE,
          options = list(`actions-box` = TRUE),
          choices = choices
        )
      }
    })

    output$msa_UI4 <- renderUI({
      data_type <- input$msa_data_type
      req(filtered_data())
      choices <- seq(1:ncol(filtered_data()))
      names(choices) <- names(filtered_data())
      UI1 <- as.numeric(input$msa_UI1)
      UI2 <- as.numeric(input$msa_UI2)
      UI3 <- as.numeric(input$msa_UI3)
      req(UI3)

      if (data_type == 2) {
        fact_selected <- c(UI1, UI2, UI3)
        temp <- seq(1:length(choices))
        temp <- temp[-fact_selected]
        choices <- choices[temp]

        pickerInput(
          inputId = ns("msa_UI4"),
          label = "Select Measurement Column",
          multiple = FALSE,
          options = list(`actions-box` = TRUE),
          choices = choices
        )
      }
    })

    output$msa_UI5 <- renderUI({
      data_type <- input$msa_data_type
      req(filtered_data())
      choices <- seq(1:ncol(filtered_data()))
      names(choices) <- names(filtered_data())
      UI1 <- as.numeric(input$msa_UI1)
      UI2 <- as.numeric(input$msa_UI2)
      UI3 <- as.numeric(input$msa_UI3)
      UI4 <- as.numeric(input$msa_UI4)
      req(UI4)

      if (data_type == 2) {
        fact_selected <- c(UI1, UI2, UI3, UI4)
        temp <- seq(1:length(choices))
        temp <- temp[-fact_selected]
        choices <- choices[temp]

        pickerInput(
          inputId = ns("msa_UI5"),
          label = "Select True Value Column",
          multiple = FALSE,
          options = list(`actions-box` = TRUE),
          choices = choices
        )
      }
    })

    msa_data_r <- reactive({
      UI1 <- as.numeric(input$msa_UI1)
      UI2 <- as.numeric(input$msa_UI2)
      UI3 <- as.numeric(input$msa_UI3)
      UI4 <- as.numeric(input$msa_UI4)
      file <- filtered_data()
      req(UI1, UI2, UI3, UI4, file)
      data <- cbind(file[UI1], file[UI2], file[UI3], file[UI4])
      names(data) <- c("Appraiser", "Part", "Trial", "Measures")
      data$Appraiser <- factor(
        data$Appraiser,
        levels = unique(data$Appraiser),
        labels = paste0("Appraiser ", unique(data$Appraiser))
      )
      data[with(data, order(Appraiser, Trial, Part)), ]
    })

    output$msa_out <- renderUI({
      req(filtered_data())
      level <- input$msa_level
      type <- input$msa_type
      standard <- input$msa_stand
      calc <- input$msa_calc
      use_range <- input$msa_range_b
      spec_range <- input$msa_range
      USL <- input$msa_USL
      LSL <- input$msa_LSL
      sigmas <- input$msa_sigmas
      conf <- input$conf_msa
      R <- input$deci_msa
      diagnostic <- input$msa_diagnostic

      if (sigmas) {
        sigma <- 6
      } else {
        sigma <- 5.15
      }

      trigger <- input$msa_c_go
      button_test <- isolate(button_state_c())
      if (!button_test && !isTRUE(diagnostic)) {
        return(HTML("<hr><h3>Set up study and click the Perform Analysis button</h3><hr>"))
      }

      if (use_range) {
        spec_range <- USL - LSL
      }

      data <- msa_data_r()
      req(data)

      app_id <- unique(data$Appraiser)
      app_num(length(app_id))
      trials <- unique(data$Trial)
      parts <- unique(data$Part)
      by_ap_part <- group_by(.data = data, Appraiser, Part)

      if (calc == 1) {
        R_bar <- NULL
        X_bar <- NULL
        by_ap_part <- mutate(by_ap_part, Range = max(Measures) - min(Measures), Mean = mean(Measures))
        for (app in app_id) {
          R_bar[app] <- mean(by_ap_part$Range[by_ap_part$Appraiser == app])
          X_bar[app] <- mean(by_ap_part$Measures[by_ap_part$Appraiser == app])
        }
        R_t <- mean(R_bar)
        R_x <- max(X_bar) - min(X_bar)

        sig_rpd(R_x / d2.star(g = 1, m = length(app_id)))
        if (length(app_id) == 1) {
          sig_rpd(0)
        }
        sig_rpt(R_t / d2.star(g = length(parts) * length(app_id), m = length(trials)))
        sig_e(sqrt(sig_rpt()^2 + sig_rpd()^2))
        pct_EV <- NA
        pct_AV <- NA
        pct_RR <- NA
        if (!is.na(spec_range)) {
          pct_EV <- 100 * sigma * sig_rpt() / spec_range
          pct_AV <- 100 * sigma * sig_rpd() / spec_range
          pct_RR <- 100 * sigma * sig_e() / spec_range
        }

        output_header <- c(
          "<h4>Range Potential MSA</h4><br/>",
          "Confidence Level = ", conf * 100, "%<br/><br/>",
          "<u>Specifications</u><br/>",
          if (!use_range) {
            c("Range = ", spec_range, "<br/>")
          } else {
            c("USL = ", USL, "<br/>LSL = ", LSL, "<br/>")
          },
          "<br/><u>Overall Summary Results</u>",
          "<table style='margin-left:auto;margin-right:auto'>",
          "<tr><td>", paste(withMathJax("$\\sigma_{RPT}=$")), "</td><td>", ro(sig_rpt(), R), "</td><td></td><td>%EV = ", ro(pct_EV, 1), "%</td></tr>",
          "<tr><td>", paste(withMathJax("$\\sigma_{RPD}=$")), "</td><td>", ro(sig_rpd(), R), "</td><td></td><td>%AV = ", ro(pct_AV, 1), "%</td></tr>",
          "<tr><td>", paste(withMathJax("$\\sigma_{e}=$")), "</td><td>", ro(sig_e(), R), "</td><td></td><td>%RR = ", ro(pct_RR, 1), "%</td></tr></table><br/>"
        )
      } else if (calc == 2) {
        R_bar <- NULL
        X_bar <- NULL
        s_bar <- NULL
        by_ap_part <- mutate(by_ap_part, Range = max(Measures) - min(Measures), Mean = mean(Measures), s = sd(Measures))
        for (app in app_id) {
          R_bar[app] <- mean(by_ap_part$Range[by_ap_part$Appraiser == app])
          X_bar[app] <- mean(by_ap_part$Measures[by_ap_part$Appraiser == app])
          s_bar[app] <- mean(by_ap_part$s[by_ap_part$Appraiser == app])
        }
        s_t <- mean(by_ap_part$s)
        R_x <- max(X_bar) - min(X_bar)

        sig_rpd(R_x / d2.star(g = 1, m = length(app_id)))
        if (length(app_id) == 1) {
          sig_rpd(0)
        }
        sig_rpt(s_t / spc.constant.calculation.c4(sample.size = length(trials)))
        sig_e(sqrt(sig_rpt()^2 + sig_rpd()^2))
        pct_EV <- NA
        pct_AV <- NA
        pct_RR <- NA
        if (!is.na(spec_range)) {
          pct_EV <- 100 * sigma * sig_rpt() / spec_range
          pct_AV <- 100 * sigma * sig_rpd() / spec_range
          pct_RR <- 100 * sigma * sig_e() / spec_range
        }

        output_header <- c(
          "<h4>Standard Deviation Potential MSA</h4><br/>",
          "Confidence Level = ", conf * 100, "%<br/><br/>",
          "<u>Specifications</u><br/>",
          if (!use_range) {
            c("Range = ", spec_range, "<br/>")
          } else {
            c("USL = ", USL, "<br/>LSL = ", LSL, "<br/>")
          },
          "<br/><u>Overall Summary Results</u>",
          "<table style='margin-left:auto;margin-right:auto'>",
          "<tr><td>", paste(withMathJax("$\\sigma_{RPT}=$")), "</td><td>", ro(sig_rpt(), R), "</td><td></td><td>%EV = ", ro(pct_EV, 1), "%</td></tr>",
          "<tr><td>", paste(withMathJax("$\\sigma_{RPD}=$")), "</td><td>", ro(sig_rpd(), R), "</td><td></td><td>%AV = ", ro(pct_AV, 1), "%</td></tr>",
          "<tr><td>", paste(withMathJax("$\\sigma_{e}=$")), "</td><td>", ro(sig_e(), R), "</td><td></td><td>%RR = ", ro(pct_RR, 1), "%</td></tr></table><br/>"
        )
      } else if (calc == 3) {
        if (!use_range) {
          LSL <- 0
          USL <- LSL + spec_range
        }

        s_bar <- NULL
        R_bar <- NULL
        X_bar <- NULL
        by_ap_part <- mutate(by_ap_part, Range = max(Measures) - min(Measures), Mean = mean(Measures), s = sd(Measures))
        for (app in app_id) {
          R_bar[app] <- mean(by_ap_part$Range[by_ap_part$Appraiser == app])
          X_bar[app] <- mean(by_ap_part$Measures[by_ap_part$Appraiser == app])
          s_bar[app] <- mean(by_ap_part$s[by_ap_part$Appraiser == app])
        }
        R_t <- mean(R_bar)
        R_x <- max(X_bar) - min(X_bar)
        s_t <- mean(by_ap_part$s)

        anova_pot <- msa.continuous.repeatability.reproducibility(
          measurement = data$Measures,
          part = data$Part,
          appraiser = data$Appraiser,
          conf.level.interaction = conf,
          stat.lsl = LSL,
          stat.usl = USL
        )

        sig_rpt(anova_pot[["ev.full"]][2, 3])
        sig_rpd(anova_pot[["ev.full"]][3, 3])
        sig_e(anova_pot[["ev.full"]][1, 3])
        sig_part_app(anova_pot[["ev.full"]][5, 3])
        sig_app(anova_pot[["ev.full"]][4, 3])
        pct_EV <- anova_pot[["ev.full"]][2, 6] * (sigma / 6)
        pct_AV <- anova_pot[["ev.full"]][3, 6] * (sigma / 6)
        pct_RR <- anova_pot[["ev.full"]][1, 6] * (sigma / 6)

        output_header <- c(
          "<h4>ANOVA Potential MSA</h4><br/>",
          "Confidence Level = ", conf * 100, "%<br/><br/>",
          "<u>Specifications</u><br/>",
          if (!use_range) {
            c("Range = ", spec_range, "<br/>")
          } else {
            c("USL = ", USL, "<br/>LSL = ", LSL, "<br/>")
          },
          "<br/><u>Overall Summary Results</u>",
          "<table style='margin-left:auto;margin-right:auto'>",
          "<tr><td>", paste(withMathJax("$\\sigma_{RPT}=$")), "</td><td>", ro(sig_rpt(), R), "</td><td></td><td>%EV = ", ro(pct_EV, 1), "%</td></tr>",
          "<tr><td>", paste(withMathJax("$\\sigma_{RPD}=$")), "</td><td>", ro(sig_rpd(), R), "</td><td></td><td>%AV = ", ro(pct_AV, 1), "%</td></tr>",
          "<tr><td>", paste(withMathJax("$\\sigma_{e}=$")), "</td><td>", ro(sig_e(), R), "</td><td></td><td>%RR = ", ro(pct_RR, 1), "%</td></tr></table><br/>"
        )

        msa_anova <- paste0(
          "<h5><u>ANOVA Source Table</u></h5>",
          "<table style='margin-left:auto;margin-right:auto' class='msa-table' style='border-collapse: collapse;'><tr><th>Source</th><th>df</th><th>SS</th><th>MS</th><th>F</th><th>p</th></tr>"
        )

        if (length(app_id) == 1) {
          rows <- seq(1, 3)
          rownames(anova_pot[["summary.aov.full"]][[1]])[3] <- "Total"
        } else {
          rows <- seq(1, 5)
        }

        for (row in rows) {
          msa_anova <- paste0(
            msa_anova,
            "<tr><td>", rownames(anova_pot[["summary.aov.full"]][[1]])[row],
            "</td><td>", anova_pot[["summary.aov.full"]][[1]]$Df[row],
            "</td><td>", ro(anova_pot[["summary.aov.full"]][[1]]$`Sum Sq`[row], R),
            "</td><td>", if (is.na(anova_pot[["summary.aov.full"]][[1]]$`Mean Sq`[row])) {
              ""
            } else {
              ro(anova_pot[["summary.aov.full"]][[1]]$`Mean Sq`[row], R)
            },
            "</td><td>", if (is.na(anova_pot[["summary.aov.full"]][[1]]$`F value`[row])) {
              ""
            } else {
              ro(anova_pot[["summary.aov.full"]][[1]]$`F value`[row], R)
            },
            "</td><td>", if (is.na(anova_pot[["summary.aov.full"]][[1]]$`Pr(>F)`[row])) {
              ""
            } else {
              ro(anova_pot[["summary.aov.full"]][[1]]$`Pr(>F)`[row], R)
            },
            if (!is.na(anova_pot[["summary.aov.full"]][[1]]$`Pr(>F)`[row]) &&
              anova_pot[["summary.aov.full"]][[1]]$`Pr(>F)`[row] < (1 - conf)) {
              "*"
            } else {
              ""
            },
            "</td>"
          )
        }

        output_header <- c(output_header, msa_anova, "</table><br/>")

        msa_comp_var <- paste0(
          "<h5><u>Components of Variation Table</u></h5>",
          "<table style='margin-left:auto;margin-right:auto' class='msa-table' style='border-collapse: collapse;'><tr><th>Source</th><th>Comp. Variance</th><th>% Variance</th><th>Comp. s</th><th>Study Variance</th><th>% Study Variance</th><th>% Tolerance</th></tr>"
        )

        for (row in seq_len(nrow(anova_pot[["ev.full"]]) - 1)) {
          msa_comp_var <- paste0(msa_comp_var, "<tr>")
          for (col in seq_len(ncol(anova_pot[["ev.full"]]))[-1]) {
            if (col == 1) {
              msa_comp_var <- paste0(msa_comp_var, "<td>", anova_pot[["ev.full"]][row, col], "</td>")
            } else {
              msa_comp_var <- paste0(msa_comp_var, "<td>", ro(anova_pot[["ev.full"]][row, col], R), "</td>")
            }
          }
          msa_comp_var <- paste0(msa_comp_var, "</tr>")
        }

        msa_comp_var <- paste0(msa_comp_var, "</table><br/>")
        output_header <- c(output_header, msa_comp_var)
      }

      msa_init({
        req(sig_e())
        sig_e()
      })

      lt_ready(isTRUE(input$msa_type == 3))

      if (!diagnostic) {
        ui_render_flags$msa_out <- TRUE
        return(HTML(paste0(
          "<style>
     .msa-table tr:nth-child(even) { background-color: #f2f2f2; }
     .msa-table td:nth-child(even) { background-color: rgba(173, 216, 230, 0.5); }
     .msa-table td { text-align: center; }
     .msa-table th { text-align: center; }
   </style>",
          output_header
        )))
      }

      #if you want diagnostics, here we go
      
      #repeatability within appraiser
      html_title<-c("<h4>Diagnostic Analysis</h4>")
      
      rep_analysis<-matrix(nrow = 0,ncol = 9)#repeatability within appraiser
      colnames(rep_analysis)<-c("Trials","n","r","p(r)","t(Var)","p(Var)","Diff","t(Means)","p(Means)")
      
      for (i in app_id){#within appraiser
        test_combis<-combn(length(trials),2)
        test_num<-length(test_combis)/2
        tests<-seq(1,test_num)
        app_data<-data[data$Appraiser==i,]
        for(j in tests){#across trials
          cor_this<-cor.pearson.r.onesample(x = app_data$Measures[app_data$Trial==test_combis[1,j]],y =app_data$Measures[app_data$Trial==test_combis[2,j]],null.hypothesis.rho = 0,conf.level = conf)
          t_var_this<-variance.test.twosample.dependent(g1 = app_data$Measures[app_data$Trial==test_combis[1,j]],g2 = app_data$Measures[app_data$Trial==test_combis[2,j]],conf.level = conf)
          t_this<-t.test.twosample.dependent(x1 = app_data$Measures[app_data$Trial==test_combis[1,j]],x2 = app_data$Measures[app_data$Trial==test_combis[2,j]],null.hypothesis.difference = 0,conf.level = conf)
          rep_analysis<-rbind(rep_analysis,c(
            paste0("m",test_combis[1,j]," vs m",test_combis[2,j]),
            ro(cor_this[["estimate"]][["sample.size"]],R),
            ro(cor_this[["estimate"]][["sample.r"]],R),
            ro(cor_this[["p.value"]],R),
            ro(t_var_this[["statistic"]][["t statistic"]],R),
            ro(t_var_this[["p.value"]],R),
            ro(t_this[["estimate"]][["sample.mean"]],R),
            ro(t_this[["statistic"]][["t statistic"]],R),
            ro(t_this[["p.value"]],R)
          )
          )
          rownames(rep_analysis)[nrow(rep_analysis)]<-i
          
        }#end trials loop
      }#end appraiser loop
      #rep_analysis #has the complete repeatability within appraisers analysis in a matrix
      
      app_rep<-NULL
      app_repr<-NULL#in case one appraiser
      apps<-1
      #repeatability across appraisers
      if(length(app_id)!=1){#multiple appraiser
        
         app_combis<-combn(app_id,2)
        
      
      
      #output_repeat_ap<-NULL
      #if(length(app_id)!=1){#skip if one appraiser
      
      app_rep<-matrix(nrow=0,ncol=8)#repeatability across appraisers
      if(calc==1 || calc==2){
        colnames(app_rep)<-c("n","r(Ranges)","p(r)","t(Var R)","p(Var R)","Rmin/Rmax","t(Ranges)","p(Ranges)")
      }
      if(calc==3){
        colnames(app_rep)<-c("n","r(s)","p(r)","t(Var s)","p(Var s)","s min/s max","t(s)","p(s)")
      }
      
        
        
     
      app_repr<-matrix(nrow=0,ncol=8)#reproducibility across appraisers
      colnames(app_repr)<-c("n","r","p(r)","t(Var)","p(Var)","Diff","t(Means)","p(Means)")
      
      
        
        app_num<-length(app_combis)/2
      
      apps<-seq(1,app_num)
      for(i in apps){
        
        if(calc==1){
          cor_this<-cor.pearson.r.onesample(x = by_ap_part$Range[by_ap_part$Appraiser==app_combis[1,i] & by_ap_part$Trial==1],y = by_ap_part$Range[by_ap_part$Appraiser==app_combis[2,i] & by_ap_part$Trial==1],null.hypothesis.rho = 0,conf.level = conf)
          t_var_this<-variance.test.twosample.dependent(g1 = by_ap_part$Range[by_ap_part$Appraiser==app_combis[1,i] & by_ap_part$Trial==1],g2 = by_ap_part$Range[by_ap_part$Appraiser==app_combis[2,i] & by_ap_part$Trial==1],conf.level = conf)
          t_this<-t.test.twosample.dependent(x1 = by_ap_part$Range[by_ap_part$Appraiser==app_combis[1,i] & by_ap_part$Trial==1],x2 = by_ap_part$Range[by_ap_part$Appraiser==app_combis[2,i] & by_ap_part$Trial==1],null.hypothesis.difference = 0,conf.level = conf)
          R_maxmin<-min(R_bar[app_combis[1,i]],R_bar[app_combis[2,i]])/max(R_bar[app_combis[1,i]],R_bar[app_combis[2,i]])
        }
        
        if(calc==2 || calc == 3){
          cor_this<-cor.pearson.r.onesample(x = by_ap_part$s[by_ap_part$Appraiser==app_combis[1,i] & by_ap_part$Trial==1],y = by_ap_part$s[by_ap_part$Appraiser==app_combis[2,i] & by_ap_part$Trial==1],null.hypothesis.rho = 0,conf.level = conf)
          t_var_this<-variance.test.twosample.dependent(g1 = by_ap_part$s[by_ap_part$Appraiser==app_combis[1,i] & by_ap_part$Trial==1],g2 = by_ap_part$s[by_ap_part$Appraiser==app_combis[2,i] & by_ap_part$Trial==1],conf.level = conf)
          t_this<-t.test.twosample.dependent(x1 = by_ap_part$s[by_ap_part$Appraiser==app_combis[1,i] & by_ap_part$Trial==1],x2 = by_ap_part$s[by_ap_part$Appraiser==app_combis[2,i] & by_ap_part$Trial==1],null.hypothesis.difference = 0,conf.level = conf)
          R_maxmin<-min(s_bar[app_combis[1,i]],s_bar[app_combis[2,i]])/max(s_bar[app_combis[1,i]],s_bar[app_combis[2,i]])
        }
        
        app_rep<-rbind(app_rep,c(
          cor_this[["estimate"]][["sample.size"]],
          cor_this[["estimate"]][["sample.r"]],
          cor_this[["p.value"]],
          t_var_this[["statistic"]][["t statistic"]],
          t_var_this[["p.value"]],
          R_maxmin,
          t_this[["statistic"]][["t statistic"]],
          t_this[["p.value"]]
        )
        )
        rownames(app_rep)[nrow(app_rep)]<-paste0(app_combis[1,i]," vs ",app_combis[2,i])
        #reproducibility across appraiser
        
        cor_this<-cor.pearson.r.onesample(x = by_ap_part$Mean[by_ap_part$Appraiser==app_combis[1,i] & by_ap_part$Trial==1],y = by_ap_part$Mean[by_ap_part$Appraiser==app_combis[2,i] & by_ap_part$Trial==1],null.hypothesis.rho = 0,conf.level = conf)
        t_var_this<-variance.test.twosample.dependent(g1 = by_ap_part$Mean[by_ap_part$Appraiser==app_combis[1,i] & by_ap_part$Trial==1],g2 = by_ap_part$Mean[by_ap_part$Appraiser==app_combis[2,i] & by_ap_part$Trial==1],conf.level = conf)
        t_this<-t.test.twosample.dependent(x1 = by_ap_part$Mean[by_ap_part$Appraiser==app_combis[1,i] & by_ap_part$Trial==1],x2 = by_ap_part$Mean[by_ap_part$Appraiser==app_combis[2,i] & by_ap_part$Trial==1],null.hypothesis.difference = 0,conf.level = conf)
        
        app_repr<-rbind(app_repr,c(
          cor_this[["estimate"]][["sample.size"]],
          cor_this[["estimate"]][["sample.r"]],
          cor_this[["p.value"]],
          t_var_this[["statistic"]][["t statistic"]],
          t_var_this[["p.value"]],
          t_this[["estimate"]][["sample.mean"]],
          t_this[["statistic"]][["t statistic"]],
          t_this[["p.value"]]
        )
        )
        
        rownames(app_repr)[nrow(app_repr)]<-paste0(app_combis[1,i]," vs ",app_combis[2,i])
        
      }
      
    }#end multi appraiser
      
      # app_rep has the repeatabilty across appraiser
      # app_repr has the reproducibility across appraiser
      #have all information for output for potential study
      #potential study html output
      
      app_rep<-ro(app_rep,R)
      app_repr<-ro(app_repr,R)
      repeat_width<-seq(1,9)
      
      
      
      output_repeat_ap<-c("<h5><b><u>Repeatability (Variation Measuring the Same Items Within and Across Appraiser)</b></u></h5>")
      
      for(i in as.numeric(app_id)){#each appraiser
        output_repeat_ap<-c(output_repeat_ap,
                            "<u><b>",rownames(rep_analysis)[(i-1)*test_num+1]," Repeatability Analysis</b></u><br/>",
                            "<table style='margin-left:auto;margin-right:auto' class='msa-table' style='border-collapse: collapse;'><tr><th>Trials</th><th>n</th><th>r</th><th>p(r)</th><th>t(Var)</th><th>p(Var)</th><th>Diff</th><th>t(Means)</th><th>p(Means)</th></tr>"
        )
        
        for(j in tests){#for each comparison we have a row
          output_repeat_ap<-c(output_repeat_ap,"<tr>")
          for(k in repeat_width){#across table row
            if(k==4 || k==6 || k==9){
              if(as.numeric(rep_analysis[(test_num*(i-1)+j),k]) < (1-conf)){
                output_repeat_ap<-c(output_repeat_ap,
                                    "<td>",rep_analysis[(test_num*(i-1)+j),k],"*</td>")
              } else {
                output_repeat_ap<-c(output_repeat_ap,
                                    "<td>",rep_analysis[(test_num*(i-1)+j),k],"</td>")
              }
            } else {
              output_repeat_ap<-c(output_repeat_ap,
                                  "<td>",rep_analysis[(test_num*(i-1)+j),k],"</td>")
            }
            
            
          }#end row of table
          output_repeat_ap<-c(output_repeat_ap,"</tr>")
          
        }#end tests for
        output_repeat_ap<-c(output_repeat_ap,"</table><br/>")
      }#end app_id for
      
      #repeatability by appraiser 
      output_repsys_ap<-NULL
      if(length(app_id)!=1){#single appraiser
      repeat_width<-seq(1,8)
      
      if(calc==1){
        output_repsys_ap<-c(output_repsys_ap,
                            "<br/><u><b>System Repeatability Analysis</b></u><br/>",
                            "<table style='margin-left:auto;margin-right:auto' class='msa-table' style='border-collapse: collapse;'><tr><th>Appraisers</th><th>n</th><th>r(Range)</th><th>p(r)</th><th>t(Var Range)</th><th>p(Var Range)</th><th>Rmin/Rmax</th><th>t(Range)</th><th>p(Range)</th></tr>"
        )
      }
      if(calc==2 || calc==3){
        output_repsys_ap<-c(output_repsys_ap,
                            "<br/><u><b>System Repeatability Analysis</b></u><br/>",
                            "<table style='margin-left:auto;margin-right:auto' class='msa-table' style='border-collapse: collapse;'><tr><th>Appraisers</th><th>n</th><th>r(s)</th><th>p(r)</th><th>t(Var s)</th><th>p(Var s)</th><th>s min/s max</th><th>t(s)</th><th>p(s)</th></tr>"
        )
      }
      
      for(j in apps){#for each comparison we have a row
        output_repsys_ap<-c(output_repsys_ap,"<tr><td>",rownames(app_rep)[j],"</td>")
        for(k in repeat_width){#across table row
          if(k==3 || k==5 || k==8){
            if(as.numeric(app_rep[j,k]) < (1-conf)){
              output_repsys_ap<-c(output_repsys_ap,
                                  "<td>",app_rep[j,k],"*</td>")
            } else {
              output_repsys_ap<-c(output_repsys_ap,
                                  "<td>",app_rep[j,k],"</td>")
            }
          } else {
            output_repsys_ap<-c(output_repsys_ap,
                                "<td>",app_rep[j,k],"</td>")
          }
          
        }#end row of table
        output_repsys_ap<-c(output_repsys_ap,"</tr>")
        
      }#end tests for
      output_repsys_ap<-c(output_repsys_ap,"</table><br/>")
      #}#end app_id for
      }
      #overall repeatability by appraiser and overall
      output_repsys<-NULL
      if(calc==1){
        output_repsys<-c(output_repsys,"<br/>",
                         "<table style='margin-left:auto;margin-right:auto'  class='msa-table' style='border-collapse: collapse;'><tr><th>Appraiser</th><th>Repeated Measures</th><th>n</th><th>",paste(withMathJax("$\\bar{R}$")),"</th><th>",paste(withMathJax("$\\sigma_{EV}$")),"</th><th>",paste0(sigma,withMathJax("$\\sigma_{EV}$")),"</th><th>%EV</th></tr>"
        )
      }
      if(calc==2 || calc==3){
        output_repsys<-c(output_repsys,"<br/>",
                         "<table style='margin-left:auto;margin-right:auto' class='msa-table' style='border-collapse: collapse;'><tr><th>Appraiser</th><th>Repeated Measures</th><th>n</th><th>",paste(withMathJax("$\\bar{s}$")),"</th><th>",paste(withMathJax("$\\sigma_{EV}$")),"</th><th>",paste0(sigma,withMathJax("$\\sigma_{EV}$")),"</th><th>%EV</th></tr>"
        )
      }
      
      repeat_width<-seq(1,7)
      
      for(i in app_id){
        g<-length(unique(by_ap_part$Part[by_ap_part$Appraiser==i]))
        m<-length(unique(by_ap_part$Trial[by_ap_part$Appraiser==i]))
        if(calc==1){
          std_rpt<-ro(R_bar[i]/d2.star(g = g,m = m),R)
        } else if (calc==2 || calc==3){
          std_rpt<-ro(s_bar[i]/spc.constant.calculation.c4(m),R)
        }
        nt_ev<-ro(sigma*std_rpt,R)
        if(calc==1){
          output_repsys<-c(output_repsys,"<tr><td>",i,"</td>",
                           "<td>",m,"</td>",
                           "<td>",g,"</td>",
                           "<td>",ro(R_bar[i],R),"</td>",
                           "<td>",std_rpt,"</td>",
                           "<td>",nt_ev,"</td>",
                           "<td>",ro(100*nt_ev/spec_range,1),"%</td></tr>"
          )
        }
        if(calc==2 || calc==3){
          output_repsys<-c(output_repsys,"<tr><td>",i,"</td>",
                           "<td>",m,"</td>",
                           "<td>",g,"</td>",
                           "<td>",ro(s_bar[i],R),"</td>",
                           "<td>",std_rpt,"</td>",
                           "<td>",nt_ev,"</td>",
                           "<td>",ro(100*nt_ev/spec_range,1),"%</td></tr>"
          )
        }
      }#end loop through app_id
      
      #add overall row
      g<-length(parts)
      m<-length(trials)
      if(calc==1){
        output_repsys<-c(output_repsys,"<tr><td>","Overall</td>",
                         "<td>",m,"</td>",
                         "<td>",g,"</td>",
                         "<td>",ro(R_t,R),"</td>",
                         "<td>",ro(sig_rpt(),R),"</td>",
                         "<td>",ro(sigma*sig_rpt(),R),"</td>",
                         "<td>",ro(100*sigma*sig_rpt()/spec_range,1),"%</td></tr></table>"
        )
      }
      if(calc==2 || calc==3){
        output_repsys<-c(output_repsys,"<tr><td>","Overall</td>",
                         "<td>",m,"</td>",
                         "<td>",g,"</td>",
                         "<td>",ro(s_t,R),"</td>",#steve
                         "<td>",ro(sig_rpt(),R),"</td>",
                         "<td>",ro(sigma*sig_rpt(),R),"</td>",
                         "<td>",ro(100*sigma*sig_rpt()/spec_range,1),"%</td></tr></table>"
        )
      }
      #output_repsys<-unlist(output_repsys)
      #end of repeatability
      
      #reproducibility
      output_repro_head<-NULL
      output_repro_app<-NULL
      output_system_repro<-NULL
      
      if(length(app_id) != 1){#skip if single appraiser
      output_repro_head<-"<br/><h5><b><u>Reproducibility (Variation in Means of the Same Items Between and Across Appraisers)</b></u></h5>"
      
      output_repro_app<-"<table style='margin-left:auto;margin-right:auto' class='msa-table' style='border-collapse: collapse;'><tr><th>Trials</th><th>n</th><th>r</th><th>p(r)</th><th>t(Var)</th><th>p(Var)</th><th>Diff</th><th>t(Means)</th><th>p(Means)</th></tr>"
      repeat_width<-seq(1,8)
      
      #reproducibility across operators
      for(i in apps){#each appraiser
        output_repro_app<-c(output_repro_app,"<tr><td>",rownames(app_repr)[i],"</td>")
        
        for(k in repeat_width){#across table row
          if(k==3 || k==5 || k==8){
            if(as.numeric(app_repr[i,k]) < (1-conf)){
              output_repro_app<-c(output_repro_app,
                                  "<td>",app_repr[i,k],"*</td>")
            } else {
              output_repro_app<-c(output_repro_app,
                                  "<td>",app_repr[i,k],"</td>")
            }
          } else {
            output_repro_app<-c(output_repro_app,
                                "<td>",app_repr[i,k],"</td>")
          }
          
          
        }#end row of table
        output_repro_app<-c(output_repro_app,"</tr>")
      }
      output_repro_app<-c(output_repro_app,"</table>")
      
      
      #######################
      output_system_repro<-c("<br/><u><b>System Reproducibility Analysis</b></u><br/>",
                             "<table style='margin-left:auto;margin-right:auto' class='msa-table' style='border-collapse: collapse;'><tr><th></th><th>Range</th><th>",paste0(withMathJax("$\\sigma_{AV}$")),"</th><th>",paste0(sigma,withMathJax("$\\sigma_{AV}$")),"</th><th>%AV</th></tr>",
                             "<tr><td>Overall</td><td>",ro(R_x,R),"</td><td>",ro(sig_rpd(),R),"</td><td>",ro(sigma*sig_rpd(),R),"</td><td>",ro(pct_AV,1),"%</td></tr></table>"
      )
      #output_system_repro<-unlist(output_system_repro)
      
      }#end if for not single appraiser
      output_system_all<-c("<br/><u><b>Total System Measurement Error</b></u><br/>",
                           "<table style='margin-left:auto;margin-right:auto' class='msa-table' style='border-collapse: collapse;'><tr><th></th><th>%EV</th><th>%AV</th><th>",paste0(sigma,withMathJax("$\\sigma_{e}$")),"</th><th>%R&R</th></tr>",
                           "<tr><td>Overall</td><td>",ro(pct_EV,1),"%</td><td>",ro(pct_AV,1),"%</td><td>",ro(sigma*sig_e(),R),"</td><td>",ro(pct_RR,1),"%</td></tr></table>"
      )
      
      #######################
      
      #final output with diagnostics
      ui_render_flags$msa_out <- TRUE
      
      output<-HTML(c("<style>
     .msa-table tr:nth-child(even) { background-color: #f2f2f2; }
     .msa-table td:nth-child(even) { background-color: rgba(173, 216, 230, 0.5); }
     .msa-table td { text-align: center; }
     .msa-table th { text-align: center; }
   </style>",
        output_header,html_title,output_repeat_ap,output_repsys_ap,output_repsys,output_repro_head,output_repro_app,output_system_repro,output_system_all))
      sendSweetAlert(title = "Detailed Diagnostics",text = output,html = TRUE,width = "100%",showCloseButton = TRUE,btn_labels = "Close")
      updateSwitchInput(inputId = "msa_diagnostic",value = FALSE)
      HTML(output_header)
    })

    d2.star <- function(g = 2, m = 2) {
      d2.star.tab <- c(1.41421, 1.91155, 2.23887, 2.48124, 2.67253, 2.82981, 2.96288, 3.07794, 3.17905, 3.26909, 3.35016, 3.42378, 3.49116, 3.55333, 3.61071, 3.66422, 3.71424, 3.76118, 3.80537, 1.27931, 1.80538, 2.15069, 2.40484, 2.60438, 2.76779, 2.90562, 3.02446, 3.12869, 3.22134, 3.30463, 3.38017, 3.44922, 3.51287, 3.57156, 3.62625, 3.67734, 3.72524, 3.77032, 1.23105, 1.76858, 2.12049, 2.37883, 2.58127, 2.74681, 2.88628, 3.00643, 3.11173, 3.20526, 3.28931, 3.3655, 3.43512, 3.49927, 3.55842, 3.61351, 3.66495, 3.71319, 3.75857, 1.20621, 1.74989, 2.10522, 2.36571, 2.56964, 2.73626, 2.87656, 2.99737, 3.10321, 3.1972, 3.28163, 3.35815, 3.42805, 3.49246, 3.55183, 3.60712, 3.65875, 3.70715, 3.75268, 1.19105, 1.73857, 2.09601, 2.35781, 2.56263, 2.72991, 2.87071, 2.99192, 3.09808, 3.19235, 3.27701, 3.35372, 3.42381, 3.48836, 3.54787, 3.60328, 3.65502, 3.70352, 3.74914, 1.18083, 1.73099, 2.08985, 2.35253, 2.55795, 2.72567, 2.8668, 2.98829, 3.09467, 3.18911, 3.27392, 3.35077, 3.42097, 3.48563, 3.54522, 3.60072, 3.65253, 3.70109, 3.74678, 1.17348, 1.72555, 2.08543, 2.34875, 2.5546, 2.72263, 2.86401, 2.98568, 3.09222, 3.18679, 3.27172, 3.34866, 3.41894, 3.48368, 3.54333, 3.59888, 3.65075, 3.69936, 3.74509, 1.16794, 1.72147, 2.08212, 2.34591, 2.55208, 2.72036, 2.86192, 2.98373, 3.09039, 3.18506, 3.27006, 3.34708, 3.41742, 3.48221, 3.54192, 3.59751, 3.64941, 3.69806, 3.74382, 1.16361, 1.71828, 2.07953, 2.3437, 2.55013, 2.71858, 2.86028, 2.98221, 3.08896, 3.1837, 3.26878, 3.34585, 3.41624, 3.48107, 3.54081, 3.59644, 3.64838, 3.69705, 3.74284, 1.16014, 1.71573, 2.07746, 2.34192, 2.54856, 2.71717, 2.85898, 2.981, 3.08781, 3.18262, 3.26775, 3.34486, 3.41529, 3.48016, 3.53993, 3.59559, 3.64755, 3.69625, 3.74205, 1.15729, 1.71363, 2.07577, 2.34048, 2.54728, 2.716, 2.85791, 2.98, 3.08688, 3.18174, 3.2669, 3.34406, 3.41452, 3.47941, 3.53921, 3.59489, 3.64687, 3.69558, 3.74141, 1.1549, 1.71189, 2.07436, 2.33927, 2.54621, 2.71504, 2.85702, 2.97917, 3.0861, 3.181, 3.2662, 3.34339, 3.41387, 3.47879, 3.53861, 3.5943, 3.6463, 3.69503, 3.74087, 1.15289, 1.71041, 2.07316, 2.33824, 2.5453, 2.71422, 2.85627, 2.97847, 3.08544, 3.18037, 3.26561, 3.34282, 3.41333, 3.47826, 3.5381, 3.59381, 3.64582, 3.69457, 3.74041, 1.15115, 1.70914, 2.07213, 2.33737, 2.54452, 2.71351, 2.85562, 2.97787, 3.08487, 3.17984, 3.2651, 3.34233, 3.41286, 3.47781, 3.53766, 3.59339, 3.64541, 3.69417, 3.74002, 1.14965, 1.70804, 2.07125, 2.33661, 2.54385, 2.7129, 2.85506, 2.97735, 3.08438, 3.17938, 3.26465, 3.34191, 3.41245, 3.47742, 3.53728, 3.59302, 3.64505, 3.69382, 3.73969, 1.14833, 1.70708, 2.07047, 2.33594, 2.54326, 2.71237, 2.85457, 2.97689, 3.08395, 3.17897, 3.26427, 3.34154, 3.4121, 3.47707, 3.53695, 3.5927, 3.64474, 3.69351, 3.73939, 1.14717, 1.70623, 2.06978, 2.33535, 2.54274, 2.7119, 2.85413, 2.97649, 3.08358, 3.17861, 3.26393, 3.34121, 3.41178, 3.47677, 3.53666, 3.59242, 3.64447, 3.69325, 3.73913, 1.14613, 1.70547, 2.06917, 2.33483, 2.54228, 2.71148, 2.85375, 2.97613, 3.08324, 3.17829, 3.26362, 3.34092, 3.4115, 3.4765, 3.5364, 3.59216, 3.64422, 3.69301, 3.7389, 1.1452, 1.7048, 2.06862, 2.33436, 2.54187, 2.71111, 2.85341, 2.97581, 3.08294, 3.17801, 3.26335, 3.34066, 3.41125, 3.47626, 3.53617, 3.59194, 3.644, 3.6928, 3.73869, 1.14437, 1.70419, 2.06813, 2.33394, 2.54149, 2.71077, 2.8531, 2.97552, 3.08267, 3.17775, 3.26311, 3.34042, 3.41103, 3.47605, 3.53596, 3.59174, 3.6438, 3.6926, 3.7385, 1.12838, 1.69257, 2.05875, 2.32593, 2.53441, 2.70436, 2.8472, 2.97003, 3.07751, 3.17287, 3.25846, 3.33598, 3.40676, 3.47193, 3.53198, 3.58788, 3.64006, 3.68896, 3.735)
      if (g > 20) g <- 21
      if (m > 20) m <- 20
      d2.star.tab[(m - 1) + ((g - 1) * 19)]
    }

    output$msadangerzone <- renderPlot({
      ui_render_flags$msadangerzone <- TRUE
      msa_dangerzone()
    })
    msadangerzone_height <- reactive(400 * 4)
    msadangerzone_width <- reactive(400 * 8)
    downloadServer("msadangerzone", msa_dangerzone, height = msadangerzone_height, width = msadangerzone_width)

    msa_dangerzone <- reactive({
      if (!isTRUE(input$msa_graphs)) {
        return()
      }

      proc_mean <- input$proc_mean
      proc_std <- input$proc_std
      sigmas <- input$msa_sigmas
      R <- input$deci_msa
      use_range <- input$msa_range_b
      spec_range <- input$msa_range
      USL <- input$msa_USL
      LSL <- input$msa_LSL
      UI4 <- as.numeric(UI4_d())

      validate(
        need(sig_e(), "No measurement error calculated"),
        need(proc_mean, "Need to enter a process mean"),
        need(proc_std, "Need to enter a process standard devation without measurement error"),
        need(!(use_range && (is.na(USL) && is.na(LSL))), "Need to enter specifications"),
        need(!(!use_range && is.na(spec_range)), "Need to enter specification range")
      )

      if (sigmas) {
        sigma <- 6
      } else {
        sigma <- 5.15
      }

      if (!use_range) {
        USL <- proc_mean + spec_range / 2
        LSL <- proc_mean - spec_range / 2
      }

      no_USL <- FALSE
      no_LSL <- FALSE

      if (use_range && is.na(USL)) {
        no_USL <- TRUE
      }
      if (use_range && is.na(LSL)) {
        no_LSL <- TRUE
      }

      if (use_range) {
        spec_range <- USL - LSL
      }

      x_lim <- c(proc_mean - (4 * proc_std), proc_mean + (4 * proc_std))
      y_dat <- dnorm(x = x_lim, mean = proc_mean, sd = proc_std)
      data <- as.data.frame(cbind(x_lim, y_dat))
      norm_max <- dnorm(x = proc_mean, mean = proc_mean, sd = proc_std)

      if (use_range) {
        if (is.na(input$msa_LSL)) {
          real_LSL <- Inf
        } else {
          real_LSL <- input$msa_LSL
        }
        if (is.na(input$msa_USL)) {
          real_USL <- Inf
        } else {
          real_USL <- input$msa_USL
        }
      } else {
        real_LSL <- LSL
        real_USL <- USL
      }

      result_in_spec <- integrate(
        total_misclass_in_spec_prob,
        lower = real_LSL,
        upper = real_USL,
        mu_p = proc_mean,
        sigma_p = proc_std,
        LSL = real_LSL,
        USL = real_USL,
        sigma_m = sig_e()
      )
      result_out_spec_below <- integrate(
        total_misclass_out_of_spec_prob,
        lower = -Inf,
        upper = real_LSL,
        mu_p = proc_mean,
        sigma_p = proc_std,
        LSL = real_LSL,
        USL = real_USL,
        sigma_m = sig_e()
      )
      result_out_spec_above <- integrate(
        total_misclass_out_of_spec_prob,
        lower = real_USL,
        upper = Inf,
        mu_p = proc_mean,
        sigma_p = proc_std,
        LSL = real_LSL,
        USL = real_USL,
        sigma_m = sig_e()
      )
      total_misclass_prob <- result_in_spec$value + result_out_spec_below$value + result_out_spec_above$value

      color <- colors()
      col_fill <- color[3]
      p <- ggplot(data = data.frame(x_axis = seq(x_lim[1], x_lim[2], length.out = 10000)), aes(x = x_axis))

      if (!no_USL) {
        danger <- ggplot_dangerzone(USL, sig_e(), norm_max, 10000)
        p <- p +
          geom_rect(
            data = danger,
            aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, alpha = alpha),
            fill = color[3],
            inherit.aes = FALSE
          ) +
          guides(alpha = "none") +
          geom_vline(aes(xintercept = USL), linetype = 5, color = color[2])
      }

      if (!no_LSL) {
        danger <- ggplot_dangerzone(LSL, sig_e(), norm_max, 10000)
        p <- p +
          geom_rect(
            data = danger,
            aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, alpha = alpha),
            fill = color[4],
            inherit.aes = FALSE
          ) +
          guides(alpha = "none") +
          geom_vline(aes(xintercept = LSL), linetype = 5, color = color[2])
      }

      dis_cat <- msa.postprocess.ev.number.distinct.categories.simple(study.variation.part.part = proc_std, study.variation.total = sig_e())
      dis_rat <- msa.postprocess.ev.number.discrimination.ratio.simple(component.variance.total = proc_std^2 + sig_e()^2, component.variance.gage.r.r = sig_e()^2)
      p <- p +
        geom_area(stat = "function", fun = dnorm, args = list(mean = proc_mean, sd = proc_std), color = color[4], fill = color[4], alpha = 0.2) +
        annotate(geom = "label", label = "Process without Measurement Error", x = proc_mean, y = 0.25 * norm_max) +
        annotate(geom = "text", label = paste0("Process Mean = ", proc_mean), x = -Inf, y = 1 * norm_max, hjust = "left") +
        annotate(geom = "text", label = paste0("Process s =", proc_std), x = -Inf, y = 0.95 * norm_max, hjust = "left") +
        annotate(geom = "text", label = paste0("Measurement Error = ", ro(sig_e(), R)), x = -Inf, y = 0.9 * norm_max, hjust = "left") +
        annotate(geom = "text", label = paste0("Proportion M.E. = ", ro(sig_e()^2 / proc_std^2, R)), x = -Inf, y = 0.85 * norm_max, hjust = "left") +
        annotate(geom = "text", label = paste0(sigma, " sigmas"), x = -Inf, y = 0.75 * norm_max, hjust = "left") +
        annotate(geom = "text", label = paste0("Discrete Categories = ", ro(dis_cat, R)), x = -Inf, y = 0.7 * norm_max, hjust = "left") +
        annotate(geom = "text", label = paste0("Discrimination Ratio = ", ro(dis_rat, R)), x = -Inf, y = 0.65 * norm_max, hjust = "left") +
        labs(
          x = "True Value",
          y = "PDF",
          title = "Misclassification Scenario",
          caption = "Assuming the process being measured and the measurement error are in control and normally distributed",
          subtitle = paste0(
            "Misclassified as in spec = ", ro(100 * (result_out_spec_above$value + result_out_spec_below$value), 2), "%",
            " Misclassified as out of spec = ", ro(100 * result_in_spec$value, 2), "%",
            " Total % Misclassified = ", ro(100 * total_misclass_prob, 2), "%"
          )
        )

      if (!is.na(spec_range)) {
        p <- p + annotate(geom = "text", label = paste0(ro(100 * sigma * sig_e() / spec_range, 1), "%R&R"), x = -Inf, y = 0.8 * norm_max, hjust = "left")
      }
      if (!no_USL) {
        p <- p +
          annotate(geom = "label", label = "USL", x = USL, y = 0.4 * norm_max) +
          annotate(geom = "label", label = "USL Danger Zone", x = USL, y = norm_max / 2)
      }
      if (!no_LSL) {
        p <- p +
          annotate(geom = "label", label = "LSL", x = LSL, y = 0.4 * norm_max) +
          annotate(geom = "label", label = "LSL Danger Zone", x = LSL, y = norm_max / 2)
      }
      p
    })

    ggplot_dangerzone <- function(mean, sd, max, divisions = 100) {
      x_steps <- seq(mean - 4 * sd, mean + 4 * sd, length.out = divisions + 1)
      alpha_steps <- dnorm(x_steps[-(divisions + 1)], mean = mean, sd = sd)
      alpha_steps[divisions / 2] <- 2 * alpha_steps[divisions / 2]
      y_min <- rep(0, divisions)
      y_max <- rep(max, divisions)
      data.frame(
        xmin = x_steps[-(divisions + 1)],
        xmax = x_steps[-1],
        alpha = alpha_steps,
        ymin = y_min,
        ymax = y_max
      )
    }

    output$msavarcompgraph <- renderPlot({
      ui_render_flags$msavarcompgraph <- TRUE
      msa_var_comp_graph()
    })
    msavarcompgraph_width <- reactive(400 * 4)
    msavarcompgraph_height <- reactive(400 * 4)
    downloadServer("msavarcompgraph", msa_var_comp_graph, height = msavarcompgraph_height, width = msavarcompgraph_width)

    msa_UI1 <- reactive({ input$msa_UI1 })
    UI1_d <- debounce(msa_UI1, millis = 500)
    msa_UI2 <- reactive({ input$msa_UI2 })
    UI2_d <- debounce(msa_UI2, millis = 500)
    msa_UI3 <- reactive({ input$msa_UI3 })
    UI3_d <- debounce(msa_UI3, millis = 500)
    msa_UI4 <- reactive({ input$msa_UI4 })
    UI4_d <- debounce(msa_UI4, millis = 500)

    msa_var_comp_graph <- reactive({
      calc <- input$msa_calc
      level <- input$msa_level
      if (level == 2) {
        return()
      }
      UI1 <- as.numeric(UI1_d())
      UI2 <- as.numeric(UI2_d())
      UI3 <- as.numeric(UI3_d())
      UI4 <- as.numeric(UI4_d())
      if (length(UI4) == 0 || UI3 == UI4) {
        return(HTML("Waiting"))
      }
      data <- msa_data_r()
      validate(need(all(!is.na(data)), "Missing data"))
      req(msa_init(), calc, UI1, UI2, UI3, UI4, data)

      button_test <- isolate(button_state_c())
      if (!button_test) {
        return(HTML("<h3>Set up study and click the Perform Analysis button</h3>"))
      }

      if (!is.numeric(sig_e()) || sig_e() == 0) {
        return()
      }

      color <- colors()
      if (calc == 1 || calc == 2) {
        sig_rpd_2 <- sig_rpd()^2
        sig_rpt_2 <- sig_rpt()^2
        sig_e_2 <- sig_e()^2
        pct_rpd <- ro(100 * sig_rpd_2 / sig_e_2, 2)
        pct_rpt <- ro(100 * sig_rpt_2 / sig_e_2, 2)

        data <- as.data.frame(rbind(pct_rpt, pct_rpd))
        data <- cbind(x = c("Total Measurement Variance", "Total Measurement Variance"), y = data, Group = c("Repeatability", "Reproducibility"))
        colnames(data) <- c("x", "y", "Source")

        ggplot(data = data, aes(x = x, y = y, fill = Source, label = y)) +
          geom_bar(stat = "identity") +
          geom_label(aes(group = Source), fill = "white", label = paste0(data$y, "%"), position = position_stack(vjust = 0.5)) +
          scale_y_continuous(labels = function(x) paste0(x, "%")) +
          scale_fill_manual(values = color[-1]) +
          labs(title = "Components of Variation", x = "", y = "% of Measurement Error Variance")
      } else {
        sig_rpd_2 <- sig_rpd()^2
        sig_rpt_2 <- sig_rpt()^2
        sig_e_2 <- sig_e()^2
        sig_app_2 <- sig_app()^2
        sig_part_app_2 <- sig_part_app()^2
        pct_rpd <- ro(100 * sig_rpd_2 / sig_e_2, 2)
        pct_rpt <- ro(100 * sig_rpt_2 / sig_e_2, 2)
        pct_app <- ro(100 * sig_app_2 / sig_e_2, 2)
        pct_part_app <- ro(100 * sig_part_app_2 / sig_e_2, 2)

        data <- as.data.frame(rbind(pct_rpt, pct_rpd, pct_app, pct_part_app))
        x <- factor(c("Measurement Variance", "Measurement Variance", "Reproducibility", "Reproducibility"), levels = c("Measurement Variance", "Reproducibility"))
        Source <- factor(c("Repeatability", "Reproducibility", "Appraiser", "Appraiser x Part"), levels = c("Repeatability", "Reproducibility", "Appraiser", "Appraiser x Part"))
        data <- cbind(x, y = data, Source)
        colnames(data) <- c("x", "y", "Source")

        ggplot(data = data, aes(x = x, y = y, fill = Source, label = y)) +
          geom_bar(stat = "identity") +
          geom_label(aes(group = Source), fill = "white", label = paste0(data$y, "%"), position = position_stack(vjust = 0.5)) +
          scale_y_continuous(labels = function(x) paste0(x, "%")) +
          scale_fill_manual(values = color[-1]) +
          labs(title = "Components of Variation", x = "", y = "% of Measurement Error Variance")
      }
    })

    msa_scatter <- reactive({
      calc <- input$msa_calc
      level <- input$msa_level
      if (level == 2) {
        return()
      }
      UI1 <- as.numeric(UI1_d())
      UI2 <- as.numeric(UI2_d())
      UI3 <- as.numeric(UI3_d())
      UI4 <- as.numeric(UI4_d())
      if (length(UI4) == 0 || UI3 == UI4) {
        return(HTML("Waiting"))
      }
      R <- input$deci_msa
      conf <- input$conf_msa
      data <- msa_data_r()

      if (is.atomic(data)) {
        return(HTML("Need data"))
      }
      req(msa_init(), calc, UI1, UI2, UI3, UI4, data)

      button_test <- isolate(button_state_c())
      if (!button_test) {
        return(HTML("<h3>Set up study and click the Perform Analysis button</h3>"))
      }

      if (!is.numeric(data$Part) || !is.numeric(data$Trial) || !is.numeric(data$Measures)) {
        return(HTML("Need numeric data"))
      }

      by_ap_part <- group_by(.data = data, Appraiser, Part)
      by_ap_part <- mutate(by_ap_part, Range = max(Measures) - min(Measures), Mean = mean(Measures), sd = sd(Measures))
      by_ap_part$Appraiser <- factor(by_ap_part$Appraiser)

      plot_data <- by_ap_part[which(by_ap_part$Trial == 1), ][-4][-3]
      plot_data$Appraiser <- factor(plot_data$Appraiser)
      if (nrow(plot_data) / length(unique(plot_data$Appraiser)) < 3) {
        return()
      }
      app_id <- as.factor(unique(by_ap_part$Appraiser))
      msa_r <- NULL
      msa_p <- NULL

      if (calc == 1) {
        p <- ggplot(data = plot_data, aes(x = Mean, y = Range, color = Appraiser, shape = Appraiser, linetype = Appraiser))
        for (i in app_id) {
          r <- cor.pearson.r.onesample(x = plot_data$Mean[which(plot_data$Appraiser == i)], y = plot_data$Range[which(plot_data$Appraiser == i)])
          msa_r[i] <- ro(r[["estimate"]][["sample.r"]], R)
          msa_p[i] <- ro(r[["p.value"]], R)
          p <- p + geom_smooth(method = "lm", se = FALSE)
        }
        min_x <- min(plot_data$Mean)
        max_y <- max(plot_data$Range)
        max_x <- max(plot_data$Mean)
        min_y <- min(plot_data$Range)
      } else if (calc == 2) {
        p <- ggplot(data = plot_data, aes(x = Mean, y = sd, color = Appraiser, shape = Appraiser, linetype = Appraiser))
        for (i in app_id) {
          r <- cor.pearson.r.onesample(x = plot_data$Mean[which(plot_data$Appraiser == i)], y = plot_data$sd[which(plot_data$Appraiser == i)])
          msa_r[i] <- ro(r[["estimate"]][["sample.r"]], R)
          msa_p[i] <- ro(r[["p.value"]], R)
          p <- p + geom_smooth(method = "lm", se = FALSE)
        }
        min_x <- min(plot_data$Mean)
        max_y <- max(plot_data$sd)
        max_x <- max(plot_data$Mean)
        min_y <- min(plot_data$sd)
      } else if (calc == 3) {
        p <- ggplot(data = plot_data, aes(x = Mean, y = sd, color = Appraiser, linetype = Appraiser))
        for (i in app_id) {
          r <- cor.pearson.r.onesample(x = plot_data$Mean[which(plot_data$Appraiser == i)], y = plot_data$sd[which(plot_data$Appraiser == i)])
          msa_r[i] <- ro(r[["estimate"]][["sample.r"]], R)
          msa_p[i] <- ro(r[["p.value"]], R)
          p <- p + geom_smooth(method = "lm", se = FALSE)
        }
        min_x <- min(plot_data$Mean)
        max_y <- max(plot_data$sd)
        max_x <- max(plot_data$Mean)
        min_y <- min(plot_data$sd)
      }

      p <- p + geom_point(aes(color = Appraiser, shape = Appraiser), size = 5)

      loop <- 1
      for (i in app_id) {
        note <- paste0(i, ": r = ", msa_r[i], " p = ", msa_p[i], if (msa_p[i] < (1 - conf)) {
          "*"
        } else {
          ""
        })
        p <- p +
          annotate(geom = "text", x = min_x, y = max_y - ((loop / 20) * (max_y - min_y)), label = note, hjust = 0) +
          labs(title = "Uniformity of Dispersion", subtitle = "If significant, dispersion changes with magnitude measured")
        loop <- loop + 1
      }
      p
    })

    output$msascatter <- renderPlot({
      ui_render_flags$msascatter <- TRUE
      msa_scatter()
    })
    msascatter_width <- reactive(400 * 4)
    msascatter_height <- reactive(400 * 4)
    downloadServer("msascatter", msa_scatter, height = msascatter_height, width = msascatter_width)

    output$msabox <- renderPlot({
      ui_render_flags$msa_box <- TRUE
      msa_box()
    })
    msabox_width <- reactive(400 * 4)
    msabox_height <- reactive(app_num() * 400 * 4)
    downloadServer("msabox", msa_box, height = msabox_height, width = msabox_width)

    output$msa_box <- renderUI({
      level <- input$msa_level
      if (level == 2) {
        return()
      }
      UI1 <- as.numeric(UI1_d())
      UI2 <- as.numeric(UI2_d())
      UI3 <- as.numeric(UI3_d())
      UI4 <- as.numeric(UI4_d())
      if (length(UI4) == 0 || UI3 == UI4) {
        return(HTML("Waiting"))
      }
      data <- msa_data_r()
      req(msa_init(), UI1, UI2, UI3, UI4, length(data) > 1, app_num() > 0)
      plotOutput(ns("msabox"), height = app_num() * 400)
    })

    msa_box_data <- eventReactive(button_state_c(), {
      calc <- input$msa_calc
      level <- input$msa_level
      if (level == 2) {
        return()
      }
      UI1 <- as.numeric(UI1_d())
      UI2 <- as.numeric(UI2_d())
      UI3 <- as.numeric(UI3_d())
      UI4 <- as.numeric(UI4_d())
      if (length(UI4) == 0 || UI3 == UI4) {
        return(HTML("Waiting"))
      }
      R <- input$deci_msa
      data <- msa_data_r()
      conf <- input$conf_msa
      req(msa_init(), calc, UI1, UI2, UI3, UI4, data, R)

      data$Part <- factor(data$Part)
      by_ap_part <- group_by(.data = data, Appraiser, Part)
      by_ap_part <- mutate(by_ap_part, Mean = mean(Measures), Normalized = Measures - Mean)
      data$Normalized <- by_ap_part$Normalized
      list(data = data)
    })

    msa_box <- reactive({
      norm <- input$norm_box
      data <- msa_box_data()
      color <- colors()
      col_fill <- color[3]

      if (norm) {
        title <- "Normalized Plot by Apprasier Boxplot"
      } else {
        title <- "Part by Appraiser Boxplot"
      }

      if (norm) {
        p <- ggplot(data = data$data, aes(x = Part, y = Normalized))
      } else {
        p <- ggplot(data = data$data, aes(x = Part, y = Measures))
      }
      p + geom_boxplot(fill = col_fill) +
        facet_grid(Appraiser ~ .) +
        labs(title = title)
    })

    output$msacchart1 <- renderPlot({
      req(input$msa_c_go)
      button_test <- isolate(button_state_c())
      if (!button_test) {
        return(HTML("<h3>Set up study and click the Perform Analysis button</h3>"))
      }
      req(UI1_d(), UI2_d(), UI3_d(), UI4_d(), msa_init(), msa_data_r(), input$msa_level, msacchart1_width(), msacchart1_height())
      msa_cchart1()
    })
    msacchart1_width <- reactive(400 * 8)
    msacchart1_height <- reactive(app_num() * 400 * 2)
    downloadServer("msacchart1", msa_cchart1, height = msacchart1_height, width = msacchart1_width)

    output$msa_cchart1 <- renderUI({
      level <- input$msa_level
      if (level == 2) {
        return()
      }
      UI1 <- as.numeric(UI1_d())
      UI2 <- as.numeric(UI2_d())
      UI3 <- as.numeric(UI3_d())
      UI4 <- as.numeric(UI4_d())
      if (length(UI4) == 0 || UI3 == UI4) {
        return(HTML("Waiting"))
      }
      data <- msa_data_r()
      req(msa_init(), UI1, UI2, UI3, UI4, length(data) > 1, app_num() > 0)

      ui_render_flags$msa_cchart1 <- TRUE
      plotOutput(ns("msacchart1"), height = app_num() * 200)
    })

    msa_cchart1 <- reactive({
      calc <- input$msa_calc
      level <- input$msa_level
      if (level == 2) {
        return()
      }
      UI1 <- as.numeric(UI1_d())
      UI2 <- as.numeric(UI2_d())
      UI3 <- as.numeric(UI3_d())
      UI4 <- as.numeric(UI4_d())
      if (length(UI4) == 0 || UI3 == UI4) {
        return(HTML("Waiting"))
      }
      R <- input$deci_msa
      conf <- input$conf_msa
      data <- msa_data_r()
      validate(need(all(!is.na(data)), "Missing data"))

      button_test <- isolate(button_state_c())
      if (!button_test) {
        return(HTML("<h3>Set up study and click the Perform Analysis button</h3>"))
      }

      by_ap_part <- group_by(.data = data, Appraiser, Part)
      by_ap_part <- mutate(by_ap_part, Range = max(Measures) - min(Measures), sd = sd(Measures), Mean = mean(Measures))
      trials <- length(unique(by_ap_part$Trial))
      plot_data <- by_ap_part[which(by_ap_part$Trial == 1), ][-4][-3]
      rows <- nrow(plot_data)
      by_ap <- group_by(by_ap_part[which(by_ap_part$Trial == 1), ], Appraiser)
      by_ap <- mutate(by_ap, Ave_Range = mean(Range), Ave_sd = mean(sd), Ave_Mean = mean(Mean))
      plot_data <- ungroup(by_ap)

      color <- colors()
      if (calc == 1) {
        UCL <- plot_data$Ave_Range * spc.constant.calculation.D4(sample.size = trials)
        LCL <- plot_data$Ave_Range * spc.constant.calculation.D3(sample.size = trials)
        plot_data <- cbind(plot_data, "UCL" = UCL, "LCL" = LCL)
        ool <- spc.controlviolation.evaluate.rules(
          control.rules = spc.rulesets.outside.limits(),
          chart.series = plot_data$Range,
          center.line = plot_data$Ave_Range,
          control.limits.ucl = plot_data$UCL,
          control.limits.lcl = plot_data$LCL,
          zone.a.upper = plot_data$UCL,
          zone.a.lower = plot_data$LCL
        )
        ool[["rule.results"]][["outside.limits"]][which(ool[["rule.results"]][["outside.limits"]] == FALSE)] <- NA
        ool[["rule.results"]][["outside.limits"]] <- ool[["rule.results"]][["outside.limits"]] * plot_data$Range

        p <- ggplot(data = plot_data, aes(x = Part, y = Range)) +
          labs(y = "Range Within Part")
      } else {
        UCL <- plot_data$Ave_sd * spc.constant.calculation.B4(sample.size = trials)
        LCL <- plot_data$Ave_sd * spc.constant.calculation.B3(sample.size = trials)
        plot_data <- cbind(plot_data, "UCL" = UCL, "LCL" = LCL)
        ool <- spc.controlviolation.evaluate.rules(
          control.rules = spc.rulesets.outside.limits(),
          chart.series = plot_data$sd,
          center.line = plot_data$Ave_sd,
          control.limits.ucl = plot_data$UCL,
          control.limits.lcl = plot_data$LCL,
          zone.a.upper = plot_data$UCL,
          zone.a.lower = plot_data$LCL
        )
        ool[["rule.results"]][["outside.limits"]][which(ool[["rule.results"]][["outside.limits"]] == FALSE)] <- NA
        ool[["rule.results"]][["outside.limits"]] <- ool[["rule.results"]][["outside.limits"]] * plot_data$sd

        p <- ggplot(data = plot_data, aes(x = Part, y = sd)) +
          labs(y = "Standard Deviation Within Part")
      }

      p <- p +
        geom_point(size = 3, color = color[4]) +
        geom_hline(aes(yintercept = UCL), color = color[2], linetype = 5) +
        scale_x_continuous(breaks = plot_data$Part) +
        facet_grid(rows = vars(plot_data$Appraiser)) +
        labs(title = "Dispersion within Part Control Chart", subtitle = "Not in time order, only look for points outside of the limits")
      if (min(is.na(ool[["rule.results"]][["outside.limits"]])) == 0) {
        p <- p + geom_point(aes(y = ool[["rule.results"]][["outside.limits"]]), size = 4, color = color[2], shape = 8)
      }
      if (!all(is.na(LCL))) {
        p <- p + geom_hline(aes(yintercept = LCL), color = color[2], linetype = 5)
      }
      if (nrow(data) / app_num() < 100) {
        p <- p + labs(caption = "With fewer than 100 observations per appraiser, these limits are likely to change")
      }
      p
    })

    msa_chart0_data <- eventReactive(list(button_state_c(), filtered_data()), {
      calc <- input$msa_calc
      level <- input$msa_level
      if (level == 2) {
        return()
      }
      UI1 <- as.numeric(UI1_d())
      UI2 <- as.numeric(UI2_d())
      UI3 <- as.numeric(UI3_d())
      UI4 <- as.numeric(UI4_d())
      if (length(UI4) == 0 || UI3 == UI4) {
        return(HTML("Waiting"))
      }
      all_data <- filtered_data()
      req(all_data)
      data <- msa_data_r()
      req(data)

      req(msa_init(), calc, UI1, UI2, UI3, UI4, data)
      sigmas <- input$msa_sigmas
      if (sigmas) {
        sigma <- 6
      } else {
        sigma <- 5.15
      }

      sigma_e <- sig_e()

      by_ap_part <- group_by(.data = data, Appraiser, Part)
      by_ap_part <- mutate(by_ap_part, Range = max(Measures) - min(Measures), Mean = mean(Measures), sd = sd(Measures), norm = Measures - Mean)
      by_ap_part$Appraiser <- factor(by_ap_part$Appraiser, labels = "Appraiser ")
      data$Normalized <- by_ap_part$norm

      by_ap <- group_by(.data = data, Appraiser)
      by_ap <- mutate(by_ap, Mean = mean(Measures))

      res_band <- sigma * sigma_e / 2
      list(
        data = data,
        by_ap = by_ap,
        res_band = res_band
      )
    })

    msa_chart0 <- reactive({
      USL <- input$msa_USL
      LSL <- input$msa_LSL
      chart_data <- msa_chart0_data()
      use_spec <- input$msa_range_b
      if (is.null(chart_data)) return()

      if (isTruthy(USL)) {
        data <- cbind(chart_data$data, USL)
      }
      if (isTruthy(LSL)) {
        data <- cbind(chart_data$data, LSL)
      }

      if (input$msa_jitter) {
        jitter_width <- 0.1
      } else {
        jitter_width <- 0
      }
      norm <- input$norm_chart0

      if (norm) {
        chart_data$by_ap$Mean <- 0
        p <- ggplot(data = chart_data$data, aes(x = Part, y = Normalized))
      } else {
        p <- ggplot(data = chart_data$data, aes(x = Part, y = Measures))
      }

      color <- colors()
      col_fill <- color[3]
      if (input$msa_violin) {
        p <- p + geom_violin(aes(group = cut_width(Part, 0.1)), fill = adjustcolor(col = col_fill, alpha.f = 0.5), bw = "sj")
      }
      p <- p +
        geom_jitter(width = jitter_width) +
        stat_summary(geom = "point", fun = "mean", size = 4, color = color[2]) +
        facet_grid(rows = chart_data$data$Appraiser)
      if (norm) {
        p <- p +
          geom_hline(aes(yintercept = chart_data$by_ap$Mean, linetype = "Normalized Average")) +
          scale_linetype_manual("Legend", values = c("Normalized Average" = 1, "Measurement Error" = 3, "Spec Limit" = 4))
      } else {
        p <- p +
          geom_hline(aes(yintercept = chart_data$by_ap$Mean, linetype = "Average of Parts")) +
          scale_linetype_manual("Legend", values = c("Average of Parts" = 1, "Measurement Error" = 3, "Spec Limit" = 4))
      }
      p <- p +
        geom_hline(aes(yintercept = chart_data$by_ap$Mean + chart_data$res_band, linetype = "Measurement Error"), linewidth = 1) +
        geom_hline(aes(yintercept = chart_data$by_ap$Mean - chart_data$res_band, linetype = "Measurement Error"), linewidth = 1) +
        labs(title = "Measurement Error, Parts, and Specifications", y = "Means and Individual Measurements", subtitle = "Small dots are individual measurements, large dots are the averages for each part.") +
        theme(legend.position = "top") +
        scale_x_continuous(breaks = chart_data$data$Part)
      if (use_spec && !norm) {
        if (!is.na(USL)) {
          p <- p + geom_hline(aes(yintercept = USL, linetype = "Spec Limit"), linewidth = 0.75)
        }
        if (!is.na(LSL)) {
          p <- p + geom_hline(aes(yintercept = LSL, linetype = "Spec Limit"), linewidth = 0.75)
        }
      }
      range <- input$msa_range
      if (!use_spec && norm && !is.na(range)) {
        p <- p +
          geom_hline(aes(yintercept = -range / 2, linetype = "Spec Limit"), linewidth = 0.75) +
          geom_hline(aes(yintercept = range / 2, linetype = "Spec Limit"), linewidth = 0.75)
      }
      p
    })

    output$msa_lt <- renderPlot({
      req(lt_ready(), UI1_d, UI2_d, UI3_d, UI1_d, UI4_d, isTruthy(msa_init()), length(msa_data_r()) > 1, msalt_width() > 0, msalt_height() > 0)
      plot <- msa_lt()
      if (!inherits(plot, "ggplot")) {
        return(NULL)
      }
      plot
    })
    msalt_width <- reactive(400 * 8)
    msalt_height <- reactive({
      req(app_num())
      app_num() * 1600 * 4
    })
    downloadServer("msalt", msa_lt, width = msalt_width, height = msalt_height)

    msa_lt <- reactive({
      calc <- input$msa_calc
      level <- input$msa_level
      if (level == 2) {
        return()
      }
      UI1 <- as.numeric(UI1_d())
      UI2 <- as.numeric(UI2_d())
      UI3 <- as.numeric(UI3_d())
      UI4 <- as.numeric(UI4_d())
      if (length(UI4) == 0 || UI3 == UI4) {
        return(HTML("Waiting"))
      }
      button_test <- isolate(button_state_c())
      if (!button_test && !isTruthy(msa_init())) {
        return(HTML("<h3>Set up study and click the Perform Analysis button</h3>"))
      }

      data <- msa_data_r()
      validate(need(all(!is.na(data)), "Missing data"))
      req(msa_init(), calc, UI1, UI2, UI3, UI4, length(data) > 1, msalt_width() > 0, msalt_height() > 0)
      sigmas <- input$msa_sigmas
      if (sigmas) {
        sigma <- 6
      } else {
        sigma <- 5.15
      }
      use_range <- input$msa_range_b
      USL <- input$msa_USL
      LSL <- input$msa_LSL
      match_axis <- input$msd_lt_axis
      R <- input$deci_msa
      conf <- input$conf_msa
      type <- input$msa_type

      data <- data[order(data$Appraiser, data$Trial), ]
      ooc_rules <- spc.rulesets.nelson.1984.test.1.2.3.4()
      ooc_rules$runs <- spc.controlviolation.nelson.1984.test2.runs.create(point.count = 8)

      by_ap_part <- group_by(.data = data, Appraiser, Part)
      by_ap_part <- mutate(by_ap_part, Range = max(Measures) - min(Measures), Mean = mean(Measures), sd = sd(Measures))
      by_ap_part$Appraiser <- factor(by_ap_part$Appraiser, labels = "Appraiser ")

      by_ap <- group_by(.data = data, Appraiser)
      by_ap <- mutate(by_ap, Mean = mean(Measures))

      app_id <- unique(data$Appraiser)
      trials <- max(data$Trial)
      parts <- unique(data$Part)
      data <- cbind(data, UCL = NA, LCL = NA, Mean = NA, ooc_x = NA, ooc_mr = NA, facet = NA)

      for (i in app_id) {
        for (j in parts) {
          subdat <- data[which(data$Appraiser == i & data$Part == j), ]$Measures
          mr <- MR_span(data = subdat)
          mr_bar <- mean(mr[-1])
          x_bar <- mean(subdat)
          facet <- paste0(i, ", Part ", j)
          UCL <- x_bar + mr_bar * spc.constant.calculation.A2(sample.size = 2, n.sigma = 3) * sqrt(2)
          LCL <- x_bar - mr_bar * spc.constant.calculation.A2(sample.size = 2, n.sigma = 3) * sqrt(2)
          UCL_mr <- mr_bar * spc.constant.calculation.D4(sample.size = 2, n.sigma = 3)
          ooc_mr <- spc.controlviolation.evaluate.rules(control.rules = spc.rulesets.outside.limits(), chart.series = mr, center.line = mr_bar, control.limits.ucl = UCL_mr, zone.a.upper = UCL_mr)
          if (length(subdat) < 3) {
            ooc_x <- spc.controlviolation.evaluate.rules(control.rules = spc.rulesets.outside.limits(), chart.series = subdat, center.line = x_bar, control.limits.ucl = UCL, zone.a.upper = UCL, control.limits.lcl = LCL, zone.a.lower = LCL)
          } else {
            ooc_x <- spc.controlviolation.evaluate.rules(control.rules = ooc_rules, chart.series = subdat, center.line = x_bar, control.limits.ucl = UCL, zone.a.upper = UCL, control.limits.lcl = LCL, zone.a.lower = LCL)
          }
          ooc_x <- subdat * ooc_x$overall.results
          ooc_mr <- subdat * ooc_mr$overall.results
          data[which(data$Appraiser == i & data$Part == j), ]$UCL <- UCL
          data[which(data$Appraiser == i & data$Part == j), ]$LCL <- LCL
          data[which(data$Appraiser == i & data$Part == j), ]$Mean <- x_bar
          data[which(data$Appraiser == i & data$Part == j), ]$ooc_x <- ooc_x
          data[which(data$Appraiser == i & data$Part == j), ]$ooc_mr <- ooc_mr
          data[which(data$Appraiser == i & data$Part == j), ]$facet <- facet
        }
      }
      data$ooc_x[data$ooc_x == 0] <- NA
      data$ooc_mr[data$ooc_mr == 0] <- NA
      data$facet <- factor(data$facet, levels = unique(data$facet), labels = unique(data$facet))

      color <- colors()
      p <- ggplot(data = data, aes(x = Trial, y = Measures)) +
        geom_point(color = color[4]) +
        geom_line(color = color[4]) +
        geom_line(aes(y = UCL), color = color[2], linetype = 2) +
        geom_line(aes(y = LCL), color = color[2], linetype = 2) +
        geom_line(aes(y = Mean), color = color[3]) +
        geom_point(aes(y = ooc_x), color = color[2], shape = 8, size = 4) +
        geom_point(aes(y = ooc_mr), color = color[6], shape = 6, size = 4) +
        labs(title = "Parts through Time", subtitle = "Out of Limits, Runs, Trends, Alternating Values and Out of Moving Range Limits Marked")

      if (match_axis) {
        p <- p + facet_grid(rows = data$facet)
      } else {
        p <- p + facet_grid(rows = data$facet, scales = "free")
      }
      if (max(data$Trial) < 25) {
        p <- p + labs(caption = "With fewer than 25 observations, these limits are likely to change")
      }
      p
    })

    output$msachart0 <- renderPlot({
      msa_chart0()
    })
    msachart0_width <- reactive(400 * 8)
    msachart0_height <- reactive({
      req(app_num())
      app_num() * 400 * 4
    })
    downloadServer("msachart0", msa_chart0, height = msachart0_height, width = msachart0_width)

    output$msa_cchart0 <- renderUI({
      level <- input$msa_level
      if (level == 2) {
        return()
      }
      UI1 <- as.numeric(UI1_d())
      UI2 <- as.numeric(UI2_d())
      UI3 <- as.numeric(UI3_d())
      UI4 <- as.numeric(input$msa_UI4)
      data <- msa_data_r()
      req(msa_init(), UI1, UI2, UI3, UI4, length(data) > 1, app_num() > 0)

      ui_render_flags$msa_cchart0 <- TRUE
      plotOutput(ns("msachart0"), height = app_num() * 400)
    })

    output$lt_plot <- renderUI({
      req(lt_ready())
      level <- input$msa_level
      if (level == 2) {
        return()
      }
      UI1 <- as.numeric(UI1_d())
      UI2 <- as.numeric(UI2_d())
      UI3 <- as.numeric(UI3_d())
      UI4 <- as.numeric(input$msa_UI4)
      if (length(UI4) == 0 || UI3 == UI4) {
        return(HTML("Waiting"))
      }
      button_test <- isolate(button_state_c())
      if (!button_test && !isTruthy(msa_init())) {
        return(HTML("<h3>Set up study and click the Perform Analysis button</h3>"))
      }

      data <- msa_data_r()
      req(msa_init(), UI1, UI2, UI3, UI4, length(data) > 1)
      ui_render_flags$lt_plot <- TRUE
      plotOutput(ns("msa_lt"), height = app_num() * 1600)
    })

    msa_lt_overall <- reactive({
      req(lt_ready())
      level <- input$msa_level
      if (level == 2) {
        return()
      }
      UI1 <- as.numeric(UI1_d())
      UI2 <- as.numeric(UI2_d())
      UI3 <- as.numeric(UI3_d())
      UI4 <- as.numeric(UI4_d())
      if (length(UI4) == 0 || UI3 == UI4) {
        return(HTML("Waiting"))
      }
      button_test <- isolate(button_state_c())
      if (!button_test && !isTruthy(msa_init())) {
        return(HTML("<h3>Set up study and click the Perform Analysis button</h3>"))
      }

      data <- msa_data_r()
      req(msa_init(), UI1, UI2, UI3, UI4, length(data) > 1, app_num() > 0)
      validate(
        need(max(data$Trial) > 2, "Not enough data for chart"),
        need(all(!is.na(data)), "Missing data")
      )

      ooc_rules <- spc.rulesets.nelson.1984.test.1.2.3.4()
      ooc_rules$runs <- spc.controlviolation.nelson.1984.test2.runs.create(point.count = 8)
      data$Appraiser <- factor(data$Appraiser)

      by_ap_trial <- group_by(.data = data, Appraiser, Trial)
      by_ap_trial$Appraiser <- factor(by_ap_trial$Appraiser)
      by_ap_trial <- mutate(by_ap_trial, Mean = mean(Measures), sd = sd(Measures))

      plot_data <- by_ap_trial[which(by_ap_trial$Part == 1), ][-2]
      temp <- group_by(.data = plot_data, Appraiser)
      temp <- mutate(
        temp,
        x_bar = mean(Mean),
        s_bar = mean(sd),
        mr_x = MR_span(Mean),
        mr_s = MR_span(sd),
        mean_mr_x = mean(mr_x, na.rm = TRUE),
        mean_mr_s = mean(mr_s, na.rm = TRUE),
        UCL_x = x_bar + mean_mr_x * spc.constant.calculation.A2(2) * sqrt(2),
        LCL_x = x_bar - mean_mr_x * spc.constant.calculation.A2(2) * sqrt(2),
        UCL_s = s_bar + mean_mr_s * spc.constant.calculation.A2(2) * sqrt(2),
        LCL_s = s_bar - mean_mr_s * spc.constant.calculation.A2(2) * sqrt(2),
        UCL_mr_x = mean_mr_x * spc.constant.calculation.D4(2) * sqrt(2),
        LCL_mr_x = mean_mr_x * spc.constant.calculation.D3(2) * sqrt(2),
        UCL_mr_s = mean_mr_s * spc.constant.calculation.D4(2) * sqrt(2),
        LCL_mr_s = mean_mr_s * spc.constant.calculation.D3(2) * sqrt(2),
        ooc_x = spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = Mean,
          center.line = x_bar,
          control.limits.ucl = UCL_x,
          zone.a.upper = UCL_x,
          ontrol.limits.lcl = LCL_x,
          zone.a.lower = LCL_x
        )$overall.results * Mean,
        ooc_x_mr = spc.controlviolation.evaluate.rules(
          control.rules = spc.rulesets.outside.limits(),
          chart.series = mr_x,
          center.line = mean_mr_x,
          control.limits.ucl = UCL_mr_x,
          zone.a.upper = UCL_mr_x,
          control.limits.lcl = LCL_mr_x,
          zone.a.lower = LCL_mr_x
        )$overall.results * Mean,
        ooc_s = spc.controlviolation.evaluate.rules(
          control.rules = ooc_rules,
          chart.series = sd,
          center.line = s_bar,
          control.limits.ucl = UCL_s,
          zone.a.upper = UCL_s,
          control.limits.lcl = LCL_s,
          zone.a.lower = LCL_s
        )$overall.results * sd,
        ooc_s_mr = spc.controlviolation.evaluate.rules(
          control.rules = spc.rulesets.outside.limits(),
          chart.series = mr_s,
          center.line = mean_mr_s,
          control.limits.ucl = UCL_mr_s,
          zone.a.upper = UCL_mr_s,
          control.limits.lcl = LCL_mr_s,
          zone.a.lower = LCL_mr_s
        )$overall.results * sd
      )

      plot_data <- temp
      plot_data$ooc_x[plot_data$ooc_x == 0] <- NA
      plot_data$ooc_x_mr[plot_data$ooc_x_mr == 0] <- NA
      plot_data$ooc_s[plot_data$ooc_s == 0] <- NA
      plot_data$ooc_s_mr[plot_data$ooc_s_mr == 0] <- NA

      trials <- max(plot_data$Trial)
      temp <- cbind.data.frame(
        Appraiser = c(plot_data$Appraiser, plot_data$Appraiser),
        trial = c(plot_data$Trial, plot_data$Trial),
        chart = c(rep("Mean", trials * app_num()), rep("s", trials * app_num())),
        point = as.numeric(c(plot_data$Mean, plot_data$sd)),
        UCL = as.numeric(c(plot_data$UCL_x, plot_data$UCL_s)),
        LCL = as.numeric(c(plot_data$LCL_x, plot_data$LCL_s)),
        Mean = as.numeric(c(plot_data$x_bar, plot_data$s_bar)),
        ooc = as.numeric(c(plot_data$ooc_x, plot_data$ooc_s)),
        ooc_mr = as.numeric(c(plot_data$ooc_x_mr, plot_data$ooc_s_mr))
      )

      factor <- paste0(temp$Appraiser, " ", temp$chart)
      levels <- as.data.frame(unique(factor))
      order <- c(seq(from = 1, to = app_num() * 2, by = 2), seq(from = 2, to = app_num() * 2, by = 2))
      levels <- cbind(levels, order)
      levels <- levels[order(order), ]
      temp <- cbind(temp, facet = factor)
      temp$facet <- factor(temp$facet, levels = levels$`unique(factor)`, labels = levels$`unique(factor)`, ordered = TRUE)

      color <- colors()
      p <- ggplot(data = temp, aes(x = trial, y = point, group = 1)) +
        geom_point(aes(y = point), color = color[4]) +
        geom_line(aes(y = point), color = color[4]) +
        geom_line(aes(y = Mean), color = color[3]) +
        geom_line(aes(y = UCL), color = color[2], linetype = 2) +
        geom_line(aes(y = LCL), color = color[2], linetype = 2) +
        geom_point(aes(y = ooc), color = color[2], shape = 8, size = 4) +
        geom_point(aes(y = ooc_mr), color = color[6], shape = 6, size = 4) +
        facet_grid(row = vars(facet), scales = "free") +
        labs(title = "Average and Standard Deviation by Trial - MR Limits", subtitle = "Out of Limits, Runs, Trends, Alternating Values and Out of Moving Range Limits Marked", x = "Trial", y = "Measures")

      if (max(data$Trial) < 25) {
        p <- p + labs(caption = "With fewer than 25 observations, these limits are likely to change")
      }
      p
    })

    output$msa_lt_overall <- renderPlot({
      req(lt_ready(), isTruthy(msa_init()))
      plot <- msa_lt_overall()
      if (!inherits(plot, "ggplot")) {
        return(NULL)
      }
      plot
    })
    msaltoverall_height <- reactive({
      req(app_num() > 0)
      app_num() * 400 * 4
    })
    msaltoverall_width <- reactive(400 * 8)
    downloadServer("msa_lt_overall", msa_lt_overall, height = msaltoverall_height, width = msaltoverall_width)

    output$lt_plot_2 <- renderUI({
      req(lt_ready())
      level <- input$msa_level
      if (level == 2) {
        return()
      }
      UI1 <- as.numeric(UI1_d())
      UI2 <- as.numeric(UI2_d())
      UI3 <- as.numeric(UI3_d())
      UI4 <- as.numeric(input$msa_UI4)
      if (length(UI4) == 0 || UI3 == UI4) {
        return(HTML("Waiting"))
      }
      data <- msa_data_r()
      req(msa_init(), UI1, UI2, UI3, UI4, length(data) > 1, app_num() > 0)
      ui_render_flags$lt_plot_2 <- TRUE
      plotOutput(ns("msa_lt_overall"), height = app_num() * 400)
    })

    observe({
      type <- input$msa_type
      req(type)
      if (type == 1) {
        updateRadioButtons(inputId = "msa_calc", selected = 1)
      } else if (type == 3) {
        updateRadioButtons(inputId = "msa_calc", selected = 2)
      }
    })

    observeEvent(input$msa_details, {
      flag <- input$msa_details
      type <- input$msa_type
      if (!flag) {
        return()
      }
      req(type)

      if (type == 1) {
        title <- "Potential Study Details"
        desc <- "A <b>potential study</b> is a quick way to determine if a particular measurement system has a chance of being useful. A potential study is not sufficient by itself to guarantee long- or even short-term performance.<br/><br/>In a potential study 10 or more specimens are measured by one or more appraisers or systems two or three times."
      } else if (type == 2) {
        title <- "Short-Term Study Details"
        desc <- "A <b>short-term study</b> is to gather more information on a measurement system with some minimal information about stability through time. It provides more information and better estimates than a potential study. A short-term study does not monitor a process through time.<br/><br/>In a short-term study 25 specimens are measured by one or more appraisers four to eight times each."
      } else if (type == 3) {
        title <- "Long-Term Study Details"
        desc <- "A <b>long-term study</b> is to continually monitor a measurement system to assure that its results are stable through time and that the measurement system error is known. A long-term study can signal when it is time to recalibrate a measurement device and rule out measurement error as a source of nonconformance.<br/><br/>In a long-term study 8 or more parts are retained and measured periodically. After 25 measures, control limits can be established and used going forward."
      }

      sendSweetAlert(title = title, text = HTML(desc), html = TRUE, showCloseButton = TRUE, btn_labels = "Close", type = "info")
      updateCheckboxInput(inputId = "msa_details", value = FALSE)
    })

    list(
      button_state_c = button_state_c,
      ui_render_flags = ui_render_flags,
      msa_data_r = msa_data_r,
      msa_init = msa_init,
      app_num = app_num,
      UI1_d = UI1_d,
      UI2_d = UI2_d,
      UI3_d = UI3_d,
      UI4_d = UI4_d,
      sig_e = sig_e,
      sig_rpt = sig_rpt,
      sig_rpd = sig_rpd,
      sig_app = sig_app,
      sig_part_app = sig_part_app
    )
  })
}
