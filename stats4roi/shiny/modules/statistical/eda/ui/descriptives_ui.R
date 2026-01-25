# Descriptives UI Component
# Replicated from original app.R lines 1233-1282

create_descriptives_ui <- function(ns) {
  tagList(
    h3("Descriptive Statistics"),
    sidebarLayout(
      sidebarPanel(
        numericInput(
          inputId = ns("decimals_desc2"),
          label = "Decimals",
          value = 5,
          min = 0,
          step = 1,
          width = "75px"
        ),
        uiOutput(ns("desc_data_list")),
        pickerInput(
          inputId = ns("desc_stats"),
          label = "Select Statistics",
          choices = list(
            File_Info = c(
              "Total Sample Size" = "stat.total.n=T",
              "Missing" = "stat.miss=T",
              "Sum" = "stat.sum=T",
              "Distinct Values, no NA" = "stat.distinct=T",
              "Distinct Values with NA" = "stat.distinct.withna=T"
            ),
            Measures_of_Location = c(
              "Mean" = "stat.mean=T",
              "Median" = "stat.median=T",
              "True Mode" = "stat.true.mode=T"
            ),
            Measures_of_Spread = c(
              "Variance" = "stat.var=T",
              "Standard Deviation" = "stat.sd=T",
              "Range" = "stat.range=T",
              "Coefficient of Variation" = "stat.coefvar=T",
              "Mean ADA" = "stat.mean.ADA=T",
              "Mean ADM" = "stat.mean.ADM=T",
              "Mean ADM(n-1)" = "stat.mean.ADMn1=T",
              "Inter-Quartile Range" = "stat.iqr=T",
              "Semi-Interquartile Range" = "stat.sir=T",
              "Pseudo Standard Deviation" = "stat.psd=T"
            ),
            Measure_of_Shape = c(
              "Skewness" = "stat.skew.test=T",
              "Kurtosis" = "stat.kurt.test=T"
            ),
            Measures_of_Position = c(
              "Minimum" = "stat.min=T",
              "First Quartile" = "stat.q1=T",
              "Second Quartile" = "stat.quantile=.5",
              "Third Quartile" = "stat.q3=T",
              "Maximum" = "stat.max=T"
            )
          ),
          multiple = TRUE,
          options = list(`actions-box` = TRUE),
          selected = c("stat.mean=T", "stat.sd=T", "stat.min=T", "stat.max=T", "stat.range=T")
        )
      ),
      mainPanel(
        textOutput(ns("desc_test_output")),
        br(),
        DTOutput(ns("desc_out")),
        HTML("<br><br>Quantiles are calculated using Type 6 <a href='https://www.rdocumentation.org/packages/stats/versions/3.4.3/topics/quantile'>Learn more here.</a>")
      )
    )
  )
}
