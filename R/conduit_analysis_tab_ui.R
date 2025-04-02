conduit_analysis_tab_ui = function(){
  tabItem(
    "analysis",
    # Add sub-tabs within Analysis
    tabsetPanel(
      tabPanel(
        "QC",
        fluidPage(
          tabsetPanel(
            tabPanel(
              "Feature Numbers",
              box("feature_number_plot")
            ),
            tabPanel(
              "Missing Values",
              box("missing_values_plot")
            ),
            tabPanel(
              "Sample Correlation",
              box("sample_correlation_plot")
            ),
            tabPanel(
              "Density Plot",
              box("density_plot")
            ),
            tabPanel(
              "Sample Coverage",
              box("sample_coverage_plot")
            )
          )
        )
      ),
      tabPanel(
        "Clustering",
        fluidRow(
          column(
            width = 6,
            box(title = "PCA", id = "pca_plot")
          ),
          column(
            width = 6,
            box(title = "Heatmap", id = "heatmap_plot")
          )
        )
      ),
      tabPanel(
        "Statistics",
        fluidRow(
          column(
            width = 3,
            textInput(
              inputId = "limma_formula",
              label = "Enter formula for limma analysis"
            )
          ),
          column(
            width = 6,
            selectizeInput(
              inputId = "contrasts",
              "Select Contrast",
              choices = "placeholder"
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            box(title = "Statistics", id = "statistics_df")
          ),
          column(
            width = 6,
            box(title = "Volcano Plot", id = "volcano_plot")
          ),
        )
      ),
      tabPanel(
        "Biomarkers",
        fluidRow(
          column(
            width = 3,
            selectizeInput(
              inputId = "prediction",
              label = "Variable to Predict",
              choices = "placeholder"
            )
          ),
          column(width = 3, sliderInput(
            inputId = "per_training",
            label = "Percent Training Set Split",
            min = 50,
            max = 100,
            value = 75
          )),
          column(
            width = 3,
            selectInput(
              inputId = "model",
              label = "model to use",
              choices = "placeholder"
            )
          )
        )
      ),
      tabPanel(
        "Pathway Analysis",
        h4("Pathway Analysis"),
        p("Perform pathway enrichment analysis.")
      )
    )
  )
}
