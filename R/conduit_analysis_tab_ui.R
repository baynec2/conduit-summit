conduit_analysis_tab_ui <- function() {
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
              plotOutput("feature_number_plot",
                         height = "600px")
            ),
            tabPanel(
              "Missing Values",
              fluidRow(
                box(
                  title = "Plot Options",
                  status = "primary",
                  width = 12,
                  solidHeader = TRUE,
                  column(
                    width = 6,
                    uiOutput("miss_val_heatmap_col_color_choices_ui")
                  ),
                  column(
                    width = 6,
                    uiOutput("miss_val_heatmap_row_color_choices_ui")
                  )
                )
              ),
              fluidRow(
                plotOutput("missing_value_plot",
                           height = "600px")
              )
            ),
            tabPanel(
              "Sample Correlation",
              fluidRow(
                box(
                  title = "Plot Options",
                  status = "primary",
                  width = 12,
                  solidHeader = TRUE,
                  uiOutput("sample_cor_heatmap_color_choices_ui")
                )
              ),
              fluidRow(
                plotOutput("sample_cor_heatmap",
                           height = "600px")
              )
            ),
            tabPanel(
              "Intensity Distribuiton",
              plotOutput("intensity_distribution_plot")
            ),
            tabPanel(
              "Density Plot",
              fluidRow(
                box(
                  title = "Plot Options",
                  status = "primary",
                  width = 12,
                  solidHeader = TRUE,
              uiOutput("density_plot_color_choice_ui",
                       )
                )
              ),
              fluidRow(
              plotOutput("density_plot",height = "600px")
              )
            )
          )
        )
      ),
      tabPanel(
        "PCA",
        fluidRow(
          box(
            title = "PCA Plot Options",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            column(
              width = 4,
              textInput("pca_plot_formula",
                        "Enter facet formula",
                        value = "~ NULL")
            ),
            column(
              width = 4,
              uiOutput("pca_plot_color_choice_ui")
            ),
            column(
              width = 4,
              uiOutput("pca_plot_shape_choice_ui")
            )
          )
        ),
        fluidRow(
          plotOutput("pca_plot",height = "600px")
        )
      ),
      tabPanel(
        "Heatmap",
        fluidRow(
          box("Plot Options",
              status = "primary",
              solidHeader = TRUE,
          column(
            width = 4,
            selectInput("heatmap_plot_type", "Select type of heatmap",
              choices = c("static", "interactive"),
              selected = "static"
            )
          ),
          column(
            width = 4,
            uiOutput("heatmap_col_color_choices_ui")
          ),
          column(
            width = 4,
            uiOutput("heatmap_row_color_choices_ui")
          )
        )
        ),
        fluidRow(
          uiOutput("heatmap_plot_ui")
        )
      ),
      tabPanel("Relative Abundance",
        fluidRow(
          box(width = 12,
              "Plot Options",
              status = "primary",
              solidHeader = TRUE,
              column(12,
                     textInput("relative_abundance_plot_formula",
                               "Enter Facet Formula",
                               value = "~NULL"
                               )
                     )
              )
          ),
        fluidRow(
        plotOutput("relative_abundance_plot",height = "600px")
        )
      ),
      tabPanel(
        "Statistics",
        fluidRow(
          box(
            title = "Limma Model Setup",
            width = 12,  # Full width
            solidHeader = TRUE,
            status = "primary",
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
                box(title = "Avalible contrast terms",
                    status = "primary",
                    solidHeader = TRUE,
                verbatimTextOutput("possible_contrasts")
                )
              ),
              column(
                width = 3,
                textInput(
                  inputId = "limma_contrast",
                  label = "Specify Contrast"
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
              DT::DTOutput("limma_statistics_table")
            ),
          column(
            width = 6,
            fluidRow(
              box(
                title = "Volcano Plot Settings",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                fluidRow(
                  column(
                    width = 4,
                    textInput("limma_volcano_facet_formula",
                              "Faceting Formula",
                              value = "~NULL"
                    )
                  ),
                  column(
                    width = 4,
                    uiOutput("limma_volcano_color_ui")
                  ),
                  column(
                    width = 4,
                    shiny::sliderInput("volcano_p_threshold",
                                       "p-value threshold",
                                       min = 0,
                                       max = 1,
                                       value = 0.05,
                                       step = 0.01
                    )
                  )
                )
              )
            ),
            fluidRow(
              box(
                width = 12,
                title = "Volcano Plot",
                solidHeader = TRUE,
                status = "primary",
                plotOutput("limma_volcano_plot")
              )
            )
          )
        ),
        fluidRow(
          actionButton("enrichment_analysis_button",
                       "Click to navigate to enrichment analysis page")

        ),
        fluidRow(
          box(width = 12,
              title = "Selected Feature Plot Options",
              solidHeader = TRUE,
              status = "primary",
          column(2,
                 uiOutput("selected_feature_plot_x_axis_ui")
          ),
          column(3,
                 uiOutput("selected_feature_plot_facet_formula_ui")
                 ),
          column(2,
                 uiOutput("selected_feature_plot_color_ui")
                 ),
          column(2,
                 uiOutput("selected_feature_plot_shape_ui")
                 ),
          column(2,
                 selectInput("selected_feature_plot_data_type_ui",
                             "What type of data to show",
                             choices = c("","_log2","_log2_imputed","_log2_imputed_norm"),
                             selected = "_log2_imputed"
                               )
                 )
        )
        ),
        fluidRow(
          plotOutput("selected_feature_plot",height = "600px"
          )
        )
      ),
      tabPanel(
        "Outcome Prediction",
        fluidRow(
      box(
        title = "Prediction Settings",
        status = "primary",
        solidHeader = TRUE,
        width = 4,
        selectInput("predict_var", "Select outcome variable", choices = NULL),
        sliderInput("split_ratio", "Train/Test Split % (Train)", min = 50, max = 90, value = 70),
        selectInput("model_type", "Model Type", choices = c("Logistic Regression", "Random Forest", "XGBoost")),
        checkboxInput("show_advanced", "Show advanced options", value = FALSE),

        conditionalPanel(
          condition = "input.show_advanced == true",
          tags$hr(),
          selectInput("predictors_to_include", "Predictors to include (optional):",
                      choices = NULL, multiple = TRUE, selectize = TRUE),
          numericInput("cv_folds", "Number of CV folds:", value = 5, min = 2, max = 20),
          numericInput("cv_repeats", "Number of repeats (if applicable):", value = 1, min = 1, max = 10),
          selectInput("metric_to_optimize", "Metric to optimize:",
                      choices = c("roc_auc", "accuracy", "sens", "spec"), selected = "roc_auc"),
          numericInput("random_seed", "Random seed:", value = 123)
        ),

        actionButton("run_model", "Run Model", icon = icon("cogs"))
      ),
      box(
        title = "ROC Curve - Training",
        width = 8,
        plotOutput("roc_train")
      )
    ),
    fluidRow(
      box(
        title = "ROC Curve - Test",
        width = 6,
        plotOutput("roc_test")
      ),
      box(
        title = "Feature Importance",
        width = 6,
        plotOutput("feature_importance")
      )
    )
      )
    )
  )
}
