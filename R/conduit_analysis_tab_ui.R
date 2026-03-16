conduit_analysis_tab_ui <- function(id = "analysis") {
  ns <- NS(id)

  # Thin right sidebar used by several sub-panels
  ctrl_sidebar <- function(..., width = 260) {
    bslib::sidebar(
      title  = "Plot Controls",
      width  = width,
      bg     = "#f8f9fb",
      fg     = "#1a1a2e",
      ...
    )
  }

  bslib::navset_card_tab(
    id = "analysis_tabs",

    # ── QC ───────────────────────────────────────────────────────────────────
    bslib::nav_panel(
      "QC",
      bslib::navset_underline(
        id = "qc_sub_tabs",

        bslib::nav_panel(
          "Feature Numbers",
          shinycssloaders::withSpinner(
            plotOutput(ns("feature_number_plot"), height = "600px"),
            type = 8, caption = "Loading feature number plot...", color = "#15131e"
          )
        ),

        bslib::nav_panel(
          "Missing Values",
          bslib::layout_sidebar(
            sidebar = ctrl_sidebar(
              uiOutput(ns("miss_val_heatmap_col_color_choices_ui")),
              uiOutput(ns("miss_val_heatmap_row_color_choices_ui"))
            ),
            shinycssloaders::withSpinner(
              plotOutput(ns("missing_value_plot"), height = "600px"),
              type = 8, caption = "Loading missing value heatmap...", color = "#15131e"
            )
          )
        ),

        bslib::nav_panel(
          "Sample Correlation",
          bslib::layout_sidebar(
            sidebar = ctrl_sidebar(
              uiOutput(ns("sample_cor_heatmap_color_choices_ui"))
            ),
            shinycssloaders::withSpinner(
              plotOutput(ns("sample_cor_heatmap"), height = "600px"),
              type = 8, caption = "Loading sample correlation heatmap...", color = "#15131e"
            )
          )
        ),

        bslib::nav_panel(
          "Intensity Distribution",
          shinycssloaders::withSpinner(
            plotOutput(ns("intensity_distribution_plot"), height = "600px"),
            type = 8, caption = "Loading intensity distribution...", color = "#15131e"
          )
        ),

        bslib::nav_panel(
          "Density Plot",
          bslib::layout_sidebar(
            sidebar = ctrl_sidebar(
              uiOutput(ns("density_plot_color_choice_ui"))
            ),
            shinycssloaders::withSpinner(
              plotOutput(ns("density_plot"), height = "600px"),
              type = 8, caption = "Loading density plot...", color = "#15131e"
            )
          )
        )
      )
    ),

    # ── PCA ──────────────────────────────────────────────────────────────────
    bslib::nav_panel(
      "PCA",
      bslib::layout_sidebar(
        sidebar = ctrl_sidebar(
          textInput(ns("pca_plot_formula"), "Facet formula", value = "~ NULL"),
          uiOutput(ns("pca_plot_color_choice_ui")),
          uiOutput(ns("pca_plot_shape_choice_ui"))
        ),
        shinycssloaders::withSpinner(
          plotOutput(ns("pca_plot"), height = "600px"),
          type = 8, caption = "Loading PCA plot...", color = "#15131e"
        )
      )
    ),

    # ── Heatmap ──────────────────────────────────────────────────────────────
    bslib::nav_panel(
      "Heatmap",
      bslib::layout_sidebar(
        sidebar = ctrl_sidebar(
          selectInput(
            ns("heatmap_plot_type"), "Heatmap type",
            choices = c("static", "interactive"), selected = "static"
          ),
          uiOutput(ns("heatmap_feature_number_ui")),
          uiOutput(ns("heatmap_col_color_choices_ui")),
          uiOutput(ns("heatmap_row_color_choices_ui"))
        ),
        uiOutput(ns("heatmap_plot_ui"))
      )
    ),

    # ── Relative Abundance ───────────────────────────────────────────────────
    bslib::nav_panel(
      "Relative Abundance",
      bslib::layout_sidebar(
        sidebar = ctrl_sidebar(
          textInput(ns("relative_abundance_plot_formula"), "Facet formula", value = "~NULL")
        ),
        shinycssloaders::withSpinner(
          plotOutput(ns("relative_abundance_plot"), height = "600px"),
          type = 8, caption = "Loading relative abundance plot...", color = "#15131e"
        )
      )
    ),

    # ── Statistics ───────────────────────────────────────────────────────────
    bslib::nav_panel(
      "Statistics",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title  = "Analysis Controls",
          width  = 300,
          bg     = "#f8f9fb",
          fg     = "#1a1a2e",
          open   = "open",
          bslib::accordion(
            open = TRUE,
            bslib::accordion_panel(
              "Limma Setup", icon = icon("flask"),
              textInput(ns("limma_formula"), "Formula"),
              tags$label("Available contrasts:"),
              verbatimTextOutput(ns("possible_contrasts")),
              textInput(ns("limma_contrast"), "Contrast"),
              actionButton(
                ns("run_limma"), "Run Analysis",
                icon = icon("play"), class = "btn-primary w-100"
              )
            ),
            bslib::accordion_panel(
              "Volcano Options", icon = icon("chart-simple"),
              textInput(ns("limma_volcano_facet_formula"), "Facet formula", value = "~NULL"),
              uiOutput(ns("limma_volcano_color_ui"))
            ),
            bslib::accordion_panel(
              "Feature Selection", icon = icon("sliders"),
              actionButton(
                ns("enrichment_analysis_button"),
                tagList(icon("network-wired"), " Enrichment Analysis"),
                class = "btn-outline-primary btn-sm w-100 mb-1"
              ),
              actionButton(
                ns("pathway_analysis_button"),
                tagList(icon("diagram-project"), " Pathway Analysis"),
                class = "btn-outline-primary btn-sm w-100"
              )
            ),
            bslib::accordion_panel(
              "Feature Plot Options", icon = icon("magnifying-glass-chart"),
              uiOutput(ns("selected_feature_plot_x_axis_ui")),
              uiOutput(ns("selected_feature_plot_facet_formula_ui")),
              uiOutput(ns("selected_feature_plot_color_ui")),
              uiOutput(ns("selected_feature_plot_shape_ui")),
              selectInput(
                ns("selected_feature_plot_data_type_ui"), "Data type",
                choices  = c("", "_log2", "_log2_imputed", "_log2_imputed_norm"),
                selected = "_log2_imputed"
              )
            )
          )
        ),
        tagList(
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              full_screen = TRUE,
              class       = "card-light",
              bslib::card_header("Volcano Plot"),
              bslib::card_body(
                shinycssloaders::withSpinner(
                  plotOutput(ns("limma_volcano_plot"), height = "480px"),
                  type = 8, caption = "Loading volcano plot...", color = "#15131e"
                )
              )
            ),
            bslib::card(
              full_screen = TRUE,
              class       = "card-light",
              bslib::card_header("Statistics Table"),
              bslib::card_body(
                shinycssloaders::withSpinner(
                  DT::DTOutput(ns("limma_statistics_table")),
                  type = 8, caption = "Loading statistics table...", color = "#15131e"
                )
              ),
              bslib::card_footer(
                downloadButton(ns("download_limma_stats_table"), "Download Table")
              )
            )
          ),
          bslib::card(
            full_screen = TRUE,
            class       = "card-light",
            bslib::card_header("Selected Feature Plot"),
            bslib::card_body(
              shinycssloaders::withSpinner(
                plotOutput(ns("selected_feature_plot"), height = "500px"),
                type = 8, caption = "Loading feature plot...", color = "#15131e"
              )
            )
          )
        )
      )
    ),

    # ── Enrichment ───────────────────────────────────────────────────────────
    bslib::nav_panel(
      "Enrichment",
      conduit_enrichment_tab_ui(ns("enrichment"))
    ),

    # ── Pathway ───────────────────────────────────────────────────────────────
    bslib::nav_panel(
      "Pathway",
      conduit_pathway_tab_ui(ns("pathway"))
    ),

    # ── Classification Prediction ─────────────────────────────────────────────
    bslib::nav_panel(
      "Classification Prediction",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title  = "Model Settings",
          width  = 280,
          bg     = "#f8f9fb",
          fg     = "#1a1a2e",
          open   = "open",
          selectInput("outcome_var", "Outcome variable", choices = NULL),
          sliderInput("split_ratio", "Train/Test split % (train)", min = 50, max = 90, value = 70),
          selectInput(
            "model_type", "Model type",
            choices = c("lasso_regression", "random_forest", "xgboost")
          ),
          checkboxInput("show_advanced", "Show advanced options", value = FALSE),
          conditionalPanel(
            condition = "input.show_advanced == true",
            hr(),
            numericInput("cv_folds", "CV folds:", value = 5, min = 2, max = 20),
            numericInput("random_seed", "Random seed:", value = 123)
          ),
          selectInput(
            "model_plot_type", "Plot type",
            choices = c("ROC", "precision_recall")
          ),
          hr(),
          actionButton(
            "run_classification_model", "Run Model",
            icon = icon("cogs"), class = "btn-primary w-100"
          )
        ),
        tagList(
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              full_screen = TRUE,
              class       = "card-light",
              bslib::card_header("Confusion Matrix"),
              bslib::card_body(
                shinycssloaders::withSpinner(
                  plotOutput("confusion_matrix_plot", height = "400px"),
                  type = 8, caption = "Loading confusion matrix...", color = "#15131e"
                )
              )
            ),
            bslib::layout_columns(
              col_widths = c(12, 12),
              bslib::card(
                full_screen = TRUE,
                class       = "card-light",
                bslib::card_header("Test Set"),
                bslib::card_body(
                  shinycssloaders::withSpinner(
                    plotOutput("test_plot", height = "185px"),
                    type = 8, color = "#15131e"
                  )
                )
              ),
              bslib::card(
                full_screen = TRUE,
                class       = "card-light",
                bslib::card_header("Training Set"),
                bslib::card_body(
                  shinycssloaders::withSpinner(
                    plotOutput("train_plot", height = "185px"),
                    type = 8, color = "#15131e"
                  )
                )
              )
            )
          ),
          bslib::card(
            class = "card-light",
            bslib::card_header("Feature Importance Rank"),
            bslib::card_body(uiOutput("features_to_show_slider_ui"))
          ),
          bslib::card(
            full_screen = TRUE,
            class       = "card-light",
            bslib::card_header("Feature Importance"),
            bslib::card_body(
              shinycssloaders::withSpinner(
                plotOutput("feature_importance_plot", height = "500px"),
                type = 8, color = "#15131e"
              )
            )
          )
        )
      )
    )
  )
}
