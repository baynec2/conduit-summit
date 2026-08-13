conduit_analysis_tab_ui <- function(id = "analysis") {
  ns <- NS(id)

  # Thin right sidebar used by several sub-panels
  ctrl_sidebar <- function(..., width = 260) {
    bslib::sidebar(
      title  = "Plot Controls",
      width  = width,
      bg     = "#faf6ee",
      fg     = "#3b352a",
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
            type = 8, caption = "Loading feature number plot...", color = "#2f2a20"
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
              type = 8, caption = "Loading missing value heatmap...", color = "#2f2a20"
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
              type = 8, caption = "Loading sample correlation heatmap...", color = "#2f2a20"
            )
          )
        ),

        bslib::nav_panel(
          "Intensity Distribution",
          shinycssloaders::withSpinner(
            plotOutput(ns("intensity_distribution_plot"), height = "600px"),
            type = 8, caption = "Loading intensity distribution...", color = "#2f2a20"
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
              type = 8, caption = "Loading density plot...", color = "#2f2a20"
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
          type = 8, caption = "Loading PCA plot...", color = "#2f2a20"
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
          type = 8, caption = "Loading relative abundance plot...", color = "#2f2a20"
        )
      )
    ),

    # ── Explore ──────────────────────────────────────────────────────────────
    bslib::nav_panel(
      "Explore",
      bslib::layout_sidebar(
        sidebar = ctrl_sidebar(
          width = 300,
          bslib::accordion(
            open = c("Group & Summarize", "Plot Controls"),
            bslib::accordion_panel(
              "Group & Summarize", icon = icon("layer-group"),
              uiOutput(ns("explore_group_by_ui")),
              selectInput(
                ns("explore_summary_fn"), "Summarize intensity by",
                choices = c(
                  "Mean"      = "mean",
                  "Median"    = "median",
                  "Sum"       = "sum",
                  "Std dev"   = "sd",
                  "N (count)" = "n"
                ),
                selected = "mean"
              ),
              actionButton(
                ns("explore_apply"), "Apply",
                icon = icon("play"), class = "btn-primary w-100"
              )
            ),
            bslib::accordion_panel(
              "Plot Controls", icon = icon("sliders"),
              selectInput(
                ns("explore_plot_type"), "Plot type",
                choices = c(
                  "Scatter"   = "scatter",
                  "Bar"       = "bar",
                  "Line"      = "line",
                  "Boxplot"   = "boxplot",
                  "Violin"    = "violin",
                  "Histogram" = "histogram"
                ),
                selected = "boxplot"
              ),
              uiOutput(ns("explore_x_axis_ui")),
              uiOutput(ns("explore_y_axis_ui")),
              uiOutput(ns("explore_color_ui")),
              uiOutput(ns("explore_shape_ui")),
              textInput(ns("explore_facet_formula"), "Facet formula", value = "~NULL")
            )
          )
        ),
        shinycssloaders::withSpinner(
          plotOutput(ns("explore_plot"), height = "600px"),
          type = 8, caption = "Building plot...", color = "#2f2a20"
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
          bg     = "#faf6ee",
          fg     = "#3b352a",
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
              uiOutput(ns("limma_volcano_color_ui")),
              numericInput(ns("limma_fc_threshold"), "LogFC threshold", value = 1, min = 0),
              sliderInput(
                ns("limma_p_threshold"), "Adjusted p-value threshold",
                min = 0, max = 1, value = 0.05, step = 0.01
              )
            ),
            bslib::accordion_panel(
              "Feature Plot Options", icon = icon("magnifying-glass-chart"),
              uiOutput(ns("selected_feature_plot_x_axis_ui")),
              uiOutput(ns("selected_feature_plot_facet_formula_ui")),
              uiOutput(ns("selected_feature_plot_color_ui")),
              uiOutput(ns("selected_feature_plot_shape_ui")),
              uiOutput(ns("selected_feature_plot_data_type_ui")),
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
                  plotly::plotlyOutput(ns("limma_volcano_plot"), height = "480px"),
                  type = 8, caption = "Loading volcano plot...", color = "#2f2a20"
                )
              )
            ),
            bslib::card(
              full_screen = TRUE,
              class       = "card-light",
              bslib::card_header(
                class = "d-flex justify-content-between align-items-center",
                "Selected Feature Plot",
                actionButton(
                  ns("clear_volcano_selection"),
                  tagList(icon("xmark"), " Clear Selection"),
                  class = "btn-outline-secondary btn-sm"
                )
              ),
              bslib::card_body(
                shinycssloaders::withSpinner(
                  plotOutput(ns("selected_feature_plot"), height = "480px"),
                  type = 8, caption = "Loading feature plot...", color = "#2f2a20"
                )
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
                type = 8, caption = "Loading statistics table...", color = "#2f2a20"
              )
            ),
            bslib::card_footer(
              downloadButton(ns("download_limma_stats_table"), "Download Table")
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
      conduit_prediction_tab_ui("prediction")
    )
  )
}
