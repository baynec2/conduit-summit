conduit_prediction_tab_ui <- function(id) {
  ns <- NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title  = "Model Settings",
      width  = 280,
      bg     = "#f8f9fb",
      fg     = "#1a1a2e",
      open   = "open",
      selectInput(ns("outcome_var"), "Outcome variable", choices = NULL),
      sliderInput(ns("split_ratio"), "Train/Test split % (train)", min = 50, max = 90, value = 70),
      selectInput(
        ns("model_type"), "Model type",
        choices = c("lasso_regression", "random_forest", "xgboost")
      ),
      checkboxInput(ns("show_advanced"), "Show advanced options", value = FALSE),
      conditionalPanel(
        condition = sprintf("input['%s'] === true", ns("show_advanced")),
        hr(),
        numericInput(ns("cv_folds"), "CV folds:", value = 5, min = 2, max = 20),
        numericInput(ns("random_seed"), "Random seed:", value = 123)
      ),
      selectInput(
        ns("model_plot_type"), "Plot type",
        choices = c("ROC", "precision_recall")
      ),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Feature Importance Rank",
          icon = shiny::icon("sliders"),
          uiOutput(ns("features_to_show_slider_ui"))
        )
      ),
      hr(),
      shinyjs::hidden(
        div(
          id = ns("model_running_banner"),
          class = "alert alert-info p-2 mb-2 small",
          style = "border-radius:6px;",
          tags$strong("Model is running...")
        )
      ),
      uiOutput(ns("model_status_banner")),
      actionButton(
        ns("run_classification_model"), "Run Model",
        icon = icon("cogs"), class = "btn-primary w-100"
      )
    ),
    uiOutput(ns("prediction_main_content"))
  )
}
