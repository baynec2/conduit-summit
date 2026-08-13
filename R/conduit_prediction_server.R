conduit_prediction_server <- function(id, final_qf, processed_assay, final_colData_names) {
  moduleServer(id, function(input, output, session) {

    final_colData <- reactive({
      req(final_qf())
      SummarizedExperiment::colData(final_qf())
    })

    # Populate outcome variable with all colData columns
    observe({
      req(final_colData_names())
      updateSelectInput(session, "outcome_var", choices = final_colData_names(), selected = NULL)
    })

    # Detect outcome type from the selected variable
    outcome_type <- reactive({
      req(input$outcome_var, final_colData())
      col <- final_colData()[[input$outcome_var]]
      if (is.numeric(col)) "regression"
      else if (nlevels(as.factor(col)) == 2) "binary_classification"
      else "multiclass_classification"
    })

    # Show/hide plot type selector — only relevant for binary classification
    observe({
      req(outcome_type())
      if (outcome_type() == "binary_classification") {
        shinyjs::show(session$ns("model_plot_type"))
      } else {
        shinyjs::hide(session$ns("model_plot_type"))
      }
    })

    # Async model task
    model_task <- ExtendedTask$new(function(qf, assay_name, outcome, train_pct,
                                            model_type, cv_folds, seed, otype) {
      future::future({
        withr::with_seed(seed, {
          if (otype == "regression") {
            conduitR::predict_regression(
              qf, assay_name, outcome, train_pct, model_type, cv_folds
            )
          } else {
            conduitR::predict_classification(
              qf, assay_name, outcome, train_pct, model_type, cv_folds
            )
          }
        })
      })
    })

    observeEvent(input$run_classification_model, {
      req(
        final_qf(), processed_assay(), input$outcome_var, input$split_ratio,
        input$model_type, input$cv_folds, input$random_seed, outcome_type()
      )
      model_task$invoke(
        final_qf(), processed_assay(), input$outcome_var,
        input$split_ratio, input$model_type, input$cv_folds,
        input$random_seed, outcome_type()
      )
    })

    # Show running banner instantly on click (client-side, no server round-trip)
    shinyjs::onclick(session$ns("run_classification_model"),
                     shinyjs::show(session$ns("model_running_banner")))

    observeEvent(input$cancel_model, {
      model_task$cancel()
    })

    observeEvent(model_task$status(), {
      if (model_task$status() %in% c("success", "error", "cancelled", "idle")) {
        shinyjs::hide(session$ns("model_running_banner"))
      }
    })

    # Sidebar status banner — Cancel button while running, error message on failure
    output$model_status_banner <- renderUI({
      switch(model_task$status(),
        running = actionButton(
          session$ns("cancel_model"), "Cancel",
          icon = icon("times"), class = "btn-danger w-100 mb-2"
        ),
        error = tags$div(
          class = "alert alert-danger p-2 mb-2 small",
          style = "border-radius:6px;",
          tags$strong("Error: "),
          conditionMessage(model_task$result())
        ),
        NULL
      )
    })

    # Returns the task result (reactive — updates when task finishes)
    model_result <- reactive({
      model_task$result()
    })

    # Helper: returns a waiting_plot for any non-success state, NULL on success
    model_waiting_plot <- reactive({
      switch(model_task$status(),
        idle      = waiting_plot("Configure settings and click \u201cRun Model\u201d"),
        running   = waiting_plot("Model is running\u2026"),
        cancelled = waiting_plot("Model run was cancelled"),
        error     = waiting_plot(paste("Error:", conditionMessage(model_result()))),
        NULL
      )
    })

    # Dynamic main content — layout changes based on outcome type
    output$prediction_main_content <- renderUI({
      req(outcome_type())
      otype <- outcome_type()

      feature_importance_card <- bslib::card(
        full_screen = TRUE,
        class       = "card-light",
        bslib::card_header("Feature Importance"),
        bslib::card_body(
          shinycssloaders::withSpinner(
            plotOutput(session$ns("feature_importance_plot"), height = "500px"),
            type = 8, color = "#2f2a20"
          )
        )
      )

      if (otype == "regression") {
        tagList(
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              full_screen = TRUE,
              class       = "card-light",
              bslib::card_header("Test Set \u2014 Predicted vs Actual"),
              bslib::card_body(
                shinycssloaders::withSpinner(
                  plotOutput(session$ns("test_plot"), height = "400px"),
                  type = 8, color = "#2f2a20"
                )
              )
            ),
            bslib::card(
              full_screen = TRUE,
              class       = "card-light",
              bslib::card_header("Training Set \u2014 Predicted vs Actual"),
              bslib::card_body(
                shinycssloaders::withSpinner(
                  plotOutput(session$ns("train_plot"), height = "400px"),
                  type = 8, color = "#2f2a20"
                )
              )
            )
          ),
          feature_importance_card
        )
      } else if (otype == "binary_classification") {
        tagList(
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              full_screen = TRUE,
              class       = "card-light",
              bslib::card_header("Confusion Matrix"),
              bslib::card_body(
                shinycssloaders::withSpinner(
                  plotOutput(session$ns("confusion_matrix_plot"), height = "400px"),
                  type = 8, caption = "Loading confusion matrix...", color = "#2f2a20"
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
                    plotOutput(session$ns("test_plot"), height = "185px"),
                    type = 8, color = "#2f2a20"
                  )
                )
              ),
              bslib::card(
                full_screen = TRUE,
                class       = "card-light",
                bslib::card_header("Training Set"),
                bslib::card_body(
                  shinycssloaders::withSpinner(
                    plotOutput(session$ns("train_plot"), height = "185px"),
                    type = 8, color = "#2f2a20"
                  )
                )
              )
            )
          ),
          feature_importance_card
        )
      } else {
        # multiclass_classification — ROC/PR not applicable
        tagList(
          bslib::card(
            full_screen = TRUE,
            class       = "card-light",
            bslib::card_header("Confusion Matrix"),
            bslib::card_body(
              shinycssloaders::withSpinner(
                plotOutput(session$ns("confusion_matrix_plot"), height = "500px"),
                type = 8, caption = "Loading confusion matrix...", color = "#2f2a20"
              )
            )
          ),
          feature_importance_card
        )
      }
    })

    # Plot renderers
    output$confusion_matrix_plot <- renderPlot({
      msg <- model_waiting_plot()
      if (!is.null(msg)) return(msg)
      conduitR::plot_confusion_matrix(model_result())
    })

    output$test_plot <- renderPlot({
      msg <- model_waiting_plot()
      if (!is.null(msg)) return(msg)
      result <- model_result()
      if (outcome_type() == "regression") {
        conduitR::plot_predicted_vs_actual(result, "test")
      } else {
        req(input$model_plot_type)
        if (input$model_plot_type == "ROC") conduitR::plot_roc(result, "test")
        else conduitR::plot_precision_recall(result, "test")
      }
    })

    output$train_plot <- renderPlot({
      msg <- model_waiting_plot()
      if (!is.null(msg)) return(msg)
      result <- model_result()
      if (outcome_type() == "regression") {
        conduitR::plot_predicted_vs_actual(result, "training")
      } else {
        req(input$model_plot_type)
        if (input$model_plot_type == "ROC") conduitR::plot_roc(result, "training")
        else conduitR::plot_precision_recall(result, "training")
      }
    })

    # Feature importance slider
    output$features_to_show_slider_ui <- renderUI({
      if (model_task$status() != "success") return(NULL)
      max_val <- max(length(model_result()$importance$feature), na.rm = TRUE)
      sliderInput(
        session$ns("features_to_show_slider"),
        "Select rank of features to show",
        min   = 1,
        max   = max_val,
        value = c(1, min(10, max_val))
      )
    })

    output$feature_importance_plot <- renderPlot({
      msg <- model_waiting_plot()
      if (!is.null(msg)) return(msg)
      from <- input$features_to_show_slider[1] %||% 1
      to   <- input$features_to_show_slider[2] %||% 10
      conduitR::plot_feature_importance(model_result(), from, to)
    })

  })
}
