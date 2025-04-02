conduit_data_filter_server = function(){
  # Get sample variables
  sample_vars <- colnames(colData(qf))
  feature_vars <- colnames(rowData(qf[[1]]))  # assuming we filter on the first assay

  # Global sample filters
  output$sample_filters <- renderUI({
    lapply(sample_vars, function(var) {
      pickerInput(paste0("sample_", var),
                  label = var,
                  choices = unique(colData(qf)[[var]]),
                  selected = unique(colData(qf)[[var]]),
                  multiple = TRUE)
    })
  })

  # Feature filters for selected assay
  output$feature_filters <- renderUI({
    req(input$selected_assay)
    se <- qf[[input$selected_assay]]
    lapply(feature_vars, function(var) {
      pickerInput(paste0("feature_", var),
                  label = var,
                  choices = unique(rowData(se)[[var]]),
                  selected = unique(rowData(se)[[var]]),
                  multiple = TRUE)
    })
  })

  # Filtering
  filtered_qf <- reactive({
    qf_filtered <- qf

    # Sample filters
    for (var in sample_vars) {
      sel <- input[[paste0("sample_", var)]]
      if (!is.null(sel)) {
        keep_samples <- colData(qf_filtered)[[var]] %in% sel
        qf_filtered <- qf_filtered[, keep_samples]
      }
    }

    # Feature filters only for selected assay
    if (!is.null(input$selected_assay)) {
      se <- qf_filtered[[input$selected_assay]]
      for (var in feature_vars) {
        sel <- input[[paste0("feature_", var)]]
        if (!is.null(sel)) {
          keep_features <- rowData(se)[[var]] %in% sel
          se <- se[keep_features, ]
        }
      }
      qf_filtered[[input$selected_assay]] <- se
    }

    qf_filtered
  })

  # Show filtered object (for testing)
  output$filtered_result <- renderPrint({
    filtered_qf()
  })
}
