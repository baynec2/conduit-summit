conduit_filter_data_server <- function(id, qf, colData, selected_assay) {
  moduleServer(id, function(input, output, session) {

    sample_vars <- reactive({
      req(colData())
      colnames(colData())
    })

    feature_vars <- reactive({
      req(qf(), selected_assay())
      colnames(SummarizedExperiment::rowData(qf()[[selected_assay()]]))
    })

    output$sample_filters <- renderUI({
      req(sample_vars(), colData())
      lapply(sample_vars(), function(var) {
        if (is.list(colData()[[var]])) return(NULL)
        vals <- unique(colData()[[var]])
        pickerInput(
          session$ns(paste0("sample_", var)),
          label = var,
          choices = vals,
          selected = vals,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        )
      })
    })

    output$feature_filters <- renderUI({
      req(qf(), selected_assay())
      if (!(selected_assay() %in% names(qf()))) return(NULL)

      se <- qf()[[selected_assay()]]
      rd <- SummarizedExperiment::rowData(se)

      # Only show pickerInputs for columns with <50 unique scalar values;
      # skip list-type columns (e.g. GO term lists) which cause shinyWidgets
      # to throw "All sub-lists in 'choices' must be named".
      lapply(feature_vars(), function(var) {
        if (is.list(rd[[var]])) return(NULL)
        vals <- unique(rd[[var]])
        if (length(vals) >= 50) return(NULL)
        pickerInput(
          session$ns(paste0("feature_", var)),
          label = var,
          choices = vals,
          selected = vals,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        )
      })
    })

    filtered_qf <- reactive({
      req(qf(), colData(), selected_assay(), sample_vars(), feature_vars())

      qf_filtered <- qf()

      for (var in sample_vars()) {
        sel <- input[[paste0("sample_", var)]]
        if (!is.null(sel) && length(sel) > 0) {
          keep_samples <- SummarizedExperiment::colData(qf_filtered)[[var]] %in% sel
          if (!any(keep_samples)) return(NULL)
          qf_filtered <- qf_filtered[, keep_samples]
        }
      }

      if (!(selected_assay() %in% names(qf_filtered))) return(NULL)

      se <- qf_filtered[[selected_assay()]]
      if (is.null(se) || nrow(se) == 0) return(NULL)

      for (var in feature_vars()) {
        sel <- input[[paste0("feature_", var)]]
        if (!is.null(sel) && length(sel) > 0) {
          keep_features <- SummarizedExperiment::rowData(se)[[var]] %in% sel
          if (!any(keep_features)) return(NULL)
          se <- se[keep_features, ]
        }
      }

      qf_filtered[[selected_assay()]] <- se
      qf_filtered
    })

    output$filtered_qfeatures <- renderPrint({
      req(filtered_qf())
      attr(filtered_qf(), "ExperimentList")
    })

    # Return filtered_qf so the main server can pass it downstream
    return(filtered_qf)
  })
}
