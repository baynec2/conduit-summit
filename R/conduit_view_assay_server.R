conduit_view_assay_server <- function(id, final_qf, selected_assay, processed_assay) {
  moduleServer(id, function(input, output, session) {

    # Update assay choices whenever final_qf changes; default to processed assay
    observe({
      req(final_qf())
      choices <- names(final_qf())
      sel <- if (!is.null(processed_assay()) && processed_assay() %in% choices) {
        processed_assay()
      } else {
        choices[1]
      }
      updateSelectInput(session, "assay_to_show", choices = choices, selected = sel)
    })

    # Reactive: wide data frame — features as rows, samples as columns
    assay_df <- reactive({
      req(final_qf(), input$assay_to_show)
      validate(need(
        input$assay_to_show %in% names(final_qf()),
        "Selected assay not found in the processed data."
      ))

      se  <- final_qf()[[input$assay_to_show]]
      mat <- SummarizedExperiment::assay(se)
      df  <- as.data.frame(mat)

      if (isTRUE(input$include_row_data)) {
        rd <- as.data.frame(SummarizedExperiment::rowData(se))
        if (ncol(rd) > 0) {
          df <- cbind(rd, df)
        }
      }
      df
    })

    output$n_samples <- renderText({
      req(final_qf(), input$assay_to_show %in% names(final_qf()))
      ncol(SummarizedExperiment::assay(final_qf()[[input$assay_to_show]]))
    })

    output$n_features <- renderText({
      req(final_qf(), input$assay_to_show %in% names(final_qf()))
      nrow(SummarizedExperiment::assay(final_qf()[[input$assay_to_show]]))
    })

    output$table_title <- renderText({
      req(input$assay_to_show)
      paste0("Assay: ", input$assay_to_show)
    })

    output$assay_table <- DT::renderDT({
      df <- assay_df()
      req(df)
      dt <- DT::datatable(
        df,
        class   = "table table-hover table-sm",
        escape  = FALSE,
        options = list(
          pageLength = 15,
          scrollX    = TRUE,
          autoWidth  = TRUE,
          dom        = '<"conduit-dt-top d-flex justify-content-between align-items-center mb-2"f>t<"conduit-dt-bottom d-flex justify-content-between align-items-center mt-2"ip>',
          language   = list(
            search            = "",
            searchPlaceholder = "Search\u2026",
            paginate          = list(previous = "\u2039", `next` = "\u203a"),
            info              = "Showing _START_\u2013_END_ of _TOTAL_"
          ),
          columnDefs = conduit_truncate_column_defs()
        )
      )
      double_cols <- names(df)[sapply(df, is.double)]
      if (length(double_cols) > 0) {
        dt <- DT::formatRound(dt, columns = double_cols, digits = 2)
      }
      dt
    })

    output$download_assay_csv <- downloadHandler(
      filename = function() { paste0(input$assay_to_show, ".csv") },
      content  = function(file) {
        req(assay_df())
        readr::write_csv(assay_df(), file)
      }
    )
  })
}
