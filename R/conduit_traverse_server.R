conduit_traverse_server <- function(id, conduit_obj, qf, colData) {
  moduleServer(id, function(input, output, session) {

    observe({
      req(qf(), input$traverse_features)
      updateSelectInput(session, inputId = "traverse_assay", choices = names(qf()))
    })

    observe({
      req(colData())
      updateSelectInput(session, inputId = "traverse_xaxis", choices = names(colData()))
    })

    traverse_data <- reactive({
      req(qf(), input$traverse_features, input$traverse_assay)

      se <- qf()[input$traverse_features, ][[input$traverse_assay]]

      cd <- colData() |>
        as.data.frame() |>
        tibble::rownames_to_column("sample")

      assay_df <- as.data.frame(SummarizedExperiment::assay(se)) |>
        tibble::rownames_to_column("feature_id") |>
        tidyr::pivot_longer(
          cols = everything()[-1],
          names_to = "sample",
          values_to = "intensity"
        )

      dplyr::left_join(assay_df, cd, by = "sample")
    })

    output$qf_plot <- plotly::renderPlotly({
      req(conduit_obj())
      plot(conduit_obj()@QFeatures, interactive = TRUE)
    })

    output$traverse_info <- DT::renderDT({
      req(qf(), input$traverse_features)
      se <- qf()[input$traverse_features, ]
      dims <- sapply(SummarizedExperiment::assays(se), dim)
      rownames(dims) <- c("# Features", "# Samples")
      as.data.frame(dims[1, ])
    })

    traverse_plot <- reactive({
      req(traverse_data(), input$traverse_xaxis)
      traverse_data() |>
        ggplot2::ggplot(ggplot2::aes(!!rlang::sym(input$traverse_xaxis), y = intensity)) +
        ggplot2::geom_point() +
        ggplot2::ylab(paste(input$traverse_assay, " Intensity"))
    })

    output$traverse_plot <- renderPlot({
      traverse_plot()
    })

    return(traverse_plot)
  })
}
