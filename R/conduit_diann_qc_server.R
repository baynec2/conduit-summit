conduit_diann_qc_server <- function(id, conduit_obj, metrics) {
  moduleServer(id, function(input, output, session) {

    output$diann_qc_columns_ui <- renderUI({
      req(metrics())
      selectInput(
        "diann_qc_metric",
        "Choose QC metric to plot",
        choices = names(metrics()$diann_stats),
        selected = "Proteins.Identified"
      )
    })

    diann_qc_plot <- reactive({
      req(conduit_obj(), input$diann_qc_metric)
      conduitR::plot_conduit_metric_diann(conduit_obj(), input$diann_qc_metric)
    })

    output$diann_qc_plot <- renderPlot({
      diann_qc_plot()
    })

    diann_stats <- reactive({
      req(metrics())
      metrics()$diann_stats
    })

    output$diann_qc_table <- DT::renderDT({
      diann_stats()
    })

    output$download_diann_stats <- downloadHandler(
      filename = function() {
        "diann_stats.csv"
      },
      content = function(file) {
        readr::write_csv(diann_stats(), file)
      }
    )

    # Return the plot reactive so the download-current-plot handler can use it
    return(diann_qc_plot)
  })
}
