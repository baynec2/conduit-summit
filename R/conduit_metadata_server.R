conduit_metadata_server <- function(id, conduit_obj, colData) {
  moduleServer(id, function(input, output, session) {

    output$num_continuous_variables <- renderText({
      req(colData())
      sum(sapply(colData(), is.numeric))
    })

    output$num_discrete_variables <- renderText({
      req(colData())
      sum(sapply(colData(), function(x) !is.numeric(x)))
    })

    output$colData <- DT::renderDataTable({
      req(colData())
      DT::datatable(
        as.data.frame(colData()),
        options = list(scrollX = TRUE)
      )
    })

    output$download_colData_table <- downloadHandler(
      filename = function() {
        "colData.csv"
      },
      content = function(file) {
        readr::write_csv(as.data.frame(colData()), file)
      }
    )

    output$metadata_variable_choices_to_plot_ui <- renderUI({
      req(colData())
      selectInput(
        "metadata_variable_choices_to_plot",
        "Choose variable to plot",
        choices = names(colData()),
        selected = NULL
      )
    })

    metadata_distribution_plot <- reactive({
      req(conduit_obj(), input$metadata_variable_choices_to_plot, colData())
      conduitR::plot_colData_distribution(
        conduit_obj(),
        input$metadata_variable_choices_to_plot
      )
    })

    output$metadata_distribution_plot <- renderPlot({
      metadata_distribution_plot()
    })

    # Return the plot reactive for the download-current-plot handler
    return(metadata_distribution_plot)
  })
}
