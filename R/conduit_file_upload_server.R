conduit_database_server <- function(id, conduit_obj, qf, metrics, colData, rowData) {
  moduleServer(id, function(input, output, session) {

    # Stats box outputs
    output$num_samples <- renderText({
      req(conduit_obj())
      nrow(colData())
    })

    output$num_species_detected <- renderText({
      req(conduit_obj())
      metrics()$protein_coverage_species |>
        dplyr::pull(taxon) |>
        length()
    })

    output$per_species_detected <- renderText({
      req(conduit_obj())
      metrics()$protein_coverage_species |>
        dplyr::pull(taxon) |>
        length()
    })

    output$num_proteins_detected <- renderText({
      req(qf())
      nrow(SummarizedExperiment::rowData(qf()[["protein_groups"]]))
    })

    output$per_proteins_detected <- renderText({
      req(conduit_obj())
      nrow(SummarizedExperiment::rowData(qf()[["protein_groups"]]))
    })

    # Taxonomic tree plot
    taxa_tree_plot <- reactive({
      req(conduit_obj())
      conduitR::plot_percent_detected_taxa_tree(conduit_obj())
    })

    output$taxa_tree_plot <- renderPlot({
      taxa_tree_plot()
    })

    # Species coverage table
    protein_taxonomy_table <- reactive({
      req(metrics())
      dplyr::arrange(metrics()$protein_coverage_species, dplyr::desc(coverage))
    })

    output$protein_taxonomy <- DT::renderDT({
      conduit_datatable(protein_taxonomy_table())
    })

    output$download_protein_taxonomy <- downloadHandler(
      filename = function() {
        "protein_taxonomy_coverage.csv"
      },
      content = function(file) {
        readr::write_csv(protein_taxonomy_table(), file)
      }
    )

    # Return taxa tree plot for download-current-plot handler
    return(taxa_tree_plot)
  })
}
