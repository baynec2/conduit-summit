conduit_pathway_server <- function(id, conduit_obj, limma_stats_results, session_parent) {
  moduleServer(id, function(input, output, session) {

    output$pathway_select_ui <- renderUI({
      req(conduit_obj())

      kegg_pathways <- conduit_obj()@annotations |>
        dplyr::filter(annotation_type == "kegg_pathway") |>
        dplyr::select(organism_id, term, description) |>
        dplyr::distinct()

      taxonomy <- conduit_obj()@taxonomy |>
        dplyr::select(organism_id, species)

      kegg_pathway_with_taxa <- dplyr::right_join(taxonomy, kegg_pathways, by = "organism_id") |>
        dplyr::mutate(id = paste0(species, ": ", description))

      possible_kegg_ids <- kegg_pathway_with_taxa$term
      names(possible_kegg_ids) <- kegg_pathway_with_taxa$id

      selectInput("selected_kegg_pathway", "Select Kegg Pathway To Show",
        choices = possible_kegg_ids, multiple = FALSE)
    })

    pathway_plot <- reactive({
      req(limma_stats_results(), input$selected_kegg_pathway)
      plot_kegg_pathway(
        stats_results = limma_stats_results(),
        kegg_pathway_id = input$selected_kegg_pathway
      )
    })

    output$pathway_plot <- plotly::renderPlotly({
      plotly::ggplotly(pathway_plot())
    })

    observeEvent(input$pathway_return_to_stats_button, {
      bslib::nav_select("analysis_tabs", "Statistics", session = session_parent)
    })

    return(pathway_plot)
  })
}
