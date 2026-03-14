conduit_enrichment_server <- function(id, conduit_obj, limma_stats_results,
                                       limma_fc_threshold, limma_p_threshold,
                                       session_parent) {
  moduleServer(id, function(input, output, session) {

    output$enrichment_plot_options_ui <- renderUI({
      req(input$enrichment_type)
      if (input$enrichment_type == "gsea") {
        selectInput("enrichment_plot_type", "Choose Enrichment Plot Type",
          choices = c("ridgeplot", "dotplot", "treeplot", "upsetplot"),
          selected = "ridgeplot")
      } else if (input$enrichment_type == "ora") {
        selectInput("enrichment_plot_type", "Choose Enrichment Plot Type",
          choices = c("barplot", "dotplot", "treeplot", "upsetplot", "cnetplot"),
          selected = "barplot")
      }
    })

    output$enrichment_direction_ui <- renderUI({
      req(input$enrichment_type)
      if (input$enrichment_type == "gsea") {
        selectInput("enrichment_direction", "Direction of Change for Enrichment",
          choices = c("both"), multiple = FALSE, selected = "both")
      } else if (input$enrichment_type == "ora") {
        selectInput("enrichment_direction", "Direction of Change for Enrichment",
          choices = c("up", "down"), multiple = FALSE, selected = "up")
      }
    })

    enrichment_results <- eventReactive(input$run_enrichment, {
      req(limma_stats_results(), conduit_obj(), input$annotation_type, input$enrichment_type)

      if (input$enrichment_type == "gsea") {
        conduitR::perform_gsea(
          limma_stats_results(),
          conduit = conduit_obj(),
          annotation_type = input$annotation_type,
          ranking_column = "logFC"
        )
      } else if (input$enrichment_type == "ora") {
        req(input$enrichment_direction, limma_fc_threshold())
        conduitR::perform_ora(
          limma_stats_results(),
          direction = input$enrichment_direction,
          conduit = conduit_obj(),
          annotation_type = input$annotation_type,
          adj_pval_threshold = limma_p_threshold(),
          logFC_threshold = if (input$enrichment_direction == "down") {
            limma_fc_threshold() * -1
          } else {
            limma_fc_threshold()
          }
        )
      }
    })

    output$enrichment_summary <- renderPrint({
      req(enrichment_results())
      enrichment_results()
    })

    enrichment_plot <- reactive({
      req(enrichment_results())
      if (input$enrichment_plot_type == "ridgeplot") {
        enrichplot::ridgeplot(enrichment_results())
      } else if (input$enrichment_plot_type == "dotplot") {
        enrichplot::dotplot(enrichment_results())
      } else if (input$enrichment_plot_type == "treeplot") {
        pw <- enrichplot::pairwise_termsim(enrichment_results())
        enrichplot::treeplot(pw)
      } else if (input$enrichment_plot_type == "barplot") {
        barplot(enrichment_results())
      } else if (input$enrichment_plot_type == "cnetplot") {
        enrichplot::cnetplot(enrichment_results())
      }
    })

    output$enrichment_plot <- renderPlot({
      enrichment_plot()
    })

    observeEvent(input$enrichment_return_to_stats_button, {
      updateTabItems(session_parent, "main_tabs", "analysis")
      updateTabsetPanel(session_parent, "analysis_tabs", selected = "stats")
    })

    return(enrichment_plot)
  })
}
