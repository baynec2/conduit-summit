conduit_pathway_server <- function(id, conduit_obj, limma_stats_results, session_parent) {
  moduleServer(id, function(input, output, session) {

    # ── Is limma available? ──────────────────────────────────────────────────
    limma_available <- reactive({
      tryCatch(!is.null(limma_stats_results()), error = function(e) FALSE)
    })

    # Disable all controls until limma has been run
    controls <- c("kegg_annotation_type", "significant_only",
                  "pathway_fc_threshold", "pathway_p_threshold", "run_pathway")

    observe({
      if (limma_available()) {
        purrr::walk(controls, shinyjs::enable)
      } else {
        purrr::walk(controls, shinyjs::disable)
      }
    })

    # Informative notice shown until limma is run
    output$pathway_stats_notice <- renderUI({
      if (limma_available()) return(NULL)
      tags$div(
        class = "alert alert-warning p-2 mb-3",
        style = "font-size: 0.8rem; line-height: 1.4;",
        icon("triangle-exclamation", style = "margin-right: 4px;"),
        "Run an analysis in the ",
        tags$strong("Statistics"), " tab first to enable pathway controls."
      )
    })

    # ── Pathway select dropdown ──────────────────────────────────────────────
    output$pathway_select_ui <- renderUI({
      req(conduit_obj(), input$kegg_annotation_type)

      kegg_pathways <- conduit_obj()@annotations |>
        dplyr::filter(annotation_type == input$kegg_annotation_type) |>
        dplyr::select(term, description) |>
        dplyr::distinct() |>
        dplyr::mutate(
          term        = as.character(term),
          description = as.character(description),
          label       = dplyr::if_else(
            is.na(description) | description == "",
            term,
            paste0(term, ": ", description)
          )
        )

      possible_kegg_ids        <- kegg_pathways$term
      names(possible_kegg_ids) <- kegg_pathways$label

      disabled <- !limma_available()
      selectInput(session$ns("selected_kegg_pathway"), "Select KEGG Pathway",
        choices = possible_kegg_ids, multiple = FALSE) |>
        (\(x) if (disabled) shinyjs::disabled(x) else x)()
    })

    # ── Data for plot ────────────────────────────────────────────────────────
    stats_for_plot <- reactive({
      req(limma_stats_results())
      stats <- limma_stats_results()
      if (isTRUE(input$significant_only)) {
        req(input$pathway_fc_threshold, input$pathway_p_threshold)
        stats <- dplyr::filter(stats,
          abs(logFC) >= input$pathway_fc_threshold,
          adj.P.Val  <= input$pathway_p_threshold
        )
      }
      stats
    })

    # ── Plot ─────────────────────────────────────────────────────────────────
    pathway_plot <- eventReactive(input$run_pathway, {
      req(stats_for_plot(), input$selected_kegg_pathway)
      use_ko <- input$kegg_annotation_type == "kegg_map_pathway"
      plot_kegg_pathway(
        stats_results   = stats_for_plot(),
        kegg_pathway_id = input$selected_kegg_pathway,
        use_ko          = use_ko
      )
    })

    output$pathway_plot <- plotly::renderPlotly({
      req(input$run_pathway > 0)
      plotly::ggplotly(pathway_plot())
    })

    observeEvent(input$pathway_return_to_stats_button, {
      bslib::nav_select("analysis_tabs", "Statistics", session = session_parent)
    })

    return(pathway_plot)
  })
}
