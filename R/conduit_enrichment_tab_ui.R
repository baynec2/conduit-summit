conduit_enrichment_tab_ui <- function(id = "enrichment") {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title  = "Enrichment Controls",
      width  = 280,
      bg     = "#f8f9fb",
      fg     = "#1a1a2e",
      open   = "open",
      actionButton(
        ns("enrichment_return_to_stats_button"),
        tagList(icon("arrow-left"), " Return to Stats"),
        class = "btn-outline-secondary btn-sm w-100 mb-3"
      ),
      tags$div(
        class = "alert alert-info p-2 mb-3",
        style = "font-size: 0.8rem; line-height: 1.4;",
        icon("circle-info", style = "margin-right: 4px;"),
        "Results use the formula and contrast configured in the ",
        tags$strong("Statistics"), " tab."
      ),
      selectInput(
        ns("annotation_type"), "Term Type",
        choices = c("go", "kegg", "domain", "kingdom", "phylum", "class",
                    "order", "family", "genus", "species")
      ),
      selectInput(
        ns("enrichment_type"), "Enrichment Method",
        choices = c("gsea", "ora")
      ),
      uiOutput(ns("enrichment_direction_ui")),
      uiOutput(ns("enrichment_plot_options_ui")),
      hr(),
      actionButton(
        ns("run_enrichment"), "Run Enrichment",
        icon  = icon("play"),
        class = "btn-primary w-100"
      )
    ),
    tagList(
      uiOutput(ns("enrichment_summary_card")),
      bslib::card(
        full_screen = TRUE,
        class       = "card-light",
        bslib::card_header("Enrichment Results"),
        bslib::card_body(
          shinycssloaders::withSpinner(
            plotOutput(ns("enrichment_plot"), height = "600px"),
            type = 8, caption = "Generating enrichment plot...",
            color = "#15131e"
          )
        )
      )
    )
  )
}
