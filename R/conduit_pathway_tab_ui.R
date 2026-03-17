conduit_pathway_tab_ui <- function(id = "pathway") {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title  = "Pathway Controls",
      width  = 280,
      bg     = "#f8f9fb",
      fg     = "#1a1a2e",
      open   = "open",
      actionButton(
        ns("pathway_return_to_stats_button"),
        tagList(icon("arrow-left"), " Return to Stats"),
        class = "btn-outline-secondary btn-sm w-100 mb-3"
      ),
      uiOutput(ns("pathway_stats_notice")),
      tags$strong("Annotation Settings"),
      selectInput(
        ns("kegg_annotation_type"), "Annotation Type",
        choices = c(
          "KEGG Map Pathway (KO)"                    = "kegg_map_pathway",
          "UniProt KEGG Pathway (organism-specific)" = "uniprot_kegg_pathway"
        )
      ),
      uiOutput(ns("pathway_select_ui")),
      hr(),
      tags$strong("Protein Filter"),
      checkboxInput(ns("significant_only"), "Significant proteins only", value = FALSE),
      conditionalPanel(
        condition = sprintf("input['%s'] === true", ns("significant_only")),
        numericInput(ns("pathway_fc_threshold"),  "LogFC threshold",        value = 1,    min = 0),
        sliderInput( ns("pathway_p_threshold"),   "Adjusted p-value cutoff",
                     min = 0, max = 1, value = 0.05, step = 0.01)
      ),
      hr(),
      actionButton(
        ns("run_pathway"), "Plot Pathway",
        icon  = icon("play"),
        class = "btn-primary w-100"
      )
    ),
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(ns("pathway_plot"), width = "100%", height = "88vh"),
      type    = 8,
      caption = "Loading pathway map...",
      color   = "#15131e"
    )
  )
}
