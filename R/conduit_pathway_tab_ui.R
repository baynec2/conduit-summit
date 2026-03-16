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
      uiOutput(ns("pathway_select_ui"))
    ),
    plotly::plotlyOutput(ns("pathway_plot"), width = "100%", height = "88vh")
  )
}
