conduit_pathway_tab_ui <- function(id = "pathway") {
  ns <- NS(id)
  tabItem(
    tabName = "pathway",
    fluidRow(
      column(
        width = 4,
        actionButton(inputId = ns("pathway_return_to_stats_button"), "Click to Return to Stats")
      )
    ),
    fluidRow(
      box(
        title = "Kegg Pathway Selection",
        solidHeader = TRUE,
        status = "primary",
        width = 12,
        column(width = 12, uiOutput(ns("pathway_select_ui")))
      )
    ),
    fluidRow(
      column(
        width = 12,
        plotly::plotlyOutput(ns("pathway_plot"), width = "100%", height = "90vh")
      )
    )
  )
}
