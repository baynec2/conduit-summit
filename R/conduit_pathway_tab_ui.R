conduit_pathway_tab_ui <- function() {
  tabItem(
    tabName = "pathway",
    # KEGG pathway selection
    fluidRow(
      column(
        width = 4,
        actionButton(
          inputId = "pathway_return_to_stats_button",
          "Click to Return to Stats"
        )
      )
    ),
    fluidRow(
      box(
        title = "Kegg Pathway Selection",
        solidHeader = TRUE,
        status = "primary",
        width = 12,
        column(
          width = 12,
          uiOutput("pathway_select_ui")
        )
      )
    ),
    # Plot output
    fluidRow(
      column(
        width = 12,
        plotly::plotlyOutput("pathway_plot", width = "100%", height = "90vh")
      )
    )
  )
}
