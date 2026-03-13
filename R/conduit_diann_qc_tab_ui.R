conduit_diann_qc_tab_ui <- function() {
  tabItem(
    tabName = "diann_qc",
    fluidRow(
      box(
        solidHeader = TRUE,
        title = "Metric to Plot",
        background = NULL,
        width = 12,
        status = "primary",
      uiOutput("diann_qc_columns_ui")
      ),
    ),
    fluidRow(
      plotly::plotlyOutput("diann_qc_plot", height = "600px")
    ),
    fluidRow(
      column(
        width = 12,
        DT::DTOutput("diann_qc_table",width = "100%")
      )
    ),
    column(
      width = 12,
      downloadButton("download_diann_stats", "Download Table", class = "btn-block")
    )
  )
}
