conduit_diann_qc_tab_ui <- function() {
  tabItem(
    tabName = "diann_qc",
    fluidRow(
      uiOutput("diann_qc_columns_ui")
    ),
    fluidRow(
      plotOutput("diann_qc_plot", height = "600px")
    ),
    fluidRow(
      dataTableOutput("diann_qc_table")
    ),
    column(
      width = 12,
      downloadButton("download_diann_stats", "Download Table", class = "btn-block")
    )
  )
}
