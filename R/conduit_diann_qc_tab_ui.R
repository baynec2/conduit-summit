conduit_diann_qc_tab_ui <- function(id = "diann_qc") {
  ns <- NS(id)
  tabItem(
    tabName = "diann_qc",
    fluidRow(
      uiOutput(ns("diann_qc_columns_ui"))
    ),
    fluidRow(
      plotOutput(ns("diann_qc_plot"), height = "600px")
    ),
    fluidRow(
      DT::dataTableOutput(ns("diann_qc_table"))
    ),
    column(
      width = 12,
      downloadButton(ns("download_diann_stats"), "Download Table", class = "btn-block")
    )
  )
}
