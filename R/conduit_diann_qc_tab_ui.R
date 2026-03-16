conduit_diann_qc_tab_ui <- function(id = "diann_qc") {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title  = "Controls",
      width  = 260,
      bg     = "#f8f9fb",
      fg     = "#1a1a2e",
      uiOutput(ns("diann_qc_columns_ui"))
    ),
    tagList(
      bslib::card(
        full_screen = TRUE,
        class       = "card-light",
        bslib::card_header("DIA-NN QC Plot"),
        bslib::card_body(
          shinycssloaders::withSpinner(
            plotOutput(ns("diann_qc_plot"), height = "580px"),
            type = 8, caption = "Loading QC plot...", color = "#15131e"
          )
        )
      ),
      bslib::card(
        full_screen = TRUE,
        class       = "card-light",
        bslib::card_header("DIA-NN Statistics"),
        bslib::card_body(
          shinycssloaders::withSpinner(
            DT::dataTableOutput(ns("diann_qc_table")),
            type = 8, caption = "Loading statistics table...", color = "#15131e"
          )
        ),
        bslib::card_footer(
          downloadButton(ns("download_diann_stats"), "Download Table")
        )
      )
    )
  )
}
