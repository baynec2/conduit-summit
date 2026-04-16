conduit_provenance_tab_ui <- function(id = "provenance") {
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::value_box(
        title    = "Workflow Version",
        value    = textOutput(ns("version")),
        showcase = icon("code-branch"),
        theme    = "primary"
      ),
      bslib::value_box(
        title    = "Generated Date",
        value    = textOutput(ns("date")),
        showcase = icon("calendar"),
        theme    = "primary"
      ),
      bslib::value_box(
        title    = "UniProtKB Release",
        value    = textOutput(ns("uniprot")),
        showcase = icon("database"),
        theme    = "primary"
      )
    ),
    bslib::card(
      full_screen = TRUE,
      class       = "card-light",
      bslib::card_header("Workflow Configuration"),
      bslib::card_body(
        bslib::navset_tab(
          bslib::nav_panel(
            "Snakemake Config",
            verbatimTextOutput(ns("txt_snakemake"))
          ),
          bslib::nav_panel(
            "DIA-NN Library",
            verbatimTextOutput(ns("txt_diann_lib"))
          ),
          bslib::nav_panel(
            "DIA-NN Run",
            verbatimTextOutput(ns("txt_diann_run"))
          ),
          bslib::nav_panel(
            "Runtime",
            verbatimTextOutput(ns("txt_runtime"))
          )
        )
      ),
      bslib::card_footer(
        uiOutput(ns("download_buttons_ui"))
      )
    )
  )
}
