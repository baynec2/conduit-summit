conduit_database_tab_ui <- function(id = "database") {
  ns <- NS(id)
  tagList(
    # ── Dataset stats ────────────────────────────────────────────────────────
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::value_box(
        title    = "Samples",
        value    = textOutput(ns("num_samples")),
        showcase = icon("vials"),
        theme    = "primary"
      ),
      bslib::value_box(
        title    = "Species Detected",
        value    = textOutput(ns("num_species_detected")),
        showcase = icon("bacteria"),
        theme    = "primary"
      ),
      bslib::value_box(
        title    = "Protein Groups Detected",
        value    = textOutput(ns("num_proteins_detected")),
        showcase = icon("atom"),
        theme    = "primary"
      )
    ),

    # ── Taxonomic coverage ───────────────────────────────────────────────────
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Taxonomic Tree by Coverage"),
      bslib::card_body(
        shinycssloaders::withSpinner(
          plotOutput(ns("taxa_tree_plot"), width = "100%", height = "900px"),
          type = 8, caption = "Please wait, the taxonomic tree is loading...",
          color = "#15131e"
        )
      )
    ),

    # ── Per-species coverage table ───────────────────────────────────────────
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Coverage per Species"),
      bslib::card_body(
        DT::dataTableOutput(ns("protein_taxonomy"), height = "400px")
      ),
      bslib::card_footer(
        downloadButton(ns("download_protein_taxonomy"), "Download Table")
      )
    )
  )
}
