conduit_file_upload_tab_ui <- function(id = "file_upload") {
  ns <- NS(id)
  tabItem(
    "file_upload",
    h3(
      "Conduit-Summit uses the file created by ",
      a("Conduit-Ascent", href = "https://github.com/baynec2/conduit-ascent")
    ),
    fileInput("conduit_rds", "Upload Conduit .rds File", accept = ".rds"),
    box(
      solidHeader = TRUE,
      title = "Uploaded File Stats",
      background = NULL,
      width = 12,
      status = "info",
      footer = fluidRow(
        column(
          width = 4,
          descriptionBlock(
            text = "# Samples",
            header = textOutput(ns("num_samples")),
            rightBorder = TRUE,
            marginBottom = FALSE
          )
        ),
        column(
          width = 4,
          descriptionBlock(
            text = "# of Species Detected",
            header = textOutput(ns("num_species_detected")),
            number = textOutput(ns("per_species_detected")),
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        ),
        column(
          width = 4,
          descriptionBlock(
            text = "# of Proteins Detected",
            header = textOutput(ns("num_proteins_detected")),
            number = textOutput(ns("per_proteins_detected")),
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        )
      )
    ),
    fluidRow(
      box(title = "Taxonomic Tree by Coverage",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          shinycssloaders::withSpinner(
            plotOutput(ns("taxa_tree_plot"), width = "100%", height = "900px"),
            type = 8, caption = "Please wait, the taxonomic tree is loading...",
            color = "#15131efe"
          )
      )
    ),
    fluidRow(
      box(title = "Coverage per Species",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          height = "500px",
          DT::dataTableOutput(ns("protein_taxonomy"), height = "400px")
      ),
      column(
        width = 12,
        downloadButton(ns("download_protein_taxonomy"), "Download Table", class = "btn-block")
      )
    )
  )
}
