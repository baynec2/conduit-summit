conduit_file_upload_tab_ui <- function() {
  tabItem(
    "file_upload",
    # Main body where the user will upload files.
    # Description of Conduit-GUI usage with hyperlink
    h3(
      "Conduit-GUI uses the file created by ",
      a("Conduit", href = "https://github.com/baynec2/conduit")
    ),
    fileInput("conduit.rds", "Upload Conduit .rds File", accept = ".rds"),
    # Bottom Box that will show the user what they have uploaded
    box(
      solidHeader = TRUE,
      title = "Uploaded File Stats",
      background = NULL,
      width = 12,
      status = "info",
      footer = fluidRow(
        column(
          width = 3,
          descriptionBlock(
            text = "# Samples",
            header = textOutput("num_samples"), # Corrected to textOutput directly
            rightBorder = TRUE,
            marginBottom = FALSE
          )
        ),
        column(
          width = 3,
          descriptionBlock(
            text = "# of Species Detected",
            header = textOutput("num_species_detected"),
            number = textOutput("per_species_detected"), # Corrected to the correct output
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        ),
        column(
          width = 3,
          descriptionBlock(
            text = "# of Proteins Detected",
            header = textOutput("num_proteins_detected"),
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        ),
        column(
          width = 3,
          descriptionBlock(
            text = "# of Peptides Detected",
            header = textOutput("num_peptides_detected"),
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "Taxa Tree Plot Options",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        column(
          width = 4,
          selectInput(
            "taxa_tree_filter",
            "Select Taxonomic Level to Show",
            choices = c(
              "domain",
              "kingdom",
              "phylum",
              "class",
              "order",
              "family",
              "genus",
              "species"
            ),
            selected = "species"
          )
        ),
        column(
          width = 4,
          selectInput(
            "taxa_tree_color",
            "Select variable to color by",
            choices = c(
              "domain",
              "kingdom",
              "phylum",
              "class",
              "order",
              "family",
              "genus",
              "species",
              "download_info"
            ),
            selected = "download_info"
          )
        ),
        column(
          width = 4,
          selectInput(
            "taxa_tree_layout",
            "Select Layout of Plot",
            choices = c(
              "automatic",
              "reingold-tilford",
              "davidson-harel",
              "gem",
              "graphopt",
              "mds",
              "fruchterman-reingold",
              "kamada-kawai",
              "large-graph"
            ),
            selected = "automatic"
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "Taxonomic Tree by Protein Coverage",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        shinycssloaders::withSpinner(
          plotOutput("taxa_tree_plot", width = "100%", height = "900px"),
          type = 8,
          caption = "Please wait, the taxonomic tree is loading...",
          color = "#15131efe"
        )
      )
    ),
    fluidRow(
      DT::DTOutput("protein_taxonomy", height = "400px")
    ),
    fluidRow(
      column(
        width = 12,
        downloadButton(
          "download_protein_taxonomy",
          "Download Table",
          class = "btn-block"
        )
      )
    )
  )
}
