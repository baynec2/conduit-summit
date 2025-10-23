conduit_file_upload_tab_ui = function(){
  tabItem(
    "file_upload",
    # Main body where the user will upload files.
    # Description of Conduit-GUI usage with hyperlink
    h3(
      "Conduit-Summit uses the file created by ",
      a("Conduit-Ascent", href = "https://github.com/baynec2/conduit-ascent")
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
          width = 4,
          descriptionBlock(
            text = "# Samples",
            header = textOutput("num_samples"),  # Corrected to textOutput directly
            rightBorder = TRUE,
            marginBottom = FALSE
          )
        ),
        column(
          width = 4,
          descriptionBlock(
            text = "# of Species Detected",
            header =textOutput("num_species_detected"),
            number = textOutput("per_species_detected"),# Corrected to the correct output
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        ),
        column(
          width = 4,
          descriptionBlock(
            text = "# of Proteins Detected",
            header = textOutput("num_proteins_detected"),
            number = textOutput("per_proteins_detected"),  # Corrected to the correct output
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
            plotOutput("taxa_tree_plot", width = "100%",height = "900px"),
            type = 8,caption = "Please wait, the taxonomic tree is loading...",
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
            DT::dataTableOutput("protein_taxonomy", height = "400px")
            ),
        column(
          width = 12,
          downloadButton("download_protein_taxonomy", "Download Table", class = "btn-block")
        )
      )
    )
}
