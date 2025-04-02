conduit_file_upload_tab_ui = function(){
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
            header = textOutput("num_samples"),  # Corrected to textOutput directly
            rightBorder = TRUE,
            marginBottom = FALSE
          )
        ),
        column(
          width = 3,
          descriptionBlock(
            text = "# of Sample Metadata Variables",
            header =textOutput("num_sample_md_variables"),  # Corrected to the correct output
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        ),
        column(
          width = 3,
          descriptionBlock(
            text = "# of Species Detected",
            header =textOutput("num_species_detected"),
            number = textOutput("per_species_detected"),# Corrected to the correct output
            rightBorder = FALSE,
            marginBottom = FALSE
          )
        ),
        column(
          width = 3,
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
      selectInput("pg_to_consider", "Select Protein Grouping To Consider",
                  choices = c("multiple_proteins_in_group", "one_protein_in_group"))
    ),

    fluidRow(
      box(title = "Percent of Detected Proteins Taxonomic Tree",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          plotOutput("taxa_tree_plot", width = "100%",height = "900px")
      )
    ),
    fluidRow(
        box(title = "Protein Taxonomy Table",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            height = "500px",
            DT::dataTableOutput("protein_taxonomy", height = "400px")
        )
      )
    )
}
