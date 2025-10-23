conduit_metadata_tab_ui = function(){
  tabItem(
    "view_metadata",
    fluidRow(
      box(
        solidHeader = TRUE,
        title = "Metadata Metrics",
        background = NULL,
        width = 12,
        status = "info",
        footer = fluidRow(
          column(
            width = 3,
            descriptionBlock(
              text = "# of Variables",
              header = textOutput("num_sample_md_variables"),
              rightBorder = TRUE,
              marginBottom = FALSE
            )
          ),
          column(
            width = 3,
            descriptionBlock(
              text = "# Continuous",
              header = textOutput("num_continuous_variables"),
              rightBorder = FALSE,
              marginBottom = FALSE
            )
          ),
          column(
            width = 3,
            descriptionBlock(
              text = "# Discrete",
              header = textOutput("num_discrete_variables"),
              rightBorder = FALSE,
              marginBottom = FALSE
            )
          )
        )
      ),
      fluidRow(
        column(width = 6,
               box(
                 title = "colData",
                 width = 12,  # Full width inside the column
                 status = "primary",
                 solidHeader = TRUE,
                 background = NULL,
                 DT::dataTableOutput("colData")  # Remove width here; control via server
               )
        ),
        column(width = 6,
               uiOutput("metadata_variable_choices_to_plot_ui"),  # Proper way to include dynamic selectInput
               box(
                 title = "Distribution of Selected Variable",
                 width = 12,  # Full width inside the column
                 status = "primary",
                 solidHeader = TRUE,
                 background = NULL,
                 plotOutput("metadata_distribution_plot")
               )
        )
      ),
      column(
        width = 6,
        downloadButton("download_colData_table", "Download Table", class = "btn-block")
      )
    )
  )

}
