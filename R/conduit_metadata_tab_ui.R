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
        DT::DTOutput("colData",width = "100%")
        ),
        column(width = 6,
               uiOutput("metadata_variable_choices_to_plot_ui"),  # Proper way to include dynamic selectInput
               plotOutput("metadata_distribution_plot")
        )
      )
    )
  )

}
