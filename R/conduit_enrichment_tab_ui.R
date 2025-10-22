conduit_enrichment_tab_ui <- function() {
  tabItem(
    "enrichment",
    fluidRow(
      column(
        4,
        actionButton(
          "enrichment_return_to_stats_button",
          "Click to Return To Stats"
        )
      )
    ),
    fluidRow(
      box(
        title = "Enrichment Options",
        solidHeader = TRUE,
        status = "primary",
        width = 12,
        column(
          width = 3,
          selectInput("annotation_type", "Term Type",
            choices = c(
              "go", "kegg", "domain", "kingdom", "phylum", "class",
              "order", "family", "genus", "species"
            )
          )
        ),
        column(
          width = 3,
          selectInput("enrichment_type", "Type of Enrichment to Perform",
            choices = c("gsea", "ora")
          )
        ),
        column(
          width = 3,
          uiOutput("enrichment_direction_ui")
        ),
        column(
          width = 3,
          fluidRow(uiOutput("enrichment_plot_options_ui"))
        )
      ),
      fluidRow(
        shinycssloaders::withSpinner(
          verbatimTextOutput("enrichment_summary"),
          type = 8, caption = "Calculating Enrichment Results",
          color = "#15131efe"
        )
      ),
      fluidRow(
        shinycssloaders::withSpinner(
          plotOutput("enrichment_plot"),
          type = 8, caption = "Feature Plot Loading...",
          color = "#15131efe"
        )
      )
    )
  )
}
