conduit_enrichment_tab_ui <- function(id = "enrichment") {
  ns <- NS(id)
  tabItem(
    "enrichment",
    fluidRow(
      column(
        4,
        actionButton(ns("enrichment_return_to_stats_button"), "Click to Return To Stats")
      )
    ),
    fluidRow(
      box(
        title = "Enrichment Options",
        solidHeader = TRUE,
        status = "primary",
        width = 12,
        column(width = 3,
          selectInput(ns("annotation_type"), "Term Type",
            choices = c("go", "kegg", "domain", "kingdom", "phylum", "class",
                        "order", "family", "genus", "species"))
        ),
        column(width = 3,
          selectInput(ns("enrichment_type"), "Type of Enrichment to Perform",
            choices = c("gsea", "ora"))
        ),
        column(width = 3, uiOutput(ns("enrichment_direction_ui"))),
        column(width = 3, fluidRow(uiOutput(ns("enrichment_plot_options_ui")))),
        column(width = 12, actionButton(ns("run_enrichment"), "Run Enrichment", icon = icon("play"),
                                       class = "btn-primary"))
      ),
      fluidRow(
        shinycssloaders::withSpinner(
          verbatimTextOutput(ns("enrichment_summary")),
          type = 8, caption = "Calculating Enrichment Results",
          color = "#15131efe"
        )
      ),
      fluidRow(
        shinycssloaders::withSpinner(
          plotOutput(ns("enrichment_plot")),
          type = 8, caption = "Feature Plot Loading...",
          color = "#15131efe"
        )
      )
    )
  )
}
