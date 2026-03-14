conduit_traverse_tab_ui <- function(id = "traverse") {
  ns <- NS(id)
  tabItem(
    tabName = "traverse",
    fluidRow(
      column(
        width = 12,
        h3("Traverse Feature Hierarchies"),
        tags$p("Explore linked intensities across assays and hierarchical relationships
              between precursors, peptides, proteins, and higher-level annotations.")
      )
    ),
    fluidRow(
      box(
        width = 8,
        height = "550px",
        title = "Feature Intensities Across Assays",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotly::plotlyOutput(ns("qf_plot"))
      ),
      box(
        width = 4,
        height = "550px",
        title = "Controls",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        textInput(ns("traverse_features"), label = "Feature to explore (copy paste)"),
        selectInput(ns("traverse_assay"), label = "Assay to query feature", choices = NULL),
        selectInput(ns("traverse_xaxis"), label = "X-axis variable", choices = NULL)
      )
    ),
    fluidRow(
      box(
        title = "Feature Intensities Across Assays",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 8,
        height = "550px",
        plotOutput(ns("traverse_plot"))
      ),
      box(
        width = 4,
        height = "550px",
        title = "Feature Intensities Across Assays",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        DT::DTOutput(ns("traverse_info"))
      )
    )
  )
}
