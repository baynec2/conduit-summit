conduit_traverse_tab_ui <- function() {
  tabItem(
    tabName = "traverse",

    # Header / description
    fluidRow(
      column(
        width = 12,
        h3("Traverse Feature Hierarchies"),
        tags$p("Explore linked intensities across assays and hierarchical relationships
              between precursors, peptides, proteins, and higher-level annotations.")
      )
    ),

    # Controls + plot layout
    fluidRow(
      # Left-side control panel
      # Right-side plot display
      box(
        width =8,
        height = "550px",
        title = "Feature Intensities Across Assays",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotly::plotlyOutput("qf_plot")
      ),
      box(
        width = 4,
        height = "550px",
        title = "Controls",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        textInput("traverse_features",
                  label = "Feature to explore (copy paste)"
        ),
        selectInput("traverse_assay",
                    label = "Assay to query feature",
                    choices = NULL
        ),
        selectInput("traverse_xaxis",
                    label = "X-axis variable",
                    choices = NULL
        )
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
        plotOutput("traverse_plot")
      ),
      box(
        width = 4,
        height = "550px",
        title = "Feature Intensities Across Assays",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        DT::DTOutput("traverse_info")
      )
    )
    )
}
