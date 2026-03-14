conduit_filter_data_tab_ui <- function(id = "filter_data") {
  ns <- NS(id)
  tabItem(
    "filter_data",
    fluidRow(
      box(
        width = 12,
        solidHeader = TRUE,
        background = NULL,
        status = "primary",
        title = "Metadata (Sample) Filter",
        uiOutput(ns("sample_filters"))
      )
    ),
    fluidRow(
      box(
        width = 12,
        solidHeader = TRUE,
        background = NULL,
        status = "primary",
        title = "Feature Filter (Per Assay)",
        uiOutput(ns("feature_filters"))
      )
    ),
    fluidRow(
      box(
        width = 12,
        solidHeader = TRUE,
        background = NULL,
        status = "primary",
        title = "Filtered Data Preview",
        verbatimTextOutput(ns("filtered_qfeatures"))
      )
    )
  )
}
